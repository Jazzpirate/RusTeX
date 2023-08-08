//! Default implementations for [`Gullet`] methods.
use std::hint::unreachable_unchecked;
use std::marker::PhantomData;
use crate::{debug_log, file_end, throw};
use crate::engine::EngineType;
use crate::engine::gullet::Gullet;
use crate::engine::state::State;
use crate::tex::catcodes::{CategoryCode, CategoryCodeScheme};
use crate::tex::commands::{Command, BaseCommand, ResolvedToken, ConditionalFun, TokenCont, CommandSource};
use crate::tex::numbers::Int;
use crate::tex::token::{BaseToken, Token};
use crate::utils::Ptr;
use crate::engine::mouth::Mouth;
use crate::tex::commands::etex::{UNEXPANDED, UNLESS};
use crate::tex::commands::methods::expand_def;
use crate::tex::commands::tex::{ELSE, FI, IFCASE, NOEXPAND, THE};
use crate::tex::ConditionalBranch;
use crate::tex::fonts::FontStore;
use crate::utils::errors::TeXError;
use crate::utils::strings::{CharType, TeXStr};
use crate::utils::strings::AllCharsTrait;


/// Expands [`Token`]s for as long as possible and returns the [`ResolvedToken`] for the next unexpandable [`Token`] encountered
/// (or [`None`] if the [`Mouth`] is empty)
pub fn get_next_unexpandable<ET:EngineType>(gullet:&mut ET::Gullet,state:&mut ET::State) -> Result<Option<ResolvedToken<ET>>,TeXError<ET::Token>> {
    while let Some((next,e)) = gullet.mouth().get_next::<ET>(state)? {
        if !e {return Ok(Some(ResolvedToken{command:BaseCommand::Relax,source:CommandSource{cause:next,reference:None},expand:false}))}
        match gullet.expand(state,resolve_token(state,next))? {
            Some(r) => return Ok(Some(r)),
            _ => ()
        }
    }
    Ok(None)
}


/// Resolves the given [`Token`]
pub fn resolve_token<ET:EngineType>(state:&ET::State,tk:ET::Token) -> ResolvedToken<ET> {
    match match tk.base() {
        BaseToken::Char(c, CategoryCode::Active) => state.get_ac_command(c),
        BaseToken::CS(name) => state.get_command(name),
        BaseToken::Char(char,catcode) =>
            return ResolvedToken {
                command: BaseCommand::Char { char: *char, catcode: *catcode },
                source: CommandSource { cause: tk, reference: None },
                expand: true
            }
    } {
        None => ResolvedToken {
            command:BaseCommand::None,
            source:CommandSource{cause:tk,reference:None},
            expand:true
        },
        Some(cmd) => ResolvedToken{
            command:cmd.base.clone(),
            source:CommandSource{cause:tk,reference:cmd.reference.clone()},
            expand:true
        }
    }
}

pub fn do_conditional<ET:EngineType>(gullet:&mut ET::Gullet,state:&mut ET::State,cmd:CommandSource<ET>,name:&'static str,apply:ConditionalFun<ET>,unless:bool) -> Result<(),TeXError<ET::Token>> {
    if name == IFCASE {
        debug_log!(trace=>"ifcase");
        let i = gullet.new_conditional(IFCASE);
        let ret = gullet.get_int(state)?.to_i64();
        debug_log!(trace=>"ifcase: {}",ret);
        gullet.set_conditional(i, ConditionalBranch::Case(ret, 0));
        if ret == 0 {
            debug_log!(trace=>"True conditional");
            Ok(())
        } else {
            false_loop::<ET>(gullet,state,"ifcase",i)
        }
    } else {
        let i = gullet.new_conditional(name);
        let b = apply(state,gullet,cmd)?;
        if (b && !unless) || (!b && unless) {
            gullet.set_conditional(i,ConditionalBranch::True(name));
            debug_log!(trace=>"True conditional");
            Ok(())
        } else { false_loop::<ET>(gullet, state,name,i) }
    }
}


pub fn get_string<ET:EngineType>(gullet:&mut ET::Gullet, state: &mut ET::State) -> Result<String,TeXError<ET::Token>> {
    debug_log!(trace=>"Reading string {}...\n at {}",gullet.mouth().preview(50).replace("\n","\\n"),gullet.mouth().file_line());
    let mut ret = String::new();
    gullet.mouth().skip_whitespace::<ET>(state)?;
    let mut quoted = false;
    while let Some(next) = gullet.get_next_unexpandable(state)? {
        match next.command {
            BaseCommand::Char {catcode:CategoryCode::Space,..} if quoted => ret.push(' '),
            BaseCommand::Char {catcode:CategoryCode::Space,..} => return Ok(ret),
            BaseCommand::Char {char,..} if char.to_usize() == 34 => { // "
                quoted = !quoted;
            }
            BaseCommand::Char {char,..} => ret.push_str(std::str::from_utf8(char.as_bytes()).unwrap()), //(char.as_bytes())),//ret.push(char.to_char()),
            _ => {
                gullet.mouth().requeue(next.source.cause);
                return Ok(ret)
            }
        }
    }
    Ok(ret)
}


pub fn get_braced_string<ET:EngineType>(gullet:&mut ET::Gullet, state: &mut ET::State) -> Result<String,TeXError<ET::Token>> {
    let mut ret = vec!();
    let cc = state.get_catcode_scheme().clone();
    let esc = state.get_escapechar();
    gullet.get_expanded_group(state,false,false,true,&mut |s,t| {
        match t.base() {
            BaseToken::Char(c,_) => {
                for u in c.as_bytes() { ret.push(*u) }
            }
            BaseToken::CS(name) => {
                if let Some(c) = esc { for u in c.as_bytes() { ret.push(*u) } }
                for c in name.as_vec() { for u in c.as_bytes() { ret.push(*u) } }
                if name.len() > 1 || *cc.get(&name.as_vec()[0]) != CategoryCode::Letter {
                    ret.push(b' ')
                }
            }
        }
        Ok(())
    })?;
    Ok(String::from_utf8(ret).unwrap())
}

pub fn get_group<ET:EngineType>(gullet:&mut ET::Gullet, state: &mut ET::State, f:TokenCont<ET>) -> Result<(),TeXError<ET::Token>> {
    match gullet.mouth().get_next::<ET>(state)? {
        Some((t,_)) if t.catcode() == CategoryCode::BeginGroup => (),
        _ => throw!("begin group expected")
    }
    let mut ingroup = 0;
    while let Some(next) = gullet.mouth().get_next::<ET>(state)? {
        let tk = next.0;
        match tk.base() {
            BaseToken::Char(_,CategoryCode::BeginGroup) => ingroup += 1,
            BaseToken::Char(_,CategoryCode::EndGroup) => {
                if ingroup == 0 { return Ok(()) } else { ingroup -= 1; }
            }
            _ => ()
        }
        f(state,tk)?;
    }
    file_end!()
}

pub fn get_expanded_group<ET:EngineType>(gullet:&mut ET::Gullet, state: &mut ET::State, expand_protected:bool, edef_like:bool, err_on_unknowns:bool, f: TokenCont<ET>) -> Result<(),TeXError<ET::Token>> {
    match gullet.mouth().get_next::<ET>(state)? {
        Some((t,_)) if t.catcode() == CategoryCode::BeginGroup => (),
        _ => throw!("begin group expected")
    }
    let mut ingroup = 0;
    while let Some(next) = gullet.mouth().get_next::<ET>(state)? {
        if next.1 {
            let mut res = resolve_token::<ET>(state, next.0);
            match res.command {
                BaseCommand::Char { catcode: CategoryCode::BeginGroup, .. } => {
                    ingroup += 1;
                    f(state, res.source.cause)?
                }
                BaseCommand::Char { catcode: CategoryCode::EndGroup, .. } => {
                    if ingroup == 0 { return Ok(()) } else { ingroup -= 1; }
                    f(state, res.source.cause)?
                }
                BaseCommand::Def(d) if d.protected && !expand_protected =>
                    f(state, res.source.cause)?,
                BaseCommand::Expandable { name, .. } if name == NOEXPAND => {
                    match gullet.mouth().get_next::<ET>(state)? {
                        None => file_end!(),
                        Some((t, _)) => f(state, t)?
                    }
                },
                BaseCommand::Expandable { name, apply } if name == UNEXPANDED && edef_like => {
                    apply(state, gullet, res.source, &mut |s, t| {
                        if t.catcode() == CategoryCode::Parameter { f(s, t.clone())? }
                        f(s, t)
                    })?
                }
                BaseCommand::Expandable { name, apply } if name == UNEXPANDED => {
                    apply(state, gullet, res.source, f)?
                }
                BaseCommand::Expandable { name, apply } if name == THE && edef_like =>
                    apply(state, gullet, res.source, &mut |s, t| {
                        if t.catcode() == CategoryCode::Parameter { f(s, t.clone())? }
                        f(s, t)
                    })?,
                BaseCommand::None if err_on_unknowns => return Err(match res.source.cause.base() {
                    BaseToken::Char(c, _) => throw!("Undefined active character {}",c),
                    BaseToken::CS(name) => throw!("Undefined control sequence {}",name),
                }),
                _ => match gullet.expand(state, res)? {
                    Some(res) => f(state, res.source.cause)?,
                    _ => ()
                }
            }
        } else { f(state,next.0)?}
    }
    file_end!()
}

pub fn false_loop<ET:EngineType>(gullet:&mut ET::Gullet,state:&mut ET::State,condname:&'static str,condidx:usize) -> Result<(),TeXError<ET::Token>> {
    debug_log!(trace=>"False conditional. Skipping...");
    let mut incond:usize = gullet.current_conditional().1 - condidx;
    for i in 0..incond {
        gullet.pop_conditional();
    }
    while let Some((next,exp)) = gullet.mouth().get_next::<ET>(state)? {
        match next.base() {
            BaseToken::Char(c,CategoryCode::Active) =>
                match state.get_ac_command(c).map(|c| &c.base) {
                    Some(BaseCommand::Conditional {..}) => incond += 1,
                    Some(BaseCommand::Expandable {name:"else",..}) if incond == 0 => {
                        debug_log!(trace=>"...else branch.");
                        gullet.set_top_conditional(ConditionalBranch::Else(condname));
                        return Ok(())
                    }
                    Some(BaseCommand::Expandable {name:"or",..}) if incond == 0 && condname == "ifcase" => {
                        match gullet.current_conditional() {
                            (Some(ConditionalBranch::Case(i, j)),_) => {
                                gullet.set_top_conditional(ConditionalBranch::Case(i, j + 1));
                                if i == ((j+1) as i64) {
                                    debug_log!(trace=>"...or branch.");
                                    return Ok(())
                                }
                            }
                            _ => unreachable!()
                        }
                    }
                    Some(BaseCommand::Expandable {name:"fi",..}) if incond > 0 => incond -= 1,
                    Some(BaseCommand::Expandable {name:"fi",..}) => {
                        debug_log!(trace=>"...end of conditional.");
                        gullet.pop_conditional();
                        return Ok(())
                    }
                    _ => ()
                }
            BaseToken::CS(name) => {
                match state.get_command(name).as_deref().map(|c| &c.base) {
                    Some(BaseCommand::Conditional {..}) => incond += 1,
                    Some(BaseCommand::Expandable {name:"else",..}) if incond == 0 => {
                        gullet.set_top_conditional(ConditionalBranch::Else(condname));
                        debug_log!(trace=>"...else branch.");
                        return Ok(())
                    }
                    Some(BaseCommand::Expandable {name:"or",..}) if incond == 0 && condname == "ifcase" => {
                        match gullet.current_conditional() {
                            (Some(ConditionalBranch::Case(i, j)),_) => {
                                gullet.set_top_conditional(ConditionalBranch::Case(i, j + 1));
                                if i == ((j+1) as i64) {
                                    debug_log!(trace=>"...or branch.");
                                    return Ok(())
                                }
                            }
                            _ => unreachable!()
                        }
                    }
                    Some(BaseCommand::Expandable {name:"fi",..}) if incond > 0 => incond -= 1,
                    Some(BaseCommand::Expandable {name:"fi",..}) => {
                        debug_log!(trace=>"...end of conditional.");
                        gullet.pop_conditional();
                        return Ok(())
                    }
                    _ => ()
                }
            }
            _ => ()
        }
    }
    file_end!()
}

pub fn else_loop<ET:EngineType>(gullet:&mut ET::Gullet,state:&mut ET::State,condname:&'static str,condidx:usize,allowelse:bool) -> Result<(),TeXError<ET::Token>> {
    debug_log!(trace=>"\\else. Skipping...");
    gullet.set_top_conditional(ConditionalBranch::Else(condname));
    let mut incond:usize = gullet.current_conditional().1 - condidx;
    while let Some((next,exp)) = gullet.mouth().get_next::<ET>(state)? {
        match next.base() {
            BaseToken::Char(c,CategoryCode::Active) =>
                match state.get_ac_command(c).as_deref().map(|c| &c.base) {
                    Some(BaseCommand::Conditional {..}) => incond += 1,
                    Some(BaseCommand::Expandable {name:"else",..}) if incond == 0 && !allowelse => {
                        throw!("Unexpected \\else" => next)
                    }
                    Some(BaseCommand::Expandable {name:"fi",..}) if incond > 0 => incond -= 1,
                    Some(BaseCommand::Expandable {name:"fi",..}) => {
                        debug_log!(trace=>"...end of conditional.");
                        gullet.pop_conditional();
                        return Ok(())
                    }
                    _ => ()
                }
            BaseToken::CS(name) => {
                match state.get_command(name).as_deref().map(|c| &c.base) {
                    Some(BaseCommand::Conditional {..}) => incond += 1,
                    Some(BaseCommand::Expandable {name:"else",..}) if incond == 0 && !allowelse => {
                        throw!("Unexpected \\else" => next)
                    }
                    Some(BaseCommand::Expandable {name:"fi",..}) if incond > 0 => incond -= 1,
                    Some(BaseCommand::Expandable {name:"fi",..}) => {
                        debug_log!(trace=>"...end of conditional.");
                        gullet.pop_conditional();
                        return Ok(())
                    }
                    _ => ()
                }
            }
            _ => ()
        }
    }
    Ok(())
}

pub fn get_keyword<'a,ET:EngineType>(gullet:&mut ET::Gullet, state:&mut ET::State, kw:&'a str) -> Result<bool,TeXError<ET::Token>> {
    debug_log!(trace=>"Reading keyword {:?}: {}...\n at {}",kw,gullet.mouth().preview(50).replace("\n","\\n"),gullet.mouth().file_line());
    let mut current = String::new();
    let mut read_toks = gullet.mouth().new_tokensource();
    while let Some(next) = gullet.get_next_unexpandable(state)? {
        read_toks.push(next.source.cause);
        match next.command {
            BaseCommand::Char {char,..} => {
                let us = char.to_usize();
                if us < 256 {
                    current.push(us as u8 as char);
                    if current == kw { return Ok(true) }
                    else if !kw.starts_with(&current) {
                        gullet.mouth().push_tokens(read_toks);
                        return Ok(false)
                    }
                } else {
                    gullet.mouth().push_tokens(read_toks);
                    return Ok(false)
                }
            }
            _ => {
                gullet.mouth().push_tokens(read_toks);
                return Ok(false)
            }
        }
    }
    file_end!()
}

pub fn get_keywords<'a,ET:EngineType>(gullet:&mut ET::Gullet, state:&mut ET::State, mut keywords:Vec<&'a str>) -> Result<Option<&'a str>,TeXError<ET::Token>> {
    debug_log!(trace=>"Reading keywords {:?}: {}...\n at {}",keywords,gullet.mouth().preview(50).replace("\n","\\n"),gullet.mouth().file_line());
    let mut current = String::new();
    let mut read_toks = gullet.mouth().new_tokensource();
    while let Some(next) = gullet.get_next_unexpandable(state)? {
        read_toks.push(next.source.cause.clone());
        match next.command {
            BaseCommand::Char{char,..} => {
                let us = char.to_usize();
                if us < 256 && keywords.iter().any(|s| s.starts_with(&current) && s.len()>current.len() && s.as_bytes()[current.len()] == us as u8) {
                    current.push(us as u8 as char);
                    keywords = keywords.into_iter().filter(|s| s.starts_with(&current)).collect();
                    if keywords.is_empty() {
                        gullet.mouth().push_tokens(read_toks);
                        return Ok(None)
                    }
                    else if keywords.len() == 1 && keywords[0] == current { return Ok(Some(keywords[0])) }
                } else if keywords.contains(&current.as_str()) {
                    gullet.mouth().requeue(next.source.cause);
                    keywords = keywords.into_iter().filter(|s| s == &current).collect();
                    return Ok(Some(keywords[0]))
                } else {
                    gullet.mouth().push_tokens(read_toks);
                    return Ok(None)
                }
            }
            _ if keywords.contains(&current.as_str()) => {
                gullet.mouth().requeue(next.source.cause);
                keywords = keywords.into_iter().filter(|s| s == &current).collect();
                return Ok(Some(keywords[0]))
            }
            _ => {
                gullet.mouth().push_tokens(read_toks);
                return Ok(None)
            }
        }

    }
    file_end!()
}

pub fn token_to_chars<T:Token>(tk:&T,escape:Option<T::Char>,cc:&CategoryCodeScheme<T::Char>,insertspace:bool,f:&mut dyn FnMut(T) -> Result<(),TeXError<T>>) -> Result<(),TeXError<T>> {
    match tk.base() {
        BaseToken::Char(c,_) if c.to_usize() == 32 => f(T::new(BaseToken::Char(*c,CategoryCode::Space),None)),
        BaseToken::Char(c,CategoryCode::Space) => f(T::new(BaseToken::Char(*c,CategoryCode::Space),None)),
        BaseToken::Char(c,_) => f(T::new(BaseToken::Char(*c,CategoryCode::Other),None)),
        BaseToken::CS(str) => {
            match escape {
                None => (),
                Some(c) => f(T::new(BaseToken::Char(c,CategoryCode::Other),None))?
            }
            for c in str.as_vec() {
                f(T::new(BaseToken::Char(*c, if c.to_usize() == 32 { CategoryCode::Space } else { CategoryCode::Other }
                ),None))?
            }
            if insertspace && !(str.len() == 1 && *cc.get(&str.as_vec()[0]) != CategoryCode::Letter) {
                f(T::new(BaseToken::Char(T::Char::from(32),CategoryCode::Space),None))?
            }
            Ok(())
        }
    }
}
/*
pub fn token_to_bytes<T:Token>(tk:T,escapechar:Option<T::Char>,cc:&CategoryCodeScheme<T::Char>) -> Vec<u8> {
    match escapechar {
        None => match tk.base() {
                BaseToken::Char(c,CategoryCode::Space) => vec!(b' '),
                BaseToken::Char(c,_) => c.as_bytes(),
                BaseToken::CS(str) => {
                    let mut s = vec!();
                    for c in str.as_vec() {for u in c.as_bytes() {
                        s.push(u);
                    }}
                    s.push(b' ');
                    s
                }
            }
        Some(escapechar) => match tk.base() {
                BaseToken::Char(c,CategoryCode::Space) => vec!(b' '),
                BaseToken::Char(c,_) => c.as_bytes(),
                BaseToken::CS(str) => {
                    let mut s = vec!();
                    for u in escapechar.as_bytes() {s.push(u)};
                    for c in str.as_vec() {for u in c.as_bytes() {
                        s.push(u);
                    }}
                    if str.as_vec().len() != 1 || *cc.get(&str.as_vec()[0]) != CategoryCode::Letter {
                        s.push(b' ');
                    }
                    s
                }
            }
    }
}

 */

pub fn tokens_to_string<T:Token>(v:Vec<T>,escapechar:Option<T::Char>,cc:&CategoryCodeScheme<T::Char>) -> String {
    let mut s = vec!();
    match escapechar {
        None => for t in v {
                match t.base() {
                    BaseToken::Char(c,CategoryCode::Space) => s.push(b' '),
                    BaseToken::Char(c,_) => for u in c.as_bytes() {s.push(*u)},  //s.push_str(&c.char_str()),
                    BaseToken::CS(str) => {
                        for c in str.as_vec() {for u in c.as_bytes() {
                            s.push(*u);
                        }}
                        if str.as_vec().len() != 1 || *cc.get(&str.as_vec()[0]) != CategoryCode::Letter {
                            s.push(b' ');
                        }
                    }
                }
            }
        Some(escapechar) => for t in v {
                match t.base() {
                    BaseToken::Char(c,CategoryCode::Space) => s.push(b' '),
                    BaseToken::Char(c,_) => for u in c.as_bytes() {s.push(*u)},
                    BaseToken::CS(str) => {
                        for u in escapechar.as_bytes() {s.push(*u)};
                        for c in str.as_vec() {for u in c.as_bytes() {
                            s.push(*u);
                        }}
                        if str.as_vec().len() != 1 || *cc.get(&str.as_vec()[0]) != CategoryCode::Letter {
                            s.push(b' ');
                        }
                    }
                }
            }
    }
    String::from_utf8(s).unwrap()
}

pub fn string_to_tokens<ET:EngineType>(str:&[u8],state:&ET::State,f:TokenCont<ET>) -> Result<(),TeXError<ET::Token>> {
    for u in str {
        let c = ET::Char::from(*u);
        f(state,ET::Token::new(BaseToken::Char(c,if *u == 32 {CategoryCode::Space} else {CategoryCode::Other}),None))?;
    }
    Ok(())
}


pub fn get_control_sequence<ET:EngineType>(gullet:&mut ET::Gullet, state:&mut ET::State) -> Result<ET::Token,TeXError<ET::Token>> {
    gullet.mouth().skip_whitespace::<ET>(state)?;
    while let Some((next,e)) = gullet.mouth().get_next::<ET>(state)? {
        let resolved = resolve_token::<ET>(state,next);
        match resolved.command {
            BaseCommand::Char{char,catcode:CategoryCode::Active} =>
                match get_cs_check_command::<ET>(gullet, state, resolved)? {
                    None => (),
                    Some(t) => return Ok(t),
                }
            BaseCommand::Char{..} => {
                todo!()
                // error
            }
            _ => match get_cs_check_command::<ET>(gullet, state, resolved)? {
                None => (),
                Some(t) => return Ok(t),
            }
        }
    }
    file_end!()
}

fn get_cs_check_command<ET:EngineType>(gullet:&mut ET::Gullet, state:&mut ET::State, resolved:ResolvedToken<ET>)
                                       -> Result<Option<ET::Token>,TeXError<ET::Token>> {
    match resolved.command {
        BaseCommand::Expandable {..} | BaseCommand::Conditional { .. } => {
            gullet.expand(state,resolved)?;
            Ok(None)
        },
        _ => Ok(Some(resolved.source.cause))
    }
}

pub fn get_char<ET:EngineType>(gullet:&mut ET::Gullet,state:&mut ET::State) -> Result<ET::Char,TeXError<ET::Token>> {
    let num = gullet.get_int(state)?;
    match ET::Char::from_i64(num.to_i64()) {
        Some(i) => Ok(i),
        None => throw!("Not a valid character: {}",num)
    }
}

pub fn get_font<ET:EngineType>(gullet: &mut ET::Gullet,state:&mut ET::State) -> Result<ET::Font,TeXError<ET::Token>> {
    match gullet.get_next_unexpandable(state)? {
        None => file_end!(),
        Some(res) => match res.command {
            BaseCommand::Font(f) => Ok(f),
            BaseCommand::FontCommand {get,..} => {
                get(state,gullet,res.source)
            }
            _ => throw!("Expected a font, got {:?}",res.command)
        }
    }
}