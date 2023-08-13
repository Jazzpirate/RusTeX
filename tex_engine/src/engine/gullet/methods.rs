//! Default implementations for [`Gullet`] methods.
use std::hint::unreachable_unchecked;
use std::marker::PhantomData;
use crate::{debug_log, file_end, throw};
use crate::engine::{EngineMut, EngineType};
use crate::engine::gullet::Gullet;
use crate::engine::memory::Memory;
use crate::engine::state::State;
use crate::tex::catcodes::{CategoryCode, CategoryCodeScheme};
use crate::tex::commands::{Command, BaseCommand, ResolvedToken, ConditionalFun, TokenCont, CommandSource, StomachCommand};
use crate::tex::numbers::{Int, MuSkip, Skip};
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
pub fn get_next_unexpandable<ET:EngineType>(engine:&mut EngineMut<ET>) -> Result<Option<ResolvedToken<ET>>,TeXError<ET>> {
    while let Some((next,e)) = engine.get_next_token()? {
        if !e {return Ok(Some(ResolvedToken{command:BaseCommand::Relax,source:CommandSource{cause:next,reference:None},expand:false}))}
        match engine.expand(resolve_token(engine.state,next))? {
            Some(r) => return Ok(Some(r)),
            _ => ()
        }
    }
    Ok(None)
}


/// Resolves the given [`Token`]
pub fn resolve_token<ET:EngineType>(state:&ET::State,tk:Token<ET>) -> ResolvedToken<ET> {
    match match &tk.base {
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

pub fn do_conditional<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>, name:&'static str, apply:ConditionalFun<ET>, unless:bool) -> Result<(),TeXError<ET>> {
    if name == IFCASE {
        debug_log!(trace=>"ifcase");
        let i = engine.gullet.new_conditional(IFCASE);
        let ret = engine.get_int()?.to_i64();
        debug_log!(trace=>"ifcase: {}",ret);
        engine.gullet.set_conditional(i, ConditionalBranch::Case(ret, 0));
        if ret == 0 {
            debug_log!(trace=>"True conditional");
            Ok(())
        } else {
            false_loop::<ET>(engine.gullet,engine.mouth,engine.state,engine.memory,IFCASE,i)
        }
    } else {
        let i = engine.gullet.new_conditional(name);
        let b = apply(engine,cmd)?;
        if (b && !unless) || (!b && unless) {
            engine.gullet.set_conditional(i,ConditionalBranch::True(name));
            debug_log!(trace=>"True conditional");
            Ok(())
        } else { false_loop::<ET>(engine.gullet, engine.mouth,engine.state,engine.memory,name,i) }
    }
}

pub fn get_expanded_group<ET:EngineType>(engine:&mut EngineMut<ET>, expand_protected:bool, edef_like:bool, err_on_unknowns:bool, f: TokenCont<ET>) -> Result<(),TeXError<ET>> {
    match engine.get_next_token()? {
        Some((t,_)) if t.catcode() == CategoryCode::BeginGroup => (),
        _ => throw!("begin group expected")
    }
    let mut ingroup = 0;
    while let Some(next) = engine.get_next_token()? {
        if next.1 {
            let mut res = resolve_token::<ET>(engine.state, next.0);
            match res.command {
                BaseCommand::Char { catcode: CategoryCode::BeginGroup, .. } => {
                    ingroup += 1;
                    f(engine, res.source.cause)?
                }
                BaseCommand::Char { catcode: CategoryCode::EndGroup, .. } => {
                    if ingroup == 0 { return Ok(()) } else { ingroup -= 1; }
                    f(engine, res.source.cause)?
                }
                BaseCommand::Def(d) if d.protected && !expand_protected =>
                    f(engine, res.source.cause)?,
                BaseCommand::Expandable { name, .. } if name == NOEXPAND => {
                    match engine.get_next_token()? {
                        None => file_end!(),
                        Some((t, _)) => f(engine, t)?
                    }
                },
                BaseCommand::Expandable { name, apply } if name == UNEXPANDED && edef_like => {
                    apply(engine, res.source, &mut |s, t| {
                        if t.catcode() == CategoryCode::Parameter { f(s, t.clone())? }
                        f(s, t)
                    })?
                }
                BaseCommand::Expandable { name, apply } if name == UNEXPANDED => {
                    apply(engine, res.source, f)?
                }
                BaseCommand::Expandable { name, apply } if name == THE && edef_like =>
                    apply(engine, res.source, &mut |s, t| {
                        if t.catcode() == CategoryCode::Parameter { f(s, t.clone())? }
                        f(s, t)
                    })?,
                BaseCommand::None if err_on_unknowns => match res.source.cause.base {
                    BaseToken::Char(c, _) => throw!("Undefined active character {}",c),
                    BaseToken::CS(name) => throw!("Undefined control sequence {}",name.to_str(engine.memory)),
                }
                _ => match engine.expand(res)? {
                    Some(res) => f(engine, res.source.cause)?,
                    _ => ()
                }
            }
        } else { f(engine,next.0)?}
    }
    file_end!()
}

pub fn false_loop<ET:EngineType>(gullet:&mut ET::Gullet,mouth:&mut ET::Mouth,state:&mut ET::State,memory:&mut Memory<ET>,condname:&'static str,condidx:usize) -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"False conditional. Skipping...");
    let mut incond:usize = gullet.current_conditional().1 - condidx;
    for i in 0..incond {
        gullet.pop_conditional();
    }
    while let Some((next,exp)) = mouth.get_next(state,memory)? {
        match &next.base {
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

pub fn else_loop<ET:EngineType>(gullet:&mut ET::Gullet,mouth:&mut ET::Mouth,state:&mut ET::State,memory:&mut Memory<ET>,condname:&'static str,condidx:usize,allowelse:bool) -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"\\else. Skipping...");
    gullet.set_top_conditional(ConditionalBranch::Else(condname));
    let mut incond:usize = gullet.current_conditional().1 - condidx;
    while let Some((next,exp)) = mouth.get_next(state,memory)? {
        match &next.base {
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

pub fn get_keyword<'a,ET:EngineType>(engine:&mut EngineMut<ET>, kw:&'a str) -> Result<bool,TeXError<ET>> {
    debug_log!(trace=>"Reading keyword {:?}: {}...\n at {}",kw,engine.preview(50).replace("\n","\\n"),engine.current_position());
    let mut current = String::new();
    engine.add_expansion(|engine,rs|{
        while let Some(next) = engine.get_next_unexpandable()? {
            rs.push(next.source.cause,engine.memory);
            match next.command {
                BaseCommand::Char {char,..} => {
                    let us = char.to_usize();
                    if us < 256 {
                        current.push(us as u8 as char);
                        if current == kw {
                            rs.reset(engine.memory);
                            return Ok(true)
                        }
                        else if !kw.starts_with(&current) {
                            return Ok(false)
                        }
                    } else {
                        return Ok(false)
                    }
                }
                _ => {
                    return Ok(false)
                }
            }
        }
        file_end!()
    })
}

pub fn get_keywords<'a,ET:EngineType>(engine:&mut EngineMut<ET>, mut keywords:Vec<&'a str>) -> Result<Option<&'a str>,TeXError<ET>> {
    debug_log!(trace=>"Reading keywords {:?}: {}...\n at {}",keywords,engine.preview(50).replace("\n","\\n"),engine.current_position());
    let mut current = String::new();
    engine.add_expansion(|engine,rs| {
        while let Some(next) = engine.get_next_unexpandable()? {
            rs.push(next.source.cause.clone(),engine.memory);
            match next.command {
                BaseCommand::Char{char,..} => {
                    let us = char.to_usize();
                    if us < 256 && keywords.iter().any(|s| s.starts_with(&current) && s.len()>current.len() && s.as_bytes()[current.len()] == us as u8) {
                        current.push(us as u8 as char);
                        keywords = keywords.into_iter().filter(|s| s.starts_with(&current)).collect();
                        if keywords.is_empty() {
                            return Ok(None)
                        }
                        else if keywords.len() == 1 && keywords[0] == current {
                            rs.reset(engine.memory);
                            return Ok(Some(keywords[0]))
                        }
                    } else if keywords.contains(&current.as_str()) {
                        engine.mouth.requeue(next.source.cause,engine.memory);
                        rs.reset(engine.memory);
                        keywords = keywords.into_iter().filter(|s| s == &current).collect();
                        return Ok(Some(keywords[0]))
                    } else {
                        return Ok(None)
                    }
                }
                _ if keywords.contains(&current.as_str()) => {
                    engine.mouth.requeue(next.source.cause,engine.memory);
                    rs.reset(engine.memory);
                    keywords = keywords.into_iter().filter(|s| s == &current).collect();
                    return Ok(Some(keywords[0]))
                }
                _ => {
                    return Ok(None)
                }
            }

        }
        file_end!()
    })
}


pub fn get_string<ET:EngineType>(engine:&mut EngineMut<ET>,ret:&mut String) -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"Reading string {}...\n at {}",engine.preview(50).replace("\n","\\n"),engine.current_position());
    engine.skip_whitespace()?;
    let mut quoted = false;
    while let Some(next) = engine.get_next_unexpandable()? {
        match next.command {
            BaseCommand::Char {catcode:CategoryCode::Space,..} if quoted => ret.push(' '),
            BaseCommand::Char {catcode:CategoryCode::Space,..} => return Ok(()),
            BaseCommand::Char {char,..} if char.to_usize() == 34 => { // "
                quoted = !quoted;
            }
            BaseCommand::Char {char,..} => ret.push(char.as_char()), //(char.as_bytes())),//ret.push(char.to_char()),
            _ => {
                engine.mouth.requeue(next.source.cause,engine.memory);
                return Ok(())
            }
        }
    }
    Ok(())
}

pub fn get_braced_string<ET:EngineType>(engine:&mut EngineMut<ET>,ret:&mut String) -> Result<(),TeXError<ET>> {
    engine.get_expanded_group(false,false,true,&mut |engine,t| {
        match &t.base {
            BaseToken::Char(c,_) => {
                ret.push(c.as_char())
            }
            BaseToken::CS(name) => {
                if let Some(c) = engine.state.get_escapechar() { ret.push(c.as_char()) }
                let str = name.to_str(engine.memory);
                ret.push_str(str);
                if str.len() > 1 || *engine.state.get_catcode_scheme().get(&ET::Char::tokenize(str)[0]) != CategoryCode::Letter {
                    ret.push(' ')
                }
            }
        }
        Ok(())
    })?;
    Ok(())
}

pub fn tokens_to_string<ET:EngineType>(engine:&mut EngineMut<ET>,v:&Vec<Token<ET>>,string:&mut String) {
    let cc = engine.state.get_catcode_scheme();
    match engine.state.get_escapechar() {
        None => for t in v {
                match &t.base {
                    BaseToken::Char(c,CategoryCode::Space) => string.push(' '),
                    BaseToken::Char(c,_) => string.push(c.as_char()),
                    BaseToken::CS(str) => {
                        let str = str.to_str(engine.memory);
                        string.push_str(str);
                        if str.len() != 1 || *cc.get(&ET::Char::tokenize(str)[0]) != CategoryCode::Letter {
                            string.push(' ');
                        }
                    }
                }
            }
        Some(escapechar) => {
            let esc = escapechar.as_char();
            for t in v {
                match &t.base {
                    BaseToken::Char(c,CategoryCode::Space) => string.push(' '),
                    BaseToken::Char(c,_) => string.push(c.as_char()),
                    BaseToken::CS(str) => {
                        string.push(esc);
                        let str = str.to_str(engine.memory);
                        string.push_str(str);
                        if str.len() != 1 || *cc.get(&ET::Char::tokenize(str)[0]) != CategoryCode::Letter {
                            string.push(' ');
                        }
                    }
                }
            }
        }
    }
}

pub fn get_control_sequence<ET:EngineType>(engine:&mut EngineMut<ET>) -> Result<Token<ET>,TeXError<ET>> {
    engine.skip_whitespace()?;
    while let Some((next,e)) = engine.get_next_token()? {
        let resolved = resolve_token::<ET>(engine.state,next);
        match resolved.command {
            BaseCommand::Char{char,catcode:CategoryCode::Active} =>
                match get_cs_check_command::<ET>(engine, resolved)? {
                    None => (),
                    Some(t) => return Ok(t),
                }
            BaseCommand::Char{..} => {
                todo!()
                // error
            }
            _ => match get_cs_check_command::<ET>(engine, resolved)? {
                None => (),
                Some(t) => return Ok(t),
            }
        }
    }
    file_end!()
}

fn get_cs_check_command<ET:EngineType>(engine:&mut EngineMut<ET>, resolved:ResolvedToken<ET>)
                                       -> Result<Option<Token<ET>>,TeXError<ET>> {
    match resolved.command {
        BaseCommand::Expandable {..} | BaseCommand::Conditional { .. } => {
            engine.expand(resolved)?;
            Ok(None)
        },
        _ => Ok(Some(resolved.source.cause))
    }
}

pub fn get_font<ET:EngineType>(engine:&mut EngineMut<ET>) -> Result<ET::Font,TeXError<ET>> {
    match engine.get_next_unexpandable()? {
        None => file_end!(),
        Some(res) => match res.command {
            BaseCommand::Font(f) => Ok(f),
            BaseCommand::FontCommand {get,..} => {
                get(engine,res.source)
            }
            _ => throw!("Expected a font, got {:?}",res.command)
        }
    }
}


impl<ET:EngineType> EngineMut<'_,ET> {

    /// Expands [`Token`]s for as long as possible and returns the [`ResolvedToken`] for the next unexpandable [`Token`] encountered
    /// (or [`None`] if the [`Mouth`] is empty)
    pub fn get_next_unexpandable(&mut self) -> Result<Option<ResolvedToken<ET>>,TeXError<ET>> {
        ET::Gullet::get_next_unexpandable(self)
    }

    /// Returns the next primitive to be processed by the [`Stomach`](crate::engine::stomach::Stomach) from
    /// the input stream, after expanding macros as necessary.
    pub fn get_next_stomach_command(&mut self) -> Result<Option<StomachCommand<ET>>,TeXError<ET>> {
        ET::Gullet::get_next_stomach_command(self)
    }

    /// read a single keyword from the input stream; returns `true` if the keyword is found.
    pub fn get_keyword<'a>(&mut self, keyword:&'a str) -> Result<bool,TeXError<ET>> {
        ET::Gullet::get_keyword(self, keyword)
    }

    /// reads one of several optional keywords from the input stream;
    /// returns `None` if none of the keywords are found.
    pub fn get_keywords<'a>(&mut self, mut keywords:Vec<&'a str>) -> Result<Option<&'a str>,TeXError<ET>> {
        ET::Gullet::get_keywords(self, keywords)
    }

    /// Reads a number from the input stream.
    pub fn get_int(&mut self) -> Result<ET::Int,TeXError<ET>> {
        ET::Gullet::get_int(self)
    }

    /// Reads a dimension from the input stream.
    pub fn get_dim(&mut self) -> Result<ET::Dim,TeXError<ET>> {
        ET::Gullet::get_dim(self)
    }

    /// Reads a skip from the input stream.
    pub fn get_skip(&mut self) -> Result<Skip<ET::SkipDim>,TeXError<ET>> {
        ET::Gullet::get_skip(self)
    }

    /// Reads a muskip from the input stream.
    pub fn get_muskip(&mut self) -> Result<MuSkip<ET::MuDim,ET::MuStretchShrinkDim>,TeXError<ET>> {
        ET::Gullet::get_muskip(self)
    }

    pub fn get_expanded_group(&mut self, expand_protected:bool, keep_the:bool, err_on_unknowns:bool, f: TokenCont<ET>) -> Result<(),TeXError<ET>> {
        get_expanded_group::<ET>(self, expand_protected, keep_the, err_on_unknowns,f)
    }

    pub fn get_control_sequence(&mut self) -> Result<Token<ET>,TeXError<ET>> {
        get_control_sequence::<ET>(self)
    }

    pub fn get_char(&mut self) -> Result<ET::Char,TeXError<ET>> {
        let num = self.get_int()?;
        match ET::Char::from_i64(num.to_i64()) {
            Some(i) => Ok(i),
            None => throw!("Not a valid character: {}",num)
        }
    }

    pub fn get_string(&mut self,string:&mut String) -> Result<(),TeXError<ET>> {
        ET::Gullet::get_string(self,string)
    }

    pub fn get_braced_string(&mut self,string:&mut String) -> Result<(),TeXError<ET>> {
        get_braced_string::<ET>(self,string)
    }

    pub fn get_font(&mut self) -> Result<ET::Font,TeXError<ET>> {
        ET::Gullet::get_font(self)
    }

    pub fn is_next_char_one_of(&mut self,chars:&'static [u8]) -> Result<Option<u8>,TeXError<ET>> {
        match self.get_next_unexpandable()? {
            None => file_end!(),
            Some(res) => match res.command {
                BaseCommand::Char {char,..} if (char.as_char() as u32) < 256 => {
                    let c = char.as_char() as u8;
                    if chars.contains(&c) {
                        Ok(Some(c))
                    } else {
                        self.mouth.requeue(res.source.cause,self.memory);
                        Ok(None)
                    }
                }
                _ => {
                    self.mouth.requeue(res.source.cause,self.memory);
                    Ok(None)
                }
            }
        }
    }

    pub fn is_next_char(&mut self,char:u8) -> Result<bool,TeXError<ET>> {
        match self.get_next_unexpandable()? {
            None => file_end!(),
            Some(res) => match res.command {
                BaseCommand::Char { char:c, .. } if c.as_bytes() == [char] => Ok(true),
                _ => {
                    self.mouth.requeue(res.source.cause,self.memory);
                    Ok(false)
                }
            }
        }
    }

    pub fn expand_until_group(&mut self,f: TokenCont<ET>) -> Result<(),TeXError<ET>> {
        match self.get_next_unexpandable()? {
            None => file_end!(),
            Some(res) => match res.command {
                BaseCommand::Char { catcode:CategoryCode::BeginGroup, .. } => {
                    ET::Mouth::get_until_endgroup(self,f)
                }
                _ => throw!("begin group expected")
            }
        }
    }

    pub fn string_to_tokens(&mut self,str:&[u8],f:TokenCont<ET>) -> Result<(),TeXError<ET>> {
        for u in str {
            let c = ET::Char::from(*u);
            f(self,Token::new(BaseToken::Char(c,if *u == 32 {CategoryCode::Space} else {CategoryCode::Other}),None))?;
        }
        Ok(())
    }

    pub fn token_to_others(&mut self,tk:&Token<ET>, insertspace:bool, mut f:TokenCont<ET>) -> Result<(),TeXError<ET>> {
        match &tk.base {
            BaseToken::Char(c,_) if c.to_usize() == 32 => f(self,Token::new(BaseToken::Char(*c,CategoryCode::Space),None)),
            BaseToken::Char(c,CategoryCode::Space) => f(self,Token::new(BaseToken::Char(*c,CategoryCode::Space),None)),
            BaseToken::Char(c,_) => f(self,Token::new(BaseToken::Char(*c,CategoryCode::Other),None)),
            BaseToken::CS(str) => {
                match self.state.get_escapechar() {
                    None => (),
                    Some(c) => f(self,Token::new(BaseToken::Char(c,CategoryCode::Other),None))?
                }
                let str = ET::Char::tokenize(str.to_str(self.memory)).to_vec();
                for c in &str {
                    f(self,Token::new(BaseToken::Char(*c, if c.to_usize() == 32 { CategoryCode::Space } else { CategoryCode::Other }
                    ),None))?
                }
                if insertspace && !(str.len() == 1 && *self.state.get_catcode_scheme().get(&str[0]) != CategoryCode::Letter) {
                    f(self,Token::new(BaseToken::Char(ET::Char::from(32),CategoryCode::Space),None))?
                }
                Ok(())
            }
        }
    }

    pub fn expand(&mut self,r:ResolvedToken<ET>) -> Result<Option<ResolvedToken<ET>>,TeXError<ET>> {
        ET::Gullet::expand(self,r)
    }
}