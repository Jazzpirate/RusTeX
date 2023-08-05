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
use crate::tex::commands::tex::IFCASE;
use crate::tex::ConditionalBranch;
use crate::tex::fonts::FontStore;
use crate::utils::errors::TeXError;
use crate::utils::strings::{CharType, TeXStr};
use crate::utils::strings::AllCharsTrait;


/// Expands [`Token`]s for as long as possible and returns the [`ResolvedToken`] for the next unexpandable [`Token`] encountered
/// (or [`None`] if the [`Mouth`] is empty)
pub fn get_next_unexpandable<ET:EngineType>(gullet:&mut ET::Gullet,state:&mut ET::State) -> Result<Option<ResolvedToken<ET>>,TeXError<ET::Token>> {
    while let Some((next,e)) = gullet.mouth().get_next::<ET>(state)? {
        if !e {return Ok(Some(ResolvedToken{command:BaseCommand::None,source:CommandSource{cause:next,reference:None},expand:false}))}
        let mut ret: Vec<ET::Token> = vec!();
        match expand(gullet,state,next,&mut |_,t| Ok(ret.push(t)))? {
            Some(r) => return Ok(Some(r)),
            _ => gullet.mouth().push_tokens(ret)
        }
    }
    Ok(None)
}

/// Expands the given [`Token`], if expandable, by calling `f` on every element of its expansion and returns [`None`].
/// If not expandable, returns the [`ResolvedToken`] for `tk`
pub fn expand<ET:EngineType>(gullet:&mut ET::Gullet,state:&mut ET::State,tk:ET::Token,f:TokenCont<ET>) -> Result<Option<ResolvedToken<ET>>,TeXError<ET::Token>> {
    let ret = resolve_token::<ET>(state,tk);
    match ret.command {
        BaseCommand::Def(d) => todo!(),// d.expand(state,gullet.mouth(),ret),
        BaseCommand::Expandable {apply,..} => {
            apply(state,gullet,ret.source,f)?;
            Ok(None)
        },
        BaseCommand::Conditional {name,apply} => {
            do_conditional(gullet, state,ret.source, name,apply, false)?;
            Ok(None)
        }
        _ => Ok(Some(ret))
    }
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

/*
pub fn process_cmd_for_stomach<ET:EngineType>(gullet:&mut ET::Gullet, state: &mut ET::State, cause:ET::Token, cmd:Ptr<BaseCommand<ET::Token>>) -> Result<Option<StomachCommand<ET::Token>>,TeXError<ET::Token>> {
match &*cmd {
    BaseCommand::Relax => Ok(Some(StomachCommand{cause,cmd:StomachCommandInner::Relax})),
    BaseCommand::Def(ref def, _) => {
        let v = def.expand::<ET>(state,gullet.mouth(),cmd.clone(),Ptr::new(cause.clone()))?;
        if !v.is_empty() {
            gullet.mouth().push_tokens(v);
        }
        Ok(None)
    }
    BaseCommand::Conditional{name,index} => {
        do_conditional::<ET>(gullet,state,cause,name,*index,false)?;
        Ok(None)
    },
    BaseCommand::Expandable {name,index} => {
        do_expandable::<ET>(gullet,state,cause,name,*index)?;
        Ok(None)
    }
    BaseCommand::MathChar(i) => Ok(Some(StomachCommand{cause,cmd:StomachCommandInner::MathChar(*i)})),
    BaseCommand::Whatsit {name,index} => Ok(Some(StomachCommand{cause,cmd:StomachCommandInner::Whatsit{name,index:*index}})),
    BaseCommand::Value{name,index,tp,..} => Ok(Some(StomachCommand{cause,cmd:StomachCommandInner::Value{name,tp:*tp,index:*index}})),
    BaseCommand::ValueRegister{index,tp} => Ok(Some(StomachCommand{cause,cmd:StomachCommandInner::ValueRegister(*index, *tp)})),
    BaseCommand::ValueAssignment {name,assignment_index,value_index,tp} =>
        Ok(Some(StomachCommand{cause,cmd:StomachCommandInner::ValueAssignment {name,assignment_index:*assignment_index,value_index:*value_index,tp:*tp}})),
    BaseCommand::AssignableValue {name,tp} => Ok(Some(StomachCommand{cause,cmd:StomachCommandInner::AssignableValue {name,tp:*tp}})),
    BaseCommand::Assignment{name,index} => Ok(Some(StomachCommand{cause,cmd:StomachCommandInner::Assignment{name,index:*index}})),
    BaseCommand::Unexpandable {name,index} => Ok(Some(StomachCommand{cause,cmd:StomachCommandInner::Command{name,index:*index}})),
    BaseCommand::OpenBox {name,index,mode} => Ok(Some(StomachCommand{cause,cmd:StomachCommandInner::OpenBox{name,index:*index,mode:*mode}})),
    BaseCommand::Char {char,catcode} =>
        Ok(Some(char_to_command(cause, *char, *catcode,true)))
}
}
 */

/*
pub fn char_to_command<T:Token>(cause:T, char:T::Char, catcode:CategoryCode,from_chardef:bool) -> StomachCommand<T> {
    use CategoryCode::*;
    StomachCommand{cause,cmd:match catcode {
        Superscript => StomachCommandInner::Superscript(char),
        Subscript => StomachCommandInner::Subscript(char),
        Space => StomachCommandInner::Space,
        MathShift => StomachCommandInner::MathShift(char),
        BeginGroup => StomachCommandInner::BeginGroup(char),
        EndGroup => StomachCommandInner::EndGroup(char),
        Letter|Other|AlignmentTab => StomachCommandInner::Char{char,from_chardef},
        EOF => StomachCommandInner::Relax,
        Parameter => StomachCommandInner::Parameter(char),
        _ => unreachable!() // Already excluded: Active, Ignored, EndLine
        // TODO: exclude: AlignmentTab proper, Parameter
    }}
}

 */

macro_rules! expand_group_without_unknowns {
    ($state:ident,$gullet:ident,$finish:expr,($tk:ident,$expand:ident,$cmd:ident) => $f:expr;$($branch:tt)*) => {
        if let Some((tk,b)) = $gullet.mouth().get_next::<ET>($state)? {
            match tk.catcode() {
                CategoryCode::BeginGroup => (),
                _ => return Err(ExpectedToken{expected:ET::Token::new(BaseToken::Char(ET::Char::from(b'{'),CategoryCode::BeginGroup),None),found:tk}.into())
            }
        }
        let mut depth = 1;
        while let Some(($tk,$expand)) = $gullet.mouth().get_next::<ET>($state)? {
            match $tk.catcode() {
                CategoryCode::BeginGroup => {
                    depth += 1;
                    $f;
                }
                CategoryCode::EndGroup => {
                    depth -= 1;
                    if depth == 0 { $finish }
                    if depth < 0 {
                        return Err(UnexpectedEndgroup($tk).into())
                    }
                    $f;
                },
                _ => {
                    match $tk.base() {
                        BaseToken::CS(n) => {
                            let $cmd = $state.need_command(n)?;
                            match &*$cmd {
                                $($branch)*
                                Command::Gullet {name,index} => {
                                    do_expandable::<ET>($gullet,$state,$tk,name,*index)?;
                                }
                                Command::Def(def,_) => {
                                    let v = def.expand::<ET>($state,$gullet.mouth(),$cmd.clone(),Ptr::new($tk))?;
                                    if !v.is_empty() {
                                        $gullet.mouth().push_tokens(v);
                                    }
                                }
                                Command::Conditional {name,index} => {
                                    do_conditional::<ET>($gullet,$state,$tk,name,*index,false)?;
                                }
                                _ => $f
                            }
                        }
                        BaseToken::Char(c, CategoryCode::Active) => {
                            let $cmd = $state.need_ac_command(*c)?;
                            match &*$cmd {
                                $($branch)*
                                Command::Gullet {name,index} => {
                                    do_expandable::<ET>($gullet,$state,$tk,name,*index)?;
                                }
                                Command::Def(def,_) => {
                                    let v = def.expand::<ET>($state,$gullet.mouth(),$cmd.clone(),Ptr::new($tk))?;
                                    if !v.is_empty() {
                                        $gullet.mouth().push_tokens(v);
                                    }
                                }
                                Command::Conditional {name,index} => {
                                    do_conditional::<ET>($gullet,$state,$tk,name,*index,false)?;
                                }
                                _ => $f
                            }
                        }
                        _ => $f
                    }
                }
            }
        }
        file_end!()
    }
}

macro_rules! expand_group_with_unknowns {
    ($state:ident,$gullet:ident,$finish:expr,($tk:ident,$expand:ident,$cmd:ident) => $f:expr;$($branch:tt)*) => {
        if let Some((tk,b)) = $gullet.mouth().get_next::<ET>($state)? {
            match tk.catcode() {
                CategoryCode::BeginGroup => (),
                _ => return Err(ExpectedToken{expected:ET::Token::new(BaseToken::Char(ET::Char::from(b'{'),CategoryCode::BeginGroup),None),found:tk}.into())
            }
        }
        let mut depth = 1;
        while let Some(($tk,$expand)) = $gullet.mouth().get_next::<ET>($state)? {
            match $tk.catcode() {
                CategoryCode::BeginGroup => {
                    depth += 1;
                    $f;
                }
                CategoryCode::EndGroup => {
                    depth -= 1;
                    if depth == 0 { $finish }
                    if depth < 0 {
                        return Err(UnexpectedEndgroup($tk).into())
                    }
                    $f;
                },
                _ => {
                    match $tk.base() {
                        BaseToken::CS(n) => {
                            match $state.get_command(n) {
                                None => $f,
                                Some(_) if !$expand => $f,
                                Some($cmd) => match &*$cmd {
                                    $($branch)*,
                                    Command::Gullet {name,index} => {
                                        do_expandable::<ET>($gullet,$state,$tk,name,*index)?;
                                    }
                                    Command::Def(def,_) => {
                                        let v = def.expand::<ET>($state,$gullet.mouth(),$cmd.clone(),Ptr::new($tk))?;
                                        if !v.is_empty() {
                                            $gullet.mouth().push_tokens(v);
                                        }
                                    }
                                    Command::Conditional {name,index} => {
                                        do_conditional::<ET>($gullet,$state,$tk,name,*index,false)?;
                                    }
                                    _ => $f
                                }
                            }
                        }
                        BaseToken::Char(c, CategoryCode::Active) => {
                            match $state.get_ac_command(*c) {
                                None => $f,
                                Some(_) if !$expand => $f,
                                Some($cmd) => match &*$cmd {
                                    $($branch)*,
                                    Command::Gullet {name,index} => {
                                        do_expandable::<ET>($gullet,$state,$tk,name,*index)?;
                                    }
                                    Command::Def(def,_) => {
                                        let v = def.expand::<ET>($state,$gullet.mouth(),$cmd.clone(),Ptr::new($tk))?;
                                        if !v.is_empty() {
                                            $gullet.mouth().push_tokens(v);
                                        }
                                    }
                                    Command::Conditional {name,index} => {
                                        do_conditional::<ET>($gullet,$state,$tk,name,*index,false)?;
                                    }
                                    _ => $f
                                }
                            }
                        }
                        _ => $f
                    }
                }
            }
        }
        file_end!()
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


pub fn get_braced_string<ET:EngineType>(gullet:&mut ET::Gullet, state: &mut ET::State) -> Result<Vec<u8>,TeXError<ET::Token>> {
    todo!()
    /*
    let mut ret = vec!();
    let esc = state.get_escapechar();
    expand_group_without_unknowns!(state,gullet,return Ok(ret),(tk,expand,cmd) =>
        for u in token_to_string(tk,esc,state.get_catcode_scheme()) {
            ret.push(u)
        };
        BaseCommand::Expandable {name:"unexpanded",index} if expand => {
                match gullet.primitive(*index) {
                    Some(f) => {
                        for t in f(state,gullet,GulletCommand{cause:tk.clone()})? {
                            for u in token_to_string(t,esc,state.get_catcode_scheme()) {
                                ret.push(u);
                            }
                        }
                    }
                    None => return Err(OtherError{msg:"\\unexpanded not implemented".to_string(),cause:Some(tk),source:None}.into())
                }
            }
        BaseCommand::Expandable {name:"noexpand",..} if expand => {
            match gullet.mouth().get_next::<ET>(state)? {
                Some((tk,_)) => for u in token_to_string(tk,esc,state.get_catcode_scheme()) {
                        ret.push(u)
                    },
                None => return Err(UnexpectedEndgroup(tk).into())
            }
        }
        BaseCommand::Def(def,_) if def.protected || !expand => {
            for u in token_to_string(tk,esc,state.get_catcode_scheme()) {
                ret.push(u)
            }
        }
    );

     */
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

pub fn get_expanded_group<ET:EngineType>(gullet:&mut ET::Gullet, state: &mut ET::State, expand_protected:bool, keep_the:bool, err_on_unknowns:bool, f: TokenCont<ET>) -> Result<(),TeXError<ET::Token>> {
    match gullet.mouth().get_next::<ET>(state)? {
        Some((t,_)) if t.catcode() == CategoryCode::BeginGroup => (),
        _ => throw!("begin group expected")
    }
    let mut ingroup = 0;
    while let Some(next) = gullet.mouth().get_next::<ET>(state)? {
        let mut res = resolve_token::<ET>(state,next.0);
        res.expand = next.1;
        match res.command {
            BaseCommand::Char{catcode:CategoryCode::BeginGroup,..} => {ingroup += 1;f(state,res.source.cause)?}
            BaseCommand::Char{catcode:CategoryCode::EndGroup,..} => {
                if ingroup == 0 { return Ok(()) } else { ingroup -= 1; }
                f(state,res.source.cause)?
            }
            BaseCommand::Def(d) if res.expand && (!d.protected || expand_protected) => {
                d.expand(state,gullet.mouth(),res.source,f)?
            }
            BaseCommand::Conditional {name,apply} if res.expand => {
                do_conditional(gullet,state,res.source,name,apply,false)?
            }
            BaseCommand::Expandable {name:"noexpand",..} if res.expand => (),
            BaseCommand::Expandable {name:"unexpanded",apply} if res.expand => {
                apply(state,gullet,res.source,f)?
            }
            BaseCommand::Expandable {name:THE,apply} if res.expand && keep_the =>
                apply(state,gullet,res.source,f)?,
            BaseCommand::Expandable {..} if res.expand => todo!(),
            BaseCommand::None if err_on_unknowns => return Err(match res.source.cause.base() {
                BaseToken::Char(c,_) => throw!("Undefined active character {}",c),
                BaseToken::CS(name) => throw!("Undefined control sequence {}",name),
            }),
            _ => f(state,res.source.cause)?
        }
    }
    file_end!()
    /*
    let mut tks = vec!();
    if err_on_unknowns {
        expand_group_without_unknowns!(state,gullet,return Ok(tks),(tk,expand,cmd) => tks.push(tk);
            BaseCommand::Expandable {name:THE,..} if keep_the => todo!("'the' in expansion"),
            BaseCommand::Expandable {name:"unexpanded",index} if expand => {
                match gullet.primitive(*index) {
                    Some(f) => {
                        for t in f(state,gullet,GulletCommand{cause:tk.clone()})? {
                            /*if t.catcode() == CategoryCode::Parameter {
                                tks.push(t.clone());
                                tks.push(t);
                            } else {*/
                                tks.push(t);
                            //}
                        }
                    }
                    None => return Err(OtherError{msg:"\\unexpanded not implemented".to_string(),cause:Some(tk),source:None}.into())
                }
            }
            BaseCommand::Expandable {name:"noexpand",..} if expand => {
                match gullet.mouth().get_next::<ET>(state)? {
                    Some((tk,_)) => tks.push(tk),
                    None => return Err(UnexpectedEndgroup(tk).into())
                }
            }
            BaseCommand::Def(def,_) if def.protected && !expand_protected => {tks.push(tk)}
        );
    } else {
        expand_group_with_unknowns!(state,gullet,return Ok(tks),(tk,expand,cmd) => tks.push(tk);
            BaseCommand::Expandable {name:THE,..} if keep_the => todo!("'the' in expansion"),
            BaseCommand::Expandable {name:"unexpanded",index} if expand => {
                match gullet.primitive(*index) {
                    Some(f) => {
                        for t in f(state,gullet,GulletCommand{cause:tk.clone()})? {
                            /*if t.catcode() == CategoryCode::Parameter {
                                tks.push(t.clone());
                                tks.push(t);
                            } else {*/
                                tks.push(t);
                            //}
                        }
                    }
                    None => return Err(OtherError{msg:"\\unexpanded not implemented".to_string(),cause:Some(tk),source:None}.into())
                }
            }
            BaseCommand::Expandable {name:"noexpand",..} if expand => {
                match gullet.mouth().get_next::<ET>(state)? {
                    Some((tk,_)) => tks.push(tk),
                    None => return Err(UnexpectedEndgroup(tk).into())
                }
            }
            BaseCommand::Def(def,_) if def.protected && !expand_protected => {tks.push(tk)}
        );
    }

     */
}

/*
pub fn process_token_for_stomach<ET:EngineType>(gullet:&mut ET::Gullet, token:ET::Token, state: &mut ET::State) -> Result<Option<StomachCommand<ET::Token>>,TeXError<ET::Token>> {
    match token.base() {
        BaseToken::CS(n) => {
            let cmd = state.need_command(&n)?;
            process_cmd_for_stomach::<ET>(gullet, state, token, cmd)
        }
        BaseToken::Char(c, CategoryCode::Active) => {
            let cmd = state.need_ac_command(*c)?;
            process_cmd_for_stomach::<ET>(gullet, state, token, cmd)
        }
        BaseToken::Char(c, cat) => {
            let c = *c;
            let cat = *cat;
            Ok(Some(char_to_command(token, c, cat,false)))
        }
    }
}
*/

/*
pub fn do_expandable<ET:EngineType>(gullet:&mut ET::Gullet,state:&mut ET::State,cause:ET::Token,name:&str,index:usize) -> Result<(),TeXError<ET::Token>> {
    match gullet.primitive(index) {
        None => Err(ImplementationError(format!("Missing implementation for primitive command {}",name),PhantomData).into()),
        Some(f) => {
            let v = f(state,gullet,GulletCommand{cause})?;
            if !v.is_empty() {
                gullet.mouth().push_tokens(v);
            }
            Ok(())
        }
    }
}

 */

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
    let mut read_toks: Vec<ET::Token> = vec!();
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
    let mut read_toks: Vec<ET::Token> = vec!();
    while let Some(next) = gullet.get_next_unexpandable(state)? {
        read_toks.push(next.source.cause.clone());
        match next.command {
            BaseCommand::Char{char,..} => {
                let us = char.to_usize();
                if us < 256 {
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

pub fn token_to_chars<T:Token>(tk:T,escape:Option<T::Char>,f:&mut dyn FnMut(T) -> Result<(),TeXError<T>>) -> Result<(),TeXError<T>> {
    match tk.base() {
        BaseToken::Char(c,_) if c.to_usize() == 32 => f(T::new(BaseToken::Char(T::Char::from(32),CategoryCode::Space),None)),
        BaseToken::Char(c,_) => f(T::new(BaseToken::Char(T::Char::from(32),CategoryCode::Other),None)),
        BaseToken::CS(str) => {
            match escape {
                None => (),
                Some(c) => f(T::new(BaseToken::Char(c,CategoryCode::Other),None))?
            }
            for c in str.as_vec() {
                f(T::new(BaseToken::Char(T::Char::from(32),
                                         if c.to_usize() == 32 { CategoryCode::Space } else { CategoryCode::Other }
                ),None))?
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

pub fn string_to_tokens<T:Token>(str:&[u8]) -> Vec<T> {
    let mut ret = vec!();
    for u in str {
        let c = T::Char::from(*u);
        ret.push(T::new(BaseToken::Char(c,if *u == 32 {CategoryCode::Space} else {CategoryCode::Other}),None));
    }
    ret
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
        BaseCommand::Expandable {apply,..} => {
            let mut ret = vec!();
            apply(state,gullet,resolved.source,&mut |_,t| Ok(ret.push(t)))?;
            gullet.mouth().push_tokens(ret);
            Ok(None)
        },
        BaseCommand::Conditional {name,apply} => {
            do_conditional(gullet, state, resolved.source,name,apply, false)?;
            Ok(None)
        }
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
    todo!()
    /*
    match gullet.get_next_stomach_command(state)? {
        Some(cmd) => match cmd.cmd {
            StomachCommandInner::ValueRegister(i,Assignable::Font) =>
                Ok(i),
            StomachCommandInner::ValueAssignment {name:"font",..} =>
                Ok(state.get_current_font()),
            o => todo!("get_font: {:?}",o),
        }
        None => file_end!(),
    }

     */
}