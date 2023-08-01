//! Default implementations for [`Gullet`] methods.
use std::marker::PhantomData;
use crate::{debug_log, file_end};
use crate::engine::EngineType;
use crate::engine::gullet::Gullet;
use crate::engine::state::State;
use crate::tex::catcodes::{CategoryCode, CategoryCodeScheme};
use crate::tex::commands::{Assignable, Command, GulletCommand, StomachCommand, StomachCommandInner};
use crate::tex::numbers::Int;
use crate::tex::token::{BaseToken, Token};
use crate::utils::errors::{ExpectedToken, ImplementationError, OtherError, TeXError, UnexpectedEndgroup};
use crate::utils::Ptr;
use crate::engine::mouth::Mouth;
use crate::tex::ConditionalBranch;
use crate::tex::fonts::FontStore;
use crate::utils::strings::{CharType, TeXStr};
use crate::utils::strings::AllCharsTrait;

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

pub fn get_string<ET:EngineType>(gullet:&mut ET::Gullet, state: &mut ET::State) -> Result<TeXStr<ET::Char>,Box<dyn TeXError<ET::Token>>> {
    debug_log!(trace=>"Reading string {}...\n at {}",gullet.mouth().preview(50).replace("\n","\\n"),gullet.mouth().file_line());
    let mut ret: Vec<ET::Char> = Vec::with_capacity(50); // seems to speed things up
    gullet.mouth().skip_whitespace::<ET>(state)?;
    let mut quoted = false;
    while let Some((tk,_)) = gullet.mouth().get_next::<ET>(state)? {
        match tk.base() {
            BaseToken::Char(c,CategoryCode::Active) => {
                let cmd = state.get_ac_command(*c);
                match cmd.as_deref() {
                    Some(Command::Gullet {name,index}) =>
                        do_expandable::<ET>(gullet,state,tk,name,*index)?,
                    Some(Command::Def(def,_)) => {
                        let v = def.expand::<ET>(state,gullet.mouth(),unsafe {cmd.clone().unwrap_unchecked()},Ptr::new(tk))?;
                        if !v.is_empty() {
                            gullet.mouth().push_tokens(v);
                        }
                    }
                    Some(Command::Conditional {name,index}) => {
                        do_conditional::<ET>(gullet,state,tk,name,*index,false)?;
                    }
                    _ => //if state.get_escapechar() == T::Char::MAX =>
                        ret.push(*c),
                }
            }
            BaseToken::CS(name) => {
                let cmd = state.get_command(name);
                match cmd.as_deref() {
                    Some(Command::Gullet {name,index}) =>
                        do_expandable::<ET>(gullet,state,tk,name,*index)?,
                    Some(Command::Def(def,_)) => {
                        let v = def.expand::<ET>(state,gullet.mouth(),unsafe {cmd.clone().unwrap_unchecked()},Ptr::new(tk))?;
                        if !v.is_empty() {
                            gullet.mouth().push_tokens(v);
                        }
                    }
                    Some(Command::Conditional {name,index}) => {
                        do_conditional::<ET>(gullet,state,tk,name,*index,false)?;
                    }
                    Some(Command::Relax) if !quoted => return Ok(ret.into()),
                    _ => match state.get_escapechar() {
                        None => ret.extend(name.as_vec()),
                        Some(esc) => {
                            ret.push(esc);
                            ret.extend(name.as_vec());
                        }
                    }
                }
            }
            BaseToken::Char(_,CategoryCode::Space) if quoted =>
                ret.push(ET::Char::from(32)), // space
            BaseToken::Char(_,CategoryCode::Space) =>
                return Ok(ret.into()),
            BaseToken::Char(c,_) if c.to_usize() == 34 => { // "
                quoted = !quoted;
            }
            BaseToken::Char(c,_) => ret.push(*c)
        }
    }
    Ok(ret.into())
}


pub fn get_braced_string<ET:EngineType>(gullet:&mut ET::Gullet, state: &mut ET::State) -> Result<Vec<u8>,Box<dyn TeXError<ET::Token>>> {
    let mut ret = Vec::with_capacity(50); // seems to speed things up
    let esc = state.get_escapechar();
    expand_group_without_unknowns!(state,gullet,return Ok(ret),(tk,expand,cmd) =>
        for u in token_to_string(tk,esc,state.get_catcode_scheme()) {
            ret.push(u)
        };
        Command::Gullet {name:"unexpanded",index} if expand => {
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
        Command::Gullet {name:"noexpand",..} if expand => {
            match gullet.mouth().get_next::<ET>(state)? {
                Some((tk,_)) => for u in token_to_string(tk,esc,state.get_catcode_scheme()) {
                        ret.push(u)
                    },
                None => return Err(UnexpectedEndgroup(tk).into())
            }
        }
        Command::Def(def,_) if def.protected || !expand => {
            for u in token_to_string(tk,esc,state.get_catcode_scheme()) {
                ret.push(u)
            }
        }
    );
}

pub fn get_expanded_group<ET:EngineType>(gullet:&mut ET::Gullet, state: &mut ET::State, expand_protected:bool, keep_the:bool, err_on_unknowns:bool) -> Result<Vec<ET::Token>,Box<dyn TeXError<ET::Token>>> {
    let mut tks = Vec::with_capacity(50); // seems to speed things up
    if err_on_unknowns {
        expand_group_without_unknowns!(state,gullet,return Ok(tks),(tk,expand,cmd) => tks.push(tk);
            Command::Gullet {name:"the",..} if keep_the => todo!("'the' in expansion"),
            Command::Gullet {name:"unexpanded",index} if expand => {
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
            Command::Gullet {name:"noexpand",..} if expand => {
                match gullet.mouth().get_next::<ET>(state)? {
                    Some((tk,_)) => tks.push(tk),
                    None => return Err(UnexpectedEndgroup(tk).into())
                }
            }
            Command::Def(def,_) if def.protected && !expand_protected => {tks.push(tk)}
        );
    } else {
        expand_group_with_unknowns!(state,gullet,return Ok(tks),(tk,expand,cmd) => tks.push(tk);
            Command::Gullet {name:"the",..} if keep_the => todo!("'the' in expansion"),
            Command::Gullet {name:"unexpanded",index} if expand => {
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
            Command::Gullet {name:"noexpand",..} if expand => {
                match gullet.mouth().get_next::<ET>(state)? {
                    Some((tk,_)) => tks.push(tk),
                    None => return Err(UnexpectedEndgroup(tk).into())
                }
            }
            Command::Def(def,_) if def.protected && !expand_protected => {tks.push(tk)}
        );
    }
}

pub fn process_token_for_stomach<ET:EngineType>(gullet:&mut ET::Gullet, token:ET::Token, state: &mut ET::State) -> Result<Option<StomachCommand<ET::Token>>,Box<dyn TeXError<ET::Token>>> {
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

pub fn process_cmd_for_stomach<ET:EngineType>(gullet:&mut ET::Gullet, state: &mut ET::State, cause:ET::Token, cmd:Ptr<Command<ET::Token>>) -> Result<Option<StomachCommand<ET::Token>>,Box<dyn TeXError<ET::Token>>> {
    match &*cmd {
        Command::Relax => Ok(Some(StomachCommand{cause,cmd:StomachCommandInner::Relax})),
        Command::Def(ref def,_) => {
            let v = def.expand::<ET>(state,gullet.mouth(),cmd.clone(),Ptr::new(cause.clone()))?;
            if !v.is_empty() {
                gullet.mouth().push_tokens(v);
            }
            Ok(None)
        }
        Command::Conditional{name,index} => {
            do_conditional::<ET>(gullet,state,cause,name,*index,false)?;
            Ok(None)
        },
        Command::Gullet {name,index} => {
            do_expandable::<ET>(gullet,state,cause,name,*index)?;
            Ok(None)
        }
        Command::MathChar(i) => Ok(Some(StomachCommand{cause,cmd:StomachCommandInner::MathChar(*i)})),
        Command::Whatsit {name,index} => Ok(Some(StomachCommand{cause,cmd:StomachCommandInner::Whatsit{name,index:*index}})),
        Command::Value{name,index,tp,..} => Ok(Some(StomachCommand{cause,cmd:StomachCommandInner::Value{name,tp:*tp,index:*index}})),
        Command::ValueRegister{index,tp} => Ok(Some(StomachCommand{cause,cmd:StomachCommandInner::ValueRegister(*index,*tp)})),
        Command::ValueAssignment {name,assignment_index,value_index,tp} =>
            Ok(Some(StomachCommand{cause,cmd:StomachCommandInner::ValueAssignment {name,assignment_index:*assignment_index,value_index:*value_index,tp:*tp}})),
        Command::AssignableValue {name,tp} => Ok(Some(StomachCommand{cause,cmd:StomachCommandInner::AssignableValue {name,tp:*tp}})),
        Command::Assignment{name,index} => Ok(Some(StomachCommand{cause,cmd:StomachCommandInner::Assignment{name,index:*index}})),
        Command::Stomach {name,index} => Ok(Some(StomachCommand{cause,cmd:StomachCommandInner::Command{name,index:*index}})),
        Command::OpenBox {name,index,mode} => Ok(Some(StomachCommand{cause,cmd:StomachCommandInner::OpenBox{name,index:*index,mode:*mode}})),
        Command::Char {char,catcode} =>
            Ok(Some(char_to_command(cause, *char, *catcode,true)))
    }
}

pub fn do_expandable<ET:EngineType>(gullet:&mut ET::Gullet,state:&mut ET::State,cause:ET::Token,name:&str,index:usize) -> Result<(),Box<dyn TeXError<ET::Token>>> {
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

pub fn do_conditional<ET:EngineType>(gullet:&mut ET::Gullet,state:&mut ET::State,cause:ET::Token,name:&'static str,index:usize,unless:bool) -> Result<(),Box<dyn TeXError<ET::Token>>> {
    if name == "ifcase" {
        debug_log!(trace=>"ifcase");
        let i = gullet.new_conditional("ifcase");
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
        match gullet.conditional(index) {
            None => Err(ImplementationError(format!("Missing implementation for primitive command {}", name), PhantomData).into()),
            Some(c) => {
                let i = gullet.new_conditional(name);
                let b = c(state, gullet, GulletCommand { cause })?;
                if (b && !unless) || (!b && unless) {
                    gullet.set_conditional(i,ConditionalBranch::True(name));
                    debug_log!(trace=>"True conditional");
                    Ok(())
                } else { false_loop::<ET>(gullet, state,name,i) }
            }
        }
    }
}

pub fn false_loop<ET:EngineType>(gullet:&mut ET::Gullet,state:&mut ET::State,condname:&'static str,condidx:usize) -> Result<(),Box<dyn TeXError<ET::Token>>> {
    debug_log!(trace=>"False conditional. Skipping...");
    let mut incond:usize = gullet.current_conditional().1 - condidx;
    for i in 0..incond {
        gullet.pop_conditional();
    }
    while let Some((next,exp)) = gullet.mouth().get_next::<ET>(state)? {
        match next.base() {
            BaseToken::Char(c,CategoryCode::Active) =>
                match state.get_ac_command(*c).as_deref() {
                    Some(Command::Conditional {..}) => incond += 1,
                    Some(Command::Gullet {name:"else",..}) if incond == 0 => {
                        debug_log!(trace=>"...else branch.");
                        gullet.set_top_conditional(ConditionalBranch::Else(condname));
                        return Ok(())
                    }
                    Some(Command::Gullet {name:"or",..}) if incond == 0 && condname == "ifcase" => {
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
                    Some(Command::Gullet {name:"fi",..}) if incond > 0 => incond -= 1,
                    Some(Command::Gullet {name:"fi",..}) => {
                        debug_log!(trace=>"...end of conditional.");
                        gullet.pop_conditional();
                        return Ok(())
                    }
                    _ => ()
                }
            BaseToken::CS(name) => {
                match state.get_command(name).as_deref() {
                    Some(Command::Conditional {..}) => incond += 1,
                    Some(Command::Gullet {name:"else",..}) if incond == 0 => {
                        gullet.set_top_conditional(ConditionalBranch::Else(condname));
                        debug_log!(trace=>"...else branch.");
                        return Ok(())
                    }
                    Some(Command::Gullet {name:"or",..}) if incond == 0 && condname == "ifcase" => {
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
                    Some(Command::Gullet {name:"fi",..}) if incond > 0 => incond -= 1,
                    Some(Command::Gullet {name:"fi",..}) => {
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

pub fn else_loop<ET:EngineType>(gullet:&mut ET::Gullet,state:&mut ET::State,condname:&'static str,condidx:usize,allowelse:bool) -> Result<(),Box<dyn TeXError<ET::Token>>> {
    debug_log!(trace=>"\\else. Skipping...");
    gullet.set_top_conditional(ConditionalBranch::Else(condname));
    let mut incond:usize = gullet.current_conditional().1 - condidx;
    while let Some((next,exp)) = gullet.mouth().get_next::<ET>(state)? {
        match next.base() {
            BaseToken::Char(c,CategoryCode::Active) =>
                match state.get_ac_command(*c).as_deref() {
                    Some(Command::Conditional {..}) => incond += 1,
                    Some(Command::Gullet {name:"else",..}) if incond == 0 && !allowelse => {
                        return Err(OtherError{msg:"Unexpected \\else".into(),source:None,cause:None}.into())
                    }
                    Some(Command::Gullet {name:"fi",..}) if incond > 0 => incond -= 1,
                    Some(Command::Gullet {name:"fi",..}) => {
                        debug_log!(trace=>"...end of conditional.");
                        gullet.pop_conditional();
                        return Ok(())
                    }
                    _ => ()
                }
            BaseToken::CS(name) => {
                match state.get_command(name).as_deref() {
                    Some(Command::Conditional {..}) => incond += 1,
                    Some(Command::Gullet {name:"else",..}) if incond == 0 && !allowelse => {
                        return Err(OtherError{msg:"Unexpected \\else".into(),source:None,cause:None}.into())
                    }
                    Some(Command::Gullet {name:"fi",..}) if incond > 0 => incond -= 1,
                    Some(Command::Gullet {name:"fi",..}) => {
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

pub fn get_keyword<'a,ET:EngineType>(gullet:&mut ET::Gullet, state:&mut ET::State, kw:&'a str) -> Result<bool,Box<dyn TeXError<ET::Token>>> {
    debug_log!(trace=>"Reading keyword {:?}: {}...\n at {}",kw,gullet.mouth().preview(50).replace("\n","\\n"),gullet.mouth().file_line());
    let mut current = String::new();
    let mut read_toks: Vec<ET::Token> = vec!();
    while let Some(next) = gullet.get_next_stomach_command(state)? {
        read_toks.push(next.cause.clone());
        match next.cmd {
            StomachCommandInner::Char{char,from_chardef:false} => {
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

pub fn get_keywords<'a,ET:EngineType>(gullet:&mut ET::Gullet, state:&mut ET::State, mut keywords:Vec<&'a str>) -> Result<Option<&'a str>,Box<dyn TeXError<ET::Token>>> {
    debug_log!(trace=>"Reading keywords {:?}: {}...\n at {}",keywords,gullet.mouth().preview(50).replace("\n","\\n"),gullet.mouth().file_line());
    let mut current = String::new();
    let mut read_toks: Vec<ET::Token> = vec!();
    while let Some(next) = gullet.get_next_stomach_command(state)? {
        read_toks.push(next.cause.clone());
        match next.cmd {
            StomachCommandInner::Char{char,from_chardef:false} => {
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
                    gullet.mouth().requeue(next.cause);
                    keywords = keywords.into_iter().filter(|s| s == &current).collect();
                    return Ok(Some(keywords[0]))
                } else {
                    gullet.mouth().push_tokens(read_toks);
                    return Ok(None)
                }
            }
            _ if keywords.contains(&current.as_str()) => {
                gullet.mouth().requeue(next.cause);
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

pub fn token_to_string<T:Token>(tk:T,escapechar:Option<T::Char>,cc:&CategoryCodeScheme<T::Char>) -> Vec<u8> {
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
                    if str.as_vec().len() != 1 || *cc.get(str.as_vec()[0]) != CategoryCode::Letter {
                        s.push(b' ');
                    }
                    s
                }
            }
    }
}

pub fn tokens_to_string<T:Token>(v:Vec<T>,escapechar:Option<T::Char>,cc:&CategoryCodeScheme<T::Char>) -> String {
    let mut s = vec!();
    match escapechar {
        None => for t in v {
                match t.base() {
                    BaseToken::Char(c,CategoryCode::Space) => s.push(b' '),
                    BaseToken::Char(c,_) => for u in c.as_bytes() {s.push(u)},  //s.push_str(&c.char_str()),
                    BaseToken::CS(str) => {
                        for c in str.as_vec() {for u in c.as_bytes() {
                            s.push(u);
                        }}
                        if str.as_vec().len() != 1 || *cc.get(str.as_vec()[0]) != CategoryCode::Letter {
                            s.push(b' ');
                        }
                    }
                }
            }
        Some(escapechar) => for t in v {
                match t.base() {
                    BaseToken::Char(c,CategoryCode::Space) => s.push(b' '),
                    BaseToken::Char(c,_) => for u in c.as_bytes() {s.push(u)},
                    BaseToken::CS(str) => {
                        for u in escapechar.as_bytes() {s.push(u)};
                        for c in str.as_vec() {for u in c.as_bytes() {
                            s.push(u);
                        }}
                        if str.as_vec().len() != 1 || *cc.get(str.as_vec()[0]) != CategoryCode::Letter {
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


pub fn get_control_sequence<ET:EngineType>(gullet:&mut ET::Gullet, state:&mut ET::State) -> Result<BaseToken<ET::Char>,Box<dyn TeXError<ET::Token>>> {
    gullet.mouth().skip_whitespace::<ET>(state)?;
    while let Some((next,e)) = gullet.mouth().get_next::<ET>(state)? {
        match next.base() {
            BaseToken::Char(c,CategoryCode::Active) =>
                match get_cs_check_command::<ET>(gullet, state, state.get_ac_command(*c), next)? {
                    None => (),
                    Some(t) => return Ok(t),
                }
            BaseToken::Char(c,_) => {
                // error
            }
            BaseToken::CS(s) =>
                match get_cs_check_command::<ET>(gullet, state, state.get_command(s), next)? {
                    None => (),
                    Some(t) => return Ok(t),
                }
        }
    }
    file_end!()
}

fn get_cs_check_command<ET:EngineType>(gullet:&mut ET::Gullet, state:&mut ET::State, cmd:Option<Ptr<Command<ET::Token>>>, tk:ET::Token)
    -> Result<Option<BaseToken<ET::Char>>,Box<dyn TeXError<ET::Token>>> {
    match cmd.as_deref() {
        Some(Command::Conditional {name,index}) => {
            do_conditional::<ET>(gullet,state,tk,name,*index,false)?;
            Ok(None)
        }
        Some(Command::Gullet {name,index}) => {
            do_expandable::<ET>(gullet,state,tk,name,*index)?;
            Ok(None)
        }
        _ => Ok(Some(tk.base().clone()))
    }
}

pub fn get_font<ET:EngineType>(gullet: &mut ET::Gullet,state:&mut ET::State) -> Result<usize,Box<dyn TeXError<ET::Token>>> {
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
}