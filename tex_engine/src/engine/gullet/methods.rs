//! Default implementations for [`Gullet`] methods.
use std::marker::PhantomData;
use crate::{catch, debug_log, file_end};
use crate::engine::gullet::Gullet;
use crate::engine::state::State;
use crate::tex::catcodes::CategoryCode;
use crate::tex::commands::{Assignable, Command, GulletCommand, StomachCommand, StomachCommandInner};
use crate::tex::numbers::{Dim, Int, NumSet, Skip, SkipDim};
use crate::tex::token::{BaseToken, Token};
use crate::utils::errors::{ExpectedInteger, ExpectedToken, ExpectedUnit, ImplementationError, OtherError, TeXError, UnexpectedEndgroup};
use crate::utils::Ptr;
use crate::engine::mouth::Mouth;
use crate::tex::ConditionalBranch;
use crate::utils::strings::{CharType, TeXStr};

pub fn char_to_command<T:Token>(cause:T, char:T::Char, catcode:CategoryCode) -> StomachCommand<T> {
    use CategoryCode::*;
    StomachCommand{cause,cmd:match catcode {
        Superscript => StomachCommandInner::Superscript(char),
        Subscript => StomachCommandInner::Subscript(char),
        Space => StomachCommandInner::Space,
        MathShift => StomachCommandInner::MathShift(char),
        BeginGroup => StomachCommandInner::BeginGroup(char),
        EndGroup => StomachCommandInner::EndGroup(char),
        Letter|Other => StomachCommandInner::Char(char,false),
        EOF => StomachCommandInner::Relax,
        _ => unreachable!() // Already excluded: Active, Ignored, EndLine
        // TODO: exclude: AlignmentTab, Parameter, Invalid
    }}
}

macro_rules! expand_group_without_unknowns {
    ($state:ident,$gullet:ident,$finish:expr,($tk:ident,$expand:ident) => $f:expr;$($branch:tt)*) => {
        if let Some((tk,b)) = $gullet.mouth().get_next($state)? {
            match tk.catcode() {
                CategoryCode::BeginGroup => (),
                _ => return Err(ExpectedToken{expected:T::new(BaseToken::Char(T::Char::from(b'{'),CategoryCode::BeginGroup),None),found:tk}.into())
            }
        }
        let mut depth = 1;
        while let Some(($tk,$expand)) = $gullet.mouth().get_next($state)? {
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
                            let cmd = $state.need_command(n)?;
                            match &*cmd {
                                $($branch)*
                                Command::Gullet {name,index} => {
                                    do_expandable($gullet,$state,$tk,name,*index)?;
                                }
                                Command::Def(def,_) => {
                                    let v = def.expand($state,$gullet.mouth(),cmd.clone(),Ptr::new($tk))?;
                                    if !v.is_empty() {
                                        $gullet.mouth().push_tokens(v);
                                    }
                                }
                                Command::Conditional {name,index} => {
                                    do_conditional($gullet,$state,$tk,name,*index)?;
                                }
                                _ => $f
                            }
                        }
                        BaseToken::Char(c, CategoryCode::Active) => {
                            let cmd = $state.need_ac_command(*c)?;
                            match &*cmd {
                                $($branch)*
                                Command::Gullet {name,index} => {
                                    do_expandable($gullet,$state,$tk,name,*index)?;
                                }
                                Command::Def(def,_) => {
                                    let v = def.expand($state,$gullet.mouth(),cmd.clone(),Ptr::new($tk))?;
                                    if !v.is_empty() {
                                        $gullet.mouth().push_tokens(v);
                                    }
                                }
                                Command::Conditional {name,index} => {
                                    do_conditional($gullet,$state,$tk,name,*index)?;
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
    ($state:ident,$gullet:ident,$finish:expr,($tk:ident,$expand:ident) => $f:expr;$($branch:tt)*) => {
        if let Some((tk,b)) = $gullet.mouth().get_next($state)? {
            match tk.catcode() {
                CategoryCode::BeginGroup => (),
                _ => return Err(ExpectedToken{expected:T::new(BaseToken::Char(T::Char::from(b'{'),CategoryCode::BeginGroup),None),found:tk}.into())
            }
        }
        let mut depth = 1;
        while let Some(($tk,$expand)) = $gullet.mouth().get_next($state)? {
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
                                Some(cmd) => match &*cmd {
                                    $($branch)*,
                                    Command::Gullet {name,index} => {
                                        do_expandable($gullet,$state,$tk,name,*index)?;
                                    }
                                    Command::Def(def,_) => {
                                        let v = def.expand($state,$gullet.mouth(),cmd.clone(),Ptr::new($tk))?;
                                        if !v.is_empty() {
                                            $gullet.mouth().push_tokens(v);
                                        }
                                    }
                                    Command::Conditional {name,index} => {
                                        do_conditional($gullet,$state,$tk,name,*index)?;
                                    }
                                    _ => $f
                                }
                            }
                        }
                        BaseToken::Char(c, CategoryCode::Active) => {
                            match $state.get_ac_command(*c) {
                                None => $f,
                                Some(_) if !$expand => $f,
                                Some(cmd) => match &*cmd {
                                    $($branch)*,
                                    Command::Gullet {name,index} => {
                                        do_expandable($gullet,$state,$tk,name,*index)?;
                                    }
                                    Command::Def(def,_) => {
                                        let v = def.expand($state,$gullet.mouth(),cmd.clone(),Ptr::new($tk))?;
                                        if !v.is_empty() {
                                            $gullet.mouth().push_tokens(v);
                                        }
                                    }
                                    Command::Conditional {name,index} => {
                                        do_conditional($gullet,$state,$tk,name,*index)?;
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

pub fn get_string<T:Token,S:State<T>,Gu:Gullet<T,S=S>>(gullet:&mut Gu, state: &mut S) -> Result<TeXStr<T::Char>,Box<dyn TeXError<T>>> {
    debug_log!(trace=>"Reading string {}...\n at {}",gullet.mouth().preview(50).replace("\n","\\n"),gullet.mouth().file_line());
    let mut ret: Vec<T::Char> = Vec::with_capacity(50); // seems to speed things up
    gullet.mouth().skip_whitespace(state)?;
    let mut quoted = false;
    while let Some((tk,_)) = gullet.mouth().get_next(state)? {
        match tk.base() {
            BaseToken::Char(c,CategoryCode::Active) => {
                let cmd = state.get_ac_command(*c);
                match cmd.as_deref() {
                    Some(Command::Gullet {name,index}) =>
                        do_expandable(gullet,state,tk,name,*index)?,
                    Some(Command::Def(def,_)) => {
                        let v = def.expand(state,gullet.mouth(),unsafe {cmd.clone().unwrap_unchecked()},Ptr::new(tk))?;
                        if !v.is_empty() {
                            gullet.mouth().push_tokens(v);
                        }
                    }
                    Some(Command::Conditional {name,index}) => {
                        do_conditional(gullet,state,tk,name,*index)?;
                    }
                    _ => //if state.get_escapechar() == T::Char::MAX =>
                        ret.push(*c),
                }
            }
            BaseToken::CS(name) => {
                let cmd = state.get_command(name);
                match cmd.as_deref() {
                    Some(Command::Gullet {name,index}) =>
                        do_expandable(gullet,state,tk,name,*index)?,
                    Some(Command::Def(def,_)) => {
                        let v = def.expand(state,gullet.mouth(),unsafe {cmd.clone().unwrap_unchecked()},Ptr::new(tk))?;
                        if !v.is_empty() {
                            gullet.mouth().push_tokens(v);
                        }
                    }
                    Some(Command::Conditional {name,index}) => {
                        do_conditional(gullet,state,tk,name,*index)?;
                    }
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
                ret.push(T::Char::from(32)), // space
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


pub fn get_braced_string<T:Token,S:State<T>,Gu:Gullet<T,S=S>>(gullet:&mut Gu, state: &mut S) -> Result<Vec<u8>,Box<dyn TeXError<T>>> {
    let mut ret = Vec::with_capacity(50); // seems to speed things up
    let esc = state.get_escapechar();
    expand_group_without_unknowns!(state,gullet,return Ok(ret),(tk,expand) =>
        for u in token_to_string(tk,esc) {
            ret.push(u)
        };
        Command::Gullet {name:"unexpanded",..} => todo!("'unexpanded' in expansion"),
        Command::Gullet {name:"noexpand",..} => {
            match gullet.mouth().get_next(state)? {
                Some((tk,_)) => for u in token_to_string(tk,esc) {
                        ret.push(u)
                    },
                None => return Err(UnexpectedEndgroup(tk).into())
            }
        }
        Command::Def(def,_) if def.protected => {
            for u in token_to_string(tk,esc) {
                ret.push(u)
            }
        }
    );
}

pub fn get_expanded_group<T:Token,S:State<T>,Gu:Gullet<T,S=S>>(gullet:&mut Gu, state: &mut S, expand_protected:bool, keep_the:bool, err_on_unknowns:bool) -> Result<Vec<T>,Box<dyn TeXError<T>>> {
    let mut tks = Vec::with_capacity(50); // seems to speed things up
    if err_on_unknowns {
        expand_group_without_unknowns!(state,gullet,return Ok(tks),(tk,expand) => tks.push(tk);
            Command::Gullet {name:"the",..} if keep_the => todo!("'the' in expansion"),
            Command::Gullet {name:"unexpanded",..} => todo!("'unexpanded' in expansion"),
            Command::Gullet {name:"noexpand",..} => {
                match gullet.mouth().get_next(state)? {
                    Some((tk,_)) => tks.push(tk),
                    None => return Err(UnexpectedEndgroup(tk).into())
                }
            }
            Command::Def(def,_) if def.protected && !expand_protected => {tks.push(tk)}
        );
    } else {
        expand_group_with_unknowns!(state,gullet,return Ok(tks),(tk,expand) => tks.push(tk);
            Command::Gullet {name:"the",..} if keep_the => todo!("'the' in expansion"),
            Command::Gullet {name:"unexpanded",..} => todo!("'unexpanded' in expansion"),
            Command::Gullet {name:"noexpand",..} => {
                match gullet.mouth().get_next(state)? {
                    Some((tk,_)) => tks.push(tk),
                    None => return Err(UnexpectedEndgroup(tk).into())
                }
            }
            Command::Def(def,_) if def.protected && !expand_protected => {tks.push(tk)}
        );
    }
}

pub fn process_token_for_stomach<T:Token,S:State<T>,Gu:Gullet<T,S=S>>(gullet:&mut Gu, token:T, state: &mut S) -> Result<Option<StomachCommand<T>>,Box<dyn TeXError<T>>> {
    match token.base() {
        BaseToken::CS(n) => {
            let cmd = state.need_command(&n)?;
            process_cmd_for_stomach(gullet, state, token, cmd)
        }
        BaseToken::Char(c, CategoryCode::Active) => {
            let cmd = state.need_ac_command(*c)?;
            process_cmd_for_stomach(gullet, state, token, cmd)
        }
        BaseToken::Char(c, cat) => {
            let c = *c;
            let cat = *cat;
            Ok(Some(char_to_command(token, c, cat)))
        }
    }
}

pub fn process_cmd_for_stomach<T:Token,S:State<T>,Gu:Gullet<T,S=S>>(gullet:&mut Gu, state: &mut S, cause:T, cmd:Ptr<Command<T>>) -> Result<Option<StomachCommand<T>>,Box<dyn TeXError<T>>> {
    match &*cmd {
        Command::Relax => Ok(Some(StomachCommand{cause,cmd:StomachCommandInner::Relax})),
        Command::Def(ref def,_) => {
            let v = def.expand(state,gullet.mouth(),cmd.clone(),Ptr::new(cause.clone()))?;
            if !v.is_empty() {
                gullet.mouth().push_tokens(v);
            }
            Ok(None)
        }
        Command::Conditional{name,index} => {
            do_conditional(gullet,state,cause,name,*index)?;
            Ok(None)
        },
        Command::Gullet {name,index} => {
            do_expandable(gullet,state,cause,name,*index)?;
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
        Command::Char {char,catcode} =>
            Ok(Some(StomachCommand{cause,cmd:StomachCommandInner::Char(*char,true)}))
    }
}

pub fn do_expandable<T:Token,S:State<T>,Gu:Gullet<T,S=S>>(gullet:&mut Gu,state:&mut S,cause:T,name:&str,index:usize) -> Result<(),Box<dyn TeXError<T>>> {
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

pub fn do_conditional<T:Token,S:State<T>,Gu:Gullet<T,S=S>>(gullet:&mut Gu,state:&mut S,cause:T,name:&str,index:usize) -> Result<(),Box<dyn TeXError<T>>> {
    match gullet.conditional(index) {
        None => Err(ImplementationError(format!("Missing implementation for primitive command {}",name),PhantomData).into()),
        Some(c) => {
            gullet.new_conditional();
            let b = c(state,gullet,GulletCommand{cause})?;
            if b {
                gullet.set_conditional(ConditionalBranch::True);
                debug_log!(trace=>"True conditional");
                Ok(())
            } else { false_loop(gullet,state) }
        }
    }
}

pub fn false_loop<T:Token,S:State<T>,Gu:Gullet<T,S=S>>(gullet:&mut Gu,state:&mut S) -> Result<(),Box<dyn TeXError<T>>> {
    debug_log!(trace=>"False conditional. Skipping...");
    let mut incond:u8 = 0;
    while let Some((next,exp)) = gullet.mouth().get_next(state)? {
        match next.base() {
            BaseToken::Char(c,CategoryCode::Active) =>
                match state.get_ac_command(*c).as_deref() {
                    Some(Command::Conditional {..}) => incond += 1,
                    Some(Command::Gullet {name:"else",..}) if incond == 0 => {
                        debug_log!(trace=>"...else branch.");
                        gullet.set_conditional(ConditionalBranch::Else);
                        return Ok(())
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
                        gullet.set_conditional(ConditionalBranch::Else);
                        debug_log!(trace=>"...else branch.");
                        return Ok(())
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

pub fn else_loop<T:Token,S:State<T>,Gu:Gullet<T,S=S>>(gullet:&mut Gu,state:&mut S) -> Result<(),Box<dyn TeXError<T>>> {
    debug_log!(trace=>"\\else. Skipping...");
    gullet.set_conditional(ConditionalBranch::Else);
    let mut incond:u8 = 0;
    while let Some((next,exp)) = gullet.mouth().get_next(state)? {
        match next.base() {
            BaseToken::Char(c,CategoryCode::Active) =>
                match state.get_ac_command(*c).as_deref() {
                    Some(Command::Conditional {..}) => incond += 1,
                    Some(Command::Gullet {name:"else",..}) if incond == 0 => {
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
                    Some(Command::Gullet {name:"else",..}) if incond == 0 => {
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

fn is_ascii_digit(u:usize) -> bool {
    u >= 48 && u <= 57
}
fn is_ascii_oct_digit(u:usize) -> bool {
    u >= 48 && u <= 55
}
fn is_ascii_hex_digit(u:usize) -> bool {
    is_ascii_digit(u) || (u >= 65 && u <= 70) || (u >= 97 && u <= 102)
}

pub fn get_int<T:Token,Gu:Gullet<T>>(gullet:&mut Gu, state:&mut Gu::S) -> Result<<<Gu::S as State<T>>::NumSet as NumSet>::Int,Box<dyn TeXError<T>>> {
    debug_log!(trace=>"Reading number {}...\n at {}",gullet.mouth().preview(50).replace("\n","\\n"),gullet.mouth().file_line());
    gullet.mouth().skip_whitespace(state)?;

    let mut isnegative = false;
    let mut ishex = false;
    let mut isoct = false;

    while let Some(next) = gullet.get_next_stomach_command(state)? {
        match next.cmd {
            StomachCommandInner::Char(c,false) => {
                let us = c.to_usize();
                match us {
                    45 => { // -
                        isnegative = !isnegative;
                        catch!(gullet.mouth().skip_whitespace(state)
                            => next.cause
                        );
                    }
                    43 => /* + */ catch!(gullet.mouth().skip_whitespace(state)
                        => next.cause
                    ),
                    34 if !ishex && !isoct => ishex = true, // "
                    39 if !ishex && !isoct => isoct = true, // '
                    96 if !ishex && !isoct => { // `
                        match catch!(gullet.mouth().get_next(state)
                            => next.cause
                        ) {
                            None => file_end!(next.cause),
                            Some((tk,_)) => {
                                let c = match tk.base() {
                                    BaseToken::Char(c,_) => *c,
                                    BaseToken::CS(str) if str.len() == 1 =>
                                        unsafe{ *str.as_vec().first().unwrap_unchecked() }
                                    _ => return Err(ExpectedInteger(tk,PhantomData).into())
                                };
                                catch!(expand_until_space(gullet,state) => tk);
                                let us = c.to_usize() as i64;
                                return Ok(catch!(<<Gu::S as State<T>>::NumSet as NumSet>::Int::from_i64(us) => tk))
                            }
                        }
                    },
                    _ if is_ascii_hex_digit(us) && ishex =>
                        // TODO: texnically, this requires catcode 12 for 0-9 and catcode 11 or 12 for A-F
                        todo!("Hex digit in read_number"),
                    _ if is_ascii_digit(us) && !isoct =>
                        // TODO: texnically, this requires catcode 12
                        return read_decimal_number(gullet,state,us as u8,isnegative),
                    _ if is_ascii_oct_digit(us) => // isoct == true
                        // TODO: texnically, this requires catcode 12
                        todo!("Octal digit in read_number"),
                    _ =>
                        todo!("Non-digit in read_number")
                }
            }
            StomachCommandInner::ValueRegister(index,Assignable::Int) => {
                let val = state.get_int_register(index);
                let val = if isnegative { -val } else { val };
                debug_log!(trace=>"Returning {}",val);
                return Ok(val)
            }
            StomachCommandInner::Value {index,name,tp:Assignable::Int,..} => {
                match gullet.primitive_int(index) {
                    None => return Err(ImplementationError("Missing implementation for primitive int".to_string(),PhantomData).into()),
                    Some(f) => {
                        let val = f(state,gullet,GulletCommand{cause:next.cause})?;
                        let val = if isnegative { -val } else { val };
                        debug_log!(trace=>"Returning {}",val);
                        return Ok(val)
                    }
                }
            }
            StomachCommandInner::ValueAssignment {value_index,tp:Assignable::Int,..} => {
                match gullet.primitive_int(value_index) {
                    None => return Err(ImplementationError("Missing implementation for primitive int".to_string(),PhantomData).into()),
                    Some(f) => {
                        let val = f(state,gullet,GulletCommand{cause:next.cause})?;
                        let val = if isnegative { -val } else { val };
                        debug_log!(trace=>"Returning {}",val);
                        return Ok(val)
                    }
                }
            }
            StomachCommandInner::Char(c,_) => {
                let c = <<Gu::S as State<T>>::NumSet as NumSet>::Int::from_i64(c.to_usize() as i64)?;
                debug_log!(trace=>"Returning {}",c);
                return Ok(c)
            }
            StomachCommandInner::MathChar(u) => {
                let c = <<Gu::S as State<T>>::NumSet as NumSet>::Int::from_i64(u as i64)?;
                debug_log!(trace=>"Returning {}",c);
                return Ok(c)
            }
            o => todo!("Non-char in read_number: {:?}",o)
        }
    }
    file_end!()
}

pub fn expand_until_space<T:Token,S:State<T>,Gu:Gullet<T,S=S>>(gullet:&mut Gu,state:&mut S) -> Result<(),Box<dyn TeXError<T>>> {
    match gullet.get_next_stomach_command(state)? {
        Some(StomachCommand{cmd:StomachCommandInner::Space,..}) => Ok(()), // eat the space
        Some(o) => {
            gullet.mouth().requeue(o.cause);
            Ok(())
        }
        None => Ok(())
    }
}

pub fn read_decimal_number<NS:NumSet,T:Token,S:State<T,NumSet=NS>,Gu:Gullet<T,S=S>>(gullet:&mut Gu,state:&mut S,firstchar:u8,isnegative:bool) -> Result<NS::Int,Box<dyn TeXError<T>>> {
    debug_log!(trace=>"Reading decimal number {}...",(firstchar as char));
    let mut rets = vec!(firstchar);

    while let Some(next) = gullet.get_next_stomach_command(state)? {
        match next.cmd {
            StomachCommandInner::Char(c,false) => {
                let us = c.to_usize();
                if is_ascii_digit(us) {
                    rets.push(us as u8);
                } else {
                    gullet.mouth().requeue(next.cause);
                    break
                }
            }
            StomachCommandInner::Space => break, // eat one space
            _ => {
                gullet.mouth().requeue(next.cause);
                break
            }
        }
    }
    use std::str::FromStr;
    let i = i64::from_str(std::str::from_utf8(&rets).unwrap()).unwrap();
    debug_log!(trace=>"Returning {}",i);
    Ok(NS::Int::from_i64(if isnegative { -i } else { i })?)
}

pub fn get_dim<NS:NumSet,T:Token,S:State<T,NumSet=NS>,Gu:Gullet<T,S=S>>(gullet:&mut Gu, state:&mut S) -> Result<NS::Dim,Box<dyn TeXError<T>>> {
    debug_log!(trace=>"Reading dimension {}...\n at {}",gullet.mouth().preview(50).replace("\n","\\n"),gullet.mouth().file_line());
    gullet.mouth().skip_whitespace(state)?;
    let mut isnegative = false;
    while let Some(next) = gullet.get_next_stomach_command(state)? {
        match next.cmd {
            StomachCommandInner::Char(c,false) => {
                let us = c.to_usize();
                match us {
                    45 => { // -
                        isnegative = !isnegative;
                        catch!(gullet.mouth().skip_whitespace(state)
                            => next.cause
                        );
                    }
                    43 => /* + */ catch!(gullet.mouth().skip_whitespace(state)
                        => next.cause
                    ),
                    _ => return get_dim_inner(gullet,state,isnegative,StomachCommandInner::Char(c,false),next.cause)
                }
            }
            o => return get_dim_inner(gullet,state,isnegative,o,next.cause)
        }
    }
    file_end!()
}
pub fn get_dim_inner<NS:NumSet,T:Token,S:State<T,NumSet=NS>,Gu:Gullet<T,S=S>>(gullet:&mut Gu, state:&mut S,isnegative:bool,next:StomachCommandInner<T::Char>,cause:T) -> Result<NS::Dim,Box<dyn TeXError<T>>> {
    match next {
        StomachCommandInner::Char(c,false) => {
            let us = c.to_usize();
            match us {
                46 | 44 => /* . / ,*/ {
                    let f = catch!(read_float(gullet,state,us as u8,isnegative)
                            => cause
                        );
                    return read_unit(gullet,state,f)
                },
                _ if is_ascii_digit(us) => {
                    let f = catch!(read_float(gullet,state,us as u8,isnegative)
                            => cause
                        );
                    return read_unit(gullet,state,f)
                },
                _ => todo!("Non-digit in read_dimension")
            }
        }
        StomachCommandInner::ValueAssignment {value_index,tp:Assignable::Dim,..} => {
            match gullet.primitive_dim(value_index) {
                None => return Err(ImplementationError("Missing implementation for primitive dim".to_string(),PhantomData).into()),
                Some(f) => {
                    let val = f(state,gullet,GulletCommand{cause})?;
                    return Ok(if isnegative { -val } else { val })
                }
            }
        }
        StomachCommandInner::ValueRegister(i,Assignable::Dim) => {
            let val = state.get_dim_register(i);
            return Ok(if isnegative { -val } else { val })
        }
        o => todo!("Non-char in read_dim: {:?}",o)
    }
}

pub fn get_skipdim<NS:NumSet,T:Token,S:State<T,NumSet=NS>,Gu:Gullet<T,S=S>>(gullet:&mut Gu, state:&mut S) -> Result<NS::SkipDim,Box<dyn TeXError<T>>> {
    debug_log!(trace=>"Reading dimension {}...\n at {}",gullet.mouth().preview(50).replace("\n","\\n"),gullet.mouth().file_line());
    gullet.mouth().skip_whitespace(state)?;
    let mut isnegative = false;
    while let Some(next) = gullet.get_next_stomach_command(state)? {
        match next.cmd {
            StomachCommandInner::Char(c,false) => {
                let us = c.to_usize();
                match us {
                    45 => { // -
                        isnegative = !isnegative;
                        catch!(gullet.mouth().skip_whitespace(state)
                            => next.cause
                        );
                    }
                    43 => /* + */ catch!(gullet.mouth().skip_whitespace(state)
                        => next.cause
                    ),
                    46 | 44 => /* . / ,*/ {
                        let f = catch!(read_float(gullet,state,us as u8,isnegative)
                            => next.cause
                        );
                        return read_skip_unit(gullet,state,f)
                    },
                    _ if is_ascii_digit(us) => {
                        let f = catch!(read_float(gullet,state,us as u8,isnegative)
                            => next.cause
                        );
                        return read_skip_unit(gullet,state,f)
                    },
                    _ => todo!("Non-digit in read_skipdim")
                }
            }
            StomachCommandInner::ValueAssignment {value_index,tp:Assignable::Dim,..} => {
                match gullet.primitive_dim(value_index) {
                    None => return Err(ImplementationError("Missing implementation for primitive dim".to_string(),PhantomData).into()),
                    Some(f) => {
                        let val = f(state,gullet,GulletCommand{cause:next.cause})?;
                        let ret = if isnegative { -val } else { val };
                        return Ok(NS::SkipDim::from_dim(ret))
                    }
                }
            }
            _ => todo!("Non-char in read_skipdim")
        }
    }
    file_end!()
}

pub fn get_skip<NS:NumSet,T:Token,S:State<T,NumSet=NS>,Gu:Gullet<T,S=S>>(gullet:&mut Gu, state:&mut S) -> Result<Skip<NS::SkipDim>,Box<dyn TeXError<T>>> {
    debug_log!(trace=>"Reading skip {}...\n at {}",gullet.mouth().preview(50).replace("\n","\\n"),gullet.mouth().file_line());
    gullet.mouth().skip_whitespace(state)?;
    let mut isnegative = false;
    while let Some(next) = gullet.get_next_stomach_command(state)? {
        match next.cmd {
            StomachCommandInner::Char(c,false) => {
                let us = c.to_usize();
                match us {
                    45 => { // -
                        isnegative = !isnegative;
                        catch!(gullet.mouth().skip_whitespace(state)
                            => next.cause
                        );
                    }
                    43 => /* + */ catch!(gullet.mouth().skip_whitespace(state)
                        => next.cause
                    ),
                    _ => return get_skip_inner(gullet,state,isnegative,StomachCommandInner::Char(c,false),next.cause)
                }
            }
            StomachCommandInner::ValueAssignment {value_index,tp:Assignable::Skip,..} => {
                match gullet.primitive_skip(value_index) {
                    None => return Err(ImplementationError("Missing implementation for primitive skip".to_string(),PhantomData).into()),
                    Some(f) => {
                        let val = f(state,gullet,GulletCommand{cause:next.cause})?;
                        return Ok(if isnegative { -val } else { val })
                    }
                }
            }
            _ => return get_skip_inner(gullet,state,isnegative,next.cmd,next.cause)
        }
    }
    file_end!()
}

fn get_skip_inner<NS:NumSet,T:Token,S:State<T,NumSet=NS>,Gu:Gullet<T,S=S>>(gullet:&mut Gu, state:&mut S,isnegative:bool,next:StomachCommandInner<T::Char>,cause:T) -> Result<Skip<NS::SkipDim>,Box<dyn TeXError<T>>> {
    let base = get_dim_inner(gullet,state,isnegative,next,cause)?;
    gullet.mouth().skip_whitespace(state);
    let stretch = if gullet.get_keyword(state,"plus")? {
        Some(get_skipdim(gullet,state)?)
    } else {None};
    gullet.mouth().skip_whitespace(state);
    let shrink = if gullet.get_keyword(state,"minus")? {
        Some(get_skipdim(gullet,state)?)
    } else {None};
    Ok(Skip{base,stretch,shrink})
}

pub fn get_muskip<NS:NumSet,T:Token,S:State<T,NumSet=NS>,Gu:Gullet<T,S=S>>(gullet:&mut Gu, state:&mut S) -> Result<NS::MuSkip,Box<dyn TeXError<T>>> {
    todo!("get_muskip")
}


pub fn read_float<NS:NumSet,T:Token,S:State<T,NumSet=NS>,Gu:Gullet<T,S=S>>(gullet:&mut Gu,state:&mut S,firstchar:u8,isnegative:bool) -> Result<f64,Box<dyn TeXError<T>>> {
    debug_log!(trace=>"Reading float {}...",(firstchar as char));
    let mut in_float = firstchar == b'.' || firstchar == b',';
    let mut rets = if in_float {vec!(b'0',b'.')} else {vec!(firstchar)};
    while let Some(next) = gullet.get_next_stomach_command(state)? {
        match next.cmd {
            StomachCommandInner::Char(c,false) => {
                let us = c.to_usize();
                if is_ascii_digit(us) {
                    rets.push(us as u8);
                }
                else if !in_float && (us == 46 || us == 44) {
                    rets.push(b'.');
                    in_float = true;
                }
                else {
                    gullet.mouth().requeue(next.cause.clone());
                    break
                }
            }
            StomachCommandInner::Space => break, // eat one space
            _ => todo!("Non-char in read_decimal_number")
        }
    }
    use std::str::FromStr;
    let f = f64::from_str(std::str::from_utf8(&rets).unwrap()).unwrap();
    debug_log!(trace=>"Returning {}",f);
    Ok(if isnegative {-f} else {f})
}

pub fn read_unit<NS:NumSet,T:Token,S:State<T,NumSet=NS>,Gu:Gullet<T,S=S>>(gullet:&mut Gu,state:&mut S,float:f64) -> Result<NS::Dim,Box<dyn TeXError<T>>> {
    debug_log!(trace=>"Reading unit {}...\n at {}",gullet.mouth().preview(50).replace("\n","\\n"),gullet.mouth().file_line());
    gullet.mouth().skip_whitespace(state)?;
    if get_keyword(gullet, state, "true")? {
        gullet.mouth().skip_whitespace(state)?;
        let mag = state.get_primitive_int("mag").to_i64() as f64 / 1000.0;
        match get_keywords(gullet, state, NS::Dim::units())? {
            Some(dim) => Ok(NS::Dim::from_float(dim,float * mag)),
            _ => Err(ExpectedUnit(PhantomData).into())
        }
    } else {
        match get_keywords(gullet, state, NS::Dim::units())? {
            Some(dim) => Ok(NS::Dim::from_float(dim,float)),
            _ => todo!("Non-unit in read_unit")
        }
    }
}


pub fn read_skip_unit<NS:NumSet,T:Token,S:State<T,NumSet=NS>,Gu:Gullet<T,S=S>>(gullet:&mut Gu,state:&mut S,float:f64) -> Result<NS::SkipDim,Box<dyn TeXError<T>>> {
    debug_log!(trace=>"Reading skip unit {}...\n at {}",gullet.mouth().preview(50).replace("\n","\\n"),gullet.mouth().file_line());
    gullet.mouth().skip_whitespace(state)?;
    if get_keyword(gullet, state, "true")? {
        gullet.mouth().skip_whitespace(state)?;
        let mag = state.get_primitive_int("mag").to_i64() as f64 / 1000.0;
        match get_keywords(gullet, state, NS::Dim::units())? {
            Some(dim) => Ok(NS::SkipDim::from_float(dim,float * mag)),
            _ => Err(ExpectedUnit(PhantomData).into())
        }
    } else {
        match get_keywords(gullet, state, NS::SkipDim::units())? {
            Some(dim) => Ok(NS::SkipDim::from_float(dim,float)),
            _ => todo!("Non-unit in read_skip_unit: {}",gullet.mouth().preview(50).replace("\n","\\n"))
        }
    }
}

pub fn get_keyword<'a,T:Token,S:State<T>,Gu:Gullet<T,S=S>>(gullet:&mut Gu, state:&mut S, kw:&'a str) -> Result<bool,Box<dyn TeXError<T>>> {
    debug_log!(trace=>"Reading keyword {:?}: {}...\n at {}",kw,gullet.mouth().preview(50).replace("\n","\\n"),gullet.mouth().file_line());
    let mut current = String::new();
    let mut read_toks: Vec<T> = vec!();
    while let Some(next) = gullet.get_next_stomach_command(state)? {
        read_toks.push(next.cause.clone());
        match next.cmd {
            StomachCommandInner::Char(c,false) => {
                let us = c.to_usize();
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

pub fn get_keywords<'a,T:Token,S:State<T>,Gu:Gullet<T,S=S>>(gullet:&mut Gu, state:&mut S, mut keywords:Vec<&'a str>) -> Result<Option<&'a str>,Box<dyn TeXError<T>>> {
    debug_log!(trace=>"Reading keywords {:?}: {}...\n at {}",keywords,gullet.mouth().preview(50).replace("\n","\\n"),gullet.mouth().file_line());
    let mut current = String::new();
    let mut read_toks: Vec<T> = vec!();
    while let Some(next) = gullet.get_next_stomach_command(state)? {
        read_toks.push(next.cause.clone());
        match next.cmd {
            StomachCommandInner::Char(c,false) => {
                let us = c.to_usize();
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

pub fn token_to_string<T:Token>(tk:T,escapechar:Option<T::Char>) -> Vec<u8> {
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
                    s.push(b' ');
                    s
                }
            }
    }
}

pub fn tokens_to_string<T:Token>(v:Vec<T>,escapechar:Option<T::Char>) -> String {
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
                        s.push(b' ');
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
                        s.push(b' ');
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
        ret.push(T::new(BaseToken::Char(c,CategoryCode::Other),None));
    }
    ret
}


pub fn get_control_sequence<T:Token,S:State<T>,Gu:Gullet<T,S=S>>(gullet:&mut Gu, state:&mut S) -> Result<BaseToken<T::Char>,Box<dyn TeXError<T>>> {
    gullet.mouth().skip_whitespace(state)?;
    while let Some((next,e)) = gullet.mouth().get_next(state)? {
        match next.base() {
            BaseToken::Char(c,CategoryCode::Active) =>
                match get_cs_check_command(gullet, state, state.get_ac_command(*c), next)? {
                    None => (),
                    Some(t) => return Ok(t),
                }
            BaseToken::Char(c,_) => {
                // error
            }
            BaseToken::CS(s) =>
                match get_cs_check_command(gullet, state, state.get_command(s), next)? {
                    None => (),
                    Some(t) => return Ok(t),
                }
        }
    }
    file_end!()
}

fn get_cs_check_command<T:Token,S:State<T>,Gu:Gullet<T,S=S>>(gullet:&mut Gu, state:&mut S, cmd:Option<Ptr<Command<T>>>, tk:T)
    -> Result<Option<BaseToken<T::Char>>,Box<dyn TeXError<T>>> {
    match cmd.as_deref() {
        Some(Command::Conditional {name,index}) => {
            do_conditional(gullet,state,tk,name,*index)?;
            Ok(None)
        }
        Some(Command::Gullet {name,index}) => {
            do_expandable(gullet,state,tk,name,*index)?;
            Ok(None)
        }
        _ => Ok(Some(tk.base().clone()))
    }
}