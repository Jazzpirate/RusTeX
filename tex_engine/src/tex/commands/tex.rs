//! TeX primitive [`Command`]s

use std::marker::PhantomData;
use crate::{debug_log, register_assign, register_conditional, register_gullet, register_int_assign, register_stomach, register_tok_assign, map_group, register_int, register_whatsit, register_value_assign_int, register_value_assign_dim, register_value_assign_muskip, register_value_assign_skip, register_dim_assign, register_skip_assign, cmtodo, register_value_assign_font, register_open_box};
use crate::engine::filesystem::{File, FileSystem};
use crate::engine::gullet::Gullet;
use crate::engine::gullet::methods::{tokens_to_string, do_expandable, do_conditional, string_to_tokens};
use crate::engine::state::State;
use crate::engine::mouth::Mouth;
use crate::engine::state::modes::{BoxMode, GroupType, TeXMode};
use crate::engine::stomach::Stomach;
use crate::tex::catcodes::{CategoryCode, CategoryCodeScheme};
use crate::tex::commands::{Assignable, Command, Def, ExpToken, GulletCommand, ParamToken, StomachCommand, StomachCommandInner};
use crate::tex::commands::methods::{assign_primitive_dim, assign_primitive_int, assign_primitive_skip, assign_primitive_toks, parse_signature};
use crate::tex::numbers::{Int, NumSet, Skip, Numeric, MuSkip, Dim};
use crate::tex::token::{BaseToken, Token, TokenList};
use crate::utils::errors::{catch_prim, ErrorInPrimitive, file_end_prim, ExpectedToken, UnexpectedEndgroup, ImplementationError, TeXError};
use crate::utils::Ptr;
use crate::utils::strings::{AllCharsTrait, CharType, TeXStr};
use chrono::{Datelike, Timelike};
use crate::engine::{EngineType, gullet};
use crate::engine::stomach::methods::{assign_dim_register, assign_int_register, assign_muskip_register, assign_skip_register, assign_toks_register};
use crate::tex::boxes::{HBox, OpenBox, StomachNode, TeXNode, Whatsit};
use crate::tex::ConditionalBranch;
use crate::tex::fonts::{FontStore,Font};
use super::etex::protected;

/* TODO

SPACE
\/
\-
 */

pub fn SPACE<ET:EngineType>(_stomach:&mut ET::Stomach,state:&mut ET::State,_cmd:StomachCommand<ET::Token>)
                                     -> Result<(),ErrorInPrimitive<ET::Token>> {
    todo!("\\ ")
}

pub fn advance<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:StomachCommand<ET::Token>,global:bool)
                                                     -> Result<(),ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"\\advance");
    catch_prim!(gullet.mouth().skip_whitespace::<ET>(state) => ("advance",cmd));
    match catch_prim!(gullet.get_next_stomach_command(state) => ("advance",cmd)) {
        None => file_end_prim!("advance",cmd),
        Some(ocmd) => match ocmd.cmd {
            StomachCommandInner::ValueRegister(u,Assignable::Int) => {
                catch_prim!(gullet.get_keyword(state,"by") => ("advance",cmd));
                debug_log!(debug => "  \\count{}",u);
                let i = catch_prim!(gullet.get_int(state) => ("advance",cmd));
                let ov = state.get_int_register(u);
                debug_log!(debug => "  =={}+{}",ov,i);
                let nv : ET::Int = ov + i;
                debug_log!(debug => "  ={}",nv);
                state.set_int_register(u,nv,global);
                return Ok(())
            }
            StomachCommandInner::AssignableValue {name,tp:crate::tex::commands::Assignable::Int} => {
                debug_log!(debug => "  \\{}",name);
                catch_prim!(gullet.get_keyword(state,"by") => ("advance",cmd));
                let ov = state.get_primitive_int(name);
                let i = catch_prim!(gullet.get_int(state) => ("advance",cmd));
                debug_log!(debug => "  =={}+{}",ov,i);
                let nv : ET::Int = ov + i;
                debug_log!(debug => "  ={}",nv);
                state.set_primitive_int(name,nv,global);
                return Ok(())
            }
            StomachCommandInner::AssignableValue {name,tp:crate::tex::commands::Assignable::Dim} => {
                debug_log!(debug => "  \\{}",name);
                catch_prim!(gullet.get_keyword(state,"by") => ("advance",cmd));
                let ov = state.get_primitive_dim(name);
                let i = catch_prim!(gullet.get_dim(state) => ("advance",cmd));
                debug_log!(debug => "  =={}+{}",ov,i);
                let nv : ET::Dim = ov + i;
                debug_log!(debug => "  ={}",nv);
                state.set_primitive_dim(name,nv,global);
                return Ok(())
            }
            StomachCommandInner::ValueAssignment {name:"count",..} => {
                let i = catch_prim!(gullet.get_int(state) => ("advance",cmd));
                let u = match i.try_into() {
                    Ok(u) => u,
                    _ => return Err(ErrorInPrimitive{name:"count",msg:Some(format!("Not a valid register")),cause:Some(cmd.cause),source:None})
                };
                catch_prim!(gullet.get_keyword(state,"by") => ("advance",cmd));
                debug_log!(debug => "  \\count{}",u);
                let i = catch_prim!(gullet.get_int(state) => (stringify!(name),cmd));
                let ov = state.get_int_register(u);
                debug_log!(debug => "  =={}+{}",ov,i);
                let nv : ET::Int = ov + i;
                debug_log!(debug => "  ={}",nv);
                state.set_int_register(u,nv,global);
                return Ok(())
            }
            o => return Err(ErrorInPrimitive{name:"advance",msg:Some(format!("expected register after \\advance;got:{:?}",o)),cause:Some(cmd.cause),source:None})
        }
    }
}

pub fn begingroup<ET:EngineType>(state:&mut ET::State,_cmd:StomachCommand<ET::Token>)
     -> Result<(),ErrorInPrimitive<ET::Token>> {
    state.stack_push(GroupType::CS);
    Ok(())
}

pub fn catcode_assign<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:StomachCommand<ET::Token>,global:bool) -> Result<(),ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"Assigning category code");
    let i = catch_prim!(gullet.get_int(state) => ("catcode",cmd));
    let c: ET::Char = match ET::Char::from_i64(i.to_i64()) {
        Some(i) => i,
        None => return Err(ErrorInPrimitive{name:"catcode",msg:Some(format!("Not a valid character: {}",i)),cause:Some(cmd.cause),source:None})
    };
    catch_prim!(gullet.mouth().skip_eq_char::<ET>(state) => ("catcode",cmd));
    let v = catch_prim!(gullet.get_int(state) => ("catcode",cmd)).to_i64();
    if v < 0 || v > 15 {
        return Err(ErrorInPrimitive{name:"catcode",msg:Some(format!("Invalid category code: {}",v)),cause:Some(cmd.cause),source:None})
    }
    let cc: CategoryCode = unsafe{(v as u8).try_into().unwrap_unchecked()};
    debug_log!(debug=>"\\catcode '{}' = {}",c.char_str(),cc);
    state.set_catcode(c,cc,global);
    Ok(())
}
pub fn catcode_get<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:GulletCommand<ET::Token>) -> Result<ET::Int,ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"Getting category code");
    let i = catch_prim!(gullet.get_int(state) => ("catcode",cmd));
    let c: ET::Char = match ET::Char::from_i64(i.to_i64()) {
        Some(i) => i,
        None => return Err(ErrorInPrimitive{name:"catcode",msg:Some(format!("Not a valid character: {}",i)),cause:Some(cmd.cause),source:None})
    };
    let cc = *state.get_catcode_scheme().get(c);
    let v : u8 = cc.into();
    debug_log!(debug=>"\\catcode '{}' == {}",c.char_str(),cc);
    Ok(v.into())
}

pub fn chardef<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:StomachCommand<ET::Token>,global:bool) -> Result<(),ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"chardef");
    let name = catch_prim!(gullet.get_control_sequence(state) => ("chardef",cmd));
    catch_prim!(gullet.mouth().skip_eq_char::<ET>(state) => ("chardef",cmd));
    let num = catch_prim!(gullet.get_int(state) => ("chardef",cmd));
    let char = match ET::Char::from_i64(num.to_i64()) {
        Some(i) => i,
        None => return Err(ErrorInPrimitive{name:"chardef",msg:Some(format!("Not a valid character: {}",num)),cause:Some(cmd.cause),source:None})
    };
    match name {
        BaseToken::Char(c,_) => {
            debug_log!(debug=>"\\chardef: {} = {}",c.char_str(),char.char_str());
            state.set_ac_command(c, Some(Ptr::new(Command::Char{char,catcode:CategoryCode::Other})), global);
        }
        BaseToken::CS(name) => {
            debug_log!(debug=>"\\chardef: \\{} = {}",name,char.char_str());
            state.set_command(name, Some(Ptr::new(Command::Char{char,catcode:CategoryCode::Other})), global);
        }
    }
    Ok(())
}


pub fn closein<ET:EngineType>(state: &mut ET::State, gullet:&mut ET::Gullet, stomach:&mut ET::Stomach, cmd:StomachCommand<ET::Token>) -> Result<(), ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"\\closein");
    let i = catch_prim!(gullet.get_int(state) => ("closein",cmd));
    let i : usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => return Err(ErrorInPrimitive{name:"closein",msg:Some(format!("Invalid file number: {}",i)),cause:Some(cmd.cause),source:None})
    };
    state.file_closein(i); // TODO error?
    Ok(())
}

pub fn closeout<ET:EngineType>(state: &mut ET::State, gullet:&mut ET::Gullet, stomach:&mut ET::Stomach, cmd:StomachCommand<ET::Token>) -> Result<Whatsit<ET>, ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"\\closeout");
    let i = catch_prim!(gullet.get_int(state) => ("closeout",cmd));
    let i : usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => return Err(ErrorInPrimitive{name:"closeout",msg:Some(format!("Invalid file number: {}",i)),cause:Some(cmd.cause),source:None})
    };
    let apply = Box::new(move |_: &mut _,s:&mut ET::State,_: &mut _| {
        s.file_closeout(i); // TODO error?
        Ok(())
    });
    Ok(Whatsit { apply })
}

pub fn count_assign<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:StomachCommand<ET::Token>,global:bool) -> Result<(),ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"Assigning \\count");
    let i = catch_prim!(gullet.get_int(state) => ("count",cmd));
    let i:usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => return Err(ErrorInPrimitive{name:"count",msg:Some(format!("Not a valid register: {}",i)),cause:Some(cmd.cause),source:None})
    };
    catch_prim!(gullet.mouth().skip_eq_char::<ET>(state) => ("count",cmd));
    let v = catch_prim!(gullet.get_int(state) => ("count",cmd));
    debug_log!(debug=>"\\count{} = {}",i,v);
    state.set_int_register(i,v,global);
    Ok(())
}
pub fn count_get<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:GulletCommand<ET::Token>) -> Result<ET::Int,ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"Getting \\count");
    let i = catch_prim!(gullet.get_int(state) => ("count",cmd));
    let i:usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => return Err(ErrorInPrimitive{name:"count",msg:Some(format!("Not a valid register: {}",i)),cause:Some(cmd.cause),source:None})
    };
    let v = state.get_int_register(i);
    debug_log!(debug=>"\\count{} == {}",i,v);
    Ok(v)
}

pub fn countdef<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:StomachCommand<ET::Token>,global:bool)
    -> Result<(),ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"countdef");
    let name = catch_prim!(gullet.get_control_sequence(state) => ("countdef",cmd));
    match &name {
        BaseToken::Char(c,_) => {
            state.set_ac_command(*c,Some(Ptr::new(Command::Relax)),false)
        }
        BaseToken::CS(name) => {
            state.set_command(name.clone(),Some(Ptr::new(Command::Relax)),false)
        }
    }
    catch_prim!(gullet.mouth().skip_eq_char::<ET>(state) => ("countdef",cmd));
    let num = catch_prim!(gullet.get_int(state) => ("countdef",cmd));
    if num.to_i64() < 0 {
        return Err(ErrorInPrimitive{name:"countdef",msg:Some(format!("Invalid count register index: {}",num)),cause:Some(cmd.cause),source:None})
    }
    let num = num.to_i64() as usize;
    match name {
        BaseToken::Char(c,_) => {
            debug_log!(debug=>"\\countdef: {} = \\count{}",c.char_str(),num);
            state.set_ac_command(c, Some(Ptr::new(Command::ValueRegister{index:num,tp:Assignable::Int})), global);
        }
        BaseToken::CS(name) => {
            debug_log!(debug=>"\\countdef: \\{} = {}",name,num);
            state.set_command(name, Some(Ptr::new(Command::ValueRegister{index:num,tp:Assignable::Int})), global);
        }
    }
    Ok(())
}

pub fn get_csname<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:GulletCommand<ET::Token>,name:&'static str)
    -> Result<TeXStr<ET::Char>,ErrorInPrimitive<ET::Token>>{
    let csidx = state.push_csname();
    let mut csname = Vec::new();
    while state.current_csname() == Some(csidx) {
        match catch_prim!(gullet.get_next_stomach_command(state) => (name,cmd)) {
            None => return file_end_prim!(name,cmd),
            Some(sc) => match sc.cmd {
                StomachCommandInner::Command {name:"endcsname",..} => state.pop_csname(),
                StomachCommandInner::Char{char,..} => csname.push(char),
                StomachCommandInner::BeginGroup(c) => csname.push(c),
                StomachCommandInner::EndGroup(c) => csname.push(c),
                StomachCommandInner::Superscript(c) => csname.push(c),
                StomachCommandInner::Subscript(c) => csname.push(c),
                StomachCommandInner::MathShift(c) => csname.push(c),
                StomachCommandInner::Space => csname.push(ET::Char::from(b' ')),
                o => return Err(ErrorInPrimitive{name:"csname",msg:Some(format!("Unexpected token in {}: {:?}",name,o)),cause:Some(cmd.cause),source:None})
            }
        }
    }
    Ok(csname.into())
}

pub fn csname<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:GulletCommand<ET::Token>) -> Result<Vec<ET::Token>,ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"csname");
    let str = get_csname::<ET>(state,gullet,cmd,"csname")?;
    debug_log!(trace=>"csname {}",str.to_string());
    match state.get_command(&str) {
        None => state.set_command(str.clone(),Some(Ptr::new(Command::Relax)),false),
        _ => ()
    }
    Ok(vec!(ET::Token::new(BaseToken::CS(str),None)))
}

pub fn day<ET:EngineType>(state:&mut ET::State,cmd:GulletCommand<ET::Token>) -> Result<ET::Int,ErrorInPrimitive<ET::Token>> {
    Ok(catch_prim!(ET::Int::from_i64(
        state.get_start_time().day() as i64
    ) => ("day",cmd)))
}


pub fn def<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:StomachCommand<ET::Token>,global:bool,protected:bool,long:bool,outer:bool)
    -> Result<(),ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"def");
    let csO = catch_prim!(gullet.mouth().get_next::<ET>(state) => ("def",cmd));
    let cs = match &csO {
        None => file_end_prim!("def",cmd),
        Some((t,_)) => t.base()
    };
    match cs {
        BaseToken::Char(_,CategoryCode::Active) => (),
        BaseToken::CS(_) => (),
        _ => return Err(ErrorInPrimitive{name:"def",msg:Some(format!("Command expected after \\def")),cause:Some(csO.unwrap().0),source:None})
    }
    let (endswithbrace,arity,signature) = parse_signature::<ET>(state,gullet,cmd.clone(),"def")?;
    let mut replacement: Vec<ExpToken<ET::Token>> = Vec::with_capacity(50);
    map_group!("def",cmd,state,gullet.mouth(),{
        let def = Command::Def(Def{protected,long,outer,endswithbrace,arity,signature,replacement},cmd.cause.clone());
        match cs {
            BaseToken::Char(c,_) => {
                debug_log!(debug=>"\\def: {} = {:?}",c,def);
                state.set_ac_command(*c,Some(Ptr::new(def)),global)
            }
            BaseToken::CS(name) => {
                debug_log!(debug=>"\\def: \\{} = {:?}",name,def);
                state.set_command(name.clone(),Some(Ptr::new(def)),global)
            }
        }
        return Ok(())
    },tk => {
        match tk.base() {
            BaseToken::Char(c,CategoryCode::Parameter) => {
                match catch_prim!(gullet.mouth().get_next::<ET>(state) => ("def",cmd)) {
                    None => file_end_prim!("def",cmd),
                    Some((stk,_)) => match stk.base() {
                        BaseToken::Char(_,CategoryCode::Parameter) =>
                            replacement.push(ExpToken::ParamToken(tk)),
                        BaseToken::Char(c,_) => {
                            let u = c.to_usize();
                            if u < 48 || u - 48 > (arity as usize) {
                                return Err(ErrorInPrimitive{name:"def",msg:Some(format!("Illegal parameter number {}",u-48)),cause:Some(cmd.cause),source:None})
                            }
                            replacement.push(ExpToken::Param(tk,(u-49) as u8))
                        }
                        _ =>
                        return Err(ErrorInPrimitive{name:"def",msg:Some(format!("Expected number after #, got {}",stk)),cause:Some(cmd.cause),source:None})
                    }
                }
            }
            _ => replacement.push(ExpToken::Token(tk))
        }
    });
    file_end_prim!("def",cmd)
}

pub fn detokenize<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:GulletCommand<ET::Token>) -> Result<Vec<ET::Token>,ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"detokenize");
    match catch_prim!(gullet.get_next_stomach_command(state) => ("detokenize",cmd)) {
        None => file_end_prim!("detokenize",cmd),
        Some(sc) => match sc.cmd {
            StomachCommandInner::BeginGroup(_) => (),
            _ => return Err(ErrorInPrimitive{name:"detokenize",msg:Some("Expected a begin group after \\detokenize".to_string()),cause:Some(cmd.cause),source:None})
        }
    }
    let mut ingroups = 1;
    let mut ret = vec!();
    let escape = state.get_escapechar();
    let cc = state.get_catcode_scheme();
    let mut succeeded = false;
    while let Some((next,_)) = catch_prim!(gullet.mouth().get_next::<ET>(state) => ("detokenize",cmd)) {
        match next.base() {
            BaseToken::Char(c,CategoryCode::BeginGroup) => {
                ingroups += 1;
                ret.push(ET::Token::new(BaseToken::Char(*c,CategoryCode::Other),None))
            }
            BaseToken::Char(c,CategoryCode::EndGroup) => {
                ingroups -= 1;
                if ingroups == 0 {
                    succeeded = true;
                    break
                }
                ret.push(ET::Token::new(BaseToken::Char(*c,CategoryCode::Other),None))
            }
            BaseToken::Char(c,CategoryCode::Space) => {
                ret.push(next)
            }
            BaseToken::Char(c,CategoryCode::Parameter) => {
                ret.push(ET::Token::new(BaseToken::Char(*c,CategoryCode::Other),None));
                ret.push(ET::Token::new(BaseToken::Char(*c,CategoryCode::Other),None))
            }
            BaseToken::Char(c,_) => {
                ret.push(ET::Token::new(BaseToken::Char(*c,CategoryCode::Other),None))
            }
            BaseToken::CS(name) => {
                match escape {
                    None => (),
                    Some(c) => ret.push(ET::Token::new(BaseToken::Char(c,CategoryCode::Other),None))
                }
                for c in name.as_vec() {
                    if *cc.get(*c) == CategoryCode::Space {
                        ret.push(ET::Token::new(BaseToken::Char(*c,CategoryCode::Space),None));
                    } else {
                        ret.push(ET::Token::new(BaseToken::Char(c.clone(), CategoryCode::Other), None));
                    }
                }
                if name.as_vec().len() != 1 || *state.get_catcode_scheme().get(name.as_vec()[0]) != CategoryCode::Letter {
                    ret.push(ET::Token::new(BaseToken::Char(ET::Char::from(b' '),CategoryCode::Space),None))
                }
            }
        }
    }
    if !succeeded { file_end_prim!("detokenize",cmd) }
    debug_log!(debug=>"detokenize: {:?}",ret);
    Ok(ret)
}

pub fn dimen_assign<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:StomachCommand<ET::Token>,global:bool)
    -> Result<(),ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"Assigning \\dimen");
    let i = catch_prim!(gullet.get_int(state) => ("dimen",cmd));
    let i:usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => return Err(ErrorInPrimitive{name:"dimen",msg:Some(format!("Not a valid register: {}",i)),cause:Some(cmd.cause),source:None})
    };
    catch_prim!(gullet.mouth().skip_eq_char::<ET>(state) => ("dimen",cmd));
    let v = catch_prim!(gullet.get_dim(state) => ("dimen",cmd));
    debug_log!(debug=>"\\dimen{} = {}",i,v);
    state.set_dim_register(i,v,global);
    Ok(())
}
pub fn dimen_get<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:GulletCommand<ET::Token>)
    -> Result<ET::Dim,ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"Getting \\dimen");
    let i = catch_prim!(gullet.get_int(state) => ("dimen",cmd));
    let i:usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => return Err(ErrorInPrimitive{name:"dimen",msg:Some(format!("Not a valid register: {}",i)),cause:Some(cmd.cause),source:None})
    };
    let v = state.get_dim_register(i);
    debug_log!(debug=>"\\dimen{} == {}",i,v);
    Ok(v)
}

pub fn dimendef<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:StomachCommand<ET::Token>,global:bool)
                                                     -> Result<(),ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"dimendef");
    let name = catch_prim!(gullet.get_control_sequence(state) => ("dimendef",cmd));
    match &name {
        BaseToken::Char(c,_) => {
            state.set_ac_command(*c,Some(Ptr::new(Command::Relax)),false)
        }
        BaseToken::CS(name) => {
            state.set_command(name.clone(),Some(Ptr::new(Command::Relax)),false)
        }
    }
    catch_prim!(gullet.mouth().skip_eq_char::<ET>(state) => ("dimendef",cmd));
    let num = catch_prim!(gullet.get_int(state) => ("dimendef",cmd));
    if num.to_i64() < 0 {
        return Err(ErrorInPrimitive{name:"dimendef",msg:Some(format!("Invalid dimen register index: {}",num)),cause:Some(cmd.cause),source:None})
    }
    let num = num.to_i64() as usize;
    match name {
        BaseToken::Char(c,_) => {
            debug_log!(debug=>"\\dimendef: {} = \\dimen{}",c.char_str(),num);
            state.set_ac_command(c, Some(Ptr::new(Command::ValueRegister{index:num,tp:Assignable::Dim})), global);
        }
        BaseToken::CS(name) => {
            debug_log!(debug=>"\\dimendef: \\{} = {}",name,num);
            state.set_command(name, Some(Ptr::new(Command::ValueRegister{index:num,tp:Assignable::Dim})), global);
        }
    }
    Ok(())
}

pub fn divide<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:StomachCommand<ET::Token>,global:bool)
    -> Result<(),ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"\\divide");
    catch_prim!(gullet.mouth().skip_whitespace::<ET>(state) => ("divide",cmd));
    match catch_prim!(gullet.get_next_stomach_command(state) => ("divide",cmd)) {
        None => file_end_prim!("divide",cmd),
        Some(ocmd) => match ocmd.cmd {
            StomachCommandInner::ValueRegister(u,Assignable::Int) => {
                catch_prim!(gullet.get_keyword(state,"by") => ("divide",cmd));
                debug_log!(debug => "  \\count{}",u);
                let i = catch_prim!(gullet.get_int(state) => ("divide",cmd));
                let ov = state.get_int_register(u);
                debug_log!(debug => "  =={}/{}",ov,i);
                let nv : ET::Int = ov / i;
                debug_log!(debug => "  ={}",nv);
                state.set_int_register(u,nv,global);
                return Ok(())
            }
            StomachCommandInner::AssignableValue {name,tp:crate::tex::commands::Assignable::Int} => {
                debug_log!(debug => "  \\{}",name);
                catch_prim!(gullet.get_keyword(state,"by") => ("divide",cmd));
                let ov = state.get_primitive_int(name);
                let i = catch_prim!(gullet.get_int(state) => ("divide",cmd));
                debug_log!(debug => "  =={}/{}",ov,i);
                if i.to_i64() == 0 {
                    return Err(ErrorInPrimitive{name:"divide",msg:Some(format!("Division by zero: {} / {}",ov,i)),cause:Some(cmd.cause),source:None})
                }
                let nv : ET::Int = ov / i;
                debug_log!(debug => "  ={}",nv);
                state.set_primitive_int(name,nv,global);
                return Ok(())
            }
            StomachCommandInner::AssignableValue {name,tp:crate::tex::commands::Assignable::Dim} => {
                debug_log!(debug => "  \\{}",name);
                catch_prim!(gullet.get_keyword(state,"by") => ("divide",cmd));
                let ov = state.get_primitive_dim(name);
                let i = catch_prim!(gullet.get_int(state) => ("divide",cmd));
                debug_log!(debug => "  =={}/{}",ov,i);
                if i.to_i64() == 0 {
                    return Err(ErrorInPrimitive{name:"divide",msg:Some(format!("Division by zero: {} / {}",ov,i)),cause:Some(cmd.cause),source:None})
                }
                let nv : ET::Dim = ov.tex_div(i.to_i64());
                debug_log!(debug => "  ={}",nv);
                state.set_primitive_dim(name,nv,global);
                return Ok(())
            }
            StomachCommandInner::ValueAssignment {name:"count",..} => {
                let i = catch_prim!(gullet.get_int(state) => ("divide",cmd));
                let u = match i.try_into() {
                    Ok(u) => u,
                    _ => return Err(ErrorInPrimitive{name:"count",msg:Some(format!("Not a valid register")),cause:Some(cmd.cause),source:None})
                };
                catch_prim!(gullet.get_keyword(state,"by") => ("divide",cmd));
                debug_log!(debug => "  \\count{}",u);
                let i = catch_prim!(gullet.get_int(state) => (stringify!(name),cmd));
                let ov = state.get_int_register(u);
                if i.to_i64() == 0 {
                    return Err(ErrorInPrimitive{name:"divide",msg:Some(format!("Division by zero: {} / {}",ov,i)),cause:Some(cmd.cause),source:None})
                }
                debug_log!(debug => "  =={}/{}",ov,i);
                let nv : ET::Int = ov / i;
                debug_log!(debug => "  ={}",nv);
                state.set_int_register(u,nv,global);
                return Ok(())
            }
            o => return Err(ErrorInPrimitive{name:"divide",msg:Some(format!("expected register after \\divide;got:{:?}",o)),cause:Some(cmd.cause),source:None})
        }
    }
}

pub fn edef<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:StomachCommand<ET::Token>,global:bool,protected:bool,long:bool,outer:bool)
                                                -> Result<(),ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"edef");
    let csO = catch_prim!(gullet.mouth().get_next::<ET>(state) => ("edef",cmd));
    let cs = match &csO {
        None => file_end_prim!("edef",cmd),
        Some((t,_)) => t.base()
    };
    match cs {
        BaseToken::Char(_,CategoryCode::Active) => (),
        BaseToken::CS(_) => (),
        _ => return Err(ErrorInPrimitive{name:"edef",msg:Some(format!("Command expected after \\edef")),cause:Some(csO.unwrap().0),source:None})
    }
    let (endswithbrace,arity,signature) = parse_signature::<ET>(state,gullet,cmd.clone(),"edef")?;
    let mut replacement: Vec<ExpToken<ET::Token>> = Vec::with_capacity(50);

    macro_rules! expand_group_with_unknowns {
        ($state:ident,$gullet:ident,$finish:expr,($tk:ident,$expand:ident) => $f:expr;$($branch:tt)*) => {
            if let Some((tk,b)) = catch_prim!($gullet.mouth().get_next::<ET>($state) => ("edef",cmd)) {
                match tk.catcode() {
                    CategoryCode::BeginGroup => (),
                    _ =>
                    return Err(ErrorInPrimitive{name:"edef",msg:None,cause:Some(cmd.cause),source:Some(
                        ExpectedToken{expected:ET::Token::new(BaseToken::Char(ET::Char::from(b'{'),CategoryCode::BeginGroup),None),found:tk}.into()
                    )})
                }
            }
            let mut depth = 1;
            while let Some(($tk,$expand)) = catch_prim!($gullet.mouth().get_next::<ET>($state) => ("edef",cmd)) {
                match $tk.catcode() {
                    CategoryCode::BeginGroup => {
                        depth += 1;
                        $f;
                    }
                    CategoryCode::EndGroup => {
                        depth -= 1;
                        if depth == 0 { $finish }
                        if depth < 0 {
                            return Err(ErrorInPrimitive{name:"edef",msg:None,cause:Some(cmd.cause),source:Some(
                                UnexpectedEndgroup($tk).into()
                            )})
                        }
                        $f;
                    },
                    _ => {
                        match $tk.base() {
                            BaseToken::CS(n) => {
                                match $state.get_command(n) {
                                    None => $f,
                                    Some(_) if !$expand => $f,
                                    Some(ncmd) => match &*ncmd {
                                        $($branch)*,
                                        Command::Gullet {name,index} => {
                                            catch_prim!(do_expandable::<ET>($gullet,$state,$tk,name,*index) => ("edef",cmd));
                                        }
                                        Command::Def(def,_) if $expand => {
                                            let v = catch_prim!(def.expand::<ET>($state,$gullet.mouth(),ncmd.clone(),Ptr::new($tk)) => ("edef",cmd));
                                            if !v.is_empty() {
                                                $gullet.mouth().push_tokens(v);
                                            }
                                        }
                                        Command::Conditional {name,index} => {
                                            catch_prim!(do_conditional::<ET>($gullet,$state,$tk,name,*index) => ("edef",cmd));
                                        }
                                        _ => $f
                                    }
                                }
                            }
                            BaseToken::Char(c, CategoryCode::Active) => {
                                match $state.get_ac_command(*c) {
                                    None => $f,
                                    Some(_) if !$expand => $f,
                                    Some(ncmd) => match &*ncmd {
                                        $($branch)*,
                                        Command::Gullet {name,index} => {
                                            catch_prim!(do_expandable::<ET>($gullet,$state,$tk,name,*index) => ("edef",cmd));
                                        }
                                        Command::Def(def,_) if $expand => {
                                            let v = catch_prim!(def.expand::<ET>($state,$gullet.mouth(),ncmd.clone(),Ptr::new($tk)) => ("edef",cmd));
                                            if !v.is_empty() {
                                                $gullet.mouth().push_tokens(v);
                                            }
                                        }
                                        Command::Conditional {name,index} => {
                                            catch_prim!(do_conditional::<ET>($gullet,$state,$tk,name,*index) => ("edef",cmd));
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
            file_end_prim!("edef",cmd)
        }
    }

    expand_group_with_unknowns!(state,gullet,{
        let def = Command::Def(Def{protected,long,outer,endswithbrace,arity,signature,replacement},cmd.cause.clone());
        match cs {
            BaseToken::Char(c,_) => {
                debug_log!(debug=>"\\edef: {} = {:?}",c,def);
                state.set_ac_command(*c,Some(Ptr::new(def)),global)
            }
            BaseToken::CS(name) => {
                debug_log!(debug=>"\\edef: \\{} = {:?}",name,def);
                state.set_command(name.clone(),Some(Ptr::new(def)),global)
            }
        }
        return Ok(())
    },(tk,expand) => {
        match tk.base() {
            BaseToken::Char(c,CategoryCode::Parameter) => {
                match catch_prim!(gullet.mouth().get_next::<ET>(state) => ("edef",cmd)) {
                    None => file_end_prim!("edef",cmd),
                    Some((stk,_)) => match stk.base() {
                        BaseToken::Char(_,CategoryCode::Parameter) =>
                            replacement.push(ExpToken::ParamToken(tk)),
                        BaseToken::Char(c,_) => {
                            let u = c.to_usize();
                            if u < 48 || u - 48 > (arity as usize) {
                                return Err(ErrorInPrimitive{name:"edef",msg:Some(format!("Illegal parameter number {}",u-48)),cause:Some(cmd.cause),source:None})
                            }
                            replacement.push(ExpToken::Param(tk,(u-49) as u8))
                        }
                        _ =>
                        return Err(ErrorInPrimitive{name:"edef",msg:Some(format!("Expected number after #, got {}",stk)),cause:Some(cmd.cause),source:None})
                    }
                }
            }
            _ => replacement.push(ExpToken::Token(tk))
        }
    };
            Command::Gullet {name:"the",..} => {
                for t in catch_prim!(the::<ET>(state,gullet,GulletCommand{cause:cmd.cause.clone()}) => ("edef",cmd)) {
                    if t.catcode() == CategoryCode::Parameter {
                        replacement.push(ExpToken::ParamToken(t));
                    } else {
                        replacement.push(ExpToken::Token(t));
                    }
                }
            }
            Command::Gullet {name:"unexpanded",index} => {
                match gullet.primitive(*index) {
                    Some(f) => {
                        for t in catch_prim!(f(state,gullet,GulletCommand{cause:cmd.cause.clone()}) => ("edef",cmd)) {
                            if t.catcode() == CategoryCode::Parameter {
                                replacement.push(ExpToken::ParamToken(t));
                            } else {
                                replacement.push(ExpToken::Token(t));
                            }
                        }
                    }
                    None => return Err(ErrorInPrimitive{name:"edef",msg:Some("\\unexpanded not implemented".to_string()),cause:Some(cmd.cause),source:None})
                }
            }
            Command::Def(def,_) if def.protected => replacement.push(ExpToken::Token(tk))
    );
    file_end_prim!("edef",cmd)
}

pub fn else_<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:GulletCommand<ET::Token>)
    -> Result<Vec<ET::Token>,ErrorInPrimitive<ET::Token>> {
    match gullet.current_conditional() {
        (None,_) => return Err(ErrorInPrimitive{name:"else",msg:Some("Not in a conditional".to_string()),cause:Some(cmd.cause),source:None}),
        (Some(ConditionalBranch::True(name)),i) =>
            catch_prim!(crate::engine::gullet::methods::else_loop::<ET>(gullet,state,name,i) => ("else",cmd)),
        (Some(ConditionalBranch::Case(_,_)),i) =>
            catch_prim!(crate::engine::gullet::methods::else_loop::<ET>(gullet,state,"ifcase",i) => ("else",cmd)),
        o => unreachable!("{:?}\nat:{}\n{}\n",o,gullet.mouth().file_line(),gullet.mouth().preview(200))
    }
    Ok(vec![])
}

pub fn end<ET:EngineType>(_stomach:&mut ET::Stomach,_state:&mut ET::State,_cmd:StomachCommand<ET::Token>)
    -> Result<(),ErrorInPrimitive<ET::Token>> {
    todo!("end")
}

pub fn endcsname<T:Token>(cmd:StomachCommand<T>) -> Result<(),ErrorInPrimitive<T>> {
    Err(ErrorInPrimitive{name:"endcsname",msg:Some("Not in a \\csname".to_string()),cause:Some(cmd.cause),source:None})
}

pub fn endgroup<ET:EngineType>(_stomach:&mut ET::Stomach,state:&mut ET::State,gullet:&mut ET::Gullet,cmd:StomachCommand<ET::Token>)
    -> Result<(),ErrorInPrimitive<ET::Token>> {
    match state.stack_pop() {
        Some((v,GroupType::CS)) => {
            if !v.is_empty() {gullet.mouth().push_tokens(v);}
            Ok(())
        }
        _ => Err(ErrorInPrimitive{name:"endgroup",msg:Some("No group to end".to_string()),cause:Some(cmd.cause),source:None}),
    }
}

pub fn endlinechar_assign<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:StomachCommand<ET::Token>,global:bool)
    -> Result<(),ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"Assigning \\endlinechar");
    catch_prim!(gullet.mouth().skip_eq_char::<ET>(state) => ("endlinechar",cmd));
    let i = catch_prim!(gullet.get_int(state) => ("endlinechar",cmd));
    let c = match i.to_i64() {
        -1|255 => None,
        j => match ET::Char::from_i64(j) {
            Some(c) => Some(c),
            None => return Err(ErrorInPrimitive{name:"endlinechar",msg:Some(format!("Not a valid character: {}",j)),cause:Some(cmd.cause),source:None})
        }
    };
    debug_log!(debug=>"\\endlinechar = {:?}",c.map(|c| c.char_str()));
    state.set_endlinechar(c,global);
    Ok(())
}
pub fn endlinechar_get<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:GulletCommand<ET::Token>)
    -> Result<ET::Int,ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"Getting \\endlinechar");
    let c = match state.get_endlinechar() {
        None => -1,
        Some(c) => c.to_usize() as i64
    };
    debug_log!(debug=>"\\endlinechar == {:?}",c);
    Ok(ET::Int::from_i64::<ET::Token>(c).unwrap())
}

// \errhelp

pub fn errmessage<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:StomachCommand<ET::Token>)
    -> Result<(),ErrorInPrimitive<ET::Token>> {
    debug_log!(debug=>"errmessage");
    catch_prim!(gullet.mouth().skip_whitespace::<ET>(state) => ("errmessage",cmd));
    let errmsg = String::from_utf8(catch_prim!(gullet.get_braced_string(state) => ("errmessage",cmd))).unwrap();
    let eh = state.get_primitive_toks("errhelp");
    // TODO errhelp
    Err(ErrorInPrimitive{
        name:"errmessage",
        msg:Some(errmsg + "\n\n" + &gullet.mouth().file_line()),
        cause:Some(cmd.cause),
        source:None
    }.into())
}

pub fn escapechar_assign<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:StomachCommand<ET::Token>,global:bool)
    -> Result<(),ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"Assigning \\escapechar");
    catch_prim!(gullet.mouth().skip_eq_char::<ET>(state) => ("escapechar",cmd));
    let i = catch_prim!(gullet.get_int(state) => ("escapechar",cmd));
    let c = match i.to_i64() {
        -1|255 => None,
        j => match ET::Char::from_i64(j) {
            Some(c) => Some(c),
            None => return Err(ErrorInPrimitive{name:"escapechar",msg:Some(format!("Not a valid character: {}",j)),cause:Some(cmd.cause),source:None})
        }
    };
    debug_log!(debug=>"\\escapechar = {:?}",c.map(|c| c.char_str()));
    state.set_escapechar(c,global);
    Ok(())
}
pub fn escapechar_get<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:GulletCommand<ET::Token>) -> Result<ET::Int,ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"Getting \\escapechar");
    let c = match state.get_escapechar() {
        None => -1,
        Some(c) => c.to_usize() as i64
    };
    debug_log!(debug=>"\\escapechar == {}",c);
    Ok(ET::Int::from_i64::<ET::Token>(c).unwrap())
}

pub fn expandafter<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:GulletCommand<ET::Token>)
    -> Result<Vec<ET::Token>,ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"expandafter");
    let (first,exp) = match catch_prim!(gullet.mouth().get_next::<ET>(state) => ("expandafter",cmd)){
        None => file_end_prim!("expandafter",cmd),
        Some((t,b)) => (t,b)
    };
    debug_log!(debug=>"expandafter: 1. {}",first);
    match catch_prim!(gullet.mouth().get_next::<ET>(state) => ("expandafter",cmd)){
        None => file_end_prim!("expandafter",cmd),
        Some((t,false)) => {
            debug_log!(debug=>"expandafter: 2. \\noexpand{}",t);
            gullet.mouth().push_noexpand(t);
        }
        Some((t,_)) => {
            debug_log!(debug=>"expandafter: 2. {}",t);
            if let Some(ncmd) = match t.base() {
                BaseToken::CS(n) => Some(catch_prim!(state.need_command(&n) => ("expandafter",cmd))),
                BaseToken::Char(c, CategoryCode::Active) =>
                    Some(catch_prim!(state.need_ac_command(*c) => ("expandafter",cmd))),
                _ => {
                    gullet.mouth().requeue(t.clone());
                    None
                }
            } {
                match &*ncmd {
                    Command::Conditional{name,index} =>
                        catch_prim!(crate::engine::gullet::methods::do_conditional::<ET>(gullet,state,t,name,*index) => ("expandafter",cmd)),
                    Command::Gullet {name,index} =>
                        catch_prim!(crate::engine::gullet::methods::do_expandable::<ET>(gullet,state,t,name,*index) => ("expandafter",cmd)),
                    Command::Def(d,_) => {
                        let v = catch_prim!(d.expand::<ET>(state,gullet.mouth(),ncmd.clone(),Ptr::new(t.clone())) => ("expandafter",cmd));
                        if !v.is_empty() {
                            gullet.mouth().push_tokens(v);
                        }
                    }
                    _ => gullet.mouth().requeue(t)
                }
            }
        }
    };
    if exp {
        gullet.mouth().requeue(first);
    } else {
        gullet.mouth().push_noexpand(first);
    }
    Ok(vec!())
}

pub fn fi<ET:EngineType>(_state:&mut ET::State,gullet:&mut ET::Gullet,_cmd:GulletCommand<ET::Token>)
    -> Result<Vec<ET::Token>,ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"...end of conditional.");
    gullet.pop_conditional();
    Ok(vec![])
}

pub fn font_assign<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:StomachCommand<ET::Token>,global:bool)
    -> Result<(),ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"Assigning \\font");
    let cs = catch_prim!(gullet.get_control_sequence(state) => ("font",cmd));
    catch_prim!(gullet.mouth().skip_eq_char::<ET>(state) => ("font",cmd));
    let mut fontname = catch_prim!(gullet.get_string(state) => ("font",cmd)).to_string();
    if !fontname.ends_with(".tfm") {
        fontname = fontname + ".tfm"
    }
    let index = catch_prim!(state.fontstore_mut().get_new(&fontname) => ("font",cmd));
    match catch_prim!(gullet.get_keywords(state,vec!("at","scaled")) => ("font",cmd)) {
        Some(s) if s == "at" => {
            let dim = catch_prim!(gullet.get_dim(state) => ("font",cmd));
            state.fontstore_mut().get_mut(index).set_at(dim.to_sp());
        }
        Some(s) if s == "scaled" => {
            let r = catch_prim!(crate::engine::gullet::numeric_methods::read_float::<ET>(gullet,state,b'0',false) => ("font",cmd));
            let font = state.fontstore_mut().get_mut(index);
            let new_at = ((font.get_at() as f64) * r).round() as i64;
            font.set_at(new_at);
        }
        _ => ()
    }
    let fontcmd = Command::ValueRegister{index,tp:Assignable::Font};
    match cs.base() {
        BaseToken::Char(c,CategoryCode::Active) =>
            state.set_ac_command(*c,Some(Ptr::new(fontcmd)),global),
        BaseToken::CS(name) =>
            state.set_command(name.clone(),Some(Ptr::new(fontcmd)),global),
        _ => unreachable!()
    }
    Ok(())
}
pub fn font_get<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:GulletCommand<ET::Token>)
    -> Result<usize,ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"Getting \\font");
    todo!("\\font_get")
}

pub fn fontdimen_assign<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:StomachCommand<ET::Token>,global:bool)
    -> Result<(),ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"Assigning \\fontdimen");
    let o = catch_prim!(gullet.get_int(state) => ("fontdimen",cmd));
    let i = match o.try_into() {
        Ok(i) => i,
        Err(_) => return Err(ErrorInPrimitive{name:"fontdimen",
            msg: Some(format!("Invalid font dimension index")),
            cause:Some(cmd.cause),source:None
        })
    };
    let fontidx = catch_prim!(gullet.get_font(state) => ("fontdimen",cmd));
    catch_prim!(gullet.mouth().skip_eq_char::<ET>(state) => ("fontdimen",cmd));
    let dim = catch_prim!(gullet.get_dim(state) => ("fontdimen",cmd));
    let font = state.fontstore_mut().get_mut(fontidx);
    font.set_dim::<ET::Numbers>(i,dim);
    Ok(())
}

pub fn fontdimen_get<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:GulletCommand<ET::Token>)
    -> Result<ET::Dim,ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"Getting \\fontdimen");
    let o = catch_prim!(gullet.get_int(state) => ("fontdimen",cmd));
    let i = match o.try_into() {
        Ok(i) => i,
        Err(_) => return Err(ErrorInPrimitive{name:"fontdimen",
            msg: Some(format!("Invalid font dimension index")),
            cause:Some(cmd.cause),source:None
        })
    };
    let idx = catch_prim!(gullet.get_font(state) => ("fontdimen",cmd));
    let font = state.fontstore_mut().get_mut(idx);
    Ok(font.get_dim::<ET::Numbers>(i))
}

pub fn gdef<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:StomachCommand<ET::Token>,global:bool,protected:bool,long:bool,outer:bool)
                                                 -> Result<(),ErrorInPrimitive<ET::Token>> {
    def::<ET>(state,gullet,cmd,true,protected,long,outer)
}

pub fn global<ET:EngineType>(stomach:&mut ET::Stomach,state:&mut ET::State,gullet:&mut ET::Gullet,cmd:StomachCommand<ET::Token>,global_:bool,protected_:bool,long_:bool,outer_:bool)
                                                 -> Result<(),ErrorInPrimitive<ET::Token>> {
    debug_log!(trace => "\\global");
    catch_prim!(gullet.mouth().skip_whitespace::<ET>(state) => ("global",cmd));
    match catch_prim!(gullet.get_next_stomach_command(state) => ("global",cmd)) {
        None => file_end_prim!("global",cmd),
        Some(c) => match c.cmd {
            StomachCommandInner::Assignment {name:"global",..} => global::<ET>(stomach,state,gullet,cmd,true,protected_,long_,outer_),
            StomachCommandInner::Assignment {name:"protected",..} => protected::<ET>(stomach,state,gullet,cmd,true,protected_,long_,outer_),
            StomachCommandInner::Assignment {name:"long",..} => long::<ET>(stomach,state,gullet,cmd,true,protected_,long_,outer_),
            StomachCommandInner::Assignment {name:"outer",..} => outer::<ET>(stomach,state,gullet,cmd,true,protected_,long_,outer_),
            StomachCommandInner::Assignment {name:"def",..} => def::<ET>(state,gullet,cmd,true,protected_,long_,outer_),
            StomachCommandInner::Assignment {name:"edef",..} => edef::<ET>(state,gullet,cmd,true,protected_,long_,outer_),
            StomachCommandInner::Assignment {name:"gdef",..} => gdef::<ET>(state,gullet,cmd,true,protected_,long_,outer_),
            StomachCommandInner::Assignment {name:"xdef",..} => xdef::<ET>(state,gullet,cmd,true,protected_,long_,outer_),
            StomachCommandInner::Assignment {index,..} => {
                match stomach.command(index) {
                    None => Err(ErrorInPrimitive{name:"global",msg:Some(format!("Invalid assignment: {}",index)),cause:Some(cmd.cause),source:None}),
                    Some(f) => f(state,gullet,stomach,c,true)
                }
            }
            StomachCommandInner::ValueAssignment {assignment_index,..} => {
                match stomach.command(assignment_index) {
                    None => Err(ErrorInPrimitive{name:"global",msg:Some(format!("Invalid assignment: {}",assignment_index)),cause:Some(cmd.cause),source:None}),
                    Some(f) => f(state,gullet,stomach,c,true)
                 }
            }
            StomachCommandInner::AssignableValue {name,tp:Assignable::Int} =>
                Ok(catch_prim!(assign_primitive_int::<ET>(state,gullet,cmd.clone(),name,true) => ("global",cmd))),
            StomachCommandInner::AssignableValue {name,tp:Assignable::Dim} =>
                Ok(catch_prim!(assign_primitive_dim::<ET>(state,gullet,cmd.clone(),name,true) => ("global",cmd))),
            StomachCommandInner::AssignableValue {name,tp:Assignable::Skip} =>
                Ok(catch_prim!(assign_primitive_skip::<ET>(state,gullet,cmd.clone(),name,true) => ("global",cmd))),
            StomachCommandInner::AssignableValue {name,tp:Assignable::Toks} =>
                Ok(catch_prim!(assign_primitive_toks::<ET>(state,gullet,cmd.clone(),name,true) => ("global",cmd))),
            StomachCommandInner::ValueRegister(u,Assignable::Int) => Ok(catch_prim!(assign_int_register::<ET>(state,gullet,u,cmd.clone(),true) => ("global",cmd))),
            StomachCommandInner::ValueRegister(u,Assignable::Dim) => Ok(catch_prim!(assign_dim_register::<ET>(state,gullet,u,cmd.clone(),true) => ("global",cmd))),
            StomachCommandInner::ValueRegister(u,Assignable::Skip) => Ok(catch_prim!(assign_skip_register::<ET>(state,gullet,u,cmd.clone(),true) => ("global",cmd))),
            StomachCommandInner::ValueRegister(u,Assignable::MuSkip) => Ok(catch_prim!(assign_muskip_register::<ET>(state,gullet,u,cmd.clone(),true) => ("global",cmd))),
            StomachCommandInner::ValueRegister(u,Assignable::Toks) => Ok(catch_prim!(assign_toks_register::<ET>(state,gullet,u,cmd.clone(),true) => ("global",cmd))),
            o => todo!("global: {:?} at {}",o,gullet.mouth().preview(100))
        }
    }
}


pub fn hbox<ET:EngineType>(stomach:&mut ET::Stomach, state:&mut ET::State,gullet:&mut ET::Gullet,cmd:StomachCommand<ET::Token>)
                                         -> Result<Box<dyn FnOnce(&mut ET::Stomach, &mut ET::State, &mut ET::Gullet,Vec<StomachNode<ET>>) -> Option<StomachNode<ET>>>,ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"\\hbox");
    let (to,spread) = match catch_prim!(gullet.get_keywords(state,vec!("spread","to")) => ("hbox",cmd)) {
        None => (None,None),
        Some(s) if s == "to" => {
            let a = catch_prim!(gullet.get_dim(state) => ("hbox",cmd));
            (Some(a),None)
        },
        Some(s) if s == "spread" => {
            let a = catch_prim!(gullet.get_dim(state) => ("hbox",cmd));
            (None,Some(a))
        },
        _ => unreachable!()
    };
    while let Some(next) = catch_prim!(gullet.get_next_stomach_command(state) => ("hbox",cmd)) {
        match next.cmd {
            StomachCommandInner::Space => {},
            StomachCommandInner::Relax => {},
            StomachCommandInner::BeginGroup(_) => {
                state.stack_push(GroupType::Box(BoxMode::H));
                gullet.mouth().push_tokens(state.get_primitive_toks("everyhbox"));
                return Ok(Box::new(move |sto,s,gu,children| {
                    Some(StomachNode::HBox(HBox {
                        children, to, spread,
                        ..Default::default()
                    }))
                }))
            }
            _ => return Err(ErrorInPrimitive{name:"hbox",msg:Some(format!("Expected begin group, found {:?}",next.cause)),cause:Some(cmd.cause),source:None})
        }
    }
    file_end_prim!("hbox",cmd);
}

pub fn hyphenchar_assign<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:StomachCommand<ET::Token>,global:bool)
    -> Result<(),ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"Assigning \\hyphenchar");
    let fontidx = catch_prim!(gullet.get_font(state) => ("hyphenchar",cmd));
    catch_prim!(gullet.mouth().skip_eq_char::<ET>(state) => ("hyphenchar",cmd));
    let i = catch_prim!(gullet.get_int(state) => ("hyphenchar",cmd)).to_i64();
    let font = state.fontstore_mut().get_mut(fontidx);
    debug_log!(debug=>"\\hyphenchar\\{:?} = {:?}",font,i);
    font.set_hyphenchar(i);
    Ok(())
}
pub fn hyphenchar_get<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:GulletCommand<ET::Token>) -> Result<ET::Int,ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"Getting \\hyphenchar");
    let fontidx = catch_prim!(gullet.get_font(state) => ("hyphenchar",cmd));
    let font = state.fontstore_mut().get_mut(fontidx);
    debug_log!(debug=>"\\hyphenchar == {:?}",font.get_hyphenchar());
    Ok(catch_prim!(ET::Int::from_i64::<ET::Token>(font.get_hyphenchar()) => ("hyphenchar",cmd)))
}

pub fn if_<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:GulletCommand<ET::Token>)
    -> Result<bool,ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"if");
    let first = get_if_token::<ET>(state,gullet,cmd.clone(),"if")?;
    let second = get_if_token::<ET>(state,gullet,cmd,"if")?;
    debug_log!(trace=>"if: {:?} == {:?}",first,second);
    Ok(match (first,second) {
        (None,_) | (_,None) => false,
        (Some(f),Some(s)) => match (f.base(),s.base()) {
            (BaseToken::Char(f,_),BaseToken::Char(s,_)) => f == s,
            (BaseToken::CS(_),BaseToken::CS(_)) => true,
            _ => false
        }
    })
}

pub fn ifcase<ET:EngineType>() -> Result<bool,ErrorInPrimitive<ET::Token>> {
    unreachable!("executed in Gullet")
}

pub fn ifcat<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:GulletCommand<ET::Token>)
    -> Result<bool,ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"ifcat");
    let first = get_if_token::<ET>(state,gullet,cmd.clone(),"ifcat")?;
    let second = get_if_token::<ET>(state,gullet,cmd,"ifcat")?;
    debug_log!(trace=>"ifcat: {:?} == {:?}",first,second);
    let first = match first {
        None => return Ok(false),
        Some(first) => match first.base() {
            BaseToken::Char(_,cc) => *cc,
            BaseToken::CS(name) => match state.get_command(name) {
                None => CategoryCode::Escape,
                Some(cmd) => match &*cmd {
                    Command::Char {catcode,..} => *catcode,
                    _ => CategoryCode::Escape
                }
            }
        }
    };
    let second = match second {
        None => return Ok(false),
        Some(first) => match first.base() {
            BaseToken::Char(_,cc) => *cc,
            BaseToken::CS(name) => match state.get_command(name) {
                None => CategoryCode::Escape,
                Some(cmd) => match &*cmd {
                    Command::Char {catcode,..} => *catcode,
                    _ => CategoryCode::Escape
                }
            }
        }
    };
    Ok(first == second)
}

pub fn get_if_token<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:GulletCommand<ET::Token>,name:&'static str)
    -> Result<Option<ET::Token>,ErrorInPrimitive<ET::Token>> {
    // need to be careful not to expand \else and \fi before conditional is done.
    while let Some((t,e)) = catch_prim!(gullet.mouth().get_next::<ET>(state) => (name,cmd)) {
        let cmdo = match t.base() {
            BaseToken::Char(c,CategoryCode::Active) => state.get_ac_command(*c),
            BaseToken::CS(n) => state.get_command(n),
            _ => return Ok(Some(t))
        };
        match cmdo {
            None => return Ok(Some(t)),
            Some(c) => match &*c {
                Command::Conditional{name,index} if e => {
                    catch_prim!(crate::engine::gullet::methods::do_conditional::<ET>(gullet,state,t,name,*index) => (name,cmd));
                },
                Command::Def(d,_) if e => {
                    let v = catch_prim!(d.expand::<ET>(state,gullet.mouth(),c.clone(),Ptr::new(t.clone())) => (name,cmd));
                    if !v.is_empty() {
                        gullet.mouth().push_tokens(v);
                    }
                },
                Command::Gullet {name,..} if e && (*name == "else" || *name == "fi") && (match gullet.current_conditional() {
                    (Some(ConditionalBranch::None(_)),_) => true,
                    _ => false
                }) => {
                    gullet.mouth().requeue(t);
                    return Ok(None)
                }
                Command::Gullet {name,index} if e => {
                    catch_prim!(crate::engine::gullet::methods::do_expandable::<ET>(gullet,state,t,name,*index) => (name,cmd));
                },
                _ => return Ok(Some(t))
            }
        }
    }
    file_end_prim!(name,cmd)
}

pub fn ifeof<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:GulletCommand<ET::Token>)
    -> Result<bool,ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"ifeof");
    let i = catch_prim!(gullet.get_int(state) => ("ifeof",cmd));
    let i : usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => return Err(ErrorInPrimitive{name:"openin",msg:Some(format!("Invalid file number: {}",i)),cause:Some(cmd.cause),source:None})
    };
    if i == 18 { return Ok(false) }
    let f = match state.get_open_in_file(i) {
        None => return Err(ErrorInPrimitive{name:"openin",msg:Some(format!("No in file open at index: {}",i)),cause:Some(cmd.cause),source:None}.into()),
        Some(f) => f
    };
    Ok(f.eof())
}

pub fn ifnum<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:GulletCommand<ET::Token>)
    -> Result<bool,ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"ifnum");
    let i1 = catch_prim!(gullet.get_int(state) => ("ifnum",cmd));
    let rel = match catch_prim!(gullet.get_keywords(state,vec!["<",">","="]) => ("ifnum",cmd)) {
        None => return Err(ErrorInPrimitive{name:"ifnum",msg:Some("Expected one of '<','>','='".to_string()),cause:Some(cmd.cause),source:None}),
        Some(r) => r
    };
    let i2 = catch_prim!(gullet.get_int(state) => ("ifnum",cmd));
    match rel {
        "<" => Ok(i1<i2),
        ">" => Ok(i1>i2),
        "=" => Ok(i1==i2),
        _ => unreachable!()
    }
}

pub fn ifodd<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:GulletCommand<ET::Token>)
    -> Result<bool,ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"ifodd");
    let num = catch_prim!(gullet.get_int(state) => ("ifodd",cmd));
    Ok(num.to_i64() % 2 != 0)
}

pub fn ifx<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:GulletCommand<ET::Token>)
    -> Result<bool,ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"ifx");
    let (t1,exp1) = match catch_prim!(gullet.mouth().get_next::<ET>(state) => ("ifx",cmd)) {
        None => file_end_prim!("ifx",cmd),
        Some(t) => t
    };
    let (t2,exp2) = match catch_prim!(gullet.mouth().get_next::<ET>(state) => ("ifx",cmd)) {
        None => file_end_prim!("ifx",cmd),
        Some(t) => t
    };
    debug_log!(trace=>"ifx: {} == {}?",t1,t2);
    match (t1.base(),t2.base()) {
        (BaseToken::Char(c1,CategoryCode::Active),BaseToken::Char(c2,CategoryCode::Active)) => {
            let cmd1 = state.get_ac_command(*c1);
            let cmd2 = state.get_ac_command(*c2);
            Ok(ifx_eq_cmd(cmd1,t1,exp1,cmd2,t2,exp2))
        }
        (BaseToken::CS(name),BaseToken::Char(c2,CategoryCode::Active)) =>{
            let cmd1 = state.get_command(name);
            let cmd2 = state.get_ac_command(*c2);
            Ok(ifx_eq_cmd(cmd1,t1,exp1,cmd2,t2,exp2))
        }
        (BaseToken::Char(c1,CategoryCode::Active),BaseToken::CS(name)) =>{
            let cmd1 = state.get_ac_command(*c1);
            let cmd2 = state.get_command(name);
            Ok(ifx_eq_cmd(cmd1,t1,exp1,cmd2,t2,exp2))
        }
        (BaseToken::CS(name1),BaseToken::CS(name2)) =>{
            let cmd1 = state.get_command(name1);
            let cmd2 = state.get_command(name2);
            Ok(ifx_eq_cmd(cmd1,t1,exp1,cmd2,t2,exp2))
        }
        (BaseToken::Char(c1,cc1),BaseToken::Char(c2,cc2)) =>
            Ok(c1==c2 && cc1 == cc2),
        (BaseToken::Char(c1,cc1),BaseToken::CS(name)) =>
            Ok(match state.get_command(name).as_deref() {
                Some(Command::Char{char,catcode}) => *char == *c1 && *catcode == *cc1,
                _ => false
            }),
        (BaseToken::CS(name),BaseToken::Char(c2,cc2)) =>
            Ok(match state.get_command(name).as_deref() {
                Some(Command::Char{char,catcode}) => *char == *c2 && *catcode == *cc2,
                _ => false
            })
    }
}

fn ifx_eq_cmd<T:Token>(cmd1:Option<Ptr<Command<T>>>,t1:T,expand1:bool,cmd2:Option<Ptr<Command<T>>>,t2:T,expand2:bool) -> bool {
    debug_log!(debug=>"ifx_eq_cmd: {:?} == {:?}?",cmd1,cmd2);
    if expand1 && expand2 {cmd1 == cmd2}
    else if !expand1 && !expand2 {
        t1 == t2
    }
    else { false }
}


pub fn immediate<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,stomach:&mut ET::Stomach,cmd:StomachCommand<ET::Token>)
                                                 -> Result<(),ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"immediate");
    match catch_prim!(gullet.get_next_stomach_command(state) => ("immediate",cmd)) {
        None => file_end_prim!("immediate",cmd),
        Some(sc) => match sc.cmd {
            StomachCommandInner::Whatsit {name,index} => {
                let fun = match stomach.get_whatsit_cmd(index) {
                    Some(f) => f,
                    None => return Err(ErrorInPrimitive{name:"immediate",msg:None,cause:Some(cmd.cause),source:Some(
                        ImplementationError(format!("Missing implementation for whatsit command {}",name),PhantomData).into()
                    )})
                };
                let wi = catch_prim!(fun(state,gullet,stomach,sc) => ("immediate",cmd));
                catch_prim!((wi.apply)(stomach,state,gullet) => ("immediate",cmd));
                Ok(())
            }
            _ => {
                gullet.mouth().requeue(sc.cause);
                Ok(())
            }
        }
    }
}

pub fn input<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:GulletCommand<ET::Token>)
    -> Result<Vec<ET::Token>,ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"input");
    let filename = catch_prim!(gullet.get_string(state) => ("input",cmd)).to_string();
    debug_log!(trace=>"input: {}",filename);
    let file = state.filesystem().get(&filename);
    debug_log!(trace=>"input resolved: {:?}",file.path());
    if !file.exists() {
        Err(ErrorInPrimitive{name:"input",msg:Some(format!("I can't find file `{}'",filename)),cause:Some(cmd.cause),source:None})
    } else {
        (state.outputs().file_open)(file.path().to_str().unwrap());
        gullet.mouth().push_file(&file);
        Ok(vec!())
    }
}

pub fn lccode_assign<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:StomachCommand<ET::Token>,global:bool)
    -> Result<(),ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"Assigning lower case character");
    let i = catch_prim!(gullet.get_int(state) => ("lccode",cmd));
    let c: ET::Char = match ET::Char::from_i64(i.to_i64()) {
        Some(i) => i,
        None => return Err(ErrorInPrimitive{name:"lccode",msg:Some(format!("Not a valid character: {}",i)),cause:Some(cmd.cause),source:None})
    };
    catch_prim!(gullet.mouth().skip_eq_char::<ET>(state) => ("lccode",cmd));
    let i = catch_prim!(gullet.get_int(state) => ("lccode",cmd));
    let lc: ET::Char = match ET::Char::from_i64(i.to_i64()) {
        Some(i) => i,
        None => return Err(ErrorInPrimitive{name:"lccode",msg:Some(format!("Not a valid character: {}",i)),cause:Some(cmd.cause),source:None})
    };
    debug_log!(debug=>"\\lccode '{}' = {}",c.char_str(),lc.char_str());
    state.set_lccode(c,lc,global);
    Ok(())
}

pub fn lccode_get<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:GulletCommand<ET::Token>)
    -> Result<ET::Int,ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"Getting lower case character");
    let i = catch_prim!(gullet.get_int(state) => ("lccode",cmd));
    let c: ET::Char = match ET::Char::from_i64(i.to_i64()) {
        Some(i) => i,
        None => return Err(ErrorInPrimitive{name:"lccode",msg:Some(format!("Not a valid character: {}",i)),cause:Some(cmd.cause),source:None})
    };
    let v = catch_prim!(ET::Int::from_i64(state.get_lccode(c).to_usize() as i64) => ("lccode",cmd));
    debug_log!(debug=>"\\lccode '{}' == {}",c.char_str(),v);
    Ok(v)
}

pub fn let_<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:StomachCommand<ET::Token>,globally:bool)
    -> Result<(),ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"let");
    let csO = catch_prim!(gullet.mouth().get_next::<ET>(state) => ("let",cmd));
    let cs = match &csO {
        None => file_end_prim!("let",cmd),
        Some((t,_)) => t.base()
    };
    match cs {
        BaseToken::Char(c,CategoryCode::Active) => {
            catch_prim!(gullet.mouth().skip_eq_char::<ET>(state) => ("let",cmd));
            let csO = catch_prim!(gullet.mouth().get_next::<ET>(state) => ("let",cmd));
            let cs = match &csO {
                None => file_end_prim!("let",cmd),
                Some((t,_)) => t.base()
            };
            let cmd = match cs {
                BaseToken::Char(c,CategoryCode::Active) => state.get_ac_command(*c),
                BaseToken::CS(name) => state.get_command(name),
                BaseToken::Char(c,cc) =>
                    Some(Ptr::new(Command::Char{char:*c,catcode:*cc}))
            };
            debug_log!(debug=>"let: {} = {:?}",c,cmd);
            state.set_ac_command(*c,cmd,globally);
        }
        BaseToken::CS(name) => {
            catch_prim!(gullet.mouth().skip_eq_char::<ET>(state) => ("let",cmd));
            let csO = catch_prim!(gullet.mouth().get_next::<ET>(state) => ("let",cmd));
            let cs = match &csO {
                None => file_end_prim!("let",cmd),
                Some((t,_)) => t.base()
            };
            let cmd = match cs {
                BaseToken::Char(c,CategoryCode::Active) => state.get_ac_command(*c),
                BaseToken::CS(name) => state.get_command(name),
                BaseToken::Char(c,cc) =>
                    Some(Ptr::new(Command::Char{char:*c,catcode:*cc}))
            };
            debug_log!(debug=>"let: \\{} = {:?}",name,cmd);
            state.set_command(name.clone(),cmd,globally);
        }
        _ => return Err(ErrorInPrimitive{name:"let",msg:Some("Expected a control sequence".to_string()),cause:Some(csO.unwrap().0),source:None})
    }
    Ok(())
}

pub fn long<ET:EngineType>(stomach:&mut ET::Stomach, state:&mut ET::State,gullet:&mut ET::Gullet,cmd:StomachCommand<ET::Token>,global_:bool,protected_:bool,long_:bool,outer_:bool)
                                                -> Result<(),ErrorInPrimitive<ET::Token>> {
    debug_log!(trace => "\\long");
    match catch_prim!(gullet.get_next_stomach_command(state) => ("long",cmd)) {
        None => file_end_prim!("long",cmd),
        Some(c) => match c.cmd {
            StomachCommandInner::Assignment {name:"global",..} => global::<ET>(stomach,state,gullet,cmd,global_,protected_,true,outer_),
            StomachCommandInner::Assignment {name:"protected",..} => protected::<ET>(stomach,state,gullet,cmd,global_,protected_,true,outer_),
            StomachCommandInner::Assignment {name:"long",..} => long::<ET>(stomach,state,gullet,cmd,global_,protected_,true,outer_),
            StomachCommandInner::Assignment {name:"outer",..} => outer::<ET>(stomach,state,gullet,cmd,global_,protected_,true,outer_),
            StomachCommandInner::Assignment {name:"def",..} => def::<ET>(state,gullet,cmd,global_,protected_,true,outer_),
            StomachCommandInner::Assignment {name:"edef",..} => edef::<ET>(state,gullet,cmd,global_,protected_,true,outer_),
            StomachCommandInner::Assignment {name:"gdef",..} => gdef::<ET>(state,gullet,cmd,global_,protected_,true,outer_),
            StomachCommandInner::Assignment {name:"xdef",..} => xdef::<ET>(state,gullet,cmd,global_,protected_,true,outer_),
            _ => return Err(ErrorInPrimitive{name:"long",msg:Some("Expected a macro definition after \\long".to_string()),cause:Some(cmd.cause),source:None})
        }
    }
}

pub fn lowercase<ET:EngineType>(stomach:&mut ET::Stomach, state:&mut ET::State,gullet:&mut ET::Gullet,cmd:StomachCommand<ET::Token>)
                                         -> Result<(),ErrorInPrimitive<ET::Token>> {
    debug_log!(trace => "\\lowercase");
    match catch_prim!(gullet.get_next_stomach_command(state) => ("lowercase",cmd)) {
        None => file_end_prim!("lowercase",cmd),
        Some(sc) => match sc.cmd {
            StomachCommandInner::BeginGroup(_) => (),
            _ => return Err(ErrorInPrimitive{name:"lowercase",msg:Some("Expected a begin group after \\lowercase".to_string()),cause:Some(cmd.cause),source:None})
        }
    }
    let mut ingroups = 1;
    let mut ret = vec!();
    while let Some((next,_)) = catch_prim!(gullet.mouth().get_next::<ET>(state) => ("lowercase",cmd)) {
        match next.base() {
            BaseToken::Char(c,CategoryCode::BeginGroup) => {
                ingroups += 1;
                let nc = state.get_lccode(*c);
                ret.push(ET::Token::new(BaseToken::Char(nc,CategoryCode::BeginGroup),None));
            },
            BaseToken::Char(c,CategoryCode::EndGroup) => {
                ingroups -= 1;
                if ingroups == 0 {
                    break;
                } else {
                    let nc = state.get_lccode(*c);
                    ret.push(ET::Token::new(BaseToken::Char(nc,CategoryCode::EndGroup),None));
                }
            },
            BaseToken::Char(c,cc) => {
                let nc = state.get_lccode(*c);
                ret.push(ET::Token::new(BaseToken::Char(nc,*cc),None));
            },
            _ => ret.push(next)
        }
    }
    gullet.mouth().push_tokens(ret);
    Ok(())
}

pub fn mathchardef<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:StomachCommand<ET::Token>,globally:bool)
    -> Result<(),ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"mathchardef");
    let csO = catch_prim!(gullet.mouth().get_next::<ET>(state) => ("mathchardef",cmd));
    let cs = match &csO {
        None => file_end_prim!("mathchardef",cmd),
        Some((t,_)) => t.base()
    };
    match cs {
        BaseToken::Char(_,CategoryCode::Active) => (),
        BaseToken::CS(_) => (),
        _ => return Err(ErrorInPrimitive{name:"mathchardef",msg:Some(format!("Command expected after \\mathchardef")),cause:Some(csO.unwrap().0),source:None})
    }
    catch_prim!(gullet.mouth().skip_eq_char::<ET>(state) => ("mathchardef",cmd));
    let i = catch_prim!(gullet.get_int(state) => ("mathchardef",cmd)).to_i64();
    if i < 0 {
        return Err(ErrorInPrimitive{name:"mathchardef",msg:Some(format!("Invalid math char: {}",i)),cause:Some(csO.unwrap().0),source:None})
    }
    match cs {
        BaseToken::Char(c,_) => state.set_ac_command(*c,Some(Ptr::new(Command::MathChar(i as u32))),globally),
        BaseToken::CS(name) => state.set_command(name.clone(),Some(Ptr::new(Command::MathChar(i as u32))),globally)
    }
    Ok(())
}

pub fn meaning<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:GulletCommand<ET::Token>)
    -> Result<Vec<ET::Token>,ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"meaning");
    match catch_prim!(gullet.mouth().get_next::<ET>(state) => ("meaning",cmd)) {
        None => file_end_prim!("meaning",cmd),
        Some((_,false)) => {
            match state.get_escapechar() {
                None => Ok(string_to_tokens("relax".as_bytes())),
                Some(c) => {
                    let mut string = vec!();
                    for u in c.as_bytes() {string.push(u)}
                    for u in "relax".as_bytes() {string.push(*u)}
                    Ok(string_to_tokens(&string))
                }
            }
        }
        Some((t,_)) => match t.base() {
            BaseToken::CS(name) => Ok(meaning_cmd::<ET>(state.get_command(name),state)),
            BaseToken::Char(c,CategoryCode::Active) => Ok(meaning_cmd::<ET>(state.get_ac_command(*c),state)),
            BaseToken::Char(c,cc) => Ok(meaning_char(*c,*cc)),
        }
    }
}

pub fn meaning_char<T:Token>(c:T::Char,cc:CategoryCode) -> Vec<T> {
    match cc {
        CategoryCode::BeginGroup => string_to_tokens(&format!("begin-group character {}",c.char_str()).as_bytes()),
        CategoryCode::EndGroup => string_to_tokens(&format!("end-group character {}",c.char_str()).as_bytes()),
        CategoryCode::MathShift => string_to_tokens(&format!("math shift character {}",c.char_str()).as_bytes()),
        CategoryCode::AlignmentTab => string_to_tokens(&format!("alignment tab character {}",c.char_str()).as_bytes()),
        CategoryCode::Parameter => string_to_tokens(&format!("macro parameter character {}",c.char_str()).as_bytes()),
        CategoryCode::Superscript => string_to_tokens(&format!("superscript character {}",c.char_str()).as_bytes()),
        CategoryCode::Subscript => string_to_tokens(&format!("subscript character {}",c.char_str()).as_bytes()),
        CategoryCode::Space => string_to_tokens(&format!("blank space {}",c.char_str()).as_bytes()),
        CategoryCode::Letter => string_to_tokens(&format!("the letter {}",c.char_str()).as_bytes()),
        _ => string_to_tokens(&format!("the character {}",c).as_bytes()),
    }
}

pub fn meaning_cmd<ET:EngineType>(cmd:Option<Ptr<Command<ET::Token>>>,state:&ET::State) -> Vec<ET::Token> {
    match cmd {
        None => string_to_tokens("undefined".as_bytes()),
        Some(cmd) => {
            match &*cmd {
                Command::Def(d,_) => {
                    let esc = match state.get_escapechar() {
                        None => vec!(),
                        Some(c) => c.as_bytes()
                    };
                    let mut ret = vec!();
                    if d.protected {
                        ret.extend(esc.clone());
                        ret.extend("protected ".as_bytes());
                    }
                    if d.long {
                        ret.extend(esc.clone());
                        ret.extend("long ".as_bytes());
                    }
                    if d.outer {
                        ret.extend(esc.clone());
                        ret.extend("outer ".as_bytes());
                    }
                    ret.extend("macro:".as_bytes());
                    let mut i = 0;
                    for s in &d.signature {
                        match s {
                            ParamToken::Token(t) => ret.extend(tokens_to_string(vec!(t.clone()),state.get_escapechar(),state.get_catcode_scheme()).as_bytes()),
                            ParamToken::Param => {
                                i += 1;
                                ret.extend(format!("#{}",i).as_bytes());
                            }
                        }
                    }
                    if d.endswithbrace { ret.push(b'#'); }
                    ret.push(b'-'); ret.push(b'>');
                    for t in &d.replacement {
                        match t {
                            ExpToken::Token(t) => ret.extend(tokens_to_string(vec!(t.clone()),state.get_escapechar(),state.get_catcode_scheme()).as_bytes()),
                            ExpToken::ParamToken(t) => ret.extend(tokens_to_string(vec!(t.clone(),t.clone()),state.get_escapechar(),state.get_catcode_scheme()).as_bytes()),
                            ExpToken::Param(t,i) => {
                                ret.extend(tokens_to_string(vec!(t.clone()),state.get_escapechar(),state.get_catcode_scheme()).as_bytes());
                                ret.extend(i.to_string().as_bytes());
                            }
                        }
                    }
                    debug_log!(debug=>"meaning_cmd: {}",std::str::from_utf8(&ret).unwrap());
                    string_to_tokens(&ret)
                }
                Command::Stomach {name,..} => {
                    match state.get_escapechar() {
                        None => string_to_tokens(name.as_bytes()),
                        Some(c) => {
                            let mut string = vec!();
                            for u in c.as_bytes() {string.push(u)}
                            for u in name.as_bytes() {string.push(*u)}
                            string_to_tokens(&string)
                        }
                    }
                }
                Command::OpenBox {name,..} => {
                    match state.get_escapechar() {
                        None => string_to_tokens(name.as_bytes()),
                        Some(c) => {
                            let mut string = vec!();
                            for u in c.as_bytes() {string.push(u)}
                            for u in name.as_bytes() {string.push(*u)}
                            string_to_tokens(&string)
                        }
                    }
                }
                Command::AssignableValue {name,..} => {
                    match state.get_escapechar() {
                        None => string_to_tokens(name.as_bytes()),
                        Some(c) => {
                            let mut string = vec!();
                            for u in c.as_bytes() {string.push(u)}
                            for u in name.as_bytes() {string.push(*u)}
                            string_to_tokens(&string)
                        }
                    }
                }
                Command::Value {name,..} => {
                    match state.get_escapechar() {
                        None => string_to_tokens(name.as_bytes()),
                        Some(c) => {
                            let mut string = vec!();
                            for u in c.as_bytes() {string.push(u)}
                            for u in name.as_bytes() {string.push(*u)}
                            string_to_tokens(&string)
                        }
                    }
                }
                Command::ValueRegister {tp:Assignable::Int,index} => {
                    let mut string = vec!();
                    match state.get_escapechar() {
                        None => (),
                        Some(c) => {
                            for u in c.as_bytes() {string.push(u)}
                        }
                    }
                    for u in "count".as_bytes() {string.push(*u)}
                    for u in index.to_string().as_bytes() {string.push(*u)}
                    string_to_tokens(&string)
                }
                Command::ValueRegister {tp:Assignable::Dim,index} => {
                    let mut string = vec!();
                    match state.get_escapechar() {
                        None => (),
                        Some(c) => {
                            for u in c.as_bytes() {string.push(u)}
                        }
                    }
                    for u in "dimen".as_bytes() {string.push(*u)}
                    for u in index.to_string().as_bytes() {string.push(*u)}
                    string_to_tokens(&string)
                }
                Command::ValueRegister {tp:Assignable::Skip,index} => {
                    let mut string = vec!();
                    match state.get_escapechar() {
                        None => (),
                        Some(c) => {
                            for u in c.as_bytes() {string.push(u)}
                        }
                    }
                    for u in "skip".as_bytes() {string.push(*u)}
                    for u in index.to_string().as_bytes() {string.push(*u)}
                    string_to_tokens(&string)
                }
                Command::ValueRegister {tp:Assignable::MuSkip,index} => {
                    let mut string = vec!();
                    match state.get_escapechar() {
                        None => (),
                        Some(c) => {
                            for u in c.as_bytes() {string.push(u)}
                        }
                    }
                    for u in "muskip".as_bytes() {string.push(*u)}
                    for u in index.to_string().as_bytes() {string.push(*u)}
                    string_to_tokens(&string)
                }
                Command::ValueRegister {tp:Assignable::Font,index} => {
                    let mut string = vec!();
                    for u in "select font ".as_bytes() {string.push(*u)}
                    for u in state.fontstore().get(*index).to_string().as_bytes() {string.push(*u)}
                    string_to_tokens(&string)
                }
                Command::ValueRegister {..} => todo!("meaning_cmd: ValueRegister"),
                Command::ValueAssignment {name,..} => {
                    match state.get_escapechar() {
                        None => string_to_tokens(name.as_bytes()),
                        Some(c) => {
                            let mut string = vec!();
                            for u in c.as_bytes() {string.push(u)}
                            for u in name.as_bytes() {string.push(*u)}
                            string_to_tokens(&string)
                        }
                    }
                }
                Command::Assignment {name,..} => {
                    match state.get_escapechar() {
                        None => string_to_tokens(name.as_bytes()),
                        Some(c) => {
                            let mut string = vec!();
                            for u in c.as_bytes() {string.push(u)}
                            for u in name.as_bytes() {string.push(*u)}
                            string_to_tokens(&string)
                        }
                    }
                }
                Command::Relax => {
                    match state.get_escapechar() {
                        None => string_to_tokens("relax".as_bytes()),
                        Some(c) => {
                            let mut string = vec!();
                            for u in c.as_bytes() {string.push(u)}
                            for u in "relax".as_bytes() {string.push(*u)}
                            string_to_tokens(&string)
                        }
                    }
                }
                Command::Conditional {name,..} => {
                    match state.get_escapechar() {
                        None => string_to_tokens(name.as_bytes()),
                        Some(c) => {
                            let mut string = vec!();
                            for u in c.as_bytes() {string.push(u)}
                            for u in name.as_bytes() {string.push(*u)}
                            string_to_tokens(&string)
                        }
                    }
                }
                Command::Gullet {name,..} => {
                    match state.get_escapechar() {
                        None => string_to_tokens(name.as_bytes()),
                        Some(c) => {
                            let mut string = vec!();
                            for u in c.as_bytes() {string.push(u)}
                            for u in name.as_bytes() {string.push(*u)}
                            string_to_tokens(&string)
                        }
                    }
                }
                Command::Char {char,catcode} => meaning_char(*char,*catcode),
                Command::MathChar(index) => {
                    let mut string = vec!();
                    match state.get_escapechar() {
                        None => (),
                        Some(c) => {
                            for u in c.as_bytes() {string.push(u)}
                        }
                    }
                    for u in "mathchar\"".as_bytes() {string.push(*u)}
                    for u in format!("{:X}", index).as_bytes() {string.push(*u)}
                    string_to_tokens(&string)
                }
                Command::Whatsit {name,..} => {
                    match state.get_escapechar() {
                        None => string_to_tokens(name.as_bytes()),
                        Some(c) => {
                            let mut string = vec!();
                            for u in c.as_bytes() {string.push(u)}
                            for u in name.as_bytes() {string.push(*u)}
                            string_to_tokens(&string)
                        }
                    }
                }
            }
        }
    }
}

pub fn message<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:StomachCommand<ET::Token>)
    -> Result<(),ErrorInPrimitive<ET::Token>> {
    debug_log!(debug=>"message");
    catch_prim!(gullet.mouth().skip_whitespace::<ET>(state) => ("message",cmd));
    let msg = String::from_utf8(catch_prim!(gullet.get_braced_string(state) => ("message",cmd))).unwrap();
    (state.outputs().message)(&msg);
    Ok(())
}

pub fn month<ET:EngineType>(state:&mut ET::State,cmd:GulletCommand<ET::Token>) -> Result<ET::Int,ErrorInPrimitive<ET::Token>> {
    Ok(catch_prim!(ET::Int::from_i64(
        state.get_start_time().month() as i64
    ) => ("month",cmd)))
}

pub fn multiply<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:StomachCommand<ET::Token>,global:bool)
                                                   -> Result<(),ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"\\multiply");
    catch_prim!(gullet.mouth().skip_whitespace::<ET>(state) => ("multiply",cmd));
    match catch_prim!(gullet.get_next_stomach_command(state) => ("multiply",cmd)) {
        None => file_end_prim!("multiply",cmd),
        Some(ocmd) => match ocmd.cmd {
            StomachCommandInner::ValueRegister(u,Assignable::Int) => {
                catch_prim!(gullet.get_keyword(state,"by") => ("multiply",cmd));
                debug_log!(debug => "  \\count{}",u);
                let i = catch_prim!(gullet.get_int(state) => ("multiply",cmd));
                let ov = state.get_int_register(u);
                debug_log!(debug => "  =={}*{}",ov,i);
                let nv : ET::Int = ov * i;
                debug_log!(debug => "  ={}",nv);
                state.set_int_register(u,nv,global);
                return Ok(())
            }
            StomachCommandInner::AssignableValue {name,tp:crate::tex::commands::Assignable::Int} => {
                debug_log!(debug => "  \\{}",name);
                catch_prim!(gullet.get_keyword(state,"by") => ("multiply",cmd));
                let ov = state.get_primitive_int(name);
                let i = catch_prim!(gullet.get_int(state) => ("multiply",cmd));
                debug_log!(debug => "  =={}*{}",ov,i);
                let nv : ET::Int = ov * i;
                debug_log!(debug => "  ={}",nv);
                state.set_primitive_int(name,nv,global);
                return Ok(())
            }
            StomachCommandInner::AssignableValue {name,tp:crate::tex::commands::Assignable::Dim} => {
                debug_log!(debug => "  \\{}",name);
                catch_prim!(gullet.get_keyword(state,"by") => ("multiply",cmd));
                let ov = state.get_primitive_dim(name);
                let i = catch_prim!(gullet.get_int(state) => ("multiply",cmd));
                debug_log!(debug => "  =={}*{}",ov,i);
                let nv : ET::Dim = ov.tex_mult(i.to_i64() as f64);
                debug_log!(debug => "  ={}",nv);
                state.set_primitive_dim(name,nv,global);
                return Ok(())
            }
            StomachCommandInner::ValueAssignment {name:"count",..} => {
                let i = catch_prim!(gullet.get_int(state) => ("multiply",cmd));
                let u = match i.try_into() {
                    Ok(u) => u,
                    _ => return Err(ErrorInPrimitive{name:"count",msg:Some(format!("Not a valid register")),cause:Some(cmd.cause),source:None})
                };
                catch_prim!(gullet.get_keyword(state,"by") => ("multiply",cmd));
                debug_log!(debug => "  \\count{}",u);
                let i = catch_prim!(gullet.get_int(state) => (stringify!(name),cmd));
                let ov = state.get_int_register(u);
                debug_log!(debug => "  =={}*{}",ov,i);
                let nv : ET::Int = ov * i;
                debug_log!(debug => "  ={}",nv);
                state.set_int_register(u,nv,global);
                return Ok(())
            }
            o => return Err(ErrorInPrimitive{name:"multiply",msg:Some(format!("expected register after \\multiply;got:{:?}",o)),cause:Some(cmd.cause),source:None})
        }
    }
}


pub fn muskip_assign<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:StomachCommand<ET::Token>,global:bool)
    -> Result<(),ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"Assigning \\muskip");
    let i = catch_prim!(gullet.get_int(state) => ("muskip",cmd));
    let i:usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => return Err(ErrorInPrimitive{name:"muskip",msg:Some(format!("Not a valid register: {}",i)),cause:Some(cmd.cause),source:None})
    };
    catch_prim!(gullet.mouth().skip_eq_char::<ET>(state) => ("muskip",cmd));
    let v = catch_prim!(gullet.get_muskip(state) => ("muskip",cmd));
    debug_log!(debug=>"\\muskip{} = {}",i,v);
    state.set_muskip_register(i,v,global);
    Ok(())
}

pub fn muskip_get<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:GulletCommand<ET::Token>) ->
                                                                                                           Result<MuSkip<ET::MuDim,ET::MuStretchShrinkDim>,ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"Getting \\muskip");
    let i = catch_prim!(gullet.get_int(state) => ("muskip",cmd));
    let i:usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => return Err(ErrorInPrimitive{name:"muskip",msg:Some(format!("Not a valid register: {}",i)),cause:Some(cmd.cause),source:None})
    };
    let v = state.get_muskip_register(i);
    debug_log!(debug=>"\\muskip{} == {}",i,v);
    Ok(v)
}

pub fn muskipdef<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:StomachCommand<ET::Token>,global:bool)
                                                     -> Result<(),ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"muskipdef");
    let name = catch_prim!(gullet.get_control_sequence(state) => ("muskipdef",cmd));
    match &name {
        BaseToken::Char(c,_) => {
            state.set_ac_command(*c,Some(Ptr::new(Command::Relax)),false)
        }
        BaseToken::CS(name) => {
            state.set_command(name.clone(),Some(Ptr::new(Command::Relax)),false)
        }
    }
    catch_prim!(gullet.mouth().skip_eq_char::<ET>(state) => ("muskipdef",cmd));
    let num = catch_prim!(gullet.get_int(state) => ("muskipdef",cmd));
    if num.to_i64() < 0 {
        return Err(ErrorInPrimitive{name:"muskipdef",msg:Some(format!("Invalid muskip register index: {}",num)),cause:Some(cmd.cause),source:None})
    }
    let num = num.to_i64() as usize;
    match name {
        BaseToken::Char(c,_) => {
            debug_log!(debug=>"\\muskipdef: {} = \\muskip{}",c.char_str(),num);
            state.set_ac_command(c, Some(Ptr::new(Command::ValueRegister{index:num,tp:Assignable::MuSkip })), global);
        }
        BaseToken::CS(name) => {
            debug_log!(debug=>"\\muskipdef: \\{} = {}",name,num);
            state.set_command(name, Some(Ptr::new(Command::ValueRegister{index:num,tp:Assignable::MuSkip})), global);
        }
    }
    Ok(())
}


pub fn newlinechar_assign<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:StomachCommand<ET::Token>,global:bool)
    -> Result<(),ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"Assigning \\newlinechar");
    catch_prim!(gullet.mouth().skip_eq_char::<ET>(state) => ("newlinechar",cmd));
    let i = catch_prim!(gullet.get_int(state) => ("newlinechar",cmd));
    let c = match i.to_i64() {
        -1|255 => None,
        j => match ET::Char::from_i64(j) {
            Some(c) => Some(c),
            None => return Err(ErrorInPrimitive{name:"newlinechar",msg:Some(format!("Not a valid character: {}",j)),cause:Some(cmd.cause),source:None})
        }
    };
    debug_log!(debug=>"\\newlinechar = {:?}",c.map(|c| c.char_str()));
    state.set_newlinechar(c,global);
    Ok(())
}
pub fn newlinechar_get<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:GulletCommand<ET::Token>)
    -> Result<ET::Int,ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"Getting \\newlinechar");
    let c = match state.get_newlinechar() {
        None => -1,
        Some(c) => c.to_usize() as i64
    };
    debug_log!(debug=>"\\newlinechar == {}",c);
    Ok(ET::Int::from_i64::<ET::Token>(c).unwrap())
}

/// invariant: adds token as nonexpanded to the gullet iff the original token was expandable
/// in the first place
pub fn noexpand<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:GulletCommand<ET::Token>)
    -> Result<Vec<ET::Token>,ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"\\noexpand");
    match catch_prim!(gullet.mouth().get_next::<ET>(state) => ("noexpand",cmd)) {
        None => file_end_prim!("noexpand",cmd),
        Some((t,_)) => match t.base() {
            BaseToken::Char(c,CategoryCode::Active) => {
                match state.get_ac_command(*c) {
                    None => gullet.mouth().requeue(t),
                    Some(ac) => {
                        match &*ac {
                            Command::Def(_,_) => gullet.mouth().push_noexpand(t),
                            Command::Gullet {..} => gullet.mouth().push_noexpand(t),
                            Command::Conditional {..} => gullet.mouth().push_noexpand(t),
                            _ => gullet.mouth().requeue(t)
                        }
                    }
                }
            }
            BaseToken::CS(name) => {
                match state.get_command(name) {
                    None => gullet.mouth().requeue(t),
                    Some(ac) => {
                        match &*ac {
                            Command::Def(_,_) => gullet.mouth().push_noexpand(t),
                            Command::Gullet {..} => gullet.mouth().push_noexpand(t),
                            Command::Conditional {..} => gullet.mouth().push_noexpand(t),
                            _ => gullet.mouth().requeue(t)
                        }
                    }
                }
            }
            _ => gullet.mouth().requeue(t)
        }
    }
    Ok(vec!())
}

pub fn number<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:GulletCommand<ET::Token>)
    -> Result<Vec<ET::Token>,ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"\\number");
    let num = catch_prim!(gullet.get_int(state) => ("number",cmd));
    let ret =
        num.to_i64().to_string().as_bytes().into_iter().map(|c| ET::Token::new(BaseToken::Char(ET::Char::from(*c),
            if *c == 32 {CategoryCode::Space} else {CategoryCode::Other}
        ),None)).collect();
    Ok(ret)
}


pub fn openin<ET:EngineType>(state: &mut ET::State, gullet:&mut ET::Gullet, stomach:&mut ET::Stomach, cmd:StomachCommand<ET::Token>)
    -> Result<(), ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"\\openin");
    let i = catch_prim!(gullet.get_int(state) => ("openin",cmd));
    let i : usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => return Err(ErrorInPrimitive{name:"openin",msg:Some(format!("Invalid file number: {}",i)),cause:Some(cmd.cause),source:None})
    };
    catch_prim!(gullet.mouth().skip_eq_char::<ET>(state) => ("openin",cmd));
    let filename = catch_prim!(gullet.get_string(state) => ("openin",cmd)).to_string();
    let f = state.filesystem().get(&filename);
    state.file_openin(i,f); // TODO error?
    Ok(())
}

pub fn openout<ET:EngineType>(state: &mut ET::State, gullet:&mut ET::Gullet, stomach:&mut ET::Stomach, cmd:StomachCommand<ET::Token>)
    -> Result<Whatsit<ET>, ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"\\openout");
    let i = catch_prim!(gullet.get_int(state) => ("openout",cmd));
    let i : usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => return Err(ErrorInPrimitive{name:"openout",msg:Some(format!("Invalid file number: {}",i)),cause:Some(cmd.cause),source:None})
    };
    catch_prim!(gullet.mouth().skip_eq_char::<ET>(state) => ("openout",cmd));
    let filename = catch_prim!(gullet.get_string(state) => ("openout",cmd)).to_string();
    let apply = Box::new(move |_stomach:&mut ET::Stomach,state:&mut ET::State,_gullet:&mut ET::Gullet| {
        let f = state.filesystem().get(&filename);
        state.file_openout(i,f); // TODO error?
        Ok(())
    });
    Ok(Whatsit { apply })
}

pub fn or<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:GulletCommand<ET::Token>)
    -> Result<Vec<ET::Token>,ErrorInPrimitive<ET::Token>> {
    match gullet.current_conditional() {
        (Some(ConditionalBranch::Case(_,_)),i) =>
            catch_prim!(crate::engine::gullet::methods::else_loop::<ET>(gullet,state,"ifcase",i) => ("or",cmd)),
        _ => return Err(ErrorInPrimitive{name:"or",msg:Some("Not in an \\ifcase".to_string()),cause:Some(cmd.cause),source:None}),
    }
    Ok(vec![])
}

pub fn outer<ET:EngineType>(stomach:&mut ET::Stomach,state:&mut ET::State,gullet:&mut ET::Gullet,cmd:StomachCommand<ET::Token>,global_:bool,protected_:bool,long_:bool,outer_:bool)
                                                      -> Result<(),ErrorInPrimitive<ET::Token>> {
    debug_log!(trace => "\\outer");
    match catch_prim!(gullet.get_next_stomach_command(state) => ("outer",cmd)) {
        None => file_end_prim!("outer",cmd),
        Some(c) => match c.cmd {
            StomachCommandInner::Assignment {name:"global",..} => global::<ET>(stomach,state,gullet,cmd,global_,protected_,long_,true),
            StomachCommandInner::Assignment {name:"protected",..} => protected::<ET>(stomach,state,gullet,cmd,global_,protected_,long_,true),
            StomachCommandInner::Assignment {name:"long",..} => long::<ET>(stomach,state,gullet,cmd,global_,protected_,long_,true),
            StomachCommandInner::Assignment {name:"outer",..} => outer::<ET>(stomach,state,gullet,cmd,global_,protected_,long_,true),
            StomachCommandInner::Assignment {name:"def",..} => def::<ET>(state,gullet,cmd,global_,protected_,long_,true),
            StomachCommandInner::Assignment {name:"edef",..} => edef::<ET>(state,gullet,cmd,global_,protected_,long_,true),
            StomachCommandInner::Assignment {name:"gdef",..} => gdef::<ET>(state,gullet,cmd,global_,protected_,long_,true),
            StomachCommandInner::Assignment {name:"xdef",..} => xdef::<ET>(state,gullet,cmd,global_,protected_,long_,true),
            _ => return Err(ErrorInPrimitive{name:"outer",msg:Some("Expected a macro definition after \\outer".to_string()),cause:Some(cmd.cause),source:None})
        }
    }
}

pub fn par<ET:EngineType>(_stomach:&mut ET::Stomach,state:&mut ET::State,_cmd:StomachCommand<ET::Token>) -> Result<(),ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"par");
    if state.mode().is_vertical() {Ok(())} else {
        todo!("par in horizontal mode")
    }
}

pub fn read<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:StomachCommand<ET::Token>,globally:bool)
    -> Result<(),ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"read");
    let i = catch_prim!(gullet.get_int(state) => ("read",cmd));
    let i : usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => return Err(ErrorInPrimitive{name:"read",msg:Some(format!("Invalid file number: {}",i)),cause:Some(cmd.cause),source:None})
    };
    let file = match state.get_open_in_file(i) {
        None => return Err(ErrorInPrimitive{name:"read",msg:Some(format!("File {} not open for reading",i)),cause:Some(cmd.cause),source:None}),
        Some(f) => f
    };
    if !catch_prim!(gullet.get_keyword(state,"to") => ("read",cmd)) {
        return Err(ErrorInPrimitive{name:"read",msg:Some("Expected 'to' after \\read".to_string()),cause:Some(cmd.cause),source:None})
    }
    let newcmd = catch_prim!(gullet.get_control_sequence(state) => ("read",cmd));
    let ret = catch_prim!(file.read::<ET>(state) => ("read",cmd)).into_iter().map(|tk| ExpToken::Token(tk)).collect();
    let def = Def {
        protected: false,
        long: false,
        outer: false,
        endswithbrace:false,
        replacement: ret,
        arity:0,
        signature:vec!()
    };
    match newcmd {
        BaseToken::CS(name) => state.set_command(name,Some(Ptr::new(
            Command::Def(def,cmd.cause.clone())
        )),globally),
        BaseToken::Char(c,_) => state.set_ac_command(c,Some(Ptr::new(
            Command::Def(def,cmd.cause.clone())
        )),globally)
    }
    Ok(())
}

pub fn romannumeral<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:GulletCommand<ET::Token>)
    -> Result<Vec<ET::Token>,ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"romannumeral");
    let mut num = catch_prim!(gullet.get_int(state) => ("romannumeral",cmd)).to_i64();
    if num <= 0 {
        return Ok(vec!())
    }
    let mut ret : Vec<u8> = vec!();
    while num >= 1000 {
        num -= 1000;
        ret.push(b'm');
    }
    if num >= 900 {
        num -= 900;
        ret.push(b'c');
        ret.push(b'm');
    }
    if num >= 500 {
        num -= 500;
        ret.push(b'd');
    }
    if num >= 400 {
        num -= 400;
        ret.push(b'c');
        ret.push(b'd');
    }
    while num >= 100 {
        num -= 100;
        ret.push(b'c');
    }
    if num >= 90 {
        num -= 90;
        ret.push(b'x');
        ret.push(b'c');
    }
    if num >= 50 {
        num -= 50;
        ret.push(b'l');
    }
    if num >= 40 {
        num -= 40;
        ret.push(b'x');
        ret.push(b'l');
    }
    while num >= 10 {
        num -= 10;
        ret.push(b'x');
    }
    if num >= 9 {
        num -= 9;
        ret.push(b'i');
        ret.push(b'x');
    }
    if num >= 5 {
        num -= 5;
        ret.push(b'v');
    }
    if num >= 4 {
        num -= 4;
        ret.push(b'i');
        ret.push(b'v');
    }
    while num >= 1 {
        num -= 1;
        ret.push(b'i');
    }
    Ok(string_to_tokens(&ret))
}

pub fn setbox<ET:EngineType>(state:&mut ET::State, gullet:&mut ET::Gullet, stomach:&mut ET::Stomach, cmd:StomachCommand<ET::Token>, global:bool)
    -> Result<(),ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"\\setbox");
    let i = catch_prim!(gullet.get_int(state) => ("setbox",cmd)).to_i64();
    if i < 0  {
        return Err(ErrorInPrimitive{name:"setbox",msg:Some(format!("Invalid box number: {}",i)),cause:Some(cmd.cause),source:None})
    }
    catch_prim!(gullet.mouth().skip_eq_char::<ET>(state) => ("setbox",cmd));
    match catch_prim!(gullet.get_next_stomach_command(state) => ("setbox",cmd)) {
        None => file_end_prim!("setbox",cmd),
        Some(c) => match c.cmd {
            StomachCommandInner::OpenBox {index,name,mode} => {
                let f = match stomach.get_box_cmd(index) {
                    None => return Err(ErrorInPrimitive{name:"setbox",msg:None,cause:Some(cmd.cause),source:Some(
                        ImplementationError(format!("Command not implemented: {}",name),PhantomData).into()
                    )}),
                    Some(f) => catch_prim!(f(state,gullet,stomach,c) => ("setbox",cmd))
                };
                state.box_stack_mut().push(OpenBox::Box {list:vec!(),mode,on_close:Box::new(move |sto,s,gu,v| {
                    let bx = match f(sto,s,gu,v) {
                        Some(r) => {r}
                        None => {todo!("make void box")}
                    };
                    todo!("set box register");
                    //s.set_box_register(i as usize,Some(bx));
                    None
                })});
                Ok(())
            }
            _ => Err(ErrorInPrimitive{name:"setbox",msg:Some(format!("Box expected: {}",c.cause)),cause:Some(cmd.cause),source:None})
        }
    }

}

pub fn sfcode_assign<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:StomachCommand<ET::Token>,global:bool)
    -> Result<(),ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"Assigning space factor code");
    let i = catch_prim!(gullet.get_int(state) => ("sfcode",cmd));
    let c: ET::Char = match ET::Char::from_i64(i.to_i64()) {
        Some(i) => i,
        None => return Err(ErrorInPrimitive{name:"catcode",msg:Some(format!("Not a valid character: {}",i)),cause:Some(cmd.cause),source:None})
    };
    catch_prim!(gullet.mouth().skip_eq_char::<ET>(state) => ("catcode",cmd));
    let v = catch_prim!(gullet.get_int(state) => ("catcode",cmd));
    debug_log!(debug=>"\\sfcode '{}' = {}",c.char_str(),v);
    state.set_sfcode(c,v,global);
    Ok(())
}
pub fn sfcode_get<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:GulletCommand<ET::Token>)
    -> Result<ET::Int,ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"Getting space factor code");
    let i = catch_prim!(gullet.get_int(state) => ("sfcode",cmd));
    let c: ET::Char = match ET::Char::from_i64(i.to_i64()) {
        Some(i) => i,
        None => return Err(ErrorInPrimitive{name:"sfcode",msg:Some(format!("Not a valid character: {}",i)),cause:Some(cmd.cause),source:None})
    };
    let v = state.get_sfcode(&c);
    debug_log!(debug=>"\\sfcode '{}' == {}",c.char_str(),v);
    Ok(v)
}

pub fn skip_assign<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:StomachCommand<ET::Token>,global:bool)
    -> Result<(),ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"Assigning \\skip");
    let i = catch_prim!(gullet.get_int(state) => ("skip",cmd));
    let i:usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => return Err(ErrorInPrimitive{name:"skip",msg:Some(format!("Not a valid register: {}",i)),cause:Some(cmd.cause),source:None})
    };
    catch_prim!(gullet.mouth().skip_eq_char::<ET>(state) => ("skip",cmd));
    let v = catch_prim!(gullet.get_skip(state) => ("skip",cmd));
    debug_log!(debug=>"\\skip{} = {}",i,v);
    state.set_skip_register(i,v,global);
    Ok(())
}

pub fn skip_get<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:GulletCommand<ET::Token>)
    -> Result<Skip<ET::SkipDim>,ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"Getting \\skip");
    let i = catch_prim!(gullet.get_int(state) => ("skip",cmd));
    let i:usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => return Err(ErrorInPrimitive{name:"skip",msg:Some(format!("Not a valid register: {}",i)),cause:Some(cmd.cause),source:None})
    };
    let v = state.get_skip_register(i);
    debug_log!(debug=>"\\skip{} == {}",i,v);
    Ok(v)
}

pub fn skipdef<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:StomachCommand<ET::Token>,global:bool)
                                                     -> Result<(),ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"skipdef");
    let name = catch_prim!(gullet.get_control_sequence(state) => ("skipdef",cmd));
    match &name {
        BaseToken::Char(c,_) => {
            state.set_ac_command(*c,Some(Ptr::new(Command::Relax)),false)
        }
        BaseToken::CS(name) => {
            state.set_command(name.clone(),Some(Ptr::new(Command::Relax)),false)
        }
    }
    catch_prim!(gullet.mouth().skip_eq_char::<ET>(state) => ("skipdef",cmd));
    let num = catch_prim!(gullet.get_int(state) => ("skipdef",cmd));
    if num.to_i64() < 0 {
        return Err(ErrorInPrimitive{name:"skipdef",msg:Some(format!("Invalid skip register index: {}",num)),cause:Some(cmd.cause),source:None})
    }
    let num = num.to_i64() as usize;
    match name {
        BaseToken::Char(c,_) => {
            debug_log!(debug=>"\\skipdef: {} = \\skip{}",c.char_str(),num);
            state.set_ac_command(c, Some(Ptr::new(Command::ValueRegister{index:num,tp:Assignable::Skip })), global);
        }
        BaseToken::CS(name) => {
            debug_log!(debug=>"\\skipdef: \\{} = {}",name,num);
            state.set_command(name, Some(Ptr::new(Command::ValueRegister{index:num,tp:Assignable::Skip})), global);
        }
    }
    Ok(())
}

pub fn string<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:GulletCommand<ET::Token>)
    -> Result<Vec<ET::Token>,ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"string");
    let cc = state.get_catcode_scheme();
    match catch_prim!(gullet.mouth().get_next::<ET>(state) => ("string",cmd)) {
        None => file_end_prim!("string",cmd),
        Some((t,_)) => match t.base() {
            BaseToken::Char(c,_) => {
                let ret = gullet::methods::string_to_tokens::<ET::Token>(&c.char_str().as_bytes());
                Ok(ret)
            }
            BaseToken::CS(name) => {
                let mut ret = Vec::new();
                match state.get_escapechar() {
                    None => (),
                    Some(c) => ret.push(ET::Token::new(BaseToken::Char(c,CategoryCode::Other),None))
                }
                for t in name.as_vec() {
                    if *cc.get(*t) == CategoryCode::Space {
                        ret.push(ET::Token::new(BaseToken::Char(*t,CategoryCode::Space),None));
                    } else {
                        ret.push(ET::Token::new(BaseToken::Char(t.clone(), CategoryCode::Other), None));
                    }
                }
                Ok(ret)
            }
        }
    }

}

pub fn the<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:GulletCommand<ET::Token>)
    -> Result<Vec<ET::Token>,ErrorInPrimitive<ET::Token>> {
    debug_log!(trace => "\\the");
    let next = catch_prim!(gullet.get_next_stomach_command(state) => ("the",cmd));
    match next {
        None => file_end_prim!("the",cmd),
        Some(c) => match c.cmd {
            StomachCommandInner::ValueRegister(i,Assignable::Int) => {
                let val = state.get_int_register(i);
                let str = format!("{}",val);
                debug_log!(debug => "the: {}",str);
                let ret = gullet::methods::string_to_tokens::<ET::Token>(&str.as_bytes());
                Ok(ret)
            }
            StomachCommandInner::ValueRegister(i,Assignable::Toks) =>
                Ok(state.get_toks_register(i)),
            StomachCommandInner::ValueRegister(_,tp) => todo!("\\the ValueRegister {:?}",tp),
            StomachCommandInner::AssignableValue {tp:Assignable::Toks,name} =>
                Ok(state.get_primitive_toks(name)),
            StomachCommandInner::AssignableValue {tp,..} => todo!("\\the AssignableValue {:?}",tp),
            StomachCommandInner::Value {name,index,tp:Assignable::Int} => {
                match gullet.primitive_int(index) {
                    None =>return Err(ErrorInPrimitive{name:"the",msg:None,cause:Some(cmd.cause),source:Some(
                        ImplementationError(format!("Missing implementation for {}",name),PhantomData).into()
                    )}),
                    Some(fun) => {
                        let val = catch_prim!(fun(state,gullet,cmd.clone()) => ("the",cmd));
                        let str = format!("{}",val);
                        debug_log!(debug => "the: {}",str);
                        let ret = gullet::methods::string_to_tokens::<ET::Token>(&str.as_bytes());
                        Ok(ret)
                    }
                }
            }
            StomachCommandInner::Value {name,index,tp:Assignable::Dim} => {
                match gullet.primitive_dim(index) {
                    None =>return Err(ErrorInPrimitive{name:"the",msg:None,cause:Some(cmd.cause),source:Some(
                        ImplementationError(format!("Missing implementation for {}",name),PhantomData).into()
                    )}),
                    Some(fun) => {
                        let val = catch_prim!(fun(state,gullet,cmd.clone()) => ("the",cmd));
                        let str = format!("{}",val);
                        debug_log!(debug => "the: {}",str);
                        let ret = gullet::methods::string_to_tokens::<ET::Token>(&str.as_bytes());
                        Ok(ret)
                    }
                }
            }
            StomachCommandInner::Value {name,index,tp:Assignable::Skip} => {
                match gullet.primitive_skip(index) {
                    None =>return Err(ErrorInPrimitive{name:"the",msg:None,cause:Some(cmd.cause),source:Some(
                        ImplementationError(format!("Missing implementation for {}",name),PhantomData).into()
                    )}),
                    Some(fun) => {
                        let val = catch_prim!(fun(state,gullet,cmd.clone()) => ("the",cmd));
                        let str = format!("{}",val);
                        debug_log!(debug => "the: {}",str);
                        let ret = gullet::methods::string_to_tokens::<ET::Token>(&str.as_bytes());
                        Ok(ret)
                    }
                }
            }
            StomachCommandInner::Value {name,index,tp:Assignable::MuSkip} => {
                match gullet.primitive_muskip(index) {
                    None =>return Err(ErrorInPrimitive{name:"the",msg:None,cause:Some(cmd.cause),source:Some(
                        ImplementationError(format!("Missing implementation for {}",name),PhantomData).into()
                    )}),
                    Some(fun) => {
                        let val = catch_prim!(fun(state,gullet,cmd.clone()) => ("the",cmd));
                        let str = format!("{}",val);
                        debug_log!(debug => "the: {}",str);
                        let ret = gullet::methods::string_to_tokens::<ET::Token>(&str.as_bytes());
                        Ok(ret)
                    }
                }
            }
            StomachCommandInner::ValueAssignment {name,value_index,tp:Assignable::Int,..} => {
                match gullet.primitive_int(value_index) {
                    None =>return Err(ErrorInPrimitive{name:"the",msg:None,cause:Some(cmd.cause),source:Some(
                        ImplementationError(format!("Missing implementation for {}",name),PhantomData).into()
                    )}),
                    Some(fun) => {
                        let val = catch_prim!(fun(state,gullet,cmd.clone()) => ("the",cmd));
                        let str = format!("{}",val);
                        debug_log!(debug => "the: {}",str);
                        let ret = gullet::methods::string_to_tokens::<ET::Token>(&str.as_bytes());
                        Ok(ret)
                    }
                }
            }
            StomachCommandInner::Assignment {name:"toks",..} => todo!("toks in \\the"),
            StomachCommandInner::Value {name,index,tp:_} => {
                todo!()
            }
            StomachCommandInner::ValueAssignment {..} => todo!(),
            _ => return Err(ErrorInPrimitive{name:"the",msg:Some("Expected a value after \\the".to_string()),cause:Some(cmd.cause),source:None})
        }
    }
}

pub fn time<ET:EngineType>(state:&mut ET::State,cmd:GulletCommand<ET::Token>) -> Result<ET::Int,ErrorInPrimitive<ET::Token>> {
    let t = state.get_start_time();
    Ok(catch_prim!(ET::Int::from_i64( ((t.hour() * 60) + t.minute()) as i64 ) => ("time",cmd)))
}


pub fn toks<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:StomachCommand<ET::Token>,global:bool)
    -> Result<(),ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"Assigning \\toks");
    let i = catch_prim!(gullet.get_int(state) => ("toks",cmd));
    let i:usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => return Err(ErrorInPrimitive{name:"toks",msg:Some(format!("Not a valid register: {}",i)),cause:Some(cmd.cause),source:None})
    };
    catch_prim!(gullet.mouth().skip_eq_char::<ET>(state) => ("toks",cmd));
    match catch_prim!(gullet.mouth().get_next::<ET>(state) => ("toks",cmd)) {
        None => file_end_prim!("toks",cmd),
        Some((t,_)) if t.catcode() == CategoryCode::BeginGroup => (),
        Some((o,_)) => return Err(ErrorInPrimitive{name:"toks",msg:Some(format!("Expected begin group token after \\toks, got: {}",o)),cause:Some(cmd.cause),source:None})
    }
    let v = catch_prim!(gullet.mouth().read_until_endgroup::<ET>(state) => ("toks",cmd));
    debug_log!(debug=>"\\toks{} = {}",i,TokenList(v.clone()));
    state.set_toks_register(i,v,global);
    Ok(())
}

pub fn toksdef<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:StomachCommand<ET::Token>,global:bool)
                                                      -> Result<(),ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"toksdef");
    let name = catch_prim!(gullet.get_control_sequence(state) => ("toksdef",cmd));
    match &name {
        BaseToken::Char(c,_) => {
            state.set_ac_command(*c,Some(Ptr::new(Command::Relax)),false)
        }
        BaseToken::CS(name) => {
            state.set_command(name.clone(),Some(Ptr::new(Command::Relax)),false)
        }
    }
    catch_prim!(gullet.mouth().skip_eq_char::<ET>(state) => ("toksdef",cmd));
    let num = catch_prim!(gullet.get_int(state) => ("toksdef",cmd));
    if num.to_i64() < 0 {
        return Err(ErrorInPrimitive{name:"toksdef",msg:Some(format!("Invalid toks register index: {}",num)),cause:Some(cmd.cause),source:None})
    }
    let num = num.to_i64() as usize;
    match name {
        BaseToken::Char(c,_) => {
            debug_log!(debug=>"\\toksdef: {} = \\toks{}",c.char_str(),num);
            state.set_ac_command(c, Some(Ptr::new(Command::ValueRegister{index:num,tp:Assignable::Toks })), global);
        }
        BaseToken::CS(name) => {
            debug_log!(debug=>"\\toksdef: \\{} = {}",name,num);
            state.set_command(name, Some(Ptr::new(Command::ValueRegister{index:num,tp:Assignable::Toks})), global);
        }
    }
    Ok(())
}

pub fn uccode_assign<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:StomachCommand<ET::Token>,global:bool)
    -> Result<(),ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"Assigning upper case character");
    let i = catch_prim!(gullet.get_int(state) => ("uccode",cmd));
    let c: ET::Char = match ET::Char::from_i64(i.to_i64()) {
        Some(i) => i,
        None => return Err(ErrorInPrimitive{name:"uccode",msg:Some(format!("Not a valid character: {}",i)),cause:Some(cmd.cause),source:None})
    };
    catch_prim!(gullet.mouth().skip_eq_char::<ET>(state) => ("uccode",cmd));
    let i = catch_prim!(gullet.get_int(state) => ("uccode",cmd));
    let lc: ET::Char = match ET::Char::from_i64(i.to_i64()) {
        Some(i) => i,
        None => return Err(ErrorInPrimitive{name:"uccode",msg:Some(format!("Not a valid character: {}",i)),cause:Some(cmd.cause),source:None})
    };
    debug_log!(debug=>"\\uccode '{}' = {}",c.char_str(),lc.char_str());
    state.set_uccode(c,lc,global);
    Ok(())
}

pub fn uccode_get<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:GulletCommand<ET::Token>)
    -> Result<ET::Int,ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"Getting upper case character");
    let i = catch_prim!(gullet.get_int(state) => ("uccode",cmd));
    let c: ET::Char = match ET::Char::from_i64(i.to_i64()) {
        Some(i) => i,
        None => return Err(ErrorInPrimitive{name:"uccode",msg:Some(format!("Not a valid character: {}",i)),cause:Some(cmd.cause),source:None})
    };
    let v = catch_prim!(ET::Int::from_i64(state.get_uccode(c).to_usize() as i64) => ("uccode",cmd));
    debug_log!(debug=>"\\uccode '{}' == {}",c.char_str(),v);
    Ok(v)
}

pub fn uppercase<ET:EngineType>(stomach:&mut ET::Stomach, state:&mut ET::State,gullet:&mut ET::Gullet,cmd:StomachCommand<ET::Token>)
                                         -> Result<(),ErrorInPrimitive<ET::Token>> {
    debug_log!(trace => "\\uppercase");
    match catch_prim!(gullet.get_next_stomach_command(state) => ("uppercase",cmd)) {
        None => file_end_prim!("uppercase",cmd),
        Some(sc) => match sc.cmd {
            StomachCommandInner::BeginGroup(_) => (),
            _ => return Err(ErrorInPrimitive{name:"uppercase",msg:Some("Expected a begin group after \\uppercase".to_string()),cause:Some(cmd.cause),source:None})
        }
    }
    let mut ingroups = 1;
    let mut ret = vec!();
    while let Some((next,_)) = catch_prim!(gullet.mouth().get_next::<ET>(state) => ("uppercase",cmd)) {
        match next.base() {
            BaseToken::Char(c,CategoryCode::BeginGroup) => {
                ingroups += 1;
                let nc = state.get_uccode(*c);
                ret.push(ET::Token::new(BaseToken::Char(nc,CategoryCode::BeginGroup),None));
            },
            BaseToken::Char(c,CategoryCode::EndGroup) => {
                ingroups -= 1;
                if ingroups == 0 {
                    break;
                } else {
                    let nc = state.get_uccode(*c);
                    ret.push(ET::Token::new(BaseToken::Char(nc,CategoryCode::EndGroup),None));
                }
            },
            BaseToken::Char(c,cc) => {
                let nc = state.get_uccode(*c);
                ret.push(ET::Token::new(BaseToken::Char(nc,*cc),None));
            },
            _ => ret.push(next)
        }
    }
    gullet.mouth().push_tokens(ret);
    Ok(())
}

pub fn write<ET:EngineType>(state: &mut ET::State, gullet:&mut ET::Gullet, stomach:&mut ET::Stomach, cmd:StomachCommand<ET::Token>)
    -> Result<Whatsit<ET>, ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"\\write");
    let i = catch_prim!(gullet.get_int(state) => ("write",cmd));
    let i = i.to_i64();

    match catch_prim!(gullet.mouth().get_next::<ET>(state) => ("write",cmd)) {
        None => file_end_prim!("write",cmd),
        Some((t,_)) if t.catcode() == CategoryCode::BeginGroup => (),
        Some((o,_)) => return Err(ErrorInPrimitive{name:"write",msg:Some(format!("Expected begin group token after \\write, got: {}",o)),cause:Some(cmd.cause),source:None})
    }
    let mut tks = catch_prim!(gullet.mouth().read_until_endgroup::<ET>(state) => ("write",cmd));

    let apply = Box::new(move |_stomach:&mut ET::Stomach,state:&mut ET::State,gullet:&mut ET::Gullet| {
        tks.push(ET::Token::new(BaseToken::Char(ET::Char::from(b'}'),CategoryCode::EndGroup),None));
        tks.insert(0,ET::Token::new(BaseToken::Char(ET::Char::from(b'{'),CategoryCode::BeginGroup),None));
        let old = gullet.switch_mouth(tks);
        let string = String::from_utf8(gullet.get_braced_string(state)?).unwrap();
        if i == 18 {
            (state.outputs().write_18)(&string)
        }
        else if i == 17 {
            (state.outputs().write_17)(&string)
        }
        else if i < 0 {
            (state.outputs().write_neg1)(&string)
        }
        else {
            match state.get_open_out_file(i as usize) {
                None =>
                    (state.outputs().write_other)(&string),
                Some(f) => f.write(&string)
            }
        }
        gullet.restore_mouth(old);
        Ok(())
    });
    Ok(Whatsit { apply })
}


pub fn xdef<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:StomachCommand<ET::Token>,global:bool,protected:bool,long:bool,outer:bool)
                                                 -> Result<(),ErrorInPrimitive<ET::Token>> {
    edef::<ET>(state,gullet,cmd,true,protected,long,outer)
}

pub fn year<ET:EngineType>(state:&mut ET::State,cmd:GulletCommand<ET::Token>) -> Result<ET::Int,ErrorInPrimitive<ET::Token>> {
    Ok(catch_prim!(ET::Int::from_i64(
        state.get_start_time().year() as i64
    ) => ("year",cmd)))
}

// --------------------------------------------------------------------------------------------------


pub fn initialize_tex_primitives<ET:EngineType>(state:&mut ET::State,stomach:&mut ET::Stomach,gullet:&mut ET::Gullet) {

    register_skip_assign!(abovedisplayshortskip,state,stomach,gullet);
    register_skip_assign!(abovedisplayskip,state,stomach,gullet);
    register_int_assign!(adjdemerits,state,stomach,gullet);
    register_assign!(advance,state,stomach,gullet,(s,gu,_,cmd,global) =>advance::<ET>(s,gu,cmd,global));
    register_skip_assign!(baselineskip,state,stomach,gullet);
    register_stomach!(begingroup,state,stomach,gullet,(s,_,sto,cmd,_) =>begingroup::<ET>(s,cmd));
    register_skip_assign!(belowdisplayskip,state,stomach,gullet);
    register_skip_assign!(belowdisplayshortskip,state,stomach,gullet);
    register_int_assign!(binoppenalty,state,stomach,gullet);
    register_dim_assign!(boxmaxdepth,state,stomach,gullet);
    register_int_assign!(brokenpenalty,state,stomach,gullet);
    register_value_assign_int!(catcode,state,stomach,gullet);
    register_assign!(chardef,state,stomach,gullet,(s,gu,_,cmd,global) =>chardef::<ET>(s,gu,cmd,global));
    register_stomach!(closein,state,stomach,gullet,(s,gu,sto,cmd,_) =>closein::<ET>(s,gu,sto,cmd));
    register_whatsit!(closeout,state,stomach,gullet,(s,gu,sto,cmd) =>closeout::<ET>(s,gu,sto,cmd));
    register_int_assign!(clubpenalty,state,stomach,gullet);
    register_value_assign_int!(count,state,stomach,gullet);
    register_assign!(countdef,state,stomach,gullet,(s,gu,_,cmd,global) =>countdef::<ET>(s,gu,cmd,global));
    register_gullet!(csname,state,stomach,gullet,(s,gu,cmd) =>csname::<ET>(s,gu,cmd));
    register_int!(day,state,stomach,gullet,(s,g,c) => day::<ET>(s,c));
    register_assign!(def,state,stomach,gullet,(s,gu,_,cmd,global) =>def::<ET>(s,gu,cmd,global,false,false,false));
    register_int_assign!(defaulthyphenchar,state,stomach,gullet);
    register_int_assign!(defaultskewchar,state,stomach,gullet);
    register_int_assign!(delimiterfactor,state,stomach,gullet);
    register_dim_assign!(delimitershortfall,state,stomach,gullet);
    register_gullet!(detokenize,state,stomach,gullet,(s,gu,cmd) =>detokenize::<ET>(s,gu,cmd));
    register_value_assign_dim!(dimen,state,stomach,gullet);
    register_assign!(dimendef,state,stomach,gullet,(s,gu,_,cmd,global) =>dimendef::<ET>(s,gu,cmd,global));
    register_dim_assign!(displayindent,state,stomach,gullet);
    register_int_assign!(displaywidowpenalty,state,stomach,gullet);
    register_dim_assign!(displaywidth,state,stomach,gullet);
    register_assign!(divide,state,stomach,gullet,(s,gu,_,cmd,global) =>divide::<ET>(s,gu,cmd,global));
    register_int_assign!(doublehyphendemerits,state,stomach,gullet);
    register_assign!(edef,state,stomach,gullet,(s,gu,_,cmd,global) =>edef::<ET>(s,gu,cmd,global,false,false,false));
    register_gullet!(else,state,stomach,gullet,(s,gu,cmd) =>else_::<ET>(s,gu,cmd));
    register_dim_assign!(emergencystretch,state,stomach,gullet);
    register_stomach!(end,state,stomach,gullet,(s,_,sto,cmd,_) =>end::<ET>(sto,s,cmd));
    register_stomach!(endcsname,state,stomach,gullet,(_,_,_,cmd,_) =>endcsname::<ET::Token>(cmd));
    register_stomach!(endgroup,state,stomach,gullet,(s,gu,sto,cmd,_) =>endgroup::<ET>(sto,s,gu,cmd));
    register_value_assign_int!(endlinechar,state,stomach,gullet);
    register_tok_assign!(errhelp,state,stomach,gullet);

    let em = stomach.register_primitive("errmessage",|s,gu,_sto,cmd,_global|
        errmessage::<ET>(s,gu,cmd));
    state.set_command(ET::Char::from_str("errmessage"),Some(Ptr::new(Command::Stomach {
        name:"errmessage",
        index:em
    })),true);
    state.set_command(ET::Char::from_str("LaTeX3 error:"),Some(Ptr::new(Command::Stomach {
        name:"LaTeX3 error:",
        index:em
    })),true);

    register_int_assign!(errorcontextlines,state,stomach,gullet);
    register_value_assign_int!(escapechar,state,stomach,gullet);
    register_int_assign!(exhyphenpenalty,state,stomach,gullet);
    register_gullet!(expandafter,state,stomach,gullet,(s,g,c) => expandafter::<ET>(s,g,c));
    register_tok_assign!(everypar,state,stomach,gullet);
    register_tok_assign!(everymath,state,stomach,gullet);
    register_tok_assign!(everydisplay,state,stomach,gullet);
    register_tok_assign!(everyhbox,state,stomach,gullet);
    register_tok_assign!(everyvbox,state,stomach,gullet);
    register_tok_assign!(everyjob,state,stomach,gullet);
    register_tok_assign!(everycr,state,stomach,gullet);
    register_int_assign!(fam,state,stomach,gullet);
    register_gullet!(fi,state,stomach,gullet,(s,gu,cmd) =>fi::<ET>(s,gu,cmd));
    register_int_assign!(finalhyphendemerits,state,stomach,gullet);
    register_int_assign!(floatingpenalty,state,stomach,gullet);
    register_value_assign_font!(font,state,stomach,gullet);
    register_value_assign_dim!(fontdimen,state,stomach,gullet);
    register_assign!(gdef,state,stomach,gullet,(s,gu,_,cmd,global) =>gdef::<ET>(s,gu,cmd,global,false,false,false));
    register_assign!(global,state,stomach,gullet,(s,gu,sto,cmd,g) =>global::<ET>(sto,s,gu,cmd,g,false,false,false));
    register_int_assign!(globaldefs,state,stomach,gullet);
    register_int_assign!(hangafter,state,stomach,gullet);
    register_dim_assign!(hangindent,state,stomach,gullet);
    register_int_assign!(hbadness,state,stomach,gullet);
    register_open_box!(hbox,state,stomach,gullet,BoxMode::H,(s,gu,sto,cmd) =>hbox::<ET>(sto,s,gu,cmd));
    register_dim_assign!(hfuzz,state,stomach,gullet);
    register_dim_assign!(hoffset,state,stomach,gullet);
    register_int_assign!(holdinginserts,state,stomach,gullet);
    register_dim_assign!(hsize,state,stomach,gullet);
    register_value_assign_int!(hyphenchar,state,stomach,gullet);
    register_int_assign!(hyphenpenalty,state,stomach,gullet);
    register_conditional!(if,state,stomach,gullet,(s,gu,cmd) =>if_::<ET>(s,gu,cmd));
    register_conditional!(ifcase,state,stomach,gullet,(s,gu,cmd) =>ifcase::<ET>());
    register_conditional!(ifcat,state,stomach,gullet,(s,gu,cmd) =>ifcat::<ET>(s,gu,cmd));
    register_conditional!(ifdim,state,stomach,gullet,(s,gu,cmd) =>todo!("ifdim"));
    register_conditional!(ifeof,state,stomach,gullet,(s,gu,cmd) =>ifeof::<ET>(s,gu,cmd));
    register_conditional!(iffalse,state,stomach,gullet,(s,gu,cmd) => Ok(false));
    register_conditional!(ifhbox,state,stomach,gullet,(s,gu,cmd) =>todo!("ifhbox"));
    register_conditional!(ifhmode,state,stomach,gullet,(s,gu,cmd) =>todo!("ifhmode"));
    register_conditional!(ifinner,state,stomach,gullet,(s,gu,cmd) =>todo!("ifinner"));
    register_conditional!(ifmmode,state,stomach,gullet,(s,gu,cmd) =>todo!("ifmmode"));
    register_conditional!(ifnum,state,stomach,gullet,(s,gu,cmd) =>ifnum::<ET>(s,gu,cmd));
    register_conditional!(ifodd,state,stomach,gullet,(s,gu,cmd) =>ifodd::<ET>(s,gu,cmd));
    register_conditional!(iftrue,state,stomach,gullet,(s,gu,cmd) => Ok(true));
    register_conditional!(ifvbox,state,stomach,gullet,(s,gu,cmd) =>todo!("ifvbox"));
    register_conditional!(ifvmode,state,stomach,gullet,(s,gu,cmd) =>todo!("ifvmode"));
    register_conditional!(ifvoid,state,stomach,gullet,(s,gu,cmd) =>todo!("ifvoid"));
    register_conditional!(ifx,state,stomach,gullet,(s,gu,cmd) =>ifx::<ET>(s,gu,cmd));
    register_stomach!(immediate,state,stomach,gullet,(s,gu,sto,cmd,_) =>immediate::<ET>(s,gu,sto,cmd));
    register_gullet!(input,state,stomach,gullet,(s,gu,cmd) =>input::<ET>(s,gu,cmd));
    register_int_assign!(interlinepenalty,state,stomach,gullet);
    register_int_assign!(language,state,stomach,gullet);
    register_value_assign_int!(lccode,state,stomach,gullet);
    register_int_assign!(lefthyphenmin,state,stomach,gullet);
    register_skip_assign!(leftskip,state,stomach,gullet);
    register_assign!(let,state,stomach,gullet,(s,gu,_,cmd,global) =>let_::<ET>(s,gu,cmd,global));
    register_int_assign!(linepenalty,state,stomach,gullet);
    register_skip_assign!(lineskip,state,stomach,gullet);
    register_dim_assign!(lineskiplimit,state,stomach,gullet);
    register_assign!(long,state,stomach,gullet,(s,gu,sto,cmd,g) =>long::<ET>(sto,s,gu,cmd,g,false,false,false));
    register_stomach!(lowercase,state,stomach,gullet,(s,gu,sto,cmd,_) =>lowercase::<ET>(sto,s,gu,cmd));
    register_int_assign!(looseness,state,stomach,gullet);
    register_int_assign!(mag,state,stomach,gullet);
    register_int_assign!(maxdeadcycles,state,stomach,gullet);
    register_dim_assign!(maxdepth,state,stomach,gullet);
    register_assign!(mathchardef,state,stomach,gullet,(s,gu,_,cmd,global) =>mathchardef::<ET>(s,gu,cmd,global));
    register_dim_assign!(mathsurround,state,stomach,gullet);
    register_gullet!(meaning,state,stomach,gullet,(s,g,c) => meaning::<ET>(s,g,c));
    register_stomach!(message,state,stomach,gullet,(s,gu,_,cmd,_) =>message::<ET>(s,gu,cmd));
    register_int!(month,state,stomach,gullet,(s,g,c) => month::<ET>(s,c));
    register_assign!(multiply,state,stomach,gullet,(s,gu,_,cmd,global) =>multiply::<ET>(s,gu,cmd,global));
    register_value_assign_muskip!(muskip,state,stomach,gullet);
    register_assign!(muskipdef,state,stomach,gullet,(s,gu,_,cmd,global) =>muskipdef::<ET>(s,gu,cmd,global));
    register_value_assign_int!(newlinechar,state,stomach,gullet);
    register_gullet!(noexpand,state,stomach,gullet,(s,g,c) => noexpand::<ET>(s,g,c));
    register_dim_assign!(nulldelimiterspace,state,stomach,gullet);
    register_gullet!(number,state,stomach,gullet,(s,g,c) => number::<ET>(s,g,c));
    register_stomach!(openin,state,stomach,gullet,(s,gu,sto,cmd,_) =>openin::<ET>(s,gu,sto,cmd));
    register_whatsit!(openout,state,stomach,gullet,(s,gu,sto,cmd) =>openout::<ET>(s,gu,sto,cmd));
    register_gullet!(or,state,stomach,gullet,(s,g,c) => or::<ET>(s,g,c));
    register_assign!(outer,state,stomach,gullet,(s,gu,sto,cmd,g) =>outer::<ET>(sto,s,gu,cmd,g,false,false,false));
    register_tok_assign!(output,state,stomach,gullet);
    register_int_assign!(outputpenalty,state,stomach,gullet);
    register_dim_assign!(overfullrule,state,stomach,gullet);

    state.set_command(ET::Char::par_token(),Some(Ptr::new(Command::Stomach {
        name:"par",
        index:stomach.register_primitive("par",|s,_gu,sto,cmd,_global| par::<ET>(sto,s,cmd))
    })),true);
    register_skip_assign!(parfillskip,state,stomach,gullet);
    register_dim_assign!(parindent,state,stomach,gullet);
    register_skip_assign!(parskip,state,stomach,gullet);
    register_int_assign!(pausing,state,stomach,gullet);
    register_int_assign!(postdisplaypenalty,state,stomach,gullet);
    register_int_assign!(predisplaypenalty,state,stomach,gullet);
    register_dim_assign!(predisplaysize,state,stomach,gullet);
    register_int_assign!(relpenalty,state,stomach,gullet);
    register_int_assign!(righthyphenmin,state,stomach,gullet);
    register_int_assign!(pretolerance,state,stomach,gullet);
    register_assign!(read,state,stomach,gullet,(s,gu,_,cmd,global) =>read::<ET>(s,gu,cmd,global));
    state.set_command(ET::Char::relax_token(),Some(Ptr::new(Command::Relax)),true);
    register_int_assign!(relpenalty,state,stomach,gullet);
    register_skip_assign!(rightskip,state,stomach,gullet);
    register_gullet!(romannumeral,state,stomach,gullet,(s,g,c) => romannumeral::<ET>(s,g,c));
    register_dim_assign!(scriptspace,state,stomach,gullet);
    register_assign!(setbox,state,stomach,gullet,(s,gu,sto,cmd,global) =>setbox::<ET>(s,gu,sto,cmd,global));
    register_value_assign_int!(sfcode,state,stomach,gullet);
    register_int_assign!(showboxbreadth,state,stomach,gullet);
    register_int_assign!(showboxdepth,state,stomach,gullet);
    register_value_assign_skip!(skip,state,stomach,gullet);
    register_assign!(skipdef,state,stomach,gullet,(s,gu,_,cmd,global) =>skipdef::<ET>(s,gu,cmd,global));
    register_skip_assign!(spaceskip,state,stomach,gullet);
    register_dim_assign!(splitmaxdepth,state,stomach,gullet);
    register_skip_assign!(splittopskip,state,stomach,gullet);
    register_gullet!(string,state,stomach,gullet,(s,g,c) => string::<ET>(s,g,c));
    register_skip_assign!(tabskip,state,stomach,gullet);
    register_gullet!(the,state,stomach,gullet,(s,g,c) => the::<ET>(s,g,c));
    register_int!(time,state,stomach,gullet,(s,g,c) => time::<ET>(s,c));
    register_assign!(toks,state,stomach,gullet,(s,gu,_,cmd,global) =>toks::<ET>(s,gu,cmd,global));
    register_assign!(toksdef,state,stomach,gullet,(s,gu,_,cmd,global) =>toksdef::<ET>(s,gu,cmd,global));
    register_int_assign!(tolerance,state,stomach,gullet);
    register_skip_assign!(topskip,state,stomach,gullet);
    register_int_assign!(tracingcommands,state,stomach,gullet);
    register_int_assign!(tracinglostchars,state,stomach,gullet);
    register_int_assign!(tracingmacros,state,stomach,gullet);
    register_int_assign!(tracingonline,state,stomach,gullet);
    register_int_assign!(tracingoutput,state,stomach,gullet);
    register_int_assign!(tracingpages,state,stomach,gullet);
    register_int_assign!(tracingparagraphs,state,stomach,gullet);
    register_int_assign!(tracingrestores,state,stomach,gullet);
    register_int_assign!(tracingstats,state,stomach,gullet);
    register_value_assign_int!(uccode,state,stomach,gullet);
    register_int_assign!(uchyph,state,stomach,gullet);
    register_stomach!(uppercase,state,stomach,gullet,(s,gu,sto,cmd,_) =>uppercase::<ET>(sto,s,gu,cmd));
    register_int_assign!(vbadness,state,stomach,gullet);
    register_dim_assign!(vfuzz,state,stomach,gullet);
    register_dim_assign!(voffset,state,stomach,gullet);
    register_dim_assign!(vsize,state,stomach,gullet);
    register_int_assign!(widowpenalty,state,stomach,gullet);
    register_whatsit!(write,state,stomach,gullet,(s,gu,sto,cmd) =>write::<ET>(s,gu,sto,cmd));
    register_assign!(xdef,state,stomach,gullet,(s,gu,_,cmd,global) =>xdef::<ET>(s,gu,cmd,global,false,false,false));
    register_skip_assign!(xspaceskip,state,stomach,gullet);
    register_int!(year,state,stomach,gullet,(s,g,c) => year::<ET>(s,c));

    // TODOS ---------------------------------------------------------------------

    cmtodo!(state,stomach,gullet,thinmuskip);
    cmtodo!(state,stomach,gullet,medmuskip);
    cmtodo!(state,stomach,gullet,thickmuskip);

    cmtodo!(state,stomach,gullet,lastpenalty);
    cmtodo!(state,stomach,gullet,parshape);
    cmtodo!(state,stomach,gullet,inputlineno);
    cmtodo!(state,stomach,gullet,skewchar);
    cmtodo!(state,stomach,gullet,badness);
    cmtodo!(state,stomach,gullet,spacefactor);
    cmtodo!(state,stomach,gullet,prevgraf);
    cmtodo!(state,stomach,gullet,deadcycles);
    cmtodo!(state,stomach,gullet,insertpenalties);
    cmtodo!(state,stomach,gullet,mathcode);
    cmtodo!(state,stomach,gullet,delcode);
    cmtodo!(state,stomach,gullet,textfont);
    cmtodo!(state,stomach,gullet,scriptfont);
    cmtodo!(state,stomach,gullet,scriptscriptfont);
    cmtodo!(state,stomach,gullet,lastkern);
    cmtodo!(state,stomach,gullet,prevdepth);
    cmtodo!(state,stomach,gullet,pagegoal);
    cmtodo!(state,stomach,gullet,pagetotal);
    cmtodo!(state,stomach,gullet,pagestretch);
    cmtodo!(state,stomach,gullet,pagefilstretch);
    cmtodo!(state,stomach,gullet,pagefillstretch);
    cmtodo!(state,stomach,gullet,pagefilllstretch);
    cmtodo!(state,stomach,gullet,pageshrink);
    cmtodo!(state,stomach,gullet,pagedepth);
    cmtodo!(state,stomach,gullet,ht);
    cmtodo!(state,stomach,gullet,wd);
    cmtodo!(state,stomach,gullet,dp);
    cmtodo!(state,stomach,gullet,lastskip);
    cmtodo!(state,stomach,gullet,futurelet);
    cmtodo!(state,stomach,gullet,hyphenation);
    cmtodo!(state,stomach,gullet,patterns);
    cmtodo!(state,stomach,gullet,errorstopmode);
    cmtodo!(state,stomach,gullet,scrollmode);
    cmtodo!(state,stomach,gullet,nonstopmode);
    cmtodo!(state,stomach,gullet,batchmode);
    cmtodo!(state,stomach,gullet,box);
    cmtodo!(state,stomach,gullet,copy);
    cmtodo!(state,stomach,gullet,lastbox);
    cmtodo!(state,stomach,gullet,vsplit);
    cmtodo!(state,stomach,gullet,vbox);
    cmtodo!(state,stomach,gullet,vtop);
    cmtodo!(state,stomach,gullet,show);
    cmtodo!(state,stomach,gullet,showbox);
    cmtodo!(state,stomach,gullet,showlists);
    cmtodo!(state,stomach,gullet,showthe);
    cmtodo!(state,stomach,gullet,shipout);
    cmtodo!(state,stomach,gullet,ignorespaces);
    cmtodo!(state,stomach,gullet,afterassignment);
    cmtodo!(state,stomach,gullet,aftergroup);
    cmtodo!(state,stomach,gullet,special);
    cmtodo!(state,stomach,gullet,penalty);
    cmtodo!(state,stomach,gullet,kern);
    cmtodo!(state,stomach,gullet,mkern);
    cmtodo!(state,stomach,gullet,unpenalty);
    cmtodo!(state,stomach,gullet,unkern);
    cmtodo!(state,stomach,gullet,unskip);
    cmtodo!(state,stomach,gullet,mark);
    cmtodo!(state,stomach,gullet,topmark);
    cmtodo!(state,stomach,gullet,firstmark);
    cmtodo!(state,stomach,gullet,botmark);
    cmtodo!(state,stomach,gullet,splitfirstmark);
    cmtodo!(state,stomach,gullet,splitbotmark);
    cmtodo!(state,stomach,gullet,insert);
    cmtodo!(state,stomach,gullet,vadjust);
    cmtodo!(state,stomach,gullet,vskip);
    cmtodo!(state,stomach,gullet,vfil);
    cmtodo!(state,stomach,gullet,vfill);
    cmtodo!(state,stomach,gullet,vss);
    cmtodo!(state,stomach,gullet,vfilneg);
    cmtodo!(state,stomach,gullet,leaders);
    cmtodo!(state,stomach,gullet,cleaders);
    cmtodo!(state,stomach,gullet,xleaders);
    cmtodo!(state,stomach,gullet,vrule);
    cmtodo!(state,stomach,gullet,hrule);
    cmtodo!(state,stomach,gullet,moveleft);
    cmtodo!(state,stomach,gullet,moveright);
    cmtodo!(state,stomach,gullet,unvbox);
    cmtodo!(state,stomach,gullet,unvcopy);
    cmtodo!(state,stomach,gullet,halign);
    cmtodo!(state,stomach,gullet,valign);
    cmtodo!(state,stomach,gullet,indent);
    cmtodo!(state,stomach,gullet,noindent);
    cmtodo!(state,stomach,gullet,noboundary);
    cmtodo!(state,stomach,gullet,hfil);
    cmtodo!(state,stomach,gullet,hfill);
    cmtodo!(state,stomach,gullet,hfilneg);
    cmtodo!(state,stomach,gullet,hss);
    cmtodo!(state,stomach,gullet,accent);
    cmtodo!(state,stomach,gullet,discretionary);
    cmtodo!(state,stomach,gullet,dump);
    cmtodo!(state,stomach,gullet,raise);
    cmtodo!(state,stomach,gullet,lower);
    cmtodo!(state,stomach,gullet,setlanguage);
    cmtodo!(state,stomach,gullet,delimiter);
    cmtodo!(state,stomach,gullet,mathcode);
    cmtodo!(state,stomach,gullet,nonscript);
    cmtodo!(state,stomach,gullet,vcenter);
    cmtodo!(state,stomach,gullet,mathord);
    cmtodo!(state,stomach,gullet,mathop);
    cmtodo!(state,stomach,gullet,mathbin);
    cmtodo!(state,stomach,gullet,mathrel);
    cmtodo!(state,stomach,gullet,mathopen);
    cmtodo!(state,stomach,gullet,mathclose);
    cmtodo!(state,stomach,gullet,mathpunct);
    cmtodo!(state,stomach,gullet,mathinner);
    cmtodo!(state,stomach,gullet,underline);
    cmtodo!(state,stomach,gullet,overline);
    cmtodo!(state,stomach,gullet,mathaccent);
    cmtodo!(state,stomach,gullet,radical);
    cmtodo!(state,stomach,gullet,displaylimits);
    cmtodo!(state,stomach,gullet,limits);
    cmtodo!(state,stomach,gullet,nolimits);
    cmtodo!(state,stomach,gullet,mathchoice);
    cmtodo!(state,stomach,gullet,displaystyle);
    cmtodo!(state,stomach,gullet,textstyle);
    cmtodo!(state,stomach,gullet,scriptstyle);
    cmtodo!(state,stomach,gullet,scriptscriptstyle);
    cmtodo!(state,stomach,gullet,left);
    cmtodo!(state,stomach,gullet,right);
    cmtodo!(state,stomach,gullet,over);
    cmtodo!(state,stomach,gullet,atop);
    cmtodo!(state,stomach,gullet,above);
    cmtodo!(state,stomach,gullet,overwithdelims);
    cmtodo!(state,stomach,gullet,atopwithdelims);
    cmtodo!(state,stomach,gullet,abovewithdelims);
    cmtodo!(state,stomach,gullet,eqno);
    cmtodo!(state,stomach,gullet,leqno);
    cmtodo!(state,stomach,gullet,bigskip);
    cmtodo!(state,stomach,gullet,bye);
    cmtodo!(state,stomach,gullet,char);
    cmtodo!(state,stomach,gullet,cr);
    cmtodo!(state,stomach,gullet,crcr);
    cmtodo!(state,stomach,gullet,endinput);
    cmtodo!(state,stomach,gullet,fontname);
    cmtodo!(state,stomach,gullet,hskip);
    cmtodo!(state,stomach,gullet,italiccorr);
    cmtodo!(state,stomach,gullet,jobname);
    cmtodo!(state,stomach,gullet,mathchar);
    cmtodo!(state,stomach,gullet,medskip);
    cmtodo!(state,stomach,gullet,mskip);
    cmtodo!(state,stomach,gullet,noalign);
    cmtodo!(state,stomach,gullet,nullfont);
    cmtodo!(state,stomach,gullet,omit);
    cmtodo!(state,stomach,gullet,smallskip);
    cmtodo!(state,stomach,gullet,span);
    cmtodo!(state,stomach,gullet,unhbox);
    cmtodo!(state,stomach,gullet,unhcopy);
}

