//! TeX primitive [`BaseCommand`]s

use std::collections::VecDeque;
use std::hint::unreachable_unchecked;
use std::marker::PhantomData;
use crate::{debug_log, register_assign, register_conditional, register_int_assign, register_unexpandable, register_tok_assign, register_int, register_whatsit, register_value_assign_int, register_value_assign_dim, register_value_assign_muskip, register_value_assign_skip, register_dim_assign, register_skip_assign, cmtodo, register_value_assign_font, register_open_box, cmstodo, register_muskip_assign, register_expandable, catch, file_end, throw, catch_prim, file_end_prim, register_value_assign_toks};
use crate::engine::filesystem::{File, FileSystem};
use crate::engine::gullet::Gullet;
use crate::engine::gullet::methods::{tokens_to_string, string_to_tokens, token_to_chars, get_char, resolve_token};
use crate::engine::state::State;
use crate::engine::mouth::Mouth;
use crate::engine::state::modes::{BoxMode, GroupType, TeXMode};
use crate::engine::stomach::Stomach;
use crate::tex::catcodes::{CategoryCode, CategoryCodeScheme};
use crate::tex::commands::{BaseCommand, Def, ExpToken, ParamToken, Command, ResolvedToken, BaseStomachCommand, BoxFun, CloseBoxFun, TokenCont, ValueCommand, CommandSource, DefI};
use crate::tex::commands::methods::{parse_signature, set_relax};
use crate::tex::numbers::{Int, Skip, Numeric, MuSkip, Dim};
use crate::tex::token::{BaseToken, Token, TokenList};
use crate::utils::errors::{TeXError};
use crate::utils::Ptr;
use crate::utils::strings::{AllCharsTrait, CharType, TeXStr};
use chrono::{Datelike, Timelike};
use log::warn;
use crate::engine::{EngineType, gullet};
use crate::engine::gullet::numeric_methods::expand_until_space;
use crate::tex::nodes::{HBox, HorV, HVBox, NodeTrait, OpenBox, SimpleNode, TeXNode, Whatsit};
use crate::tex::commands::etex::UNLESS;
use crate::tex::ConditionalBranch;
use crate::tex::fonts::{FontStore,Font};
//use super::etex::protected;

/* TODO

SPACE
\/
\-
 */

pub fn SPACE<ET:EngineType>(_stomach:&mut ET::Stomach,state:&mut ET::State,_cmd:CommandSource<ET>)
                                     -> Result<(),TeXError<ET>> {
    todo!("\\ ")
}

pub static ADVANCE: &str = "advance";
pub fn advance<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,global:bool)
                                                     -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"\\advance");
    catch_prim!(gullet.mouth().skip_whitespace(state) => (ADVANCE,cmd));
    match catch_prim!(gullet.get_next_unexpandable(state) => (ADVANCE,cmd)) {
        None => file_end_prim!(ADVANCE,cmd),
        Some(ncmd) => match ncmd.command {
            BaseCommand::Int(a) => {
                macro_rules! finish {
                    ($old:expr,$nv:ident => $set:expr) => {{
                        catch_prim!(gullet.get_keyword(state,"by") => (ADVANCE,cmd));
                        let i = catch_prim!(gullet.get_int(state) => (ADVANCE,cmd));
                        let $nv = $old + i;
                        $set;
                        Ok(())
                    }}
                }
                match a {
                    ValueCommand::Register(u) => finish!(state.get_int_register(u),nv => state.set_int_register(u,nv,global)),
                    ValueCommand::Primitive(name) => finish!(state.get_primitive_int(name),nv => state.set_primitive_int(name,nv,global)),
                    ValueCommand::Complex {name,..} if name == COUNT => {
                        let int = catch_prim!(gullet.get_int(state) => (ADVANCE,cmd));
                        let u = match int.try_into() {
                            Ok(u) => u,
                            _ => throw!("Not a valid register: {}",int => cmd.cause)
                        };
                        finish!(state.get_int_register(u),nv => state.set_int_register(u,nv,global))
                    }
                    _ => throw!("Unexpected token after \\advance" => cmd.cause)
                }
            }
            BaseCommand::Dim(a) => {
                macro_rules! finish {
                    ($old:expr,$nv:ident => $set:expr) => {{
                        catch_prim!(gullet.get_keyword(state,"by") => (ADVANCE,cmd));
                        let i = catch_prim!(gullet.get_dim(state) => (ADVANCE,cmd));
                        let $nv = $old + i;
                        $set;
                        Ok(())
                    }}
                }
                match a {
                    ValueCommand::Register(u) => finish!(state.get_dim_register(u),nv => state.set_dim_register(u,nv,global)),
                    ValueCommand::Primitive(name) => finish!(state.get_primitive_dim(name),nv => state.set_primitive_dim(name,nv,global)),
                    ValueCommand::Complex {name,..} if name == DIMEN => {
                        let int = catch_prim!(gullet.get_int(state) => (ADVANCE,cmd));
                        let u = match int.try_into() {
                            Ok(u) => u,
                            _ => throw!("Not a valid register: {}",int => cmd.cause)
                        };
                        finish!(state.get_dim_register(u),nv => state.set_dim_register(u,nv,global))
                    }
                    _ => throw!("Unexpected token after \\advance" => cmd.cause)
                }
            }
            BaseCommand::Skip(a) => {
                macro_rules! finish {
                    ($old:expr,$nv:ident => $set:expr) => {{
                        catch_prim!(gullet.get_keyword(state,"by") => (ADVANCE,cmd));
                        let i = catch_prim!(gullet.get_skip(state) => (ADVANCE,cmd));
                        let $nv = $old + i;
                        $set;
                        Ok(())
                    }}
                }
                match a {
                    ValueCommand::Register(u) => finish!(state.get_skip_register(u),nv => state.set_skip_register(u,nv,global)),
                    ValueCommand::Primitive(name) => finish!(state.get_primitive_skip(name),nv => state.set_primitive_skip(name,nv,global)),
                    ValueCommand::Complex {name,..} if name == SKIP => {
                        let int = catch_prim!(gullet.get_int(state) => (ADVANCE,cmd));
                        let u = match int.try_into() {
                            Ok(u) => u,
                            _ => throw!("Not a valid register: {}",int => cmd.cause)
                        };
                        finish!(state.get_skip_register(u),nv => state.set_skip_register(u,nv,global))
                    }
                    _ => throw!("Unexpected token after \\advance" => cmd.cause)
                }
            }
            BaseCommand::MuSkip(a) => {
                macro_rules! finish {
                    ($old:expr,$nv:ident => $set:expr) => {{
                        catch_prim!(gullet.get_keyword(state,"by") => (ADVANCE,cmd));
                        let i = catch_prim!(gullet.get_muskip(state) => (ADVANCE,cmd));
                        let $nv = $old + i;
                        $set;
                        Ok(())
                    }}
                }
                match a {
                    ValueCommand::Register(u) => finish!(state.get_muskip_register(u),nv => state.set_muskip_register(u,nv,global)),
                    ValueCommand::Primitive(name) => finish!(state.get_primitive_muskip(name),nv => state.set_primitive_muskip(name,nv,global)),
                    ValueCommand::Complex {name,..} if name == MUSKIP => {
                        let int = catch_prim!(gullet.get_int(state) => (ADVANCE,cmd));
                        let u = match int.try_into() {
                            Ok(u) => u,
                            _ => throw!("Not a valid register: {}",int => cmd.cause)
                        };
                        finish!(state.get_muskip_register(u),nv => state.set_muskip_register(u,nv,global))
                    }
                    _ => throw!("Unexpected token after \\advance" => cmd.cause)
                }
            }
            o => throw!("expected register after \\advance;got:{:?}",o => cmd.cause)
        }
    }
}

pub fn afterassignment<ET:EngineType>(state: &mut ET::State, gullet:&mut ET::Gullet, cmd:CommandSource<ET>) -> Result<(), TeXError<ET>> {
    debug_log!(trace=>"\\afterassignment");
    let next = match catch_prim!(gullet.mouth().get_next(state) => ("afterassignment",cmd)) {
        None => file_end!(cmd.cause),
        Some((t,_)) => t
    };
    state.set_afterassignment(next);
    Ok(())
}

pub fn begingroup<ET:EngineType>(state:&mut ET::State)-> Result<(),TeXError<ET>> {
    state.stack_push(GroupType::CS);
    Ok(())
}

pub fn catcode_assign<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,global:bool) -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"Assigning category code");
    let i = catch_prim!(gullet.get_int(state) => ("catcode",cmd));
    let c: ET::Char = match ET::Char::from_i64(i.to_i64()) {
        Some(i) => i,
        None => throw!("Not a valid character: {}",i => cmd.cause)
    };
    catch_prim!(gullet.mouth().skip_eq_char(state) => ("catcode",cmd));
    let v = catch_prim!(gullet.get_int(state) => ("catcode",cmd)).to_i64();
    if v < 0 || v > 15 {
        throw!("Invalid category code: {}",v => cmd.cause)
    }
    let cc: CategoryCode = unsafe{(v as u8).try_into().unwrap_unchecked()};
    debug_log!(debug=>"\\catcode '{}' = {}",c.char_str(),cc);
    state.set_catcode(c,cc,global);
    Ok(())
}
pub fn catcode_get<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>) -> Result<ET::Int,TeXError<ET>> {
    debug_log!(trace=>"Getting category code");
    let i = catch_prim!(gullet.get_int(state) => ("catcode",cmd));
    let c: ET::Char = match ET::Char::from_i64(i.to_i64()) {
        Some(i) => i,
        None => throw!("Not a valid character: {}",i => cmd.cause)
    };
    let cc = *state.get_catcode_scheme().get(&c);
    let v : u8 = cc.into();
    debug_log!(debug=>"\\catcode '{}' == {}",c.char_str(),cc);
    Ok(v.into())
}

pub static CHARDEF: &str = "chardef";
pub fn chardef<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,global:bool) -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"chardef");
    let name = catch_prim!(gullet.get_control_sequence(state) => (CHARDEF,cmd));
    catch_prim!(gullet.mouth().skip_eq_char(state) => (CHARDEF,cmd));
    let char = catch_prim!(get_char::<ET>(gullet,state) => (CHARDEF,cmd));
    let cmd = Command::new(BaseCommand::CharDef(char),Some(&cmd));
    state.set_command_for_tk(name, Some(cmd), global);
    Ok(())
}

pub fn closein<ET:EngineType>(state: &mut ET::State, gullet:&mut ET::Gullet, cmd:CommandSource<ET>) -> Result<(), TeXError<ET>> {
    debug_log!(trace=>"\\closein");
    let i = catch_prim!(gullet.get_int(state) => ("closein",cmd));
    let i : usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => throw!("Invalid file number: {}",i => cmd.cause)
    };
    state.file_closein(i); // TODO error?
    Ok(())
}

pub fn closeout<ET:EngineType>(state: &mut ET::State, gullet:&mut ET::Gullet, cmd:CommandSource<ET>) -> Result<Whatsit<ET>, TeXError<ET>> {
    debug_log!(trace=>"\\closeout");
    let i = catch_prim!(gullet.get_int(state) => ("closeout",cmd));
    let i : usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => throw!("Invalid file number: {}",i => cmd.cause)
    };
    let apply = Box::new(move |s:&mut ET::State,_: &mut _| {
        s.file_closeout(i); // TODO error?
        Ok(())
    });
    Ok(Whatsit::new(apply))
}

pub static COUNT : &str = "count";
pub fn count_assign<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,global:bool) -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"Assigning \\count");
    let i = catch_prim!(gullet.get_int(state) => ("count",cmd));
    let i:usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => throw!("Not a valid register: {}",i => cmd.cause)
    };
    catch_prim!(gullet.mouth().skip_eq_char(state) => ("count",cmd));
    let v = catch_prim!(gullet.get_int(state) => ("count",cmd));
    debug_log!(debug=>"\\count{} = {}",i,v);
    state.set_int_register(i,v,global);
    Ok(())
}
pub fn count_get<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>) -> Result<ET::Int,TeXError<ET>> {
    debug_log!(trace=>"Getting \\count");
    let i = catch_prim!(gullet.get_int(state) => ("count",cmd));
    let i:usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => throw!("Not a valid register: {}",i => cmd.cause)
    };
    let v = state.get_int_register(i);
    debug_log!(debug=>"\\count{} == {}",i,v);
    Ok(v)
}

pub static COUNTDEF : &str = "countdef";
pub fn countdef<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,global:bool)
    -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"countdef");
    let name = catch_prim!(gullet.get_control_sequence(state) => (COUNTDEF,cmd));
    catch_prim!(set_relax::<ET>(state,&name,&cmd,global) => (COUNTDEF,cmd));
    catch_prim!(gullet.mouth().skip_eq_char(state) => (COUNTDEF,cmd));
    let num = catch_prim!(gullet.get_int(state) => ("countdef",cmd));
    if num.to_i64() < 0 {
        throw!("Invalid count register index: {}",num => cmd.cause)
    }
    let num = num.to_i64() as usize;
    let cmd = Command::new(BaseCommand::Int(ValueCommand::Register(num)),Some(&cmd));
    state.set_command_for_tk(name,Some(cmd),global);
    Ok(())
}

pub fn get_csname<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:&CommandSource<ET>,name:&'static str)
    -> Result<TeXStr<ET::Char>,TeXError<ET>>{
    debug_log!(trace=>"get_csname: {}",gullet.mouth().preview(200));
    let csidx = state.push_csname();
    let mut csname = Vec::with_capacity(16);
    while state.current_csname() == Some(csidx) {
        match catch_prim!(gullet.get_next_unexpandable(state) => (name,cmd)) {
            None => return file_end!(cmd.cause),
            Some(sc) => match sc.command {
                BaseCommand::Unexpandable {name:"endcsname",..} => state.pop_csname(),
                BaseCommand::Char{catcode:CategoryCode::Space,..} => csname.push(ET::Char::from(b' ')),
                BaseCommand::Char{char,..} => csname.push(char),
                o => throw!("Unexpected token in {}: {:?}",name,o => cmd.cause)
            }
        }
    }
    Ok(csname.into())
}

pub fn csname<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,f:TokenCont<ET>) -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"csname");
    let str = get_csname::<ET>(state,gullet,&cmd,"csname")?;
    debug_log!(trace=>"csname {}",str.to_string());
    match state.get_command(&str) {
        None => state.set_command(str.clone(), Some(Command::new(BaseCommand::Relax,Some(&cmd))), false),
        _ => ()
    }
    gullet.mouth().requeue(Token::new(BaseToken::CS(str),None));
    Ok(())
}

pub fn day<ET:EngineType>(state:&mut ET::State,cmd:CommandSource<ET>) -> Result<ET::Int,TeXError<ET>> {
    Ok(catch_prim!(ET::Int::from_i64(state.get_start_time().day() as i64) => ("day",cmd)))
}

pub static DEF : &str = "def";
pub fn def<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,global:bool,protected:bool,long:bool,outer:bool)
    -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"def");
    let csO = catch_prim!(gullet.mouth().get_next(state) => (DEF,cmd));
    let cs = match csO {
        None => file_end_prim!(DEF,cmd),
        Some((t,_)) => t
    };
    match &cs.base {
        BaseToken::Char(_,CategoryCode::Active) => (),
        BaseToken::CS(_) => (),
        _ => throw!("Command expected after \\def" => cs)
    }
    let (endswithbrace,arity,signature) = parse_signature::<ET>(state,gullet,&cmd,DEF)?;
    let mut replacement: Vec<ExpToken<ET>> = vec!();
    let mut partk = None;
    catch_prim!(gullet.get_group(state,&mut |_,t| match (std::mem::take(&mut partk),&t.base) {
        (None,BaseToken::Char(c,CategoryCode::Parameter)) => Ok(partk = Some(t)),
        (Some(t),BaseToken::Char(_,CategoryCode::Parameter)) =>
            Ok(replacement.push(ExpToken::ParamToken(t))),
        (Some(t),BaseToken::Char(c,_)) => {
            let u = c.to_usize();
            if u < 48 || u - 48 > (arity as usize) {
                throw!("Illegal parameter number {}",(u-48) => cmd.cause.clone())
            }
            Ok(replacement.push(ExpToken::Param(t,(u-49) as u8)))
        }
        (Some(_),_) =>
            throw!("Expected number after #, got {}",t => cmd.cause.clone()),
        (_,_) => Ok(replacement.push(ExpToken::Token(t)))
    }) => (DEF,cmd));
    let def = Command::new(BaseCommand::Def(Ptr::new(DefI{protected,long,outer,endswithbrace,arity,signature,replacement})),Some(&cmd));
    debug_log!(trace=>"def {:?} = {:?}",cs,def);
    state.set_command_for_tk(cs,Some(def),global);
    Ok(())
}

pub fn delcode_assign<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,global:bool)
                                      -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"Assigning delcode");
    let i = catch_prim!(gullet.get_int(state) => ("delcode",cmd));
    let c: ET::Char = match ET::Char::from_i64(i.to_i64()) {
        Some(i) => i,
        None => throw!("Not a valid character: {}",i => cmd.cause)
    };
    catch_prim!(gullet.mouth().skip_eq_char(state) => ("delcode",cmd));
    let i = catch_prim!(gullet.get_int(state) => ("delcode",cmd));
    debug_log!(debug=>"\\delcode '{}' = {}",c.char_str(),i);
    state.set_delcode(c,i,global);
    Ok(())
}

pub fn delcode_get<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>)
                                   -> Result<ET::Int,TeXError<ET>> {
    debug_log!(trace=>"Getting delcode");
    let i = catch_prim!(gullet.get_int(state) => ("delcode",cmd));
    let c: ET::Char = match ET::Char::from_i64(i.to_i64()) {
        Some(i) => i,
        None => throw!("Not a valid character: {}",i => cmd.cause)
    };
    let v = state.get_delcode(c);
    debug_log!(debug=>"\\delcode '{}' == {}",c.char_str(),v);
    Ok(v)
}

pub static DIMEN : &str = "dimen";

pub fn dimen_assign<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,global:bool)
    -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"Assigning \\dimen");
    let i = catch_prim!(gullet.get_int(state) => ("dimen",cmd));
    let i:usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => throw!("Not a valid register: {}",i => cmd.cause)
    };
    catch_prim!(gullet.mouth().skip_eq_char(state) => ("dimen",cmd));
    let v = catch_prim!(gullet.get_dim(state) => ("dimen",cmd));
    debug_log!(debug=>"\\dimen{} = {}",i,v);
    state.set_dim_register(i,v,global);
    Ok(())
}

pub fn dimen_get<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>)
    -> Result<ET::Dim,TeXError<ET>> {
    debug_log!(trace=>"Getting \\dimen");
    let i = catch_prim!(gullet.get_int(state) => ("dimen",cmd));
    let i:usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => throw!("Not a valid register: {}",i => cmd.cause)
    };
    let v = state.get_dim_register(i);
    debug_log!(debug=>"\\dimen{} == {}",i,v);
    Ok(v)
}

pub static DIMENDEF : &str = "dimendef";
pub fn dimendef<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,global:bool)
                                                     -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"dimendef");
    let name = catch_prim!(gullet.get_control_sequence(state) => (DIMENDEF,cmd));
    catch_prim!(set_relax::<ET>(state,&name,&cmd,global) => (DIMENDEF,cmd));
    catch_prim!(gullet.mouth().skip_eq_char(state) => (DIMENDEF,cmd));
    let num = catch_prim!(gullet.get_int(state) => (DIMENDEF,cmd));
    if num.to_i64() < 0 {
        throw!("Invalid dimen register index: {}",num => cmd.cause)
    }
    let num = num.to_i64() as usize;
    let ret = Command::new(BaseCommand::Dim(ValueCommand::Register(num)),Some(&cmd));
    state.set_command_for_tk(name,Some(ret),global);
    Ok(())
}
pub static DIVIDE : &str = "divide";
pub fn divide<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,global:bool)
    -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"\\divide");
    catch_prim!(gullet.mouth().skip_whitespace(state) => (DIVIDE,cmd));
    match catch_prim!(gullet.get_next_unexpandable(state) => (DIVIDE,cmd)) {
        None => file_end_prim!(DIVIDE,cmd),
        Some(ncmd) => match ncmd.command {
            BaseCommand::Int(a) => {
                macro_rules! finish {
                    ($old:expr,$nv:ident => $set:expr) => {{
                        catch_prim!(gullet.get_keyword(state,"by") => (DIVIDE,cmd));
                        let i = catch_prim!(gullet.get_int(state) => (DIVIDE,cmd));
                        if i.to_i64() == 0 {
                            throw!("Division by zero: {} / {}",$old,i => cmd.cause)
                        }
                        let $nv = $old / i;
                        $set;
                        Ok(())
                    }}
                }
                match a {
                    ValueCommand::Register(u) => finish!(state.get_int_register(u),nv => state.set_int_register(u,nv,global)),
                    ValueCommand::Primitive(name) => finish!(state.get_primitive_int(name),nv => state.set_primitive_int(name,nv,global)),
                    ValueCommand::Complex {name,..} if name == COUNT => {
                        let int = catch_prim!(gullet.get_int(state) => (DIVIDE,cmd));
                        let u = match int.try_into() {
                            Ok(u) => u,
                            _ => throw!("Not a valid register: {}",int => cmd.cause)
                        };
                        finish!(state.get_int_register(u),nv => state.set_int_register(u,nv,global))
                    }
                    _ => throw!("Unexpected token after \\divide" => cmd.cause)
                }
            }
            BaseCommand::Dim(a) => {
                macro_rules! finish {
                    ($old:expr,$nv:ident => $set:expr) => {{
                        catch_prim!(gullet.get_keyword(state,"by") => (DIVIDE,cmd));
                        let i = catch_prim!(gullet.get_int(state) => (DIVIDE,cmd)).to_i64();
                        if i == 0 {
                            throw!("Division by zero: {} / {}",$old,i => cmd.cause)
                        }
                        let $nv = $old.tex_div(i);
                        $set;
                        Ok(())
                    }}
                }
                match a {
                    ValueCommand::Register(u) => finish!(state.get_dim_register(u),nv => state.set_dim_register(u,nv,global)),
                    ValueCommand::Primitive(name) => finish!(state.get_primitive_dim(name),nv => state.set_primitive_dim(name,nv,global)),
                    ValueCommand::Complex {name,..} if name == DIMEN => {
                        let int = catch_prim!(gullet.get_int(state) => (DIVIDE,cmd));
                        let u = match int.try_into() {
                            Ok(u) => u,
                            _ => throw!("Not a valid register: {}",int => cmd.cause)
                        };
                        finish!(state.get_dim_register(u),nv => state.set_dim_register(u,nv,global))
                    }
                    _ => throw!("Unexpected token after \\divide" => cmd.cause)
                }
            }
            BaseCommand::Skip(a) => {
                macro_rules! finish {
                    ($old:expr,$nv:ident => $set:expr) => {{
                        catch_prim!(gullet.get_keyword(state,"by") => (DIVIDE,cmd));
                        let i = catch_prim!(gullet.get_int(state) => (DIVIDE,cmd)).to_i64();
                        if i == 0 {
                            throw!("Division by zero: {} / {}",$old,i => cmd.cause)
                        }
                        let $nv = $old.tex_div(i);
                        $set;
                        Ok(())
                    }}
                }
                match a {
                    ValueCommand::Register(u) => finish!(state.get_skip_register(u),nv => state.set_skip_register(u,nv,global)),
                    ValueCommand::Primitive(name) => finish!(state.get_primitive_skip(name),nv => state.set_primitive_skip(name,nv,global)),
                    ValueCommand::Complex {name,..} if name == SKIP => {
                        let int = catch_prim!(gullet.get_int(state) => (DIVIDE,cmd));
                        let u = match int.try_into() {
                            Ok(u) => u,
                            _ => throw!("Not a valid register: {}",int => cmd.cause)
                        };
                        finish!(state.get_skip_register(u),nv => state.set_skip_register(u,nv,global))
                    }
                    _ => throw!("Unexpected token after \\divide" => cmd.cause)
                }
            }
            BaseCommand::MuSkip(a) => {
                macro_rules! finish {
                    ($old:expr,$nv:ident => $set:expr) => {{
                        catch_prim!(gullet.get_keyword(state,"by") => (DIVIDE,cmd));
                        let i = catch_prim!(gullet.get_int(state) => (DIVIDE,cmd)).to_i64();
                        if i == 0 {
                            throw!("Division by zero: {} / {}",$old,i => cmd.cause)
                        }
                        let $nv = $old.tex_div(i);
                        $set;
                        Ok(())
                    }}
                }
                match a {
                    ValueCommand::Register(u) => finish!(state.get_muskip_register(u),nv => state.set_muskip_register(u,nv,global)),
                    ValueCommand::Primitive(name) => finish!(state.get_primitive_muskip(name),nv => state.set_primitive_muskip(name,nv,global)),
                    ValueCommand::Complex {name,..} if name == MUSKIP => {
                        let int = catch_prim!(gullet.get_int(state) => (DIVIDE,cmd));
                        let u = match int.try_into() {
                            Ok(u) => u,
                            _ => throw!("Not a valid register: {}",int => cmd.cause)
                        };
                        finish!(state.get_muskip_register(u),nv => state.set_muskip_register(u,nv,global))
                    }
                    _ => throw!("Unexpected token after \\divide" => cmd.cause)
                }
            }
            o => throw!("expected register after \\divide;got:{:?}",o => cmd.cause)
        }
    }
}

pub fn dump<ET:EngineType>()
                                    -> Result<(), TeXError<ET>> {
    debug_log!(trace=>"\\dump");
    // TODO
    Ok(())
}

pub static EDEF: &str = "edef";
pub fn edef<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,global:bool,protected:bool,long:bool,outer:bool)
                                                -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"edef");
    let csO = catch_prim!(gullet.mouth().get_next(state) => (EDEF,cmd));
    let cs = match csO {
        None => file_end_prim!(DEF,cmd),
        Some((t,_)) => t
    };
    match &cs.base {
        BaseToken::Char(_,CategoryCode::Active) => (),
        BaseToken::CS(_) => (),
        _ => throw!("Command expected after \\def" => cs)
    }
    debug_log!(trace=>"edef: {}",cs);
    let (endswithbrace,arity,signature) = parse_signature::<ET>(state,gullet,&cmd,EDEF)?;
    let mut replacement: Vec<ExpToken<ET>> = vec!();
    let mut partk = None;
    catch_prim!(gullet.get_expanded_group(state,false,true,false,&mut |_,t| match (std::mem::take(&mut partk),&t.base) {
        (None,BaseToken::Char(c,CategoryCode::Parameter)) => Ok(partk = Some(t)),
        (Some(t),BaseToken::Char(_,CategoryCode::Parameter)) =>
            Ok(replacement.push(ExpToken::ParamToken(t))),
        (Some(t),BaseToken::Char(c,_)) => {
            let u = c.to_usize();
            if u < 48 || u - 48 > (arity as usize) {
                throw!("Illegal parameter number {}",(u-48) => cmd.cause.clone())
            }
            Ok(replacement.push(ExpToken::Param(t,(u-49) as u8)))
        }
        (Some(_),_) =>
            throw!("Expected number after #, got {}",t => cmd.cause.clone()),
        (_,_) => Ok(replacement.push(ExpToken::Token(t)))
    }) => (DEF,cmd));
    let def = Command::new(BaseCommand::Def(Ptr::new(DefI{protected,long,outer,endswithbrace,arity,signature,replacement})),Some(&cmd));
    debug_log!(trace=>"edef {:?} = {:?}",cs,def);
    state.set_command_for_tk(cs,Some(def),global);
    Ok(())
}

pub static ELSE: &str = "else";
pub fn else_<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>)
    -> Result<(),TeXError<ET>> {
    match gullet.current_conditional() {
        (None,_) => throw!("Not in a conditional" => cmd.cause),
        (Some(ConditionalBranch::True(name)),i) =>
            catch_prim!(crate::engine::gullet::methods::else_loop::<ET>(gullet,state,name,i,false) => (ELSE,cmd)),
        (Some(ConditionalBranch::Case(_,_)),i) =>
            catch_prim!(crate::engine::gullet::methods::else_loop::<ET>(gullet,state,"ifcase",i,false) => ("ifcase",cmd)),
        o => unreachable!("{:?}\nat:{}\n{}\n",o,gullet.mouth().file_line(),gullet.mouth().preview(200))
    }
    Ok(())
}

pub fn end<ET:EngineType>()
    -> Result<(),TeXError<ET>> {
    todo!("end")
}

pub fn endcsname<ET:EngineType>(cmd:CommandSource<ET>) -> Result<(),TeXError<ET>> {
    throw!("Unexpected \\endcsname; not in a \\csname" => cmd.cause)
}

pub fn endgroup<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>)
    -> Result<(),TeXError<ET>> {
    let mut ret = gullet.mouth().new_tokensource();
    match state.stack_pop() {
        Some((mut v,GroupType::CS)) => {
            for t in v.drain(..) {
                ret.push(t);
            }
            gullet.mouth().push_tokens(ret);
            Ok(())
        }
        _ => throw!("No group to end" => cmd.cause)
    }
}

pub fn endinput<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>)
                            -> Result<(),TeXError<ET>> {
    gullet.mouth().endinput(state);
    Ok(())
}

pub fn endlinechar_assign<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,global:bool)
    -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"Assigning \\endlinechar");
    catch_prim!(gullet.mouth().skip_eq_char(state) => ("endlinechar",cmd));
    let i = catch_prim!(gullet.get_int(state) => ("endlinechar",cmd));
    let c = match i.to_i64() {
        -1|255 => None,
        j => match ET::Char::from_i64(j) {
            Some(c) => Some(c),
            None => throw!("Not a valid character: {}",j => cmd.cause)
        }
    };
    debug_log!(debug=>"\\endlinechar = {:?}",c.map(|c| c.char_str()));
    state.set_endlinechar(c,global);
    Ok(())
}
pub fn endlinechar_get<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>)
    -> Result<ET::Int,TeXError<ET>> {
    debug_log!(trace=>"Getting \\endlinechar");
    let c = match state.get_endlinechar() {
        None => -1,
        Some(c) => c.to_usize() as i64
    };
    debug_log!(debug=>"\\endlinechar == {:?}",c);
    Ok(ET::Int::from_i64::<ET>(c).unwrap())
}

// \errhelp
pub static ERRMESSAGE : &str = "errmessage";

pub fn errmessage<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>)
    -> Result<(),TeXError<ET>> {
    debug_log!(debug=>"errmessage");
    catch_prim!(gullet.mouth().skip_whitespace(state) => (ERRMESSAGE,cmd));
    let errmsg = catch_prim!(gullet.get_braced_string(state) => (ERRMESSAGE,cmd));
    let eh = state.get_primitive_toks("errhelp");
    // TODO errhelp
    Err(TeXError{
        msg:errmsg + "\n\n" + &gullet.mouth().file_line(),
        cause:Some(cmd.cause),
        source:None
    }.into())
}

pub fn errorstopmode<ET:EngineType>()
                                  -> Result<(), TeXError<ET>> {
    debug_log!(trace=>"\\errorstopmode");
    // TODO
    Ok(())
}

pub fn escapechar_assign<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,global:bool)
    -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"Assigning \\escapechar");
    catch_prim!(gullet.mouth().skip_eq_char(state) => ("escapechar",cmd));
    let i = catch_prim!(gullet.get_int(state) => ("escapechar",cmd));
    let c = match i.to_i64() {
        -1|255 => None,
        j => match ET::Char::from_i64(j) {
            Some(c) => Some(c),
            None => throw!("Not a valid character: {}",j => cmd.cause)
        }
    };
    debug_log!(debug=>"\\escapechar = {:?}",c.map(|c| c.char_str()));
    state.set_escapechar(c,global);
    Ok(())
}
pub fn escapechar_get<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>) -> Result<ET::Int,TeXError<ET>> {
    debug_log!(trace=>"Getting \\escapechar");
    let c = match state.get_escapechar() {
        None => -1,
        Some(c) => c.to_usize() as i64
    };
    debug_log!(debug=>"\\escapechar == {}",c);
    Ok(ET::Int::from_i64::<ET>(c).unwrap())
}

pub static EXPANDAFTER : &str = "expandafter";
pub fn expandafter<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,f:TokenCont<ET>)
    -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"expandafter");
    let first = match catch_prim!(gullet.mouth().get_next(state) => (EXPANDAFTER,cmd)){
        None => file_end_prim!(EXPANDAFTER,cmd),
        Some((t,_)) => t
    };
    let next = match catch_prim!(gullet.mouth().get_next(state) => (EXPANDAFTER,cmd)){
        None => file_end_prim!(EXPANDAFTER,cmd),
        Some((t,_)) => t
    };
    match gullet.expand(state,resolve_token::<ET>(state,next))? {
        None => (),
        Some(next) => {
            gullet.mouth().requeue(next.source.cause);
        }
    }
    gullet.mouth().requeue(first);
    Ok(())
}

pub static FI : &str = "fi";
pub fn fi<ET:EngineType>(_state:&mut ET::State,gullet:&mut ET::Gullet,_cmd:CommandSource<ET>)
    -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"...end of conditional.");
    gullet.pop_conditional();
    Ok(())
}

pub static FONT :&str = "font";
pub fn font_assign<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,global:bool)
    -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"Assigning \\font");
    let cs = catch_prim!(gullet.get_control_sequence(state) => (FONT,cmd));
    catch_prim!(gullet.mouth().skip_eq_char(state) => (FONT,cmd));
    let mut fontname = catch_prim!(gullet.get_string(state) => (FONT,cmd)).to_string();
    if !fontname.ends_with(".tfm") {
        fontname = fontname + ".tfm"
    }
    let mut font = catch_prim!(state.fontstore_mut().get_new(&fontname) => (FONT,cmd));
    match catch_prim!(gullet.get_keywords(state,vec!("at","scaled")) => (FONT,cmd)) {
        Some(s) if s == "at" => {
            let dim = catch_prim!(gullet.get_dim(state) => (FONT,cmd));
            font.set_at(dim.to_sp());
        }
        Some(s) if s == "scaled" => {
            let r = catch_prim!(crate::engine::gullet::numeric_methods::read_float::<ET>(gullet,state,b'0',false) => (FONT,cmd));
            let new_at = ((font.get_at() as f64) * r).round() as i64;
            font.set_at(new_at);
        }
        _ => ()
    }
    let fontcmd = Command::new(BaseCommand::Font(font),Some(&cmd));
    state.set_command_for_tk(cs,Some(fontcmd),global);
    Ok(())
}
pub fn font_get<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>)
    -> Result<ET::Font,TeXError<ET>> {
    debug_log!(trace=>"Getting \\font");
    Ok(state.get_current_font().clone())
}

pub fn fontdimen_assign<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,global:bool)
    -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"Assigning \\fontdimen");
    let o = catch_prim!(gullet.get_int(state) => ("fontdimen",cmd));
    let i = match o.try_into() {
        Ok(i) => i,
        Err(_) => throw!("Invalid font dimension index" => cmd.cause)
    };
    let mut font = catch_prim!(gullet.get_font(state) => ("fontdimen",cmd));
    catch_prim!(gullet.mouth().skip_eq_char(state) => ("fontdimen",cmd));
    let dim = catch_prim!(gullet.get_dim(state) => ("fontdimen",cmd));
    font.set_dim::<ET::Dim>(i,dim);
    Ok(())
}

pub fn fontdimen_get<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>)
    -> Result<ET::Dim,TeXError<ET>> {
    debug_log!(trace=>"Getting \\fontdimen");
    let o = catch_prim!(gullet.get_int(state) => ("fontdimen",cmd));
    let i = match o.try_into() {
        Ok(i) => i,
        Err(_) => throw!("Invalid font dimension index: {}",o => cmd.cause)
    };
    let font = catch_prim!(gullet.get_font(state) => ("fontdimen",cmd));
    Ok(font.get_dim::<ET::Dim>(i))
}

pub fn futurelet<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,global:bool)
                                -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"\\futurelet");
    let cs = match catch_prim!(gullet.mouth().get_next(state) => ("futurelet",cmd)) {
        None => file_end_prim!("futurelet",cmd),
        Some((t,_)) => match &t.base {
            BaseToken::Char(_,CategoryCode::Active) => t,
            BaseToken::CS(_) => t,
            _ => throw!("Expected control sequence after \\futurelet" => cmd.cause)
        }
    };
    let first = match catch_prim!(gullet.mouth().get_next(state) => ("futurelet",cmd)) {
        None => file_end_prim!("futurelet",cmd),
        Some((t,_)) => t
    };
    let second = match catch_prim!(gullet.mouth().get_next(state) => ("futurelet",cmd)) {
        None => file_end_prim!("futurelet",cmd),
        Some((t,_)) => t
    };
    let newcmd = match &second.base {
        BaseToken::Char(c,CategoryCode::Active) => state.get_ac_command(c).cloned(),
        BaseToken::CS(name) => state.get_command(name).cloned(),
        BaseToken::Char(c,cc) =>
            Some(Command::new(BaseCommand::Char{char:*c,catcode:*cc},Some(&cmd)))
    };
    debug_log!(debug=>"\\futurelet: setting {} to {:?}",cs,newcmd);
    state.set_command_for_tk(cs,newcmd,global);
    let mut ret = gullet.mouth().new_tokensource();
    ret.push(first);
    ret.push(second);
    gullet.mouth().push_tokens(ret);
    Ok(())
}


pub fn gdef<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,global:bool,protected:bool,long:bool,outer:bool)
                                                 -> Result<(),TeXError<ET>> {
    def::<ET>(state,gullet,cmd,true,protected,long,outer)
}

pub static GLOBAL: &str = "global";
pub fn global<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,global_:bool,protected_:bool,long_:bool,outer_:bool)
                                                 -> Result<(),TeXError<ET>> {
    debug_log!(trace => "\\global");
    catch_prim!(gullet.mouth().skip_whitespace(state) => (GLOBAL,cmd));
    match catch_prim!(gullet.get_next_stomach_command(state) => (GLOBAL,cmd)) {
        None => file_end_prim!(GLOBAL,cmd),
        Some(c) => match c.command {
            BaseStomachCommand::Assignment {name:Some("global"),..} => global::<ET>(state,gullet,cmd,true,protected_,long_,outer_),
            BaseStomachCommand::Assignment{name:Some("protected"),..} => super::etex::protected::<ET>(state,gullet,cmd,true,protected_,long_,outer_),
            BaseStomachCommand::Assignment {name:Some("long"),..} => long::<ET>(state,gullet,cmd,true,protected_,long_,outer_),
            BaseStomachCommand::Assignment {name:Some("outer"),..} => outer::<ET>(state,gullet,cmd,true,protected_,long_,outer_),
            BaseStomachCommand::Assignment {name:Some("def"),..} => def::<ET>(state,gullet,cmd,true,protected_,long_,outer_),
            BaseStomachCommand::Assignment {name:Some("edef"),..} => edef::<ET>(state,gullet,cmd,true,protected_,long_,outer_),
            BaseStomachCommand::Assignment {name:Some("gdef"),..} => gdef::<ET>(state,gullet,cmd,true,protected_,long_,outer_),
            BaseStomachCommand::Assignment {name:Some("xdef"),..} => xdef::<ET>(state,gullet,cmd,true,protected_,long_,outer_),
            BaseStomachCommand::Assignment {set,..} => Ok(catch_prim!(set(state,gullet,c.source,true) => (GLOBAL,cmd))),
            BaseStomachCommand::ValueAss(set) => Ok(catch_prim!(set(state,gullet,c.source,true) => (GLOBAL,cmd))),
            o => todo!("global: {:?} at {}",o,gullet.mouth().preview(100))
        }
    }
}

pub static HBOX: &str = "hbox";
pub fn hbox<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>)
                                         -> Result<CloseBoxFun<ET>,TeXError<ET>> {
    debug_log!(trace=>"\\hbox");
    let (to,spread) = match catch_prim!(gullet.get_keywords(state,vec!("spread","to")) => (HBOX,cmd)) {
        None => (None,None),
        Some(s) if s == "to" => {
            let a = catch_prim!(gullet.get_dim(state) => (HBOX,cmd));
            (Some(a),None)
        },
        Some(s) if s == "spread" => {
            let a = catch_prim!(gullet.get_dim(state) => (HBOX,cmd));
            (None,Some(a))
        },
        _ => unreachable!()
    };
    while let Some(next) = catch_prim!(gullet.get_next_unexpandable(state) => (HBOX,cmd)) {
        match next.command {
            BaseCommand::Char{catcode:CategoryCode::Space,..} => {},
            BaseCommand::Relax => {},
            BaseCommand::Char{catcode:CategoryCode::BeginGroup,..} => {
                state.stack_push(GroupType::Box(BoxMode::H));
                let mut ret = gullet.mouth().new_tokensource();
                for t in state.get_primitive_toks("everyhbox") {
                    ret.push(t.clone());
                }
                gullet.mouth().push_tokens(ret);
                return Ok(Box::new(move |s,gu,children| {
                    Some(HVBox::H(HBox {
                        children, to:to.clone(), spread:spread.clone(),
                        ..Default::default()
                    }))
                }))
            }
            _ => throw!("Expected begin group, found {:?}",next.source.cause => cmd.cause)
        }
    }
    file_end_prim!("hbox",cmd);
}

pub static HRULE: &str = "hrule";
pub fn hrule<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>)
                            -> Result<(),TeXError<ET>> {
    debug_log!(trace => "\\hrule");
    let mut width = None;
    let mut height = None;
    let mut depth = None;
    loop {
        match catch_prim!(gullet.get_keywords(state,vec!("width","height","depth")) => (HRULE,cmd)) {
            None => break,
            Some(s) => {
                let val = catch_prim!(gullet.get_dim(state) => (HRULE,cmd));
                match s {
                    "width" => width = Some(val),
                    "height" => height = Some(val),
                    "depth" => depth = Some(val),
                    _ => unsafe{unreachable_unchecked()}
                }
            }
        }
    }
    state.push_node(SimpleNode::Rule {
        width,height,depth,axis:HorV::Horizontal
    }.as_node());
    Ok(())
}

pub fn hyphenation<ET:EngineType>(state: &mut ET::State, gullet:&mut ET::Gullet,cmd:CommandSource<ET>)
                               -> Result<(), TeXError<ET>> {
    debug_log!(trace=>"\\hyphenation");
    // TODO
    catch_prim!(gullet.mouth().read_argument(state,&mut |_,_| Ok(())) => ("hyphenation",cmd));
    Ok(())
}

pub fn hyphenchar_assign<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,global:bool)
    -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"Assigning \\hyphenchar");
    let mut font = catch_prim!(gullet.get_font(state) => ("hyphenchar",cmd));
    catch_prim!(gullet.mouth().skip_eq_char(state) => ("hyphenchar",cmd));
    let i = catch_prim!(gullet.get_int(state) => ("hyphenchar",cmd)).to_i64();
    debug_log!(debug=>"\\hyphenchar\\{:?} = {:?}",font,i);
    font.set_hyphenchar(i);
    Ok(())
}
pub fn hyphenchar_get<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>) -> Result<ET::Int,TeXError<ET>> {
    debug_log!(trace=>"Getting \\hyphenchar");
    let font = catch_prim!(gullet.get_font(state) => ("hyphenchar",cmd));
    debug_log!(debug=>"\\hyphenchar == {:?}",font.get_hyphenchar());
    Ok(catch_prim!(ET::Int::from_i64::<ET>(font.get_hyphenchar()) => ("hyphenchar",cmd)))
}

pub fn if_<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>)
    -> Result<bool,TeXError<ET>> {
    debug_log!(trace=>"if");
    let first = get_if_token::<ET>(state,gullet,&cmd,"if")?;
    let second = get_if_token::<ET>(state,gullet,&cmd,"if")?;
    debug_log!(trace=>"if: {:?} == {:?}",first,second);
    Ok(match (first,second) {
        (None,_) | (_,None) => false,
        (Some(f),Some(s)) => match (f.source.cause.base,s.source.cause.base) {
            (BaseToken::Char(f,_),BaseToken::Char(s,_)) => f == s,
            (BaseToken::CS(_),BaseToken::CS(_)) => true,
            _ => false
        }
    })
}

pub static IFCASE: &str = "ifcase";
pub fn ifcase<ET:EngineType>() -> Result<bool,TeXError<ET>> {
    unreachable!("executed in Gullet")
}

pub static IFCAT : &str = "ifcat";
pub fn ifcat<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>)
    -> Result<bool,TeXError<ET>> {
    debug_log!(trace=>"ifcat");
    let first = catch_prim!(get_if_token::<ET>(state,gullet,&cmd,IFCAT) => (IFCAT,cmd));
    let second = catch_prim!(get_if_token::<ET>(state,gullet,&cmd,IFCAT) => (IFCAT,cmd));
    debug_log!(trace=>"ifcat: {:?} == {:?}",first,second);
    let first = match first {
        None => return Ok(false),
        Some(first) => match first.source.cause.base {
            BaseToken::Char(_,cc) => cc,
            BaseToken::CS(name) => match first.command {
                BaseCommand::Char{catcode,..} => catcode,
                _ => CategoryCode::Escape
            }
        }
    };
    let second = match second {
        None => return Ok(false),
        Some(second) => match second.source.cause.base {
            BaseToken::Char(_,cc) => cc,
            BaseToken::CS(name) => match second.command {
                BaseCommand::Char{catcode,..} => catcode,
                _ => CategoryCode::Escape
            }
        }
    };
    Ok(first == second)
}

pub fn get_if_token<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:&CommandSource<ET>,name:&'static str)
    -> Result<Option<ResolvedToken<ET>>,TeXError<ET>> {
    // need to be careful not to expand \else and \fi before conditional is done.
    while let Some((t,e)) = catch_prim!(gullet.mouth().get_next(state) => (name,cmd)) {
        let r = resolve_token(state,t);
        match r.command {
            BaseCommand::Expandable {name,..} if e && (name == ELSE || name == FI) && (match gullet.current_conditional() {
                (Some(ConditionalBranch::None(_)),_) => true,
                _ => false
            }) => {
                gullet.mouth().requeue(r.source.cause);
                return Ok(None)
            }
            _ if e => match gullet.expand(state,r)? {
                Some(c) => return Ok(Some(c)),
                None => {}
            }
            _ => return Ok(Some(r))
        }
    }
    file_end_prim!(name,cmd)
}


pub fn ifdim<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>)
                            -> Result<bool,TeXError<ET>> {
    debug_log!(trace=>"ifdim");
    let i1 = catch_prim!(gullet.get_dim(state) => ("ifdim",cmd));
    let rel = match catch_prim!(gullet.get_keywords(state,vec!["<",">","="]) => ("ifdim",cmd)) {
        None => throw!("Expected one of '<','>','='".to_string() => cmd.cause),
        Some(r) => r
    };
    let i2 = catch_prim!(gullet.get_dim(state) => ("ifdim",cmd));
    match rel {
        "<" => Ok(i1.to_sp() < i2.to_sp()),
        ">" => Ok(i1.to_sp() > i2.to_sp()),
        "=" => Ok(i1.to_sp() == i2.to_sp()),
        _ => unreachable!()
    }
}

pub fn ifeof<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>)
    -> Result<bool,TeXError<ET>> {
    debug_log!(trace=>"ifeof");
    let i = catch_prim!(gullet.get_int(state) => ("ifeof",cmd));
    let i : usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => throw!("Invalid file number: {}",i => cmd.cause)
    };
    if i == 18 { return Ok(false) }
    let f = match state.get_open_in_file(i) {
        None => throw!("No in file open at index: {}",i => cmd.cause),
        Some(f) => f
    };
    Ok(f.eof::<ET>(state))
}

pub fn ifhmode<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>)
                            -> Result<bool,TeXError<ET>> {
    debug_log!(trace=>"ifhmode");
    Ok(match state.mode() {
        TeXMode::Horizontal | TeXMode::RestrictedHorizontal => true,
        _ => false
    })
}

pub fn ifinner<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>)
                              -> Result<bool,TeXError<ET>> {
    debug_log!(trace=>"ifhmode");
    Ok(match state.mode() {
        TeXMode::RestrictedHorizontal | TeXMode::InternalVertical | TeXMode::Math => true,
        _ => false
    })
}

pub fn ifmmode<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>)
                              -> Result<bool,TeXError<ET>> {
    debug_log!(trace=>"ifmmode");
    Ok(match state.mode() {
        TeXMode::Math | TeXMode::Displaymath => true,
        _ => false
    })
}

pub fn ifnum<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>)
    -> Result<bool,TeXError<ET>> {
    debug_log!(trace=>"ifnum");
    let i1 = catch_prim!(gullet.get_int(state) => ("ifnum",cmd));
    let rel = match catch_prim!(gullet.get_keywords(state,vec!["<",">","="]) => ("ifnum",cmd)) {
        None => throw!("Expected one of '<','>','='" => cmd.cause),
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

pub fn ifodd<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>)
    -> Result<bool,TeXError<ET>> {
    debug_log!(trace=>"ifodd");
    let num = catch_prim!(gullet.get_int(state) => ("ifodd",cmd));
    Ok(num.to_i64() % 2 != 0)
}


pub fn ifvmode<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>)
                              -> Result<bool,TeXError<ET>> {
    debug_log!(trace=>"ifvmode");
    Ok(match state.mode() {
        TeXMode::Vertical | TeXMode::InternalVertical => true,
        _ => false
    })
}

pub fn ifx<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>)
    -> Result<bool,TeXError<ET>> {
    debug_log!(trace=>"ifx");
    let t1 = match catch_prim!(gullet.mouth().get_next(state) => ("ifx",cmd)) {
        None => file_end_prim!("ifx",cmd),
        Some((t,e)) => resolve_token::<ET>(state,t).with_expand(e)
    };
    let t2 = match catch_prim!(gullet.mouth().get_next(state) => ("ifx",cmd)) {
        None => file_end_prim!("ifx",cmd),
        Some((t,e)) => resolve_token::<ET>(state,t).with_expand(e)
    };
    debug_log!(trace=>"ifx: {} == {}?",t1.source.cause,t2.source.cause);
    Ok(if t1.expand && t2.expand { t1.command == t2.command }
    else if !t1.expand && !t2.expand { t1.source.cause == t2.source.cause }
    else { false })
}

pub fn ignorespaces<ET:EngineType>(state: &mut ET::State, gullet:&mut ET::Gullet, cmd:CommandSource<ET>)
                                  -> Result<(), TeXError<ET>> {
    debug_log!(trace=>"\\ignorespaces");
    loop {
        match catch_prim!(gullet.get_next_stomach_command(state) => ("ignorespaces",cmd)) {
            None => return Ok(()),
            Some(sc) => match sc.command {
                BaseStomachCommand::Space => (),
                _ => {
                    gullet.mouth().requeue(sc.source.cause);
                    return Ok(())
                }
            }
        }
    }
}

pub static IMMEDIATE: &str ="immediate";
pub fn immediate<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>)
                                                 -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"immediate");
    match catch_prim!(gullet.get_next_stomach_command(state) => (IMMEDIATE,cmd)) {
        None => file_end_prim!(IMMEDIATE,cmd),
        Some(sc) => match sc.command {
            BaseStomachCommand::Whatsit { name, apply } => {
                let wi = catch_prim!(apply(state,gullet,sc.source) => (IMMEDIATE,cmd));
                catch_prim!(wi.apply(state,gullet) => (IMMEDIATE,cmd));
                Ok(())
            }
            _ => {
                gullet.mouth().requeue(sc.source.cause);
                Ok(())
            }
        }
    }
}

pub fn input<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>)
    -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"input");
    let filename = catch_prim!(gullet.get_string(state) => ("input",cmd)).to_string();
    debug_log!(trace=>"input: {}",filename);
    if filename.is_empty() {
        panic!("HERE: {}",gullet.mouth().file_line())
    }
    let file = state.filesystem().get(&filename);
    debug_log!(trace=>"input resolved: {:?}",file.path());
    if !file.exists() {
        throw!("I can't find file `{}'",filename => cmd.cause)
    } else {
        (state.outputs().file_open)(file.path().to_str().unwrap());
        gullet.mouth().push_file(&file);
        Ok(())
    }
}

pub fn inputlineno<ET:EngineType>(gullet:&mut ET::Gullet,cmd:CommandSource<ET>) -> Result<ET::Int,TeXError<ET>> {
    Ok(catch_prim!(ET::Int::from_i64(
        gullet.mouth().line_no() as i64
    ) => ("month",cmd)))
}

pub fn jobname<ET:EngineType>(state:&mut ET::State,f:TokenCont<ET>)
                         -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"jobname");
    string_to_tokens::<ET>(state.get_jobname().as_bytes(),state,f)
}

pub fn lccode_assign<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,global:bool)
    -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"Assigning lower case character");
    let i = catch_prim!(gullet.get_int(state) => ("lccode",cmd));
    let c: ET::Char = match ET::Char::from_i64(i.to_i64()) {
        Some(i) => i,
        None => throw!("Not a valid character: {}",i => cmd.cause)
    };
    catch_prim!(gullet.mouth().skip_eq_char(state) => ("lccode",cmd));
    let i = catch_prim!(gullet.get_int(state) => ("lccode",cmd));
    let lc: ET::Char = match ET::Char::from_i64(i.to_i64()) {
        Some(i) => i,
        None => throw!("Not a valid character: {}",i => cmd.cause)
    };
    debug_log!(debug=>"\\lccode '{}' = {}",c.char_str(),lc.char_str());
    state.set_lccode(c,lc,global);
    Ok(())
}

pub fn lccode_get<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>)
    -> Result<ET::Int,TeXError<ET>> {
    debug_log!(trace=>"Getting lower case character");
    let i = catch_prim!(gullet.get_int(state) => ("lccode",cmd));
    let c: ET::Char = match ET::Char::from_i64(i.to_i64()) {
        Some(i) => i,
        None => throw!("Not a valid character: {}",i => cmd.cause)
    };
    let v = catch_prim!(ET::Int::from_i64(state.get_lccode(&c).to_usize() as i64) => ("lccode",cmd));
    debug_log!(debug=>"\\lccode '{}' == {}",c.char_str(),v);
    Ok(v)
}

pub static LET : &str = "let";
pub fn let_<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,globally:bool)
    -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"let");
    let csO = catch_prim!(gullet.mouth().get_next(state) => (LET,cmd));
    let cs = match &csO {
        None => file_end_prim!(LET,cmd),
        Some((t,_)) => &t.base
    };
    match cs {
        BaseToken::Char(_,CategoryCode::Active) | BaseToken::CS(_) => (),
        _ => throw!("Expected a control sequence" => csO.unwrap().0)
    }
    catch_prim!(gullet.mouth().skip_eq_char(state) => (LET,cmd));
    let cm = match catch_prim!(gullet.mouth().get_next(state) => (LET,cmd)) {
        Some((t,_)) => t,
        None =>file_end_prim!(LET,cmd)
    };
    let cmd = match cm.base {
        BaseToken::Char(c,CategoryCode::Active) => state.get_ac_command(&c).map(|c|c.clone().copy_with(&cmd)),
        BaseToken::CS(name) => state.get_command(&name).map(|c|c.clone().copy_with(&cmd)),
        BaseToken::Char(c,cc) =>
            Some(Command::new(BaseCommand::Char{char:c,catcode:cc},Some(&cmd))),
    };
    debug_log!(debug=>"let: {} = {:?}",cs,cmd);
    match cs {
        BaseToken::Char(c,_) => state.set_ac_command(*c,cmd,globally),
        BaseToken::CS(name) => state.set_command(name.clone(),cmd,globally)
    }
    Ok(())
}

pub fn long<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd: CommandSource<ET>,global_:bool,protected_:bool,long_:bool,outer_:bool)
                                                -> Result<(),TeXError<ET>> {
    debug_log!(trace => "\\long");
    match catch_prim!(gullet.get_next_stomach_command(state) => ("long",cmd)) {
        None => file_end_prim!("long",cmd),
        Some(c) => match c.command {
            BaseStomachCommand::Assignment {name:Some("global"),..} => global::<ET>(state,gullet,cmd,global_,protected_,true,outer_),
            BaseStomachCommand::Assignment {name:Some("protected"),..} => super::etex::protected::<ET>(state,gullet,cmd,global_,protected_,true,outer_),
            BaseStomachCommand::Assignment {name:Some("long"),..} => long::<ET>(state,gullet,cmd,global_,protected_,true,outer_),
            BaseStomachCommand::Assignment {name:Some("outer"),..} => outer::<ET>(state,gullet,cmd,global_,protected_,true,outer_),
            BaseStomachCommand::Assignment {name:Some("def"),..} => def::<ET>(state,gullet,cmd,global_,protected_,true,outer_),
            BaseStomachCommand::Assignment {name:Some("edef"),..} => edef::<ET>(state,gullet,cmd,global_,protected_,true,outer_),
            BaseStomachCommand::Assignment {name:Some("gdef"),..} => gdef::<ET>(state,gullet,cmd,global_,protected_,true,outer_),
            BaseStomachCommand::Assignment {name:Some("xdef"),..} => xdef::<ET>(state,gullet,cmd,global_,protected_,true,outer_),
            _ => throw!("Expected a macro definition after \\long" => cmd.cause)
        }
    }
}
pub static LOWERCASE : &str = "lowercase";
pub fn lowercase<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>)
                                         -> Result<(),TeXError<ET>> {
    debug_log!(trace => "\\lowercase");
    catch_prim!(expand_until_space::<ET>(gullet,state) => (LOWERCASE,cmd));
    let mut ret = gullet.mouth().new_tokensource();
    catch_prim!(gullet.get_group(state,&mut |s,next| match &next.base {
        BaseToken::Char(c,cc) => {
            let nc = s.get_lccode(c);
            Ok(ret.push(Token::new(BaseToken::Char(nc, *cc), None)))
        }
        _ => Ok(ret.push(next))
    }) => (LOWERCASE,cmd));
    gullet.mouth().push_tokens(ret);
    Ok(())
}

pub static MATHCHARDEF : &str = "mathchardef";
pub fn mathchardef<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,globally:bool)
    -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"mathchardef");
    let csO = catch_prim!(gullet.mouth().get_next(state) => (MATHCHARDEF,cmd));
    let cs = match csO {
        None => file_end_prim!(MATHCHARDEF,cmd),
        Some((t,_)) => t
    };
    match &cs.base {
        BaseToken::Char(_,CategoryCode::Active) => (),
        BaseToken::CS(_) => (),
        _ => throw!("Command expected after \\mathchardef" => cs)
    }
    catch_prim!(gullet.mouth().skip_eq_char(state) => (MATHCHARDEF,cmd));
    let i = catch_prim!(gullet.get_int(state) => (MATHCHARDEF,cmd)).to_i64();
    if i < 0 {
        throw!("Invalid math char: {}",i => cmd.cause)
    }
    state.set_command_for_tk(cs,Some(Command::new(BaseCommand::MathChar(i as u32),Some(&cmd))),globally);
    Ok(())
}

pub fn mathcode_assign<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,global:bool)
                                    -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"Assigning math code");
    let i = catch_prim!(gullet.get_int(state) => ("mathcode",cmd));
    let c: ET::Char = match ET::Char::from_i64(i.to_i64()) {
        Some(i) => i,
        None => throw!("Not a valid character: {}",i => cmd.cause)
    };
    catch_prim!(gullet.mouth().skip_eq_char(state) => ("lccode",cmd));
    let i = catch_prim!(gullet.get_int(state) => ("mathcode",cmd));
    debug_log!(debug=>"\\mathcode '{}' = {}",c.char_str(),i);
    state.set_mathcode(c,i,global);
    Ok(())
}

pub fn mathcode_get<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>)
                                 -> Result<ET::Int,TeXError<ET>> {
    debug_log!(trace=>"Getting mathcode");
    let i = catch_prim!(gullet.get_int(state) => ("mathcode",cmd));
    let c: ET::Char = match ET::Char::from_i64(i.to_i64()) {
        Some(i) => i,
        None => throw!("Not a valid character: {}",i => cmd.cause)
    };
    let v = state.get_mathcode(c);
    debug_log!(debug=>"\\mathcode '{}' == {}",c.char_str(),v);
    Ok(v)
}

pub static MEANING : &str = "meaning";
pub fn meaning<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,f:TokenCont<ET>)
    -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"meaning");
    let esc = state.get_escapechar();
    match catch_prim!(gullet.mouth().get_next(state) => (MEANING,cmd)) {
        None => file_end_prim!(MEANING,cmd),
        Some((_,false)) => {
            if let Some(c) = esc { f(state,Token::new(BaseToken::Char(c,CategoryCode::Other),None))? }
            string_to_tokens::<ET>(RELAX.as_bytes(),state,f)
        }
        Some((t,_)) => {
            let n:ResolvedToken<ET> = resolve_token::<ET>(state,t);
            let string = match n.command {
                BaseCommand::Char{char,catcode:CategoryCode::BeginGroup} => format!("begin-group character {}",char.char_str()),
                BaseCommand::Char{char,catcode:CategoryCode::EndGroup} => format!("end-group character {}",char.char_str()),
                BaseCommand::Char{char,catcode:CategoryCode::MathShift} => format!("math shift character {}",char.char_str()),
                BaseCommand::Char{char,catcode:CategoryCode::AlignmentTab} => format!("alignment tab character {}",char.char_str()),
                BaseCommand::Char{char,catcode:CategoryCode::Parameter} => format!("macro parameter character {}",char.char_str()),
                BaseCommand::Char{char,catcode:CategoryCode::Superscript} => format!("superscript character {}",char.char_str()),
                BaseCommand::Char{char,catcode:CategoryCode::Subscript} => format!("subscript character {}",char.char_str()),
                BaseCommand::Char{char,catcode:CategoryCode::Space} => format!("blank space {}",char.char_str()),
                BaseCommand::Char{char,catcode:CategoryCode::Letter} => format!("the letter {}",char.char_str()),
                BaseCommand::Char{char,..} => format!("the character {}",char.char_str()),
                BaseCommand::Expandable {name,..} => {
                    if let Some(c) = esc { f(state,Token::new(BaseToken::Char(c,CategoryCode::Other),None))? }
                    return string_to_tokens::<ET>(name.as_bytes(),state,f)
                }
                BaseCommand::Unexpandable {name,..} => {
                    if let Some(c) = esc { f(state,Token::new(BaseToken::Char(c,CategoryCode::Other),None))? }
                    return string_to_tokens::<ET>(name.as_bytes(),state,f)
                }
                BaseCommand::OpenBox {name,..} => {
                    if let Some(c) = esc { f(state,Token::new(BaseToken::Char(c,CategoryCode::Other),None))? }
                    return string_to_tokens::<ET>(name.as_bytes(),state,f)
                }
                BaseCommand::Whatsit {name,..} => {
                    if let Some(c) = esc { f(state,Token::new(BaseToken::Char(c,CategoryCode::Other),None))? }
                    return string_to_tokens::<ET>(name.as_bytes(),state,f)
                }
                BaseCommand::Assignment {name,..} => {
                    if let Some(c) = esc { f(state,Token::new(BaseToken::Char(c,CategoryCode::Other),None))? }
                    return string_to_tokens::<ET>(name.as_bytes(),state,f)
                }
                BaseCommand::Relax => {
                    if let Some(c) = esc { f(state,Token::new(BaseToken::Char(c,CategoryCode::Other),None))? }
                    return string_to_tokens::<ET>(RELAX.as_bytes(),state,f)
                }
                BaseCommand::Conditional {name,..} => {
                    if let Some(c) = esc { f(state,Token::new(BaseToken::Char(c,CategoryCode::Other),None))? }
                    return string_to_tokens::<ET>(name.as_bytes(),state,f)
                }
                BaseCommand::FontCommand {name,..} => {
                    if let Some(c) = esc { f(state,Token::new(BaseToken::Char(c,CategoryCode::Other),None))? }
                    return string_to_tokens::<ET>(name.as_bytes(),state,f)
                }
                BaseCommand::Int(ValueCommand::Value {name,..}) => {
                    if let Some(c) = esc { f(state,Token::new(BaseToken::Char(c,CategoryCode::Other),None))? }
                    return string_to_tokens::<ET>(name.as_bytes(),state,f)
                }
                BaseCommand::Int(ValueCommand::Complex {name,..}) => {
                    if let Some(c) = esc { f(state,Token::new(BaseToken::Char(c,CategoryCode::Other),None))? }
                    return string_to_tokens::<ET>(name.as_bytes(),state,f)
                }
                BaseCommand::Int(ValueCommand::Primitive(name)) => {
                    if let Some(c) = esc { f(state,Token::new(BaseToken::Char(c,CategoryCode::Other),None))? }
                    return string_to_tokens::<ET>(name.as_bytes(),state,f)
                }
                BaseCommand::Dim(ValueCommand::Value {name,..}) => {
                    if let Some(c) = esc { f(state,Token::new(BaseToken::Char(c,CategoryCode::Other),None))? }
                    return string_to_tokens::<ET>(name.as_bytes(),state,f)
                }
                BaseCommand::Dim(ValueCommand::Complex {name,..}) => {
                    if let Some(c) = esc { f(state,Token::new(BaseToken::Char(c,CategoryCode::Other),None))? }
                    return string_to_tokens::<ET>(name.as_bytes(),state,f)
                }
                BaseCommand::Dim(ValueCommand::Primitive(name)) => {
                    if let Some(c) = esc { f(state,Token::new(BaseToken::Char(c,CategoryCode::Other),None))? }
                    return string_to_tokens::<ET>(name.as_bytes(),state,f)
                }
                BaseCommand::Skip(ValueCommand::Value {name,..}) => {
                    if let Some(c) = esc { f(state,Token::new(BaseToken::Char(c,CategoryCode::Other),None))? }
                    return string_to_tokens::<ET>(name.as_bytes(),state,f)
                }
                BaseCommand::Skip(ValueCommand::Complex {name,..}) => {
                    if let Some(c) = esc { f(state,Token::new(BaseToken::Char(c,CategoryCode::Other),None))? }
                    return string_to_tokens::<ET>(name.as_bytes(),state,f)
                }
                BaseCommand::Skip(ValueCommand::Primitive(name)) => {
                    if let Some(c) = esc { f(state,Token::new(BaseToken::Char(c,CategoryCode::Other),None))? }
                    return string_to_tokens::<ET>(name.as_bytes(),state,f)
                }
                BaseCommand::MuSkip(ValueCommand::Value {name,..}) => {
                    if let Some(c) = esc { f(state,Token::new(BaseToken::Char(c,CategoryCode::Other),None))? }
                    return string_to_tokens::<ET>(name.as_bytes(),state,f)
                }
                BaseCommand::MuSkip(ValueCommand::Complex {name,..}) => {
                    if let Some(c) = esc { f(state,Token::new(BaseToken::Char(c,CategoryCode::Other),None))? }
                    return string_to_tokens::<ET>(name.as_bytes(),state,f)
                }
                BaseCommand::MuSkip(ValueCommand::Primitive(name)) => {
                    if let Some(c) = esc { f(state,Token::new(BaseToken::Char(c,CategoryCode::Other),None))? }
                    return string_to_tokens::<ET>(name.as_bytes(),state,f)
                }
                BaseCommand::Toks(ValueCommand::Value {name,..}) => {
                    if let Some(c) = esc { f(state,Token::new(BaseToken::Char(c,CategoryCode::Other),None))? }
                    return string_to_tokens::<ET>(name.as_bytes(),state,f)
                }
                BaseCommand::Toks(ValueCommand::Complex {name,..}) => {
                    if let Some(c) = esc { f(state,Token::new(BaseToken::Char(c,CategoryCode::Other),None))? }
                    return string_to_tokens::<ET>(name.as_bytes(),state,f)
                }
                BaseCommand::Toks(ValueCommand::Primitive(name)) => {
                    if let Some(c) = esc { f(state,Token::new(BaseToken::Char(c,CategoryCode::Other),None))? }
                    return string_to_tokens::<ET>(name.as_bytes(),state,f)
                }
                BaseCommand::Int(ValueCommand::Register(u)) => {
                    if let Some(c) = esc { f(state,Token::new(BaseToken::Char(c,CategoryCode::Other),None))? }
                    format!("count{}",u)
                }
                BaseCommand::Dim(ValueCommand::Register(u)) => {
                    if let Some(c) = esc { f(state,Token::new(BaseToken::Char(c,CategoryCode::Other),None))? }
                    format!("dimen{}",u)
                }
                BaseCommand::Skip(ValueCommand::Register(u)) => {
                    if let Some(c) = esc { f(state,Token::new(BaseToken::Char(c,CategoryCode::Other),None))? }
                    format!("skip{}",u)
                }
                BaseCommand::MuSkip(ValueCommand::Register(u)) => {
                    if let Some(c) = esc { f(state,Token::new(BaseToken::Char(c,CategoryCode::Other),None))? }
                    format!("muskip{}",u)
                }
                BaseCommand::Toks(ValueCommand::Register(u)) => {
                    if let Some(c) = esc { f(state,Token::new(BaseToken::Char(c,CategoryCode::Other),None))? }
                    format!("toks{}",u)
                }
                BaseCommand::CharDef(c) => {
                    if let Some(c) = esc { f(state,Token::new(BaseToken::Char(c,CategoryCode::Other),None))? }
                    format!("char\"{:X}",c.to_usize())
                }
                BaseCommand::MathChar(c) => {
                    if let Some(c) = esc { f(state,Token::new(BaseToken::Char(c,CategoryCode::Other),None))? }
                    format!("mathchar\"{:X}",c)
                }
                BaseCommand::Font(fnt) => {
                    format!("select font {}",fnt)
                }
                BaseCommand::None => {
                    return string_to_tokens::<ET>("undefined".as_bytes(),state,f)
                }
                BaseCommand::Def(d) => {
                    let cc = state.get_catcode_scheme().clone();
                    if d.protected {
                        if let Some(c) = esc { f(state,Token::new(BaseToken::Char(c,CategoryCode::Other),None))? }
                        string_to_tokens::<ET>("protected ".as_bytes(),state,f)?
                    }
                    if d.long {
                        if let Some(c) = esc { f(state,Token::new(BaseToken::Char(c,CategoryCode::Other),None))? }
                        string_to_tokens::<ET>("long ".as_bytes(),state,f)?
                    }
                    if d.outer {
                        if let Some(c) = esc { f(state,Token::new(BaseToken::Char(c,CategoryCode::Other),None))? }
                        string_to_tokens::<ET>("outer ".as_bytes(),state,f)?
                    }
                    string_to_tokens::<ET>("macro:".as_bytes(),state,f)?;
                    let mut i = 0;
                    for s in &d.signature {
                        match s {
                            ParamToken::Token(t) => {
                                token_to_chars::<ET,_>(t,esc.clone(),&cc,true,|t| f(state,t))?;
                            }
                                //ret.extend(tokens_to_string(vec!(t.clone()), state.get_escapechar(), state.get_catcode_scheme()).as_bytes()),
                            ParamToken::Param => {
                                i += 1;
                                string_to_tokens::<ET>(format!("#{}", i).as_bytes(),state,f);
                            }
                        }
                    }
                    if d.endswithbrace { f(state,Token::new(BaseToken::Char(ET::Char::from(b'#'),CategoryCode::Other),None))? }
                    f(state,Token::new(BaseToken::Char(ET::Char::from(b'-'),CategoryCode::Other),None))?;
                    f(state,Token::new(BaseToken::Char(ET::Char::from(b'>'),CategoryCode::Other),None))?;
                    for t in &d.replacement {
                        match t {
                            ExpToken::Token(t) => token_to_chars::<ET,_>(t,esc.clone(),&cc,true,&mut |t| f(state,t))?,
                            ExpToken::ParamToken(t) => {
                                token_to_chars::<ET,_>(t,esc.clone(),&cc,true,|t| f(state,t))?;
                                token_to_chars::<ET,_>(t,esc.clone(),&cc,true,|t| f(state,t))?;
                            }
                            ExpToken::Param(t, i) => {
                                token_to_chars::<ET,_>(t,esc.clone(),&cc,true,|t| f(state,t))?;
                                string_to_tokens::<ET>(i.to_string().as_bytes(),state,f)?
                            }
                        }
                    }
                    return Ok(())
                }
            };
            string_to_tokens::<ET>(string.as_bytes(),state,f)
        }
    }
}

pub fn message<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>)
    -> Result<(),TeXError<ET>> {
    debug_log!(debug=>"message");
    catch_prim!(gullet.mouth().skip_whitespace(state) => ("message",cmd));
    let msg = catch_prim!(gullet.get_braced_string(state) => ("message",cmd));
    (state.outputs().message)(&msg);
    Ok(())
}

pub fn month<ET:EngineType>(state:&mut ET::State,cmd:CommandSource<ET>) -> Result<ET::Int,TeXError<ET>> {
    Ok(catch_prim!(ET::Int::from_i64(
        state.get_start_time().month() as i64
    ) => ("month",cmd)))
}

pub static MULTIPLY:&str = "multiply";
pub fn multiply<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,global:bool)
                                                   -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"\\multiply");
    catch_prim!(gullet.mouth().skip_whitespace(state) => (MULTIPLY,cmd));
    match catch_prim!(gullet.get_next_unexpandable(state) => (MULTIPLY,cmd)) {
        None => file_end_prim!(MULTIPLY,cmd),
        Some(ncmd) => match ncmd.command {
            BaseCommand::Int(a) => {
                macro_rules! finish {
                    ($old:expr,$nv:ident => $set:expr) => {{
                        catch_prim!(gullet.get_keyword(state,"by") => (MULTIPLY,cmd));
                        let i = catch_prim!(gullet.get_int(state) => (MULTIPLY,cmd));
                        let $nv = $old * i;
                        $set;
                        Ok(())
                    }}
                }
                match a {
                    ValueCommand::Register(u) => finish!(state.get_int_register(u),nv => state.set_int_register(u,nv,global)),
                    ValueCommand::Primitive(name) => finish!(state.get_primitive_int(name),nv => state.set_primitive_int(name,nv,global)),
                    ValueCommand::Complex {name,..} if name == COUNT => {
                        let int = catch_prim!(gullet.get_int(state) => (MULTIPLY,cmd));
                        let u = match int.try_into() {
                            Ok(u) => u,
                            _ => throw!("Not a valid register: {}",int => cmd.cause)
                        };
                        finish!(state.get_int_register(u),nv => state.set_int_register(u,nv,global))
                    }
                    _ => throw!("Unexpected token after \\multiply" => cmd.cause)
                }
            }
            BaseCommand::Dim(a) => {
                macro_rules! finish {
                    ($old:expr,$nv:ident => $set:expr) => {{
                        catch_prim!(gullet.get_keyword(state,"by") => (MULTIPLY,cmd));
                        let i = catch_prim!(gullet.get_int(state) => (MULTIPLY,cmd)).to_i64();
                        let $nv = $old.tex_mult(i as f64);
                        $set;
                        Ok(())
                    }}
                }
                match a {
                    ValueCommand::Register(u) => finish!(state.get_dim_register(u),nv => state.set_dim_register(u,nv,global)),
                    ValueCommand::Primitive(name) => finish!(state.get_primitive_dim(name),nv => state.set_primitive_dim(name,nv,global)),
                    ValueCommand::Complex {name,..} if name == DIMEN => {
                        let int = catch_prim!(gullet.get_int(state) => (MULTIPLY,cmd));
                        let u = match int.try_into() {
                            Ok(u) => u,
                            _ => throw!("Not a valid register: {}",int => cmd.cause)
                        };
                        finish!(state.get_dim_register(u),nv => state.set_dim_register(u,nv,global))
                    }
                    _ => throw!("Unexpected token after \\multiply" => cmd.cause)
                }
            }
            BaseCommand::Skip(a) => {
                macro_rules! finish {
                    ($old:expr,$nv:ident => $set:expr) => {{
                        catch_prim!(gullet.get_keyword(state,"by") => (MULTIPLY,cmd));
                        let i = catch_prim!(gullet.get_int(state) => (MULTIPLY,cmd)).to_i64();
                        let $nv = $old.tex_mult(i as f64);
                        $set;
                        Ok(())
                    }}
                }
                match a {
                    ValueCommand::Register(u) => finish!(state.get_skip_register(u),nv => state.set_skip_register(u,nv,global)),
                    ValueCommand::Primitive(name) => finish!(state.get_primitive_skip(name),nv => state.set_primitive_skip(name,nv,global)),
                    ValueCommand::Complex {name,..} if name == SKIP => {
                        let int = catch_prim!(gullet.get_int(state) => (MULTIPLY,cmd));
                        let u = match int.try_into() {
                            Ok(u) => u,
                            _ => throw!("Not a valid register: {}",int => cmd.cause)
                        };
                        finish!(state.get_skip_register(u),nv => state.set_skip_register(u,nv,global))
                    }
                    _ => throw!("Unexpected token after \\multiply" => cmd.cause)
                }
            }
            BaseCommand::MuSkip(a) => {
                macro_rules! finish {
                    ($old:expr,$nv:ident => $set:expr) => {{
                        catch_prim!(gullet.get_keyword(state,"by") => (MULTIPLY,cmd));
                        let i = catch_prim!(gullet.get_int(state) => (MULTIPLY,cmd)).to_i64();
                        let $nv = $old.tex_mult(i as f64);
                        $set;
                        Ok(())
                    }}
                }
                match a {
                    ValueCommand::Register(u) => finish!(state.get_muskip_register(u),nv => state.set_muskip_register(u,nv,global)),
                    ValueCommand::Primitive(name) => finish!(state.get_primitive_muskip(name),nv => state.set_primitive_muskip(name,nv,global)),
                    ValueCommand::Complex {name,..} if name == MUSKIP => {
                        let int = catch_prim!(gullet.get_int(state) => (MULTIPLY,cmd));
                        let u = match int.try_into() {
                            Ok(u) => u,
                            _ => throw!("Not a valid register: {}",int => cmd.cause)
                        };
                        finish!(state.get_muskip_register(u),nv => state.set_muskip_register(u,nv,global))
                    }
                    _ => throw!("Unexpected token after \\multiply" => cmd.cause)
                }
            }
            o => throw!("expected register after \\multiply;got:{:?}",o => cmd.cause)
        }
    }
}

pub static MUSKIP : &str = "muskip";
pub fn muskip_assign<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,global:bool)
    -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"Assigning \\muskip");
    let i = catch_prim!(gullet.get_int(state) => (MUSKIP,cmd));
    let i:usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => throw!("Not a valid register: {}",i => cmd.cause)
    };
    catch_prim!(gullet.mouth().skip_eq_char(state) => (MUSKIP,cmd));
    let v = catch_prim!(gullet.get_muskip(state) => (MUSKIP,cmd));
    debug_log!(debug=>"\\muskip{} = {}",i,v);
    state.set_muskip_register(i,v,global);
    Ok(())
}

pub fn muskip_get<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>) ->
                                                                                                           Result<MuSkip<ET::MuDim,ET::MuStretchShrinkDim>,TeXError<ET>> {
    debug_log!(trace=>"Getting \\muskip");
    let i = catch_prim!(gullet.get_int(state) => (MUSKIP,cmd));
    let i:usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => throw!("Not a valid register: {}",i => cmd.cause)
    };
    let v = state.get_muskip_register(i);
    debug_log!(debug=>"\\muskip{} == {}",i,v);
    Ok(v)
}

pub static MUSKIPDEF : &str = "muskipdef";
pub fn muskipdef<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,global:bool)
                                                     -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"muskipdef");
    let name = catch_prim!(gullet.get_control_sequence(state) => (MUSKIPDEF,cmd));
    catch_prim!(set_relax::<ET>(state,&name,&cmd,global) => (MUSKIPDEF,cmd));
    catch_prim!(gullet.mouth().skip_eq_char(state) => (MUSKIPDEF,cmd));
    let num = catch_prim!(gullet.get_int(state) => (MUSKIPDEF,cmd));
    if num.to_i64() < 0 {
        throw!("Invalid muskip register index: {}",num => cmd.cause)
    }
    let num = num.to_i64() as usize;
    let ret = Command::new(BaseCommand::MuSkip(ValueCommand::Register(num)),Some(&cmd));
    state.set_command_for_tk(name,Some(ret),global);
    Ok(())
}


pub fn newlinechar_assign<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,global:bool)
    -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"Assigning \\newlinechar");
    catch_prim!(gullet.mouth().skip_eq_char(state) => ("newlinechar",cmd));
    let i = catch_prim!(gullet.get_int(state) => ("newlinechar",cmd));
    let c = match i.to_i64() {
        -1|255 => None,
        j => match ET::Char::from_i64(j) {
            Some(c) => Some(c),
            None => throw!("Not a valid character: {}",j => cmd.cause)
        }
    };
    debug_log!(debug=>"\\newlinechar = {:?}",c.map(|c| c.char_str()));
    state.set_newlinechar(c,global);
    Ok(())
}
pub fn newlinechar_get<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>)
    -> Result<ET::Int,TeXError<ET>> {
    debug_log!(trace=>"Getting \\newlinechar");
    let c = match state.get_newlinechar() {
        None => -1,
        Some(c) => c.to_usize() as i64
    };
    debug_log!(debug=>"\\newlinechar == {}",c);
    Ok(ET::Int::from_i64::<ET>(c).unwrap())
}

pub static NOEXPAND: &str = "noexpand";
/// invariant: adds token as nonexpanded to the gullet iff the original token was expandable
/// in the first place
pub fn noexpand<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,f:TokenCont<ET>)
    -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"\\noexpand");
    match catch_prim!(gullet.mouth().get_next(state) => ("noexpand",cmd)) {
        None => file_end_prim!("noexpand",cmd),
        Some((t,_)) => {
            let res = resolve_token::<ET>(state,t);
            match res.command {
                BaseCommand::Def(_) => gullet.mouth().push_noexpand(res.source.cause),
                BaseCommand::Expandable {..} => gullet.mouth().push_noexpand(res.source.cause),
                BaseCommand::Conditional {..} => gullet.mouth().push_noexpand(res.source.cause),
                _ => f(state,res.source.cause)?
            }
        }
    }
    Ok(())
}

pub fn number<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,f:TokenCont<ET>)
    -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"\\number");
    let num = catch_prim!(gullet.get_int(state) => ("number",cmd));
    string_to_tokens::<ET>(num.to_i64().to_string().as_bytes(),state,f)
}

pub fn openin<ET:EngineType>(state: &mut ET::State, gullet:&mut ET::Gullet, cmd:CommandSource<ET>)
    -> Result<(), TeXError<ET>> {
    debug_log!(trace=>"\\openin");
    let i = catch_prim!(gullet.get_int(state) => ("openin",cmd));
    let i : usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => throw!("Invalid file number: {}",i => cmd.cause)
    };
    catch_prim!(gullet.mouth().skip_eq_char(state) => ("openin",cmd));
    let filename = catch_prim!(gullet.get_string(state) => ("openin",cmd)).to_string();
    let f = state.filesystem().get(&filename);
    state.file_openin(i,f); // TODO error?
    Ok(())
}

pub fn openout<ET:EngineType>(state: &mut ET::State, gullet:&mut ET::Gullet, cmd:CommandSource<ET>)
    -> Result<Whatsit<ET>, TeXError<ET>> {
    debug_log!(trace=>"\\openout");
    let i = catch_prim!(gullet.get_int(state) => ("openout",cmd));
    let i : usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => throw!("Invalid file number: {}",i => cmd.cause)
    };
    catch_prim!(gullet.mouth().skip_eq_char(state) => ("openout",cmd));
    let filename = catch_prim!(gullet.get_string(state) => ("openout",cmd)).to_string();
    let apply = Box::new(move |state:&mut ET::State,_gullet:&mut ET::Gullet| {
        let f = state.filesystem().get(&filename);
        state.file_openout(i,f); // TODO error?
        Ok(())
    });
    Ok(Whatsit::new(apply))
}

pub fn or<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>)
    -> Result<(),TeXError<ET>> {
    match gullet.current_conditional() {
        (Some(ConditionalBranch::Case(_,_)),i) =>
            catch_prim!(crate::engine::gullet::methods::else_loop::<ET>(gullet,state,"ifcase",i,true) => ("or",cmd)),
        _ => throw!("Not in an \\ifcase" => cmd.cause)
    }
    Ok(())
}

pub fn outer<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,global_:bool,protected_:bool,long_:bool,outer_:bool)
                                                      -> Result<(),TeXError<ET>> {
    debug_log!(trace => "\\outer");
    match catch_prim!(gullet.get_next_stomach_command(state) => ("outer",cmd)) {
        None => file_end_prim!("outer",cmd),
        Some(c) => match c.command {
            BaseStomachCommand::Assignment {name:Some("global"),..} => global::<ET>(state,gullet,cmd,global_,protected_,long_,true),
            BaseStomachCommand::Assignment {name:Some("protected"),..} => super::etex::protected::<ET>(state,gullet,cmd,global_,protected_,long_,true),
            BaseStomachCommand::Assignment {name:Some("long"),..} => long::<ET>(state,gullet,cmd,global_,protected_,long_,true),
            BaseStomachCommand::Assignment {name:Some("outer"),..} => outer::<ET>(state,gullet,cmd,global_,protected_,long_,true),
            BaseStomachCommand::Assignment {name:Some("def"),..} => def::<ET>(state,gullet,cmd,global_,protected_,long_,true),
            BaseStomachCommand::Assignment {name:Some("edef"),..} => edef::<ET>(state,gullet,cmd,global_,protected_,long_,true),
            BaseStomachCommand::Assignment {name:Some("gdef"),..} => gdef::<ET>(state,gullet,cmd,global_,protected_,long_,true),
            BaseStomachCommand::Assignment {name:Some("xdef"),..} => xdef::<ET>(state,gullet,cmd,global_,protected_,long_,true),
            _ => throw!("Expected a macro definition after \\outer" => cmd.cause)
        }
    }
}

pub fn par<ET:EngineType>(state:&mut ET::State,_cmd:CommandSource<ET>) -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"par");
    if state.mode().is_vertical() {Ok(())} else {
        todo!("par in horizontal mode")
    }
}

pub fn patterns<ET:EngineType>(state: &mut ET::State, gullet:&mut ET::Gullet, cmd:CommandSource<ET>)
                             -> Result<(), TeXError<ET>> {
    debug_log!(trace=>"\\patterns");
    // TODO
    catch_prim!(gullet.mouth().read_argument(state,&mut |_,_| Ok(())) => ("patterns",cmd));
    Ok(())
}

pub fn read<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,globally:bool)
    -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"read");
    let i = catch_prim!(gullet.get_int(state) => ("read",cmd));
    let i : usize = match i.try_into() {
        Ok(i) => i,
        Err(_) => throw!("Invalid file number" => cmd.cause)
    };
    let file = match state.get_open_in_file(i) {
        None => throw!("File {} not open for reading",i),
        Some(f) => f
    };
    if !catch_prim!(gullet.get_keyword(state,"to") => ("read",cmd)) {
        return throw!("Expected 'to' after \\read" => cmd.cause)
    }
    let newcmd = catch_prim!(gullet.get_control_sequence(state) => ("read",cmd));
    let mut ret = vec!();
    catch_prim!(file.read::<ET,_>(state.get_catcode_scheme(),state.get_endlinechar(),|t| ret.push(t)) => ("read",cmd));
    debug_log!(trace=>"read: {} = {}",newcmd,TokenList(&ret));
    if ret.is_empty() {
        match state.get_endlinechar() {
            None => (),
            Some(c) => ret.push(Token::new(BaseToken::Char(c,*state.get_catcode_scheme().get(&c)),None))
        }
    }
    /*
        if TokenList(ret.clone()).to_string().starts_with("102A0;CARIAN LETTER A") {
            println!("Here!");
            std::env::set_var("RUST_LOG","debug,tex_engine::tex::commands=trace,tex_engine::engine::gullet=trace");
            env_logger::init();
        }
    */
    let def = Command::new(BaseCommand::Def(DefI::simple(ret)),Some(&cmd));
    state.set_command_for_tk(newcmd,Some(def),globally);
    Ok(())
}

pub static RELAX: &str = "relax";

pub fn romannumeral<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,f:TokenCont<ET>)
    -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"romannumeral");
    let mut num = catch_prim!(gullet.get_int(state) => ("romannumeral",cmd)).to_i64();
    if num <= 0 {
        return Ok(())
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
    string_to_tokens::<ET>(&ret,state,f)
}

pub static SETBOX: &str = "setbox";
pub fn setbox<ET:EngineType>(state:&mut ET::State, gullet:&mut ET::Gullet, cmd:CommandSource<ET>, global:bool)
    -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"\\setbox");
    let i = catch_prim!(gullet.get_int(state) => (SETBOX,cmd)).to_i64();
    if i < 0  { throw!("Invalid box number: {}",i => cmd.cause) }
    catch_prim!(gullet.mouth().skip_eq_char(state) => (SETBOX,cmd));
    match catch_prim!(gullet.get_next_unexpandable(state) => (SETBOX,cmd)) {
        None => file_end_prim!(SETBOX,cmd),
        Some(c) => match c.command {
            BaseCommand::OpenBox {name,mode,apply} => {
                let f = catch_prim!(apply(state,gullet,c.source) => (SETBOX,cmd));
                state.open_box(OpenBox::Box {list:vec!(),mode,on_close:Ptr::new(move |s,gu,v| {
                    let bx = match f(s,gu,v) {
                        Some(r) => {r}
                        None => {todo!("make void box")}
                    };
                    s.set_box_register(i as usize,bx,global);
                    None
                })});
                Ok(())
            }
            _ => throw!("Box expected: {}",c.source.cause)
        }
    }
}

pub fn sfcode_assign<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,global:bool)
    -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"Assigning space factor code");
    let i = catch_prim!(gullet.get_int(state) => ("sfcode",cmd));
    let c: ET::Char = match ET::Char::from_i64(i.to_i64()) {
        Some(i) => i,
        None => throw!("Not a valid character: {}",i => cmd.cause)
    };
    catch_prim!(gullet.mouth().skip_eq_char(state) => ("catcode",cmd));
    let v = catch_prim!(gullet.get_int(state) => ("catcode",cmd));
    debug_log!(debug=>"\\sfcode '{}' = {}",c.char_str(),v);
    state.set_sfcode(c,v,global);
    Ok(())
}
pub fn sfcode_get<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>)
    -> Result<ET::Int,TeXError<ET>> {
    debug_log!(trace=>"Getting space factor code");
    let i = catch_prim!(gullet.get_int(state) => ("sfcode",cmd));
    let c: ET::Char = match ET::Char::from_i64(i.to_i64()) {
        Some(i) => i,
        None => throw!("Not a valid character: {}",i => cmd.cause)
    };
    let v = state.get_sfcode(&c);
    debug_log!(debug=>"\\sfcode '{}' == {}",c.char_str(),v);
    Ok(v)
}

pub fn skewchar_assign<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,global:bool)
                                        -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"Assigning \\kewchar");
    let mut font = catch_prim!(gullet.get_font(state) => ("skewchar",cmd));
    catch_prim!(gullet.mouth().skip_eq_char(state) => ("skewchar",cmd));
    let i = catch_prim!(gullet.get_int(state) => ("skewchar",cmd)).to_i64();
    debug_log!(debug=>"\\skewchar\\{:?} = {:?}",font,i);
    font.set_skewchar(i);
    Ok(())
}
pub fn skewchar_get<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>) -> Result<ET::Int,TeXError<ET>> {
    debug_log!(trace=>"Getting \\skewchar");
    let font = catch_prim!(gullet.get_font(state) => ("skewchar",cmd));
    debug_log!(debug=>"\\skewchar == {:?}",font.get_skewchar());
    Ok(catch_prim!(ET::Int::from_i64::<ET>(font.get_skewchar()) => ("hyphenchar",cmd)))
}

pub static SKIP : &str = "skip";
pub fn skip_assign<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,global:bool)
    -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"Assigning \\skip");
    let i = catch_prim!(gullet.get_int(state) => (SKIP,cmd));
    let i:usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => throw!("Not a valid register: {}",i => cmd.cause)
    };
    catch_prim!(gullet.mouth().skip_eq_char(state) => (SKIP,cmd));
    let v = catch_prim!(gullet.get_skip(state) => (SKIP,cmd));
    debug_log!(debug=>"\\skip{} = {}",i,v);
    state.set_skip_register(i,v,global);
    Ok(())
}

pub fn skip_get<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>)
    -> Result<Skip<ET::SkipDim>,TeXError<ET>> {
    debug_log!(trace=>"Getting \\skip");
    let i = catch_prim!(gullet.get_int(state) => (SKIP,cmd));
    let i:usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => throw!("Not a valid register: {}",i => cmd.cause)
    };
    let v = state.get_skip_register(i);
    debug_log!(debug=>"\\skip{} == {}",i,v);
    Ok(v)
}

pub static SKIPDEF : &str = "skipdef";
pub fn skipdef<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,global:bool)
                                                     -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"skipdef");
    let name = catch_prim!(gullet.get_control_sequence(state) => (SKIPDEF,cmd));
    catch_prim!(set_relax::<ET>(state,&name,&cmd,global) => (SKIPDEF,cmd));
    catch_prim!(gullet.mouth().skip_eq_char(state) => (SKIPDEF,cmd));
    let num = catch_prim!(gullet.get_int(state) => (SKIPDEF,cmd));
    if num.to_i64() < 0 {
        throw!("Invalid skip register index: {}",num => cmd.cause)
    }
    let num = num.to_i64() as usize;
    let ret = Command::new(BaseCommand::Skip(ValueCommand::Register(num)),Some(&cmd));
    state.set_command_for_tk(name,Some(ret),global);
    Ok(())
}

pub fn string<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,f:TokenCont<ET>)
    -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"string");
    match catch_prim!(gullet.mouth().get_next(state) => ("string",cmd)) {
        None => file_end_prim!("string",cmd),
        Some((t,_)) => token_to_chars(&t,state.get_escapechar(),state.get_catcode_scheme(),false,&mut |t| f(state,t))?
    }
    Ok(())
}

pub static THE : &str = "the";
pub fn the<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,f:TokenCont<ET>)
    -> Result<(),TeXError<ET>> {
    debug_log!(trace => "\\the");
    match catch_prim!(gullet.get_next_unexpandable(state) => (THE,cmd)) {
        Some(c) => match c.command {
            BaseCommand::Int(ass) => {
                let val = ass.get(state,gullet,c.source)?;
                string_to_tokens::<ET>(format!("{}",val).as_bytes(),state,f)
            }
            BaseCommand::Dim(ass) => {
                let val = ass.get(state,gullet,c.source)?;
                string_to_tokens::<ET>(format!("{}",val).as_bytes(),state,f)
            }
            BaseCommand::Skip(ass) => {
                let val = ass.get(state,gullet,c.source)?;
                string_to_tokens::<ET>(format!("{}",val).as_bytes(),state,f)
            }
            BaseCommand::MuSkip(ass) => {
                let val = ass.get(state,gullet,c.source)?;
                string_to_tokens::<ET>(format!("{}",val).as_bytes(),state,f)
            }
            BaseCommand::Toks(ass) => {
                for t in ass.get(state, gullet, c.source)? { f(state, t)? }
                Ok(())
            }
            BaseCommand::CharDef(c) => string_to_tokens::<ET>(c.to_usize().to_string().as_bytes(),state,f),
            _ => throw!("Expected a value after \\the; got: {}", c.source.cause => c.source.cause)
        }
        None => file_end_prim!(THE,cmd)
    }
}

pub fn time<ET:EngineType>(state:&mut ET::State,cmd:CommandSource<ET>) -> Result<ET::Int,TeXError<ET>> {
    let t = state.get_start_time();
    Ok(catch_prim!(ET::Int::from_i64( ((t.hour() * 60) + t.minute()) as i64 ) => ("time",cmd)))
}

pub static TOKS:&str = "toks";
pub fn toks_assign<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,global:bool)
    -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"Assigning \\toks");
    let i = catch_prim!(gullet.get_int(state) => (TOKS,cmd));
    let i:usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => throw!("Not a valid register: {}",i => cmd.cause)
    };
    catch_prim!(gullet.mouth().skip_eq_char(state) => (TOKS,cmd));
    match catch_prim!(gullet.mouth().get_next(state) => (TOKS,cmd)) {
        None => file_end_prim!(TOKS,cmd),
        Some((t,_)) if t.catcode() == CategoryCode::BeginGroup => (),
        Some((o,_)) => throw!("Expected begin group token after \\toks, got: {}",o => cmd.cause)
    }
    let mut v = vec!();
    catch_prim!(gullet.mouth().read_until_endgroup(state,&mut |_,t| Ok(v.push(t))) => (TOKS,cmd));
    debug_log!(debug=>"\\toks{} = {}",i,TokenList(&v));
    state.set_toks_register(i,v,global);
    Ok(())
}
pub fn toks_get<'a,ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>)
                -> Result<Vec<Token<ET>>,TeXError<ET>> {
    let i = catch_prim!(gullet.get_int(state) => (TOKS,cmd));
    let i:usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => throw!("Not a valid register: {}",i => cmd.cause)
    };
    Ok(state.get_toks_register(i).clone())
}

pub static TOKSDEF : &str = "toksdef";
pub fn toksdef<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,global:bool)
                                                      -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"toksdef");
    let name = catch_prim!(gullet.get_control_sequence(state) => (TOKSDEF,cmd));
    catch_prim!(set_relax::<ET>(state,&name,&cmd,global) => (TOKSDEF,cmd));
    catch_prim!(gullet.mouth().skip_eq_char(state) => (TOKSDEF,cmd));
    let num = catch_prim!(gullet.get_int(state) => (TOKSDEF,cmd));
    if num.to_i64() < 0 {
        throw!("Invalid token register index: {}",num => cmd.cause)
    }
    let num = num.to_i64() as usize;
    let ret = Command::new(BaseCommand::Toks(ValueCommand::Register(num)),Some(&cmd));
    state.set_command_for_tk(name,Some(ret),global);
    Ok(())
}

pub fn uccode_assign<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,global:bool)
    -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"Assigning upper case character");
    let i = catch_prim!(gullet.get_int(state) => ("uccode",cmd));
    let c: ET::Char = match ET::Char::from_i64(i.to_i64()) {
        Some(i) => i,
        None => throw!("Not a valid character: {}",i => cmd.cause)
    };
    catch_prim!(gullet.mouth().skip_eq_char(state) => ("uccode",cmd));
    let i = catch_prim!(gullet.get_int(state) => ("uccode",cmd));
    let lc: ET::Char = match ET::Char::from_i64(i.to_i64()) {
        Some(i) => i,
        None => throw!("Not a valid character: {}",i => cmd.cause)
    };
    debug_log!(debug=>"\\uccode '{}' = {}",c.char_str(),lc.char_str());
    state.set_uccode(c,lc,global);
    Ok(())
}

pub fn uccode_get<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>)
    -> Result<ET::Int,TeXError<ET>> {
    debug_log!(trace=>"Getting upper case character");
    let i = catch_prim!(gullet.get_int(state) => ("uccode",cmd));
    let c: ET::Char = match ET::Char::from_i64(i.to_i64()) {
        Some(i) => i,
        None => throw!("Not a valid character: {}",i => cmd.cause)
    };
    let v = catch_prim!(ET::Int::from_i64(state.get_uccode(&c).to_usize() as i64) => ("uccode",cmd));
    debug_log!(debug=>"\\uccode '{}' == {}",c.char_str(),v);
    Ok(v)
}

pub static UPPERCASE: &str = "uppercase";
pub fn uppercase<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>)
                                         -> Result<(),TeXError<ET>> {
    debug_log!(trace => "\\uppercase");
    catch_prim!(expand_until_space::<ET>(gullet,state) => (UPPERCASE,cmd));
    let mut ret = gullet.mouth().new_tokensource();
    catch_prim!(gullet.get_group(state,&mut |s,next| match &next.base {
        BaseToken::Char(c,cc) => {
            let nc = s.get_uccode(c);
            Ok(ret.push(Token::new(BaseToken::Char(nc, *cc), None)))
        }
        _ => Ok(ret.push(next))

    }) => (UPPERCASE,cmd));
    gullet.mouth().push_tokens(ret);
    Ok(())
}


pub static VRULE: &str = "vrule";
pub fn vrule<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>)
                                -> Result<(),TeXError<ET>> {
    debug_log!(trace => "\\vrule");
    let mut width = None;
    let mut height = None;
    let mut depth = None;
    loop {
        match catch_prim!(gullet.get_keywords(state,vec!("width","height","depth")) => (VRULE,cmd)) {
            None => break,
            Some(s) => {
                let val = catch_prim!(gullet.get_dim(state) => (VRULE,cmd));
                match s {
                    "width" => width = Some(val),
                    "height" => height = Some(val),
                    "depth" => depth = Some(val),
                    _ => unsafe{unreachable_unchecked()}
                }
            }
        }
    }
    state.push_node(SimpleNode::Rule {
        width,height,depth,axis:HorV::Vertical
    }.as_node());
    Ok(())
}

pub fn write<ET:EngineType>(state: &mut ET::State, gullet:&mut ET::Gullet, cmd:CommandSource<ET>)
    -> Result<Whatsit<ET>, TeXError<ET>> {
    debug_log!(trace=>"\\write");
    let i = catch_prim!(gullet.get_int(state) => ("write",cmd));
    let i = i.to_i64();

    match catch_prim!(gullet.mouth().get_next(state) => ("write",cmd)) {
        None => file_end_prim!("write",cmd),
        Some((t,_)) if t.catcode() == CategoryCode::BeginGroup => (),
        Some((o,_)) => throw!("Expected begin group token after \\write, got: {}",o => cmd.cause)
    }
    let mut tks = vec!();
    catch_prim!(gullet.mouth().read_until_endgroup(state,&mut |_,t| Ok(tks.push(t))) => ("write",cmd));

    let apply = Box::new(move |state:&mut ET::State,gullet:&mut ET::Gullet| {
        tks.push(Token::new(BaseToken::Char(ET::Char::from(b'}'),CategoryCode::EndGroup),None));
        tks.insert(0,Token::new(BaseToken::Char(ET::Char::from(b'{'),CategoryCode::BeginGroup),None));
        gullet.with_mouth(tks,|gullet| {
            let string = catch_prim!(gullet.get_braced_string(state) => ("write",cmd));
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
            Ok(())
        })
    });
    Ok(Whatsit::new(apply))
}

pub fn xdef<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,global:bool,protected:bool,long:bool,outer:bool)
                                                 -> Result<(),TeXError<ET>> {
    edef::<ET>(state,gullet,cmd,true,protected,long,outer)
}

pub fn year<ET:EngineType>(state:&mut ET::State,cmd:CommandSource<ET>) -> Result<ET::Int,TeXError<ET>> {
    Ok(catch_prim!(ET::Int::from_i64(
        state.get_start_time().year() as i64
    ) => ("year",cmd)))
}

// --------------------------------------------------------------------------------------------------


pub fn initialize_tex_primitives<ET:EngineType>(state:&mut ET::State,stomach:&mut ET::Stomach,gullet:&mut ET::Gullet) {

    register_skip_assign!(abovedisplayshortskip,state,stomach,gullet);
    register_skip_assign!(abovedisplayskip,state,stomach,gullet);
    register_int_assign!(adjdemerits,state,stomach,gullet);
    register_assign!(advance,state,stomach,gullet,(s,gu,cmd,global) =>advance::<ET>(s,gu,cmd,global));
    register_unexpandable!(afterassignment,state,stomach,gullet,false,(s,gu,cmd) =>afterassignment::<ET>(s,gu,cmd));
    register_skip_assign!(baselineskip,state,stomach,gullet);
    register_unexpandable!(begingroup,state,stomach,gullet,false,(s,_,_) =>begingroup::<ET>(s));
    register_skip_assign!(belowdisplayskip,state,stomach,gullet);
    register_skip_assign!(belowdisplayshortskip,state,stomach,gullet);
    register_int_assign!(binoppenalty,state,stomach,gullet);
    register_dim_assign!(boxmaxdepth,state,stomach,gullet);
    register_int_assign!(brokenpenalty,state,stomach,gullet);
    register_value_assign_int!(catcode,state,stomach,gullet);
    register_assign!(chardef,state,stomach,gullet,(s,gu,cmd,global) =>chardef::<ET>(s,gu,cmd,global));
    register_unexpandable!(closein,state,stomach,gullet,false,(s,gu,cmd) =>closein::<ET>(s,gu,cmd));
    register_whatsit!(closeout,state,stomach,gullet,(s,gu,cmd) =>closeout::<ET>(s,gu,cmd));
    register_int_assign!(clubpenalty,state,stomach,gullet);
    register_value_assign_int!(count,state,stomach,gullet);
    register_assign!(countdef,state,stomach,gullet,(s,gu,cmd,global) =>countdef::<ET>(s,gu,cmd,global));
    register_expandable!(csname,state,stomach,gullet,(s,gu,cmd,f) =>csname::<ET>(s,gu,cmd,f));
    register_int!(day,state,stomach,gullet,(s,g,c) => day::<ET>(s,c));
    register_assign!(def,state,stomach,gullet,(s,gu,cmd,global) =>def::<ET>(s,gu,cmd,global,false,false,false));
    register_int_assign!(defaulthyphenchar,state,stomach,gullet);
    register_int_assign!(defaultskewchar,state,stomach,gullet);
    register_int_assign!(delimiterfactor,state,stomach,gullet);
    register_dim_assign!(delimitershortfall,state,stomach,gullet);
    register_value_assign_int!(delcode,state,stomach,gullet);
    register_value_assign_dim!(dimen,state,stomach,gullet);
    register_assign!(dimendef,state,stomach,gullet,(s,gu,cmd,global) =>dimendef::<ET>(s,gu,cmd,global));
    register_dim_assign!(displayindent,state,stomach,gullet);
    register_int_assign!(displaywidowpenalty,state,stomach,gullet);
    register_dim_assign!(displaywidth,state,stomach,gullet);
    register_assign!(divide,state,stomach,gullet,(s,gu,cmd,global) =>divide::<ET>(s,gu,cmd,global));
    register_int_assign!(doublehyphendemerits,state,stomach,gullet);
    register_unexpandable!(dump,state,stomach,gullet,false,(s,sto,cmd) =>dump::<ET>());
    register_assign!(edef,state,stomach,gullet,(s,gu,cmd,global) =>edef::<ET>(s,gu,cmd,global,false,false,false));
    register_expandable!(else,state,stomach,gullet,(s,gu,cmd,f) =>else_::<ET>(s,gu,cmd));
    register_dim_assign!(emergencystretch,state,stomach,gullet);
    register_unexpandable!(end,state,stomach,gullet,false,(_,_,_) =>end::<ET>());
    register_unexpandable!(endcsname,state,stomach,gullet,false,(_,_,cmd) =>endcsname::<ET>(cmd));
    register_expandable!(endinput,state,stomach,gullet,(s,g,c,f) => endinput::<ET>(s,g,c));
    register_unexpandable!(endgroup,state,stomach,gullet,false,(s,gu,cmd) =>endgroup::<ET>(s,gu,cmd));
    register_value_assign_int!(endlinechar,state,stomach,gullet);
    register_tok_assign!(errhelp,state,stomach,gullet);

    let em = Some(Command::new(BaseCommand::Unexpandable {
        name:ERRMESSAGE,
        apply:|s,gu,cmd| errmessage::<ET>(s,gu,cmd),
        starts_paragraph:false
    },None));
    state.set_command(ET::Char::from_str("errmessage"),em.clone(),true);
    state.set_command(ET::Char::from_str("LaTeX3 error:"),em,true);

    register_int_assign!(errorcontextlines,state,stomach,gullet);
    register_unexpandable!(errorstopmode,state,stomach,gullet,false,(s,gu,cmd) =>errorstopmode::<ET>());
    register_value_assign_int!(escapechar,state,stomach,gullet);
    register_int_assign!(exhyphenpenalty,state,stomach,gullet);
    register_expandable!(expandafter,state,stomach,gullet,(s,g,c,f) => expandafter::<ET>(s,g,c,f));
    register_tok_assign!(everypar,state,stomach,gullet);
    register_tok_assign!(everymath,state,stomach,gullet);
    register_tok_assign!(everydisplay,state,stomach,gullet);
    register_tok_assign!(everyhbox,state,stomach,gullet);
    register_tok_assign!(everyvbox,state,stomach,gullet);
    register_tok_assign!(everyjob,state,stomach,gullet);
    register_tok_assign!(everycr,state,stomach,gullet);
    register_int_assign!(fam,state,stomach,gullet);
    register_expandable!(fi,state,stomach,gullet,(s,gu,cmd,f) =>fi::<ET>(s,gu,cmd));
    register_int_assign!(finalhyphendemerits,state,stomach,gullet);
    register_int_assign!(floatingpenalty,state,stomach,gullet);
    register_value_assign_font!(font,state,stomach,gullet);
    register_value_assign_dim!(fontdimen,state,stomach,gullet);
    register_assign!(futurelet,state,stomach,gullet,(s,gu,cmd,global) =>futurelet::<ET>(s,gu,cmd,global));
    register_assign!(gdef,state,stomach,gullet,(s,gu,cmd,global) =>gdef::<ET>(s,gu,cmd,global,false,false,false));
    register_assign!(global,state,stomach,gullet,(s,gu,cmd,g) =>global::<ET>(s,gu,cmd,g,false,false,false));
    register_int_assign!(globaldefs,state,stomach,gullet);
    register_int_assign!(hangafter,state,stomach,gullet);
    register_dim_assign!(hangindent,state,stomach,gullet);
    register_int_assign!(hbadness,state,stomach,gullet);
    register_open_box!(hbox,state,stomach,gullet,BoxMode::H,(s,gu,cmd) =>hbox::<ET>(s,gu,cmd));
    register_dim_assign!(hfuzz,state,stomach,gullet);
    register_dim_assign!(hoffset,state,stomach,gullet);
    register_int_assign!(holdinginserts,state,stomach,gullet);
    register_unexpandable!(hrule,state,stomach,gullet,true,(s,gu,cmd) =>hrule::<ET>(s,gu,cmd));
    register_dim_assign!(hsize,state,stomach,gullet);
    register_unexpandable!(hyphenation,state,stomach,gullet,false,(s,gu,cmd) =>hyphenation::<ET>(s,gu,cmd));
    register_value_assign_int!(hyphenchar,state,stomach,gullet);
    register_int_assign!(hyphenpenalty,state,stomach,gullet);
    register_conditional!(if,state,stomach,gullet,(s,gu,cmd) =>if_::<ET>(s,gu,cmd));
    register_conditional!(ifcase,state,stomach,gullet,(s,gu,cmd) =>ifcase::<ET>());
    register_conditional!(ifcat,state,stomach,gullet,(s,gu,cmd) =>ifcat::<ET>(s,gu,cmd));
    register_conditional!(ifdim,state,stomach,gullet,(s,gu,cmd) =>ifdim::<ET>(s,gu,cmd));
    register_conditional!(ifeof,state,stomach,gullet,(s,gu,cmd) =>ifeof::<ET>(s,gu,cmd));
    register_conditional!(iffalse,state,stomach,gullet,(s,gu,cmd) => Ok(false));
    register_conditional!(ifhbox,state,stomach,gullet,(s,gu,cmd) =>todo!("ifhbox"));
    register_conditional!(ifhmode,state,stomach,gullet,(s,gu,cmd) =>ifhmode::<ET>(s,gu,cmd));
    register_conditional!(ifinner,state,stomach,gullet,(s,gu,cmd) =>ifinner::<ET>(s,gu,cmd));
    register_conditional!(ifmmode,state,stomach,gullet,(s,gu,cmd) =>ifmmode::<ET>(s,gu,cmd));
    register_conditional!(ifnum,state,stomach,gullet,(s,gu,cmd) =>ifnum::<ET>(s,gu,cmd));
    register_conditional!(ifodd,state,stomach,gullet,(s,gu,cmd) =>ifodd::<ET>(s,gu,cmd));
    register_conditional!(iftrue,state,stomach,gullet,(s,gu,cmd) => Ok(true));
    register_conditional!(ifvbox,state,stomach,gullet,(s,gu,cmd) =>todo!("ifvbox"));
    register_conditional!(ifvmode,state,stomach,gullet,(s,gu,cmd) =>ifvmode::<ET>(s,gu,cmd));
    register_conditional!(ifvoid,state,stomach,gullet,(s,gu,cmd) =>todo!("ifvoid"));
    register_conditional!(ifx,state,stomach,gullet,(s,gu,cmd) =>ifx::<ET>(s,gu,cmd));
    register_unexpandable!(immediate,state,stomach,gullet,false,(s,gu,cmd) =>immediate::<ET>(s,gu,cmd));
    register_unexpandable!(ignorespaces,state,stomach,gullet,false,(s,gu,cmd) => ignorespaces::<ET>(s,gu,cmd));
    register_expandable!(input,state,stomach,gullet,(s,gu,cmd,f) =>input::<ET>(s,gu,cmd));
    register_int!(inputlineno,state,stomach,gullet,(s,g,c) => inputlineno::<ET>(g,c));
    register_int_assign!(interlinepenalty,state,stomach,gullet);
    register_expandable!(jobname,state,stomach,gullet,(s,gu,cmd,f) =>jobname::<ET>(s,f));
    register_int_assign!(language,state,stomach,gullet);
    register_value_assign_int!(lccode,state,stomach,gullet);
    register_int_assign!(lefthyphenmin,state,stomach,gullet);
    register_skip_assign!(leftskip,state,stomach,gullet);
    register_assign!(let,state,stomach,gullet,(s,gu,cmd,global) =>let_::<ET>(s,gu,cmd,global));
    register_int_assign!(linepenalty,state,stomach,gullet);
    register_skip_assign!(lineskip,state,stomach,gullet);
    register_dim_assign!(lineskiplimit,state,stomach,gullet);
    register_assign!(long,state,stomach,gullet,(s,gu,cmd,g) =>long::<ET>(s,gu,cmd,g,false,false,false));
    register_unexpandable!(lowercase,state,stomach,gullet,false,(s,gu,cmd) =>lowercase::<ET>(s,gu,cmd));
    register_int_assign!(looseness,state,stomach,gullet);
    register_int_assign!(mag,state,stomach,gullet);
    register_int_assign!(maxdeadcycles,state,stomach,gullet);
    register_dim_assign!(maxdepth,state,stomach,gullet);
    register_assign!(mathchardef,state,stomach,gullet,(s,gu,cmd,global) =>mathchardef::<ET>(s,gu,cmd,global));
    register_value_assign_int!(mathcode,state,stomach,gullet);
    register_dim_assign!(mathsurround,state,stomach,gullet);
    register_expandable!(meaning,state,stomach,gullet,(s,g,c,f) => meaning::<ET>(s,g,c,f));
    register_unexpandable!(message,state,stomach,gullet,false,(s,gu,cmd) =>message::<ET>(s,gu,cmd));
    register_int!(month,state,stomach,gullet,(s,g,c) => month::<ET>(s,c));
    register_assign!(multiply,state,stomach,gullet,(s,gu,cmd,global) =>multiply::<ET>(s,gu,cmd,global));
    register_value_assign_muskip!(muskip,state,stomach,gullet);
    register_assign!(muskipdef,state,stomach,gullet,(s,gu,cmd,global) =>muskipdef::<ET>(s,gu,cmd,global));
    register_value_assign_int!(newlinechar,state,stomach,gullet);
    register_expandable!(noexpand,state,stomach,gullet,(s,g,c,f) => noexpand::<ET>(s,g,c,f));
    register_dim_assign!(nulldelimiterspace,state,stomach,gullet);
    state.set_command(ET::Char::from_str("nullfont"), Some(Command::new(
        BaseCommand::Font(state.fontstore().null())
        ,None)), true);
    register_expandable!(number,state,stomach,gullet,(s,g,c,f) => number::<ET>(s,g,c,f));
    register_unexpandable!(openin,state,stomach,gullet,false,(s,gu,cmd) =>openin::<ET>(s,gu,cmd));
    register_whatsit!(openout,state,stomach,gullet,(s,gu,cmd) =>openout::<ET>(s,gu,cmd));
    register_expandable!(or,state,stomach,gullet,(s,g,c,f) => or::<ET>(s,g,c));
    register_assign!(outer,state,stomach,gullet,(s,gu,cmd,g) =>outer::<ET>(s,gu,cmd,g,false,false,false));
    register_tok_assign!(output,state,stomach,gullet);
    register_int_assign!(outputpenalty,state,stomach,gullet);
    register_dim_assign!(overfullrule,state,stomach,gullet);

    state.set_command(ET::Char::par_token(),Some(Command::new(BaseCommand::Unexpandable {
        name:"par",
        apply:|s,gu,cmd| par::<ET>(s,cmd),
        starts_paragraph:false
    },None)),true);
    register_skip_assign!(parfillskip,state,stomach,gullet);
    register_dim_assign!(parindent,state,stomach,gullet);
    register_skip_assign!(parskip,state,stomach,gullet);
    register_unexpandable!(patterns,state,stomach,gullet,false,(s,gu,cmd) =>patterns::<ET>(s,gu,cmd));
    register_int_assign!(pausing,state,stomach,gullet);
    register_int_assign!(postdisplaypenalty,state,stomach,gullet);
    register_int_assign!(predisplaypenalty,state,stomach,gullet);
    register_dim_assign!(predisplaysize,state,stomach,gullet);
    register_int_assign!(relpenalty,state,stomach,gullet);
    register_int_assign!(righthyphenmin,state,stomach,gullet);
    register_int_assign!(pretolerance,state,stomach,gullet);
    register_assign!(read,state,stomach,gullet,(s,gu,cmd,global) =>read::<ET>(s,gu,cmd,global));
    state.set_command(ET::Char::relax_token(), Some(Command::new(BaseCommand::Relax,None)), true);
    register_int_assign!(relpenalty,state,stomach,gullet);
    register_skip_assign!(rightskip,state,stomach,gullet);
    register_expandable!(romannumeral,state,stomach,gullet,(s,g,c,f) => romannumeral::<ET>(s,g,c,f));
    register_dim_assign!(scriptspace,state,stomach,gullet);
    register_assign!(setbox,state,stomach,gullet,(s,gu,cmd,global) =>setbox::<ET>(s,gu,cmd,global));
    register_value_assign_int!(sfcode,state,stomach,gullet);
    register_int_assign!(showboxbreadth,state,stomach,gullet);
    register_int_assign!(showboxdepth,state,stomach,gullet);
    register_value_assign_int!(skewchar,state,stomach,gullet);
    register_value_assign_skip!(skip,state,stomach,gullet);
    register_assign!(skipdef,state,stomach,gullet,(s,gu,cmd,global) =>skipdef::<ET>(s,gu,cmd,global));
    register_skip_assign!(spaceskip,state,stomach,gullet);
    register_dim_assign!(splitmaxdepth,state,stomach,gullet);
    register_skip_assign!(splittopskip,state,stomach,gullet);
    register_expandable!(string,state,stomach,gullet,(s,g,c,f) => string::<ET>(s,g,c,f));
    register_skip_assign!(tabskip,state,stomach,gullet);
    register_expandable!(the,state,stomach,gullet,(s,g,c,f) => the::<ET>(s,g,c,f));
    register_int!(time,state,stomach,gullet,(s,g,c) => time::<ET>(s,c));
    register_value_assign_toks!(toks,state,stomach,gullet);
    register_assign!(toksdef,state,stomach,gullet,(s,gu,cmd,global) =>toksdef::<ET>(s,gu,cmd,global));
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
    register_unexpandable!(uppercase,state,stomach,gullet,false,(s,gu,cmd) =>uppercase::<ET>(s,gu,cmd));
    register_int_assign!(vbadness,state,stomach,gullet);
    register_dim_assign!(vfuzz,state,stomach,gullet);
    register_dim_assign!(voffset,state,stomach,gullet);
    register_unexpandable!(vrule,state,stomach,gullet,true,(s,gu,cmd) =>vrule::<ET>(s,gu,cmd));
    register_dim_assign!(vsize,state,stomach,gullet);
    register_int_assign!(widowpenalty,state,stomach,gullet);
    register_whatsit!(write,state,stomach,gullet,(s,gu,cmd) =>write::<ET>(s,gu,cmd));
    register_assign!(xdef,state,stomach,gullet,(s,gu,cmd,global) =>xdef::<ET>(s,gu,cmd,global,false,false,false));
    register_skip_assign!(xspaceskip,state,stomach,gullet);
    register_int!(year,state,stomach,gullet,(s,g,c) => year::<ET>(s,c));

    register_muskip_assign!(thinmuskip,state,stomach,gullet);
    register_muskip_assign!(medmuskip,state,stomach,gullet);
    register_muskip_assign!(thickmuskip,state,stomach,gullet);
    

    // TODOS ---------------------------------------------------------------------

    cmstodo!(state,stomach,gullet,mathord);
    cmstodo!(state,stomach,gullet,mathop);
    cmstodo!(state,stomach,gullet,mathbin);
    cmstodo!(state,stomach,gullet,mathrel);
    cmstodo!(state,stomach,gullet,mathopen);
    cmstodo!(state,stomach,gullet,mathclose);
    cmstodo!(state,stomach,gullet,mathpunct);
    cmstodo!(state,stomach,gullet,mathinner);
    cmstodo!(state,stomach,gullet,mathaccent);
    cmstodo!(state,stomach,gullet,radical);
    cmstodo!(state,stomach,gullet,delimiter);
    cmstodo!(state,stomach,gullet,prevdepth);


    cmtodo!(state,stomach,gullet,lastpenalty);
    cmtodo!(state,stomach,gullet,parshape);
    cmtodo!(state,stomach,gullet,badness);
    cmtodo!(state,stomach,gullet,spacefactor);
    cmtodo!(state,stomach,gullet,prevgraf);
    cmtodo!(state,stomach,gullet,deadcycles);
    cmtodo!(state,stomach,gullet,insertpenalties);
    cmtodo!(state,stomach,gullet,textfont);
    cmtodo!(state,stomach,gullet,scriptfont);
    cmtodo!(state,stomach,gullet,scriptscriptfont);
    cmtodo!(state,stomach,gullet,lastkern);
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
    cmtodo!(state,stomach,gullet,raise);
    cmtodo!(state,stomach,gullet,lower);
    cmtodo!(state,stomach,gullet,setlanguage);
    cmtodo!(state,stomach,gullet,nonscript);
    cmtodo!(state,stomach,gullet,vcenter);
    cmtodo!(state,stomach,gullet,underline);
    cmtodo!(state,stomach,gullet,overline);
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
    cmtodo!(state,stomach,gullet,fontname);
    cmtodo!(state,stomach,gullet,hskip);
    cmtodo!(state,stomach,gullet,italiccorr);
    cmtodo!(state,stomach,gullet,mathchar);
    cmtodo!(state,stomach,gullet,medskip);
    cmtodo!(state,stomach,gullet,mskip);
    cmtodo!(state,stomach,gullet,noalign);
    cmtodo!(state,stomach,gullet,omit);
    cmtodo!(state,stomach,gullet,smallskip);
    cmtodo!(state,stomach,gullet,span);
    cmtodo!(state,stomach,gullet,unhbox);
    cmtodo!(state,stomach,gullet,unhcopy);
}

