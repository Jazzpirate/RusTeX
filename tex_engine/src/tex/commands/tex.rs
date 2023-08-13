//! TeX primitive [`BaseCommand`]s

use std::collections::VecDeque;
use std::hint::unreachable_unchecked;
use std::marker::PhantomData;
use std::path::Components;
use crate::{debug_log, register_assign, register_conditional, register_int_assign, register_unexpandable, register_tok_assign, register_int, register_whatsit, register_value_assign_int, register_value_assign_dim, register_value_assign_muskip, register_value_assign_skip, register_dim_assign, register_skip_assign, cmtodo, register_value_assign_font, register_open_box, cmstodo, register_muskip_assign, register_expandable, catch, file_end, throw, catch_prim, file_end_prim, register_value_assign_toks};
use crate::engine::filesystem::{File, FileSystem};
use crate::engine::gullet::Gullet;
use crate::engine::gullet::methods::{tokens_to_string, token_to_chars, resolve_token};
use crate::engine::state::State;
use crate::engine::mouth::Mouth;
use crate::engine::state::modes::{BoxMode, GroupType, TeXMode};
use crate::engine::stomach::Stomach;
use crate::tex::catcodes::{CategoryCode, CategoryCodeScheme};
use crate::tex::commands::{BaseCommand, Def, ExpToken, ParamToken, Command, ResolvedToken, BaseStomachCommand, BoxFun, CloseBoxFun, TokenCont, ValueCommand, CommandSource, DefI};
use crate::tex::commands::methods::parse_signature;
use crate::tex::numbers::{Int, Skip, Numeric, MuSkip, Dim};
use crate::tex::token::{BaseToken, Token, TokenList};
use crate::utils::errors::{TeXError};
use crate::utils::Ptr;
use crate::utils::strings::{AllCharsTrait, CharType, TeXStr};
use chrono::{Datelike, Timelike};
use log::warn;
use crate::engine::{EngineMut, EngineType, gullet};
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

pub fn SPACE<ET:EngineType>(_:&mut EngineMut<ET>, _cmd:CommandSource<ET>)
                            -> Result<(),TeXError<ET>> {
    todo!("\\ ")
}

pub static ADVANCE: &str = "advance";
pub fn advance<ET:EngineType>(engine: &mut EngineMut<ET>, cmd:CommandSource<ET>, global:bool)
                              -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"\\advance");
    catch_prim!(engine.skip_whitespace() => (ADVANCE,cmd));
    match catch_prim!(engine.get_next_unexpandable() => (ADVANCE,cmd)) {
        None => file_end_prim!(ADVANCE,cmd),
        Some(ncmd) => match ncmd.command {
            BaseCommand::Int(a) => {
                macro_rules! finish {
                    ($old:expr,$nv:ident => $set:expr) => {{
                        catch_prim!(engine.get_keyword("by") => (ADVANCE,cmd));
                        let i = catch_prim!(engine.get_int() => (ADVANCE,cmd));
                        let $nv = $old + i;
                        $set;
                        Ok(())
                    }}
                }
                match a {
                    ValueCommand::Register(u) => finish!(engine.state.get_int_register(u),nv => engine.state.set_int_register(u,nv,global)),
                    ValueCommand::Primitive(name) => finish!(engine.state.get_primitive_int(name),nv => engine.state.set_primitive_int(name,nv,global)),
                    ValueCommand::Complex {name,..} if name == COUNT => {
                        let int = catch_prim!(engine.get_int() => (ADVANCE,cmd));
                        let u = match int.try_into() {
                            Ok(u) => u,
                            _ => throw!("Not a valid register: {}",int => cmd.cause)
                        };
                        finish!(engine.state.get_int_register(u),nv => engine.state.set_int_register(u,nv,global))
                    }
                    _ => throw!("Unexpected token after \\advance" => cmd.cause)
                }
            }
            BaseCommand::Dim(a) => {
                macro_rules! finish {
                    ($old:expr,$nv:ident => $set:expr) => {{
                        catch_prim!(engine.get_keyword("by") => (ADVANCE,cmd));
                        let i = catch_prim!(engine.get_dim() => (ADVANCE,cmd));
                        let $nv = $old + i;
                        $set;
                        Ok(())
                    }}
                }
                match a {
                    ValueCommand::Register(u) => finish!(engine.state.get_dim_register(u),nv => engine.state.set_dim_register(u,nv,global)),
                    ValueCommand::Primitive(name) => finish!(engine.state.get_primitive_dim(name),nv => engine.state.set_primitive_dim(name,nv,global)),
                    ValueCommand::Complex {name,..} if name == DIMEN => {
                        let int = catch_prim!(engine.get_int() => (ADVANCE,cmd));
                        let u = match int.try_into() {
                            Ok(u) => u,
                            _ => throw!("Not a valid register: {}",int => cmd.cause)
                        };
                        finish!(engine.state.get_dim_register(u),nv => engine.state.set_dim_register(u,nv,global))
                    }
                    _ => throw!("Unexpected token after \\advance" => cmd.cause)
                }
            }
            BaseCommand::Skip(a) => {
                macro_rules! finish {
                    ($old:expr,$nv:ident => $set:expr) => {{
                        catch_prim!(engine.get_keyword("by") => (ADVANCE,cmd));
                        let i = catch_prim!(engine.get_skip() => (ADVANCE,cmd));
                        let $nv = $old + i;
                        $set;
                        Ok(())
                    }}
                }
                match a {
                    ValueCommand::Register(u) => finish!(engine.state.get_skip_register(u),nv => engine.state.set_skip_register(u,nv,global)),
                    ValueCommand::Primitive(name) => finish!(engine.state.get_primitive_skip(name),nv => engine.state.set_primitive_skip(name,nv,global)),
                    ValueCommand::Complex {name,..} if name == SKIP => {
                        let int = catch_prim!(engine.get_int() => (ADVANCE,cmd));
                        let u = match int.try_into() {
                            Ok(u) => u,
                            _ => throw!("Not a valid register: {}",int => cmd.cause)
                        };
                        finish!(engine.state.get_skip_register(u),nv => engine.state.set_skip_register(u,nv,global))
                    }
                    _ => throw!("Unexpected token after \\advance" => cmd.cause)
                }
            }
            BaseCommand::MuSkip(a) => {
                macro_rules! finish {
                    ($old:expr,$nv:ident => $set:expr) => {{
                        catch_prim!(engine.get_keyword("by") => (ADVANCE,cmd));
                        let i = catch_prim!(engine.get_muskip() => (ADVANCE,cmd));
                        let $nv = $old + i;
                        $set;
                        Ok(())
                    }}
                }
                match a {
                    ValueCommand::Register(u) => finish!(engine.state.get_muskip_register(u),nv => engine.state.set_muskip_register(u,nv,global)),
                    ValueCommand::Primitive(name) => finish!(engine.state.get_primitive_muskip(name),nv => engine.state.set_primitive_muskip(name,nv,global)),
                    ValueCommand::Complex {name,..} if name == MUSKIP => {
                        let int = catch_prim!(engine.get_int() => (ADVANCE,cmd));
                        let u = match int.try_into() {
                            Ok(u) => u,
                            _ => throw!("Not a valid register: {}",int => cmd.cause)
                        };
                        finish!(engine.state.get_muskip_register(u),nv => engine.state.set_muskip_register(u,nv,global))
                    }
                    _ => throw!("Unexpected token after \\advance" => cmd.cause)
                }
            }
            o => throw!("expected register after \\advance;got:{:?}",o => cmd.cause)
        }
    }
}

pub static AFTERASSIGNMENT: &str = "afterassignment";
pub fn afterassignment<ET:EngineType>(engine: &mut EngineMut<ET>, cmd:CommandSource<ET>) -> Result<(), TeXError<ET>> {
    debug_log!(trace=>"\\afterassignment");
    let next = match catch_prim!(engine.get_next_token() => (AFTERASSIGNMENT,cmd)) {
        None => file_end!(cmd.cause),
        Some((t,_)) => t
    };
    engine.state.set_afterassignment(next);
    Ok(())
}

pub fn begingroup<ET:EngineType>(state:&mut ET::State)-> Result<(),TeXError<ET>> {
    state.stack_push(GroupType::CS);
    Ok(())
}

pub static CATCODE: &str = "catcode";
pub fn catcode_assign<ET:EngineType>(engine: &mut EngineMut<ET>, cmd:CommandSource<ET>, global:bool) -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"Assigning category code");
    let c = catch_prim!(engine.get_char() => (CATCODE,cmd));
    catch_prim!(engine.skip_eq_char() => (CATCODE,cmd));
    let v = catch_prim!(engine.get_int() => (CATCODE,cmd)).to_i64();
    if v < 0 || v > 15 {
        throw!("Invalid category code: {}",v => cmd.cause)
    }
    let cc: CategoryCode = unsafe{(v as u8).try_into().unwrap_unchecked()};
    debug_log!(debug=>"\\catcode '{}' = {}",c.char_str(),cc);
    engine.state.set_catcode(c,cc,global);
    Ok(())
}
pub fn catcode_get<ET:EngineType>(engine: &mut EngineMut<ET>, cmd:CommandSource<ET>) -> Result<ET::Int,TeXError<ET>> {
    debug_log!(trace=>"Getting category code");
    let c = catch_prim!(engine.get_char() => (CATCODE,cmd));
    let cc = *engine.state.get_catcode_scheme().get(&c);
    let v : u8 = cc.into();
    debug_log!(debug=>"\\catcode '{}' == {}",c.char_str(),cc);
    Ok(v.into())
}

pub static CHARDEF: &str = "chardef";
pub fn chardef<ET:EngineType>(engine: &mut EngineMut<ET>, cmd:CommandSource<ET>, global:bool) -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"chardef");
    let name = catch_prim!(engine.get_control_sequence() => (CHARDEF,cmd));
    catch_prim!(engine.skip_eq_char() => (CHARDEF,cmd));
    let char = catch_prim!(engine.get_char() => (CHARDEF,cmd));
    let cmd = Command::new(BaseCommand::CharDef(char),Some(&cmd));
    engine.set_command_for_tk(name, Some(cmd), global);
    Ok(())
}

pub static CLOSEIN: &str = "closein";
pub fn closein<ET:EngineType>(engine: &mut EngineMut<ET>, cmd:CommandSource<ET>) -> Result<(), TeXError<ET>> {
    debug_log!(trace=>"\\closein");
    let i = catch_prim!(engine.get_int() => (CLOSEIN,cmd));
    let i : usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => throw!("Invalid file number: {}",i => cmd.cause)
    };
    engine.state.file_closein(i); // TODO error?
    Ok(())
}

pub static CLOSEOUT: &str = "closeout";
pub fn closeout<ET:EngineType>(engine: &mut EngineMut<ET>, cmd:CommandSource<ET>) -> Result<Whatsit<ET>, TeXError<ET>> {
    debug_log!(trace=>"\\closeout");
    let i = catch_prim!(engine.get_int() => (CLOSEOUT,cmd));
    let i : usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => throw!("Invalid file number: {}",i => cmd.cause)
    };
    let apply = Box::new(move |e: &mut EngineMut<ET>| {
        e.state.file_closeout(i); // TODO error?
        Ok(())
    });
    Ok(Whatsit::new(apply))
}

pub static COUNT : &str = "count";
pub fn count_assign<ET:EngineType>(engine: &mut EngineMut<ET>, cmd:CommandSource<ET>, global:bool) -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"Assigning \\count");
    let i = catch_prim!(engine.get_int() => ("count",cmd));
    let i:usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => throw!("Not a valid register: {}",i => cmd.cause)
    };
    catch_prim!(engine.skip_eq_char() => ("count",cmd));
    let v = catch_prim!(engine.get_int() => ("count",cmd));
    debug_log!(debug=>"\\count{} = {}",i,v);
    engine.state.set_int_register(i,v,global);
    Ok(())
}
pub fn count_get<ET:EngineType>(engine: &mut EngineMut<ET>, cmd:CommandSource<ET>) -> Result<ET::Int,TeXError<ET>> {
    debug_log!(trace=>"Getting \\count");
    let i = catch_prim!(engine.get_int() => ("count",cmd));
    let i:usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => throw!("Not a valid register: {}",i => cmd.cause)
    };
    let v = engine.state.get_int_register(i);
    debug_log!(debug=>"\\count{} == {}",i,v);
    Ok(v)
}

pub static COUNTDEF : &str = "countdef";
pub fn countdef<ET:EngineType>(engine: &mut EngineMut<ET>, cmd:CommandSource<ET>, global:bool)
                               -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"countdef");
    let name = catch_prim!(engine.get_control_sequence() => (COUNTDEF,cmd));
    catch_prim!(engine.set_relax(&name,&cmd,global) => (COUNTDEF,cmd));
    catch_prim!(engine.skip_eq_char() => (COUNTDEF,cmd));
    let num = catch_prim!(engine.get_int() => ("countdef",cmd));
    if num.to_i64() < 0 {
        throw!("Invalid count register index: {}",num => cmd.cause)
    }
    let num = num.to_i64() as usize;
    let cmd = Command::new(BaseCommand::Int(ValueCommand::Register(num)),Some(&cmd));
    engine.set_command_for_tk(name,Some(cmd),global);
    Ok(())
}

pub fn get_csname<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:&CommandSource<ET>, name:&'static str)
                                 -> Result<TeXStr<ET::Char>,TeXError<ET>>{
    debug_log!(trace=>"get_csname: {}",engine.preview(200));
    let csidx = engine.state.push_csname();
    let mut csname = Vec::with_capacity(16);
    while engine.state.current_csname() == Some(csidx) {
        match catch_prim!(engine.get_next_unexpandable() => (name,cmd)) {
            None => return file_end!(cmd.cause),
            Some(sc) => match sc.command {
                BaseCommand::Unexpandable {name:"endcsname",..} => engine.state.pop_csname(),
                BaseCommand::Char{catcode:CategoryCode::Space,..} => csname.push(ET::Char::from(b' ')),
                BaseCommand::Char{char,..} => csname.push(char),
                o => throw!("Unexpected token in {}: {:?}",name,o => cmd.cause)
            }
        }
    }
    Ok(csname.into())
}

pub fn csname<ET:EngineType>(engine: &mut EngineMut<ET>, cmd:CommandSource<ET>, f:TokenCont<ET>) -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"csname");
    let str = get_csname::<ET>(engine,&cmd,"csname")?;
    debug_log!(trace=>"csname {}",str.to_string());
    match engine.state.get_command(&str) {
        None => engine.state.set_command(str.clone(), Some(Command::new(BaseCommand::Relax,Some(&cmd))), false),
        _ => ()
    }
    engine.mouth.requeue(Token::new(BaseToken::CS(str),None),engine.memory);
    Ok(())
}

pub fn day<ET:EngineType>(state:&mut ET::State,cmd:CommandSource<ET>) -> Result<ET::Int,TeXError<ET>> {
    Ok(catch_prim!(ET::Int::from_i64(state.get_start_time().day() as i64) => ("day",cmd)))
}

pub static DEF : &str = "def";
pub fn def<ET:EngineType>(engine: &mut EngineMut<ET>, cmd:CommandSource<ET>, global:bool, protected:bool, long:bool, outer:bool)
                          -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"def");
    let csO = catch_prim!(engine.get_next_token() => (DEF,cmd));
    let cs = match csO {
        None => file_end_prim!(DEF,cmd),
        Some((t,_)) => t
    };
    match &cs.base {
        BaseToken::Char(_,CategoryCode::Active) => (),
        BaseToken::CS(_) => (),
        _ => throw!("Command expected after \\def" => cs)
    }
    let (endswithbrace,arity,signature) = parse_signature::<ET>(engine,&cmd,DEF)?;
    let mut replacement: Vec<ExpToken<ET>> = vec!();
    let mut partk = None;
    catch_prim!(engine.get_group(&mut |_,t| match (std::mem::take(&mut partk),&t.base) {
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
    engine.set_command_for_tk(cs,Some(def),global);
    Ok(())
}

pub fn delcode_assign<ET:EngineType>(engine: &mut EngineMut<ET>, cmd:CommandSource<ET>, global:bool)
                                     -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"Assigning delcode");
    let i = catch_prim!(engine.get_int() => ("delcode",cmd));
    let c: ET::Char = match ET::Char::from_i64(i.to_i64()) {
        Some(i) => i,
        None => throw!("Not a valid character: {}",i => cmd.cause)
    };
    catch_prim!(engine.skip_eq_char() => ("delcode",cmd));
    let i = catch_prim!(engine.get_int() => ("delcode",cmd));
    debug_log!(debug=>"\\delcode '{}' = {}",c.char_str(),i);
    engine.state.set_delcode(c,i,global);
    Ok(())
}

pub fn delcode_get<ET:EngineType>(engine: &mut EngineMut<ET>, cmd:CommandSource<ET>)
                                  -> Result<ET::Int,TeXError<ET>> {
    debug_log!(trace=>"Getting delcode");
    let i = catch_prim!(engine.get_int() => ("delcode",cmd));
    let c: ET::Char = match ET::Char::from_i64(i.to_i64()) {
        Some(i) => i,
        None => throw!("Not a valid character: {}",i => cmd.cause)
    };
    let v = engine.state.get_delcode(c);
    debug_log!(debug=>"\\delcode '{}' == {}",c.char_str(),v);
    Ok(v)
}

pub static DIMEN : &str = "dimen";

pub fn dimen_assign<ET:EngineType>(engine: &mut EngineMut<ET>, cmd:CommandSource<ET>, global:bool)
                                   -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"Assigning \\dimen");
    let i = catch_prim!(engine.get_int() => ("dimen",cmd));
    let i:usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => throw!("Not a valid register: {}",i => cmd.cause)
    };
    catch_prim!(engine.skip_eq_char() => ("dimen",cmd));
    let v = catch_prim!(engine.get_dim() => ("dimen",cmd));
    debug_log!(debug=>"\\dimen{} = {}",i,v);
    engine.state.set_dim_register(i,v,global);
    Ok(())
}

pub fn dimen_get<ET:EngineType>(engine: &mut EngineMut<ET>, cmd:CommandSource<ET>)
                                -> Result<ET::Dim,TeXError<ET>> {
    debug_log!(trace=>"Getting \\dimen");
    let i = catch_prim!(engine.get_int() => ("dimen",cmd));
    let i:usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => throw!("Not a valid register: {}",i => cmd.cause)
    };
    let v = engine.state.get_dim_register(i);
    debug_log!(debug=>"\\dimen{} == {}",i,v);
    Ok(v)
}

pub static DIMENDEF : &str = "dimendef";
pub fn dimendef<ET:EngineType>(engine: &mut EngineMut<ET>, cmd:CommandSource<ET>, global:bool)
                               -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"dimendef");
    let name = catch_prim!(engine.get_control_sequence() => (DIMENDEF,cmd));
    catch_prim!(engine.set_relax(&name,&cmd,global) => (DIMENDEF,cmd));
    catch_prim!(engine.skip_eq_char() => (DIMENDEF,cmd));
    let num = catch_prim!(engine.get_int() => (DIMENDEF,cmd));
    if num.to_i64() < 0 {
        throw!("Invalid dimen register index: {}",num => cmd.cause)
    }
    let num = num.to_i64() as usize;
    let ret = Command::new(BaseCommand::Dim(ValueCommand::Register(num)),Some(&cmd));
    engine.set_command_for_tk(name,Some(ret),global);
    Ok(())
}
pub static DIVIDE : &str = "divide";
pub fn divide<ET:EngineType>(engine: &mut EngineMut<ET>, cmd:CommandSource<ET>, global:bool)
                             -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"\\divide");
    catch_prim!(engine.skip_whitespace() => (DIVIDE,cmd));
    match catch_prim!(engine.get_next_unexpandable() => (DIVIDE,cmd)) {
        None => file_end_prim!(DIVIDE,cmd),
        Some(ncmd) => match ncmd.command {
            BaseCommand::Int(a) => {
                macro_rules! finish {
                    ($old:expr,$nv:ident => $set:expr) => {{
                        catch_prim!(engine.get_keyword("by") => (DIVIDE,cmd));
                        let i = catch_prim!(engine.get_int() => (DIVIDE,cmd));
                        if i.to_i64() == 0 {
                            throw!("Division by zero: {} / {}",$old,i => cmd.cause)
                        }
                        let $nv = $old / i;
                        $set;
                        Ok(())
                    }}
                }
                match a {
                    ValueCommand::Register(u) => finish!(engine.state.get_int_register(u),nv => engine.state.set_int_register(u,nv,global)),
                    ValueCommand::Primitive(name) => finish!(engine.state.get_primitive_int(name),nv => engine.state.set_primitive_int(name,nv,global)),
                    ValueCommand::Complex {name,..} if name == COUNT => {
                        let int = catch_prim!(engine.get_int() => (DIVIDE,cmd));
                        let u = match int.try_into() {
                            Ok(u) => u,
                            _ => throw!("Not a valid register: {}",int => cmd.cause)
                        };
                        finish!(engine.state.get_int_register(u),nv => engine.state.set_int_register(u,nv,global))
                    }
                    _ => throw!("Unexpected token after \\divide" => cmd.cause)
                }
            }
            BaseCommand::Dim(a) => {
                macro_rules! finish {
                    ($old:expr,$nv:ident => $set:expr) => {{
                        catch_prim!(engine.get_keyword("by") => (DIVIDE,cmd));
                        let i = catch_prim!(engine.get_int() => (DIVIDE,cmd)).to_i64();
                        if i == 0 {
                            throw!("Division by zero: {} / {}",$old,i => cmd.cause)
                        }
                        let $nv = $old.tex_div(i);
                        $set;
                        Ok(())
                    }}
                }
                match a {
                    ValueCommand::Register(u) => finish!(engine.state.get_dim_register(u),nv => engine.state.set_dim_register(u,nv,global)),
                    ValueCommand::Primitive(name) => finish!(engine.state.get_primitive_dim(name),nv => engine.state.set_primitive_dim(name,nv,global)),
                    ValueCommand::Complex {name,..} if name == DIMEN => {
                        let int = catch_prim!(engine.get_int() => (DIVIDE,cmd));
                        let u = match int.try_into() {
                            Ok(u) => u,
                            _ => throw!("Not a valid register: {}",int => cmd.cause)
                        };
                        finish!(engine.state.get_dim_register(u),nv => engine.state.set_dim_register(u,nv,global))
                    }
                    _ => throw!("Unexpected token after \\divide" => cmd.cause)
                }
            }
            BaseCommand::Skip(a) => {
                macro_rules! finish {
                    ($old:expr,$nv:ident => $set:expr) => {{
                        catch_prim!(engine.get_keyword("by") => (DIVIDE,cmd));
                        let i = catch_prim!(engine.get_int() => (DIVIDE,cmd)).to_i64();
                        if i == 0 {
                            throw!("Division by zero: {} / {}",$old,i => cmd.cause)
                        }
                        let $nv = $old.tex_div(i);
                        $set;
                        Ok(())
                    }}
                }
                match a {
                    ValueCommand::Register(u) => finish!(engine.state.get_skip_register(u),nv => engine.state.set_skip_register(u,nv,global)),
                    ValueCommand::Primitive(name) => finish!(engine.state.get_primitive_skip(name),nv => engine.state.set_primitive_skip(name,nv,global)),
                    ValueCommand::Complex {name,..} if name == SKIP => {
                        let int = catch_prim!(engine.get_int() => (DIVIDE,cmd));
                        let u = match int.try_into() {
                            Ok(u) => u,
                            _ => throw!("Not a valid register: {}",int => cmd.cause)
                        };
                        finish!(engine.state.get_skip_register(u),nv => engine.state.set_skip_register(u,nv,global))
                    }
                    _ => throw!("Unexpected token after \\divide" => cmd.cause)
                }
            }
            BaseCommand::MuSkip(a) => {
                macro_rules! finish {
                    ($old:expr,$nv:ident => $set:expr) => {{
                        catch_prim!(engine.get_keyword("by") => (DIVIDE,cmd));
                        let i = catch_prim!(engine.get_int() => (DIVIDE,cmd)).to_i64();
                        if i == 0 {
                            throw!("Division by zero: {} / {}",$old,i => cmd.cause)
                        }
                        let $nv = $old.tex_div(i);
                        $set;
                        Ok(())
                    }}
                }
                match a {
                    ValueCommand::Register(u) => finish!(engine.state.get_muskip_register(u),nv => engine.state.set_muskip_register(u,nv,global)),
                    ValueCommand::Primitive(name) => finish!(engine.state.get_primitive_muskip(name),nv => engine.state.set_primitive_muskip(name,nv,global)),
                    ValueCommand::Complex {name,..} if name == MUSKIP => {
                        let int = catch_prim!(engine.get_int() => (DIVIDE,cmd));
                        let u = match int.try_into() {
                            Ok(u) => u,
                            _ => throw!("Not a valid register: {}",int => cmd.cause)
                        };
                        finish!(engine.state.get_muskip_register(u),nv => engine.state.set_muskip_register(u,nv,global))
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
pub fn edef<ET:EngineType>(engine: &mut EngineMut<ET>, cmd:CommandSource<ET>, global:bool, protected:bool, long:bool, outer:bool)
                           -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"edef");
    let csO = catch_prim!(engine.get_next_token() => (EDEF,cmd));
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
    let (endswithbrace,arity,signature) = parse_signature::<ET>(engine,&cmd,EDEF)?;
    let mut replacement: Vec<ExpToken<ET>> = vec!();
    let mut partk = None;
    catch_prim!(engine.get_expanded_group(false,true,false,&mut |_,t| match (std::mem::take(&mut partk),&t.base) {
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
    engine.set_command_for_tk(cs,Some(def),global);
    Ok(())
}

pub static ELSE: &str = "else";
pub fn else_<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>)
                            -> Result<(),TeXError<ET>> {
    match engine.gullet.current_conditional() {
        (None,_) => throw!("Not in a conditional" => cmd.cause),
        (Some(ConditionalBranch::True(name)),i) =>
            catch_prim!(crate::engine::gullet::methods::else_loop::<ET>(engine.gullet,engine.mouth,engine.state,engine.memory,name,i,false) => (ELSE,cmd)),
        (Some(ConditionalBranch::Case(_,_)),i) =>
            catch_prim!(crate::engine::gullet::methods::else_loop::<ET>(engine.gullet,engine.mouth,engine.state,engine.memory,IFCASE,i,false) => (IFCASE,cmd)),
        o => unreachable!("{:?}\nat:{}\n{}\n",o,engine.current_position(),engine.preview(200))
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

pub fn endgroup<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>)
                               -> Result<(),TeXError<ET>> {
    engine.add_expansion(|engine,s| {
        match engine.state.stack_pop() {
            Some((mut v,GroupType::CS)) => {
                for t in v.drain(..) {
                    s.push(t,engine.memory);
                }
                Ok(())
            }
            _ => throw!("No group to end" => cmd.cause)
        }
    })
}

pub fn endinput<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>)
                               -> Result<(),TeXError<ET>> {
    engine.mouth.endinput(engine.state,engine.memory);
    Ok(())
}

pub fn endlinechar_assign<ET:EngineType>(engine: &mut EngineMut<ET>, cmd:CommandSource<ET>, global:bool)
                                         -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"Assigning \\endlinechar");
    catch_prim!(engine.skip_eq_char() => ("endlinechar",cmd));
    let i = catch_prim!(engine.get_int() => ("endlinechar",cmd));
    let c = match i.to_i64() {
        -1|255 => None,
        j => match ET::Char::from_i64(j) {
            Some(c) => Some(c),
            None => throw!("Not a valid character: {}",j => cmd.cause)
        }
    };
    debug_log!(debug=>"\\endlinechar = {:?}",c.map(|c| c.char_str()));
    engine.state.set_endlinechar(c,global);
    Ok(())
}
pub fn endlinechar_get<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>)
                                      -> Result<ET::Int,TeXError<ET>> {
    debug_log!(trace=>"Getting \\endlinechar");
    let c = match engine.state.get_endlinechar() {
        None => -1,
        Some(c) => c.to_usize() as i64
    };
    debug_log!(debug=>"\\endlinechar == {:?}",c);
    Ok(ET::Int::from_i64::<ET>(c).unwrap())
}

// \errhelp
pub static ERRMESSAGE : &str = "errmessage";

pub fn errmessage<ET:EngineType>(engine: &mut EngineMut<ET>, cmd:CommandSource<ET>)
                                 -> Result<(),TeXError<ET>> {
    debug_log!(debug=>"errmessage");
    catch_prim!(engine.skip_whitespace() => (ERRMESSAGE,cmd));
    let errmsg = catch_prim!(engine.get_braced_string() => (ERRMESSAGE,cmd));
    let eh = engine.state.get_primitive_toks("errhelp");
    // TODO errhelp
    Err(TeXError{
        msg:errmsg + "\n\n" + &engine.current_position(),
        cause:Some(cmd.cause),
        source:None
    }.into())
}

pub fn errorstopmode<ET:EngineType>() -> Result<(), TeXError<ET>> {
    debug_log!(trace=>"\\errorstopmode");
    // TODO
    Ok(())
}

pub fn escapechar_assign<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>, global:bool)
                                        -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"Assigning \\escapechar");
    catch_prim!(engine.skip_eq_char() => ("escapechar",cmd));
    let i = catch_prim!(engine.get_int() => ("escapechar",cmd));
    let c = match i.to_i64() {
        -1|255 => None,
        j => match ET::Char::from_i64(j) {
            Some(c) => Some(c),
            None => throw!("Not a valid character: {}",j => cmd.cause)
        }
    };
    debug_log!(debug=>"\\escapechar = {:?}",c.map(|c| c.char_str()));
    engine.state.set_escapechar(c,global);
    Ok(())
}
pub fn escapechar_get<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>) -> Result<ET::Int,TeXError<ET>> {
    debug_log!(trace=>"Getting \\escapechar");
    let c = match engine.state.get_escapechar() {
        None => -1,
        Some(c) => c.to_usize() as i64
    };
    debug_log!(debug=>"\\escapechar == {}",c);
    Ok(ET::Int::from_i64::<ET>(c).unwrap())
}

pub static EXPANDAFTER : &str = "expandafter";
pub fn expandafter<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>, f:TokenCont<ET>)
                                  -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"expandafter");
    let first = match catch_prim!(engine.get_next_token() => (EXPANDAFTER,cmd)){
        None => file_end_prim!(EXPANDAFTER,cmd),
        Some((t,_)) => t
    };
    let next = match catch_prim!(engine.get_next_token() => (EXPANDAFTER,cmd)){
        None => file_end_prim!(EXPANDAFTER,cmd),
        Some((t,_)) => t
    };

    match engine.expand(resolve_token::<ET>(engine.state,next))? {
        None => (),
        Some(next) => {
            engine.mouth.requeue(next.source.cause,engine.memory);
        }
    }
    engine.mouth.requeue(first,engine.memory);
    Ok(())
}

pub static FI : &str = "fi";
pub fn fi<ET:EngineType>(engine:&mut EngineMut<ET>, _cmd:CommandSource<ET>)
                         -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"...end of conditional.");
    engine.gullet.pop_conditional();
    Ok(())
}

pub static FONT :&str = "font";
pub fn font_assign<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>, global:bool)
                                  -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"Assigning \\font");
    let cs = catch_prim!(engine.get_control_sequence() => (FONT,cmd));
    catch_prim!(engine.skip_eq_char() => (FONT,cmd));
    let mut fontname = catch_prim!(engine.get_string() => (FONT,cmd)).to_string();
    if !fontname.ends_with(".tfm") {
        fontname = fontname + ".tfm"
    }
    let mut font = catch_prim!(engine.state.fontstore_mut().get_new(&fontname) => (FONT,cmd));
    match catch_prim!(engine.get_keywords(vec!("at","scaled")) => (FONT,cmd)) {
        Some(s) if s == "at" => {
            let dim = catch_prim!(engine.get_dim() => (FONT,cmd));
            font.set_at(dim.to_sp());
        }
        Some(s) if s == "scaled" => {
            let r = catch_prim!(crate::engine::gullet::numeric_methods::read_float::<ET>(engine,b'0',false) => (FONT,cmd));
            let new_at = ((font.get_at() as f64) * r).round() as i64;
            font.set_at(new_at);
        }
        _ => ()
    }
    let fontcmd = Command::new(BaseCommand::Font(font),Some(&cmd));
    engine.set_command_for_tk(cs,Some(fontcmd),global);
    Ok(())
}
pub fn font_get<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>)
                               -> Result<ET::Font,TeXError<ET>> {
    debug_log!(trace=>"Getting \\font");
    Ok(engine.state.get_current_font().clone())
}

pub fn fontdimen_assign<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>, global:bool)
                                       -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"Assigning \\fontdimen");
    let o = catch_prim!(engine.get_int() => ("fontdimen",cmd));
    let i = match o.try_into() {
        Ok(i) => i,
        Err(_) => throw!("Invalid font dimension index" => cmd.cause)
    };
    let mut font = catch_prim!(engine.get_font() => ("fontdimen",cmd));
    catch_prim!(engine.skip_eq_char() => ("fontdimen",cmd));
    let dim = catch_prim!(engine.get_dim() => ("fontdimen",cmd));
    font.set_dim::<ET::Dim>(i,dim);
    Ok(())
}

pub fn fontdimen_get<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>)
                                    -> Result<ET::Dim,TeXError<ET>> {
    debug_log!(trace=>"Getting \\fontdimen");
    let o = catch_prim!(engine.get_int() => ("fontdimen",cmd));
    let i = match o.try_into() {
        Ok(i) => i,
        Err(_) => throw!("Invalid font dimension index: {}",o => cmd.cause)
    };
    let font = catch_prim!(engine.get_font() => ("fontdimen",cmd));
    Ok(font.get_dim::<ET::Dim>(i))
}

pub fn futurelet<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>, global:bool)
                                -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"\\futurelet");
    let cs = match catch_prim!(engine.get_next_token() => ("futurelet",cmd)) {
        None => file_end_prim!("futurelet",cmd),
        Some((t,_)) => match &t.base {
            BaseToken::Char(_,CategoryCode::Active) => t,
            BaseToken::CS(_) => t,
            _ => throw!("Expected control sequence after \\futurelet" => cmd.cause)
        }
    };
    let first = match catch_prim!(engine.get_next_token() => ("futurelet",cmd)) {
        None => file_end_prim!("futurelet",cmd),
        Some((t,_)) => t
    };
    let second = match catch_prim!(engine.get_next_token() => ("futurelet",cmd)) {
        None => file_end_prim!("futurelet",cmd),
        Some((t,_)) => t
    };
    let newcmd = match &second.base {
        BaseToken::Char(c,CategoryCode::Active) => engine.state.get_ac_command(c).cloned(),
        BaseToken::CS(name) => engine.state.get_command(name).cloned(),
        BaseToken::Char(c,cc) =>
            Some(Command::new(BaseCommand::Char{char:*c,catcode:*cc},Some(&cmd)))
    };
    debug_log!(debug=>"\\futurelet: setting {} to {:?}",cs,newcmd);
    engine.set_command_for_tk(cs,newcmd,global);
    engine.add_expansion(move |e,s| {
        s.push(first,e.memory);s.push(second,e.memory);Ok(())
    })
}


pub fn gdef<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>, global:bool, protected:bool, long:bool, outer:bool)
                           -> Result<(),TeXError<ET>> {
    def::<ET>(engine,cmd,true,protected,long,outer)
}

pub static GLOBAL: &str = "global";
pub fn global<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>, global_:bool, protected_:bool, long_:bool, outer_:bool)
                             -> Result<(),TeXError<ET>> {
    debug_log!(trace => "\\global");
    catch_prim!(engine.skip_whitespace() => (GLOBAL,cmd));
    match catch_prim!(engine.get_next_stomach_command() => (GLOBAL,cmd)) {
        None => file_end_prim!(GLOBAL,cmd),
        Some(c) => match c.command {
            BaseStomachCommand::Assignment {name:Some("global"),..} => global::<ET>(engine,cmd,true,protected_,long_,outer_),
            BaseStomachCommand::Assignment{name:Some("protected"),..} => super::etex::protected::<ET>(engine,cmd,true,protected_,long_,outer_),
            BaseStomachCommand::Assignment {name:Some("long"),..} => long::<ET>(engine,cmd,true,protected_,long_,outer_),
            BaseStomachCommand::Assignment {name:Some("outer"),..} => outer::<ET>(engine,cmd,true,protected_,long_,outer_),
            BaseStomachCommand::Assignment {name:Some("def"),..} => def::<ET>(engine,cmd,true,protected_,long_,outer_),
            BaseStomachCommand::Assignment {name:Some("edef"),..} => edef::<ET>(engine,cmd,true,protected_,long_,outer_),
            BaseStomachCommand::Assignment {name:Some("gdef"),..} => gdef::<ET>(engine,cmd,true,protected_,long_,outer_),
            BaseStomachCommand::Assignment {name:Some("xdef"),..} => xdef::<ET>(engine,cmd,true,protected_,long_,outer_),
            BaseStomachCommand::Assignment {set,..} => Ok(catch_prim!(set(engine,c.source,true) => (GLOBAL,cmd))),
            BaseStomachCommand::ValueAss(set) => Ok(catch_prim!(set(engine,c.source,true) => (GLOBAL,cmd))),
            o => todo!("global: {:?} at {}",o,engine.preview(100))
        }
    }
}

pub static HBOX: &str = "hbox";
pub fn hbox<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>)
                           -> Result<CloseBoxFun<ET>,TeXError<ET>> {
    debug_log!(trace=>"\\hbox");
    let (to,spread) = match catch_prim!(engine.get_keywords(vec!("spread","to")) => (HBOX,cmd)) {
        None => (None,None),
        Some(s) if s == "to" => {
            let a = catch_prim!(engine.get_dim() => (HBOX,cmd));
            (Some(a),None)
        },
        Some(s) if s == "spread" => {
            let a = catch_prim!(engine.get_dim() => (HBOX,cmd));
            (None,Some(a))
        },
        _ => unreachable!()
    };
    while let Some(next) = catch_prim!(engine.get_next_unexpandable() => (HBOX,cmd)) {
        match next.command {
            BaseCommand::Char{catcode:CategoryCode::Space,..} => {},
            BaseCommand::Relax => {},
            BaseCommand::Char{catcode:CategoryCode::BeginGroup,..} => {
                engine.state.stack_push(GroupType::Box(BoxMode::H));
                engine.add_expansion(|engine,s| {
                    for t in engine.state.get_primitive_toks("everyhbox") {
                        s.push(t.clone(),engine.memory);
                    }
                });
                return Ok(Box::new(move |e,children| {
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
pub fn hrule<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>)
                            -> Result<(),TeXError<ET>> {
    debug_log!(trace => "\\hrule");
    let mut width = None;
    let mut height = None;
    let mut depth = None;
    loop {
        match catch_prim!(engine.get_keywords(vec!("width","height","depth")) => (HRULE,cmd)) {
            None => break,
            Some(s) => {
                let val = catch_prim!(engine.get_dim() => (HRULE,cmd));
                match s {
                    "width" => width = Some(val),
                    "height" => height = Some(val),
                    "depth" => depth = Some(val),
                    _ => unsafe{unreachable_unchecked()}
                }
            }
        }
    }
    engine.state.push_node(SimpleNode::Rule {
        width,height,depth,axis:HorV::Horizontal
    }.as_node());
    Ok(())
}

pub fn hyphenation<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>)
                                  -> Result<(), TeXError<ET>> {
    debug_log!(trace=>"\\hyphenation");
    // TODO
    catch_prim!(engine.get_argument(&mut |_,_| Ok(())) => ("hyphenation",cmd));
    Ok(())
}

pub fn hyphenchar_assign<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>, global:bool)
                                        -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"Assigning \\hyphenchar");
    let mut font = catch_prim!(engine.get_font() => ("hyphenchar",cmd));
    catch_prim!(engine.skip_eq_char() => ("hyphenchar",cmd));
    let i = catch_prim!(engine.get_int() => ("hyphenchar",cmd)).to_i64();
    debug_log!(debug=>"\\hyphenchar\\{:?} = {:?}",font,i);
    font.set_hyphenchar(i);
    Ok(())
}
pub fn hyphenchar_get<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>) -> Result<ET::Int,TeXError<ET>> {
    debug_log!(trace=>"Getting \\hyphenchar");
    let font = catch_prim!(engine.get_font() => ("hyphenchar",cmd));
    debug_log!(debug=>"\\hyphenchar == {:?}",font.get_hyphenchar());
    Ok(catch_prim!(ET::Int::from_i64::<ET>(font.get_hyphenchar()) => ("hyphenchar",cmd)))
}

pub fn if_<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>)
                          -> Result<bool,TeXError<ET>> {
    debug_log!(trace=>"if");
    let first = get_if_token::<ET>(engine,&cmd,"if")?;
    let second = get_if_token::<ET>(engine,&cmd,"if")?;
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
pub fn ifcat<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>)
                            -> Result<bool,TeXError<ET>> {
    debug_log!(trace=>"ifcat");
    let first = catch_prim!(get_if_token::<ET>(engine,&cmd,IFCAT) => (IFCAT,cmd));
    let second = catch_prim!(get_if_token::<ET>(engine,&cmd,IFCAT) => (IFCAT,cmd));
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

pub fn get_if_token<ET:EngineType>(engine: &mut EngineMut<ET>, cmd:&CommandSource<ET>, name:&'static str)
                                   -> Result<Option<ResolvedToken<ET>>,TeXError<ET>> {
    // need to be careful not to expand \else and \fi before conditional is done.
    while let Some((t,e)) = catch_prim!(engine.get_next_token() => (name,cmd)) {
        let r = resolve_token(engine.state,t);
        match r.command {
            BaseCommand::Expandable {name,..} if e && (name == ELSE || name == FI) && (match engine.gullet.current_conditional() {
                (Some(ConditionalBranch::None(_)),_) => true,
                _ => false
            }) => {
                engine.mouth.requeue(r.source.cause,engine.memory);
                return Ok(None)
            }
            _ if e => match engine.expand(r)? {
                Some(c) => return Ok(Some(c)),
                None => {}
            }
            _ => return Ok(Some(r))
        }
    }
    file_end_prim!(name,cmd)
}

pub static LGE : [u8;3] = [b'<',b'>',b'='];
pub static IFDIM : &str = "ifdim";
pub fn ifdim<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>)
                            -> Result<bool,TeXError<ET>> {
    debug_log!(trace=>"ifdim");
    let i1 = catch_prim!(engine.get_dim() => (IFDIM,cmd));
    let rel = match catch_prim!(engine.is_next_char_one_of(&LGE) => (IFDIM,cmd)) {
        None => throw!("Expected one of '<','>','='".to_string() => cmd.cause),
        Some(r) => r
    };
    let i2 = catch_prim!(engine.get_dim() => (IFDIM,cmd));
    match rel {
        b'<' => Ok(i1.to_sp() < i2.to_sp()),
        b'>' => Ok(i1.to_sp() > i2.to_sp()),
        b'=' => Ok(i1.to_sp() == i2.to_sp()),
        _ => unreachable!()
    }
}

pub static IFEOF : &str = "ifeof";
pub fn ifeof<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>)
                            -> Result<bool,TeXError<ET>> {
    debug_log!(trace=>"ifeof");
    let i = catch_prim!(engine.get_int() => (IFEOF,cmd));
    let i : usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => throw!("Invalid file number: {}",i => cmd.cause)
    };
    if i == 18 { return Ok(false) }
    let f = match engine.state.get_open_in_file(i) {
        None => throw!("No in file open at index: {}",i => cmd.cause),
        Some(f) => f
    };
    Ok(f.eof::<ET>(engine.state))
}

pub fn ifhmode<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>)
                              -> Result<bool,TeXError<ET>> {
    debug_log!(trace=>"ifhmode");
    Ok(match engine.state.mode() {
        TeXMode::Horizontal | TeXMode::RestrictedHorizontal => true,
        _ => false
    })
}

pub fn ifinner<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>)
                              -> Result<bool,TeXError<ET>> {
    debug_log!(trace=>"ifhmode");
    Ok(match engine.state.mode() {
        TeXMode::RestrictedHorizontal | TeXMode::InternalVertical | TeXMode::Math => true,
        _ => false
    })
}

pub fn ifmmode<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>)
                              -> Result<bool,TeXError<ET>> {
    debug_log!(trace=>"ifmmode");
    Ok(match engine.state.mode() {
        TeXMode::Math | TeXMode::Displaymath => true,
        _ => false
    })
}

pub static IFNUM : &str = "ifnum";
pub fn ifnum<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>)
                            -> Result<bool,TeXError<ET>> {
    debug_log!(trace=>"ifnum");
    let i1 = catch_prim!(engine.get_int() => (IFNUM,cmd));
    let rel = match catch_prim!(engine.is_next_char_one_of(&LGE) => (IFNUM,cmd)) {
        None => throw!("Expected one of '<','>','='" => cmd.cause),
        Some(r) => r
    };
    let i2 = catch_prim!(engine.get_int() => (IFNUM,cmd));
    match rel {
        b'<' => Ok(i1<i2),
        b'>' => Ok(i1>i2),
        b'=' => Ok(i1==i2),
        _ => unreachable!()
    }
}

pub static IFODD: &str = "ifodd";
pub fn ifodd<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>)
                            -> Result<bool,TeXError<ET>> {
    debug_log!(trace=>"ifodd");
    let num = catch_prim!(engine.get_int() => (IFODD,cmd));
    Ok(num.to_i64() % 2 != 0)
}


pub fn ifvmode<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>)
                              -> Result<bool,TeXError<ET>> {
    debug_log!(trace=>"ifvmode");
    Ok(match engine.state.mode() {
        TeXMode::Vertical | TeXMode::InternalVertical => true,
        _ => false
    })
}

pub static IFX : &str = "ifx";
pub fn ifx<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>)
                          -> Result<bool,TeXError<ET>> {
    debug_log!(trace=>"ifx");
    let t1 = match catch_prim!(engine.get_next_token() => (IFX,cmd)) {
        None => file_end_prim!("ifx",cmd),
        Some((t,e)) => resolve_token::<ET>(engine.state,t).with_expand(e)
    };
    let t2 = match catch_prim!(engine.get_next_token() => (IFX,cmd)) {
        None => file_end_prim!("ifx",cmd),
        Some((t,e)) => resolve_token::<ET>(engine.state,t).with_expand(e)
    };
    debug_log!(trace=>"ifx: {} == {}?",t1.source.cause,t2.source.cause);
    Ok(if t1.expand && t2.expand { t1.command == t2.command }
    else if !t1.expand && !t2.expand { t1.source.cause == t2.source.cause }
    else { false })
}

pub static IGNORESPACES : &str = "ignorespaces";
pub fn ignorespaces<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>)
                                   -> Result<(), TeXError<ET>> {
    debug_log!(trace=>"\\ignorespaces");
    loop {
        match catch_prim!(engine.get_next_stomach_command() => (IGNORESPACES,cmd)) {
            None => return Ok(()),
            Some(sc) => match sc.command {
                BaseStomachCommand::Space => (),
                _ => {
                    engine.mouth.requeue(sc.source.cause,engine.memory);
                    return Ok(())
                }
            }
        }
    }
}

pub static IMMEDIATE: &str ="immediate";
pub fn immediate<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>)
                                -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"immediate");
    match catch_prim!(engine.get_next_stomach_command() => (IMMEDIATE,cmd)) {
        None => file_end_prim!(IMMEDIATE,cmd),
        Some(sc) => match sc.command {
            BaseStomachCommand::Whatsit { name, apply } => {
                let wi = catch_prim!(apply(engine,sc.source) => (IMMEDIATE,cmd));
                catch_prim!(wi.apply(engine) => (IMMEDIATE,cmd));
                Ok(())
            }
            _ => {
                engine.mouth.requeue(sc.source.cause,engine.memory);
                Ok(())
            }
        }
    }
}

pub static INPUT: &str = "input";
pub fn input<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>)
                            -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"input");
    let filename = catch_prim!(engine.get_string() => (INPUT,cmd)).to_string();
    debug_log!(trace=>"input: {}",filename);
    if filename.is_empty() {
        panic!("HERE: {}",engine.current_position())
    }
    let file = engine.state.filesystem().get(&filename);
    debug_log!(trace=>"input resolved: {:?}",file.path());
    if !file.exists() {
        throw!("I can't find file `{}'",filename => cmd.cause)
    } else {
        (engine.state.outputs().file_open)(file.path().to_str().unwrap());
        engine.mouth.push_file(&file);
        Ok(())
    }
}

pub fn inputlineno<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>) -> Result<ET::Int,TeXError<ET>> {
    Ok(catch_prim!(ET::Int::from_i64(
        engine.mouth.line_no() as i64
    ) => ("month",cmd)))
}

pub fn jobname<ET:EngineType>(engine:&mut EngineMut<ET>,f:TokenCont<ET>)
                         -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"jobname");
    let jobname = engine.state.get_jobname().to_string();
    engine.string_to_tokens(jobname.as_bytes(),f)
}

pub static LCCODE: &str = "lccode";
pub fn lccode_assign<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>, global:bool)
                                    -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"Assigning lower case character");
    let c = catch_prim!(engine.get_char() => (LCCODE,cmd));
    catch_prim!(engine.skip_eq_char() => ("lccode",cmd));
    let lc = catch_prim!(engine.get_char() => (LCCODE,cmd));
    debug_log!(debug=>"\\lccode '{}' = {}",c.char_str(),lc.char_str());
    engine.state.set_lccode(c,lc,global);
    Ok(())
}

pub fn lccode_get<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>)
                                 -> Result<ET::Int,TeXError<ET>> {
    debug_log!(trace=>"Getting lower case character");
    let c = catch_prim!(engine.get_char() => (LCCODE,cmd));
    let v = catch_prim!(ET::Int::from_i64(engine.state.get_lccode(&c).to_usize() as i64) => (LCCODE,cmd));
    debug_log!(debug=>"\\lccode '{}' == {}",c.char_str(),v);
    Ok(v)
}

pub static LET : &str = "let";
pub fn let_<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>, globally:bool)
                           -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"let");
    let csO = catch_prim!(engine.get_next_token() => (LET,cmd));
    let cs = match csO {
        None => file_end_prim!(LET,cmd),
        Some((t,_)) => t
    };
    match &cs.base {
        BaseToken::Char(_,CategoryCode::Active) | BaseToken::CS(_) => (),
        _ => throw!("Expected a control sequence" => cs)
    }
    catch_prim!(engine.skip_eq_char() => (LET,cmd));
    let cm = match catch_prim!(engine.get_next_token() => (LET,cmd)) {
        Some((t,_)) => t,
        None =>file_end_prim!(LET,cmd)
    };
    let cmd = match cm.base {
        BaseToken::Char(c,CategoryCode::Active) => engine.state.get_ac_command(&c).map(|c|c.clone().copy_with(&cmd)),
        BaseToken::CS(name) => engine.state.get_command(&name).map(|c|c.clone().copy_with(&cmd)),
        BaseToken::Char(c,cc) =>
            Some(Command::new(BaseCommand::Char{char:c,catcode:cc},Some(&cmd))),
    };
    debug_log!(debug=>"let: {} = {:?}",cs,cmd);
    engine.set_command_for_tk(cs,cmd,globally);
    Ok(())
}

pub fn long<ET:EngineType>(engine:&mut EngineMut<ET>, cmd: CommandSource<ET>, global_:bool, protected_:bool, long_:bool, outer_:bool)
                           -> Result<(),TeXError<ET>> {
    debug_log!(trace => "\\long");
    match catch_prim!(engine.get_next_stomach_command() => ("long",cmd)) {
        None => file_end_prim!("long",cmd),
        Some(c) => match c.command {
            BaseStomachCommand::Assignment {name:Some("global"),..} => global::<ET>(engine,cmd,global_,protected_,true,outer_),
            BaseStomachCommand::Assignment {name:Some("protected"),..} => super::etex::protected::<ET>(engine,cmd,global_,protected_,true,outer_),
            BaseStomachCommand::Assignment {name:Some("long"),..} => long::<ET>(engine,cmd,global_,protected_,true,outer_),
            BaseStomachCommand::Assignment {name:Some("outer"),..} => outer::<ET>(engine,cmd,global_,protected_,true,outer_),
            BaseStomachCommand::Assignment {name:Some("def"),..} => def::<ET>(engine,cmd,global_,protected_,true,outer_),
            BaseStomachCommand::Assignment {name:Some("edef"),..} => edef::<ET>(engine,cmd,global_,protected_,true,outer_),
            BaseStomachCommand::Assignment {name:Some("gdef"),..} => gdef::<ET>(engine,cmd,global_,protected_,true,outer_),
            BaseStomachCommand::Assignment {name:Some("xdef"),..} => xdef::<ET>(engine,cmd,global_,protected_,true,outer_),
            _ => throw!("Expected a macro definition after \\long" => cmd.cause)
        }
    }
}
pub static LOWERCASE : &str = "lowercase";
pub fn lowercase<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>)
                                -> Result<(),TeXError<ET>> {
    debug_log!(trace => "\\lowercase");
    engine.add_expansion(|engine,rs|{
        catch_prim!(engine.expand_until_group(&mut |engine,next| match &next.base {
            BaseToken::Char(c,cc) => {
                let nc = engine.state.get_lccode(c);
                Ok(rs.push(Token::new(BaseToken::Char(nc, *cc), None),engine.memory))
            }
            _ => Ok(rs.push(next,engine.memory))
        }) => (LOWERCASE,cmd));
        Ok(())
    })
}

pub static MATHCHARDEF : &str = "mathchardef";
pub fn mathchardef<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>, globally:bool)
                                  -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"mathchardef");
    let csO = catch_prim!(engine.get_next_token() => (MATHCHARDEF,cmd));
    let cs = match csO {
        None => file_end_prim!(MATHCHARDEF,cmd),
        Some((t,_)) => t
    };
    match &cs.base {
        BaseToken::Char(_,CategoryCode::Active) => (),
        BaseToken::CS(_) => (),
        _ => throw!("Command expected after \\mathchardef" => cs)
    }
    catch_prim!(engine.skip_eq_char() => (MATHCHARDEF,cmd));
    let i = catch_prim!(engine.get_int() => (MATHCHARDEF,cmd)).to_i64();
    if i < 0 {
        throw!("Invalid math char: {}",i => cmd.cause)
    }
    engine.set_command_for_tk(cs,Some(Command::new(BaseCommand::MathChar(i as u32),Some(&cmd))),globally);
    Ok(())
}

pub static MATHCODE : &str = "mathcode";
pub fn mathcode_assign<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>, global:bool)
                                      -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"Assigning math code");
    let c = catch_prim!(engine.get_char() => (MATHCODE,cmd));
    catch_prim!(engine.skip_eq_char() => (MATHCODE,cmd));
    let i = catch_prim!(engine.get_int() => (MATHCODE,cmd));
    debug_log!(debug=>"\\mathcode '{}' = {}",c.char_str(),i);
    engine.state.set_mathcode(c,i,global);
    Ok(())
}

pub fn mathcode_get<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>)
                                   -> Result<ET::Int,TeXError<ET>> {
    debug_log!(trace=>"Getting mathcode");
    let c = catch_prim!(engine.get_char() => (MATHCODE,cmd));
    let v = engine.state.get_mathcode(c);
    debug_log!(debug=>"\\mathcode '{}' == {}",c.char_str(),v);
    Ok(v)
}

pub static MEANING : &str = "meaning";
pub fn meaning<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>, f:TokenCont<ET>)
                              -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"meaning");
    let esc = engine.state.get_escapechar();
    match catch_prim!(engine.get_next_token() => (MEANING,cmd)) {
        None => file_end_prim!(MEANING,cmd),
        Some((_,false)) => {
            if let Some(c) = esc { f(engine,Token::new(BaseToken::Char(c,CategoryCode::Other),None))? }
            engine.string_to_tokens(RELAX.as_bytes(),f)
        }
        Some((t,_)) => {
            let n:ResolvedToken<ET> = resolve_token::<ET>(engine.state,t);
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
                    if let Some(c) = esc { f(engine,Token::new(BaseToken::Char(c,CategoryCode::Other),None))? }
                    return engine.string_to_tokens(name.as_bytes(),f)
                }
                BaseCommand::Unexpandable {name,..} => {
                    if let Some(c) = esc { f(engine,Token::new(BaseToken::Char(c,CategoryCode::Other),None))? }
                    return engine.string_to_tokens(name.as_bytes(),f)
                }
                BaseCommand::OpenBox {name,..} => {
                    if let Some(c) = esc { f(engine,Token::new(BaseToken::Char(c,CategoryCode::Other),None))? }
                    return engine.string_to_tokens(name.as_bytes(),f)
                }
                BaseCommand::Whatsit {name,..} => {
                    if let Some(c) = esc { f(engine,Token::new(BaseToken::Char(c,CategoryCode::Other),None))? }
                    return engine.string_to_tokens(name.as_bytes(),f)
                }
                BaseCommand::Assignment {name,..} => {
                    if let Some(c) = esc { f(engine,Token::new(BaseToken::Char(c,CategoryCode::Other),None))? }
                    return engine.string_to_tokens(name.as_bytes(),f)
                }
                BaseCommand::Relax => {
                    if let Some(c) = esc { f(engine,Token::new(BaseToken::Char(c,CategoryCode::Other),None))? }
                    return engine.string_to_tokens(RELAX.as_bytes(),f)
                }
                BaseCommand::Conditional {name,..} => {
                    if let Some(c) = esc { f(engine,Token::new(BaseToken::Char(c,CategoryCode::Other),None))? }
                    return engine.string_to_tokens(name.as_bytes(),f)
                }
                BaseCommand::FontCommand {name,..} => {
                    if let Some(c) = esc { f(engine,Token::new(BaseToken::Char(c,CategoryCode::Other),None))? }
                    return engine.string_to_tokens(name.as_bytes(),f)
                }
                BaseCommand::Int(ValueCommand::Value {name,..}) => {
                    if let Some(c) = esc { f(engine,Token::new(BaseToken::Char(c,CategoryCode::Other),None))? }
                    return engine.string_to_tokens(name.as_bytes(),f)
                }
                BaseCommand::Int(ValueCommand::Complex {name,..}) => {
                    if let Some(c) = esc { f(engine,Token::new(BaseToken::Char(c,CategoryCode::Other),None))? }
                    return engine.string_to_tokens(name.as_bytes(),f)
                }
                BaseCommand::Int(ValueCommand::Primitive(name)) => {
                    if let Some(c) = esc { f(engine,Token::new(BaseToken::Char(c,CategoryCode::Other),None))? }
                    return engine.string_to_tokens(name.as_bytes(),f)
                }
                BaseCommand::Dim(ValueCommand::Value {name,..}) => {
                    if let Some(c) = esc { f(engine,Token::new(BaseToken::Char(c,CategoryCode::Other),None))? }
                    return engine.string_to_tokens(name.as_bytes(),f)
                }
                BaseCommand::Dim(ValueCommand::Complex {name,..}) => {
                    if let Some(c) = esc { f(engine,Token::new(BaseToken::Char(c,CategoryCode::Other),None))? }
                    return engine.string_to_tokens(name.as_bytes(),f)
                }
                BaseCommand::Dim(ValueCommand::Primitive(name)) => {
                    if let Some(c) = esc { f(engine,Token::new(BaseToken::Char(c,CategoryCode::Other),None))? }
                    return engine.string_to_tokens(name.as_bytes(),f)
                }
                BaseCommand::Skip(ValueCommand::Value {name,..}) => {
                    if let Some(c) = esc { f(engine,Token::new(BaseToken::Char(c,CategoryCode::Other),None))? }
                    return engine.string_to_tokens(name.as_bytes(),f)
                }
                BaseCommand::Skip(ValueCommand::Complex {name,..}) => {
                    if let Some(c) = esc { f(engine,Token::new(BaseToken::Char(c,CategoryCode::Other),None))? }
                    return engine.string_to_tokens(name.as_bytes(),f)
                }
                BaseCommand::Skip(ValueCommand::Primitive(name)) => {
                    if let Some(c) = esc { f(engine,Token::new(BaseToken::Char(c,CategoryCode::Other),None))? }
                    return engine.string_to_tokens(name.as_bytes(),f)
                }
                BaseCommand::MuSkip(ValueCommand::Value {name,..}) => {
                    if let Some(c) = esc { f(engine,Token::new(BaseToken::Char(c,CategoryCode::Other),None))? }
                    return engine.string_to_tokens(name.as_bytes(),f)
                }
                BaseCommand::MuSkip(ValueCommand::Complex {name,..}) => {
                    if let Some(c) = esc { f(engine,Token::new(BaseToken::Char(c,CategoryCode::Other),None))? }
                    return engine.string_to_tokens(name.as_bytes(),f)
                }
                BaseCommand::MuSkip(ValueCommand::Primitive(name)) => {
                    if let Some(c) = esc { f(engine,Token::new(BaseToken::Char(c,CategoryCode::Other),None))? }
                    return engine.string_to_tokens(name.as_bytes(),f)
                }
                BaseCommand::Toks(ValueCommand::Value {name,..}) => {
                    if let Some(c) = esc { f(engine,Token::new(BaseToken::Char(c,CategoryCode::Other),None))? }
                    return engine.string_to_tokens(name.as_bytes(),f)
                }
                BaseCommand::Toks(ValueCommand::Complex {name,..}) => {
                    if let Some(c) = esc { f(engine,Token::new(BaseToken::Char(c,CategoryCode::Other),None))? }
                    return engine.string_to_tokens(name.as_bytes(),f)
                }
                BaseCommand::Toks(ValueCommand::Primitive(name)) => {
                    if let Some(c) = esc { f(engine,Token::new(BaseToken::Char(c,CategoryCode::Other),None))? }
                    return engine.string_to_tokens(name.as_bytes(),f)
                }
                BaseCommand::Int(ValueCommand::Register(u)) => {
                    if let Some(c) = esc { f(engine,Token::new(BaseToken::Char(c,CategoryCode::Other),None))? }
                    format!("count{}",u)
                }
                BaseCommand::Dim(ValueCommand::Register(u)) => {
                    if let Some(c) = esc { f(engine,Token::new(BaseToken::Char(c,CategoryCode::Other),None))? }
                    format!("dimen{}",u)
                }
                BaseCommand::Skip(ValueCommand::Register(u)) => {
                    if let Some(c) = esc { f(engine,Token::new(BaseToken::Char(c,CategoryCode::Other),None))? }
                    format!("skip{}",u)
                }
                BaseCommand::MuSkip(ValueCommand::Register(u)) => {
                    if let Some(c) = esc { f(engine,Token::new(BaseToken::Char(c,CategoryCode::Other),None))? }
                    format!("muskip{}",u)
                }
                BaseCommand::Toks(ValueCommand::Register(u)) => {
                    if let Some(c) = esc { f(engine,Token::new(BaseToken::Char(c,CategoryCode::Other),None))? }
                    format!("toks{}",u)
                }
                BaseCommand::CharDef(c) => {
                    if let Some(c) = esc { f(engine,Token::new(BaseToken::Char(c,CategoryCode::Other),None))? }
                    format!("char\"{:X}",c.to_usize())
                }
                BaseCommand::MathChar(c) => {
                    if let Some(c) = esc { f(engine,Token::new(BaseToken::Char(c,CategoryCode::Other),None))? }
                    format!("mathchar\"{:X}",c)
                }
                BaseCommand::Font(fnt) => {
                    format!("select font {}",fnt)
                }
                BaseCommand::None => {
                    return engine.string_to_tokens("undefined".as_bytes(),f)
                }
                BaseCommand::Def(d) => {
                    let cc = engine.state.get_catcode_scheme().clone();
                    if d.protected {
                        if let Some(c) = esc { f(engine,Token::new(BaseToken::Char(c,CategoryCode::Other),None))? }
                        engine.string_to_tokens("protected ".as_bytes(),f)?
                    }
                    if d.long {
                        if let Some(c) = esc { f(engine,Token::new(BaseToken::Char(c,CategoryCode::Other),None))? }
                        engine.string_to_tokens("long ".as_bytes(),f)?
                    }
                    if d.outer {
                        if let Some(c) = esc { f(engine,Token::new(BaseToken::Char(c,CategoryCode::Other),None))? }
                        engine.string_to_tokens("outer ".as_bytes(),f)?
                    }
                    engine.string_to_tokens("macro:".as_bytes(),f)?;
                    let mut i = 0;
                    for s in &d.signature {
                        match s {
                            ParamToken::Token(t) => {
                                token_to_chars::<ET,_>(t,esc.clone(),&cc,true,|t| f(engine,t))?;
                            }
                            ParamToken::Param => {
                                i += 1;
                                engine.string_to_tokens(format!("#{}", i).as_bytes(),f);
                            }
                        }
                    }
                    if d.endswithbrace { f(engine,Token::new(BaseToken::Char(ET::Char::from(b'#'),CategoryCode::Other),None))? }
                    f(engine,Token::new(BaseToken::Char(ET::Char::from(b'-'),CategoryCode::Other),None))?;
                    f(engine,Token::new(BaseToken::Char(ET::Char::from(b'>'),CategoryCode::Other),None))?;
                    for t in &d.replacement {
                        match t {
                            ExpToken::Token(t) => token_to_chars::<ET,_>(t,esc.clone(),&cc,true,&mut |t| f(engine,t))?,
                            ExpToken::ParamToken(t) => {
                                token_to_chars::<ET,_>(t,esc.clone(),&cc,true,|t| f(engine,t))?;
                                token_to_chars::<ET,_>(t,esc.clone(),&cc,true,|t| f(engine,t))?;
                            }
                            ExpToken::Param(t, i) => {
                                token_to_chars::<ET,_>(t,esc.clone(),&cc,true,|t| f(engine,t))?;
                                engine.string_to_tokens(i.to_string().as_bytes(),f)?
                            }
                        }
                    }
                    return Ok(())
                }
            };
            engine.string_to_tokens(string.as_bytes(),f)
        }
    }
}

pub static MESSAGE:&str = "message";
pub fn message<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>)
                              -> Result<(),TeXError<ET>> {
    debug_log!(debug=>"message");
    catch_prim!(engine.skip_whitespace() => (MESSAGE,cmd));
    let msg = catch_prim!(engine.get_braced_string() => (MESSAGE,cmd));
    (engine.state.outputs().message)(&msg);
    Ok(())
}

pub fn month<ET:EngineType>(state:&mut ET::State,cmd:CommandSource<ET>) -> Result<ET::Int,TeXError<ET>> {
    Ok(catch_prim!(ET::Int::from_i64(
        state.get_start_time().month() as i64
    ) => ("month",cmd)))
}

pub static MULTIPLY:&str = "multiply";
pub fn multiply<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>, global:bool)
                               -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"\\multiply");
    catch_prim!(engine.skip_whitespace() => (MULTIPLY,cmd));
    match catch_prim!(engine.get_next_unexpandable() => (MULTIPLY,cmd)) {
        None => file_end_prim!(MULTIPLY,cmd),
        Some(ncmd) => match ncmd.command {
            BaseCommand::Int(a) => {
                macro_rules! finish {
                    ($old:expr,$nv:ident => $set:expr) => {{
                        catch_prim!(engine.get_keyword("by") => (MULTIPLY,cmd));
                        let i = catch_prim!(engine.get_int() => (MULTIPLY,cmd));
                        let $nv = $old * i;
                        $set;
                        Ok(())
                    }}
                }
                match a {
                    ValueCommand::Register(u) => finish!(engine.state.get_int_register(u),nv => engine.state.set_int_register(u,nv,global)),
                    ValueCommand::Primitive(name) => finish!(engine.state.get_primitive_int(name),nv => engine.state.set_primitive_int(name,nv,global)),
                    ValueCommand::Complex {name,..} if name == COUNT => {
                        let int = catch_prim!(engine.get_int() => (MULTIPLY,cmd));
                        let u = match int.try_into() {
                            Ok(u) => u,
                            _ => throw!("Not a valid register: {}",int => cmd.cause)
                        };
                        finish!(engine.state.get_int_register(u),nv => engine.state.set_int_register(u,nv,global))
                    }
                    _ => throw!("Unexpected token after \\multiply" => cmd.cause)
                }
            }
            BaseCommand::Dim(a) => {
                macro_rules! finish {
                    ($old:expr,$nv:ident => $set:expr) => {{
                        catch_prim!(engine.get_keyword("by") => (MULTIPLY,cmd));
                        let i = catch_prim!(engine.get_int() => (MULTIPLY,cmd)).to_i64();
                        let $nv = $old.tex_mult(i as f64);
                        $set;
                        Ok(())
                    }}
                }
                match a {
                    ValueCommand::Register(u) => finish!(engine.state.get_dim_register(u),nv => engine.state.set_dim_register(u,nv,global)),
                    ValueCommand::Primitive(name) => finish!(engine.state.get_primitive_dim(name),nv => engine.state.set_primitive_dim(name,nv,global)),
                    ValueCommand::Complex {name,..} if name == DIMEN => {
                        let int = catch_prim!(engine.get_int() => (MULTIPLY,cmd));
                        let u = match int.try_into() {
                            Ok(u) => u,
                            _ => throw!("Not a valid register: {}",int => cmd.cause)
                        };
                        finish!(engine.state.get_dim_register(u),nv => engine.state.set_dim_register(u,nv,global))
                    }
                    _ => throw!("Unexpected token after \\multiply" => cmd.cause)
                }
            }
            BaseCommand::Skip(a) => {
                macro_rules! finish {
                    ($old:expr,$nv:ident => $set:expr) => {{
                        catch_prim!(engine.get_keyword("by") => (MULTIPLY,cmd));
                        let i = catch_prim!(engine.get_int() => (MULTIPLY,cmd)).to_i64();
                        let $nv = $old.tex_mult(i as f64);
                        $set;
                        Ok(())
                    }}
                }
                match a {
                    ValueCommand::Register(u) => finish!(engine.state.get_skip_register(u),nv => engine.state.set_skip_register(u,nv,global)),
                    ValueCommand::Primitive(name) => finish!(engine.state.get_primitive_skip(name),nv => engine.state.set_primitive_skip(name,nv,global)),
                    ValueCommand::Complex {name,..} if name == SKIP => {
                        let int = catch_prim!(engine.get_int() => (MULTIPLY,cmd));
                        let u = match int.try_into() {
                            Ok(u) => u,
                            _ => throw!("Not a valid register: {}",int => cmd.cause)
                        };
                        finish!(engine.state.get_skip_register(u),nv => engine.state.set_skip_register(u,nv,global))
                    }
                    _ => throw!("Unexpected token after \\multiply" => cmd.cause)
                }
            }
            BaseCommand::MuSkip(a) => {
                macro_rules! finish {
                    ($old:expr,$nv:ident => $set:expr) => {{
                        catch_prim!(engine.get_keyword("by") => (MULTIPLY,cmd));
                        let i = catch_prim!(engine.get_int() => (MULTIPLY,cmd)).to_i64();
                        let $nv = $old.tex_mult(i as f64);
                        $set;
                        Ok(())
                    }}
                }
                match a {
                    ValueCommand::Register(u) => finish!(engine.state.get_muskip_register(u),nv => engine.state.set_muskip_register(u,nv,global)),
                    ValueCommand::Primitive(name) => finish!(engine.state.get_primitive_muskip(name),nv => engine.state.set_primitive_muskip(name,nv,global)),
                    ValueCommand::Complex {name,..} if name == MUSKIP => {
                        let int = catch_prim!(engine.get_int() => (MULTIPLY,cmd));
                        let u = match int.try_into() {
                            Ok(u) => u,
                            _ => throw!("Not a valid register: {}",int => cmd.cause)
                        };
                        finish!(engine.state.get_muskip_register(u),nv => engine.state.set_muskip_register(u,nv,global))
                    }
                    _ => throw!("Unexpected token after \\multiply" => cmd.cause)
                }
            }
            o => throw!("expected register after \\multiply;got:{:?}",o => cmd.cause)
        }
    }
}

pub static MUSKIP : &str = "muskip";
pub fn muskip_assign<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>, global:bool)
                                    -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"Assigning \\muskip");
    let i = catch_prim!(engine.get_int() => (MUSKIP,cmd));
    let i:usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => throw!("Not a valid register: {}",i => cmd.cause)
    };
    catch_prim!(engine.skip_eq_char() => (MUSKIP,cmd));
    let v = catch_prim!(engine.get_muskip() => (MUSKIP,cmd));
    debug_log!(debug=>"\\muskip{} = {}",i,v);
    engine.state.set_muskip_register(i,v,global);
    Ok(())
}

pub fn muskip_get<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>) ->
                                                                                                           Result<MuSkip<ET::MuDim,ET::MuStretchShrinkDim>,TeXError<ET>> {
    debug_log!(trace=>"Getting \\muskip");
    let i = catch_prim!(engine.get_int() => (MUSKIP,cmd));
    let i:usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => throw!("Not a valid register: {}",i => cmd.cause)
    };
    let v = engine.state.get_muskip_register(i);
    debug_log!(debug=>"\\muskip{} == {}",i,v);
    Ok(v)
}

pub static MUSKIPDEF : &str = "muskipdef";
pub fn muskipdef<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>, global:bool)
                                -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"muskipdef");
    let name = catch_prim!(engine.get_control_sequence() => (MUSKIPDEF,cmd));
    catch_prim!(engine.set_relax(&name,&cmd,global) => (MUSKIPDEF,cmd));
    catch_prim!(engine.skip_eq_char() => (MUSKIPDEF,cmd));
    let num = catch_prim!(engine.get_int() => (MUSKIPDEF,cmd));
    if num.to_i64() < 0 {
        throw!("Invalid muskip register index: {}",num => cmd.cause)
    }
    let num = num.to_i64() as usize;
    let ret = Command::new(BaseCommand::MuSkip(ValueCommand::Register(num)),Some(&cmd));
    engine.set_command_for_tk(name,Some(ret),global);
    Ok(())
}

pub static NEWLINECHAR : &str = "newlinechar";
pub fn newlinechar_assign<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>, global:bool)
                                         -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"Assigning \\newlinechar");
    catch_prim!(engine.skip_eq_char() => (NEWLINECHAR,cmd));
    let i = catch_prim!(engine.get_int() => (NEWLINECHAR,cmd));
    let c = match i.to_i64() {
        -1 => None,
        i => match ET::Char::from_i64(i) {
            Some(c) => Some(c),
            None => throw!("Not a valid character: {}",i => cmd.cause)
        }
    };
    debug_log!(debug=>"\\newlinechar = {:?}",c.map(|c| c.char_str()));
    engine.state.set_newlinechar(c,global);
    Ok(())
}
pub fn newlinechar_get<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>)
                                      -> Result<ET::Int,TeXError<ET>> {
    debug_log!(trace=>"Getting \\newlinechar");
    let c = match engine.state.get_newlinechar() {
        None => -1,
        Some(c) => c.to_usize() as i64
    };
    debug_log!(debug=>"\\newlinechar == {}",c);
    Ok(ET::Int::from_i64::<ET>(c).unwrap())
}

pub static NOEXPAND: &str = "noexpand";
/// invariant: adds token as nonexpanded to the gullet iff the original token was expandable
/// in the first place
pub fn noexpand<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>, f:TokenCont<ET>)
                               -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"\\noexpand");
    match catch_prim!(engine.get_next_token() => (NOEXPAND,cmd)) {
        None => file_end_prim!(NOEXPAND,cmd),
        Some((t,_)) => {
            let res = resolve_token::<ET>(engine.state,t);
            match res.command {
                BaseCommand::Def(_) => engine.mouth.push_noexpand(res.source.cause,engine.memory),
                BaseCommand::Expandable {..} => engine.mouth.push_noexpand(res.source.cause,engine.memory),
                BaseCommand::Conditional {..} => engine.mouth.push_noexpand(res.source.cause,engine.memory),
                _ => f(engine,res.source.cause)?
            }
        }
    }
    Ok(())
}

pub static NUMBER: &str = "number";
pub fn number<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>, f:TokenCont<ET>)
                             -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"\\number");
    let num = catch_prim!(engine.get_int() => (NUMBER,cmd));
    engine.string_to_tokens(num.to_i64().to_string().as_bytes(),f)
}

pub static OPENIN: &str = "openin";
pub fn openin<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>)
                             -> Result<(), TeXError<ET>> {
    debug_log!(trace=>"\\openin");
    let i = catch_prim!(engine.get_int() => (OPENIN,cmd));
    let i : usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => throw!("Invalid file number: {}",i => cmd.cause)
    };
    catch_prim!(engine.skip_eq_char() => (OPENIN,cmd));
    let filename = catch_prim!(engine.get_string() => (OPENIN,cmd)).to_string();
    let f = engine.state.filesystem().get(&filename);
    engine.state.file_openin(i,f); // TODO error?
    Ok(())
}

pub static OPENOUT: &str = "openout";
pub fn openout<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>)
                              -> Result<Whatsit<ET>, TeXError<ET>> {
    debug_log!(trace=>"\\openout");
    let i = catch_prim!(engine.get_int() => (OPENOUT,cmd));
    let i : usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => throw!("Invalid file number: {}",i => cmd.cause)
    };
    catch_prim!(engine.skip_eq_char() => (OPENOUT,cmd));
    let filename = catch_prim!(engine.get_string() => (OPENOUT,cmd)).to_string();
    let apply = Box::new(move |e:&mut EngineMut<ET>| {
        let f = e.state.filesystem().get(&filename);
        e.state.file_openout(i,f); // TODO error?
        Ok(())
    });
    Ok(Whatsit::new(apply))
}

pub static OR : &str = "or";
pub fn or<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>)
                         -> Result<(),TeXError<ET>> {
    match engine.gullet.current_conditional() {
        (Some(ConditionalBranch::Case(_,_)),i) =>
            catch_prim!(crate::engine::gullet::methods::else_loop::<ET>(engine.gullet,engine.mouth,engine.state,engine.memory,IFCASE,i,true) => (OR,cmd)),
        _ => throw!("Not in an \\ifcase" => cmd.cause)
    }
    Ok(())
}

pub static OUTER: &str = "outer";
pub fn outer<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>, global_:bool, protected_:bool, long_:bool, outer_:bool)
                            -> Result<(),TeXError<ET>> {
    debug_log!(trace => "\\outer");
    match catch_prim!(engine.get_next_stomach_command() => (OUTER,cmd)) {
        None => file_end_prim!("outer",cmd),
        Some(c) => match c.command {
            BaseStomachCommand::Assignment {name:Some("global"),..} => global::<ET>(engine,cmd,global_,protected_,long_,true),
            BaseStomachCommand::Assignment {name:Some("protected"),..} => super::etex::protected::<ET>(engine,cmd,global_,protected_,long_,true),
            BaseStomachCommand::Assignment {name:Some("long"),..} => long::<ET>(engine,cmd,global_,protected_,long_,true),
            BaseStomachCommand::Assignment {name:Some("outer"),..} => outer::<ET>(engine,cmd,global_,protected_,long_,true),
            BaseStomachCommand::Assignment {name:Some("def"),..} => def::<ET>(engine,cmd,global_,protected_,long_,true),
            BaseStomachCommand::Assignment {name:Some("edef"),..} => edef::<ET>(engine,cmd,global_,protected_,long_,true),
            BaseStomachCommand::Assignment {name:Some("gdef"),..} => gdef::<ET>(engine,cmd,global_,protected_,long_,true),
            BaseStomachCommand::Assignment {name:Some("xdef"),..} => xdef::<ET>(engine,cmd,global_,protected_,long_,true),
            _ => throw!("Expected a macro definition after \\outer" => cmd.cause)
        }
    }
}

pub static PAR: &str = "par";
pub fn par<ET:EngineType>(engine:&mut EngineMut<ET>, _cmd:CommandSource<ET>) -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"par");
    if engine.state.mode().is_vertical() {Ok(())} else {
        todo!("par in horizontal mode")
    }
}

pub static PATTERNS: &str = "patterns";
pub fn patterns<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>)
                               -> Result<(), TeXError<ET>> {
    debug_log!(trace=>"\\patterns");
    // TODO
    catch_prim!(engine.get_argument(&mut |_,_| Ok(())) => (PATTERNS,cmd));
    Ok(())
}

pub static READ: &str = "read";
pub fn read<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>, globally:bool)
                           -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"read");
    let i = catch_prim!(engine.get_int() => (READ,cmd));
    let i : usize = match i.try_into() {
        Ok(i) => i,
        Err(_) => throw!("Invalid file number" => cmd.cause)
    };
    let file = match engine.state.get_open_in_file(i) {
        None => throw!("File {} not open for reading",i),
        Some(f) => f
    };
    if !catch_prim!(engine.get_keyword("to") => (READ,cmd)) {
        return throw!("Expected 'to' after \\read" => cmd.cause)
    }
    let newcmd = catch_prim!(engine.get_control_sequence() => (READ,cmd));
    let mut ret = vec!();
    catch_prim!(file.read::<ET,_>(engine.state.get_catcode_scheme(),engine.state.get_endlinechar(),|t| ret.push(t)) => (READ,cmd));
    debug_log!(trace=>"read: {} = {}",newcmd,TokenList(&ret));
    if ret.is_empty() {
        match engine.state.get_endlinechar() {
            None => (),
            Some(c) => ret.push(Token::new(BaseToken::Char(c,*engine.state.get_catcode_scheme().get(&c)),None))
        }
    }
    let def = Command::new(BaseCommand::Def(DefI::simple(ret)),Some(&cmd));
    engine.set_command_for_tk(newcmd,Some(def),globally);
    Ok(())
}

pub static RELAX: &str = "relax";

pub static ROMANNUMERAL: &str = "romannumeral";
pub fn romannumeral<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>, f:TokenCont<ET>)
                                   -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"romannumeral");
    let mut num = catch_prim!(engine.get_int() => (ROMANNUMERAL,cmd)).to_i64();
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
    engine.string_to_tokens(&ret,f)
}

pub static SETBOX: &str = "setbox";
pub fn setbox<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>, global:bool)
                             -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"\\setbox");
    let i = catch_prim!(engine.get_int() => (SETBOX,cmd)).to_i64();
    if i < 0  { throw!("Invalid box number: {}",i => cmd.cause) }
    catch_prim!(engine.skip_eq_char() => (SETBOX,cmd));
    match catch_prim!(engine.get_next_unexpandable() => (SETBOX,cmd)) {
        None => file_end_prim!(SETBOX,cmd),
        Some(c) => match c.command {
            BaseCommand::OpenBox {name,mode,apply} => {
                let f = catch_prim!(apply(engine,c.source) => (SETBOX,cmd));
                engine.state.open_box(OpenBox::Box {list:vec!(),mode,on_close:Ptr::new(move |e,v| {
                    let bx = match f(e,v) {
                        Some(r) => {r}
                        None => {todo!("make void box")}
                    };
                    e.state.set_box_register(i as usize,bx,global);
                    None
                })});
                Ok(())
            }
            _ => throw!("Box expected: {}",c.source.cause)
        }
    }
}

pub static SFCODE: &str = "sfcode";
pub fn sfcode_assign<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>, global:bool)
                                    -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"Assigning space factor code");
    let c = catch_prim!(engine.get_char() => (SFCODE,cmd));
    catch_prim!(engine.skip_eq_char() => (SFCODE,cmd));
    let v = catch_prim!(engine.get_int() => (SFCODE,cmd));
    debug_log!(debug=>"\\sfcode '{}' = {}",c.char_str(),v);
    engine.state.set_sfcode(c,v,global);
    Ok(())
}
pub fn sfcode_get<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>)
                                 -> Result<ET::Int,TeXError<ET>> {
    debug_log!(trace=>"Getting space factor code");
    let c = catch_prim!(engine.get_char() => (SFCODE,cmd));
    let v = engine.state.get_sfcode(&c);
    debug_log!(debug=>"\\sfcode '{}' == {}",c.char_str(),v);
    Ok(v)
}

pub static SKEWCHAR: &str = "skewchar";
pub fn skewchar_assign<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>, global:bool)
                                      -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"Assigning \\kewchar");
    let mut font = catch_prim!(engine.get_font() => (SKEWCHAR,cmd));
    catch_prim!(engine.skip_eq_char() => (SKEWCHAR,cmd));
    let i = catch_prim!(engine.get_int() => (SKEWCHAR,cmd)).to_i64();
    debug_log!(debug=>"\\skewchar\\{:?} = {:?}",font,i);
    font.set_skewchar(i);
    Ok(())
}
pub fn skewchar_get<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>) -> Result<ET::Int,TeXError<ET>> {
    debug_log!(trace=>"Getting \\skewchar");
    let font = catch_prim!(engine.get_font() => (SKEWCHAR,cmd));
    debug_log!(debug=>"\\skewchar == {:?}",font.get_skewchar());
    Ok(catch_prim!(ET::Int::from_i64::<ET>(font.get_skewchar()) => (SKEWCHAR,cmd)))
}

pub static SKIP : &str = "skip";
pub fn skip_assign<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>, global:bool)
                                  -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"Assigning \\skip");
    let i = catch_prim!(engine.get_int() => (SKIP,cmd));
    let i:usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => throw!("Not a valid register: {}",i => cmd.cause)
    };
    catch_prim!(engine.skip_eq_char() => (SKIP,cmd));
    let v = catch_prim!(engine.get_skip() => (SKIP,cmd));
    debug_log!(debug=>"\\skip{} = {}",i,v);
    engine.state.set_skip_register(i,v,global);
    Ok(())
}

pub fn skip_get<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>)
                               -> Result<Skip<ET::SkipDim>,TeXError<ET>> {
    debug_log!(trace=>"Getting \\skip");
    let i = catch_prim!(engine.get_int() => (SKIP,cmd));
    let i:usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => throw!("Not a valid register: {}",i => cmd.cause)
    };
    let v = engine.state.get_skip_register(i);
    debug_log!(debug=>"\\skip{} == {}",i,v);
    Ok(v)
}

pub static SKIPDEF : &str = "skipdef";
pub fn skipdef<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>, global:bool)
                              -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"skipdef");
    let name = catch_prim!(engine.get_control_sequence() => (SKIPDEF,cmd));
    catch_prim!(engine.set_relax(&name,&cmd,global) => (SKIPDEF,cmd));
    catch_prim!(engine.skip_eq_char() => (SKIPDEF,cmd));
    let num = catch_prim!(engine.get_int() => (SKIPDEF,cmd));
    if num.to_i64() < 0 {
        throw!("Invalid skip register index: {}",num => cmd.cause)
    }
    let num = num.to_i64() as usize;
    let ret = Command::new(BaseCommand::Skip(ValueCommand::Register(num)),Some(&cmd));
    engine.set_command_for_tk(name,Some(ret),global);
    Ok(())
}

pub static STRING: &str = "string";
pub fn string<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>, f:TokenCont<ET>)
                             -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"string");
    match catch_prim!(engine.get_next_token() => (STRING,cmd)) {
        None => file_end_prim!("string",cmd),
        Some((t,_)) => {
            let esc = engine.state.get_escapechar();
            let cat = engine.state.get_catcode_scheme().clone();
            token_to_chars(&t,esc,&cat,false,&mut |t| f(engine,t))?
        }
    }
    Ok(())
}

pub static THE : &str = "the";
pub fn the<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>, f:TokenCont<ET>)
                          -> Result<(),TeXError<ET>> {
    debug_log!(trace => "\\the");
    match catch_prim!(engine.get_next_unexpandable() => (THE,cmd)) {
        Some(c) => match c.command {
            BaseCommand::Int(ass) => {
                let val = ass.get(engine,c.source)?;
                engine.string_to_tokens(format!("{}",val).as_bytes(),f)
            }
            BaseCommand::Dim(ass) => {
                let val = ass.get(engine,c.source)?;
                engine.string_to_tokens(format!("{}",val).as_bytes(),f)
            }
            BaseCommand::Skip(ass) => {
                let val = ass.get(engine,c.source)?;
                engine.string_to_tokens(format!("{}",val).as_bytes(),f)
            }
            BaseCommand::MuSkip(ass) => {
                let val = ass.get(engine,c.source)?;
                engine.string_to_tokens(format!("{}",val).as_bytes(),f)
            }
            BaseCommand::Toks(ass) => {
                for t in ass.get(engine, c.source)? { f(engine, t)? }
                Ok(())
            }
            BaseCommand::CharDef(c) => engine.string_to_tokens(c.to_usize().to_string().as_bytes(),f),
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
pub fn toks_assign<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>, global:bool)
                                  -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"Assigning \\toks");
    let i = catch_prim!(engine.get_int() => (TOKS,cmd));
    let i:usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => throw!("Not a valid register: {}",i => cmd.cause)
    };
    catch_prim!(engine.skip_eq_char() => (TOKS,cmd));
    let mut v = vec!();
    catch_prim!(engine.get_group(&mut |_,t| Ok(v.push(t))) => (TOKS,cmd));
    debug_log!(debug=>"\\toks{} = {}",i,TokenList(&v));
    engine.state.set_toks_register(i,v,global);
    Ok(())
}
pub fn toks_get<'a,ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>)
                                  -> Result<Vec<Token<ET>>,TeXError<ET>> {
    let i = catch_prim!(engine.get_int() => (TOKS,cmd));
    let i:usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => throw!("Not a valid register: {}",i => cmd.cause)
    };
    Ok(engine.state.get_toks_register(i).clone())
}

pub static TOKSDEF : &str = "toksdef";
pub fn toksdef<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>, global:bool)
                              -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"toksdef");
    let name = catch_prim!(engine.get_control_sequence() => (TOKSDEF,cmd));
    catch_prim!(engine.set_relax(&name,&cmd,global) => (TOKSDEF,cmd));
    catch_prim!(engine.skip_eq_char() => (TOKSDEF,cmd));
    let num = catch_prim!(engine.get_int() => (TOKSDEF,cmd));
    if num.to_i64() < 0 {
        throw!("Invalid token register index: {}",num => cmd.cause)
    }
    let num = num.to_i64() as usize;
    let ret = Command::new(BaseCommand::Toks(ValueCommand::Register(num)),Some(&cmd));
    engine.set_command_for_tk(name,Some(ret),global);
    Ok(())
}

pub static UCCODE : &str = "uccode";
pub fn uccode_assign<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>, global:bool)
                                    -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"Assigning upper case character");
    let c = catch_prim!(engine.get_char() => (UCCODE,cmd));
    catch_prim!(engine.skip_eq_char() => (UCCODE,cmd));
    let lc = catch_prim!(engine.get_char() => (UCCODE,cmd));
    debug_log!(debug=>"\\uccode '{}' = {}",c.char_str(),lc.char_str());
    engine.state.set_uccode(c,lc,global);
    Ok(())
}

pub fn uccode_get<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>)
                                 -> Result<ET::Int,TeXError<ET>> {
    debug_log!(trace=>"Getting upper case character");
    let c = catch_prim!(engine.get_char() => (UCCODE,cmd));
    let v = catch_prim!(ET::Int::from_i64(engine.state.get_uccode(&c).to_usize() as i64) => (UCCODE,cmd));
    debug_log!(debug=>"\\uccode '{}' == {}",c.char_str(),v);
    Ok(v)
}

pub static UPPERCASE: &str = "uppercase";
pub fn uppercase<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>)
                                -> Result<(),TeXError<ET>> {
    debug_log!(trace => "\\uppercase");
    engine.add_expansion(|engine,rs| {
        catch_prim!(engine.expand_until_group(&mut |engine,next| match &next.base {
            BaseToken::Char(c,cc) => {
                let nc = engine.state.get_uccode(c);
                Ok(rs.push(Token::new(BaseToken::Char(nc, *cc), None),engine.memory))
            }
            _ => Ok(rs.push(next,engine.memory))
        }) => (UPPERCASE,cmd));
            Ok(())
    })
}


pub static VRULE: &str = "vrule";
pub fn vrule<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>)
                            -> Result<(),TeXError<ET>> {
    debug_log!(trace => "\\vrule");
    let mut width = None;
    let mut height = None;
    let mut depth = None;
    loop {
        match catch_prim!(engine.get_keywords(vec!("width","height","depth")) => (VRULE,cmd)) {
            None => break,
            Some(s) => {
                let val = catch_prim!(engine.get_dim() => (VRULE,cmd));
                match s {
                    "width" => width = Some(val),
                    "height" => height = Some(val),
                    "depth" => depth = Some(val),
                    _ => unsafe{unreachable_unchecked()}
                }
            }
        }
    }
    engine.state.push_node(SimpleNode::Rule {
        width,height,depth,axis:HorV::Vertical
    }.as_node());
    Ok(())
}

pub static WRITE: &str = "write";
pub fn write<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>)
                            -> Result<Whatsit<ET>, TeXError<ET>> {
    debug_log!(trace=>"\\write");
    let i = catch_prim!(engine.get_int() => (WRITE,cmd));
    let i = i.to_i64();
    let mut tks = vec!();
    catch_prim!(engine.get_group(&mut |_,t| Ok(tks.push(t))) => (WRITE,cmd));

    let apply = Box::new(move |engine:&mut EngineMut<ET>| {
        tks.push(Token::new(BaseToken::Char(ET::Char::from(b'}'),CategoryCode::EndGroup),None));
        tks.insert(0,Token::new(BaseToken::Char(ET::Char::from(b'{'),CategoryCode::BeginGroup),None));
        engine.with_mouth(tks,|engine| {
            let string = catch_prim!(engine.get_braced_string() => (WRITE,cmd));
            if i == 18 {
                (engine.state.outputs().write_18)(&string)
            }
            else if i == 17 {
                (engine.state.outputs().write_17)(&string)
            }
            else if i < 0 {
                (engine.state.outputs().write_neg1)(&string)
            }
            else {
                match engine.state.get_open_out_file(i as usize) {
                    None =>
                        (engine.state.outputs().write_other)(&string),
                    Some(f) => f.write(&string)
                }
            }
            Ok(())
        })
    });
    Ok(Whatsit::new(apply))
}

pub fn xdef<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>, global:bool, protected:bool, long:bool, outer:bool)
                           -> Result<(),TeXError<ET>> {
    edef::<ET>(engine,cmd,true,protected,long,outer)
}

pub fn year<ET:EngineType>(state:&mut ET::State,cmd:CommandSource<ET>) -> Result<ET::Int,TeXError<ET>> {
    Ok(catch_prim!(ET::Int::from_i64(
        state.get_start_time().year() as i64
    ) => ("year",cmd)))
}

// --------------------------------------------------------------------------------------------------


pub fn initialize_tex_primitives<ET:EngineType>(engine:&mut EngineMut<ET>) {
    register_skip_assign!(abovedisplayshortskip,engine);
    register_skip_assign!(abovedisplayskip,engine);
    register_int_assign!(adjdemerits,engine);
    register_assign!(advance,engine,(e,cmd,global) =>advance::<ET>(e,cmd,global));
    register_unexpandable!(afterassignment,engine,false,(e,cmd) =>afterassignment::<ET>(e,cmd));
    register_skip_assign!(baselineskip,engine);
    register_unexpandable!(begingroup,engine,false,(e,_) =>begingroup::<ET>(e.state));
    register_skip_assign!(belowdisplayskip,engine);
    register_skip_assign!(belowdisplayshortskip,engine);
    register_int_assign!(binoppenalty,engine);
    register_dim_assign!(boxmaxdepth,engine);
    register_int_assign!(brokenpenalty,engine);
    register_value_assign_int!(catcode,engine);
    register_assign!(chardef,engine,(e,cmd,global) =>chardef::<ET>(e,cmd,global));
    register_unexpandable!(closein,engine,false,(e,cmd) =>closein::<ET>(e,cmd));
    register_whatsit!(closeout,engine,(e,cmd) =>closeout::<ET>(e,cmd));
    register_int_assign!(clubpenalty,engine);
    register_value_assign_int!(count,engine);
    register_assign!(countdef,engine,(e,cmd,global) =>countdef::<ET>(e,cmd,global));
    register_expandable!(csname,engine,(e,cmd,f) =>csname::<ET>(e,cmd,f));
    register_int!(day,engine,(e,c) => day::<ET>(e.state,c));
    register_assign!(def,engine,(e,cmd,global) =>def::<ET>(e,cmd,global,false,false,false));
    register_int_assign!(defaulthyphenchar,engine);
    register_int_assign!(defaultskewchar,engine);
    register_int_assign!(delimiterfactor,engine);
    register_dim_assign!(delimitershortfall,engine);
    register_value_assign_int!(delcode,engine);
    register_value_assign_dim!(dimen,engine);
    register_assign!(dimendef,engine,(e,cmd,global) =>dimendef::<ET>(e,cmd,global));
    register_dim_assign!(displayindent,engine);
    register_int_assign!(displaywidowpenalty,engine);
    register_dim_assign!(displaywidth,engine);
    register_assign!(divide,engine,(e,cmd,global) =>divide::<ET>(e,cmd,global));
    register_int_assign!(doublehyphendemerits,engine);
    register_unexpandable!(dump,engine,false,(_,cmd) =>dump::<ET>());
    register_assign!(edef,engine,(e,cmd,global) =>edef::<ET>(e,cmd,global,false,false,false));
    register_expandable!(else,engine,(e,cmd,f) =>else_::<ET>(e,cmd));
    register_dim_assign!(emergencystretch,engine);
    register_unexpandable!(end,engine,false,(_,_) =>end::<ET>());
    register_unexpandable!(endcsname,engine,false,(_,cmd) =>endcsname::<ET>(cmd));
    register_expandable!(endinput,engine,(e,c,f) => endinput::<ET>(e,c));
    register_unexpandable!(endgroup,engine,false,(e,cmd) =>endgroup::<ET>(e,cmd));
    register_value_assign_int!(endlinechar,engine);
    register_tok_assign!(errhelp,engine);

    let em = Some(Command::new(BaseCommand::Unexpandable {
        name:ERRMESSAGE,
        apply:|e,cmd| errmessage::<ET>(e,cmd),
        starts_paragraph:false
    },None));
    engine.state.set_command(ET::Char::from_str("errmessage"),em.clone(),true);
    engine.state.set_command(ET::Char::from_str("LaTeX3 error:"),em,true);

    register_int_assign!(errorcontextlines,engine);
    register_unexpandable!(errorstopmode,engine,false,(_,cmd) =>errorstopmode::<ET>());
    register_value_assign_int!(escapechar,engine);
    register_int_assign!(exhyphenpenalty,engine);
    register_expandable!(expandafter,engine,(e,c,f) => expandafter::<ET>(e,c,f));
    register_tok_assign!(everypar,engine);
    register_tok_assign!(everymath,engine);
    register_tok_assign!(everydisplay,engine);
    register_tok_assign!(everyhbox,engine);
    register_tok_assign!(everyvbox,engine);
    register_tok_assign!(everyjob,engine);
    register_tok_assign!(everycr,engine);
    register_int_assign!(fam,engine);
    register_expandable!(fi,engine,(e,cmd,f) =>fi::<ET>(e,cmd));
    register_int_assign!(finalhyphendemerits,engine);
    register_int_assign!(floatingpenalty,engine);
    register_value_assign_font!(font,engine);
    register_value_assign_dim!(fontdimen,engine);
    register_assign!(futurelet,engine,(e,cmd,global) =>futurelet::<ET>(e,cmd,global));
    register_assign!(gdef,engine,(e,cmd,global) =>gdef::<ET>(e,cmd,global,false,false,false));
    register_assign!(global,engine,(e,cmd,g) =>global::<ET>(e,cmd,g,false,false,false));
    register_int_assign!(globaldefs,engine);
    register_int_assign!(hangafter,engine);
    register_dim_assign!(hangindent,engine);
    register_int_assign!(hbadness,engine);
    register_open_box!(hbox,engine,BoxMode::H,(e,cmd) =>hbox::<ET>(e,cmd));
    register_dim_assign!(hfuzz,engine);
    register_dim_assign!(hoffset,engine);
    register_int_assign!(holdinginserts,engine);
    register_unexpandable!(hrule,engine,true,(e,cmd) =>hrule::<ET>(e,cmd));
    register_dim_assign!(hsize,engine);
    register_unexpandable!(hyphenation,engine,false,(e,cmd) =>hyphenation::<ET>(e,cmd));
    register_value_assign_int!(hyphenchar,engine);
    register_int_assign!(hyphenpenalty,engine);
    register_conditional!(if,engine,(e,cmd) =>if_::<ET>(e,cmd));
    register_conditional!(ifcase,engine,(_,cmd) =>ifcase::<ET>());
    register_conditional!(ifcat,engine,(e,cmd) =>ifcat::<ET>(e,cmd));
    register_conditional!(ifdim,engine,(e,cmd) =>ifdim::<ET>(e,cmd));
    register_conditional!(ifeof,engine,(e,cmd) =>ifeof::<ET>(e,cmd));
    register_conditional!(iffalse,engine,(_,_) => Ok(false));
    register_conditional!(ifhbox,engine,(e,cmd) => todo!("ifhbox"));
    register_conditional!(ifhmode,engine,(e,cmd) =>ifhmode::<ET>(e,cmd));
    register_conditional!(ifinner,engine,(e,cmd) =>ifinner::<ET>(e,cmd));
    register_conditional!(ifmmode,engine,(e,cmd) =>ifmmode::<ET>(e,cmd));
    register_conditional!(ifnum,engine,(e,cmd) =>ifnum::<ET>(e,cmd));
    register_conditional!(ifodd,engine,(e,cmd) =>ifodd::<ET>(e,cmd));
    register_conditional!(iftrue,engine,(_,_) => Ok(true));
    register_conditional!(ifvbox,engine,(e,cmd) =>todo!("ifvbox"));
    register_conditional!(ifvmode,engine,(e,cmd) =>ifvmode::<ET>(e,cmd));
    register_conditional!(ifvoid,engine,(e,cmd) =>todo!("ifvoid"));
    register_conditional!(ifx,engine,(e,cmd) =>ifx::<ET>(e,cmd));
    register_unexpandable!(immediate,engine,false,(e,cmd) =>immediate::<ET>(e,cmd));
    register_unexpandable!(ignorespaces,engine,false,(e,cmd) => ignorespaces::<ET>(e,cmd));
    register_expandable!(input,engine,(e,cmd,f) =>input::<ET>(e,cmd));
    register_int!(inputlineno,engine,(e,c) => inputlineno::<ET>(e,c));
    register_int_assign!(interlinepenalty,engine);
    register_expandable!(jobname,engine,(e,_,f) =>jobname::<ET>(e,f));
    register_int_assign!(language,engine);
    register_value_assign_int!(lccode,engine);
    register_int_assign!(lefthyphenmin,engine);
    register_skip_assign!(leftskip,engine);
    register_assign!(let,engine,(e,cmd,global) =>let_::<ET>(e,cmd,global));
    register_int_assign!(linepenalty,engine);
    register_skip_assign!(lineskip,engine);
    register_dim_assign!(lineskiplimit,engine);
    register_assign!(long,engine,(e,cmd,g) =>long::<ET>(e,cmd,g,false,false,false));
    register_unexpandable!(lowercase,engine,false,(e,cmd) =>lowercase::<ET>(e,cmd));
    register_int_assign!(looseness,engine);
    register_int_assign!(mag,engine);
    register_int_assign!(maxdeadcycles,engine);
    register_dim_assign!(maxdepth,engine);
    register_assign!(mathchardef,engine,(e,cmd,global) =>mathchardef::<ET>(e,cmd,global));
    register_value_assign_int!(mathcode,engine);
    register_dim_assign!(mathsurround,engine);
    register_expandable!(meaning,engine,(e,c,f) => meaning::<ET>(e,c,f));
    register_unexpandable!(message,engine,false,(e,cmd) =>message::<ET>(e,cmd));
    register_int!(month,engine,(e,c) => month::<ET>(e.state,c));
    register_assign!(multiply,engine,(e,cmd,global) =>multiply::<ET>(e,cmd,global));
    register_value_assign_muskip!(muskip,engine);
    register_assign!(muskipdef,engine,(e,cmd,global) =>muskipdef::<ET>(e,cmd,global));
    register_value_assign_int!(newlinechar,engine);
    register_expandable!(noexpand,engine,(e,c,f) => noexpand::<ET>(e,c,f));
    register_dim_assign!(nulldelimiterspace,engine);
    engine.state.set_command(ET::Char::from_str("nullfont"), Some(Command::new(
        BaseCommand::Font(engine.state.fontstore().null())
        ,None)), true);
    register_expandable!(number,engine,(e,c,f) => number::<ET>(e,c,f));
    register_unexpandable!(openin,engine,false,(e,cmd) =>openin::<ET>(e,cmd));
    register_whatsit!(openout,engine,(e,cmd) =>openout::<ET>(e,cmd));
    register_expandable!(or,engine,(e,c,f) => or::<ET>(e,c));
    register_assign!(outer,engine,(e,cmd,g) =>outer::<ET>(e,cmd,g,false,false,false));
    register_tok_assign!(output,engine);
    register_int_assign!(outputpenalty,engine);
    register_dim_assign!(overfullrule,engine);

    engine.state.set_command(ET::Char::par_token(),Some(Command::new(BaseCommand::Unexpandable {
        name:"par",
        apply:|e,cmd| par::<ET>(e,cmd),
        starts_paragraph:false
    },None)),true);
    register_skip_assign!(parfillskip,engine);
    register_dim_assign!(parindent,engine);
    register_skip_assign!(parskip,engine);
    register_unexpandable!(patterns,engine,false,(e,cmd) =>patterns::<ET>(e,cmd));
    register_int_assign!(pausing,engine);
    register_int_assign!(postdisplaypenalty,engine);
    register_int_assign!(predisplaypenalty,engine);
    register_dim_assign!(predisplaysize,engine);
    register_int_assign!(relpenalty,engine);
    register_int_assign!(righthyphenmin,engine);
    register_int_assign!(pretolerance,engine);
    register_assign!(read,engine,(e,cmd,global) =>read::<ET>(e,cmd,global));
    engine.state.set_command(ET::Char::relax_token(), Some(Command::new(BaseCommand::Relax,None)), true);
    register_int_assign!(relpenalty,engine);
    register_skip_assign!(rightskip,engine);
    register_expandable!(romannumeral,engine,(e,c,f) => romannumeral::<ET>(e,c,f));
    register_dim_assign!(scriptspace,engine);
    register_assign!(setbox,engine,(e,cmd,global) =>setbox::<ET>(e,cmd,global));
    register_value_assign_int!(sfcode,engine);
    register_int_assign!(showboxbreadth,engine);
    register_int_assign!(showboxdepth,engine);
    register_value_assign_int!(skewchar,engine);
    register_value_assign_skip!(skip,engine);
    register_assign!(skipdef,engine,(e,cmd,global) =>skipdef::<ET>(e,cmd,global));
    register_skip_assign!(spaceskip,engine);
    register_dim_assign!(splitmaxdepth,engine);
    register_skip_assign!(splittopskip,engine);
    register_expandable!(string,engine,(e,c,f) => string::<ET>(e,c,f));
    register_skip_assign!(tabskip,engine);
    register_expandable!(the,engine,(e,c,f) => the::<ET>(e,c,f));
    register_int!(time,engine,(e,c) => time::<ET>(e.state,c));
    register_value_assign_toks!(toks,engine);
    register_assign!(toksdef,engine,(e,cmd,global) =>toksdef::<ET>(e,cmd,global));
    register_int_assign!(tolerance,engine);
    register_skip_assign!(topskip,engine);
    register_int_assign!(tracingcommands,engine);
    register_int_assign!(tracinglostchars,engine);
    register_int_assign!(tracingmacros,engine);
    register_int_assign!(tracingonline,engine);
    register_int_assign!(tracingoutput,engine);
    register_int_assign!(tracingpages,engine);
    register_int_assign!(tracingparagraphs,engine);
    register_int_assign!(tracingrestores,engine);
    register_int_assign!(tracingstats,engine);
    register_value_assign_int!(uccode,engine);
    register_int_assign!(uchyph,engine);
    register_unexpandable!(uppercase,engine,false,(e,cmd) =>uppercase::<ET>(e,cmd));
    register_int_assign!(vbadness,engine);
    register_dim_assign!(vfuzz,engine);
    register_dim_assign!(voffset,engine);
    register_unexpandable!(vrule,engine,true,(e,cmd) =>vrule::<ET>(e,cmd));
    register_dim_assign!(vsize,engine);
    register_int_assign!(widowpenalty,engine);
    register_whatsit!(write,engine,(e,cmd) =>write::<ET>(e,cmd));
    register_assign!(xdef,engine,(e,cmd,global) =>xdef::<ET>(e,cmd,global,false,false,false));
    register_skip_assign!(xspaceskip,engine);
    register_int!(year,engine,(e,c) => year::<ET>(e.state,c));

    register_muskip_assign!(thinmuskip,engine);
    register_muskip_assign!(medmuskip,engine);
    register_muskip_assign!(thickmuskip,engine);
    

    // TODOS ---------------------------------------------------------------------

    cmstodo!(engine,mathord);
    cmstodo!(engine,mathop);
    cmstodo!(engine,mathbin);
    cmstodo!(engine,mathrel);
    cmstodo!(engine,mathopen);
    cmstodo!(engine,mathclose);
    cmstodo!(engine,mathpunct);
    cmstodo!(engine,mathinner);
    cmstodo!(engine,mathaccent);
    cmstodo!(engine,radical);
    cmstodo!(engine,delimiter);
    cmstodo!(engine,prevdepth);


    cmtodo!(engine,lastpenalty);
    cmtodo!(engine,parshape);
    cmtodo!(engine,badness);
    cmtodo!(engine,spacefactor);
    cmtodo!(engine,prevgraf);
    cmtodo!(engine,deadcycles);
    cmtodo!(engine,insertpenalties);
    cmtodo!(engine,textfont);
    cmtodo!(engine,scriptfont);
    cmtodo!(engine,scriptscriptfont);
    cmtodo!(engine,lastkern);
    cmtodo!(engine,pagegoal);
    cmtodo!(engine,pagetotal);
    cmtodo!(engine,pagestretch);
    cmtodo!(engine,pagefilstretch);
    cmtodo!(engine,pagefillstretch);
    cmtodo!(engine,pagefilllstretch);
    cmtodo!(engine,pageshrink);
    cmtodo!(engine,pagedepth);
    cmtodo!(engine,ht);
    cmtodo!(engine,wd);
    cmtodo!(engine,dp);
    cmtodo!(engine,lastskip);
    cmtodo!(engine,scrollmode);
    cmtodo!(engine,nonstopmode);
    cmtodo!(engine,batchmode);
    cmtodo!(engine,box);
    cmtodo!(engine,copy);
    cmtodo!(engine,lastbox);
    cmtodo!(engine,vsplit);
    cmtodo!(engine,vbox);
    cmtodo!(engine,vtop);
    cmtodo!(engine,show);
    cmtodo!(engine,showbox);
    cmtodo!(engine,showlists);
    cmtodo!(engine,showthe);
    cmtodo!(engine,shipout);
    cmtodo!(engine,aftergroup);
    cmtodo!(engine,special);
    cmtodo!(engine,penalty);
    cmtodo!(engine,kern);
    cmtodo!(engine,mkern);
    cmtodo!(engine,unpenalty);
    cmtodo!(engine,unkern);
    cmtodo!(engine,unskip);
    cmtodo!(engine,mark);
    cmtodo!(engine,topmark);
    cmtodo!(engine,firstmark);
    cmtodo!(engine,botmark);
    cmtodo!(engine,splitfirstmark);
    cmtodo!(engine,splitbotmark);
    cmtodo!(engine,insert);
    cmtodo!(engine,vadjust);
    cmtodo!(engine,vskip);
    cmtodo!(engine,vfil);
    cmtodo!(engine,vfill);
    cmtodo!(engine,vss);
    cmtodo!(engine,vfilneg);
    cmtodo!(engine,leaders);
    cmtodo!(engine,cleaders);
    cmtodo!(engine,xleaders);
    cmtodo!(engine,moveleft);
    cmtodo!(engine,moveright);
    cmtodo!(engine,unvbox);
    cmtodo!(engine,unvcopy);
    cmtodo!(engine,halign);
    cmtodo!(engine,valign);
    cmtodo!(engine,indent);
    cmtodo!(engine,noindent);
    cmtodo!(engine,noboundary);
    cmtodo!(engine,hfil);
    cmtodo!(engine,hfill);
    cmtodo!(engine,hfilneg);
    cmtodo!(engine,hss);
    cmtodo!(engine,accent);
    cmtodo!(engine,discretionary);
    cmtodo!(engine,raise);
    cmtodo!(engine,lower);
    cmtodo!(engine,setlanguage);
    cmtodo!(engine,nonscript);
    cmtodo!(engine,vcenter);
    cmtodo!(engine,underline);
    cmtodo!(engine,overline);
    cmtodo!(engine,displaylimits);
    cmtodo!(engine,limits);
    cmtodo!(engine,nolimits);
    cmtodo!(engine,mathchoice);
    cmtodo!(engine,displaystyle);
    cmtodo!(engine,textstyle);
    cmtodo!(engine,scriptstyle);
    cmtodo!(engine,scriptscriptstyle);
    cmtodo!(engine,left);
    cmtodo!(engine,right);
    cmtodo!(engine,over);
    cmtodo!(engine,atop);
    cmtodo!(engine,above);
    cmtodo!(engine,overwithdelims);
    cmtodo!(engine,atopwithdelims);
    cmtodo!(engine,abovewithdelims);
    cmtodo!(engine,eqno);
    cmtodo!(engine,leqno);
    cmtodo!(engine,bigskip);
    cmtodo!(engine,bye);
    cmtodo!(engine,char);
    cmtodo!(engine,cr);
    cmtodo!(engine,crcr);
    cmtodo!(engine,fontname);
    cmtodo!(engine,hskip);
    cmtodo!(engine,italiccorr);
    cmtodo!(engine,mathchar);
    cmtodo!(engine,medskip);
    cmtodo!(engine,mskip);
    cmtodo!(engine,noalign);
    cmtodo!(engine,omit);
    cmtodo!(engine,smallskip);
    cmtodo!(engine,span);
    cmtodo!(engine,unhbox);
    cmtodo!(engine,unhcopy);
}

