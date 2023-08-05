//! TeX primitive [`BaseCommand`]s

use std::marker::PhantomData;
use crate::{debug_log, register_assign, register_conditional, register_int_assign, register_unexpandable, register_tok_assign, register_int, register_whatsit, register_value_assign_int, register_value_assign_dim, register_value_assign_muskip, register_value_assign_skip, register_dim_assign, register_skip_assign, cmtodo, register_value_assign_font, register_open_box, cmstodo, register_muskip_assign, register_expandable, catch, file_end, throw, catch_prim, file_end_prim};
use crate::engine::filesystem::{File, FileSystem};
use crate::engine::gullet::Gullet;
use crate::engine::gullet::methods::{tokens_to_string, string_to_tokens, token_to_chars, get_char};
use crate::engine::state::State;
use crate::engine::mouth::Mouth;
use crate::engine::state::modes::{BoxMode, GroupType, TeXMode};
use crate::engine::stomach::Stomach;
use crate::tex::catcodes::{CategoryCode, CategoryCodeScheme};
use crate::tex::commands::{BaseCommand, Def, ExpToken, ParamToken, Command, ResolvedToken, BaseStomachCommand, BoxFun, CloseBoxFun, TokenCont, ValueCommand, CommandSource};
use crate::tex::commands::methods::parse_signature;
use crate::tex::numbers::{Int, Skip, Numeric, MuSkip, Dim};
use crate::tex::token::{BaseToken, Token, TokenList};
use crate::utils::errors::{TeXError};
use crate::utils::Ptr;
use crate::utils::strings::{AllCharsTrait, CharType, TeXStr};
use chrono::{Datelike, Timelike};
use crate::engine::{EngineType, gullet};
use crate::tex::boxes::{HBox, HVBox, OpenBox, StomachNode, TeXNode, Whatsit};
use crate::tex::ConditionalBranch;
use crate::tex::fonts::{FontStore,Font};
//use super::etex::protected;

/* TODO

SPACE
\/
\-
 */



pub fn SPACE<ET:EngineType>(_stomach:&mut ET::Stomach,state:&mut ET::State,_cmd:CommandSource<ET>)
                                     -> Result<(),TeXError<ET::Token>> {
    todo!("\\ ")
}

pub static ADVANCE: &str = "advance";

pub fn advance<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,global:bool)
                                                     -> Result<(),TeXError<ET::Token>> {
    debug_log!(trace=>"\\advance");
    todo!()
    /*
    catch_prim!(gullet.mouth().skip_whitespace::<ET>(state) => (ADVANCE,cmd));
    match catch_prim!(gullet.get_next_unexpandable(state) => (ADVANCE,cmd)) {
        None => file_end_prim!(ADVANCE,cmd),
        Some(ncmd) => match ncmd.command.base() {
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
                    ValueCommand::Register(u) => finish!(state.get_int_register(*u),nv => state.set_int_register(*u,nv,global)),
                    ValueCommand::Primitive(name) => finish!(state.get_primitive_int(name),nv => state.set_primitive_int(name,nv,global)),
                    ValueCommand::Complex {name,..} if *name == COUNT => {
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
                    ValueCommand::Register(u) => finish!(state.get_dim_register(*u),nv => state.set_dim_register(*u,nv,global)),
                    ValueCommand::Primitive(name) => finish!(state.get_primitive_dim(name),nv => state.set_primitive_dim(name,nv,global)),
                    ValueCommand::Complex {name,..} if *name == DIMEN => {
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
            o => throw!("expected register after \\advance;got:{:?}",o => cmd.cause)
        }
    }

     */
}

pub fn afterassignment<ET:EngineType>(state: &mut ET::State, gullet:&mut ET::Gullet, cmd:CommandSource<ET>) -> Result<(), TeXError<ET::Token>> {
    debug_log!(trace=>"\\afterassignment");
    let next = match catch_prim!(gullet.mouth().get_next::<ET>(state) => ("afterassignment",cmd)) {
        None => file_end!(cmd.cause),
        Some((t,_)) => t
    };
    state.set_afterassignment(next);
    Ok(())
}

pub fn begingroup<ET:EngineType>(state:&mut ET::State)-> Result<(),TeXError<ET::Token>> {
    state.stack_push(GroupType::CS);
    Ok(())
}

pub fn catcode_assign<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,global:bool) -> Result<(),TeXError<ET::Token>> {
    debug_log!(trace=>"Assigning category code");
    let i = catch_prim!(gullet.get_int(state) => ("catcode",cmd));
    let c: ET::Char = match ET::Char::from_i64(i.to_i64()) {
        Some(i) => i,
        None => throw!("Not a valid character: {}",i => cmd.cause)
    };
    catch_prim!(gullet.mouth().skip_eq_char::<ET>(state) => ("catcode",cmd));
    let v = catch_prim!(gullet.get_int(state) => ("catcode",cmd)).to_i64();
    if v < 0 || v > 15 {
        throw!("Invalid category code: {}",v => cmd.cause)
    }
    let cc: CategoryCode = unsafe{(v as u8).try_into().unwrap_unchecked()};
    debug_log!(debug=>"\\catcode '{}' = {}",c.char_str(),cc);
    state.set_catcode(c,cc,global);
    Ok(())
}
pub fn catcode_get<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>) -> Result<ET::Int,TeXError<ET::Token>> {
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

pub fn chardef<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,global:bool) -> Result<(),TeXError<ET::Token>> {
    debug_log!(trace=>"chardef");
    todo!()
    /*
    let name = catch_prim!(gullet.get_control_sequence(state) => (CHARDEF,cmd));
    catch_prim!(gullet.mouth().skip_eq_char::<ET>(state) => (CHARDEF,cmd));
    let char = catch_prim!(get_char::<ET>(gullet,state) => (CHARDEF,cmd));
    let cmd = Some(ET::Command::new(BaseCommand::CharDef(char),Some(&cmd.cause)));
    match name {
        BaseToken::Char(c,_) => {
            debug_log!(debug=>"\\chardef: {} = {}",c.char_str(),char.char_str());
            state.set_ac_command(c, cmd, global);
        }
        BaseToken::CS(name) => {
            debug_log!(debug=>"\\chardef: \\{} = {}",name,char.char_str());
            state.set_command(name, cmd, global);
        }
    }
    Ok(())

     */
}

pub fn closein<ET:EngineType>(state: &mut ET::State, gullet:&mut ET::Gullet, cmd:CommandSource<ET>) -> Result<(), TeXError<ET::Token>> {
    debug_log!(trace=>"\\closein");
    let i = catch_prim!(gullet.get_int(state) => ("closein",cmd));
    let i : usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => throw!("Invalid file number: {}",i => cmd.cause)
    };
    state.file_closein(i); // TODO error?
    Ok(())
}

pub fn closeout<ET:EngineType>(state: &mut ET::State, gullet:&mut ET::Gullet, cmd:CommandSource<ET>) -> Result<Whatsit<ET>, TeXError<ET::Token>> {
    debug_log!(trace=>"\\closeout");
    let i = catch_prim!(gullet.get_int(state) => ("closeout",cmd));
    let i : usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => throw!("Invalid file number: {}",i => cmd.cause)
    };
    let apply = Box::new(move |_: &mut _,s:&mut ET::State,_: &mut _| {
        s.file_closeout(i); // TODO error?
        Ok(())
    });
    Ok(Whatsit::new(apply))
}

pub static COUNT : &str = "count";

pub fn count_assign<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,global:bool) -> Result<(),TeXError<ET::Token>> {
    debug_log!(trace=>"Assigning \\count");
    let i = catch_prim!(gullet.get_int(state) => ("count",cmd));
    let i:usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => throw!("Not a valid register: {}",i => cmd.cause)
    };
    catch_prim!(gullet.mouth().skip_eq_char::<ET>(state) => ("count",cmd));
    let v = catch_prim!(gullet.get_int(state) => ("count",cmd));
    debug_log!(debug=>"\\count{} = {}",i,v);
    state.set_int_register(i,v,global);
    Ok(())
}
pub fn count_get<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>) -> Result<ET::Int,TeXError<ET::Token>> {
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
    -> Result<(),TeXError<ET::Token>> {
    debug_log!(trace=>"countdef");
    todo!()
    /*
    let name = catch_prim!(gullet.get_control_sequence(state) => (COUNTDEF,cmd));
    super::methods::set_relax::<ET>(state,&name,&cmd.cause,global);
    catch_prim!(gullet.mouth().skip_eq_char::<ET>(state) => (COUNTDEF,cmd));
    let num = catch_prim!(gullet.get_int(state) => ("countdef",cmd));
    if num.to_i64() < 0 {
        throw!("Invalid count register index: {}",num => cmd.cause)
    }
    let num = num.to_i64() as usize;
    let cmd = Some(ET::Command::new(BaseCommand::Int(ValueCommand::Register(num)), Some(&cmd.cause)));
    match name {
        BaseToken::Char(c,_) => {
            debug_log!(debug=>"\\countdef: {} = \\count{}",c.char_str(),num);
            state.set_ac_command(c, cmd, global);
        }
        BaseToken::CS(name) => {
            debug_log!(debug=>"\\countdef: \\{} = {}",name,num);
            state.set_command(name, cmd, global);
        }
    }
    Ok(())

     */
}

pub fn get_csname<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:&CommandSource<ET>,name:&'static str)
    -> Result<TeXStr<ET::Char>,TeXError<ET::Token>>{
    todo!()
    /*
    let csidx = state.push_csname();
    let mut csname = Vec::new();
    while state.current_csname() == Some(csidx) {
        match catch_prim!(gullet.get_next_unexpandable(state) => (name,cmd)) {
            None => return file_end!(cmd.cause),
            Some(sc) => match sc.command.base() {
                BaseCommand::Unexpandable {name:"endcsname",..} => state.pop_csname(),
                BaseCommand::Char{char,catcode:CategoryCode::Space} => csname.push(ET::Char::from(b' ')),
                BaseCommand::Char{char,..} => csname.push(*char),
                o => throw!("Unexpected token in {}: {:?}",name,o => cmd.cause)
            }
        }
    }
    Ok(csname.into())

     */
}

pub fn csname<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,f:TokenCont<ET>) -> Result<(),TeXError<ET::Token>> {
    debug_log!(trace=>"csname");
    todo!()
    /*
    let str = get_csname::<ET>(state,gullet,cmd,"csname")?;
    debug_log!(trace=>"csname {}",str.to_string());
    match state.get_command(&str) {
        None => state.set_command(str.clone(), Some(Ptr::new(BaseCommand::Relax)), false),
        _ => ()
    }
    f(ET::Token::new(BaseToken::CS(str),None));
    Ok(())

     */
}

pub fn day<ET:EngineType>(state:&mut ET::State,cmd:CommandSource<ET>) -> Result<ET::Int,TeXError<ET::Token>> {
    Ok(catch_prim!(ET::Int::from_i64(state.get_start_time().day() as i64) => ("day",cmd)))
}

pub static DEF : &str = "def";
pub fn def<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,global:bool,protected:bool,long:bool,outer:bool)
    -> Result<(),TeXError<ET::Token>> {
    debug_log!(trace=>"def");
    todo!()
    /*
    let csO = catch_prim!(gullet.mouth().get_next::<ET>(state) => (DEF,cmd));
    let cs = match &csO {
        None => file_end_prim!(DEF,cmd),
        Some((t,_)) => t.base()
    };
    match cs {
        BaseToken::Char(_,CategoryCode::Active) => (),
        BaseToken::CS(_) => (),
        _ => throw!("Command expected after \\def" => csO.unwrap().0)
    }
    let (endswithbrace,arity,signature) = parse_signature::<ET>(state,gullet,&cmd,DEF)?;
    let mut replacement: Vec<ExpToken<ET::Token>> = vec!();
    let mut partk = None;
    catch_prim!(gullet.get_group(state,&mut |_,t| match (std::mem::take(&mut partk),t.base()) {
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
    let def = ET::Command::new(BaseCommand::Def(Ptr::new(Def{protected,long,outer,endswithbrace,arity,signature,replacement})),Some(&cmd.cause));
    match cs {
        BaseToken::Char(c,_) => {
            debug_log!(debug=>"\\def: {} = {:?}",c,def);
            state.set_ac_command(*c,Some(def),global)
        }
        BaseToken::CS(name) => {
            debug_log!(debug=>"\\def: \\{} = {:?}",name,def);
            state.set_command(name.clone(),Some(def),global)
        }
    }
    return Ok(())

     */
}

pub fn delcode_assign<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,global:bool)
                                      -> Result<(),TeXError<ET::Token>> {
    debug_log!(trace=>"Assigning delcode");
    let i = catch_prim!(gullet.get_int(state) => ("delcode",cmd));
    let c: ET::Char = match ET::Char::from_i64(i.to_i64()) {
        Some(i) => i,
        None => throw!("Not a valid character: {}",i => cmd.cause)
    };
    catch_prim!(gullet.mouth().skip_eq_char::<ET>(state) => ("delcode",cmd));
    let i = catch_prim!(gullet.get_int(state) => ("delcode",cmd));
    debug_log!(debug=>"\\delcode '{}' = {}",c.char_str(),i);
    state.set_delcode(c,i,global);
    Ok(())
}

pub fn delcode_get<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>)
                                   -> Result<ET::Int,TeXError<ET::Token>> {
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
    -> Result<(),TeXError<ET::Token>> {
    debug_log!(trace=>"Assigning \\dimen");
    let i = catch_prim!(gullet.get_int(state) => ("dimen",cmd));
    let i:usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => throw!("Not a valid register: {}",i => cmd.cause)
    };
    catch_prim!(gullet.mouth().skip_eq_char::<ET>(state) => ("dimen",cmd));
    let v = catch_prim!(gullet.get_dim(state) => ("dimen",cmd));
    debug_log!(debug=>"\\dimen{} = {}",i,v);
    state.set_dim_register(i,v,global);
    Ok(())
}

pub fn dimen_get<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>)
    -> Result<ET::Dim,TeXError<ET::Token>> {
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

pub fn dimendef<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,global:bool)
                                                     -> Result<(),TeXError<ET::Token>> {
    debug_log!(trace=>"dimendef");
    todo!()
    /*
    let name = catch_prim!(gullet.get_control_sequence(state) => ("dimendef",cmd));
    match &name {
        BaseToken::Char(c,_) => {
            state.set_ac_command(*c, Some(Ptr::new(BaseCommand::Relax)), false)
        }
        BaseToken::CS(name) => {
            state.set_command(name.clone(), Some(Ptr::new(BaseCommand::Relax)), false)
        }
    }
    catch_prim!(gullet.mouth().skip_eq_char::<ET>(state) => ("dimendef",cmd));
    let num = catch_prim!(gullet.get_int(state) => ("dimendef",cmd));
    if num.to_i64() < 0 {
        return Err(ErrorInPrimitive{name:"dimendef",msg:Some(format!("Invalid dimen register index: {}",num)),cause:Some(cmd.cause),source:None})
    }
    let num = num.to_i64() as usize;
    let ret = ET::Command::new(BaseCommand::Dim {
        name:Some(DIMEN),index:Some(num),
        get:|s,_,_| s.get_dim_register(num),
        set:Some(|s,g,c,gl| super::methods::assign_dim_register(s,g,num,c,gl))
    },Some(cmd.cause));
    match name {
        BaseToken::Char(c,_) => {
            debug_log!(debug=>"\\dimendef: {} = \\dimen{}",c.char_str(),num);
            state.set_ac_command(c, Some(ret), global);
        }
        BaseToken::CS(name) => {
            debug_log!(debug=>"\\dimendef: \\{} = {}",name,num);
            state.set_command(name, Some(ret), global);
        }
    }
    Ok(())

     */
}
pub static DIVIDE : &str = "divide";
pub fn divide<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,global:bool)
    -> Result<(),TeXError<ET::Token>> {
    todo!()
    /*
    debug_log!(trace=>"\\divide");
    catch_prim!(gullet.mouth().skip_whitespace::<ET>(state) => (DIVIDE,cmd));
    match catch_prim!(gullet.get_next_unexpandable(state) => (DIVIDE,cmd)) {
        None => file_end_prim!(DIVIDE,cmd),
        Some(cmd) => match cmd.command.base() {
            BaseCommand::Int{name,index,..}  if *name == COUNT => {
                let u = match index {
                    Some(u) => *u,
                    None => {
                        let int = catch_prim!(gullet.get_int(state) => (DIVIDE,cmd));
                        match int.try_into() {
                            Ok(u) => u,
                            _ => throw!("Not a valid register: {}",int => cmd.cause)
                        }
                    }
                };
                catch_prim!(gullet.get_keyword(state,"by") => (DIVIDE,cmd));
                let i = catch_prim!(gullet.get_int(state) => (DIVIDE,cmd));
                debug_log!(debug => "  \\count{}",u);
                let ov = state.get_int_register(u);
                debug_log!(debug => "  =={}/{}",ov,i);
                let nv : ET::Int = ov / i;
                debug_log!(debug => "  ={}",nv);
                state.set_int_register(u,nv,global);
                return Ok(())
            }
            BaseCommand::Int{name,index:None,..} => { // TODO this covers potentially too many cases
                debug_log!(debug => "  \\{}",name);
                catch_prim!(gullet.get_keyword(state,"by") => (DIVIDE,cmd));
                let ov = state.get_primitive_int(name);
                let i = catch_prim!(gullet.get_int(state) => (DIVIDE,cmd));
                debug_log!(debug => "  =={}/{}",ov,i);
                if i.to_i64() == 0 {
                    throw!("Division by zero: {} / {}",ov,i => cmd.cause)
                }
                let nv : ET::Int = ov / i;
                debug_log!(debug => "  ={}",nv);
                state.set_primitive_int(name,nv,global);
                return Ok(())
            }
            BaseCommand::Dim{name,index,..}  if *name == DIMEN => {
                let u = match index {
                    Some(u) => *u,
                    None => {
                        let int = catch_prim!(gullet.get_int(state) => (DIVIDE,cmd));
                        match int.try_into() {
                            Ok(u) => u,
                            _ => throw!("Not a valid register: {}",int => cmd.cause)
                        }
                    }
                };
                catch_prim!(gullet.get_keyword(state,"by") => (DIVIDE,cmd));
                let i = catch_prim!(gullet.get_int(state) => (DIVIDE,cmd)).to_i64();
                debug_log!(debug => "  \\count{}",u);
                let ov = state.get_dim_register(u);
                debug_log!(debug => "  =={}/{}",ov,i);
                let nv = ov.tex_div(i);
                debug_log!(debug => "  ={}",nv);
                state.set_dim_register(u,nv,global);
                return Ok(())
            }
            BaseCommand::Dim{name,index:None,..} => { // TODO this covers potentially too many cases
            debug_log!(debug => "  \\{}",name);
                catch_prim!(gullet.get_keyword(state,"by") => (DIVIDE,cmd));
                let ov = state.get_primitive_dim(name);
                let i = catch_prim!(gullet.get_int(state) => (DIVIDE,cmd)).to_i64();
                debug_log!(debug => "  =={}/{}",ov,i);
                if i == 0 {
                    throw!("Division by zero: {} / {}",ov,i => cmd.cause)
                }
                let nv = ov.tex_div(i);
                debug_log!(debug => "  ={}",nv);
                state.set_primitive_dim(name,nv,global);
                return Ok(())
            }
            o => throw!("expected register after \\divide;got:{:?}",o => cmd.cause)
        }
    }

     */
}

pub fn dump<ET:EngineType>()
                                    -> Result<(), TeXError<ET::Token>> {
    debug_log!(trace=>"\\dump");
    // TODO
    Ok(())
}

pub fn edef<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,global:bool,protected:bool,long:bool,outer:bool)
                                                -> Result<(),TeXError<ET::Token>> {
    debug_log!(trace=>"edef");
    todo!()
    /*
    let csO = catch_prim!(gullet.mouth().get_next::<ET>(state) => (DEF,cmd));
    let cs = match &csO {
        None => file_end_prim!(DEF,cmd),
        Some((t,_)) => t.base()
    };
    match cs {
        BaseToken::Char(_,CategoryCode::Active) => (),
        BaseToken::CS(_) => (),
        _ => throw!("Command expected after \\def" => csO.unwrap().0)
    }
    let (endswithbrace,arity,signature) = parse_signature::<ET>(state,gullet,&cmd,DEF)?;
    let mut replacement: Vec<ExpToken<ET::Token>> = vec!();
    let mut partk = None;
    catch_prim!(gullet.get_expanded_group(state,false,true,false,&mut |_,t| match (std::mem::take(&mut partk),t.base()) {
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
    let def = ET::Command::new(BaseCommand::Def(Ptr::new(Def{protected,long,outer,endswithbrace,arity,signature,replacement})),Some(&cmd.cause));
    match cs {
        BaseToken::Char(c,_) => {
            debug_log!(debug=>"\\def: {} = {:?}",c,def);
            state.set_ac_command(*c,Some(def),global)
        }
        BaseToken::CS(name) => {
            debug_log!(debug=>"\\def: \\{} = {:?}",name,def);
            state.set_command(name.clone(),Some(def),global)
        }
    }
    return Ok(())

     */
}

pub fn else_<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>)
    -> Result<(),TeXError<ET::Token>> {
    match gullet.current_conditional() {
        (None,_) => throw!("Not in a conditional" => cmd.cause),
        (Some(ConditionalBranch::True(name)),i) =>
            catch_prim!(crate::engine::gullet::methods::else_loop::<ET>(gullet,state,name,i,false) => ("else",cmd)),
        (Some(ConditionalBranch::Case(_,_)),i) =>
            catch_prim!(crate::engine::gullet::methods::else_loop::<ET>(gullet,state,"ifcase",i,false) => ("ifcase",cmd)),
        o => unreachable!("{:?}\nat:{}\n{}\n",o,gullet.mouth().file_line(),gullet.mouth().preview(200))
    }
    Ok(())
}

pub fn end<ET:EngineType>()
    -> Result<(),TeXError<ET::Token>> {
    todo!("end")
}

pub fn endcsname<ET:EngineType>(cmd:CommandSource<ET>) -> Result<(),TeXError<ET::Token>> {
    throw!("Unexpected \\endcsname; not in a \\csname" => cmd.cause)
}

pub fn endgroup<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>)
    -> Result<(),TeXError<ET::Token>> {
    match state.stack_pop() {
        Some((v,GroupType::CS)) => {
            gullet.mouth().push_tokens(v);
            Ok(())
        }
        _ => throw!("No group to end" => cmd.cause)
    }
}

pub fn endinput<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>)
                            -> Result<(),TeXError<ET::Token>> {
    gullet.mouth().endinput::<ET>(state);
    Ok(())
}

pub fn endlinechar_assign<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,global:bool)
    -> Result<(),TeXError<ET::Token>> {
    debug_log!(trace=>"Assigning \\endlinechar");
    catch_prim!(gullet.mouth().skip_eq_char::<ET>(state) => ("endlinechar",cmd));
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
    -> Result<ET::Int,TeXError<ET::Token>> {
    debug_log!(trace=>"Getting \\endlinechar");
    let c = match state.get_endlinechar() {
        None => -1,
        Some(c) => c.to_usize() as i64
    };
    debug_log!(debug=>"\\endlinechar == {:?}",c);
    Ok(ET::Int::from_i64::<ET::Token>(c).unwrap())
}

// \errhelp
pub static ERRMESSAGE : &str = "errmessage";

pub fn errmessage<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>)
    -> Result<(),TeXError<ET::Token>> {
    debug_log!(debug=>"errmessage");
    catch_prim!(gullet.mouth().skip_whitespace::<ET>(state) => (ERRMESSAGE,cmd));
    let errmsg = String::from_utf8(catch_prim!(gullet.get_braced_string(state) => (ERRMESSAGE,cmd))).unwrap();
    let eh = state.get_primitive_toks("errhelp");
    // TODO errhelp
    Err(TeXError{
        msg:errmsg + "\n\n" + &gullet.mouth().file_line(),
        cause:Some(cmd.cause),
        source:None
    }.into())
}

pub fn errorstopmode<ET:EngineType>()
                                  -> Result<(), TeXError<ET::Token>> {
    debug_log!(trace=>"\\errorstopmode");
    // TODO
    Ok(())
}

pub fn escapechar_assign<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,global:bool)
    -> Result<(),TeXError<ET::Token>> {
    debug_log!(trace=>"Assigning \\escapechar");
    catch_prim!(gullet.mouth().skip_eq_char::<ET>(state) => ("escapechar",cmd));
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
pub fn escapechar_get<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>) -> Result<ET::Int,TeXError<ET::Token>> {
    debug_log!(trace=>"Getting \\escapechar");
    let c = match state.get_escapechar() {
        None => -1,
        Some(c) => c.to_usize() as i64
    };
    debug_log!(debug=>"\\escapechar == {}",c);
    Ok(ET::Int::from_i64::<ET::Token>(c).unwrap())
}

pub fn expandafter<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,f:TokenCont<ET>)
    -> Result<(),TeXError<ET::Token>> {
    debug_log!(trace=>"expandafter");
    todo!()
    /*
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
                    BaseCommand::Conditional{name,index} =>
                        catch_prim!(crate::engine::gullet::methods::do_conditional::<ET>(gullet,state,t,name,*index,false) => ("expandafter",cmd)),
                    BaseCommand::Expandable {name,index} =>
                        catch_prim!(crate::engine::gullet::methods::do_expandable::<ET>(gullet,state,t,name,*index) => ("expandafter",cmd)),
                    BaseCommand::Def(d, _) => {
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

     */
}

pub fn fi<ET:EngineType>(_state:&mut ET::State,gullet:&mut ET::Gullet,_cmd:CommandSource<ET>)
    -> Result<(),TeXError<ET::Token>> {
    debug_log!(trace=>"...end of conditional.");
    gullet.pop_conditional();
    Ok(())
}

pub fn font_assign<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,global:bool)
    -> Result<(),TeXError<ET::Token>> {
    debug_log!(trace=>"Assigning \\font");
    todo!()
    /*
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
    let fontcmd = BaseCommand::ValueRegister{index,tp:Assignable::Font};
    match cs.base() {
        BaseToken::Char(c,CategoryCode::Active) =>
            state.set_ac_command(*c,Some(Ptr::new(fontcmd)),global),
        BaseToken::CS(name) =>
            state.set_command(name.clone(),Some(Ptr::new(fontcmd)),global),
        _ => unreachable!()
    }
    Ok(())

     */
}
pub fn font_get<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>)
    -> Result<ET::Font,TeXError<ET::Token>> {
    debug_log!(trace=>"Getting \\font");
    todo!("\\font_get")
}

pub fn fontdimen_assign<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,global:bool)
    -> Result<(),TeXError<ET::Token>> {
    debug_log!(trace=>"Assigning \\fontdimen");
    let o = catch_prim!(gullet.get_int(state) => ("fontdimen",cmd));
    let i = match o.try_into() {
        Ok(i) => i,
        Err(_) => throw!("Invalid font dimension index" => cmd.cause)
    };
    let mut font = catch_prim!(gullet.get_font(state) => ("fontdimen",cmd));
    catch_prim!(gullet.mouth().skip_eq_char::<ET>(state) => ("fontdimen",cmd));
    let dim = catch_prim!(gullet.get_dim(state) => ("fontdimen",cmd));
    font.set_dim::<ET::Dim>(i,dim);
    Ok(())
}

pub fn fontdimen_get<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>)
    -> Result<ET::Dim,TeXError<ET::Token>> {
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
                                -> Result<(),TeXError<ET::Token>> {
    debug_log!(trace=>"\\futurelet");
    todo!()
    /*
    let cs = match catch_prim!(gullet.mouth().get_next::<ET>(state) => ("futurelet",cmd)) {
        None => file_end_prim!("futurelet",cmd),
        Some((t,_)) => match t.base() {
            BaseToken::Char(c,CategoryCode::Active) => {
                BaseToken::Char(*c,CategoryCode::Active)
            }
            BaseToken::CS(name) => {
                BaseToken::CS(name.clone())
            }
            _ => return Err(ErrorInPrimitive{name:"futurelet",msg:Some("Expected control sequence after \\futurelet".into()),cause:Some(cmd.cause),source:None})
        }
    };
    let first = match catch_prim!(gullet.mouth().get_next::<ET>(state) => ("futurelet",cmd)) {
        None => file_end_prim!("futurelet",cmd),
        Some((t,_)) => t
    };
    let second = match catch_prim!(gullet.mouth().get_next::<ET>(state) => ("futurelet",cmd)) {
        None => file_end_prim!("futurelet",cmd),
        Some((t,_)) => t
    };
    let newcmd = match second.base() {
        BaseToken::Char(c,CategoryCode::Active) => state.get_ac_command(c),
        BaseToken::CS(name) => state.get_command(name),
        BaseToken::Char(c,cc) =>
            Some(Ptr::new(BaseCommand::Char{char:*c,catcode:*cc}))
    };
    debug_log!(debug=>"\\futurelet: setting {} to {:?}",cs,newcmd);
    match cs {
        BaseToken::Char(c,CategoryCode::Active) => {
            state.set_ac_command(c,newcmd,global);
        }
        BaseToken::CS(name) => {
            state.set_command(name,newcmd,global);
        }
        _ => unreachable!()
    }
    gullet.mouth().push_tokens(vec!(first,second));
    Ok(())

     */
}


pub fn gdef<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,global:bool,protected:bool,long:bool,outer:bool)
                                                 -> Result<(),TeXError<ET::Token>> {
    def::<ET>(state,gullet,cmd,true,protected,long,outer)
}
pub fn global<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,global_:bool,protected_:bool,long_:bool,outer_:bool)
                                                 -> Result<(),TeXError<ET::Token>> {
    debug_log!(trace => "\\global");
    catch_prim!(gullet.mouth().skip_whitespace::<ET>(state) => ("global",cmd));
    match catch_prim!(gullet.get_next_stomach_command(state) => ("global",cmd)) {
        None => file_end_prim!("global",cmd),
        Some(c) => match c.command {
            BaseStomachCommand::Assignment {name:Some("global"),..} => global::<ET>(state,gullet,cmd,true,protected_,long_,outer_),
            BaseStomachCommand::Assignment{name:Some("protected"),..} => super::etex::protected::<ET>(state,gullet,cmd,true,protected_,long_,outer_),
            BaseStomachCommand::Assignment {name:Some("long"),..} => long::<ET>(state,gullet,cmd,true,protected_,long_,outer_),
            BaseStomachCommand::Assignment {name:Some("outer"),..} => outer::<ET>(state,gullet,cmd,true,protected_,long_,outer_),
            BaseStomachCommand::Assignment {name:Some("def"),..} => def::<ET>(state,gullet,cmd,true,protected_,long_,outer_),
            BaseStomachCommand::Assignment {name:Some("edef"),..} => edef::<ET>(state,gullet,cmd,true,protected_,long_,outer_),
            BaseStomachCommand::Assignment {name:Some("gdef"),..} => gdef::<ET>(state,gullet,cmd,true,protected_,long_,outer_),
            BaseStomachCommand::Assignment {name:Some("xdef"),..} => xdef::<ET>(state,gullet,cmd,true,protected_,long_,outer_),
            BaseStomachCommand::Assignment {set,..} => Ok(catch_prim!(set(state,gullet,c.source,true) => ("global",cmd))),
            o => todo!("global: {:?} at {}",o,gullet.mouth().preview(100))
        }
    }
}

pub fn hbox<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>)
                                         -> Result<CloseBoxFun<ET>,TeXError<ET::Token>> {
    debug_log!(trace=>"\\hbox");
    todo!()
    /*
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
                    Some(HVBox::H(HBox {
                        children, to:to.clone(), spread:spread.clone(),
                        ..Default::default()
                    }))
                }))
            }
            _ => return Err(ErrorInPrimitive{name:"hbox",msg:Some(format!("Expected begin group, found {:?}",next.cause)),cause:Some(cmd.cause),source:None})
        }
    }
    file_end_prim!("hbox",cmd);

     */
}

pub fn hyphenation<ET:EngineType>(state: &mut ET::State, gullet:&mut ET::Gullet,cmd:CommandSource<ET>)
                               -> Result<(), TeXError<ET::Token>> {
    debug_log!(trace=>"\\hyphenation");
    // TODO
    catch_prim!(gullet.mouth().read_argument::<ET>(state,&mut |_,_| Ok(())) => ("hyphenation",cmd));
    Ok(())
}

pub fn hyphenchar_assign<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,global:bool)
    -> Result<(),TeXError<ET::Token>> {
    debug_log!(trace=>"Assigning \\hyphenchar");
    let mut font = catch_prim!(gullet.get_font(state) => ("hyphenchar",cmd));
    catch_prim!(gullet.mouth().skip_eq_char::<ET>(state) => ("hyphenchar",cmd));
    let i = catch_prim!(gullet.get_int(state) => ("hyphenchar",cmd)).to_i64();
    debug_log!(debug=>"\\hyphenchar\\{:?} = {:?}",font,i);
    font.set_hyphenchar(i);
    Ok(())
}
pub fn hyphenchar_get<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>) -> Result<ET::Int,TeXError<ET::Token>> {
    debug_log!(trace=>"Getting \\hyphenchar");
    let font = catch_prim!(gullet.get_font(state) => ("hyphenchar",cmd));
    debug_log!(debug=>"\\hyphenchar == {:?}",font.get_hyphenchar());
    Ok(catch_prim!(ET::Int::from_i64::<ET::Token>(font.get_hyphenchar()) => ("hyphenchar",cmd)))
}

pub fn if_<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>)
    -> Result<bool,TeXError<ET::Token>> {
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

pub static IFCASE: &str = "ifcase";
pub fn ifcase<ET:EngineType>() -> Result<bool,TeXError<ET::Token>> {
    unreachable!("executed in Gullet")
}

pub fn ifcat<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>)
    -> Result<bool,TeXError<ET::Token>> {
    debug_log!(trace=>"ifcat");
    todo!()
    /*
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
                    BaseCommand::Char {catcode,..} => *catcode,
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
                    BaseCommand::Char {catcode,..} => *catcode,
                    _ => CategoryCode::Escape
                }
            }
        }
    };
    Ok(first == second)

     */
}

pub fn get_if_token<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,name:&'static str)
    -> Result<Option<ET::Token>,TeXError<ET::Token>> {
    todo!()
    /*
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
                BaseCommand::Conditional{name,index} if e => {
                    catch_prim!(crate::engine::gullet::methods::do_conditional::<ET>(gullet,state,t,name,*index,false) => (name,cmd));
                },
                BaseCommand::Def(d, _) if e => {
                    let v = catch_prim!(d.expand::<ET>(state,gullet.mouth(),c.clone(),Ptr::new(t.clone())) => (name,cmd));
                    if !v.is_empty() {
                        gullet.mouth().push_tokens(v);
                    }
                },
                BaseCommand::Expandable {name,..} if e && (*name == "else" || *name == "fi") && (match gullet.current_conditional() {
                    (Some(ConditionalBranch::None(_)),_) => true,
                    _ => false
                }) => {
                    gullet.mouth().requeue(t);
                    return Ok(None)
                }
                BaseCommand::Expandable {name,index} if e => {
                    catch_prim!(crate::engine::gullet::methods::do_expandable::<ET>(gullet,state,t,name,*index) => (name,cmd));
                },
                _ => return Ok(Some(t))
            }
        }
    }
    file_end_prim!(name,cmd)

     */
}


pub fn ifdim<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>)
                            -> Result<bool,TeXError<ET::Token>> {
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
    -> Result<bool,TeXError<ET::Token>> {
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
                            -> Result<bool,TeXError<ET::Token>> {
    debug_log!(trace=>"ifhmode");
    Ok(match state.mode() {
        TeXMode::Horizontal | TeXMode::RestrictedHorizontal => true,
        _ => false
    })
}

pub fn ifinner<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>)
                              -> Result<bool,TeXError<ET::Token>> {
    debug_log!(trace=>"ifhmode");
    Ok(match state.mode() {
        TeXMode::RestrictedHorizontal | TeXMode::InternalVertical | TeXMode::Math => true,
        _ => false
    })
}

pub fn ifnum<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>)
    -> Result<bool,TeXError<ET::Token>> {
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
    -> Result<bool,TeXError<ET::Token>> {
    debug_log!(trace=>"ifodd");
    let num = catch_prim!(gullet.get_int(state) => ("ifodd",cmd));
    Ok(num.to_i64() % 2 != 0)
}


pub fn ifvmode<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>)
                              -> Result<bool,TeXError<ET::Token>> {
    debug_log!(trace=>"ifvmode");
    Ok(match state.mode() {
        TeXMode::Vertical | TeXMode::InternalVertical => true,
        _ => false
    })
}

pub fn ifx<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>)
    -> Result<bool,TeXError<ET::Token>> {
    debug_log!(trace=>"ifx");
    todo!()
    /*
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
            let cmd1 = state.get_ac_command(c1);
            let cmd2 = state.get_ac_command(c2);
            Ok(ifx_eq_cmd::<ET>(cmd1,t1,exp1,cmd2,t2,exp2))
        }
        (BaseToken::CS(name),BaseToken::Char(c2,CategoryCode::Active)) =>{
            let cmd1 = state.get_command(name);
            let cmd2 = state.get_ac_command(c2);
            Ok(ifx_eq_cmd::<ET>(cmd1,t1,exp1,cmd2,t2,exp2))
        }
        (BaseToken::Char(c1,CategoryCode::Active),BaseToken::CS(name)) =>{
            let cmd1 = state.get_ac_command(c1);
            let cmd2 = state.get_command(name);
            Ok(ifx_eq_cmd::<ET>(cmd1,t1,exp1,cmd2,t2,exp2))
        }
        (BaseToken::CS(name1),BaseToken::CS(name2)) =>{
            let cmd1 = state.get_command(name1);
            let cmd2 = state.get_command(name2);
            Ok(ifx_eq_cmd::<ET>(cmd1,t1,exp1,cmd2,t2,exp2))
        }
        (BaseToken::Char(c1,cc1),BaseToken::Char(c2,cc2)) =>
            Ok(c1==c2 && cc1 == cc2),
        (BaseToken::Char(c1,cc1),BaseToken::CS(name)) =>
            Ok(match state.get_command(name).map(|c| c.base()) {
                Some(BaseCommand::Char{char,catcode}) => *char == *c1 && *catcode == *cc1,
                _ => false
            }),
        (BaseToken::CS(name),BaseToken::Char(c2,cc2)) =>
            Ok(match state.get_command(name).map(|c| c.base()) {
                Some(BaseCommand::Char{char,catcode}) => *char == *c2 && *catcode == *cc2,
                _ => false
            })
    }

     */
}
/*
fn ifx_eq_cmd<ET:EngineType>(cmd1:Option<&ET::Command>, t1:ET::Token, expand1:bool, cmd2:Option<&ET::Command>, t2:ET::Token, expand2:bool) -> bool {
    debug_log!(debug=>"ifx_eq_cmd: {:?} == {:?}?",cmd1,cmd2);
    if expand1 && expand2 {cmd1 == cmd2}
    else if !expand1 && !expand2 {
        t1 == t2
    }
    else { false }
}

 */

pub fn ignorespaces<ET:EngineType>(state: &mut ET::State, gullet:&mut ET::Gullet, cmd:CommandSource<ET>)
                                  -> Result<(), TeXError<ET::Token>> {
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

pub fn immediate<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>)
                                                 -> Result<(),TeXError<ET::Token>> {
    debug_log!(trace=>"immediate");
    todo!()
    /*
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
                catch_prim!(wi.apply(stomach,state,gullet) => ("immediate",cmd));
                Ok(())
            }
            _ => {
                gullet.mouth().requeue(sc.cause);
                Ok(())
            }
        }
    }

     */
}

pub fn input<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>)
    -> Result<(),TeXError<ET::Token>> {
    debug_log!(trace=>"input");
    let filename = catch_prim!(gullet.get_string(state) => ("input",cmd)).to_string();
    debug_log!(trace=>"input: {}",filename);
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

pub fn inputlineno<ET:EngineType>(gullet:&mut ET::Gullet,cmd:CommandSource<ET>) -> Result<ET::Int,TeXError<ET::Token>> {
    Ok(catch_prim!(ET::Int::from_i64(
        gullet.mouth().line_no() as i64
    ) => ("month",cmd)))
}

pub fn jobname<ET:EngineType>(state:&mut ET::State,f:TokenCont<ET>)
                         -> Result<(),TeXError<ET::Token>> {
    debug_log!(trace=>"jobname");
    for t in string_to_tokens(state.get_jobname().as_bytes()) { f(state,t)? }
    Ok(())
}

pub fn lccode_assign<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,global:bool)
    -> Result<(),TeXError<ET::Token>> {
    debug_log!(trace=>"Assigning lower case character");
    let i = catch_prim!(gullet.get_int(state) => ("lccode",cmd));
    let c: ET::Char = match ET::Char::from_i64(i.to_i64()) {
        Some(i) => i,
        None => throw!("Not a valid character: {}",i => cmd.cause)
    };
    catch_prim!(gullet.mouth().skip_eq_char::<ET>(state) => ("lccode",cmd));
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
    -> Result<ET::Int,TeXError<ET::Token>> {
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
    -> Result<(),TeXError<ET::Token>> {
    debug_log!(trace=>"let");
    let csO = catch_prim!(gullet.mouth().get_next::<ET>(state) => (LET,cmd));
    let cs = match &csO {
        None => file_end_prim!(LET,cmd),
        Some((t,_)) => t.base()
    };
    match cs {
        BaseToken::Char(_,CategoryCode::Active) | BaseToken::CS(_) => (),
        _ => throw!("Expected a control sequence" => csO.unwrap().0)
    }
    catch_prim!(gullet.mouth().skip_eq_char::<ET>(state) => (LET,cmd));
    let cm = match catch_prim!(gullet.mouth().get_next::<ET>(state) => (LET,cmd)) {
        Some((t,_)) => t,
        None =>file_end_prim!(LET,cmd)
    };
    let cmd = match cm.base() {
        BaseToken::Char(c,CategoryCode::Active) => state.get_ac_command(&c).map(|c|c.clone().copy_with(&cmd)),
        BaseToken::CS(name) => state.get_command(&name).map(|c|c.clone().copy_with(&cmd)),
        BaseToken::Char(c,cc) =>
            Some(Command::new(BaseCommand::Char{char:*c,catcode:*cc},Some(&cmd))),
    };
    debug_log!(debug=>"let: {} = {:?}",cs,cmd);
    match cs {
        BaseToken::Char(c,_) => state.set_ac_command(*c,cmd,globally),
        BaseToken::CS(name) => state.set_command(name.clone(),cmd,globally)
    }
    Ok(())
}

pub fn long<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd: CommandSource<ET>,global_:bool,protected_:bool,long_:bool,outer_:bool)
                                                -> Result<(),TeXError<ET::Token>> {
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
                                         -> Result<(),TeXError<ET::Token>> {
    debug_log!(trace => "\\lowercase");
    let mut ret = vec!();
    catch_prim!(gullet.get_group(state,&mut |s,next| match next.base() {
        BaseToken::Char(c,cc) => {
            let nc = s.get_lccode(c);
            Ok(ret.push(ET::Token::new(BaseToken::Char(nc, *cc), None)))
        }
        _ => Ok(ret.push(next))
    }) => (LOWERCASE,cmd));
    gullet.mouth().push_tokens(ret);
    Ok(())
}

pub fn mathchardef<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,globally:bool)
    -> Result<(),TeXError<ET::Token>> {
    debug_log!(trace=>"mathchardef");
    todo!()
    /*
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
        BaseToken::Char(c,_) => state.set_ac_command(*c, Some(Ptr::new(BaseCommand::MathChar(i as u32))), globally),
        BaseToken::CS(name) => state.set_command(name.clone(), Some(Ptr::new(BaseCommand::MathChar(i as u32))), globally)
    }
    Ok(())

     */
}

pub fn mathcode_assign<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,global:bool)
                                    -> Result<(),TeXError<ET::Token>> {
    debug_log!(trace=>"Assigning math code");
    let i = catch_prim!(gullet.get_int(state) => ("mathcode",cmd));
    let c: ET::Char = match ET::Char::from_i64(i.to_i64()) {
        Some(i) => i,
        None => throw!("Not a valid character: {}",i => cmd.cause)
    };
    catch_prim!(gullet.mouth().skip_eq_char::<ET>(state) => ("lccode",cmd));
    let i = catch_prim!(gullet.get_int(state) => ("mathcode",cmd));
    debug_log!(debug=>"\\mathcode '{}' = {}",c.char_str(),i);
    state.set_mathcode(c,i,global);
    Ok(())
}

pub fn mathcode_get<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>)
                                 -> Result<ET::Int,TeXError<ET::Token>> {
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

pub fn meaning<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,f:TokenCont<ET>)
    -> Result<(),TeXError<ET::Token>> {
    debug_log!(trace=>"meaning");
    todo!()
    /*
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

     */
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

/*
pub fn meaning_cmd<ET:EngineType>(cmd:Option<&ET::Command>, state:&ET::State) -> Vec<ET::Token> {
todo!()
match cmd {
    None => string_to_tokens("undefined".as_bytes()),
    Some(cmd) => {
        match cmd.base() {
            BaseCommand::Def(d) => {
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
            BaseCommand::Unexpandable {name,..} => {
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
            BaseCommand::OpenBox {name,..} => {
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
            BaseCommand::AssignableValue {name,..} => {
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
            BaseCommand::Value {name,..} => {
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
            BaseCommand::ValueRegister {tp:Assignable::Int,index} => {
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
            BaseCommand::ValueRegister {tp:Assignable::Dim,index} => {
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
            BaseCommand::ValueRegister {tp:Assignable::Skip,index} => {
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
            BaseCommand::ValueRegister {tp:Assignable::MuSkip,index} => {
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
            BaseCommand::ValueRegister {tp:Assignable::Font,index} => {
                let mut string = vec!();
                for u in "select font ".as_bytes() {string.push(*u)}
                for u in state.fontstore().get(*index).to_string().as_bytes() {string.push(*u)}
                string_to_tokens(&string)
            }
            BaseCommand::ValueRegister {..} => todo!("meaning_cmd: ValueRegister"),
            BaseCommand::ValueAssignment {name,..} => {
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
            BaseCommand::Assignment {name,..} => {
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
            BaseCommand::Relax => {
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
            BaseCommand::Conditional {name,..} => {
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
            BaseCommand::Expandable {name,..} => {
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
            BaseCommand::Char {char,catcode} => meaning_char(*char, *catcode),
            BaseCommand::MathChar(index) => {
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
            BaseCommand::Whatsit {name,..} => {
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
 */

pub fn message<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>)
    -> Result<(),TeXError<ET::Token>> {
    debug_log!(debug=>"message");
    catch_prim!(gullet.mouth().skip_whitespace::<ET>(state) => ("message",cmd));
    let msg = String::from_utf8(catch_prim!(gullet.get_braced_string(state) => ("message",cmd))).unwrap();
    (state.outputs().message)(&msg);
    Ok(())
}

pub fn month<ET:EngineType>(state:&mut ET::State,cmd:CommandSource<ET>) -> Result<ET::Int,TeXError<ET::Token>> {
    Ok(catch_prim!(ET::Int::from_i64(
        state.get_start_time().month() as i64
    ) => ("month",cmd)))
}

pub static MULTIPLY:&str = "multiply";
pub fn multiply<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,global:bool)
                                                   -> Result<(),TeXError<ET::Token>> {
    debug_log!(trace=>"\\multiply");
    todo!()
    /*
    catch_prim!(gullet.mouth().skip_whitespace::<ET>(state) => (MULTIPLY,cmd));
    match catch_prim!(gullet.get_next_unexpandable(state) => (MULTIPLY,cmd)) {
        None => file_end_prim!(MULTIPLY,cmd),
        Some(cmd) => match cmd.command.base() {
            BaseCommand::Int{name,index,..}  if *name == COUNT => {
                let u = match index {
                    Some(u) => *u,
                    None => {
                        let int = catch_prim!(gullet.get_int(state) => (MULTIPLY,cmd));
                        match int.try_into() {
                            Ok(u) => u,
                            _ => throw!("Not a valid register: {}",int => cmd.cause)
                        }
                    }
                };
                catch_prim!(gullet.get_keyword(state,"by") => (MULTIPLY,cmd));
                let i = catch_prim!(gullet.get_int(state) => (MULTIPLY,cmd));
                debug_log!(debug => "  \\count{}",u);
                let ov = state.get_int_register(u);
                debug_log!(debug => "  =={}*{}",ov,i);
                let nv : ET::Int = ov * i;
                debug_log!(debug => "  ={}",nv);
                state.set_int_register(u,nv,global);
                return Ok(())
            }
            BaseCommand::Int{name,index:None,..} => { // TODO this covers potentially too many cases
            debug_log!(debug => "  \\{}",name);
                catch_prim!(gullet.get_keyword(state,"by") => (MULTIPLY,cmd));
                let ov = state.get_primitive_int(name);
                let i = catch_prim!(gullet.get_int(state) => (MULTIPLY,cmd));
                debug_log!(debug => "  =={}*{}",ov,i);
                let nv : ET::Int = ov * i;
                debug_log!(debug => "  ={}",nv);
                state.set_primitive_int(name,nv,global);
                return Ok(())
            }
            BaseCommand::Dim{name,index,..}  if *name == DIMEN => {
                let u = match index {
                    Some(u) => *u,
                    None => {
                        let int = catch_prim!(gullet.get_int(state) => (MULTIPLY,cmd));
                        match int.try_into() {
                            Ok(u) => u,
                            _ => throw!("Not a valid register: {}",int => cmd.cause)
                        }
                    }
                };
                catch_prim!(gullet.get_keyword(state,"by") => (MULTIPLY,cmd));
                let i = catch_prim!(gullet.get_int(state) => (MULTIPLY,cmd)).to_i64();
                debug_log!(debug => "  \\count{}",u);
                let ov = state.get_dim_register(u);
                debug_log!(debug => "  =={}*{}",ov,i);
                let nv = ov.tex_mult(i as f64);
                debug_log!(debug => "  ={}",nv);
                state.set_dim_register(u,nv,global);
                return Ok(())
            }
            BaseCommand::Dim{name,index:None,..} => { // TODO this covers potentially too many cases
            debug_log!(debug => "  \\{}",name);
                catch_prim!(gullet.get_keyword(state,"by") => (MULTIPLY,cmd));
                let ov = state.get_primitive_dim(name);
                let i = catch_prim!(gullet.get_int(state) => (MULTIPLY,cmd)).to_i64();
                debug_log!(debug => "  =={}*{}",ov,i);
                let nv = ov.tex_mult(i as f64);
                debug_log!(debug => "  ={}",nv);
                state.set_primitive_dim(name,nv,global);
                return Ok(())
            }
            o => throw!("expected register after \\multiply;got:{:?}",o => cmd.cause)
        }
    }

     */
}


pub fn muskip_assign<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,global:bool)
    -> Result<(),TeXError<ET::Token>> {
    debug_log!(trace=>"Assigning \\muskip");
    let i = catch_prim!(gullet.get_int(state) => ("muskip",cmd));
    let i:usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => throw!("Not a valid register: {}",i => cmd.cause)
    };
    catch_prim!(gullet.mouth().skip_eq_char::<ET>(state) => ("muskip",cmd));
    let v = catch_prim!(gullet.get_muskip(state) => ("muskip",cmd));
    debug_log!(debug=>"\\muskip{} = {}",i,v);
    state.set_muskip_register(i,v,global);
    Ok(())
}

pub fn muskip_get<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>) ->
                                                                                                           Result<MuSkip<ET::MuDim,ET::MuStretchShrinkDim>,TeXError<ET::Token>> {
    debug_log!(trace=>"Getting \\muskip");
    let i = catch_prim!(gullet.get_int(state) => ("muskip",cmd));
    let i:usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => throw!("Not a valid register: {}",i => cmd.cause)
    };
    let v = state.get_muskip_register(i);
    debug_log!(debug=>"\\muskip{} == {}",i,v);
    Ok(v)
}

pub fn muskipdef<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,global:bool)
                                                     -> Result<(),TeXError<ET::Token>> {
    debug_log!(trace=>"muskipdef");
    /*
    let name = catch_prim!(gullet.get_control_sequence(state) => ("muskipdef",cmd));
    match &name {
        BaseToken::Char(c,_) => {
            state.set_ac_command(*c, Some(Ptr::new(BaseCommand::Relax)), false)
        }
        BaseToken::CS(name) => {
            state.set_command(name.clone(), Some(Ptr::new(BaseCommand::Relax)), false)
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
            state.set_ac_command(c, Some(Ptr::new(BaseCommand::ValueRegister{index:num,tp:Assignable::MuSkip })), global);
        }
        BaseToken::CS(name) => {
            debug_log!(debug=>"\\muskipdef: \\{} = {}",name,num);
            state.set_command(name, Some(Ptr::new(BaseCommand::ValueRegister{index:num,tp:Assignable::MuSkip})), global);
        }
    }

     */
    Ok(())
}


pub fn newlinechar_assign<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,global:bool)
    -> Result<(),TeXError<ET::Token>> {
    debug_log!(trace=>"Assigning \\newlinechar");
    catch_prim!(gullet.mouth().skip_eq_char::<ET>(state) => ("newlinechar",cmd));
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
    -> Result<ET::Int,TeXError<ET::Token>> {
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
pub fn noexpand<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,f:TokenCont<ET>)
    -> Result<(),TeXError<ET::Token>> {
    debug_log!(trace=>"\\noexpand");
    todo!()
    /*
    match catch_prim!(gullet.mouth().get_next::<ET>(state) => ("noexpand",cmd)) {
        None => file_end_prim!("noexpand",cmd),
        Some((t,_)) => match t.base() {
            BaseToken::Char(c,CategoryCode::Active) => {
                match state.get_ac_command(*c) {
                    None => gullet.mouth().requeue(t),
                    Some(ac) => {
                        match &*ac {
                            BaseCommand::Def(_, _) => gullet.mouth().push_noexpand(t),
                            BaseCommand::Expandable {..} => gullet.mouth().push_noexpand(t),
                            BaseCommand::Conditional {..} => gullet.mouth().push_noexpand(t),
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
                            BaseCommand::Def(_, _) => gullet.mouth().push_noexpand(t),
                            BaseCommand::Expandable {..} => gullet.mouth().push_noexpand(t),
                            BaseCommand::Conditional {..} => gullet.mouth().push_noexpand(t),
                            _ => gullet.mouth().requeue(t)
                        }
                    }
                }
            }
            _ => gullet.mouth().requeue(t)
        }
    }
    Ok(vec!())

     */
}

pub fn number<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,f:TokenCont<ET>)
    -> Result<(),TeXError<ET::Token>> {
    debug_log!(trace=>"\\number");
    let num = catch_prim!(gullet.get_int(state) => ("number",cmd));
    for t in string_to_tokens(num.to_i64().to_string().as_bytes()) {f(state,t)?}
    Ok(())
}

pub fn openin<ET:EngineType>(state: &mut ET::State, gullet:&mut ET::Gullet, cmd:CommandSource<ET>)
    -> Result<(), TeXError<ET::Token>> {
    debug_log!(trace=>"\\openin");
    let i = catch_prim!(gullet.get_int(state) => ("openin",cmd));
    let i : usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => throw!("Invalid file number: {}",i => cmd.cause)
    };
    catch_prim!(gullet.mouth().skip_eq_char::<ET>(state) => ("openin",cmd));
    let filename = catch_prim!(gullet.get_string(state) => ("openin",cmd)).to_string();
    let f = state.filesystem().get(&filename);
    state.file_openin(i,f); // TODO error?
    Ok(())
}

pub fn openout<ET:EngineType>(state: &mut ET::State, gullet:&mut ET::Gullet, cmd:CommandSource<ET>)
    -> Result<Whatsit<ET>, TeXError<ET::Token>> {
    debug_log!(trace=>"\\openout");
    let i = catch_prim!(gullet.get_int(state) => ("openout",cmd));
    let i : usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => throw!("Invalid file number: {}",i => cmd.cause)
    };
    catch_prim!(gullet.mouth().skip_eq_char::<ET>(state) => ("openout",cmd));
    let filename = catch_prim!(gullet.get_string(state) => ("openout",cmd)).to_string();
    let apply = Box::new(move |_stomach:&mut ET::Stomach,state:&mut ET::State,_gullet:&mut ET::Gullet| {
        let f = state.filesystem().get(&filename);
        state.file_openout(i,f); // TODO error?
        Ok(())
    });
    Ok(Whatsit::new(apply))
}

pub fn or<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>)
    -> Result<(),TeXError<ET::Token>> {
    match gullet.current_conditional() {
        (Some(ConditionalBranch::Case(_,_)),i) =>
            catch_prim!(crate::engine::gullet::methods::else_loop::<ET>(gullet,state,"ifcase",i,true) => ("or",cmd)),
        _ => throw!("Not in an \\ifcase" => cmd.cause)
    }
    Ok(())
}

pub fn outer<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,global_:bool,protected_:bool,long_:bool,outer_:bool)
                                                      -> Result<(),TeXError<ET::Token>> {
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

pub fn par<ET:EngineType>(state:&mut ET::State,_cmd:CommandSource<ET>) -> Result<(),TeXError<ET::Token>> {
    debug_log!(trace=>"par");
    if state.mode().is_vertical() {Ok(())} else {
        todo!("par in horizontal mode")
    }
}

pub fn patterns<ET:EngineType>(state: &mut ET::State, gullet:&mut ET::Gullet, cmd:CommandSource<ET>)
                             -> Result<(), TeXError<ET::Token>> {
    debug_log!(trace=>"\\patterns");
    // TODO
    catch_prim!(gullet.mouth().read_argument::<ET>(state,&mut |_,_| Ok(())) => ("patterns",cmd));
    Ok(())
}

pub fn read<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,globally:bool)
    -> Result<(),TeXError<ET::Token>> {
    debug_log!(trace=>"read");
    todo!()
    /*
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
    let mut ret = catch_prim!(file.read::<ET::Token>(state.get_catcode_scheme(),state.get_endlinechar()) => ("read",cmd));
    debug_log!(trace=>"read: {} = {}",newcmd,TokenList(&ret));
    if ret.is_empty() {
        match state.get_endlinechar() {
            None => (),
            Some(c) => ret.push(ET::Token::new(BaseToken::Char(c,*state.get_catcode_scheme().get(&c)),None))
        }
    }

/*
    if TokenList(ret.clone()).to_string().starts_with("102A0;CARIAN LETTER A") {
        println!("Here!");
        std::env::set_var("RUST_LOG","debug,tex_engine::tex::commands=trace,tex_engine::engine::gullet=trace");
        env_logger::init();
    }
*/


    let ret = ret.into_iter().map(|tk| ExpToken::Token(tk)).collect();
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
            BaseCommand::Def(def, cmd.cause.clone())
        )),globally),
        BaseToken::Char(c,_) => state.set_ac_command(c,Some(Ptr::new(
            BaseCommand::Def(def, cmd.cause.clone())
        )),globally)
    }
    Ok(())

     */
}

pub fn romannumeral<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,f:TokenCont<ET>)
    -> Result<(),TeXError<ET::Token>> {
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
    for t in string_to_tokens(&ret) {f(state,t)?}
    Ok(())
}

pub fn setbox<ET:EngineType>(state:&mut ET::State, gullet:&mut ET::Gullet, cmd:CommandSource<ET>, global:bool)
    -> Result<(),TeXError<ET::Token>> {
    debug_log!(trace=>"\\setbox");
    todo!()
    /*
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
                state.box_stack_mut().push(OpenBox::Box {list:vec!(),mode,on_close:Ptr::new(move |sto,s,gu,v| {
                    let bx = match f(sto,s,gu,v) {
                        Some(r) => {r}
                        None => {todo!("make void box")}
                    };
                    s.set_box_register(i as usize,bx,global);
                    None
                })});
                Ok(())
            }
            _ => Err(ErrorInPrimitive{name:"setbox",msg:Some(format!("Box expected: {}",c.cause)),cause:Some(cmd.cause),source:None})
        }
    }

     */

}

pub fn sfcode_assign<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,global:bool)
    -> Result<(),TeXError<ET::Token>> {
    debug_log!(trace=>"Assigning space factor code");
    let i = catch_prim!(gullet.get_int(state) => ("sfcode",cmd));
    let c: ET::Char = match ET::Char::from_i64(i.to_i64()) {
        Some(i) => i,
        None => throw!("Not a valid character: {}",i => cmd.cause)
    };
    catch_prim!(gullet.mouth().skip_eq_char::<ET>(state) => ("catcode",cmd));
    let v = catch_prim!(gullet.get_int(state) => ("catcode",cmd));
    debug_log!(debug=>"\\sfcode '{}' = {}",c.char_str(),v);
    state.set_sfcode(c,v,global);
    Ok(())
}
pub fn sfcode_get<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>)
    -> Result<ET::Int,TeXError<ET::Token>> {
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
                                        -> Result<(),TeXError<ET::Token>> {
    debug_log!(trace=>"Assigning \\kewchar");
    let mut font = catch_prim!(gullet.get_font(state) => ("skewchar",cmd));
    catch_prim!(gullet.mouth().skip_eq_char::<ET>(state) => ("skewchar",cmd));
    let i = catch_prim!(gullet.get_int(state) => ("skewchar",cmd)).to_i64();
    debug_log!(debug=>"\\skewchar\\{:?} = {:?}",font,i);
    font.set_skewchar(i);
    Ok(())
}
pub fn skewchar_get<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>) -> Result<ET::Int,TeXError<ET::Token>> {
    debug_log!(trace=>"Getting \\skewchar");
    let font = catch_prim!(gullet.get_font(state) => ("skewchar",cmd));
    debug_log!(debug=>"\\skewchar == {:?}",font.get_skewchar());
    Ok(catch_prim!(ET::Int::from_i64::<ET::Token>(font.get_skewchar()) => ("hyphenchar",cmd)))
}

pub fn skip_assign<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,global:bool)
    -> Result<(),TeXError<ET::Token>> {
    debug_log!(trace=>"Assigning \\skip");
    let i = catch_prim!(gullet.get_int(state) => ("skip",cmd));
    let i:usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => throw!("Not a valid register: {}",i => cmd.cause)
    };
    catch_prim!(gullet.mouth().skip_eq_char::<ET>(state) => ("skip",cmd));
    let v = catch_prim!(gullet.get_skip(state) => ("skip",cmd));
    debug_log!(debug=>"\\skip{} = {}",i,v);
    state.set_skip_register(i,v,global);
    Ok(())
}

pub fn skip_get<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>)
    -> Result<Skip<ET::SkipDim>,TeXError<ET::Token>> {
    debug_log!(trace=>"Getting \\skip");
    let i = catch_prim!(gullet.get_int(state) => ("skip",cmd));
    let i:usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => throw!("Not a valid register: {}",i => cmd.cause)
    };
    let v = state.get_skip_register(i);
    debug_log!(debug=>"\\skip{} == {}",i,v);
    Ok(v)
}

pub fn skipdef<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,global:bool)
                                                     -> Result<(),TeXError<ET::Token>> {
    debug_log!(trace=>"skipdef");
    /*
    let name = catch_prim!(gullet.get_control_sequence(state) => ("skipdef",cmd));
    match &name {
        BaseToken::Char(c,_) => {
            state.set_ac_command(*c, Some(Ptr::new(BaseCommand::Relax)), false)
        }
        BaseToken::CS(name) => {
            state.set_command(name.clone(), Some(Ptr::new(BaseCommand::Relax)), false)
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
            state.set_ac_command(c, Some(Ptr::new(BaseCommand::ValueRegister{index:num,tp:Assignable::Skip })), global);
        }
        BaseToken::CS(name) => {
            debug_log!(debug=>"\\skipdef: \\{} = {}",name,num);
            state.set_command(name, Some(Ptr::new(BaseCommand::ValueRegister{index:num,tp:Assignable::Skip})), global);
        }
    }

     */
    Ok(())
}

pub fn string<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,f:TokenCont<ET>)
    -> Result<(),TeXError<ET::Token>> {
    debug_log!(trace=>"string");
    match catch_prim!(gullet.mouth().get_next::<ET>(state) => ("string",cmd)) {
        None => file_end_prim!("string",cmd),
        Some((t,_)) => token_to_chars(t,state.get_escapechar(),&mut |t| f(state,t))?
    }
    Ok(())
}

pub static THE : &str = "the";
pub fn the<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,f:TokenCont<ET>)
    -> Result<(),TeXError<ET::Token>> {
    debug_log!(trace => "\\the");
    todo!()
    /*
    let next = catch_prim!(gullet.get_next_stomach_command(state) => (THE,cmd));
    match next {
        None => file_end_prim!(THE,cmd),
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
            StomachCommandInner::ValueRegister(i,Assignable::Dim) => {
                let val = state.get_dim_register(i);
                let str = format!("{}",val);
                debug_log!(debug => "the: {}",str);
                let ret = gullet::methods::string_to_tokens::<ET::Token>(&str.as_bytes());
                Ok(ret)
            }
            StomachCommandInner::ValueRegister(i,Assignable::Skip) => {
                let val = state.get_skip_register(i);
                let str = format!("{}",val);
                debug_log!(debug => "the: {}",str);
                let ret = gullet::methods::string_to_tokens::<ET::Token>(&str.as_bytes());
                Ok(ret)
            }
            StomachCommandInner::ValueRegister(_,tp) => todo!("\\the ValueRegister {:?}",tp),
            StomachCommandInner::AssignableValue {tp:Assignable::Toks,name} =>
                Ok(state.get_primitive_toks(name)),
            StomachCommandInner::AssignableValue {tp:Assignable::Int,name} => {
                let val = state.get_primitive_int(name);
                let str = format!("{}",val);
                debug_log!(debug => "the: {}",str);
                let ret = gullet::methods::string_to_tokens::<ET::Token>(&str.as_bytes());
                Ok(ret)
            }
            StomachCommandInner::AssignableValue {tp:Assignable::Dim,name} => {
                let val = state.get_primitive_dim(name);
                let str = format!("{}",val);
                debug_log!(debug => "the: {}",str);
                let ret = gullet::methods::string_to_tokens::<ET::Token>(&str.as_bytes());
                Ok(ret)
            }
            StomachCommandInner::AssignableValue {tp:Assignable::Skip,name} => {
                let val = state.get_primitive_skip(name);
                let str = format!("{}",val);
                debug_log!(debug => "the: {}",str);
                let ret = gullet::methods::string_to_tokens::<ET::Token>(&str.as_bytes());
                Ok(ret)
            }
            StomachCommandInner::AssignableValue {tp,..} => todo!("\\the AssignableValue {:?}",tp),
            StomachCommandInner::Value {name,index,tp:Assignable::Int} => {
                match gullet.primitive_int(index) {
                    None =>return Err(ErrorInPrimitive{name:THE,msg:None,cause:Some(cmd.cause),source:Some(
                        ImplementationError(format!("Missing implementation for {}",name),PhantomData).into()
                    )}),
                    Some(fun) => {
                        let val = catch_prim!(fun(state,gullet,cmd.clone()) => (THE,cmd));
                        let str = format!("{}",val);
                        debug_log!(debug => "the: {}",str);
                        let ret = gullet::methods::string_to_tokens::<ET::Token>(&str.as_bytes());
                        Ok(ret)
                    }
                }
            }
            StomachCommandInner::Value {name,index,tp:Assignable::Dim} => {
                match gullet.primitive_dim(index) {
                    None =>return Err(ErrorInPrimitive{name:THE,msg:None,cause:Some(cmd.cause),source:Some(
                        ImplementationError(format!("Missing implementation for {}",name),PhantomData).into()
                    )}),
                    Some(fun) => {
                        let val = catch_prim!(fun(state,gullet,cmd.clone()) => (THE,cmd));
                        let str = format!("{}",val);
                        debug_log!(debug => "the: {}",str);
                        let ret = gullet::methods::string_to_tokens::<ET::Token>(&str.as_bytes());
                        Ok(ret)
                    }
                }
            }
            StomachCommandInner::Value {name,index,tp:Assignable::Skip} => {
                match gullet.primitive_skip(index) {
                    None =>return Err(ErrorInPrimitive{name:THE,msg:None,cause:Some(cmd.cause),source:Some(
                        ImplementationError(format!("Missing implementation for {}",name),PhantomData).into()
                    )}),
                    Some(fun) => {
                        let val = catch_prim!(fun(state,gullet,cmd.clone()) => (THE,cmd));
                        let str = format!("{}",val);
                        debug_log!(debug => "the: {}",str);
                        let ret = gullet::methods::string_to_tokens::<ET::Token>(&str.as_bytes());
                        Ok(ret)
                    }
                }
            }
            StomachCommandInner::Value {name,index,tp:Assignable::MuSkip} => {
                match gullet.primitive_muskip(index) {
                    None =>return Err(ErrorInPrimitive{name:THE,msg:None,cause:Some(cmd.cause),source:Some(
                        ImplementationError(format!("Missing implementation for {}",name),PhantomData).into()
                    )}),
                    Some(fun) => {
                        let val = catch_prim!(fun(state,gullet,cmd.clone()) => (THE,cmd));
                        let str = format!("{}",val);
                        debug_log!(debug => "the: {}",str);
                        let ret = gullet::methods::string_to_tokens::<ET::Token>(&str.as_bytes());
                        Ok(ret)
                    }
                }
            }
            StomachCommandInner::ValueAssignment {name,value_index,tp:Assignable::Int,..} => {
                match gullet.primitive_int(value_index) {
                    None =>return Err(ErrorInPrimitive{name:THE,msg:None,cause:Some(cmd.cause),source:Some(
                        ImplementationError(format!("Missing implementation for {}",name),PhantomData).into()
                    )}),
                    Some(fun) => {
                        let val = catch_prim!(fun(state,gullet,cmd.clone()) => (THE,cmd));
                        let str = format!("{}",val);
                        debug_log!(debug => "the: {}",str);
                        let ret = gullet::methods::string_to_tokens::<ET::Token>(&str.as_bytes());
                        Ok(ret)
                    }
                }
            }
            StomachCommandInner::Assignment {name:"toks",index} => {
                let i = catch_prim!(gullet.get_int(state) => ("toks",cmd));
                let i:usize = match i.clone().try_into() {
                    Ok(i) => i,
                    Err(_) => return Err(ErrorInPrimitive{name:"toks",msg:Some(format!("Not a valid register: {}",i)),cause:Some(cmd.cause),source:None})
                };
                Ok(state.get_toks_register(i))
            }
            StomachCommandInner::Value {name,index,tp:_} => {
                todo!()
            }
            StomachCommandInner::ValueAssignment {..} => todo!(),
            StomachCommandInner::Char {char,from_chardef:true} => {
                let ret = gullet::methods::string_to_tokens::<ET::Token>(char.to_usize().to_string().as_bytes());
                Ok(ret)
            }
            _ => return Err(ErrorInPrimitive{name:THE,msg:Some("Expected a value after \\the".to_string()),cause:Some(cmd.cause),source:None})
        }
    }

     */
}

pub fn time<ET:EngineType>(state:&mut ET::State,cmd:CommandSource<ET>) -> Result<ET::Int,TeXError<ET::Token>> {
    let t = state.get_start_time();
    Ok(catch_prim!(ET::Int::from_i64( ((t.hour() * 60) + t.minute()) as i64 ) => ("time",cmd)))
}


pub fn toks<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,global:bool)
    -> Result<(),TeXError<ET::Token>> {
    debug_log!(trace=>"Assigning \\toks");
    let i = catch_prim!(gullet.get_int(state) => ("toks",cmd));
    let i:usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => throw!("Not a valid register: {}",i => cmd.cause)
    };
    catch_prim!(gullet.mouth().skip_eq_char::<ET>(state) => ("toks",cmd));
    match catch_prim!(gullet.mouth().get_next::<ET>(state) => ("toks",cmd)) {
        None => file_end_prim!("toks",cmd),
        Some((t,_)) if t.catcode() == CategoryCode::BeginGroup => (),
        Some((o,_)) => throw!("Expected begin group token after \\toks, got: {}",o => cmd.cause)
    }
    let mut v = vec!();
    catch_prim!(gullet.mouth().read_until_endgroup::<ET>(state,&mut |_,t| Ok(v.push(t))) => ("toks",cmd));
    debug_log!(debug=>"\\toks{} = {}",i,TokenList(&v));
    state.set_toks_register(i,v,global);
    Ok(())
}

pub fn toksdef<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,global:bool)
                                                      -> Result<(),TeXError<ET::Token>> {
    debug_log!(trace=>"toksdef");
    todo!()
    /*
    let name = catch_prim!(gullet.get_control_sequence(state) => ("toksdef",cmd));
    match &name {
        BaseToken::Char(c,_) => {
            state.set_ac_command(*c, Some(Ptr::new(BaseCommand::Relax)), false)
        }
        BaseToken::CS(name) => {
            state.set_command(name.clone(), Some(Ptr::new(BaseCommand::Relax)), false)
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
            state.set_ac_command(c, Some(Ptr::new(BaseCommand::ValueRegister{index:num,tp:Assignable::Toks })), global);
        }
        BaseToken::CS(name) => {
            debug_log!(debug=>"\\toksdef: \\{} = {}",name,num);
            state.set_command(name, Some(Ptr::new(BaseCommand::ValueRegister{index:num,tp:Assignable::Toks})), global);
        }
    }
    Ok(())

     */
}

pub fn uccode_assign<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,global:bool)
    -> Result<(),TeXError<ET::Token>> {
    debug_log!(trace=>"Assigning upper case character");
    let i = catch_prim!(gullet.get_int(state) => ("uccode",cmd));
    let c: ET::Char = match ET::Char::from_i64(i.to_i64()) {
        Some(i) => i,
        None => throw!("Not a valid character: {}",i => cmd.cause)
    };
    catch_prim!(gullet.mouth().skip_eq_char::<ET>(state) => ("uccode",cmd));
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
    -> Result<ET::Int,TeXError<ET::Token>> {
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
                                         -> Result<(),TeXError<ET::Token>> {
    debug_log!(trace => "\\uppercase");
    let mut ret = vec!();
    catch_prim!(gullet.get_group(state,&mut |s,next| match next.base() {
        BaseToken::Char(c,cc) => {
            let nc = s.get_uccode(c);
            Ok(ret.push(ET::Token::new(BaseToken::Char(nc, *cc), None)))
        }
        _ => Ok(ret.push(next))

    }) => (UPPERCASE,cmd));
    gullet.mouth().push_tokens(ret);
    Ok(())
}

pub fn write<ET:EngineType>(state: &mut ET::State, gullet:&mut ET::Gullet, cmd:CommandSource<ET>)
    -> Result<Whatsit<ET>, TeXError<ET::Token>> {
    debug_log!(trace=>"\\write");
    let i = catch_prim!(gullet.get_int(state) => ("write",cmd));
    let i = i.to_i64();

    match catch_prim!(gullet.mouth().get_next::<ET>(state) => ("write",cmd)) {
        None => file_end_prim!("write",cmd),
        Some((t,_)) if t.catcode() == CategoryCode::BeginGroup => (),
        Some((o,_)) => throw!("Expected begin group token after \\write, got: {}",o => cmd.cause)
    }
    let mut tks = vec!();
    catch_prim!(gullet.mouth().read_until_endgroup::<ET>(state,&mut |_,t| Ok(tks.push(t))) => ("write",cmd));

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
    Ok(Whatsit::new(apply))
}

pub fn xdef<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,global:bool,protected:bool,long:bool,outer:bool)
                                                 -> Result<(),TeXError<ET::Token>> {
    edef::<ET>(state,gullet,cmd,true,protected,long,outer)
}

pub fn year<ET:EngineType>(state:&mut ET::State,cmd:CommandSource<ET>) -> Result<ET::Int,TeXError<ET::Token>> {
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
    register_unexpandable!(afterassignment,state,stomach,gullet,(s,gu,cmd) =>afterassignment::<ET>(s,gu,cmd));
    register_skip_assign!(baselineskip,state,stomach,gullet);
    register_unexpandable!(begingroup,state,stomach,gullet,(s,_,_) =>begingroup::<ET>(s));
    register_skip_assign!(belowdisplayskip,state,stomach,gullet);
    register_skip_assign!(belowdisplayshortskip,state,stomach,gullet);
    register_int_assign!(binoppenalty,state,stomach,gullet);
    register_dim_assign!(boxmaxdepth,state,stomach,gullet);
    register_int_assign!(brokenpenalty,state,stomach,gullet);
    register_value_assign_int!(catcode,state,stomach,gullet);
    register_assign!(chardef,state,stomach,gullet,(s,gu,cmd,global) =>chardef::<ET>(s,gu,cmd,global));
    register_unexpandable!(closein,state,stomach,gullet,(s,gu,cmd) =>closein::<ET>(s,gu,cmd));
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
    register_unexpandable!(dump,state,stomach,gullet,(s,sto,cmd) =>dump::<ET>());
    register_assign!(edef,state,stomach,gullet,(s,gu,cmd,global) =>edef::<ET>(s,gu,cmd,global,false,false,false));
    register_expandable!(else,state,stomach,gullet,(s,gu,cmd,f) =>else_::<ET>(s,gu,cmd));
    register_dim_assign!(emergencystretch,state,stomach,gullet);
    register_unexpandable!(end,state,stomach,gullet,(_,_,_) =>end::<ET>());
    register_unexpandable!(endcsname,state,stomach,gullet,(_,_,cmd) =>endcsname::<ET>(cmd));
    register_expandable!(endinput,state,stomach,gullet,(s,g,c,f) => endinput::<ET>(s,g,c));
    register_unexpandable!(endgroup,state,stomach,gullet,(s,gu,cmd) =>endgroup::<ET>(s,gu,cmd));
    register_value_assign_int!(endlinechar,state,stomach,gullet);
    register_tok_assign!(errhelp,state,stomach,gullet);

    let em = Some(Command::new(BaseCommand::Unexpandable {
        name:ERRMESSAGE,
        apply:|s,gu,cmd| errmessage::<ET>(s,gu,cmd)
    },None));
    state.set_command(ET::Char::from_str("errmessage"),em.clone(),true);
    state.set_command(ET::Char::from_str("LaTeX3 error:"),em,true);

    register_int_assign!(errorcontextlines,state,stomach,gullet);
    register_unexpandable!(errorstopmode,state,stomach,gullet,(s,gu,cmd) =>errorstopmode::<ET>());
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
    register_dim_assign!(hsize,state,stomach,gullet);
    register_unexpandable!(hyphenation,state,stomach,gullet,(s,gu,cmd) =>hyphenation::<ET>(s,gu,cmd));
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
    register_conditional!(ifmmode,state,stomach,gullet,(s,gu,cmd) =>todo!("ifmmode"));
    register_conditional!(ifnum,state,stomach,gullet,(s,gu,cmd) =>ifnum::<ET>(s,gu,cmd));
    register_conditional!(ifodd,state,stomach,gullet,(s,gu,cmd) =>ifodd::<ET>(s,gu,cmd));
    register_conditional!(iftrue,state,stomach,gullet,(s,gu,cmd) => Ok(true));
    register_conditional!(ifvbox,state,stomach,gullet,(s,gu,cmd) =>todo!("ifvbox"));
    register_conditional!(ifvmode,state,stomach,gullet,(s,gu,cmd) =>ifvmode::<ET>(s,gu,cmd));
    register_conditional!(ifvoid,state,stomach,gullet,(s,gu,cmd) =>todo!("ifvoid"));
    register_conditional!(ifx,state,stomach,gullet,(s,gu,cmd) =>ifx::<ET>(s,gu,cmd));
    register_unexpandable!(immediate,state,stomach,gullet,(s,gu,cmd) =>immediate::<ET>(s,gu,cmd));
    register_unexpandable!(ignorespaces,state,stomach,gullet,(s,gu,cmd) => ignorespaces::<ET>(s,gu,cmd));
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
    register_unexpandable!(lowercase,state,stomach,gullet,(s,gu,cmd) =>lowercase::<ET>(s,gu,cmd));
    register_int_assign!(looseness,state,stomach,gullet);
    register_int_assign!(mag,state,stomach,gullet);
    register_int_assign!(maxdeadcycles,state,stomach,gullet);
    register_dim_assign!(maxdepth,state,stomach,gullet);
    register_assign!(mathchardef,state,stomach,gullet,(s,gu,cmd,global) =>mathchardef::<ET>(s,gu,cmd,global));
    register_value_assign_int!(mathcode,state,stomach,gullet);
    register_dim_assign!(mathsurround,state,stomach,gullet);
    register_expandable!(meaning,state,stomach,gullet,(s,g,c,f) => meaning::<ET>(s,g,c,f));
    register_unexpandable!(message,state,stomach,gullet,(s,gu,cmd) =>message::<ET>(s,gu,cmd));
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
    register_unexpandable!(openin,state,stomach,gullet,(s,gu,cmd) =>openin::<ET>(s,gu,cmd));
    register_whatsit!(openout,state,stomach,gullet,(s,gu,cmd) =>openout::<ET>(s,gu,cmd));
    register_expandable!(or,state,stomach,gullet,(s,g,c,f) => or::<ET>(s,g,c));
    register_assign!(outer,state,stomach,gullet,(s,gu,cmd,g) =>outer::<ET>(s,gu,cmd,g,false,false,false));
    register_tok_assign!(output,state,stomach,gullet);
    register_int_assign!(outputpenalty,state,stomach,gullet);
    register_dim_assign!(overfullrule,state,stomach,gullet);

    state.set_command(ET::Char::par_token(),Some(Command::new(BaseCommand::Unexpandable {
        name:"par",
        apply:|s,gu,cmd| par::<ET>(s,cmd)
    },None)),true);
    register_skip_assign!(parfillskip,state,stomach,gullet);
    register_dim_assign!(parindent,state,stomach,gullet);
    register_skip_assign!(parskip,state,stomach,gullet);
    register_unexpandable!(patterns,state,stomach,gullet,(s,gu,cmd) =>patterns::<ET>(s,gu,cmd));
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
    register_assign!(toks,state,stomach,gullet,(s,gu,cmd,global) =>toks::<ET>(s,gu,cmd,global));
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
    register_unexpandable!(uppercase,state,stomach,gullet,(s,gu,cmd) =>uppercase::<ET>(s,gu,cmd));
    register_int_assign!(vbadness,state,stomach,gullet);
    register_dim_assign!(vfuzz,state,stomach,gullet);
    register_dim_assign!(voffset,state,stomach,gullet);
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

