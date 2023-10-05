//! TeX primitive [`BaseCommand`]s

use std::hint::unreachable_unchecked;
use crate::{debug_log, register_assign, register_conditional, register_int_assign, register_unexpandable, register_tok_assign, register_int, register_whatsit, register_value_assign_int, register_value_assign_dim, register_value_assign_muskip, register_value_assign_skip, register_dim_assign, register_skip_assign, cmtodo, register_value_assign_font, register_open_box, cmstodo, register_muskip_assign, register_expandable, file_end, throw, catch_prim, file_end_prim, register_value_assign_toks, register_box, register_skip, register_expandable_notk, register_dim};
use crate::engine::filesystem::{File, FileSystem};
use crate::engine::gullet::Gullet;
use crate::engine::gullet::methods::resolve_token;
use crate::engine::state::State;
use crate::engine::mouth::{AlignSpec, Mouth};
use crate::engine::state::modes::{BoxMode, FontStyle, GroupType, TeXMode};
use crate::engine::stomach::Stomach;
use crate::tex::catcodes::CategoryCode;
use crate::tex::commands::{BaseCommand, ExpToken, ParamToken, Command, ResolvedToken, BaseStomachCommand, CloseBoxFun, ValueCommand, CommandSource, ToksCommand, Def};
use crate::tex::commands::methods::parse_signature;
use crate::tex::numbers::{Int, Skip, MuSkip, Dim, MuDim};
use crate::tex::token::{CSLike, Token, PrintableTokenList, DeTokenizer};
use crate::utils::errors::{TeXError};
use crate::utils::Ptr;
use crate::utils::strings::{AllCharsTrait, CharType, TeXStr};
use chrono::{Datelike, Timelike};
use log::debug;
use crate::engine::{EngineRef, EngineType};
use crate::tex::nodes::{HBox, HorV, HVBox, LeadersType, MathClass, NodeTrait, OpenBox, SimpleNode, SkipNode, TeXNode, VBox, Whatsit};
use crate::tex::ConditionalBranch;
use crate::tex::fonts::{FontStore,Font};
//use super::etex::protected;

/* TODO

SPACE
\/
\-
 */

pub fn SPACE<ET:EngineType>(engine:&mut EngineRef<ET>, _cmd:&CommandSource<ET>) {
    ET::Stomach::push_node(engine,SkipNode::Space.as_node());
}

pub const ADVANCE: &str = "advance";
pub fn advance<ET:EngineType>(engine: &mut EngineRef<ET>, cmd:&CommandSource<ET>, global:bool) {
    debug_log!(trace=>"\\advance");
    engine.skip_whitespace();
    match engine.get_next_unexpandable() {
        None => file_end!(),
        Some(ncmd) => {
            engine.skip_whitespace();engine.get_keyword("by");
            match ncmd.command {
                BaseCommand::Int(a) => {
                    macro_rules! finish {
                    ($old:expr,$nv:ident => $set:expr) => {{
                        let i = catch_prim!(engine.get_int() => (ADVANCE,cmd));
                        let $nv = $old + i;
                        $set;
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
                    }}
                }
                    match a {
                        ValueCommand::Register(u) => finish!(engine.state.get_dim_register(u),nv => engine.state.set_dim_register(u,nv,global)),
                        ValueCommand::Primitive(name) => finish!(engine.state.get_primitive_dim(name),nv => engine.state.set_primitive_dim(name,nv,global)),
                        ValueCommand::Complex {name,..} if name == DIMEN => {
                            let int = engine.get_int();
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
                    }}
                }
                    match a {
                        ValueCommand::Register(u) => finish!(engine.state.get_skip_register(u),nv => engine.state.set_skip_register(u,nv,global)),
                        ValueCommand::Primitive(name) => finish!(engine.state.get_primitive_skip(name),nv => engine.state.set_primitive_skip(name,nv,global)),
                        ValueCommand::Complex {name,..} if name == SKIP => {
                            let int = engine.get_int();
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
                    }}
                }
                    match a {
                        ValueCommand::Register(u) => finish!(engine.state.get_muskip_register(u),nv => engine.state.set_muskip_register(u,nv,global)),
                        ValueCommand::Primitive(name) => finish!(engine.state.get_primitive_muskip(name),nv => engine.state.set_primitive_muskip(name,nv,global)),
                        ValueCommand::Complex {name,..} if name == MUSKIP => {
                            let int = engine.get_int();
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
}

pub const AFTERASSIGNMENT: &str = "afterassignment";
pub fn afterassignment<ET:EngineType>(engine: &mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    debug_log!(trace=>"\\afterassignment");
    let next = match engine.get_next_token() {
        None => file_end!(cmd.cause),
        Some((t,_)) => t
    };
    engine.state.set_afterassignment(next);
}


pub const AFTERGROUP: &str = "aftergroup";
pub fn aftergroup<ET:EngineType>(engine: &mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    debug_log!(trace=>"\\aftergroup");
    let next = match engine.get_next_token() {
        None => file_end!(cmd.cause),
        Some((t,_)) => t
    };
    engine.state.push_aftergroup(next);
}


pub fn begingroup<ET:EngineType>(state:&mut ET::State) {
    state.stack_push(GroupType::CS);
}

pub fn box_<ET:EngineType>(engine:&mut EngineRef<ET>,cmd:&CommandSource<ET>) -> HVBox<ET> {
    debug_log!(trace=>"\\box");
    let i = engine.get_int();
    let i : usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => throw!("Invalid box number: {}",i => cmd.cause)
    };
    engine.state.take_box_register(i)
}

pub const CATCODE: &str = "catcode";
pub fn catcode_assign<ET:EngineType>(engine: &mut EngineRef<ET>, cmd:&CommandSource<ET>, global:bool) {
    debug_log!(trace=>"Assigning category code");
    let c = engine.get_char();
    engine.skip_eq_char();
    let v = engine.get_int().to_i64();
    if v < 0 || v > 15 {
        throw!("Invalid category code: {}",v => cmd.cause)
    }
    let cc: CategoryCode = (v as u8).try_into().unwrap();
    debug_log!(debug=>"\\catcode '{}' = {}",c.char_str(),cc);
    engine.state.set_catcode(c,cc,global);
}
pub fn catcode_get<ET:EngineType>(engine: &mut EngineRef<ET>, cmd:&CommandSource<ET>) -> ET::Int {
    debug_log!(trace=>"Getting category code");
    let c = engine.get_char();
    let cc = *engine.state.get_catcode_scheme().get(c);
    let v : u8 = cc.into();
    debug_log!(debug=>"\\catcode '{}' == {}",c.char_str(),cc);
    v.into()
}

pub const CHAR: &str = "char";
pub fn char<ET:EngineType>(engine: &mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    debug_log!(trace=>"\\char");
    let char = engine.get_char();
    ET::Stomach::push_node(engine,SimpleNode::Char {char, font:engine.state.get_current_font().clone(),cls:None}.as_node());
}

pub const CHARDEF: &str = "chardef";
pub fn chardef<ET:EngineType>(engine: &mut EngineRef<ET>, cmd:&CommandSource<ET>, global:bool) {
    debug_log!(trace=>"chardef");
    let name = engine.get_control_sequence();
    engine.skip_eq_char();
    let char = engine.get_char();
    let cmd = Command::new(BaseCommand::CharDef(char),Some(&cmd));
    engine.set_command_for_tk(name, Some(cmd), global);
}

pub const CLOSEIN: &str = "closein";
pub fn closein<ET:EngineType>(engine: &mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    debug_log!(trace=>"\\closein");
    let i = catch_prim!(engine.get_int() => (CLOSEIN,cmd));
    let i : usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => throw!("Invalid file number: {}",i => cmd.cause)
    };
    engine.state.file_closein(i);
}

pub const CLOSEOUT: &str = "closeout";
pub fn closeout<ET:EngineType>(engine: &mut EngineRef<ET>, cmd:&CommandSource<ET>) -> Whatsit<ET> {
    debug_log!(trace=>"\\closeout");
    let i = engine.get_int();
    let i : usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => throw!("Invalid file number: {}",i => cmd.cause)
    };
    let apply = Box::new(move |e: &mut EngineRef<ET>| {
        e.state.file_closeout(i); // TODO error?
    });
    Whatsit::new(apply)
}

pub fn copy<ET:EngineType>(engine:&mut EngineRef<ET>,cmd:&CommandSource<ET>) -> HVBox<ET> {
    debug_log!(trace=>"\\copy");
    let i = engine.get_int();
    let i : usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => throw!("Invalid box number: {}",i => cmd.cause)
    };
    match engine.state.get_box_register(i) {
        Some(b) => b.clone(),
        None => HVBox::Void
    }

}

pub const COUNT : &str = "count";
pub fn count_assign<ET:EngineType>(engine: &mut EngineRef<ET>, cmd:&CommandSource<ET>, global:bool) {
    debug_log!(trace=>"Assigning \\count");
    let i = engine.get_int();
    let i:usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => throw!("Not a valid register: {}",i => cmd.cause)
    };
    engine.skip_eq_char();
    let v = engine.get_int();
    debug_log!(debug=>"\\count{} = {}",i,v);
    engine.state.set_int_register(i,v,global);
}
pub fn count_get<ET:EngineType>(engine: &mut EngineRef<ET>, cmd:&CommandSource<ET>) -> ET::Int {
    debug_log!(trace=>"Getting \\count");
    let i = engine.get_int();
    let i:usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => throw!("Not a valid register: {}",i => cmd.cause)
    };
    let v = engine.state.get_int_register(i);
    debug_log!(debug=>"\\count{} == {}",i,v);
    v
}

pub const COUNTDEF : &str = "countdef";
pub fn countdef<ET:EngineType>(engine: &mut EngineRef<ET>, cmd:&CommandSource<ET>, global:bool) {
    debug_log!(trace=>"countdef");
    let name = engine.get_control_sequence();
    engine.set_relax(name,&cmd,global);
    engine.skip_eq_char();
    let num = engine.get_int();
    if num.to_i64() < 0 {
        throw!("Invalid count register index: {}",num => cmd.cause)
    }
    let num = num.to_i64() as usize;
    let cmd = Command::new(BaseCommand::Int(ValueCommand::Register(num)),Some(&cmd));
    engine.set_command_for_tk(name,Some(cmd),global);
}

pub const CR: &str = "cr";
pub fn cr<ET:EngineType>(engine: &mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    todo!("cr")
}

pub const CRCR: &str = "crcr";
pub fn crcr<ET:EngineType>(engine: &mut EngineRef<ET>, cmd:&CommandSource<ET>) {}

pub fn get_csname<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>, name:&'static str)
                                 -> TeXStr {
    debug_log!(trace=>"get_csname: {}",engine.preview(200));
    let csidx = engine.state.push_csname();
    let mut csname = engine.memory.get_string();
    while engine.state.current_csname() == Some(csidx) {
        match engine.get_next_unexpandable() {
            None => file_end!(cmd.cause),
            Some(sc) => match sc.command {
                BaseCommand::Unexpandable {name:"endcsname",..} => engine.state.pop_csname(),
                BaseCommand::Char{catcode:CategoryCode::Space,..} => csname.push(' '),
                BaseCommand::Char{char,..} => csname.push(char.as_char()),
                o => throw!("Unexpected token in {}: {:?}",name,o => cmd.cause)
            }
        }
    }
    let ret = TeXStr::from_string(&csname, &mut engine.interner);
    engine.memory.return_string(csname);
    ret
}

pub fn csname<ET:EngineType>(engine: &mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    debug_log!(trace=>"csname");
    let str = get_csname::<ET>(engine,&cmd,"csname");
    debug_log!(trace=>"csname {}",str.to_str(&engine.interner));
    match engine.state.get_command(str) {
        None => engine.state.set_command(str, Some(Command::new(BaseCommand::Relax,Some(&cmd))), false),
        _ => ()
    }
    engine.mouth.requeue(ET::Token::new_cs_from_command(str, cmd));
}

pub fn day<ET:EngineType>(engine:&mut EngineRef<ET>,cmd:&CommandSource<ET>) -> ET::Int {
    ET::Int::from_i64(engine.start_time.day() as i64)
}

pub const DEF : &str = "def";
pub fn def<ET:EngineType>(engine: &mut EngineRef<ET>, cmd:&CommandSource<ET>, global:bool, protected:bool, long:bool, outer:bool) {
    debug_log!(trace=>"def");
    let csO = engine.get_next_token();
    let cs = match csO {
        None => file_end_prim!(DEF,cmd),
        Some((t,_)) => t
    };
    let cm = match cs.as_cs_like() {
        None => throw!("Command expected after \\def" => cs),
        Some(cm) => cm
    };
    let (endswithbrace,arity,signature) = parse_signature::<ET>(engine,&cmd,DEF);
    let mut replacement: Vec<ExpToken<ET>> = vec!();
    let mut partk = None;

    engine.get_group(|engine,t| match partk.take() {
        None if t.is_parameter() =>
            partk = Some(t),
        Some(t2) if t.is_parameter() =>
            replacement.push(ExpToken::Token(t2)),
        Some(t2) => match t.get_char() {
            Some(c) if c.as_bytes().len() == 1 => {
                let u = c.as_bytes()[0];
                if u < 49 || u - 48 > arity {
                    throw!("Illegal parameter number {}",(u-48) => cmd.cause.clone())
                }
                replacement.push(ExpToken::Param(t2,(u-49)))
            }
            _ => throw!("Expected number after #, got {}",t.printable(&engine.interner) => cmd.cause.clone()),
        }
        _ => replacement.push(ExpToken::Token(t))
    });
    let def = Def::new(protected,long,outer,endswithbrace,arity,signature,replacement);
    debug_log!(trace=>"def {} = {}",cs.printable(&engine.interner),def.as_str(&engine.interner));
    let def = Command::new(BaseCommand::Def(def),Some(&cmd));
    engine.set_command_for_tk(cm,Some(def),global);
}

pub fn delcode_assign<ET:EngineType>(engine: &mut EngineRef<ET>, cmd:&CommandSource<ET>, global:bool) {
    debug_log!(trace=>"Assigning delcode");
    let i = engine.get_int();
    let c: ET::Char = match ET::Char::from_i64(i.to_i64()) {
        Some(i) => i,
        None => throw!("Not a valid character: {}",i => cmd.cause)
    };
    engine.skip_eq_char();
    let i = engine.get_int();
    debug_log!(debug=>"\\delcode '{}' = {}",c.char_str(),i);
    engine.state.set_delcode(c,i,global);
}

pub fn delcode_get<ET:EngineType>(engine: &mut EngineRef<ET>, cmd:&CommandSource<ET>) -> ET::Int {
    debug_log!(trace=>"Getting delcode");
    let i = engine.get_int();
    let c: ET::Char = match ET::Char::from_i64(i.to_i64()) {
        Some(i) => i,
        None => throw!("Not a valid character: {}",i => cmd.cause)
    };
    let v = engine.state.get_delcode(c);
    debug_log!(debug=>"\\delcode '{}' == {}",c.char_str(),v);
    v
}

pub fn delimiter<ET:EngineType>(engine: &mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    match engine.state.mode() {
        TeXMode::Math | TeXMode::Displaymath => (),
        _ => throw!("\\delimiter allowed only in math mode" => cmd.cause)
    }
    let num = engine.get_int().to_i64();
    if num < 0 { throw!("Invalid delimiter code: {}",num => cmd.cause) }
    let num = num as u32;
    let large = num & 0xFFF;
    let small = num >> 12;
    let (small_char,small_font,small_cls) = do_mathchar::<ET>(&engine.state,small,None);
    let (large_char,large_font,large_cls) = do_mathchar::<ET>(&engine.state,large,None);
    ET::Stomach::push_node(engine,SimpleNode::Delimiter {
        small_char,small_font,large_char,large_font,small_cls,large_cls
    }.as_node());
}

pub const DIMEN : &str = "dimen";

pub fn dimen_assign<ET:EngineType>(engine: &mut EngineRef<ET>, cmd:&CommandSource<ET>, global:bool) {
    debug_log!(trace=>"Assigning \\dimen");
    let i = engine.get_int();
    let i:usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => throw!("Not a valid register: {}",i => cmd.cause)
    };
    engine.skip_eq_char();
    let v = engine.get_dim();
    debug_log!(debug=>"\\dimen{} = {}",i,v);
    engine.state.set_dim_register(i,v,global);
}

pub fn dimen_get<ET:EngineType>(engine: &mut EngineRef<ET>, cmd:&CommandSource<ET>) -> ET::Dim {
    debug_log!(trace=>"Getting \\dimen");
    let i = engine.get_int();
    let i:usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => throw!("Not a valid register: {}",i => cmd.cause)
    };
    let v = engine.state.get_dim_register(i);
    debug_log!(debug=>"\\dimen{} == {}",i,v);
    v
}

pub const DIMENDEF : &str = "dimendef";
pub fn dimendef<ET:EngineType>(engine: &mut EngineRef<ET>, cmd:&CommandSource<ET>, global:bool) {
    debug_log!(trace=>"dimendef");
    let name = engine.get_control_sequence();
    engine.set_relax(name,&cmd,global);
    engine.skip_eq_char();
    let num = catch_prim!(engine.get_int() => (DIMENDEF,cmd));
    if num.to_i64() < 0 {
        throw!("Invalid dimen register index: {}",num => cmd.cause)
    }
    let num = num.to_i64() as usize;
    let ret = Command::new(BaseCommand::Dim(ValueCommand::Register(num)),Some(&cmd));
    engine.set_command_for_tk(name,Some(ret),global);
}

pub fn discretionary<ET:EngineType>(engine: &mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    debug_log!(trace=>"\\discretionary");
    let pre = engine.get_nodes_h("discretionary");
    let post = engine.get_nodes_h("discretionary");
    let non = engine.get_nodes_h("discretionary");
    ET::Stomach::push_node(engine,SimpleNode::Discretionary {pre,post,non}.as_node());
}

pub fn displaylimits<ET:EngineType>(engine: &mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    debug_log!(trace=>"\\displaylimits");
    let last_ls = engine.stomach.shipout_data_mut().box_stack.last_mut().unwrap().ls_mut();
    match last_ls.last_mut() {
        Some(TeXNode::Simple(SimpleNode::Char {cls:Some(MathClass::Op),..})) => {
            if let Some(char) = last_ls.pop() {
                last_ls.push(TeXNode::Math {ls:vec!(char),display:engine.state.get_displaymode(),cls:None})
            } else { unreachable!()}
        }
        Some(TeXNode::Math{display,..}) => *display = engine.state.get_displaymode(),
        _ => throw!("\\displaylimits only allowed after math operator" => cmd.cause),
    }
}

pub const DIVIDE : &str = "divide";
pub fn divide<ET:EngineType>(engine: &mut EngineRef<ET>, cmd:&CommandSource<ET>, global:bool) {
    debug_log!(trace=>"\\divide");
    engine.skip_whitespace();
    match engine.get_next_unexpandable() {
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
                    }}
                }
                match a {
                    ValueCommand::Register(u) => finish!(engine.state.get_int_register(u),nv => engine.state.set_int_register(u,nv,global)),
                    ValueCommand::Primitive(name) => finish!(engine.state.get_primitive_int(name),nv => engine.state.set_primitive_int(name,nv,global)),
                    ValueCommand::Complex {name,..} if name == COUNT => {
                        let int = engine.get_int();
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
                    }}
                }
                match a {
                    ValueCommand::Register(u) => finish!(engine.state.get_dim_register(u),nv => engine.state.set_dim_register(u,nv,global)),
                    ValueCommand::Primitive(name) => finish!(engine.state.get_primitive_dim(name),nv => engine.state.set_primitive_dim(name,nv,global)),
                    ValueCommand::Complex {name,..} if name == DIMEN => {
                        let int = engine.get_int();
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
                    }}
                }
                match a {
                    ValueCommand::Register(u) => finish!(engine.state.get_skip_register(u),nv => engine.state.set_skip_register(u,nv,global)),
                    ValueCommand::Primitive(name) => finish!(engine.state.get_primitive_skip(name),nv => engine.state.set_primitive_skip(name,nv,global)),
                    ValueCommand::Complex {name,..} if name == SKIP => {
                        let int = engine.get_int();
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
                    }}
                }
                match a {
                    ValueCommand::Register(u) => finish!(engine.state.get_muskip_register(u),nv => engine.state.set_muskip_register(u,nv,global)),
                    ValueCommand::Primitive(name) => finish!(engine.state.get_primitive_muskip(name),nv => engine.state.set_primitive_muskip(name,nv,global)),
                    ValueCommand::Complex {name,..} if name == MUSKIP => {
                        let int = engine.get_int();
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

pub const DP : &str = "dp";
pub fn dp_assign<ET:EngineType>(engine: &mut EngineRef<ET>, cmd:&CommandSource<ET>, global:bool) {
    debug_log!(trace=>"Assigning \\dp");
    let i = engine.get_int();
    let i:usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => throw!("Not a valid register: {}",i => cmd.cause)
    };
    engine.skip_eq_char();
    let v = engine.get_dim();
    debug_log!(debug=>"\\dp{} = {}",i,v);
    if let Some(b) = engine.state.get_box_register(i) {b.set_depth(v)}
}

pub fn dp_get<ET:EngineType>(engine: &mut EngineRef<ET>, cmd:&CommandSource<ET>) -> ET::Dim {
    debug_log!(trace=>"Getting \\dp");
    let i = engine.get_int();
    let i:usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => throw!("Not a valid register: {}",i => cmd.cause)
    };
    let v = engine.state.get_box_register(i).map(|b|b.depth(&engine.fontstore)).unwrap_or(ET::Dim::from_sp(0));
    debug_log!(debug=>"\\dp{} == {}",i,v);
    v
}

pub fn dump<ET:EngineType>() {
    debug_log!(trace=>"\\dump");
}

pub const EDEF: &str = "edef";
pub fn edef<ET:EngineType>(engine: &mut EngineRef<ET>, cmd:&CommandSource<ET>, global:bool, protected:bool, long:bool, outer:bool) {
    debug_log!(trace=>"edef");
    let csO = engine.get_next_token();
    let cs = match csO {
        None => file_end_prim!(DEF,cmd),
        Some((t,_)) => t
    };
    let cm = match cs.as_cs_like() {
        None => throw!("Command expected after \\def" => cs),
        Some(cm) => cm
    };
    debug_log!(trace=>"edef: {}",cs.printable(&engine.interner));
    let (endswithbrace,arity,signature) = parse_signature::<ET>(engine,&cmd,EDEF);
    let mut replacement: Vec<ExpToken<ET>> = vec!();
    let mut partk: Option<ET::Token> = None;

    use super::etex::UNEXPANDED;

    engine.get_expanded_group(false,true,false,|engine,t| match partk.take() {
        None if t.is_parameter() => partk = Some(t),
        Some(t2) if t.is_parameter() => replacement.push(ExpToken::Token(t2)),
        Some(t2) => match t.get_char() {
            Some(c) if c.as_bytes().len() == 1 => {
                let u = c.as_bytes()[0];
                if u < 49 || u - 48 > arity {
                    throw!("Illegal parameter number {}",(u-48) => cmd.cause.clone())
                }
                replacement.push(ExpToken::Param(t2,(u-49)))
            }
            _ => throw!("Expected number after #, got {}",t.printable(&engine.interner) => cmd.cause.clone())
        }
        _ => replacement.push(ExpToken::Token(t))
    });

    let def = Def::new(protected,long,outer,endswithbrace,arity,signature,replacement);
    debug_log!(trace=>"edef {} = {}",cs.printable(&engine.interner),def.as_str(&engine.interner));
    let def = Command::new(BaseCommand::Def(def),Some(&cmd));
    engine.set_command_for_tk(cm,Some(def),global);
}

pub const ELSE: &str = "else";
pub fn else_<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    match engine.gullet.current_conditional() {
        (None,_) => throw!("Not in a conditional" => cmd.cause),
        (Some(ConditionalBranch::True(name)),i) =>
            crate::engine::gullet::methods::else_loop::<ET>(engine,name,i,false),
        (Some(ConditionalBranch::Case(_,_)),i) =>
            crate::engine::gullet::methods::else_loop::<ET>(engine,IFCASE,i,false),
        _ => {
            engine.mouth.requeue(cmd.cause.clone());
            engine.mouth.requeue(ET::Token::new_cs_from_command(engine.interner.relax,&cmd));
        }
        //o => unreachable!("{:?}\nat:{}\n{}\n",o,engine.current_position(),engine.preview(200))
    }
}

pub fn end<ET:EngineType>() {
    todo!("end")
}

pub fn endcsname<ET:EngineType>(cmd:&CommandSource<ET>) {
    throw!("Unexpected \\endcsname; not in a \\csname" => cmd.cause)
}

pub fn endgroup<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    match engine.state.stack_pop(&mut engine.memory) {
        Some((mut v, GroupType::CS)) => {
            if !v.is_empty() {
                let mut s = engine.mouth.get_expansion();
                s.extend(v.into_iter());
                engine.mouth.push_expansion(s);
            }
        }
        _ => throw!("No group to end" => cmd.cause)
    }
}

pub fn endinput<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    engine.mouth.endinput(&engine.interner,&mut engine.outputs);
}

pub fn endlinechar_assign<ET:EngineType>(engine: &mut EngineRef<ET>, cmd:&CommandSource<ET>, global:bool) {
    debug_log!(trace=>"Assigning \\endlinechar");
    engine.skip_eq_char();
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
}

pub fn endlinechar_get<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>)
                                      -> ET::Int {
    debug_log!(trace=>"Getting \\endlinechar");
    let c = match engine.state.get_endlinechar() {
        None => -1,
        Some(c) => c.to_usize() as i64
    };
    debug_log!(debug=>"\\endlinechar == {:?}",c);
    ET::Int::from_i64(c)
}

// \errhelp
pub const ERRMESSAGE : &str = "errmessage";

pub fn errmessage<ET:EngineType>(engine: &mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    debug_log!(debug=>"errmessage");
    engine.skip_whitespace();
    let mut errmsg = String::new();
    engine.get_braced_string(&mut errmsg);
    let eh = engine.state.get_primitive_toks("errhelp");
    throw!(errmsg + "\n\n" + &engine.current_position())
}

pub fn errorstopmode<ET:EngineType>() {
    debug_log!(trace=>"\\errorstopmode");
}

pub fn escapechar_assign<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>, global:bool) {
    debug_log!(trace=>"Assigning \\escapechar");
    engine.skip_eq_char();
    let i = engine.get_int();
    let c = match i.to_i64() {
        -1|255 => None,
        j => match ET::Char::from_i64(j) {
            Some(c) => Some(c),
            None => throw!("Not a valid character: {}",j => cmd.cause)
        }
    };
    debug_log!(debug=>"\\escapechar = {:?}",c.map(|c| c.char_str()));
    engine.state.set_escapechar(c,global);
}
pub fn escapechar_get<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) -> ET::Int {
    debug_log!(trace=>"Getting \\escapechar");
    let c = match engine.state.get_escapechar() {
        None => -1,
        Some(c) => c.to_usize() as i64
    };
    debug_log!(debug=>"\\escapechar == {}",c);
    ET::Int::from_i64(c)
}

pub const EXPANDAFTER : &str = "expandafter";
pub fn expandafter<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    debug_log!(trace=>"expandafter");
    let first = match engine.get_next_token() {
        None => file_end_prim!(EXPANDAFTER,cmd),
        Some((t,_)) => t
    };
    let next = match engine.get_next_token() {
        None => file_end_prim!(EXPANDAFTER,cmd),
        Some((t,_)) => t
    };

    match engine.expand(resolve_token::<ET>(&engine.state,next)) {
        None => (),
        Some(next) => {
            engine.mouth.requeue(next.source.cause);
        }
    }
    engine.mouth.requeue(first);
}

pub const FI : &str = "fi";
pub fn fi<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    debug_log!(trace=>"...end of conditional.");
    match engine.gullet.current_conditional().0 {
        None => throw!("Not in a conditional" => cmd.cause),
        Some(ConditionalBranch::True(_) | ConditionalBranch::Case(_,_) | ConditionalBranch::Else(_)) =>
            engine.gullet.pop_conditional(),
        _ => {
            engine.mouth.requeue(cmd.cause.clone());
            engine.mouth.requeue(ET::Token::new_cs_from_command(engine.interner.relax,&cmd));
        }
    }
}

pub const FONT :&str = "font";
pub fn font_assign<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>, global:bool) {
    debug_log!(trace=>"Assigning \\font");
    let cs = engine.get_control_sequence();
    match cs {
        CSLike::ActiveChar(_) => throw!("Expected control sequence after \\font" => cmd.cause),
        CSLike::CS(cs) => {
            engine.skip_eq_char();
            let mut fontname = engine.memory.get_string();
            engine.get_string(&mut fontname);
            if !fontname.ends_with(".tfm") {
                fontname = fontname + ".tfm"
            }
            let fontid = engine.fontstore.get_new::<ET>(&fontname,cs);
            engine.memory.return_string(fontname);
            match engine.get_keywords(vec!("at","scaled")) {
                Some(s) if s == "at" => {
                    let dim = engine.get_dim();
                    let mut font = engine.fontstore.get_mut(fontid);
                    font.set_at(dim.to_sp());
                }
                Some(s) if s == "scaled" => {
                    let r = crate::engine::gullet::numeric_methods::read_float::<ET>(engine,b'0',false);
                    let mut font = engine.fontstore.get_mut(fontid);
                    let new_at = ((font.get_at() as f64) * r).round() as i64;
                    font.set_at(new_at);
                }
                _ => ()
            }
            let fontcmd = Command::new(BaseCommand::Font(fontid),Some(&cmd));
            engine.state.set_command(cs,Some(fontcmd),global);
        }
    }
}
pub fn font_get<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>)
                               -> ET::FontRef {
    debug_log!(trace=>"Getting \\font");
    engine.state.get_current_font()
}

pub fn fontdimen_assign<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>, global:bool) {
    debug_log!(trace=>"Assigning \\fontdimen");
    let o = engine.get_int();
    let i = match o.try_into() {
        Ok(i) => i,
        Err(_) => throw!("Invalid font dimension index" => cmd.cause)
    };
    let fontid = engine.get_font();
    engine.skip_eq_char();
    let dim = engine.get_dim();
    engine.fontstore.get_mut(fontid).set_dim::<ET::Dim>(i,dim);
}

pub fn fontdimen_get<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) -> ET::Dim {
    debug_log!(trace=>"Getting \\fontdimen");
    let o = engine.get_int();
    let i = match o.try_into() {
        Ok(i) => i,
        Err(_) => throw!("Invalid font dimension index: {}",o => cmd.cause)
    };
    let fontid = engine.get_font();
    engine.fontstore.get(fontid).get_dim::<ET::Dim>(i)
}

pub fn futurelet<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>, global:bool) {
    debug_log!(trace=>"\\futurelet");
    let cs =match engine.get_next_token() {
        None => file_end!(),
        Some((cs,_)) => cs
    };
    let cm = match cs.as_cs_like() {
        None => throw!("Expected control sequence after \\futurelet" => cmd.cause),
        Some(cm) => cm
    };
    let first = match engine.get_next_token() {
        None => file_end_prim!("futurelet",cmd),
        Some((t,_)) => t
    };
    let second = match engine.get_next_token() {
        None => file_end_prim!("futurelet",cmd),
        Some((t,_)) => t
    };
    let newcmd = match second.as_cs_like() {
        Some(CSLike::ActiveChar(c)) => engine.state.get_ac_command(c).cloned(),
        Some(CSLike::CS(name)) => engine.state.get_command(name).cloned(),
        None => Some(Command::new(second.to_command(),Some(&cmd)))
    };
    debug_log!(debug=>"\\futurelet: setting {} to {:?}",cs.printable(&engine.interner),newcmd);
    engine.set_command_for_tk(cm,newcmd,global);
    engine.mouth.requeue(second);
    engine.mouth.requeue(first);
}


pub fn gdef<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>, global:bool, protected:bool, long:bool, outer:bool) {
    def::<ET>(engine,cmd,true,protected,long,outer)
}

pub const GLOBAL: &str = "global";
pub fn global<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>, global_:bool, protected_:bool, long_:bool, outer_:bool) {
    debug_log!(trace => "\\global");
    engine.skip_whitespace();
    match engine.get_next_stomach_command() {
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
            BaseStomachCommand::Assignment {set,..} => set(engine,c.source,true),
            BaseStomachCommand::ValueAss(set) => set(engine,c.source,true),
            o => todo!("global: {:?} at {}",o,engine.preview(100))
        }
    }
}

pub fn do_align<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>, colmode:BoxMode, betweenmode:BoxMode, to:Option<ET::Dim>) {
    while let Some(next) = engine.get_next_unexpandable() {
        match next.command {
            BaseCommand::Char{catcode:CategoryCode::Space,..} => {},
            BaseCommand::Char{catcode:CategoryCode::BeginGroup,..} => {
                engine.state.stack_push(GroupType::Box(betweenmode));

                let mut tabskip = engine.state.get_primitive_skip("tabskip");
                let mut in_v = false;
                let mut columns = vec!((vec!(),vec!(),tabskip));//vec!((engine.memory.get_token_vec(), engine.memory.get_token_vec(), tabskip));
                let mut recindex: Option<usize> = None;

                while let Some((next, e)) = engine.get_next_token() {
                    let next = resolve_token::<ET>(&engine.state,next);
                    match next.command {
                        BaseCommand::Char {catcode:CategoryCode::AlignmentTab,..} if !in_v && columns.last().unwrap().0.is_empty() => {
                            recindex = Some(columns.len() - 1);
                            engine.skip_whitespace()
                        }
                        BaseCommand::Char {catcode:CategoryCode::AlignmentTab,..} => {
                            columns.push((engine.memory.get_token_vec(), engine.memory.get_token_vec(), tabskip));
                            in_v = false;
                        }
                        BaseCommand::Char {catcode:CategoryCode::Parameter,..} if !in_v => {
                            in_v = true
                        }
                        BaseCommand::Char {catcode:CategoryCode::Parameter,..} => throw!("Misplaced # in alignment preamble"),
                        _ if !e =>
                            if in_v { columns.last_mut().unwrap().1.push(next.source.cause) } else { columns.last_mut().unwrap().0.push(next.source.cause) }
                        BaseCommand::Skip(ValueCommand::Primitive("tabskip")) => {
                            tabskip = engine.get_skip();
                            columns.last_mut().unwrap().2 = tabskip;
                        }
                        BaseCommand::Unexpandable { name, .. } if name == CR || name == CRCR => {
                            engine.mouth.insert_every(&engine.state, "everycr");
                            break
                        }
                        BaseCommand::Unexpandable { name, .. } if name == SPAN => {
                            if let Some((next, b)) = engine.get_next_token() {
                                let res = resolve_token(&engine.state, next);
                                match engine.expand(res) {
                                    Some(r) => throw!("Expected expandable command after \\span: {}",r.source.cause.printable(&engine.interner)),
                                    _ => ()
                                }
                            } else { file_end!() }
                        }
                        _ =>
                            if in_v { columns.last_mut().unwrap().1.push(next.source.cause) } else { columns.last_mut().unwrap().0.push(next.source.cause) }
                    }
                    /*
                    if next.is_align_tab() && !in_v && columns.last().unwrap().0.is_empty() {
                        recindex = Some(columns.len() - 1);
                        engine.skip_whitespace()
                    } else if next.is_align_tab() {
                        columns.push((engine.memory.get_token_vec(), engine.memory.get_token_vec(), tabskip));
                        in_v = false;
                    } else if next.is_parameter() && !in_v {
                        in_v = true
                    } else if next.is_parameter() {
                        throw!("Misplaced # in alignment preamble")
                    } else if !e {
                        if in_v { columns.last_mut().unwrap().1.push(next) } else { columns.last_mut().unwrap().0.push(next) }
                    } else {
                        let res = resolve_token::<ET>(&engine.state, next);
                        match res.command {
                            BaseCommand::Skip(ValueCommand::Primitive("tabskip")) => {
                                tabskip = engine.get_skip();
                                columns.last_mut().unwrap().2 = tabskip;
                            }
                            BaseCommand::Unexpandable { name, .. } if name == CR || name == CRCR => {
                                engine.mouth.insert_every(&engine.state, "everycr");
                                break
                            }
                            BaseCommand::Unexpandable { name, .. } if name == SPAN => {
                                if let Some((next, b)) = engine.get_next_token() {
                                    let res = resolve_token(&engine.state, next);
                                    match engine.expand(res) {
                                        Some(r) => throw!("Expected expandable command after \\span: {}",r.source.cause.printable(&engine.interner)),
                                        _ => ()
                                    }
                                } else { file_end!() }
                            }
                            _ =>
                                if in_v { columns.last_mut().unwrap().1.push(res.source.cause) } else { columns.last_mut().unwrap().0.push(res.source.cause) }
                        }
                    }

                     */
                }

/*
                std::env::set_var("RUST_LOG","debug,tex_engine::tex::commands=trace,tex_engine::engine::gullet=trace,,tex_engine::engine::state=trace");
                env_logger::try_init();
                debug!("columns:");
                for (l,r,_) in &columns {
                    debug!("Left: {}\nRight: {}\n",PrintableTokenList::<ET>(l,&engine.interner),PrintableTokenList::<ET>(r,&engine.interner));
                }
*/

                engine.mouth.add_align_spec(&mut engine.interner,columns.into_iter().map(|(left,right,skip)| {
                    AlignSpec{left,right,skip}
                }).collect(),recindex,colmode);

                engine.stomach.open_box(OpenBox::Box {
                    mode: betweenmode,
                    list: vec!(),
                    on_close: Ptr::new(move |e,children| {
                        match colmode {
                            BoxMode::V =>
                                Some(HVBox::H(HBox {
                                    kind:"VAlign", children, to, ..Default::default()
                                })),
                            BoxMode::H =>
                                Some(HVBox::V(VBox {
                                    kind:"HAlign", children, to, ..Default::default()
                                })),
                            _ => unreachable!()
                        }
                    })
                });

                start_row(engine,cmd,colmode);
                return ()
            }
            _ => throw!("Expected begin group, found {}",next.source.cause.printable(&engine.interner) => cmd.cause)
        }
    }
    file_end!()
}

fn start_row<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>, colmode:BoxMode) {
    engine.mouth.get_align_spec().expect("Not in \\halign or \\valign").current = 0;
    loop {
        match engine.get_next_unexpandable() {
            Some(res) => {
                match res.command {
                    BaseCommand::Char {catcode:CategoryCode::EndGroup,..} => {
                        engine.mouth.requeue(res.source.cause);
                        engine.mouth.pop_align_spec();
                        return ()
                    },
                    BaseCommand::Char {catcode:CategoryCode::Space,..} => (),
                    BaseCommand::Unexpandable {name,..} if name == CRCR => (),
                    BaseCommand::Unexpandable {name,..} if name == NOALIGN => todo!(),
                    _ => {
                        engine.mouth.requeue(res.source.cause);
                        break
                    }
                }
            }
            None => file_end!()
        }
    }

    //engine.state.stack_push(GroupType::Box(colmode));
    engine.stomach.open_box(OpenBox::Box {
        mode: colmode,
        list: vec!(),
        on_close: Ptr::new(move |e,children| {
            match colmode {
                BoxMode::V =>
                    Some(HVBox::V(VBox {
                        kind:"VAlignColumn", children, ..Default::default()
                    })),
                BoxMode::H =>
                    Some(HVBox::H(HBox {
                        kind:"HAlignRow", children, ..Default::default()
                    })),
                _ => unreachable!()
            }
        })
    });
    start_cell(engine,cmd,colmode)
}

fn start_cell<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>, colmode:BoxMode) {
    loop {
        match engine.get_next_unexpandable() {
            Some(res) => {
                match res.command {
                    BaseCommand::Char {catcode:CategoryCode::EndGroup,..} => return (),
                    BaseCommand::Char {catcode:CategoryCode::Space,..} => (),
                    BaseCommand::Unexpandable {name,..} if name == CRCR => (),
                    BaseCommand::Unexpandable {name,..} if name == OMIT => todo!(),
                    _ => {
                        engine.mouth.requeue(res.source.cause);
                        break
                    }
                }
            }
            None => file_end!()
        }
    }
    let omit = {
        let spec = engine.mouth.get_align_spec().expect("Not in \\halign or \\valign");
        if spec.specs.len() <= spec.current {
            match spec.recindex {
                Some(i) => spec.current = i,
                _ => throw!("Unexpected alignment tab")
            }
        }
        spec.in_right = false;
        spec.omit
    };
    if !omit {
        let mut r = engine.mouth.get_expansion();
        {
            let spec = engine.mouth.get_align_spec().unwrap();
            let v = &spec.specs[spec.current];
            for t in v.left.iter().rev() { r.push(t.clone()) }
        }
        engine.mouth.push_expansion_norev(r);
    }

    engine.state.stack_push(GroupType::Box(colmode));
    engine.stomach.open_box(OpenBox::Box {
        mode: colmode,
        list: vec!(),
        on_close: Ptr::new(move |e,children| {
            match colmode {
                BoxMode::V =>
                    Some(HVBox::V(VBox {
                        kind:"cell", children, ..Default::default()
                    })),
                BoxMode::H =>
                    Some(HVBox::H(HBox {
                        kind:"cell", children, ..Default::default()
                    })),
                _ => unreachable!()
            }
        })
    });
}

pub const HALIGN: &str = "halign";
pub fn halign<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    let wd = if engine.get_keyword("to") {
        Some(engine.get_dim())
    } else {
        None
    };
    do_align(engine,cmd,BoxMode::H,BoxMode::V,wd)
}

pub const CR_END: &str = "!!!§%^°`/{\"CR$END$TOKEN\"}\\`°^%§!!!";
pub fn cr_end<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    let colmode = {
        let spec = engine.mouth.get_align_spec().unwrap();
        spec.omit = false;
        spec.colmode
    };
    match engine.state.stack_pop(&mut engine.memory) {
        Some((v,GroupType::Box(b))) => {
            if !v.is_empty() {
                let mut rs = engine.mouth.get_expansion();
                rs.extend(v.into_iter());
                engine.mouth.push_expansion(rs);
            }
            match engine.stomach.shipout_data_mut().box_stack.pop() {
                Some(crate::tex::nodes::OpenBox::Box {list,mode,on_close}) if mode == b => {
                    match on_close(engine,list) {
                        Some(b) => ET::Stomach::push_node(engine,b.as_node()),
                        None => {}
                    }
                }
                _ =>throw!("Unexpected box on stack" => cmd.cause)
            }
        }
        o =>
            throw!("No group to end: {:?}",o => cmd.cause)
    }
/*
    match engine.state.stack_pop(&mut engine.memory) {
        Some((v,GroupType::Box(b))) => {
            if !v.is_empty() {
                let mut rs = engine.mouth.get_expansion();
                rs.extend(v.into_iter());
                engine.mouth.push_expansion(rs);
            }
            match engine.stomach.shipout_data_mut().box_stack.pop() {
                Some(crate::tex::nodes::OpenBox::Box {list,mode,on_close}) if mode == b => {
                    match on_close(engine,list) {
                        Some(b) => engine.stomach.push_node(&engine.fontstore,&engine.state,b.as_node()),
                        None => {}
                    }
                }
                _ =>throw!("Unexpected box on stack" => cmd.cause)
            }
        }
        o =>
            throw!("No group to end: {:?}",o => cmd.cause)
    }
 */

    match engine.stomach.shipout_data_mut().box_stack.pop() {
        Some(crate::tex::nodes::OpenBox::Box {list,mode,on_close}) => {
            match on_close(engine,list) {
                Some(b) => ET::Stomach::push_node(engine,b.as_node()),
                None => {}
            }
        }
        _ =>throw!("Unexpected box on stack" => cmd.cause)
    }

    start_row(engine,cmd,colmode)
}

pub const ALIGN_END: &str = "!!!§%^°`/{\"ALIGN$END$TOKEN\"}\\`°^%§!!!";
pub fn align_end<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    let (span,colmode) = {
        let spec = engine.mouth.get_align_spec().unwrap();
        spec.current += 1;
        spec.omit = false;
        (spec.span,spec.colmode)
    };
    if !span {
        match engine.state.stack_pop(&mut engine.memory) {
            Some((v,GroupType::Box(b))) => {
                if !v.is_empty() {
                    let mut rs = engine.mouth.get_expansion();
                    rs.extend(v.into_iter());
                    engine.mouth.push_expansion(rs);
                }
                match engine.stomach.shipout_data_mut().box_stack.pop() {
                    Some(crate::tex::nodes::OpenBox::Box {list,mode,on_close}) if mode == b => {
                        match on_close(engine,list) {
                            Some(b) => ET::Stomach::push_node(engine,b.as_node()),
                            None => {}
                        }
                    }
                    _ =>throw!("Unexpected box on stack" => cmd.cause)
                }
            }
            o =>
                throw!("No group to end: {:?}",o => cmd.cause)
        }
    }
    start_cell(engine,cmd,colmode)
}

pub const HBOX: &str = "hbox";
pub fn hbox<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) -> CloseBoxFun<ET> {
    debug_log!(trace=>"\\hbox");
    let (to,spread) = match engine.get_keywords(vec!("spread","to")) {
        None => (None,None),
        Some(s) if s == "to" => {
            let a = engine.get_dim();
            (Some(a),None)
        },
        Some(s) if s == "spread" => {
            let a = engine.get_dim();
            (None,Some(a))
        },
        _ => unreachable!()
    };
    while let Some(next) = engine.get_next_unexpandable() {
        match next.command {
            BaseCommand::Char{catcode:CategoryCode::Space,..} => {},
            BaseCommand::Relax => {},
            BaseCommand::Char{catcode:CategoryCode::BeginGroup,..} => {
                engine.state.stack_push(GroupType::Box(BoxMode::H));
                engine.mouth.insert_every(&engine.state,"everyhbox");
                return Ptr::new(move |e,children| {
                    Some(HVBox::H(HBox {
                        children, to:to.clone(), spread:spread.clone(),
                        ..Default::default()
                    }))
                })
            }
            _ => throw!("Expected begin group, found {:?}",next.source.cause => cmd.cause)
        }
    }
    file_end_prim!("hbox",cmd);
}

pub const HFIL: &str = "hfil";
pub fn hfil<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    debug_log!(trace => "\\hfil");
    ET::Stomach::push_node(engine,SkipNode::HFil.as_node());
}

pub const HFILL: &str = "hfill";
pub fn hfill<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    debug_log!(trace => "\\hfill");
    ET::Stomach::push_node(engine,SkipNode::HFill.as_node());
}

pub const HFILNEG: &str = "hfilneg";
pub fn hfilneg<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    debug_log!(trace => "\\hfilneg");
    ET::Stomach::push_node(engine,SkipNode::HFilneg.as_node());
}

pub const HSS: &str = "hss";
pub fn hss<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    debug_log!(trace => "\\hss");
    ET::Stomach::push_node(engine,SkipNode::Hss.as_node());
}

pub const HT : &str = "ht";
pub fn ht_assign<ET:EngineType>(engine: &mut EngineRef<ET>, cmd:&CommandSource<ET>, global:bool) {
    debug_log!(trace=>"Assigning \\ht");
    let i = engine.get_int();
    let i:usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => throw!("Not a valid register: {}",i => cmd.cause)
    };
    engine.skip_eq_char();
    let v = engine.get_dim();
    debug_log!(debug=>"\\ht{} = {}",i,v);
    if let Some(b) = engine.state.get_box_register(i) {b.set_height(v)}
}

pub fn ht_get<ET:EngineType>(engine: &mut EngineRef<ET>, cmd:&CommandSource<ET>) -> ET::Dim {
    debug_log!(trace=>"Getting \\ht");
    let i = engine.get_int();
    let i:usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => throw!("Not a valid register: {}",i => cmd.cause)
    };
    let v = engine.state.get_box_register(i).map(|b| b.height(&engine.fontstore)).unwrap_or(ET::Dim::from_sp(0));
    debug_log!(debug=>"\\ht{} == {}",i,v);
    v
}

pub const HRULE: &str = "hrule";
pub fn hrule<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    debug_log!(trace => "\\hrule");
    let mut width = None;
    let mut height = None;
    let mut depth = None;
    loop {
        match engine.get_keywords(vec!("width","height","depth")) {
            None => break,
            Some(s) => {
                let val = engine.get_dim();
                match s {
                    "width" => width = Some(val),
                    "height" => height = Some(val),
                    "depth" => depth = Some(val),
                    _ => unreachable!()
                }
            }
        }
    }
    ET::Stomach::push_node(engine,SimpleNode::Rule {
        width,height,depth,axis:HorV::Horizontal
    }.as_node());
}

pub const HSKIP: &str = "hskip";
pub fn hskip<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    debug_log!(trace => "\\hskip");
    let skip = engine.get_skip();
    ET::Stomach::push_node(engine,SkipNode::Skip{skip,axis:HorV::Horizontal}.as_node());
}

pub fn hyphenation<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    debug_log!(trace=>"\\hyphenation");
    // TODO
    let mut v = engine.memory.get_token_vec();
    engine.get_argument(&mut v);
    engine.memory.return_token_vec(v);
}

pub fn hyphenchar_assign<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>, global:bool) {
    debug_log!(trace=>"Assigning \\hyphenchar");
    let fontid = engine.get_font();
    engine.skip_eq_char();
    let i = engine.get_int().to_i64();
    let mut font = engine.fontstore.get_mut(fontid);
    debug_log!(debug=>"\\hyphenchar\\{:?} = {:?}",font,i);
    font.set_hyphenchar(i);
}
pub fn hyphenchar_get<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) -> ET::Int {
    debug_log!(trace=>"Getting \\hyphenchar");
    let fontid = engine.get_font();
    let hc = engine.fontstore.get(fontid).get_hyphenchar();
    debug_log!(debug=>"\\hyphenchar == {:?}",hc);
    ET::Int::from_i64(hc)
}

pub fn if_<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) -> bool {
    debug_log!(trace=>"if");
    let first = get_if_token::<ET>(engine,&cmd,"if");
    let second = get_if_token::<ET>(engine,&cmd,"if");
    debug_log!(trace=>"if: {:?} == {:?}",
        first.as_ref().map(|t| t.source.cause.printable(&engine.interner).to_string()),
        second.as_ref().map(|t| t.source.cause.printable(&engine.interner).to_string())
    );
    match (first,second) {
        (None,_) | (_,None) => false,
        (Some(f),Some(s)) => match (f.source.cause.get_char(),s.source.cause.get_char()) {
            (Some(f),Some(s)) => f == s,
            (None,None) => true,
            _ => false
        }
    }
}

pub const IFCASE: &str = "ifcase";
pub fn ifcase<ET:EngineType>() -> bool {
    unreachable!("executed in Gullet")
}

pub const IFCAT : &str = "ifcat";
pub fn ifcat<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>)
                            -> bool {
    debug_log!(trace=>"ifcat");
    let first = get_if_token::<ET>(engine,&cmd,IFCAT);
    let second = get_if_token::<ET>(engine,&cmd,IFCAT);
    debug_log!(trace=>"ifcat: {:?} == {:?}",
        first.as_ref().map(|t| t.source.cause.printable(&engine.interner).to_string()),
        second.as_ref().map(|t| t.source.cause.printable(&engine.interner).to_string())
    );
    let first = match first {
        None => return false,
        Some(first) => match first.command {
            BaseCommand::Char{catcode,..} => catcode,
            _ => CategoryCode::Escape
        }
    };
    let second = match second {
        None => return false,
        Some(first) => match first.command {
            BaseCommand::Char{catcode,..} => catcode,
            _ => CategoryCode::Escape
        }
    };
    first == second
}

pub fn get_if_token<ET:EngineType>(engine: &mut EngineRef<ET>, cmd:&CommandSource<ET>, name:&'static str)
                                   -> Option<ResolvedToken<ET>> {
    // need to be careful not to expand \else and \fi before conditional is done.
    while let Some((t,e)) = catch_prim!(engine.get_next_token() => (name,cmd)) {
        let r = resolve_token(&engine.state,t);
        match r.command {
            BaseCommand::ExpandableNoTokens {name,..} if e && (name == ELSE || name == FI) && (match engine.gullet.current_conditional() {
                (Some(ConditionalBranch::None(_)),_) => true,
                _ => false
            }) => {
                engine.mouth.requeue(r.source.cause);
                return None
            }
            _ if e => match engine.expand(r) {
                Some(c) => return Some(c),
                None => {}
            }
            _ => return Some(r)
        }
    }
    file_end_prim!(name,cmd)
}

pub const LGE : [u8;3] = [b'<',b'>',b'='];
pub const IFDIM : &str = "ifdim";
pub fn ifdim<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>)
                            -> bool {
    debug_log!(trace=>"ifdim");
    let i1 = engine.get_dim();
    let rel = match engine.is_next_char_one_of(&LGE) {
        None => throw!("Expected one of '<','>','='".to_string() => cmd.cause),
        Some(r) => r
    };
    let i2 = catch_prim!(engine.get_dim() => (IFDIM,cmd));
    match rel {
        b'<' => i1.to_sp() < i2.to_sp(),
        b'>' => i1.to_sp() > i2.to_sp(),
        b'=' => i1.to_sp() == i2.to_sp(),
        _ => unreachable!()
    }
}

pub const IFEOF : &str = "ifeof";
pub fn ifeof<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) -> bool {
    debug_log!(trace=>"ifeof");
    let i = engine.get_int();
    let i : usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => throw!("Invalid file number: {}",i => cmd.cause)
    };
    if i == 18 { return false }
    let f = match engine.state.get_open_in_file(i) {
        None => throw!("No in file open at index: {}",i => cmd.cause),
        Some(f) => f
    };
    f.eof::<ET>(&engine.state)
}

pub fn ifhmode<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) -> bool {
    debug_log!(trace=>"ifhmode");
    match engine.state.mode() {
        TeXMode::Horizontal | TeXMode::RestrictedHorizontal => true,
        _ => false
    }
}

pub fn ifhbox<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) -> bool {
    debug_log!(trace=>"ifhbox");
    let i = engine.get_int().to_i64();
    if i < 0 { throw!("Invalid box register: {}",i => cmd.cause) }
    match engine.state.get_box_register(i as usize) {
        Some(HVBox::H(_)) => true,
        _ => false
    }
}

pub fn ifinner<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) -> bool {
    debug_log!(trace=>"ifinner: {}; {:?}",engine.state.mode(), engine.stomach.shipout_data().box_stack);
    match engine.state.mode() {
        TeXMode::RestrictedHorizontal | TeXMode::InternalVertical | TeXMode::Math => true,
        _ => false
    }
}

pub fn ifmmode<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) -> bool {
    debug_log!(trace=>"ifmmode");
    match engine.state.mode() {
        TeXMode::Math | TeXMode::Displaymath => true,
        _ => false
    }
}

pub const IFNUM : &str = "ifnum";
pub fn ifnum<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) -> bool {
    debug_log!(trace=>"ifnum");
    let i1 = engine.get_int();
    engine.skip_whitespace();
    let rel = match engine.is_next_char_one_of(&LGE) {
        None => throw!("Expected one of '<','>','='" => cmd.cause),
        Some(r) => r
    };
    let i2 = engine.get_int();
    match rel {
        b'<' => i1<i2,
        b'>' => i1>i2,
        b'=' => i1==i2,
        _ => unreachable!()
    }
}

pub const IFODD: &str = "ifodd";
pub fn ifodd<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) -> bool {
    debug_log!(trace=>"ifodd");
    let num = engine.get_int();
    num.to_i64() % 2 != 0
}

pub fn ifvbox<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) -> bool {
    debug_log!(trace=>"ifvbox");
    let i = engine.get_int().to_i64();
    if i < 0 { throw!("Invalid box register: {}",i => cmd.cause) }
    match engine.state.get_box_register(i as usize) {
        Some(HVBox::V(_)) => true,
        _ => false
    }
}

pub fn ifvmode<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) -> bool {
    debug_log!(trace=>"ifvmode");
    match engine.state.mode() {
        TeXMode::Vertical | TeXMode::InternalVertical => true,
        _ => false
    }
}

pub fn ifvoid<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) -> bool {
    debug_log!(trace=>"ifvoid");
    let i = engine.get_int().to_i64();
    if i < 0 { throw!("Invalid box register: {}",i => cmd.cause) }
    match engine.state.get_box_register(i as usize) {
        Some(HVBox::Void) | None => true,
        _ => false
    }
}

pub const IFX : &str = "ifx";
pub fn ifx<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) -> bool {
    debug_log!(trace=>"ifx");
    let t1 = match engine.get_next_token() {
        None => file_end_prim!("ifx",cmd),
        Some((t,e)) => resolve_token::<ET>(&engine.state,t).with_expand(e)
    };
    let t2 = match engine.get_next_token() {
        None => file_end_prim!("ifx",cmd),
        Some((t,e)) => resolve_token::<ET>(&engine.state,t).with_expand(e)
    };
    debug_log!(trace=>"ifx: {} == {}?",t1.source.cause.printable(&engine.interner),t2.source.cause.printable(&engine.interner));
    debug_log!(trace=>"First : {}",t1.command.as_str(&engine.interner));
    debug_log!(trace=>"Second: {}",t2.command.as_str(&engine.interner));
    if t1.expand && t2.expand { t1.command == t2.command }
    else if !t1.expand && !t2.expand { t1.source.cause == t2.source.cause }
    else { false }
}

pub const IGNORESPACES : &str = "ignorespaces";
pub fn ignorespaces<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    debug_log!(trace=>"\\ignorespaces");
    loop {
        match engine.get_next_stomach_command() {
            None => return (),
            Some(sc) => match sc.command {
                BaseStomachCommand::Space => (),
                _ => {
                    engine.mouth.requeue(sc.source.cause);
                    return ()
                }
            }
        }
    }
}

pub const IMMEDIATE: &str ="immediate";
pub fn immediate<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    debug_log!(trace=>"immediate");
    match engine.get_next_stomach_command() { // TODO: pdfxform, pdfobj
        None => file_end_prim!(IMMEDIATE,cmd),
        Some(sc) => match sc.command {
            BaseStomachCommand::Whatsit { name, apply } => {
                let wi = apply(engine,sc.source);
                wi.apply(engine)
            }
            _ => {
                engine.mouth.requeue(sc.source.cause)
            }
        }
    }
}

pub fn indent<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    debug_log!(trace=>"indent");
    ET::Stomach::push_node(engine,HBox {
        kind:"indent",
        children:vec!(SkipNode::Skip{skip:engine.state.get_primitive_skip("parindent"),axis:HorV::Horizontal}.as_node()),
        ..Default::default()
    }.as_node());
}

pub const INPUT: &str = "input";
pub fn input<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    debug_log!(trace=>"input");
    let mut filename = engine.memory.get_string();
    engine.get_string(&mut filename);
    debug_log!(trace=>"input: {}",filename);
    if filename.is_empty() {
        throw!("Empty file name in \\input: {}",engine.current_position())
    }
    let file = engine.filesystem.get(&filename);
    debug_log!(trace=>"input resolved: {:?}",file.path());
    if !file.exists() {
        throw!("I can't find file `{}'",filename => cmd.cause)
    } else {
        engine.memory.return_string(filename);
        (engine.outputs.file_open)(file.path().to_str().unwrap());
        engine.mouth.push_file(&file,&mut engine.interner);
    }
}

pub fn inputlineno<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) -> ET::Int {
    ET::Int::from_i64(engine.mouth.line_no() as i64)
}

pub fn insert<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    let n = engine.get_int().to_i64();
    if n < 0 {
        throw!("Invalid insert number: {}",n => cmd.cause)
    }
    while let Some(next) = engine.get_next_unexpandable() {
        match next.command {
            BaseCommand::Char{catcode:CategoryCode::Space,..} => {},
            BaseCommand::Relax => {},
            BaseCommand::Char{catcode:CategoryCode::BeginGroup,..} => {
                engine.state.stack_push(GroupType::Box(BoxMode::V));
                engine.stomach.open_box(OpenBox::Box {
                    mode: BoxMode::V,
                    list: vec![],
                    on_close: Ptr::new(move |engine,children| {
                        ET::Stomach::push_node(engine,
                            TeXNode::Insert(n as usize,children)
                        );
                        None
                    }),
                });
                return ()
            }
            _ => throw!("Expected begin group, found {}",next.source.cause.printable(&engine.interner) => cmd.cause)
        }
    }
    file_end!()
}

pub fn jobname<ET:EngineType>(engine:&mut EngineRef<ET>,cmd:&CommandSource<ET>, exp:&mut Vec<ET::Token>) {
    debug_log!(trace=>"jobname");
    let jobname = engine.jobname.clone();
    crate::tex::token::tokenize_string(&jobname, cmd, |t| exp.push(t))
}

pub fn kern<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    let dim = engine.get_dim();
    ET::Stomach::push_node(engine,TeXNode::Kern {dim,axis:match engine.state.mode() {
        TeXMode::Vertical | TeXMode::InternalVertical => HorV::Vertical,
        _ => HorV::Horizontal
    }});
}

pub fn lastbox<ET:EngineType>(engine:&mut EngineRef<ET>,cmd:&CommandSource<ET>) -> HVBox<ET> {
    debug_log!(trace=>"\\lastbox");
    if engine.state.mode() == TeXMode::Vertical {
        throw!("\\lastbox not allowed in vertical mode" => cmd.cause)
    }
    let sd = engine.stomach.shipout_data_mut();
    let ls = sd.box_stack.last_mut().map(|t| t.ls_mut()).unwrap_or(&mut sd.page);
    match ls.last() {
        Some(TeXNode::Box(_)) => {
            match ls.pop() {
                Some(TeXNode::Box(b)) => b,
                _ => unreachable!()
            }
        }
        _ => HVBox::Void
    }
}

pub fn lastkern<ET:EngineType>(engine:&mut EngineRef<ET>,cmd:&CommandSource<ET>) -> ET::Dim {
    let ls = match engine.stomach.shipout_data().box_stack.last() {
        None => &engine.stomach.shipout_data().page,
        Some(bx) => bx.ls()
    };
    for n in ls.iter().rev() { match n {
        TeXNode::Kern{dim,..} => return *dim,
        _ => return ET::Dim::default()
    }}
    ET::Dim::default()
}

pub fn lastskip<ET:EngineType>(engine:&mut EngineRef<ET>,cmd:&CommandSource<ET>) -> Skip<ET::SkipDim> {
    let ls = match engine.stomach.shipout_data().box_stack.last() {
        None => &engine.stomach.shipout_data().page,
        Some(bx) => bx.ls()
    };
    for n in ls.iter().rev() { match n {
        TeXNode::Skip(SkipNode::Skip { skip: val,..}) => return val.clone(),
        TeXNode::Penalty(_) => (),
        _ => return Skip::default()
    }}
    Skip::default()
}

pub fn lastpenalty<ET:EngineType>(engine:&mut EngineRef<ET>,cmd:&CommandSource<ET>) -> ET::Int {
    let ls = match engine.stomach.shipout_data().box_stack.last() {
        None => &engine.stomach.shipout_data().page,
        Some(bx) => bx.ls()
    };
    for n in ls.iter().rev() { match n {
        TeXNode::Penalty(i) => return ET::Int::from_i64(*i as i64),
        _ => return ET::Int::default()
    }}
    ET::Int::default()
}

pub const LCCODE: &str = "lccode";
pub fn lccode_assign<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>, global:bool) {
    debug_log!(trace=>"Assigning lower case character");
    let c = engine.get_char();
    engine.skip_eq_char();
    let lc = engine.get_char();
    debug_log!(debug=>"\\lccode '{}' = {}",c.char_str(),lc.char_str());
    engine.state.set_lccode(c,lc,global);
}

pub fn lccode_get<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) -> ET::Int {
    debug_log!(trace=>"Getting lower case character");
    let c = engine.get_char();
    let v = ET::Int::from_i64(engine.state.get_lccode(c).to_usize() as i64);
    debug_log!(debug=>"\\lccode '{}' == {}",c.char_str(),v);
    v
}

pub fn do_leaders<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>,tp:LeadersType) {
    debug_log!(trace=>"leaders");
    match engine.get_keywords(vec!("width","height","depth")) {
        Some(_) => todo!("Dimensions"),
        _ => {
            let c = match engine.get_next_unexpandable() {
                None => file_end!(),
                Some(c) => c
            };
            match c.command {
                BaseCommand::OpenBox {name,mode,apply} => {
                    let f = apply(engine,c.source);
                    engine.stomach.open_box(OpenBox::Box {list:vec!(),mode,on_close:Ptr::new(move |e,v| {
                        let bx = match f(e,v) {
                            Some(r) => {r}
                            None => {todo!("make void box")}
                        };
                        let skip = leaders_skip(e);
                        ET::Stomach::push_node(e,SimpleNode::Leaders {bx,skip,tp}.as_node());
                        None
                    })})
                }
                BaseCommand::FinishedBox {get,..} => {
                    let bx = get(engine,c.source);
                    let skip = leaders_skip(engine);
                    ET::Stomach::push_node(engine,SimpleNode::Leaders {bx,skip,tp}.as_node());
                }
                BaseCommand::Unexpandable {name:HRULE,..} => todo!(),
                BaseCommand::Unexpandable {name:VRULE,..} => todo!(),
                _ => throw!("Box expected: {}",c.source.cause.printable(&engine.interner))
            }
        }
    }
}

pub fn leaders_skip<ET:EngineType>(engine:&mut EngineRef<ET>) -> SkipNode<ET> {
    match engine.get_next_unexpandable() {
        None => file_end!(),
        Some(c) => match (c.command,engine.state.mode()) {
            (BaseCommand::Unexpandable{name:VSKIP,..},TeXMode::Vertical|TeXMode::InternalVertical) =>
                SkipNode::Skip{skip:engine.get_skip(),axis:HorV::Vertical},
            (BaseCommand::Unexpandable{name:VFIL,..},TeXMode::Vertical|TeXMode::InternalVertical) =>
                SkipNode::VFil,
            (BaseCommand::Unexpandable{name:VFILL,..},TeXMode::Vertical|TeXMode::InternalVertical) =>
                SkipNode::VFill,
            (BaseCommand::Unexpandable{name:HSKIP,..},TeXMode::Horizontal|TeXMode::RestrictedHorizontal|TeXMode::Math|TeXMode::Displaymath) =>
                SkipNode::Skip{skip:engine.get_skip(),axis:HorV::Vertical},
            (BaseCommand::Unexpandable{name:HFIL,..},TeXMode::Horizontal|TeXMode::RestrictedHorizontal|TeXMode::Math|TeXMode::Displaymath) =>
                SkipNode::HFil,
            (BaseCommand::Unexpandable{name:HFILL,..},TeXMode::Horizontal|TeXMode::RestrictedHorizontal|TeXMode::Math|TeXMode::Displaymath) =>
                SkipNode::HFill,
            _ => throw!("Expected one of \\vskip, \\vfil, \\vfill, \\hskip, \\hfil, \\hfill after \\leaders" => c.source.cause)
        }
    }
}

pub fn leaders<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    do_leaders(engine,cmd,LeadersType::Normal)
}
pub fn cleaders<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    do_leaders(engine,cmd,LeadersType::C)
}
pub fn xleaders<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    do_leaders(engine,cmd,LeadersType::X)
}

use string_interner::Symbol;
use crate::engine::stomach::methods::do_mathchar;

pub const LET : &str = "let";
pub fn let_<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>, globally:bool) {
    debug_log!(trace=>"let");
    let cs = match engine.get_next_token() {
        None => file_end_prim!(LET,cmd),
        Some((t,_)) => t
    };
    let cm = match cs.as_cs_like() {
        Some(cm) => cm,
        None => throw!("Expected a control sequence" => cs)
    };
    catch_prim!(engine.skip_eq_char() => (LET,cmd));
    let target = match engine.get_next_token() {
        Some((t,_)) => t,
        None =>file_end_prim!(LET,cmd)
    };
    let cmd = match target.as_cs_like() {
        Some(CSLike::ActiveChar(c)) => engine.state.get_ac_command(c).map(|c|c.clone().copy_with(&cmd)),
        Some(CSLike::CS(name)) => engine.state.get_command(name).map(|c|c.clone().copy_with(&cmd)),
        None => Some(Command::new(target.to_command(),Some(cmd)))
    };
    debug_log!(debug=>"let: {} = {}",cs.printable(&engine.interner),cmd.as_ref().map(|c|c.base.as_str(&engine.interner)).unwrap_or("undefined".to_string()));
    engine.set_command_for_tk(cm,cmd,globally);
}

pub fn long<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>, global_:bool, protected_:bool, long_:bool, outer_:bool) {
    debug_log!(trace => "\\long");
    match engine.get_next_stomach_command() {
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

pub const LOWER: &str = "lower";
pub fn lower<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    debug_log!(trace=>"\\lower");
    let i = engine.get_dim();
    match engine.get_next_unexpandable() {
        None => file_end_prim!(RAISE,cmd),
        Some(c) => match c.command {
            BaseCommand::OpenBox {name,mode,apply} => {
                let f = apply(engine,c.source);
                engine.stomach.open_box(OpenBox::Box {list:vec!(),mode,on_close:Ptr::new(move |engine,v| {
                    let bx = match f(engine,v) {
                        Some(r) => {r}
                        None => {todo!("make void box")}
                    };
                    ET::Stomach::push_node(engine,SimpleNode::Raise {by:-i,node:bx}.as_node());
                    None
                })})
            }
            BaseCommand::FinishedBox {get,..} => {
                let bx = get(engine,c.source);
                ET::Stomach::push_node(engine,SimpleNode::Raise {by:-i,node:bx}.as_node());
            }
            _ => throw!("Box expected: {}",c.source.cause.printable(&engine.interner))
        }
    }
}

pub const LOWERCASE : &str = "lowercase";
pub fn lowercase<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    debug_log!(trace => "\\lowercase");
    let mut rs = engine.mouth.get_expansion();
    engine.expand_until_group(|engine,next| match next.split_char_and_catcode() {
        Some((c,cc)) => {
            let nc = engine.state.get_lccode(c);
            if nc.to_usize() == 0 { rs.push(next) }
            else { rs.push(ET::Token::new_char_from_command(nc, cc, cmd)) }
        }
        _ => rs.push(next)
    });
    engine.mouth.push_expansion(rs);
}

pub fn mark<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    do_mark(engine,cmd,0)
}

pub fn do_mark<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>,idx:usize) {
    let mut v = Vec::new();
    engine.get_expanded_group(true,true,true,|_,t| v.push(t));
    for b in engine.stomach.shipout_data_mut().box_stack.iter_mut().rev() {
        match b {
            OpenBox::Box {mode:BoxMode::V,list,..} => {
                list.push(TeXNode::Mark(idx,v.clone()));
                return ()
            }
            OpenBox::Box {..} => (),
            OpenBox::Math {..} => (),
            OpenBox::Paragraph {list,..} => {
                list.push(TeXNode::Mark(idx,v.clone()));
                return ()
            }
        }
    }
    ET::Stomach::push_node(engine,TeXNode::Mark(
        idx,v
    ).as_node());
}

pub fn do_topmark<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>,idx:usize,f:&mut Vec<ET::Token>) {
    match engine.stomach.shipout_data().topmarks.get(&idx) {
        Some(v) => {
            for t in v {f.push(t.clone())}
        }
        _ => ()
    }
}

pub fn do_firstmark<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>,idx:usize,f:&mut Vec<ET::Token>) {
    match engine.stomach.shipout_data().firstmarks.get(&idx) {
        Some(v) => {
            for t in v {f.push(t.clone())}
        }
        _ => ()
    }
}

pub fn do_botmark<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>,idx:usize,f:&mut Vec<ET::Token>) {
    match engine.stomach.shipout_data().botmarks.get(&idx) {
        Some(v) => {
            for t in v {f.push(t.clone())}
        }
        _ => ()
    }
}

pub fn do_splitfirstmark<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>,idx:usize,f:&mut Vec<ET::Token>) {
    match engine.stomach.shipout_data().splitfirstmarks.get(&idx) {
        Some(v) => {
            for t in v {f.push(t.clone())}
        }
        _ => ()
    }
}

pub fn do_splitbotmark<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>,idx:usize,f:&mut Vec<ET::Token>) {
    match engine.stomach.shipout_data().splitbotmarks.get(&idx) {
        Some(v) => {
            for t in v {f.push(t.clone())}
        }
        _ => ()
    }
}

pub fn topmark<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>,f:&mut Vec<ET::Token>) {
    do_topmark(engine,cmd,0,f)
}

pub fn firstmark<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>,f:&mut Vec<ET::Token>) {
    do_firstmark(engine,cmd,0,f)
}

pub fn botmark<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>,f:&mut Vec<ET::Token>) {
    do_botmark(engine,cmd,0,f)
}

pub fn splitfirstmark<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>,f:&mut Vec<ET::Token>) {
    do_splitfirstmark(engine,cmd,0,f)
}

pub fn splitbotmark<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>,f:&mut Vec<ET::Token>) {
    do_splitbotmark(engine,cmd,0,f)
}

pub fn mathchar<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    debug_log!(trace=>"mathchar");
    match engine.state.mode() {
        TeXMode::Math | TeXMode::Displaymath => (),
        o => throw!("mathchar not allowed in mode: {}",o => cmd.cause)
    }
    let num =engine.get_int().to_i64();
    if num < 0 {
        throw!("Invalid math char: {}",num => cmd.cause)
    }
    let (char,font,cls) = do_mathchar::<ET>(&engine.state,num as u32,None);
    ET::Stomach::push_node(engine,TeXNode::Simple(SimpleNode::Char {char,font,cls:Some(cls)}))
}

pub fn mathclass_get<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>,cls:MathClass) {
    use crate::engine::stomach::Stomach;
    match engine.state.mode() {
        TeXMode::Math | TeXMode::Displaymath => (),
        o => throw!("\\math[] not allowed in mode: {}",o => cmd.cause)
    }
    match engine.get_next_unexpandable() {
        None => file_end!(),
        Some(c) => match c.command {
            BaseCommand::Char {char,catcode:CategoryCode::BeginGroup} => {
                engine.state.stack_push(GroupType::Box(if engine.state.get_displaymode() {BoxMode::DM} else {BoxMode::M}));
                engine.stomach.open_box(crate::tex::nodes::OpenBox::Math {list:vec!(),display:engine.state.get_displaymode(),cls:Some(cls)});
            }
            BaseCommand::Char {char,..} => {
                let (char,font,_) = do_mathchar::<ET>(&engine.state,engine.state.get_mathcode(char).to_i64() as u32,Some(char));
                ET::Stomach::push_node(engine,TeXNode::Simple(SimpleNode::Char {char,font,cls:Some(cls)}))
            },
            _ => throw!("Expected character or group after \\math[ord|op|bin|...]" => c.source.cause)
        }
    }
}

pub fn mathord<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) { mathclass_get(engine,cmd,MathClass::Ord) }
pub fn mathop<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) { mathclass_get(engine,cmd,MathClass::Op) }
pub fn mathbin<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) { mathclass_get(engine,cmd,MathClass::Bin) }
pub fn mathrel<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) { mathclass_get(engine,cmd,MathClass::Rel) }
pub fn mathopen<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) { mathclass_get(engine,cmd,MathClass::Open) }
pub fn mathclose<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) { mathclass_get(engine,cmd,MathClass::Close) }
pub fn mathpunct<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) { mathclass_get(engine,cmd,MathClass::Punct) }
pub fn mathinner<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) { mathclass_get(engine,cmd,MathClass::Ord) } // TODO?

fn mathchoice_get_one<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>,display:bool,style:FontStyle) -> Vec<TeXNode<ET>> {
    engine.get_nodes("mathchoice",|engine,sc| {
        ET::Stomach::digest(engine,sc);
        engine.state.set_displaymode(display,false);
        engine.state.set_fontstyle(style,false);
    },|engine,sc| {
        ET::Stomach::digest(engine,sc);
        let sd = engine.stomach.shipout_data_mut();
        let ls = if sd.box_stack.is_empty() {&mut sd.page} else {sd.box_stack.last_mut().unwrap().ls_mut()};
        match ls.pop() {
            Some(TeXNode::Math {ls,..}) => return ls,
            o => unreachable!("{:?}",o)
        }
    })
    /*

    match engine.get_next_stomach_command() {
        None => file_end!(),
        Some(sc) => match sc.command {
            BaseStomachCommand::BeginGroup => ET::Stomach::digest(engine,sc),
            _ => throw!("Expected begin group after \\mathchoice" => sc.source.cause)
        }
    }
    engine.state.set_displaymode(display,false);
    engine.state.set_fontstyle(style,false);
    let grouplevel = engine.state.grouplevel();
    loop {
        match engine.get_next_stomach_command() {
            Some(cmd) => {
                match cmd.command {
                    BaseStomachCommand::EndGroup if grouplevel == engine.state.grouplevel() => {
                        ET::Stomach::digest(engine,cmd);
                        let sd = engine.stomach.shipout_data_mut();
                        let ls = if sd.box_stack.is_empty() {&mut sd.page} else {sd.box_stack.last_mut().unwrap().ls_mut()};
                        match ls.pop() {
                            Some(TeXNode::Math {ls,..}) => return ls,
                            o => unreachable!("{:?}",o)
                        }
                    }
                    _ => ET::Stomach::digest(engine,cmd)
                }
            }
            None => file_end!()
        }
    }

     */
}

pub fn mathchoice<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    match engine.state.mode() {
        TeXMode::Math | TeXMode::Displaymath => (),
        o => throw!("\\mathchoice not allowed in mode: {}",o => cmd.cause)
    }
    let display = mathchoice_get_one(engine,cmd,true,FontStyle::Text);
    let text = mathchoice_get_one(engine,cmd,false,FontStyle::Text);
    let script = mathchoice_get_one(engine,cmd,false,FontStyle::Script);
    let scriptscript = mathchoice_get_one(engine,cmd,false,FontStyle::Scriptscript);
    ET::Stomach::push_node(engine,TeXNode::Simple(SimpleNode::MathChoice {display,text,script,scriptscript}));
}

pub const MATHCHARDEF : &str = "mathchardef";
pub fn mathchardef<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>, globally:bool) {
    debug_log!(trace=>"mathchardef");
    let cm = match engine.get_next_token() {
        None => file_end!(),
        Some((t,_)) => match t.as_cs_like() {
            Some(cm) => cm,
            None => throw!("Command expected after \\mathchardef" => t)
        }
    };
    engine.skip_eq_char();
    let i = engine.get_int().to_i64();
    if i < 0 {
        throw!("Invalid math char: {}",i => cmd.cause)
    }
    engine.set_command_for_tk(cm,Some(Command::new(BaseCommand::MathChar(i as u32),Some(&cmd))),globally);
}

pub const MATHCODE : &str = "mathcode";
pub fn mathcode_assign<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>, global:bool) {
    debug_log!(trace=>"Assigning math code");
    let c = engine.get_char();
    engine.skip_eq_char();
    let i = engine.get_int();
    debug_log!(debug=>"\\mathcode '{}' = {}",c.char_str(),i);
    engine.state.set_mathcode(c,i,global);
}

pub fn mathcode_get<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) -> ET::Int {
    debug_log!(trace=>"Getting mathcode");
    let c = engine.get_char();
    let v = engine.state.get_mathcode(c);
    debug_log!(debug=>"\\mathcode '{}' == {}",c.char_str(),v);
    v
}

pub const MEANING : &str = "meaning";
pub fn meaning<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>, exp:&mut Vec<ET::Token>) {
    debug_log!(trace=>"meaning");
    let esc = engine.state.get_escapechar();
    macro_rules! do_esc {
        () => {if let Some(c) = esc { exp.push(ET::Token::new_char_from_command(c,CategoryCode::Other,cmd)) }}
    }
    match engine.get_next_token() {
        None => file_end_prim!(MEANING,cmd),
        Some((_,false)) => {
            do_esc!();
            crate::tex::token::tokenize_string(RELAX, cmd, |t| exp.push(t))
        }
        Some((t,_)) => {
            let n:ResolvedToken<ET> = resolve_token::<ET>(&engine.state,t);
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
                BaseCommand::Expandable {name:name@("firstmark"|"topmark"|"botmark"|"splitfirstmark"|"splitbotmark"),..} => {
                    do_esc!();
                    crate::tex::token::tokenize_string(name, cmd, |t| exp.push(t));
                    crate::tex::token::tokenize_string(":",cmd,|t|exp.push(t));
                    let sd = engine.stomach.shipout_data();
                    let v = match name {
                        "firstmark" => sd.firstmarks.get(&0),
                        "topmark" => sd.topmarks.get(&0),
                        "botmark" => sd.botmarks.get(&0),
                        "splitfirstmark" => sd.firstmarks.get(&0),
                        "splitbotmark" => sd.botmarks.get(&0),
                        _ => unreachable!()
                    };
                    match v {
                        Some(v) => for t in v {
                            crate::tex::token::detokenize(true,&engine.state,&engine.interner,t, cmd, |t| exp.push(t))
                        }
                        _ => ()
                    }
                    return
                }
                BaseCommand::Expandable {name,..} => {
                    do_esc!();
                    return crate::tex::token::tokenize_string(name, cmd, |t| exp.push(t))
                }
                BaseCommand::ExpandableNoTokens {name,..} => {
                    do_esc!();
                    return crate::tex::token::tokenize_string(name, cmd, |t| exp.push(t))
                }
                BaseCommand::Unexpandable {name,..} => {
                    do_esc!();
                    return crate::tex::token::tokenize_string(name, cmd, |t| exp.push(t))
                }
                BaseCommand::OpenBox {name,..} => {
                    do_esc!();
                    return crate::tex::token::tokenize_string(name, cmd, |t| exp.push(t))
                }
                BaseCommand::FinishedBox {name,..} => {
                    do_esc!();
                    return crate::tex::token::tokenize_string(name, cmd, |t| exp.push(t))
                }
                BaseCommand::ProvidesNode {name,..} => {
                    do_esc!();
                    return crate::tex::token::tokenize_string(name, cmd, |t| exp.push(t))
                }
                BaseCommand::Whatsit {name,..} => {
                    do_esc!();
                    return crate::tex::token::tokenize_string(name, cmd, |t| exp.push(t))
                }
                BaseCommand::Assignment {name,..} => {
                    do_esc!();
                    return crate::tex::token::tokenize_string(name, cmd, |t| exp.push(t))
                }
                BaseCommand::Relax => {
                    do_esc!();
                    return crate::tex::token::tokenize_string(RELAX, cmd, |t| exp.push(t))
                }
                BaseCommand::Conditional {name,..} => {
                    do_esc!();
                    return crate::tex::token::tokenize_string(name, cmd, |t| exp.push(t))
                }
                BaseCommand::FontCommand {name,..} => {
                    do_esc!();
                    return crate::tex::token::tokenize_string(name, cmd, |t| exp.push(t))
                }
                BaseCommand::Int(ValueCommand::Value {name,..}) => {
                    do_esc!();
                    return crate::tex::token::tokenize_string(name, cmd, |t| exp.push(t))
                }
                BaseCommand::Int(ValueCommand::Complex {name,..}) => {
                    do_esc!();
                    return crate::tex::token::tokenize_string(name, cmd, |t| exp.push(t))
                }
                BaseCommand::Int(ValueCommand::Primitive(name)) => {
                    do_esc!();
                    return crate::tex::token::tokenize_string(name, cmd, |t| exp.push(t))
                }
                BaseCommand::Dim(ValueCommand::Value {name,..}) => {
                    do_esc!();
                    return crate::tex::token::tokenize_string(name, cmd, |t| exp.push(t))
                }
                BaseCommand::Dim(ValueCommand::Complex {name,..}) => {
                    do_esc!();
                    return crate::tex::token::tokenize_string(name, cmd, |t| exp.push(t))
                }
                BaseCommand::Dim(ValueCommand::Primitive(name)) => {
                    do_esc!();
                    return crate::tex::token::tokenize_string(name, cmd, |t| exp.push(t))
                }
                BaseCommand::Skip(ValueCommand::Value {name,..}) => {
                    do_esc!();
                    return crate::tex::token::tokenize_string(name, cmd, |t| exp.push(t))
                }
                BaseCommand::Skip(ValueCommand::Complex {name,..}) => {
                    do_esc!();
                    return crate::tex::token::tokenize_string(name, cmd, |t| exp.push(t))
                }
                BaseCommand::Skip(ValueCommand::Primitive(name)) => {
                    do_esc!();
                    return crate::tex::token::tokenize_string(name, cmd, |t| exp.push(t))
                }
                BaseCommand::MuSkip(ValueCommand::Value {name,..}) => {
                    do_esc!();
                    return crate::tex::token::tokenize_string(name, cmd, |t| exp.push(t))
                }
                BaseCommand::MuSkip(ValueCommand::Complex {name,..}) => {
                    do_esc!();
                    return crate::tex::token::tokenize_string(name, cmd, |t| exp.push(t))
                }
                BaseCommand::MuSkip(ValueCommand::Primitive(name)) => {
                    do_esc!();
                    return crate::tex::token::tokenize_string(name, cmd, |t| exp.push(t))
                }
                BaseCommand::Toks(ToksCommand::Value {name,..}) => {
                    do_esc!();
                    return crate::tex::token::tokenize_string(name, cmd, |t| exp.push(t))
                }
                BaseCommand::Toks(ToksCommand::Complex {name,..}) => {
                    do_esc!();
                    return crate::tex::token::tokenize_string(name, cmd, |t| exp.push(t))
                }
                BaseCommand::Toks(ToksCommand::Primitive(name)) => {
                    do_esc!();
                    return crate::tex::token::tokenize_string(name, cmd, |t| exp.push(t))
                }
                BaseCommand::Int(ValueCommand::Register(u)) => {
                    do_esc!();
                    format!("count{}",u)
                }
                BaseCommand::Dim(ValueCommand::Register(u)) => {
                    do_esc!();
                    format!("dimen{}",u)
                }
                BaseCommand::Skip(ValueCommand::Register(u)) => {
                    do_esc!();
                    format!("skip{}",u)
                }
                BaseCommand::MuSkip(ValueCommand::Register(u)) => {
                    do_esc!();
                    format!("muskip{}",u)
                }
                BaseCommand::Toks(ToksCommand::Register(u)) => {
                    do_esc!();
                    format!("toks{}",u)
                }
                BaseCommand::CharDef(c) => {
                    do_esc!();
                    format!("char\"{:X}",c.to_usize())
                }
                BaseCommand::MathChar(c) => {
                    do_esc!();
                    format!("mathchar\"{:X}",c)
                }
                BaseCommand::Font(fnt) => {
                    format!("select font {}",engine.fontstore.get(fnt))
                }
                BaseCommand::None => {
                    return crate::tex::token::tokenize_string("undefined", cmd, |t| exp.push(t))
                }
                BaseCommand::Def(d) => {
                    let cc = engine.state.get_catcode_scheme().clone();
                    if d.protected {
                        do_esc!();
                        crate::tex::token::tokenize_string("protected ", cmd, |t| exp.push(t))
                    }
                    if d.long {
                        do_esc!();
                        crate::tex::token::tokenize_string("long ", cmd, |t| exp.push(t))
                    }
                    if d.outer {
                        do_esc!();
                        crate::tex::token::tokenize_string("outer ", cmd, |t| exp.push(t))
                    }
                    crate::tex::token::tokenize_string("macro:", cmd, |t| exp.push(t));
                    let mut i = 0;
                    for s in &*d.signature {
                        match s {
                            ParamToken::Token(t) => {
                                crate::tex::token::detokenize(true,&engine.state,&engine.interner,t, cmd, |t| exp.push(t));
                            }
                            ParamToken::Param => {
                                i += 1;
                                crate::tex::token::tokenize_string(&format!("#{}", i), cmd, |t| exp.push(t));
                            }
                        }
                    }
                    if d.endswithbrace { exp.push(ET::Token::new_char_from_command(b'#'.into(),CategoryCode::Other,cmd)) }
                    exp.push(ET::Token::new_char_from_command(b'-'.into(),CategoryCode::Other,cmd));
                    exp.push(ET::Token::new_char_from_command(b'>'.into(),CategoryCode::Other,cmd));
                    for t in &*d.replacement {
                        match t {
                            ExpToken::Token(t) if t.is_parameter() => {
                                crate::tex::token::detokenize(true,&engine.state,&engine.interner,t, cmd, |t| exp.push(t));
                                crate::tex::token::detokenize(true,&engine.state,&engine.interner,t, cmd, |t| exp.push(t));
                            }
                            ExpToken::Token(t) =>
                                crate::tex::token::detokenize(true,&engine.state,&engine.interner,t, cmd, |t| exp.push(t)),
                            ExpToken::Param(t, i) => {
                                crate::tex::token::detokenize(true,&engine.state,&engine.interner,t, cmd, |t| exp.push(t));
                                crate::tex::token::tokenize_string(&(i+1).to_string(), cmd, |t| exp.push(t));
                            }
                        }
                    }
                    return ()
                }
            };
            crate::tex::token::tokenize_string(&string, cmd, |t| exp.push(t));
        }
    }
}

pub const MESSAGE:&str = "message";
pub fn message<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    debug_log!(debug=>"message");
    engine.skip_whitespace();
    let mut string = engine.memory.get_string();
    let msg = engine.get_braced_string(&mut string);
    (engine.outputs.message)(string.as_str());
    engine.memory.return_string(string);
}

pub fn mkern<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    debug_log!(trace => "\\mkern");
    match engine.state.mode() {
        TeXMode::Math | TeXMode::Displaymath => (),
        _ => throw!("\\mkern is only allowed in math mode" => cmd.cause)
    }
    let mudim = engine.get_mudim();
    let font = match engine.state.get_fontstyle() {
        FontStyle::Text => engine.state.get_textfont(2),
        FontStyle::Script => engine.state.get_scriptfont(2),
        FontStyle::Scriptscript => engine.state.get_scriptscriptfont(2),
    };
    let dim = engine.fontstore.get(font).get_dim::<ET::Dim>(6);
    let dim = mudim.to_dim::<ET::Dim>(dim);
    ET::Stomach::push_node(engine,TeXNode::Kern{dim,axis:HorV::Horizontal});
}

pub fn month<ET:EngineType>(engine:&mut EngineRef<ET>,cmd:&CommandSource<ET>) -> ET::Int {
    ET::Int::from_i64(engine.start_time.month() as i64)
}

pub const MOVERIGHT: &str = "moveright";
pub fn moveright<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    debug_log!(trace=>"\\moveright");
    let i = engine.get_dim();
    match engine.get_next_unexpandable() {
        None => file_end_prim!(RAISE,cmd),
        Some(c) => match c.command {
            BaseCommand::OpenBox {name,mode,apply} => {
                let f = apply(engine,c.source);
                engine.stomach.open_box(OpenBox::Box {list:vec!(),mode,on_close:Ptr::new(move |engine,v| {
                    let bx = match f(engine,v) {
                        Some(r) => {r}
                        None => {todo!("make void box")}
                    };
                    ET::Stomach::push_node(engine,SimpleNode::MoveRight {by:i,node:bx}.as_node());
                    None
                })})
            }
            BaseCommand::FinishedBox {get,..} => {
                let bx = get(engine,c.source);
                ET::Stomach::push_node(engine,SimpleNode::MoveRight {by:i,node:bx}.as_node());
            }
            _ => throw!("Box expected: {}",c.source.cause.printable(&engine.interner))
        }
    }
}

pub const MOVELEFT: &str = "moveleft";
pub fn moveleft<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    debug_log!(trace=>"\\moveleft");
    let i = engine.get_dim();
    match engine.get_next_unexpandable() {
        None => file_end_prim!(RAISE,cmd),
        Some(c) => match c.command {
            BaseCommand::OpenBox {name,mode,apply} => {
                let f = apply(engine,c.source);
                engine.stomach.open_box(OpenBox::Box {list:vec!(),mode,on_close:Ptr::new(move |engine,v| {
                    let bx = match f(engine,v) {
                        Some(r) => {r}
                        None => {todo!("make void box")}
                    };
                    ET::Stomach::push_node(engine,SimpleNode::MoveRight {by:-i,node:bx}.as_node());
                    None
                })})
            }
            BaseCommand::FinishedBox {get,..} => {
                let bx = get(engine,c.source);
                ET::Stomach::push_node(engine,SimpleNode::MoveRight {by:-i,node:bx}.as_node());
            }
            _ => throw!("Box expected: {}",c.source.cause.printable(&engine.interner))
        }
    }
}

pub fn mskip<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    debug_log!(trace => "\\mskip");
    match engine.state.mode() {
        TeXMode::Math | TeXMode::Displaymath => (),
        _ => throw!("\\mskip is only allowed in math mode" => cmd.cause)
    }
    let skip = engine.get_muskip();
    let font = match engine.state.get_fontstyle() {
        FontStyle::Text => engine.state.get_textfont(2),
        FontStyle::Script => engine.state.get_scriptfont(2),
        FontStyle::Scriptscript => engine.state.get_scriptscriptfont(2),
    };
    let dim = engine.fontstore.get(font).get_dim::<ET::Dim>(6);
    let skip = skip.to_skip::<ET::Dim,ET::SkipDim>(dim);
    ET::Stomach::push_node(engine,SkipNode::Skip{skip,axis:HorV::Horizontal}.as_node());
}

pub const MULTIPLY:&str = "multiply";
pub fn multiply<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>, global:bool) {
    debug_log!(trace=>"\\multiply");
    engine.skip_whitespace();
    match engine.get_next_unexpandable() {
        None => file_end_prim!(MULTIPLY,cmd),
        Some(ncmd) => match ncmd.command {
            BaseCommand::Int(a) => {
                macro_rules! finish {
                    ($old:expr,$nv:ident => $set:expr) => {{
                        catch_prim!(engine.get_keyword("by") => (MULTIPLY,cmd));
                        let i = catch_prim!(engine.get_int() => (MULTIPLY,cmd));
                        let $nv = $old * i;
                        $set;
                    }}
                }
                match a {
                    ValueCommand::Register(u) => finish!(engine.state.get_int_register(u),nv => engine.state.set_int_register(u,nv,global)),
                    ValueCommand::Primitive(name) => finish!(engine.state.get_primitive_int(name),nv => engine.state.set_primitive_int(name,nv,global)),
                    ValueCommand::Complex {name,..} if name == COUNT => {
                        let int = engine.get_int();
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
                    }}
                }
                match a {
                    ValueCommand::Register(u) => finish!(engine.state.get_dim_register(u),nv => engine.state.set_dim_register(u,nv,global)),
                    ValueCommand::Primitive(name) => finish!(engine.state.get_primitive_dim(name),nv => engine.state.set_primitive_dim(name,nv,global)),
                    ValueCommand::Complex {name,..} if name == DIMEN => {
                        let int = engine.get_int();
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
                    }}
                }
                match a {
                    ValueCommand::Register(u) => finish!(engine.state.get_skip_register(u),nv => engine.state.set_skip_register(u,nv,global)),
                    ValueCommand::Primitive(name) => finish!(engine.state.get_primitive_skip(name),nv => engine.state.set_primitive_skip(name,nv,global)),
                    ValueCommand::Complex {name,..} if name == SKIP => {
                        let int = engine.get_int();
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
                    }}
                }
                match a {
                    ValueCommand::Register(u) => finish!(engine.state.get_muskip_register(u),nv => engine.state.set_muskip_register(u,nv,global)),
                    ValueCommand::Primitive(name) => finish!(engine.state.get_primitive_muskip(name),nv => engine.state.set_primitive_muskip(name,nv,global)),
                    ValueCommand::Complex {name,..} if name == MUSKIP => {
                        let int = engine.get_int();
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

pub const MUSKIP : &str = "muskip";
pub fn muskip_assign<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>, global:bool) {
    debug_log!(trace=>"Assigning \\muskip");
    let i = engine.get_int();
    let i:usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => throw!("Not a valid register: {}",i => cmd.cause)
    };
    engine.skip_eq_char();
    let v = engine.get_muskip();
    debug_log!(debug=>"\\muskip{} = {}",i,v);
    engine.state.set_muskip_register(i,v,global);
}

pub fn muskip_get<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) -> MuSkip<ET::MuDim,ET::MuStretchShrinkDim> {
    debug_log!(trace=>"Getting \\muskip");
    let i = engine.get_int();
    let i:usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => throw!("Not a valid register: {}",i => cmd.cause)
    };
    let v = engine.state.get_muskip_register(i);
    debug_log!(debug=>"\\muskip{} == {}",i,v);
    v
}

pub const MUSKIPDEF : &str = "muskipdef";
pub fn muskipdef<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>, global:bool) {
    debug_log!(trace=>"muskipdef");
    let name = engine.get_control_sequence();
    engine.set_relax(name,&cmd,global);
    engine.skip_eq_char();
    let num = engine.get_int();
    if num.to_i64() < 0 {
        throw!("Invalid muskip register index: {}",num => cmd.cause)
    }
    let num = num.to_i64() as usize;
    let ret = Command::new(BaseCommand::MuSkip(ValueCommand::Register(num)),Some(&cmd));
    engine.set_command_for_tk(name,Some(ret),global);
}

pub const NEWLINECHAR : &str = "newlinechar";
pub fn newlinechar_assign<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>, global:bool) {
    debug_log!(trace=>"Assigning \\newlinechar");
    engine.skip_eq_char();
    let i = engine.get_int();
    let c = match i.to_i64() {
        -1 => None,
        i => match ET::Char::from_i64(i) {
            Some(c) => Some(c),
            None => throw!("Not a valid character: {}",i => cmd.cause)
        }
    };
    debug_log!(debug=>"\\newlinechar = {:?}",c.map(|c| c.char_str()));
    engine.state.set_newlinechar(c,global);
}
pub fn newlinechar_get<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>)
                                      -> ET::Int {
    debug_log!(trace=>"Getting \\newlinechar");
    let c = match engine.state.get_newlinechar() {
        None => -1,
        Some(c) => c.to_usize() as i64
    };
    debug_log!(debug=>"\\newlinechar == {}",c);
    ET::Int::from_i64(c)
}

pub const NOALIGN: &str = "noalign";
pub fn noalign<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    throw!("Unexpected \\noalign")
}

pub const NOEXPAND: &str = "noexpand";
pub const NOEXPAND_INTERNAL: &str = "!!!§%^°`/{\"NO$EXPAND\"}\\`°^%§!!!";
/// invariant: adds token as nonexpanded to the gullet iff the original token was expandable
/// in the first place
pub fn noexpand<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    debug_log!(trace=>"\\noexpand");
    match engine.get_next_token() {
        None => file_end_prim!(NOEXPAND,cmd),
        Some((t,_)) => {
            let res = resolve_token::<ET>(&engine.state,t);
            match res.command {
                BaseCommand::Def(_) | BaseCommand::Expandable {..} | BaseCommand::ExpandableNoTokens {..} | BaseCommand::Conditional {..} =>
                    engine.mouth.push_noexpand(res.source.cause,&engine.interner),
                BaseCommand::Char{catcode:CategoryCode::EOL,..} => (),
                _ => engine.mouth.requeue(res.source.cause)
            }
        }
    }
}

pub fn noindent<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    debug_log!(trace=>"noindent");
    let sd = engine.stomach.shipout_data_mut();
    match sd.box_stack.last_mut() {
        Some(OpenBox::Paragraph {list,..}) => {
            loop {
                match list.last_mut() {
                    Some(TeXNode::Box(HVBox::H(b))) if b.kind == "indent" => {b.children.pop();}
                    _ => break
                }
            }
        }
        _ => ()
    }
}

pub const NUMBER: &str = "number";
pub fn number<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>, exp:&mut Vec<ET::Token>) {
    debug_log!(trace=>"\\number");
    let num = engine.get_int();
    crate::tex::token::tokenize_string(&num.to_i64().to_string(), cmd, |t| exp.push(t));
}

pub const OMIT: &str = "omit";
pub fn omit<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    throw!("Unexpected \\omit")
}

pub const OPENIN: &str = "openin";
pub fn openin<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    debug_log!(trace=>"\\openin");
    let i = engine.get_int();
    let i : usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => throw!("Invalid file number: {}",i => cmd.cause)
    };
    engine.skip_eq_char();
    let mut filename = engine.memory.get_string();
    engine.get_string(&mut filename);
    let f = engine.filesystem.get(&filename);
    engine.memory.return_string(filename);
    engine.state.file_openin(i,f,&mut engine.interner); // TODO error?
}

pub const OPENOUT: &str = "openout";
pub fn openout<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) -> Whatsit<ET> {
    debug_log!(trace=>"\\openout");
    let i = engine.get_int();
    let i : usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => throw!("Invalid file number: {}",i => cmd.cause)
    };
    engine.skip_eq_char();
    let mut filename = engine.memory.get_string();
    engine.get_string(&mut filename);
    let apply = Box::new(move |e:&mut EngineRef<ET>| {
        let f = e.filesystem.get(&filename);
        e.memory.return_string(filename);
        e.state.file_openout(i,f); // TODO error?
    });
    Whatsit::new(apply)
}

pub const OR : &str = "or";
pub fn or<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    match engine.gullet.current_conditional() {
        (Some(ConditionalBranch::Case(_,_)),i) =>
            crate::engine::gullet::methods::else_loop::<ET>(engine,IFCASE,i,true),
        _ => throw!("Not in an \\ifcase" => cmd.cause)
    }
}

pub const OUTER: &str = "outer";
pub fn outer<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>, global_:bool, protected_:bool, long_:bool, outer_:bool) {
    debug_log!(trace => "\\outer");
    match engine.get_next_stomach_command() {
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

pub fn pagegoal_assign<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>, global:bool) {
    debug_log!(trace=>"Assigning \\pagegoal");
    engine.skip_eq_char();
    let i = engine.get_dim();
    engine.stomach.shipout_data_mut().pagegoal = i;
}

pub fn pagegoal_get<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) -> ET::Dim {
    debug_log!(trace=>"Getting \\pagegoal");
    engine.stomach.shipout_data().pagegoal
}

pub fn pagetotal_assign<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>, global:bool) {
    debug_log!(trace=>"Assigning \\pagetotal");
    engine.skip_eq_char();
    let i = engine.get_dim();
    engine.stomach.shipout_data_mut().pagetotal = i;
}
pub fn pagetotal_get<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) -> ET::Dim {
    debug_log!(trace=>"Getting \\pagetotal");
    engine.stomach.shipout_data().pagetotal
}

pub fn pagestretch_assign<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>, global:bool) {
    debug_log!(trace=>"Assigning \\pagestretch");
    engine.skip_eq_char();
    let i = engine.get_dim();
    engine.stomach.shipout_data_mut().pagestretch = i;
}
pub fn pagestretch_get<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) -> ET::Dim {
    debug_log!(trace=>"Getting \\pagestretch");
    engine.stomach.shipout_data().pagestretch
}

pub fn pagefilstretch_assign<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>, global:bool) {
    debug_log!(trace=>"Assigning \\pagefilstretch");
    engine.skip_eq_char();
    let i = engine.get_dim();
    engine.stomach.shipout_data_mut().pagefilstretch = i;
}
pub fn pagefilstretch_get<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) -> ET::Dim {
    debug_log!(trace=>"Getting \\pagefilstretch");
    engine.stomach.shipout_data().pagefilstretch
}

pub fn pagefillstretch_assign<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>, global:bool) {
    debug_log!(trace=>"Assigning \\pagefillstretch");
    engine.skip_eq_char();
    let i = engine.get_dim();
    engine.stomach.shipout_data_mut().pagefillstretch = i;
}
pub fn pagefillstretch_get<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) -> ET::Dim {
    debug_log!(trace=>"Getting \\pagefillstretch");
    engine.stomach.shipout_data().pagefillstretch
}

pub fn pagefilllstretch_assign<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>, global:bool) {
    debug_log!(trace=>"Assigning \\pagefilllstretch");
    engine.skip_eq_char();
    let i = engine.get_dim();
    engine.stomach.shipout_data_mut().pagefilllstretch = i;
}
pub fn pagefilllstretch_get<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) -> ET::Dim {
    debug_log!(trace=>"Getting \\pagefilllstretch");
    engine.stomach.shipout_data().pagefilllstretch
}

pub fn pageshrink_assign<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>, global:bool) {
    debug_log!(trace=>"Assigning \\pageshrink");
    engine.skip_eq_char();
    let i = engine.get_dim();
    engine.stomach.shipout_data_mut().pageshrink = i;
}
pub fn pageshrink_get<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) -> ET::Dim {
    debug_log!(trace=>"Getting \\pageshrink");
    engine.stomach.shipout_data().pageshrink
}

pub fn pagedepth_assign<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>, global:bool) {
    debug_log!(trace=>"Assigning \\pagedepth");
    engine.skip_eq_char();
    let i = engine.get_dim();
    engine.stomach.shipout_data_mut().pagedepth = i;
}
pub fn pagedepth_get<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) -> ET::Dim {
    debug_log!(trace=>"Getting \\pagedepth");
    engine.stomach.shipout_data().pagedepth
}

pub const PAR: &str = "par";
pub fn par<ET:EngineType>(engine:&mut EngineRef<ET>, _cmd:&CommandSource<ET>) {
    debug_log!(trace=>"par");
}

pub fn parshape_assign<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>, global:bool) {
    debug_log!(trace=>"Assigning parshape");
    let len = engine.get_int().to_i64();
    if len < 0 {
        throw!("Invalid parshape length: {}",len => cmd.cause)
    }
    let mut vals : Vec<(ET::Dim,ET::Dim)> = Vec::with_capacity(len as usize);
    for i in 0..len {
        let a = engine.get_dim();
        let b = engine.get_dim();
        vals.push((a,b));
    }
    engine.state.set_parshape(vals,false);
}
pub fn parshape_get<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) -> ET::Int {
    debug_log!(trace=>"Getting parshape");
    match engine.state.get_parshape() {
        None => ET::Int::from_i64(0),
        Some(v) => ET::Int::from_i64(v.len() as i64)
    }
}

pub const PATTERNS: &str = "patterns";
pub fn patterns<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    debug_log!(trace=>"\\patterns");
    // TODO
    let mut v = engine.memory.get_token_vec();
    engine.get_argument(&mut v);
    engine.memory.return_token_vec(v);
}

pub const PENALTY: &str = "penalty";
pub fn penalty<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    debug_log!(trace=>"\\penalty");
    let i = engine.get_int().to_i64() as i32;
    ET::Stomach::push_node(engine,TeXNode::Penalty(i));
}

pub const PREVDEPTH : &str = "prevdepth";
pub fn prevdepth_assign<ET:EngineType>(engine: &mut EngineRef<ET>, cmd:&CommandSource<ET>, global:bool) {
    debug_log!(trace=>"Assigning \\prevdepth");
    engine.skip_eq_char();
    let v = engine.get_dim();
    debug_log!(debug=>"\\prevdepth = {}",v);
    engine.stomach.shipout_data_mut().prevdepth = v;
}

pub fn prevdepth_get<ET:EngineType>(engine: &mut EngineRef<ET>, cmd:&CommandSource<ET>) -> ET::Dim {
    debug_log!(trace=>"Getting \\prevdepth");
    engine.stomach.shipout_data().prevdepth
}

pub const RAISE: &str = "raise";
pub fn raise<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    debug_log!(trace=>"\\raise");
    let i = engine.get_dim();
    match engine.get_next_unexpandable() {
        None => file_end_prim!(RAISE,cmd),
        Some(c) => match c.command {
            BaseCommand::OpenBox {name,mode,apply} => {
                let f = apply(engine,c.source);
                engine.stomach.open_box(OpenBox::Box {list:vec!(),mode,on_close:Ptr::new(move |engine,v| {
                    let bx = match f(engine,v) {
                        Some(r) => {r}
                        None => {todo!("make void box")}
                    };
                    ET::Stomach::push_node(engine,SimpleNode::Raise {by:i,node:bx}.as_node());
                    None
                })})
            }
            BaseCommand::FinishedBox {get,..} => {
                let bx = get(engine,c.source);
                ET::Stomach::push_node(engine,SimpleNode::Raise {by:i,node:bx}.as_node());
            }
            _ => throw!("Box expected: {}",c.source.cause.printable(&engine.interner))
        }
    }
}

pub const READ: &str = "read";
pub fn read<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>, globally:bool) {
    debug_log!(trace=>"read");
    let i = engine.get_int();
    let i : usize = match i.try_into() {
        Ok(i) => i,
        Err(_) => throw!("Invalid file number" => cmd.cause)
    };
    let file = match engine.state.get_open_in_file(i) {
        None => throw!("File {} not open for reading",i),
        Some(f) => f
    };
    if !engine.get_keyword("to") {
        throw!("Expected 'to' after \\read" => cmd.cause)
    }
    let newcmd = engine.get_control_sequence();
    let mut ret = vec!();
    file.read::<ET,_>(&mut engine.interner,engine.state.get_catcode_scheme(),engine.state.get_endlinechar(),|t| ret.push(t));
    debug_log!(trace=>"read: {} = {}",newcmd.printable(&engine.interner),PrintableTokenList::<ET>(&ret,&engine.interner));
    if ret.is_empty() {
        match engine.state.get_endlinechar() {
            None => (),
            Some(c) => ret.push(ET::Token::new_char_from_command(c,*engine.state.get_catcode_scheme().get(c),cmd))
        }
    }
    let def = Command::new(BaseCommand::Def(Def::<ET>::simple(ret)),Some(&cmd));
    engine.set_command_for_tk(newcmd,Some(def),globally);
}

pub const RELAX: &str = "relax";

pub const ROMANNUMERAL: &str = "romannumeral";
pub fn romannumeral<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>, exp:&mut Vec<ET::Token>) {
    debug_log!(trace=>"romannumeral");
    macro_rules! push {
        ($c:expr) => {exp.push(ET::Token::new_char_from_command($c.into(),CategoryCode::Other,cmd))}
    }
    let mut num = engine.get_int().to_i64();
    if num <= 0 {
        return ()
    }
    while num >= 1000 {
        num -= 1000;
        push!(b'm');
    }
    if num >= 900 {
        num -= 900;
        push!(b'c');
        push!(b'm');
    }
    if num >= 500 {
        num -= 500;
        push!(b'd');
    }
    if num >= 400 {
        num -= 400;
        push!(b'c');
        push!(b'd');
    }
    while num >= 100 {
        num -= 100;
        push!(b'c');
    }
    if num >= 90 {
        num -= 90;
        push!(b'x');
        push!(b'c');
    }
    if num >= 50 {
        num -= 50;
        push!(b'l');
    }
    if num >= 40 {
        num -= 40;
        push!(b'x');
        push!(b'l');
    }
    while num >= 10 {
        num -= 10;
        push!(b'x');
    }
    if num >= 9 {
        num -= 9;
        push!(b'i');
        push!(b'x');
    }
    if num >= 5 {
        num -= 5;
        push!(b'v');
    }
    if num >= 4 {
        num -= 4;
        push!(b'i');
        push!(b'v');
    }
    while num >= 1 {
        num -= 1;
        push!(b'i');
    }
}

pub fn scriptfont_assign<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>, global:bool) {
    debug_log!(trace=>"Assigning \\scriptfont");
    let num = engine.get_int().to_i64();
    if num < 0 || num > 15 {
        throw!("Invalid font number: {}",num => cmd.cause)
    }
    let fnt = engine.get_font();
    engine.state.set_scriptfont(num as usize,fnt,global);
}
pub fn scriptfont_get<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>)
                               -> ET::FontRef {
    debug_log!(trace=>"Getting \\scriptfont");
    let num = engine.get_int().to_i64();
    if num < 0 || num > 15 {
        throw!("Invalid font number: {}",num => cmd.cause)
    }
    engine.state.get_scriptfont(num as usize)
}


pub fn scriptscriptfont_assign<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>, global:bool) {
    debug_log!(trace=>"Assigning \\scriptscriptfont");
    let num = engine.get_int().to_i64();
    if num < 0 || num > 15 {
        throw!("Invalid font number: {}",num => cmd.cause)
    }
    let fnt = engine.get_font();
    engine.state.set_scriptscriptfont(num as usize,fnt,global);
}
pub fn scriptscriptfont_get<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>)
                                     -> ET::FontRef {
    debug_log!(trace=>"Getting \\sscriptcriptfont");
    let num = engine.get_int().to_i64();
    if num < 0 || num > 15 {
        throw!("Invalid font number: {}",num => cmd.cause)
    }
    engine.state.get_scriptscriptfont(num as usize)
}

pub const SETBOX: &str = "setbox";
pub fn setbox<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>, global:bool) {
    debug_log!(trace=>"\\setbox");
    let i = engine.get_int().to_i64();
    if i < 0  { throw!("Invalid box number: {}",i => cmd.cause) }
    engine.skip_eq_char();
    match engine.get_next_unexpandable() {
        None => file_end_prim!(SETBOX,cmd),
        Some(c) => match c.command {
            BaseCommand::OpenBox {name,mode,apply} => {
                let f = apply(engine,c.source);
                engine.stomach.open_box(OpenBox::Box {list:vec!(),mode,on_close:Ptr::new(move |e,v| {
                    let bx = match f(e,v) {
                        Some(r) => {r}
                        None => {todo!("make void box")}
                    };
                    e.state.set_box_register(i as usize,bx,global);
                    None
                })})
            }
            BaseCommand::FinishedBox {get,..} => {
                let bx = get(engine,c.source);
                engine.state.set_box_register(i as usize,bx,global);
            }
            _ => throw!("Box expected: {}",c.source.cause.printable(&engine.interner))
        }
    }
}

pub const SFCODE: &str = "sfcode";
pub fn sfcode_assign<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>, global:bool) {
    debug_log!(trace=>"Assigning space factor code");
    let c = engine.get_char();
    engine.skip_eq_char();
    let v = engine.get_int();
    debug_log!(debug=>"\\sfcode '{}' = {}",c.char_str(),v);
    engine.state.set_sfcode(c,v,global);
}
pub fn sfcode_get<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) -> ET::Int {
    debug_log!(trace=>"Getting space factor code");
    let c = engine.get_char();
    let v = engine.state.get_sfcode(c);
    debug_log!(debug=>"\\sfcode '{}' == {}",c.char_str(),v);
    v
}

pub const SHIPOUT: &str = "shipout";
pub fn shipout<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    debug_log!(trace => "\\shipout");
    match engine.get_next_unexpandable() {
        None => file_end_prim!(SHIPOUT,cmd),
        Some(c) => match c.command {
            BaseCommand::OpenBox {name,mode,apply} => {
                let f = apply(engine,c.source);
                engine.stomach.open_box(OpenBox::Box {list:vec!(),mode,on_close:Ptr::new(move |e,v| {
                    let bx = match f(e,v) {
                        Some(r) => {r}
                        None => {todo!("make void box")}
                    };
                    e.stomach.shipout(bx);
                    None
                })});
            }
            BaseCommand::FinishedBox {get,..} => {
                let bx = get(engine,c.source);
                engine.stomach.shipout(bx);
            }
            _ => throw!("Box expected: {}",c.source.cause.printable(&engine.interner))
        }
    }
}

pub const SKEWCHAR: &str = "skewchar";
pub fn skewchar_assign<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>, global:bool) {
    debug_log!(trace=>"Assigning \\kewchar");
    let fontid = engine.get_font();
    engine.skip_eq_char();
    let i = engine.get_int().to_i64();
    let mut font = engine.fontstore.get_mut(fontid);
    debug_log!(debug=>"\\skewchar\\{:?} = {:?}",font,i);
    font.set_skewchar(i);
}
pub fn skewchar_get<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) -> ET::Int {
    debug_log!(trace=>"Getting \\skewchar");
    let fontid = engine.get_font();
    let sk = engine.fontstore.get(fontid).get_skewchar();
    debug_log!(debug=>"\\skewchar == {:?}",sk);
    ET::Int::from_i64(sk)
}

pub const SKIP : &str = "skip";
pub fn skip_assign<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>, global:bool){
    debug_log!(trace=>"Assigning \\skip");
    let i = engine.get_int();
    let i:usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => throw!("Not a valid register: {}",i => cmd.cause)
    };
    engine.skip_eq_char();
    let v = engine.get_skip();
    debug_log!(debug=>"\\skip{} = {}",i,v);
    engine.state.set_skip_register(i,v,global);
}

pub fn skip_get<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) -> Skip<ET::SkipDim> {
    debug_log!(trace=>"Getting \\skip");
    let i = engine.get_int();
    let i:usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => throw!("Not a valid register: {}",i => cmd.cause)
    };
    let v = engine.state.get_skip_register(i);
    debug_log!(debug=>"\\skip{} == {}",i,v);
    v
}

pub const SKIPDEF : &str = "skipdef";
pub fn skipdef<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>, global:bool) {
    debug_log!(trace=>"skipdef");
    let name = engine.get_control_sequence();
    engine.set_relax(name,&cmd,global);
    engine.skip_eq_char();
    let num = engine.get_int();
    if num.to_i64() < 0 {
        throw!("Invalid skip register index: {}",num => cmd.cause)
    }
    let num = num.to_i64() as usize;
    let ret = Command::new(BaseCommand::Skip(ValueCommand::Register(num)),Some(&cmd));
    engine.set_command_for_tk(name,Some(ret),global);
}

pub fn spacefactor_assign<ET:EngineType>(engine: &mut EngineRef<ET>, cmd:&CommandSource<ET>, global:bool) {
    debug_log!(trace=>"Assigning space factor");
    engine.skip_eq_char();
    let v = engine.get_int().to_i64();
    engine.stomach.shipout_data_mut().spacefactor = v as i32;
}
pub fn spacefactor_get<ET:EngineType>(engine: &mut EngineRef<ET>, cmd:&CommandSource<ET>) -> ET::Int {
    debug_log!(trace=>"Getting space factor");
    ET::Int::from_i64(engine.stomach.shipout_data().spacefactor as i64)
}

pub const SPAN: &str = "span";
pub fn span<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    todo!()
}

pub const STRING: &str = "string";
pub fn string<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>, exp:&mut Vec<ET::Token>) {
    debug_log!(trace=>"string");
    match engine.get_next_token() {
        None => file_end_prim!("string",cmd),
        Some((t,_)) => {
            crate::tex::token::detokenize(false,&engine.state,&engine.interner,&t, cmd, |t| exp.push(t));
        }
    }
}

pub fn textfont_assign<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>, global:bool) {
    debug_log!(trace=>"Assigning \\textfont");
    let num = engine.get_int().to_i64();
    if num < 0 || num > 15 {
        throw!("Invalid font number: {}",num => cmd.cause)
    }
    let fnt = engine.get_font();
    engine.state.set_textfont(num as usize,fnt,global);
}
pub fn textfont_get<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>)
                                     -> ET::FontRef {
    debug_log!(trace=>"Getting \\textfont");
    let num = engine.get_int().to_i64();
    if num < 0 || num > 15 {
        throw!("Invalid font number: {}",num => cmd.cause)
    }
    engine.state.get_textfont(num as usize)
}

pub const THE : &str = "the";

pub fn do_the<ET:EngineType,F: FnMut(&mut EngineRef<ET>, ET::Token)>(engine:&mut EngineRef<ET>,cmd:&CommandSource<ET>,mut f:F) {
    match engine.get_next_unexpandable() {
        Some(c) => match c.command {
            BaseCommand::Int(ass) => {
                let val = ass.get(engine,c.source);
                crate::tex::token::tokenize_string(&format!("{}", val), cmd, |t| f(engine, t))
            }
            BaseCommand::Dim(ass) => {
                let val = ass.get(engine,c.source);
                crate::tex::token::tokenize_string(&format!("{}", val), cmd, |t| f(engine, t))
            }
            BaseCommand::Skip(ass) => {
                let val = ass.get(engine,c.source);
                crate::tex::token::tokenize_string(&format!("{}", val), cmd, |t| f(engine, t))
            }
            BaseCommand::MuSkip(ass) => {
                let val = ass.get(engine,c.source);
                crate::tex::token::tokenize_string(&format!("{}", val), cmd, |t| f(engine, t))
            }
            BaseCommand::Toks(ass) => {
                let v = ass.get(engine, c.source).cloned();
                match v {
                    Some(v) if !v.is_empty() => for t in v { f(engine,t) }
                        _ => ()
                }
            }
            BaseCommand::MathChar(u) =>
                crate::tex::token::tokenize_string(&u.to_string(), cmd, |t| f(engine, t)),
            BaseCommand::CharDef(c) =>
                crate::tex::token::tokenize_string(&c.to_usize().to_string(), cmd, |t| f(engine, t)),
            BaseCommand::Font(fnt) => {
                let t = ET::Token::new_cs_from_command(engine.fontstore.get(fnt).name(),&c.source); f(engine,t)
            },
            BaseCommand::FontCommand {get,..} => {
                let fnt = get(engine,c.source.clone());
                let t = ET::Token::new_cs_from_command(engine.fontstore.get(fnt).name(),&c.source); f(engine,t)
            }
            _ => throw!("Expected a value after \\the; got: {}", c.source.cause.printable(&engine.interner) => c.source.cause)
        }
        None => file_end!()
    }
}

pub fn the<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>, exp:&mut Vec<ET::Token>) {
    debug_log!(trace => "\\the");
    do_the(engine,cmd,|_,t| exp.push(t));
}

pub fn time<ET:EngineType>(engine:&mut EngineRef<ET>,cmd:&CommandSource<ET>) -> ET::Int {
    let t = &engine.start_time;
    ET::Int::from_i64( ((t.hour() * 60) + t.minute()) as i64 )
}

pub const TOKS:&str = "toks";
pub fn toks_assign<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>, global:bool) {catch_prim!({
    debug_log!(trace=>"Assigning \\toks");
    let i = engine.get_int();
    let i:usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => throw!("Not a valid register: {}",i => cmd.cause.clone())
    };
    engine.skip_eq_char();
    let mut v = engine.memory.get_token_vec();

    engine.expand_until_group(|_,t| v.push(t));
    //catch_prim!(engine.get_group(&mut |_,t| Ok(v.push(t))) => (TOKS,cmd));

    debug_log!(debug=>"\\toks{} = {}",i,PrintableTokenList::<ET>(&v,&engine.interner));
    engine.state.set_toks_register(i,v,global,&mut engine.memory);
} => (TOKS,cmd))}
pub fn toks_get<'a,ET:EngineType>(engine:&'a mut EngineRef<ET>, cmd:&CommandSource<ET>)
                                  -> Option<&'a Vec<ET::Token>> {
    let i = catch_prim!({
        let i = engine.get_int();
        let i:usize = match i.clone().try_into() {
            Ok(i) => i,
            Err(_) => throw!("Not a valid register: {}",i)
        };
        i
    } => (TOKS,cmd));
    engine.state.get_toks_register(i)
}

pub const TOKSDEF : &str = "toksdef";
pub fn toksdef<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>, global:bool) {
    debug_log!(trace=>"toksdef");
    let name = engine.get_control_sequence();
    engine.set_relax(name,&cmd,global);
    engine.skip_eq_char();
    let num = engine.get_int().to_i64();
    if num < 0 {
        throw!("Invalid token register index: {}",num => cmd.cause)
    }
    let num = num as usize;
    let ret = Command::new(BaseCommand::Toks(ToksCommand::Register(num)),Some(&cmd));
    engine.set_command_for_tk(name,Some(ret),global);
}

pub const UCCODE : &str = "uccode";
pub fn uccode_assign<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>, global:bool) {
    debug_log!(trace=>"Assigning upper case character");
    let c = engine.get_char();
    engine.skip_eq_char();
    let lc = engine.get_char();
    debug_log!(debug=>"\\uccode '{}' = {}",c.char_str(),lc.char_str());
    engine.state.set_uccode(c,lc,global);
}

pub fn uccode_get<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) -> ET::Int {
    debug_log!(trace=>"Getting upper case character");
    let c = engine.get_char();
    let v = ET::Int::from_i64(engine.state.get_uccode(c).to_usize() as i64);
    debug_log!(debug=>"\\uccode '{}' == {}",c.char_str(),v);
    v
}

pub fn unhbox<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    debug_log!(trace => "\\unhbox");
    let i = engine.get_int().to_i64();
    if i < 0  { throw!("Invalid box number: {}",i => cmd.cause) }
    let bx = engine.state.take_box_register(i as usize);
    match bx {
        HVBox::H(hb) => {
            for n in hb.children {
                ET::Stomach::push_node(engine,n);
            }
        }
        HVBox::Void => (),
        _ => throw!("incompatible list can't be unboxed" => cmd.cause)
    }
}
pub fn unhcopy<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    debug_log!(trace => "\\unhcopy");
    let i = engine.get_int().to_i64();
    if i < 0  { throw!("Invalid box number: {}",i => cmd.cause) }
    let bx = match engine.state.get_box_register(i as usize).cloned() {
        None => return (),
        Some(b) => {
            match b {
                HVBox::H(hb) => {
                    for n in hb.children {
                        ET::Stomach::push_node(engine,n);
                    }
                }
                HVBox::Void => (),
                _ => throw!("incompatible list can't be unboxed" => cmd.cause)
            }

        }
    };
}
pub fn unvbox<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    debug_log!(trace => "\\unvbox");
    let i = engine.get_int().to_i64();
    if i < 0  { throw!("Invalid box number: {}",i => cmd.cause) }
    let bx = engine.state.take_box_register(i as usize);
    match bx {
        HVBox::V(hb) => {
            for n in hb.children {
                ET::Stomach::push_node(engine,n);
            }
        }
        HVBox::Void => (),
        _ => throw!("incompatible list can't be unboxed" => cmd.cause)
    }
}
pub fn unvcopy<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    debug_log!(trace => "\\unvcopy");
    let i = engine.get_int().to_i64();
    if i < 0  { throw!("Invalid box number: {}",i => cmd.cause) }
    let bx = match engine.state.get_box_register(i as usize).cloned() {
        None => return (),
        Some(b) => {
            match b {
                HVBox::V(hb) => {
                    for n in hb.children {
                        ET::Stomach::push_node(engine,n);
                    }
                }
                HVBox::Void => (),
                _ => throw!("incompatible list can't be unboxed" => cmd.cause)
            }

        }
    };
}

pub fn unskip<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    let mut sd = engine.stomach.shipout_data_mut();
    let mut ls = match sd.box_stack.last_mut() {
        Some(b) => b.ls_mut(),
        None => &mut sd.page
    };
    loop {
        match ls.last() {
            Some(TeXNode::Skip(_)) => {ls.pop();}
            _ => break
        }
    }
}
pub fn unkern<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    let mut sd = engine.stomach.shipout_data_mut();
    let mut ls = match sd.box_stack.last_mut() {
        Some(b) => b.ls_mut(),
        None => &mut sd.page
    };
    loop {
        match ls.last() {
            Some(TeXNode::Kern{..}) => {ls.pop();}
            _ => break
        }
    }
}
pub fn unpenalty<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    let mut sd = engine.stomach.shipout_data_mut();
    let mut ls = match sd.box_stack.last_mut() {
        Some(b) => b.ls_mut(),
        None => &mut sd.page
    };
    loop {
        match ls.last() {
            Some(TeXNode::Penalty(_)) => {ls.pop();}
            _ => break
        }
    }
}

pub const UPPERCASE: &str = "uppercase";
pub fn uppercase<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    debug_log!(trace => "\\uppercase");
    let mut rs = engine.mouth.get_expansion();
    engine.expand_until_group(|engine,next| match next.split_char_and_catcode() {
        Some((c,cc)) => {
            let nc = engine.state.get_uccode(c);
            if nc.to_usize() == 0 { rs.push(next) }
            else { rs.push(ET::Token::new_char_from_command(nc, cc, cmd)) }
        }
        _ => rs.push(next)
    });
    engine.mouth.push_expansion(rs);
}

pub fn vadjust<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) -> CloseBoxFun<ET> {
    debug_log!(trace=>"\\vadjust");
    while let Some(next) = engine.get_next_unexpandable() {
        match next.command {
            BaseCommand::Char{catcode:CategoryCode::Space,..} => {},
            BaseCommand::Relax => {},
            BaseCommand::Char{catcode:CategoryCode::BeginGroup,..} => {
                engine.state.stack_push(GroupType::Box(BoxMode::V));
                engine.mouth.insert_every(&engine.state,"everyvbox");
                return Ptr::new(move |engine,children| {
                    ET::Stomach::push_node(engine,TeXNode::VAdjust(children));
                    None
                })
            }
            _ => throw!("Expected begin group, found {:?}",next.source.cause => cmd.cause)
        }
    }
    file_end_prim!("vadjust",cmd);
}

pub const VALIGN: &str = "valign";
pub fn valign<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    let ht = if engine.get_keyword("to") {
        Some(engine.get_dim())
    } else {
        None
    };
    do_align(engine,cmd,BoxMode::V,BoxMode::H,ht)
}

pub const VBOX: &str = "vbox";
pub fn vbox<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) -> CloseBoxFun<ET> {
    debug_log!(trace=>"\\vbox");
    let (to,spread) = match engine.get_keywords(vec!("spread","to")) {
        None => (None,None),
        Some(s) if s == "to" => {
            let a = engine.get_dim();
            (Some(a),None)
        },
        Some(s) if s == "spread" => {
            let a = engine.get_dim();
            (None,Some(a))
        },
        _ => unreachable!()
    };
    while let Some(next) = engine.get_next_unexpandable() {
        match next.command {
            BaseCommand::Char{catcode:CategoryCode::Space,..} => {},
            BaseCommand::Relax => {},
            BaseCommand::Char{catcode:CategoryCode::BeginGroup,..} => {
                engine.state.stack_push(GroupType::Box(BoxMode::V));
                engine.mouth.insert_every(&engine.state,"everyvbox");
                return Ptr::new(move |e,children| {
                    Some(HVBox::V(VBox {
                        children, to, spread,
                        ..Default::default()
                    }))
                })
            }
            _ => throw!("Expected begin group, found {:?}",next.source.cause => cmd.cause)
        }
    }
    file_end_prim!("vbox",cmd);
}

pub fn vcenter<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) -> CloseBoxFun<ET> {
    debug_log!(trace=>"\\vcenter");
    match engine.state.mode() {
        TeXMode::Displaymath | TeXMode::Math => (),
        _ => throw!("\\vcenter can only be used in math mode" => cmd.cause)
    }
    let (to,spread) = match engine.get_keywords(vec!("spread","to")) {
        None => (None,None),
        Some(s) if s == "to" => {
            let a = engine.get_dim();
            (Some(a),None)
        },
        Some(s) if s == "spread" => {
            let a = engine.get_dim();
            (None,Some(a))
        },
        _ => unreachable!()
    };
    while let Some(next) = engine.get_next_unexpandable() {
        match next.command {
            BaseCommand::Char{catcode:CategoryCode::Space,..} => {},
            BaseCommand::Relax => {},
            BaseCommand::Char{catcode:CategoryCode::BeginGroup,..} => {
                engine.state.stack_push(GroupType::Box(BoxMode::V));
                engine.mouth.insert_every(&engine.state,"everyvbox");
                return Ptr::new(move |e,children| {
                    Some(HVBox::V(VBox {
                        kind:"vcenter",
                        children, to, spread,
                        ..Default::default()
                    }))
                })
            }
            _ => throw!("Expected begin group, found {:?}",next.source.cause => cmd.cause)
        }
    }
    file_end_prim!("vcenter",cmd);
}

pub const VFIL: &str = "vfil";
pub fn vfil<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    debug_log!(trace => "\\vfil");
    ET::Stomach::push_node(engine,SkipNode::VFil.as_node());
}

pub const VFILL: &str = "vfill";
pub fn vfill<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    debug_log!(trace => "\\vfill");
    ET::Stomach::push_node(engine,SkipNode::VFill.as_node());
}

pub const VFILNEG: &str = "vfilneg";
pub fn vfilneg<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    debug_log!(trace => "\\vfilneg");
    ET::Stomach::push_node(engine,SkipNode::VFilneg.as_node());
}

pub const VRULE: &str = "vrule";
pub fn vrule<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    debug_log!(trace => "\\vrule");
    let mut width = None;
    let mut height = None;
    let mut depth = None;
    loop {
        match engine.get_keywords(vec!("width","height","depth")) {
            None => break,
            Some(s) => {
                let val = engine.get_dim();
                match s {
                    "width" => width = Some(val),
                    "height" => height = Some(val),
                    "depth" => depth = Some(val),
                    _ => unreachable!()
                }
            }
        }
    }
    ET::Stomach::push_node(engine,SimpleNode::Rule {
        width,height,depth,axis:HorV::Vertical
    }.as_node());
}


pub const VSKIP: &str = "vskip";
pub fn vskip<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    debug_log!(trace => "\\vskip");
    let skip = engine.get_skip();
    ET::Stomach::push_node(engine,SkipNode::Skip{skip,axis:HorV::Vertical}.as_node());
}

pub fn vsplit<ET:EngineType>(engine:&mut EngineRef<ET>,cmd:&CommandSource<ET>) -> HVBox<ET> {
    debug_log!(trace=>"\\vsplit");
    let i = engine.get_int();
    let i : usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => throw!("Invalid box number: {}",i => cmd.cause)
    };
    let v = engine.state.take_box_register(i);
    if !engine.get_keyword("to") {
        throw!("Expected 'to' after \\vsplit" => cmd.cause)
    }
    engine.skip_whitespace();
    let target = engine.get_dim();
    match v {
        HVBox::V(vb) => {
            let mut first = VBox {
                children:vec!(),
                to:Some(target),
                spread:None,
                assigned_depth:None,
                assigned_height:None,
                assigned_width:vb.assigned_width,
                kind:"vbox",
            };
            let mut second = VBox {
                children:vec!(),
                to:None,
                spread:None,
                assigned_depth:None,
                assigned_height:None,
                assigned_width:vb.assigned_width,
                kind:"vbox",
            };
            let (f,s) = ET::Stomach::split_vertical(engine,vb.children,target);
            first.children = f;
            second.children = s;
            engine.state.set_box_register(i,HVBox::V(second),false);
            HVBox::V(first)
        }
        _ => throw!("Incompatible box type: {}",i => cmd.cause)
    }
}


pub const VSS: &str = "vss";
pub fn vss<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    debug_log!(trace => "\\vss");
    ET::Stomach::push_node(engine,SkipNode::Vss.as_node());
}

pub const WD : &str = "wd";
pub fn wd_assign<ET:EngineType>(engine: &mut EngineRef<ET>, cmd:&CommandSource<ET>, global:bool) {
    debug_log!(trace=>"Assigning \\wd");
    let i = engine.get_int();
    let i:usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => throw!("Not a valid register: {}",i => cmd.cause)
    };
    engine.skip_eq_char();
    let v = engine.get_dim();
    debug_log!(debug=>"\\wd{} = {}",i,v);
    if let Some(b) = engine.state.get_box_register(i){b.set_width(v)}
}

pub fn wd_get<ET:EngineType>(engine: &mut EngineRef<ET>, cmd:&CommandSource<ET>) -> ET::Dim {
    debug_log!(trace=>"Getting \\wd");
    let i = engine.get_int();
    let i:usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => throw!("Not a valid register: {}",i => cmd.cause)
    };
    let v = engine.state.get_box_register(i).map(|b| b.width(&engine.fontstore)).unwrap_or(ET::Dim::from_sp(0));
    debug_log!(debug=>"\\wd{} == {}",i,v);
    v
}

pub const WRITE: &str = "write";
pub fn write<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>)
                            -> Whatsit<ET> {
    debug_log!(trace=>"\\write");
    let i = engine.get_int();
    let i = i.to_i64();
    let mut tks = vec!();

    engine.get_group(|_,t| tks.push(t));
    //catch_prim!(engine.get_group(&mut |_,t| Ok(tks.push(t))) => (WRITE,cmd));
    let ncmd = cmd.clone();

    let apply = Box::new(move |engine:&mut EngineRef<ET>| {
        tks.push(ET::Token::new_char_from_command(b'}'.into(),CategoryCode::EndGroup,&ncmd));
        tks.insert(0,ET::Token::new_char_from_command(b'{'.into(),CategoryCode::BeginGroup,&ncmd));
        engine.with_mouth(tks,|engine| {
            let mut string = engine.memory.get_string();
            engine.get_braced_string(&mut string);
            if i == 18 {
                (engine.outputs.write_18)(&string)
            }
            else if i == 17 {
                (engine.outputs.write_17)(&string)
            }
            else if i < 0 {
                (engine.outputs.write_neg1)(&string)
            }
            else {
                match engine.state.get_open_out_file(i as usize) {
                    None =>
                        (engine.outputs.write_other)(&string),
                    Some(f) => f.write(&string)
                }
            }
            engine.memory.return_string(string);
        })
    });
    Whatsit::new(apply)
}

pub fn xdef<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>, global:bool, protected:bool, long:bool, outer:bool) {
    edef::<ET>(engine,cmd,true,protected,long,outer)
}

pub fn year<ET:EngineType>(engine:&mut EngineRef<ET>,cmd:&CommandSource<ET>) -> ET::Int {
    ET::Int::from_i64(engine.start_time.year() as i64)
}

// --------------------------------------------------------------------------------------------------


pub fn initialize_tex_primitives<ET:EngineType>(engine:&mut EngineRef<ET>) {
    engine.state.set_command(ET::Char::from_str(CR_END,&mut engine.interner),Some(Command::new(BaseCommand::Unexpandable {
        name:CR_END,
        apply:|e,cmd| cr_end::<ET>(e,&cmd),
        forces_mode:None
    },None)),true);
    engine.state.set_command(ET::Char::from_str(ALIGN_END,&mut engine.interner),Some(Command::new(BaseCommand::Unexpandable {
        name:ALIGN_END,
        apply:|e,cmd| align_end::<ET>(e,&cmd),
        forces_mode:None
    },None)),true);

    register_skip_assign!(abovedisplayshortskip,engine);
    register_skip_assign!(abovedisplayskip,engine);
    register_int_assign!(adjdemerits,engine);
    register_assign!(advance,engine,(e,cmd,global) =>advance::<ET>(e,&cmd,global));
    register_unexpandable!(afterassignment,engine,None,(e,cmd) =>afterassignment::<ET>(e,&cmd));
    register_unexpandable!(aftergroup,engine,None,(e,cmd) =>aftergroup::<ET>(e,&cmd));
    register_int_assign!(badness,engine);
    register_skip_assign!(baselineskip,engine);
    register_unexpandable!(begingroup,engine,None,(e,cmd) =>begingroup::<ET>(&mut e.state));
    register_skip_assign!(belowdisplayskip,engine);
    register_skip_assign!(belowdisplayshortskip,engine);
    register_int_assign!(binoppenalty,engine);
    register_box!(box,engine,(e,cmd) =>box_::<ET>(e,&cmd));
    register_dim_assign!(boxmaxdepth,engine);
    register_int_assign!(brokenpenalty,engine);
    register_value_assign_int!(catcode,engine);
    register_unexpandable!(char,engine,Some(HorV::Horizontal),(e,cmd) =>char::<ET>(e,&cmd));
    register_assign!(chardef,engine,(e,cmd,global) =>chardef::<ET>(e,&cmd,global));
    register_unexpandable!(closein,engine,None,(e,cmd) =>closein::<ET>(e,&cmd));
    register_whatsit!(closeout,engine,(e,cmd) =>closeout::<ET>(e,&cmd));
    register_int_assign!(clubpenalty,engine);
    register_box!(copy,engine,(e,cmd) =>copy::<ET>(e,&cmd));
    register_value_assign_int!(count,engine);
    register_assign!(countdef,engine,(e,cmd,global) =>countdef::<ET>(e,&cmd,global));
    register_unexpandable!(cr,engine,None,(e,cmd) => cr::<ET>(e,&cmd));
    register_unexpandable!(crcr,engine,None,(e,cmd) => crcr::<ET>(e,&cmd));
    register_expandable_notk!(csname,engine,(e,cmd) =>csname::<ET>(e,&cmd));
    register_int!(day,engine,(e,cmd) => day::<ET>(e,&cmd));
    register_assign!(def,engine,(e,cmd,global) =>def::<ET>(e,&cmd,global,false,false,false));
    register_unexpandable!(delimiter,engine,None,(e,cmd) =>delimiter::<ET>(e,&cmd));
    register_int_assign!(defaulthyphenchar,engine);
    register_int_assign!(defaultskewchar,engine);
    register_int_assign!(delimiterfactor,engine);
    register_dim_assign!(delimitershortfall,engine);
    register_value_assign_int!(delcode,engine);
    register_value_assign_dim!(dimen,engine);
    register_assign!(dimendef,engine,(e,cmd,global) =>dimendef::<ET>(e,&cmd,global));
    register_unexpandable!(discretionary,engine,Some(HorV::Horizontal),(e,cmd) =>discretionary::<ET>(e,&cmd));
    register_dim_assign!(displayindent,engine);
    register_unexpandable!(displaylimits,engine,None,(e,cmd) =>displaylimits::<ET>(e,&cmd));
    register_int_assign!(displaywidowpenalty,engine);
    register_dim_assign!(displaywidth,engine);
    register_assign!(divide,engine,(e,cmd,global) =>divide::<ET>(e,&cmd,global));
    register_int_assign!(doublehyphendemerits,engine);
    register_value_assign_dim!(dp,engine);
    register_unexpandable!(dump,engine,None,(_,cmd) =>dump::<ET>());
    register_assign!(edef,engine,(e,cmd,global) =>edef::<ET>(e,&cmd,global,false,false,false));
    register_expandable_notk!(else,engine,(e,cmd) =>else_::<ET>(e,&cmd));
    register_dim_assign!(emergencystretch,engine);
    register_unexpandable!(end,engine,None,(_,cmd) =>end::<ET>());
    register_unexpandable!(endcsname,engine,None,(_,cmd) =>endcsname::<ET>(&cmd));
    register_expandable_notk!(endinput,engine,(e,cmd) => endinput::<ET>(e,&cmd));
    register_unexpandable!(endgroup,engine,None,(e,cmd) =>endgroup::<ET>(e,&cmd));
    register_value_assign_int!(endlinechar,engine);
    register_tok_assign!(errhelp,engine);

    let em = Some(Command::new(BaseCommand::Unexpandable {
        name:ERRMESSAGE,
        apply:|e,cmd| errmessage::<ET>(e,&cmd),
        forces_mode:None
    },None));
    engine.state.set_command(ET::Char::from_str(ERRMESSAGE,&mut engine.interner),em.clone(),true);
    engine.state.set_command(ET::Char::from_str("LaTeX3 error:",&mut engine.interner),em,true);

    register_int_assign!(errorcontextlines,engine);
    register_unexpandable!(errorstopmode,engine,None,(_,cmd) =>errorstopmode::<ET>());
    register_value_assign_int!(escapechar,engine);
    register_int_assign!(exhyphenpenalty,engine);
    register_expandable_notk!(expandafter,engine,(e,cmd) => expandafter::<ET>(e,&cmd));
    register_tok_assign!(everypar,engine);
    register_tok_assign!(everymath,engine);
    register_tok_assign!(everydisplay,engine);
    register_tok_assign!(everyhbox,engine);
    register_tok_assign!(everyvbox,engine);
    register_tok_assign!(everyjob,engine);
    register_tok_assign!(everycr,engine);
    register_int_assign!(fam,engine);
    register_expandable_notk!(fi,engine,(e,cmd) =>fi::<ET>(e,&cmd));
    register_int_assign!(finalhyphendemerits,engine);
    register_int_assign!(floatingpenalty,engine);
    register_value_assign_font!(font,engine);
    register_value_assign_dim!(fontdimen,engine);
    register_assign!(futurelet,engine,(e,cmd,global) =>futurelet::<ET>(e,&cmd,global));
    register_assign!(gdef,engine,(e,cmd,global) =>gdef::<ET>(e,&cmd,global,false,false,false));
    register_assign!(global,engine,(e,cmd,g) =>global::<ET>(e,&cmd,g,false,false,false));
    register_int_assign!(globaldefs,engine);
    register_unexpandable!(halign,engine,Some(HorV::Vertical),(e,cmd) =>halign::<ET>(e,&cmd));
    register_int_assign!(hangafter,engine);
    register_dim_assign!(hangindent,engine);
    register_int_assign!(hbadness,engine);
    register_open_box!(hbox,engine,BoxMode::H,(e,cmd) =>hbox::<ET>(e,&cmd));
    register_unexpandable!(hfil,engine,Some(HorV::Horizontal),(e,cmd) =>hfil::<ET>(e,&cmd));
    register_unexpandable!(hfill,engine,Some(HorV::Horizontal),(e,cmd) =>hfill::<ET>(e,&cmd));
    register_unexpandable!(hfilneg,engine,Some(HorV::Horizontal),(e,cmd) =>hfilneg::<ET>(e,&cmd));
    register_unexpandable!(hss,engine,Some(HorV::Horizontal),(e,cmd) =>hss::<ET>(e,&cmd));
    register_dim_assign!(hfuzz,engine);
    register_dim_assign!(hoffset,engine);
    register_int_assign!(holdinginserts,engine);
    register_unexpandable!(hrule,engine,Some(HorV::Vertical),(e,cmd) =>hrule::<ET>(e,&cmd));
    register_dim_assign!(hsize,engine);
    register_unexpandable!(hskip,engine,Some(HorV::Horizontal),(e,cmd) =>hskip::<ET>(e,&cmd));
    register_value_assign_dim!(ht,engine);
    register_unexpandable!(hyphenation,engine,None,(e,cmd) =>hyphenation::<ET>(e,&cmd));
    register_value_assign_int!(hyphenchar,engine);
    register_int_assign!(hyphenpenalty,engine);
    register_conditional!(if,engine,(e,cmd) =>if_::<ET>(e,&cmd));
    register_conditional!(ifcase,engine,(_,cmd) =>ifcase::<ET>());
    register_conditional!(ifcat,engine,(e,cmd) =>ifcat::<ET>(e,&cmd));
    register_conditional!(ifdim,engine,(e,cmd) =>ifdim::<ET>(e,&cmd));
    register_conditional!(ifeof,engine,(e,cmd) =>ifeof::<ET>(e,&cmd));
    register_conditional!(iffalse,engine,(_,_) => false);
    register_conditional!(ifhbox,engine,(e,cmd) => ifhbox::<ET>(e,&cmd));
    register_conditional!(ifhmode,engine,(e,cmd) =>ifhmode::<ET>(e,&cmd));
    register_conditional!(ifinner,engine,(e,cmd) =>ifinner::<ET>(e,&cmd));
    register_conditional!(ifmmode,engine,(e,cmd) =>ifmmode::<ET>(e,&cmd));
    register_conditional!(ifnum,engine,(e,cmd) =>ifnum::<ET>(e,&cmd));
    register_conditional!(ifodd,engine,(e,cmd) =>ifodd::<ET>(e,&cmd));
    register_conditional!(iftrue,engine,(_,_) => true);
    register_conditional!(ifvbox,engine,(e,cmd) =>ifvbox::<ET>(e,&cmd));
    register_conditional!(ifvmode,engine,(e,cmd) =>ifvmode::<ET>(e,&cmd));
    register_conditional!(ifvoid,engine,(e,cmd) =>ifvoid::<ET>(e,&cmd));
    register_conditional!(ifx,engine,(e,cmd) =>ifx::<ET>(e,&cmd));
    register_unexpandable!(ignorespaces,engine,None,(e,cmd) => ignorespaces::<ET>(e,&cmd));
    register_unexpandable!(immediate,engine,None,(e,cmd) =>immediate::<ET>(e,&cmd));
    register_unexpandable!(indent,engine,Some(HorV::Horizontal),(e,cmd) =>indent::<ET>(e,&cmd));
    register_expandable_notk!(input,engine,(e,cmd) =>input::<ET>(e,&cmd));
    register_int!(inputlineno,engine,(e,cmd) => inputlineno::<ET>(e,&cmd));
    register_unexpandable!(insert,engine,None,(e,cmd) =>insert::<ET>(e,&cmd));
    register_int_assign!(interlinepenalty,engine);
    register_expandable!(jobname,engine,(e,c,f) =>jobname::<ET>(e,&c,f));
    register_unexpandable!(kern,engine,None,(e,cmd) =>kern::<ET>(e,&cmd));
    register_int_assign!(language,engine);
    register_box!(lastbox,engine,(e,cmd) =>lastbox::<ET>(e,&cmd));
    register_dim!(lastkern,engine,(e,cmd) => lastkern::<ET>(e,&cmd));
    register_skip!(lastskip,engine,(e,cmd) => lastskip::<ET>(e,&cmd));
    register_int!(lastpenalty,engine,(e,cmd) => lastpenalty::<ET>(e,&cmd));
    register_value_assign_int!(lccode,engine);
    register_unexpandable!(leaders,engine,None,(e,cmd) =>leaders::<ET>(e,&cmd));
    register_unexpandable!(cleaders,engine,None,(e,cmd) =>cleaders::<ET>(e,&cmd));
    register_unexpandable!(xleaders,engine,None,(e,cmd) =>xleaders::<ET>(e,&cmd));
    register_int_assign!(lefthyphenmin,engine);
    register_skip_assign!(leftskip,engine);
    register_assign!(let,engine,(e,cmd,global) =>let_::<ET>(e,&cmd,global));
    register_int_assign!(linepenalty,engine);
    register_skip_assign!(lineskip,engine);
    register_dim_assign!(lineskiplimit,engine);
    register_assign!(long,engine,(e,cmd,g) =>long::<ET>(e,&cmd,g,false,false,false));
    register_int_assign!(looseness,engine);
    register_unexpandable!(lower,engine,Some(HorV::Horizontal),(e,cmd) =>lower::<ET>(e,&cmd));
    register_unexpandable!(lowercase,engine,None,(e,cmd) =>lowercase::<ET>(e,&cmd));
    register_int_assign!(mag,engine);
    register_unexpandable!(mark,engine,None,(e,cmd) =>mark::<ET>(e,&cmd));
    register_expandable!(topmark,engine,(e,c,f) =>topmark::<ET>(e,&c,f));
    register_expandable!(firstmark,engine,(e,c,f) =>firstmark::<ET>(e,&c,f));
    register_expandable!(botmark,engine,(e,c,f) =>botmark::<ET>(e,&c,f));
    register_expandable!(splitfirstmark,engine,(e,c,f) =>splitfirstmark::<ET>(e,&c,f));
    register_expandable!(splitbotmark,engine,(e,c,f) =>splitbotmark::<ET>(e,&c,f));
    register_int_assign!(maxdeadcycles,engine);
    register_dim_assign!(maxdepth,engine);
    register_unexpandable!(mathchar,engine,None,(e,cmd) =>mathchar::<ET>(e,&cmd));
    register_unexpandable!(mathord,engine,None,(e,cmd) =>mathord::<ET>(e,&cmd));
    register_unexpandable!(mathop,engine,None,(e,cmd) =>mathop::<ET>(e,&cmd));
    register_unexpandable!(mathbin,engine,None,(e,cmd) =>mathbin::<ET>(e,&cmd));
    register_unexpandable!(mathrel,engine,None,(e,cmd) =>mathrel::<ET>(e,&cmd));
    register_unexpandable!(mathopen,engine,None,(e,cmd) =>mathopen::<ET>(e,&cmd));
    register_unexpandable!(mathclose,engine,None,(e,cmd) =>mathclose::<ET>(e,&cmd));
    register_unexpandable!(mathpunct,engine,None,(e,cmd) =>mathpunct::<ET>(e,&cmd));
    register_unexpandable!(mathinner,engine,None,(e,cmd) =>mathinner::<ET>(e,&cmd));
    register_unexpandable!(mathchoice,engine,None,(e,cmd) =>mathchoice::<ET>(e,&cmd));
    register_assign!(mathchardef,engine,(e,cmd,global) =>mathchardef::<ET>(e,&cmd,global));
    register_value_assign_int!(mathcode,engine);
    register_dim_assign!(mathsurround,engine);
    register_expandable!(meaning,engine,(e,cmd,f) => meaning::<ET>(e,&cmd,f));
    register_unexpandable!(message,engine,None,(e,cmd) =>message::<ET>(e,&cmd));
    register_unexpandable!(mkern,engine,None,(e,cmd) =>mkern::<ET>(e,&cmd));
    register_int!(month,engine,(e,cmd) => month::<ET>(e,&cmd));
    register_unexpandable!(moveright,engine,Some(HorV::Vertical),(e,cmd) =>moveright::<ET>(e,&cmd));
    register_unexpandable!(moveleft,engine,Some(HorV::Vertical),(e,cmd) =>moveleft::<ET>(e,&cmd));
    register_unexpandable!(mskip,engine,None,(e,cmd) =>mskip::<ET>(e,&cmd));
    register_assign!(multiply,engine,(e,cmd,global) =>multiply::<ET>(e,&cmd,global));
    register_value_assign_muskip!(muskip,engine);
    register_assign!(muskipdef,engine,(e,cmd,global) =>muskipdef::<ET>(e,&cmd,global));
    register_value_assign_int!(newlinechar,engine);
    register_unexpandable!(noalign,engine,None,(e,cmd) =>noalign::<ET>(e,&cmd));
    register_expandable_notk!(noexpand,engine,(e,cmd) => noexpand::<ET>(e,&cmd));
    register_unexpandable!(noindent,engine,Some(HorV::Horizontal),(e,cmd) =>noindent::<ET>(e,&cmd));
    register_dim_assign!(nulldelimiterspace,engine);
    engine.state.set_command(ET::Char::from_str("nullfont",&mut engine.interner), Some(Command::new(
        BaseCommand::Font(ET::FontRef::default())
        ,None)), true);
    register_expandable!(number,engine,(e,cmd,f) => number::<ET>(e,&cmd,f));
    register_unexpandable!(omit,engine,None,(e,cmd) =>omit::<ET>(e,&cmd));
    register_unexpandable!(openin,engine,None,(e,cmd) =>openin::<ET>(e,&cmd));
    register_whatsit!(openout,engine,(e,cmd) =>openout::<ET>(e,&cmd));
    register_expandable_notk!(or,engine,(e,cmd) => or::<ET>(e,&cmd));
    register_assign!(outer,engine,(e,cmd,g) =>outer::<ET>(e,&cmd,g,false,false,false));
    register_tok_assign!(output,engine);
    register_int_assign!(outputpenalty,engine);
    register_dim_assign!(overfullrule,engine);
    register_value_assign_dim!(pagegoal,engine);
    register_value_assign_dim!(pagetotal,engine);
    register_value_assign_dim!(pagestretch,engine);
    register_value_assign_dim!(pagefilstretch,engine);
    register_value_assign_dim!(pagefillstretch,engine);
    register_value_assign_dim!(pagefilllstretch,engine);
    register_value_assign_dim!(pageshrink,engine);
    register_value_assign_dim!(pagedepth,engine);

    engine.state.set_command(ET::Char::from_str("par",&mut engine.interner),Some(Command::new(BaseCommand::Unexpandable {
        name:"par",
        apply:|e,cmd| par::<ET>(e,&cmd),
        forces_mode:Some(HorV::Vertical)
    },None)),true);
    register_skip_assign!(parfillskip,engine);
    register_dim_assign!(parindent,engine);
    register_value_assign_int!(parshape,engine);
    register_skip_assign!(parskip,engine);
    register_unexpandable!(patterns,engine,None,(e,cmd) =>patterns::<ET>(e,&cmd));
    register_int_assign!(pausing,engine);
    register_unexpandable!(penalty,engine,None,(e,cmd) =>penalty::<ET>(e,&cmd));
    register_int_assign!(postdisplaypenalty,engine);
    register_int_assign!(predisplaypenalty,engine);
    register_dim_assign!(predisplaysize,engine);
    register_value_assign_dim!(prevdepth,engine);
    register_int_assign!(relpenalty,engine);
    register_int_assign!(righthyphenmin,engine);
    register_int_assign!(pretolerance,engine);
    register_unexpandable!(raise,engine,Some(HorV::Horizontal),(e,cmd) =>raise::<ET>(e,&cmd));
    register_assign!(read,engine,(e,cmd,global) =>read::<ET>(e,&cmd,global));
    engine.state.set_command(engine.interner.relax, Some(Command::new(BaseCommand::Relax,None)), true);
    register_int_assign!(relpenalty,engine);
    register_skip_assign!(rightskip,engine);
    register_expandable!(romannumeral,engine,(e,cmd,f) => romannumeral::<ET>(e,&cmd,f));
    register_value_assign_font!(scriptfont,engine);
    register_value_assign_font!(scriptscriptfont,engine);
    register_dim_assign!(scriptspace,engine);
    register_assign!(setbox,engine,(e,cmd,global) =>setbox::<ET>(e,&cmd,global));
    register_value_assign_int!(sfcode,engine);
    register_unexpandable!(shipout,engine,None,(e,cmd) =>shipout::<ET>(e,&cmd));
    register_int_assign!(showboxbreadth,engine);
    register_int_assign!(showboxdepth,engine);
    register_value_assign_int!(skewchar,engine);
    register_value_assign_skip!(skip,engine);
    register_assign!(skipdef,engine,(e,cmd,global) =>skipdef::<ET>(e,&cmd,global));
    register_value_assign_int!(spacefactor,engine);
    register_skip_assign!(spaceskip,engine);
    register_unexpandable!(span,engine,None,(e,cmd) => span::<ET>(e,&cmd));
    register_dim_assign!(splitmaxdepth,engine);
    register_skip_assign!(splittopskip,engine);
    register_expandable!(string,engine,(e,cmd,f) => string::<ET>(e,&cmd,f));
    register_skip_assign!(tabskip,engine);
    register_value_assign_font!(textfont,engine);
    register_expandable!(the,engine,(e,cmd,f) => the::<ET>(e,&cmd,f));
    register_int!(time,engine,(e,cmd) => time::<ET>(e,&cmd));
    register_value_assign_toks!(toks,engine);
    register_assign!(toksdef,engine,(e,cmd,global) =>toksdef::<ET>(e,&cmd,global));
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
    register_unexpandable!(unhbox,engine,Some(HorV::Horizontal),(e,cmd) =>unhbox::<ET>(e,&cmd));
    register_unexpandable!(unhcopy,engine,Some(HorV::Horizontal),(e,cmd) =>unhcopy::<ET>(e,&cmd));
    register_unexpandable!(unvbox,engine,Some(HorV::Vertical),(e,cmd) =>unvbox::<ET>(e,&cmd));
    register_unexpandable!(unvcopy,engine,Some(HorV::Vertical),(e,cmd) =>unvcopy::<ET>(e,&cmd));
    register_unexpandable!(unskip,engine,None,(e,cmd) =>unskip::<ET>(e,&cmd));
    register_unexpandable!(unkern,engine,None,(e,cmd) =>unkern::<ET>(e,&cmd));
    register_unexpandable!(unpenalty,engine,None,(e,cmd) =>unpenalty::<ET>(e,&cmd));
    register_unexpandable!(uppercase,engine,None,(e,cmd) =>uppercase::<ET>(e,&cmd));
    register_int_assign!(vbadness,engine);
    register_open_box!(vadjust,engine,BoxMode::V,(e,cmd) =>vadjust::<ET>(e,&cmd));
    register_unexpandable!(valign,engine,Some(HorV::Vertical),(e,cmd) =>valign::<ET>(e,&cmd));
    register_open_box!(vbox,engine,BoxMode::V,(e,cmd) =>vbox::<ET>(e,&cmd));
    register_open_box!(vcenter,engine,BoxMode::V,(e,cmd) =>vcenter::<ET>(e,&cmd));
    register_unexpandable!(vfil,engine,Some(HorV::Vertical),(e,cmd) =>vfil::<ET>(e,&cmd));
    register_unexpandable!(vfill,engine,Some(HorV::Vertical),(e,cmd) =>vfill::<ET>(e,&cmd));
    register_unexpandable!(vfilneg,engine,Some(HorV::Vertical),(e,cmd) =>vfilneg::<ET>(e,&cmd));
    register_unexpandable!(vskip,engine,Some(HorV::Vertical),(e,cmd) =>vskip::<ET>(e,&cmd));
    register_box!(vsplit,engine,(e,cmd) =>vsplit::<ET>(e,&cmd));
    register_unexpandable!(vss,engine,Some(HorV::Vertical),(e,cmd) =>vss::<ET>(e,&cmd));
    register_dim_assign!(vfuzz,engine);
    register_dim_assign!(voffset,engine);
    register_unexpandable!(vrule,engine,Some(HorV::Horizontal),(e,cmd) =>vrule::<ET>(e,&cmd));
    register_dim_assign!(vsize,engine);
    register_value_assign_dim!(wd,engine);
    register_int_assign!(widowpenalty,engine);
    register_whatsit!(write,engine,(e,cmd) =>write::<ET>(e,&cmd));
    register_assign!(xdef,engine,(e,cmd,global) =>xdef::<ET>(e,&cmd,global,false,false,false));
    register_skip_assign!(xspaceskip,engine);
    register_int!(year,engine,(e,cmd) => year::<ET>(e,&cmd));

    register_muskip_assign!(thinmuskip,engine);
    register_muskip_assign!(medmuskip,engine);
    register_muskip_assign!(thickmuskip,engine);

    engine.state.set_command(ET::Char::from_str(" ",&mut engine.interner),Some(
        Command::new(BaseCommand::Unexpandable {
            name:" ",
            apply:|e,cmd| SPACE::<ET>(e,&cmd),
            forces_mode:Some(HorV::Horizontal)
        },None)),true);
    

    // TODOS ---------------------------------------------------------------------


    cmstodo!(engine,mathaccent);
    cmstodo!(engine,radical);
    cmstodo!(engine,displaystyle);
    cmstodo!(engine,textstyle);
    cmstodo!(engine,scriptstyle);
    cmstodo!(engine,scriptscriptstyle);

    cmtodo!(engine,prevgraf);
    cmtodo!(engine,deadcycles);
    cmtodo!(engine,insertpenalties);
    cmtodo!(engine,scrollmode);
    cmtodo!(engine,nonstopmode);
    cmtodo!(engine,batchmode);
    cmtodo!(engine,vtop);
    cmtodo!(engine,show);
    cmtodo!(engine,showbox);
    cmtodo!(engine,showlists);
    cmtodo!(engine,showthe);
    cmtodo!(engine,special);
    cmtodo!(engine,noboundary);
    cmtodo!(engine,accent);
    cmtodo!(engine,setlanguage);
    cmtodo!(engine,nonscript);
    cmtodo!(engine,underline);
    cmtodo!(engine,overline);
    cmtodo!(engine,limits);
    cmtodo!(engine,nolimits);
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
    cmtodo!(engine,fontname);
    cmtodo!(engine,italiccorr);
    cmtodo!(engine,medskip);
    cmtodo!(engine,smallskip);
}

