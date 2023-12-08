use chrono::{Datelike, Timelike};
use crate::{cmstodo, cmstodos, cmtodo, cmtodos, debug_log, expand_loop};
use crate::commands::{Assignment, Command, DimCommand, Expandable, FontCommand, IntCommand, Macro, MacroSignature, MuSkipCommand, NodeCommandScope, SimpleExpandable, SkipCommand, Unexpandable, Whatsit};
use crate::engine::{EngineAux, EngineReferences, EngineTypes, TeXEngine};
use crate::engine::filesystem::{File, FileSystem};
use crate::engine::gullet::{ActiveConditional, AlignData, Gullet, ResolvedToken};
use crate::engine::gullet::methods::ACOrCS;
use crate::engine::mouth::Mouth;
use crate::engine::mouth::pretokenized::{ExpansionContainer, TLVecMeaning, Tokenizer, TokenList};
use super::primitives::*;
use crate::engine::state::State;
use crate::engine::utils::memory::{MemoryManager, PrimitiveIdentifier, PRIMITIVES, PrintableIdentifier};
use crate::tex::catcodes::{CategoryCode, CategoryCodeScheme, CommandCode};
use crate::tex::numerics::{Numeric, NumSet};
use crate::tex::input_text::{Character, CharacterMap};
use crate::engine::utils::outputs::Outputs;
use crate::tex::token::{StandardToken, Token};
use crate::engine::stomach::{SplitResult, Stomach, StomachData};
use crate::tex::types::{BoxType, GroupType, TeXMode};
use std::fmt::{Display, Write};
use crate::engine::fontsystem::FontSystem;
use crate::utils::errors::ErrorHandler;
use crate::utils::{HMap, Ptr};
use crate::tex::control_sequences::{ControlSequenceNameHandler, ResolvedCSName};
use crate::engine::fontsystem::Font;
use crate::tex::nodes::{BoxInfo, BoxTarget, KernNode, NodeList, NodeListType, PreShipoutNodeTrait, SimpleNode, SkipNode, TeXBox, PreShipoutNode, ToOrSpread, ShipoutNode};use crate::tex::nodes::NodeTrait;

type Int<E> = <<E as EngineTypes>::Num as NumSet>::Int;
type Dim<E> = <<E as EngineTypes>::Num as NumSet>::Dim;
type Fnt<E> = <<E as EngineTypes>::FontSystem as FontSystem>::Font;

pub fn read_register<ET:EngineTypes>(engine: &mut EngineReferences<ET>) -> u16 {
    let idx = engine.read_int(false).into();
    if idx < 0 || idx > u16::MAX.into() {
        todo!("register out of range")
    }
    idx as u16
}

pub fn afterassignment<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) {
    let next = match engine.get_next() {
        Some(t) => t,
        None => todo!("file end")
    };
    *engine.stomach.afterassignment() = Some(next);
}

pub fn aftergroup<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) {
    let next = match engine.get_next() {
        Some(t) => t,
        None => todo!("file end")
    };
    engine.state.aftergroup(next)
}

pub fn begingroup<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) {
    engine.state.push(engine.aux,GroupType::ControlSequence,engine.mouth.line_number())
}
pub fn endgroup<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) {
    match engine.state.get_group_type() {
        Some(GroupType::ControlSequence) => (),
        _ => todo!("throw error")
    }
    engine.state.pop(engine.aux,engine.mouth)
}

#[inline(always)]
pub fn endinput<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) {
    engine.mouth.endinput()
}

#[inline(always)]
pub fn errorstopmode<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) {}

pub fn expandafter<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) {
    let first = match engine.get_next() {
        None => todo!("throw error"),
        Some(t) => t
    };
    let second = match engine.get_next() {
        None => todo!("throw error"),
        Some(t) => t
    };
    match ET::Gullet::resolve(engine.state,second) {
        ResolvedToken::Cmd{cmd: Some(cmd),token} => match cmd {
            Command::Macro(m) => ET::Gullet::do_macro(engine,m.clone(),token),
            Command::Conditional(cond) => ET::Gullet::do_conditional(engine,cond.name,token,cond.expand,false),
            Command::Expandable(e) => ET::Gullet::do_expandable(engine,e.name,token,e.expand),
            Command::SimpleExpandable(e) => ET::Gullet::do_simple_expandable(engine,e.name,token,e.expand),
            _ => engine.requeue(token)
        }
        ResolvedToken::Cmd{token,..} | ResolvedToken::Tk {token,..} =>
            engine.requeue(token)
    }
    engine.requeue(first)
}

pub fn catcode_get<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) -> Int<ET> {
    let char = engine.read_charcode(false);
    let u : u8 = (*engine.state.get_catcode_scheme().get(char)).into();
    Int::<ET>::from(u as i32)
}
pub fn catcode_set<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token,globally:bool) {
    let char = engine.read_charcode(false);
    let val: i64 = engine.read_int(true).into();
    if val < 0 || val > 15 {
        todo!("catcode out of range")
    }
    let cc: CategoryCode = (val as u8).try_into().unwrap();
    engine.state.set_catcode(engine.aux,char,cc,globally)
}

pub fn sfcode_get<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) -> Int<ET> {
    let char = engine.read_charcode(false);
    let u : u16 = engine.state.get_sfcode(char);
    Int::<ET>::from(u as i32)
}
pub fn sfcode_set<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token,globally:bool) {
    let char = engine.read_charcode(false);
    let val: i64 = engine.read_int(true).into();
    if val < 0 || val > 32767 {
        todo!("sfcode out of range")
    }
    let sf = val as u16;
    engine.state.set_sfcode(engine.aux,char,sf,globally)
}

#[inline(always)]
pub fn spacefactor_get<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) -> Int<ET> {
    Int::<ET>::from(engine.stomach.data_mut().spacefactor)
}
pub fn spacefactor_set<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token,globally:bool) {
    let val = match engine.read_int(true).try_into() {
        Ok(v) => v,
        _ => todo!("throw error")
    };
    engine.stomach.data_mut().spacefactor = val;
}

pub fn lccode_get<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) -> Int<ET> {
    let char = engine.read_charcode(false);
    let u = engine.state.get_lccode(char).into();
    match Int::<ET>::try_from(u as i64) {
        Ok(v) => v,
        _ => todo!("throw error")
    }
}
pub fn lccode_set<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token,globally:bool) {
    let char = engine.read_charcode(false);
    let val = engine.read_charcode(true);
    engine.state.set_lccode(engine.aux,char,val,globally)
}

pub fn uccode_get<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) -> Int<ET> {
    let char = engine.read_charcode(false);
    let u = engine.state.get_uccode(char).into();
    match Int::<ET>::try_from(u as i64) {
        Ok(v) => v,
        _ => todo!("throw error")
    }
}
pub fn uccode_set<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token,globally:bool) {
    let char = engine.read_charcode(false);
    let val = engine.read_charcode(true);
    engine.state.set_uccode(engine.aux,char,val,globally)
}

pub fn mathcode_get<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) -> Int<ET> {
    let char = engine.read_charcode(false);
    let u = engine.state.get_mathcode(char);
    match Int::<ET>::try_from(u as i64) {
        Ok(v) => v,
        _ => todo!("throw error")
    }
}
pub fn mathcode_set<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token,globally:bool) {
    let char = engine.read_charcode(false);
    let val = engine.read_int(true).into();
    if val < 0 || val > u32::MAX.into() {
        todo!("throw error")
    }
    engine.state.set_mathcode(engine.aux,char,val as u32,globally)
}

pub fn delcode_get<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) -> Int<ET> {
    let char = engine.read_charcode(false);
    engine.state.get_delcode(char)
}
pub fn delcode_set<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token,globally:bool) {
    let char = engine.read_charcode(false);
    let val = engine.read_int(true);
    engine.state.set_delcode(engine.aux,char,val,globally)
}

pub fn chardef<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token,globally:bool) {
    let name = engine.read_control_sequence();
    let char = engine.read_charcode(true);
    let cmd = Command::CharDef(char);
    engine.set_command(&name,Some(cmd),globally)
}

pub fn csname<ET:EngineTypes>(engine:&mut EngineReferences<ET>,tk:ET::Token) {
    let name = do_csname(engine);
    if engine.state.get_command(&name).is_none() {
        engine.state.set_command(engine.aux,name.clone(),Some(Command::Relax),false)
    }
    engine.mouth.requeue(ET::Token::from_cs(name))
}
pub fn do_csname<ET:EngineTypes>(engine:&mut EngineReferences<ET>) -> ET::CSName {
    *engine.gullet.csnames() += 1;
    let mut name = vec!();
    crate::expand_loop!(engine,
        ResolvedToken::Tk {char,..} => name.push(char),
        ResolvedToken::Cmd {cmd:Some(Command::Unexpandable(e)),..} if e.name == PRIMITIVES.endcsname => {
            *engine.gullet.csnames() -= 1;
            let id = engine.aux.memory.cs_interner_mut().from_chars(&name);
            //engine.aux.memory.return_string(name);
            return id
        }
        o => todo!("csname: {:?}",o)
    );
    todo!("file end")
}
pub fn endcsname<ET:EngineTypes>(engine:&mut EngineReferences<ET>,tk:ET::Token) {
    todo!("throw error")
}


pub fn count_get<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) -> Int<ET> {
    let idx = read_register(engine);
    engine.state.get_int_register(idx)
}
pub fn count_set<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token,globally:bool) {
    let idx = read_register(engine);
    let val = engine.read_int(true);
    engine.state.set_int_register(engine.aux,idx,val,globally)
}

pub fn dimen_get<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) -> Dim<ET> {
    let idx = read_register(engine);
    engine.state.get_dim_register(idx)
}
pub fn dimen_set<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token,globally:bool) {
    let idx = read_register(engine);
    let val = engine.read_dim(true);
    engine.state.set_dim_register(engine.aux,idx,val,globally)
}

pub fn skip_get<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) -> ET::Skip {
    let idx = read_register(engine);
    engine.state.get_skip_register(idx)
}
pub fn skip_set<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token,globally:bool) {
    let idx = read_register(engine);
    let val = engine.read_skip(true);
    engine.state.set_skip_register(engine.aux,idx,val,globally)
}

pub fn muskip_get<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) -> ET::MuSkip {
    let idx = read_register(engine);
    engine.state.get_muskip_register(idx)
}
pub fn muskip_set<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token,globally:bool) {
    let idx = read_register(engine);
    let val = engine.read_muskip(true);
    engine.state.set_muskip_register(engine.aux,idx,val,globally)
}

pub fn countdef<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token,globally:bool) {
    let name = engine.read_control_sequence();
    let i = engine.read_int(true).into();
    if i < 0 || i > u16::MAX as i64 {
        todo!("countdef out of range")
    }
    let cmd = Command::IntRegister(i as u16);
    engine.set_command(&name,Some(cmd),globally)
}

pub fn dimendef<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token,globally:bool) {
    let name = engine.read_control_sequence();
    let i = engine.read_int(true).into();
    if i < 0 || i > u16::MAX as i64 {
        todo!("dimendef out of range")
    }
    let cmd = Command::DimRegister(i as u16);
    engine.set_command(&name,Some(cmd),globally)
}

pub fn skipdef<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token,globally:bool) {
    let name = engine.read_control_sequence();
    let i = engine.read_int(true).into();
    if i < 0 || i > u16::MAX as i64 {
        todo!("skipdef out of range")
    }
    let cmd = Command::SkipRegister(i as u16);
    engine.set_command(&name,Some(cmd),globally)
}

pub fn muskipdef<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token,globally:bool) {
    let name = engine.read_control_sequence();
    let i = engine.read_int(true).into();
    if i < 0 || i > u16::MAX as i64 {
        todo!("muskipdef out of range")
    }
    let cmd = Command::MuSkipRegister(i as u16);
    engine.set_command(&name,Some(cmd),globally)
}

pub fn toksdef<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token,globally:bool) {
    let name = engine.read_control_sequence();
    let i = engine.read_int(true).into();
    if i < 0 || i > u16::MAX as i64 {
        todo!("toksdef out of range")
    }
    let cmd = Command::ToksRegister(i as u16);
    engine.set_command(&name,Some(cmd),globally)
}

pub fn def<ET:EngineTypes>(engine:&mut EngineReferences<ET>,tk:ET::Token,outer:bool,long:bool,protected:bool,globally:bool) {
    let cm = match engine.get_next() {
        Some(t) => match t.to_enum() {
            StandardToken::Character(c,CommandCode::Active) =>
                ACOrCS::Active(c),
            StandardToken::ControlSequence(cs) => ACOrCS::Name(cs),
            _ => todo!("throw error")
        }
        None => todo!("file end error")
    };
    let (sig,end) = parse_signature(engine,&cm);
    let mut exp = shared_vector::Vector::new();
    let mut inparam = false;
    engine.read_until_endgroup(|_,_,t| {
        if inparam {
            inparam = false;
            if t.is_param() {
                exp.push(t);
            } else {
                match t.char_value() {
                    Some(c) => match c.try_into() {
                        Ok(u) if u > 48 && u - 49 < sig.arity => exp.push(ET::Token::argument_marker(u-49)),
                        _ => todo!("error")
                    }
                    None => todo!("error")
                }
            }
        } else {
            if t.is_param() {
                inparam = true;
            } else {
                exp.push(t);
            }
        }
    });
    if inparam {todo!("error")}
    if let Some(e) = end {
        exp.push(e);
    }
    let cmd = Macro {
        long,outer,protected,
        expansion:exp.into(),
        signature:sig
    };
    engine.set_command(&cm,Some(Command::Macro(cmd)),globally)
}

pub fn edef<ET:EngineTypes>(engine:&mut EngineReferences<ET>,tk:ET::Token,outer:bool,long:bool,protected:bool,globally:bool) {
    let cm = match engine.get_next() {
        Some(t) => match t.to_enum() {
            StandardToken::Character(c,CommandCode::Active) =>
                ACOrCS::Active(c),
            StandardToken::ControlSequence(cs) => ACOrCS::Name(cs),
            _ => todo!("throw error")
        }
        None => todo!("file end error")
    };
    let (sig,end) = parse_signature(engine,&cm);
    let mut exp = shared_vector::Vector::new();
    let mut inparam = false;
    ET::Gullet::expand_until_endgroup(engine,false,true,|_,_,_,t| {
        if inparam {
            inparam = false;
            if t.is_param() {
                exp.push(t);
            }
            else {
                match t.char_value() {
                    Some(c) => match c.try_into() {
                        Ok(u) if u > 48 && u - 49 < sig.arity => exp.push(ET::Token::argument_marker(u-49)),
                        _ => todo!("error")
                    }
                    None => todo!("error")
                }
            }
        } else {
            if t.is_param() {
                inparam = true;
            } else {
                exp.push(t);
            }
        }
    });
    if inparam {todo!("error")}
    if let Some(e) = end {
        exp.push(e);
    }
    let cmd = Macro {
        long,outer,protected,
        expansion:exp.into(),
        signature:sig
    };
    engine.set_command(&cm,Some(Command::Macro(cmd)),globally)
}

#[inline(always)]
pub fn xdef<ET:EngineTypes>(engine:&mut EngineReferences<ET>,tk:ET::Token,outer:bool,long:bool,protected:bool,globally:bool) {
    edef(engine,tk,outer,long,protected,true)
}

#[inline(always)]
pub fn gdef<ET:EngineTypes>(engine:&mut EngineReferences<ET>,tk:ET::Token,outer:bool,long:bool,protected:bool,globally:bool) {
    def(engine,tk,outer,long,protected,true)
}

pub fn dp_get<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) -> Dim<ET> {
    let idx = read_register(engine);
    match engine.state.get_box_register(idx) {
        None => ET::Dim::default(),
        Some(b) => b.depth()
    }
}
pub fn dp_set<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token,globally:bool) {
    let idx = read_register(engine);
    let dim = engine.read_dim(true);
    if let Some(b) = engine.state.get_box_register_mut(idx) {
        b.info.assigned_depth = Some(dim)
    }
}

pub fn ht_get<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) -> Dim<ET> {
    let idx = read_register(engine);
    match engine.state.get_box_register(idx) {
        None => ET::Dim::default(),
        Some(b) => b.height()
    }
}
pub fn ht_set<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token,globally:bool) {
    let idx = read_register(engine);
    let dim = engine.read_dim(true);
    if let Some(b) = engine.state.get_box_register_mut(idx) {
        b.info.assigned_height = Some(dim)
    }
}

pub fn wd_get<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) -> Dim<ET> {
    let idx = read_register(engine);
    match engine.state.get_box_register(idx) {
        None => ET::Dim::default(),
        Some(b) => b.width()
    }
}
pub fn wd_set<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token,globally:bool) {
    let idx = read_register(engine);
    let dim = engine.read_dim(true);
    if let Some(b) = engine.state.get_box_register_mut(idx) {
        b.info.assigned_width = Some(dim)
    }
}

pub fn global<ET:EngineTypes>(engine:&mut EngineReferences<ET>,tk:ET::Token,outer:bool,long:bool,protected:bool,globally:bool) {
    let allow_others = !outer && !long && !protected;
    crate::expand_loop!(engine,
        ResolvedToken::Cmd {cmd:Some(Command::Assignment(a)),token} => match a.name {
            n if n == PRIMITIVES.outer => return self::outer(engine,token,outer,long,protected,true),
            n if n == PRIMITIVES.long => return self::long(engine,token,outer,long,protected,true),
            n if n == PRIMITIVES.protected => return super::etex::protected(engine,token,outer,long,protected,true),
            n if n == PRIMITIVES.global => return self::global(engine,token,outer,long,protected,true),
            n if n == PRIMITIVES.def => return self::def(engine,token,outer,long,protected,true),
            n if n == PRIMITIVES.edef => return self::edef(engine,token,outer,long,protected,true),
            n if n == PRIMITIVES.xdef => return self::xdef(engine,token,outer,long,protected,true),
            n if n == PRIMITIVES.gdef => return self::gdef(engine,token,outer,long,protected,true),
            n if allow_others => return ET::Stomach::do_assignment(engine,n,token,a.assign,true),
            _ => todo!("throw error")
        }
        ResolvedToken::Cmd {cmd:Some(Command::IntRegister(u)),..} if allow_others =>
            return ET::Stomach::assign_int_register(engine,*u,true),
        ResolvedToken::Cmd {cmd:Some(Command::PrimitiveInt(name)),..} if allow_others =>
            return ET::Stomach::assign_primitive_int(engine,*name,true),
        ResolvedToken::Cmd {cmd:Some(Command::Int(IntCommand{name,assign:Some(f),..})),token} if allow_others =>
            return ET::Stomach::do_assignment(engine,*name,token,*f,true),
        ResolvedToken::Cmd {cmd:Some(Command::DimRegister(u)),..} if allow_others =>
            return ET::Stomach::assign_dim_register(engine,*u,true),
        ResolvedToken::Cmd {cmd:Some(Command::PrimitiveDim(name)),..} if allow_others =>
            return ET::Stomach::assign_primitive_dim(engine,*name,true),
        ResolvedToken::Cmd {cmd:Some(Command::Dim(DimCommand{name,assign:Some(f),..})),token} if allow_others =>
            return ET::Stomach::do_assignment(engine,*name,token,*f,true),
        ResolvedToken::Cmd {cmd:Some(Command::SkipRegister(u)),..} if allow_others =>
            return ET::Stomach::assign_skip_register(engine,*u,true),
        ResolvedToken::Cmd {cmd:Some(Command::PrimitiveSkip(name)),..} if allow_others =>
            return ET::Stomach::assign_primitive_skip(engine,*name,true),
        ResolvedToken::Cmd {cmd:Some(Command::Skip(SkipCommand{name,assign:Some(f),..})),token} if allow_others =>
            return ET::Stomach::do_assignment(engine,*name,token,*f,true),
        ResolvedToken::Cmd {cmd:Some(Command::MuSkipRegister(u)),..} if allow_others =>
            return ET::Stomach::assign_muskip_register(engine,*u,true),
        ResolvedToken::Cmd {cmd:Some(Command::PrimitiveMuSkip(name)),..} if allow_others =>
            return ET::Stomach::assign_primitive_muskip(engine,*name,true),
        ResolvedToken::Cmd {cmd:Some(Command::MuSkip(MuSkipCommand{name,assign:Some(f),..})),token} if allow_others =>
            return ET::Stomach::do_assignment(engine,*name,token,*f,true),
        ResolvedToken::Cmd {cmd:Some(Command::ToksRegister(u)),..} if allow_others =>
            return ET::Stomach::assign_toks_register(engine,*u,true),
        ResolvedToken::Cmd {cmd:Some(Command::PrimitiveToks(name)),..} if allow_others =>
            return ET::Stomach::assign_primitive_toks(engine,*name,true),
        ResolvedToken::Cmd {cmd:Some(Command::FontCmd(FontCommand{name,assign:Some(f),..})),token} if allow_others =>
            return ET::Stomach::do_assignment(engine,*name,token,*f,true),
        o => todo!("\\global {:?}",o)
    )
}
pub fn outer<ET:EngineTypes>(engine:&mut EngineReferences<ET>,tk:ET::Token,outer:bool,long:bool,protected:bool,globally:bool) {
    crate::expand_loop!(engine,
        ResolvedToken::Cmd {cmd:Some(Command::Assignment(a)),token} => match a.name {
            n if n == PRIMITIVES.outer => return self::outer(engine,token,true,long,protected,globally),
            n if n == PRIMITIVES.long => return self::long(engine,token,true,long,protected,globally),
            n if n == PRIMITIVES.protected => return super::etex::protected(engine,token,true,long,protected,globally),
            n if n == PRIMITIVES.global => return self::global(engine,token,true,long,protected,globally),
            n if n == PRIMITIVES.def => return self::def(engine,token,true,long,protected,globally),
            n if n == PRIMITIVES.edef => return self::edef(engine,token,true,long,protected,globally),
            n if n == PRIMITIVES.xdef => return self::xdef(engine,token,true,long,protected,globally),
            n if n == PRIMITIVES.gdef => return self::gdef(engine,token,true,long,protected,globally),
            _ => todo!("throw error")
        }
        _ => todo!("throw error")
    )
}

pub fn long<ET:EngineTypes>(engine:&mut EngineReferences<ET>,tk:ET::Token,outer:bool,long:bool,protected:bool,globally:bool) {
    crate::expand_loop!(engine,
        ResolvedToken::Cmd {cmd:Some(Command::Assignment(a)),token} => match a.name {
            n if n == PRIMITIVES.outer => return self::outer(engine,token,outer,true,protected,globally),
            n if n == PRIMITIVES.long => return self::long(engine,token,outer,true,protected,globally),
            n if n == PRIMITIVES.protected => return super::etex::protected(engine,token,outer,true,protected,globally),
            n if n == PRIMITIVES.global => return self::global(engine,token,outer,true,protected,globally),
            n if n == PRIMITIVES.def => return self::def(engine,token,outer,true,protected,globally),
            n if n == PRIMITIVES.edef => return self::edef(engine,token,outer,true,protected,globally),
            n if n == PRIMITIVES.xdef => return self::xdef(engine,token,outer,true,protected,globally),
            n if n == PRIMITIVES.gdef => return self::gdef(engine,token,outer,true,protected,globally),
            _ => todo!("throw error")
        }
        _ => todo!("throw error")
    )
}

pub fn parse_signature<ET:EngineTypes>(engine:&mut EngineReferences<ET>,cm:&ACOrCS<ET::Token>)
    -> (MacroSignature<ET::Token>,Option<ET::Token>) {
    let mut arity = 0;
    let mut params = shared_vector::Vector::new();
    let mut inparam = false;
    let mut ends_with_brace = None;
    engine.mouth.iterate(engine.aux,engine.state.get_catcode_scheme(),engine.state.get_endline_char(),|_,t| {
        if t.is_begin_group() {
            if inparam {
                params.push(t.clone());
                ends_with_brace = Some(t);
            }
            return false
        }
        if inparam {
            inparam = false;
            if t.is_param() {
                params.push(t);
            }
            else {
                match t.char_value() {
                    Some(c) => match c.try_into() {
                        Ok(u) if u > 48 && u == 49 + arity => params.push(ET::Token::argument_marker(arity)),
                        _ => todo!("error")
                    }
                    None => todo!("error")
                }
                arity += 1
            }
        } else {
            if t.is_param() {
                inparam = true;
            } else {
                params.push(t);
            }
        }
        true
    });
    (MacroSignature{
        arity,params:params.into()
    },ends_with_brace)
}

fn modify_int_register<ET:EngineTypes,O:FnOnce(Int<ET>,&mut EngineReferences<ET>) -> Int<ET>>(engine: &mut EngineReferences<ET>,idx:u16,globally:bool,op:O) {
    engine.read_keyword(b"by");
    let old = engine.state.get_int_register(idx);
    let new = op(old,engine);
    engine.state.set_int_register(engine.aux,idx,new,globally);
}

fn modify_dim_register<ET:EngineTypes,O:FnOnce(Dim<ET>,&mut EngineReferences<ET>) -> Dim<ET>>(engine: &mut EngineReferences<ET>,idx:u16,globally:bool,op:O) {
    engine.read_keyword(b"by");
    let old = engine.state.get_dim_register(idx);
    let new = op(old,engine);
    engine.state.set_dim_register(engine.aux,idx,new,globally);
}

fn modify_skip_register<ET:EngineTypes,O:FnOnce(ET::Skip,&mut EngineReferences<ET>) -> ET::Skip>(engine: &mut EngineReferences<ET>,idx:u16,globally:bool,op:O) {
    engine.read_keyword(b"by");
    let old = engine.state.get_skip_register(idx);
    let new = op(old,engine);
    engine.state.set_skip_register(engine.aux,idx,new,globally);
}

macro_rules! modify_num {
    ($engine:ident,$globally:ident,$int:expr,$dim:expr,$skip:expr) => {
        crate::expand_loop!($engine,
            ResolvedToken::Cmd {cmd:Some(cm),token} => match cm {
                Command::Int(IntCommand{name,..}) if *name == PRIMITIVES.count => {
                    let idx = $engine.read_int(false);
                    let idx = match idx.try_into() {
                        Ok(i) if i >= 0 && i <= u16::MAX as i32 => i as u16,
                        _ => todo!("throw error")
                    };
                    return modify_int_register($engine,idx,$globally,$int)
                }
                Command::IntRegister(idx) => {
                    return modify_int_register($engine,*idx,$globally,$int)
                }
                Command::Dim(DimCommand{name,..}) if *name == PRIMITIVES.dimen => {
                    let idx = $engine.read_int(false);
                    let idx = match idx.try_into() {
                        Ok(i) if i >= 0 && i <= u16::MAX as i32 => i as u16,
                        _ => todo!("throw error")
                    };
                    return modify_dim_register($engine,idx,$globally,$dim)
                }
                Command::DimRegister(idx) => {
                    return modify_dim_register($engine,*idx,$globally,$dim)
                }
                Command::Skip(SkipCommand{name,..}) if *name == PRIMITIVES.skip => {
                    let idx = $engine.read_int(false);
                    let idx = match idx.try_into() {
                        Ok(i) if i >= 0 && i <= u16::MAX as i32 => i as u16,
                        _ => todo!("throw error")
                    };
                    return modify_skip_register($engine,idx,$globally,$skip)
                }
                Command::SkipRegister(idx) => {
                    return modify_skip_register($engine,*idx,$globally,$skip)
                }
                o => todo!("{:?} in \\advance",o)
            }
            _ => todo!("throw error")
        );
        todo!("file end")
    };
}

pub fn advance<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token,globally:bool) {
    modify_num!(engine,globally,
        |a,e| a + e.read_int(false),
        |a,e| a + e.read_dim(false),
        |a,e| a + e.read_skip(false)
    );
}
pub fn divide<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token,globally:bool) {
    modify_num!(engine,globally,|a,e| {
        let b = e.read_int(false);
        if b == Int::<ET>::default() {
            todo!("divide by zero")
        }
        a / b
    },|a,e| {
        let b = e.read_int(false);
        if b == Int::<ET>::default() {
            todo!("divide by zero")
        }
        a.scale(Int::<ET>::from(1),b)
    },|a,e| {
        let b = e.read_int(false);
        if b == Int::<ET>::default() {
            todo!("divide by zero")
        }
        a.scale(Int::<ET>::from(1),b)
    }
    );
}
pub fn multiply<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token,globally:bool) {
    modify_num!(engine,globally,
        |a,e| a * e.read_int(false),
        |a,e| {
            let b = e.read_int(false);
            a.scale(b,Int::<ET>::from(1))
        },
        |a,e| {
            let b = e.read_int(false);
            a.scale(b,Int::<ET>::from(1))
        }
    );
}

pub fn else_<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) {
    let conds = engine.gullet.get_conditionals();
    let name = match conds.pop() {
        Some(ActiveConditional::True(id)) => {
            conds.push(ActiveConditional::Else(id));
            id
        }
        Some(c@ActiveConditional::Case(_)) => {
            conds.push(c);PRIMITIVES.ifcase
        }
        Some(u@ActiveConditional::Unfinished(_)) => {
            conds.push(u);
            engine.mouth.requeue(tk);
            let relax = engine.aux.memory.cs_interner_mut().new("relax");
            engine.mouth.requeue(ET::Token::from_cs(relax));
            return
        }
        o => todo!("HERE: {:?}",o)
    };
    let trace = engine.state.get_primitive_int(PRIMITIVES.tracingifs) > Int::<ET>::default();
    let index = conds.len();
    if trace {
        engine.aux.outputs.write_neg1(
            format_args!("{{{}else: {} (level {}) entered on line {}}}",
                        <ET::Char as Character>::displayable_opt(engine.state.get_escape_char()),
                         PRIMITIVES.printable(name,engine.state.get_escape_char()),index,engine.mouth.line_number()));
    }
    crate::engine::gullet::methods::false_loop(engine,index,false,false);
    if trace {
        engine.aux.outputs.write_neg1(
            format_args!("{{{}fi: {} (level {}) entered on line {}}}",
                         <ET::Char as Character>::displayable_opt(engine.state.get_escape_char()),
                         PRIMITIVES.printable(name, engine.state.get_escape_char()),
                         index, engine.mouth.line_number()));
    }
}

pub fn or<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) {
    let conds = engine.gullet.get_conditionals();
    match conds.pop() {
        Some(ActiveConditional::Case(num)) => {
            conds.push(ActiveConditional::Else(PRIMITIVES.ifcase));
        }
        _ => todo!()
    };
    let trace = engine.state.get_primitive_int(PRIMITIVES.tracingifs) > Int::<ET>::default();
    let index = conds.len();
    if trace {
        engine.aux.outputs.write_neg1(
            format_args!("{{{}or: {}ifcase (level {}) entered on line {}}}",
                         <ET::Char as Character>::displayable_opt(engine.state.get_escape_char()),
                         <ET::Char as Character>::displayable_opt(engine.state.get_escape_char()),
                        index,engine.mouth.line_number()));
    }
    crate::engine::gullet::methods::false_loop(engine,index,false,true);
    if trace {
        engine.aux.outputs.write_neg1(
            format_args!("{{{}or: {}ifcase (level {}) entered on line {}}}",
                         <ET::Char as Character>::displayable_opt(engine.state.get_escape_char()),
                         <ET::Char as Character>::displayable_opt(engine.state.get_escape_char()),
                         index, engine.mouth.line_number()));
    }
}

pub fn endlinechar_get<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) -> Int<ET> {
    Int::<ET>::from(match engine.state.get_endline_char() {
        Some(c) => c.into() as i32,
        _ => -1
    })
}
pub fn endlinechar_set<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token,globally:bool) {
    let val: i64 = engine.read_int(true).into();
    let val = if val == -1 { None } else
    if val < -1 {
        todo!("endlinechar out of range")
    } else {
        match ET::Char::try_from(val as u64) {
            Ok(c) => Some(c),
            _ => todo!("endlinechar out of range")
        }
    };
    engine.state.set_endline_char(engine.aux,val,globally)
}

pub fn escapechar_get<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) -> Int<ET> {
    Int::<ET>::from(match engine.state.get_escape_char() {
        Some(c) => c.into() as i32,
        _ => -1
    })
}
pub fn escapechar_set<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token,globally:bool) {
    let val: i64 = engine.read_int(true).into();
    let val = if val == -1 { None } else
    if val < -1 {
        todo!("escapechar out of range")
    } else {
        match ET::Char::try_from(val as u64) {
            Ok(c) => Some(c),
            _ => todo!("escapechar out of range")
        }
    };
    engine.state.set_escape_char(engine.aux,val,globally)
}

#[inline(always)]
pub fn font_get<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) -> Fnt<ET> {
    engine.state.get_current_font().clone()
}

pub fn font_set<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token,global:bool) {
    let cs = match engine.read_control_sequence() {
        ACOrCS::Name(name) => name,
        _ => todo!("throw error")
    };
    let mut name = engine.aux.memory.get_string();
    engine.read_string(true,&mut name);
    let mut font = engine.fontsystem.new_font(&name,cs.clone(),engine.filesystem);
    engine.aux.memory.return_string(name);
    match engine.read_keywords(&[b"at",b"scaled"]) {
        Some(b"at") => {
            let size = engine.read_dim(false);
            font.set_at(size);
        }
        Some(b"scaled") => {
            todo!("read float and scale")
        }
        _ => ()
    }
    engine.state.set_command(engine.aux,cs,Some(Command::Font(font)),global)
}


pub fn fontdimen_get<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) -> Dim<ET> {
    let idx = match engine.read_int(false).try_into() {
        Ok(i) if i-1 >= 0 && i-1 <= u16::MAX.into() => (i-1) as u16,
        _ => todo!("throw error")
    };
    let font = engine.read_font();
    font.get_dim(idx)
}
pub fn fontdimen_set<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token,globally:bool) {
    let i = engine.read_int(false);
    let idx = match i.try_into() {
        Ok(i) if i-1 >= 0 && i-1 <= u16::MAX.into() => (i-1) as u16,
        _ => todo!("throw error: {}",i)
    };
    let mut font = engine.read_font();
    let dim = engine.read_dim(true);
    font.set_dim(idx,dim);
}

fn do_box_start<ET:EngineTypes>(engine:&mut EngineReferences<ET>,tp:BoxType,every:PrimitiveIdentifier) -> ToOrSpread<ET::Dim> {
    let scaled = match engine.read_keywords(&[b"to",b"spread"]) {
        Some(b"to") => {
            let to = engine.read_dim(false);
            ToOrSpread::To(to)
        }
        Some(b"spread") => {
            let spread = engine.read_dim(false);
            ToOrSpread::Spread(spread)
        }
        _ => ToOrSpread::None
    };
    let mut ate_relax = scaled == ToOrSpread::None;
    crate::expand_loop!(engine,
        ResolvedToken::Tk {code:CommandCode::Space,..} => (),
        ResolvedToken::Cmd {cmd:Some(Command::Relax),..} if !ate_relax => ate_relax = true,
        ResolvedToken::Tk {code:CommandCode::BeginGroup,..} |
        ResolvedToken::Cmd {cmd:Some(Command::Char{code:CommandCode::BeginGroup,..}),..} => {
            engine.state.push(engine.aux,GroupType::Box(tp),engine.mouth.line_number());
            engine.mouth.insert_every::<ET>(&engine.state,every);
            return scaled
        }
        o => todo!("throw error: {:?}",o)
    );
    todo!("file end")
}

pub fn box_<ET:EngineTypes>(engine:&mut EngineReferences<ET>,tk:ET::Token) -> Result<Option<TeXBox<ET,PreShipoutNode<ET>>>,BoxInfo<ET>> {
    let idx = read_register(engine);
    Ok(engine.state.take_box_register(idx))
}
pub fn copy<ET:EngineTypes>(engine:&mut EngineReferences<ET>,tk:ET::Token) -> Result<Option<TeXBox<ET,PreShipoutNode<ET>>>,BoxInfo<ET>> {
    let idx = read_register(engine);
    Ok(engine.state.get_box_register(idx).cloned())
}

pub fn unbox<ET:EngineTypes>(engine:&mut EngineReferences<ET>,tk:ET::Token,scope:NodeCommandScope,tp:BoxType,copy:bool) {
    if ET::Stomach::maybe_switch_mode(engine,scope,tk) {
        let idx = read_register(engine);
        let bx = if copy {engine.state.get_box_register(idx).cloned()} else {engine.state.take_box_register(idx)};
        match bx {
            None => (),
            Some(TeXBox {info:BoxInfo{tp:btp,..},children,..}) if btp == tp => {
                for c in children {
                    ET::Stomach::add_node(engine,c)
                }
            }
            _ => todo!("error")
        }
    }
}

pub fn unhbox<ET:EngineTypes>(engine:&mut EngineReferences<ET>,tk:ET::Token) {
    unbox(engine,tk,NodeCommandScope::SwitchesToHorizontal,BoxType::Horizontal,false)
}
pub fn unhcopy<ET:EngineTypes>(engine:&mut EngineReferences<ET>,tk:ET::Token) {
    unbox(engine,tk,NodeCommandScope::SwitchesToHorizontal,BoxType::Horizontal,true)
}
pub fn unvbox<ET:EngineTypes>(engine:&mut EngineReferences<ET>,tk:ET::Token) {
    unbox(engine,tk,NodeCommandScope::SwitchesToVertical,BoxType::Vertical,false)
}
pub fn unvcopy<ET:EngineTypes>(engine:&mut EngineReferences<ET>,tk:ET::Token) {
    unbox(engine,tk,NodeCommandScope::SwitchesToVertical,BoxType::Vertical,true)
}

pub fn hbox<ET:EngineTypes>(engine:&mut EngineReferences<ET>,tk:ET::Token) -> Result<Option<TeXBox<ET,PreShipoutNode<ET>>>,BoxInfo<ET>> {
    let scaled = do_box_start(engine,BoxType::Horizontal,PRIMITIVES.everyhbox);
    Err(BoxInfo {
        tp: BoxType::Horizontal,
        kind: "hbox",
        scaled,
        assigned_width: None,
        assigned_height: None,
        assigned_depth: None,
        moved_left:None,raised:None
    })
}

pub fn vbox<ET:EngineTypes>(engine:&mut EngineReferences<ET>,tk:ET::Token) -> Result<Option<TeXBox<ET,PreShipoutNode<ET>>>,BoxInfo<ET>> {
    let scaled = do_box_start(engine,BoxType::Vertical,PRIMITIVES.everyvbox);
    Err(BoxInfo {
        tp: BoxType::Vertical,
        kind: "vbox",
        scaled,
        assigned_width: None,
        assigned_height: None,
        assigned_depth: None,
        moved_left:None,raised:None
    })
}

pub fn halign<ET:EngineTypes>(engine:&mut EngineReferences<ET>,token:ET::Token) {
    engine.gullet.push_align(AlignData { ingroups: 0 })
}

pub fn hyphenchar_get<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) -> Int<ET> {
    let font = engine.read_font();
    font.get_hyphenchar()
}
pub fn hyphenchar_set<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token,globally:bool) {
    let mut font = engine.read_font();
    let val = engine.read_int(true);
    font.set_hyphenchar(val);
}

pub fn skewchar_get<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) -> Int<ET> {
    let font = engine.read_font();
    font.get_skewchar()
}
pub fn skewchar_set<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token,globally:bool) {
    let mut font = engine.read_font();
    let val = engine.read_int(true);
    font.set_skewchar(val);
}

pub fn if_<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) -> bool {
    let first = get_if_token(engine);
    let second = get_if_token(engine);
    first.0 == second.0
}

pub fn ifcase<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) -> bool {
    let index = engine.gullet.get_conditionals().len() - 1;
    let num = engine.read_int(false);
    *engine.gullet.get_conditionals().get_mut(index).unwrap() = ActiveConditional::Case(num);
    num == <ET::Num as NumSet>::Int::default()
}

pub fn ifcat<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) -> bool {
    let first = get_if_token(engine);
    let second = get_if_token(engine);
    first.1 == second.1
}
fn get_if_token<ET:EngineTypes>(engine:&mut EngineReferences<ET>) -> (Option<ET::Char>,CommandCode) {
    let mut exp = true;
    while let Some(t) = engine.get_next() {
        if t.is_noexpand_marker() {
            exp = false;
            continue
        }
        match ET::Gullet::resolve(engine.state,t) {
            ResolvedToken::Tk {char,code,..} => return (Some(char),code),
            ResolvedToken::Cmd {cmd,token} => match cmd {
                Some(Command::Macro(m)) if exp =>
                    ET::Gullet::do_macro(engine,m.clone(),token),
                Some(Command::Conditional(cond)) if exp =>
                    ET::Gullet::do_conditional(engine,cond.name,token,cond.expand,false),
                Some(Command::Expandable(e)) if exp =>
                    ET::Gullet::do_expandable(engine,e.name,token,e.expand),
                Some(Command::SimpleExpandable(e)) if exp =>
                    ET::Gullet::do_simple_expandable(engine,e.name,token,e.expand),
                Some(Command::Char {char,code},..) => {
                    return (Some(*char),*code)
                }
                _ => return (None,CommandCode::Escape)
            },
        }
    }
    todo!("throw error")
}

pub fn ifdim<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) -> bool {
    let first = engine.read_dim(false);
    let rel = match engine.read_chars(&[b'=',b'<',b'>']) {
        Ok(b) => b,
        _ => todo!("throw error")
    };
    let second = engine.read_dim(false);
    //debug_log!(debug=>"Comparing {} {} {}",first,rel as char,second);
    match rel {
        b'=' => first == second,
        b'<' => first < second,
        b'>' => first > second,
        _ => unreachable!()
    }
}

pub fn ifeof<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) -> bool {
    let idx = read_file_index(engine);
    engine.filesystem.eof(idx)
}

pub fn ifhmode<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) -> bool {
    match engine.state.get_mode() {
        TeXMode::Horizontal | TeXMode::RestrictedHorizontal => true,
        _ => false

    }
}
pub fn ifinner<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) -> bool {
    match engine.state.get_mode() {
        TeXMode::RestrictedHorizontal | TeXMode::InternalVertical | TeXMode::InlineMath => true,
        _ => false

    }
}
pub fn ifmmode<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) -> bool {
    match engine.state.get_mode() {
        TeXMode::InlineMath | TeXMode::DisplayMath => true,
        _ => false

    }
}

pub fn ifnum<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) -> bool {
    let first = engine.read_int(false);
    let rel = match engine.read_chars(&[b'=',b'<',b'>']) {
        Ok(b) => b,
        _ => todo!("throw error")
    };
    let second = engine.read_int(false);
    //debug_log!(debug=>"Comparing {} {} {}",first,rel as char,second);
    match rel {
        b'=' => first == second,
        b'<' => first < second,
        b'>' => first > second,
        _ => unreachable!()
    }
}

#[inline(always)]
pub fn ifodd<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) -> bool {
    engine.read_int(false).into() % 2 != 0
}

#[inline(always)]
pub fn iftrue<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) -> bool {
    true
}

#[inline(always)]
pub fn iffalse<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) -> bool {
    false
}

pub fn ifvmode<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) -> bool {
    match engine.state.get_mode() {
        TeXMode::Vertical | TeXMode::InternalVertical => true,
        _ => false

    }
}
pub fn ifvbox<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) -> bool {
    let idx = read_register(engine);
    match engine.state.get_box_register(idx) {
        Some(TeXBox {info:BoxInfo{tp:BoxType::Vertical,..},..}) => true,
        _ => false
    }
}
pub fn ifvoid<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) -> bool {
    let idx = read_register(engine);
    engine.state.get_box_register(idx).is_none()
}
pub fn ifhbox<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) -> bool {
    let idx = read_register(engine);
    match engine.state.get_box_register(idx) {
        Some(TeXBox {info:BoxInfo{tp:BoxType::Horizontal,..},..}) => true,
        _ => false
    }
}

enum IfxCmd<ET:EngineTypes> {
    Char(ET::Char,CommandCode),
    Undefined,
    Primitive(PrimitiveIdentifier),
    Noexpand(ET::Token),
    Chardef(ET::Char),
    Font(<ET::FontSystem as FontSystem>::Font),
    MathChar(u32),
    Macro(Macro<ET::Token>),
    IntRegister(u16),
    DimRegister(u16),
    SkipRegister(u16),
    MuSkipRegister(u16),
    ToksRegister(u16),
    BoxRegister(u16),
}
impl<ET:EngineTypes> IfxCmd<ET> {
    fn read(engine:&mut EngineReferences<ET>) -> Self {
        match engine.get_next() {
            Some(t) if t.is_noexpand_marker() =>
                IfxCmd::Noexpand(engine.get_next().unwrap()),
            Some(t) => Self::resolve(engine.resolve(t)),
            _ => todo!("throw error")
        }
    }
    fn resolve<'a>(r:ResolvedToken<'a,ET>) -> Self {
        match r {
            ResolvedToken::Tk {char,code,..} => Self::Char(char,code),
            ResolvedToken::Cmd {cmd,..} => match cmd {
                Some(Command::Char {char,code}) => Self::Char(*char,*code),
                None => Self::Undefined,
                Some(Command::Expandable(Expandable{name,..}) |
                     Command::SimpleExpandable(SimpleExpandable{name,..})
                ) => Self::Primitive(*name),
                Some(Command::Macro(m)) => Self::Macro(m.clone()),
                Some(Command::Relax) => Self::Primitive(PRIMITIVES.relax),
                Some(Command::CharDef(c)) => Self::Chardef(*c),
                Some(Command::Int(i)) => Self::Primitive(i.name),
                Some(Command::Dim(i)) => Self::Primitive(i.name),
                Some(Command::Skip(i)) => Self::Primitive(i.name),
                Some(Command::MuSkip(i)) => Self::Primitive(i.name),
                Some(Command::Assignment(a)) => Self::Primitive(a.name),
                Some(Command::Unexpandable(a)) => Self::Primitive(a.name),
                Some(Command::Conditional(i)) => Self::Primitive(i.name),
                Some(Command::Box(i)) => Self::Primitive(i.name),
                Some(Command::Node(n)) => Self::Primitive(n.name),
                Some(Command::Whatsit(n)) => Self::Primitive(n.name),
                Some(Command::Font(f)) => Self::Font(f.clone()),
                Some(Command::MathChar(u)) => Self::MathChar(*u),
                Some(Command::IntRegister(u)) => Self::IntRegister(*u),
                Some(Command::DimRegister(u)) => Self::DimRegister(*u),
                Some(Command::SkipRegister(u)) => Self::SkipRegister(*u),
                Some(Command::MuSkipRegister(u)) => Self::MuSkipRegister(*u),
                Some(Command::ToksRegister(u)) => Self::ToksRegister(*u),
                Some(Command::PrimitiveInt(id)) => Self::Primitive(*id),
                Some(Command::PrimitiveDim(id)) => Self::Primitive(*id),
                Some(Command::PrimitiveSkip(id)) => Self::Primitive(*id),
                Some(Command::PrimitiveMuSkip(id)) => Self::Primitive(*id),
                Some(Command::PrimitiveToks(id)) => Self::Primitive(*id),
                //Some(Command::BoxRegister(u)) => Self::BoxRegister(*u),
                o => todo!("{:?}",o)
            },
        }
    }
}

impl<ET:EngineTypes> PartialEq for IfxCmd<ET> {
    fn eq(&self, other: &Self) -> bool {
        match (self,other) {
            (Self::Char(_,CommandCode::Space),Self::Char(_,CommandCode::Space)) => true,
            (Self::Char(c1,cc1),Self::Char(c2,cc2)) => c1 == c2 && cc1 == cc2,
            (Self::Undefined,Self::Undefined) => true,
            (Self::Primitive(id),Self::Primitive(id2)) => id == id2,
            (Self::Noexpand(t1),Self::Noexpand(t2)) => t1 == t2,
            (Self::Chardef(c),Self::Chardef(c2)) => c == c2,
            (Self::Font(f1),Self::Font(f2)) => f1.name() == f2.name(),
            (Self::MathChar(u1),Self::MathChar(u2)) => u1 == u2,
            (Self::IntRegister(u1),Self::IntRegister(u2)) => u1 == u2,
            (Self::DimRegister(u1),Self::DimRegister(u2)) => u1 == u2,
            (Self::SkipRegister(u1),Self::SkipRegister(u2)) => u1 == u2,
            (Self::MuSkipRegister(u1),Self::MuSkipRegister(u2)) => u1 == u2,
            (Self::ToksRegister(u1),Self::ToksRegister(u2)) => u1 == u2,
            (Self::BoxRegister(u1),Self::BoxRegister(u2)) => u1 == u2,
            (Self::Macro(m1),Self::Macro(m2)) =>
                m1.long == m2.long && m1.outer == m2.outer && m1.protected == m2.protected &&
                    m1.signature.params == m2.signature.params &&
                    m1.expansion == m2.expansion
            ,
            _ => false
        }
    }
}

pub fn ifx<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) -> bool {
    let first = IfxCmd::read(engine);
    let second = IfxCmd::read(engine);
    first == second
}

pub fn ignorespaces<ET:EngineTypes>(engine:&mut EngineReferences<ET>,tk:ET::Token) {
    while let Some(next) = engine.get_next() {
        if !next.is_space() {
            match ET::Gullet::resolve(engine.state,next) {
                ResolvedToken::Cmd { cmd: Some(Command::Char{code:CommandCode::Space,..}), .. } => (),
                ResolvedToken::Cmd {token,..} | ResolvedToken::Tk {token,..} => {
                    engine.requeue(token);
                    break
                }
            }
        }
    }
}

pub fn immediate<ET:EngineTypes>(engine:&mut EngineReferences<ET>,tk:ET::Token) {
    expand_loop!(engine,
        ResolvedToken::Cmd{cmd:Some(Command::Whatsit(Whatsit { immediate,.. })),token} => {
            return immediate(engine,token)
        },
        ResolvedToken::Tk {token,char,code} => ET::Stomach::do_char(engine,token,char,code),
        ResolvedToken::Cmd {cmd:Some(Command::Char {char,code}),token} => ET::Stomach::do_char(engine,token,*char,*code),
        ResolvedToken::Cmd{cmd: None,token} => engine.aux.error_handler.undefined(engine.aux.memory.cs_interner(),token),
        ResolvedToken::Cmd {cmd:Some(c),token} =>
            crate::do_cmd!(engine,token,c)
    );
}

pub fn input<ET:EngineTypes>(engine:&mut EngineReferences<ET>,tk:ET::Token) {
    let mut filename = engine.aux.memory.get_string();
    engine.read_string(false,&mut filename);
    if filename.is_empty() {
        todo!("throw error")
    }
    let file = engine.filesystem.get(&filename);
    if !file.exists() {
        todo!("throw error")
    }
    engine.aux.memory.return_string(filename);
    engine.aux.outputs.file_open(&file);
    engine.push_file(file);
}

pub fn fi<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) {
    let conds = engine.gullet.get_conditionals();
    let name = match conds.pop() {
        Some(ActiveConditional::True(id)|ActiveConditional::Else(id)) => id,
        Some(ActiveConditional::Case(_)) => PRIMITIVES.ifcase,
        Some(u@ActiveConditional::Unfinished(_)) => {
            conds.push(u);
            engine.mouth.requeue(tk);
            let relax = engine.aux.memory.cs_interner_mut().new("relax");
            engine.mouth.requeue(ET::Token::from_cs(relax));
            return
        }
        o => todo!("{:?}",o)
    };
    let trace = engine.state.get_primitive_int(PRIMITIVES.tracingifs) > Int::<ET>::default();
    let index = conds.len() + 1;
    if trace {
        engine.aux.outputs.write_neg1(
            format_args!("{{{}fi: {} (level {}) entered on line {}}}",
                         <ET::Char as Character>::displayable_opt(engine.state.get_escape_char()),
                         PRIMITIVES.printable(name,engine.state.get_escape_char()),index,engine.mouth.line_number()));
    }
}

pub fn jobname<ET:EngineTypes>(engine: &mut EngineReferences<ET>,exp:&mut Vec<ET::Token>,tk:ET::Token) {
    let mut fi = |t| exp.push(t);
    let mut f = Tokenizer::new(&mut fi);
    write!(f,"{}",engine.aux.jobname).unwrap();
}

pub fn let_<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token,globally:bool) {
    let cm = match engine.get_next() {
        Some(t) => match t.to_enum() {
            StandardToken::Character(c,CommandCode::Active) =>
                ACOrCS::Active(c),
            StandardToken::ControlSequence(cs) => ACOrCS::Name(cs),
            _ => todo!("throw error")
        }
        None => todo!("file end error")
    };
    let mut after_eq = false;
    let mut after_space = false;
    while let Some(next) = engine.get_next() {
        let cmd = match next.to_enum() {
            StandardToken::Character(_,CommandCode::Space) if !after_eq => continue,
            StandardToken::Character(_,CommandCode::Space) if !after_space => {
                after_space = true;continue
            },
            StandardToken::Character(c,CommandCode::Other) if matches!(c.try_into(),Ok(b'=')) => {
                after_eq = true; continue
            }
            StandardToken::ControlSequence(cs) =>
                engine.state.get_command(&cs).cloned(),
            StandardToken::Character(c,CommandCode::Active) =>
                engine.state.get_ac_command(c).cloned(),
            StandardToken::Character(c,cc) =>
                Some(Command::Char{char:c,code:cc})
        };
        engine.set_command(&cm,cmd,globally);
        return
    }
    todo!("file end")
}


pub fn futurelet<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token,globally:bool) {
    let cm = match engine.get_next() {
        Some(t) => match t.to_enum() {
            StandardToken::Character(c,CommandCode::Active) =>
                ACOrCS::Active(c),
            StandardToken::ControlSequence(cs) => ACOrCS::Name(cs),
            _ => todo!("throw error")
        }
        None => todo!("file end error")
    };
    let first = match engine.get_next() {
        Some(t) => t,
        _ => todo!("error")
    };
    let second = match engine.get_next() {
        Some(t) => t,
        _ => todo!("error")
    };
    let cmd = match second.to_enum() {
        StandardToken::ControlSequence(cs) =>
            engine.state.get_command(&cs).cloned(),
        StandardToken::Character(c,CommandCode::Active) =>
            engine.state.get_ac_command(c).cloned(),
        StandardToken::Character(c,cc) =>
            Some(Command::Char{char:c,code:cc})
    };
    engine.set_command(&cm,cmd,globally);
    engine.requeue(second);
    engine.requeue(first);
}

pub fn lowercase<ET:EngineTypes>(engine:&mut EngineReferences<ET>,token:ET::Token) {
    engine.expand_until_bgroup(false);
    let mut exp = Vec::new();// ET::Gullet::get_expansion_container(engine);
    let state = &engine.state;
    engine.gullet.read_until_endgroup(engine.mouth,engine.aux,state,|_,_,t| {
        match t.to_enum() {
            StandardToken::ControlSequence(_) => exp.push(t),
            StandardToken::Character(c,cc) => {
                let lccode = state.get_lccode(c);
                if lccode == ET::Char::default() {
                    exp.push(t)
                } else {
                    exp.push(ET::Token::from_char_cat(lccode,cc))
                }
            }
        }
    });
    engine.mouth.push_vec(exp.into_iter());
}

pub fn uppercase<ET:EngineTypes>(engine:&mut EngineReferences<ET>,token:ET::Token) {
    engine.expand_until_bgroup(false);
    let mut exp = Vec::new();//ET::Gullet::get_expansion_container(engine);
    let state = &engine.state;
    engine.gullet.read_until_endgroup(engine.mouth,engine.aux,state,|_,_,t| {
        match t.to_enum() {
            StandardToken::ControlSequence(_) => exp.push(t),
            StandardToken::Character(c,cc) => {
                let uccode = state.get_uccode(c);
                if uccode == ET::Char::default() {
                    exp.push(t)
                } else {
                    exp.push(ET::Token::from_char_cat(uccode,cc))
                }
            }
        }
    });
    engine.mouth.push_vec(exp.into_iter());
}

pub fn mathchardef<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token,globally:bool) {
    let cm = match engine.get_next() {
        Some(t) => match t.to_enum() {
            StandardToken::Character(c,CommandCode::Active) =>
                ACOrCS::Active(c),
            StandardToken::ControlSequence(cs) => ACOrCS::Name(cs),
            _ => todo!("throw error")
        }
        None => todo!("file end error")
    };
    let i = engine.read_int(true).into();
    if i < 0 || i > u32::MAX as i64 {
        todo!("matchchardef out of range")
    }
    let i = i as u32;
    engine.set_command(&cm,Some(Command::MathChar(i)),globally)
}

pub fn meaning<ET:EngineTypes>(engine: &mut EngineReferences<ET>,exp:&mut Vec<ET::Token>,tk:ET::Token) {
    let mut fi = |t| exp.push(t);
    let mut f = Tokenizer::new(&mut fi);
    use crate::engine::mouth::pretokenized::WriteChars;
    match engine.get_next() {
        None => todo!("throw error"),
        Some(t) => match ET::Gullet::resolve(engine.state,t) {
            ResolvedToken::Cmd {cmd:None,..} => {
                if let Some(c) = engine.state.get_escape_char() {
                    f.push_char(c);
                }
                write!(f,"undefined").unwrap();
            }
            ResolvedToken::Cmd{cmd:Some(cmd),..} => {
                cmd.meaning(engine.aux.memory.cs_interner(),engine.state.get_catcode_scheme(),engine.state.get_escape_char()).write_chars(f)
            }
            ResolvedToken::Tk {char,code,..} =>
                code.meaning(char,f)
        }
    }
}

pub fn message<ET:EngineTypes>(engine:&mut EngineReferences<ET>,token:ET::Token) {
    let mut out = engine.aux.memory.get_string();
    engine.read_braced_string(false,&mut out);
    engine.aux.outputs.message(&out);
    engine.aux.memory.return_string(out);
}

pub fn errmessage<ET:EngineTypes>(engine:&mut EngineReferences<ET>,token:ET::Token) {
    let mut out = engine.aux.memory.get_string();
    engine.read_braced_string(false,&mut out);
    write!(out," (line {})",engine.mouth.line_number());
    engine.aux.outputs.errmessage(&out);
    engine.aux.memory.return_string(out);
}

pub fn newlinechar_get<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) -> Int<ET> {
    Int::<ET>::from(match engine.state.get_newline_char() {
        Some(c) => c.into() as i32,
        _ => -1
    })
}
pub fn newlinechar_set<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token,globally:bool) {
    let val: i64 = engine.read_int(true).into();
    let val = if val == -1 { None } else
        if val < -1 {
            todo!("newlinechar out of range")
        } else {
        match ET::Char::try_from(val as u64) {
            Ok(c) => Some(c),
            _ => todo!("newlinechar out of range")
        }
    };
    engine.state.set_newline_char(engine.aux,val,globally)
}

pub fn number<ET:EngineTypes>(engine: &mut EngineReferences<ET>,exp:&mut Vec<ET::Token>,tk:ET::Token) {
    let val = engine.read_int(false);
    write!(Tokenizer::new(&mut|t| exp.push(t)),"{}",val).unwrap();
}

pub fn noexpand<ET:EngineTypes>(engine:&mut EngineReferences<ET>,tk:ET::Token) {
    let res = match engine.get_next() {
        Some(t) if t == ET::Token::eof() => return,
        Some(t) => ET::Gullet::resolve(engine.state,t),
        _ => todo!("throw error")
    };
    match res {
        ResolvedToken::Tk {token,..} =>
            engine.requeue(token),
        ResolvedToken::Cmd {cmd:Some(cm),token} => {
            match cm {
                Command::Macro(_) |
                Command::Expandable(_) |
                Command::SimpleExpandable(_) |
                Command::Conditional(_) => {
                    engine.mouth.requeue(token);
                    engine.mouth.requeue(ET::Token::noexpand_marker());
                }
                _ => engine.mouth.requeue(token)
            };
        }
        ResolvedToken::Cmd {token,..} =>
            engine.mouth.requeue(token),
    }
}

pub fn noindent<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) {
    if ET::Stomach::maybe_switch_mode(engine,NodeCommandScope::SwitchesToHorizontal,tk) {
        // TODO maybe
    }
}

pub fn openout<ET:EngineTypes>(engine:&mut EngineReferences<ET>, token:ET::Token)
               -> Option<Box<dyn FnOnce(&mut EngineReferences<ET>) -> Option<ShipoutNode<ET>>>> {
    let (idx,file) = read_index_and_file(engine);
    Some(Box::new(move |engine| {engine.filesystem.open_out(idx,file);None}))
}
pub fn openout_immediate<ET:EngineTypes>(engine:&mut EngineReferences<ET>,token:ET::Token) {
    let (idx,file) = read_index_and_file(engine);
    engine.filesystem.open_out(idx,file)
}

pub fn prevdepth_get<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) -> Dim<ET> {
    engine.stomach.data_mut().prevdepth
}
pub fn prevdepth_set<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token,_globally:bool) {
    let val = engine.read_dim(true);
    engine.stomach.data_mut().prevdepth = val;
}

pub fn read<ET:EngineTypes>(engine:&mut EngineReferences<ET>,token:ET::Token,globally:bool) {
    let idx = read_file_index(engine);
    if !engine.read_keyword("to".as_bytes()) {
        todo!("throw error")
    }
    let cs = engine.read_control_sequence();
    let mut ret = shared_vector::Vector::new();
    engine.filesystem.read(idx,&engine.aux.error_handler,engine.aux.memory.cs_interner_mut(),
    engine.state.get_catcode_scheme(),engine.state.get_endline_char(),|t| ret.push(t));

/*
    let mut str = format!("{}",TLVecMeaning::new(&ret,engine.aux.memory.cs_interner(),engine.state.get_catcode_scheme(),engine.state.get_escape_char()));
    //debug_log!(debug => "Here: {}",str);
    if str.starts_with("102A0;CARIAN") {
        engine.state.set_primitive_int(engine.aux,PRIMITIVES.tracingassigns,1.into(),true);
        engine.state.set_primitive_int(engine.aux,PRIMITIVES.tracingifs,1.into(),true);
        engine.state.set_primitive_int(engine.aux,PRIMITIVES.tracingcommands,1.into(),true);
        engine.state.set_primitive_int(engine.aux,PRIMITIVES.tracinggroups,1.into(),true);
        engine.state.set_primitive_int(engine.aux,PRIMITIVES.tracingrestores,1.into(),true);
    }
*/


    let m = Macro {
        long:false,outer:false,protected:false,
        expansion:ret.into(),
        signature:MacroSignature {
            arity:0,
            params:engine.aux.memory.empty().into()
        }
    };
    engine.set_command(&cs,Some(Command::Macro(m)),globally)
}

const ROMAN: &[(u8, i64)] = &[(b'm', 0),(b'd', 2),(b'c', 5),(b'l', 2),(b'x', 5),(b'v', 2),(b'i', 5)];
pub fn romannumeral<ET:EngineTypes>(engine: &mut EngineReferences<ET>,exp:&mut Vec<ET::Token>,tk:ET::Token) {
    let mut n = engine.read_int(false).into();
    if n < 0 { return }
    let mut v = 1000;
    let mut j = 0;
    loop {
        while n >= v {
            exp.push(ET::Token::from_char_cat(ET::Char::from(ROMAN[j].0), CommandCode::Other));
            n -= v;
        }
        if n <= 0 { return }
        let mut k = ROMAN[j+1];
        let mut u = v / k.1;
        if k.1 == 2 {
            k = ROMAN[j+2];
            u /= k.1;
        }
        if n + u >= v {
            exp.push(ET::Token::from_char_cat(ET::Char::from(k.0),CommandCode::Other));
            n += u;
        } else {
            j += 1;
            v /= ROMAN[j].1;
        }
    }
}

pub fn setbox<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token,globally:bool) {
    let index = read_register(engine);
    match engine.read_box(true) {
        Ok(bx) =>
            engine.state.set_box_register(engine.aux,index,bx,globally),
        Err(bi) => {
            engine.stomach.data_mut().open_lists.push(
                NodeList {
                    children:vec!(),
                    tp:NodeListType::Box(bi,engine.mouth.start_ref(), BoxTarget::Register{index,globally})
                }
            );
        }
    }
}

pub fn moveright<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) {
    if ET::Stomach::maybe_switch_mode(engine,NodeCommandScope::SwitchesToVertical,tk) {
        let dim = engine.read_dim(false);
        match engine.read_box(false) {
            Ok(Some(mut bx)) => {
                bx.info.moved_left = Some(-dim);
                ET::Stomach::add_node(engine,bx.as_node());
            }
            Ok(None) => (),
            Err(mut bi) => {
                bi.moved_left = Some(-dim);
                engine.stomach.data_mut().open_lists.push(
                    NodeList {
                        children:vec!(),
                        tp:NodeListType::Box(bi,engine.mouth.start_ref(), BoxTarget::List)
                    }
                );
            }
        }
    }
}

pub fn moveleft<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) {
    if ET::Stomach::maybe_switch_mode(engine,NodeCommandScope::SwitchesToVertical,tk) {
        let dim = engine.read_dim(false);
        match engine.read_box(false) {
            Ok(Some(mut bx)) => {
                bx.info.moved_left = Some(dim);
                ET::Stomach::add_node(engine,bx.as_node());
            }
            Ok(None) => (),
            Err(mut bi) => {
                bi.moved_left = Some(dim);
                engine.stomach.data_mut().open_lists.push(
                    NodeList {
                        children:vec!(),
                        tp:NodeListType::Box(bi,engine.mouth.start_ref(), BoxTarget::List)
                    }
                );
            }
        }
    }
}

pub fn raise<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) {
    if ET::Stomach::maybe_switch_mode(engine,NodeCommandScope::SwitchesToHorizontal,tk) {
        let dim = engine.read_dim(false);
        match engine.read_box(false) {
            Ok(Some(mut bx)) => {
                bx.info.raised = Some(dim);
                ET::Stomach::add_node(engine,bx.as_node());
            }
            Ok(None) => (),
            Err(mut bi) => {
                bi.raised = Some(dim);
                engine.stomach.data_mut().open_lists.push(
                    NodeList {
                        children:vec!(),
                        tp:NodeListType::Box(bi,engine.mouth.start_ref(), BoxTarget::List)
                    }
                );
            }
        }
    }
}

pub fn lower<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) {
    if ET::Stomach::maybe_switch_mode(engine,NodeCommandScope::SwitchesToHorizontal,tk) {
        let dim = engine.read_dim(false);
        match engine.read_box(false) {
            Ok(Some(mut bx)) => {
                bx.info.raised = Some(-dim);
                ET::Stomach::add_node(engine,bx.as_node());
            }
            Ok(None) => (),
            Err(mut bi) => {
                bi.raised = Some(-dim);
                engine.stomach.data_mut().open_lists.push(
                    NodeList {
                        children:vec!(),
                        tp:NodeListType::Box(bi,engine.mouth.start_ref(), BoxTarget::List)
                    }
                );
            }
        }
    }
}

pub fn string<ET:EngineTypes>(engine: &mut EngineReferences<ET>,exp:&mut Vec<ET::Token>,tk:ET::Token) {
    match engine.get_next() {
        Some(t) => {
            if t.is_space() {exp.push(t)}
            else {
                match t.to_enum() {
                    StandardToken::Character(c, _) => exp.push(ET::Token::from_char_cat(c, CommandCode::Other)),
                    StandardToken::ControlSequence(cs) => {
                        let res = engine.aux.memory.cs_interner().resolve(&cs);
                        match engine.state.get_escape_char() {
                            Some(c) => exp.push(ET::Token::from_char_cat(c, CommandCode::Other)),
                            _ => ()
                        }
                        for u in res.iter() {
                            match u.try_into() {
                                Ok(b' ') => exp.push(ET::Token::space()),
                                _ =>
                                    exp.push(ET::Token::from_char_cat(u, CommandCode::Other))
                            }
                        }
                    }
                }
            }
        }
        None => todo!("file end")
    }
}

pub fn openin<ET:EngineTypes>(engine:&mut EngineReferences<ET>,token:ET::Token) {
    let (idx,file) = read_index_and_file(engine);
    engine.filesystem.open_in(idx,file)
}

pub fn closeout<ET:EngineTypes>(engine:&mut EngineReferences<ET>, token:ET::Token)
                               -> Option<Box<dyn FnOnce(&mut EngineReferences<ET>) -> Option<ShipoutNode<ET>>>> {
    let idx = read_file_index(engine);
    Some(Box::new(move |engine| {engine.filesystem.close_out(idx);None}))
}
pub fn closeout_immediate<ET:EngineTypes>(engine:&mut EngineReferences<ET>,token:ET::Token) {
    let idx = read_file_index(engine);
    engine.filesystem.close_out(idx)
}

pub fn closein<ET:EngineTypes>(engine:&mut EngineReferences<ET>,token:ET::Token) {
    let idx = read_file_index(engine);
    engine.filesystem.close_in(idx)
}

pub fn write<ET:EngineTypes>(engine:&mut EngineReferences<ET>, token:ET::Token)
                             -> Option<Box<dyn FnOnce(&mut EngineReferences<ET>) -> Option<ShipoutNode<ET>>>> {
    let idx = engine.read_int(false).into();
    let mut tks = engine.aux.memory.get_token_vec();
    tks.push(ET::Token::from_char_cat(b'{'.into(),CommandCode::BeginGroup));
    match engine.get_next() {
        Some(t) if t.is_begin_group() => (),
        Some(_) => todo!("should be begingroup"),
        None => todo!("file end")
    }
    engine.read_until_endgroup(|_,_,t| {
        tks.push(t);
    });
    tks.push(ET::Token::from_char_cat(b'}'.into(),CommandCode::EndGroup));
    Some(Box::new(move |engine| {do_write(engine,idx,tks);None}))
}
pub fn write_immediate<ET:EngineTypes>(engine:&mut EngineReferences<ET>,token:ET::Token) {
    let idx = engine.read_int(false).into();
    let mut out = engine.aux.memory.get_string();
    match engine.get_next() {
        Some(t) if t.is_begin_group() => (),
        Some(_) => todo!("should be begingroup"),
        None => todo!("file end")
    }
    ET::Gullet::expand_until_endgroup(engine,true,false,|a,s,_,t| {
        t.display_fmt(a.memory.cs_interner(),s.get_catcode_scheme(),
                      s.get_escape_char(),&mut out).unwrap();
    });
    engine.filesystem.write(idx,&out,engine.state.get_newline_char(),engine.aux);
    engine.aux.memory.return_string(out);
}
pub fn do_write<ET:EngineTypes>(engine:&mut EngineReferences<ET>,i:i64,v:Vec<ET::Token>) {
    engine.mouth.push_vec(v.into_iter());
    let mut out = String::new();
    engine.read_braced_string(false,&mut out);
    engine.filesystem.write(i,&out,engine.state.get_newline_char(),engine.aux);
}

pub fn read_file_index<ET:EngineTypes>(engine:&mut EngineReferences<ET>) -> u8 {
    let idx = engine.read_int(false);
    if idx < Int::<ET>::default() || idx.into() > 255 {
        todo!("throw error")
    }
    idx.into() as u8
}
fn read_index_and_file<ET:EngineTypes>(engine:&mut EngineReferences<ET>) -> (u8,ET::File) {
    let idx = read_file_index(engine);
    let mut filename = engine.aux.memory.get_string();
    engine.read_string(true,&mut filename);
    if filename.is_empty() {
        todo!("throw error")
    }
    let file = engine.filesystem.get(&filename);
    engine.aux.memory.return_string(filename);
    (idx,file)
}

pub fn par<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) {
    let mode = engine.state.get_mode();
    if mode.is_vertical() {return}
    if mode == TeXMode::Horizontal {
        ET::Stomach::close_paragraph(engine);return
    }
    todo!("throw error?")
}

pub fn do_the<ET:EngineTypes,F:FnMut(&mut EngineAux<ET>,&ET::State,&mut ET::Gullet,ET::Token)>(engine: &mut EngineReferences<ET>,mut cont:F) {
    expand_loop!(engine,
        ResolvedToken::Cmd {cmd:Some(c),token} => match c {
            Command::Int(ic) => {
                let val = (ic.read)(engine,token);
                write!(Tokenizer::new(&mut |t| cont(engine.aux,engine.state,engine.gullet,t)),"{}",val).unwrap();
                return ()
            }
            Command::Dim(ic) => {
                let val = (ic.read)(engine,token);
                write!(Tokenizer::new(&mut |t| cont(engine.aux,engine.state,engine.gullet,t)),"{}",val).unwrap();
                return ()
            }
            Command::Skip(ic) => {
                let val = (ic.read)(engine,token);
                write!(Tokenizer::new(&mut |t| cont(engine.aux,engine.state,engine.gullet,t)),"{}",val).unwrap();
                return ()
            }
            Command::MuSkip(ic) => {
                let val = (ic.read)(engine,token);
                write!(Tokenizer::new(&mut |t| cont(engine.aux,engine.state,engine.gullet,t)),"{}",val).unwrap();
                return ()
            }
            Command::IntRegister(u) => {
                let val = engine.state.get_int_register(*u);
                write!(Tokenizer::new(&mut |t| cont(engine.aux,engine.state,engine.gullet,t)),"{}",val).unwrap();
                return ()
            }
            Command::DimRegister(u) => {
                let val = engine.state.get_dim_register(*u);
                write!(Tokenizer::new(&mut |t| cont(engine.aux,engine.state,engine.gullet,t)),"{}",val).unwrap();
                return ()
            }
            Command::SkipRegister(u) => {
                let val = engine.state.get_skip_register(*u);
                write!(Tokenizer::new(&mut |t| cont(engine.aux,engine.state,engine.gullet,t)),"{}",val).unwrap();
                return ()
            }
            Command::MuSkipRegister(u) => {
                let val = engine.state.get_muskip_register(*u);
                write!(Tokenizer::new(&mut |t| cont(engine.aux,engine.state,engine.gullet,t)),"{}",val).unwrap();
                return ()
            }
            Command::PrimitiveInt(u) => {
                let val = engine.state.get_primitive_int(*u);
                write!(Tokenizer::new(&mut |t| cont(engine.aux,engine.state,engine.gullet,t)),"{}",val).unwrap();
                return ()
            }
            Command::PrimitiveDim(u) => {
                let val = engine.state.get_primitive_dim(*u);
                write!(Tokenizer::new(&mut |t| cont(engine.aux,engine.state,engine.gullet,t)),"{}",val).unwrap();
                return ()
            }
            Command::PrimitiveSkip(u) => {
                let val = engine.state.get_primitive_skip(*u);
                write!(Tokenizer::new(&mut |t| cont(engine.aux,engine.state,engine.gullet,t)),"{}",val).unwrap();
                return ()
            }
            Command::PrimitiveMuSkip(u) => {
                let val = engine.state.get_primitive_muskip(*u);
                write!(Tokenizer::new(&mut |t| cont(engine.aux,engine.state,engine.gullet,t)),"{}",val).unwrap();
                return ()
            }
            Command::CharDef(c) => {
                let val : u64 = (*c).into();
                write!(Tokenizer::new(&mut |t| cont(engine.aux,engine.state,engine.gullet,t)),"{}",val).unwrap();
                return ()
            }
            Command::MathChar(u) => {
                write!(Tokenizer::new(&mut |t| cont(engine.aux,engine.state,engine.gullet,t)),"{}",*u).unwrap();
                return ()
            }
            Command::ToksRegister(u) => {
                for t in engine.state.get_toks_register(*u).inner() {
                    cont(engine.aux,engine.state,engine.gullet,t.clone())
                }
                return ()
            }
            Command::Assignment(a) if a.name == PRIMITIVES.toks => {
                let u = engine.read_int(false);
                if u < Int::<ET>::default() || u.into() > u16::MAX.into() {
                    todo!("throw error")
                }
                for t in engine.state.get_toks_register(u.into() as u16).inner() {
                    cont(engine.aux,engine.state,engine.gullet,t.clone())
                }
                return ()
            }
            Command::PrimitiveToks(n) => {
                for t in engine.state.get_primitive_tokens(*n).inner() {
                    cont(engine.aux,engine.state,engine.gullet,t.clone())
                }
                return ()
            }
            Command::Font(fnt) => {
                let t = fnt.name();
                cont(engine.aux,engine.state,engine.gullet,ET::Token::from_cs(t.clone()));
                return ()
            }
            Command::FontCmd(fnt) => {
                let fnt = (fnt.read)(engine,token);
                let t = fnt.name();
                cont(engine.aux,engine.state,engine.gullet,ET::Token::from_cs(t.clone()));
                return ()
            }
            o => todo!("Here: {:?} in \\the - {}",o,engine.mouth.display_position())
        }
        o => todo!("{:?} in \\the",o)
    );
}

#[inline(always)]
pub fn the<ET:EngineTypes>(engine: &mut EngineReferences<ET>,exp:&mut Vec<ET::Token>,tk:ET::Token) {
    do_the(engine,|_,_,_,t|exp.push(t))
}

pub fn time<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) -> Int<ET> {
    let now = engine.aux.start_time;
    let i = ((now.hour() * 60) + now.minute()) as i32;
    Int::<ET>::from(i)
}

pub fn toks<ET:EngineTypes>(engine:&mut EngineReferences<ET>,tk:ET::Token,global:bool) {
    let idx = read_register(engine);
    let mut had_eq = false;
    crate::expand_loop!(engine,
        ResolvedToken::Tk{char,code,..} => match (char.try_into(),code) {
            (_,CommandCode::Space) => (),
            (Ok(b'='),CommandCode::Other) if !had_eq => {
                if had_eq { todo!("throw error") }
                had_eq = true;
            }
            (_,CommandCode::BeginGroup) => {
                let mut tks = shared_vector::Vector::new();
                let cc = engine.state.get_catcode_scheme();
                let endline = engine.state.get_endline_char();
                engine.read_until_endgroup(|_,_,t| tks.push(t));
                engine.state.set_toks_register(engine.aux,idx,TokenList::from(tks),global);
                return ()
            }
            _ => todo!("throw error")
        }
        _ => todo!("throw error")
    )
}

pub fn penalty<ET:EngineTypes>(engine:&mut EngineReferences<ET>,tk:ET::Token) -> PreShipoutNode<ET> {
    PreShipoutNode::Penalty(match engine.read_int(false).try_into() {
        Ok(i) => i,
        _ => todo!("error: penalty out of range")
    })
}

pub fn kern<ET:EngineTypes>(engine:&mut EngineReferences<ET>,tk:ET::Token) -> PreShipoutNode<ET> {
    let dim = engine.read_dim(false);
    match engine.state.get_mode() {
        TeXMode::Vertical | TeXMode::InternalVertical =>
            PreShipoutNode::Kern(KernNode::VKern(dim)),
        _ => PreShipoutNode::Kern(KernNode::HKern(dim))
    }
}

pub fn vrule<ET:EngineTypes>(engine:&mut EngineReferences<ET>,tk:ET::Token) -> PreShipoutNode<ET> {
    let start = engine.mouth.start_ref();
    let mut width = None;
    let mut height = None;
    let mut depth = None;
    loop {
        match engine.read_keywords(&[b"width",b"height",b"depth"]) {
            Some(b"width") => {
                width = Some(engine.read_dim(false));
            }
            Some(b"height") => {
                height = Some(engine.read_dim(false));
            }
            Some(b"depth") => {
                depth = Some(engine.read_dim(false));
            }
            _ => break
        }
    }
    let mut end = engine.mouth.current_sourceref();
    SimpleNode::VRule {width,height,depth,start,end}.as_node()
}

pub fn hrule<ET:EngineTypes>(engine:&mut EngineReferences<ET>,tk:ET::Token) -> PreShipoutNode<ET> {
    let start = engine.mouth.start_ref();
    let mut width = None;
    let mut height = None;
    let mut depth = None;
    loop {
        match engine.read_keywords(&[b"width",b"height",b"depth"]) {
            Some(b"width") => {
                width = Some(engine.read_dim(false));
            }
            Some(b"height") => {
                height = Some(engine.read_dim(false));
            }
            Some(b"depth") => {
                depth = Some(engine.read_dim(false));
            }
            _ => break
        }
    }
    let mut end = engine.mouth.current_sourceref();
    SimpleNode::HRule {width,height,depth,start,end}.as_node()
}

pub fn vskip<ET:EngineTypes>(engine:&mut EngineReferences<ET>,tk:ET::Token) -> PreShipoutNode<ET> {
    let sk = engine.read_skip(false);
    SkipNode::VSkip(sk).as_node()
}

pub fn vfil<ET:EngineTypes>(engine:&mut EngineReferences<ET>,tk:ET::Token) -> PreShipoutNode<ET> {
    SkipNode::VFil.as_node()
}
pub fn vfill<ET:EngineTypes>(engine:&mut EngineReferences<ET>,tk:ET::Token) -> PreShipoutNode<ET> {
    SkipNode::VFill.as_node()
}
pub fn vfilneg<ET:EngineTypes>(engine:&mut EngineReferences<ET>,tk:ET::Token) -> PreShipoutNode<ET> {
    SkipNode::VFilneg.as_node()
}
pub fn vss<ET:EngineTypes>(engine:&mut EngineReferences<ET>,tk:ET::Token) -> PreShipoutNode<ET> {
    SkipNode::Vss.as_node()
}

pub fn hskip<ET:EngineTypes>(engine:&mut EngineReferences<ET>,tk:ET::Token) -> PreShipoutNode<ET> {
    let sk = engine.read_skip(false);
    SkipNode::HSkip(sk).as_node()
}
pub fn hfil<ET:EngineTypes>(engine:&mut EngineReferences<ET>,tk:ET::Token) -> PreShipoutNode<ET> {
    SkipNode::HFil.as_node()
}
pub fn hfill<ET:EngineTypes>(engine:&mut EngineReferences<ET>,tk:ET::Token) -> PreShipoutNode<ET> {
    SkipNode::HFill.as_node()
}
pub fn hfilneg<ET:EngineTypes>(engine:&mut EngineReferences<ET>,tk:ET::Token) -> PreShipoutNode<ET> {
    SkipNode::HFilneg.as_node()
}
pub fn hss<ET:EngineTypes>(engine:&mut EngineReferences<ET>,tk:ET::Token) -> PreShipoutNode<ET> {
    SkipNode::Hss.as_node()
}

pub fn unskip<ET:EngineTypes>(engine:&mut EngineReferences<ET>,tk:ET::Token) {
    let data = engine.stomach.data_mut();
    let ls = data.get_list();

    let mut readd = arrayvec::ArrayVec::<PreShipoutNode<ET>,10>::new();
    loop {
        match ls.last_mut() {
            Some(PreShipoutNode::Skip(_)) => { ls.pop(); }
            Some(n) if n.opaque() => {
                readd.push(ls.pop().unwrap());
            }
            _ => break
        }
    }
    for n in readd.into_iter().rev() {
        ls.push(n);
    }

}
pub fn unkern<ET:EngineTypes>(engine:&mut EngineReferences<ET>,tk:ET::Token) {
    let data = engine.stomach.data_mut();
    let ls = data.get_list();

    let mut readd = arrayvec::ArrayVec::<PreShipoutNode<ET>,10>::new();
    loop {
        match ls.last_mut() {
            Some(PreShipoutNode::Kern(_)) => { ls.pop(); }
            Some(n) if n.opaque() => {
                readd.push(ls.pop().unwrap());
            }
            _ => break
        }
    }
    for n in readd.into_iter().rev() {
        ls.push(n);
    }
}
pub fn unpenalty<ET:EngineTypes>(engine:&mut EngineReferences<ET>,tk:ET::Token) {
    let data = engine.stomach.data_mut();
    let ls = data.get_list();

    let mut readd = arrayvec::ArrayVec::<PreShipoutNode<ET>,10>::new();
    loop {
        match ls.last_mut() {
            Some(PreShipoutNode::Penalty(_)) => { ls.pop(); }
            Some(n) if n.opaque() => {
                readd.push(ls.pop().unwrap());
            }
            _ => break
        }
    }
    for n in readd.into_iter().rev() {
        ls.push(n);
    }
}

pub fn lastbox<ET:EngineTypes>(engine:&mut EngineReferences<ET>,tk:ET::Token) -> Result<Option<TeXBox<ET,PreShipoutNode<ET>>>,BoxInfo<ET>> {
    if engine.state.get_mode() == TeXMode::Vertical {
        todo!("throw error")
    }
    let data = engine.stomach.data_mut();
    let ls = data.get_list();
    for (i,n) in ls.iter().enumerate().rev() {
        if n.opaque() {continue }
        match n {
            PreShipoutNode::Box(_) => {
                if let PreShipoutNode::Box(bi) = ls.remove(i) {
                    return Ok(Some(bi))
                } else {
                    unreachable!()
                }
            },
            _ => return Ok(None)
        }
    }
    Ok(None)
}

pub fn lastkern<ET:EngineTypes>(engine:&mut EngineReferences<ET>,tk:ET::Token) -> ET::Dim {
    let data = engine.stomach.data_mut();
    let ls = data.get_list();

    for n in ls.iter().rev() {
        if n.opaque() {continue }
        match n {
            PreShipoutNode::Kern(k) => return k.dim(),
            _ => return ET::Dim::default()
        }
    }
    ET::Dim::default()
}

pub fn lastskip<ET:EngineTypes>(engine:&mut EngineReferences<ET>,tk:ET::Token) -> ET::Skip {
    let data = engine.stomach.data_mut();
    let ls = data.get_list();
    for n in ls.iter().rev() {
        if n.opaque() {continue }
        match n {
            PreShipoutNode::Skip(k) => return k.skip(),
            _ => return ET::Skip::default()
        }
    }
    ET::Skip::default()
}

pub fn lastpenalty<ET:EngineTypes>(engine:&mut EngineReferences<ET>,tk:ET::Token) -> ET::Int {
    let data = engine.stomach.data_mut();
    let ls = data.get_list();

    for n in ls.iter().rev() {
        if n.opaque() {continue }
        match n {
            PreShipoutNode::Penalty(k) => return (*k).into(),
            _ => return ET::Int::default()
        }
    }
    ET::Int::default()
}

pub fn vsplit<ET:EngineTypes>(engine:&mut EngineReferences<ET>, tk:ET::Token) -> Result<Option<TeXBox<ET,PreShipoutNode<ET>>>,BoxInfo<ET>> {
    let idx = read_register(engine);
    let (kind,ls,wd,start,end) = match engine.state.get_box_register_mut(idx) {
        Some(TeXBox{info:BoxInfo{tp:BoxType::Vertical,kind,assigned_width,assigned_depth,assigned_height,..},children,start,end}) => {
            *assigned_depth = None;
            *assigned_height = None;
            (*kind, std::mem::take(children), *assigned_width,*start,*end)
        }
        _ => todo!("throw error")
    };
    if !engine.read_keyword(b"to") {
        todo!("throw error")
    }
    let target = engine.read_dim(false);
    let mut ret = TeXBox{
        children:vec!(),
        info:BoxInfo {
            tp:BoxType::Vertical,
            kind,
            scaled:ToOrSpread::To(target),
            assigned_width:wd,
            assigned_height: None,
            assigned_depth: None,
            moved_left:None,raised:None
        },
        start, end
    };
    let SplitResult {first,rest,..} = ET::Stomach::split_vertical(engine,ls,target);
    ret.children = first;
    if let Some(TeXBox{children,..}) = engine.state.get_box_register_mut(idx) {
        *children = rest;
    } else {
        unreachable!()
    }
    Ok(Some(ret))
}

#[inline(always)]
pub fn year<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) -> Int<ET> {
    Int::<ET>::from(engine.aux.start_time.year())
}

#[inline(always)]
pub fn month<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) -> Int<ET> {
    Int::<ET>::from(engine.aux.start_time.month() as i32)
}

#[inline(always)]
pub fn day<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) -> Int<ET> {
    Int::<ET>::from(engine.aux.start_time.day() as i32)
}

#[inline(always)]
pub fn inputlineno<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) -> Int<ET> {
    Int::<ET>::from(engine.mouth.line_number() as i32)
}

pub fn mark<ET:EngineTypes>(engine:&mut EngineReferences<ET>,tk:ET::Token) {
    do_marks(engine,0)
}
pub fn do_marks<ET:EngineTypes>(engine:&mut EngineReferences<ET>,idx:usize) {
    let mut v = shared_vector::Vector::new();
    engine.expand_until_bgroup(false);
    engine.expand_until_endgroup(true,true,|_,_,_,t| v.push(t));
    let data = engine.stomach.data_mut();
    for NodeList {children,tp } in data.open_lists.iter_mut().rev() {
        match tp {
            NodeListType::Box(BoxInfo {tp:BoxType::Horizontal|BoxType::InlineMath|BoxType::DisplayMath,..},_,_) => (),
            _ => {
                children.push(PreShipoutNode::Mark(idx, v.into()));
                return
            }
        }
    }
    data.page.push(PreShipoutNode::Mark(idx, v.into()));
}

pub fn get_marks<ET:EngineTypes>(engine:&mut EngineReferences<ET>,exp:&mut Vec<ET::Token>,f:fn(&mut StomachData<ET>) -> &mut HMap<usize,TokenList<ET::Token>>,idx:usize) {
    match f(engine.stomach.data_mut()).get(&idx) {
        Some(v) => exp.extend(v.0.iter().cloned()),
        _ => ()
    }
}

#[inline(always)]
pub fn topmark<ET:EngineTypes>(engine:&mut EngineReferences<ET>,exp:&mut Vec<ET::Token>,token:ET::Token) {
    get_marks(engine,exp,|d| &mut d.topmarks,0)
}
#[inline(always)]
pub fn firstmark<ET:EngineTypes>(engine:&mut EngineReferences<ET>,exp:&mut Vec<ET::Token>,token:ET::Token) {
    get_marks(engine,exp,|d| &mut d.firstmarks,0)
}
#[inline(always)]
pub fn botmark<ET:EngineTypes>(engine:&mut EngineReferences<ET>,exp:&mut Vec<ET::Token>,token:ET::Token) {
    get_marks(engine,exp,|d| &mut d.botmarks,0)
}
#[inline(always)]
pub fn splitfirstmark<ET:EngineTypes>(engine:&mut EngineReferences<ET>,exp:&mut Vec<ET::Token>,token:ET::Token) {
    get_marks(engine,exp,|d| &mut d.splitfirstmarks,0)
}
#[inline(always)]
pub fn splitbotmark<ET:EngineTypes>(engine:&mut EngineReferences<ET>,exp:&mut Vec<ET::Token>,token:ET::Token) {
    get_marks(engine,exp,|d| &mut d.splitbotmarks,0)
}

pub fn shipout<ET:EngineTypes>(engine:&mut EngineReferences<ET>,tk:ET::Token) {
    engine.stomach.data_mut().deadcycles = 0;
    match engine.read_box(false) {
        Ok(Some(bx)) => {
            if let Some(n) = bx.as_node().shipout_top(engine) {
                engine.colon.out(n)
            }
        },
        Ok(None) => (),
        Err(bi) => {
            let mut data = engine.stomach.data_mut();
            data.open_lists.push(NodeList {
                children:vec!(),
                tp:NodeListType::Box(bi,engine.mouth.start_ref(), BoxTarget::Out)
            })
        }
    }
}

const PRIMITIVE_INTS:&[&'static str] = &[
    "adjdemerits",
    "badness",
    "binoppenalty",
    "brokenpenalty",
    "clubpenalty",
    "defaulthyphenchar",
    "defaultskewchar",
    "delimiterfactor",
    "displaywidowpenalty",
    "doublehyphendemerits",
    "errorcontextlines",
    "exhyphenpenalty",
    "fam",
    "finalhyphendemerits",
    "floatingpenalty",
    "globaldefs",
    "hangafter",
    "hangindent",
    "hbadness",
    "holdinginserts",
    "hyphenpenalty",
    "interlinepenalty",
    "language",
    "lefthyphenmin",
    "linepenalty",
    "looseness",
    "mag",
    "maxdeadcycles",
    "outputpenalty",
    "pausing",
    "postdisplaypenalty",
    "predisplaypenalty",
    "relpenalty",
    "righthyphenmin",
    "pretolerance",
    "showboxbreadth",
    "showboxdepth",
    "tolerance",
    "tracingcommands",
    "tracinglostchars",
    "tracingmacros",
    "tracingonline",
    "tracingoutput",
    "tracingpages",
    "tracingparagraphs",
    "tracingrestores",
    "tracingstats",
    "uchyph",
    "vbadness",
    "widowpenalty"
];

const PRIMITIVE_DIMS:&[&'static str] = &[
    "boxmaxdepth",
    "delimitershortfall",
    "displayindent",
    "displaywidth",
    "emergencystretch",
    "hfuzz",
    "hoffset",
    "hsize",
    "lineskiplimit",
    "maxdepth",
    "mathsurround",
    "nulldelimiterspace",
    "overfullrule",
    "parindent",
    "predisplaysize",
    "scriptspace",
    "splitmaxdepth",
    "vfuzz",
    "voffset",
    "vsize"
];

const PRIMITIVE_SKIPS:&[&'static str] = &[
    "abovedisplayshortskip",
    "abovedisplayskip",
    "baselineskip",
    "belowdisplayshortskip",
    "belowdisplayskip",
    "leftskip",
    "lineskip",
    "parfillskip",
    "parskip",
    "rightskip",
    "spaceskip",
    "splittopskip",
    "tabskip",
    "topskip",
    "xspaceskip"
];

const PRIMITIVE_MUSKIPS:&[&'static str] = &[
    "thinmuskip",
    "medmuskip",
    "thickmuskip"
];

const PRIMITIVE_TOKS:&[&'static str] = &[
    "everypar",
    "everymath",
    "everydisplay",
    "everyhbox",
    "everyvbox",
    "everyjob",
    "everycr",
    "output",
    "errhelp"
];

pub fn skip_argument<ET:EngineTypes>(engine:&mut EngineReferences<ET>) {
    match engine.get_next() {
        Some(t) if t.is_begin_group() => (),
        _ => todo!("throw error")
    }
    engine.read_until_endgroup(|_,_,_| {});
}

pub fn char_space<ET:EngineTypes>(engine:&mut EngineReferences<ET>,tk:ET::Token) -> PreShipoutNode<ET> {
    PreShipoutNode::Skip(SkipNode::Space)
}
pub fn char_slash<ET:EngineTypes>(engine:&mut EngineReferences<ET>,tk:ET::Token) {
    // TODO
}
pub fn char_dash<ET:EngineTypes>(engine:&mut EngineReferences<ET>,tk:ET::Token) {
    // TODO
}

pub fn register_tex_primitives<E:TeXEngine>(engine:&mut E) {
    register_int(engine,"catcode",catcode_get,Some(catcode_set));
    register_int(engine,"hyphenchar",hyphenchar_get,Some(hyphenchar_set));
    register_int(engine,"skewchar",skewchar_get,Some(skewchar_set));
    register_int(engine,"sfcode",sfcode_get,Some(sfcode_set));
    register_int(engine,"lccode",lccode_get,Some(lccode_set));
    register_int(engine,"uccode",uccode_get,Some(uccode_set));
    register_int(engine,"mathcode",mathcode_get,Some(mathcode_set));
    register_int(engine,"delcode",delcode_get,Some(delcode_set));
    register_int(engine,"count",count_get,Some(count_set));
    register_int(engine,"endlinechar",endlinechar_get,Some(endlinechar_set));
    register_int(engine,"escapechar",escapechar_get,Some(escapechar_set));
    register_int(engine,"newlinechar",newlinechar_get,Some(newlinechar_set));
    register_int(engine,"inputlineno",inputlineno,None);
    register_int(engine,"spacefactor",spacefactor_get,Some(spacefactor_set));
    register_int(engine,"day",day,None);
    register_int(engine,"time",time,None);
    register_int(engine,"month",month,None);
    register_int(engine,"year",year,None);
    register_int(engine,"lastpenalty",lastpenalty,None);

    register_dim(engine,"fontdimen",fontdimen_get,Some(fontdimen_set));
    register_dim(engine,"dimen",dimen_get,Some(dimen_set));
    register_dim(engine,"prevdepth",prevdepth_get,Some(prevdepth_set));
    register_dim(engine,"dp",dp_get,Some(dp_set));
    register_dim(engine,"ht",ht_get,Some(ht_set));
    register_dim(engine,"wd",wd_get,Some(wd_set));
    register_dim(engine,"lastkern",lastkern,None);

    register_skip(engine,"skip",skip_get,Some(skip_set));
    register_skip(engine,"lastskip",lastskip,None);

    register_muskip(engine,"muskip",muskip_get,Some(muskip_set));

    register_font(engine,"font",font_get,Some(font_set));

    register_assignment(engine,"advance",advance);
    register_assignment(engine,"chardef",chardef);
    register_assignment(engine,"countdef",countdef);
    register_assignment(engine,"dimendef",dimendef);
    register_assignment(engine,"skipdef",skipdef);
    register_assignment(engine,"muskipdef",muskipdef);
    register_assignment(engine,"toksdef",toksdef);
    register_assignment(engine,"def",|e,cmd,g|def(e,cmd,false,false,false,g));
    register_assignment(engine,"divide",divide);
    register_assignment(engine,"edef",|e,cmd,g|edef(e,cmd,false,false,false,g));
    register_assignment(engine,"xdef",|e,cmd,g|xdef(e,cmd,false,false,false,g));
    register_assignment(engine,"gdef",|e,cmd,g|gdef(e,cmd,false,false,false,g));
    register_assignment(engine,"outer",|e,cmd,g|outer(e,cmd,false,false,false,g));
    register_assignment(engine,"long",|e,cmd,g|long(e,cmd,false,false,false,g));
    register_assignment(engine,"global",|e,cmd,g|global(e,cmd,false,false,false,g));
    register_assignment(engine,"let",let_);
    register_assignment(engine,"futurelet",futurelet);
    register_assignment(engine,"mathchardef",mathchardef);
    register_assignment(engine,"multiply",multiply);
    register_assignment(engine,"read",read);
    register_assignment(engine,"setbox",setbox);
    register_assignment(engine,"toks",toks);

    register_simple_expandable(engine,"csname",csname);
    register_simple_expandable(engine,"else",else_);
    register_simple_expandable(engine,"endinput",endinput);
    register_simple_expandable(engine,"or",or);
    register_simple_expandable(engine,"expandafter",expandafter);
    register_simple_expandable(engine,"fi",fi);
    register_simple_expandable(engine,"input",input);
    register_simple_expandable(engine,"noexpand",noexpand);

    register_expandable(engine,"jobname",jobname);
    register_expandable(engine,"meaning",meaning);
    register_expandable(engine,"number",number);
    register_expandable(engine,"romannumeral",romannumeral);
    register_expandable(engine,"string",string);
    register_expandable(engine,"the",the);
    register_expandable(engine,"topmark",topmark);
    register_expandable(engine,"firstmark",firstmark);
    register_expandable(engine,"botmark",botmark);
    register_expandable(engine,"splitfirstmark",splitfirstmark);
    register_expandable(engine,"splitbotmark",splitbotmark);

    register_conditional(engine,"if",if_);
    register_conditional(engine,"ifcase",ifcase);
    register_conditional(engine,"ifcat",ifcat);
    register_conditional(engine,"ifdim",ifdim);
    register_conditional(engine,"ifeof",ifeof);
    register_conditional(engine,"ifhbox",ifhbox);
    register_conditional(engine,"ifhmode",ifhmode);
    register_conditional(engine,"ifinner",ifinner);
    register_conditional(engine,"ifmmode",ifmmode);
    register_conditional(engine,"ifnum",ifnum);
    register_conditional(engine,"ifodd",ifodd);
    register_conditional(engine,"ifx",ifx);
    register_conditional(engine,"iftrue",iftrue);
    register_conditional(engine,"iffalse",iffalse);
    register_conditional(engine,"ifvbox",ifvbox);
    register_conditional(engine,"ifvmode",ifvmode);
    register_conditional(engine,"ifvoid",ifvoid);

    register_unexpandable(engine,"afterassignment",afterassignment);
    register_unexpandable(engine,"aftergroup",aftergroup);
    register_unexpandable(engine,"begingroup",begingroup);
    register_unexpandable(engine,"closein",closein);
    register_unexpandable(engine,"dump",|_,_|());
    register_unexpandable(engine,"endcsname",endcsname);
    register_unexpandable(engine,"endgroup",endgroup);
    register_unexpandable(engine,"errorstopmode",errorstopmode);
    register_unexpandable(engine, "hyphenation", |e,_|skip_argument(e));
    register_unexpandable(engine,"ignorespaces",ignorespaces);
    register_unexpandable(engine,"immediate",immediate);
    register_unexpandable(engine,"lowercase",lowercase);
    register_unexpandable(engine,"uppercase",uppercase);
    register_unexpandable(engine,"message",message);
    register_unexpandable(engine,"errmessage",errmessage);
    register_unexpandable(engine,"noindent",noindent);
    register_unexpandable(engine,"openin",openin);
    register_unexpandable(engine,"par",par);
    register_unexpandable(engine,"unhbox",unhbox);
    register_unexpandable(engine,"unvbox",unvbox);
    register_unexpandable(engine,"unhcopy",unhcopy);
    register_unexpandable(engine,"unvcopy",unvcopy);
    register_unexpandable(engine,"unskip",unskip);
    register_unexpandable(engine,"unkern",unkern);
    register_unexpandable(engine,"unpenalty",unpenalty);
    register_unexpandable(engine,"moveleft",moveleft);
    register_unexpandable(engine,"moveright",moveright);
    register_unexpandable(engine,"raise",raise);
    register_unexpandable(engine,"lower",lower);
    register_unexpandable(engine,"shipout",shipout);
    register_unexpandable(engine, "patterns", |e,_|skip_argument(e));
    {
        let refs = engine.get_engine_refs();
        let relax = refs.aux.memory.cs_interner_mut().new("relax");
        let nullfont = refs.aux.memory.cs_interner_mut().new("nullfont");
        refs.state.set_command(refs.aux,relax,Some(Command::Relax),true);
        refs.state.set_command(refs.aux,nullfont,Some(Command::Font(refs.fontsystem.null())),true)
    }
    register_unexpandable(engine,"mark",mark);
    register_unexpandable(engine,"/",char_slash);
    register_unexpandable(engine,"-",char_dash);
    register_unexpandable(engine,"showlists",|_,_| {}); // TODO

    register_whatsit(engine,"closeout",closeout,closeout_immediate);
    register_whatsit(engine,"openout",openout,openout_immediate);
    register_whatsit(engine,"write",write,write_immediate);

    register_box(engine,"hbox",hbox);
    register_box(engine,"vbox",vbox);
    register_box(engine,"box",box_);
    register_box(engine,"copy",copy);
    register_box(engine,"lastbox",lastbox);
    register_box(engine, "vsplit", vsplit);

    register_node(engine,"penalty",NodeCommandScope::Any,penalty);
    register_node(engine,"kern",NodeCommandScope::Any,kern);
    register_node(engine,"vrule",NodeCommandScope::SwitchesToHorizontal,vrule);
    register_node(engine,"vskip",NodeCommandScope::SwitchesToVertical,vskip);
    register_node(engine,"vfil",NodeCommandScope::SwitchesToVertical,vfil);
    register_node(engine,"vfill",NodeCommandScope::SwitchesToVertical,vfill);
    register_node(engine,"vfilneg",NodeCommandScope::SwitchesToVertical,vfilneg);
    register_node(engine,"vss",NodeCommandScope::SwitchesToVertical,vss);
    register_node(engine,"hrule",NodeCommandScope::SwitchesToVertical,hrule);
    register_node(engine,"hskip",NodeCommandScope::SwitchesToHorizontal,hskip);
    register_node(engine,"hfil",NodeCommandScope::SwitchesToHorizontal,hfil);
    register_node(engine,"hfill",NodeCommandScope::SwitchesToHorizontal,hfill);
    register_node(engine,"hfilneg",NodeCommandScope::SwitchesToHorizontal,hfilneg);
    register_node(engine,"hss",NodeCommandScope::SwitchesToHorizontal,hss);
    register_node(engine," ",NodeCommandScope::SwitchesToHorizontal,char_space);


    // TODO test to make sure this is callable from the POV of the compiler
    register_unexpandable(engine,"halign",halign);

    cmstodos!(engine,
        mathclose,mathbin,mathord,mathop,mathrel,mathopen,
        mathpunct,mathinner,mathaccent,delimiter,mathchar,
        mkern,mskip,
        indent,insert,leaders,cleaders,xleaders,left,right,valign,
        vtop,vcenter,char,
        cr,crcr,discretionary,displaylimits,end,
        mathchoice,noalign,omit,overline,
        pagegoal,pagetotal,pagestretch,pagefilstretch,pagefillstretch,pagefilllstretch,
        pagedepth,pageshrink,parshape,
        scriptfont,scriptscriptfont,span,textfont,underline,vadjust
    );

    register_primitive_int(engine,PRIMITIVE_INTS);
    register_primitive_dim(engine,PRIMITIVE_DIMS);
    register_primitive_skip(engine,PRIMITIVE_SKIPS);
    register_primitive_muskip(engine,PRIMITIVE_MUSKIPS);
    register_primitive_toks(engine,PRIMITIVE_TOKS);

    // TODOS ---------------------------------------------------------------------

    cmstodo!(engine,radical);
    cmstodo!(engine,displaystyle);
    cmstodo!(engine,textstyle);
    cmstodo!(engine,scriptstyle);
    cmstodo!(engine,scriptscriptstyle);

    cmtodos!(engine,prevgraf,deadcycles,insertpenalties,scrollmode,nonstopmode,batchmode,
        show,showbox,showthe,special,noboundary,accent,setlanguage,nonscript,
        limits,nolimits,over,atop,above,overwithdelims,atopwithdelims,abovewithdelims,eqno,
        leqno,bigskip,bye,fontname,italiccorr,medskip,smallskip
    );

/*
    register_assign!(advance,engine,(e,cmd,global) =>advance::<ET>(e,&cmd,global));
    register_unexpandable!(afterassignment,engine,None,(e,cmd) =>afterassignment::<ET>(e,&cmd));
    register_unexpandable!(aftergroup,engine,None,(e,cmd) =>aftergroup::<ET>(e,&cmd));
    register_unexpandable!(begingroup,engine,None,(e,cmd) =>begingroup::<ET>(&mut e.state));
    register_box!(box,engine,(e,cmd) =>box_::<ET>(e,&cmd));
    register_value_assign_int!(catcode,engine);
    register_unexpandable!(char,engine,Some(HorV::Horizontal),(e,cmd) =>char::<ET>(e,&cmd));
    register_assign!(chardef,engine,(e,cmd,global) =>chardef::<ET>(e,&cmd,global));
    register_unexpandable!(closein,engine,None,(e,cmd) =>closein::<ET>(e,&cmd));
    register_whatsit!(closeout,engine,(e,cmd) =>closeout::<ET>(e,&cmd));
    register_box!(copy,engine,(e,cmd) =>copy::<ET>(e,&cmd));
    register_value_assign_int!(count,engine);
    register_assign!(countdef,engine,(e,cmd,global) =>countdef::<ET>(e,&cmd,global));
    register_unexpandable!(cr,engine,None,(e,cmd) => cr::<ET>(e,&cmd));
    register_unexpandable!(crcr,engine,None,(e,cmd) => crcr::<ET>(e,&cmd));
    register_expandable_notk!(csname,engine,(e,cmd) =>csname::<ET>(e,&cmd));
    register_int!(day,engine,(e,cmd) => day::<ET>(e,&cmd));
    register_assign!(def,engine,(e,cmd,global) =>def::<ET>(e,&cmd,global,false,false,false));
    register_unexpandable!(delimiter,engine,None,(e,cmd) =>delimiter::<ET>(e,&cmd));
    register_value_assign_int!(delcode,engine);
    register_value_assign_dim!(dimen,engine);
    register_assign!(dimendef,engine,(e,cmd,global) =>dimendef::<ET>(e,&cmd,global));
    register_unexpandable!(discretionary,engine,Some(HorV::Horizontal),(e,cmd) =>discretionary::<ET>(e,&cmd));
    register_unexpandable!(displaylimits,engine,None,(e,cmd) =>displaylimits::<ET>(e,&cmd));
    register_assign!(divide,engine,(e,cmd,global) =>divide::<ET>(e,&cmd,global));
    register_value_assign_dim!(dp,engine);
    register_unexpandable!(dump,engine,None,(_,cmd) =>dump::<ET>());
    register_assign!(edef,engine,(e,cmd,global) =>edef::<ET>(e,&cmd,global,false,false,false));
    register_expandable_notk!(else,engine,(e,cmd) =>else_::<ET>(e,&cmd));
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

    register_unexpandable!(errorstopmode,engine,None,(_,cmd) =>errorstopmode::<ET>());
    register_value_assign_int!(escapechar,engine);
    register_expandable_notk!(expandafter,engine,(e,cmd) => expandafter::<ET>(e,&cmd));
    register_expandable_notk!(fi,engine,(e,cmd) =>fi::<ET>(e,&cmd));
    register_value_assign_font!(font,engine);
    register_value_assign_dim!(fontdimen,engine);
    register_assign!(futurelet,engine,(e,cmd,global) =>futurelet::<ET>(e,&cmd,global));
    register_assign!(gdef,engine,(e,cmd,global) =>gdef::<ET>(e,&cmd,global,false,false,false));
    register_assign!(global,engine,(e,cmd,g) =>global::<ET>(e,&cmd,g,false,false,false));
    register_unexpandable!(halign,engine,Some(HorV::Vertical),(e,cmd) =>halign::<ET>(e,&cmd));
    register_open_box!(hbox,engine,BoxMode::H,(e,cmd) =>hbox::<ET>(e,&cmd));
    register_unexpandable!(hfil,engine,Some(HorV::Horizontal),(e,cmd) =>hfil::<ET>(e,&cmd));
    register_unexpandable!(hfill,engine,Some(HorV::Horizontal),(e,cmd) =>hfill::<ET>(e,&cmd));
    register_unexpandable!(hfilneg,engine,Some(HorV::Horizontal),(e,cmd) =>hfilneg::<ET>(e,&cmd));
    register_unexpandable!(hss,engine,Some(HorV::Horizontal),(e,cmd) =>hss::<ET>(e,&cmd));
    register_unexpandable!(hrule,engine,Some(HorV::Vertical),(e,cmd) =>hrule::<ET>(e,&cmd));
    register_unexpandable!(hskip,engine,Some(HorV::Horizontal),(e,cmd) =>hskip::<ET>(e,&cmd));
    register_value_assign_dim!(ht,engine);
    register_unexpandable!(hyphenation,engine,None,(e,cmd) =>hyphenation::<ET>(e,&cmd));
    register_value_assign_int!(hyphenchar,engine);
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
    register_expandable!(jobname,engine,(e,c,f) =>jobname::<ET>(e,&c,f));
    register_unexpandable!(kern,engine,None,(e,cmd) =>kern::<ET>(e,&cmd));
    register_box!(lastbox,engine,(e,cmd) =>lastbox::<ET>(e,&cmd));
    register_dim!(lastkern,engine,(e,cmd) => lastkern::<ET>(e,&cmd));
    register_skip!(lastskip,engine,(e,cmd) => lastskip::<ET>(e,&cmd));
    register_int!(lastpenalty,engine,(e,cmd) => lastpenalty::<ET>(e,&cmd));
    register_value_assign_int!(lccode,engine);
    register_unexpandable!(leaders,engine,None,(e,cmd) =>leaders::<ET>(e,&cmd));
    register_unexpandable!(cleaders,engine,None,(e,cmd) =>cleaders::<ET>(e,&cmd));
    register_unexpandable!(xleaders,engine,None,(e,cmd) =>xleaders::<ET>(e,&cmd));
    register_unexpandable!(left,engine,None,(e,cmd) =>left::<ET>(e,&cmd));
    register_unexpandable!(right,engine,None,(e,cmd) =>right::<ET>(e,&cmd));
    register_assign!(let,engine,(e,cmd,global) =>let_::<ET>(e,&cmd,global));
    register_assign!(long,engine,(e,cmd,g) =>long::<ET>(e,&cmd,g,false,false,false));
    register_unexpandable!(lower,engine,Some(HorV::Horizontal),(e,cmd) =>lower::<ET>(e,&cmd));
    register_unexpandable!(lowercase,engine,None,(e,cmd) =>lowercase::<ET>(e,&cmd));
    register_unexpandable!(mark,engine,None,(e,cmd) =>mark::<ET>(e,&cmd));
    register_expandable!(topmark,engine,(e,c,f) =>topmark::<ET>(e,&c,f));
    register_expandable!(firstmark,engine,(e,c,f) =>firstmark::<ET>(e,&c,f));
    register_expandable!(botmark,engine,(e,c,f) =>botmark::<ET>(e,&c,f));
    register_expandable!(splitfirstmark,engine,(e,c,f) =>splitfirstmark::<ET>(e,&c,f));
    register_expandable!(splitbotmark,engine,(e,c,f) =>splitbotmark::<ET>(e,&c,f));
    register_unexpandable!(mathaccent,engine,None,(e,cmd) =>mathaccent::<ET>(e,&cmd));
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
    engine.state.set_command(ET::Char::from_str("nullfont",&mut engine.interner), Some(Command::new(
        BaseCommand::Font(ET::FontRef::default())
        ,None)), true);
    register_expandable!(number,engine,(e,cmd,f) => number::<ET>(e,&cmd,f));
    register_unexpandable!(omit,engine,None,(e,cmd) =>omit::<ET>(e,&cmd));
    register_unexpandable!(openin,engine,None,(e,cmd) =>openin::<ET>(e,&cmd));
    register_whatsit!(openout,engine,(e,cmd) =>openout::<ET>(e,&cmd));
    register_expandable_notk!(or,engine,(e,cmd) => or::<ET>(e,&cmd));
    register_assign!(outer,engine,(e,cmd,g) =>outer::<ET>(e,&cmd,g,false,false,false));
    register_unexpandable!(overline,engine,None,(e,cmd) =>overline::<ET>(e,&cmd));
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
    register_value_assign_int!(parshape,engine);
    register_unexpandable!(patterns,engine,None,(e,cmd) =>patterns::<ET>(e,&cmd));
    register_unexpandable!(penalty,engine,None,(e,cmd) =>penalty::<ET>(e,&cmd));
    register_value_assign_dim!(prevdepth,engine);
    register_unexpandable!(raise,engine,Some(HorV::Horizontal),(e,cmd) =>raise::<ET>(e,&cmd));
    register_assign!(read,engine,(e,cmd,global) =>read::<ET>(e,&cmd,global));
    engine.state.set_command(engine.interner.relax, Some(Command::new(BaseCommand::Relax,None)), true);
    register_expandable!(romannumeral,engine,(e,cmd,f) => romannumeral::<ET>(e,&cmd,f));
    register_value_assign_font!(scriptfont,engine);
    register_value_assign_font!(scriptscriptfont,engine);
    register_assign!(setbox,engine,(e,cmd,global) =>setbox::<ET>(e,&cmd,global));
    register_value_assign_int!(sfcode,engine);
    register_unexpandable!(shipout,engine,None,(e,cmd) =>shipout::<ET>(e,&cmd));
    register_value_assign_int!(skewchar,engine);
    register_value_assign_skip!(skip,engine);
    register_assign!(skipdef,engine,(e,cmd,global) =>skipdef::<ET>(e,&cmd,global));
    register_value_assign_int!(spacefactor,engine);
    register_unexpandable!(span,engine,None,(e,cmd) => span::<ET>(e,&cmd));
    register_expandable!(string,engine,(e,cmd,f) => string::<ET>(e,&cmd,f));
    register_value_assign_font!(textfont,engine);
    register_expandable!(the,engine,(e,cmd,f) => the::<ET>(e,&cmd,f));
    register_int!(time,engine,(e,cmd) => time::<ET>(e,&cmd));
    register_value_assign_toks!(toks,engine);
    register_assign!(toksdef,engine,(e,cmd,global) =>toksdef::<ET>(e,&cmd,global));
    register_value_assign_int!(uccode,engine);
    register_unexpandable!(underline,engine,None,(e,cmd) =>underline::<ET>(e,&cmd));
    register_unexpandable!(unhbox,engine,Some(HorV::Horizontal),(e,cmd) =>unhbox::<ET>(e,&cmd));
    register_unexpandable!(unhcopy,engine,Some(HorV::Horizontal),(e,cmd) =>unhcopy::<ET>(e,&cmd));
    register_unexpandable!(unvbox,engine,Some(HorV::Vertical),(e,cmd) =>unvbox::<ET>(e,&cmd));
    register_unexpandable!(unvcopy,engine,Some(HorV::Vertical),(e,cmd) =>unvcopy::<ET>(e,&cmd));
    register_unexpandable!(unskip,engine,None,(e,cmd) =>unskip::<ET>(e,&cmd));
    register_unexpandable!(unkern,engine,None,(e,cmd) =>unkern::<ET>(e,&cmd));
    register_unexpandable!(unpenalty,engine,None,(e,cmd) =>unpenalty::<ET>(e,&cmd));
    register_unexpandable!(uppercase,engine,None,(e,cmd) =>uppercase::<ET>(e,&cmd));
    register_open_box!(vadjust,engine,BoxMode::V,(e,cmd) =>vadjust::<ET>(e,&cmd));
    register_unexpandable!(valign,engine,Some(HorV::Vertical),(e,cmd) =>valign::<ET>(e,&cmd));
    register_open_box!(vbox,engine,BoxMode::V,(e,cmd) =>vbox::<ET>(e,&cmd));
    register_open_box!(vcenter,engine,BoxMode::V,(e,cmd) =>vcenter::<ET>(e,&cmd));
    register_open_box!(vtop,engine,BoxMode::V,(e,cmd) =>vtop::<ET>(e,&cmd));
    register_unexpandable!(vfil,engine,Some(HorV::Vertical),(e,cmd) =>vfil::<ET>(e,&cmd));
    register_unexpandable!(vfill,engine,Some(HorV::Vertical),(e,cmd) =>vfill::<ET>(e,&cmd));
    register_unexpandable!(vfilneg,engine,Some(HorV::Vertical),(e,cmd) =>vfilneg::<ET>(e,&cmd));
    register_unexpandable!(vskip,engine,Some(HorV::Vertical),(e,cmd) =>vskip::<ET>(e,&cmd));
    register_box!(vsplit,engine,(e,cmd) =>vsplit::<ET>(e,&cmd));
    register_unexpandable!(vss,engine,Some(HorV::Vertical),(e,cmd) =>vss::<ET>(e,&cmd));
    register_unexpandable!(vrule,engine,Some(HorV::Horizontal),(e,cmd) =>vrule::<ET>(e,&cmd));
    register_value_assign_dim!(wd,engine);
    register_whatsit!(write,engine,(e,cmd) =>write::<ET>(e,&cmd));
    register_assign!(xdef,engine,(e,cmd,global) =>xdef::<ET>(e,&cmd,global,false,false,false));
    register_skip_assign!(xspaceskip,engine);
    register_int!(year,engine,(e,cmd) => year::<ET>(e,&cmd));

    engine.state.set_command(ET::Char::from_str(" ",&mut engine.interner),Some(
        Command::new(BaseCommand::Unexpandable {
            name:" ",
            apply:|e,cmd| SPACE::<ET>(e,&cmd),
            forces_mode:Some(HorV::Horizontal)
        },None)),true);


 */
}