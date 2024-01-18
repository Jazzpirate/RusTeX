use chrono::{Datelike, Timelike};
use crate::{add_node, expand_loop};
use crate::commands::{TeXCommand, Macro, MacroSignature, CommandScope, ActiveConditional, ResolvedToken, PrimitiveCommand, CharOrPrimitive};
use crate::engine::{EngineReferences, EngineTypes, TeXEngine};
use crate::engine::filesystem::{File, FileSystem};
use crate::engine::gullet::Gullet;
use crate::engine::gullet::methods::CSOrActiveChar;
use crate::engine::mouth::Mouth;
use crate::tex::tokens::token_lists::{Otherize, TokenList};
use super::primitives::*;
use crate::engine::state::{GroupType, State};
use crate::tex::catcodes::{CategoryCode, CommandCode};
use crate::tex::numerics::{MuSkip, Numeric, NumSet, Skip};
use crate::tex::characters::{Character, CharacterMap};
use crate::engine::utils::outputs::Outputs;
use crate::tex::tokens::{StandardToken, Token};
use crate::engine::stomach::{Stomach, TeXMode};
use std::fmt::Write;
use crate::commands::methods::{END_TEMPLATE, END_TEMPLATE_ROW, IfxCmd, MacroParser};
use crate::engine::fontsystem::FontSystem;
use crate::tex::tokens::control_sequences::{CSHandler, ResolvedCSName};
use crate::engine::fontsystem::Font;
use crate::engine::stomach::methods::SplitResult;
use crate::tex::nodes::boxes::{BoxInfo, BoxType, HBoxInfo, TeXBox, ToOrSpread, VBoxInfo};
use crate::tex::nodes::{BoxTarget, LeaderType, ListTarget, NodeList, NodeTrait};
use crate::tex::nodes::horizontal::{HNode, HorizontalNodeListType};
use crate::tex::nodes::math::{Delimiter, EqNoPosition, MathAtom, MathChar, MathClass, MathKernel, MathNode, MathNodeList, MathNodeListType, MathNucleus, UnresolvedMarkers, UnresolvedMathChoice, UnresolvedMathFontStyle};
use crate::tex::nodes::vertical::{VerticalNodeListType, VNode};
use crate::tex::numerics::TeXDimen;
use crate::tex::tokens::token_lists::CharWrite;
use crate::tex_error;

type Int<E> = <<E as EngineTypes>::Num as NumSet>::Int;
type Dim<E> = <<E as EngineTypes>::Num as NumSet>::Dim;
type Fnt<E> = <<E as EngineTypes>::FontSystem as FontSystem>::Font;


pub fn afterassignment<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) {
    let next = match engine.get_next() {
        Some(t) => t,
        None => todo!("file end")
    };
    *engine.stomach.afterassignment() = Some(next);
}

pub fn aftergroup<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) {
    let next = match engine.get_next() {
        Some(t) => t,
        None => todo!("file end")
    };
    engine.state.aftergroup(next)
}

pub fn begingroup<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) {
    engine.state.push(engine.aux,GroupType::ControlSequence,engine.mouth.line_number())
}
pub fn endgroup<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) {
    match engine.state.get_group_type() {
        Some(GroupType::ControlSequence) => (),
        _ => todo!("throw error")
    }
    engine.state.pop(engine.aux,engine.mouth);
}

pub fn end<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) {
    ET::Stomach::flush(engine);
    engine.mouth.finish();
}

pub fn discretionary<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) {
    engine.skip_argument();
    engine.skip_argument();
    engine.skip_argument();
    // TODO
}


pub fn endinput<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) {
    engine.mouth.endinput(engine.aux);
}


pub fn errorstopmode<ET:EngineTypes>(_engine: &mut EngineReferences<ET>,_tk:ET::Token) {}

pub fn expandafter<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) {
    let first = match engine.get_next() {
        None => todo!("throw error"),
        Some(t) => t
    };
    let second = match engine.get_next() {
        None => todo!("throw error"),
        Some(t) => t
    };
    engine.expand(second);
    engine.requeue(first)
}

pub fn catcode_get<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) -> Int<ET> {
    let char = engine.read_charcode(false);
    let u : u8 = (*engine.state.get_catcode_scheme().get(char)).into();
    Int::<ET>::from(u as i32)
}
pub fn catcode_set<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token,globally:bool) {
    let char = engine.read_charcode(false);
    let val: i64 = engine.read_int(true).into();
    if val < 0 || val > 15 {
        todo!("catcode out of range")
    }
    let cc: CategoryCode = (val as u8).try_into().unwrap();
    engine.state.set_catcode(engine.aux,char,cc,globally)
}

pub fn sfcode_get<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) -> Int<ET> {
    let char = engine.read_charcode(false);
    let u : u16 = engine.state.get_sfcode(char);
    Int::<ET>::from(u as i32)
}
pub fn sfcode_set<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token,globally:bool) {
    let char = engine.read_charcode(false);
    let val: i64 = engine.read_int(true).into();
    if val < 0 || val > 32767 {
        todo!("sfcode out of range")
    }
    let sf = val as u16;
    engine.state.set_sfcode(engine.aux,char,sf,globally)
}


pub fn spacefactor_get<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) -> Int<ET> {
    Int::<ET>::from(engine.stomach.data_mut().spacefactor)
}
pub fn spacefactor_set<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token,_globally:bool) {
    let val = match engine.read_int(true).try_into() {
        Ok(v) => v,
        _ => todo!("throw error")
    };
    engine.stomach.data_mut().spacefactor = val;
}



pub fn parshape_get<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) -> Int<ET> {
    Int::<ET>::from(engine.state.get_parshape().len() as i32)
}
pub fn parshape_set<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token,globally:bool) {
    let len = engine.read_int(false).into();
    if len < 0 {
        todo!("throw error")
    }
    let mut shape = Vec::with_capacity(len as usize);
    for _ in 0..len {
        let a = engine.read_dim(false);
        let b = engine.read_dim(false);
        shape.push((a,b))
    }
    engine.state.set_parshape(engine.aux,shape,globally);
}

pub fn lccode_get<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) -> Int<ET> {
    let char = engine.read_charcode(false);
    let u = engine.state.get_lccode(char).into();
    match Int::<ET>::try_from(u as i64) {
        Ok(v) => v,
        _ => todo!("throw error")
    }
}
pub fn lccode_set<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token,globally:bool) {
    let char = engine.read_charcode(false);
    let val = engine.read_charcode(true);
    engine.state.set_lccode(engine.aux,char,val,globally)
}

pub fn uccode_get<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) -> Int<ET> {
    let char = engine.read_charcode(false);
    let u = engine.state.get_uccode(char).into();
    match Int::<ET>::try_from(u as i64) {
        Ok(v) => v,
        _ => todo!("throw error")
    }
}
pub fn uccode_set<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token,globally:bool) {
    let char = engine.read_charcode(false);
    let val = engine.read_charcode(true);
    engine.state.set_uccode(engine.aux,char,val,globally)
}

pub fn mathcode_get<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) -> Int<ET> {
    let char = engine.read_charcode(false);
    let u = engine.state.get_mathcode(char);
    match Int::<ET>::try_from(u as i64) {
        Ok(v) => v,
        _ => todo!("throw error")
    }
}
pub fn mathcode_set<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token,globally:bool) {
    let char = engine.read_charcode(false);
    let val = engine.read_int(true).into();
    if val < 0 || val > u32::MAX.into() {
        todo!("throw error")
    }
    engine.state.set_mathcode(engine.aux,char,val as u32,globally)
}

pub fn delcode_get<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) -> Int<ET> {
    let char = engine.read_charcode(false);
    engine.state.get_delcode(char)
}
pub fn delcode_set<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token,globally:bool) {
    let char = engine.read_charcode(false);
    let val = engine.read_int(true);
    engine.state.set_delcode(engine.aux,char,val,globally)
}

pub fn chardef<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token,globally:bool) {
    let name = engine.read_control_sequence();
    let char = engine.read_charcode(true);
    let cmd = TeXCommand::CharDef(char);
    engine.set_command(&name,Some(cmd),globally)
}

pub fn char_<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) {
    let char = engine.read_charcode(false);
    match engine.stomach.data_mut().mode() {
        TeXMode::DisplayMath | TeXMode::InlineMath => {
            let font = engine.state.get_current_font().clone();
            ET::Stomach::add_node_m(engine,MathNode::Atom(MathAtom {
                sup:None,sub:None,nucleus:MathNucleus::Simple {
                    cls:MathClass::Ord,
                    limits:None,
                    kernel:MathKernel::Char {
                        char,
                        style:UnresolvedMathFontStyle {
                            text_font:font.clone(),
                            script_font:font.clone(),
                            script_script_font:font
                        }
                    }
                }
            }))
        }
        _ => {
            let tk = <ET::Token as Token>::from_char_cat(char,CommandCode::Other);
            ET::Stomach::do_char(engine,tk,char,CommandCode::Other)
        }
    }
}

pub fn csname<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token) {
    let name = engine.read_csname();
    if engine.state.get_command(&name).is_none() {
        engine.state.set_command(engine.aux, name.clone(), Some(TeXCommand::Primitive {name:PRIMITIVES.relax,cmd:PrimitiveCommand::Relax}), false)
    }
    engine.mouth.requeue(ET::Token::from_cs(name))
}

pub fn endcsname<ET:EngineTypes>(_engine:&mut EngineReferences<ET>,_tk:ET::Token) {
    todo!("throw error")
}


pub fn count_get<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) -> Int<ET> {
    let idx = engine.read_register_index(false);
    engine.state.get_int_register(idx)
}
pub fn count_set<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token,globally:bool) {
    let idx = engine.read_register_index(false);
    let val = engine.read_int(true);
    engine.state.set_int_register(engine.aux,idx,val,globally)
}

pub fn dimen_get<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) -> Dim<ET> {
    let idx = engine.read_register_index(false);
    engine.state.get_dim_register(idx)
}
pub fn dimen_set<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token,globally:bool) {
    let idx = engine.read_register_index(false);
    let val = engine.read_dim(true);
    engine.state.set_dim_register(engine.aux,idx,val,globally)
}

pub fn skip_get<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) -> Skip<ET::Dim> {
    let idx = engine.read_register_index(false);
    engine.state.get_skip_register(idx)
}
pub fn skip_set<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token,globally:bool) {
    let idx = engine.read_register_index(false);
    let val = engine.read_skip(true);
    engine.state.set_skip_register(engine.aux,idx,val,globally)
}

pub fn muskip_get<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) -> MuSkip<ET::MuDim> {
    let idx = engine.read_register_index(false);
    engine.state.get_muskip_register(idx)
}
pub fn muskip_set<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token,globally:bool) {
    let idx = engine.read_register_index(false);
    let val = engine.read_muskip(true);
    engine.state.set_muskip_register(engine.aux,idx,val,globally)
}

pub fn countdef<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token,globally:bool) {
    let name = engine.read_control_sequence();
    let i = engine.read_register_index(true);
    let cmd = TeXCommand::IntRegister(i);
    engine.set_command(&name,Some(cmd),globally)
}

pub fn dimendef<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token,globally:bool) {
    let name = engine.read_control_sequence();
    let i = engine.read_register_index(true);
    let cmd = TeXCommand::DimRegister(i);
    engine.set_command(&name,Some(cmd),globally)
}

pub fn skipdef<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token,globally:bool) {
    let name = engine.read_control_sequence();
    let i = engine.read_register_index(true);
    let cmd = TeXCommand::SkipRegister(i);
    engine.set_command(&name,Some(cmd),globally)
}

pub fn muskipdef<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token,globally:bool) {
    let name = engine.read_control_sequence();
    let i = engine.read_register_index(true);
    let cmd = TeXCommand::MuSkipRegister(i);
    engine.set_command(&name,Some(cmd),globally)
}

pub fn toksdef<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token,globally:bool) {
    let name = engine.read_control_sequence();
    let i = engine.read_register_index(true);
    let cmd = TeXCommand::ToksRegister(i);
    engine.set_command(&name,Some(cmd),globally)
}

pub fn def<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token,outer:bool,long:bool,protected:bool,globally:bool) {
    let cm = match engine.get_next() {
        Some(t) => match t.to_enum() {
            StandardToken::Character(c,CommandCode::Active) =>
                CSOrActiveChar::Active(c),
            StandardToken::ControlSequence(cs) => CSOrActiveChar::Name(cs),
            _ => todo!("throw error")
        }
        None => todo!("file end error")
    };
    let mut parser = MacroParser::new();
    engine.iterate(|_,_,t| parser.do_signature_token(t));
    engine.read_until_endgroup(|_,_,t| parser.do_expansion_token(t));
    let cmd = parser.close(long,outer,protected);
    engine.set_command(&cm, Some(TeXCommand::Macro(cmd)), globally)
}

pub fn edef<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token,outer:bool,long:bool,protected:bool,globally:bool) {
    let cm = match engine.get_next() {
        Some(t) => match t.to_enum() {
            StandardToken::Character(c,CommandCode::Active) =>
                CSOrActiveChar::Active(c),
            StandardToken::ControlSequence(cs) => CSOrActiveChar::Name(cs),
            _ => todo!("throw error")
        }
        None => todo!("file end error")
    };

    let mut parser = MacroParser::new();
    engine.iterate(|_,_,t| parser.do_signature_token(t));
    engine.expand_until_endgroup(false,true,|_,_,t| parser.do_expansion_token(t));
    let cmd = parser.close(long,outer,protected);
    engine.set_command(&cm, Some(TeXCommand::Macro(cmd)), globally)
}


pub fn xdef<ET:EngineTypes>(engine:&mut EngineReferences<ET>,tk:ET::Token,outer:bool,long:bool,protected:bool,_globally:bool) {
    edef(engine,tk,outer,long,protected,true)
}


pub fn gdef<ET:EngineTypes>(engine:&mut EngineReferences<ET>,tk:ET::Token,outer:bool,long:bool,protected:bool,_globally:bool) {
    def(engine,tk,outer,long,protected,true)
}

pub fn dp_get<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) -> Dim<ET> {
    let idx = engine.read_register_index(false);
    match engine.state.get_box_register(idx) {
        None => ET::Dim::default(),
        Some(b) => b.depth()
    }
}
pub fn dp_set<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token,_globally:bool) {
    let idx = engine.read_register_index(false);
    let dim = engine.read_dim(true);
    if let Some(b) = engine.state.get_box_register_mut(idx) {
        b.assign_depth(dim)
    }
}

pub fn ht_get<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) -> Dim<ET> {
    let idx = engine.read_register_index(false);
    match engine.state.get_box_register(idx) {
        None => ET::Dim::default(),
        Some(b) => b.height()
    }
}
pub fn ht_set<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token,_globally:bool) {
    let idx = engine.read_register_index(false);
    let dim = engine.read_dim(true);
    if let Some(b) = engine.state.get_box_register_mut(idx) {
        b.assign_height(dim)
    }
}

pub fn wd_get<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) -> Dim<ET> {
    let idx = engine.read_register_index(false);
    match engine.state.get_box_register(idx) {
        None => ET::Dim::default(),
        Some(b) => b.width()
    }
}
pub fn wd_set<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token,_globally:bool) {
    let idx = engine.read_register_index(false);
    let dim = engine.read_dim(true);
    if let Some(b) = engine.state.get_box_register_mut(idx) {
        b.assign_width(dim)
    }
}

pub fn global<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token,outer:bool,long:bool,protected:bool,_globally:bool) {
    let allow_others = !outer && !long && !protected;
    crate::expand_loop!(engine,token,
        ResolvedToken::Cmd(Some(TeXCommand::Primitive{name,cmd:PrimitiveCommand::Assignment(a)})) => match *name {
            n if n == PRIMITIVES.outer => return self::outer(engine,token,outer,long,protected,true),
            n if n == PRIMITIVES.long => return self::long(engine,token,outer,long,protected,true),
            n if n == PRIMITIVES.protected => return super::etex::protected(engine,token,outer,long,protected,true),
            n if n == PRIMITIVES.global => return self::global(engine,token,outer,long,protected,true),
            n if n == PRIMITIVES.def => return self::def(engine,token,outer,long,protected,true),
            n if n == PRIMITIVES.edef => return self::edef(engine,token,outer,long,protected,true),
            n if n == PRIMITIVES.xdef => return self::xdef(engine,token,outer,long,protected,true),
            n if n == PRIMITIVES.gdef => return self::gdef(engine,token,outer,long,protected,true),
            n if allow_others => return ET::Stomach::do_assignment(engine,n,token,*a,true),
            _ => todo!("throw error")
        }
        ResolvedToken::Cmd(Some(TeXCommand::IntRegister(u))) if allow_others =>
            return ET::Stomach::assign_int_register(engine,*u,true),
        ResolvedToken::Cmd(Some(TeXCommand::Primitive{name,cmd:PrimitiveCommand::PrimitiveInt})) if allow_others =>
            return ET::Stomach::assign_primitive_int(engine,*name,true),
        ResolvedToken::Cmd(Some(TeXCommand::Primitive{cmd:PrimitiveCommand::Int {assign:Some(f),..},name})) if allow_others =>
            return ET::Stomach::do_assignment(engine,*name,token,*f,true),
        ResolvedToken::Cmd(Some(TeXCommand::DimRegister(u))) if allow_others =>
            return ET::Stomach::assign_dim_register(engine,*u,true),
        ResolvedToken::Cmd(Some(TeXCommand::Primitive{name,cmd:PrimitiveCommand::PrimitiveDim})) if allow_others =>
            return ET::Stomach::assign_primitive_dim(engine,*name,true),
        ResolvedToken::Cmd(Some(TeXCommand::Primitive{cmd:PrimitiveCommand::Dim {assign:Some(f),..},name})) if allow_others =>
            return ET::Stomach::do_assignment(engine,*name,token,*f,true),
        ResolvedToken::Cmd(Some(TeXCommand::SkipRegister(u))) if allow_others =>
            return ET::Stomach::assign_skip_register(engine,*u,true),
        ResolvedToken::Cmd(Some(TeXCommand::Primitive{name,cmd:PrimitiveCommand::PrimitiveSkip})) if allow_others =>
            return ET::Stomach::assign_primitive_skip(engine,*name,true),
        ResolvedToken::Cmd(Some(TeXCommand::Primitive{cmd:PrimitiveCommand::Skip {assign:Some(f),..},name})) if allow_others =>
            return ET::Stomach::do_assignment(engine,*name,token,*f,true),
        ResolvedToken::Cmd(Some(TeXCommand::MuSkipRegister(u))) if allow_others =>
            return ET::Stomach::assign_muskip_register(engine,*u,true),
        ResolvedToken::Cmd(Some(TeXCommand::Primitive{name,cmd:PrimitiveCommand::PrimitiveMuSkip})) if allow_others =>
            return ET::Stomach::assign_primitive_muskip(engine,*name,true),
        ResolvedToken::Cmd(Some(TeXCommand::Primitive{cmd:PrimitiveCommand::MuSkip {assign:Some(f),..},name})) if allow_others =>
            return ET::Stomach::do_assignment(engine,*name,token,*f,true),
        ResolvedToken::Cmd(Some(TeXCommand::ToksRegister(u))) if allow_others =>
            return ET::Stomach::assign_toks_register(engine,*u,true),
        ResolvedToken::Cmd(Some(TeXCommand::Primitive{name,cmd:PrimitiveCommand::PrimitiveToks})) if allow_others =>
            return ET::Stomach::assign_primitive_toks(engine,*name,true),
        ResolvedToken::Cmd(Some(TeXCommand::Primitive{cmd:PrimitiveCommand::FontCmd {assign:Some(f),..},name})) if allow_others =>
            return ET::Stomach::do_assignment(engine,*name,token,*f,true),
        o => todo!("\\global {:?}",o)
    )
}
pub fn outer<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token,_outer:bool,long:bool,protected:bool,globally:bool) {
    crate::expand_loop!(engine,token,
        ResolvedToken::Cmd(Some(TeXCommand::Primitive{name,..})) => match *name {
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

pub fn long<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token,outer:bool,_long:bool,protected:bool,globally:bool) {
    crate::expand_loop!(engine,token,
        ResolvedToken::Cmd(Some(TeXCommand::Primitive{name,..})) => match *name {
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

macro_rules! modify_num {
    ($engine:ident,$globally:ident,$int:expr,$dim:expr,$skip:expr) => {
        crate::expand_loop!($engine,token,
            ResolvedToken::Cmd(Some(cm)) => match cm {
                TeXCommand::Primitive{name,..} if *name == PRIMITIVES.count => {
                    let idx = $engine.read_register_index(false);
                    return crate::commands::methods::modify_int_register($engine,idx,$globally,$int)
                }
                TeXCommand::IntRegister(idx) => {
                    return crate::commands::methods::modify_int_register($engine,*idx,$globally,$int)
                }
                TeXCommand::Primitive{name,cmd:PrimitiveCommand::PrimitiveInt} => {
                    return crate::commands::methods::modify_primitive_int($engine,*name,$globally,$int)
                }
                TeXCommand::Primitive{name,..} if *name == PRIMITIVES.dimen => {
                    let idx = $engine.read_register_index(false);
                    return crate::commands::methods::modify_dim_register($engine,idx,$globally,$dim)
                }
                TeXCommand::DimRegister(idx) => {
                    return crate::commands::methods::modify_dim_register($engine,*idx,$globally,$dim)
                }
                TeXCommand::Primitive{name,cmd:PrimitiveCommand::PrimitiveDim} => {
                    return crate::commands::methods::modify_primitive_dim($engine,*name,$globally,$dim)
                }
                TeXCommand::Primitive{name,..} if *name == PRIMITIVES.skip => {
                    let idx = $engine.read_register_index(false);
                    return crate::commands::methods::modify_skip_register($engine,idx,$globally,$skip)
                }
                TeXCommand::SkipRegister(idx) => {
                    return crate::commands::methods::modify_skip_register($engine,*idx,$globally,$skip)
                }
                TeXCommand::Primitive{name,cmd:PrimitiveCommand::PrimitiveSkip} => {
                    return crate::commands::methods::modify_primitive_skip($engine,*name,$globally,$skip)
                }
                o => todo!("{:?} in \\advance",o)
            }
            _ => todo!("throw error")
        );
        todo!("file end")
    };
}

pub fn advance<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token,globally:bool) {
    modify_num!(engine,globally,
        |a,e| a + e.read_int(false),
        |a,e| a + e.read_dim(false),
        |a,e| a + e.read_skip(false)
    );
}
pub fn divide<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token,globally:bool) {
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
pub fn multiply<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token,globally:bool) {
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

pub fn r#else<ET:EngineTypes>(engine: &mut EngineReferences<ET>, tk:ET::Token) {
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
                         <ET::Char as Character>::display_opt(engine.state.get_escape_char()),
                         name.display(engine.state.get_escape_char()), index, engine.mouth.line_number()));
    }
    crate::engine::gullet::methods::false_loop(engine,index,false,false);
    if trace {
        engine.aux.outputs.write_neg1(
            format_args!("{{{}fi: {} (level {}) entered on line {}}}",
                         <ET::Char as Character>::display_opt(engine.state.get_escape_char()),
                         name.display(engine.state.get_escape_char()),
                         index, engine.mouth.line_number()));
    }
}

pub fn or<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) {
    let conds = engine.gullet.get_conditionals();
    match conds.pop() {
        Some(ActiveConditional::Case(_)) => {
            conds.push(ActiveConditional::Else(PRIMITIVES.ifcase));
        }
        _ => todo!()
    };
    let trace = engine.state.get_primitive_int(PRIMITIVES.tracingifs) > Int::<ET>::default();
    let index = conds.len();
    if trace {
        engine.aux.outputs.write_neg1(
            format_args!("{{{}or: {}ifcase (level {}) entered on line {}}}",
                         <ET::Char as Character>::display_opt(engine.state.get_escape_char()),
                         <ET::Char as Character>::display_opt(engine.state.get_escape_char()),
                         index, engine.mouth.line_number()));
    }
    crate::engine::gullet::methods::false_loop(engine,index,false,true);
    if trace {
        engine.aux.outputs.write_neg1(
            format_args!("{{{}or: {}ifcase (level {}) entered on line {}}}",
                         <ET::Char as Character>::display_opt(engine.state.get_escape_char()),
                         <ET::Char as Character>::display_opt(engine.state.get_escape_char()),
                         index, engine.mouth.line_number()));
    }
}

pub fn endlinechar_get<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) -> Int<ET> {
    Int::<ET>::from(match engine.state.get_endline_char() {
        Some(c) => c.into() as i32,
        _ => -1
    })
}
pub fn endlinechar_set<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token,globally:bool) {
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

pub fn escapechar_get<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) -> Int<ET> {
    Int::<ET>::from(match engine.state.get_escape_char() {
        Some(c) => c.into() as i32,
        _ => -1
    })
}
pub fn escapechar_set<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token,globally:bool) {
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


pub fn font_get<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) -> Fnt<ET> {
    engine.state.get_current_font().clone()
}

pub fn font_set<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token,global:bool) {
    let cs = match engine.read_control_sequence() {
        CSOrActiveChar::Name(name) => name,
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
    engine.state.set_command(engine.aux, cs, Some(TeXCommand::Font(font)), global)
}


pub fn textfont_get<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) -> Fnt<ET> {
    let num = engine.mathfont_index(false);
    engine.state.get_textfont(num).clone()
}

pub fn textfont_set<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token,global:bool) {
    let num = engine.mathfont_index(false);
    let fnt = engine.read_font();
    engine.state.set_textfont(engine.aux,num,fnt,global)
}


pub fn scriptfont_get<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) -> Fnt<ET> {
    let num = engine.mathfont_index(false);
    engine.state.get_scriptfont(num).clone()
}

pub fn scriptfont_set<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token,global:bool) {
    let num = engine.mathfont_index(false);
    let fnt = engine.read_font();
    engine.state.set_scriptfont(engine.aux,num,fnt,global)
}

pub fn scriptscriptfont_get<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) -> Fnt<ET> {
    let num = engine.mathfont_index(false);
    engine.state.get_scriptscriptfont(num).clone()
}

pub fn scriptscriptfont_set<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token,global:bool) {
    let num = engine.mathfont_index(false);
    let fnt = engine.read_font();
    engine.state.set_scriptscriptfont(engine.aux,num,fnt,global)
}

pub fn fontdimen_get<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) -> Dim<ET> {
    let idx = match engine.read_int(false).try_into() {
        Ok(i) if i-1 >= 0 && i-1 <= u16::MAX.into() => (i-1) as u16,
        _ => todo!("throw error")
    };
    let font = engine.read_font();
    font.get_dim(idx)
}
pub fn fontdimen_set<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token,_globally:bool) {
    let i = engine.read_int(false);
    let idx = match i.try_into() {
        Ok(i) if i-1 >= 0 && i-1 <= u16::MAX.into() => (i-1) as u16,
        _ => todo!("throw error: {}",i)
    };
    let mut font = engine.read_font();
    let dim = engine.read_dim(true);
    font.set_dim(idx,dim);
}

pub fn box_<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token) -> Result<Option<TeXBox<ET>>,BoxInfo<ET>> {
    let idx = engine.read_register_index(false);
    Ok(engine.state.take_box_register(idx))
}
pub fn copy<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token) -> Result<Option<TeXBox<ET>>,BoxInfo<ET>> {
    let idx = engine.read_register_index(false);
    Ok(engine.state.get_box_register(idx).cloned())
}

pub fn unbox<ET:EngineTypes>(engine:&mut EngineReferences<ET>, _tk:ET::Token, tp:BoxType, copy:bool) {
    let idx = engine.read_register_index(false);
    let bx = if copy {engine.state.get_box_register(idx).cloned()} else {engine.state.take_box_register(idx)};
    match bx {
        None => (),
        Some(TeXBox::V {children,..}) if tp == BoxType::Vertical => {
            for c in children.into_vec() {
                ET::Stomach::add_node_v(engine,c)
            }
        }
        Some(TeXBox::H {children,..}) if tp == BoxType::Horizontal => {
            match engine.stomach.data_mut().open_lists.last_mut() {
                Some(NodeList::Horizontal {children:ls,..}) =>
                    ls.extend(children.into_vec().into_iter()),
                _ => todo!("error: incompatible list can't be unboxed")
            }
        }
        _ => todo!("error")
    }
}

pub fn unhbox<ET:EngineTypes>(engine:&mut EngineReferences<ET>,tk:ET::Token) {
    unbox(engine, tk, BoxType::Horizontal, false)
}
pub fn unhcopy<ET:EngineTypes>(engine:&mut EngineReferences<ET>,tk:ET::Token) {
    unbox(engine, tk, BoxType::Horizontal, true)
}
pub fn unvbox<ET:EngineTypes>(engine:&mut EngineReferences<ET>,tk:ET::Token) {
    unbox(engine, tk, BoxType::Vertical, false)
}
pub fn unvcopy<ET:EngineTypes>(engine:&mut EngineReferences<ET>,tk:ET::Token) {
    unbox(engine, tk, BoxType::Vertical, true)
}

pub fn hbox<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token) -> Result<Option<TeXBox<ET>>,BoxInfo<ET>> {
    let scaled = super::methods::do_box_start(engine, BoxType::Horizontal, PRIMITIVES.everyhbox);
    Err(BoxInfo::H(HBoxInfo::new_box(scaled)))
}

pub fn vbox<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token) -> Result<Option<TeXBox<ET>>,BoxInfo<ET>> {
    let scaled = super::methods::do_box_start(engine, BoxType::Vertical, PRIMITIVES.everyvbox);
    Err(BoxInfo::V(VBoxInfo::new_box(scaled)))
}

pub fn vtop<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token) -> Result<Option<TeXBox<ET>>,BoxInfo<ET>> {
    let scaled = super::methods::do_box_start(engine, BoxType::Vertical, PRIMITIVES.everyvbox);
    Err(BoxInfo::V(VBoxInfo::new_top(scaled)))
}

pub fn vcenter<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token) {
    let scaled = super::methods::do_box_start(engine, BoxType::Vertical, PRIMITIVES.everyvbox);
    engine.stomach.data_mut().open_lists.push(NodeList::Vertical {
        children: Vec::new(),
        tp: VerticalNodeListType::VCenter(engine.mouth.start_ref(),scaled)
    });
}

pub fn halign<ET:EngineTypes>(engine:&mut EngineReferences<ET>,token:ET::Token) {
    match engine.stomach.data_mut().mode() {
        TeXMode::Horizontal => {
            engine.requeue(token);
            return ET::Stomach::close_paragraph(engine)
        },
        TeXMode::DisplayMath | TeXMode::Vertical | TeXMode::InternalVertical => (),
        _ => todo!("throw error")
    }
    let wd = if engine.read_keyword(b"to") {
        Some(engine.read_dim(false))
    } else {
        None
    };
    super::methods::do_align(engine, BoxType::Horizontal, BoxType::Vertical, wd,&token)
}

pub fn valign<ET:EngineTypes>(engine:&mut EngineReferences<ET>,tk:ET::Token) {
    let wd = if engine.read_keyword(b"to") {
        Some(engine.read_dim(false))
    } else {
        None
    };
    super::methods::do_align(engine, BoxType::Vertical, BoxType::Horizontal, wd,&tk)
}


pub fn hyphenchar_get<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) -> Int<ET> {
    let font = engine.read_font();
    font.get_hyphenchar()
}
pub fn hyphenchar_set<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token,_globally:bool) {
    let mut font = engine.read_font();
    let val = engine.read_int(true);
    font.set_hyphenchar(val);
}

pub fn skewchar_get<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) -> Int<ET> {
    let font = engine.read_font();
    font.get_skewchar()
}
pub fn skewchar_set<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token,_globally:bool) {
    let mut font = engine.read_font();
    let val = engine.read_int(true);
    font.set_skewchar(val);
}

pub fn r#if<ET:EngineTypes>(engine: &mut EngineReferences<ET>, _tk:ET::Token) -> bool {
    let first = super::methods::get_if_token(engine);
    let second = super::methods::get_if_token(engine);
    first.0 == second.0
}

pub fn ifcase<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) -> bool {
    let index = engine.gullet.get_conditionals().len() - 1;
    let num = engine.read_int(false);
    *engine.gullet.get_conditionals().get_mut(index).unwrap() = ActiveConditional::Case(num);
    num == <ET::Num as NumSet>::Int::default()
}

pub fn ifcat<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) -> bool {
    let first = super::methods::get_if_token(engine);
    let second = super::methods::get_if_token(engine);
    first.1 == second.1
}

pub fn ifdim<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) -> bool {
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

pub fn ifeof<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) -> bool {
    let idx = engine.read_file_index();
    engine.filesystem.eof(idx)
}

pub fn ifhmode<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) -> bool {
    match engine.stomach.data_mut().mode() {
        TeXMode::Horizontal | TeXMode::RestrictedHorizontal => true,
        _ => false

    }
}
pub fn ifinner<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) -> bool {
    match engine.stomach.data_mut().mode() {
        TeXMode::RestrictedHorizontal | TeXMode::InternalVertical | TeXMode::InlineMath => true,
        _ => false

    }
}
pub fn ifmmode<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) -> bool {
    match engine.stomach.data_mut().mode() {
        TeXMode::InlineMath | TeXMode::DisplayMath => true,
        _ => false

    }
}

pub fn ifnum<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) -> bool {
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


pub fn ifodd<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) -> bool {
    engine.read_int(false).into() % 2 != 0
}


pub fn iftrue<ET:EngineTypes>(_engine: &mut EngineReferences<ET>,_tk:ET::Token) -> bool { true }

pub fn iffalse<ET:EngineTypes>(_engine: &mut EngineReferences<ET>,_tk:ET::Token) -> bool {
    false
}

pub fn ifvmode<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) -> bool {
    match engine.stomach.data_mut().mode() {
        TeXMode::Vertical | TeXMode::InternalVertical => true,
        _ => false

    }
}
pub fn ifvbox<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) -> bool {
    let idx = engine.read_register_index(false);
    match engine.state.get_box_register(idx) {
        Some(TeXBox::V {..}) => true,
        _ => false
    }
}
pub fn ifvoid<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) -> bool {
    let idx = engine.read_register_index(false);
    engine.state.get_box_register(idx).is_none()
}
pub fn ifhbox<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) -> bool {
    let idx = engine.read_register_index(false);
    match engine.state.get_box_register(idx) {
        Some(TeXBox::H {..}) => true,
        _ => false
    }
}

pub fn ifx<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) -> bool {
    let first = IfxCmd::read(engine);
    let second = IfxCmd::read(engine);
    first == second
}

pub fn ignorespaces<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token) {
    while let Some(next) = engine.get_next() {
        if next.command_code() != CommandCode::Space {
            match ET::Gullet::char_or_primitive(engine.state,&next) {
                Some(CharOrPrimitive::Char(_,CommandCode::Space)) => (),
                _ => {
                    engine.requeue(next);
                    break
                }
            }
        }
    }
}

pub fn insert<ET:EngineTypes>(engine:&mut EngineReferences<ET>,tk:ET::Token) {
    let n = engine.read_int(false).into();
    if n < 0 { todo!("throw error")}
    engine.expand_until_bgroup(false,&tk);
    engine.state.push(engine.aux,GroupType::Box(BoxType::Vertical),engine.mouth.line_number());
    engine.stomach.data_mut().open_lists.push(
        NodeList::Vertical {children:vec!(),tp:VerticalNodeListType::Insert(n as usize)}
    )
}

pub fn immediate<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token) {
    expand_loop!(engine,token,
        ResolvedToken::Cmd(Some(TeXCommand::Primitive{cmd:PrimitiveCommand::Whatsit { immediate,.. },..})) => {
            return immediate(engine,token)
        },
        ResolvedToken::Tk {char,code} => ET::Stomach::do_char(engine,token,char,code),
        ResolvedToken::Cmd(Some(TeXCommand::Char {char,code})) => ET::Stomach::do_char(engine,token,*char,*code),
        ResolvedToken::Cmd(None) => tex_error!(engine,undefined,token),
        ResolvedToken::Cmd(Some(c)) =>
            crate::do_cmd!(engine,token,c)
    );
}


pub fn input<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token) {
    let mut filename = engine.aux.memory.get_string();
    engine.read_string(false,&mut filename);
    if filename.is_empty() {
        todo!("throw error")
    }
    let is_file = !filename.starts_with("|");
    let file = engine.filesystem.get(&filename);
    engine.aux.memory.return_string(filename);
    if is_file && !file.exists() {
        todo!("throw error")
    }
    engine.aux.outputs.file_open(&file);
    engine.push_file(file)
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
                         <ET::Char as Character>::display_opt(engine.state.get_escape_char()),
                         name.display(engine.state.get_escape_char()), index, engine.mouth.line_number()));
    }
}

pub fn jobname<ET:EngineTypes>(engine: &mut EngineReferences<ET>,exp:&mut Vec<ET::Token>,_tk:ET::Token) {
    let mut fi = |t| exp.push(t);
    let mut f = Otherize::new(&mut fi);
    write!(f,"{}",engine.aux.jobname).unwrap();
}

pub fn fontname<ET:EngineTypes>(engine: &mut EngineReferences<ET>,exp:&mut Vec<ET::Token>,_tk:ET::Token) {
    let font = engine.read_font();
    let mut fi = |t| exp.push(t);
    let mut f = Otherize::new(&mut fi);
    if font.has_at_set() {
        write!(f,"{} at {}",font.filename(),font.get_at()).unwrap();
    } else {
        write!(f,"{}",font.filename()).unwrap();
    }
}

pub fn let_<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token,globally:bool) {
    let cm = match engine.get_next() {
        Some(t) => match t.to_enum() {
            StandardToken::Character(c,CommandCode::Active) =>
                CSOrActiveChar::Active(c),
            StandardToken::ControlSequence(cs) => CSOrActiveChar::Name(cs),
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
            StandardToken::Primitive(id) =>
                engine.state.primitives().get_id(id).cloned(),
            StandardToken::Character(c,CommandCode::Active) =>
                engine.state.get_ac_command(c).cloned(),
            StandardToken::Character(c,cc) =>
                Some(TeXCommand::Char{char:c,code:cc})
        };
        engine.set_command(&cm,cmd,globally);
        return
    }
    todo!("file end")
}


pub fn futurelet<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token,globally:bool) {
    let cm = match engine.get_next() {
        Some(t) => match t.to_enum() {
            StandardToken::Character(c,CommandCode::Active) =>
                CSOrActiveChar::Active(c),
            StandardToken::ControlSequence(cs) => CSOrActiveChar::Name(cs),
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
            Some(TeXCommand::Char{char:c,code:cc}),
        StandardToken::Primitive(id) =>
            engine.state.primitives().get_id(id).cloned(),
    };
    engine.set_command(&cm,cmd,globally);
    engine.requeue(second);
    engine.requeue(first);
}

pub fn lowercase<ET:EngineTypes>(engine:&mut EngineReferences<ET>,tk:ET::Token) {
    engine.expand_until_bgroup(false,&tk);
    let mut exp = Vec::new();// ET::Gullet::get_expansion_container(engine);
    let state = &engine.state;
    engine.gullet.read_until_endgroup(engine.mouth,engine.aux,state,|_,_,t| {
        match t.to_enum() {
            StandardToken::Character(c,cc) => {
                let lccode = state.get_lccode(c);
                if lccode == ET::Char::default() {
                    exp.push(t)
                } else {
                    exp.push(ET::Token::from_char_cat(lccode,cc))
                }
            }
            _ => exp.push(t)
        }
    });
    engine.mouth.push_vec(exp);
}

pub fn uppercase<ET:EngineTypes>(engine:&mut EngineReferences<ET>,tk:ET::Token) {
    engine.expand_until_bgroup(false,&tk);
    let mut exp = Vec::new();//ET::Gullet::get_expansion_container(engine);
    let state = &engine.state;
    engine.gullet.read_until_endgroup(engine.mouth,engine.aux,state,|_,_,t| {
        match t.to_enum() {
            StandardToken::Character(c,cc) => {
                let uccode = state.get_uccode(c);
                if uccode == ET::Char::default() {
                    exp.push(t)
                } else {
                    exp.push(ET::Token::from_char_cat(uccode,cc))
                }
            }
            _ => exp.push(t),
        }
    });
    engine.mouth.push_vec(exp);
}

pub fn mathchardef<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token,globally:bool) {
    let cm = match engine.get_next() {
        Some(t) => match t.to_enum() {
            StandardToken::Character(c,CommandCode::Active) =>
                CSOrActiveChar::Active(c),
            StandardToken::ControlSequence(cs) => CSOrActiveChar::Name(cs),
            _ => todo!("throw error")
        }
        None => todo!("file end error")
    };
    let i = engine.read_int(true).into();
    if i < 0 || i > u32::MAX as i64 {
        todo!("matchchardef out of range")
    }
    let i = i as u32;
    engine.set_command(&cm, Some(TeXCommand::MathChar(i)), globally)
}

pub fn mathchar<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) {
    let i = engine.read_int(false).into();
    if i < 0 || i > u32::MAX as i64 {
        todo!("matchchardef out of range")
    }
    let i = i as u32;
    let ret = MathChar::from_u32(i, engine.state, None);
    ET::Stomach::add_node_m(engine, MathNode::Atom(ret.to_atom()));
}

pub fn left<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) {
    let del = engine.read_opt_delimiter();
    engine.stomach.data_mut().open_lists.push(
        NodeList::Math {
            children: MathNodeList::new(),
            start:engine.mouth.start_ref(),
            tp: MathNodeListType::LeftRight(del)
        }
    );
    engine.state.push(engine.aux,GroupType::LeftRight,engine.mouth.line_number());
}
pub fn right<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) {
    if engine.state.get_group_type() != Some(GroupType::LeftRight) {
        todo!("throw error")
    }
    let del = engine.read_opt_delimiter();
    match engine.stomach.data_mut().open_lists.pop() {
        Some(NodeList::Math{children,start,tp:MathNodeListType::LeftRight(left)}) => {
            engine.state.pop(engine.aux,engine.mouth);
            let (children,None) = children.close(start,engine.mouth.current_sourceref()).into() else {unreachable!()};
            ET::Stomach::add_node_m(engine,MathNode::Atom(MathAtom {
                nucleus: MathNucleus::LeftRight {
                    left:left.map(|d| (d.large.char,d.large.style)),
                    right:del.map(|d| (d.large.char,d.large.style)),
                    start,end:engine.mouth.current_sourceref(),
                    children:children.into()
                },
                sub:None,sup:None
            }));
        }
        _ => todo!("throw error")
    }
}

pub fn meaning<ET:EngineTypes>(engine: &mut EngineReferences<ET>,exp:&mut Vec<ET::Token>,_tk:ET::Token) {
    let mut fi = |t| exp.push(t);
    let mut f = Otherize::new(&mut fi);
    match engine.get_next() {
        None => todo!("throw error"),
        Some(t) => match engine.resolve(&t) {
            ResolvedToken::Cmd(None) => {
                if let Some(c) = engine.state.get_escape_char() {
                    f.push_char(c);
                }
                write!(f,"undefined").unwrap();
            }
            ResolvedToken::Cmd(Some(cmd)) => {
                cmd.meaning(engine.aux.memory.cs_interner(),engine.state.get_catcode_scheme(),engine.state.get_escape_char()).write_chars(&mut f)
            }
            ResolvedToken::Tk {char,code,..} =>
                code.meaning(char,f)
        }
    }
}

pub fn leaders<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token) {
    super::methods::do_leaders(engine,LeaderType::Normal)
}
pub fn xleaders<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token) {
    super::methods::do_leaders(engine,LeaderType::X)
}
pub fn cleaders<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token) {
    super::methods::do_leaders(engine,LeaderType::C)
}

pub fn message<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token) {
    let mut out = engine.aux.memory.get_string();
    engine.read_braced_string(false,true,&mut out);
    engine.aux.outputs.message(&out);
    engine.aux.memory.return_string(out);
}

pub fn errmessage<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token) {
    let mut out = engine.aux.memory.get_string();
    engine.read_braced_string(false,true,&mut out);
    write!(out," (line {})",engine.mouth.line_number()).unwrap();
    engine.aux.outputs.errmessage(&out);
    engine.aux.error_handler.error_message(&out);
    engine.aux.memory.return_string(out);
}

pub fn newlinechar_get<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) -> Int<ET> {
    Int::<ET>::from(match engine.state.get_newline_char() {
        Some(c) => c.into() as i32,
        _ => -1
    })
}
pub fn newlinechar_set<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token,globally:bool) {
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

pub fn number<ET:EngineTypes>(engine: &mut EngineReferences<ET>,exp:&mut Vec<ET::Token>,_tk:ET::Token) {
    let val = engine.read_int(false);
    write!(Otherize::new(&mut|t| exp.push(t)), "{}", val).unwrap();
}

pub fn noexpand<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token) {
    let token = match engine.get_next() {
        Some(t) if t == ET::Token::eof() => return,
        Some(t) => t,
        _ => todo!("throw error")
    };
    match engine.resolve(&token) {
        ResolvedToken::Tk{..} =>
            engine.requeue(token),
        ResolvedToken::Cmd(Some(cm)) => {
            match cm {
                TeXCommand::Macro(_) |
                TeXCommand::Primitive {cmd:
                    PrimitiveCommand::Expandable(_) |
                    PrimitiveCommand::SimpleExpandable(_) |
                    PrimitiveCommand::Conditional(_),..} => {
                    engine.mouth.requeue(token);
                    engine.mouth.requeue(ET::Token::primitive(PRIMITIVES.noexpand));
                }
                _ => engine.mouth.requeue(token)
            };
        }
        ResolvedToken::Cmd(_) =>
            engine.mouth.requeue(token),
    }
}

pub fn noindent<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) {
    if ET::Stomach::maybe_switch_mode(engine, CommandScope::SwitchesToHorizontal, tk) {
        match engine.stomach.data_mut().open_lists.last_mut() {
            Some(NodeList::Horizontal {children,..}) => match children.last_mut() {
                Some(HNode::Box(TeXBox::H{info:HBoxInfo::ParIndent {..},..})) => {children.pop();}
                _ => ()
            }
            _ => unreachable!()
        }
    }
}

pub fn openout<ET:EngineTypes>(engine:&mut EngineReferences<ET>, _tk:ET::Token)
               -> Option<Box<dyn FnOnce(&mut EngineReferences<ET>)>> {
    let (idx,file) = engine.read_filename_and_index();
    Some(Box::new(move |engine| {engine.filesystem.open_out(idx,file)}))
}
pub fn openout_immediate<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token) {
    let (idx,file) = engine.read_filename_and_index();
    engine.filesystem.open_out(idx,file)
}

pub fn prevdepth_get<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) -> Dim<ET> {
    engine.stomach.data_mut().prevdepth
}
pub fn prevdepth_set<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token,_globally:bool) {
    let val = engine.read_dim(true);
    engine.stomach.data_mut().prevdepth = val;
}

pub fn read<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token,globally:bool) {
    let idx = engine.read_file_index();
    if !engine.read_keyword("to".as_bytes()) {
        todo!("throw error")
    }
    let cs = engine.read_control_sequence();
    let mut ret = shared_vector::Vector::new();
    engine.filesystem.read::<ET,_>(idx,&engine.aux.error_handler,engine.aux.memory.cs_interner_mut(),engine.state,|t| ret.push(t));

    let m = Macro {
        long:false,outer:false,protected:false,
        expansion:ret.into(),
        signature:MacroSignature {
            arity:0,
            params:engine.aux.memory.empty_list().into()
        }
    };
    engine.set_command(&cs, Some(TeXCommand::Macro(m)), globally)
}

const ROMAN: &[(u8, i64)] = &[(b'm', 0),(b'd', 2),(b'c', 5),(b'l', 2),(b'x', 5),(b'v', 2),(b'i', 5)];
pub fn romannumeral<ET:EngineTypes>(engine: &mut EngineReferences<ET>,exp:&mut Vec<ET::Token>,_tk:ET::Token) {
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

pub fn setbox<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token,globally:bool) {
    let index = engine.read_register_index(false);
    match engine.read_box(true) {
        Ok(bx) =>
            engine.state.set_box_register(engine.aux,index,bx,globally),
        Err(bi) => {
            let target = BoxTarget::<ET>::new(move |e,b| e.state.set_box_register(e.aux,index,Some(b),globally));
            let mut ls = bi.open_list(engine.mouth.start_ref());
            match ls {
                NodeList::Horizontal {tp:HorizontalNodeListType::Box(_,_,ref mut t),..} => *t = target,
                NodeList::Vertical {tp:VerticalNodeListType::Box(_,_,ref mut t),..} => *t = target,
                _ => unreachable!()
            }
            engine.stomach.data_mut().open_lists.push(ls);
        }
    }
}

pub fn moveright<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) {
    let dim = engine.read_dim(false);
    match engine.read_box(false) {
        Ok(Some(mut bx)) => {
            match bx {
                TeXBox::H {ref mut info,..} => info.move_left(-dim),
                TeXBox::V {ref mut info,..} => info.move_left(-dim),
            }
            ET::Stomach::add_node_v(engine,VNode::Box(bx));
        }
        Ok(None) => (),
        Err(mut bi) => {
            bi.move_left(-dim);
            engine.stomach.data_mut().open_lists.push(bi.open_list(engine.mouth.start_ref()));
        }
    }
}

pub fn moveleft<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) {
    let dim = engine.read_dim(false);
    match engine.read_box(false) {
        Ok(Some(mut bx)) => {
            match bx {
                TeXBox::H {ref mut info,..} => info.move_left(dim),
                TeXBox::V {ref mut info,..} => info.move_left(dim),
            }
            ET::Stomach::add_node_v(engine,VNode::Box(bx));
        }
        Ok(None) => (),
        Err(mut bi) => {
            bi.move_left(dim);
            engine.stomach.data_mut().open_lists.push(bi.open_list(engine.mouth.start_ref()));
        }
    }
}

pub fn raise<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) {
    let dim = engine.read_dim(false);
    match engine.read_box(false) {
        Ok(Some(mut bx)) => {
            match bx {
                TeXBox::H {ref mut info,..} => info.raise(dim),
                TeXBox::V {ref mut info,..} => info.raise(dim),
            }
            ET::Stomach::add_node_h(engine,HNode::Box(bx));
        }
        Ok(None) => (),
        Err(mut bi) => {
            bi.raise(dim);
            engine.stomach.data_mut().open_lists.push(bi.open_list(engine.mouth.start_ref()));
        }
    }
}

pub fn lower<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) {
    let dim = engine.read_dim(false);
    match engine.read_box(false) {
        Ok(Some(mut bx)) => {
            match bx {
                TeXBox::H {ref mut info,..} => info.raise(-dim),
                TeXBox::V {ref mut info,..} => info.raise(-dim),
            }
            ET::Stomach::add_node_h(engine,HNode::Box(bx));
        }
        Ok(None) => (),
        Err(mut bi) => {
            bi.raise(-dim);
            engine.stomach.data_mut().open_lists.push(bi.open_list(engine.mouth.start_ref()));
        }
    }
}

pub fn string<ET:EngineTypes>(engine: &mut EngineReferences<ET>,exp:&mut Vec<ET::Token>,_tk:ET::Token) {
    match engine.get_next() {
        Some(t) => {
            if t.command_code() == CommandCode::Space {exp.push(t)}
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
                    _ => todo!("\"emergency stop\"")
                }
            }
        }
        None => todo!("file end")
    }
}

pub fn openin<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token) {
    let (idx,file) = engine.read_filename_and_index();
    engine.filesystem.open_in(idx,file)
}

pub fn closeout<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token)
                               -> Option<Box<dyn FnOnce(&mut EngineReferences<ET>)>> {
    let idx = engine.read_file_index();
    Some(Box::new(move |engine| {engine.filesystem.close_out(idx)}))
}
pub fn closeout_immediate<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token) {
    let idx = engine.read_file_index();
    engine.filesystem.close_out(idx)
}

pub fn closein<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token) {
    let idx = engine.read_file_index();
    engine.filesystem.close_in(idx)
}

pub fn write<ET:EngineTypes>(engine:&mut EngineReferences<ET>, _tk:ET::Token)
                             -> Option<Box<dyn FnOnce(&mut EngineReferences<ET>)>> {
    let idx = engine.read_int(false).into();
    let mut tks = Vec::new();
    tks.push(ET::Token::from_char_cat(b'{'.into(),CommandCode::BeginGroup));
    match engine.get_next() {
        Some(t) if t.command_code() == CommandCode::BeginGroup => (),
        Some(_) => todo!("should be begingroup"),
        None => todo!("file end")
    }
    engine.read_until_endgroup(|_,_,t| {
        tks.push(t);
    });
    tks.push(ET::Token::from_char_cat(b'}'.into(),CommandCode::EndGroup));
    Some(Box::new(move |engine| {do_write(engine,idx,tks)}))
}
pub fn write_immediate<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token) {
    let idx = engine.read_int(false).into();
    let mut out = engine.aux.memory.get_string();
    engine.read_braced_string(false,true,&mut out);
    engine.filesystem.write(idx,&out,engine.state.get_newline_char(),engine.aux);
    engine.aux.memory.return_string(out);
}
pub fn do_write<ET:EngineTypes>(engine:&mut EngineReferences<ET>,i:i64,v:Vec<ET::Token>) {
    engine.mouth.push_vec(v);
    let mut out = engine.aux.memory.get_string();
    engine.read_braced_string(false,false,&mut out);
    engine.filesystem.write(i,&out,engine.state.get_newline_char(),engine.aux);
    engine.aux.memory.return_string(out);
}


pub fn par<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) {
    let mode = engine.stomach.data_mut().mode();
    if mode.is_vertical() {return}
    if mode == TeXMode::Horizontal {
        ET::Stomach::close_paragraph(engine);return
    }
    todo!("throw error?")
}


pub fn the<ET:EngineTypes>(engine: &mut EngineReferences<ET>,exp:&mut Vec<ET::Token>,_tk:ET::Token) {
    engine.do_the(|_, _, _, t|exp.push(t))
}

pub fn time<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) -> Int<ET> {
    let now = engine.aux.start_time;
    let i = ((now.hour() * 60) + now.minute()) as i32;
    Int::<ET>::from(i)
}

pub fn toks<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token,global:bool) {
    let idx = engine.read_register_index(false);
    let mut had_eq = false;
    crate::expand_loop!(engine,token,
        ResolvedToken::Tk{char,code} => match (char.try_into(),code) {
            (_,CommandCode::Space) => (),
            (Ok(b'='),CommandCode::Other) if !had_eq => {
                if had_eq { todo!("throw error") }
                had_eq = true;
            }
            (_,CommandCode::BeginGroup) => {
                let mut tks = shared_vector::Vector::new();
                engine.read_until_endgroup(|_,_,t| tks.push(t));
                engine.state.set_toks_register(engine.aux,idx,TokenList::from(tks),global);
                return ()
            }
            _ => todo!("throw error")
        }
        _ => todo!("throw error")
    )
}

pub fn penalty<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token) {
    let i = match engine.read_int(false).try_into() {
        Ok(i) => i,
        Err(_) => todo!("throw error")
    };
    crate::add_node!(ET::Stomach;engine, VNode::Penalty(i), HNode::Penalty(i), MathNode::Penalty(i));
}

pub fn kern<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token) {
    let dim = engine.read_dim(false);
    crate::add_node!(ET::Stomach;engine,VNode::VKern(dim), HNode::HKern(dim), MathNode::HKern(dim));
}

pub fn vrule<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token) {
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
    let end = engine.mouth.current_sourceref();
    ET::Stomach::add_node_h(engine,HNode::VRule {width,height,depth,start,end})
}

pub fn hrule<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token) {
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
    let end = engine.mouth.current_sourceref();
    ET::Stomach::add_node_v(engine,VNode::HRule {width,height,depth,start,end})
}

pub fn vskip<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token) {
    let sk = engine.read_skip(false);
    ET::Stomach::add_node_v(engine,VNode::VSkip(sk))
}

pub fn vfil<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token) {
    ET::Stomach::add_node_v(engine,VNode::VFil)
}
pub fn vfill<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token) {
    ET::Stomach::add_node_v(engine,VNode::VFill)
}
pub fn vfilneg<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token) {
    ET::Stomach::add_node_v(engine,VNode::VFilneg)
}
pub fn vss<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token) {
    ET::Stomach::add_node_v(engine,VNode::Vss)
}

#[allow(unreachable_code)]
pub fn hskip<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token) {
    let sk = engine.read_skip(false);
    add_node!(ET::Stomach;engine,unreachable!(),HNode::HSkip(sk),MathNode::HSkip(sk));
}
#[allow(unreachable_code)]
pub fn hfil<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token) {
    add_node!(ET::Stomach;engine,unreachable!(),HNode::HFil,MathNode::HFil);
}
#[allow(unreachable_code)]
pub fn hfill<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token) {
    add_node!(ET::Stomach;engine,unreachable!(),HNode::HFill,MathNode::HFill);
}
#[allow(unreachable_code)]
pub fn hfilneg<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token) {
    add_node!(ET::Stomach;engine,unreachable!(),HNode::HFilneg,MathNode::HFilneg);
}
#[allow(unreachable_code)]
pub fn hss<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token) {
    add_node!(ET::Stomach;engine,unreachable!(),HNode::Hss,MathNode::Hss);
}

pub fn indent<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token) {
    let dim = engine.state.get_primitive_dim(PRIMITIVES.parindent);
    ET::Stomach::add_node_h(engine,
                            HNode::Box(TeXBox::H {children:vec!().into(),info:HBoxInfo::ParIndent(dim),start:engine.mouth.start_ref(),end:engine.mouth.current_sourceref(),preskip:None})
    )
}

pub fn delimiter<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token) {
    let num = engine.read_int(false);
    let delim = Delimiter::from_int(num,engine.state);
    ET::Stomach::add_node_m(engine,MathNode::Atom(delim.small.to_atom()))
}

pub fn mskip<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token) {
    let skip = engine.read_muskip(false);
    ET::Stomach::add_node_m(engine,MathNode::MSkip{skip,style:engine.state.get_mathfonts(2)})
}

pub fn mkern<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token) {
    let kern = engine.read_mudim(false);
    ET::Stomach::add_node_m(engine,MathNode::MKern{kern,style:engine.state.get_mathfonts(2)})
}

pub fn unskip<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token) {
    super::methods::un_x(engine,
        |v| matches!(v,VNode::VSkip(_) | VNode::VFil | VNode::VFill | VNode::VFilneg | VNode::Vss),
        |h| matches!(h,HNode::HSkip(_) | HNode::HFil | HNode::HFill | HNode::HFilneg | HNode::Hss),
        |m| matches!(m,MathNode::MSkip{..} | MathNode::HSkip(_) | MathNode::HFil | MathNode::HFill | MathNode::HFilneg | MathNode::Hss)
    )
}
pub fn unkern<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token) {
    super::methods::un_x(engine,
                         |v| matches!(v,VNode::VKern(_)),
                         |h| matches!(h,HNode::HKern(_)),
                         |m| matches!(m,MathNode::MKern{..} | MathNode::HKern(_))
    )
}
pub fn unpenalty<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token) {
    super::methods::un_x(engine,
                         |v| matches!(v,VNode::Penalty(_)),
                         |h| matches!(h,HNode::Penalty(_)),
                         |m| matches!(m,MathNode::Penalty(_))
    )
}

pub fn lastbox<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token) -> Result<Option<TeXBox<ET>>,BoxInfo<ET>> {
    if engine.stomach.data_mut().mode() == TeXMode::Vertical {
        todo!("throw error")
    }
    let data = engine.stomach.data_mut();
    match data.open_lists.last_mut() {
        None => todo!("throw error: Not allowed in vertical"),
        Some(NodeList::Vertical {children,..}) => {
            for (i,n) in children.iter().enumerate().rev() {
                if n.opaque() {continue }
                match n {
                    VNode::Box(_) => {
                        if let VNode::Box(bi) = children.remove(i) {
                            return Ok(Some(bi))
                        } else {
                            unreachable!()
                        }
                    },
                    _ => return Ok(None)
                }
            }
        }
        Some(NodeList::Horizontal {children,..}) => {
            for (i,n) in children.iter().enumerate().rev() {
                if n.opaque() {continue }
                match n {
                    HNode::Box(_) => {
                        if let HNode::Box(bi) = children.remove(i) {
                            return Ok(Some(bi))
                        } else {
                            unreachable!()
                        }
                    },
                    _ => return Ok(None)
                }
            }
        }
        _ => ()
    }
    Ok(None)
}

pub fn lastkern<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token) -> ET::Dim {
    super::methods::last_x(engine,
        |v| match v {VNode::VKern(k) => Some(*k),_ => None },
        |h| match h {HNode::HKern(k) => Some(*k),_ => None },
        |m| match m {MathNode::HKern(k) => Some(*k),_ => None }
    ).unwrap_or_default()
}

pub fn lastskip<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token) -> Skip<ET::Dim> {
    super::methods::last_x(engine,
                           |v| match v {VNode::VSkip(k) => Some(*k),_ => None },
                           |h| match h {HNode::HSkip(k) => Some(*k),_ => None },
                           |m| match m {MathNode::HSkip(k) => Some(*k),_ => None }
    ).unwrap_or_default()
}

pub fn lastpenalty<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token) -> ET::Int {
    super::methods::last_x(engine,
                           |v| match v {VNode::Penalty(k) => Some(*k),_ => None },
                           |h| match h {HNode::Penalty(k) => Some(*k),_ => None },
                           |m| match m {MathNode::Penalty(k) => Some(*k),_ => None }
    ).unwrap_or_default().into()
}

pub fn pagegoal_get<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token) -> ET::Dim {
    engine.stomach.data_mut().pagegoal
}
pub fn pagegoal_set<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token,_globally:bool) {
    let d = engine.read_dim(true);
    let data = engine.stomach.data_mut();
    if data.pagegoal != ET::Dim::from_sp(i32::MAX) {
        engine.stomach.data_mut().pagegoal = d;
    }
}
pub fn pagetotal_get<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token) -> ET::Dim {
    engine.stomach.data_mut().pagetotal
}
pub fn pagetotal_set<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token,_globally:bool) {
    let d = engine.read_dim(true);
    engine.stomach.data_mut().pagetotal = d;
}
pub fn pagestretch_get<ET:EngineTypes>(_engine:&mut EngineReferences<ET>,_tk:ET::Token) -> ET::Dim {
    ET::Dim::default() // TODO
}
pub fn pagestretch_set<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token,_globally:bool) {
    let _ = engine.read_dim(true);
    // TODO
}
pub fn pagefilstretch_get<ET:EngineTypes>(_engine:&mut EngineReferences<ET>,_tk:ET::Token) -> ET::Dim {
    ET::Dim::default() // TODO
}
pub fn pagefilstretch_set<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token,_globally:bool) {
    let _ = engine.read_dim(true);
    // TODO
}
pub fn pagefillstretch_get<ET:EngineTypes>(_engine:&mut EngineReferences<ET>,_tk:ET::Token) -> ET::Dim {
    ET::Dim::default() // TODO
}
pub fn pagefillstretch_set<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token,_globally:bool) {
    let _ = engine.read_dim(true);
    // TODO
}
pub fn pageshrink_get<ET:EngineTypes>(_engine:&mut EngineReferences<ET>,_tk:ET::Token) -> ET::Dim {
    ET::Dim::default() // TODO
}
pub fn pageshrink_set<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token,_globally:bool) {
    let _ = engine.read_dim(true);
    // TODO
}
pub fn pagefilshrink_get<ET:EngineTypes>(_engine:&mut EngineReferences<ET>,_tk:ET::Token) -> ET::Dim {
    ET::Dim::default() // TODO
}
pub fn pagefilshrink_set<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token,_globally:bool) {
    let _ = engine.read_dim(true);
    // TODO
}
pub fn pagefillshrink_get<ET:EngineTypes>(_engine:&mut EngineReferences<ET>,_tk:ET::Token) -> ET::Dim {
    ET::Dim::default() // TODO
}
pub fn pagefillshrink_set<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token,_globally:bool) {
    let _ = engine.read_dim(true);
    // TODO
}

pub fn deadcycles_get<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token) -> ET::Int {
    (engine.stomach.data_mut().deadcycles as i32).into()
}
pub fn deadcycles_set<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token,_globally:bool) {
    let i = engine.read_int(true).into();
    if i >= 0 {
        engine.stomach.data_mut().deadcycles = i as usize
    }
}

pub fn vsplit<ET:EngineTypes>(engine:&mut EngineReferences<ET>, _tk:ET::Token) -> Result<Option<TeXBox<ET>>,BoxInfo<ET>> {
    let idx = engine.read_register_index(false);
    let (mut info,ls,start,end) = match engine.state.get_box_register_mut(idx) {
        Some(TeXBox::V{info,children,start,end}) => {
            (info.clone_for_split(), std::mem::take(children).into_vec(),*start,*end)
        }
        _ => todo!("throw error")
    };
    if !engine.read_keyword(b"to") {
        todo!("throw error")
    }
    let target = engine.read_dim(false);
    match &mut info {
        VBoxInfo::VBox {scaled,..} => {
            *scaled = ToOrSpread::To(target);
        }
        VBoxInfo::VTop {scaled,..} => {
            *scaled = ToOrSpread::To(target);
        }
        _ => unreachable!()
    }
    let SplitResult {first,rest,..} = ET::Stomach::split_vertical(engine,ls,target);
    let ret = TeXBox::V{
        children:first.into(), info, start, end
    };
    if let Some(TeXBox::V{children,..}) = engine.state.get_box_register_mut(idx) {
        *children = rest.into();
    } else {
        unreachable!()
    }
    Ok(Some(ret))
}

pub fn vadjust<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) {
    match engine.stomach.data_mut().mode() {
        TeXMode::Vertical | TeXMode::InternalVertical => todo!("throw error"),
        _ => ()
    }
    engine.expand_until_bgroup(true,&tk);
    engine.stomach.data_mut().open_lists.push(NodeList::Vertical {
        children:vec!(),
        tp:VerticalNodeListType::VAdjust
    });
    engine.state.push(engine.aux,GroupType::Box(BoxType::Vertical),engine.mouth.line_number());
}


pub fn year<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) -> Int<ET> {
    Int::<ET>::from(engine.aux.start_time.year())
}


pub fn month<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) -> Int<ET> {
    Int::<ET>::from(engine.aux.start_time.month() as i32)
}


pub fn day<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) -> Int<ET> {
    Int::<ET>::from(engine.aux.start_time.day() as i32)
}


pub fn inputlineno<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) -> Int<ET> {
    Int::<ET>::from(engine.mouth.line_number() as i32)
}

pub fn mark<ET:EngineTypes>(engine:&mut EngineReferences<ET>,tk:ET::Token) {
    super::methods::do_marks(engine, 0,&tk)
}


pub fn topmark<ET:EngineTypes>(engine:&mut EngineReferences<ET>,exp:&mut Vec<ET::Token>,_tk:ET::Token) {
    super::methods::get_marks(engine, exp, |d| &mut d.topmarks, 0)
}

pub fn firstmark<ET:EngineTypes>(engine:&mut EngineReferences<ET>,exp:&mut Vec<ET::Token>,_tk:ET::Token) {
    super::methods::get_marks(engine, exp, |d| &mut d.firstmarks, 0)
}

pub fn botmark<ET:EngineTypes>(engine:&mut EngineReferences<ET>,exp:&mut Vec<ET::Token>,_tk:ET::Token) {
    super::methods::get_marks(engine, exp, |d| &mut d.botmarks, 0)
}

pub fn splitfirstmark<ET:EngineTypes>(engine:&mut EngineReferences<ET>,exp:&mut Vec<ET::Token>,_tk:ET::Token) {
    super::methods::get_marks(engine, exp, |d| &mut d.splitfirstmarks, 0)
}

pub fn splitbotmark<ET:EngineTypes>(engine:&mut EngineReferences<ET>,exp:&mut Vec<ET::Token>,_tk:ET::Token) {
    super::methods::get_marks(engine, exp, |d| &mut d.splitbotmarks, 0)
}

pub fn shipout<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token) {
    engine.stomach.data_mut().deadcycles = 0;
    match engine.read_box(false) {
        Ok(Some(bx)) => {
            engine.shipout(VNode::Box(bx));
            /*if let Some(n) = bx.as_node().shipout_top(engine) {
                engine.colon.out(n)
            }*/
        },
        Ok(None) => (),
        Err(bi) => {
            let mut list = bi.open_list(engine.mouth.start_ref());
            let target = BoxTarget::new(|e,b| e.shipout(VNode::Box(b)));
            match list {
                NodeList::Horizontal {tp:HorizontalNodeListType::Box(_,_,ref mut t),..} => *t = target,
                NodeList::Vertical {tp:VerticalNodeListType::Box(_,_,ref mut t),..} => *t = target,
                _ => unreachable!()
            }
            let data = engine.stomach.data_mut();
            data.open_lists.push(list);
        }
    }
}

pub fn displaylimits<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) {
    let ls = engine.stomach.data_mut().open_lists.last_mut().unwrap();
    match ls {
        NodeList::Math {children,..} => {
            match children.list_mut().last_mut() {
                Some(MathNode::Atom(MathAtom {sub:None,sup:None,nucleus:MathNucleus::Simple {ref mut limits,..},..})) => {
                    *limits = None;
                }
                _ => ()
            }
        }
        _ => unreachable!()
    }
}
pub fn limits<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) {
    let ls = engine.stomach.data_mut().open_lists.last_mut().unwrap();
    match ls {
        NodeList::Math {children,..} => {
            match children.list_mut().last_mut() {
                Some(MathNode::Atom(MathAtom {sub:None,sup:None,nucleus:MathNucleus::Simple {cls:MathClass::Op,ref mut limits,..},..})) => {
                    *limits = Some(true);
                }
                _ => ()
            }
        }
        _ => unreachable!()
    }
}
pub fn nolimits<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) {
    let ls = engine.stomach.data_mut().open_lists.last_mut().unwrap();
    match ls {
        NodeList::Math {children,..} => {
            match children.list_mut().last_mut() {
                Some(MathNode::Atom(MathAtom {sub:None,sup:None,nucleus:MathNucleus::Simple {cls:MathClass::Op,ref mut limits,..},..})) => {
                    *limits = Some(false);
                }
                _ => ()
            }
        }
        _ => unreachable!()
    }
}

pub fn underline<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) {
    engine.read_char_or_math_group(|_,engine,mc| {
        ET::Stomach::add_node_m(engine,MathNode::Atom(MathAtom {
            nucleus: MathNucleus::Underline(MathKernel::Char {char: mc.char,style:mc.style}),
            sub: None,
            sup: None,
        }))
    },|_| ListTarget::<ET,_>::new(
            |engine,children,start| ET::Stomach::add_node_m(engine,MathNode::Atom(
                MathAtom {
                    sup:None,sub:None,nucleus:MathNucleus::Underline(MathKernel::List {children:children.into(),start,end:engine.mouth.current_sourceref()})
                }
            ))
    ),())
}
pub fn overline<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) {
    engine.read_char_or_math_group(|_,engine,mc| {
        ET::Stomach::add_node_m(engine,MathNode::Atom(MathAtom {
            nucleus: MathNucleus::Overline(MathKernel::Char {char: mc.char,style:mc.style}),
            sub: None,
            sup: None,
        }))
    },|_| ListTarget::<ET,_>::new(
        |engine,children,start| ET::Stomach::add_node_m(engine,MathNode::Atom(
            MathAtom {
                sup:None,sub:None,nucleus:MathNucleus::Overline(MathKernel::List {children:children.into(),start,end:engine.mouth.current_sourceref()})
            }
        ))
    ),())
}

pub fn mathaccent<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) {
    let i = engine.read_int(false).into();
    if i < 0 || i > u32::MAX as i64 {
        todo!("matchchar out of range")
    }
    let char = MathChar::from_u32(i as u32, engine.state, None);
    engine.read_char_or_math_group(|(char,style),engine,mc| {
        ET::Stomach::add_node_m(engine,MathNode::Atom(MathAtom {
            nucleus: MathNucleus::Accent{
                accent:(char,style),
                inner:vec!(MathNode::Atom(mc.to_atom())).into()
            },
            sub: None,
            sup: None,
        }))
    },|(char,style)| ListTarget::<ET,_>::new(
        move |engine,children,_| ET::Stomach::add_node_m(engine,MathNode::Atom(
            MathAtom {
                sup:None,sub:None,nucleus:MathNucleus::Accent{
                    accent:(char,style),
                    inner:children.into()
                },
            }
        ))
    ),(char.char,char.style))
}

pub fn radical<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) {

    let i = engine.read_int(false).into();
    if i < 0 || i > u32::MAX as i64 {
        todo!("matchchar out of range")
    }
    let char = MathChar::from_u32(i as u32, engine.state, None);
    engine.read_char_or_math_group(|(char,style),engine,mc| {
        ET::Stomach::add_node_m(engine,MathNode::Atom(MathAtom {
            nucleus: MathNucleus::Radical{
                rad:(char,style),
                inner:vec!(MathNode::Atom(mc.to_atom())).into()
            },
            sub: None,
            sup: None,
        }))
    },|(char,style)| ListTarget::<ET,_>::new(
        move |engine,children,_| ET::Stomach::add_node_m(engine,MathNode::Atom(
            MathAtom {
                sup:None,sub:None,nucleus:MathNucleus::Radical{
                    rad:(char,style),
                    inner:children.into()
                },
            }
        ))
    ),(char.char,char.style))
}

fn do_eqno<ET:EngineTypes>(engine:&mut EngineReferences<ET>,pos:EqNoPosition) {
    match engine.stomach.data_mut().open_lists.last_mut() {
        Some(NodeList::Math {children:ch@MathNodeList::Simple(_)|ch@MathNodeList::Over {..},tp:MathNodeListType::Top {..},start}) => {
            let old = std::mem::replace(ch,MathNodeList::EqNo {
                pos,main:vec!(),eqno:vec!()
            });
            let (children,None) = old.close(*start,engine.mouth.current_sourceref()) else {unreachable!()};
            let MathNodeList::EqNo {main,..} = ch else {unreachable!()};
            *main = children;
        }
        _ => todo!("throw error")
    }
}

pub fn eqno<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) { do_eqno(engine,EqNoPosition::Right) }

pub fn leqno<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) { do_eqno(engine,EqNoPosition::Left) }

pub fn over<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) {
    match engine.stomach.data_mut().open_lists.last_mut() {
        Some(NodeList::Math {children:ch@MathNodeList::Simple(_),..}) => {
            //let v = std::mem::take(v);
            let old = std::mem::replace(ch,MathNodeList::Over {
                top: vec!(),
                bottom: vec!().into(),
                left:None,right:None,sep:None
            });
            if let MathNodeList::Over {top,..} = ch {
                *top = if let MathNodeList::Simple(v) = old {v} else {unreachable!()};
            } else {unreachable!()}
        }
        Some(NodeList::Math {..}) => todo!("throw error"),
        _ => unreachable!()
    }
}

pub fn overwithdelims<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) {
    let left = engine.read_opt_delimiter().map(|d| (d.small.char,d.small.style));
    let right = engine.read_opt_delimiter().map(|d| (d.small.char,d.small.style));
    match engine.stomach.data_mut().open_lists.last_mut() {
        Some(NodeList::Math {children:ch@MathNodeList::Simple(_),..}) => {
            //let v = std::mem::take(v);
            let old = std::mem::replace(ch,MathNodeList::Over {
                top: vec!(),
                bottom: vec!().into(),
                left,right,sep:None
            });
            if let MathNodeList::Over {top,..} = ch {
                *top = if let MathNodeList::Simple(v) = old {v} else {unreachable!()};
            } else {unreachable!()}
        }
        Some(NodeList::Math {..}) => todo!("throw error"),
        _ => unreachable!()
    }
}

pub fn above<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) {
    let sep = engine.read_dim(false);
    match engine.stomach.data_mut().open_lists.last_mut() {
        Some(NodeList::Math {children:ch@MathNodeList::Simple(_),..}) => {
            //let v = std::mem::take(v);
            let old = std::mem::replace(ch,MathNodeList::Over {
                top: vec!(),
                bottom: vec!().into(),
                left:None,right:None,sep:Some(sep)
            });
            if let MathNodeList::Over {top,..} = ch {
                *top = if let MathNodeList::Simple(v) = old {v} else {unreachable!()};
            } else {unreachable!()}
        }
        Some(NodeList::Math {..}) => todo!("throw error"),
        _ => unreachable!()
    }
}

pub fn abovewithdelims<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) {
    let left = engine.read_opt_delimiter().map(|d| (d.small.char,d.small.style));
    let right = engine.read_opt_delimiter().map(|d| (d.small.char,d.small.style));
    let sep = engine.read_dim(false);
    match engine.stomach.data_mut().open_lists.last_mut() {
        Some(NodeList::Math {children:ch@MathNodeList::Simple(_),..}) => {
            //let v = std::mem::take(v);
            let old = std::mem::replace(ch,MathNodeList::Over {
                top: vec!(),
                bottom: vec!().into(),
                left,right,sep:Some(sep)
            });
            if let MathNodeList::Over {top,..} = ch {
                *top = if let MathNodeList::Simple(v) = old {v} else {unreachable!()};
            } else {unreachable!()}
        }
        Some(NodeList::Math {..}) => todo!("throw error"),
        _ => unreachable!()
    }
}

pub fn atop<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) {
    match engine.stomach.data_mut().open_lists.last_mut() {
        Some(NodeList::Math {children:ch@MathNodeList::Simple(_),..}) => {
            //let v = std::mem::take(v);
            let old = std::mem::replace(ch,MathNodeList::Over {
                top: vec!(),
                bottom: vec!().into(),
                left:None,right:None,sep:Some(ET::Dim::default())
            });
            if let MathNodeList::Over {top,..} = ch {
                *top = if let MathNodeList::Simple(v) = old {v} else {unreachable!()};
            } else {unreachable!()}
        }
        Some(NodeList::Math {..}) => todo!("throw error"),
        _ => unreachable!()
    }
}

pub fn atopwithdelims<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) {
    let left = engine.read_opt_delimiter().map(|d| (d.small.char,d.small.style));
    let right = engine.read_opt_delimiter().map(|d| (d.small.char,d.small.style));
    match engine.stomach.data_mut().open_lists.last_mut() {
        Some(NodeList::Math {children:ch@MathNodeList::Simple(_),..}) => {
            //let v = std::mem::take(v);
            let old = std::mem::replace(ch,MathNodeList::Over {
                top: vec!(),
                bottom: vec!().into(),
                left,right,sep:Some(ET::Dim::default())
            });
            if let MathNodeList::Over {top,..} = ch {
                *top = if let MathNodeList::Simple(v) = old {v} else {unreachable!()};
            } else {unreachable!()}
        }
        Some(NodeList::Math {..}) => todo!("throw error"),
        _ => unreachable!()
    }
}

pub fn mathord<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) {
    super::methods::do_math_class(engine,Some(MathClass::Ord))
}
pub fn mathop<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) {
    super::methods::do_math_class(engine,Some(MathClass::Op))
}
pub fn mathbin<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) {
    super::methods::do_math_class(engine,Some(MathClass::Bin))
}
pub fn mathrel<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) {
    super::methods::do_math_class(engine,Some(MathClass::Rel))
}
pub fn mathopen<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) {
    super::methods::do_math_class(engine,Some(MathClass::Open))
}
pub fn mathclose<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) {
    super::methods::do_math_class(engine,Some(MathClass::Close))
}
pub fn mathpunct<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) {
    super::methods::do_math_class(engine,Some(MathClass::Punct))
}
pub fn mathinner<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) {
    super::methods::do_math_class(engine,None)
}
pub fn mathchoice<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) {
    engine.read_char_or_math_group(|_,engine,mc| mathchoice_i(engine,vec!(MathNode::Atom(mc.to_atom())).into())
                                   ,|_| ListTarget::<ET,_>::new(
        |engine,children,_| mathchoice_i(engine,children.into())
    ),())
}
type ML<ET> = Box<[MathNode<ET,UnresolvedMathFontStyle<ET>>]>;
pub fn mathchoice_i<ET:EngineTypes>(engine: &mut EngineReferences<ET>,d:ML<ET>) {
    engine.read_char_or_math_group(|d,engine,mc| mathchoice_ii(engine,d,vec!(MathNode::Atom(mc.to_atom())).into()),
                                   |d| ListTarget::<ET,_>::new(
        move |engine,children,_| mathchoice_ii(engine,d,children.into())
    ),d)
}
pub fn mathchoice_ii<ET:EngineTypes>(engine: &mut EngineReferences<ET>,d:ML<ET>,t:ML<ET>) {
    engine.read_char_or_math_group(|(d,t),engine,mc| mathchoice_iii(engine,d,t,vec!(MathNode::Atom(mc.to_atom())).into()),
                                   |(d,t)| ListTarget::<ET,_>::new(
        |engine,children,_| mathchoice_iii(engine,d,t,children.into())
    ),(d,t))
}
pub fn mathchoice_iii<ET:EngineTypes>(engine: &mut EngineReferences<ET>,d:ML<ET>,t:ML<ET>,s:ML<ET>) {
    engine.read_char_or_math_group(|(d,t,s),engine,mc| {
        ET::Stomach::add_node_m(engine,MathNode::Choice(UnresolvedMathChoice {
            display: d,
            text: t,
            script: s,
            scriptscript: vec!(MathNode::Atom(mc.to_atom())).into()
        }))
    },
                                   |(d,t,s)| ListTarget::<ET,_>::new(
        |engine,children,_|
            ET::Stomach::add_node_m(engine,MathNode::Choice(UnresolvedMathChoice {
                display: d,
                text: t,
                script: s,
                scriptscript: children.into()
            }))
    ),(d,t,s))
}

pub fn displaystyle<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) {
    ET::Stomach::add_node_m(engine,MathNode::Marker(UnresolvedMarkers::Display))
}
pub fn textstyle<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) {
    ET::Stomach::add_node_m(engine,MathNode::Marker(UnresolvedMarkers::Text))
}
pub fn scriptstyle<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) {
    ET::Stomach::add_node_m(engine,MathNode::Marker(UnresolvedMarkers::Script))
}
pub fn scriptscriptstyle<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) {
    ET::Stomach::add_node_m(engine,MathNode::Marker(UnresolvedMarkers::ScriptScript))
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
    "widowpenalty",
    "synctex" // not technically plain tex, but supported in basically all engines
];

const PRIMITIVE_DIMS:&[&'static str] = &[
    "boxmaxdepth",
    "delimitershortfall",
    "displayindent",
    "displaywidth",
    "hangindent",
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

#[allow(unreachable_code)]
pub fn char_space<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token) {
    crate::add_node!(ET::Stomach;engine,unreachable!(), HNode::Space, MathNode::Space)
}
pub fn char_slash<ET:EngineTypes>(_engine:&mut EngineReferences<ET>,_tk:ET::Token) {
    // TODO
}
pub fn char_dash<ET:EngineTypes>(_engine:&mut EngineReferences<ET>,_tk:ET::Token) {
    // TODO
}

pub fn end_template<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token) {
    match engine.gullet.get_align_data() {
        None => unreachable!(),
        Some(data) => {
            data.omit = false;
            let skip = data.columns[data.currindex].tabskip;
            match engine.stomach.data_mut().open_lists.last_mut() {
                Some(NodeList::Horizontal {children,..}) => {
                    children.push(HNode::HSkip(skip));
                }
                Some(NodeList::Vertical {children,..}) => {
                    children.push(VNode::VSkip(skip));
                }
                _ => unreachable!()
            }
            data.currindex += 1;
            if data.columns.len() <= data.currindex {
                match data.repeat_index {
                    Some(i) => data.currindex = i,
                    _ => ()
                }
            }
            if data.span {
                match engine.stomach.data_mut().open_lists.last_mut() {
                    Some(NodeList::Horizontal {tp:HorizontalNodeListType::HAlignCell(_,ref mut span),..}) |
                    Some(NodeList::Vertical {tp:VerticalNodeListType::VAlignCell(_,ref mut span),..}) => {
                        *span += 1;
                    }
                    _ => todo!("throw error")
                }
            } else {
                super::methods::pop_align_cell(engine.state, engine.aux, engine.stomach, engine.mouth, data.inner_mode);
            }
            let mode = data.inner_mode;
            crate::expand_loop!(engine,token,
                ResolvedToken::Tk{code:CommandCode::EndGroup,..} |
                ResolvedToken::Cmd(Some(TeXCommand::Char {code:CommandCode::EndGroup,..})) => {
                    todo!("close align?")
                }
                ResolvedToken::Tk{code:CommandCode::Space,..} => (),
                /*ResolvedToken::Cmd {cmd:Some(Command::Unexpandable(Unexpandable {name,..})),..}
                    if *name == PRIMITIVES.crcr => (),*/
                ResolvedToken::Cmd(Some(TeXCommand::Primitive {name,..})) if *name == PRIMITIVES.omit => {
                    engine.gullet.get_align_data().unwrap().omit = true;
                    return super::methods::open_align_cell(engine,mode)
                },
                ResolvedToken::Tk{..} | ResolvedToken::Cmd(_) => {
                    engine.requeue(token);
                    return super::methods::open_align_cell(engine,mode)
                }
            );
            todo!("file end")
        }
    }
}

pub fn end_template_row<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token) {
    match engine.gullet.get_align_data() {
        None => unreachable!(),
        Some(data) => {
            data.omit = false;
            let skip = data.columns[data.currindex].tabskip;
            match engine.stomach.data_mut().open_lists.last_mut() {
                Some(NodeList::Horizontal {children,..}) => {
                    children.push(HNode::HSkip(skip));
                }
                Some(NodeList::Vertical {children,..}) => {
                    children.push(VNode::VSkip(skip));
                }
                _ => unreachable!()
            }
            let mode = data.inner_mode;
            super::methods::pop_align_cell(engine.state, engine.aux, engine.stomach, engine.mouth, mode);
            super::methods::pop_align_row::<ET>(engine.stomach, engine.mouth, mode);
            super::methods::start_align_row(engine, mode);
        }
    }
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
    register_int(engine,"parshape",parshape_get,Some(parshape_set));
    register_int(engine,"deadcycles",deadcycles_get,Some(deadcycles_set));

    register_dim(engine,"fontdimen",fontdimen_get,Some(fontdimen_set));
    register_dim(engine,"dimen",dimen_get,Some(dimen_set));
    register_dim(engine,"prevdepth",prevdepth_get,Some(prevdepth_set));
    register_dim(engine,"dp",dp_get,Some(dp_set));
    register_dim(engine,"ht",ht_get,Some(ht_set));
    register_dim(engine,"wd",wd_get,Some(wd_set));
    register_dim(engine,"lastkern",lastkern,None);
    register_dim(engine,"pagegoal",pagegoal_get,Some(pagegoal_set));
    register_dim(engine,"pagetotal",pagetotal_get,Some(pagegoal_set));
    register_dim(engine,"pagestretch",pagestretch_get,Some(pagestretch_set));
    register_dim(engine,"pagefilstretch",pagefilstretch_get,Some(pagefilstretch_set));
    register_dim(engine,"pagefillstretch",pagefillstretch_get,Some(pagefillstretch_set));
    register_dim(engine,"pageshrink",pageshrink_get,Some(pageshrink_set));
    register_dim(engine,"pagefilshrink",pagefilshrink_get,Some(pagefilshrink_set));
    register_dim(engine,"pagefillshrink",pagefillshrink_get,Some(pagefillshrink_set));

    register_skip(engine,"skip",skip_get,Some(skip_set));
    register_skip(engine,"lastskip",lastskip,None);

    register_muskip(engine,"muskip",muskip_get,Some(muskip_set));

    register_font(engine,"font",font_get,Some(font_set));
    register_font(engine,"textfont",textfont_get,Some(textfont_set));
    register_font(engine,"scriptfont",scriptfont_get,Some(scriptfont_set));
    register_font(engine,"scriptscriptfont",scriptscriptfont_get,Some(scriptscriptfont_set));

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
    register_simple_expandable(engine, "else", r#else);
    register_simple_expandable(engine,"endinput",endinput);
    register_simple_expandable(engine,"or",or);
    register_simple_expandable(engine,"expandafter",expandafter);
    register_simple_expandable(engine,"fi",fi);
    register_simple_expandable(engine,"input",input);
    register_simple_expandable(engine,"noexpand",noexpand);

    register_expandable(engine,"jobname",jobname);
    register_expandable(engine,"fontname",fontname);
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

    register_conditional(engine, "if", r#if);
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

    register_unexpandable(engine,"afterassignment",CommandScope::Any,afterassignment);
    register_unexpandable(engine,"aftergroup",CommandScope::Any,aftergroup);
    register_unexpandable(engine,"begingroup",CommandScope::Any,begingroup);
    register_unexpandable(engine,"closein",CommandScope::Any,closein);
    register_unexpandable(engine,"char",CommandScope::SwitchesToHorizontalOrMath,char_);
    register_unexpandable(engine,"discretionary",CommandScope::Any,discretionary);
    register_unexpandable(engine,"dump",CommandScope::Any,|_,_|());
    register_unexpandable(engine,"endcsname",CommandScope::Any,endcsname);
    register_unexpandable(engine,"endgroup",CommandScope::Any,endgroup);
    register_unexpandable(engine,"end",CommandScope::Any,end);
    register_unexpandable(engine,"errorstopmode",CommandScope::Any,errorstopmode);
    register_unexpandable(engine,"halign",CommandScope::Any,halign);
    register_unexpandable(engine,"valign",CommandScope::SwitchesToHorizontal,valign);
    register_unexpandable(engine, "hyphenation",CommandScope::Any, |e,_|e.skip_argument());
    register_unexpandable(engine,"ignorespaces",CommandScope::Any,ignorespaces);
    register_unexpandable(engine,"insert",CommandScope::Any,insert);
    register_unexpandable(engine,"immediate",CommandScope::Any,immediate);
    register_unexpandable(engine,"lowercase",CommandScope::Any,lowercase);
    register_unexpandable(engine,"uppercase",CommandScope::Any,uppercase);
    register_unexpandable(engine,"leaders",CommandScope::Any,leaders);
    register_unexpandable(engine,"xleaders",CommandScope::Any,xleaders);
    register_unexpandable(engine,"cleaders",CommandScope::Any,cleaders);
    register_unexpandable(engine,"message",CommandScope::Any,message);
    register_unexpandable(engine,"errmessage",CommandScope::Any,errmessage);
    register_unexpandable(engine,"noindent",CommandScope::Any,noindent);
    register_unexpandable(engine,"openin",CommandScope::Any,openin);
    register_unexpandable(engine,"par",CommandScope::Any,par);
    register_unexpandable(engine,"unhbox",CommandScope::SwitchesToHorizontalOrMath,unhbox);
    register_unexpandable(engine,"unvbox",CommandScope::SwitchesToVertical,unvbox);
    register_unexpandable(engine,"unhcopy",CommandScope::SwitchesToHorizontalOrMath,unhcopy);
    register_unexpandable(engine,"unvcopy",CommandScope::SwitchesToVertical,unvcopy);
    register_unexpandable(engine,"unskip",CommandScope::Any,unskip);
    register_unexpandable(engine,"unkern",CommandScope::Any,unkern);
    register_unexpandable(engine,"unpenalty",CommandScope::Any,unpenalty);
    register_unexpandable(engine,"moveleft",CommandScope::SwitchesToVertical,moveleft);
    register_unexpandable(engine,"moveright",CommandScope::SwitchesToVertical,moveright);
    register_unexpandable(engine,"raise",CommandScope::SwitchesToHorizontalOrMath,raise);
    register_unexpandable(engine,"lower",CommandScope::SwitchesToHorizontalOrMath,lower);
    register_unexpandable(engine,"shipout",CommandScope::Any,shipout);
    register_unexpandable(engine, "patterns", CommandScope::Any,|e,_| e.skip_argument());
    register_unexpandable(engine,"vadjust",CommandScope::Any,vadjust);
    {
        let refs = engine.get_engine_refs();
        let relax = refs.aux.memory.cs_interner_mut().new("relax");
        let nullfont = refs.aux.memory.cs_interner_mut().new("nullfont");
        refs.state.set_command(refs.aux, relax, Some(TeXCommand::Primitive {name:PRIMITIVES.relax,cmd:PrimitiveCommand::Relax}), true);
        refs.state.set_command(refs.aux, nullfont, Some(TeXCommand::Font(refs.fontsystem.null())), true)
    }
    register_unexpandable(engine,"mark",CommandScope::Any,mark);
    register_unexpandable(engine,"/",CommandScope::Any,char_slash);
    register_unexpandable(engine,"-",CommandScope::Any,char_dash);
    register_unexpandable(engine,"showlists",CommandScope::Any,|_,_| {}); // TODO
    register_unexpandable(engine,"crcr",CommandScope::Any,|_,_| {
        print!("")
    });
    register_unexpandable(engine,"cr",CommandScope::Any,|_,_| {todo!("throw error")});
    register_unexpandable(engine,END_TEMPLATE,CommandScope::Any,end_template);
    register_unexpandable(engine,END_TEMPLATE_ROW,CommandScope::Any,end_template_row);

    register_unexpandable(engine, "delimiter", CommandScope::MathOnly, delimiter);
    register_unexpandable(engine, "mskip", CommandScope::MathOnly, mskip);
    register_unexpandable(engine, "mkern", CommandScope::MathOnly, mkern);
    register_unexpandable(engine, "mathchar", CommandScope::MathOnly, mathchar);
    register_unexpandable(engine, "left", CommandScope::MathOnly, left);
    register_unexpandable(engine, "right", CommandScope::MathOnly, right);
    register_unexpandable(engine, "displaylimits", CommandScope::MathOnly, displaylimits);
    register_unexpandable(engine, "limits", CommandScope::MathOnly, limits);
    register_unexpandable(engine, "nolimits", CommandScope::MathOnly, nolimits);
    register_unexpandable(engine, "penalty", CommandScope::Any, penalty);
    register_unexpandable(engine, "kern", CommandScope::Any, kern);
    register_unexpandable(engine, "vrule", CommandScope::SwitchesToHorizontal, vrule);
    register_unexpandable(engine, "vskip", CommandScope::SwitchesToVertical, vskip);
    register_unexpandable(engine, "vfil", CommandScope::SwitchesToVertical, vfil);
    register_unexpandable(engine, "vfill", CommandScope::SwitchesToVertical, vfill);
    register_unexpandable(engine, "vfilneg", CommandScope::SwitchesToVertical, vfilneg);
    register_unexpandable(engine, "vss", CommandScope::SwitchesToVertical, vss);
    register_unexpandable(engine, "hrule", CommandScope::SwitchesToVertical, hrule);
    register_unexpandable(engine, "hskip", CommandScope::SwitchesToHorizontalOrMath, hskip);
    register_unexpandable(engine, "hfil", CommandScope::SwitchesToHorizontalOrMath, hfil);
    register_unexpandable(engine, "hfill", CommandScope::SwitchesToHorizontalOrMath, hfill);
    register_unexpandable(engine, "hfilneg", CommandScope::SwitchesToHorizontalOrMath, hfilneg);
    register_unexpandable(engine, "hss", CommandScope::SwitchesToHorizontalOrMath, hss);
    register_unexpandable(engine, "indent", CommandScope::SwitchesToHorizontal, indent);
    register_unexpandable(engine, " ", CommandScope::SwitchesToHorizontalOrMath, char_space);
    register_unexpandable(engine,"vcenter",CommandScope::MathOnly,vcenter);

    register_unexpandable(engine, "mathord", CommandScope::MathOnly, mathord);
    register_unexpandable(engine, "mathop", CommandScope::MathOnly, mathop);
    register_unexpandable(engine, "mathbin", CommandScope::MathOnly, mathbin);
    register_unexpandable(engine, "mathrel", CommandScope::MathOnly, mathrel);
    register_unexpandable(engine, "mathopen", CommandScope::MathOnly, mathopen);
    register_unexpandable(engine, "mathclose", CommandScope::MathOnly, mathclose);
    register_unexpandable(engine, "mathpunct", CommandScope::MathOnly, mathpunct);
    register_unexpandable(engine, "mathinner", CommandScope::MathOnly, mathinner);
    register_unexpandable(engine, "mathchoice", CommandScope::MathOnly, mathchoice);
    register_unexpandable(engine, "underline", CommandScope::MathOnly, underline);
    register_unexpandable(engine, "overline", CommandScope::MathOnly, overline);
    register_unexpandable(engine, "mathaccent", CommandScope::MathOnly, mathaccent);
    register_unexpandable(engine, "radical", CommandScope::MathOnly, radical);
    register_unexpandable(engine, "eqno", CommandScope::MathOnly, eqno);
    register_unexpandable(engine, "leqno", CommandScope::MathOnly, leqno);

    register_unexpandable(engine, "over", CommandScope::MathOnly, over);
    register_unexpandable(engine, "overwithdelims", CommandScope::MathOnly, overwithdelims);
    register_unexpandable(engine, "above", CommandScope::MathOnly, above);
    register_unexpandable(engine, "abovewithdelims", CommandScope::MathOnly, abovewithdelims);
    register_unexpandable(engine, "atop", CommandScope::MathOnly, atop);
    register_unexpandable(engine, "atopwithdelims", CommandScope::MathOnly, atopwithdelims);

    register_unexpandable(engine, "displaystyle", CommandScope::MathOnly, displaystyle);
    register_unexpandable(engine, "textstyle", CommandScope::MathOnly, textstyle);
    register_unexpandable(engine, "scriptstyle", CommandScope::MathOnly, scriptstyle);
    register_unexpandable(engine, "scriptscriptstyle", CommandScope::MathOnly, scriptscriptstyle);

    register_unexpandable(engine,"nonscript",CommandScope::MathOnly,|_,_|()); // TODO

    register_whatsit(engine,"closeout",closeout,closeout_immediate);
    register_whatsit(engine,"openout",openout,openout_immediate);
    register_whatsit(engine,"write",write,write_immediate);

    register_box(engine,"hbox",hbox);
    register_box(engine,"vbox",vbox);
    register_box(engine,"vtop",vtop);
    register_box(engine,"box",box_);
    register_box(engine,"copy",copy);
    register_box(engine,"lastbox",lastbox);
    register_box(engine, "vsplit", vsplit);

    cmstodos!(engine,
        noalign,omit,
        pagedepth,span
    );

    cmtodos!(engine,prevgraf,insertpenalties,scrollmode,nonstopmode,batchmode,
        show,showbox,showthe,special,noboundary,accent,setlanguage,
        bigskip,bye,italiccorr,medskip,smallskip
    );

    register_primitive_int(engine,PRIMITIVE_INTS);
    register_primitive_dim(engine,PRIMITIVE_DIMS);
    register_primitive_skip(engine,PRIMITIVE_SKIPS);
    register_primitive_muskip(engine,PRIMITIVE_MUSKIPS);
    register_primitive_toks(engine,PRIMITIVE_TOKS);
}