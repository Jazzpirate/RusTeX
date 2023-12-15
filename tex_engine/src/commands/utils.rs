use crate::commands::{Command, Expandable, Macro, MacroSignature, SimpleExpandable};
use crate::engine::{EngineAux, EngineReferences, EngineTypes};
use crate::engine::filesystem::FileSystem;
use crate::engine::fontsystem::{Font, FontSystem};
use crate::engine::gullet::{Gullet, ResolvedToken};
use crate::engine::gullet::methods::ACOrCS;
use crate::engine::mouth::Mouth;
use crate::engine::mouth::pretokenized::{Tokenizer, TokenList};
use crate::engine::state::State;
use crate::engine::stomach::{Stomach, StomachData};
use crate::engine::utils::memory::{MemoryManager, PrimitiveIdentifier, PRIMITIVES};
use crate::expand_loop;
use crate::tex::catcodes::{CategoryCodeScheme, CommandCode};
use crate::tex::nodes::{BoxInfo, NodeList, NodeListType, PreShipoutNode, ToOrSpread};
use crate::tex::token::Token;
use crate::tex::types::{BoxType, GroupType};
use crate::utils::HMap;
use crate::tex::control_sequences::{ControlSequenceName, ControlSequenceNameHandler};
use std::fmt::Write;
use crate::engine::mouth::strings::StringTokenizer;
use crate::tex::input_text::StringLineSource;
use crate::utils::errors::ErrorThrower;

pub fn read_register<ET:EngineTypes>(engine: &mut EngineReferences<ET>) -> u16 {
    let idx = engine.read_int(false).into();
    if idx < 0 || idx > u16::MAX.into() {
        todo!("register out of range")
    }
    idx as u16
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

pub fn make_macro<ET:EngineTypes,S1:AsRef<str>,S2:AsRef<str>>(int:&mut <ET::CSName as ControlSequenceName<ET::Char>>::Handler,scheme:&CategoryCodeScheme<ET::Char>,sig:S1,exp:S2) -> Macro<ET::Token> {
    let mut arity = 0;
    let mut params = shared_vector::Vector::new();
    let mut inparam = false;
    let mut ends_with_brace = None;
    let sig = sig.as_ref();
    let sig = if sig.is_empty() {
        MacroSignature { arity:0, params: params.into() }
    } else {
        let sigsrc: StringLineSource<ET::Char> = sig.into();
        let mut sigsrc = StringTokenizer::new(sigsrc);
        while let Some(t) = sigsrc.get_next::<ET::Token, _>(&ErrorThrower, int, scheme, None) {
            parse_sig_i::<ET>(&mut arity, &mut inparam, &mut ends_with_brace, &mut params, t);
        }
        MacroSignature { arity, params: params.into() }
    };

    let exp = exp.as_ref();
    let expsrc :StringLineSource<ET::Char> = exp.into();
    let mut expsrc = StringTokenizer::new(expsrc);
    let mut exp = shared_vector::Vector::new();
    let mut inparam = false;
    while let Some(t) = expsrc.get_next::<ET::Token,_>(&ErrorThrower,int,scheme,None) {
        parse_exp_i::<ET>(arity, &mut inparam, &mut exp, t);
    }
    if let Some(e) = ends_with_brace {
        exp.push(e);
    }
    Macro {
        long:false,outer:false,protected:false,
        expansion:exp.into(),
        signature:sig
    }
}

pub fn parse_exp_i<ET:EngineTypes>(arity:u8, inparam:&mut bool, exp:&mut shared_vector::Vector<ET::Token>, t:ET::Token) {
    if *inparam {
        *inparam = false;
        if t.is_param() {
            exp.push(t);
        } else {
            match t.char_value() {
                Some(c) => match c.try_into() {
                    Ok(u) if u > 48 && u - 49 < arity => exp.push(ET::Token::argument_marker(u-49)),
                    _ => todo!("error")
                }
                None => todo!("error")
            }
        }
    } else {
        if t.is_param() {
            *inparam = true;
        } else {
            exp.push(t);
        }
    }
}

pub fn parse_sig_i<ET:EngineTypes>(arity:&mut u8,inparam:&mut bool,ends_with_brace:&mut Option<ET::Token>,params:&mut shared_vector::Vector<ET::Token>,t:ET::Token) -> bool {
    if t.is_begin_group() {
        if *inparam {
            params.push(t.clone());
            *ends_with_brace = Some(t);
        }
        return false
    }
    if *inparam {
        *inparam = false;
        if t.is_param() {
            params.push(t);
        }
        else {
            match t.char_value() {
                Some(c) => match c.try_into() {
                    Ok(u) if u > 48 && u == 49 + *arity => params.push(ET::Token::argument_marker(*arity)),
                    _ => todo!("error")
                }
                None => todo!("error")
            }
            *arity += 1
        }
    } else {
        if t.is_param() {
            *inparam = true;
        } else {
            params.push(t);
        }
    }
    true
}

pub fn parse_signature<ET:EngineTypes>(engine:&mut EngineReferences<ET>,cm:&ACOrCS<ET::Token>)
                                       -> (MacroSignature<ET::Token>,Option<ET::Token>) {
    let mut arity = 0;
    let mut params = shared_vector::Vector::new();
    let mut inparam = false;
    let mut ends_with_brace = None;
    engine.mouth.iterate(engine.aux,engine.state.get_catcode_scheme(),engine.state.get_endline_char(),|_,t| {
        parse_sig_i::<ET>(&mut arity,&mut inparam,&mut ends_with_brace,&mut params,t)
    });
    (MacroSignature{
        arity,params:params.into()
    },ends_with_brace)
}

pub fn modify_int_register<ET:EngineTypes,O:FnOnce(ET::Int,&mut EngineReferences<ET>) -> ET::Int>(engine: &mut EngineReferences<ET>, idx:u16, globally:bool, op:O) {
    engine.read_keyword(b"by");
    let old = engine.state.get_int_register(idx);
    let new = op(old,engine);
    engine.state.set_int_register(engine.aux,idx,new,globally);
}

pub fn modify_dim_register<ET:EngineTypes,O:FnOnce(ET::Dim,&mut EngineReferences<ET>) -> ET::Dim>(engine: &mut EngineReferences<ET>, idx:u16, globally:bool, op:O) {
    engine.read_keyword(b"by");
    let old = engine.state.get_dim_register(idx);
    let new = op(old,engine);
    engine.state.set_dim_register(engine.aux,idx,new,globally);
}

pub fn modify_skip_register<ET:EngineTypes,O:FnOnce(ET::Skip,&mut EngineReferences<ET>) -> ET::Skip>(engine: &mut EngineReferences<ET>,idx:u16,globally:bool,op:O) {
    engine.read_keyword(b"by");
    let old = engine.state.get_skip_register(idx);
    let new = op(old,engine);
    engine.state.set_skip_register(engine.aux,idx,new,globally);
}

#[macro_export]
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
                    return crate::commands::utils::modify_int_register($engine,idx,$globally,$int)
                }
                Command::IntRegister(idx) => {
                    return crate::commands::utils::modify_int_register($engine,*idx,$globally,$int)
                }
                Command::Dim(DimCommand{name,..}) if *name == PRIMITIVES.dimen => {
                    let idx = $engine.read_int(false);
                    let idx = match idx.try_into() {
                        Ok(i) if i >= 0 && i <= u16::MAX as i32 => i as u16,
                        _ => todo!("throw error")
                    };
                    return crate::commands::utils::modify_dim_register($engine,idx,$globally,$dim)
                }
                Command::DimRegister(idx) => {
                    return crate::commands::utils::modify_dim_register($engine,*idx,$globally,$dim)
                }
                Command::Skip(SkipCommand{name,..}) if *name == PRIMITIVES.skip => {
                    let idx = $engine.read_int(false);
                    let idx = match idx.try_into() {
                        Ok(i) if i >= 0 && i <= u16::MAX as i32 => i as u16,
                        _ => todo!("throw error")
                    };
                    return crate::commands::utils::modify_skip_register($engine,idx,$globally,$skip)
                }
                Command::SkipRegister(idx) => {
                    return crate::commands::utils::modify_skip_register($engine,*idx,$globally,$skip)
                }
                o => todo!("{:?} in \\advance",o)
            }
            _ => todo!("throw error")
        );
        todo!("file end")
    };
}

pub fn do_box_start<ET:EngineTypes>(engine:&mut EngineReferences<ET>,tp:BoxType,every:PrimitiveIdentifier) -> ToOrSpread<ET::Dim> {
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

pub fn get_if_token<ET:EngineTypes>(engine:&mut EngineReferences<ET>) -> (Option<ET::Char>,CommandCode) {
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

pub enum IfxCmd<ET:EngineTypes> {
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
    pub fn read(engine:&mut EngineReferences<ET>) -> Self {
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

pub fn read_file_index<ET:EngineTypes>(engine:&mut EngineReferences<ET>) -> u8 {
    let idx = engine.read_int(false);
    if idx < ET::Int::default() || idx.into() > 255 {
        todo!("throw error")
    }
    idx.into() as u8
}
pub fn read_index_and_file<ET:EngineTypes>(engine:&mut EngineReferences<ET>) -> (u8,ET::File) {
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
                if u < ET::Int::default() || u.into() > u16::MAX.into() {
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