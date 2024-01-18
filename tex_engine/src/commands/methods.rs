/*! Utility methods for [`TeXCommand`]s.
*/

use crate::commands::{CharOrPrimitive, TeXCommand, Macro, MacroSignature, PrimitiveCommand, ResolvedToken};
use crate::engine::{EngineAux, EngineReferences, EngineTypes};
use crate::engine::filesystem::FileSystem;
use crate::engine::fontsystem::{Font, FontSystem};
use crate::engine::gullet::Gullet;
use crate::engine::mouth::Mouth;
use crate::tex::tokens::token_lists::{Otherize, TokenList};
use crate::engine::state::{GroupType, State};
use crate::engine::stomach::{Stomach, StomachData};
use crate::expand_loop;
use crate::tex::catcodes::CommandCode;
use crate::tex::tokens::Token;
use crate::utils::HMap;
use crate::tex::tokens::control_sequences::CSHandler;
use std::fmt::Write;
use crate::commands::primitives::{PrimitiveIdentifier, PRIMITIVES};
use crate::engine::gullet::hvalign::{AlignColumn, AlignData};
use crate::tex::nodes::boxes::{BoxType, HBoxInfo, TeXBox, ToOrSpread, VBoxInfo};
use crate::tex::nodes::{BoxTarget, LeaderBody, Leaders, LeaderSkip, LeaderType, ListTarget, NodeList};
use crate::tex::nodes::horizontal::{HNode, HorizontalNodeListType};
use crate::tex::nodes::math::{Delimiter, MathAtom, MathClass, MathKernel, MathNode, MathNucleus, UnresolvedMathFontStyle};
use crate::tex::nodes::vertical::{VerticalNodeListType, VNode};
use crate::tex::nodes::NodeTrait;
use crate::engine::stomach::TeXMode;
use crate::tex::numerics::Skip;

pub(crate) struct MacroParser<T:Token> {
    arity:u8,
    params:shared_vector::Vector<T>,
    inparam:bool,
    ends_with_brace:Option<T>,
    exp:shared_vector::Vector<T>,
}
impl<T:Token> MacroParser<T> {
    pub(crate) fn new() -> Self {
        Self {
            arity:0,
            params:shared_vector::Vector::new(),
            inparam:false,
            ends_with_brace:None,
            exp:shared_vector::Vector::new(),
        }
    }
    pub(crate) fn do_signature_token(&mut self, t:T) -> bool {
        match t.command_code() {
            CommandCode::BeginGroup => {
                if self.inparam {
                self.inparam = false;
                self.params.push(t.clone());
                self.ends_with_brace = Some(t);
                }
                return false
            }
            CommandCode::Parameter if self.inparam => {
                self.inparam = false;
                self.params.push(t);
            }
            CommandCode::Parameter => {
                self.inparam = true;
            }
            _ if self.inparam => {
                self.inparam = false;
                match t.char_value() {
                    Some(c) => match c.try_into() {
                        Ok(u) if u > 48 && u == 49 + self.arity => self.params.push(T::argument_marker(self.arity)),
                        _ => todo!("error")
                    }
                    None => todo!("error")
                }
                self.arity += 1;
            }
            _ => self.params.push(t),
        }
        true
    }

    pub(crate) fn do_expansion_token(&mut self, t:T) {
        match t.command_code() {
            CommandCode::Parameter if self.inparam => {
                self.inparam = false;
                self.exp.push(t);
            }
            CommandCode::Parameter => self.inparam = true,
            _ if self.inparam => {
                self.inparam = false;
                match t.char_value() {
                    Some(c) => match c.try_into() {
                        Ok(u) if u > 48 && u - 49 < self.arity => self.exp.push(T::argument_marker(u-49)),
                        _ => todo!("error")
                    }
                    None => todo!("error")
                }
            }
            _ => self.exp.push(t),
        }
    }

    pub(crate) fn close(mut self,long:bool,outer:bool,protected:bool) -> Macro<T> {
        if let Some(t) = self.ends_with_brace {
            self.exp.push(t);
        }
        Macro {
            long,outer,protected,
            expansion:self.exp.into(),
            signature:MacroSignature { arity:self.arity, params:self.params.into() }
        }
    }
}

pub(in crate::commands) fn modify_int_register<ET:EngineTypes,O:FnOnce(ET::Int,&mut EngineReferences<ET>) -> ET::Int>(engine: &mut EngineReferences<ET>, idx:usize, globally:bool, op:O) {
    engine.read_keyword(b"by");
    let old = engine.state.get_int_register(idx);
    let new = op(old,engine);
    engine.state.set_int_register(engine.aux,idx,new,globally);
}

pub(in crate::commands) fn modify_primitive_int<ET:EngineTypes,O:FnOnce(ET::Int,&mut EngineReferences<ET>) -> ET::Int>(engine: &mut EngineReferences<ET>, name:PrimitiveIdentifier, globally:bool, op:O) {
    engine.read_keyword(b"by");
    let old = engine.state.get_primitive_int(name);
    let new = op(old,engine);
    engine.state.set_primitive_int(engine.aux,name,new,globally);
}

pub(in crate::commands) fn modify_dim_register<ET:EngineTypes,O:FnOnce(ET::Dim,&mut EngineReferences<ET>) -> ET::Dim>(engine: &mut EngineReferences<ET>, idx:usize, globally:bool, op:O) {
    engine.read_keyword(b"by");
    let old = engine.state.get_dim_register(idx);
    let new = op(old,engine);
    engine.state.set_dim_register(engine.aux,idx,new,globally);
}

pub(in crate::commands) fn modify_primitive_dim<ET:EngineTypes,O:FnOnce(ET::Dim,&mut EngineReferences<ET>) -> ET::Dim>(engine: &mut EngineReferences<ET>, name:PrimitiveIdentifier, globally:bool, op:O) {
    engine.read_keyword(b"by");
    let old = engine.state.get_primitive_dim(name);
    let new = op(old,engine);
    engine.state.set_primitive_dim(engine.aux,name,new,globally);
}

pub(in crate::commands) fn modify_skip_register<ET:EngineTypes,O:FnOnce(Skip<ET::Dim>,&mut EngineReferences<ET>) -> Skip<ET::Dim>>(engine: &mut EngineReferences<ET>,idx:usize,globally:bool,op:O) {
    engine.read_keyword(b"by");
    let old = engine.state.get_skip_register(idx);
    let new = op(old,engine);
    engine.state.set_skip_register(engine.aux,idx,new,globally);
}

pub(in crate::commands) fn modify_primitive_skip<ET:EngineTypes,O:FnOnce(Skip<ET::Dim>,&mut EngineReferences<ET>) -> Skip<ET::Dim>>(engine: &mut EngineReferences<ET>, name:PrimitiveIdentifier, globally:bool, op:O) {
    engine.read_keyword(b"by");
    let old = engine.state.get_primitive_skip(name);
    let new = op(old,engine);
    engine.state.set_primitive_skip(engine.aux,name,new,globally);
}

pub(in crate::commands) fn do_box_start<ET:EngineTypes>(engine:&mut EngineReferences<ET>,tp:BoxType,every:PrimitiveIdentifier) -> ToOrSpread<ET::Dim> {
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
    crate::expand_loop!(engine,token,
        ResolvedToken::Tk {code:CommandCode::Space,..} => (),
        ResolvedToken::Cmd(Some(TeXCommand::Primitive{cmd:PrimitiveCommand::Relax,..})) if !ate_relax => ate_relax = true,
        ResolvedToken::Tk {code:CommandCode::BeginGroup,..} |
        ResolvedToken::Cmd(Some(TeXCommand::Char{code:CommandCode::BeginGroup,..})) => {
            engine.state.push(engine.aux,GroupType::Box(tp),engine.mouth.line_number());
            engine.push_every(every);
            return scaled
        }
        o => todo!("throw error: {:?}",o)
    );
    todo!("file end")
}

pub(in crate::commands) fn get_if_token<ET:EngineTypes>(engine:&mut EngineReferences<ET>) -> (Option<ET::Char>,CommandCode) {
    let mut exp = true;
    while let Some(token) = engine.get_next() {
        if token.is_primitive() == Some(PRIMITIVES.noexpand) {
            exp = false;
            continue
        }
        match engine.resolve(&token) {
            ResolvedToken::Tk {char,code} => return (Some(char),code),
            ResolvedToken::Cmd(cmd) => match cmd {
                Some(TeXCommand::Macro(m)) if exp =>
                    ET::Gullet::do_macro(engine,m.clone(),token),
                Some(TeXCommand::Primitive {name,cmd:PrimitiveCommand::Conditional(cond)}) if exp =>
                    ET::Gullet::do_conditional(engine,*name,token,*cond,false),
                Some(TeXCommand::Primitive {name,cmd:PrimitiveCommand::Expandable(e)}) if exp =>
                    ET::Gullet::do_expandable(engine,*name,token,*e),
                Some(TeXCommand::Primitive {name,cmd:PrimitiveCommand::SimpleExpandable(e)}) if exp =>
                    ET::Gullet::do_simple_expandable(engine,*name,token,*e),
                Some(TeXCommand::Char {char,code}, ..) => {
                    return (Some(*char),*code)
                }
                _ => return (None,CommandCode::Escape)
            },
        }
    }
    todo!("throw error")
}

pub(in crate::commands) enum IfxCmd<ET:EngineTypes> {
    Char(ET::Char,CommandCode),
    Undefined,
    Primitive(PrimitiveIdentifier),
    Noexpand(ET::Token),
    Chardef(ET::Char),
    Font(<ET::FontSystem as FontSystem>::Font),
    MathChar(u32),
    Macro(Macro<ET::Token>),
    IntRegister(usize),
    DimRegister(usize),
    SkipRegister(usize),
    MuSkipRegister(usize),
    ToksRegister(usize),
}
impl<ET:EngineTypes> IfxCmd<ET> {
    pub(in crate::commands) fn read(engine:&mut EngineReferences<ET>) -> Self {
        match engine.get_next() {
            Some(t) if t.is_primitive() == Some(PRIMITIVES.noexpand) =>
                IfxCmd::Noexpand(engine.get_next().unwrap()),
            Some(t) => Self::resolve(engine.resolve(&t)),
            _ => todo!("throw error")
        }
    }
    fn resolve(r:ResolvedToken<ET>) -> Self {
        match r {
            ResolvedToken::Tk {char,code,..} => Self::Char(char,code),
            ResolvedToken::Cmd(cmd) => match cmd {
                Some(TeXCommand::Char {char,code}) => Self::Char(*char, *code),
                None => Self::Undefined,
                Some(TeXCommand::Macro(m)) => Self::Macro(m.clone()),
                Some(TeXCommand::CharDef(c)) => Self::Chardef(*c),
                Some(TeXCommand::Font(f)) => Self::Font(f.clone()),
                Some(TeXCommand::MathChar(u)) => Self::MathChar(*u),
                Some(TeXCommand::IntRegister(u)) => Self::IntRegister(*u),
                Some(TeXCommand::DimRegister(u)) => Self::DimRegister(*u),
                Some(TeXCommand::SkipRegister(u)) => Self::SkipRegister(*u),
                Some(TeXCommand::MuSkipRegister(u)) => Self::MuSkipRegister(*u),
                Some(TeXCommand::ToksRegister(u)) => Self::ToksRegister(*u),
                Some(TeXCommand::Primitive{name,..}) => Self::Primitive(*name)
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
            (Self::Macro(m1),Self::Macro(m2)) =>
                m1.long == m2.long && m1.outer == m2.outer && m1.protected == m2.protected &&
                    m1.signature.params == m2.signature.params &&
                    m1.expansion == m2.expansion
            ,
            _ => false
        }
    }
}

pub(crate) fn do_marks<ET:EngineTypes>(engine:&mut EngineReferences<ET>,idx:usize,tk:&ET::Token) {
    let mut v = shared_vector::Vector::new();
    engine.expand_until_bgroup(false,&tk);
    engine.expand_until_endgroup(true,true,|_,_,t| v.push(t));
    let data = engine.stomach.data_mut();
    for list in data.open_lists.iter_mut().rev() {
        match list {
            NodeList::Horizontal {children,tp:HorizontalNodeListType::Paragraph(..)} => {
                children.push(HNode::Mark(idx, v.into()));
                return
            }
            NodeList::Vertical {children,..} => {
                children.push(VNode::Mark(idx, v.into()));
                return
            }
            NodeList::Math {children,..} => {
                children.push(MathNode::Mark(idx, v.into()));
                return
            }
            _ => ()
        }
    }
    data.page.push(VNode::Mark(idx, v.into()));
}

pub(crate) fn get_marks<ET:EngineTypes>(engine:&mut EngineReferences<ET>,exp:&mut Vec<ET::Token>,f:fn(&mut StomachData<ET>) -> &mut HMap<usize,TokenList<ET::Token>>,idx:usize) {
    match f(engine.stomach.data_mut()).get(&idx) {
        Some(v) => exp.extend(v.0.iter().cloned()),
        _ => ()
    }
}

pub(crate) fn do_align<ET:EngineTypes>(engine:&mut EngineReferences<ET>,inner:BoxType,between:BoxType,_to:Option<ET::Dim>,tk:&ET::Token) { // TODO use to
    engine.gullet.push_align(AlignData::dummy());
    engine.expand_until_bgroup(true,&tk);
    let data = read_align_preamble(engine,inner,between);
    *engine.gullet.get_align_data().unwrap() = data;
    ET::Stomach::open_align(engine,inner,between);
    start_align_row(engine,inner);
}

fn read_align_preamble<ET:EngineTypes>(engine:&mut EngineReferences<ET>,inner_mode:BoxType,between_mode:BoxType) -> AlignData<ET::Token,ET::Dim> {
    struct AlignmentDataBuilder<ET:EngineTypes> {
        columns: shared_vector::Vector<AlignColumn<ET::Token,ET::Dim>>,
        recindex:Option<usize>,
        current_u: shared_vector::Vector<ET::Token>,
        current_v: shared_vector::Vector<ET::Token>,
        ingroups:u8,
        in_v:bool,
        tabskip:Skip<ET::Dim>,
        inner_mode:BoxType,
        between_mode:BoxType
    }
    impl<ET:EngineTypes> AlignmentDataBuilder<ET> {
        fn push(&mut self,tk:ET::Token) {
            if self.in_v {
                self.current_v.push(tk)
            } else {
                self.current_u.push(tk)
            }
        }
    }
    impl<ET:EngineTypes> Into<AlignData<ET::Token,ET::Dim>> for AlignmentDataBuilder<ET> {
        fn into(mut self) -> AlignData<ET::Token, ET::Dim> {
            self.columns.push(AlignColumn::new(self.current_u,self.current_v,self.tabskip,self.ingroups));
            AlignData {
                columns: self.columns.into(),
                ingroups: 0,
                currindex: 0,
                repeat_index: self.recindex,
                omit:false,
                span:false,
                inner_mode:self.inner_mode,outer_mode:self.between_mode
            }
        }
    }

    let tabskip = engine.state.get_primitive_skip(PRIMITIVES.tabskip);
    let mut cols = AlignmentDataBuilder::<ET> {
        columns:shared_vector::Vector::new(),
        recindex: None,
        ingroups:0,
        current_u:shared_vector::Vector::new(),
        current_v:shared_vector::Vector::new(),
        in_v:false,
        tabskip:tabskip,
        inner_mode,between_mode
    };
    let mut ingroups = 0;

    while let Some(next) = engine.mouth.get_next_opt(engine.aux,engine.state) {
        if next.is_primitive() == Some(PRIMITIVES.noexpand) {
            if let Some(n) = engine.mouth.get_next_opt(engine.aux,engine.state) {
                cols.push(n);
            } else { unreachable!() }
            continue
        }
        match next.command_code() {
            CommandCode::BeginGroup => {
                ingroups += 1;
                cols.push(next);
                continue
            }
            CommandCode::EndGroup => {
                ingroups -= 1;
                cols.push(next);
                continue
            }
            _ => ()
        }
        match ET::Gullet::char_or_primitive(engine.state,&next) {
            Some(CharOrPrimitive::Char(_,CommandCode::Parameter)) => {
                if cols.in_v { todo!("throw error") }
                cols.in_v = true;
                cols.ingroups = ingroups;
            }
            Some(CharOrPrimitive::Char(_,CommandCode::AlignmentTab)) => {
                if ingroups != 0 {
                    todo!("throw error")
                }
                if !cols.in_v && cols.current_u.is_empty() {
                    cols.recindex = Some(cols.columns.len());
                } else {
                    let (u,v,g) = (std::mem::take(&mut cols.current_u),std::mem::take(&mut cols.current_v),std::mem::take(&mut cols.ingroups));
                    cols.columns.push(AlignColumn::new(u,v,cols.tabskip,g));
                    cols.tabskip = tabskip;
                    cols.in_v = false;
                }
            }
            Some(CharOrPrimitive::Primitive(name)) if name == PRIMITIVES.tabskip => cols.tabskip = engine.read_skip(true),
            Some(CharOrPrimitive::Primitive(name)) if name == PRIMITIVES.cr || name == PRIMITIVES.crcr => {
                engine.push_every(PRIMITIVES.everycr);
                return cols.into()
            },
            Some(CharOrPrimitive::Primitive(name)) if name == PRIMITIVES.span => {
                if let Some(t) = engine.mouth.get_next_opt(engine.aux,engine.state) {
                    engine.expand(t);
                } else {
                    todo!("File end error")
                }
            }
            _ => cols.push(next)
        }
        // engine.gullet.push_align(AlignData { ingroups: 0 });
    }
    todo!("throw file end error")
}

pub(in crate::commands) fn start_align_row<ET:EngineTypes>(engine:&mut EngineReferences<ET>,mode:BoxType) {
    if let Some(d) = engine.gullet.get_align_data() {
        d.currindex = 0
    } else { todo!("throw error") }
    // avoid the gullet here as to not throw an error on '}'!
    while let Some(token) = engine.mouth.get_next_opt(engine.aux,engine.state) {
        crate::expand!(engine,token;
            ResolvedToken::Tk{code:CommandCode::EndGroup,..} |
            ResolvedToken::Cmd(Some(TeXCommand::Char {code:CommandCode::EndGroup,..})) => {
                engine.gullet.pop_align();
                return ET::Stomach::close_align(engine)
            }
            ResolvedToken::Tk{code:CommandCode::Space,..} => (),
            ResolvedToken::Cmd(Some(TeXCommand::Primitive {name,..})) if *name == PRIMITIVES.crcr => (),
            ResolvedToken::Cmd(Some(TeXCommand::Primitive {name,..})) if *name == PRIMITIVES.noalign => {
                engine.expand_until_bgroup(true,&token);
                engine.state.push(engine.aux,GroupType::Box(mode.other()),engine.mouth.line_number());
                engine.stomach.data_mut().open_lists.push(
                    match mode {
                        BoxType::Vertical => NodeList::Horizontal {
                            children:vec!(),
                            tp:HorizontalNodeListType::Box(HBoxInfo::HAlignRow,engine.mouth.start_ref(),BoxTarget::new(
                                move |engine,l| {
                                    if let TeXBox::H {children,info:HBoxInfo::HAlignRow,..} = l  {
                                        for c in children.into_vec() {
                                            ET::Stomach::add_node_h(engine,c);
                                        }
                                    } else {unreachable!()}
                                    start_align_row(engine,mode)
                                }
                            ))
                        },
                        _ => NodeList::Vertical {
                            children:vec!(),
                            tp:VerticalNodeListType::Box(VBoxInfo::VAlignColumn,engine.mouth.start_ref(),BoxTarget::new(
                                move |engine,l| {
                                    if let TeXBox::V {children,info:VBoxInfo::VAlignColumn,..} = l  {
                                        for c in children.into_vec() {
                                            ET::Stomach::add_node_v(engine,c);
                                        }
                                    } else {unreachable!()}
                                    start_align_row(engine,mode)
                                }
                            ))
                        }
                    }
                );
                return
            }
            ResolvedToken::Cmd(Some(TeXCommand::Primitive {name,..})) if *name == PRIMITIVES.omit => {
                engine.stomach.data_mut().open_lists.push(
                    match mode {
                        BoxType::Vertical => NodeList::Vertical {
                            children:vec!(),
                            tp:VerticalNodeListType::VAlignColumn(engine.mouth.start_ref())
                        },
                        _ => NodeList::Horizontal {
                            children:vec!(),
                            tp:HorizontalNodeListType::HAlignRow(engine.mouth.start_ref())
                        }
                    }
                );
                engine.gullet.get_align_data().unwrap().omit = true;
                return open_align_cell(engine,mode)
            }
            ResolvedToken::Tk{..} | ResolvedToken::Cmd(_) => {
                engine.stomach.data_mut().open_lists.push(
                    match mode {
                        BoxType::Vertical => NodeList::Vertical {
                            children:vec!(),
                            tp:VerticalNodeListType::VAlignColumn(engine.mouth.start_ref())
                        },
                        _ => NodeList::Horizontal {
                            children:vec!(),
                            tp:HorizontalNodeListType::HAlignRow(engine.mouth.start_ref())
                        }
                    }
                );
                engine.mouth.requeue(token);
                return open_align_cell(engine,mode)
            }
        );
    }
    todo!("file end")
}

pub(in crate::commands) fn open_align_cell<ET:EngineTypes>(engine:&mut EngineReferences<ET>,mode:BoxType) {
    match engine.gullet.get_align_data() {
        None => todo!("throw error"),
        Some(data) => {
            if !data.omit { engine.mouth.push_exp(&data.columns[data.currindex].left); }
            if data.span {
                data.span = false
            } else {
                engine.state.push(engine.aux, GroupType::Box(mode), engine.mouth.line_number());
                engine.stomach.data_mut().open_lists.push(
                    match mode {
                        BoxType::Vertical => NodeList::Vertical {
                            children: vec!(),
                            tp: VerticalNodeListType::VAlignCell(engine.mouth.start_ref(), 0)
                        },
                        _ => NodeList::Horizontal {
                            children: vec!(),
                            tp: HorizontalNodeListType::HAlignCell(engine.mouth.start_ref(), 0)
                        }
                    }
                )
            }
        }
    }
}

pub(in crate::commands) fn pop_align_cell<ET:EngineTypes>(state:&mut ET::State,aux:&mut EngineAux<ET>,stomach:&mut ET::Stomach,mouth:&mut ET::Mouth,inner_mode:BoxType) {
    match inner_mode {
        BoxType::Vertical => pop_align_cell_v(state,aux,stomach,mouth),
        _ => pop_align_cell_h(state,aux,stomach,mouth)
    }
}

fn pop_align_cell_v<ET:EngineTypes>(state:&mut ET::State,aux:&mut EngineAux<ET>,stomach:&mut ET::Stomach,mouth:&mut ET::Mouth) {
    let (children,start,spans) = match stomach.data_mut().open_lists.pop() {
        Some(NodeList::Vertical {children,tp:VerticalNodeListType::VAlignCell(start,i)}) => (children,start,i),
        _ => todo!("throw error")
    };
    state.pop(aux,mouth);
    let bx = TeXBox::V {
        children:children.into(),start,
        info: VBoxInfo::VAlignCell {to:None,spans},
        end: mouth.current_sourceref(),
    };
    match stomach.data_mut().open_lists.last_mut() {
        Some(NodeList::Vertical {children,tp:VerticalNodeListType::VAlignColumn(..)}) =>
            children.push(VNode::Box(bx)),
        _ => todo!("throw error")
    }
}
fn pop_align_cell_h<ET:EngineTypes>(state:&mut ET::State,aux:&mut EngineAux<ET>,stomach:&mut ET::Stomach,mouth:&mut ET::Mouth) {
    let (children,start,spans) = match stomach.data_mut().open_lists.pop() {
        Some(NodeList::Horizontal {children,tp:HorizontalNodeListType::HAlignCell(start,i)}) => (children,start,i),
        _ => todo!("throw error")
    };
    state.pop(aux,mouth);
    let bx = TeXBox::H {
        children:children.into(),start,
        info: HBoxInfo::new_cell(spans),
        end: mouth.current_sourceref(),preskip:None
    };
    match stomach.data_mut().open_lists.last_mut() {
        Some(NodeList::Horizontal {children,tp:HorizontalNodeListType::HAlignRow(..)}) =>
            children.push(HNode::Box(bx)),
        _ => todo!("throw error")
    }
}

pub(in crate::commands) fn pop_align_row<ET:EngineTypes>(stomach:&mut ET::Stomach,mouth:&mut ET::Mouth,inner_mode:BoxType) {
    match inner_mode {
        BoxType::Vertical => pop_align_row_v::<ET>(stomach,mouth),
        _ => pop_align_row_h::<ET>(stomach,mouth)
    }
}

fn pop_align_row_v<ET:EngineTypes>(stomach:&mut ET::Stomach,mouth:&mut ET::Mouth) {
    let (children,start) = match stomach.data_mut().open_lists.pop() {
        Some(NodeList::Vertical{children,tp:VerticalNodeListType::VAlignColumn(start)}) => (children, start),
        _ => todo!("throw error")
    };
    let bx = TeXBox::V {
        children:children.into(),start,
        info: VBoxInfo::VAlignColumn,
        end: mouth.current_sourceref(),
    };
    match stomach.data_mut().open_lists.last_mut() {
        Some(NodeList::Horizontal {children,tp:HorizontalNodeListType::VAlign}) =>
            children.push(HNode::Box(bx)),
        _ => todo!("throw error")

    }
}

fn pop_align_row_h<ET:EngineTypes>(stomach:&mut ET::Stomach,mouth:&mut ET::Mouth) {
    let (children,start) = match stomach.data_mut().open_lists.pop() {
        Some(NodeList::Horizontal{children,tp:HorizontalNodeListType::HAlignRow(start)}) => (children,start),
        _ => todo!("throw error")
    };
    let bx = TeXBox::H {
        children:children.into(),start,
        info: HBoxInfo::HAlignRow,
        end: mouth.current_sourceref(),preskip:None
    };
    match stomach.data_mut().open_lists.last_mut() {
        Some(NodeList::Vertical {children,tp:VerticalNodeListType::HAlign}) =>
            children.push(VNode::Box(bx)),
        _ => todo!("throw error")

    }
}

pub(crate) const END_TEMPLATE: &str = "!\"$%&/(endtemplate)\\&%$\"!";
pub(crate) const END_TEMPLATE_ROW: &str = "!\"$%&/(endtemplate_row)\\&%$\"!";

pub(crate) fn un_x<ET:EngineTypes>(engine:&mut EngineReferences<ET>,v:fn(&VNode<ET>) -> bool,h:fn(&HNode<ET>) -> bool,m:fn(&MathNode<ET,UnresolvedMathFontStyle<ET>>) -> bool) {
    let data = engine.stomach.data_mut();
    match data.open_lists.last_mut() {
        None => (),//todo!("throw error: Not allowed in vertical"), <- not entirely true; if there's contributed stuff not yet migrated to the current page, it's allowed
        Some(NodeList::Vertical {children,..}) => {
            let mut readd = arrayvec::ArrayVec::<VNode<ET>,10>::new();
            loop {
                match children.last_mut() {
                    Some(n) if v(n) => { children.pop();break }
                    Some(n) if n.opaque() => {
                        readd.push(children.pop().unwrap());
                    }
                    _ => break
                }
            }
            for n in readd.into_iter().rev() {
                children.push(n);
            }
        }
        Some(NodeList::Horizontal {children,..}) => {
            let mut readd = arrayvec::ArrayVec::<HNode<ET>,10>::new();
            loop {
                match children.last_mut() {
                    Some(n) if h(n) => { children.pop();break }
                    Some(n) if n.opaque() => {
                        readd.push(children.pop().unwrap());
                    }
                    _ => break
                }
            }
            for n in readd.into_iter().rev() {
                children.push(n);
            }
        }
        Some(NodeList::Math {children,..}) => {
            let mut readd = arrayvec::ArrayVec::<_,10>::new();
            loop {
                match children.list_mut().last_mut() {
                    Some(n) if m(n) => { children.list_mut().pop();break }
                    Some(n) if n.opaque() => {
                        readd.push(children.list_mut().pop().unwrap());
                    }
                    _ => break
                }
            }
            for n in readd.into_iter().rev() {
                children.push(n);
            }
        }
    }
}


pub(crate) fn last_x<R,ET:EngineTypes>(engine:&mut EngineReferences<ET>,v:fn(&VNode<ET>) -> Option<R>,h:fn(&HNode<ET>) -> Option<R>,m:fn(&MathNode<ET,UnresolvedMathFontStyle<ET>>) -> Option<R>) -> Option<R> {
    let data = engine.stomach.data_mut();
    match data.open_lists.last_mut() {
        None => for n in data.page.iter().rev() {
            if n.opaque() {continue}
            return v(n)
        }
        Some(NodeList::Vertical {children,..}) => {
            for n in children.iter().rev() {
                if n.opaque() {continue}
                return v(n)
            }
        }
        Some(NodeList::Horizontal {children,..}) => {
            for n in children.iter().rev() {
                if n.opaque() {continue}
                return h(n)
            }
        }
        Some(NodeList::Math {children,..}) => {
            for n in children.list_mut().iter().rev() {
                if n.opaque() {continue}
                return m(n)
            }
        }
    }
    None
}

pub(crate) fn do_leaders<ET:EngineTypes>(engine:&mut EngineReferences<ET>,tp:LeaderType) {
    match engine.read_keywords(&[b"width",b"height",b"depth"]) {
        Some(_) => todo!("leaders with dimensions"),
        _ => crate::expand_loop!(engine,token,
            ResolvedToken::Cmd(Some(TeXCommand::Primitive {cmd:PrimitiveCommand::Box(read),..})) => {
                match read(engine,token) {
                    Ok(None) => todo!(),
                    Ok(Some(bx)) => return leaders_skip(engine,LeaderBody::Box(bx),tp),
                    Err(bi) => {
                        let target = BoxTarget::<ET>::new(move |e,b| leaders_skip(e,LeaderBody::Box(b),tp));
                        let mut ls = bi.open_list(engine.mouth.start_ref());
                        match ls {
                            NodeList::Horizontal {tp:HorizontalNodeListType::Box(_,_,ref mut t),..} => *t = target,
                            NodeList::Vertical {tp:VerticalNodeListType::Box(_,_,ref mut t),..} => *t = target,
                            _ => unreachable!()
                        }
                        engine.stomach.data_mut().open_lists.push(ls);
                        return ()
                    }
                }
            }
            ResolvedToken::Cmd(Some(TeXCommand::Primitive {name,..})) if *name == PRIMITIVES.hrule || *name == PRIMITIVES.vrule => {
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
                return leaders_skip(engine,LeaderBody::Rule {width,height,depth},tp)
            }
            _ => todo!("throw error")
        )
    }
    todo!("file end")
}

fn leaders_skip<ET:EngineTypes>(engine:&mut EngineReferences<ET>, body:LeaderBody<ET>, tp:LeaderType) {
    crate::expand_loop!(engine,token,
        ResolvedToken::Cmd(Some(TeXCommand::Primitive{name,..})) => {
            let skip = match *name {
                n if n == PRIMITIVES.vskip => LeaderSkip::VSkip(engine.read_skip(false)),
                n if n == PRIMITIVES.hskip => LeaderSkip::HSkip(engine.read_skip(false)),
                n if n == PRIMITIVES.vfil => LeaderSkip::VFil,
                n if n == PRIMITIVES.hfil => LeaderSkip::HFil,
                n if n == PRIMITIVES.vfill => LeaderSkip::VFill,
                n if n == PRIMITIVES.hfill => LeaderSkip::HFill,
                _ => todo!("throw error")
            };
            let leaders = Leaders {skip,body,tp};
            crate::add_node!(ET::Stomach;engine,VNode::Leaders(leaders),HNode::Leaders(leaders),MathNode::Leaders(leaders));
            return
        }
        _ => todo!("throw error")
    );
    todo!("file end")
}

pub(crate) fn do_math_class<ET:EngineTypes>(engine:&mut EngineReferences<ET>,cls:Option<MathClass>) {
    engine.read_char_or_math_group(|_,engine,mc| ET::Stomach::add_node_m(engine,MathNode::Atom(mc.to_atom())),
                                   move |_| ListTarget::<ET,_>::new(move |engine,children,start| {
        let node = MathNode::Atom(MathAtom {
            sub:None, sup:None, nucleus:match cls {
                None => MathNucleus::Inner(MathKernel::List {start,children:children.into(),end:engine.mouth.current_sourceref()}),
                Some(cls) => MathNucleus::Simple{
                    kernel:MathKernel::List {start,children:children.into(),end:engine.mouth.current_sourceref()},
                    cls,limits:None
                },
            },
        });
        ET::Stomach::add_node_m(engine,node);
    }),())
}

impl<ET:EngineTypes> EngineReferences<'_,ET> {
    /// expands the [`Token`] if it is expandable, otherwise requeues it
    pub fn expand(&mut self,token:ET::Token) {
        match self.resolve(&token) {
            ResolvedToken::Cmd(Some(cmd)) => match cmd {
                TeXCommand::Macro(m) => ET::Gullet::do_macro(self, m.clone(), token),
                TeXCommand::Primitive{name,cmd:PrimitiveCommand::Conditional(cond)} => ET::Gullet::do_conditional(self, *name, token, *cond, false),
                TeXCommand::Primitive{name,cmd:PrimitiveCommand::Expandable(expand)} => ET::Gullet::do_expandable(self, *name, token, *expand),
                TeXCommand::Primitive{name,cmd:PrimitiveCommand::SimpleExpandable(exp)} => ET::Gullet::do_simple_expandable(self, *name, token, *exp),
                _ => self.requeue(token)
            }
            _ => self.requeue(token)
        }
    }

    /// reads an integer from the input stream and makes sure it's in the range of
    /// a state register
    pub fn read_register_index(&mut self,skip_eq:bool) -> usize {
        let idx = self.read_int(skip_eq);
        match ET::State::register_index(idx) {
            Some(idx) => idx,
            None => todo!("throw error")
        }
    }
    /// reads an integer and makes sure it's in the range of a math font index (0-15)
    pub fn mathfont_index(&mut self,skip_eq:bool) -> u8 {
        let idx = self.read_int(skip_eq).into();
        if idx < 0 || idx > 15 {
            todo!("throw error")
        }
        idx as u8
    }
    /// expects a [`BeginGroup`](CommandCode::BeginGroup) token, reads until the
    /// matching [`EndGroup`](CommandCode::EndGroup) token and discards everything
    /// in between.
    pub fn skip_argument(&mut self) {
        match self.get_next() {
            Some(t) if t.command_code() == CommandCode::BeginGroup => (),
            _ => todo!("throw error")
        }
        self.read_until_endgroup(|_,_,_| {});
    }

    /// reads the name of a control sequence until `\endcsname` and returns the
    /// corresponding [`CSName`](crate::tex::tokens::control_sequences::CSName) (i.e. what `\csname` and
    /// `\ifcsname` do)
    pub fn read_csname(&mut self) -> ET::CSName {
        *self.gullet.csnames() += 1;
        let mut namev = vec!();
        crate::expand_loop!(self,token,
            ResolvedToken::Tk {char,..} => namev.push(char),
            ResolvedToken::Cmd(Some(TeXCommand::Primitive {name,..})) if *name == PRIMITIVES.endcsname => {
                *self.gullet.csnames() -= 1;
                let id = self.aux.memory.cs_interner_mut().from_chars(&namev);
                //engine.aux.memory.return_string(name);
                return id
            }
            ResolvedToken::Cmd(_) => {
                todo!("csname: {}",token.display(self.aux.memory.cs_interner(),self.state.get_catcode_scheme(),self.state.get_escape_char()))
            }
        );
        todo!("file end")
    }
    /// reads a number from the input stream and makes sure it's in the range of
    /// a file input/output stream index (0-255)
    pub fn read_file_index(&mut self) -> u8 {
        let idx = self.read_int(false).into();
        if idx < 0 || idx > 255 {
            todo!("throw error")
        }
        idx as u8
    }

    /// reads a number from the input stream, making sure it's in the range of
    /// a file input/output stream index (0-255), and subsequently reads a filename
    /// - i.e. the first two steps of `\openin` or `\openout`
    pub fn read_filename_and_index(&mut self) -> (u8,ET::File) {
        let idx = self.read_file_index();
        let mut filename = self.aux.memory.get_string();
        self.read_string(true,&mut filename);
        if filename.is_empty() {
            todo!("throw error")
        }
        let file = self.filesystem.get(&filename);
        self.aux.memory.return_string(filename);
        (idx,file)
    }

    /// `\the`, but using a continuation function; this is used for both [`the`](super::tex::the)
    /// as well as in [`expand_until_endgroup`](Self::expand_until_endgroup)
    /// to speed things up
    pub fn do_the<F:FnMut(&mut EngineAux<ET>,&ET::State,&mut ET::Gullet,ET::Token)>(&mut self,mut cont:F) {
        expand_loop!(self,token,
            ResolvedToken::Cmd(Some(c)) => match c {
                TeXCommand::Primitive{cmd:PrimitiveCommand::Int{read,..},..} => {
                    let val = read(self,token);
                    write!(Otherize::new(&mut |t| cont(self.aux,self.state,self.gullet,t)),"{}",val).unwrap();
                    return ()
                }
                TeXCommand::Primitive{cmd:PrimitiveCommand::Dim{read,..},..} => {
                    let val = read(self,token);
                    write!(Otherize::new(&mut |t| cont(self.aux,self.state,self.gullet,t)),"{}",val).unwrap();
                    return ()
                }
                TeXCommand::Primitive{cmd:PrimitiveCommand::Skip{read,..},..} => {
                    let val = read(self,token);
                    write!(Otherize::new(&mut |t| cont(self.aux,self.state,self.gullet,t)),"{}",val).unwrap();
                    return ()
                }
                TeXCommand::Primitive{cmd:PrimitiveCommand::MuSkip{read,..},..} => {
                    let val = read(self,token);
                    write!(Otherize::new(&mut |t| cont(self.aux,self.state,self.gullet,t)),"{}",val).unwrap();
                    return ()
                }
                TeXCommand::IntRegister(u) => {
                    let val = self.state.get_int_register(*u);
                    write!(Otherize::new(&mut |t| cont(self.aux,self.state,self.gullet,t)),"{}",val).unwrap();
                    return ()
                }
                TeXCommand::DimRegister(u) => {
                    let val = self.state.get_dim_register(*u);
                    write!(Otherize::new(&mut |t| cont(self.aux,self.state,self.gullet,t)),"{}",val).unwrap();
                    return ()
                }
                TeXCommand::SkipRegister(u) => {
                    let val = self.state.get_skip_register(*u);
                    write!(Otherize::new(&mut |t| cont(self.aux,self.state,self.gullet,t)),"{}",val).unwrap();
                    return ()
                }
                TeXCommand::MuSkipRegister(u) => {
                    let val = self.state.get_muskip_register(*u);
                    write!(Otherize::new(&mut |t| cont(self.aux,self.state,self.gullet,t)),"{}",val).unwrap();
                    return ()
                }
                TeXCommand::Primitive{cmd:PrimitiveCommand::PrimitiveInt,name} => {
                    let val = self.state.get_primitive_int(*name);
                    write!(Otherize::new(&mut |t| cont(self.aux,self.state,self.gullet,t)),"{}",val).unwrap();
                    return ()
                }
                TeXCommand::Primitive{cmd:PrimitiveCommand::PrimitiveDim,name} => {
                    let val = self.state.get_primitive_dim(*name);
                    write!(Otherize::new(&mut |t| cont(self.aux,self.state,self.gullet,t)),"{}",val).unwrap();
                    return ()
                }
                TeXCommand::Primitive{cmd:PrimitiveCommand::PrimitiveSkip,name} => {
                    let val = self.state.get_primitive_skip(*name);
                    write!(Otherize::new(&mut |t| cont(self.aux,self.state,self.gullet,t)),"{}",val).unwrap();
                    return ()
                }
                TeXCommand::Primitive{cmd:PrimitiveCommand::PrimitiveMuSkip,name} => {
                    let val = self.state.get_primitive_muskip(*name);
                    write!(Otherize::new(&mut |t| cont(self.aux,self.state,self.gullet,t)),"{}",val).unwrap();
                    return ()
                }
                TeXCommand::CharDef(c) => {
                    let val : u64 = (*c).into();
                    write!(Otherize::new(&mut |t| cont(self.aux,self.state,self.gullet,t)),"{}",val).unwrap();
                    return ()
                }
                TeXCommand::MathChar(u) => {
                    write!(Otherize::new(&mut |t| cont(self.aux,self.state,self.gullet,t)),"{}",u).unwrap();
                    return ()
                }
                TeXCommand::ToksRegister(u) => {
                    for t in &self.state.get_toks_register(*u).0 {
                        cont(self.aux,self.state,self.gullet,t.clone())
                    }
                    return ()
                }
                TeXCommand::Primitive{name,..} if *name == PRIMITIVES.toks => {
                    let u = self.read_register_index(false);
                    for t in &self.state.get_toks_register(u).0 {
                        cont(self.aux,self.state,self.gullet,t.clone())
                    }
                    return ()
                }
                TeXCommand::Primitive{cmd:PrimitiveCommand::PrimitiveToks,name} => {
                    for t in &self.state.get_primitive_tokens(*name).0 {
                        cont(self.aux,self.state,self.gullet,t.clone())
                    }
                    return ()
                }
                TeXCommand::Font(fnt) => {
                    let t = fnt.name();
                    cont(self.aux,self.state,self.gullet,ET::Token::from_cs(t.clone()));
                    return ()
                }
                TeXCommand::Primitive{cmd:PrimitiveCommand::FontCmd{read,..},..} => {
                    let fnt = read(self,token);
                    let t = fnt.name();
                    cont(self.aux,self.state,self.gullet,ET::Token::from_cs(t.clone()));
                    return ()
                }
                o => todo!("Here: {:?} in \\the - {}",o,self.mouth.current_sourceref().display(self.filesystem))
            }
            o => todo!("{:?} in \\the",o)
        );
    }

    /// reads a [`Delimiter`] from the input stream;
    /// e.g. from `\delimiter` or the `\delcode` of the next character
    pub fn read_opt_delimiter(&mut self) -> Option<Delimiter<ET>> {
        crate::expand_loop!(self,token,
            ResolvedToken::Cmd(Some(TeXCommand::Primitive {name,..}))  if *name == PRIMITIVES.delimiter => {
                let num = self.read_int(false);
                return Some(Delimiter::from_int(num,self.state))
            }
            ResolvedToken::Tk{char,code:CommandCode::Letter|CommandCode::Other,..} => {
                let num = self.state.get_delcode(char);
                if num == ET::Int::default() {return None} else {
                    return Some(Delimiter::from_int(num,self.state))
                };
            }
            o => todo!("??? {:?}",o)
        );
        todo!("file end")
    }
}