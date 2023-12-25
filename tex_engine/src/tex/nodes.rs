pub mod vertical;
pub mod horizontal;
pub mod math;
pub mod boxes;

use std::cell::RefCell;
use std::fmt::{Debug, Display, Formatter, Write};
use std::marker::PhantomData;
use crate::engine::{EngineReferences, EngineTypes};
use crate::engine::filesystem::SourceReference;
use crate::tex::types::{MathClass, MathStyleType, NodeType};
use crate::engine::filesystem::File;
use crate::engine::fontsystem::{Font, FontSystem};
use crate::engine::mouth::pretokenized::TokenList;
use crate::engine::stomach::ParLineSpec;
use crate::engine::utils::memory::{PrimitiveIdentifier, PRIMITIVES};
use crate::tex::input_text::Character;
use crate::tex::nodes::boxes::{HBoxInfo, VBoxInfo};
use crate::tex::nodes::horizontal::HNode;
use crate::tex::nodes::math::{MathNode, UnresolvedMathFontStyle};
use crate::tex::nodes::vertical::VNode;
use crate::tex::numerics::{MuSkip, Skip, TeXDimen};
use crate::utils::Ptr;
use crate::tex::numerics::NumSet;

type SR<ET> = SourceReference<<<ET as EngineTypes>::File as File>::SourceRefID>;

#[derive(Clone,Debug)]
pub enum NodeList<ET:EngineTypes> {
    Vertical{tp:VerticalNodeListType<ET>,children:Vec<VNode<ET>>},
    Horizontal{tp:HorizontalNodeListType<ET>,children:Vec<HNode<ET>>},
    Math{children:Vec<MathNode<ET,UnresolvedMathFontStyle<ET::Font>>>,start:SR<ET>,top_display:Option<bool>},
}
impl<ET:EngineTypes> NodeList<ET> {
    pub fn new_math(start:SR<ET>) -> Self {
        NodeList::Math{children:Vec::new(),start,top_display:None}
    }
}

#[derive(Clone,Debug)]
pub enum VerticalNodeListType<ET:EngineTypes> {
    Box(VBoxInfo<ET>,SR<ET>,BoxTarget),
    VCenter(SR<ET>),
    VAdjust,
    VAlignRow(SR<ET>),
    VAlignCell(SR<ET>),
    HAlign
}

#[derive(Clone,Debug)]
pub enum HorizontalNodeListType<ET:EngineTypes> {
    Paragraph(SourceReference<<ET::File as File>::SourceRefID>),
    Box(HBoxInfo<ET>,SR<ET>,BoxTarget),
    VAlign,
    HAlignRow(SR<ET>),
    HAlignCell(SR<ET>),
}

#[derive(Clone,Debug)]
pub enum BoxTarget {
    Register{ index:u16, globally:bool },
    List,
    Out
}

pub trait NodeTrait<ET:EngineTypes>:Debug+Clone {
    fn height(&self) -> ET::Dim;
    fn depth(&self) -> ET::Dim;
    fn width(&self) -> ET::Dim;
    fn nodetype(&self) -> NodeType;
    fn readable_fmt(&self, indent:usize, f:&mut std::fmt::Formatter<'_>) -> std::fmt::Result;
    #[inline(always)]
    fn readable_do_indent(indent:usize,f:&mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char('\n')?;
        for _ in 0..indent {f.write_char(' ')?;}
        Ok(())
    }
    fn readable(&self) -> ReadableNode<ET,Self> where Self:Sized {
        ReadableNode(self,PhantomData)
    }
    fn opaque(&self) -> bool { false }
}

pub trait CustomNodeTrait<ET:EngineTypes>:NodeTrait<ET> where Self:Into<ET::CustomNode> {
    fn as_v(self) -> VNode<ET> { VNode::Custom(self.into()) }
    fn as_h(self) -> HNode<ET> { HNode::Custom(self.into()) }
    fn as_math(self) -> MathNode<ET,UnresolvedMathFontStyle<ET::Font>> { MathNode::Custom(self.into()) }
}

impl<ET:EngineTypes<CustomNode = ()>> NodeTrait<ET> for () {
    fn height(&self) -> ET::Dim { ET::Dim::default() }
    fn depth(&self) -> ET::Dim { ET::Dim::default() }
    fn width(&self) -> ET::Dim { ET::Dim::default() }
    fn nodetype(&self) -> NodeType { NodeType::WhatsIt }
    fn readable_fmt(&self, _indent:usize, _f:&mut std::fmt::Formatter<'_>) -> std::fmt::Result { Ok(()) }
    fn opaque(&self) -> bool { true }
}
impl<ET:EngineTypes<CustomNode = ()>> CustomNodeTrait<ET> for () {}

pub struct ReadableNode<'a,ET:EngineTypes,N: NodeTrait<ET>>(&'a N, PhantomData<ET>);
impl<'a,ET:EngineTypes,N: NodeTrait<ET>> Display for ReadableNode<'a,ET,N> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.readable_fmt(0, f)
    }
}

pub type WhatsitFunction<ET> = Ptr<RefCell<Option<Box<dyn FnOnce(&mut EngineReferences<ET>)>>>>;
#[derive(Clone)]
pub struct WhatsitNode<ET:EngineTypes>(String,WhatsitFunction<ET>);
impl<ET:EngineTypes> std::fmt::Debug for WhatsitNode<ET> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f,"<whatsit {}>",self.0)
    }
}
impl<ET:EngineTypes> WhatsitNode<ET> {
    pub fn new(f:Box<dyn FnOnce(&mut EngineReferences<ET>)>, name:PrimitiveIdentifier) -> Self {
        WhatsitNode(PRIMITIVES.printable::<ET::Char>(name, None).to_string(),
                                     Ptr::new(RefCell::new(Some(f)))
        )
    }
    pub fn call(self,engine: &mut EngineReferences<ET>) {
        if let Some(f) = self.1.replace(None) {
            f(engine);
        }
    }
}