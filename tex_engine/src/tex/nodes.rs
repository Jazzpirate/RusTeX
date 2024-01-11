pub mod vertical;
pub mod horizontal;
pub mod math;
pub mod boxes;

use std::cell::RefCell;
use std::fmt::{Debug, Display, Formatter, Write};
use std::marker::PhantomData;
use crate::engine::{EngineReferences, EngineTypes};
use crate::engine::filesystem::{SourceRef, SourceReference};
use crate::tex::types::NodeType;
use crate::engine::filesystem::File;
use crate::engine::utils::memory::{PrimitiveIdentifier, PRIMITIVES};
use crate::tex::nodes::boxes::{HBoxInfo, TeXBox, ToOrSpread, VBoxInfo};
use crate::tex::nodes::horizontal::HNode;
use crate::tex::nodes::math::{Delimiter, EqNoPosition, MathNode, UnresolvedMathFontStyle};
use crate::tex::nodes::vertical::VNode;
use crate::tex::numerics::Skip;
use crate::utils::Ptr;

#[derive(Debug,Clone,Copy,PartialEq,Eq)]
pub enum LeaderType {
    Normal,C,X
}

#[derive(Clone,Debug)]
pub enum NodeList<ET:EngineTypes> {
    Vertical{tp:VerticalNodeListType<ET>,children:Vec<VNode<ET>>},
    Horizontal{tp:HorizontalNodeListType<ET>,children:Vec<HNode<ET>>},
    Math{children:MathNodeList<ET>,start:SourceRef<ET>,tp:MathNodeListType<ET>},
}
impl<ET:EngineTypes> NodeList<ET> {
    pub fn new_math(start:SourceRef<ET>) -> Self {
        NodeList::Math{children:MathNodeList::new(),start,tp:MathNodeListType::Target(ListTarget::none())}
    }
}

#[derive(Clone,Debug)]
pub enum MathNodeList<ET:EngineTypes> {
    Simple(Vec<MathNode<ET,UnresolvedMathFontStyle<ET>>>),
    Over {
        top:Vec<MathNode<ET,UnresolvedMathFontStyle<ET>>>,
        sep:Option<ET::Dim>,
        bottom:Vec<MathNode<ET,UnresolvedMathFontStyle<ET>>>,
        left:Option<(ET::Char,UnresolvedMathFontStyle<ET>)>,
        right:Option<(ET::Char,UnresolvedMathFontStyle<ET>)>,
    },
    EqNo {
        pos:EqNoPosition,
        main:Vec<MathNode<ET,UnresolvedMathFontStyle<ET>>>,
        eqno:Vec<MathNode<ET,UnresolvedMathFontStyle<ET>>>,
    }
}
impl <ET:EngineTypes> MathNodeList<ET> {
    pub fn new() -> Self { MathNodeList::Simple(Vec::new()) }
    pub fn push(&mut self, n:MathNode<ET,UnresolvedMathFontStyle<ET>>) {
        match self {
            MathNodeList::Simple(v) => v.push(n),
            MathNodeList::Over{bottom,..} => bottom.push(n),
            MathNodeList::EqNo {eqno,..} => eqno.push(n)
        }
    }
    pub fn close(self,start:SourceRef<ET>,end:SourceRef<ET>) -> (Vec<MathNode<ET,UnresolvedMathFontStyle<ET>>>,Option<(EqNoPosition,Vec<MathNode<ET,UnresolvedMathFontStyle<ET>>>)>) {
        match self {
            MathNodeList::Simple(v) => (v,None),
            MathNodeList::Over{top,sep,bottom,left,right} => (vec!(MathNode::Over {
                start,end, top:top.into(),bottom:bottom.into(),sep,left,right
            }),None),
            MathNodeList::EqNo {main,eqno,pos} => (main,Some((pos,eqno)))
        }
    }
    pub fn list_mut(&mut self) -> &mut Vec<MathNode<ET,UnresolvedMathFontStyle<ET>>> {
        match self {
            MathNodeList::Simple(v) => v,
            MathNodeList::Over{bottom,..} => bottom,
            MathNodeList::EqNo {eqno,..} => eqno
        }
    }
    pub fn list(&self) -> &Vec<MathNode<ET,UnresolvedMathFontStyle<ET>>> {
        match self {
            MathNodeList::Simple(v) => v,
            MathNodeList::Over{bottom,..} => bottom,
            MathNodeList::EqNo {eqno,..} => eqno
        }
    }
}

#[derive(Clone,Debug)]
pub enum MathNodeListType<ET:EngineTypes> {
    Top{display:bool},
    Target(ListTarget<ET,MathNode<ET,UnresolvedMathFontStyle<ET>>>),
    LeftRight(Option<Delimiter<ET>>)
}

#[derive(Clone,Debug)]
pub enum VerticalNodeListType<ET:EngineTypes> {
    Box(VBoxInfo<ET>,SourceRef<ET>,BoxTarget<ET>),
    Insert(usize),
    VCenter(SourceRef<ET>,ToOrSpread<ET::Dim>),
    VAdjust,
    VAlignRow(SourceRef<ET>),
    VAlignCell(SourceRef<ET>,u8),
    HAlign,Page
}

#[derive(Clone,Debug)]
pub enum HorizontalNodeListType<ET:EngineTypes> {
    Paragraph(SourceReference<<ET::File as File>::SourceRefID>),
    Box(HBoxInfo<ET>,SourceRef<ET>,BoxTarget<ET>),
    VAlign,
    HAlignRow(SourceRef<ET>),
    HAlignCell(SourceRef<ET>,u8),
}

pub struct BoxTarget<ET:EngineTypes>(Option<Box<dyn FnOnce(&mut EngineReferences<ET>,TeXBox<ET>)>>);
impl<ET:EngineTypes> crate::tex::nodes::BoxTarget<ET> {
    pub fn new<F:FnOnce(&mut EngineReferences<ET>,TeXBox<ET>) + 'static>(f:F) -> Self { crate::tex::nodes::BoxTarget(Some(Box::new(f))) }
    pub fn call(self,engine: &mut EngineReferences<ET>,bx:TeXBox<ET>) {
        match self.0 {
            Some(f) => f(engine,bx),
            None => unreachable!()
        }
    }
    pub fn none() -> Self { crate::tex::nodes::BoxTarget(None) }
    pub fn is_some(&self) -> bool { self.0.is_some() }
}
impl<ET:EngineTypes> Debug for crate::tex::nodes::BoxTarget<ET> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            None => f.write_str("BoxTarget(None)"),
            Some(_) => f.write_str("BoxTarget(Some(_))")
        }
    }
}
impl<ET:EngineTypes> Clone for crate::tex::nodes::BoxTarget<ET> {
    fn clone(&self) -> Self { crate::tex::nodes::BoxTarget(None) }
}

pub struct ListTarget<ET:EngineTypes,N:NodeTrait<ET>>(Option<Box<dyn FnOnce(&mut EngineReferences<ET>,Vec<N>,SourceRef<ET>)>>);
impl<ET:EngineTypes,N:NodeTrait<ET>> ListTarget<ET,N> {
    pub fn new<F:FnOnce(&mut EngineReferences<ET>,Vec<N>,SourceRef<ET>) + 'static>(f:F) -> Self { ListTarget(Some(Box::new(f))) }
    pub fn call(self,engine: &mut EngineReferences<ET>,v:Vec<N>,start:SourceRef<ET>) {
        match self.0 {
            Some(f) => f(engine,v,start),
            None => unreachable!()
        }
    }
    pub fn none() -> Self { ListTarget(None) }
    pub fn is_some(&self) -> bool { self.0.is_some() }
}
impl<ET:EngineTypes,N:NodeTrait<ET>> Debug for ListTarget<ET,N> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            None => f.write_str("ListTarget(None)"),
            Some(_) => f.write_str("ListTarget(Some(_))")
        }
    }
}
impl<ET:EngineTypes,N:NodeTrait<ET>> Clone for ListTarget<ET,N> {
    fn clone(&self) -> Self { ListTarget(None) }
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
    fn as_math(self) -> MathNode<ET,UnresolvedMathFontStyle<ET>> { MathNode::Custom(self.into()) }
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

#[derive(Clone,Debug)]
pub enum LeaderSkip<ET:EngineTypes> {
    HSkip(ET::Skip),HFil,HFill,
    VSkip(ET::Skip), VFil,VFill
}
impl<ET:EngineTypes> LeaderSkip<ET> {
    fn is_h(&self) -> bool {
        use LeaderSkip::*;
        match self {
            HSkip(_) | HFil | HFill => true,
            _ => false
        }
    }
}

#[derive(Clone,Debug)]
pub enum LeaderBody<ET:EngineTypes> {
    Box(TeXBox<ET>),
    Rule { width:Option<ET::Dim>, height:Option<ET::Dim>, depth:Option<ET::Dim> }
}

#[derive(Clone,Debug)]
pub struct Leaders<ET:EngineTypes> {
    pub tp:LeaderType,
    pub skip:LeaderSkip<ET>,
    pub bx:LeaderBody<ET>
}
impl<ET:EngineTypes> NodeTrait<ET> for Leaders<ET> {
    fn readable_fmt(&self, indent: usize, f: &mut Formatter<'_>) -> std::fmt::Result {
        Self::readable_do_indent(indent,f)?;
        write!(f, "<leaders")?;
        match self.tp {
            LeaderType::Normal => {},
            LeaderType::C => write!(f, " type=c")?,
            LeaderType::X => write!(f, " type=x")?
        }
        match self.skip {
            LeaderSkip::HSkip(s) => write!(f, " hskip={}",s)?,
            LeaderSkip::HFil => write!(f, " hfil")?,
            LeaderSkip::HFill => write!(f, " hfill")?,
            LeaderSkip::VSkip(s) => write!(f, " vskip={}",s)?,
            LeaderSkip::VFil => write!(f, " vfil")?,
            LeaderSkip::VFill => write!(f, " vfill")?,
        }
        write!(f, ">")?;
        //self.bx.readable_fmt(indent+2, f)?;
        Self::readable_do_indent(indent,f)?;
        write!(f, "</leaders>")
    }
    fn height(&self) -> ET::Dim {
        if self.skip.is_h() {
            ET::Dim::default()//self.bx.height()
        } else {
            match self.skip {
                LeaderSkip::VSkip(s) => s.base(),
                _ => ET::Dim::default(),
            }
        }
    }
    fn width(&self) -> ET::Dim {
        if self.skip.is_h() {
            match self.skip {
                LeaderSkip::HSkip(s) => s.base(),
                _ => ET::Dim::default(),
            }
        } else {
            ET::Dim::default()//self.bx.width()
        }
    }
    fn depth(&self) -> ET::Dim {
        ET::Dim::default()
        //self.bx.depth()
    }
    fn nodetype(&self) -> NodeType {
        NodeType::Glue
    }
}