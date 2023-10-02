//! Boxes producing actual output

use std::fmt::{Debug, Formatter};
use std::marker::PhantomData;
use crate::engine::{EngineRef, EngineType};
use crate::engine::state::modes::BoxMode;
use crate::tex::commands::CloseBoxFun;
use crate::tex::fonts::{Font, FontStore};
use crate::tex::numbers::Skip;
use crate::tex::token::Token;
use crate::utils::errors::TeXError;
use crate::utils::{Mut, Ptr};
use crate::tex::numbers::Dim;

pub trait NodeTrait<ET:EngineType> {
    fn as_node(self) -> TeXNode<ET>;
    fn height(&self,fs:&ET::FontStore) -> ET::Dim;
    fn depth(&self,fs:&ET::FontStore) -> ET::Dim;
    fn width(&self,fs:&ET::FontStore) -> ET::Dim;
    fn nodetype(&self) -> u8;
}

#[derive(Debug,Clone)]
pub enum TeXNode<ET:EngineType> {
    Skip(SkipNode<ET>),
    Penalty(i32),
    Kern{ dim:ET::Dim,axis:HorV},
    Box(HVBox<ET>),
    Math{ls:Vec<TeXNode<ET>>,display:bool},
    OpenKernel(OpenKernel<ET>),
    Whatsit(Whatsit<ET>),
    Mark(usize,Vec<ET::Token>),
    Insert(usize,Vec<TeXNode<ET>>),
    Custom(ET::Node),
    Simple(SimpleNode<ET>),
    VAdjust(Vec<TeXNode<ET>>)
}

#[derive(Debug,Clone)]
pub enum OpenKernel<ET:EngineType> {
    Superscript(Box<TeXNode<ET>>),Subscript(Box<TeXNode<ET>>)
}
impl<ET:EngineType> OpenKernel<ET> {
    pub fn base(&self) -> &TeXNode<ET> {
        match self {
            OpenKernel::Superscript(b) => &**b,
            OpenKernel::Subscript(b) => &**b
        }
    }
    pub fn merge(self, next: TeXNode<ET>,limits:bool) -> TeXNode<ET> {
        match self {
            OpenKernel::Superscript(b) => match *b {
                TeXNode::Simple(SimpleNode::WithScripts {kernel,subscript,..}) =>
                    SimpleNode::WithScripts {kernel,subscript,superscript:Some(Box::new(next)),limits}.as_node(),
                _ =>
                    SimpleNode::WithScripts {kernel:b,subscript:None,superscript:Some(Box::new(next)),limits}.as_node()
            }
            OpenKernel::Subscript(b) => match *b {
                TeXNode::Simple(SimpleNode::WithScripts {kernel,superscript,..}) =>
                    SimpleNode::WithScripts {kernel,subscript:Some(Box::new(next)),superscript,limits}.as_node(),
                _ =>
                    SimpleNode::WithScripts {kernel:b,subscript:Some(Box::new(next)),superscript:None,limits}.as_node(),
            }
        }
    }
}


impl<ET:EngineType> NodeTrait<ET> for TeXNode<ET> {
    fn height(&self,fs:&ET::FontStore) -> ET::Dim {
        use TeXNode::*;
        match self {
            Skip (s) => s.height(fs),
            Kern { dim: val,axis:HorV::Vertical} => *val,
            Box(b) => b.height(fs),
            Custom(c) => c.height(fs),
            Math{ls,..} => ls.iter().map(|n| n.height(fs)).max().unwrap_or_else(|| ET::Dim::from_sp(0)),
            Simple(s) => s.height(fs),
            _ => ET::Dim::from_sp(0)
        }
    }
    fn width(&self,fs:&ET::FontStore) -> ET::Dim {
        use TeXNode::*;
        match self {
            Skip(s) => s.width(fs),
            Kern { dim: val,axis:HorV::Horizontal} => *val,
            Box(b) => b.width(fs),
            Custom(c) => c.width(fs),
            Math{ls,..} => ls.iter().map(|n| n.width(fs)).sum(),
            Simple(s) => s.width(fs),
            _ => ET::Dim::from_sp(0)
        }
    }
    fn depth(&self,fs:&ET::FontStore) -> ET::Dim {
        use TeXNode::*;
        match self {
            Box(b) => b.depth(fs),
            Custom(c) => c.depth(fs),
            Simple(s) => s.depth(fs),
            _ => ET::Dim::from_sp(0)
        }
    }
    fn as_node(self) -> TeXNode<ET> { self }
    fn nodetype(&self) -> u8 {
        use TeXNode::*;
        match self {
            Skip(_) => 11,
            Penalty(_) => 13,
            Kern{ ..} => 12,
            Box(b) =>b.nodetype(),
            Whatsit(_) => 9,
            Math{..} => 10,
            Mark(_,_) => 5,
            Insert(_,_) => 4,
            Custom(n) => n.nodetype(),
            Simple(n) => n.nodetype(),
            VAdjust(_) => 6,
            OpenKernel(k) => k.base().nodetype()
        }
    }
}

#[derive(Debug,Clone)]
pub enum SkipNode<ET:EngineType> {
    Skip{ skip:Skip<ET::SkipDim>,axis:HorV},
    Space,
    VFil,VFill,VFilneg,HFil,HFill,HFilneg,Hss,Vss
}

impl<ET:EngineType> NodeTrait<ET> for SkipNode<ET> {
    fn as_node(self) -> TeXNode<ET> {
        TeXNode::Skip(self)
    }
    fn depth(&self,fs:&ET::FontStore) -> ET::Dim {
        ET::Dim::from_sp(0)
    }
    fn height(&self,fs:&ET::FontStore) -> ET::Dim {
        use SkipNode::*;
        match self {
            Skip { skip: val,axis:HorV::Vertical} => val.base,
            _ => ET::Dim::from_sp(0)
        }
    }
    fn width(&self,fs:&ET::FontStore) -> ET::Dim {
        use SkipNode::*;
        match self {
            Skip { skip: val,axis:HorV::Horizontal} => val.base,
            _ => ET::Dim::from_sp(0)
        }
    }
    fn nodetype(&self) -> u8 { 11 }
}


#[derive(Debug,Clone)]
pub enum HorV { Horizontal, Vertical }

#[derive(Debug,Clone,Copy)]
pub enum LeadersType { Normal, C, X}

#[derive(Debug,Clone,Copy,PartialEq,Eq)]
pub enum MathClass { Ord = 0, Op = 1, Bin = 2, Rel = 3, Open = 4, Close = 5, Punct = 6 }
impl From<u8> for MathClass {
    fn from(v: u8) -> Self {
        match v {
            0 => MathClass::Ord,
            1 => MathClass::Op,
            2 => MathClass::Bin,
            3 => MathClass::Rel,
            4 => MathClass::Open,
            5 => MathClass::Close,
            6 => MathClass::Punct,
            _ => panic!("Invalid math class {}",v)
        }
    }
}

#[derive(Debug,Clone)]
pub enum SimpleNode<ET:EngineType> {
    Rule{width:Option<ET::Dim>,height:Option<ET::Dim>,depth:Option<ET::Dim>, axis:HorV},
    Raise{by:ET::Dim, node:HVBox<ET>},
    Char {char:ET::Char, font:ET::FontRef,cls:Option<MathClass> },
    Delimiter {small_char:ET::Char,small_font:ET::FontRef,large_char:ET::Char,large_font:ET::FontRef,small_cls:MathClass,large_cls:MathClass},
    WithScripts {kernel:Box<TeXNode<ET>>,superscript:Option<Box<TeXNode<ET>>>,subscript:Option<Box<TeXNode<ET>>>,limits:bool},
    Leaders {bx:HVBox<ET>,skip:SkipNode<ET>,tp:LeadersType}
}

impl<ET:EngineType> NodeTrait<ET> for SimpleNode<ET> {
    fn as_node(self) -> TeXNode<ET> {
        TeXNode::Simple(self)
    }
    fn depth(&self,fs:&ET::FontStore) -> ET::Dim {
        match self {
            SimpleNode::Rule{depth,..} => depth.unwrap_or_else(|| ET::Dim::from_sp(0)),
            SimpleNode::Raise{node,by} => {
                let d = node.depth(fs) - *by;
                if d > ET::Dim::from_sp(0) { d } else { ET::Dim::from_sp(0) }
            },
            SimpleNode::Char {char,font,..} => fs.get(*font).char_dp(*char),
            SimpleNode::Delimiter {large_char,large_font,..} => fs.get(*large_font).char_dp(*large_char),
            SimpleNode::WithScripts {kernel,subscript,..} => {
                match subscript {
                    Some(s) => kernel.depth(fs) + s.depth(fs) + s.height(fs), // TODO
                    _ => kernel.depth(fs)
                }
            }
            SimpleNode::Leaders {skip,..} => skip.depth(fs),
            _ => ET::Dim::from_sp(0)
        }
    }
    fn height(&self,fs:&ET::FontStore) -> ET::Dim {
        match self {
            SimpleNode::Rule{height,axis:HorV::Horizontal,..} => height.unwrap_or_else(|| ET::Dim::from_sp(26214)),
            SimpleNode::Rule{height,..} => height.unwrap_or_else(|| ET::Dim::from_sp(0)),
            SimpleNode::Raise{node,by} => {
                let h = node.height(fs) + *by;
                if h > ET::Dim::from_sp(0) { h } else { ET::Dim::from_sp(0) }
            },
            SimpleNode::Char {char,font,..} => fs.get(*font).char_ht(*char),
            SimpleNode::Delimiter {large_char,large_font,..} => fs.get(*large_font).char_ht(*large_char),
            SimpleNode::WithScripts {kernel,superscript,..} => {
                match superscript {
                    Some(s) => kernel.height(fs) + s.depth(fs) + s.height(fs), // TODO
                    _ => kernel.height(fs)
                }
            }
            SimpleNode::Leaders {skip,..} => skip.height(fs),
            _ => ET::Dim::from_sp(0)
        }
    }
    fn width(&self,fs:&ET::FontStore) -> ET::Dim {
        match self {
            SimpleNode::Rule{width,axis:HorV::Vertical,..} => width.unwrap_or_else(|| ET::Dim::from_sp(26214)),
            SimpleNode::Rule{width,..} => width.unwrap_or_else(|| ET::Dim::from_sp(0)),
            SimpleNode::Raise{node,..} => node.width(fs),
            SimpleNode::Char {char,font,..} => fs.get(*font).char_wd(*char),
            SimpleNode::Delimiter {large_char,large_font,..} => fs.get(*large_font).char_wd(*large_char),
            SimpleNode::WithScripts {kernel,..} => kernel.width(fs), // TODO
            SimpleNode::Leaders {skip,..} => skip.width(fs),
            _ => ET::Dim::from_sp(0)
        }
    }
    fn nodetype(&self) -> u8 {
        use SimpleNode::*;
        match self {
            Rule{..} => 3,
            Raise{node,..} => node.nodetype(),
            Delimiter {..} | WithScripts {..} => 15,
            Char{..} => 0,
            Leaders{skip,..} => skip.nodetype()
        }
    }
}

pub trait CustomNode<ET:EngineType>:Debug+Sized+Clone+NodeTrait<ET>+'static {}

impl<ET:EngineType<Node = ()>> NodeTrait<ET> for () {
    fn as_node(self) -> TeXNode<ET> {
        TeXNode::Custom(self)
    }
    fn height(&self,fs:&ET::FontStore) -> ET::Dim { ET::Dim::from_sp(0) }
    fn depth(&self,fs:&ET::FontStore) -> ET::Dim { ET::Dim::from_sp(0) }
    fn width(&self,fs:&ET::FontStore) -> ET::Dim { ET::Dim::from_sp(0) }
    fn nodetype(&self) -> u8 {9 }
}
impl<ET:EngineType<Node=()>> CustomNode<ET> for () {}

pub struct Whatsit<ET:EngineType>(Ptr<Mut<Option<Box<dyn FnOnce(&mut EngineRef<ET>)>>>>);
impl<ET:EngineType> Debug for Whatsit<ET> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("Whatsit")
    }
}
impl<ET:EngineType> Whatsit<ET> {
    pub fn new(f:Box<dyn FnOnce(&mut EngineRef<ET>)>) -> Self {
        Whatsit(Ptr::new(Mut::new(Some(f))))
    }
    pub fn apply(self, e:&mut EngineRef<ET>) {
        let f = &mut *self.0.borrow_mut();
        match std::mem::take(f) {
            None => (),
            Some(f) => f(e)
        }
    }
}
impl<ET:EngineType> Clone for Whatsit<ET> {
    fn clone(&self) -> Self {
        Whatsit(self.0.clone())
    }
}


#[derive(Clone,Debug)]
pub enum HVBox<ET:EngineType> {
    H(HBox<ET>),
    V(VBox<ET>),
    Void
}

impl<ET:EngineType> HVBox<ET> {
    pub(crate) fn set_height(&mut self, h:ET::Dim) {
        match self {
            HVBox::H(b) => b.assigned_height = Some(h),
            HVBox::V(b) => b.assigned_height = Some(h),
            HVBox::Void => {}
        }
    }
    pub(crate) fn set_depth(&mut self, d:ET::Dim) {
        match self {
            HVBox::H(b) => b.assigned_depth = Some(d),
            HVBox::V(b) => b.assigned_depth = Some(d),
            HVBox::Void => {}
        }
    }
    pub(crate) fn set_width(&mut self, w:ET::Dim) {
        match self {
            HVBox::H(b) => b.assigned_width = Some(w),
            HVBox::V(b) => b.assigned_width = Some(w),
            HVBox::Void => {}
        }
    }
}
impl<ET:EngineType> NodeTrait<ET> for HVBox<ET> {
    fn as_node(self) -> TeXNode<ET> { TeXNode::Box(self) }
    fn height(&self,fs:&ET::FontStore) -> ET::Dim {
        match self {
            HVBox::H(b) => b.height(fs),
            HVBox::V(b) => b.height(fs),
            HVBox::Void => ET::Dim::from_sp(0)
        }
    }
    fn depth(&self,fs:&ET::FontStore) -> ET::Dim {
        match self {
            HVBox::H(b) => b.depth(fs),
            HVBox::V(b) => b.depth(fs),
            HVBox::Void => ET::Dim::from_sp(0)
        }
    }
    fn width(&self,fs:&ET::FontStore) -> ET::Dim {
        match self {
            HVBox::H(b) => b.width(fs),
            HVBox::V(b) => b.width(fs),
            HVBox::Void => ET::Dim::from_sp(0)
        }
    }
    fn nodetype(&self) -> u8 {
        match self {
            HVBox::H(_) => 1,
            HVBox::V(_) => 2,
            HVBox::Void => unreachable!()
        }
    }
}

#[derive(Clone,Debug)]
pub struct HBox<ET:EngineType> {
    pub kind:&'static str,
    pub children:Vec<TeXNode<ET>>,
    pub spread:Option<ET::Dim>,
    pub to:Option<ET::Dim>,
    pub assigned_width:Option<ET::Dim>,
    pub assigned_height:Option<ET::Dim>,
    pub assigned_depth:Option<ET::Dim>
}
impl<ET:EngineType> NodeTrait<ET> for HBox<ET> {
    fn as_node(self) -> TeXNode<ET> { TeXNode::Box(HVBox::H(self)) }
    fn height(&self,fs:&ET::FontStore) -> ET::Dim {
        self.assigned_height.unwrap_or_else(|| {
            self.children.iter().map(|c| c.height(fs)).max().unwrap_or_else(|| ET::Dim::from_sp(0))
        })
    }
    fn depth(&self,fs:&ET::FontStore) -> ET::Dim {
        self.assigned_depth.unwrap_or_else(|| {
            self.children.iter().map(|c| c.depth(fs)).max().unwrap_or_else(|| ET::Dim::from_sp(0))
        })
    }
    fn width(&self,fs:&ET::FontStore) -> ET::Dim {
        self.assigned_width.unwrap_or_else(|| {
            self.children.iter().map(|c| c.width(fs)).sum()
        })
    }
    fn nodetype(&self) -> u8 {1 }
}
impl<ET:EngineType> Default for HBox<ET> {
    fn default() -> Self {
        Self {
            kind:"hbox",
            children:Vec::new(),
            spread:None,
            to:None,
            assigned_width:None,
            assigned_height:None,
            assigned_depth:None
        }
    }
}


#[derive(Clone,Debug)]
pub struct VBox<ET:EngineType> {
    pub kind:&'static str,
    pub children:Vec<TeXNode<ET>>,
    pub spread:Option<ET::Dim>,
    pub to:Option<ET::Dim>,
    pub assigned_width:Option<ET::Dim>,
    pub assigned_height:Option<ET::Dim>,
    pub assigned_depth:Option<ET::Dim>
}
impl<ET:EngineType> NodeTrait<ET> for VBox<ET> {
    fn as_node(self) -> TeXNode<ET> { TeXNode::Box(HVBox::V(self)) }
    fn height(&self,fs:&ET::FontStore) -> ET::Dim {
        self.assigned_height.unwrap_or_else(|| {
            self.children.iter().map(|c| c.height(fs)).sum()
        })
    }
    fn depth(&self,fs:&ET::FontStore) -> ET::Dim {
        self.assigned_depth.unwrap_or_else(|| {
            // TODO not quite - filter penalties etc.
            self.children.iter().rev().find(|c| c.depth(fs) > ET::Dim::from_sp(0)).map(|c| c.depth(fs)).unwrap_or_else(|| ET::Dim::from_sp(0))
        })
    }
    fn width(&self,fs:&ET::FontStore) -> ET::Dim {
        self.assigned_width.unwrap_or_else(|| {
            self.children.iter().map(|c| c.width(fs)).max().unwrap_or_else(|| ET::Dim::from_sp(0))
        })
    }
    fn nodetype(&self) -> u8 {2 }
}
impl<ET:EngineType> Default for VBox<ET> {
    fn default() -> Self {
        Self {
            kind:"vbox",
            children:Vec::new(),
            spread:None,
            to:None,
            assigned_width:None,
            assigned_height:None,
            assigned_depth:None
        }
    }
}

#[derive(Clone)]
pub enum OpenBox<ET:EngineType> {
    Paragraph { list: Vec<TeXNode<ET>> },
    Box {
        mode:BoxMode,
        list:Vec<TeXNode<ET>>,
        on_close: CloseBoxFun<ET>
    },
    Math { list: Vec<TeXNode<ET>>, display:bool },
}
impl<ET:EngineType> Debug for OpenBox<ET> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            OpenBox::Paragraph {..} => write!(f,"Paragraph"),
            OpenBox::Box {mode,..} => write!(f,"{:?} box",mode),
            OpenBox::Math {display,..} => write!(f,"Math box (display={})",display)
        }
    }
}
impl<ET:EngineType> OpenBox<ET> {
    pub fn ls_mut(&mut self) -> &mut Vec<TeXNode<ET>> {
        match self {
            Self::Paragraph { list } => list,
            Self::Box { list, .. } => list,
            Self::Math { list, .. } => list
        }
    }
    pub fn ls(&self) -> &Vec<TeXNode<ET>>{
        match self {
            Self::Paragraph { list } => list,
            Self::Box { list, .. } => list,
            Self::Math { list, .. } => list
        }
    }
    pub fn is_vertical(&self) -> bool {
        match self {
            Self::Box { mode:BoxMode::V, .. } => true,
            _ => false
        }
    }
}