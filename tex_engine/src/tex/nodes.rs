//! Boxes producing actual output

use std::fmt::{Debug, Formatter};
use std::marker::PhantomData;
use crate::engine::{EngineRef, EngineType};
use crate::engine::state::modes::BoxMode;
use crate::tex::commands::CloseBoxFun;
use crate::tex::numbers::Skip;
use crate::tex::token::Token;
use crate::utils::errors::TeXError;
use crate::utils::{Mut, Ptr};
use crate::tex::numbers::Dim;

pub trait NodeTrait<ET:EngineType> {
    fn as_node(self) -> TeXNode<ET>;
    fn height(&self) -> ET::Dim;
    fn depth(&self) -> ET::Dim;
    fn width(&self) -> ET::Dim;
    fn nodetype(&self) -> u8;
}



#[derive(Debug,Clone)]
pub enum TeXNode<ET:EngineType> {
    Skip(SkipNode<ET>),
    Penalty(i32),
    Kern{ dim:ET::Dim,axis:HorV},
    Box(HVBox<ET>),
    Whatsit(Whatsit<ET>),
    Mark(Vec<Token<ET>>),
    Custom(ET::Node),
    Simple(SimpleNode<ET>),
}
impl<ET:EngineType> NodeTrait<ET> for TeXNode<ET> {
    fn height(&self) -> ET::Dim {
        use TeXNode::*;
        match self {
            Skip (s) => s.height(),
            Kern { dim: val,axis:HorV::Vertical} => *val,
            Box(b) => b.height(),
            Custom(c) => c.height(),
            Simple(s) => s.height(),
            _ => ET::Dim::from_sp(0)
        }
    }
    fn width(&self) -> ET::Dim {
        use TeXNode::*;
        match self {
            Skip(s) => s.width(),
            Kern { dim: val,axis:HorV::Horizontal} => *val,
            Box(b) => b.width(),
            Custom(c) => c.width(),
            Simple(s) => s.width(),
            _ => ET::Dim::from_sp(0)
        }
    }
    fn depth(&self) -> ET::Dim {
        use TeXNode::*;
        match self {
            Box(b) => b.depth(),
            Custom(c) => c.depth(),
            Simple(s) => s.depth(),
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
            Mark(_) => 5,
            Custom(n) => n.nodetype(),
            Simple(n) => n.nodetype()
        }
    }
}

#[derive(Debug,Clone)]
pub enum SkipNode<ET:EngineType> {
    Skip{val:Skip<ET::SkipDim>,axis:HorV},
    VFil,VFill,VFilneg,HFil,HFill,HFilneg,Hss,Vss
}

impl<ET:EngineType> NodeTrait<ET> for SkipNode<ET> {
    fn as_node(self) -> TeXNode<ET> {
        TeXNode::Skip(self)
    }
    fn depth(&self) -> ET::Dim {
        ET::Dim::from_sp(0)
    }
    fn height(&self) -> ET::Dim {
        use SkipNode::*;
        match self {
            Skip {val,axis:HorV::Vertical} => val.base,
            _ => ET::Dim::from_sp(0)
        }
    }
    fn width(&self) -> ET::Dim {
        use SkipNode::*;
        match self {
            Skip {val,axis:HorV::Horizontal} => val.base,
            _ => ET::Dim::from_sp(0)
        }
    }
    fn nodetype(&self) -> u8 { 11 }
}


#[derive(Debug,Clone)]
pub enum HorV { Horizontal, Vertical }

#[derive(Debug,Clone)]
pub enum SimpleNode<ET:EngineType> {
    Rule{width:Option<ET::Dim>,height:Option<ET::Dim>,depth:Option<ET::Dim>, axis:HorV},
    Raise{by:ET::Dim, node:HVBox<ET>},
}

impl<ET:EngineType> NodeTrait<ET> for SimpleNode<ET> {
    fn as_node(self) -> TeXNode<ET> {
        TeXNode::Simple(self)
    }
    fn depth(&self) -> ET::Dim {
        match self {
            SimpleNode::Rule{depth,..} => depth.unwrap_or_else(|| ET::Dim::from_sp(0)),
            SimpleNode::Raise{node,by} => {
                let d = node.depth() - *by;
                if d > ET::Dim::from_sp(0) { d } else { ET::Dim::from_sp(0) }
            },
            _ => ET::Dim::from_sp(0)
        }
    }
    fn height(&self) -> ET::Dim {
        match self {
            SimpleNode::Rule{height,..} => height.unwrap_or_else(|| ET::Dim::from_sp(0)),
            SimpleNode::Raise{node,by} => {
                let h = node.height() + *by;
                if h > ET::Dim::from_sp(0) { h } else { ET::Dim::from_sp(0) }
            },
            _ => ET::Dim::from_sp(0)
        }
    }
    fn width(&self) -> ET::Dim {
        match self {
            SimpleNode::Rule{width,..} => width.unwrap_or_else(|| ET::Dim::from_sp(0)),
            SimpleNode::Raise{node,..} => node.width(),
            _ => ET::Dim::from_sp(0)
        }
    }
    fn nodetype(&self) -> u8 {
        use SimpleNode::*;
        match self {
            Rule{..} => 3,
            Raise{node,..} => node.nodetype()
        }
    }
}

pub trait CustomNode<ET:EngineType>:Debug+Sized+Clone+NodeTrait<ET>+'static {}

impl<ET:EngineType<Node = ()>> NodeTrait<ET> for () {
    fn as_node(self) -> TeXNode<ET> {
        TeXNode::Custom(self)
    }
    fn height(&self) -> ET::Dim { ET::Dim::from_sp(0) }
    fn depth(&self) -> ET::Dim { ET::Dim::from_sp(0) }
    fn width(&self) -> ET::Dim { ET::Dim::from_sp(0) }
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
    fn height(&self) -> ET::Dim {
        match self {
            HVBox::H(b) => b.height(),
            HVBox::V(b) => b.height(),
            HVBox::Void => ET::Dim::from_sp(0)
        }
    }
    fn depth(&self) -> ET::Dim {
        match self {
            HVBox::H(b) => b.depth(),
            HVBox::V(b) => b.depth(),
            HVBox::Void => ET::Dim::from_sp(0)
        }
    }
    fn width(&self) -> ET::Dim {
        match self {
            HVBox::H(b) => b.width(),
            HVBox::V(b) => b.width(),
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
    fn height(&self) -> ET::Dim {
        self.assigned_height.unwrap_or_else(|| {
            self.children.iter().map(|c| c.height()).max().unwrap_or_else(|| ET::Dim::from_sp(0))
        })
    }
    fn depth(&self) -> ET::Dim {
        self.assigned_depth.unwrap_or_else(|| {
            self.children.iter().map(|c| c.depth()).max().unwrap_or_else(|| ET::Dim::from_sp(0))
        })
    }
    fn width(&self) -> ET::Dim {
        self.assigned_width.unwrap_or_else(|| {
            self.children.iter().map(|c| c.width()).sum()
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
    fn height(&self) -> ET::Dim {
        self.assigned_height.unwrap_or_else(|| {
            self.children.iter().map(|c| c.height()).sum()
        })
    }
    fn depth(&self) -> ET::Dim {
        self.assigned_depth.unwrap_or_else(|| {
            // TODO not quite - filter penalties etc.
            self.children.iter().rev().find(|c| c.depth() > ET::Dim::from_sp(0)).map(|c| c.depth()).unwrap_or_else(|| ET::Dim::from_sp(0))
        })
    }
    fn width(&self) -> ET::Dim {
        self.assigned_width.unwrap_or_else(|| {
            self.children.iter().map(|c| c.width()).max().unwrap_or_else(|| ET::Dim::from_sp(0))
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
}
impl<ET:EngineType> Debug for OpenBox<ET> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            OpenBox::Paragraph {..} => write!(f,"Paragraph"),
            OpenBox::Box {mode,..} => write!(f,"{:?} box",mode)
        }
    }
}
impl<ET:EngineType> OpenBox<ET> {
    pub fn ls_mut(&mut self) -> &mut Vec<TeXNode<ET>> {
        match self {
            Self::Paragraph { list } => list,
            Self::Box { list, .. } => list
        }
    }
    pub fn ls(&self) -> &Vec<TeXNode<ET>>{
        match self {
            Self::Paragraph { list } => list,
            Self::Box { list, .. } => list
        }
    }
    pub fn is_vertical(&self) -> bool {
        match self {
            Self::Box { mode:BoxMode::V, .. } => true,
            _ => false
        }
    }
}