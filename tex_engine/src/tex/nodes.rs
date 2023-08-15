//! Boxes producing actual output

use std::fmt::{Debug, Formatter};
use crate::engine::{EngineRef, EngineType};
use crate::engine::state::modes::BoxMode;
use crate::engine::stomach::Stomach;
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
}

#[derive(Debug,Clone)]
pub enum TeXNode<ET:EngineType> {
    Skip(SkipNode<ET>),
    Penalty(i32),
    Kern{val:ET::Dim,axis:HorV},
    Box(HVBox<ET>),
    Whatsit(Whatsit<ET>),
    Mark(Vec<Token<ET>>),
    Custom(ET::Node),
    Simple(SimpleNode<ET>),
}
impl<ET:EngineType> TeXNode<ET> {
    pub fn height(&self) -> ET::Dim {
        use TeXNode::*;
        match self {
            Skip (s) => s.height(),
            Kern {val,axis:HorV::Vertical} => *val,
            Box(b) => b.height(),
            Custom(c) => c.height(),
            Simple(s) => s.height(),
            _ => ET::Dim::from_sp(0)
        }
    }
    pub fn width(&self) -> ET::Dim {
        use TeXNode::*;
        match self {
            Skip(s) => s.width(),
            Kern {val,axis:HorV::Horizontal} => *val,
            Box(b) => b.width(),
            Custom(c) => c.width(),
            Simple(s) => s.width(),
            _ => ET::Dim::from_sp(0)
        }
    }
    pub fn depth(&self) -> ET::Dim {
        use TeXNode::*;
        match self {
            Box(b) => b.depth(),
            Custom(c) => c.depth(),
            Simple(s) => s.depth(),
            _ => ET::Dim::from_sp(0)
        }
    }
}

#[derive(Debug,Clone)]
pub enum HorV { Horizontal, Vertical }

#[derive(Debug,Clone)]
pub enum SimpleNode<ET:EngineType> {
    Rule{width:Option<ET::Dim>,height:Option<ET::Dim>,depth:Option<ET::Dim>, axis:HorV},
    VFil,VFill,VFilneg
}
impl<ET:EngineType> NodeTrait<ET> for SimpleNode<ET> {
    fn as_node(self) -> TeXNode<ET> {
        TeXNode::Simple(self)
    }
    fn depth(&self) -> ET::Dim {
        todo!()
    }
    fn height(&self) -> ET::Dim {
        todo!()
    }
    fn width(&self) -> ET::Dim {
        todo!()
    }
}

pub trait CustomNode<ET:EngineType>:Debug+Sized+Clone+NodeTrait<ET>+'static {
    type Bx: CustomBox<ET>;
}
pub trait CustomBox<ET:EngineType>: CustomNode<ET,Bx=Self> {

}


impl<ET:EngineType<Node = ()>> NodeTrait<ET> for () {
    fn as_node(self) -> TeXNode<ET> {
        TeXNode::Custom(self)
    }
    fn height(&self) -> ET::Dim { ET::Dim::from_sp(0) }
    fn depth(&self) -> ET::Dim { ET::Dim::from_sp(0) }
    fn width(&self) -> ET::Dim { ET::Dim::from_sp(0) }
}
impl<ET:EngineType<Node=()>> CustomBox<ET> for () {}
impl<ET:EngineType<Node=()>> CustomNode<ET> for () {
    type Bx = ();
}

pub struct Whatsit<ET:EngineType>(Ptr<Mut<Option<Box<dyn FnOnce(&mut EngineRef<ET>) -> Result<(),TeXError<ET>>>>>>);
impl<ET:EngineType> Debug for Whatsit<ET> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("Whatsit")
    }
}
impl<ET:EngineType> Whatsit<ET> {
    pub fn new(f:Box<dyn FnOnce(&mut EngineRef<ET>) -> Result<(),TeXError<ET>>>) -> Self {
        Whatsit(Ptr::new(Mut::new(Some(f))))
    }
    pub fn apply(self, e:&mut EngineRef<ET>) -> Result<(),TeXError<ET>> {
        let f = &mut *self.0.borrow_mut();
        match std::mem::take(f) {
            None => Ok(()),
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
    Void
}
impl<ET:EngineType> NodeTrait<ET> for HVBox<ET> {
    fn as_node(self) -> TeXNode<ET> { TeXNode::Box(self) }
    fn height(&self) -> ET::Dim {
        match self {
            HVBox::H(b) => b.height(),
            HVBox::Void => ET::Dim::from_sp(0)
        }
    }
    fn depth(&self) -> ET::Dim {
        match self {
            HVBox::H(b) => b.depth(),
            HVBox::Void => ET::Dim::from_sp(0)
        }
    }
    fn width(&self) -> ET::Dim {
        match self {
            HVBox::H(b) => b.width(),
            HVBox::Void => ET::Dim::from_sp(0)
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

#[derive(Clone)]
pub enum OpenBox<ET:EngineType> {
    Paragraph { list: Vec<TeXNode<ET>> },
    Box {
        mode:BoxMode,
        list:Vec<TeXNode<ET>>,
        on_close: Ptr<dyn Fn(&mut EngineRef<ET>,Vec<TeXNode<ET>>) ->Option<TeXNode<ET>>>
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
    pub fn ls(&mut self) -> &mut Vec<TeXNode<ET>> {
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