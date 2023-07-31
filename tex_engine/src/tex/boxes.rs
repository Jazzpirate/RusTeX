//! Boxes producing actual output

use std::fmt::{Debug, Formatter};
use std::sync::RwLock;
use crate::engine::EngineType;
use crate::engine::state::modes::BoxMode;
use crate::engine::stomach::Stomach;
use crate::tex::numbers::NumSet;
use crate::tex::token::Token;
use crate::utils::errors::TeXError;
use crate::utils::Ptr;

pub trait TeXNode:Debug+Sized+Clone+'static {
    type Bx:TeXBox;
}
pub trait TeXBox:TeXNode<Bx=Self> {

}

#[derive(Debug,Clone)]
pub enum StandardTeXNode {}

#[derive(Debug,Clone)]
pub enum StandardTeXBox {

}
impl TeXBox for StandardTeXBox {}
impl TeXNode for StandardTeXBox {
    type Bx = StandardTeXBox;
}

pub struct Whatsit<ET:EngineType>(Ptr<RwLock<Option<Box<dyn FnOnce(&mut ET::Stomach, &mut ET::State, &mut ET::Gullet) -> Result<(),Box<dyn TeXError<ET::Token>>>>>>>);
impl<ET:EngineType> Whatsit<ET> {
    pub fn new(f:Box<dyn FnOnce(&mut ET::Stomach, &mut ET::State, &mut ET::Gullet) -> Result<(),Box<dyn TeXError<ET::Token>>>>) -> Self {
        Whatsit(Ptr::new(RwLock::new(Some(f))))
    }
    pub fn apply(self,stomach:&mut ET::Stomach, state:&mut ET::State, gullet:&mut ET::Gullet) -> Result<(),Box<dyn TeXError<ET::Token>>> {
        let f = &mut *self.0.write().unwrap();
        match std::mem::take(f) {
            None => Ok(()),
            Some(f) => f(stomach,state,gullet)
        }
    }
}
impl<ET:EngineType> Clone for Whatsit<ET> {
    fn clone(&self) -> Self {
        Whatsit(self.0.clone())
    }
}

pub enum StomachNode<ET:EngineType> {
    Node(ET::Node),
    Whatsit(Whatsit<ET>),
    HBox(HBox<ET>)
}
impl<ET:EngineType> Clone for StomachNode<ET> {
    fn clone(&self) -> Self {
        match self {
            StomachNode::Node(n) => StomachNode::Node(n.clone()),
            StomachNode::Whatsit(w) => StomachNode::Whatsit((*w).clone()),
            StomachNode::HBox(h) => StomachNode::HBox(h.clone())
        }
    }
}

#[derive(Clone)]
pub enum HVBox<ET:EngineType> {
    H(HBox<ET>),
    Void
}

#[derive(Clone)]
pub struct HBox<ET:EngineType> {
    pub kind:&'static str,
    pub children:Vec<StomachNode<ET>>,
    pub spread:Option<ET::Dim>,
    pub to:Option<ET::Dim>,
    pub assigned_width:Option<ET::Dim>,
    pub assigned_height:Option<ET::Dim>,
    pub assigned_depth:Option<ET::Dim>
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

impl TeXNode for StandardTeXNode {
    type Bx = StandardTeXBox;
}

pub enum OpenBox<ET:EngineType> {
    Top { list: Vec<StomachNode<ET>> },
    Paragraph { list: Vec<StomachNode<ET>> },
    Box {
        mode:BoxMode,
        list:Vec<StomachNode<ET>>,
        on_close: Box<dyn FnOnce(&mut ET::Stomach, &mut ET::State, &mut ET::Gullet,Vec<StomachNode<ET>>) ->Option<StomachNode<ET>>>
    },
}
impl<ET:EngineType> OpenBox<ET> {
    pub fn ls(&mut self) -> &mut Vec<StomachNode<ET>> {
        match self {
            Self::Top { list } => list,
            Self::Paragraph { list } => list,
            Self::Box { list, .. } => list
        }
    }
}