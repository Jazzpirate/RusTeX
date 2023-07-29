//! Boxes producing actual output

use std::fmt::{Debug, Formatter};
use crate::engine::stomach::Stomach;
use crate::tex::token::Token;
use crate::utils::errors::TeXError;

pub trait TeXNode:Debug+Sized+'static {
    type Bx:TeXBox;
}
pub trait TeXBox:TeXNode<Bx=Self> {

}

#[derive(Debug)]
pub enum StandardTeXNode {}

#[derive(Debug)]
pub enum StandardTeXBox {

}
impl TeXBox for StandardTeXBox {}
impl TeXNode for StandardTeXBox {
    type Bx = StandardTeXBox;
}

pub struct Whatsit<T:Token,Sto:Stomach<T>> {
    pub apply: Box<dyn FnOnce(&mut Sto, &mut Sto::S, &mut Sto::Gu) -> Result<(),Box<dyn TeXError<T>>>>,
}

pub enum NodeOrWhatsit<T:Token,Sto:Stomach<T>,B: TeXNode> {
    Node(B),
    Whatsit(Whatsit<T,Sto>),
}

impl TeXNode for StandardTeXNode {
    type Bx = StandardTeXBox;
}

pub enum OpenBox<T:Token,Sto:Stomach<T>+'static,B: TeXNode> {
    Top { list: Vec<NodeOrWhatsit<T,Sto,B>> },
    Paragraph { list: Vec<NodeOrWhatsit<T,Sto,B>> },
    Box { list:Vec<NodeOrWhatsit<T,Sto,B>>,
        on_close: Box<dyn FnOnce(&mut Sto, &mut Sto::S, &mut Sto::Gu) ->Option<B::Bx>>
    },
}