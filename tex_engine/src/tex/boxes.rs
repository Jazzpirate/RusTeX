//! Boxes producing actual output

use std::fmt::{Debug, Formatter};
use crate::engine::stomach::Stomach;
use crate::tex::token::Token;
use crate::utils::errors::TeXError;

pub trait TeXBox:Debug+Sized+'static {}

pub enum StandardTeXBox{}
impl Debug for StandardTeXBox {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "StandardTeXBox")
    }
}

pub struct Whatsit<T:Token,Sto:Stomach<T>> {
    pub apply: Box<dyn FnOnce(&mut Sto, &mut Sto::S, &mut Sto::Gu) -> Result<(),Box<dyn TeXError<T>>>>,
}

pub enum BoxOrWhatsit<T:Token,Sto:Stomach<T>,B:TeXBox> {
    Box(B),
    Whatsit(Whatsit<T,Sto>),
}

impl TeXBox for StandardTeXBox {}

pub enum OpenBox<T:Token,Sto:Stomach<T>+'static,B:TeXBox> {
    Top { list: Vec<BoxOrWhatsit<T,Sto,B>> },
    Paragraph { list: Vec<BoxOrWhatsit<T,Sto,B>> },
    HBox { list:Vec<BoxOrWhatsit<T,Sto,B>> },
    VBox { list:Vec<BoxOrWhatsit<T,Sto,B>> },
}