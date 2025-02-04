use crate::engine::Types;
use std::vec::IntoIter;
use tex_engine::prelude::{HNode, MathNode, VNode};
use tex_engine::tex::nodes::math::MathFontStyle;

#[derive(Debug)]
pub(crate) struct ExtensibleIter<T> {
    curr: IntoIter<T>,
    next: Vec<IntoIter<T>>,
}
impl<T> ExtensibleIter<T> {
    pub fn prefix(&mut self, vec: Vec<T>) {
        let old = std::mem::replace(&mut self.curr, vec.into_iter());
        self.next.push(old);
    }
}
impl<T> From<Vec<T>> for ExtensibleIter<T> {
    fn from(v: Vec<T>) -> Self {
        Self {
            curr: v.into_iter(),
            next: Vec::new(),
        }
    }
}
impl<T> From<Box<[T]>> for ExtensibleIter<T> {
    fn from(v: Box<[T]>) -> Self {
        Self {
            curr: v.into_vec().into_iter(),
            next: Vec::new(),
        }
    }
}
impl<T> Iterator for ExtensibleIter<T> {
    type Item = T;
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(n) = self.curr.next() {
            return Some(n);
        }
        while let Some(mut it) = self.next.pop() {
            if let Some(n) = it.next() {
                self.curr = it;
                return Some(n);
            }
        }
        None
    }
}

pub(crate) type VNodes = ExtensibleIter<VNode<Types>>;
pub(crate) type HNodes = ExtensibleIter<HNode<Types>>;
pub(crate) type MNodes = ExtensibleIter<MNode>;
pub(crate) type MNode = MathNode<Types, MathFontStyle<Types>>;
