/*! A control sequence is a [`Token`](super::token::Token) of the (usually) form `\foo`.
    We can just use strings to represent them, but there is room for optimization by e.g. interning
    them, which requires some more infrastructure to intern and resolve them.

    We implement the trait [`CSHandler`] for the Unit type `()`, in case
    we don't want to do any interning and just use [`Ptr`]`<str>`s.
*/

use std::fmt::{Debug, Display};
use string_interner::Symbol;
use crate::engine::utils::memory::{InternedString, StringInterner};
use crate::tex::input_text::Character;
use crate::utils::Ptr;


/** The name of a control sequence. */
pub trait CSName<C:Character>: Clone + Eq + 'static + std::hash::Hash + Debug {
    /// The type of the handler for this control sequence name.
    type Handler: CSHandler<C,Self>;
    fn as_usize(&self) -> usize;
}

pub trait ResolvedCSName<'a,C:Character>:Display {
    type Iter:Iterator<Item=C>;
    fn iter(&self) -> Self::Iter;
    fn len(&self) -> usize;
}

/** Handles control sequence names - conversion from/to strings, displaying etc. */
pub trait CSHandler<C:Character,CS: CSName<C>>:Default+Clone {
    type Resolved<'a>:ResolvedCSName<'a,C> where Self:'a;
    /// Creates a new control sequence name from a string.
    fn new(&mut self,s: &str) -> CS;
    fn from_chars(&mut self,v: &Vec<C>) -> CS;
    ///
    fn resolve<'a>(&'a self,cs:&'a CS) -> Self::Resolved<'a>;
    /// Returns the name of the `\par` control sequence.
    fn par(&self) -> CS;
    /// Returns the name of the empty control sequence.
    fn empty_str(&self) -> CS;
}

impl<'a,C:Character> ResolvedCSName<'a,C> for &'a str {
    type Iter = C::Iter<'a>;
    fn iter(&self) -> Self::Iter {
        C::string_to_iter(self)
    }
    fn len(&self) -> usize {
        C::string_to_iter(self).len()
    }
}

impl<C:Character> CSHandler<C,Ptr<str>> for () {
    type Resolved<'a> = &'a str;
    #[inline(always)]
    fn new(&mut self,s: &str) -> Ptr<str> {
        s.into()
    }
    #[inline(always)]
    fn resolve<'a>(&'a self, cs: &'a Ptr<str>) -> Self::Resolved<'a> {
        &*cs
    }
    #[inline(always)]
    fn par(&self) -> Ptr<str> { "par".to_string().into() }
    #[inline(always)]
    fn empty_str(&self) -> Ptr<str> { "".to_string().into() }
    fn from_chars(&mut self, v: &Vec<C>) -> Ptr<str> {
        let mut s = String::new();
        for c in v {
            c.display(&mut s);
        }
        s.into()
    }
}

impl<C:Character> CSName<C> for Ptr<str> {
    type Handler = ();
    fn as_usize(&self) -> usize {
        todo!()
    }
}
impl<C:Character> CSName<C> for InternedString {
    type Handler = StringInterner;
    fn as_usize(&self) -> usize {
        self.to_usize()
    }
}