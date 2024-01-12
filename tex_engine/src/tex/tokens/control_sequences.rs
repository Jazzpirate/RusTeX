/*! A control sequence is a [`Token`](super::tokens::Token) of the (usually) form `\foo`.
    We can just use strings to represent them, but there is room for optimization by e.g. interning
    them, which requires some more infrastructure to intern and resolve them.

    We implement the trait [`CSHandler`] for the Unit type `()`, in case
    we don't want to do any interning and just use [`Ptr`]`<str>`s.
*/

use std::fmt::{Debug, Display, Write};
use std::marker::PhantomData;
use crate::engine::utils::memory::CharacterVecInterner;
use crate::prelude::{CategoryCode, CategoryCodeScheme};
use crate::tex::input_text::Character;
use crate::utils::Ptr;
use crate::tex::input_text::CharacterMap;


/** The name of a control sequence. */
pub trait CSName<C:Character>: Clone + Eq + 'static + std::hash::Hash + Debug {
    /// The type of the handler for this control sequence name.
    type Handler: CSHandler<C,Self>;
    fn as_usize(&self) -> usize;
    fn display_fmt<W:Write>(&self, int:&Self::Handler, cc:&CategoryCodeScheme<C>, escapechar:Option<C>, mut f: W) -> std::fmt::Result {
        let res = int.resolve(self);
        write!(f, "{}{}", C::displayable_opt(escapechar), res)?;
        if res.len() == 1 {
            let c = res.iter().next().unwrap();
            match cc.get(c) {
                CategoryCode::Letter => write!(f," "),
                _ => Ok(())
            }
        } else {
            write!(f," ")
        }
    }
}

impl<C:Character> CSName<C> for Ptr<str> {
    type Handler = ();
    fn as_usize(&self) -> usize {
        todo!()
    }
}

pub type InternedCSName<C> = (u32,PhantomData<C>);
impl<C:Character> CSName<C> for InternedCSName<C> {
    type Handler = CharacterVecInterner<C>;
    #[inline(always)]
    fn as_usize(&self) -> usize {
        self.0 as usize
    }
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
            c.display_fmt(&mut s);
        }
        s.into()
    }
}

/*
impl<C:Character> CSName<C> for InternedString {
    type Handler = StringInterner;
    fn as_usize(&self) -> usize {
        self.to_usize()
    }
}
 */