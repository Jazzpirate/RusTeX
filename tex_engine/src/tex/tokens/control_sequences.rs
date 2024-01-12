/*! A control sequence is a [`Token`](super::Token) of the (usually) form `\foo`.
    We can just use strings to represent them, but there is room for optimization by e.g. interning
    them, which requires some more infrastructure to intern and resolve them.

    We implement the trait [`CSHandler`] for the Unit type `()`, in case
    we don't want to do any interning and just use [`Ptr`]`<str>`s.
*/

use std::fmt::{Debug, Display, Write};
use std::marker::PhantomData;
use crate::engine::utils::memory::CharacterVecInterner;
use crate::prelude::{CategoryCode, CategoryCodeScheme};
use crate::tex::characters::Character;
use crate::utils::{HMap, Ptr};
use crate::tex::characters::CharacterMap;


/** The name of a control sequence. */
pub trait CSName<C:Character>: Clone + Eq + 'static + std::hash::Hash + Debug {
    /// The type of the handler for this control sequence name.
    type Handler: CSHandler<C,Self>;
    /// The type used for mapping control sequence names to e.g. [`Command`](crate::commands::Command)s.
    type Map<A>:CSNameMap<C,Self,A> where A:Clone;
    fn display_fmt<W:Write>(&self, int:&Self::Handler, cc:&CategoryCodeScheme<C>, escapechar:Option<C>, f: &mut W) -> std::fmt::Result {
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

/// How to map control sequence names to e.g. [`Command`](crate::commands::Command)s.
pub trait CSNameMap<C:Character,CS:CSName<C>,A:Clone>:Clone+Default {
    /// Returns the value associated with the given control sequence name, if any.
    fn get(&self,cs:&CS) -> Option<&A>;
    /// Inserts a new value for the given control sequence name, returning the old value if any.
    fn insert(&mut self,cs:CS,a:A) -> Option<A>;
    /// Removes the value associated with the given control sequence name, returning it if any.
    fn remove(&mut self,cs:&CS) -> Option<A>;
}

impl<C:Character,CS:CSName<C>,A:Clone> CSNameMap<C,CS,A> for HMap<CS,A> {

    fn get(&self, cs: &CS) -> Option<&A> { self.get(cs) }

    fn insert(&mut self, cs: CS, a: A) -> Option<A> { self.insert(cs,a) }

    fn remove(&mut self, cs: &CS) -> Option<A> { self.remove(cs) }
}

impl<C:Character> CSName<C> for Ptr<str> {
    type Handler = ();
    type Map<A> = HMap<Self,A> where A:Clone;
}

/// A control sequence name that has been interned.
/// Uses *consecutive* `u32` values
pub type InternedCSName<C> = (u32,PhantomData<C>);
impl<C:Character> CSName<C> for InternedCSName<C> {
    type Handler = CharacterVecInterner<C>;
    type Map<A> = CSNameVec<C,A> where A:Clone;
}

/// A [`CSNameMap`] that uses a [`Vec`] to store the values for [`InternedCSName`].
/// Since the [`InternedCSName`]s are consecutive, we can use a [`Vec`] instead of a [`HMap`].
#[derive(Clone)]
pub struct CSNameVec<C:Character,A:Clone>(Vec<Option<A>>,PhantomData<C>);
impl<C:Character,A:Clone> Default for CSNameVec<C,A> {

    fn default() -> Self { Self(Vec::new(),PhantomData) }
}

impl<C:Character,A:Clone> CSNameMap<C,InternedCSName<C>,A> for CSNameVec<C,A> {
    fn get(&self, cs: &InternedCSName<C>) -> Option<&A> {
        self.0.get(cs.0 as usize).and_then(|x| x.as_ref())
    }
    fn insert(&mut self, cs: InternedCSName<C>, a: A) -> Option<A> {
        let idx = cs.0 as usize;
        if self.0.len() <= idx {
            self.0.resize(idx + 1, None);
        }
        std::mem::replace(&mut self.0[idx], Some(a))
    }
    fn remove(&mut self, cs: &InternedCSName<C>) -> Option<A> {
        let idx = cs.0 as usize;
        if self.0.len() <= idx {
            return None
        }
        std::mem::replace(&mut self.0[idx], None)
    }
}

/// A control sequence name that has been interned needs to be resolved again to
/// get the actual name / display it etc.
pub trait ResolvedCSName<'a,C:Character>:Display {
    /// The type of the iterator over the characters of the control sequence name.
    type Iter:Iterator<Item=C>;
    /// Returns an iterator over the characters of the control sequence name.
    fn iter(&self) -> Self::Iter;
    /// Returns the length of the control sequence name in terms of the underlying [`Character`] type.
    fn len(&self) -> usize;
}

/** Handles control sequence names - conversion from/to strings, displaying etc. */
pub trait CSHandler<C:Character,CS: CSName<C>>:Default+Clone {
    /// The type of the resolved control sequence name (for displaying / iterating over the underlying
    /// [`Character`]s.
    type Resolved<'a>:ResolvedCSName<'a,C> where Self:'a;
    /// Creates a new control sequence name from a string.
    fn new(&mut self,s: &str) -> CS;
    /// Creates a new control sequence name from a vector of characters.
    fn from_chars(&mut self,v: &Vec<C>) -> CS;
    /// Resolves a control sequence name.
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

    fn new(&mut self,s: &str) -> Ptr<str> {
        s.into()
    }

    fn resolve<'a>(&'a self, cs: &'a Ptr<str>) -> Self::Resolved<'a> {
        &*cs
    }

    fn par(&self) -> Ptr<str> { "par".to_string().into() }

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