/*! A control sequence is a [`Token`](super::token::Token) of the (usually) form `\foo`.
    We can just use strings to represent them, but there is room for optimization by e.g. interning
    them, which requires some more infrastructure to intern and resolve them.

    We implement the trait [`ControlSequenceNameHandler`] for the Unit type `()`, in case
    we don't want to do any interning and just use [`Ptr`]`<str>`s.
*/

use std::fmt::Debug;
use crate::engine::utils::memory::{InternedString, StringInterner};
use crate::utils::Ptr;


/** The name of a control sequence. */
pub trait ControlSequenceName: Clone + Eq + 'static + std::hash::Hash + Debug {
    /// The type of the handler for this control sequence name.
    type Handler: ControlSequenceNameHandler<Self>;
}

/** Handles control sequence names - conversion from/to strings, displaying etc. */
pub trait ControlSequenceNameHandler<CS: ControlSequenceName>:Default {
    type Printable<'a>:AsRef<str>+std::fmt::Display where Self:'a;
    /// Creates a new control sequence name from a string.
    fn new(&mut self,s: &str) -> CS;
    ///
    fn resolve<'a>(&'a self,cs:&'a CS) -> Self::Printable<'a>;
    /// Returns the name of the `\par` control sequence.
    fn par(&self) -> CS;
    /// Returns the name of the empty control sequence.
    fn empty_str(&self) -> CS;
}
impl ControlSequenceNameHandler<Ptr<str>> for () {
    type Printable<'a> = &'a str;
    #[inline(always)]
    fn new(&mut self,s: &str) -> Ptr<str> {
        s.into()
    }
    #[inline(always)]
    fn resolve<'a>(&'a self, cs: &'a Ptr<str>) -> Self::Printable<'a> {
        &*cs
    }
    #[inline(always)]
    fn par(&self) -> Ptr<str> { "par".to_string().into() }
    #[inline(always)]
    fn empty_str(&self) -> Ptr<str> { "".to_string().into() }
}

impl ControlSequenceName for Ptr<str> {
    type Handler = ();
}
impl ControlSequenceName for InternedString {
    type Handler = StringInterner;
}