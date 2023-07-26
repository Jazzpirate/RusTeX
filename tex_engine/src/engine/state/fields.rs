/*! Abstractions for fields of a [`State`](crate::engine::state::State). The base trait being
   [`StateField`].

  Most fields are [`KeyValueField`]s which map Keys of some type `K` to some value of type `V`.
 */

use crate::utils::strings::{AllCharsTrait, CharType};
use std::collections::hash_map::Entry;
use std::hash::Hash;
use ahash::HashMap;

/** A field of the [`State`](crate::engine::state::State) needs to be able to push and pop a new stack frame.
  This is largely implemented as a [`Vec`] of [`HashMap`]s tracking changes and reverting the local ones
  on pop.
*/
pub trait StateField {
    /// push a new stack frame
    fn push_stack(&mut self);
    /// pop the top stack frame and unroll changes
    fn pop_stack(&mut self);
}

/** A field consisting of a single value; e.g. [`endlinechar`](crate::engine::state::State::get_endlinechar),
  [`newlinechar`](crate::engine::state::State::get_newlinechar),...
*/
#[derive(Default)]
pub struct SingleValueField<V>(V,Vec<Option<V>>);

impl<V> SingleValueField<V> {
    pub fn new(initial:V) -> Self { SingleValueField(initial,Vec::new()) }
    pub fn get(&self) -> &V { &self.0 }
    pub fn set_locally(&mut self,v:V) {
        if let Some(ov) = self.1.last_mut() {
            match ov {
                ov@None => {
                    let old = std::mem::replace(&mut self.0,v);
                    *ov = Some(old)
                }
                _ => self.0 = v
            }
        } else {
            self.0 = v
        }
    }
    pub fn set_globally(&mut self,v:V) {
        self.0 = v;
        for ov in self.1.iter_mut() {
            *ov = None
        }
    }
}
impl<V> StateField for SingleValueField<V> {
    fn push_stack(&mut self) { self.1.push(None) }
    fn pop_stack(&mut self) {
        if let Some(Some(v)) = self.1.pop() {
            self.0 = v
        }
    }
}

/// A [`StateField`] storing Key-Value-Pairs.
pub trait KeyValueField<K,V>: StateField {
    /// get the value associated with a key
    fn get(&self,k:&K) -> V;
    /// set the value associated with a key locally to the current TeX Group
    fn set_locally(&mut self,k:K,v:V);
    /// set the value associated with a key globally
    fn set_globally(&mut self,k:K,v:V);
}

/// An Array/Vec of Characters with associated values of type A; e.g.
/// [`ucchar`](crate::engine::state::State::get_uccode), [`catcode`](crate::engine::state::State::get_catcode_scheme) etc.
pub struct CharField<C:CharType,A:Clone+Default>{pub charfield: C::Allchars<A>, changes: Vec<HashMap<C,A>>}
impl<C:CharType,A:Clone+Default> StateField for CharField<C,A> {
    fn push_stack(&mut self) { self.changes.push(HashMap::default()) }
    fn pop_stack(&mut self) {
        if let Some(m) = self.changes.pop() {
            for (k,v) in m {
                self.charfield.set(k,v)
            }
        }
    }
}
impl<C:CharType,A:Clone+Default> KeyValueField<C,A> for CharField<C,A> {
    //#[inline(always)]
    fn get(&self, c:&C) -> A { self.charfield.get(*c).clone() }
    //#[inline(always)]
    fn set_locally(&mut self, k: C, v:A) {
        if let Some(m) = self.changes.last_mut() {
            match m.entry(k) {
                Entry::Vacant(e) => {
                    e.insert(self.charfield.replace(k,v));
                }
                Entry::Occupied(_) => {
                    self.charfield.set(k,v);
                }
            }
        } else {
            self.charfield.set(k,v);
        }
    }
    fn set_globally(&mut self, k: C, v:A) {
        self.charfield.set(k,v);
        for ov in self.changes.iter_mut() {
            ov.remove(&k);
        }
    }
}
impl<C:CharType,A:Clone+Default> CharField<C,A> {
    /// initializes a new [`CharField`] with the given initial sequence of type [`CharType::Allchars`]
    //#[inline(always)]
    pub fn new(initial: C::Allchars<A>) -> Self { CharField{charfield:initial, changes:Vec::new()} }
}

/// A Vec of values of type A; e.g. [`intregisters`](crate::engine::state::State::get_int_register),
pub struct VecField<V:Default>(Vec<V>, Vec<HashMap<usize,V>>);
impl<V:Default+Clone> VecField<V> {
    /// initializes a new [`VecField`].
    //#[inline(always)]
    pub fn new() -> Self { VecField(Vec::with_capacity(255), Vec::new()) }
    //#[inline(always)]
    fn set_inner(vec:&mut Vec<V>,k:usize,v:V) -> Option<V> {
        if k < vec.len() {
            Some(std::mem::replace(&mut vec[k],v))
        } else {
            vec.resize(k+1,V::default());
            vec[k] = v;
            None
        }
    }
}
impl<V:Default+Clone> StateField for VecField<V> {
    fn push_stack(&mut self) { self.1.push(HashMap::default()) }
    fn pop_stack(&mut self) {
        if let Some(m) = self.1.pop() {
            for (k,v) in m {
                Self::set_inner(&mut self.0,k,v);
            }
        }
    }
}
impl<V:Default+Clone> KeyValueField<usize,V> for VecField<V> {
    // #[inline(always)]
    fn get(&self, k: &usize) -> V {
        if *k < self.0.len() {
            self.0[*k].clone()
        } else {
            V::default()
        }
    }

    // #[inline(always)]
    fn set_locally(&mut self, k: usize, v: V) {
        if let Some(m) = self.1.last_mut() {
            match m.entry(k) {
                Entry::Vacant(e) => {
                    e.insert(match Self::set_inner(&mut self.0,k,v) {
                        Some(old) => old,
                        None => V::default()
                    });
                }
                Entry::Occupied(_) => {
                    Self::set_inner(&mut self.0,k,v);
                }
            }
        } else {
            Self::set_inner(&mut self.0,k,v);
        }
    }

    // #[inline(always)]
    fn set_globally(&mut self, k: usize, v: V) {
        Self::set_inner(&mut self.0,k,v);
        for ov in self.1.iter_mut() {
            ov.remove(&k);
        }
    }
}

/// Allow for checking whether a value is the [`Default::default`] value for its type.
pub trait IsDefault:Default {
    fn is_default(&self) -> bool;
}
impl IsDefault for usize {
    //#[inline(always)]
    fn is_default(&self) -> bool { *self == 0 }
}
impl IsDefault for u8 {
    //#[inline(always)]
    fn is_default(&self) -> bool { *self == 0 }
}
impl IsDefault for i32 {
    //#[inline(always)]
    fn is_default(&self) -> bool { *self == 0 }
}
impl<A> IsDefault for Option<A> {
    //#[inline(always)]
    fn is_default(&self) -> bool { self.is_none() }
}
impl<A> IsDefault for Vec<A> {
    //#[inline(always)]
    fn is_default(&self) -> bool { self.is_empty() }
}

/// A HashMap of values of type V; e.g. [`Command`](crate::tex::commands::Command)s,
pub struct HashMapField<K:Eq+Hash+Clone,V:Default+Clone+IsDefault>(HashMap<K,V>, Vec<HashMap<K,V>>);
impl<K:Eq+Hash+Clone,V:Default+Clone+IsDefault> HashMapField<K,V> {
    /// initializes a new [`HashMapField`].
    //#[inline(always)]
    pub fn new() -> Self { HashMapField(HashMap::default(), Vec::new()) }
    //#[inline(always)]
    pub fn set_i(map:&mut HashMap<K,V>, k: K, v: V) -> Option<V> {
        if v.is_default() {
            map.remove(&k)
        } else {
            map.insert(k,v)
        }
    }
}
impl<K:Eq+Hash+Clone,V:Default+Clone+IsDefault> StateField for HashMapField<K,V> {
    // #[inline(always)]
    fn push_stack(&mut self) { self.1.push(HashMap::default()) }
    // #[inline(always)]
    fn pop_stack(&mut self) {
        if let Some(m) = self.1.pop() {
            for (k,v) in m {
                Self::set_i(&mut self.0,k,v);
            }
        }
    }
}
impl<K:Eq+Hash+Clone,V:Default+Clone+IsDefault> KeyValueField<K,V> for HashMapField<K,V> {
    // #[inline(always)]
    fn get(&self, k: &K) -> V {
        match self.0.get(k) {
            Some(v) => v.clone(),
            _ => V::default()
        }
    }

    // #[inline(always)]
    fn set_locally(&mut self, k: K, v: V) {
        if let Some(m) = self.1.last_mut() {
            match m.entry(k.clone()) {
                Entry::Vacant(e) => {
                    e.insert(Self::set_i(&mut self.0,k,v).unwrap_or_default());
                }
                Entry::Occupied(_) => {
                    Self::set_i(&mut self.0,k,v);
                }
            }
        } else {
            Self::set_i(&mut self.0,k,v);
        }
    }

    // #[inline(always)]
    fn set_globally(&mut self, k: K, v: V) {
        for ov in self.1.iter_mut() {
            ov.remove(&k);
        }
        Self::set_i(&mut self.0,k,v);
    }
}