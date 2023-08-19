/*! Abstractions for fields of a [`State`](crate::engine::state::State). The base trait being
   [`StateField`].

  Most fields are [`KeyValueField`]s which map Keys of some type `K` to some value of type `V`.
 */

use std::collections::BTreeMap;
use crate::utils::strings::{AllCharsTrait, CharType};
use std::collections::btree_map::Entry;
use std::hash::Hash;
use crate::engine::EngineType;
use crate::engine::memory::Memory;
use crate::tex::nodes::HVBox;
use crate::tex::token::Token;
use crate::utils::map::HMap;

/** A field of the [`State`](crate::engine::state::State) needs to be able to push and pop a new stack frame.
  This is largely implemented as a [`Vec`] of [`HMap`]s tracking changes and reverting the local ones
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
#[derive(Default,Clone)]
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
    fn get(&self,k:&K) -> Option<&V>;
    /// set the value associated with a key locally to the current TeX Group
    fn set_locally(&mut self,k:K,v:V);
    /// set the value associated with a key globally
    fn set_globally(&mut self,k:K,v:V);
}

/// An Array/Vec of Characters with associated values of type A; e.g.
/// [`ucchar`](crate::engine::state::State::get_uccode), [`catcode`](crate::engine::state::State::get_catcode_scheme) etc.
#[derive(Clone)]
pub struct CharField<C:CharType,A:Clone+Default>{pub charfield: C::Allchars<A>, changes: Vec<BTreeMap<C,A>>}
impl<C:CharType,A:Clone+Default> StateField for CharField<C,A> {
    fn push_stack(&mut self) { self.changes.push(BTreeMap::default()) }
    fn pop_stack(&mut self) {
        if let Some(m) = self.changes.pop() {
            for (k,v) in m {
                self.charfield.set(&k,v)
            }
        }
    }
}
impl<C:CharType,A:Clone+Default> KeyValueField<C,A> for CharField<C,A> {
    //#[inline(always)]
    fn get(&self, c:&C) -> Option<&A> { Some(self.charfield.get(c)) }
    //#[inline(always)]
    fn set_locally(&mut self, k: C, v:A) {
        if let Some(m) = self.changes.last_mut() {
            match m.entry(k) {
                Entry::Vacant(e) => {
                    e.insert(self.charfield.replace(&k,v));
                }
                Entry::Occupied(_) => {
                    self.charfield.set(&k,v);
                }
            }
        } else {
            self.charfield.set(&k,v);
        }
    }
    fn set_globally(&mut self, k: C, v:A) {
        self.charfield.set(&k,v);
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

#[derive(Clone)]
pub struct BoxField<ET:EngineType>(Vec<HVBox<ET>>,Vec<BTreeMap<usize,HVBox<ET>>>);
impl <ET:EngineType> BoxField<ET> {
    pub fn push_stack(&mut self) { self.1.push(BTreeMap::default()) }
    pub fn pop_stack(&mut self) {
        if let Some(m) = self.1.pop() {
            for (k,v) in m {
                self.0[k] = v
            }
        }
    }
    pub fn new() -> Self { BoxField(Vec::new(),Vec::new()) }
    pub fn get(&self, k: usize) -> &HVBox<ET> {
        if k < self.0.len() {
            &self.0[k]
        } else {
            &HVBox::Void
        }
    }
    pub fn get_mut(&mut self,k:usize) -> Option<&mut HVBox<ET>> {
        if k < self.0.len() {
            Some(&mut self.0[k])
        } else {
            None
        }
    }
    pub fn take(&mut self, k: usize) -> HVBox<ET> {
        if k < self.0.len() {
            std::mem::replace(&mut self.0[k],HVBox::Void)
        } else {
            HVBox::Void
        }
    }
    fn set_inner(vec:&mut Vec<HVBox<ET>>,k:usize,v:HVBox<ET>) -> Option<HVBox<ET>> {
        if k < vec.len() {
            Some(std::mem::replace(&mut vec[k],v))
        } else {
            vec.resize(k+1,HVBox::Void);
            vec[k] = v;
            None
        }
    }
    pub fn set_locally(&mut self, k: usize, v: HVBox<ET>) {
        if let Some(m) = self.1.last_mut() {
            match m.entry(k) {
                Entry::Vacant(e) => {
                    e.insert(match Self::set_inner(&mut self.0,k,v) {
                        Some(old) => old,
                        None => HVBox::Void
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
    pub fn set_globally(&mut self, k: usize, v: HVBox<ET>) {
        Self::set_inner(&mut self.0,k,v);
        for ov in self.1.iter_mut() {
            ov.remove(&k);
        }
    }
}


#[derive(Clone)]
pub struct TokField<ET:EngineType>(Vec<Vec<Token<ET>>>,Vec<BTreeMap<usize,Vec<Token<ET>>>>);
impl<ET:EngineType> TokField<ET>{
    /// initializes a new [`TokField`].
    pub fn new() -> Self { TokField(Vec::with_capacity(255), Vec::new()) }
    pub(crate) fn get(&self, k: usize) -> Option<&Vec<Token<ET>>> {
        if k < self.0.len() {
            Some(&self.0[k])
        } else {
            None
        }
    }

    pub(crate) fn set_locally(&mut self, k: usize, v: Vec<Token<ET>>,memory:&mut Memory<ET>) {
        if let Some(m) = self.1.last_mut() {
            match m.entry(k) {
                Entry::Vacant(e) => {
                    e.insert(match Self::set_inner(&mut self.0,k,v,memory) {
                        Some(old) => old,
                        None => memory.get_token_vec()
                    });
                }
                Entry::Occupied(_) => {
                    match Self::set_inner(&mut self.0,k,v,memory) {
                        None => (),
                        Some(v) => memory.return_token_vec(v)
                    }
                }
            }
        } else {
            match Self::set_inner(&mut self.0,k,v,memory) {
                None => (),
                Some(v) => memory.return_token_vec(v)
            }
        }
    }

    // #[inline(always)]
    pub(crate) fn set_globally(&mut self, k: usize, v: Vec<Token<ET>>,memory:&mut Memory<ET>) {
        Self::set_inner(&mut self.0,k,v,memory);
        for ov in self.1.iter_mut() {
            match ov.remove(&k) {
                None => (),
                Some(v) => memory.return_token_vec(v)
            }
        }
    }
    fn set_inner(vec:&mut Vec<Vec<Token<ET>>>,k:usize,v:Vec<Token<ET>>,memory:&mut Memory<ET>) -> Option<Vec<Token<ET>>> {
        if k < vec.len() {
            Some(std::mem::replace(&mut vec[k], v))
        } else {
            vec.resize(k + 1, memory.get_token_vec());
            vec[k] = v;
            None
        }
    }

    pub(crate) fn push_stack(&mut self) { self.1.push(BTreeMap::default()) }
    pub(crate) fn pop_stack(&mut self, memory:&mut Memory<ET>) {
        if let Some(m) = self.1.pop() {
            for (k,v) in m {
                match Self::set_inner(&mut self.0,k,v,memory) {
                    None => (),
                    Some(v) => memory.return_token_vec(v)
                }
            }
        }
    }

}

/// A Vec of values of type A; e.g. [`intregisters`](crate::engine::state::State::get_int_register),

#[derive(Clone)]
pub struct VecField<V:Default>(Vec<V>, Vec<BTreeMap<usize,V>>);
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
    fn push_stack(&mut self) { self.1.push(BTreeMap::default()) }
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
    fn get(&self, k: &usize) -> Option<&V> {
        if *k < self.0.len() {
            Some(&self.0[*k])
        } else {
            None
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
/// A HashMap of values of type V; e.g. [`Command`](crate::tex::commands::BaseCommand)s,
#[derive(Clone)]
pub struct TokMapField<ET:EngineType>(HMap<&'static str,Vec<Token<ET>>>,Vec<BTreeMap<&'static str,Vec<Token<ET>>>>);
impl<ET:EngineType> TokMapField<ET> {
    pub fn new() -> Self { Self(HMap::default(),Vec::new()) }
    pub fn set_i(map:&mut HMap<&'static str,Vec<Token<ET>>>,k:&'static str,v:Vec<Token<ET>>) -> Option<Vec<Token<ET>>> {
        if v.is_empty() {
            map.remove(&k)
        } else {
            map.insert(k,v)
        }
    }
    // #[inline(always)]
    pub(crate) fn push_stack(&mut self) { self.1.push(BTreeMap::default()) }
    // #[inline(always)]
    pub(crate) fn pop_stack(&mut self, memory:&mut Memory<ET>) {
        if let Some(m) = self.1.pop() {
            for (k,v) in m {
                match Self::set_i(&mut self.0,k,v) {
                    None => (),
                    Some(v) => memory.return_token_vec(v)
                }
            }
        }
    }
    pub(crate) fn get(&self, k: &'static str) -> Option<&Vec<Token<ET>>> {
        self.0.get(k)
    }

    pub(crate) fn set_locally(&mut self, k: &'static str, v: Vec<Token<ET>>, memory:&mut Memory<ET>) {
        if let Some(m) = self.1.last_mut() {
            match m.entry(k.clone()) {
                Entry::Vacant(e) => {
                    e.insert(Self::set_i(&mut self.0,k,v).unwrap_or(memory.get_token_vec()));
                }
                Entry::Occupied(_) => {
                    match Self::set_i(&mut self.0,k,v) {
                        None => (),
                        Some(v) => memory.return_token_vec(v)
                    }
                }
            }
        } else {
            match Self::set_i(&mut self.0,k,v) {
                None => (),
                Some(v) => memory.return_token_vec(v)
            }
        }
    }

    // #[inline(always)]
    pub(crate) fn set_globally(&mut self, k: &'static str, v: Vec<Token<ET>>, memory:&mut Memory<ET>) {
        for ov in self.1.iter_mut() {
            match ov.remove(&k) {
                None => (),
                Some(v) => memory.return_token_vec(v)
            }
        }
        match Self::set_i(&mut self.0,k,v) {
            None => (),
            Some(v) => memory.return_token_vec(v)
        }
    }
}

/// A HashMap of values of type V; e.g. [`Command`](crate::tex::commands::BaseCommand)s,
#[derive(Clone)]
pub struct HashMapField<K:Eq+Hash+Clone,V:Default+Clone+IsDefault>(pub HMap<K,V>, Vec<BTreeMap<K,V>>);
impl<K:Eq+Hash+Clone,V:Default+Clone+IsDefault> HashMapField<K,V> {
    /// initializes a new [`HashMapField`].
    //#[inline(always)]
    pub fn new() -> Self { HashMapField(HMap::default(), Vec::new()) }
    //#[inline(always)]
    pub fn set_i(map:&mut HMap<K,V>, k: K, v: V) -> Option<V> {
        if v.is_default() {
            map.remove(&k)
        } else {
            map.insert(k,v)
        }
    }
}
impl<K:Eq+Hash+Clone,V:Default+Clone+IsDefault> StateField for HashMapField<K,V> {
    // #[inline(always)]
    fn push_stack(&mut self) { self.1.push(BTreeMap::default()) }
    // #[inline(always)]
    fn pop_stack(&mut self) {
        if let Some(m) = self.1.pop() {
            for (k,v) in m {
                Self::set_i(&mut self.0,k,v);
            }
        }
    }
}
impl<K:Eq+Hash+Clone + Ord,V:Default+Clone+IsDefault> KeyValueField<K,V> for HashMapField<K,V> {
    // #[inline(always)]
    fn get(&self, k: &K) -> Option<&V> {
        self.0.get(k)
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