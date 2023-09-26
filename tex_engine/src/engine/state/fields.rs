/*! Abstractions for fields of a [`State`](crate::engine::state::State). The base trait being
   [`StateField`].

  Most fields are [`KeyValueField`]s which map Keys of some type `K` to some value of type `V`.
 */

use std::collections::BTreeMap;
use crate::utils::strings::{AllCharsTrait, CharType, TeXStr};
use std::collections::btree_map::Entry;
use std::hash::Hash;
use crate::engine::EngineType;
use crate::engine::memory::{Interner, Memory};
use crate::engine::state::modes::{BoxMode, GroupType, TeXMode};
use crate::engine::state::State;
use crate::tex::catcodes::{CategoryCode, CategoryCodeScheme};
use crate::tex::commands::Command;
use crate::tex::nodes::HVBox;
use crate::tex::numbers::{MuSkip, Skip};
use crate::tex::token::Token;
use crate::{debug_log, throw};
use crate::utils::collections::HMap;
use crate::tex::numbers::*;
use crate::engine::filesystem::*;
use string_interner::Symbol;

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
                self.charfield.set(k,v)
            }
        }
    }
}
impl<C:CharType,A:Clone+Default> KeyValueField<C,A> for CharField<C,A> {
    //#[inline(always)]
    fn get(&self, c:&C) -> Option<&A> { Some(self.charfield.get(*c)) }
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
pub struct TokField<ET:EngineType>(Vec<Vec<ET::Token>>,Vec<BTreeMap<usize,Vec<ET::Token>>>);
impl<ET:EngineType> TokField<ET>{
    /// initializes a new [`TokField`].
    pub fn new() -> Self { TokField(Vec::with_capacity(255), Vec::new()) }
    pub(crate) fn get(&self, k: usize) -> Option<&Vec<ET::Token>> {
        if k < self.0.len() {
            Some(&self.0[k])
        } else {
            None
        }
    }

    pub(crate) fn set_locally(&mut self, k: usize, v: Vec<ET::Token>,memory:&mut Memory<ET>) {
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
    pub(crate) fn set_globally(&mut self, k: usize, v: Vec<ET::Token>,memory:&mut Memory<ET>) {
        Self::set_inner(&mut self.0,k,v,memory);
        for ov in self.1.iter_mut() {
            match ov.remove(&k) {
                None => (),
                Some(v) => memory.return_token_vec(v)
            }
        }
    }
    fn set_inner(vec:&mut Vec<Vec<ET::Token>>,k:usize,v:Vec<ET::Token>,memory:&mut Memory<ET>) -> Option<Vec<ET::Token>> {
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
pub struct TokMapField<ET:EngineType>(HMap<&'static str,Vec<ET::Token>>,Vec<BTreeMap<&'static str,Vec<ET::Token>>>);
impl<ET:EngineType> TokMapField<ET> {
    pub fn new() -> Self { Self(HMap::default(),Vec::new()) }
    pub fn set_i(map:&mut HMap<&'static str,Vec<ET::Token>>,k:&'static str,v:Vec<ET::Token>) -> Option<Vec<ET::Token>> {
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
    pub(crate) fn get(&self, k: &'static str) -> Option<&Vec<ET::Token>> {
        self.0.get(k)
    }

    pub(crate) fn set_locally(&mut self, k: &'static str, v: Vec<ET::Token>, memory:&mut Memory<ET>) {
        if let Some(m) = self.1.last_mut() {
            match m.entry(k) {
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
    pub(crate) fn set_globally(&mut self, k: &'static str, v: Vec<ET::Token>, memory:&mut Memory<ET>) {
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


#[derive(Clone)]
pub struct FieldBasedState<ET:EngineType> {
    out_files:Vec<Option<ET::File>>,
    in_files:Vec<Option<ET::File>>,
    csnames:usize,
    afterassignment:Option<ET::Token>,
    mode: TeXMode,

    grouptype: Vec<(GroupType,Option<TeXMode>)>,
    aftergroups:Vec<Vec<ET::Token>>,

    current_font:SingleValueField<ET::FontRef>,
    parshape:SingleValueField<Option<Vec<(ET::Dim,ET::Dim)>>>,
    endlinechar: SingleValueField<Option<ET::Char>>,
    escapechar: SingleValueField<Option<ET::Char>>,
    newlinechar: SingleValueField<Option<ET::Char>>,
    pub commands: VecField<Option<Command<ET>>>,
    ac_commands: CharField<ET::Char,Option<Command<ET>>>,
    catcodes: CharField<ET::Char,CategoryCode>,
    sfcodes: CharField<ET::Char,ET::Int>,
    ucchar: CharField<ET::Char, ET::Char>,
    lcchar: CharField<ET::Char, ET::Char>,
    mathcodes: CharField<ET::Char,ET::Int>,
    delcodes: CharField<ET::Char,ET::Int>,
    intregisters: VecField<ET::Int>,
    dimregisters: VecField<ET::Dim>,
    skipregisters: VecField<Skip<ET::SkipDim>>,
    muskipregisters: VecField<MuSkip<ET::MuDim,ET::MuStretchShrinkDim>>,
    toksregisters:TokField<ET>,
    boxregisters:BoxField<ET>,
    textfonts:VecField<ET::FontRef>,
    scriptfonts:VecField<ET::FontRef>,
    scriptscriptfonts:VecField<ET::FontRef>,
    primitive_intregisters: HashMapField<&'static str,ET::Int>,
    primitive_dimregisters: HashMapField<&'static str,ET::Dim>,
    primitive_skipregisters: HashMapField<&'static str,Skip<ET::SkipDim>>,
    primitive_muskipregisters: HashMapField<&'static str,MuSkip<ET::MuDim,ET::MuStretchShrinkDim>>,
    primitive_tokregisters: TokMapField<ET>,
}

impl<ET:EngineType> FieldBasedState<ET> {
    pub fn new(fontstore:&ET::FontStore) -> Self {
        let mut state = Self {
            out_files:vec!(),
            in_files:vec!(),
            csnames:0,
            current_font:SingleValueField::new(ET::FontRef::default()),
            afterassignment:None,
            aftergroups:vec!(vec!()),
            parshape:SingleValueField::new(None),
            mode: TeXMode::Vertical,
            /* filesystem: fs,*/
            grouptype: vec![(GroupType::Top,None)],
            endlinechar: SingleValueField::new(Some(ET::Char::carriage_return())),
            escapechar: SingleValueField::new(Some(ET::Char::backslash())),
            newlinechar: SingleValueField::new(Some(ET::Char::newline())),
            commands: VecField::new(),
            ac_commands: CharField::new(ET::Char::rep_field(None)),
            catcodes: CharField::new(ET::Char::starting_catcode_scheme()),
            sfcodes: CharField::new(ET::Char::rep_field(ET::Int::default())),
            ucchar: CharField::new(ET::Char::ident()),
            lcchar: CharField::new(ET::Char::ident()),
            mathcodes: CharField::new(ET::Char::rep_field(ET::Int::default())),
            delcodes: CharField::new(ET::Char::rep_field(ET::Int::default())),
            intregisters: VecField::new(),
            dimregisters: VecField::new(),
            skipregisters: VecField::new(),
            muskipregisters: VecField::new(),
            toksregisters: TokField::new(),
            boxregisters: BoxField::new(),

            textfonts:VecField::new(),
            scriptfonts:VecField::new(),
            scriptscriptfonts:VecField::new(),

            primitive_intregisters: HashMapField::new(),
            primitive_dimregisters: HashMapField::new(),
            primitive_skipregisters: HashMapField::new(),
            primitive_muskipregisters: HashMapField::new(),
            primitive_tokregisters: TokMapField::new(),
        };
        for i in 97..123 {
            state.ucchar.set_locally((i as u8).into(),((i-32) as u8).into());
            state.lcchar.set_locally(((i-32) as u8).into(),(i as u8).into());
            state.mathcodes.set_locally(ET::Char::from(i-32),
                                        ET::Int::from_i64((i as i64-32) +
                                            (1 * 16 * 16) +
                                            (7 * 16 * 16 * 16))
            );
            state.mathcodes.set_locally(ET::Char::from(i),
                                        ET::Int::from_i64((i as i64) +
                                            (1 * 16 * 16) +
                                            (7 * 16 * 16 * 16))
            );
        }
        for i in 48..58 {
            state.mathcodes.set_locally(ET::Char::from(i),
                                        ET::Int::from_i64((i as i64) +
                                            (0 * 16 * 16) +
                                            (7 * 16 * 16 * 16))
            );
        }
        state
    }
}

impl<ET:EngineType> State<ET> for FieldBasedState<ET> {
    fn get_current_font(&self) -> ET::FontRef {
        *self.current_font.get()
    }
    fn set_current_font(&mut self, f:ET::FontRef, globally: bool) {
        let globaldefs = self.get_primitive_int("globaldefs").to_i64();
        let globally = if globaldefs == 0 {globally} else {globaldefs > 0};
        if globally {
            self.current_font.set_globally(f)
        } else {
            self.current_font.set_locally(f)
        }
    }
    fn grouplevel(&self) -> usize {
        self.grouptype.len()
    }

    fn set_afterassignment(&mut self, t: ET::Token) {
        self.afterassignment = Some(t)
    }
    fn take_afterassignment(&mut self) -> Option<ET::Token> {
        self.afterassignment.take()
    }

    fn mode(&self) -> TeXMode { self.mode }
    fn push_csname(&mut self) -> usize {
        self.csnames += 1;
        self.csnames
    }
    fn current_csname(&self) -> Option<usize> {
        match self.csnames {
            0 => None,
            _ => Some(self.csnames)
        }
    }
    fn pop_csname(&mut self) {
        self.csnames -= 1;
    }
    fn file_openin(&mut self, i: usize, f: ET::File,interner:&mut Interner) {
        if i >= self.in_files.len() {
            self.in_files.resize(i+1,None);
        }
        f.open_in(interner);
        self.in_files[i] = Some(f);
    }
    fn file_closein(&mut self, i: usize) {
        if i >= self.in_files.len() {
            todo!("Error closeout")
            //self.out_files.resize(i,None);
        }
        if let Some(f) = &self.in_files[i] {
            f.close_in();
        }
        self.in_files[i] = None
    }
    fn file_openout(&mut self, i: usize, f: ET::File) {
        f.open_out();
        if i >= self.out_files.len() {
            self.out_files.resize(i+1,None);
        }
        self.out_files[i] = Some(f);
    }
    fn file_closeout(&mut self, i: usize) {
        if i >= self.out_files.len() {
            todo!("Error closeout")
            //self.out_files.resize(i,None);
        }
        if let Some(f) = &self.out_files[i] {
            f.close_out();
        }
        self.out_files[i] = None
    }
    fn get_open_out_file(&self,i:usize) -> Option<ET::File> {
        if i >= self.out_files.len() {
            None
        } else {
            self.out_files[i].clone()
        }
    }
    fn get_open_in_file(&self,i:usize) -> Option<ET::File> {
        if i >= self.in_files.len() {
            None
        } else {
            self.in_files[i].clone()
        }
    }
    // #[inline(always)]
    fn stack_push(&mut self, g: GroupType) {
        debug_log!(trace => "PUSH {:?} => {:?}",self.mode,g);
        match g {
            GroupType::Box(m) => {
                self.grouptype.push((g,Some(self.mode)));
                self.mode = match m {
                    BoxMode::H => TeXMode::RestrictedHorizontal,
                    BoxMode::M => TeXMode::Math,
                    BoxMode::DM => TeXMode::Displaymath,
                    BoxMode::V => TeXMode::InternalVertical,
                    _ => self.mode
                };
            },
            _ => self.grouptype.push((g,None))
        }
        self.current_font.push_stack();
        self.parshape.push_stack();
        self.endlinechar.push_stack();
        self.escapechar.push_stack();
        self.newlinechar.push_stack();

        self.commands.push_stack();
        self.ac_commands.push_stack();

        self.catcodes.push_stack();
        self.sfcodes.push_stack();
        self.ucchar.push_stack();
        self.lcchar.push_stack();
        self.mathcodes.push_stack();
        self.delcodes.push_stack();

        self.intregisters.push_stack();
        self.dimregisters.push_stack();
        self.skipregisters.push_stack();
        self.muskipregisters.push_stack();
        self.toksregisters.push_stack();
        self.boxregisters.push_stack();

        self.textfonts.push_stack();
        self.scriptfonts.push_stack();
        self.scriptscriptfonts.push_stack();

        self.primitive_intregisters.push_stack();
        self.primitive_dimregisters.push_stack();
        self.primitive_skipregisters.push_stack();
        self.primitive_muskipregisters.push_stack();
        self.primitive_tokregisters.push_stack();
        self.aftergroups.push(vec!());
    }
    fn stack_pop(&mut self,memory:&mut Memory<ET>) -> Option<(Vec<ET::Token>,GroupType)> {
        let gt = match self.grouptype.pop() {
            None => return None,
            Some((gt,Some(m))) => {
                debug_log!(trace => "POP {:?} => {:?}",gt,m);
                self.mode = m;
                gt
            }
            Some((gt,_)) => gt
        };
        self.current_font.pop_stack();
        self.parshape.pop_stack();
        self.endlinechar.pop_stack();
        self.escapechar.pop_stack();
        self.newlinechar.pop_stack();

        self.commands.pop_stack();
        self.ac_commands.pop_stack();

        self.catcodes.pop_stack();
        self.sfcodes.pop_stack();
        self.ucchar.pop_stack();
        self.lcchar.pop_stack();
        self.mathcodes.pop_stack();
        self.delcodes.pop_stack();

        self.intregisters.pop_stack();
        self.dimregisters.pop_stack();
        self.skipregisters.pop_stack();
        self.muskipregisters.pop_stack();
        self.toksregisters.pop_stack(memory);
        self.boxregisters.pop_stack();

        self.textfonts.pop_stack();
        self.scriptfonts.pop_stack();
        self.scriptscriptfonts.pop_stack();

        self.primitive_intregisters.pop_stack();
        self.primitive_dimregisters.pop_stack();
        self.primitive_skipregisters.pop_stack();
        self.primitive_muskipregisters.pop_stack();
        self.primitive_tokregisters.pop_stack(memory);

        Some((self.aftergroups.pop().unwrap_or(vec!()),gt))
    }

    fn set_mode(&mut self, mode: TeXMode) {
        self.mode = mode
    }

    // #[inline(always)]
    fn get_grouptype(&self) -> GroupType { *self.grouptype.last().map(|(t,_)| t).unwrap_or(&GroupType::Top) }

    fn get_parshape(&self) -> Option<&Vec<(ET::Dim, ET::Dim)>> {
        self.parshape.get().as_ref()
    }
    fn set_parshape(&mut self, v: Vec<(ET::Dim, ET::Dim)>, globally: bool) {
        let globaldefs = self.get_primitive_int("globaldefs").to_i64();
        let globally = if globaldefs == 0 {globally} else {globaldefs > 0};
        if globally {
            self.parshape.set_globally(if v.is_empty() {None} else {Some(v)})
        } else {
            self.parshape.set_locally(if v.is_empty() {None} else {Some(v)})
        }
    }

    // #[inline(always)]
    fn get_escapechar(&self) -> Option<ET::Char> { *self.escapechar.get() }
    // #[inline(always)]
    fn set_escapechar(&mut self, c: Option<ET::Char>, globally: bool) {
        let globaldefs = self.get_primitive_int("globaldefs").to_i64();
        let globally = if globaldefs == 0 {globally} else {globaldefs > 0};
        if globally {
            self.escapechar.set_globally(c)
        } else {
            self.escapechar.set_locally(c)
        }
    }
    fn push_aftergroup(&mut self, t: ET::Token) {
        self.aftergroups.last_mut().unwrap().push(t)
    }

    // #[inline(always)]
    fn get_endlinechar(&self) -> Option<ET::Char> { *self.endlinechar.get() }
    // #[inline(always)]
    fn set_endlinechar(&mut self, c: Option<ET::Char>, globally: bool) {
        let globaldefs = self.get_primitive_int("globaldefs").to_i64();
        let globally = if globaldefs == 0 {globally} else {globaldefs > 0};
        if globally {
            self.endlinechar.set_globally(c)
        } else {
            self.endlinechar.set_locally(c)
        }
    }

    // #[inline(always)]
    fn get_newlinechar(&self) -> Option<ET::Char> { *self.newlinechar.get() }
    // #[inline(always)]
    fn set_newlinechar(&mut self, c: Option<ET::Char>, globally: bool) {
        let globaldefs = self.get_primitive_int("globaldefs").to_i64();
        let globally = if globaldefs == 0 {globally} else {globaldefs > 0};
        if globally {
            self.newlinechar.set_globally(c)
        } else {
            self.newlinechar.set_locally(c)
        }
    }

    fn get_sfcode(&self, c: ET::Char) -> ET::Int {
        *self.sfcodes.get(&c).unwrap_or(&ET::Int::default())
    }
    fn set_sfcode(&mut self, c: ET::Char, v: ET::Int, globally: bool) {
        let globaldefs = self.get_primitive_int("globaldefs").to_i64();
        let globally = if globaldefs == 0 {globally} else {globaldefs > 0};
        if globally {
            self.sfcodes.set_globally(c,v)
        } else {
            self.sfcodes.set_locally(c,v)
        }
    }

    fn get_mathcode(&self, c: ET::Char) -> ET::Int {
        *self.mathcodes.get(&c).unwrap_or(&ET::Int::default())
    }
    fn set_mathcode(&mut self, c: ET::Char, lc: ET::Int, globally: bool) {
        let globaldefs = self.get_primitive_int("globaldefs").to_i64();
        let globally = if globaldefs == 0 {globally} else {globaldefs > 0};
        if globally {
            self.mathcodes.set_globally(c,lc)
        } else {
            self.mathcodes.set_locally(c,lc)
        }
    }

    fn get_delcode(&self, c: ET::Char) -> ET::Int {
        *self.delcodes.get(&c).unwrap_or(&ET::Int::default())
    }
    fn set_delcode(&mut self, c: ET::Char, lc: ET::Int, globally: bool) {
        let globaldefs = self.get_primitive_int("globaldefs").to_i64();
        let globally = if globaldefs == 0 {globally} else {globaldefs > 0};
        if globally {
            self.delcodes.set_globally(c,lc)
        } else {
            self.delcodes.set_locally(c,lc)
        }
    }

    // #[inline(always)]
    fn get_command(&self, name: TeXStr) -> Option<&Command<ET>> {
        match self.commands.get(&name.0.to_usize()) {
            Some(r) => r.as_ref(),
            _ => None
        }//.as_ref().map(|c| *c)
    }
    // #[inline(always)]
    fn set_command(&mut self, name: TeXStr, cmd: Option<Command<ET>>, globally: bool) {
        let globaldefs = self.get_primitive_int("globaldefs").to_i64();
        let globally = if globaldefs == 0 {globally} else {globaldefs > 0};
        if globally {
            self.commands.set_globally(name.0.to_usize(),cmd)
        } else {
            self.commands.set_locally(name.0.to_usize(),cmd)
        }
    }
    fn get_ac_command(&self, c: ET::Char) -> Option<&Command<ET>> {
        match self.ac_commands.get(&c) {
            Some(r) => r.as_ref(),
            _ => None
        }
    }
    fn set_ac_command(&mut self, c: ET::Char, cmd: Option<Command<ET>>, globally: bool) {
        let globaldefs = self.get_primitive_int("globaldefs").to_i64();
        let globally = if globaldefs == 0 {globally} else {globaldefs > 0};
        if globally {
            self.ac_commands.set_globally(c,cmd)
        } else {
            self.ac_commands.set_locally(c,cmd)
        }
    }


    // #[inline(always)]
    fn get_catcode_scheme(&self) -> &CategoryCodeScheme<ET::Char> {
        &self.catcodes.charfield
    }
    // #[inline(always)]
    fn set_catcode(&mut self, c: ET::Char, cc: CategoryCode, globally: bool) {
        let globaldefs = self.get_primitive_int("globaldefs").to_i64();
        let globally = if globaldefs == 0 {globally} else {globaldefs > 0};
        if globally {
            self.catcodes.set_globally(c,cc)
        } else {
            self.catcodes.set_locally(c,cc)
        }
    }

    // #[inline(always)]
    fn get_uccode(&self, c: ET::Char) -> ET::Char {
        *self.ucchar.get(&c).unwrap_or(&ET::Char::default())
    }
    // #[inline(always)]
    fn set_uccode(&mut self, c: ET::Char, uc: ET::Char, globally: bool) {
        let globaldefs = self.get_primitive_int("globaldefs").to_i64();
        let globally = if globaldefs == 0 {globally} else {globaldefs > 0};
        if globally {
            self.ucchar.set_globally(c,uc)
        } else {
            self.ucchar.set_locally(c,uc)
        }
    }

    // #[inline(always)]
    fn get_lccode(&self, c: ET::Char) -> ET::Char {
        *self.lcchar.get(&c).unwrap_or(&ET::Char::default())
    }
    // #[inline(always)]
    fn set_lccode(&mut self, c: ET::Char, lc: ET::Char, globally: bool) {
        let globaldefs = self.get_primitive_int("globaldefs").to_i64();
        let globally = if globaldefs == 0 {globally} else {globaldefs > 0};
        if globally {
            self.lcchar.set_globally(c,lc)
        } else {
            self.lcchar.set_locally(c,lc)
        }
    }

    // #[inline(always)]
    fn get_int_register(&self, i: usize) -> ET::Int {
        *self.intregisters.get(&i).unwrap_or(&ET::Int::default())
    }
    // #[inline(always)]
    fn set_int_register(&mut self, i: usize, v: ET::Int, globally: bool) {
        let globaldefs = self.get_primitive_int("globaldefs").to_i64();
        let globally = if globaldefs == 0 {globally} else {globaldefs > 0};
        if globally {
            self.intregisters.set_globally(i,v)
        } else {
            self.intregisters.set_locally(i,v)
        }
    }

    fn get_dim_register(&self, i: usize) -> ET::Dim { *self.dimregisters.get(&i).unwrap_or(&ET::Dim::default()) }
    fn set_dim_register(&mut self, i: usize, v: ET::Dim, globally: bool) {
        let globaldefs = self.get_primitive_int("globaldefs").to_i64();
        let globally = if globaldefs == 0 {globally} else {globaldefs > 0};
        if globally {
            self.dimregisters.set_globally(i,v)
        } else {
            self.dimregisters.set_locally(i,v)
        }
    }

    fn get_skip_register(&self, i: usize) -> Skip<ET::SkipDim> { *self.skipregisters.get(&i).unwrap_or(&Skip::default()) }
    fn set_skip_register(&mut self, i: usize, v: Skip<ET::SkipDim>, globally: bool) {
        let globaldefs = self.get_primitive_int("globaldefs").to_i64();
        let globally = if globaldefs == 0 {globally} else {globaldefs > 0};
        if globally {
            self.skipregisters.set_globally(i,v)
        } else {
            self.skipregisters.set_locally(i,v)
        }
    }

    fn get_muskip_register(&self, i: usize) -> MuSkip<ET::MuDim,ET::MuStretchShrinkDim> { *self.muskipregisters.get(&i).unwrap_or(&MuSkip::default()) }
    fn set_muskip_register(&mut self, i: usize, v: MuSkip<ET::MuDim,ET::MuStretchShrinkDim>, globally: bool) {
        let globaldefs = self.get_primitive_int("globaldefs").to_i64();
        let globally = if globaldefs == 0 {globally} else {globaldefs > 0};
        if globally {
            self.muskipregisters.set_globally(i,v)
        } else {
            self.muskipregisters.set_locally(i,v)
        }
    }

    fn get_toks_register(&self, i: usize) -> Option<&Vec<ET::Token>> { self.toksregisters.get(i) }
    fn set_toks_register(&mut self, i: usize, v: Vec<ET::Token>, globally: bool,memory:&mut Memory<ET>) {
        let globaldefs = self.get_primitive_int("globaldefs").to_i64();
        let globally = if globaldefs == 0 {globally} else {globaldefs > 0};
        if globally {
            self.toksregisters.set_globally(i,v,memory)
        } else {
            self.toksregisters.set_locally(i,v,memory)
        }
    }

    fn get_box_register(&mut self, i: usize) -> Option<&mut HVBox<ET>> {
        self.boxregisters.get_mut(i)
    }
    fn take_box_register(&mut self, i: usize) -> HVBox<ET> {
        self.boxregisters.take(i)
    }
    fn set_box_register(&mut self, i: usize, v: HVBox<ET>, globally: bool) {
        let globaldefs = self.get_primitive_int("globaldefs").to_i64();
        let globally = if globaldefs == 0 {globally} else {globaldefs > 0};
        if globally {
            self.boxregisters.set_globally(i,v)
        } else {
            self.boxregisters.set_locally(i,v)
        }
    }

    fn get_primitive_int(&self, name: &'static str) -> ET::Int {
        *self.primitive_intregisters.get(&name).unwrap_or(&ET::Int::default())
    }
    fn set_primitive_int(&mut self, name: &'static str, v: ET::Int, globally: bool) {
        let globaldefs = self.get_primitive_int("globaldefs").to_i64();
        let globally = if globaldefs == 0 {globally} else {globaldefs > 0};
        if globally {
            self.primitive_intregisters.set_globally(name,v)
        } else {
            self.primitive_intregisters.set_locally(name,v)
        }
    }

    fn get_primitive_dim(&self, name: &'static str) -> ET::Dim {
        *self.primitive_dimregisters.get(&name).unwrap_or(&ET::Dim::default())
    }
    fn set_primitive_dim(&mut self, name: &'static str, v: ET::Dim, globally: bool) {
        let globaldefs = self.get_primitive_int("globaldefs").to_i64();
        let globally = if globaldefs == 0 {globally} else {globaldefs > 0};
        if globally {
            self.primitive_dimregisters.set_globally(name,v)
        } else {
            self.primitive_dimregisters.set_locally(name,v)
        }
    }

    fn get_primitive_skip(&self, name: &'static str) -> Skip<ET::SkipDim> {
        *self.primitive_skipregisters.get(&name).unwrap_or(&Skip::default())
    }
    fn set_primitive_skip(&mut self, name: &'static str, v: Skip<ET::SkipDim>, globally: bool) {
        let globaldefs = self.get_primitive_int("globaldefs").to_i64();
        let globally = if globaldefs == 0 {globally} else {globaldefs > 0};
        if globally {
            self.primitive_skipregisters.set_globally(name,v)
        } else {
            self.primitive_skipregisters.set_locally(name,v)
        }
    }

    fn get_primitive_muskip(&self, name: &'static str) -> MuSkip<ET::MuDim,ET::MuStretchShrinkDim> {
        *self.primitive_muskipregisters.get(&name).unwrap_or(&MuSkip::default())
    }
    fn set_primitive_muskip(&mut self, name: &'static str, v: MuSkip<ET::MuDim,ET::MuStretchShrinkDim>, globally: bool) {
        let globaldefs = self.get_primitive_int("globaldefs").to_i64();
        let globally = if globaldefs == 0 {globally} else {globaldefs > 0};
        if globally {
            self.primitive_muskipregisters.set_globally(name,v)
        } else {
            self.primitive_muskipregisters.set_locally(name,v)
        }
    }

    fn get_primitive_toks(&self, name: &'static str) -> Option<&Vec<ET::Token>> { self.primitive_tokregisters.get(&name) }
    fn set_primitive_toks(&mut self, name: &'static str, v: Vec<ET::Token>, globally: bool,memory:&mut Memory<ET>) {
        let globaldefs = self.get_primitive_int("globaldefs").to_i64();
        let globally = if globaldefs == 0 {globally} else {globaldefs > 0};
        if globally {
            self.primitive_tokregisters.set_globally(name,v,memory)
        } else {
            self.primitive_tokregisters.set_locally(name,v,memory)
        }
    }

    fn get_textfont(&self, i: usize) -> ET::FontRef {
        *self.textfonts.get(&i).unwrap_or(&ET::FontRef::default())
    }
    fn get_scriptfont(&self, i: usize) -> ET::FontRef {
        *self.scriptfonts.get(&i).unwrap_or(&ET::FontRef::default())
    }
    fn get_scriptscriptfont(&self, i: usize) -> ET::FontRef {
        *self.scriptscriptfonts.get(&i).unwrap_or(&ET::FontRef::default())
    }
    fn set_textfont(&mut self, i: usize, f: ET::FontRef, globally: bool) {
        let globaldefs = self.get_primitive_int("globaldefs").to_i64();
        let globally = if globaldefs == 0 {globally} else {
            globaldefs > 0
        };
        if globally {
            self.textfonts.set_globally(i,f)
        } else {
            self.textfonts.set_locally(i,f)
        }
    }
    fn set_scriptfont(&mut self, i: usize, f: ET::FontRef, globally: bool) {
        let globaldefs = self.get_primitive_int("globaldefs").to_i64();
        let globally = if globaldefs == 0 {
            globally
        } else {
            globaldefs > 0
        };
        if globally {
            self.scriptfonts.set_globally(i,f)
        } else {
            self.scriptfonts.set_locally(i,f)
        }
    }
    fn set_scriptscriptfont(&mut self, i: usize, f: ET::FontRef, globally: bool) {
        let globaldefs = self.get_primitive_int("globaldefs").to_i64();
        let globally = if globaldefs == 0 {
            globally
        } else {
            globaldefs > 0
        };
        if globally {
            self.scriptscriptfonts.set_globally(i,f)
        } else {
            self.scriptscriptfonts.set_locally(i,f)
        }
    }
}
