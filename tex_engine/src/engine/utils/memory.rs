/*! Memory manangement and string interning. */

use std::sync::RwLock;
use lazy_static::lazy_static;
use string_interner::Symbol;
use crate::tex::control_sequences::{ControlSequenceName, ControlSequenceNameHandler};
use crate::tex::input_text::Character;
use crate::tex::token::Token;
use crate::utils::ReusableVectorFactory;

/// Utility component for managing memory allocation, string interning and similar
/// tasks one might want to do.
pub trait MemoryManager<T:Token> {
    /// Returns the interner for control sequence names.
    fn cs_interner(&self) -> &<T::CS as ControlSequenceName>::Handler;
    /// Returns the interner for control sequence names mutably.
    fn cs_interner_mut(&mut self) -> &mut <T::CS as ControlSequenceName>::Handler;

    fn get_string(&mut self) -> String {
        String::new()
    }
    fn return_string(&mut self,_:String) {}
    fn get_bytes(&mut self) -> Vec<u8> {
        Vec::new()
    }
    fn return_bytes(&mut self,_:Vec<u8>) {}
    fn get_token_vec(&mut self) -> Vec<T> {
        Vec::new()
    }
    fn return_token_vec(&mut self,_:Vec<T>) {}
}
impl<CS:ControlSequenceName<Handler=()>,T:Token<CS=CS>> MemoryManager<T> for () {
    #[inline(always)]
    fn cs_interner(&self) -> &<T::CS as ControlSequenceName>::Handler { self }
    #[inline(always)]
    fn cs_interner_mut(&mut self) -> &mut <T::CS as ControlSequenceName>::Handler { self }
}

/// The default memory manager, which does not do any memory management.
pub struct DefaultMemoryManager<T:Token> {
    handler:<T::CS as ControlSequenceName>::Handler
}
impl<T:Token> DefaultMemoryManager<T> {
    pub fn new() -> Self {
        DefaultMemoryManager {
            handler:<T::CS as ControlSequenceName>::Handler::default()
        }
    }
}

/// A memory manager that reuses allocated memory for token lists.
pub struct ReuseTokenLists<T:Token> {
    factory:ReusableVectorFactory<T>,
    handler:<T::CS as ControlSequenceName>::Handler,
    strings:Vec<String>,
    bytes:Vec<Vec<u8>>,
}
impl<T:Token> ReuseTokenLists<T> {
    pub fn new() -> Self {
        ReuseTokenLists {
            factory:ReusableVectorFactory::new(32,32),
            handler:<T::CS as ControlSequenceName>::Handler::default(),
            strings:Vec::new(),
            bytes:Vec::new(),
        }
    }
}
impl<T:Token> MemoryManager<T> for ReuseTokenLists<T> {
    #[inline(always)]
    fn cs_interner(&self) -> &<T::CS as ControlSequenceName>::Handler { &self.handler }
    #[inline(always)]
    fn cs_interner_mut(&mut self) -> &mut <T::CS as ControlSequenceName>::Handler { &mut self.handler }
    #[inline(always)]
    fn get_string(&mut self) -> String {
        //self.strings.pop().unwrap_or_default()
        String::new()
    }
    #[inline(always)]
    fn return_string(&mut self,mut s:String) {
        /*s.clear();
        self.strings.push(s)*/
    }
    #[inline(always)]
    fn get_bytes(&mut self) -> Vec<u8> {
        Vec::new()//self.bytes.pop().unwrap_or_default()
    }
    #[inline(always)]
    fn return_bytes(&mut self,mut b:Vec<u8>) {
        //b.clear();
        //self.bytes.push(b)
    }
    #[inline(always)]
    fn get_token_vec(&mut self) -> Vec<T> {
        Vec::new()//self.factory.get()
    }
    #[inline(always)]
    fn return_token_vec(&mut self, mut v: Vec<T>) {
        //self.factory.give_back(v)
    }
}

/// We always intern the names for primitive macros, for efficiency; in particular for equality checks.
pub struct PrimitiveInterner {
    interner:RwLock<string_interner::StringInterner::<string_interner::backend::StringBackend<string_interner::symbol::SymbolU16>, ahash::RandomState>>,
    pub globaldefs:PrimitiveIdentifier,
    pub relax:PrimitiveIdentifier,
    pub mag:PrimitiveIdentifier,
    pub fam:PrimitiveIdentifier,
    pub ifcase:PrimitiveIdentifier,
    pub tracingifs:PrimitiveIdentifier,
    pub tracingassigns:PrimitiveIdentifier,
    pub tracingcommands:PrimitiveIdentifier,
    pub tracinggroups:PrimitiveIdentifier,
    pub tracingrestores:PrimitiveIdentifier,
    pub else_:PrimitiveIdentifier,
    pub fi:PrimitiveIdentifier,
    pub or:PrimitiveIdentifier,
    pub global:PrimitiveIdentifier,
    pub long:PrimitiveIdentifier,
    pub outer:PrimitiveIdentifier,
    pub protected:PrimitiveIdentifier,
    pub def:PrimitiveIdentifier,
    pub edef:PrimitiveIdentifier,
    pub xdef:PrimitiveIdentifier,
    pub gdef:PrimitiveIdentifier,
    pub everyeof:PrimitiveIdentifier,
    pub everyhbox:PrimitiveIdentifier,
    pub everyvbox:PrimitiveIdentifier,
    pub count:PrimitiveIdentifier,
    pub noexpand:PrimitiveIdentifier,
    pub unexpanded:PrimitiveIdentifier,
    pub endcsname:PrimitiveIdentifier,
    pub the:PrimitiveIdentifier,
    pub toks:PrimitiveIdentifier,
}
impl PrimitiveInterner {
    fn new() -> Self {
        let mut interner = string_interner::StringInterner::<string_interner::backend::StringBackend<string_interner::symbol::SymbolU16>, ahash::RandomState>::new();
        let globaldefs = PrimitiveIdentifier(interner.get_or_intern_static("globaldefs"));
        let relax = PrimitiveIdentifier(interner.get_or_intern_static("relax"));
        let mag = PrimitiveIdentifier(interner.get_or_intern_static("mag"));
        let fam = PrimitiveIdentifier(interner.get_or_intern_static("fam"));
        let ifcase = PrimitiveIdentifier(interner.get_or_intern_static("ifcase"));
        let tracingifs = PrimitiveIdentifier(interner.get_or_intern_static("tracingifs"));
        let tracingassigns = PrimitiveIdentifier(interner.get_or_intern_static("tracingassigns"));
        let tracingcommands = PrimitiveIdentifier(interner.get_or_intern_static("tracingcommands"));
        let tracinggroups = PrimitiveIdentifier(interner.get_or_intern_static("tracinggroups"));
        let tracingrestores = PrimitiveIdentifier(interner.get_or_intern_static("tracingrestores"));
        let else_ = PrimitiveIdentifier(interner.get_or_intern_static("else"));
        let fi = PrimitiveIdentifier(interner.get_or_intern_static("fi"));
        let or = PrimitiveIdentifier(interner.get_or_intern_static("or"));
        let global = PrimitiveIdentifier(interner.get_or_intern_static("global"));
        let long = PrimitiveIdentifier(interner.get_or_intern_static("long"));
        let outer = PrimitiveIdentifier(interner.get_or_intern_static("outer"));
        let protected = PrimitiveIdentifier(interner.get_or_intern_static("protected"));
        let def = PrimitiveIdentifier(interner.get_or_intern_static("def"));
        let edef = PrimitiveIdentifier(interner.get_or_intern_static("edef"));
        let xdef = PrimitiveIdentifier(interner.get_or_intern_static("xdef"));
        let gdef = PrimitiveIdentifier(interner.get_or_intern_static("gdef"));
        let everyeof = PrimitiveIdentifier(interner.get_or_intern_static("everyeof"));
        let everyhbox = PrimitiveIdentifier(interner.get_or_intern_static("everyhbox"));
        let everyvbox = PrimitiveIdentifier(interner.get_or_intern_static("everyvbox"));
        let count = PrimitiveIdentifier(interner.get_or_intern_static("count"));
        let noexpand = PrimitiveIdentifier(interner.get_or_intern_static("noexpand"));
        let endcsname = PrimitiveIdentifier(interner.get_or_intern_static("endcsname"));
        let unexpanded = PrimitiveIdentifier(interner.get_or_intern_static("unexpanded"));
        let the = PrimitiveIdentifier(interner.get_or_intern_static("the"));
        let toks = PrimitiveIdentifier(interner.get_or_intern_static("toks"));
        PrimitiveInterner{
            interner:RwLock::new(interner),
            globaldefs, relax, mag, fam, ifcase, tracingifs, tracingassigns, tracingcommands,
            tracinggroups, else_, fi, or, global, long, outer, protected, def, edef, xdef,
            gdef,everyeof,count,tracingrestores,noexpand,endcsname,unexpanded,the,toks,
            everyhbox,everyvbox
        }
    }
    /// Returns the identifier for the given primitive command name, interning it if necessary.
    /// This is thread-safe, but slow, so should only be used ideally once per primitive at startup.
    #[inline(always)]
    pub fn get(&self,s:&'static str) -> PrimitiveIdentifier {
        let mut lock = self.interner.write().unwrap();
        PrimitiveIdentifier(lock.get_or_intern_static(s))
    }
    #[inline(always)]
    pub fn printable<C:Character>(&'static self,ident:PrimitiveIdentifier,escapechar:Option<C>) -> PrintableIdentifier<C> {
        PrintableIdentifier(ident,escapechar,&self)
    }
}
pub struct PrintableIdentifier<C:Character>(PrimitiveIdentifier,Option<C>,&'static PrimitiveInterner);
impl<C:Character> std::fmt::Display for PrintableIdentifier<C> {
    fn fmt(&self,f:&mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let lock = self.2.interner.read().unwrap();
        match self.1 {
            None => (),
            Some(c) => c.display(f)
        }
        write!(f,"{}",lock.resolve(self.0.0).unwrap())
    }
}
lazy_static!(
    /// The global primitive interner.
    pub static ref PRIMITIVES:PrimitiveInterner = PrimitiveInterner::new();
);

/// A `Copy` identifier for a primitive command. Small and fast to compare.
#[derive(Copy,Clone,PartialEq,Eq,Hash,Debug)]
pub struct PrimitiveIdentifier(string_interner::symbol::SymbolU16);
impl PrimitiveIdentifier {
    #[inline(always)]
    pub fn to_usize(&self) -> usize { self.0.to_usize() }
}

/// A `Copy` interned string for a control sequence name.
pub type InternedString = string_interner::symbol::SymbolU32;
type Backend = string_interner::backend::StringBackend<InternedString>;

/// A string interner for control sequence names. Implements [`ControlSequenceNameHandler`].
pub struct StringInterner {
    interner:string_interner::StringInterner<Backend,ahash::RandomState>,
    empty_str:InternedString,
    par:InternedString
}
impl StringInterner {
    /// Creates a new string interner.
    pub fn new() -> Self {
        let mut interner = string_interner::StringInterner::<Backend,ahash::RandomState>::new();
        StringInterner {
            //relax:TeXStr::from_primitive(interner.get_or_intern_static("relax")),
            //par:TeXStr::from_primitive(interner.get_or_intern_static("par")),
            empty_str:interner.get_or_intern_static(""),
            par:interner.get_or_intern_static("par"),
            //noexpand_tk:TeXStr::from_primitive(interner.get_or_intern_static(NOEXPAND_INTERNAL)),
            interner
        }
    }
    /// Resolves an [`InternedString`] to a `&str`.
    #[inline(always)]
    pub fn resolve(&self,string:InternedString) -> &str {
        self.interner.resolve(string).unwrap()
    }
    /// Interns a `&static str`.
    #[inline(always)]
    pub fn from_static(&mut self,s:&'static str) -> InternedString {
        self.interner.get_or_intern_static(s)
    }
    /// Interns a string. For `&static str`s, use [`from_static`](Self::from_static) instead.
    #[inline(always)]
    pub fn from_string<S>(&mut self,s:S) -> InternedString where S:AsRef<str> {
        self.interner.get_or_intern(s)
    }
}

impl ControlSequenceNameHandler<InternedString> for StringInterner {
    type Printable<'a> = &'a str;
    #[inline(always)]
    fn new(&mut self,s: &str) -> InternedString {
        self.from_string(s)
    }
    #[inline(always)]
    fn par(&self) -> InternedString { self.par }
    #[inline(always)]
    fn empty_str(&self) -> InternedString { self.empty_str }
    #[inline(always)]
    fn resolve<'a>(&'a self, cs: &InternedString) -> &'a str {
        self.interner.resolve(*cs).unwrap()
    }
}
impl Default for StringInterner {
    #[inline(always)]
    fn default() -> Self { Self::new() }
}

/*
struct CharacterVecInterner<C:Character> {
    map:HMap<Box<[C]>,u32>,
    ls:Vec<C>
}
impl<C:Character> CharacterVecInterner<C> {
    fn new() -> Self {
        let mut map: HMap<Box<[C]>,u32> = HMap::default();
        map.insert(Box::new([]),0);
        CharacterVecInterner {
            map, ls:Vec::new()
        }
    }
    fn intern(&mut self,v:&[C]) -> u32 {
        match self.map.entry(v.into()) {
            std::collections::hash_map::Entry::Occupied(e) => return *e.get(),
            std::collections::hash_map::Entry::Vacant(e) => {
                for x in &**e.key() {
                    self.ls.push(*x);
                }
                let len = self.ls.len();
                e.insert(len as u32);
                len as u32
            }
        }
    }
    fn get(&self,i:u32) -> &[C] {
        if i == 0 { return &[] }
        let i = i as usize;
        &self.ls[i-1..i]
    }
}

 */