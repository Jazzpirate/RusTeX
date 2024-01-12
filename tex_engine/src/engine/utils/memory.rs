/*! Memory manangement and string interning. */

use std::fmt::Display;
use std::marker::PhantomData;
use std::sync::RwLock;
use lazy_static::lazy_static;
use crate::tex::tokens::token_lists::TokenList;
use crate::tex::tokens::control_sequences::{CSName, CSHandler, ResolvedCSName, InternedCSName};
use crate::tex::characters::Character;
use crate::tex::tokens::Token;
use crate::utils::HMap;

/// Utility struct for managing memory allocation and string interning. In particular, it
/// provides a [`CSHandler`] implementation for interning control sequence names (if applicable).
#[derive(Clone)]
pub struct MemoryManager<T:Token> {
    cs_interner: <T::CS as CSName<T::Char>>::Handler,
    strings: Vec<String>,
    bytes: Vec<Vec<u8>>,
    token_vecs: Vec<Vec<T>>,
    empty: TokenList<T>
}
impl<T:Token> MemoryManager<T> {
    /// Creates a new memory manager.
    pub fn new() -> Self {
        MemoryManager {
            cs_interner: <T::CS as CSName<T::Char>>::Handler::default(),
            strings: Vec::new(),
            bytes: Vec::new(),
            token_vecs: Vec::new(),
            empty: TokenList(shared_vector::SharedVector::new())
        }
    }
    /// Provides an empty `Vec<u8>` that can be returned using [`return_bytes`](Self::return_bytes), avoiding
    /// the need to allocate a new `Vec<u8>` every time.
    pub fn get_bytes(&mut self) -> Vec<u8> {
        self.bytes.pop().unwrap_or_default()
    }
    /// Returns a `Vec<u8>` to the memory manager, which clears it and keeps it in memory for reuse.
    pub fn return_bytes(&mut self,mut b:Vec<u8>) {
        b.clear();
        self.bytes.push(b)
    }
    /// Provides an empty `String` that can be returned using [`return_string`](Self::return_string), avoiding
    /// the need to allocate a new `String` every time.
    pub fn get_string(&mut self) -> String {
        self.strings.pop().unwrap_or_default()
    }
    /// Returns a `String` to the memory manager, which clears it and keeps it in memory for reuse.
    pub fn return_string(&mut self,mut s:String) {
        s.clear();
        self.strings.push(s)
    }
    /// Provides an empty `Vec<T>` that can be returned using [`return_token_vec`](Self::return_token_vec), avoiding
    /// the need to allocate a new `Vec<T>` every time.
    pub fn get_token_vec(&mut self) -> Vec<T> {
        self.token_vecs.pop().unwrap_or_default()
    }
    /// Returns a `Vec<T>` to the memory manager, which clears it and keeps it in memory for reuse.
    pub fn return_token_vec(&mut self,mut v:Vec<T>) {
        v.clear();
        self.token_vecs.push(v)
    }
    /// Returns a new memory manager with the given control sequence interner.
    pub fn new_with_cs_interner(cs_interner:<T::CS as CSName<T::Char>>::Handler) -> Self {
        MemoryManager {
            cs_interner,
            strings: Vec::new(),
            bytes: Vec::new(),
            token_vecs: Vec::new(),
            empty: TokenList(shared_vector::SharedVector::new())
        }
    }
    /// Sets the control sequence interner.
    pub fn set_cs_interner(&mut self,cs_interner:<T::CS as CSName<T::Char>>::Handler) {
        self.cs_interner = cs_interner;
    }

    /// Returns the control sequence interner mutably.
    pub fn cs_interner_mut(&mut self) -> &mut <T::CS as CSName<T::Char>>::Handler { &mut self.cs_interner }

    /// Returns the control sequence interner.
    pub fn cs_interner(&self) -> &<T::CS as CSName<T::Char>>::Handler { &self.cs_interner }

    /// Returns an empty token list.
    pub fn empty_list(&self) -> TokenList<T> { self.empty.clone() }
}

/// We always intern the names for primitive commands/macros, for efficiency; in particular for equality checks.
/// Uses `u16` internally, i.e. allowing for up to 65536 primitives.
///
/// It is never necessary to instantiate a new [`PrimitiveInterner`]; instead, use the global [`PRIMITIVES`](crate::tex::primitives::PRIMITIVES) instance.
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
    pub r#else:PrimitiveIdentifier,
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
    pub everyjob:PrimitiveIdentifier,
    pub count:PrimitiveIdentifier,
    pub noexpand:PrimitiveIdentifier,
    pub unexpanded:PrimitiveIdentifier,
    pub endcsname:PrimitiveIdentifier,
    pub the:PrimitiveIdentifier,
    pub toks:PrimitiveIdentifier,
    pub vsize:PrimitiveIdentifier,
    pub output:PrimitiveIdentifier,
    pub badness:PrimitiveIdentifier,
    pub outputpenalty:PrimitiveIdentifier,
    pub dimen:PrimitiveIdentifier,
    pub skip:PrimitiveIdentifier,
    pub everypar:PrimitiveIdentifier,
    pub indent:PrimitiveIdentifier,
    pub noindent:PrimitiveIdentifier,
    pub hangindent:PrimitiveIdentifier,
    pub hangafter:PrimitiveIdentifier,
    pub leftskip:PrimitiveIdentifier,
    pub rightskip:PrimitiveIdentifier,
    pub hsize:PrimitiveIdentifier,
    pub pdfpagewidth:PrimitiveIdentifier,
    pub everymath:PrimitiveIdentifier,
    pub everydisplay:PrimitiveIdentifier,
    pub char:PrimitiveIdentifier,
    pub tabskip:PrimitiveIdentifier,
    pub cr:PrimitiveIdentifier,
    pub crcr:PrimitiveIdentifier,
    pub everycr:PrimitiveIdentifier,
    pub span:PrimitiveIdentifier,
    pub noalign:PrimitiveIdentifier,
    pub omit:PrimitiveIdentifier,
    pub baselineskip:PrimitiveIdentifier,
    pub lineskip:PrimitiveIdentifier,
    pub lineskiplimit:PrimitiveIdentifier,
    pub parindent:PrimitiveIdentifier,
    pub hrule:PrimitiveIdentifier,
    pub vrule:PrimitiveIdentifier,
    pub vskip:PrimitiveIdentifier,
    pub hskip:PrimitiveIdentifier,
    pub vfil:PrimitiveIdentifier,
    pub hfil:PrimitiveIdentifier,
    pub vfill:PrimitiveIdentifier,
    pub hfill:PrimitiveIdentifier,
    pub parskip:PrimitiveIdentifier,
    pub delimiter:PrimitiveIdentifier,
    pub abovedisplayskip:PrimitiveIdentifier,
    pub belowdisplayskip:PrimitiveIdentifier,
    pub iffalse:PrimitiveIdentifier,
    pub iftrue:PrimitiveIdentifier,
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
        let r#else = PrimitiveIdentifier(interner.get_or_intern_static("else"));
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
        let everyjob = PrimitiveIdentifier(interner.get_or_intern_static("everyjob"));
        let count = PrimitiveIdentifier(interner.get_or_intern_static("count"));
        let noexpand = PrimitiveIdentifier(interner.get_or_intern_static("noexpand"));
        let endcsname = PrimitiveIdentifier(interner.get_or_intern_static("endcsname"));
        let unexpanded = PrimitiveIdentifier(interner.get_or_intern_static("unexpanded"));
        let the = PrimitiveIdentifier(interner.get_or_intern_static("the"));
        let toks = PrimitiveIdentifier(interner.get_or_intern_static("toks"));
        let vsize = PrimitiveIdentifier(interner.get_or_intern_static("vsize"));
        let output = PrimitiveIdentifier(interner.get_or_intern_static("output"));
        let badness = PrimitiveIdentifier(interner.get_or_intern_static("badness"));
        let outputpenalty = PrimitiveIdentifier(interner.get_or_intern_static("outputpenalty"));
        let dimen = PrimitiveIdentifier(interner.get_or_intern_static("dimen"));
        let skip = PrimitiveIdentifier(interner.get_or_intern_static("skip"));
        let everypar = PrimitiveIdentifier(interner.get_or_intern_static("everypar"));
        let indent = PrimitiveIdentifier(interner.get_or_intern_static("indent"));
        let noindent = PrimitiveIdentifier(interner.get_or_intern_static("noindent"));
        let hangindent = PrimitiveIdentifier(interner.get_or_intern_static("hangindent"));
        let hangafter = PrimitiveIdentifier(interner.get_or_intern_static("hangafter"));
        let leftskip = PrimitiveIdentifier(interner.get_or_intern_static("leftskip"));
        let rightskip = PrimitiveIdentifier(interner.get_or_intern_static("rightskip"));
        let hsize = PrimitiveIdentifier(interner.get_or_intern_static("hsize"));
        let pdfpagewidth = PrimitiveIdentifier(interner.get_or_intern_static("pdfpagewidth"));
        let everymath = PrimitiveIdentifier(interner.get_or_intern_static("everymath"));
        let everydisplay = PrimitiveIdentifier(interner.get_or_intern_static("everydisplay"));
        let char = PrimitiveIdentifier(interner.get_or_intern_static("char"));
        let tabskip = PrimitiveIdentifier(interner.get_or_intern_static("tabskip"));
        let cr = PrimitiveIdentifier(interner.get_or_intern_static("cr"));
        let crcr = PrimitiveIdentifier(interner.get_or_intern_static("crcr"));
        let everycr = PrimitiveIdentifier(interner.get_or_intern_static("everycr"));
        let span = PrimitiveIdentifier(interner.get_or_intern_static("span"));
        let noalign = PrimitiveIdentifier(interner.get_or_intern_static("noalign"));
        let omit = PrimitiveIdentifier(interner.get_or_intern_static("omit"));
        let baselineskip = PrimitiveIdentifier(interner.get_or_intern_static("baselineskip"));
        let lineskip = PrimitiveIdentifier(interner.get_or_intern_static("lineskip"));
        let lineskiplimit = PrimitiveIdentifier(interner.get_or_intern_static("lineskiplimit"));
        let parindent = PrimitiveIdentifier(interner.get_or_intern_static("parindent"));
        let hrule = PrimitiveIdentifier(interner.get_or_intern_static("hrule"));
        let vrule = PrimitiveIdentifier(interner.get_or_intern_static("vrule"));
        let vskip = PrimitiveIdentifier(interner.get_or_intern_static("vskip"));
        let hskip = PrimitiveIdentifier(interner.get_or_intern_static("hskip"));
        let vfil = PrimitiveIdentifier(interner.get_or_intern_static("vfil"));
        let hfil = PrimitiveIdentifier(interner.get_or_intern_static("hfil"));
        let vfill = PrimitiveIdentifier(interner.get_or_intern_static("vfill"));
        let hfill = PrimitiveIdentifier(interner.get_or_intern_static("hfill"));
        let parskip = PrimitiveIdentifier(interner.get_or_intern_static("parskip"));
        let delimiter = PrimitiveIdentifier(interner.get_or_intern_static("delimiter"));
        let abovedisplayskip = PrimitiveIdentifier(interner.get_or_intern_static("abovedisplayskip"));
        let belowdisplayskip = PrimitiveIdentifier(interner.get_or_intern_static("belowdisplayskip"));
        let iffalse = PrimitiveIdentifier(interner.get_or_intern_static("iffalse"));
        let iftrue = PrimitiveIdentifier(interner.get_or_intern_static("iftrue"));
        PrimitiveInterner{
            interner:RwLock::new(interner),
            globaldefs, relax, mag, fam, ifcase, tracingifs, tracingassigns, tracingcommands,
            tracinggroups, r#else, fi, or, global, long, outer, protected, def, edef, xdef,
            gdef,everyeof,count,tracingrestores,noexpand,endcsname,unexpanded,the,toks,
            everyhbox,everyvbox,everyjob,vsize,output,badness,outputpenalty,dimen,skip,
            everypar,indent,noindent,hangindent,hangafter,leftskip,rightskip,hsize,
            pdfpagewidth,everymath,everydisplay,char,tabskip,cr,crcr,everycr,span,
            noalign,omit,baselineskip,lineskip,lineskiplimit,parindent,hrule,vrule,
            vskip,hskip,vfil,hfil,vfill,hfill,parskip,delimiter,abovedisplayskip,
            belowdisplayskip,iffalse,iftrue
        }
    }

    /// Returns the identifier for the given primitive command name, interning it if necessary.
    /// This is thread-safe, but slow, so should only be used ideally once per primitive at startup.
    pub fn get(&self,s:&'static str) -> PrimitiveIdentifier {
        let mut lock = self.interner.write().unwrap();
        PrimitiveIdentifier(lock.get_or_intern_static(s))
    }

    /// Returns a struct implementing [`Display`](std::fmt::Display) for the given [`PrimitiveIdentifier`], and
    /// optional `\escapechar` that will be prefixed - e.g.
    /// `println!(`[`PRIMITIVES`]`.`[`printable`](Self::printable)`(`[`PRIMITIVES`]`.the, Some('\\')))`
    /// will print `\the`.
    pub fn printable<C:Character>(&'static self,ident:PrimitiveIdentifier,escapechar:Option<C>) -> impl std::fmt::Display {
        PrintableIdentifier(ident,escapechar,&self)
    }

}
struct PrintableIdentifier<C:Character>(PrimitiveIdentifier,Option<C>,&'static PrimitiveInterner);
impl<C:Character> std::fmt::Display for PrintableIdentifier<C> {
    fn fmt(&self,f:&mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let lock = self.2.interner.read().unwrap();
        match self.1 {
            None => (),
            Some(c) => c.display_fmt(f)
        }
        write!(f,"{}",lock.resolve(self.0.0).unwrap())
    }
}
lazy_static!(
    /// The global [`PrimitiveInterner`].
    pub static ref PRIMITIVES:PrimitiveInterner = PrimitiveInterner::new();
);

/// A `Copy` identifier for a primitive command. Small and fast to compare.
#[derive(Copy,Clone,PartialEq,Eq,Hash,Debug)]
pub struct PrimitiveIdentifier(string_interner::symbol::SymbolU16);

#[derive(Clone)]
pub struct CharacterVecInterner<C:Character> {
    map:HMap<Box<[C]>,u32>,
    ls:Vec<C>,
    idx:Vec<usize>
}
impl<C:Character> CharacterVecInterner<C> {
    #[allow(dead_code)]
    fn cap(&self) -> usize { self.idx.len() }
    fn new() -> Self {
        let mut map: HMap<Box<[C]>,u32> = HMap::default();
        map.insert(Box::new([]),0);
        let mut r = CharacterVecInterner {
            map, ls:Vec::new(),idx:vec!(0)
        };
        r.from_static("par");
        r
    }

    pub fn from_static(&mut self,s:&'static str) -> InternedCSName<C> {
        self.intern(C::string_to_iter(s).collect::<Vec<_>>().as_slice().into())
    }

    pub fn from_string<S:AsRef<str>>(&mut self,s:S) -> InternedCSName<C> {
        self.intern(C::string_to_iter(s.as_ref()).collect::<Vec<_>>().as_slice().into())
    }

    pub fn resolve(&self,i:InternedCSName<C>) -> &[C] {
        self.get(i.0)
    }
    fn intern(&mut self,v:&[C]) -> InternedCSName<C> {
        match self.map.get(v) {
            Some(x) => return (*x,PhantomData::default()),
            None => ()
        }
        self.ls.extend(v);
        let len = self.ls.len();
        self.idx.push(len);
        let len = self.idx.len() - 1;
        self.map.insert(v.into(),len as u32);
        (len as u32,PhantomData::default())
    }
    fn get(&self,i:u32) -> &[C] {
        if i == 0 { return &[] }
        let i = i as usize;
        let s = self.idx[i - 1];
        let e = self.idx[i];
        &self.ls[s..e]
    }
}
impl<C:Character> CSHandler<C,InternedCSName<C>> for CharacterVecInterner<C> {
    type Resolved<'a> = DisplayCSName<'a,C>;

    fn new(&mut self,s: &str) -> InternedCSName<C> {
        self.intern(C::string_to_iter(s).collect::<Vec<_>>().as_slice())
    }

    fn from_chars(&mut self, v: &Vec<C>) -> InternedCSName<C> {
        self.intern(v.as_slice())
    }

    fn par(&self) -> InternedCSName<C> { (1,PhantomData::default()) }

    fn empty_str(&self) -> InternedCSName<C> { (0,PhantomData::default()) }

    fn resolve<'a>(&'a self, cs: &InternedCSName<C>) -> DisplayCSName<'a,C> {
        DisplayCSName(self.get(cs.0))
    }
}
pub struct DisplayCSName<'a,C:Character>(&'a [C]);
impl<C:Character> Display for DisplayCSName<'_,C> {
    fn fmt(&self,f:&mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for c in self.0 {
            c.display_fmt(f)
        }
        Ok(())
    }
}

impl<'a,C:Character> ResolvedCSName<'a,C> for DisplayCSName<'a,C> {
    type Iter = std::iter::Copied<std::slice::Iter<'a,C>>;

    fn iter(&self) -> Self::Iter { self.0.iter().copied() }

    fn len(&self) -> usize { self.0.len() }
}
impl <C:Character> Default for CharacterVecInterner<C> {

    fn default() -> Self { Self::new() }
}
