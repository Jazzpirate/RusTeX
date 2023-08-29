/*!
Plain TeX parses source files as a sequence of [`u8`]s, but other engines might use bigger types, e.g. XeTeX.

The [`CharType`] trait allows us to abstract over the character type, and the trait [`TeXStr`]`<Char:`[`CharType`]`>`
abstracts over the string type.
*/

use std::fmt::{Display, Debug};
use std::hash::Hash;
use std::marker::PhantomData;
use std::vec::IntoIter;
use array_init::array_init;
use crate::engine::EngineType;
use crate::engine::memory::{Interner, Memory};
use crate::tex::catcodes::{CategoryCodeScheme, OTHER_SCHEME_U8, STARTING_SCHEME_U8};



/**
Plain TeX parses source files as a sequence of [`u8`]s, but other engines might use bigger types, e.g. XeTeX.
This trait allows us to abstract over the character type, by providing the relevant data needed to treat them
(essentially) like [`u8`]s.
 */
pub trait CharType:Copy+PartialEq+Eq+Hash+Display+Debug+'static+From<u8>+Default + Ord+Send + Sync {
    /// The type of the array/vec/whatever of all possible characters. For [`u8`], this is `[A;256]`.
    type Allchars<A:Default+Clone> : AllCharsTrait<Self,A>;

    /// The maximum value of the character type. For [`u8`], this is `255`.
    const MAX:Self;

    /// Parses a character from a byte iterator. For [`u8`], this is just `iter.next()`.
    fn from_u8_iter<A>(iter:&mut A,counter:&mut usize) -> Option<Self> where A:Iterator<Item=u8> ;

    fn from_str(s:&'static str,interner:&mut Interner) -> TeXStr {
        TeXStr::from_static(s,interner)
    }

    fn tokenize(s:&str) -> Vec<Self>;

    /// The starting category code scheme for this character type, see [`struct@STARTING_SCHEME_U8`].
    fn starting_catcode_scheme() -> CategoryCodeScheme<Self>;

    /// The category code scheme for this character type with all characters having
    /// code 12 (Other) except for ` `(32) having code 10 (Space), see [`struct@OTHER_SCHEME_U8`].
    fn other_catcode_scheme() -> CategoryCodeScheme<Self>;

    fn newline() -> Self;
    fn carriage_return() -> Self;
    fn backslash() -> Self;
    fn zeros() -> Self::Allchars<Self>;
    fn ident() -> Self::Allchars<Self>;
    fn rep_field<A:Clone+Default>(a:A) -> Self::Allchars<A>;

    fn char_str(&self) -> String;
    fn as_bytes(&self) -> &[u8];
    fn as_char(&self) -> char;

    fn from_i64(i:i64) -> Option<Self>;
    fn to_usize(self) -> usize;
}


impl CharType for u8 {
    type Allchars<A:Default+Clone> = [A;256];
    const MAX:Self=255;
    fn from_u8_iter<A>(iter: &mut A,counter:&mut usize) -> Option<Self> where A:Iterator<Item=u8> { *counter +=1; iter.next() }
    fn newline() -> Self { b'\n' }
    fn carriage_return() -> Self {b'\r'}
    fn backslash() -> Self { b'\\' }
    fn tokenize(s:&str) -> Vec<Self> {
        s.chars().map(|c| c as u8).collect()
    }
    fn as_char(&self) -> char { *self as char }
    fn starting_catcode_scheme() -> CategoryCodeScheme<Self> {
        STARTING_SCHEME_U8.clone()
    }
    fn other_catcode_scheme() -> CategoryCodeScheme<Self> {
        OTHER_SCHEME_U8.clone()
    }
    fn as_bytes(&self) -> &[u8] { std::slice::from_ref(self) }
    fn char_str(&self) -> String {
        match *self {
            0 => "\\u0000".to_string(),
            b'\n' => "\\n".to_string(),
            b'\r' => "\\r".to_string(),
            o if is_ascii(o) => (o as char).to_string(), //f.write_char((o).into()),
            o => format!("\\u00{:X}",o)
        }
    }
    fn zeros() -> Self::Allchars<Self> {
        [0;256]
    }
    fn ident() -> Self::Allchars<Self> {
        let mut a = [0;256];
        for i in 0..256 { a[i] = i as u8; }
        a
    }
    fn rep_field<A: Clone+Default>(a: A) -> Self::Allchars<A> { array_init(|_| a.clone()) }

    fn from_i64(i: i64) -> Option<Self> {
        if i == -1 {Some(255)} else if i < 0 || i > 255 { None } else { Some(i as u8) }
    }
    fn to_usize(self) -> usize { self as usize }
}

/** A trait for arrays of all possible characters. For [`u8`], this is `[A;256]`.
*/
pub trait AllCharsTrait<C:CharType,A>:Clone {
    /// Returns the value for character `u`.
    fn get(&self, u: &C) -> &A;
    /// Sets the value for character `u` to `v`.
    fn set(&mut self, u: &C, v:A);
    /// Replaces the value for character `u` with `v`, returning the old value.
    fn replace(&mut self, u: &C, v:A) -> A;
}
impl<A:Clone> AllCharsTrait<u8,A> for [A;256] {
    //#[inline(always)]
    fn get(&self, u:&u8) -> &A { &self[*u as usize] }
   // #[inline(always)]
    fn set(&mut self, u:&u8,v:A) { self[*u as usize] = v }
    // #[inline(always)]
    fn replace(&mut self, u: &u8, v: A) -> A {
        std::mem::replace(&mut self[*u as usize], v)
    }
}

/** A "string" in TeX is a sequence of characters of some [`CharType`]. [`TeXStr`]
* abstracts away the character type, e.g. for control sequence names.
*/
#[derive(Clone,Copy,PartialEq,Hash,Eq,PartialOrd,Ord,Debug)]
pub struct TeXStr(pub string_interner::symbol::SymbolU16);
impl TeXStr {
    //pub fn new(v:Vec<C>) -> Self { Self(Ptr::new(v))}
    //pub fn len(&self) -> usize { self.0.len() }
    //pub fn as_vec(&self) -> &Vec<C> { &self.0 }
    pub fn symbol(&self) -> string_interner::symbol::SymbolU16 { self.0 }
}
impl TeXStr {
    pub fn to_str<'a>(&'a self,interner:&'a Interner) -> &'a str {
        interner.resolve(self.0)
    }
    pub fn from_static(s:&'static str,interner:&mut Interner) -> Self {
        interner.from_static(s)
    }
    pub fn from_string(s:&String, interner:&mut Interner) -> Self {
        interner.from_string(s)
    }
    pub fn from_primitive(s:string_interner::symbol::SymbolU16) -> Self {
        Self(s)
    }
}

//#[inline(always)]
fn is_ascii(u:u8) -> bool { 32 <= u && u <= 126 }
/*
impl<C:CharType> Display for TeXStr<C> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        C::display_str(self, f)
    }
}
impl From<&str> for TeXStr<u8> {
    fn from(s: &str) -> Self {
        TeXStr::new(s.as_bytes().to_vec())
    }
}
impl From<String> for TeXStr<u8> {
    fn from(s: String) -> Self {
        TeXStr::new(s.into_bytes())
    }
}
impl<C:CharType> From<Vec<C>> for TeXStr<C> {
    fn from(v: Vec<C>) -> Self {
        TeXStr::new(v)
    }
}
*/


impl CharType for char {
    const MAX: Self = 255 as char;
    type Allchars<A: Default+Clone> = AllUnicodeChars<A>; // TODO
    fn from_i64(i: i64) -> Option<Self> { if i > 0 && i < 0x110000 { Some(char::from_u32(i as u32).unwrap()) } else { None } }
    fn to_usize(self) -> usize { self as usize }
    fn backslash() -> Self { '\\' }
    fn carriage_return() -> Self { '\r' }
    fn newline() -> Self { '\n' }
    fn char_str(&self) -> String { self.to_string() }
    fn tokenize(s: &str) -> Vec<Self> {
        todo!()//&mut s.chars()//s.chars().collect::<Vec<char>>().as_slice()
    }
    fn as_bytes(&self) -> &[u8] {
        todo!()//char::as_bytes(self)//self.as_bytes()
    }
    fn as_char(&self) -> char { *self }

    fn ident() -> Self::Allchars<Self> {
        todo!()
    }
    fn starting_catcode_scheme() -> CategoryCodeScheme<Self> {
        todo!()
    }
    fn other_catcode_scheme() -> CategoryCodeScheme<Self> { todo!() }
    fn from_u8_iter<A>(iter: &mut A,counter:&mut usize) -> Option<Self> where A:Iterator<Item=u8> {
        todo!()
    }
    fn rep_field<A: Clone + Default>(a: A) -> Self::Allchars<A> {
        todo!()
    }
    fn zeros() -> Self::Allchars<Self> {
        todo!()
    }
}

#[derive(Clone)]
pub struct AllUnicodeChars<A:Default>(PhantomData<A>);
impl<A:Default+Clone> AllCharsTrait<char,A> for AllUnicodeChars<A> {
    fn get(&self, u: &char) -> &A {
        todo!()
    }

    fn set(&mut self, u: &char, v: A) {
        todo!()
    }

    fn replace(&mut self, u: &char, v: A) -> A {
        todo!()
    }
}

/*
impl CharType for char {

}

#[derive(Copy,Clone,Debug,PartialEq,Eq,Hash)]
pub struct Unicode(char);

impl Default for Unicode {
    fn default() -> Self {
        Self(0 as char)
    }
}
impl Display for Unicode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f,"{}",self.0)
    }
}

impl Into<usize> for Unicode {
    fn into(self) -> usize {
        self.0 as usize
    }
}

impl From<u8> for Unicode {
    fn from(value: u8) -> Self {
        Self(char::from_u32(value as u32).unwrap())
    }
}

impl TryFrom<i64> for Unicode {
    type Error = ();

    fn try_from(val: i64) -> Result<Self, Self::Error> {
        if val > 0 && val < 0x10FFFF && (val <= 0xD800 || val > 0xDFFF) {
            Ok(Self(char::from_u32(val as u32).unwrap()))
        } else {
            Err(())
        }
    }
}

impl Into<i64> for Unicode {
    fn into(self) -> i64 {
        self.0 as i64
    }
}

impl CharType for Unicode {
    type Allchars<A:Default> = Vec<A>;
    const MAX:Unicode = Unicode(-1 as char);
    fn display_str(str: &TeXStr<Self>, f: &mut Formatter<'_>) -> std::fmt::Result {
        let str : String = str.0.iter().map(|c| c.0).collect();
        write!(f,"{}",str)
    }
    fn from_str(s: &str) -> TeXStr<Self> {
        TeXStr(Ptr::new(s.chars().map(|c| Unicode(c)).collect()))
    }
    fn backslash() -> Self { Unicode('\\') }
    fn carriage_return() -> Self { Unicode('\r') }
    fn char_str(&self) -> String { self.0.to_string() }
    fn is_eol(self) -> Option<bool> {
        match self.0 {
            '\r' => None,
            '\n' => Some(true),
            _ => Some(false)
        }
    }
    fn is_eol_pair(self, next: Self) -> bool {
        self.0 == '\r' && next.0 == '\n'
    }
    fn par_token() -> TeXStr<Self> {
        todo!()//TeXStr(Ptr::new(vec![Unicode('p'),Unicode('a'),Unicode('r')]))
    }
    fn relax_token() -> TeXStr<Self> {
        todo!()//TeXStr(Ptr::new(vec![Unicode('r'),Unicode('e'),Unicode('l'),Unicode('a'),Unicode('x')]))
    }
    fn empty_str() -> TeXStr<Self> {
        todo!()//TeXStr(Ptr::new(vec![]))
    }
    fn ident() -> Self::Allchars<Self> {
        todo!()
    }

}
impl<A:Default> AllCharsTrait<Unicode,A> for Vec<A> {
    fn get(&self, u: Unicode) -> &A { &self[u.0 as usize] }
    fn set(&mut self, u: Unicode, v:A) { self[u.0 as usize] = v }
    fn replace(&mut self, u: Unicode, v:A) -> A { std::mem::replace(&mut self[u.0 as usize],v) }
}
 */