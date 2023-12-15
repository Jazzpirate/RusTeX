/*! Data structures for reading input text. */
use std::fmt::{Debug, Display, Write};
use crate::tex::catcodes::{CategoryCode, CategoryCodeScheme};
use crate::tex::control_sequences::ResolvedCSName;

/** A single character in a `.tex` file; in plain TeX, this is a `u8`,
    but in e.g. XeTeX, it is a UTF-8 character. */
pub trait Character: Sized + Eq + Copy + Display + Debug + From<u8> + TryInto<u8> + TryFrom<u64> + Into<u64> + Ord + std::hash::Hash + Default + 'static {
    /// Type that maps characters to other data.
    type CharMap<A:Clone + Default> : CharacterMap<Self,A>;
    type Iter<'a>:ExactSizeIterator<Item=Self>;
    /// minimal value of this type in numeric form (e.g. `0` for `u8`)
    const MIN: Self;
    /// maximal value of this type in numeric form (e.g. `255` for `u8`)
    const MAX: Self;
    /// Convert a line in a file/string (as a vector of bytes) into a [`Vec`] of [`Character`]s.
    fn convert(input:Vec<u8>) -> TextLine<Self>;
    /// Display this character to a [`Write`](std::fmt::Write) (e.g. a `&mut String`).
    fn display<W:std::fmt::Write>(&self, target:&mut W);
    #[inline(always)]
    fn displayable(&self) -> DisplayableCharacter<Self> { DisplayableCharacter(*self) }

    fn to_char(&self) -> char;

    #[inline(always)]
    fn displayable_opt(c:Option<Self>) -> DisplayableCharacterOpt<Self> { DisplayableCharacterOpt(c) }
    /// The starting [`CategoryCodeScheme`] for this character type.
    fn starting_catcode_scheme() -> CategoryCodeScheme<Self>;

    //fn single_char(string:&str) -> Option<Self>;
    fn string_to_iter<'a>(string:&'a str) -> Self::Iter<'a>;
}

pub struct DisplayableCharacter<C:Character>(C);
impl<C:Character> Display for DisplayableCharacter<C> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.display(f);
        Ok(())
    }
}

pub struct DisplayableCharacterOpt<C:Character>(Option<C>);
impl<C:Character> Display for DisplayableCharacterOpt<C> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(c) = self.0{ c.display(f) }
        Ok(())
    }
}

/** A map from characters `C:`[Character] to some other type `A`. For `u8`, we can simply use `[A;256]` */
pub trait CharacterMap<C:Character,A:Default>:Clone {
    fn get(&self,c:C) -> &A;
    fn get_mut(&mut self,c:C) -> &mut A;
    fn default() -> Self;
}

impl Character for u8 {
    type CharMap<A:Clone + Default> = [A;256];
    const MIN: Self = 0;
    const MAX: Self = 255;

    #[inline(always)]
    fn to_char(&self) -> char {*self as char}
    
    type Iter<'a> = ByteIterator<'a>;

    fn string_to_iter<'a>(string: &'a str) -> Self::Iter<'a> {
        ByteIterator(string)
    }
    #[inline(always)]
    fn convert(input:Vec<u8>) -> TextLine<Self> { input.into() }

    #[inline(always)]
    #[allow(unused_must_use)]
    fn display<W:std::fmt::Write>(&self, target:&mut W) {
        if self.is_ascii() {
            target.write_char(*self as char);
        }  else if *self > 128 && (*self - 64).is_ascii() {
            target.write_str("^^");
            target.write_char((*self-64) as char);
        }
        else {
            target.write_str(format!("^^{:x}",*self).as_str());
        }
    }

    fn starting_catcode_scheme() -> [CategoryCode;256] {
        super::catcodes::STARTING_SCHEME_U8.clone()
    }
}
pub struct ByteIterator<'a>(&'a str);
impl<'a> Iterator for ByteIterator<'a> {
    type Item = u8;
    fn next(&mut self) -> Option<Self::Item> {
        if self.0.is_empty() { None } else
        if self.0.starts_with("^^") {
            let b = self.0.as_bytes()[2];
            if b <= 60 || self.0.len() == 3 {
                self.0 = &self.0[3..];
                Some(b + 64)
            } else {
                let r = u8::from_str_radix(&self.0[2..4],16).unwrap();
                self.0 = &self.0[4..];
                Some(r)
            }
        } else {
            let b = self.0.as_bytes()[0];
            self.0 = &self.0[1..];
            Some(b)
        }
    }
}

impl <'a> ExactSizeIterator for ByteIterator<'_> {
    #[inline(always)]
    fn len(&self) -> usize {
        let mut num = 0usize;
        let mut iter = self.0.as_bytes().iter();
        while let Some(b) = iter.next() {
            if *b == b'^' {
                if let Some(b'^') = iter.next() {
                    if let Some(b) = iter.next() {
                        if *b <= 60 {
                            num += 1;
                        } else {
                            iter.next();
                            num += 1;
                        }
                    } else {
                        num += 1;
                    }
                } else {
                    num += 1;
                }
            } else {
                num += 1;
            }
        }
        num
    }
}

impl <A:Clone+Default> CharacterMap<u8,A> for [A;256] {
    #[inline(always)]
    fn get(&self,c: u8) -> &A { &self[c as usize] }
    #[inline(always)]
    fn get_mut(&mut self,c: u8) -> &mut A { &mut self[c as usize] }
    fn default() -> Self {
        let mut v = array_init::array_init(|_| A::default());
        v
    }
}

/// A single line of characters.
pub type TextLine<C> = Box<[C]>;

pub struct DisplayTextLine<'a,C:Character>{
    line:&'a TextLine<C>,
    from:usize
}
impl<'a,C:Character> Display for DisplayTextLine<'a,C> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for c in &self.line[self.from..] {
            c.display(f);
        }
        Ok(())
    }
}
pub trait DisplayTextLineExt<C:Character> {
    fn displayable(&self,start:usize) -> DisplayTextLine<C>;
}
impl<C:Character> DisplayTextLineExt<C> for TextLine<C> {
    fn displayable(&self,start:usize) -> DisplayTextLine<C> {
        DisplayTextLine { line:self, from:start }
    }
}

/// A source of lines of characters, e.g. a file or a string.
pub trait TextLineSource<C:Character> {
    /// returns the next line of characters, or `None` if there are no more lines.
    fn get_line(&mut self) -> Option<TextLine<C>>;
}
/// A source of lines of characters generated from a string.
pub struct StringLineSource<C:Character> {
    pub lines:Vec<TextLine<C>>
}
impl<C:Character> From<Vec<Box<[C]>>> for StringLineSource<C> {
    fn from(lines: Vec<Box<[C]>>) -> Self { Self { lines } }
}
impl<C:Character> TextLineSource<C> for StringLineSource<C> {
    #[inline(always)]
    fn get_line(&mut self) -> Option<TextLine<C>> {
        self.lines.pop()
    }
}
impl<C:Character> From<&str> for StringLineSource<C> {
    fn from(value: &str) -> Self { value.to_string().into() }
}
impl<C:Character> From<String> for StringLineSource<C> {
    fn from(s: String) -> Self {
        let mut lines = Vec::new();
        let all = s.into_bytes().into_iter();
        let mut curr = Vec::new();
        for b in all {
            if b == b'\n' {
                if let Some(b'\r') = curr.last() {
                    curr.pop();
                }
                while let Some(b' ') = curr.last() {
                    curr.pop();
                }
                lines.push(C::convert(std::mem::take(&mut curr)));
            } else {
                curr.push(b);
            }
        }
        if !curr.is_empty() {
            lines.push(C::convert(curr));
        }
        lines.reverse();
        Self { lines }
    }
}