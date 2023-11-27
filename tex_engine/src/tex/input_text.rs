/*! Data structures for reading input text. */
use std::fmt::{Debug, Display, Write};
use crate::tex::catcodes::{CategoryCode, CategoryCodeScheme};

/** A single character in a `.tex` file; in plain TeX, this is a `u8`,
    but in e.g. XeTeX, it is a UTF-8 character. */
pub trait Character: Sized + Eq + Copy + Display + Debug + From<u8> + TryInto<u8> + TryFrom<u64> + Into<u64> + Ord + std::hash::Hash + Default + 'static {
    /// Type that maps characters to other data.
    type CharMap<A:Clone + Default> : CharacterMap<Self,A>;
    /// minimal value of this type in numeric form (e.g. `0` for `u8`)
    const MIN: Self;
    /// maximal value of this type in numeric form (e.g. `255` for `u8`)
    const MAX: Self;
    /// Convert a line in a file/string (as a vector of bytes) into a [`Vec`] of [`Character`]s.
    fn convert(input:Vec<u8>) -> TextLine<Self>;
    fn convert_fn<I:Iterator<Item=u8>,F:FnMut(Self)>(i:I,f:F);
    /// Display this character to a [`Write`](std::fmt::Write) (e.g. a `&mut String`).
    fn display<W:std::fmt::Write>(&self, target:&mut W);
    #[inline(always)]
    fn displayable(&self) -> DisplayableCharacter<Self> { DisplayableCharacter(*self) }

    #[inline(always)]
    #[allow(unused_must_use)]
    fn display_opt<W:std::fmt::Write>(c:Option<Self>, target:&mut W) {
        if let Some(c) = c { c.display(target); }
    }

    #[inline(always)]
    fn displayable_opt(c:Option<Self>) -> DisplayableCharacterOpt<Self> { DisplayableCharacterOpt(c) }
    /// The starting [`CategoryCodeScheme`] for this character type.
    fn starting_catcode_scheme() -> CategoryCodeScheme<Self>;

    fn single_char(string:&str) -> Option<Self>;
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
    fn convert(input:Vec<u8>) -> TextLine<Self> { input.into() }
    #[inline(always)]
    fn convert_fn<I:Iterator<Item=u8>,F:FnMut(Self)>(i:I,mut f:F) {
        for c in i { f(c); }
    }
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

    #[inline(always)]
    fn single_char(string: &str) -> Option<Self> {
        if string.len()==1 {
            Some(string.as_bytes()[0])
        } else if string.len() == 3 && string.starts_with("^^") {
            let b = string.as_bytes()[2];
            if b <= 255-64 {
                Some(b + 64)
            } else { None }
        } else if string.len() == 4 && string.starts_with("^^"){
            if let Ok(b) = u8::from_str_radix(&string[2..],16) {
                Some(b)
            } else { None }
        } else { None }
    }

    fn starting_catcode_scheme() -> [CategoryCode;256] {
        super::catcodes::STARTING_SCHEME_U8.clone()
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
pub struct StringLineSource {
    lines:Vec<Vec<u8>>
}
impl<C:Character> TextLineSource<C> for StringLineSource {
    #[inline(always)]
    fn get_line(&mut self) -> Option<TextLine<C>> {
        self.lines.pop().map(|v| C::convert(v))
    }
}
impl From<&str> for StringLineSource {
    fn from(value: &str) -> Self { value.to_string().into() }
}
impl From<String> for StringLineSource {
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
                lines.push(std::mem::take(&mut curr));
            } else {
                curr.push(b);
            }
        }
        if !curr.is_empty() {
            lines.push(curr);
        }
        lines.reverse();
        Self { lines }
    }
}