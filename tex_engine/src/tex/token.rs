/*! A [Token] is -- conceptually -- either a [control sequence](ControlSequenceName),
or a pair of a [character](Character) and a [category code](super::catcodes::CategoryCode).
 */

use std::fmt::Write;
use std::marker::PhantomData;
use crate::engine::utils::memory::InternedCSName;
use crate::tex::catcodes::{CategoryCode, CategoryCodeScheme, CommandCode};
use crate::tex::input_text::Character;
use crate::tex::control_sequences::{ControlSequenceName, ControlSequenceNameHandler, ResolvedCSName};
use crate::tex::input_text::CharacterMap;

/// Trait for Tokens, to be implemented for an engine.
pub trait Token:Clone+Eq+'static+std::fmt::Debug+Sized {
    /// The type of the control sequence name.
    type CS : ControlSequenceName<Self::Char>;
    /// The type of the character.
    type Char : Character;

    //const TOKEN_LIST_FACTORY: Option<RefCell<ReusableVectorFactory<Self>>> = None;

    /// Converts to the canonical enum representation of a token, i.e. [`StandardToken`].
    fn to_enum(&self) -> StandardToken<Self::Char,Self::CS>;
    /// Create a new token from a control sequence name.
    fn from_cs(cs:Self::CS) -> Self;
    /// Create a new space token.
    fn space() -> Self;
    /// Create a new noexpand marker token.
    fn noexpand_marker() -> Self;
    /// Create a new argument marker token. `i` needs to be in the range `0..=8`.
    fn argument_marker(i:u8) -> Self;
    fn eof() -> Self;
    /// Create a new character token with given [`CommandCode`] (i.e.
    /// conceptually the [`CategoryCode`](super::catcodes::CategoryCode)).
    fn from_char_cat(c:Self::Char,cat:CommandCode) -> Self;
    /// The [`Character`] value of this token, if it is a character token.
    #[inline(always)]
    fn char_value(&self) -> Option<Self::Char> {
        match self.to_enum() {
            StandardToken::Character(c, _) => Some(c),
            _ => None
        }
    }
    /// The [`CommandCode`] (i.e. conceptually the [`CategoryCode`](super::catcodes::CategoryCode)) of this token.
    #[inline(always)]
    fn command_code(&self) -> CommandCode {
        match self.to_enum() {
            StandardToken::ControlSequence(_) => CommandCode::Escape,
            StandardToken::Character(_, cat) => cat
        }
    }

    #[inline(always)]
    fn is_cs_or_active(&self) -> bool {
        match self.to_enum() {
            StandardToken::ControlSequence(_) => true,
            StandardToken::Character(_, CommandCode::Active) => true,
            _ => false
        }
    }

    #[inline(always)]
    fn is_cs(&self,name:&Self::CS) -> bool {
        match self.to_enum() {
            StandardToken::ControlSequence(cs) => cs == *name,
            _ => false
        }
    }
    /// Check if this token is a argument token, and if so, return its number (in the range `0..=8`).
    #[inline(always)]
    fn is_argument_marker(&self) -> Option<u8> {
        match self.to_enum() {
            StandardToken::Character(c, CommandCode::Argument) => Some(c.try_into().ok().unwrap()),
            _ => None
        }
    }
    /// Check if this token is a `\noexpand` marker.
    #[inline(always)]
    fn is_noexpand_marker(&self) -> bool {
        match self.to_enum() {
            StandardToken::Character(_, CommandCode::Noexpand) => true,
            _ => false
        }
    }
    /// Check if this token has [`CommandCode::BeginGroup`].
    #[inline(always)]
    fn is_begin_group(&self) -> bool {
        match self.to_enum() {
            StandardToken::Character(_, CommandCode::BeginGroup) => true,
            _ => false
        }
    }
    /// Check if this token has [`CommandCode::Space`].
    #[inline(always)]
    fn is_space(&self) -> bool {
        match self.to_enum() {
            StandardToken::Character(_, CommandCode::Space) => true,
            _ => false
        }
    }
    /// Check if this token has [`CommandCode::AlignmentTab`].
    #[inline(always)]
    fn is_align_tab(&self) -> bool {
        match self.to_enum() {
            StandardToken::Character(_, CommandCode::AlignmentTab) => true,
            _ => false
        }
    }
    /// Check if this token has [`CommandCode::EndGroup`].
    #[inline(always)]
    fn is_end_group(&self) -> bool {
        match self.to_enum() {
            StandardToken::Character(_, CommandCode::EndGroup) => true,
            _ => false
        }
    }
    #[inline(always)]
    fn is_param(&self) -> bool {
        match self.to_enum() {
            StandardToken::Character(_, CommandCode::Parameter) => true,
            _ => false
        }
    }

    fn display_fmt<W:Write>(&self,int:&<Self::CS as ControlSequenceName<Self::Char>>::Handler, cc:&CategoryCodeScheme<Self::Char>, escapechar:Option<Self::Char>, mut f: W)  -> std::fmt::Result {
        match self.to_enum() {
            StandardToken::Character(_,CommandCode::Space) => write!(f," "),
            StandardToken::Character(c,_) => write!(f,"{}",c.displayable()),
            StandardToken::ControlSequence(cs) => {
                let res = int.resolve(&cs);
                write!(f, "{}{}", Self::Char::displayable_opt(escapechar), res)?;
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
    }
}

/** The simplest (but not most efficient) way to represent a [`Token`] as an enum.

 Is [`Copy`] iff [`CS`](Token::CS) is [`Copy`].
 */
#[derive(Clone,Copy,Eq,Debug)]
pub enum StandardToken<Char:Character,CS:ControlSequenceName<Char>> {
    ControlSequence(CS),
    Character(Char,CommandCode)
}
impl<Char:Character,CS:ControlSequenceName<Char>> PartialEq for StandardToken<Char,CS> {
    fn eq(&self,other:&Self) -> bool {
        match (self,other) {
            (StandardToken::ControlSequence(a), StandardToken::ControlSequence(b)) => a==b,
            (StandardToken::Character(_, CommandCode::Space), StandardToken::Character(_, CommandCode::Space)) => true,
            (StandardToken::Character(a1, a2), StandardToken::Character(b1, b2)) => a1==b1 && a2==b2,
            _ => false
        }
    }
}
impl<Char:Character,CS:ControlSequenceName<Char>> Token for StandardToken<Char,CS> {
    type CS = CS;
    type Char = Char;
    #[inline(always)]
    fn to_enum(&self) -> StandardToken<Char,CS> { self.clone() }
    #[inline(always)]
    fn from_cs(cs:CS) -> Self { StandardToken::ControlSequence(cs) }
    #[inline(always)]
    fn space() -> Self { StandardToken::Character(Char::from(32), CommandCode::Space) }
    #[inline(always)]
    fn eof() -> Self {
        StandardToken::Character(Char::from(0), CommandCode::EOF)
    }
    #[inline(always)]
    fn from_char_cat(c:Char,cat:CommandCode) -> Self { StandardToken::Character(c, cat) }
    #[inline(always)]
    fn noexpand_marker() -> Self {
        Self::Character(Char::from(32),CommandCode::Noexpand)
    }
    #[inline(always)]
    fn argument_marker(i: u8) -> Self {
        Self::Character(Char::from(i),CommandCode::Argument)
    }
}

/** A compact representation of a [`Token`] with [`Char`](Token::Char)`==u8` and [`CS`](Token::CS)`==`[`InternedString`]
 as a single `u32` (similar to the way plain TeX does it).

 Very memory efficient and correspondingly fast, but at the disadvantage of restricting the number of total control sequence names
 to 2³¹ (which is still a lot).
*/
#[derive(Clone,Copy,Eq, Debug)]
pub struct CompactToken(u32);
impl CompactToken {
    #[inline(always)]
    fn is_string(&self) -> bool { self.0 < 0x8000_0000 }
    #[inline(always)]
    fn as_string(&self) -> Option<InternedCSName<u8>> {
        if self.is_string() {
            Some((self.0,PhantomData::default()))
            //Some(InternedString::try_from_usize(self.0 as usize).unwrap())
        } else {
            None
        }
    }
    #[inline(always)]
    fn commandcode_value(&self) -> u8 { ((self.0 & 0x0000_FF00) >> 8) as u8 }
    #[inline(always)]
    fn catcode(&self) -> CommandCode {
        CommandCode::try_from(self.commandcode_value()).unwrap()
    }
    #[inline(always)]
    fn u8(&self) -> u8 { (self.0 & 0x0000_00FF) as u8 }
}
impl PartialEq for CompactToken {
    fn eq(&self,other:&Self) -> bool {
        self.0 == other.0 || {
            if self.is_string() || other.is_string() { return false}
            let cc1 = self.catcode();
            let cc2 = other.catcode();
            if cc1 == CommandCode::Space && cc2 == CommandCode::Space {return true}
            if cc1 != cc2 {return false}
            self.u8() == other.u8()
        }
    }
}
impl Token for CompactToken {
    type CS = InternedCSName<u8>;//InternedString;
    type Char = u8;
    //const TOKEN_LIST_FACTORY: Option<RefCell<ReusableVectorFactory<Self>>> = Some(RefCell::new(ReusableVectorFactory::constant()));
    fn to_enum(&self) -> StandardToken<u8,InternedCSName<u8>> {
        match self.as_string() {
            Some(s) => StandardToken::ControlSequence(s),
            None => StandardToken::Character(self.u8(), self.catcode())
        }
    }
    #[inline(always)]
    fn from_cs(cs: Self::CS) -> Self { Self(cs.0) }
    #[inline(always)]
    fn space() -> Self { Self(0x8000_0000 | (10 << 8) | 32) }
    #[inline(always)]
    fn eof() -> Self { Self(0x8000_0000 | (5 << 8) | 0) }
    #[inline(always)]
    fn from_char_cat(c:u8,cat:CommandCode) -> Self {
        let c = u32::from(c);
        let cat = u32::from(Into::<u8>::into(cat));
        let r = 0x8000_0000 | (cat << 8) |c;
        Self(r)
    }
    #[inline(always)]
    fn noexpand_marker() -> Self { Self(0x8000_0000 | (9 << 8) | 0) }
    #[inline(always)]
    fn argument_marker(i: u8) -> Self {
        Self(0x8000_0000 | (14 << 8) | u32::from(i))
    }
    #[inline(always)]
    fn command_code(&self) -> CommandCode {
        if self.is_string() { CommandCode::Escape } else { self.catcode() }
    }
    #[inline(always)]
    fn char_value(&self) -> Option<Self::Char> {
        if self.is_string() { None } else { Some(self.u8()) }
    }

    #[inline(always)]
    fn is_cs_or_active(&self) -> bool {
        self.is_string() || ((self.0 & 0x0000_FF00) >> 8) == 13
    }

    #[inline(always)]
    fn is_cs(&self,name:&Self::CS) -> bool {
        self.0 == name.0
    }
    #[inline(always)]
    fn is_space(&self) -> bool {
        !self.is_string() && ((self.0 & 0x0000_FF00) >> 8) == 10
    }
    #[inline(always)]
    fn is_noexpand_marker(&self) -> bool {
        !self.is_string() && ((self.0 & 0x0000_FF00) >> 8) == 9
    }
    #[inline(always)]
    fn is_argument_marker(&self) -> Option<u8> {
        if !self.is_string() && ((self.0 & 0x0000_FF00) >> 8) == 14 {Some(self.u8())} else { None }
    }
    #[inline(always)]
    fn is_begin_group(&self) -> bool {
        !self.is_string() && ((self.0 & 0x0000_FF00) >> 8) == 1
    }
    #[inline(always)]
    fn is_end_group(&self) -> bool {
        !self.is_string() && ((self.0 & 0x0000_FF00) >> 8) == 2
    }
    #[inline(always)]
    fn is_align_tab(&self) -> bool {
        !self.is_string() && ((self.0 & 0x0000_FF00) >> 8) == 4
    }

    #[inline(always)]
    fn is_param(&self) -> bool {
        !self.is_string() && ((self.0 & 0x0000_FF00) >> 8) == 6
    }
}