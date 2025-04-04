/*! A [Token] is -- conceptually -- either a [control sequence](CSName),
or a pair of a [character](Character) and a [`CategoryCode`](super::catcodes::CategoryCode). In practice, we use
[`CommandCode`] instead, which omits "impossible" codes (e.g. [`Invalid`](super::catcodes::CategoryCode::Invalid) or
[`Comment`](super::catcodes::CategoryCode::Comment)) and adds internal ones (e.g. [`Primitive`](CommandCode::Primitive) or
[`Argument`](CommandCode::Argument)).

The "canonical" way to represent a [`Token`] is [`StandardToken`], which is an enum with two variants.
However, since [`Token`]s are read, processed, inspected, passed around, stored and retrieved extremely often, and in
the most performance-critical parts of the engine, their representation matters. In particular, we want them to be small
and ideally `Copy`, which excludes representing control sequences as strings; hence the generic [`CSName`] type
and [`CompactToken`] as a significantly more efficient representation.
 */

use crate::commands::primitives::PrimitiveIdentifier;
use crate::tex::catcodes::{CategoryCodeScheme, CommandCode};
use crate::tex::characters::Character;
use crate::tex::tokens::control_sequences::{CSName, InternedCSName};
use std::fmt::Write;
use std::marker::PhantomData;
use std::num::NonZeroU32;

pub mod control_sequences;
pub mod token_lists;

/// Trait for Tokens, to be implemented for an engine (see [above](crate::tex::tokens)).
/// Note that two [`Space`](CommandCode::Space) tokens are always considered equal.
pub trait Token: Clone + Eq + 'static + std::fmt::Debug + Sized {
    /// The [`CSName`] type used for control sequence names (e.g. `Rc<str>` or something interned).
    type CS: CSName<Self::Char>;
    /// The [`Character`] type for char/catcode-pair tokens.
    type Char: Character;
    /// Converts to the canonical enum representation of a token, i.e. [`StandardToken`].
    fn to_enum(&self) -> StandardToken<Self::Char, Self::CS>;
    /// Create a new token from a control sequence name.
    fn from_cs(cs: Self::CS) -> Self;
    /// Create a new space token.
    fn space() -> Self;

    /// Create a new token representing a [primitive](PrimitiveIdentifier) [`Command`](crate::commands::TeXCommand).
    fn primitive(id: PrimitiveIdentifier) -> Self;
    /// Create a new argument marker token. `i` needs to be in the range `0..=8`.
    fn argument_marker(i: u8) -> Self;
    /// Create a new end-of-file token.
    fn eof() -> Self;
    /// Create a new character token with given [`CommandCode`] (i.e.
    /// conceptually the [`CategoryCode`](super::catcodes::CategoryCode)).
    fn from_char_cat(c: Self::Char, cat: CommandCode) -> Self;
    /// The [`Character`] value of this token, if it is a character token.

    fn char_value(&self) -> Option<Self::Char> {
        match self.to_enum() {
            StandardToken::Character(c, _) => Some(c),
            _ => None,
        }
    }
    /// The [`CommandCode`] (i.e. conceptually the [`CategoryCode`](super::catcodes::CategoryCode)) of this token.

    fn command_code(&self) -> CommandCode {
        match self.to_enum() {
            StandardToken::ControlSequence(_) => CommandCode::Escape,
            StandardToken::Character(_, cat) => cat,
            StandardToken::Primitive(_) => CommandCode::Primitive,
        }
    }

    /// Check if this token is a control sequence or an active character

    fn is_cs_or_active(&self) -> bool {
        matches!(
            self.to_enum(),
            StandardToken::ControlSequence(_)
                | StandardToken::Character(_, CommandCode::Active)
                | StandardToken::Primitive(_)
        )
    }

    /// Check if this token is a control sequence with the given name.

    fn is_cs(&self, name: &Self::CS) -> bool {
        match self.to_enum() {
            StandardToken::ControlSequence(cs) => cs == *name,
            _ => false,
        }
    }
    fn is_primitive(&self) -> Option<PrimitiveIdentifier> {
        match self.to_enum() {
            StandardToken::Primitive(id) => Some(id),
            _ => None,
        }
    }
    /// Check if this token is a argument token, and if so, return its number (in the range `0..=8`).
    fn is_argument_marker(&self) -> Option<u8> {
        match self.to_enum() {
            StandardToken::Character(c, CommandCode::Argument) => Some(c.try_into().ok().unwrap()),
            _ => None,
        }
    }

    /// Display this token to a writer, using the given [`CSHandler`](control_sequences::CSHandler) (in case it is a control sequence).
    /// In that case, we also need the current `\escapechar` to optionally insert it in front of the control sequence
    /// name, and the current [`CategoryCodeScheme`] to determine whether or not to insert a space afterwards - which
    /// we do unless the control sequence name is a single character with any [`CommandCode`] other than
    /// [`Letter`](CommandCode::Letter).
    fn display_fmt<W: Write>(
        &self,
        int: &<Self::CS as CSName<Self::Char>>::Handler,
        cc: &CategoryCodeScheme<Self::Char>,
        escapechar: Option<Self::Char>,
        f: &mut W,
    ) -> std::fmt::Result {
        match self.to_enum() {
            StandardToken::Character(_, CommandCode::Space) => f.write_char(' '),
            StandardToken::Character(c, _) => {
                c.display_fmt(f);
                Ok(())
            }
            StandardToken::ControlSequence(cs) => cs.display_fmt(int, cc, escapechar, f),
            StandardToken::Primitive(id) => write!(
                f,
                "{}pdfprimitive {}",
                Self::Char::display_opt(escapechar),
                id.display(escapechar)
            ),
        }
    }
    /// Returns a helper struct implementing [`Display`](std::fmt::Display) for this token.
    fn display<'a>(
        &'a self,
        int: &'a <Self::CS as CSName<Self::Char>>::Handler,
        cc: &'a CategoryCodeScheme<Self::Char>,
        escapechar: Option<Self::Char>,
    ) -> DisplayToken<'a, Self> {
        DisplayToken {
            tk: self,
            int,
            cc,
            escapechar,
        }
    }
}

pub struct DisplayToken<'a, T: Token> {
    tk: &'a T,
    int: &'a <T::CS as CSName<T::Char>>::Handler,
    cc: &'a CategoryCodeScheme<T::Char>,
    escapechar: Option<T::Char>,
}
impl<'a, T: Token> std::fmt::Display for DisplayToken<'a, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.tk.display_fmt(self.int, self.cc, self.escapechar, f)
    }
}

/** The simplest (but not most efficient) way to represent a [`Token`] as an enum.

Is [`Copy`] iff [`CS`](Token::CS) is [`Copy`].
*/
#[derive(Clone, Copy, Eq, Debug)]
pub enum StandardToken<Char: Character, CS: CSName<Char>> {
    ControlSequence(CS),
    Character(Char, CommandCode),
    Primitive(PrimitiveIdentifier),
}
impl<Char: Character, CS: CSName<Char>> PartialEq for StandardToken<Char, CS> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (StandardToken::ControlSequence(a), StandardToken::ControlSequence(b)) => a == b,
            (
                StandardToken::Character(_, CommandCode::Space),
                StandardToken::Character(_, CommandCode::Space),
            ) => true,
            (StandardToken::Character(a1, a2), StandardToken::Character(b1, b2)) => {
                a1 == b1 && a2 == b2
            }
            (StandardToken::Primitive(a), StandardToken::Primitive(b)) => a == b,
            _ => false,
        }
    }
}
impl<Char: Character, CS: CSName<Char>> Token for StandardToken<Char, CS> {
    type CS = CS;
    type Char = Char;

    fn to_enum(&self) -> StandardToken<Char, CS> {
        self.clone()
    }

    fn from_cs(cs: CS) -> Self {
        StandardToken::ControlSequence(cs)
    }

    fn space() -> Self {
        StandardToken::Character(Char::from(32), CommandCode::Space)
    }

    fn eof() -> Self {
        StandardToken::Character(Char::from(0), CommandCode::EOF)
    }

    fn from_char_cat(c: Char, cat: CommandCode) -> Self {
        StandardToken::Character(c, cat)
    }

    fn primitive(id: PrimitiveIdentifier) -> Self {
        Self::Primitive(id)
    }

    fn argument_marker(i: u8) -> Self {
        Self::Character(Char::from(i), CommandCode::Argument)
    }
}

/** A compact representation of a [`Token`] with [`Char`](Token::Char)`==u8` and [`CS`](Token::CS)`==`[`InternedCSName`]
 as a single `u32` (similar to the way plain TeX does it) -- i.e. it is small and `Copy`, which yields a significant
 performance improvement in the most performance critical parts of the code.

Values up to `0x8000_0000` are interpreted as interned control sequences, and the rest as character tokens. The downside
is that we need an interning table for control sequences, that needs passing around whenever we want to
make a Token from a control sequence name or display a [`CompactToken`] to the user in a comprehensible way.

(Also, we can only have 2³¹ control sequences in total, but that limit is ridiculously large.)
*/
#[derive(Clone, Copy, Eq, Debug)]
pub struct CompactToken(NonZeroU32);
impl CompactToken {
    fn is_string(&self) -> bool {
        self.0.get() < 0x8000_0000
    }

    fn as_string(&self) -> Option<InternedCSName<u8>> {
        if self.is_string() {
            Some((self.0, PhantomData))
            //Some(InternedString::try_from_usize(self.0 as usize).unwrap())
        } else {
            None
        }
    }

    fn commandcode_value(&self) -> u8 {
        ((self.0.get() & 0x00FF_0000) >> 16) as u8
    }

    fn code(&self) -> CommandCode {
        CommandCode::try_from(self.commandcode_value()).unwrap()
    }

    fn u8(&self) -> u8 {
        (self.0.get() & 0x0000_00FF) as u8
    }
}
impl PartialEq for CompactToken {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0 || {
            if self.is_string() || other.is_string() {
                return false;
            }
            let cc1 = self.code();
            let cc2 = other.code();
            if cc1 == CommandCode::Space && cc2 == CommandCode::Space {
                return true;
            }
            if cc1 != cc2 {
                return false;
            }
            self.u8() == other.u8()
        }
    }
}
impl Token for CompactToken {
    type CS = InternedCSName<u8>; //InternedString;
    type Char = u8;
    //const TOKEN_LIST_FACTORY: Option<RefCell<ReusableVectorFactory<Self>>> = Some(RefCell::new(ReusableVectorFactory::constant()));
    fn to_enum(&self) -> StandardToken<u8, InternedCSName<u8>> {
        match self.as_string() {
            Some(s) => StandardToken::ControlSequence(s),
            None => match self.is_primitive() {
                Some(i) => StandardToken::Primitive(i),
                None => StandardToken::Character(self.u8(), self.code()),
            },
        }
    }

    fn from_cs(cs: Self::CS) -> Self {
        Self(cs.0)
    }

    fn from_char_cat(c: u8, cat: CommandCode) -> Self {
        Self(NonZeroU32::new(0x8000_0000 | ((cat.as_byte() as u32) << 16) | (c as u32)).unwrap())
    }

    fn space() -> Self {
        Self::from_char_cat(32, CommandCode::Space)
    }

    fn eof() -> Self {
        Self::from_char_cat(0, CommandCode::EOF)
    }

    fn primitive(id: PrimitiveIdentifier) -> Self {
        Self(
            NonZeroU32::new(
                0x8000_0000
                    | ((CommandCode::Primitive.as_byte() as u32) << 16)
                    | (id.as_u16() as u32),
            )
            .unwrap(),
        )
    }
    fn is_primitive(&self) -> Option<PrimitiveIdentifier> {
        if !self.is_string()
            && (((self.0.get() & 0x00FF_0000) >> 16) as u8) == CommandCode::Primitive.as_byte()
        {
            PrimitiveIdentifier::try_from_u16((self.0.get() & 0x0000_FFFF) as u16)
        } else {
            None
        }
    }

    fn argument_marker(i: u8) -> Self {
        Self::from_char_cat(i, CommandCode::Argument)
    }

    fn command_code(&self) -> CommandCode {
        if self.is_string() {
            CommandCode::Escape
        } else {
            self.code()
        }
    }

    fn char_value(&self) -> Option<Self::Char> {
        if self.is_string() {
            None
        } else {
            Some(self.u8())
        }
    }

    fn is_cs_or_active(&self) -> bool {
        self.is_string() || {
            let cc = ((self.0.get() & 0x00FF_0000) >> 16) as u8;
            cc == CommandCode::Active.as_byte() || cc == CommandCode::Primitive.as_byte()
        }
    }

    fn is_cs(&self, name: &Self::CS) -> bool {
        self.0 == name.0
    }

    fn is_argument_marker(&self) -> Option<u8> {
        if !self.is_string()
            && (((self.0.get() & 0x00FF_0000) >> 16) as u8) == CommandCode::Argument.as_byte()
        {
            Some(self.u8())
        } else {
            None
        }
    }
}
