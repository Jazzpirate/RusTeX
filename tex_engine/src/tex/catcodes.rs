/*!
    Category codes for characters, implemented as the enum [`CategoryCode`]. A [`CategoryCodeScheme`]
    maps each character of type `Char:`[`CharType`] to a [`CategoryCode`].
*/

use std::fmt::Formatter;
use lazy_static::lazy_static;
use crate::utils::strings::CharType;

/** The category code of a character.

To convert between [`CategoryCode`]s and their numerical values (as [`u8`]), use [`CategoryCode::try_from`]
and [`CategoryCode::into`], respectively.

# Example
```rust
use tex_engine::tex::catcodes::CategoryCode;

let cat = CategoryCode::BeginGroup;
let num : u8 = cat.into();
assert_eq!(num,1);
let cat2 = CategoryCode::try_from(1).unwrap();
assert_eq!(cat2,cat);
```
 */
#[derive(Copy,PartialEq,Clone)]
pub enum CategoryCode {
    /// Escape character (0); usually `\`
    Escape,
    /// Begin group character (1); usually `{`
    BeginGroup,
    /// End group character (2); usually `}`
    EndGroup,
    /// Math shift character (3); usually `$`
    MathShift,
    /// Alignment tab character (4); usually `&`
    AlignmentTab,
    /// End of line character (5); usually `\n`
    EOL,
    /// Parameter character (6); usually `#`
    Parameter,
    /// Superscript character (7); usually `^`
    Superscript,
    /// Subscript character (8); usually `_`
    Subscript,
    /// Ignored character (9)
    Ignored,
    /// Space character (10); usually ` `
    Space,
    /// Letter character (11), usually a-z and A-Z
    Letter,
    /// Other character (12), usually e.g. `@`, `!`, `?`, etc.
    Other,
    /// Active character (13); usually `~`
    Active,
    /// Comment character (14); usually `%`
    Comment,
    /// Invalid character (15)
    Invalid,
    /// Not an "official" category code, but used to mark the end of a file for error messaging
    /// ("file ended while scanning...").
    EOF
}
impl Default for CategoryCode {
    fn default() -> Self { CategoryCode::Other }
}

impl std::fmt::Debug for CategoryCode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self,f)
    }
}
impl std::fmt::Display for CategoryCode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use CategoryCode::*;
        write!(f,"{}",match self {
            Escape => "Escape",
            BeginGroup => "BeginGroup",
            EndGroup => "EndGroup",
            MathShift => "MathShift",
            AlignmentTab => "AlignmentTab",
            EOL => "EOL",
            Parameter => "Parameter",
            Superscript => "Superscript",
            Subscript => "Subscript",
            Ignored => "Ignored",
            Space => "Space",
            Letter => "Letter",
            Other => "Other",
            Active => "Active",
            Comment => "Comment",
            Invalid => "Invalid",
            EOF => "EOF"
        })
    }
}

impl Into<u8> for CategoryCode {
    fn into(self) -> u8 {
        use CategoryCode::*;
        match self {
            Escape => 0,
            BeginGroup => 1,
            EndGroup => 2,
            MathShift => 3,
            AlignmentTab => 4,
            EOL => 5,
            Parameter => 6,
            Superscript => 7,
            Subscript => 8,
            Ignored => 9,
            Space => 10,
            Letter => 11,
            Other => 12,
            Active => 13,
            Comment => 14,
            Invalid => 15,
            EOF => 16
        }
    }
}

impl TryFrom<u8> for CategoryCode {
    type Error = ();

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        use CategoryCode::*;
        Ok(match value {
            0 => Escape,
            1 => BeginGroup,
            2 => EndGroup,
            3 => MathShift,
            4 => AlignmentTab,
            5 => EOL,
            6 => Parameter,
            7 => Superscript,
            8 => Subscript,
            9 => Ignored,
            10 => Space,
            11 => Letter,
            12 => Other,
            13 => Active,
            14 => Comment,
            15 => Invalid,
            _ => return Err(())
        })
    }
}

/// A [`CategoryCodeScheme`] assigns a [`CategoryCode`] to each character for a given [`CharType`].
pub type CategoryCodeScheme<Char> = <Char as CharType>::Allchars<CategoryCode>;


lazy_static! {
    /** The [`CategoryCodeScheme`] where all characters have [`CategoryCode::Other`] (12) except for
        the space character, which has [`CategoryCode::Space`] (10).
     */
    pub static ref OTHER_SCHEME_U8 : CategoryCodeScheme<u8> = {
        let mut catcodes = [CategoryCode::Other;256];
        catcodes[32] = CategoryCode::Space;
        catcodes
    };
    /**
    The default [`CategoryCodeScheme`] for TeX, pre latex.ltx.

    All characters have [`CategoryCode::Other`] (12) except for:

    | Character    | Category Code   |
    |--------------|-----------------
     | ` `          | [`Space`](CategoryCode::Space)|
    | a-z, A-Z     | [`Letter`](CategoryCode::Letter) |
    | `\`          | [`Escape`](CategoryCode::Other)  |
    | `\n`         | [`EOL`](CategoryCode::EOL)       |
    | `%`          | [`Comment`](CategoryCode::Comment)|
    */
    pub static ref STARTING_SCHEME_U8 : CategoryCodeScheme<u8> = {
        let mut catcodes = [CategoryCode::Other;256];
        catcodes[92] = CategoryCode::Escape;
        catcodes[32] = CategoryCode::Space;
        catcodes[13] = CategoryCode::EOL;
        catcodes[37] = CategoryCode::Comment;
        for i in 65..91 { catcodes[i] = CategoryCode::Letter}
        for i in 97..123 { catcodes[i] = CategoryCode::Letter}
        catcodes
    };
        /**
        The default [`CategoryCodeScheme`] used almost everywhere in LaTeX.

        All characters have [`CategoryCode::Other`] (12) except for:

        | Character    | Category Code   |
        |--------------|-----------------
         | ` `          | [`Space`](CategoryCode::Space)|
        | a-z, A-Z     | [`Letter`](CategoryCode::Letter) |
        | `\`          | [`Escape`](CategoryCode::Other)  |
        | `\n`         | [`EOL`](CategoryCode::EOL)       |
        | `%`          | [`Comment`](CategoryCode::Comment)|
        | `~`          | [`Active`](CategoryCode::Active)  |
        | `#`          | [`Parameter`](CategoryCode::Parameter)|
        | `^`          | [`Superscript`](CategoryCode::Superscript)|
        | `_`          | [`Subscript`](CategoryCode::Subscript)|
        | `{`          | [`BeginGroup`](CategoryCode::BeginGroup)|
        | `}`          | [`EndGroup`](CategoryCode::EndGroup)|
        | `$`          | [`MathShift`](CategoryCode::MathShift)|
        | `&`          | [`AlignmentTab`](CategoryCode::AlignmentTab)|
         */
    pub static ref DEFAULT_SCHEME_U8 : CategoryCodeScheme<u8> = {
        let mut catcodes = [CategoryCode::Other;256];
        catcodes[123] = CategoryCode::BeginGroup;
        catcodes[125] = CategoryCode::EndGroup;
        catcodes[36] = CategoryCode::MathShift;
        catcodes[38] = CategoryCode::AlignmentTab;
        catcodes[35] = CategoryCode::Parameter;
        catcodes[94] = CategoryCode::Superscript;
        catcodes[95] = CategoryCode::Subscript;
        catcodes[126] = CategoryCode::Active;
        catcodes[92] = CategoryCode::Escape;
        catcodes[32] = CategoryCode::Space;
        catcodes[13] = CategoryCode::EOL;
        catcodes[37] = CategoryCode::Comment;
        for i in 65..91 { catcodes[i] = CategoryCode::Letter}
        for i in 97..123 { catcodes[i] = CategoryCode::Letter}
        catcodes
    };
}