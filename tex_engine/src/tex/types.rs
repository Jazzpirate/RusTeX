/*! enums for various TeX types, e.g. [`GroupType`]s, [`BoxType`]s, [`TeXMode`]s.
*/

use std::fmt::Display;

#[derive(Clone,Copy,Eq,PartialEq,Debug)]
pub struct MathStyle {
    pub cramped: bool,
    pub forced_from: Option<MathStyleType>,
    pub style:MathStyleType
}
#[derive(Clone,Copy,Eq,PartialEq,Debug)]
pub enum MathStyleType { Display, Text, Script, ScriptScript }

impl MathStyle {
    pub fn sup(self) -> Self {
        match self.style {
            MathStyleType::Text | MathStyleType::Display => MathStyle{cramped:self.cramped, forced_from:None,style:MathStyleType::Script},
            MathStyleType::Script | MathStyleType::ScriptScript => MathStyle{cramped:self.cramped, forced_from:None,style:MathStyleType::ScriptScript},
        }
    }
    #[inline(always)]
    pub fn cramp(mut self) -> Self {
        self.cramped = true;
        self
    }
    #[inline(always)]
    pub fn sub(self) -> Self { self.sup().cramp() }
    pub fn numerator(self) -> Self {
        match self.style {
            MathStyleType::Display => MathStyle{cramped:self.cramped, forced_from:None,style:MathStyleType::Text},
            MathStyleType::Text => MathStyle{cramped:self.cramped, forced_from:None,style:MathStyleType::Script},
            MathStyleType::Script | MathStyleType::ScriptScript => MathStyle{cramped:self.cramped, forced_from:None,style:MathStyleType::ScriptScript},
        }
    }
    #[inline(always)]
    pub fn denominator(self) -> Self {
        self.numerator().cramp()
    }
}

#[derive(Debug,Clone,Copy,PartialEq,Eq)]
pub enum MathClass { Ord = 0, Op = 1, Bin = 2, Rel = 3, Open = 4, Close = 5, Punct = 6 }
impl From<u8> for MathClass {
    fn from(v: u8) -> Self {
        match v {
            0 => MathClass::Ord,
            1 => MathClass::Op,
            2 => MathClass::Bin,
            3 => MathClass::Rel,
            4 => MathClass::Open,
            5 => MathClass::Close,
            6 => MathClass::Punct,
            _ => panic!("Invalid math class {}",v)
        }
    }
}

/// The type of a group, e.g. `{...}`, `\begingroup...\endgroup`, `$...$`.
#[derive(Clone,Copy,Eq,PartialEq,Debug)]
pub enum GroupType {
    /// A group delimited by `{` and `}`.
    Character,
    /// A group delimited by `\begingroup` and `\endgroup`.
    ControlSequence,
    /// A box (e.g. `\hbox` or `\vbox`), or a math group.
    Box(BoxType),
    Math{display:bool},
    LeftRight
}

/// The type of a box, e.g. `\hbox` or `\vbox`.
#[derive(Clone,Copy,Eq,PartialEq,Debug)]
pub enum BoxType {
    /// A horizontal box, e.g. `\hbox`.
    Horizontal,
    /// A vertical box, e.g. `\vbox`.
    Vertical,
}
impl BoxType {
    pub fn other(&self) -> Self {
        match self {
            BoxType::Horizontal => BoxType::Vertical,
            BoxType::Vertical => BoxType::Horizontal,
        }
    }
}
impl Display for BoxType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BoxType::Horizontal => write!(f, "hbox"),
            BoxType::Vertical => write!(f, "vbox"),
            //BoxType::InlineMath | BoxType::DisplayMath => write!(f, "math shift")
        }
    }
}

#[derive(Clone,Copy,Eq,PartialEq,Debug)]
pub enum NodeType {
    Char = 0,
    HList = 1,
    VList = 2,
    Rule = 3,
    Insertion = 4,
    Mark = 5,
    Adjust = 6,
    Ligature = 7,
    Discretionary = 8,
    WhatsIt = 9,
    Math = 10,
    Glue = 11,
    Kern = 12,
    Penalty = 13,
    Unset = 14,
    MathChar = 15,
}
impl NodeType {
    pub fn to_u8(&self) -> u8 {
        *self as u8
    }
}


/// The mode the engine is currently in, e.g. horizontal mode or vertical mode.
#[derive(Clone,Copy,Eq,PartialEq,Debug)]
pub enum TeXMode {
    /// The mode the engine is in at the start of a document, outside of boxes or paragraphs
    Vertical,
    /// The mode the engine is in inside a vertical box
    InternalVertical,
    /// The mode the engine is in inside a paragraph
    Horizontal,
    /// The mode the engine is in inside a horizontal box
    RestrictedHorizontal,
    /// The mode the engine is in inside an inline math box
    InlineMath,
    /// The mode the engine is in inside a display math box
    DisplayMath
}
impl TeXMode {
    pub fn is_vertical(&self) -> bool {
        match self {
            TeXMode::Vertical | TeXMode::InternalVertical => true,
            _ => false
        }
    }
    pub fn is_horizontal(&self) -> bool {
        match self {
            TeXMode::Horizontal | TeXMode::RestrictedHorizontal => true,
            _ => false
        }
    }
    pub fn is_math(&self) -> bool {
        match self {
            TeXMode::InlineMath | TeXMode::DisplayMath => true,
            _ => false
        }
    }
    pub fn h_or_m(&self) -> bool {
        match self {
            TeXMode::Horizontal | TeXMode::RestrictedHorizontal | TeXMode::InlineMath | TeXMode::DisplayMath => true,
            _ => false
        }
    }
}
impl From<BoxType> for TeXMode {
    fn from(bt: BoxType) -> Self {
        match bt {
            BoxType::Horizontal => TeXMode::RestrictedHorizontal,
            BoxType::Vertical => TeXMode::InternalVertical,
        }
    }
}