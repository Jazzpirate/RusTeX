//! Different enums for the different "modes" of TeX engines, groups, etc.

use std::fmt::Display;

/// A TeX engine is always in one of these modes
#[derive(Copy,Clone,PartialEq,Debug)]
pub enum TeXMode {
    /// initial mode outside of paragraphs and other boxes
    Vertical,
    /// mode inside of `\vbox` and similar boxes
    InternalVertical,
    /// mode inside of paragraphs
    Horizontal,
    /// mode inside of `\hbox` and similar boxes
    RestrictedHorizontal,
    /// mode inside of inline math
    Math,
    /// mode inside of display math
    Displaymath
}

impl Default for TeXMode {
    fn default() -> Self { TeXMode::Vertical }
}

impl TeXMode {
    pub fn is_vertical(&self) -> bool {
        use TeXMode::*;
        match self {
            Vertical | InternalVertical => true,
            _ => false
        }
    }
}
impl Display for TeXMode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use TeXMode::*;
        match self {
            Vertical => write!(f, "vertical"),
            InternalVertical => write!(f, "internal vertical"),
            Horizontal => write!(f, "horizontal"),
            RestrictedHorizontal => write!(f, "restricted horizontal"),
            Math => write!(f, "math"),
            Displaymath => write!(f, "display math")
        }
    }
}

/// The types of TeX groups that exist
#[derive(Copy,Clone,PartialEq,Debug)]
pub enum GroupType {
    /// The initial "group" containing the entire document.
    Top,
    /// A group delimited by `{` and `}`.
    Token,
    /// A group delimited by `\begingroup` and `\endgroup`.
    CS,
    /// A box of some [`BoxMode`].
    Box(BoxMode),
}
impl Default for GroupType {
    fn default() -> Self { GroupType::Top }
}

/// An enum for the different types of boxes in TeX.
#[derive(Copy,Clone,PartialEq,Debug)]
pub enum BoxMode {
    /// A horizontal box
    H,
    /// A vertical box
    V,
    /// An inline math box
    M,
    /// A display math box
    DM,
    /// A box delimited by `\left` and `\right`
    LeftRight,
    /// A box with no content
    Void
}


#[derive(Copy,Clone,PartialEq)]
pub enum FontStyle {
    Text,Script,Scriptscript
}
impl Default for FontStyle {
    fn default() -> Self { FontStyle::Text }
}
impl FontStyle {
    pub fn inc(&self) -> FontStyle {
        use FontStyle::*;
        match self {
            Text => Script,
            _ => Scriptscript
        }
    }
}