//! Different enums for the different "modes" of TeX engines, groups, etc.

use std::fmt::Display;

/// A TeX engine is always in one of these modes
#[derive(Copy,Clone,PartialEq)]
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
pub trait GroupType:Clone+PartialEq+Default {
    fn from_catcode_token() -> Self;
    fn from_begingroup_cs() -> Self;
}


/// An default implementation of [`GroupType`] for the different types of TeX groups.
#[derive(Copy,Clone,PartialEq)]
pub enum TeXGroupType {
    /// The initial "group" containing the entire document.
    Top,
    /// A group delimited by `{` and `}`.
    Token,
    /// A group delimited by `\begingroup` and `\endgroup`.
    CS,
    /// A box of some [`BoxMode`].
    Box(BoxMode),
}
impl Default for TeXGroupType {
    fn default() -> Self { TeXGroupType::Top }
}
impl GroupType for TeXGroupType {
    fn from_begingroup_cs() -> Self { TeXGroupType::CS }
    fn from_catcode_token() -> Self { TeXGroupType::Token }
}

/// An enum for the different types of boxes in TeX.
#[derive(Copy,Clone,PartialEq)]
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