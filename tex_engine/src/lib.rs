#![forbid(unsafe_code)]

pub mod utils;
pub mod engine;
pub mod tex;
pub mod commands;

#[cfg(feature="pdflatex")]
pub mod pdflatex;

#[doc(hidden)]
pub mod tests;

pub mod prelude {
    pub use crate::tex::catcodes::{CategoryCode, CategoryCodeScheme, CommandCode};
    pub use crate::tex::input_text::{Character,CharacterMap};
    pub use crate::tex::token::Token;
    pub use crate::tex::control_sequences::{CSName,CSHandler};
}