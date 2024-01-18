#![forbid(unsafe_code)]

pub mod utils;
pub mod engine;
pub mod tex;
pub mod commands;

#[cfg(feature="pdflatex")]
pub mod pdflatex;

#[doc(hidden)]
pub mod tests;

/// Default `pub use` prelude for the crate
pub mod prelude {
    pub use crate::tex::catcodes::{CategoryCode, CategoryCodeScheme, CommandCode};
    pub use crate::tex::characters::{Character, CharacterMap};
    pub use crate::tex::tokens::Token;
    pub use crate::tex::tokens::control_sequences::{CSName,CSHandler,ResolvedCSName,InternedCSName};
    pub use crate::tex::tokens::token_lists::{CharWrite, TokenList};
    pub use crate::engine::EngineTypes;
    pub use crate::engine::stomach::TeXMode;
    pub use crate::utils::errors::ErrorHandler;
}