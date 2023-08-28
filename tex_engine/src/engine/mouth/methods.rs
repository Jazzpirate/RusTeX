//! Default implementations for [`MouthTrait`] methods

use crate::engine::mouth::{ExpansionContainer, Mouth, MouthTrait};
use crate::debug_log;
use crate::engine::{EngineRef, EngineType};
use crate::tex::catcodes::CategoryCode;
use crate::tex::token::{BaseToken, Token};
use crate::utils::errors::TeXError;
use crate::utils::strings::CharType;

impl<ET:EngineType> EngineRef<ET> {
    /// get the next [`Token`] from the [`MouthTrait`]
    pub fn get_next_token(&mut self) -> Option<(Token<ET>,bool)> {
        self.mouth.get_next(&self.state,&mut self.interner,&mut self.outputs)
    }

    /// Skip whitespace characters from the [`MouthTrait`]
    pub fn skip_whitespace(&mut self) {
        self.mouth.skip_whitespace(&self.state,&mut self.interner)
    }

    /// read optional `=` characters from the [`MouthTrait`]
    pub fn skip_eq_char(&mut self) {
        self.skip_whitespace();
        debug_log!(trace=>"skipping '='");
        if let Some((tk,_)) = self.get_next_token() {
            match &tk.base {
                BaseToken::Char(c,_) if c.to_usize() == 61 => {
                    match self.get_next_token() {
                        Some((tk,_)) if tk.catcode() == CategoryCode::Space => (),
                        Some((tk,_)) => self.mouth.requeue(tk),
                        _ => ()
                    }
                },
                _ => self.mouth.requeue(tk)
            }
        }
    }

    /// reads a macro argument from the [`MouthTrait`], i.e. a sequence of [`Token`]s enclosed in
    /// braces (category codes [`BeginGroup`](CategoryCode::BeginGroup) and
    /// [`EndGroup`](CategoryCode::EndGroup)), or a single non-space [`Token`] if the argument is
    /// not enclosed.
    pub fn get_argument(&mut self,vec: &mut Vec<Token<ET>>) {
        Mouth::get_argument(self,vec)
    }
/*
    /// reads [`Token`]s from the [`Mouth`] until the next suitable [`EndGroup`](CategoryCode::EndGroup)
    /// or throws an error if the next [`Token`] is not a [`BeginGroup`](CategoryCode::BeginGroup)
    pub fn get_group(&mut self, f:TokenCont<ET>) -> Result<(),TeXError<ET>> {
        match self.get_next_token()? {
            Some((t,_)) if t.catcode() == CategoryCode::BeginGroup => (),
            _ => throw!("begin group expected")
        }
        let mut ingroup = 0;
        while let Some(next) = self.get_next_token()? {
            let tk = next.0;
            match &tk.base {
                BaseToken::Char(_,CategoryCode::BeginGroup) => ingroup += 1,
                BaseToken::Char(_,CategoryCode::EndGroup) => {
                    if ingroup == 0 { return Ok(()) } else { ingroup -= 1; }
                }
                _ => ()
            }
            f(self,tk)?;
        }
        file_end!()
    }

 */

    pub fn with_mouth<F:FnMut(&mut EngineRef<ET>) -> R,R>(&mut self, tks:Vec<Token<ET>>, f:F) -> R {
        Mouth::with_mouth(self,tks,f)
    }

    pub fn add_expansion<F,R>(&mut self,f:F) -> R where F:FnOnce(&mut EngineRef<ET>,&mut ExpansionContainer<ET>) -> R {
        Mouth::add_expansion(self,f)
    }

    /// Return the next n characters from the [`MouthTrait`] as a [`String`], without consuming them
    /// (for error messages, debugging purposes, etc.)
    pub fn preview(&mut self,len:usize) -> String {
        self.mouth.preview(len,&self.interner)
    }

    pub fn current_position(&mut self) -> String {
        self.mouth.file_line(&self.interner)
    }
}

#[macro_export]
macro_rules! get_until_endgroup {
    ($engine:ident,$tk:ident => $f:expr) => {
        let mut depth = 1;
        let mut ok = false;
        while let Some($tk) = $engine.mouth.get_next_simple(&$engine.state,&mut $engine.interner) {
            match $tk.catcode() {
                CategoryCode::BeginGroup => depth += 1,
                CategoryCode::EndGroup => {
                    depth -= 1;
                    if depth == 0 { ok = true; break }
                },
                CategoryCode::EOF => crate::file_end!(),
                _ => ()
            }
            $f;
        }
        if (!ok) {crate::file_end!()}
    }
}

#[macro_export]
macro_rules! get_group {
    ($engine:ident,$tk:ident => $f:expr) => {
        match $engine.get_next_token() {
            Some((t,_)) if t.catcode() == CategoryCode::BeginGroup => (),
            _ => throw!("begin group expected")
        }
        let mut ingroup = 0;
        let mut ok = false;
        while let Some(next) = $engine.get_next_token() {
            let $tk = next.0;
            match &$tk.base {
                BaseToken::Char(_,CategoryCode::BeginGroup) => ingroup += 1,
                BaseToken::Char(_,CategoryCode::EndGroup) => {
                    if ingroup == 0 { ok = true;break } else { ingroup -= 1; }
                }
                BaseToken::Char(_,CategoryCode::EOF) =>
                    crate::file_end!(),
                _ => ()
            }
            $f
        }
        if !ok { file_end!() }
    }
}