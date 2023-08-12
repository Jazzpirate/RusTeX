//! Default implementations for [`Mouth`] methods

use crate::engine::mouth::Mouth;
use crate::{debug_log, file_end, throw};
use crate::engine::{EngineRef, EngineMut, EngineType};
use crate::engine::gullet::Gullet;
use crate::engine::state::State;
use crate::tex::catcodes::CategoryCode;
use crate::tex::commands::TokenCont;
use crate::tex::token::{BaseToken, Token};
use crate::utils::errors::TeXError;
use crate::utils::strings::CharType;

impl<ET:EngineType> EngineMut<'_,ET> {
    /// get the next [`Token`] from the [`Mouth`]
    pub fn get_next_token(&mut self) -> Result<Option<(Token<ET>,bool)>,TeXError<ET>> {
        self.gullet.mouth().get_next(self.state)
    }

    /// Skip whitespace characters from the [`Mouth`]
    pub fn skip_whitespace(&mut self) -> Result<(),TeXError<ET>> {
        self.gullet.mouth().skip_whitespace(self.state)
    }

    /// read optional `=` characters from the [`Mouth`]
    pub fn skip_eq_char(&mut self) -> Result<(),TeXError<ET>> {
        self.skip_whitespace()?;
        debug_log!(trace=>"skipping '='");
        if let Some((tk,_)) = self.get_next_token()? {
            match &tk.base {
                BaseToken::Char(c,_) if c.to_usize() == 61 => {
                    match self.get_next_token()? {
                        Some((tk,_)) if tk.catcode() == CategoryCode::Space => (),
                        Some((tk,_)) => self.gullet.mouth().requeue(tk),
                        _ => ()
                    }
                },
                _ => self.gullet.mouth().requeue(tk)
            }
        }
        Ok(())
    }

    /// reads a macro argument from the [`Mouth`], i.e. a sequence of [`Token`]s enclosed in
    /// braces (category codes [`BeginGroup`](CategoryCode::BeginGroup) and
    /// [`EndGroup`](CategoryCode::EndGroup)), or a single non-space [`Token`] if the argument is
    /// not enclosed.
    pub fn get_argument(&mut self,f:TokenCont<ET>) -> Result<(),TeXError<ET>> {
        self.gullet.mouth().get_argument(self.state,f)
    }

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
            f(self.state,tk)?;
        }
        file_end!()
    }

    /// Return the next n characters from the [`Mouth`] as a [`String`], without consuming them
    /// (for error messages, debugging purposes, etc.)
    pub fn preview(&mut self,len:usize) -> String {
        self.gullet.mouth().preview(len)
    }

    pub fn current_position(&mut self) -> String {
        self.gullet.mouth().file_line()
    }
}

impl<ET:EngineType> EngineRef<'_,ET> {

    /// Return the next n characters from the [`Mouth`] as a [`String`], without consuming them
    /// (for error messages, debugging purposes, etc.)
    pub fn preview(&self,len:usize) -> String {
        todo!("make this non-mutable")//self.gullet.mouth().preview(len)
    }

    pub fn current_position(&self) -> String {
        todo!("make this non-mutable")//self.gullet.mouth().file_line()
    }
}