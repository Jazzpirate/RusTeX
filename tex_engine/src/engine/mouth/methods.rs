//! Default implementations for [`Mouth`] methods

use crate::engine::mouth::{Mouth, TokenSource};
use crate::{debug_log, file_end, throw};
use crate::engine::{EngineRef, EngineMut, EngineType};
use crate::engine::gullet::Gullet;
use crate::engine::memory::Memory;
use crate::engine::state::State;
use crate::tex::catcodes::CategoryCode;
use crate::tex::commands::TokenCont;
use crate::tex::token::{BaseToken, Token};
use crate::utils::errors::TeXError;
use crate::utils::strings::CharType;

impl<ET:EngineType> EngineMut<'_,ET> {
    /// get the next [`Token`] from the [`Mouth`]
    pub fn get_next_token(&mut self) -> Result<Option<(Token<ET>,bool)>,TeXError<ET>> {
        self.mouth.get_next(self.state,self.memory)
    }

    /// Skip whitespace characters from the [`Mouth`]
    pub fn skip_whitespace(&mut self) -> Result<(),TeXError<ET>> {
        self.mouth.skip_whitespace(self.state,self.memory)
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
                        Some((tk,_)) => self.mouth.requeue(tk),
                        _ => ()
                    }
                },
                _ => self.mouth.requeue(tk)
            }
        }
        Ok(())
    }

    /// reads a macro argument from the [`Mouth`], i.e. a sequence of [`Token`]s enclosed in
    /// braces (category codes [`BeginGroup`](CategoryCode::BeginGroup) and
    /// [`EndGroup`](CategoryCode::EndGroup)), or a single non-space [`Token`] if the argument is
    /// not enclosed.
    pub fn get_argument(&mut self,f:TokenCont<ET>) -> Result<(),TeXError<ET>> {
        self.mouth.get_argument(self.state,self.memory,f)
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

    pub fn split_mouth(&mut self) -> (&mut ET::Mouth,EngineMutNoMouth<ET>) {
        (self.mouth,EngineMutNoMouth {
            state: self.state,
            stomach: self.stomach,
            memory: self.memory,
            gullet: self.gullet,
        })
    }

    pub fn with_mouth<F:FnMut(&mut EngineMut<ET>) -> R,R>(&mut self,tks:Vec<Token<ET>>,mut f:F) -> R {
        let (m,mut r) = self.split_mouth();
        m.with_mouth(&mut r,tks,f)
    }

    pub fn add_expansion<F,R>(&mut self,f:F) -> R where F:FnOnce(&mut EngineMut<ET>,&mut TokenSource<ET>) -> R {
        let (m,mut r) = self.split_mouth();
        m.add_expansion(&mut r,f)
    }

    /// Return the next n characters from the [`Mouth`] as a [`String`], without consuming them
    /// (for error messages, debugging purposes, etc.)
    pub fn preview(&mut self,len:usize) -> String {
        self.mouth.preview(len)
    }

    pub fn current_position(&mut self) -> String {
        self.mouth.file_line()
    }
}

impl<ET:EngineType> EngineRef<'_,ET> {

    /// Return the next n characters from the [`Mouth`] as a [`String`], without consuming them
    /// (for error messages, debugging purposes, etc.)
    pub fn preview(&self,len:usize) -> String {
        self.mouth.preview(len)
    }

    pub fn current_position(&self) -> String {
        self.mouth.file_line()
    }
}

pub struct EngineMutNoMouth<'a,ET:EngineType> {
    pub state:&'a mut ET::State,
    pub stomach:&'a mut ET::Stomach,
    pub memory:&'a mut Memory<ET>,
    pub gullet:&'a mut ET::Gullet,
}

impl<ET:EngineType> EngineMutNoMouth<'_,ET> {
    pub fn join_mouth<'b>(&'b mut self,mouth:&'b mut ET::Mouth) -> EngineMut<'b,ET> {
        EngineMut {
            state: self.state,
            stomach: self.stomach,
            memory: self.memory,
            mouth,
            gullet:self.gullet,
        }
    }
}