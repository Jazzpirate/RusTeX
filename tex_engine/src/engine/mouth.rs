/*! A [`Mouth`] is the source of [`Token`]s to be processed by a TeX engine.

 The default implementations ([`NoTracingMouth`] and [`StandardMouth`]) are
 just wrappers around a [`Vec`]`<`[`TeXMouthSource`]`>`,
 which are either [`TokenSource`]s or [`StringSource`]s.

 [`TokenSource`]s are [`Token`]s that have already been processed, while [`StringSource`]s
 represent a string of characters to be tokenized; e.g. from a source file.
 */
pub mod string_source;
pub mod methods;

use std::vec::IntoIter;
use log::debug;
use crate::engine::EngineType;
use crate::engine::filesystem::File;
use crate::engine::mouth::string_source::StringSource;
use crate::engine::state::State;
use crate::{file_end, throw};
use crate::tex::catcodes::{CategoryCode, CategoryCodeScheme};
use crate::tex::commands::TokenCont;
use crate::tex::token::{BaseToken, Token, TokenList, TokenWithSourceref};
use crate::utils::errors::TeXError;
use crate::utils::Ptr;
use crate::utils::strings::CharType;

/// A [`Mouth`] is the source of [`Token`]s to be processed by a TeX engine.
pub trait Mouth<T:Token>:Sized+ Clone +'static {
    fn new() -> Self;

    /// Insert a [`Vec`] of [`Token`]s into the [`Mouth`], to be processed next
    fn push_tokens(&mut self,tks:Vec<T>);

    /// Insert a [`File`] into the [`Mouth`], to be processed next
    fn push_file<F:File<T::Char>>(&mut self,file:&F);

    /// Insert a single [`Token`] into the [`Mouth`], not to be expanded when processed
    fn push_noexpand(&mut self,tk:T);

    /// Insert a single [`Token`] into the [`Mouth`], to be processed next
    /// (for implementations with a peek buffer)
    fn requeue(&mut self,tk:T);

    fn has_next<ET:EngineType<Char=T::Char,Token=T,Mouth=Self>>(&mut self, state:&ET::State) -> Result<bool,TeXError<T>>;

    /// Return the next [`Token`] from the [`Mouth`], and whether to expand it (due to `\noexpand`)
    //#[inline(always)]
    fn get_next<ET:EngineType<Char=T::Char,Token=T,Mouth=Self>>(&mut self,state:&ET::State) -> Result<Option<(T,bool)>,TeXError<T>>;

    /// like [`get_next`](`Mouth::get_next`), but throws an error on `\par` (and [`EOF`](crate::tex::catcodes::CategoryCode::EOF))
    fn get_next_nopar<ET:EngineType<Char=T::Char,Token=T,Mouth=Self>>(&mut self,state:&ET::State) -> Result<Option<(T,bool)>,TeXError<T>> {
        match self.get_next::<ET>(state)? {
            Some((t,b)) => {
                match t.base() {
                    BaseToken::CS(name) if *name == T::Char::par_token() =>
                        throw!("Paragraph ended while reading argument" => t),
                    BaseToken::Char(_,CategoryCode::EOF) =>
                        file_end!(),
                    _ => Ok(Some((t,b)))
                }
            }
            o => Ok(o)
        }
    }

    /// Return the next n characters from the [`Mouth`] as a [`String`], without consuming them
    /// (for error messages, debugging purposes, etc.)
    fn preview(&self,len:usize) -> String;

    /// Return the current file and line number as presentable string
    fn file_line(&self) -> String;

    fn line_no(&self) -> usize;

    fn endinput<ET:EngineType<Token=T,Mouth=Self>>(&mut self,state:&mut ET::State);

    /// Skip whitespace characters from the [`Mouth`]
    fn skip_whitespace<ET:EngineType<Token=T,Mouth=Self>>(&mut self, state:&ET::State) -> Result<(),TeXError<T>> {
        methods::skip_whitespace::<ET>(self,state)
    }

    /// read optional `=` characters from the [`Mouth`]
    fn skip_eq_char<ET:EngineType<Token=T,Mouth=Self>>(&mut self,state:&ET::State) -> Result<(),TeXError<T>> {
        methods::skip_eq_char::<ET>(self,state)
    }

    /// reads a macro argument from the [`Mouth`], i.e. a sequence of [`Token`]s enclosed in
    /// braces (category codes [`BeginGroup`](CategoryCode::BeginGroup) and
    /// [`EndGroup`](CategoryCode::EndGroup)), or a single non-space [`Token`] if the argument is
    /// not enclosed.
    fn read_argument<ET:EngineType<Token=T,Mouth=Self>>(&mut self, state:&mut ET::State,f:TokenCont<ET>) -> Result<(),TeXError<T>> {
        methods::read_argument::<ET>(self,state,f)
    }

    /// Like [`read_argument`](`Mouth::read_argument`), but throws an error on `\par` (and [`EOF`](crate::tex::catcodes::CategoryCode::EOF))
    fn read_argument_nopar<ET:EngineType<Token=T,Mouth=Self>>(&mut self, state:&mut ET::State,f:TokenCont<ET>) -> Result<(),TeXError<T>> {
        methods::read_argument_nopar::<ET>(self,state,f)
    }


    /// reads [`Token`]s from the [`Mouth`] until the next suitable [`EndGroup`](CategoryCode::EndGroup)
    /// [`Token`] is encountered, and returns them as a [`Vec`], respecting nested groups.
    fn read_until_endgroup<ET:EngineType<Token=T,Mouth=Self>>(&mut self, state:&mut ET::State,f:TokenCont<ET>) -> Result<(),TeXError<T>> {
        methods::read_until_endgroup::<ET>(self,state,f)
    }
}


/// Either a [`TokenSource`] or a [`StringSource`]
#[derive(Clone)]
pub enum TeXMouthSource<T:Token> {
    Token(TokenSource<T>),
    /// Only because of `\noexpand`, which sets the [`Command`](crate::tex::commands::BaseCommand)
    /// of an expandable [`Token`] to `\relax` (funnily enough, `\unexpanded` does not do that).
    NoExpand(T),
    String(StringSource<T::Char>)
}

/// A [`TokenSource`] is essentially a pretokenized [`Token`] list
#[derive(Clone)]
pub struct TokenSource<T:Token>(IntoIter<T>);
impl<T:Token> TokenSource<T> {
    pub(crate) fn new(ls:Vec<T>) -> Self { Self(ls.into_iter()) }
    pub fn get_next(&mut self) -> Option<T> { self.0.next() }
    fn preview(&self) -> String {
        let tks : Vec<T> = self.0.clone().collect();
        TokenList(&tks).to_string()
        //crate::interpreter::tokens_to_string_default(&tks)
    }
}

#[derive(Clone)]
pub struct StandardMouth<T:Token>{ sources:Vec<TeXMouthSource<T>>,buffer:Option<T>}

impl<T:Token> Mouth<T> for StandardMouth<T> {
    fn new() -> Self {
        StandardMouth { sources:Vec::new(),buffer:None}
    }

    //#[inline(always)]
    fn push_tokens(&mut self,mut tks:Vec<T>) {
        if tks.is_empty() { return }
        match std::mem::take(&mut self.buffer) {
            Some(t) => tks.push(t),
            _ => ()
        }
        self.sources.push(TeXMouthSource::Token(TokenSource::new(tks)))
    }

    fn push_noexpand(&mut self, tk: T) {
        match self.buffer.take() {
            Some(tk2) => self.sources.push(TeXMouthSource::Token(TokenSource::new(vec!(tk2)))),
            None => ()
        }
        self.sources.push(TeXMouthSource::NoExpand(tk))
    }

    fn push_file<F: File<T::Char>>(&mut self, file: &F) {
        debug!("Pushing file {:?}", file.path());
        match self.buffer.take() {
            Some(tk2) => self.sources.push(TeXMouthSource::Token(TokenSource::new(vec!(tk2)))),
            None => ()
        }
        self.sources.push(TeXMouthSource::String(StringSource::new(
            (*file.content_string()).clone().unwrap(),
            Some(Ptr::new(file.path().to_str().unwrap().to_string()))
        )))
    }

    fn requeue(&mut self, tk: T) {
        match self.buffer.take() {
            Some(tk2) => {
                self.push_tokens(vec!(tk,tk2))
            },
            None => self.buffer = Some(tk)
        }
    }

    fn has_next<ET: EngineType<Char=T::Char,Token=T,Mouth=Self>>(&mut self, state: &ET::State) -> Result<bool, TeXError<T>> {
        match self.buffer {
            Some(_) => Ok(true),
            _ => self.has_next_i(state.get_catcode_scheme(),state.get_endlinechar())
        }
    }

    fn get_next<ET: EngineType<Char=T::Char,Token=T,Mouth=Self>>(&mut self, state: &ET::State) -> Result<Option<(T, bool)>, TeXError<T>> {
        if let Some(tk) = std::mem::take(&mut self.buffer) { return Ok(Some((tk,true))) } else {
            self.get_next_i::<ET>(state)
        }
    }

    fn preview(&self,len:usize) -> String {
        let mut ret = String::new();
        match &self.buffer {
            Some(t) => ret.push_str(format!("{}",t).as_str()),
            None => ()
        }
        for s in self.sources.iter().rev() {
            ret.push_str(&match s {
                TeXMouthSource::NoExpand(t) => TokenList(&vec!(t.clone())).to_string(),
                TeXMouthSource::Token(ts) => ts.preview(),
                TeXMouthSource::String(ss) => ss.preview()
            });
            if ret.len() > len { return ret.chars().take(len).collect() }
        }
        ret.chars().take(len).collect()
    }

    fn line_no(&self) -> usize {
        for s in self.sources.iter().rev() {
            match s {
                TeXMouthSource::String(ss) => return ss.line(),
                _ => ()
            }
        }
        0
    }

    fn endinput<ET: EngineType<Token=T,Mouth=Self>>(&mut self, state: &mut ET::State) {
        for s in self.sources.iter().enumerate().rev() {
            match s.1 {
                TeXMouthSource::String(ss) => {
                    match &ss.source {
                        Some(s) => (state.outputs().file_close)(&*s),
                        None => ()
                    }
                    self.sources.remove(s.0);
                    return
                },
                _ => ()
            }
        }
    }

    fn file_line(&self) -> String {
        for s in self.sources.iter().rev() {
            match s {
                TeXMouthSource::String(ss) => {
                    match &ss.source {
                        Some(s) => return format!("{}:({},{})",s,ss.line(),ss.column()),
                        None => ()
                    }
                }
                _ => ()
            }
        }
        "unknown source".to_string()
    }
}

impl<T:Token> StandardMouth<T> {
    fn has_next_i(&mut self, cc:&CategoryCodeScheme<T::Char>, endline: Option<T::Char>) -> Result<bool,TeXError<T>> {
        if let Some(source) = self.sources.last_mut() {
            return if let Some(tk) = match source {
                TeXMouthSource::Token(ts) => ts.get_next(),
                TeXMouthSource::String(ss) => ss.get_next(cc, endline)?,
                TeXMouthSource::NoExpand(t) => return Ok(true)
            } {
                self.buffer = Some(tk);
                Ok(true)
            } else {
                match source {
                    TeXMouthSource::String(_) => {
                        self.buffer = Some(T::new(BaseToken::Char(T::Char::from(b'\n'),CategoryCode::EOF),None));
                        self.sources.pop();
                        Ok(true)
                    }
                    _ => {
                        self.sources.pop();
                        self.has_next_i(cc, endline)
                    }
                }
            }
        } else { Ok(false) }
    }
    fn get_next_i<ET:EngineType<Char=T::Char,Token=T>>(&mut self, state:&ET::State) -> Result<Option<(T, bool)>,TeXError<T>> {
        loop {
            if let Some(source) = self.sources.last_mut() {
                if let Some(tk) = match source {
                    TeXMouthSource::Token(ts) => ts.get_next(),
                    TeXMouthSource::String(ss) => ss.get_next(state.get_catcode_scheme(), state.get_endlinechar())?,
                    TeXMouthSource::NoExpand(_) => {
                        match self.sources.pop() {
                            Some(TeXMouthSource::NoExpand(t)) => return Ok(Some((t, false))),
                            _ => unreachable!()
                        }
                    }
                } {
                    return Ok(Some((tk, true)))
                } else {
                    match source {
                        TeXMouthSource::String(s) => {
                            match &s.source {
                                Some(s) => (state.outputs().file_close)(&*s),
                                None => ()
                            }
                            self.sources.pop();
                            debug!("file end; inserting \\everyeof");
                            let mut v = state.get_primitive_toks("everyeof");
                            if v.is_empty() {
                                return Ok(Some((T::new(BaseToken::Char(T::Char::from(b'\n'), CategoryCode::EOF), None), true)))
                            } else {
                                v.push(T::new(BaseToken::Char(T::Char::from(b'\n'), CategoryCode::EOF), None));
                                self.push_tokens(v);
                            }
                        }
                        _ => {
                            self.sources.pop();
                        }
                    }
                }
            } else { return Ok(None) }
        }
    }
}