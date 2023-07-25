/*! A [`Mouth`] is the source of [`Token`]s to be processed by a TeX engine.

 The default implementations ([`NoTracingMouth`] and [`TracingMouth`]) are
 just wrappers around a [`Vec`]`<`[`TeXMouthSource`]`>`,
 which are either [`TokenSource`]s or [`StringSource`]s.

 [`TokenSource`]s are [`Token`]s that have already been processed, while [`StringSource`]s
 represent a string of characters to be tokenized; e.g. from a source file.
 */
pub mod string_source;
pub mod methods;

use std::vec::IntoIter;
use log::debug;
use crate::engine::filesystem::File;
use crate::engine::mouth::string_source::StringSource;
use crate::engine::state::State;
use crate::file_end;
use crate::tex::catcodes::{CategoryCode, CategoryCodeScheme};
use crate::tex::token::{BaseToken, Token, TokenList, TokenWithSourceref};
use crate::utils::errors::{InvalidCharacter, OtherError, TeXError};
use crate::utils::Ptr;
use crate::utils::strings::CharType;

/// Either a [`TokenSource`] or a [`StringSource`]
pub enum TeXMouthSource<T:Token> {
    Token(TokenSource<T>),
    /// Only because of `\noexpand`, which sets the [`Command`](crate::tex::commands::Command)
    /// of an expandable [`Token`] to `\relax` (funnily enough, `\unexpanded` does not do that).
    NoExpand(T),
    String(StringSource<T::Char>)
}

/// A [`TokenSource`] is essentially a pretokenized [`Token`] list
pub struct TokenSource<T:Token>(IntoIter<T>);
impl<T:Token> TokenSource<T> {
    pub(crate) fn new(ls:Vec<T>) -> Self { Self(ls.into_iter()) }
    pub fn get_next(&mut self) -> Option<T> { self.0.next() }
    fn preview(&self) -> String {
        let tks : Vec<T> = self.0.clone().collect();
        TokenList(tks).to_string()
        //crate::interpreter::tokens_to_string_default(&tks)
    }
}

/// A [`Mouth`] is the source of [`Token`]s to be processed by a TeX engine.
pub trait Mouth<T:Token>:Sized+'static {
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

    fn has_next<S:State<T>>(&mut self, state:&S) -> Result<bool,InvalidCharacter<T>>;

    /// Return the next [`Token`] from the [`Mouth`], and whether to expand it (due to `\noexpand`)
    //#[inline(always)]
    fn get_next<S:State<T>>(&mut self,state:&S) -> Result<Option<(T,bool)>,InvalidCharacter<T>>;

    /// like [`get_next`](`Mouth::get_next`), but throws an error on `\par` (and [`EOF`](crate::tex::catcodes::CategoryCode::EOF))
    fn get_next_nopar<S:State<T>>(&mut self,state:&S) -> Result<Option<(T,bool)>,Box<dyn TeXError<T>>> {
        match self.get_next(state)? {
            Some((t,b)) => {
                match t.base() {
                    BaseToken::CS(name) if *name == T::Char::par_token() =>
                        Err(OtherError{msg:format!("Paragraph ended while reading argument"),cause:Some(t),source:None}.into()),
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

    /// Skip whitespace characters from the [`Mouth`]
    fn skip_whitespace<S:State<T>>(&mut self, state:&S) -> Result<(),InvalidCharacter<T>> {
        methods::skip_whitespace(self,state)
    }

    /// read optional `=` characters from the [`Mouth`]
    fn skip_eq_char<S:State<T>>(&mut self,state:&S) -> Result<(),InvalidCharacter<T>> {
        methods::skip_eq_char(self,state)
    }

    /// reads a macro argument from the [`Mouth`], i.e. a sequence of [`Token`]s enclosed in
    /// braces (category codes [`BeginGroup`](CategoryCode::BeginGroup) and
    /// [`EndGroup`](CategoryCode::EndGroup)), or a single non-space [`Token`] if the argument is
    /// not enclosed.
    fn read_argument<S:State<T>>(&mut self, state:&S) -> Result<Vec<T>,Box<dyn TeXError<T>>> {
        methods::read_argument(self,state)
    }

    /// Like [`read_argument`](`Mouth::read_argument`), but throws an error on `\par` (and [`EOF`](crate::tex::catcodes::CategoryCode::EOF))
    fn read_argument_nopar<S:State<T>>(&mut self, state:&S) -> Result<Vec<T>,Box<dyn TeXError<T>>> {
        methods::read_argument_nopar(self,state)
    }


    /// reads [`Token`]s from the [`Mouth`] until the next suitable [`EndGroup`](CategoryCode::EndGroup)
    /// [`Token`] is encountered, and returns them as a [`Vec`], respecting nested groups.
    fn read_until_endgroup<S:State<T>>(&mut self, state:&S) -> Result<Vec<T>,Box<dyn TeXError<T>>> {
        methods::read_until_endgroup(self,state)
    }
}

/// A [`NoTracingMouth`] is the standard implementation of a [`Mouth`] that does not keep track of
/// source references
pub struct NoTracingMouth<T:Token>{ sources:Vec<TeXMouthSource<T>>,buffer:Option<T>}
impl<T:Token> NoTracingMouth<T> {

/*
    fn has_next_i(&mut self, cc:&CategoryCodeScheme<T::Char>, endline: T::Char) -> Result<bool,TeXError<T>> {
        if let Some(source) = self.sources.last_mut() {
            return if let Some(tk) = match source {
                TeXMouthSource::Token(ts) => ts.get_next(),
                TeXMouthSource::String(ss) => ss.get_next_plain(cc, endline)?,
                TeXMouthSource::NoExpand(_) => return Ok(true)
            } {
                self.buffer = Some(tk);
                Ok(true)
            } else {
                self.sources.pop();
                self.has_next_i(cc, endline)
            }
        } else { Ok(false) }
    }

    fn get_next_i(&mut self, cc:&CategoryCodeScheme<T::Char>, endline: T::Char) -> Result<Option<T>,TeXError<T>> {
        if let Some(source) = self.sources.last_mut() {
            return if let Some(tk) = match source {
                TeXMouthSource::Token(ts) => ts.get_next(),
                TeXMouthSource::String(ss) => ss.get_next_plain(cc, endline)?,
                TeXMouthSource::NoExpand(_) => {
                    match self.sources.pop() {
                        Some(TeXMouthSource::NoExpand(t)) => return Ok(Some(t)), // todo: Make `\relax`
                        _ => unreachable!()
                    }
                }
            } {
                debug_log!(debug=>"Read token {:?}", tk);
                Ok(Some(tk))
            } else {
                debug_log!(debug=>"Top Mouth empty");
                self.sources.pop();
                self.get_next_i(cc, endline)
            }
        } else { Ok(None) }
    }

 */
}

impl<T:Token> Mouth<T> for NoTracingMouth<T> {

    fn new() -> Self {
        NoTracingMouth { sources:Vec::new(),buffer:None}
    }
    //#[inline(always)]
    fn push_tokens(&mut self,mut tks:Vec<T>) {
        match self.buffer.take() {
            Some(tk2) => tks.push(tk2),
            None => ()
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
            file.content_string().clone(),
            Some(Ptr::new(file.path().to_str().unwrap().to_string()))
        )))
    }

    fn requeue(&mut self, tk: T) {
        match self.buffer.take() {
            Some(tk2) => {
                self.push_tokens(vec!(tk2,tk))
            },
            None => self.buffer = Some(tk)
        }
    }

    fn has_next<S:State<T>>(&mut self, state:&S) -> Result<bool,InvalidCharacter<T>> {
        todo!("has_next for NoTracingMouth")
    }

    //#[inline(always)]
    fn get_next<S:State<T>>(&mut self, state:&S) -> Result<Option<(T,bool)>,InvalidCharacter<T>> {
        todo!("get_next for NoTracingMouth")
    }

    fn preview(&self,_len:usize) -> String { todo!("preview in NoTracingMouth") }
    fn file_line(&self) -> String { "unknown source".to_string() }
}

/// Like [`NoTracingMouth`], but keeping track of [`SourceReference`](crate::tex::token::SourceReference)s
// I wish there was a smarter way to implement this than literally copy-pasting the whole thing
// just for adapting one method -.-
pub struct TracingMouth<Char:CharType>{ sources:Vec<TeXMouthSource<TokenWithSourceref<Char>>>,buffer:Option<TokenWithSourceref<Char>>}

impl<Char:CharType> Mouth<TokenWithSourceref<Char>> for TracingMouth<Char> {
    fn new() -> Self {
        TracingMouth { sources:Vec::new(),buffer:None}
    }

    //#[inline(always)]
    fn push_tokens(&mut self,mut tks:Vec<TokenWithSourceref<Char>>) {
        match std::mem::take(&mut self.buffer) {
            Some(t) => tks.push(t),
            _ => ()
        }
        self.sources.push(TeXMouthSource::Token(TokenSource::new(tks)))
    }

    fn push_noexpand(&mut self, tk: TokenWithSourceref<Char>) {
        match self.buffer.take() {
            Some(tk2) => self.sources.push(TeXMouthSource::Token(TokenSource::new(vec!(tk2)))),
            None => ()
        }
        self.sources.push(TeXMouthSource::NoExpand(tk))
    }

    fn push_file<F: File<Char>>(&mut self, file: &F) {
        debug!("Pushing file {:?}", file.path());
        match self.buffer.take() {
            Some(tk2) => self.sources.push(TeXMouthSource::Token(TokenSource::new(vec!(tk2)))),
            None => ()
        }
        self.sources.push(TeXMouthSource::String(StringSource::new(
            file.content_string(),
            Some(Ptr::new(file.path().to_str().unwrap().to_string()))
        )))
    }

    fn requeue(&mut self, tk: TokenWithSourceref<Char>) {
        match self.buffer.take() {
            Some(tk2) => {
                self.push_tokens(vec!(tk2,tk))
            },
            None => self.buffer = Some(tk)
        }
    }

    fn has_next<S:State<TokenWithSourceref<Char>>>(&mut self, state:&S) -> Result<bool,InvalidCharacter<TokenWithSourceref<Char>>> {
        match self.buffer {
            Some(_) => Ok(true),
            _ => self.has_next_i(state.get_catcode_scheme(),state.get_endlinechar())
        }
    }

    //#[inline(always)]
    fn get_next<S:State<TokenWithSourceref<Char>>>(&mut self, s:&S) -> Result<Option<(TokenWithSourceref<Char>, bool)>,InvalidCharacter<TokenWithSourceref<Char>>> {
        if let Some(tk) = std::mem::take(&mut self.buffer) { return Ok(Some((tk,true))) } else {
            self.get_next_i(s)
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
                TeXMouthSource::NoExpand(t) => todo!("preview `\noexpand`ed token"),
                TeXMouthSource::Token(ts) => ts.preview(),
                TeXMouthSource::String(ss) => ss.preview()
            });
            if ret.len() > len { return ret.chars().take(len).collect() }
        }
        ret.chars().take(len).collect()
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

impl<C:CharType> TracingMouth<C> {

    fn has_next_i(&mut self, cc:&CategoryCodeScheme<C>, endline: Option<C>) -> Result<bool,InvalidCharacter<TokenWithSourceref<C>>> {
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
                        self.buffer = Some(TokenWithSourceref::<C>::new(BaseToken::Char(C::from(b'\n'),CategoryCode::EOF),None));
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
    fn get_next_i<S:State<TokenWithSourceref<C>>>(&mut self, state:&S) -> Result<Option<(TokenWithSourceref<C>, bool)>,InvalidCharacter<TokenWithSourceref<C>>> {
        if let Some(source) = self.sources.last_mut() {
            return if let Some(tk) = match source {
                TeXMouthSource::Token(ts) => ts.get_next(),
                TeXMouthSource::String(ss) => ss.get_next(state.get_catcode_scheme(), state.get_endlinechar())?,
                TeXMouthSource::NoExpand(_) => {
                    match self.sources.pop() {
                        Some(TeXMouthSource::NoExpand(t)) => return Ok(Some((t,false))),
                        _ => unreachable!()
                    }
                }
            } {
                Ok(Some((tk,true)))
            } else {
                match source {
                    TeXMouthSource::String(_) => {
                        self.sources.pop();
                        debug!("file end; inserting \\everyeof");
                        let mut v = state.get_primitive_toks("everyeof");
                        if v.is_empty() {
                            Ok(Some((TokenWithSourceref::<C>::new(BaseToken::Char(C::from(b'\n'),CategoryCode::EOF),None),true)))
                        } else {
                            v.push(TokenWithSourceref::<C>::new(BaseToken::Char(C::from(b'\n'), CategoryCode::EOF),None));
                            self.push_tokens(v);
                            self.get_next_i(state)
                        }
                    }
                    _ => {
                        self.sources.pop();
                        self.get_next_i(state)
                    }
                }
            }
        } else { Ok(None) }
    }
}