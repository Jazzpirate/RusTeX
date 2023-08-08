/*! A [`Mouth`] is the source of [`Token`]s to be processed by a TeX engine.

 The default implementations ([`NoTracingMouth`] and [`StandardMouth`]) are
 just wrappers around a [`Vec`]`<`[`TeXMouthSource`]`>`,
 which are either [`TokenSource`]s or [`StringSource`]s.

 [`TokenSource`]s are [`Token`]s that have already been processed, while [`StringSource`]s
 represent a string of characters to be tokenized; e.g. from a source file.
 */
pub mod string_source;
pub mod methods;

use std::hint::unreachable_unchecked;
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

    fn new_with(tks:Vec<T>) -> Self;

    /// Insert a [`Vec`] of [`Token`]s into the [`Mouth`], to be processed next
    fn push_tokens(&mut self,tks:TokenSource<T>);

    fn new_tokensource(&mut self) -> TokenSource<T>;

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
pub struct TokenSource<T:Token>(pub Vec<T>,usize,T);
impl<T:Token> TokenSource<T> {
    pub fn new() -> Self { Self(Vec::with_capacity(2097152),0,T::new(BaseToken::Char(T::Char::from(b'X'),CategoryCode::Invalid),None)) }
    fn new_with(v:Vec<T>) -> Self { Self(v,0,T::new(BaseToken::Char(T::Char::from(b'X'),CategoryCode::Invalid),None)) }
    fn is_empty(&self) -> bool { self.0.len() == self.1 }
    fn get_next(&mut self) -> (T,bool) {
        let mut ret = self.2.clone();
        unsafe { std::mem::swap(self.0.get_mut(self.1).unwrap_unchecked(),&mut ret) };
        self.1 += 1;
        (ret,self.is_empty())
    }
    fn preview(&self) -> String {
        let tks  = &self.0[self.1..];
        TokenList(tks).to_string()
        //crate::interpreter::tokens_to_string_default(&tks)
    }
    pub(crate) fn reset(mut self) -> Self {
        self.0.clear();
        self.1 = 0;
        self
    }
    pub fn push(&mut self,t:T) {
        self.0.push(t)
    }
}

#[derive(Clone)]
pub struct StandardMouth<T:Token>{ pub sources:Vec<TeXMouthSource<T>>,buffer:Option<T>,pub news:Vec<TokenSource<T>>}

impl<T:Token> Mouth<T> for StandardMouth<T> {
    fn new() -> Self {
        let mut s = StandardMouth { sources:Vec::with_capacity(32),buffer:None,news:Vec::with_capacity(32)};
        for _ in (0..32) { s.news.push(TokenSource::new()) }
        s
    }
    fn new_with(tks: Vec<T>) -> Self {
        StandardMouth{buffer:None,news:Vec::new(),sources:vec!(TeXMouthSource::Token(TokenSource::new_with(tks)))}
    }

    fn new_tokensource(&mut self) -> TokenSource<T> {
        match self.news.pop() {
            Some(t) => t,
            None => TokenSource::new()
        }
    }

    //#[inline(always)]
    fn push_tokens(&mut self,mut tks:TokenSource<T>) {
        if tks.is_empty() {
            self.news.push(tks);
            return
        }
        match std::mem::take(&mut self.buffer) {
            Some(t) => tks.push(t),
            _ => ()
        }
        self.sources.push(TeXMouthSource::Token(tks))
    }

    fn push_noexpand(&mut self, tk: T) {
        match self.buffer.take() {
            Some(tk2) => {
                let mut new = self.new_tokensource();
                new.push(tk2);
                self.sources.push(TeXMouthSource::Token(new))
            }
            None => ()
        }
        self.sources.push(TeXMouthSource::NoExpand(tk))
    }

    fn push_file<F: File<T::Char>>(&mut self, file: &F) {
        debug!("Pushing file {:?}", file.path());
        match self.buffer.take() {
            Some(tk2) => {
                let mut new = self.new_tokensource();
                new.push(tk2);
                self.sources.push(TeXMouthSource::Token(new))
            }
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
                let mut next = self.new_tokensource();
                next.push(tk);
                next.push(tk2);
                self.push_tokens(next)
            },
            None => self.buffer = Some(tk)
        }
    }

    fn has_next<ET: EngineType<Char=T::Char,Token=T,Mouth=Self>>(&mut self, state: &ET::State) -> Result<bool, TeXError<T>> {
        match self.buffer {
            Some(_) => Ok(true),
            _ => self.has_next_i::<ET>(state)
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
    fn has_next_i<ET: EngineType<Char=T::Char, Token=T>>(&mut self, state:&ET::State) -> Result<bool, TeXError<T>> {
        if let Some(source) = self.sources.last_mut() {
            match source {
                TeXMouthSource::Token(ts) => Ok(true),
                TeXMouthSource::String(ss) => {
                    match ss.get_next(state.get_catcode_scheme(), state.get_endlinechar())? {
                        Some(t) => {
                            self.buffer = Some(t);
                            Ok(true)
                        },
                        None => {
                            match self.pop_string::<ET>(state) {
                                None => self.has_next_i::<ET>(state),
                                Some(t) => {
                                    self.buffer = Some(t);
                                    Ok(true)
                                }
                            }
                        }
                    }
                }
                TeXMouthSource::NoExpand(t) => return Ok(true)
            }
        } else { Ok(false) }
    }
    fn get_next_i<ET: EngineType<Char=T::Char, Token=T>>(&mut self, state: &ET::State) -> Result<Option<(T, bool)>, TeXError<T>> {
        loop {
            if let Some(source) = self.sources.last_mut() {
                match source {
                    TeXMouthSource::String(ss) =>
                        match ss.get_next(state.get_catcode_scheme(), state.get_endlinechar())? {
                            Some(t) => return Ok(Some((t,true))),
                            _ => match self.pop_string::<ET>(state) {
                                None => (),
                                Some(t) => return Ok(Some((t,true)))
                            }
                        }
                    TeXMouthSource::Token(ts) => match ts.get_next() {
                        (t, true) => {
                            let old = unsafe {
                                match self.sources.pop().unwrap_unchecked() {
                                    TeXMouthSource::Token(ts) => ts,
                                    _ => unreachable_unchecked()
                                }
                            };
                            self.news.push(old.reset());
                            return Ok(Some((t,true)))
                        }
                        (t, _) => return Ok(Some((t,true)))
                    }
                    TeXMouthSource::NoExpand(_) => {
                        match self.sources.pop() {
                            Some(TeXMouthSource::NoExpand(t)) => return Ok(Some((t, false))),
                            _ => unreachable!()
                        }
                    }
                }
            } else { return Ok(None) }
        }
    }

    fn pop_string<ET: EngineType<Char=T::Char, Token=T>>(&mut self,state:&ET::State) -> Option<ET::Token> {
        let ss = unsafe{ match self.sources.pop() {
            Some(TeXMouthSource::String(ss)) => ss,
            _ => unreachable_unchecked()
        }};
        match ss.source {
            Some(s) => (state.outputs().file_close)(&s),
            None => ()
        }
        debug!("file end; inserting \\everyeof");
        let mut v = state.get_primitive_toks("everyeof"); // TODO
        if v.is_empty() {
            Some(T::new(BaseToken::Char(T::Char::from(b'\n'), CategoryCode::EOF), None))
        } else {
            let mut next = self.new_tokensource();
            next.push(T::new(BaseToken::Char(T::Char::from(b'\n'), CategoryCode::EOF), None));
            for t in v { next.push(t);}
            self.push_tokens(next);
            None
        }
    }
}