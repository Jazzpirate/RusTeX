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
use log::debug;
use crate::engine::EngineType;
use crate::engine::filesystem::File;
use crate::engine::mouth::string_source::StringSource;
use crate::engine::state::State;
use crate::{file_end, throw};
use crate::tex::catcodes::{CategoryCode, CategoryCodeScheme};
use crate::tex::commands::TokenCont;
use crate::tex::token::{BaseToken, Token, TokenList};
use crate::utils::errors::TeXError;
use crate::utils::Ptr;
use crate::utils::strings::CharType;

/// A [`Mouth`] is the source of [`Token`]s to be processed by a TeX engine.
pub trait Mouth<ET:EngineType<Mouth=Self>>:Sized+ Clone +'static {
    fn new() -> Self;

    fn new_with(tks:Vec<Token<ET>>) -> Self;

    /// Insert a [`Vec`] of [`Token`]s into the [`Mouth`], to be processed next
    fn push_tokens(&mut self,tks:TokenSource<ET>);

    fn new_tokensource(&mut self) -> TokenSource<ET>;

    /// Insert a [`File`] into the [`Mouth`], to be processed next
    fn push_file(&mut self,file:&ET::File);

    /// Insert a single [`Token`] into the [`Mouth`], not to be expanded when processed
    fn push_noexpand(&mut self,tk:Token<ET>);

    /// Insert a single [`Token`] into the [`Mouth`], to be processed next
    /// (for implementations with a peek buffer)
    fn requeue(&mut self,tk:Token<ET>);

    fn has_next(&mut self, state:&ET::State) -> Result<bool,TeXError<ET>>;

    /// Return the next [`Token`] from the [`Mouth`], and whether to expand it (due to `\noexpand`)
    //#[inline(always)]
    fn get_next(&mut self,state:&ET::State) -> Result<Option<(Token<ET>,bool)>,TeXError<ET>>;

    /// like [`get_next`](`Mouth::get_next`), but throws an error on `\par` (and [`EOF`](crate::tex::catcodes::CategoryCode::EOF))
    fn get_next_nopar(&mut self,state:&ET::State) -> Result<Option<(Token<ET>,bool)>,TeXError<ET>> {
        match self.get_next(state)? {
            Some((t,b)) => {
                match &t.base {
                    BaseToken::CS(name) if *name == ET::Char::par_token() =>
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

    fn endinput(&mut self,state:&mut ET::State);

    /// Skip whitespace characters from the [`Mouth`]
    fn skip_whitespace(&mut self, state:&ET::State) -> Result<(),TeXError<ET>> {
        methods::skip_whitespace::<ET>(self,state)
    }

    /// read optional `=` characters from the [`Mouth`]
    fn skip_eq_char(&mut self,state:&ET::State) -> Result<(),TeXError<ET>> {
        methods::skip_eq_char::<ET>(self,state)
    }

    /// reads a macro argument from the [`Mouth`], i.e. a sequence of [`Token`]s enclosed in
    /// braces (category codes [`BeginGroup`](CategoryCode::BeginGroup) and
    /// [`EndGroup`](CategoryCode::EndGroup)), or a single non-space [`Token`] if the argument is
    /// not enclosed.
    fn read_argument(&mut self, state:&mut ET::State,f:TokenCont<ET>) -> Result<(),TeXError<ET>> {
        methods::read_argument::<ET>(self,state,f)
    }

    /// Like [`read_argument`](`Mouth::read_argument`), but throws an error on `\par` (and [`EOF`](crate::tex::catcodes::CategoryCode::EOF))
    fn read_argument_nopar(&mut self, state:&mut ET::State,f:TokenCont<ET>) -> Result<(),TeXError<ET>> {
        methods::read_argument_nopar::<ET>(self,state,f)
    }


    /// reads [`Token`]s from the [`Mouth`] until the next suitable [`EndGroup`](CategoryCode::EndGroup)
    /// [`Token`] is encountered, and returns them as a [`Vec`], respecting nested groups.
    fn read_until_endgroup(&mut self, state:&mut ET::State,f:TokenCont<ET>) -> Result<(),TeXError<ET>> {
        methods::read_until_endgroup::<ET>(self,state,f)
    }
}


/// Either a [`TokenSource`] or a [`StringSource`]
#[derive(Clone)]
pub enum TeXMouthSource<ET:EngineType> {
    Token(TokenSource<ET>),
    /// Only because of `\noexpand`, which sets the [`Command`](crate::tex::commands::BaseCommand)
    /// of an expandable [`Token`] to `\relax` (funnily enough, `\unexpanded` does not do that).
    NoExpand(Token<ET>),
    String(StringSource<ET::Char>)
}

/// A [`TokenSource`] is essentially a pretokenized [`Token`] list
#[derive(Clone)]
pub struct TokenSource<ET:EngineType>(pub Vec<Token<ET>>,usize,Token<ET>);
impl<ET:EngineType> TokenSource<ET> {
    pub fn new() -> Self { Self(Vec::with_capacity(2097152),0,Token::new(BaseToken::Char(ET::Char::from(b'X'),CategoryCode::Invalid),None)) }
    fn new_with(v:Vec<Token<ET>>) -> Self { Self(v,0,Token::new(BaseToken::Char(ET::Char::from(b'X'),CategoryCode::Invalid),None)) }
    fn is_empty(&self) -> bool { self.0.len() == self.1 }
    fn get_next(&mut self) -> (Token<ET>,bool) {
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
    pub fn push(&mut self,t:Token<ET>) {
        self.0.push(t)
    }
}

#[derive(Clone)]
pub struct StandardMouth<ET:EngineType<Mouth=Self>>{ pub sources:Vec<TeXMouthSource<ET>>,buffer:Option<Token<ET>>,pub news:Vec<TokenSource<ET>>}

impl<ET:EngineType<Mouth=Self>> Mouth<ET> for StandardMouth<ET> {
    fn new() -> Self {
        let mut s = StandardMouth { sources:Vec::with_capacity(32),buffer:None,news:Vec::with_capacity(32)};
        for _ in (0..32) { s.news.push(TokenSource::new()) }
        s
    }
    fn new_with(tks: Vec<Token<ET>>) -> Self {
        StandardMouth{buffer:None,news:Vec::new(),sources:vec!(TeXMouthSource::Token(TokenSource::new_with(tks)))}
    }

    fn new_tokensource(&mut self) -> TokenSource<ET> {
        match self.news.pop() {
            Some(t) => t,
            None => TokenSource::new()
        }
    }

    //#[inline(always)]
    fn push_tokens(&mut self,mut tks:TokenSource<ET>) {
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

    fn push_noexpand(&mut self, tk: Token<ET>) {
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

    fn push_file(&mut self, file: &ET::File) {
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

    fn requeue(&mut self, tk: Token<ET>) {
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

    fn has_next(&mut self, state: &ET::State) -> Result<bool, TeXError<ET>> {
        match self.buffer {
            Some(_) => Ok(true),
            _ => self.has_next_i(state)
        }
    }

    fn get_next(&mut self, state: &ET::State) -> Result<Option<(Token<ET>, bool)>, TeXError<ET>> {
        if let Some(tk) = std::mem::take(&mut self.buffer) { return Ok(Some((tk,true))) } else {
            self.get_next_i(state)
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

    fn endinput(&mut self, state: &mut ET::State) {
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

impl<ET:EngineType<Mouth=Self>> StandardMouth<ET> {
    fn has_next_i(&mut self, state:&ET::State) -> Result<bool, TeXError<ET>> {
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
                            match self.pop_string(state) {
                                None => self.has_next_i(state),
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
    fn get_next_i(&mut self, state: &ET::State) -> Result<Option<(Token<ET>, bool)>, TeXError<ET>> {
        loop {
            if let Some(source) = self.sources.last_mut() {
                match source {
                    TeXMouthSource::String(ss) =>
                        match ss.get_next(state.get_catcode_scheme(), state.get_endlinechar())? {
                            Some(t) => return Ok(Some((t,true))),
                            _ => match self.pop_string(state) {
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

    fn pop_string(&mut self,state:&ET::State) -> Option<Token<ET>> {
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
            Some(Token::new(BaseToken::Char(ET::Char::from(b'\n'), CategoryCode::EOF), None))
        } else {
            let mut next = self.new_tokensource();
            next.push(Token::new(BaseToken::Char(ET::Char::from(b'\n'), CategoryCode::EOF), None));
            for t in v { next.push(t);}
            self.push_tokens(next);
            None
        }
    }
}