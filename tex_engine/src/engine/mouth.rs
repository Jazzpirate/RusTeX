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
use crate::{debug_log, file_end, throw};
use crate::engine::gullet::BMouth;
use crate::tex::catcodes::{CategoryCode, CategoryCodeScheme};
use crate::tex::commands::TokenCont;
use crate::tex::token::{BaseToken, Token, TokenList};
use crate::utils::errors::TeXError;
use crate::utils::Ptr;
use crate::utils::strings::CharType;

/// A [`Mouth`] is the source of [`Token`]s to be processed by a TeX engine.
pub trait Mouth<ET:EngineType> {
    //fn new() -> Self;

    //fn new_with(tks:Vec<Token<ET>>) -> Self;

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

    /// Return the next n characters from the [`Mouth`] as a [`String`], without consuming them
    /// (for error messages, debugging purposes, etc.)
    fn preview(&self,len:usize) -> String;

    /// Return the current file and line number as presentable string
    fn file_line(&self) -> String;

    fn line_no(&self) -> usize;

    fn endinput(&mut self,state:&mut ET::State);

    /// Skip whitespace characters from the [`Mouth`]
    fn skip_whitespace(&mut self,state:&ET::State) -> Result<(),TeXError<ET>> {
        debug_log!(trace=>"skipping whitespace");
        while let Some((tk,_)) = self.get_next(state)? {
            match tk.catcode() {
                CategoryCode::Space => (),
                _ => {
                    self.requeue(tk);
                    break
                }
            }
        }
        Ok(())
    }

    /// reads a macro argument from the [`Mouth`], i.e. a sequence of [`Token`]s enclosed in
    /// braces (category codes [`BeginGroup`](CategoryCode::BeginGroup) and
    /// [`EndGroup`](CategoryCode::EndGroup)), or a single non-space [`Token`] if the argument is
    /// not enclosed.
    fn get_argument(&mut self,state:&ET::State,f:TokenCont<ET>) -> Result<(),TeXError<ET>> {
        match self.get_next(state)? {
            None => file_end!(),
            Some((t,_)) if t.catcode() == CategoryCode::BeginGroup => self.get_until_endgroup(state,f),
            Some((o,_)) => {
                f(state,o)?;
                Ok(())
            }
        }
    }

    /// reads [`Token`]s from the [`Mouth`] until the next suitable [`EndGroup`](CategoryCode::EndGroup)
    /// [`Token`] is encountered, and returns them as a [`Vec`], respecting nested groups.
    fn get_until_endgroup(&mut self,state:&ET::State ,f:TokenCont<ET>) -> Result<(),TeXError<ET>> {
        let mut depth = 1;
        while let Some((tk,_)) = self.get_next(state)? {
            match tk.catcode() {
                CategoryCode::BeginGroup => depth += 1,
                CategoryCode::EndGroup => {
                    depth -= 1;
                    if depth == 0 { return Ok(()) }
                },
                CategoryCode::EOF => file_end!(),
                _ => ()
            }
            f(state,tk)?;
        }
        file_end!()
    }

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

    /// Like [`read_argument`](`Mouth::read_argument`), but throws an error on `\par` (and [`EOF`](crate::tex::catcodes::CategoryCode::EOF))
    fn get_argument_nopar(&mut self,state:&ET::State, f:TokenCont<ET>) -> Result<(),TeXError<ET>> {
        match self.get_next(state)? {
            None => file_end!(),
            Some((t,_)) if t.catcode() == CategoryCode::BeginGroup => {
                let mut depth = 1;
                while let Some((tk,_)) = self.get_next(state)? {
                    match &tk.base {
                        BaseToken::Char(_,CategoryCode::BeginGroup) => depth += 1,
                        BaseToken::Char(_,CategoryCode::EndGroup) => {
                            depth -= 1;
                            if depth == 0 { return Ok(()) }
                        },
                        BaseToken::CS(n) if *n == ET::Char::par_token() =>
                            throw!("Paragraph ended while reading argument" => t),
                        _ => ()
                    }
                    f(state,tk)?;
                }
                file_end!()
            }
            Some((t,_)) if t.catcode() == CategoryCode::EOF => file_end!(),
            Some((o,_)) => {
                f(state,o)?;
                Ok(())
            }
        }
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
pub struct TokenSource<ET:EngineType>(Vec<Option<Token<ET>>>,usize);
impl<ET:EngineType> TokenSource<ET> {
    pub fn new() -> Self { Self(Vec::with_capacity(2097152),0)}
    fn new_with(v:Vec<Token<ET>>) -> Self { Self(v.into_iter().map(|t| Some(t)).collect(),0) }
    fn is_empty(&self) -> bool { self.0.len() == self.1 }
    fn get_next(&mut self) -> (Token<ET>,bool) {
        //let mut ret = self.2.clone();
        let mut ret = unsafe { std::mem::take(&mut self.0[self.1]).unwrap_unchecked() };
        self.1 += 1;
        (ret,self.is_empty())
    }
    fn preview(&self) -> String {
        let tks : Vec<Token<ET>> = self.0[self.1..].iter().map(|t| unsafe{t.clone().unwrap_unchecked()}).collect();
        TokenList(&tks).to_string()
        //crate::interpreter::tokens_to_string_default(&tks)
    }
    pub(crate) fn reset(mut self) -> Self {
        self.0.clear();
        self.1 = 0;
        self
    }
    pub fn push(&mut self,t:Token<ET>) {
        self.0.push(Some(t))
    }
}

#[derive(Clone)]
pub struct StandardMouth<ET:EngineType>{ pub sources:Vec<TeXMouthSource<ET>>,buffer:Option<Token<ET>>,pub news:Vec<TokenSource<ET>>}

impl<ET:EngineType> Mouth<ET> for StandardMouth<ET> {

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

impl<ET:EngineType> StandardMouth<ET> {
    pub fn new() -> Self {
        let mut s = StandardMouth { sources:Vec::with_capacity(32),buffer:None,news:Vec::with_capacity(32)};
        for _ in (0..32) { s.news.push(TokenSource::new()) }
        s
    }
    pub fn new_with(tks: Vec<Token<ET>>) -> Self {
        StandardMouth{buffer:None,news:Vec::new(),sources:vec!(TeXMouthSource::Token(TokenSource::new_with(tks)))}
    }
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
                            let old =  match self.sources.pop() {
                                    Some(TeXMouthSource::Token(ts)) => ts,
                                    _ => unsafe { unreachable_unchecked() }
                            };
                            self.news.push(old.reset());
                            return Ok(Some((t,true)))
                        }
                        (t, _) => return Ok(Some((t,true)))
                    }
                    TeXMouthSource::NoExpand(_) => {
                            match self.sources.pop() {
                                Some(TeXMouthSource::NoExpand(t)) => return Ok(Some((t, false))),
                                _ => unsafe {unreachable_unchecked() }
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


/*
use std::alloc::{alloc,dealloc,Layout};
use std::ptr;

struct AllocationPool<A>{free:*mut LinkedNode<A>,layout:Layout}
impl<A> AllocationPool<A> {
    fn new() -> Self {
        AllocationPool{free:ptr::null_mut(),layout:Layout::new::<LinkedNode<A>>()}
    }
}
impl<A> Drop for AllocationPool<A> {
    fn drop(&mut self) {
        unsafe {
            let mut next = self.free;
            while !next.is_null() {
                let current = next;
                next = (*next).next;
                dealloc(current as *mut u8,self.layout);
            }
        }
    }
}

struct LinkedNode<A> {
    value: A,
    next: *mut LinkedNode<A>
}
struct LinkedArray<A> {
    head:*mut LinkedNode<A>,
    pools:Vec<AllocationPool<A>>
}
impl<A> LinkedArray<A> {
    fn new() -> Self {
        LinkedArray {head:ptr::null_mut(),pools:vec!(AllocationPool::new()) }
    }
    fn next(&mut self) -> Option<A> {
        if self.head.is_null() {
            None
        } else { unsafe {
            if self.pools.is_empty() { self.pools.push(AllocationPool::new()); }
            let mut pool = unsafe{ self.pools.last_mut().unwrap_unchecked()};
            let value = ptr::read(&(*self.head).value);
            let next = (*self.head).next;
            (*self.head).next = pool.free;
            pool.free = self.head;
            self.head = next;
            Some(value)
        } }
    }
}

impl<A> Drop for LinkedArray<A> {
    fn drop(&mut self) {
        unsafe {
            let mut next = self.head;
            let layout = Layout::new::<LinkedNode<A>>();
            while !next.is_null() {
                let current = next;
                next = (*next).next;
                ptr::drop_in_place(&mut (*current).value);
                dealloc(current as *mut u8,layout);
            }
        }
    }
}

struct LinkedArrayPrepender<A> {
    head:*mut LinkedNode<A>,
    last:*mut LinkedNode<A>,
    pool:AllocationPool<A>,
}
impl<A> LinkedArrayPrepender<A> {
    fn new(pool:AllocationPool<A>) -> Self {
        LinkedArrayPrepender {head:ptr::null_mut(),last:ptr::null_mut(),pool}
    }
    fn push(&mut self, a: A) {
        let node = if self.pool.free.is_null() {
            unsafe {alloc(self.pool.layout) as *mut LinkedNode<A>}
        } else {
            let n = self.pool.free;
            self.pool.free = unsafe{ (*n).next };
            n
        };
        unsafe {
            if self.head.is_null() {
                self.head = node;
                self.last = node;
            } else {
                (*self.last).next = node;
                self.last = node;
            }
            ptr::write(&mut (*node).value, a);
            (*node).next = ptr::null_mut();
        }
    }
    fn close(mut self,arr: &mut LinkedArray<A>) {
        if self.head.is_null() {return ()}
        unsafe {
            (*self.last).next = arr.head;
            arr.head = self.head;
            self.head = ptr::null_mut();
            self.last = ptr::null_mut();
            arr.pools.push(self.pool)
        }
    }
}

 */