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
use crate::engine::{EngineMut, EngineType};
use crate::engine::filesystem::File;
use crate::engine::mouth::string_source::StringSource;
use crate::engine::state::State;
use crate::{debug_log, file_end, throw};
use crate::engine::memory::{ExpansionContainer, Memory, TokenArray};
use crate::engine::mouth::methods::EngineMutNoMouth;
use crate::tex::catcodes::{CategoryCode, CategoryCodeScheme};
use crate::tex::commands::TokenCont;
use crate::tex::token::{BaseToken, Token, TokenList};
use crate::utils::errors::TeXError;
use crate::utils::Ptr;
use crate::utils::strings::CharType;

/// A [`Mouth`] is the source of [`Token`]s to be processed by a TeX engine.
pub trait Mouth<ET:EngineType<Mouth=Self>>:Sized {
    fn with_mouth<F:FnMut(&mut EngineMut<ET>) -> R,R>(&mut self,engine:&mut EngineMutNoMouth<ET>,tks:Vec<Token<ET>>,mut f:F) -> R {
        let old = std::mem::replace(self, Self::new_with(tks,engine.memory));
        let mut engine = engine.join_mouth(self);
        let ret = f(&mut engine);
        std::mem::replace(self, old);
        ret
    }
    fn new(memory:&mut Memory<ET>) -> Self;
    fn new_with(tks:Vec<Token<ET>>,memory:&mut Memory<ET>) -> Self;

    /// Insert a [`Vec`] of [`Token`]s into the [`Mouth`], to be processed next
    fn add_expansion<F,R>(&mut self,engine:&mut EngineMutNoMouth<ET>,f:F) -> R where F:FnOnce(&mut EngineMut<ET>,&mut ExpansionContainer<ET>) -> R;
    /// Insert a [`File`] into the [`Mouth`], to be processed next
    fn push_file(&mut self,file:&ET::File,memory:&mut Memory<ET>);

    /// Insert a single [`Token`] into the [`Mouth`], not to be expanded when processed
    fn push_noexpand(&mut self,tk:Token<ET>,memory:&mut Memory<ET>);

    /// Insert a single [`Token`] into the [`Mouth`], to be processed next
    /// (for implementations with a peek buffer)
    fn requeue(&mut self,tk:Token<ET>,memory:&mut Memory<ET>);

    /// Return the next [`Token`] from the [`Mouth`], and whether to expand it (due to `\noexpand`)
    //#[inline(always)]
    fn get_next(&mut self,state:&ET::State,memory:&mut Memory<ET>) -> Result<Option<(Token<ET>,bool)>,TeXError<ET>>;

    /// Return the next n characters from the [`Mouth`] as a [`String`], without consuming them
    /// (for error messages, debugging purposes, etc.)
    fn preview(&self,len:usize,memory:&Memory<ET>) -> String;

    /// Return the current file and line number as presentable string
    fn file_line(&self,memory:&Memory<ET>) -> String;

    fn line_no(&self) -> usize;

    fn endinput(&mut self,state:&mut ET::State,memory:&mut Memory<ET>);

    /// Skip whitespace characters from the [`Mouth`]
    fn skip_whitespace(&mut self,state:&ET::State,memory:&mut Memory<ET>) -> Result<(),TeXError<ET>> {
        debug_log!(trace=>"skipping whitespace");
        while let Some((tk,_)) = self.get_next(state,memory)? {
            match tk.catcode() {
                CategoryCode::Space => (),
                _ => {
                    self.requeue(tk,memory);
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
    fn get_argument(&mut self,engine:&mut EngineMutNoMouth<ET>,f:TokenCont<ET>) -> Result<(),TeXError<ET>> {
        match self.get_next(engine.state,engine.memory)? {
            None => file_end!(),
            Some((t,_)) if t.catcode() == CategoryCode::BeginGroup => self.get_until_endgroup(engine,f),
            Some((o,_)) => {
                let mut engine = engine.join_mouth(self);
                f(&mut engine,o)?;
                Ok(())
            }
        }
    }

    /// reads [`Token`]s from the [`Mouth`] until the next suitable [`EndGroup`](CategoryCode::EndGroup)
    /// [`Token`] is encountered, and returns them as a [`Vec`], respecting nested groups.
    fn get_until_endgroup(&mut self,engine:&mut EngineMutNoMouth<ET>,f:TokenCont<ET>) -> Result<(),TeXError<ET>> {
        let mut engine = engine.join_mouth(self);
        let mut depth = 1;
        while let Some((tk,_)) = engine.get_next_token()? {
            match tk.catcode() {
                CategoryCode::BeginGroup => depth += 1,
                CategoryCode::EndGroup => {
                    depth -= 1;
                    if depth == 0 { return Ok(()) }
                },
                CategoryCode::EOF => file_end!(),
                _ => ()
            }
            f(&mut engine,tk)?;
        }
        file_end!()
    }

    /// like [`get_next`](`Mouth::get_next`), but throws an error on `\par` (and [`EOF`](crate::tex::catcodes::CategoryCode::EOF))
    fn get_next_nopar(&mut self,state:&ET::State,memory:&mut Memory<ET>) -> Result<Option<(Token<ET>,bool)>,TeXError<ET>> {
        match self.get_next(state,memory)? {
            Some((t,b)) => {
                match &t.base {
                    BaseToken::CS(name) if *name == memory.par =>
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
    fn get_argument_nopar(&mut self,engine:&mut EngineMutNoMouth<ET>, f:TokenCont<ET>) -> Result<(),TeXError<ET>> {
        match self.get_next(engine.state,engine.memory)? {
            None => file_end!(),
            Some((t,_)) if t.catcode() == CategoryCode::BeginGroup => {
                let mut depth = 1;
                while let Some((tk,_)) = self.get_next(engine.state,engine.memory)? {
                    match &tk.base {
                        BaseToken::Char(_,CategoryCode::BeginGroup) => depth += 1,
                        BaseToken::Char(_,CategoryCode::EndGroup) => {
                            depth -= 1;
                            if depth == 0 { return Ok(()) }
                        },
                        BaseToken::CS(n) if *n == engine.memory.par =>
                            throw!("Paragraph ended while reading argument" => t),
                        _ => ()
                    }
                    let mut engine = engine.join_mouth(self);
                    f(&mut engine,tk)?;
                }
                file_end!()
            }
            Some((t,_)) if t.catcode() == CategoryCode::EOF => file_end!(),
            Some((o,_)) => {
                let mut engine = engine.join_mouth(self);
                f(&mut engine,o)?;
                Ok(())
            }
        }
    }
}


/// Either a [`TokenSource`] or a [`StringSource`]
#[derive(Clone)]
pub enum TeXMouthSource<ET:EngineType> {
    Token((Token<ET>,bool)),
    String(StringSource<ET::Char>)
}

#[derive(Clone)]
pub struct StandardMouth<ET:EngineType<Mouth=Self>>{ pub stack:Vec<TeXMouthSource<ET>> }

impl<ET:EngineType<Mouth=Self>> Mouth<ET> for StandardMouth<ET> {
    fn new(memory:&mut Memory<ET>) -> Self {
        StandardMouth { stack:Vec::with_capacity(2097152)}
    }
    fn new_with(tks: Vec<Token<ET>>,memory:&mut Memory<ET>) -> Self {
        StandardMouth{stack:tks.into_iter().rev().map(|t| TeXMouthSource::Token((t,true))).collect()}
    }

    /// Insert a [`Vec`] of [`Token`]s into the [`Mouth`], to be processed next
    fn add_expansion<F,R>(&mut self,engine:&mut EngineMutNoMouth<ET>,f:F) -> R where F:FnOnce(&mut EngineMut<ET>,&mut ExpansionContainer<ET>) -> R {
        let mut source = engine.memory.get_expansion_container();
        let ret = {
            let mut engine = engine.join_mouth(self);
            f(&mut engine,&mut source)
        };
        source.consume::<_,()>(engine.memory,|s| {
            self.stack.push(TeXMouthSource::Token(s))
        });
        ret
    }

    fn push_noexpand(&mut self, tk: Token<ET>,memory:&mut Memory<ET>) {
        self.stack.push(TeXMouthSource::Token((tk,false)))
    }

    fn push_file(&mut self, file: &ET::File,memory:&mut Memory<ET>) {
        debug!("Pushing file {:?}", file.path());
        let mut source = TeXMouthSource::String(StringSource::new(
            (*file.content_string()).clone().unwrap(),
            Some(memory.interner.get_or_intern(file.path().to_str().unwrap().to_string()))
        ));
        self.stack.push(source);
    }

    fn requeue(&mut self, tk: Token<ET>,memory:&mut Memory<ET>) {
        self.stack.push(TeXMouthSource::Token((tk,true)))
    }

    fn get_next(&mut self, state: &ET::State,memory:&mut Memory<ET>) -> Result<Option<(Token<ET>, bool)>, TeXError<ET>> {
        match self.stack.last_mut() {
            Some(TeXMouthSource::Token(ref mut tks)) => match self.stack.pop() {
                Some(TeXMouthSource::Token((t, b))) => Ok(Some((t, b))),
                _ => unreachable!()
            },
            Some(TeXMouthSource::String(ref mut s)) => {
                match s.get_next(memory,state.get_catcode_scheme(), state.get_endlinechar())? {
                    Some(t) => return Ok(Some((t, true))),
                    None => {
                        match &s.source {
                            None => (),
                            Some(s) => (state.outputs().file_close)(memory.interner.resolve(*s).unwrap())
                        }
                        self.stack.pop();
                        debug_log!(debug => "file end; inserting \\everyeof");
                        let mut v = state.get_primitive_toks("everyeof"); // TODO
                        if v.is_empty() {
                            Ok(Some((Token::new(BaseToken::Char(ET::Char::from(b'\n'), CategoryCode::EOF), None),true)))
                        } else {
                            self.stack.push(TeXMouthSource::Token((Token::new(BaseToken::Char(ET::Char::from(b'\n'), CategoryCode::EOF), None),true)));
                            for t in v.into_iter().rev() { self.stack.push(TeXMouthSource::Token((t,true))) };
                            match self.stack.pop() {
                                Some(TeXMouthSource::Token((t, b))) => Ok(Some((t, b))),
                                _ => unreachable!()
                            }
                        }
                    }
                }
            },
            None => Ok(None)
        }
        /*
        loop {
            match self.current {
                TeXMouthSource::Token(ref mut tks) => {
                    match tks.get_next() {
                        s @ Some(_) => return Ok(s),
                        None => {
                            match self.stack.pop() {
                                Some(mut s) => {
                                    std::mem::swap(&mut self.current, &mut s);
                                    memory.return_token_array(unsafe{
                                        match s {
                                            TeXMouthSource::Token(t) => t,
                                            _ => unreachable_unchecked()
                                        }
                                    })
                                },
                                None => return Ok(None)
                            }
                        }
                    }
                },
                TeXMouthSource::String(ref mut s) => {
                    match s.get_next(state.get_catcode_scheme(), state.get_endlinechar())? {
                        Some(t) => return Ok(Some((t, true))),
                        None => {

                        }
                    }
                }
            }
        }

         */
    }

    fn preview(&self,len:usize,memory:&Memory<ET>) -> String { // TODO memory
        let mut ret = String::new();
        for s in self.stack.iter().rev() {
            ret.push_str(&match s {
                TeXMouthSource::Token(ts) => ts.0.base.to_str(memory,Some(ET::Char::backslash())),
                TeXMouthSource::String(ss) => ss.preview()
            });
            if ret.len() > len { ret.split_off(len);return ret }
        }
        ret
    }

    fn line_no(&self) -> usize {
        for s in self.stack.iter().rev() {
            match s {
                TeXMouthSource::String(ss) => return ss.line(),
                _ => ()
            }
        }
        0
    }

    fn endinput(&mut self, state: &mut ET::State,memory:&mut Memory<ET>) {
        for s in self.stack.iter().enumerate().rev() {
            match s.1 {
                TeXMouthSource::String(ss) => {
                    match &ss.source {
                        Some(s) => (state.outputs().file_close)(memory.interner.resolve(*s).unwrap()),
                        None => ()
                    }
                    self.stack.remove(s.0);
                    return
                },
                _ => ()
            }
        }
    }

    fn file_line(&self,memory:&Memory<ET>) -> String {
        for s in self.stack.iter().rev() {
            match s {
                TeXMouthSource::String(ss) => {
                    match &ss.source {
                        Some(s) => return format!("{}:({},{})",memory.interner.resolve(*s).unwrap(),ss.line(),ss.column()),
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
    /*
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
            self.sources.push(TeXMouthSource::Token(next));
            None
        }
    }

     */
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
    fn preview(&self,memory:&Memory<ET>) -> String {
        let tks : Vec<Token<ET>> = self.0[self.1..].iter().map(|t| unsafe{t.clone().unwrap_unchecked()}).collect();
        TokenList(&tks).to_str(memory)
        //crate::interpreter::tokens_to_string_default(&tks)
    }
    pub fn reset(&mut self) {
        self.0.clear();
        self.1 = 0;
    }
    pub fn push(&mut self,t:Token<ET>) {
        self.0.push(Some(t))
    }
}