/*! A [`Mouth`] is the source of [`Token`]s to be processed by a TeX engine.

 The default implementations ([`NoTracingMouth`] and [`StandardMouth`]) are
 just wrappers around a [`Vec`]`<`[`TeXMouthSource`]`>`,
 which are either [`TokenSource`]s or [`StringSource`]s.

 [`TokenSource`]s are [`Token`]s that have already been processed, while [`StringSource`]s
 represent a string of characters to be tokenized; e.g. from a source file.
 */
pub mod string_source;
pub mod methods;

use log::debug;
use crate::engine::{EngineRef, EngineType, Outputs};
use crate::engine::filesystem::File;
use crate::engine::mouth::string_source::StringSource;
use crate::engine::state::State;
use crate::{debug_log, file_end, get_until_endgroup, throw};
use crate::engine::memory::{ExpansionContainer, Interner, Memory};
use crate::tex::catcodes::CategoryCode;
use crate::tex::token::{BaseToken, Token, TokenList};
use crate::utils::errors::TeXError;
use crate::utils::strings::CharType;

/// A [`Mouth`] is the source of [`Token`]s to be processed by a TeX engine.
pub trait Mouth<ET:EngineType<Mouth=Self>>:Sized {
    fn with_mouth<F:FnMut(&mut EngineRef<ET>) -> R,R>(engine:&mut EngineRef<ET>, tks:Vec<Token<ET>>, mut f:F) -> R {
        let old = std::mem::replace(engine.mouth, Self::new_with(tks,engine.memory));
        let ret = f(engine);
        *engine.mouth = old;
        ret
    }
    fn new(memory:&mut Memory<ET>) -> Self;
    fn new_with(tks:Vec<Token<ET>>,memory:&mut Memory<ET>) -> Self;

    fn get_expansion(engine:&mut EngineRef<ET>) -> ExpansionContainer<ET>;
    fn push_expansion(engine:&mut EngineRef<ET>,expansion:ExpansionContainer<ET>);
    /// Insert a [`Vec`] of [`Token`]s into the [`Mouth`], to be processed next
    fn add_expansion<F,R>(engine:&mut EngineRef<ET>, f:F) -> R where F:FnOnce(&mut EngineRef<ET>,&mut ExpansionContainer<ET>) -> R;
    /// Insert a [`File`] into the [`Mouth`], to be processed next
    fn push_file(&mut self, file: &ET::File,interner:&mut Interner<ET::Char>);

    /// Insert a single [`Token`] into the [`Mouth`], not to be expanded when processed
    fn push_noexpand(&mut self,tk:Token<ET>,memory:&mut Memory<ET>);

    /// Insert a single [`Token`] into the [`Mouth`], to be processed next
    /// (for implementations with a peek buffer)
    fn requeue(&mut self,tk:Token<ET>);

    /// Return the next [`Token`] from the [`Mouth`], and whether to expand it (due to `\noexpand`)
    //#[inline(always)]
    fn get_next(&mut self,state:&ET::State,interner:&mut Interner<ET::Char>,outputs:&mut Outputs) -> Result<Option<(Token<ET>,bool)>,TeXError<ET>>;

    fn get_next_simple(&mut self,state:&ET::State,interner:&mut Interner<ET::Char>) -> Result<Option<Token<ET>>,TeXError<ET>>;

    /// Return the next n characters from the [`Mouth`] as a [`String`], without consuming them
    /// (for error messages, debugging purposes, etc.)
    fn preview(&self,len:usize,interner:&Interner<ET::Char>) -> String;

    /// Return the current file and line number as presentable string
    fn file_line(&self,interner:&Interner<ET::Char>) -> String;

    fn line_no(&self) -> usize;

    fn endinput(&mut self, state: &mut ET::State,interner:&Interner<ET::Char>,outputs:&mut Outputs);

    /// Skip whitespace characters from the [`Mouth`]
    #[inline(always)]
    fn skip_whitespace(&mut self,state:&ET::State,interner:&mut Interner<ET::Char>) -> Result<(),TeXError<ET>> {
        debug_log!(trace=>"skipping whitespace");
        while let Some(tk) = self.get_next_simple(state,interner)? {
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
    fn get_argument(engine:&mut EngineRef<ET>, vec: &mut Vec<Token<ET>>) -> Result<(),TeXError<ET>> {
        match engine.mouth.get_next_simple(engine.state,engine.interner)? {
            None => file_end!(),
            Some(t) if t.catcode() == CategoryCode::BeginGroup => {
                get_until_endgroup!(engine,t => vec.push(t));
                Ok(())
                //Self::get_until_endgroup(engine,&mut|_,t| Ok(vec.push(t)))
            }
            Some(o) => {
                vec.push(o);
                Ok(())
            }
        }
    }
/*
    /// reads [`Token`]s from the [`Mouth`] until the next suitable [`EndGroup`](CategoryCode::EndGroup)
    /// [`Token`] is encountered, and returns them as a [`Vec`], respecting nested groups.
    fn get_until_endgroup(engine:&mut EngineRef<ET>, f:TokenCont<ET>) -> Result<(),TeXError<ET>> {
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
            f(engine,tk)?;
        }
        file_end!()
    }

 */

    /// like [`get_next`](`Mouth::get_next`), but throws an error on `\par` (and [`EOF`](crate::tex::catcodes::CategoryCode::EOF))
    #[inline(always)]
    fn get_next_nopar(&mut self,state:&ET::State,interner:&mut Interner<ET::Char>) -> Result<Option<Token<ET>>,TeXError<ET>> {
        match self.get_next_simple(state,interner)? {
            Some(t) => {
                match &t.base {
                    BaseToken::CS(name) if *name == interner.par =>
                        throw!("Paragraph ended while reading argument" => t),
                    BaseToken::Char(_,CategoryCode::EOF) =>
                        file_end!(),
                    _ => Ok(Some(t))
                }
            }
            o => Ok(o)
        }
    }

    /// Like [`read_argument`](`Mouth::read_argument`), but throws an error on `\par` (and [`EOF`](crate::tex::catcodes::CategoryCode::EOF))
    fn get_argument_nopar(engine:&mut EngineRef<ET>, v:&mut Vec<Token<ET>>) -> Result<(),TeXError<ET>> {
        match engine.mouth.get_next_simple(engine.state,engine.interner)? {
            None => file_end!(),
            Some(t) if t.catcode() == CategoryCode::BeginGroup => {
                let mut depth = 1;
                while let Some(tk) = engine.mouth.get_next_simple(engine.state,engine.interner)? {
                    match &tk.base {
                        BaseToken::Char(_,CategoryCode::BeginGroup) => depth += 1,
                        BaseToken::Char(_,CategoryCode::EndGroup) => {
                            depth -= 1;
                            if depth == 0 { return Ok(()) }
                        },
                        BaseToken::CS(n) if *n == engine.interner.par =>
                            throw!("Paragraph ended while reading argument" => t),
                        _ => ()
                    }
                    v.push(tk);
                }
                file_end!()
            }
            Some(t) if t.catcode() == CategoryCode::EOF => file_end!(),
            Some(o) => {
                v.push(o);
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

    fn get_expansion(engine: &mut EngineRef<ET>) -> ExpansionContainer<ET> {
        engine.memory.get_expansion_container()
    }
    fn push_expansion(engine: &mut EngineRef<ET>, expansion: ExpansionContainer<ET>) {
        expansion.consume::<_,()>(engine.memory,|s| {
            engine.mouth.stack.push(TeXMouthSource::Token(s))
        });
    }
    /// Insert a [`Vec`] of [`Token`]s into the [`Mouth`], to be processed next
    fn add_expansion<F,R>(engine:&mut EngineRef<ET>, f:F) -> R where F:FnOnce(&mut EngineRef<ET>,&mut ExpansionContainer<ET>) -> R {
        let mut source = engine.memory.get_expansion_container();
        let ret = {
            f(engine,&mut source)
        };
        source.consume::<_,()>(engine.memory,|s| {
            engine.mouth.stack.push(TeXMouthSource::Token(s))
        });
        ret
    }

    fn push_noexpand(&mut self, tk: Token<ET>,memory:&mut Memory<ET>) {
        self.stack.push(TeXMouthSource::Token((tk,false)))
    }

    fn push_file(&mut self, file: &ET::File,interner:&mut Interner<ET::Char>) {
        debug!("Pushing file {:?}", file.path());
        let source = TeXMouthSource::String(StringSource::new(
            (*file.content_string()).clone().unwrap(),
            Some(interner.from_string(file.path().to_str().unwrap()))
        ));
        self.stack.push(source);
    }

    fn requeue(&mut self, tk: Token<ET>) {
        self.stack.push(TeXMouthSource::Token((tk,true)))
    }

    fn get_next_simple(&mut self, state: &ET::State, interner: &mut Interner<ET::Char>) -> Result<Option<Token<ET>>, TeXError<ET>> {
        match self.stack.last_mut() {
            Some(TeXMouthSource::Token(ref mut tks)) => match self.stack.pop() {
                Some(TeXMouthSource::Token((t, b))) => Ok(Some(t)),
                _ => unreachable!()
            },
            Some(TeXMouthSource::String(ref mut s)) => {
                match s.get_next(interner,state.get_catcode_scheme(),state.get_endlinechar())? {
                    Some(t) => Ok(Some(t)),
                    None => throw!("File ended unexpectedly")
                }
            }
            None => Ok(None)
        }
    }


    fn get_next(&mut self, state: &ET::State,interner:&mut Interner<ET::Char>,outputs:&mut Outputs) -> Result<Option<(Token<ET>, bool)>, TeXError<ET>> {
        match self.stack.last_mut() {
            Some(TeXMouthSource::Token(ref mut tks)) => match self.stack.pop() {
                Some(TeXMouthSource::Token((t, b))) => Ok(Some((t, b))),
                _ => unreachable!()
            },
            Some(TeXMouthSource::String(ref mut s)) => {
                match s.get_next(interner,state.get_catcode_scheme(), state.get_endlinechar())? {
                    Some(t) => return Ok(Some((t, true))),
                    None => {
                        match &s.source {
                            None => (),
                            Some(s) => (outputs.file_close)(interner.resolve(s.symbol()))
                        }
                        self.stack.pop();
                        debug_log!(debug => "file end; inserting \\everyeof");
                        let eof = (Token::new(BaseToken::Char(ET::Char::from(b'\n'), CategoryCode::EOF), None),true);
                        match state.get_primitive_toks("everyeof") {
                            None => Ok(Some(eof)),
                            Some(v) if v.is_empty() => Ok(Some(eof)),
                            Some(v) => {
                                self.stack.push(TeXMouthSource::Token(eof));
                                for t in v.iter().rev() { self.stack.push(TeXMouthSource::Token((t.clone(),true))) };
                                match self.stack.pop() {
                                    Some(TeXMouthSource::Token((t, b))) => Ok(Some((t, b))),
                                    _ => unreachable!()
                                }
                            }
                        }
                    }
                }
            },
            None => Ok(None)
        }
    }

    fn preview(&self,len:usize,interner:&Interner<ET::Char>) -> String { // TODO memory
        let mut ret = String::new();
        for s in self.stack.iter().rev() {
            ret.push_str(&match s {
                TeXMouthSource::Token(ts) => ts.0.base.to_str(interner,Some(ET::Char::backslash())),
                TeXMouthSource::String(ss) => ss.preview()
            });
            if ret.len() > len { ret.truncate(len);return ret }
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

    fn endinput(&mut self, state: &mut ET::State,interner:&Interner<ET::Char>,outputs:&mut Outputs) {
        for s in self.stack.iter().enumerate().rev() {
            match s.1 {
                TeXMouthSource::String(ss) => {
                    match &ss.source {
                        Some(s) => (outputs.file_close)(interner.resolve(s.symbol())),
                        None => ()
                    }
                    self.stack.remove(s.0);
                    return
                },
                _ => ()
            }
        }
    }

    fn file_line(&self,interner:&Interner<ET::Char>) -> String {
        for s in self.stack.iter().rev() {
            match s {
                TeXMouthSource::String(ss) => {
                    match &ss.source {
                        Some(s) => return format!("{}:({},{})",interner.resolve(s.symbol()),ss.line(),ss.column()),
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
        let ret = unsafe { std::mem::take(&mut self.0[self.1]).unwrap_unchecked() };
        self.1 += 1;
        (ret,self.is_empty())
    }
    fn preview(&self,interner:&Interner<ET::Char>) -> String {
        let tks : Vec<Token<ET>> = self.0[self.1..].iter().map(|t| unsafe{t.clone().unwrap_unchecked()}).collect();
        TokenList(&tks).to_str(interner)
    }
    pub fn reset(&mut self) {
        self.0.clear();
        self.1 = 0;
    }
    pub fn push(&mut self,t:Token<ET>) {
        self.0.push(Some(t))
    }
}