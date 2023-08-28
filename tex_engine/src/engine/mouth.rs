/*! A [`MouthTrait`] is the source of [`Token`]s to be processed by a TeX engine.

 The default implementations ([`NoTracingMouth`] and [`Mouth`]) are
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
use crate::{debug_log, file_end, throw};
use crate::engine::memory::{Interner, Memory, VEC_SIZE};
use crate::tex::catcodes::CategoryCode;
use crate::tex::commands::{DefReplacement, TokenCont};
use crate::tex::token::{BaseToken, Token, TokenList};
use crate::utils::errors::TeXError;
use crate::utils::Ptr;
use crate::utils::strings::CharType;

/// A [`MouthTrait`] is the source of [`Token`]s to be processed by a TeX engine.
pub trait MouthTrait<ET:EngineType>:Sized {
    fn with_mouth<F:FnMut(&mut EngineRef<ET>) -> R,R>(engine:&mut EngineRef<ET>, tks:Vec<Token<ET>>, mut f:F) -> R {
        let old = std::mem::replace(engine.mouth, Self::new_with(tks,engine.memory));
        let ret = f(engine);
        *engine.mouth = old;
        ret
    }
    fn new(memory:&mut Memory<ET>) -> Self;
    fn new_with(tks:Vec<Token<ET>>,memory:&mut Memory<ET>) -> Mouth<ET>;

    fn get_expansion(engine:&mut EngineRef<ET>) -> ExpansionContainer<ET>;
    fn push_expansion(engine:&mut EngineRef<ET>,expansion:ExpansionContainer<ET>);
    /// Insert a [`Vec`] of [`Token`]s into the [`MouthTrait`], to be processed next
    fn add_expansion<F,R>(engine:&mut EngineRef<ET>, f:F) -> R where F:FnOnce(&mut EngineRef<ET>,&mut ExpansionContainer<ET>) -> R;
    fn add_expansion_rev<F,R>(engine:&mut EngineRef<ET>, f:F) -> R where F:FnOnce(&mut EngineRef<ET>,&mut ExpansionContainer<ET>) -> R;
    /// Insert a [`File`] into the [`MouthTrait`], to be processed next
    fn push_file(&mut self, file: &ET::File,interner:&mut Interner<ET::Char>);
    fn push_string(&mut self,str:&Vec<u8>);

    /// Insert a single [`Token`] into the [`MouthTrait`], not to be expanded when processed
    fn push_noexpand(&mut self,tk:Token<ET>,memory:&mut Memory<ET>);

    /// Insert a single [`Token`] into the [`MouthTrait`], to be processed next
    /// (for implementations with a peek buffer)
    fn requeue(&mut self,tk:Token<ET>);

    /// Return the next [`Token`] from the [`MouthTrait`], and whether to expand it (due to `\noexpand`)
    //#[inline(always)]
    fn get_next(&mut self,state:&ET::State,interner:&mut Interner<ET::Char>,outputs:&mut Outputs) -> Option<(Token<ET>,bool)>;

    fn get_next_simple(&mut self,state:&ET::State,interner:&mut Interner<ET::Char>) -> Option<Token<ET>>;

    /// Return the next n characters from the [`MouthTrait`] as a [`String`], without consuming them
    /// (for error messages, debugging purposes, etc.)
    fn preview(&self,len:usize,interner:&Interner<ET::Char>) -> String;

    /// Return the current file and line number as presentable string
    fn file_line(&self,interner:&Interner<ET::Char>) -> String;

    fn line_no(&self) -> usize;

    fn endinput(&mut self, state: &mut ET::State,interner:&Interner<ET::Char>,outputs:&mut Outputs);

    /// like [`get_next`](`Mouth::get_next`), but throws an error on `\par` (and [`EOF`](crate::tex::catcodes::CategoryCode::EOF))
    fn get_next_nopar(&mut self,state:&ET::State,interner:&mut Interner<ET::Char>) -> Option<Token<ET>> {
        match self.get_next_simple(state,interner) {
            Some(t) => {
                match &t.base {
                    BaseToken::CS(name) if *name == interner.par =>
                        throw!("Paragraph ended while reading argument" => t),
                    BaseToken::Char(_,CategoryCode::EOF) =>
                        file_end!(),
                    _ => Some(t)
                }
            }
            o => o
        }
    }

}


/// Either a [`TokenSource`] or a [`StringSource`]
#[derive(Clone)]
pub enum TeXMouthSource<ET:EngineType> {
    Tkls(Vec<Token<ET>>),
    Noexpand(Token<ET>),
    String(StringSource<ET::Char>)
}

#[derive(Clone)]
pub struct Mouth<ET:EngineType>{ pub stack:Vec<TeXMouthSource<ET>>,buffer:Vec<Vec<Token<ET>>> }

impl<ET:EngineType> MouthTrait<ET> for Mouth<ET> {
    fn new(memory:&mut Memory<ET>) -> Self {
        Mouth { stack:Vec::with_capacity(2097152),buffer:vec!()}
    }
    fn new_with(mut tks: Vec<Token<ET>>,memory:&mut Memory<ET>) -> Self {
        tks.reverse();
        Mouth {stack:vec!(TeXMouthSource::Tkls(tks)),buffer:vec!()}
    }

    fn get_expansion(engine: &mut EngineRef<ET>) -> ExpansionContainer<ET> {
        ExpansionContainer{array:engine.mouth.get_vec()}
    }
    fn push_expansion(engine: &mut EngineRef<ET>, expansion: ExpansionContainer<ET>) {
        let mut v = expansion.destroy();
        if v.is_empty() { engine.mouth.buffer.push(v) } else {
            v.reverse();
            engine.mouth.stack.push(TeXMouthSource::Tkls(v))
        }
    }
    /// Insert a [`Vec`] of [`Token`]s into the [`MouthTrait`], to be processed next
    fn add_expansion<F,R>(engine:&mut EngineRef<ET>, f:F) -> R where F:FnOnce(&mut EngineRef<ET>,&mut ExpansionContainer<ET>) -> R {
        let mut expansion = ExpansionContainer{array:engine.mouth.get_vec()};
        let ret = {
            f(engine,&mut expansion)
        };
        let mut v = expansion.destroy();
        if v.is_empty() { engine.mouth.buffer.push(v) } else {
            v.reverse();
            engine.mouth.stack.push(TeXMouthSource::Tkls(v))
        }
        ret
    }

    fn add_expansion_rev<F,R>(engine:&mut EngineRef<ET>, f:F) -> R where F:FnOnce(&mut EngineRef<ET>,&mut ExpansionContainer<ET>) -> R {
        let mut expansion = ExpansionContainer{array:engine.mouth.get_vec()};
        let ret = {
            f(engine,&mut expansion)
        };
        let mut v = expansion.destroy();
        if v.is_empty() { engine.mouth.buffer.push(v) } else {
            engine.mouth.stack.push(TeXMouthSource::Tkls(v))
        }
        ret
    }

    fn push_noexpand(&mut self, tk: Token<ET>,memory:&mut Memory<ET>) {
        self.stack.push(TeXMouthSource::Noexpand(tk))
    }

    fn push_file(&mut self, file: &ET::File,interner:&mut Interner<ET::Char>) {
        debug!("Pushing file {:?}", file.path());
        let source = TeXMouthSource::String(StringSource::new(
            file.content_string().unwrap().clone(),
            Some(interner.from_string(file.path().to_str().unwrap()))
        ));
        self.stack.push(source);
    }

    fn push_string(&mut self, str: &Vec<u8>) {
        let source = TeXMouthSource::String(StringSource::new(StringSource::<ET::Char>::from_str(str),None));
        self.stack.push(source);
    }

    fn requeue(&mut self, tk: Token<ET>) {
        match self.stack.last_mut() {
            Some(TeXMouthSource::Tkls(v)) => v.push(tk),
            _ => {
                let mut v = self.get_vec();
                v.push(tk);
                self.stack.push(TeXMouthSource::Tkls(v))
            }
        }
    }

    fn get_next_simple(&mut self, state: &ET::State, interner: &mut Interner<ET::Char>) -> Option<Token<ET>> {
        match self.stack.last_mut() {
            Some(TeXMouthSource::Noexpand(tk)) => match self.stack.pop() {
                Some(TeXMouthSource::Noexpand(t)) => Some(t),
                _ => unreachable!()
            },
            Some(TeXMouthSource::Tkls(ref mut v)) => {
                let ret = v.pop().unwrap();
                if v.is_empty() {
                    match self.stack.pop() {
                        Some(TeXMouthSource::Tkls(v)) => {
                            self.buffer.push(v);
                        }
                        _ => unreachable!()
                    }
                }
                Some(ret)
            }
            Some(TeXMouthSource::String(ref mut s)) => {
                match s.get_next(interner,state.get_catcode_scheme(),state.get_endlinechar()) {
                    Some(t) => Some(t),
                    None => panic!("File ended unexpectedly")
                }
            }
            None => None
        }
    }


    fn get_next(&mut self, state: &ET::State,interner:&mut Interner<ET::Char>,outputs:&mut Outputs) -> Option<(Token<ET>, bool)> {
        match self.stack.last_mut() {
            Some(TeXMouthSource::Noexpand(tk)) => match self.stack.pop() {
                Some(TeXMouthSource::Noexpand(t)) => Some((t, false)),
                _ => unreachable!()
            },
            Some(TeXMouthSource::Tkls(v)) => {
                let ret = (v.pop().unwrap(), true);
                if v.is_empty() {
                    match self.stack.pop() {
                        Some(TeXMouthSource::Tkls(v)) => {
                            self.buffer.push(v);
                        }
                        _ => unreachable!()
                    }
                }
                Some(ret)
            }
            Some(TeXMouthSource::String(ref mut s)) => {
                match s.get_next(interner,state.get_catcode_scheme(), state.get_endlinechar()) {
                    Some(t) => return Some((t, true)),
                    None => {
                        match &s.source {
                            None => (),
                            Some(s) => (outputs.file_close)(interner.resolve(s.symbol()))
                        }
                        self.stack.pop();
                        debug_log!(debug => "file end; inserting \\everyeof");
                        let eof = Token::new(BaseToken::Char(ET::Char::from(b'\n'), CategoryCode::EOF), None);
                        let everyeof = state.get_primitive_toks("everyeof");
                        debug_log!(debug => "everyeof: {:?}",match everyeof {
                            None => "None".to_string(),
                            Some(v) => TokenList(v).to_str(interner)
                        });
                        match everyeof {
                            None => Some((eof,true)),
                            Some(v) if v.is_empty() => Some((eof,true)),
                            Some(v) => {
                                let mut nv = self.get_vec();
                                nv.push(eof);
                                for t in v.iter().skip(1).rev() { nv.push(t.clone()) };
                                self.stack.push(TeXMouthSource::Tkls(nv));
                                Some((v.first().unwrap().clone(),true))
                            }
                        }
                    }
                }
            },
            None => None
        }
    }

    fn preview(&self,len:usize,interner:&Interner<ET::Char>) -> String { // TODO memory
        let mut ret = String::new();
        for s in self.stack.iter().rev() {
            ret.push_str(&match s {
                TeXMouthSource::Noexpand(ts) => ts.base.to_str(interner,Some(ET::Char::backslash())),
                TeXMouthSource::Tkls(v) => v.iter().rev().map(|t| t.base.to_str(interner,Some(ET::Char::backslash()))).collect::<String>(),
                TeXMouthSource::String(ss) => ss.preview(len - ret.len())
            });
            if ret.len() > len { /*ret.truncate(len);*/return ret.replace("\r","\\r").replace("\n","\\n") }
        }
        ret.replace("\r","\\r").replace("\n","\\n")
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

macro_rules! get_while {
    ($mouth:expr,$state:expr,$interner:expr,$label:tt => $t:ident => $f:expr) => {
        $label:loop {
            match $mouth.stack.last_mut() {
                Some(TeXMouthSource::Noexpand(_)) => match $mouth.stack.pop() {
                    Some(TeXMouthSource::Noexpand($t)) => $f,
                    _ => unreachable!()
                },
                Some(TeXMouthSource::Tkls(ref mut v)) => loop {
                    let $t = v.pop().unwrap();
                    if v.is_empty() {
                        match $mouth.stack.pop() {
                            Some(TeXMouthSource::Tkls(v)) => {
                                $mouth.buffer.push(v);
                            }
                            _ => unreachable!()
                        }
                        $f;break
                    }
                    $f;
                }
                Some(TeXMouthSource::String(ref mut s)) => {
                    let cc = $state.get_catcode_scheme();
                    let endline = $state.get_endlinechar();
                    while let Some($t) = s.get_next($interner,cc,endline) {
                        $f;
                    }
                    panic!("File ended unexpectedly")
                }
                None => panic!("File ended unexpectedly")
            }
        }
    }
}

impl<ET:EngineType> Mouth<ET> {
    fn get_vec(&mut self) -> Vec<Token<ET>> {
        self.buffer.pop().unwrap_or(Vec::with_capacity(VEC_SIZE))
    }


    /// Skip whitespace characters from the [`MouthTrait`]
    pub fn skip_whitespace(&mut self,state:&ET::State,interner:&mut Interner<ET::Char>) {
        debug_log!(trace=>"skipping whitespace");
        get_while!(self,state,interner,'A => tk => {
            match tk.catcode() {
                CategoryCode::Space => (),
                _ => {
                    self.requeue(tk);break 'A
                }
            }
        });
    }

    /// Like [`read_argument`](`Mouth::read_argument`), but throws an error on `\par` (and [`EOF`](crate::tex::catcodes::CategoryCode::EOF))
    pub fn get_argument_nopar(engine:&mut EngineRef<ET>, v:&mut Vec<Token<ET>>) {
        match engine.mouth.get_next_simple(engine.state,engine.interner) {
            None => file_end!(),
            Some(t) if t.catcode() == CategoryCode::BeginGroup => {
                let mut depth = 1;
                let par = engine.interner.par;
                get_while!(engine.mouth,engine.state,engine.interner,'A => tk => {
                    match tk.base {
                        BaseToken::Char(_,CategoryCode::BeginGroup) => depth += 1,
                        BaseToken::Char(_,CategoryCode::EndGroup) => {
                            depth -= 1;
                            if depth == 0 { break 'A }
                        },
                        BaseToken::CS(n) if n == par =>
                            throw!("Paragraph ended while reading argument" => t),
                        _ => ()
                    }
                    v.push(tk);
                });
            }
            Some(o) => {
                v.push(o);
            }
        }
    }

    /// reads a macro argument from the [`MouthTrait`], i.e. a sequence of [`Token`]s enclosed in
    /// braces (category codes [`BeginGroup`](CategoryCode::BeginGroup) and
    /// [`EndGroup`](CategoryCode::EndGroup)), or a single non-space [`Token`] if the argument is
    /// not enclosed.
    fn get_argument(engine:&mut EngineRef<ET>, vec: &mut Vec<Token<ET>>) {
        match engine.mouth.get_next_simple(engine.state,engine.interner) {
            None => file_end!(),
            Some(t) if t.catcode() == CategoryCode::BeginGroup => {
                let mut ingroup = 1;
                get_while!(engine.mouth,engine.state,engine.interner,'A => tk => {
                    match tk.base {
                        BaseToken::Char(_,CategoryCode::BeginGroup) => ingroup += 1,
                        BaseToken::Char(_,CategoryCode::EndGroup) => {
                            ingroup -= 1;
                            if ingroup == 0 { break 'A }
                        },
                        _ => ()
                    }
                    vec.push(tk);
                })
                //Self::get_until_endgroup(engine,&mut|_,t| Ok(vec.push(t)))
            }
            Some(o) => {
                vec.push(o);
            }
        }
    }
/*
    fn expand_until_group(engine:&mut EngineRef<ET>, f:TokenCont<ET>) {
        match engine.get_next_unexpandable_same_file() {
            None => file_end!(),
            Some(res) if res.source.cause.catcode() == CategoryCode::BeginGroup => (),
            Some(res) => throw!("begin group expected; found:{}",res.source.cause.to_str(engine.interner,Some(ET::Char::backslash())))
        }
        let mut ingroup = 1;
        get_while!(engine.mouth,engine.state,engine.interner,'A => tk => {
            match tk.base {
                BaseToken::Char(_,CategoryCode::BeginGroup) => ingroup += 1,
                BaseToken::Char(_,CategoryCode::EndGroup) => {
                    ingroup -= 1;
                    if ingroup == 0 { break 'A }
                },
                _ => ()
            }
            f(engine,tk);
        })
    }

 */
}

pub struct ExpansionContainer<ET:EngineType>{array:Vec<Token<ET>>}
impl<ET:EngineType> ExpansionContainer<ET> {
    pub fn push(&mut self, t:Token<ET>,memory:&mut Memory<ET>) {
        self.array.push(t);
    }
    pub fn reset(&mut self,memory:&mut Memory<ET>) {
        self.array.clear();
    }

    pub fn destroy(self) -> Vec<Token<ET>> {
        self.array
    }

}

/* ------------- with trait objects: --------------------------------------------------------------

pub struct Mouth<ET:EngineType>{ buffer:Vec<Vec<Token<ET>>>, stack:Vec<Box<dyn TokenSourceT<ET>>> }

impl<ET:EngineType> MouthTrait<ET> for Mouth<ET> {
    fn new(memory:&mut Memory<ET>) -> Self {
        Mouth { buffer:vec!(), stack:Vec::with_capacity(1024)}
    }
    fn new_with(mut tks: Vec<Token<ET>>,memory:&mut Memory<ET>) -> Self {
        tks.reverse();
        Mouth {buffer:vec!(), stack:vec!(Box::new(Pretokenized(tks)))}
    }

    fn get_expansion(engine: &mut EngineRef<ET>) -> ExpansionContainer<ET> {
        ExpansionContainer{array:engine.mouth.get_vec()}
    }
    fn push_expansion(engine: &mut EngineRef<ET>, expansion: ExpansionContainer<ET>) {
        let mut v = expansion.destroy();
        if v.is_empty() { engine.mouth.buffer.push(v) } else {
            v.reverse();
            engine.mouth.stack.push(Box::new(Pretokenized(v)))
        }
    }
    /// Insert a [`Vec`] of [`Token`]s into the [`MouthTrait`], to be processed next
    fn add_expansion<F,R>(engine:&mut EngineRef<ET>, f:F) -> R where F:FnOnce(&mut EngineRef<ET>,&mut ExpansionContainer<ET>) -> R {
        let mut expansion = ExpansionContainer{array:engine.mouth.get_vec()};
        let ret = {
            f(engine,&mut expansion)
        };
        let mut v = expansion.destroy();
        if v.is_empty() { engine.mouth.buffer.push(v) } else {
            v.reverse();
            engine.mouth.stack.push(Box::new(Pretokenized(v)))
        }
        ret
    }

    fn add_expansion_rev<F,R>(engine:&mut EngineRef<ET>, f:F) -> R where F:FnOnce(&mut EngineRef<ET>,&mut ExpansionContainer<ET>) -> R {
        let mut expansion = ExpansionContainer{array:engine.mouth.get_vec()};
        let ret = {
            f(engine,&mut expansion)
        };
        let mut v = expansion.destroy();
        if v.is_empty() { engine.mouth.buffer.push(v) } else {
            engine.mouth.stack.push(Box::new(Pretokenized(v)))
        }
        ret
    }

    fn push_noexpand(&mut self, tk: Token<ET>,memory:&mut Memory<ET>) {
        self.stack.push(Box::new(Requeued(Some((tk,false)))))
    }

    fn push_file(&mut self, file: &ET::File,interner:&mut Interner<ET::Char>) {
        debug!("Pushing file {:?}", file.path());
        let source = StringSource::new(
            (*file.content_string()).clone().unwrap(),
            Some(interner.from_string(file.path().to_str().unwrap()))
        );
        self.stack.push(Box::new(StringMouth(source,false)));
    }

    fn push_string(&mut self, str: Vec<u8>) {
        let source = StringSource::new(str,None);
        self.stack.push(Box::new(StringMouth(source,false)));
    }

    fn requeue(&mut self, tk: Token<ET>) {
        self.stack.push(Box::new(Requeued(Some((tk,true)))))
    }

    fn get_next_simple(&mut self, state: &ET::State, interner: &mut Interner<ET::Char>) -> Option<Token<ET>> {
        match self.stack.last_mut() {
            Some(bx) => {
                match bx.get_next_simple(state,interner) {
                    Some(t) => {
                        if bx.is_empty() {
                            let r = self.stack.pop().unwrap();
                            r.destroy_safe(self);
                        }
                        Some(t)
                    }
                    None => {
                        let r = self.stack.pop().unwrap();
                        r.destroy_safe(self);
                        self.get_next_simple(state,interner)
                    }
                }
            }
            None => None
        }
    }

    fn get_next(&mut self, state: &ET::State,interner:&mut Interner<ET::Char>,outputs:&mut Outputs) -> Option<(Token<ET>, bool)> {
        match self.stack.last_mut() {
            Some(bx) => {
                match bx.get_next(state,interner) {
                    Some((t,expand)) => {
                        if bx.is_empty() {
                            let r = self.stack.pop().unwrap();
                            r.destroy(state,interner,self,outputs);
                        }
                        Some((t,expand))
                    }
                    None => {
                        let r = self.stack.pop().unwrap();
                        r.destroy(state,interner,self,outputs);
                        self.get_next(state,interner,outputs)
                    }
                }
            }
            None => None
        }
    }

    fn preview(&self,len:usize,interner:&Interner<ET::Char>) -> String { // TODO memory
        let mut ret = String::new();
        for s in self.stack.iter().rev() {
            ret.push_str(&s.preview(interner));
            if ret.len() > len { ret = ret.chars().take(len).collect();return ret.replace("\r","\\r").replace("\n","\\n") }
        }
        ret.replace("\r","\\r").replace("\n","\\n")
    }

    fn line_no(&self) -> usize {
        for s in self.stack.iter().rev() {
            match s.line_no() {
                Some(l) => return l,
                _ => ()
            }
        }
        0
    }

    fn endinput(&mut self, state: &mut ET::State,interner:&Interner<ET::Char>,outputs:&mut Outputs) {
        for s in self.stack.iter().enumerate().rev() {
            if s.1.is_file() {
                self.stack.remove(s.0);
                return
            }
        }
    }

    fn file_line(&self,interner:&Interner<ET::Char>) -> String {
        for s in self.stack.iter().rev() {
            match s.file_line(interner) {
                Some(s) => return s,
                _ => ()
            }
        }
        "unknown source".to_string()
    }
}

macro_rules! get_while {
    ($mouth:expr,$state:expr,$interner:expr,$label:tt => $t:ident => $f:expr) => {
        $label:loop {
            match $mouth.stack.last_mut() {
                Some(bx) => loop {
                    match bx.get_next_simple($state,$interner) {
                        Some($t) => {
                            if bx.is_empty() {
                                let r = $mouth.stack.pop().unwrap();
                                break {
                                    r.destroy_safe($mouth);
                                    $f
                                }
                            }
                            $f
                        }
                        None => {
                            let r = $mouth.stack.pop().unwrap();
                            r.destroy_safe($mouth);
                            break
                        }
                    }
                }
                None => panic!("File ended unexpectedly")
            }
        }
    }
}

impl<ET:EngineType> Mouth<ET> {
    fn get_vec(&mut self) -> Vec<Token<ET>> {
        self.buffer.pop().unwrap_or(Vec::with_capacity(VEC_SIZE))
    }


    /// Skip whitespace characters from the [`MouthTrait`]
    pub fn skip_whitespace(&mut self,state:&ET::State,interner:&mut Interner<ET::Char>) {
        debug_log!(trace=>"skipping whitespace");
        get_while!(self,state,interner,'A => tk => {
            match tk.catcode() {
                CategoryCode::Space => (),
                _ => {
                    self.requeue(tk);break 'A
                }
            }
        });
    }

    /// Like [`read_argument`](`Mouth::read_argument`), but throws an error on `\par` (and [`EOF`](crate::tex::catcodes::CategoryCode::EOF))
    pub fn get_argument_nopar(engine:&mut EngineRef<ET>, v:&mut Vec<Token<ET>>) {
        match engine.mouth.get_next_simple(engine.state,engine.interner) {
            None => file_end!(),
            Some(t) if t.catcode() == CategoryCode::BeginGroup => {
                let mut depth = 1;
                let par = engine.interner.par;
                get_while!(engine.mouth,engine.state,engine.interner,'A => tk => {
                    match tk.base {
                        BaseToken::Char(_,CategoryCode::BeginGroup) => depth += 1,
                        BaseToken::Char(_,CategoryCode::EndGroup) => {
                            depth -= 1;
                            if depth == 0 { break 'A }
                        },
                        BaseToken::CS(n) if n == par =>
                            throw!("Paragraph ended while reading argument" => t),
                        _ => ()
                    }
                    v.push(tk);
                });
            }
            Some(o) => {
                v.push(o);
            }
        }
    }

    /// reads a macro argument from the [`MouthTrait`], i.e. a sequence of [`Token`]s enclosed in
    /// braces (category codes [`BeginGroup`](CategoryCode::BeginGroup) and
    /// [`EndGroup`](CategoryCode::EndGroup)), or a single non-space [`Token`] if the argument is
    /// not enclosed.
    fn get_argument(engine:&mut EngineRef<ET>, vec: &mut Vec<Token<ET>>) {
        match engine.mouth.get_next_simple(engine.state,engine.interner) {
            None => file_end!(),
            Some(t) if t.catcode() == CategoryCode::BeginGroup => {
                let mut ingroup = 1;
                get_while!(engine.mouth,engine.state,engine.interner,'A => tk => {
                    match tk.base {
                        BaseToken::Char(_,CategoryCode::BeginGroup) => ingroup += 1,
                        BaseToken::Char(_,CategoryCode::EndGroup) => {
                            ingroup -= 1;
                            if ingroup == 0 { break 'A }
                        },
                        _ => ()
                    }
                    vec.push(tk);
                })
                //Self::get_until_endgroup(engine,&mut|_,t| Ok(vec.push(t)))
            }
            Some(o) => {
                vec.push(o);
            }
        }
    }
/*
    fn expand_until_group(engine:&mut EngineRef<ET>, f:TokenCont<ET>) {
        match engine.get_next_unexpandable_same_file() {
            None => file_end!(),
            Some(res) if res.source.cause.catcode() == CategoryCode::BeginGroup => (),
            Some(res) => throw!("begin group expected; found:{}",res.source.cause.to_str(engine.interner,Some(ET::Char::backslash())))
        }
        let mut ingroup = 1;
        get_while!(engine.mouth,engine.state,engine.interner,'A => tk => {
            match tk.base {
                BaseToken::Char(_,CategoryCode::BeginGroup) => ingroup += 1,
                BaseToken::Char(_,CategoryCode::EndGroup) => {
                    ingroup -= 1;
                    if ingroup == 0 { break 'A }
                },
                _ => ()
            }
            f(engine,tk);
        })
    }

 */
}

pub struct ExpansionContainer<ET:EngineType>{array:Vec<Token<ET>>}
impl<ET:EngineType> ExpansionContainer<ET> {
    pub fn push(&mut self, t:Token<ET>,memory:&mut Memory<ET>) {
        self.array.push(t);
    }
    pub fn reset(&mut self,memory:&mut Memory<ET>) {
        self.array.clear();
    }

    pub fn destroy(self) -> Vec<Token<ET>> {
        self.array
    }

}


pub trait TokenSourceT<ET:EngineType> {
   /* Tkls(Vec<Token<ET>>),
    Noexpand(Token<ET>),
    String(StringSource<ET::Char>)*/

    fn get_next(&mut self, state: &ET::State,interner:&mut Interner<ET::Char>) -> Option<(Token<ET>, bool)> {
        match self.get_next_simple(state,interner) {
            Some(t) => Some((t,true)),
            _ => None
        }
    }
    fn get_next_simple(&mut self, state: &ET::State, interner: &mut Interner<ET::Char>) -> Option<Token<ET>>;
    fn destroy(self:Box<Self>, state:&ET::State, interner: &mut Interner<ET::Char>, mouth: &mut Mouth<ET>, outputs:&mut Outputs) {
        self.destroy_safe(mouth)
    }
    fn destroy_safe(self:Box<Self>, mouth: &mut Mouth<ET>) {}
    fn is_empty(&self) -> bool;
    fn preview(&self,interner:&Interner<ET::Char>) -> String { todo!() }
    fn line_no(&self) -> Option<usize> {None}
    fn is_file(&self) -> bool {false}
    fn file_line(&self,interner:&Interner<ET::Char>) -> Option<String> { None }
}

pub struct Requeued<ET:EngineType>(Option<(Token<ET>,bool)>);
impl<ET:EngineType> TokenSourceT<ET> for Requeued<ET> {
    fn get_next(&mut self, state: &ET::State, interner: &mut Interner<ET::Char>) -> Option<(Token<ET>, bool)> {
        match std::mem::take(&mut self.0) {
            Some(t) => Some(t),
            _ => None
        }
    }
    fn get_next_simple(&mut self, state: &ET::State, interner: &mut Interner<ET::Char>) -> Option<Token<ET>> {
        std::mem::take(&mut self.0).map(|p| p.0)
    }
    fn is_empty(&self) -> bool { self.0.is_none() }
}

pub struct Pretokenized<ET:EngineType>(pub Vec<Token<ET>>);
impl<ET:EngineType> TokenSourceT<ET> for Pretokenized<ET> {
    fn get_next_simple(&mut self, state: &ET::State, interner: &mut Interner<ET::Char>) -> Option<Token<ET>> {
        self.0.pop()
    }
    fn is_empty(&self) -> bool { self.0.is_empty() }
    fn destroy_safe(self: Box<Self>, mouth: &mut Mouth<ET>) {
        mouth.buffer.push(self.0)
    }
}
pub struct StringMouth<ET:EngineType>(StringSource<ET::Char>,bool);
impl<ET:EngineType> TokenSourceT<ET> for StringMouth<ET> {
    fn get_next(&mut self, state: &ET::State, interner: &mut Interner<ET::Char>) -> Option<(Token<ET>, bool)> {
        match self.0.get_next(interner,state.get_catcode_scheme(),state.get_endlinechar()) {
            Some(t) => Some((t,true)),
            None => {
                self.1 = true;
                None
            }
        }
    }
    fn get_next_simple(&mut self, state: &ET::State, interner: &mut Interner<ET::Char>) -> Option<Token<ET>> {
        match self.0.get_next(interner,state.get_catcode_scheme(),state.get_endlinechar()) {
            Some(t) => Some(t),
            None => panic!("File ended unexpectedly")
        }
    }
    fn is_empty(&self) -> bool { self.1 }
    fn destroy_safe(self: Box<Self>, mouth: &mut Mouth<ET>) {
        panic!("File ended unexpectedly")
    }
    fn destroy(self: Box<Self>, state: &ET::State, interner: &mut Interner<ET::Char>, mouth: &mut Mouth<ET>, outputs: &mut Outputs) {
        debug_log!(debug => "file end; inserting \\everyeof");
        match (&*self).0.source {
            None => (),
            Some(s) => (outputs.file_close)(interner.resolve(s.symbol()))
        }
        let eof = Token::new(BaseToken::Char(ET::Char::from(b'\n'), CategoryCode::EOF), None);
        let mut nv = mouth.get_vec();
        nv.push(eof);
        match state.get_primitive_toks("everyeof") {
            None => (),
            Some(v) if v.is_empty() => (),
            Some(v) => {
                for t in v.iter().skip(1).rev() { nv.push(t.clone()) };
            }
        }
        mouth.stack.push(Box::new(Pretokenized(nv)));
    }
    fn line_no(&self) -> Option<usize> {
        Some(self.0.line())
    }
    fn is_file(&self) -> bool { true }
    fn file_line(&self, interner: &Interner<ET::Char>) -> Option<String> {
        match self.0.source {
            None => None,
            Some(s) => Some(format!("{}:({},{})", interner.resolve(s.symbol()), self.0.line(), self.0.column()))
        }
    }
}
 */