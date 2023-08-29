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
use crate::tex::commands::{DefReplacement, ExpToken, TokenCont};
use crate::tex::token::{BaseToken, Token, TokenList};
use crate::utils::errors::TeXError;
use crate::utils::Ptr;
use crate::utils::strings::CharType;

/// A [`MouthTrait`] is the source of [`Token`]s to be processed by a TeX engine.
pub trait MouthTrait<ET:EngineType>:Sized {
    fn new(memory:&mut Memory<ET>) -> Self;
    fn new_with(tks:Vec<Token<ET>>,memory:&mut Memory<ET>) -> Mouth<ET>;

    /// Insert a [`File`] into the [`MouthTrait`], to be processed next
    fn push_file(&mut self, file: &ET::File,interner:&mut Interner);
    fn push_string(&mut self,str:&Vec<u8>);

    /// Insert a single [`Token`] into the [`MouthTrait`], not to be expanded when processed
    fn push_noexpand(&mut self,tk:Token<ET>,memory:&mut Memory<ET>);

    /// Insert a single [`Token`] into the [`MouthTrait`], to be processed next
    /// (for implementations with a peek buffer)
    fn requeue(&mut self,tk:Token<ET>);

    /// Return the next [`Token`] from the [`MouthTrait`], and whether to expand it (due to `\noexpand`)
    //#[inline(always)]
    fn get_next(&mut self,state:&ET::State,interner:&mut Interner,outputs:&mut Outputs) -> Option<(Token<ET>,bool)>;

    fn get_next_simple(&mut self,state:&ET::State,interner:&mut Interner) -> Option<Token<ET>>;

    /// Return the next n characters from the [`MouthTrait`] as a [`String`], without consuming them
    /// (for error messages, debugging purposes, etc.)
    fn preview(&self,len:usize,interner:&Interner) -> String;

    /// Return the current file and line number as presentable string
    fn file_line(&self,interner:&Interner) -> String;

    fn line_no(&self) -> usize;

    fn endinput(&mut self, interner:&Interner,outputs:&mut Outputs);

    /// like [`get_next`](`Mouth::get_next`), but throws an error on `\par` (and [`EOL`](crate::tex::catcodes::CategoryCode::EOL))
    fn get_next_nopar(&mut self,state:&ET::State,interner:&mut Interner) -> Option<Token<ET>> {
        match self.get_next_simple(state,interner) {
            Some(t) => {
                match &t.base {
                    BaseToken::CS(name) if *name == interner.par =>
                        throw!("Paragraph ended while reading argument" => t),
                    BaseToken::Char(_,CategoryCode::EOL) =>
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
        Mouth { stack:Vec::with_capacity(2097152),buffer:(0..32).map(|_|Vec::with_capacity(2940000)).collect()}
    }
    fn new_with(mut tks: Vec<Token<ET>>,memory:&mut Memory<ET>) -> Self {
        tks.reverse();
        Mouth {stack:vec!(TeXMouthSource::Tkls(tks)),buffer:vec!()}
    }

    fn push_noexpand(&mut self, tk: Token<ET>,memory:&mut Memory<ET>) {
        self.stack.push(TeXMouthSource::Noexpand(tk))
    }

    fn push_file(&mut self, file: &ET::File,interner:&mut Interner) {
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

    fn get_next_simple(&mut self, state: &ET::State, interner: &mut Interner) -> Option<Token<ET>> {
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


    fn get_next(&mut self, state: &ET::State,interner:&mut Interner,outputs:&mut Outputs) -> Option<(Token<ET>, bool)> {
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
                        let eof = Token::new(BaseToken::Char(ET::Char::from(b'\n'), CategoryCode::EOL), None);
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
                                nv.extend(v.iter().skip(1).rev().map(|t| t.clone()));
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

    fn preview(&self,len:usize,interner:&Interner) -> String { // TODO memory
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

    fn endinput(&mut self, interner:&Interner,outputs:&mut Outputs) {
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

    fn file_line(&self,interner:&Interner) -> String {
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
    pub fn print_stats(&self) {
        println!("\nBuffer: {}",self.buffer.len());
        for b in &self.buffer {
            println!(" -  {}",b.capacity());
        }
    }
    pub fn get_expansion(&mut self) -> Vec<Token<ET>> {
        self.get_vec()
    }
    pub fn push_expansion(&mut self, mut expansion: Vec<Token<ET>>) {
        if expansion.is_empty() { self.buffer.push(expansion) } else {
            expansion.reverse();
            self.stack.push(TeXMouthSource::Tkls(expansion))
        }
    }

    pub fn push_expansion_norev(&mut self, mut expansion: Vec<Token<ET>>) {
        if expansion.is_empty() { self.buffer.push(expansion) } else {
            self.stack.push(TeXMouthSource::Tkls(expansion))
        }
    }

    fn with_mouth<F:FnMut(&mut EngineRef<ET>) -> R,R>(engine:&mut EngineRef<ET>, tks:Vec<Token<ET>>, mut f:F) -> R {
        let old = std::mem::replace(&mut engine.mouth, Self::new_with(tks,&mut engine.memory));
        let ret = f(engine);
        engine.mouth = old;
        ret
    }

    pub fn get_vec(&mut self) -> Vec<Token<ET>> {
        self.buffer.pop().unwrap_or(Vec::with_capacity(VEC_SIZE))
    }

    /// Skip whitespace characters from the [`MouthTrait`]
    pub fn skip_whitespace(&mut self,state:&ET::State,interner:&mut Interner) {
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
        match engine.mouth.get_next_simple(&engine.state,&mut engine.interner) {
            None => file_end!(),
            Some(t) if t.catcode() == CategoryCode::BeginGroup => {
                let mut depth = 1;
                let par = engine.interner.par;
                get_while!(&mut engine.mouth,&engine.state,&mut engine.interner,'A => tk => {
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
        match engine.mouth.get_next_simple(&engine.state,&mut engine.interner) {
            None => file_end!(),
            Some(t) if t.catcode() == CategoryCode::BeginGroup => {
                let mut ingroup = 1;
                get_while!(&mut engine.mouth,&engine.state,&mut engine.interner,'A => tk => {
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

/*
pub struct MouthII<ET:EngineType>(Option<Box<dyn TokenSource<ET>>>);
impl<ET:EngineType> MouthII<ET> {
    pub fn new() -> Self {
        MouthII(None)
    }
    pub fn new_with(mut tks:Vec<Token<ET>>) -> Self {
        tks.reverse();
        MouthII(Some(Box::new(PreTokenizedSource(tks,None))))
    }
    pub fn get_expansion(engine:&mut EngineRef<ET>) -> Vec<Token<ET>> {
        engine.memory.get_token_vec()
    }
    pub fn push_expansion(engine:&mut EngineRef<ET>,mut expansion:Vec<Token<ET>>) {
        if expansion.is_empty() { engine.memory.return_token_vec(expansion) } else {
            expansion.reverse();
            engine.mouth2.0 = Some(Box::new(PreTokenizedSource(expansion,engine.mouth2.0.take())))
        }
    }
    pub fn add_expansion<F,R>(engine:&mut EngineRef<ET>, f:F) -> R where F:FnOnce(&mut EngineRef<ET>,&mut Vec<Token<ET>>) -> R {
        let mut expansion = engine.memory.get_token_vec();
        let ret = {
            f(engine,&mut expansion)
        };
        if expansion.is_empty() { engine.memory.return_token_vec(expansion) } else {
            expansion.reverse();
            engine.mouth2.0 = Some(Box::new(PreTokenizedSource(expansion,engine.mouth2.0.take())))
        }
        ret
    }
    pub fn add_expansion_rev<F,R>(engine:&mut EngineRef<ET>, f:F) -> R where F:FnOnce(&mut EngineRef<ET>,&mut Vec<Token<ET>>) -> R {
        let mut expansion = engine.memory.get_token_vec();
        let ret = {
            f(engine,&mut expansion)
        };
        if expansion.is_empty() { engine.memory.return_token_vec(expansion) } else {
            engine.mouth2.0 = Some(Box::new(PreTokenizedSource(expansion,engine.mouth2.0.take())))
        }
        ret
    }
    pub fn push_noexpand(&mut self, tk: Token<ET>,memory:&mut Memory<ET>) {
        self.0 = Some(Box::new(NoexpandSource(Some(tk),self.0.take())))
    }
    pub fn requeue(&mut self, tk: Token<ET>) {
        self.0 = Some(Box::new(RequeuedSource(Some(tk),self.0.take())))
    }
    pub fn push_file(&mut self, file: &ET::File,interner:&mut Interner) {
        debug!("Pushing file {:?}", file.path());
        let source = StringSource::new(
            file.content_string().unwrap().clone(),
            Some(interner.from_string(file.path().to_str().unwrap()))
        );
        self.0 = Some(Box::new(FileStringSource(source,self.0.take())))
    }
    pub fn push_string(&mut self, str: &Vec<u8>) {
        let source = StringSource::new(StringSource::<ET::Char>::from_str(str),None);
        self.0 = Some(Box::new(FileStringSource(source,self.0.take())))
    }

    pub fn preview(&self,len:usize,interner:&Interner) -> String {
        let mut ret = String::new();
        match &self.0 {
            None => (),
            Some(s) => s.preview(len,interner,&mut ret)
        }
        ret
    }
    pub fn line_no(&self) -> usize {
        match &self.0 {
            None => 0,
            Some(b) => b.line_no()
        }
    }
    pub fn endinput(&mut self, interner:&Interner,outputs:&mut Outputs) {
         match &mut self.0 {
             None => (),
             Some(b) => match b.endinput(interner,outputs) {
                 Some(b) => self.0 = b,
                 _ => ()
             }
         }
    }

    fn file_line(&self,interner:&Interner) -> String {
        match &self.0 {
            None => "unknown source".to_string(),
            Some(b) => b.file_line(interner)
        }
    }
    
    pub fn get_next_nopar(&mut self,state:&ET::State,interner:&mut Interner,memory:&mut Memory<ET>) -> Option<Token<ET>> {
        match self.get_next_simple(state,interner,memory) {
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
    pub fn get_next(&mut self, state: &ET::State,interner:&mut Interner,memory:&mut Memory<ET>,outputs:&mut Outputs) -> Option<(Token<ET>, bool)> { loop {
        match &mut self.0 {
            None => return None,
            Some(s) => match s.get_next(state,interner) {
                Some(r) => return Some(r),
                _ => self.0 = self.0.take().unwrap().destroy(state,interner,memory,outputs)
            }
        }
    }}
    pub fn get_next_simple(&mut self, state: &ET::State, interner: &mut Interner,memory:&mut Memory<ET>) -> Option<Token<ET>> { loop {
        match &mut self.0 {
            None => return None,
            Some(s) => match s.next_simple(state,interner) {
                Some(r) => return Some(r),
                _ => self.0 = self.0.take().unwrap().destroy_simple(memory)
            }
        }
    }}
    pub fn iter(&mut self,state:&ET::State,interner:&mut Interner,memory:&mut Memory<ET>,f:&mut dyn FnMut(Token<ET>) -> bool) {
        loop {
            match &mut self.0 {
                None => file_end!(),
                Some(next) => {
                    if next.iter(state,interner,f) {
                        self.0 = self.0.take().unwrap().destroy_simple(memory);
                    } else {return ()}
                }
            }
        }
    }

    pub fn skip_whitespace(&mut self,state:&ET::State,interner:&mut Interner,memory:&mut Memory<ET>) {
        let mut ret = None;
        self.iter(state,interner,memory,&mut |t| {
            match t.catcode() {
                CategoryCode::Space => true,
                _ => {
                    ret = Some(t);
                    false
                }
            }
        });
        if let Some(t) = ret { self.requeue(t)}
    }

    pub fn get_argument_nopar(engine:&mut EngineRef<ET>, v:&mut Vec<Token<ET>>) {
        match engine.mouth2.get_next_simple(&engine.state,&mut engine.interner,&mut engine.memory) {
            None => file_end!(),
            Some(t) if t.catcode() == CategoryCode::BeginGroup => {
                let mut depth = 1;
                let par = engine.interner.par;
                engine.mouth2.iter(&engine.state,&mut engine.interner,&mut engine.memory,&mut |tk| {
                    match tk.base {
                        BaseToken::Char(_,CategoryCode::BeginGroup) => depth += 1,
                        BaseToken::Char(_,CategoryCode::EndGroup) => {
                            depth -= 1;
                            if depth == 0 { return false }
                        },
                        BaseToken::CS(n) if n == par =>
                            throw!("Paragraph ended while reading argument" => t),
                        _ => ()
                    }
                    v.push(tk);
                    true
                });
            }
            Some(o) => {
                v.push(o);
            }
        }
    }

    fn get_argument(engine:&mut EngineRef<ET>, vec: &mut Vec<Token<ET>>) {
        match engine.mouth2.get_next_simple(&engine.state,&mut engine.interner,&mut engine.memory) {
            None => file_end!(),
            Some(t) if t.catcode() == CategoryCode::BeginGroup => {
                let mut ingroup = 1;
                engine.mouth2.iter(&engine.state,&mut engine.interner,&mut engine.memory,&mut |tk| {
                    match tk.base {
                        BaseToken::Char(_,CategoryCode::BeginGroup) => ingroup += 1,
                        BaseToken::Char(_,CategoryCode::EndGroup) => {
                            ingroup -= 1;
                            if ingroup == 0 { return false }
                        },
                        _ => ()
                    }
                    vec.push(tk);
                    true
                })
            }
            Some(o) => {
                vec.push(o);
            }
        }
    }

}

pub trait TokenSource<ET:EngineType> {
    fn next_simple(&mut self, state:&ET::State,interner:&mut Interner) -> Option<Token<ET>>;
    fn get_next(&mut self, state:&ET::State,interner:&mut Interner) -> Option<(Token<ET>,bool)> {
        self.next_simple(state,interner).map(|t| (t,true))
    }
    fn destroy_simple(self:Box<Self>,memory:&mut Memory<ET>) -> Option<Box<dyn TokenSource<ET>>>;
    fn destroy(self:Box<Self>,state:&ET::State,interner:&Interner,memory:&mut Memory<ET>,outputs:&mut Outputs) -> Option<Box<dyn TokenSource<ET>>> {
        self.destroy_simple(memory)
    }
    fn iter(&mut self,state:&ET::State,interner:&mut Interner,f:&mut dyn FnMut(Token<ET>) -> bool) -> bool;
    fn preview(&self,len:usize,interner:&Interner,string:&mut String);
    fn line_no(&self) -> usize;
    fn endinput(&mut self, interner:&Interner,outputs:&mut Outputs) -> Option<Option<Box<dyn TokenSource<ET>>>>;
    fn file_line(&self,interner:&Interner) -> String;
}

struct NoexpandSource<ET:EngineType>(Option<Token<ET>>,Option<Box<dyn TokenSource<ET>>>);
impl<ET:EngineType> TokenSource<ET> for NoexpandSource<ET> {
    fn next_simple(&mut self, state: &ET::State, interner: &mut Interner) -> Option<Token<ET>> {
        self.0.take()
    }
    fn get_next(&mut self, state: &ET::State, interner: &mut Interner) -> Option<(Token<ET>, bool)> {
        self.0.take().map(|t| (t,false))
    }
    fn destroy_simple(self:Box<Self>,memory:&mut Memory<ET>) -> Option<Box<dyn TokenSource<ET>>> {
        self.1
    }
    fn iter(&mut self,state:&ET::State,interner:&mut Interner,f:&mut dyn FnMut(Token<ET>) -> bool) -> bool {
        if let Some(t) = self.0.take() { f(t) } else { true }
    }
    fn preview(&self, len: usize, interner: &Interner, string: &mut String) {
        if let Some(t) = &self.0 {
            string.push_str(&t.base.to_str(interner,Some(ET::Char::backslash())));
        }
        if string.len() > len { return () }
        if let Some(s) = &self.1 {
            s.preview(len,interner,string);
        }
    }
    fn line_no(&self) -> usize {
        if let Some(s) = &self.1 {
            s.line_no()
        } else { 0 }
    }
    fn endinput(&mut self, interner: &Interner, outputs: &mut Outputs) -> Option<Option<Box<dyn TokenSource<ET>>>> {
        match &mut self.1 {
            None => (),
            Some(s) => match s.endinput(interner,outputs) {
                None => (),
                Some(s) => self.1 = s
            }
        }
        None
    }
    fn file_line(&self,interner:&Interner) -> String {
        if let Some(s) = &self.1 {
            s.file_line(interner)
        } else { "unknown source".to_string() }
    }
}
struct RequeuedSource<ET:EngineType>(Option<Token<ET>>,Option<Box<dyn TokenSource<ET>>>);
impl<ET:EngineType> TokenSource<ET> for RequeuedSource<ET> {
    fn next_simple(&mut self, state: &ET::State, interner: &mut Interner) -> Option<Token<ET>> {
        self.0.take()
    }
    fn destroy_simple(self:Box<Self>,memory:&mut Memory<ET>) -> Option<Box<dyn TokenSource<ET>>> {
        self.1
    }
    fn iter(&mut self,state:&ET::State,interner:&mut Interner,f:&mut dyn FnMut(Token<ET>) -> bool) -> bool {
        if let Some(t) = self.0.take() { f(t) } else { true }
    }
    fn preview(&self, len: usize, interner: &Interner, string: &mut String) {
        if let Some(t) = &self.0 {
            string.push_str(&t.base.to_str(interner,Some(ET::Char::backslash())));
        }
        if string.len() > len { return () }
        if let Some(s) = &self.1 {
            s.preview(len,interner,string);
        }
    }
    fn line_no(&self) -> usize {
        if let Some(s) = &self.1 {
            s.line_no()
        } else { 0 }
    }
    fn endinput(&mut self, interner: &Interner, outputs: &mut Outputs) -> Option<Option<Box<dyn TokenSource<ET>>>> {
        match &mut self.1 {
            None => (),
            Some(s) => match s.endinput(interner,outputs) {
                None => (),
                Some(s) => self.1 = s
            }
        }
        None
    }
    fn file_line(&self,interner:&Interner) -> String {
        if let Some(s) = &self.1 {
            s.file_line(interner)
        } else { "unknown source".to_string() }
    }
}

struct PreTokenizedSource<ET:EngineType>(Vec<Token<ET>>,Option<Box<dyn TokenSource<ET>>>);
impl<ET:EngineType> TokenSource<ET> for PreTokenizedSource<ET> {
    fn next_simple(&mut self, state: &ET::State, interner: &mut Interner) -> Option<Token<ET>> {
        self.0.pop()
    }
    fn get_next(&mut self, state: &ET::State, interner: &mut Interner) -> Option<(Token<ET>, bool)> {
        self.0.pop().map(|t| (t,true))
    }
    fn destroy_simple(self:Box<Self>,memory:&mut Memory<ET>) -> Option<Box<dyn TokenSource<ET>>> {
        memory.return_token_vec(self.0);
        self.1
    }
    fn iter(&mut self, state: &ET::State, interner: &mut Interner, f: &mut dyn FnMut(Token<ET>) -> bool) -> bool {
        while let Some(t) = self.0.pop() {
            if !f(t) { return false }
        }
        true
    }
    fn preview(&self, len: usize, interner: &Interner, string: &mut String) {
       for t in self.0.iter().rev() {
          string.push_str(&t.base.to_str(interner,Some(ET::Char::backslash())));
          if string.len() > len { return () }
       }
        if let Some(s) = &self.1 {
          s.preview(len,interner,string);
        }
    }
    fn line_no(&self) -> usize {
        if let Some(s) = &self.1 {
            s.line_no()
        } else { 0 }
    }
    fn endinput(&mut self, interner: &Interner, outputs: &mut Outputs) -> Option<Option<Box<dyn TokenSource<ET>>>> {
        match &mut self.1 {
            None => (),
            Some(s) => match s.endinput(interner,outputs) {
                None => (),
                Some(s) => self.1 = s
            }
        }
        None
    }
    fn file_line(&self,interner:&Interner) -> String {
        if let Some(s) = &self.1 {
            s.file_line(interner)
        } else { "unknown source".to_string() }
    }
}
struct ImmutableSource<ET:EngineType>(Ptr<[Token<ET>]>,usize,Option<Box<dyn TokenSource<ET>>>);
impl<ET:EngineType> TokenSource<ET> for ImmutableSource<ET> {
    fn next_simple(&mut self, state: &ET::State, interner: &mut Interner) -> Option<Token<ET>> {
        if self.1 >= self.0.len() { return None }
        let next = self.0.get(self.1).map(|t| t.clone());
        self.1 += 1;
        next
    }
    fn destroy_simple(self:Box<Self>,memory:&mut Memory<ET>) -> Option<Box<dyn TokenSource<ET>>> {
        self.2
    }
    fn iter(&mut self, state: &ET::State, interner: &mut Interner, f: &mut dyn FnMut(Token<ET>) -> bool) -> bool {
        for t in &self.0[self.1..] {
            self.1 += 1;
            if !f(t.clone()) { return false }
        }
        true
    }
    fn preview(&self, len: usize, interner: &Interner, string: &mut String) {
        if self.1 >= self.0.len() {
            if let Some(s) = &self.2 {
                s.preview(len,interner,string);
            }
            return ()
        }
        for t in &self.0[self.1..] {
            string.push_str(&t.base.to_str(interner,Some(ET::Char::backslash())));
            if string.len() > len { return () }
        }
        if let Some(s) = &self.2 {
            s.preview(len,interner,string);
        }
    }
    fn line_no(&self) -> usize {
        if let Some(s) = &self.2 {
            s.line_no()
        } else { 0 }
    }
    fn endinput(&mut self, interner: &Interner, outputs: &mut Outputs) -> Option<Option<Box<dyn TokenSource<ET>>>> {
        match &mut self.2 {
            None => (),
            Some(s) => match s.endinput(interner,outputs) {
                None => (),
                Some(s) => self.2 = s
            }
        }
        None
    }
    fn file_line(&self,interner:&Interner) -> String {
        if let Some(s) = &self.2 {
            s.file_line(interner)
        } else { "unknown source".to_string() }
    }
}
struct DefReplacementSource<ET:EngineType>(DefReplacement<ET>,usize,[Vec<Token<ET>>;9],Option<(u8,usize)>,Option<Box<dyn TokenSource<ET>>>);
impl<ET:EngineType> TokenSource<ET> for DefReplacementSource<ET> {
    fn next_simple(&mut self, state: &ET::State, interner: &mut Interner) -> Option<Token<ET>> {
        match &mut self.3 {
            Some((arg,idx)) if *idx < (self.2)[*arg as usize].len() => {
                let r = self.2[*arg as usize][*idx].clone();
                *idx += 1;
                Some(r)
            }
            Some(_) => {
                self.3 = None;
                self.next_simple(state,interner)
            }
            None if self.1 < self.0.len() => {
                match &self.0[self.1] {
                    ExpToken::Token(t) => {
                        self.1 += 1;
                        Some(t.clone())
                    }
                    ExpToken::Param(_,i) => {
                        self.3 = Some((*i,0));
                        self.1 += 1;
                        self.next_simple(state,interner)
                    }
                }
            }
            _ => None
        }
    }
    fn destroy_simple(self: Box<Self>, memory: &mut Memory<ET>) -> Option<Box<dyn TokenSource<ET>>> {
        let self_ = *self;
        for v in self_.2 {
            memory.return_token_vec(v);
        }
        self_.4
    }
    fn iter(&mut self, state: &ET::State, interner: &mut Interner, f: &mut dyn FnMut(Token<ET>) -> bool) -> bool {
        loop {
            match &mut self.3 {
                Some((arg,idx)) if *idx < (self.2)[*arg as usize].len() => {
                    for t in &self.2[*arg as usize][*idx..] {
                        *idx += 1;
                        if !f(t.clone()) { return false }
                    }
                }
                Some(..) => {
                    self.3 = None;
                    continue
                }
                None if self.1 < self.0.len() => {
                    for t in &self.0[self.1..] {
                        self.1 += 1;
                        match t {
                            ExpToken::Token(t) => {
                                if !f(t.clone()) { return false }
                            }
                            ExpToken::Param(_,i) => {
                                self.3 = Some((*i,0));
                                self.1 += 1;
                                continue
                            }
                        }
                    }
                }
                _ => return true
            }
        }
    }
    fn preview(&self, len: usize, interner: &Interner, string: &mut String) {
        todo!()
    }
    fn line_no(&self) -> usize {
        if let Some(s) = &self.4 {
            s.line_no()
        } else { 0 }
    }
    fn endinput(&mut self, interner: &Interner, outputs: &mut Outputs) -> Option<Option<Box<dyn TokenSource<ET>>>> {
        match &mut self.4 {
            None => (),
            Some(s) => match s.endinput(interner,outputs) {
                None => (),
                Some(s) => self.4 = s
            }
        }
        None
    }
    fn file_line(&self,interner:&Interner) -> String {
        if let Some(s) = &self.4 {
            s.file_line(interner)
        } else { "unknown source".to_string() }
    }
}

struct FileStringSource<ET:EngineType>(StringSource<ET::Char>,Option<Box<dyn TokenSource<ET>>>);
impl<ET:EngineType> TokenSource<ET> for FileStringSource<ET> {
    fn next_simple(&mut self, state: &ET::State, interner: &mut Interner) -> Option<Token<ET>> {
        self.0.get_next(interner,state.get_catcode_scheme(),state.get_endlinechar())
    }
    fn get_next(&mut self, state: &ET::State, interner: &mut Interner) -> Option<(Token<ET>, bool)> {
        self.0.get_next(interner,state.get_catcode_scheme(),state.get_endlinechar()).map(|t| (t,true))
    }
    fn destroy_simple(self:Box<Self>,memory:&mut Memory<ET>) -> Option<Box<dyn TokenSource<ET>>> {
        throw!("File ended unexpectedly")
    }
    fn destroy(self: Box<Self>,state:&ET::State,interner:&Interner, memory: &mut Memory<ET>, outputs: &mut Outputs) -> Option<Box<dyn TokenSource<ET>>> {
        let self_:Self = *self;
        match &self_.0.source {
            None => (),
            Some(s) => (outputs.file_close)(interner.resolve(s.symbol()))
        }
        debug_log!(debug => "file end; inserting \\everyeof");
        let eof = Token::new(BaseToken::Char(ET::Char::from(b'\n'), CategoryCode::EOF), None);
        let everyeof = state.get_primitive_toks("everyeof");
        debug_log!(debug => "everyeof: {:?}",match everyeof {
            None => "None".to_string(),
            Some(v) => TokenList(v).to_str(interner)
        });
        match everyeof {
            None => Some(Box::new(RequeuedSource(Some(eof),self_.1))),
            Some(v) if v.is_empty() => Some(Box::new(RequeuedSource(Some(eof),self_.1))),
            Some(v) => {
                let mut nv = memory.get_token_vec();
                nv.push(eof);
                nv.extend(v.iter().rev().map(|t| t.clone()));
                Some(Box::new(PreTokenizedSource(nv,self_.1)))
            }
        }
    }
    fn iter(&mut self, state: &ET::State, interner: &mut Interner, f: &mut dyn FnMut(Token<ET>) -> bool) -> bool {
        while let Some(t) = self.0.get_next(interner,state.get_catcode_scheme(),state.get_endlinechar()) {
            if !f(t) { return false }
        }
        true
    }
    fn preview(&self, len: usize, interner: &Interner, string: &mut String) {
        string.push_str(&self.0.preview(len-string.len()));
        if string.len() > len { return () }
        if let Some(s) = &self.1 {
            s.preview(len,interner,string);
        }
    }
    fn line_no(&self) -> usize {
        self.0.line()
    }
    fn endinput(&mut self, interner: &Interner, outputs: &mut Outputs) -> Option<Option<Box<dyn TokenSource<ET>>>> {
        Some(self.1.take())
    }
    fn file_line(&self,interner:&Interner) -> String {
        match &self.0.source {
            Some(s) => format!("{}:({},{})",interner.resolve(s.symbol()),self.0.line(),self.0.column()),
            None => match &self.1 {
                Some(s) => s.file_line(interner),
                None => "unknown source".to_string()
            }
        }
    }
}
*/