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
use crate::tex::commands::DefReplacement;
use crate::tex::token::{BaseToken, Token, TokenList};
use crate::utils::errors::TeXError;
use crate::utils::Ptr;
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
    fn push_string(&mut self,str:Vec<u8>);

    /// Insert a single [`Token`] into the [`Mouth`], not to be expanded when processed
    fn push_noexpand(&mut self,tk:Token<ET>,memory:&mut Memory<ET>);

    /// Insert a single [`Token`] into the [`Mouth`], to be processed next
    /// (for implementations with a peek buffer)
    fn requeue(&mut self,tk:Token<ET>);

    /// Return the next [`Token`] from the [`Mouth`], and whether to expand it (due to `\noexpand`)
    //#[inline(always)]
    fn get_next(&mut self,state:&ET::State,interner:&mut Interner<ET::Char>,outputs:&mut Outputs) -> Option<(Token<ET>,bool)>;

    fn get_next_simple(&mut self,state:&ET::State,interner:&mut Interner<ET::Char>) -> Option<Token<ET>>;

    /// Return the next n characters from the [`Mouth`] as a [`String`], without consuming them
    /// (for error messages, debugging purposes, etc.)
    fn preview(&self,len:usize,interner:&Interner<ET::Char>) -> String;

    /// Return the current file and line number as presentable string
    fn file_line(&self,interner:&Interner<ET::Char>) -> String;

    fn line_no(&self) -> usize;

    fn endinput(&mut self, state: &mut ET::State,interner:&Interner<ET::Char>,outputs:&mut Outputs);

    /// Skip whitespace characters from the [`Mouth`]
    #[inline(always)]
    fn skip_whitespace(&mut self,state:&ET::State,interner:&mut Interner<ET::Char>) {
        debug_log!(trace=>"skipping whitespace");
        while let Some(tk) = self.get_next_simple(state,interner) {
            match tk.catcode() {
                CategoryCode::Space => (),
                _ => {
                    self.requeue(tk);
                    break
                }
            }
        }
    }

    /// reads a macro argument from the [`Mouth`], i.e. a sequence of [`Token`]s enclosed in
    /// braces (category codes [`BeginGroup`](CategoryCode::BeginGroup) and
    /// [`EndGroup`](CategoryCode::EndGroup)), or a single non-space [`Token`] if the argument is
    /// not enclosed.
    fn get_argument(engine:&mut EngineRef<ET>, vec: &mut Vec<Token<ET>>) {
        match engine.mouth.get_next_simple(engine.state,engine.interner) {
            None => file_end!(),
            Some(t) if t.catcode() == CategoryCode::EOF =>
                file_end!(),
            Some(t) if t.catcode() == CategoryCode::BeginGroup => {
                get_until_endgroup!(engine,t => vec.push(t));
                //Self::get_until_endgroup(engine,&mut|_,t| Ok(vec.push(t)))
            }
            Some(o) => {
                vec.push(o);
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

    /// Like [`read_argument`](`Mouth::read_argument`), but throws an error on `\par` (and [`EOF`](crate::tex::catcodes::CategoryCode::EOF))
    fn get_argument_nopar(engine:&mut EngineRef<ET>, v:&mut Vec<Token<ET>>) {
        match engine.mouth.get_next_simple(engine.state,engine.interner) {
            None => file_end!(),
            Some(t) if t.catcode() == CategoryCode::BeginGroup => {
                let mut depth = 1;
                while let Some(tk) = engine.mouth.get_next_simple(engine.state,engine.interner) {
                    match &tk.base {
                        BaseToken::Char(_,CategoryCode::BeginGroup) => depth += 1,
                        BaseToken::Char(_,CategoryCode::EndGroup) => {
                            depth -= 1;
                            if depth == 0 { return () }
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

    fn push_string(&mut self, str: Vec<u8>) {
        let source = TeXMouthSource::String(StringSource::new(str,None));
        self.stack.push(source);
    }

    fn requeue(&mut self, tk: Token<ET>) {
        self.stack.push(TeXMouthSource::Token((tk,true)))
    }

    fn get_next_simple(&mut self, state: &ET::State, interner: &mut Interner<ET::Char>) -> Option<Token<ET>> {
        match self.stack.last_mut() {
            Some(TeXMouthSource::Token(ref mut tks)) => match self.stack.pop() {
                Some(TeXMouthSource::Token((t, b))) => Some(t),
                _ => unreachable!()
            },
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
            Some(TeXMouthSource::Token(ref mut tks)) => match self.stack.pop() {
                Some(TeXMouthSource::Token((t, b))) => Some((t, b)),
                _ => unreachable!()
            },
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
                        let eof = (Token::new(BaseToken::Char(ET::Char::from(b'\n'), CategoryCode::EOF), None),true);
                        match state.get_primitive_toks("everyeof") {
                            None => Some(eof),
                            Some(v) if v.is_empty() => Some(eof),
                            Some(v) => {
                                self.stack.push(TeXMouthSource::Token(eof));
                                for t in v.iter().skip(1).rev() { self.stack.push(TeXMouthSource::Token((t.clone(),true))) };
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
                TeXMouthSource::Token(ts) => ts.0.base.to_str(interner,Some(ET::Char::backslash())),
                TeXMouthSource::String(ss) => ss.preview()
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

impl<ET:EngineType<Mouth=Self>> StandardMouth<ET> {
}

/*
/// Either a [`TokenSource`] or a [`StringSource`]
#[derive(Clone)]
pub enum TeXMouthSource<ET:EngineType> {
    Token((Token<ET>,bool)),
    String(StringSource<ET::Char>)
}
*/

pub enum TokenSource<ET:EngineType> {
    String(StringSource<ET::Char>),
    Simple(SimpleExpansion<ET>),
    Single{token:Token<ET>,expand:bool},
    Def(DefExpansion<ET>)
}

pub struct SimpleExpansion<ET:EngineType>(Ptr<Vec<Token<ET>>>,usize);
pub struct DefExpansion<ET:EngineType>(DefReplacement<ET>,[Vec<Token<ET>>;9],usize);