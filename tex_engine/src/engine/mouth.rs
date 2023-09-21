/*! A [`Mouth`] is the source of [`Token`]s to be processed by a TeX engine.

 The default implementations ([`NoTracingMouth`] and [`Mouth`]) are
 just wrappers around a [`Vec`]`<`[`TeXMouthSource`]`>`,
 which are either [`TokenSource`]s or [`StringSource`]s.

 [`TokenSource`]s are [`Token`]s that have already been processed, while [`StringSource`]s
 represent a string of characters to be tokenized; e.g. from a source file.
 */
pub mod string_source;
pub mod methods;
pub mod experiments;

use log::debug;
use crate::engine::{EngineRef, EngineType, Outputs};
use crate::engine::filesystem::File;
use crate::engine::mouth::string_source::StringSource;
use crate::engine::state::State;
use crate::{debug_log, file_end, throw};
use crate::engine::memory::{Interner, Memory, VEC_SIZE};
use crate::tex::catcodes::CategoryCode;
use crate::tex::commands::{BaseCommand, DefReplacement, ExpToken};
use crate::tex::commands::tex::{ALIGN_END, CR, CR_END, CRCR};
use crate::tex::numbers::Skip;
use crate::tex::token::{Token, PrintableTokenList, CSLike};
use crate::utils::errors::TeXError;
use crate::utils::Ptr;
use crate::utils::strings::{CharType, TeXStr};

/// A [`Mouth`] is the source of [`Token`]s to be processed by a TeX engine.
pub trait Mouth<ET:EngineType>:Sized {
    fn new(memory:&mut Memory<ET>) -> Self;
    fn with_mouth<F:FnMut(&mut EngineRef<ET>) -> R,R>(engine:&mut EngineRef<ET>, tks:Vec<ET::Token>, f:F) -> R;

    /// Insert a [`File`] into the [`Mouth`], to be processed next
    fn push_file(&mut self, file: &ET::File,interner:&mut Interner);
    fn push_string(&mut self,str:&Vec<u8>);

    /// Insert a single [`Token`] into the [`Mouth`], not to be expanded when processed
    fn push_noexpand(&mut self,tk:ET::Token,interner:&Interner);

    fn add_align_spec(&mut self,interner:&mut Interner, spec: Vec<AlignSpec<ET>>, rec_index: Option<usize>);

    /// Insert a single [`Token`] into the [`Mouth`], to be processed next
    /// (for implementations with a peek buffer)
    fn requeue(&mut self,tk:ET::Token);

    /// Return the next [`Token`] from the [`Mouth`], and whether to expand it (due to `\noexpand`)
    //#[inline(always)]
    fn get_next(&mut self,state:&ET::State,interner:&mut Interner,outputs:&mut Outputs) -> Option<ET::Token>;

    fn get_next_simple(&mut self,state:&ET::State,interner:&mut Interner) -> Option<ET::Token>;

    /// Return the next n characters from the [`Mouth`] as a [`String`], without consuming them
    /// (for error messages, debugging purposes, etc.)
    fn preview(&self,len:usize,interner:&Interner) -> String;

    /// Return the current file and line number as presentable string
    fn file_line(&self,interner:&Interner) -> String;

    fn line_no(&self) -> usize;

    fn endinput(&mut self, interner:&Interner,outputs:&mut Outputs);
    fn get_expansion(&mut self) -> Vec<ET::Token>;
    fn push_expansion(&mut self, expansion: Vec<ET::Token>);
    fn push_expansion_norev(&mut self, expansion: Vec<ET::Token>);
    fn insert_every(&mut self,state:&ET::State,every:&'static str);

    /// like [`get_next`](`Mouth::get_next`), but throws an error on `\par` (and [`EOL`](crate::tex::catcodes::CategoryCode::EOL))
    fn get_next_no_par(&mut self, state:&ET::State, interner:&mut Interner) -> Option<ET::Token> {
        match self.get_next_simple(state,interner) {
            Some(t) => {
                match t.get_cs_name() {
                    Some(s) if s == interner.par => throw!("Paragraph ended while reading argument" => t),
                    None if t.is_eof() => file_end!(),
                    _ => Some(t)
                }
            }
            o => o
        }
    }

    /// reads a macro argument from the [`Mouth`], i.e. a sequence of [`Token`]s enclosed in
    /// braces (category codes [`BeginGroup`](CategoryCode::BeginGroup) and
    /// [`EndGroup`](CategoryCode::EndGroup)), or a single non-space [`Token`] if the argument is
    /// not enclosed.
    fn get_argument(engine:&mut EngineRef<ET>, vec: &mut Vec<ET::Token>);
    fn get_argument_no_par(engine:&mut EngineRef<ET>, v:&mut Vec<ET::Token>);

    /// Skip whitespace characters from the [`Mouth`]
    fn skip_whitespace(&mut self,state:&ET::State,interner:&mut Interner);
}


/// Either a [`TokenSource`] or a [`StringSource`]
#[derive(Clone)]
pub enum TeXMouthSource<ET:EngineType> {
    Tkls(Vec<ET::Token>),
    //Noexpand(ET::Token),
    String(StringSource<ET::Char>)
}

#[derive(Clone)]
pub struct AlignSpec<ET:EngineType> {
    pub left:Vec<ET::Token>,
    pub right:Vec<ET::Token>,
    pub skip:Skip<ET::SkipDim>
}

#[derive(Clone)]
pub struct AlignData<ET:EngineType> {
    pub specs:Vec<AlignSpec<ET>>,
    pub recindex:Option<usize>,
    pub current:usize,
    pub in_right:bool,
    pub ingroups:usize,
}
impl<ET:EngineType> AlignData<ET> {
    pub fn new(specs:Vec<AlignSpec<ET>>,recindex:Option<usize>) -> Self {
        AlignData {
            specs,
            recindex,
            current:0,
            in_right:false,
            ingroups:0
        }
    }
}

#[derive(Clone)]
pub struct StandardMouth<ET:EngineType>{
    pub stack:Vec<TeXMouthSource<ET>>,
    pub buffer:Vec<Vec<ET::Token>>,
    alignspecs:Vec<AlignData<ET>>,
}

impl<ET:EngineType<Mouth=Self>> Mouth<ET> for StandardMouth<ET> {
    fn new(memory:&mut Memory<ET>) -> Self {
        StandardMouth {
            stack:Vec::with_capacity(2097152),
            buffer:(0..32).map(|_|Vec::with_capacity(2940000)).collect(),
            alignspecs:vec!()
        }
    }

    fn get_next_simple(&mut self, state: &ET::State, interner: &mut Interner) -> Option<ET::Token> {
        if self.alignspecs.is_empty() {
            self.next_simple(state, interner)
        } else {
            let tk = self.next_simple(state,interner);
            let spec = self.alignspecs.last_mut().unwrap();
            match tk {
                Some(t) if t.is_begin_group() => {
                    spec.ingroups += 1;
                    Some(t)
                }
                Some(t) if t.is_end_group() => {
                    if spec.ingroups == 0 { throw!("Unexpected end group token" => t)}
                    spec.ingroups -= 1;
                    Some(t)
                }
                Some(t) if t.is_align_tab() && spec.ingroups == 0 => {
                    if spec.in_right {throw!("Unexpected alignment tab character" => t)}
                    spec.in_right = true;
                    let v = &spec.specs[spec.current];
                    let mut r = self.buffer.pop().unwrap_or(Vec::with_capacity(VEC_SIZE));
                    r.push(ET::Token::new_cs_from_string(TeXStr::from_static(ALIGN_END,interner),None,(0,0),(0,0)));
                    for t in v.right.iter().rev() { r.push(t.clone()) }
                    self.push_expansion(r);
                    return self.get_next_simple(state,interner);
                }
                Some(t) if !spec.in_right && spec.ingroups == 0 => {
                    match t.as_cs_like() {
                        Some(csl) => {
                            let cm = match csl {
                                CSLike::CS(name) => state.get_command(name),
                                CSLike::ActiveChar(c) => state.get_ac_command(c)
                            };
                            match cm {
                                Some(b) => {
                                    match b.base {
                                        BaseCommand::Unexpandable {name,..} if name == CR || name == CRCR => {
                                            spec.in_right = true;
                                            let v = &spec.specs[spec.current];
                                            let mut r = self.buffer.pop().unwrap_or(Vec::with_capacity(VEC_SIZE));
                                            r.push(ET::Token::new_cs_from_string(TeXStr::from_static(ALIGN_END,interner),None,(0,0),(0,0)));
                                            for t in v.right.iter().rev() { r.push(t.clone()) }
                                            self.push_expansion(r);
                                            return self.get_next_simple(state,interner);
                                        }
                                        _ => Some(t)
                                    }
                                }
                                None => Some(t)
                            }
                        }
                        None => Some(t)
                    }
                }
                Some(t) => Some(t),
                _ => file_end!()
            }
        }
    }

    fn get_next(&mut self, state: &ET::State,interner:&mut Interner,outputs:&mut Outputs) -> Option<ET::Token> {
        if self.alignspecs.is_empty() {
            self.next(state, interner, outputs)
        } else {
            let tk = self.next(state, interner, outputs);
            let spec = self.alignspecs.last_mut().unwrap();
            match tk {
                Some(t) if t.is_begin_group() => {
                    spec.ingroups += 1;
                    Some(t)
                }
                Some(t) if t.is_end_group() => {
                    if spec.ingroups == 0 { throw!("Unexpected end group token" => t)}
                    spec.ingroups -= 1;
                    Some(t)
                }
                Some(t) if t.is_align_tab() && spec.ingroups == 0 => {
                    if spec.in_right {throw!("Unexpected alignment tab character" => t)}
                    spec.in_right = true;
                    let v = &spec.specs[spec.current];
                    let mut r = self.buffer.pop().unwrap_or(Vec::with_capacity(VEC_SIZE));
                    r.push(ET::Token::new_cs_from_string(TeXStr::from_static(ALIGN_END,interner),None,(0,0),(0,0)));
                    for t in v.right.iter().rev() { r.push(t.clone()) }
                    self.push_expansion(r);
                    return self.next(state,interner,outputs);
                }
                Some(t) if !spec.in_right && spec.ingroups == 0 => {
                    match t.as_cs_like() {
                        Some(csl) => {
                            let cm = match csl {
                                CSLike::CS(name) => state.get_command(name),
                                CSLike::ActiveChar(c) => state.get_ac_command(c)
                            };
                            match cm {
                                Some(b) => {
                                    match b.base {
                                        BaseCommand::Unexpandable {name,..} if name == CR || name == CRCR => {
                                            spec.in_right = true;
                                            let v = &spec.specs[spec.current];
                                            let mut r = self.buffer.pop().unwrap_or(Vec::with_capacity(VEC_SIZE));
                                            r.push(ET::Token::new_cs_from_string(TeXStr::from_static(ALIGN_END,interner),None,(0,0),(0,0)));
                                            for t in v.right.iter().rev() { r.push(t.clone()) }
                                            self.push_expansion(r);
                                            return self.next(state,interner,outputs);
                                        }
                                        _ => Some(t)
                                    }
                                }
                                None => Some(t)
                            }
                        }
                        None => Some(t)
                    }
                }
                Some(t) => Some(t),
                _ => file_end!()
            }
        }
    }

    fn add_align_spec(&mut self,interner:&mut Interner, spec: Vec<AlignSpec<ET>>, rec_index: Option<usize>) {
        self.alignspecs.push(AlignData::new(spec,rec_index));
        self.requeue(ET::Token::new_cs_from_string(TeXStr::from_static(CR_END,interner),None,(0,0),(0,0)))
    }

    fn with_mouth<F:FnMut(&mut EngineRef<ET>) -> R,R>(engine:&mut EngineRef<ET>, mut tks:Vec<ET::Token>, mut f:F) -> R {
        tks.reverse();
        let old = std::mem::replace(&mut engine.mouth.stack,vec!(TeXMouthSource::Tkls(tks)));
        let ret = f(engine);
        engine.mouth.stack = old;
        ret
    }

    fn push_noexpand(&mut self, tk: ET::Token,interner:&Interner) {
        match self.stack.last_mut() {
            Some(TeXMouthSource::Tkls(v)) => {
                v.push(tk);
                v.push(ET::Token::new_cs_from_string(interner.noexpand_tk,None,(0,0),(0,0)));
            },
            _ => {
                let mut v = self.get_vec();
                v.push(tk);
                v.push(ET::Token::new_cs_from_string(interner.noexpand_tk,None,(0,0),(0,0)));
                self.stack.push(TeXMouthSource::Tkls(v))
            }
        }
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

    fn requeue(&mut self, tk: ET::Token) {
        match self.stack.last_mut() {
            Some(TeXMouthSource::Tkls(v)) => v.push(tk),
            _ => {
                let mut v = self.get_vec();
                v.push(tk);
                self.stack.push(TeXMouthSource::Tkls(v))
            }
        }
    }

    fn preview(&self,len:usize,interner:&Interner) -> String { // TODO memory
        let mut ret = String::new();
        for s in self.stack.iter().rev() {
            ret.push_str(&match s {
                //TeXMouthSource::Noexpand(ts) => ts.printable(interner).to_string(),
                TeXMouthSource::Tkls(v) => v.iter().rev().map(|t| t.printable(interner).to_string()).collect::<String>(),
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

    fn get_expansion(&mut self) -> Vec<ET::Token> {
        self.get_vec()
    }

    /// Like [`read_argument`](`Mouth::read_argument`), but throws an error on `\par` (and [`EOF`](crate::tex::catcodes::CategoryCode::EOF))
    fn get_argument_no_par(engine:&mut EngineRef<ET>, v:&mut Vec<ET::Token>) {
        match engine.mouth.get_next_simple(&engine.state,&mut engine.interner) {
            None => file_end!(),
            Some(t) if t.is_begin_group() => {
                let mut depth = 1;
                let par = engine.interner.par;
                while let Some(t) = engine.mouth.get_next_simple(&engine.state,&mut engine.interner) {
                    if t.is_begin_group() { depth += 1}
                    else if t.is_end_group() {
                        depth -= 1;
                        if depth == 0 { return () }
                    } else {
                        match t.get_cs_name() {
                            Some(n) if n == par => throw!("Paragraph ended while reading argument" => t),
                            _ => ()
                        }
                    }
                    v.push(t);
                }
                file_end!()
            }
            Some(o) => {
                v.push(o);
            }
        }
    }

    fn push_expansion(&mut self, mut expansion: Vec<ET::Token>) {
        if expansion.is_empty() { self.buffer.push(expansion) } else {
            expansion.reverse();
            self.stack.push(TeXMouthSource::Tkls(expansion))
        }
    }

    fn insert_every(&mut self,state:&ET::State,every:&'static str) {
        match state.get_primitive_toks(every) {
            None => (),
            Some(v) if v.is_empty() => (),
            Some(v) =>{
                let mut rs = self.get_vec();
                rs.extend(v.iter().rev().map(|t| t.clone()));
                self.push_expansion_norev(rs);
            }
        }
    }

    /// Skip whitespace characters from the [`Mouth`]
    fn skip_whitespace(&mut self,state:&ET::State,interner:&mut Interner) {
        debug_log!(trace=>"skipping whitespace");
        while let Some(tk) = self.get_next_simple(state,interner) {
            if !tk.is_space() {
                self.requeue(tk);break
            }
        }
    }

    fn get_argument(engine:&mut EngineRef<ET>, vec: &mut Vec<ET::Token>) {
        match engine.mouth.get_next_simple(&engine.state,&mut engine.interner) {
            None => file_end!(),
            Some(t) if t.is_begin_group() => {
                let mut ingroup = 1;
                while let Some(t) = engine.mouth.get_next_simple(&engine.state,&mut engine.interner) {
                    if t.is_begin_group() { ingroup += 1 }
                    else if t.is_end_group() {
                        ingroup -= 1;
                        if ingroup == 0 { return () }
                    }
                    vec.push(t);
                }
                file_end!()
            }
            Some(o) => {
                vec.push(o);
            }
        }
    }

    fn push_expansion_norev(&mut self, mut expansion: Vec<ET::Token>) {
        if expansion.is_empty() { self.buffer.push(expansion) } else {
            self.stack.push(TeXMouthSource::Tkls(expansion))
        }
    }
}

impl<ET:EngineType> StandardMouth<ET> {
    pub fn print_stats(&self) {
        println!("\nBuffer: {}",self.buffer.len());
        for b in &self.buffer {
            println!(" -  {}",b.capacity());
        }
    }

    pub fn get_vec(&mut self) -> Vec<ET::Token> {
        self.buffer.pop().unwrap_or(Vec::with_capacity(VEC_SIZE))
    }

    /*pub fn in_align<R,F:FnOnce(&mut Self,&ET::State,&mut Interner) -> R>(&mut self,state:&ET::State,interner:&mut Interner,next:F) -> R {

    }*/

    fn next_simple(&mut self, state: &ET::State, interner: &mut Interner) -> Option<ET::Token> {
        match self.stack.last_mut() {
            /*Some(TeXMouthSource::Noexpand(tk)) => todo!() match self.stack.pop() {
                Some(TeXMouthSource::Noexpand(t)) => Some(t),
                _ => unreachable!()
            },*/
            Some(TeXMouthSource::Tkls(ref mut v)) => {
                let mut ret = v.pop().unwrap();
                if ret.is_noexpand_marker(&*interner) {
                    ret = v.pop().unwrap();
                }
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
                match s.get_next::<ET>(interner,state.get_catcode_scheme(),state.get_endlinechar()) {
                    Some(t) => Some(t),
                    None => panic!("File ended unexpectedly")
                }
            }
            None => None
        }
    }


    fn next(&mut self, state: &ET::State,interner:&mut Interner,outputs:&mut Outputs) -> Option<ET::Token> {
        match self.stack.last_mut() {
            /*Some(TeXMouthSource::Noexpand(tk)) => todo!(),match self.stack.pop() {
                Some(TeXMouthSource::Noexpand(t)) => Some((t, false)),
                _ => unreachable!()
            },*/
            Some(TeXMouthSource::Tkls(v)) => {
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
                match s.get_next::<ET>(interner,state.get_catcode_scheme(), state.get_endlinechar()) {
                    Some(t) => return Some(t),
                    None => {
                        match &s.source {
                            None => (),
                            Some(s) => (outputs.file_close)(interner.resolve(s.symbol()))
                        }
                        self.stack.pop();
                        debug_log!(debug => "file end; inserting \\everyeof");
                        let eof = ET::Token::eof();
                        let everyeof = state.get_primitive_toks("everyeof");
                        debug_log!(debug => "everyeof: {}",match everyeof {
                            None => "None".to_string(),
                            Some(v) => PrintableTokenList::<ET>(v,interner).to_string()
                        });
                        match everyeof {
                            None => Some(eof),
                            Some(v) if v.is_empty() => Some(eof),
                            Some(v) => {
                                let mut nv = self.get_vec();
                                nv.push(eof);
                                nv.extend(v.iter().skip(1).rev().map(|t| t.clone()));
                                self.stack.push(TeXMouthSource::Tkls(nv));
                                Some(v.first().unwrap().clone())
                            }
                        }
                    }
                }
            },
            None => None
        }
    }
}