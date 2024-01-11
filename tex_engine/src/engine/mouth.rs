use std::fmt::Display;
use crate::engine::{EngineAux, EngineReferences, EngineTypes};
use crate::engine::filesystem::{File, FileLineSource};
use crate::engine::filesystem::SourceReference;
use crate::engine::utils::memory::{MemoryManager, PrimitiveIdentifier, PRIMITIVES};
use crate::engine::mouth::pretokenized::{MacroExpansion, TokenListIterator, TokenVecIterator};
use crate::engine::mouth::strings::InputTokenizer;
use crate::engine::state::State;
use crate::engine::utils::outputs::Outputs;
use crate::tex::catcodes::CategoryCodeScheme;
use crate::tex::control_sequences::CSName;
use crate::tex::input_text::StringLineSource;
use crate::tex::token::Token;

pub mod strings;
pub mod pretokenized;

type C<M> = <<M as Mouth>::Token as Token>::Char;

pub trait Mouth:Sized {
    type Token:Token;
    type File:File<Char=C<Self>>;
    fn new<ET:EngineTypes<Mouth=Self>>(aux:&mut EngineAux<ET>,state:&mut ET::State) -> Self;
    fn current_sourceref(&self) -> SourceReference<<Self::File as File>::SourceRefID>;
    fn start_ref(&self) -> SourceReference<<Self::File as File>::SourceRefID>;
    fn update_start_ref(&mut self);
    fn get_args(&mut self) -> [Vec<Self::Token>;9];
    fn return_args(&mut self,args:[Vec<Self::Token>;9]);
    fn push_file(&mut self,f:Self::File);
    fn push_string(&mut self,s:StringLineSource<C<Self>>);
    fn push_exp(&mut self,exp:TokenListIterator<Self::Token>);
    fn push_vec(&mut self, exp: TokenVecIterator<Self::Token>);
    fn push_macro_exp(&mut self,exp:MacroExpansion<Self::Token>);
    fn get_next_opt<ET:EngineTypes<Char = C<Self>,Token = Self::Token,File = Self::File>>(&mut self, aux:&mut EngineAux<ET>, state:&ET::State) -> Option<Self::Token>;
    fn iterate<ET:EngineTypes<Token = Self::Token,File = Self::File>,Fn:FnMut(&mut EngineAux<ET>,Self::Token) -> bool>(&mut self,aux:&mut EngineAux<ET>,cc:&CategoryCodeScheme<C<Self>>,endline:Option<C<Self>>,cont:Fn);
    fn requeue(&mut self,t:Self::Token);
    fn num_exps(&self) -> usize;
    fn line_number(&self) -> usize;
    fn endinput<ET:EngineTypes>(&mut self, aux:&EngineAux<ET>);
    fn finish(&mut self);
    fn read_until_endgroup<ET:EngineTypes<Token = Self::Token,File = Self::File>,Fn:FnMut(&mut EngineAux<ET>,Self::Token)>(&mut self,aux:&mut EngineAux<ET>,cc:&CategoryCodeScheme<C<Self>>,endline:Option<C<Self>>,mut cont:Fn) -> Self::Token {
        let mut ingroups = 0;
        let mut eg:Option<Self::Token> = None;
        self.iterate(aux,cc,endline,|a,t| {
            if t.is_end_group()  {
                if ingroups == 0 { eg = Some(t);  return false }
                ingroups -= 1;
            }
            if t.is_begin_group() {
                ingroups += 1;
            }
            if t.is_noexpand_marker() { return true }
            cont(a,t);
            true
        });
        eg.unwrap()
    }
    fn insert_every<ET:EngineTypes<Char = C<Self>,Token = Self::Token,File = Self::File>>(&mut self,state:&ET::State,every:PrimitiveIdentifier);
    fn current_position_fmt<W:std::fmt::Write>(&self,w:W) -> std::fmt::Result;
    #[inline(always)]
    fn display_position(&self) -> MouthPosition<Self> {
        MouthPosition(self)
    }
    fn preview(&self, int:&<<Self::Token as Token>::CS as CSName<C<Self>>>::Handler, cc:&CategoryCodeScheme<C<Self>>, esc:Option<C<Self>>) -> String;
}

pub struct MouthPosition<'a,M:Mouth>(&'a M);
impl<'a,M:Mouth> Display for MouthPosition<'a,M> {
    #[inline(always)]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.current_position_fmt(f)
    }
}

impl<ET:EngineTypes> EngineReferences<'_,ET> {
    #[inline(always)]
    pub fn push_file(&mut self,f:ET::File) {
        self.mouth.push_file(f);
    }
}

pub enum TokenSource<T:Token,F:File<Char=T::Char>> {
    String(InputTokenizer<T::Char,StringLineSource<T::Char>>),
    File(InputTokenizer<T::Char,F::LineSource>, F::SourceRefID),
    Vec(Vec<T>)
}


pub struct DefaultMouth<T:Token,F:File<Char=T::Char>> {
    inputs:Vec<TokenSource<T,F>>,
    args:Option<[Vec<T>;9]>,
    start_ref:Vec<SourceReference<F::SourceRefID>>,
    vecs:Vec<Vec<T>>
}
impl<T:Token,F:File<Char=T::Char>> DefaultMouth<T,F> {
    /*
fn clean(&mut self) {
    loop {
        match self.inputs.last_mut() {
            Some(TokenSource::Expansion(e)) =>
                if !e.has_next() {
                    self.inputs.pop();
                    continue
                } else {
                    break
                }
            Some(TokenSource::TokenList(e)) =>
                if !e.has_next() {
                    self.inputs.pop();
                    continue
                } else {
                    break
                }
            _ => break
        }
    }
}
     */
    fn with_list<Fn:FnOnce(&mut Vec<T>)>(&mut self,f:Fn) {
        match self.inputs.last_mut() {
            Some(TokenSource::Vec(v)) => f(v),
            _ => {
                let v = match self.vecs.pop() {
                    Some(mut v) => {f(&mut v);v}
                    None => {
                        let mut v = Vec::new();
                        f(&mut v);
                        v
                    }
                };
                self.inputs.push(TokenSource::Vec(v));
            }
        };
    }
}
impl<T:Token,F:File<Char=T::Char>> Mouth for DefaultMouth<T,F> {
    type Token = T;
    type File = F;

    fn new<ET: EngineTypes<Mouth=Self>>(_aux: &mut EngineAux<ET>, _state: &mut ET::State) -> Self {
        Self {
            inputs:Vec::new(),args:Some(array_init::array_init(|_| Vec::new())),
            start_ref:vec!(),vecs:vec!()
        }
    }

    fn finish(&mut self) {
        for s in self.inputs.drain(..) { match s {
          TokenSource::Vec(mut v) => {
              v.clear(); self.vecs.push(v)
          }
            _ => ()
        } }
        self.start_ref.clear();
    }

    fn current_sourceref(&self) -> SourceReference<<Self::File as File>::SourceRefID> {
        for s in self.inputs.iter().rev() {
            match s {
                TokenSource::File(f,id) =>
                    return SourceReference {
                        file:*id,
                        line:f.line(),
                        column:f.column()
                    },
                _ => ()
            }
        }
        unreachable!()
    }
    fn start_ref(&self) -> SourceReference<<Self::File as File>::SourceRefID> {
        *self.start_ref.last().unwrap()
    }
    fn update_start_ref(&mut self) {
        match self.inputs.last() {
            Some(TokenSource::File(f,id)) => {
                let rf = SourceReference {
                    file:*id,
                    line:f.line(),
                    column:f.column()
                };
                match self.start_ref.last_mut() {
                    None => self.start_ref.push(rf),
                    Some(s) => *s = rf
                }
            }
            Some(TokenSource::Vec(v)) if v.is_empty() => {
                if let Some(TokenSource::Vec(v)) = self.inputs.pop() {
                    self.vecs.push(v)
                } else {unreachable!()}
                self.update_start_ref()
            }
            _ => ()
        }
    }

    #[inline(always)]
    fn get_args(&mut self) -> [Vec<T>;9] {
        match std::mem::take(&mut self.args) {
            Some(a) => a,
            None => unreachable!()//array_init::array_init(|_| Vec::new())
        }
        //array_init::array_init(|_| Vec::new())
    }
    #[inline(always)]
    fn return_args(&mut self,mut exp:[Vec<T>;9]) {
        for a in exp.iter_mut() { a.clear() }
        self.args = Some(exp);
    }

    fn endinput<ET:EngineTypes>(&mut self, aux:&EngineAux<ET>) {
        for (i,s) in self.inputs.iter().enumerate().rev() {
            match s {
                TokenSource::File(f,_) => {
                    aux.outputs.file_close(f.source.path().display());
                    self.inputs.remove(i);
                    self.start_ref.pop();
                    return
                }
                _ => ()
            }
        }
    }

    fn num_exps(&self) -> usize {
        let ret = 0;
        /*for s in self.inputs.iter().rev() {
            match s {
                TokenSource::Expansion(_) => ret += 1,
                TokenSource::TokenList(_) => ret += 1,
                _ => return ret
            }
        }*/
        return ret
    }
    fn line_number(&self) -> usize {
        for s in self.inputs.iter().rev() {
            match s {
                TokenSource::File(s,_) => return s.line(),
                _ => ()
            }
        }
        0
    }
    #[inline(always)]
    fn push_macro_exp(&mut self, mut exp: MacroExpansion<Self::Token>) {
        self.with_list(|v| exp.consume_rev(v));
        self.return_args(exp.args);
    }
    #[inline(always)]
    fn push_exp(&mut self, exp: TokenListIterator<Self::Token>) {
        self.with_list(|v|v.extend(exp.ls.0.iter().rev().cloned()))
    }
    #[inline(always)]
    fn push_vec(&mut self, exp: TokenVecIterator<Self::Token>) {
        self.with_list(|v| v.extend(exp.rev()))
    }
    #[inline(always)]
    fn push_string(&mut self, s: StringLineSource<C<Self>>) {
        self.inputs.push(TokenSource::String(InputTokenizer::new(s)));
    }

    fn insert_every<ET: EngineTypes<Char=C<Self>, Token=Self::Token, File=Self::File>>(&mut self, state: &ET::State, every: PrimitiveIdentifier) {
        let tks = state.get_primitive_tokens(every);
        if !tks.is_empty() {
            self.with_list(|v| v.extend(tks.0.iter().rev().cloned()));
        }
    }

    #[inline(always)]
    fn requeue(&mut self,t:T) {
        self.with_list(|v| v.push(t))
    }

    #[inline(always)]
    fn push_file(&mut self, f: F) {
        //self.clean();
        let id = f.sourceref();
        let s = f.line_source().unwrap();
        let rf = SourceReference {
            file:id,
            line:0,
            column:0
        };
        self.inputs.push(TokenSource::File(InputTokenizer::new(s), id));
        self.start_ref.push(rf);
    }
    fn current_position_fmt<W:std::fmt::Write>(&self,mut w:W) -> std::fmt::Result {
        for i in self.inputs.iter().rev() { match i {
            TokenSource::File(s,_) => return write!(w,"{} l. {} c. {}",s.source.path().display(),s.line(),s.column()),
            _ => ()
        }}
        Ok(())
    }
    fn get_next_opt<ET:EngineTypes<Char=T::Char,Token =T,File = F>>(&mut self,aux:&mut EngineAux<ET>,state:&ET::State) -> Option<T> {
        while let Some(src) = self.inputs.last_mut() {
            match src {
                TokenSource::Vec(v) => {
                    match v.pop() {
                        Some(t) => return Some(t),
                        _ => {
                            if let Some(TokenSource::Vec(v)) = self.inputs.pop() {
                                self.vecs.push(v);
                            } else {unreachable!()}
                        }
                    }
                }
                TokenSource::String(s) => {
                    match s.get_next(&aux.error_handler, aux.memory.cs_interner_mut(), state.get_catcode_scheme(), state.get_endline_char()) {
                        Ok(Some(t)) => return Some(t),
                        Ok(_) => return Some(self.end_file(aux,state)),
                        Err(_) => todo!()
                    }
                }
                TokenSource::File(s,_) => {
                    let cc: &CategoryCodeScheme<T::Char> = state.get_catcode_scheme();
                    let endline: Option<T::Char> = state.get_endline_char();
                    match s.get_next(&aux.error_handler, aux.memory.cs_interner_mut(), cc, endline) {
                        Ok(Some(t)) => return Some(t),
                        Ok(_) => return Some(self.end_file(aux,state)),
                        Err(_) => todo!()
                    }
                }
            }
        }
        None
    }

    fn iterate<ET:EngineTypes<Token = T,File = F>,Fn:FnMut(&mut EngineAux<ET>,T) -> bool>(&mut self,aux:&mut EngineAux<ET>,cc:&CategoryCodeScheme<T::Char>,endline:Option<T::Char>,mut cont:Fn) {
        loop {
            match self.inputs.last_mut() {
                Some(TokenSource::Vec(v)) => {
                    while let Some(t) = v.pop() {
                        if !cont(aux,t) {return}
                    }
                    if let Some(TokenSource::Vec(v)) = self.inputs.pop() {
                        self.vecs.push(v);
                    } else {unreachable!()}
                }
                Some(TokenSource::String(s)) => {
                    loop {
                        match s.get_next(&aux.error_handler, aux.memory.cs_interner_mut(), cc, endline) {
                            Ok(Some(t)) => if !cont(aux,t) { return },
                            Ok(_) => todo!(),
                            Err(_) => todo!()
                        }
                    }
                }
                Some(TokenSource::File(s,_)) => {
                    loop {
                        match s.get_next(&aux.error_handler, aux.memory.cs_interner_mut(), cc, endline) {
                            Ok(Some(t)) => if !cont(aux,t) { return },
                            Ok(_) => todo!(),
                            Err(_) => todo!()
                        }
                    }
                }
                None => todo!()
            }
        }
    }

    fn preview(&self, int:&<T::CS as CSName<T::Char>>::Handler, cc:&CategoryCodeScheme<T::Char>, esc:Option<T::Char>) -> String {
        let mut str = String::new();
        for src in self.inputs.iter().rev() {
            match src {
                //TokenSource::TokenList(s) => s.preview(int,cc,esc,&mut str),
                TokenSource::String(s) => s.preview(&mut 500,&mut str).unwrap(),
                //TokenSource::Expansion(s) => s.preview(int,cc,esc,&mut str),
                TokenSource::File(s,_) => {
                    s.preview(&mut 500,&mut str).unwrap();break
                },
                TokenSource::Vec(v) => {
                    for t in v.iter().rev().take(500) {
                        t.display_fmt(int,cc,esc,&mut str).unwrap()
                    }
                }
                /*TokenSource::Requeued(t) => {
                    t.display_fmt(int,cc,esc,&mut str).unwrap()
                }*/
            }
        }
        str
    }
}

impl<ET:EngineTypes> EngineReferences<'_,ET> {
    pub fn preview(&self) -> String {
        self.mouth.preview(self.aux.memory.cs_interner(),self.state.get_catcode_scheme(),self.state.get_escape_char())
    }
}

impl<T:Token,F:File<Char=T::Char>> DefaultMouth<T,F> {
    fn end_file<ET:EngineTypes<Char=T::Char,Token =T,File = F>>(&mut self,aux:&mut EngineAux<ET>,state:&ET::State) -> T {
        match self.inputs.pop() {
            Some(TokenSource::File(f,_)) => {
                self.start_ref.pop();
                aux.outputs.file_close(f.source.path().display());
            }
            Some(TokenSource::String(_)) => (),//aux.outputs.file_close(""),
            _ => unreachable!()
        };
        let everyeof = state.get_primitive_tokens(PRIMITIVES.everyeof);
        if everyeof.is_empty() {
            T::eof()
        } else {
            self.requeue(T::eof());
            self.insert_every::<ET>(state,PRIMITIVES.everyeof);
            if let Some(TokenSource::Vec(v)) = self.inputs.last_mut() {
                v.pop().unwrap()
            } else { unreachable!() }
        }
    }
}