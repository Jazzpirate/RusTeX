use crate::engine::{EngineAux, EngineReferences, EngineTypes};
use crate::engine::filesystem::{File, FileLineSource};
use crate::engine::filesystem::SourceReference;
use crate::engine::utils::memory::{PrimitiveIdentifier, PRIMITIVES};
use crate::tex::tokens::token_lists::MacroExpansion;
use crate::engine::mouth::strings::InputTokenizer;
use crate::engine::state::State;
use crate::engine::utils::outputs::Outputs;
use crate::prelude::TokenList;
use crate::tex::catcodes::CategoryCodeScheme;
use crate::tex::tokens::control_sequences::CSName;
use crate::tex::characters::StringLineSource;
use crate::tex::tokens::Token;

pub mod strings;

pub trait Mouth<ET:EngineTypes> {
    fn new(aux:&mut EngineAux<ET>,state:&mut ET::State) -> Self;
    fn current_sourceref(&self) -> SourceReference<<ET::File as File>::SourceRefID>;
    fn start_ref(&self) -> SourceReference<<ET::File as File>::SourceRefID>;
    fn update_start_ref(&mut self);
    fn get_args(&mut self) -> [Vec<ET::Token>;9];
    fn return_args(&mut self,args:[Vec<ET::Token>;9]);
    fn push_file(&mut self,f:ET::File);
    fn push_string(&mut self,s:StringLineSource<ET::Char>);
    fn push_exp(&mut self,exp:&TokenList<ET::Token>);
    fn push_vec(&mut self, exp: Vec<ET::Token>);
    fn push_macro_exp(&mut self,exp:MacroExpansion<ET::Token>);
    fn get_next_opt(&mut self, aux:&mut EngineAux<ET>, state:&ET::State) -> Option<ET::Token>;
    fn iterate<Fn:FnMut(&mut EngineAux<ET>,ET::Token) -> bool>(&mut self,aux:&mut EngineAux<ET>,state:&ET::State,cont:Fn);
    fn requeue(&mut self,t:ET::Token);
    fn num_exps(&self) -> usize;
    fn line_number(&self) -> usize;
    fn endinput(&mut self, aux:&EngineAux<ET>);
    fn finish(&mut self);
    fn read_until_endgroup<Fn:FnMut(&mut EngineAux<ET>,ET::Token)>(&mut self,aux:&mut EngineAux<ET>,state:&ET::State,mut cont:Fn) -> ET::Token {
        let mut ingroups = 0;
        let mut eg:Option<ET::Token> = None;
        self.iterate(aux,state,|a,t| {
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

    fn preview(&self, int:&<ET::CSName as CSName<ET::Char>>::Handler, cc:&CategoryCodeScheme<ET::Char>, esc:Option<ET::Char>) -> String;
}

impl<ET:EngineTypes> EngineReferences<'_,ET> {

    pub fn push_file(&mut self,f:ET::File) {
        self.mouth.push_file(f);
    }
}

pub enum TokenSource<T:Token,F:File<Char=T::Char>> {
    String(InputTokenizer<T::Char,StringLineSource<T::Char>>),
    File(InputTokenizer<T::Char,F::LineSource>, F::SourceRefID),
    Vec(Vec<T>)
}


pub struct DefaultMouth<ET:EngineTypes> {
    inputs:Vec<TokenSource<ET::Token,ET::File>>,
    args:Option<[Vec<ET::Token>;9]>,
    start_ref:Vec<SourceReference<<ET::File as File>::SourceRefID>>,
    vecs:Vec<Vec<ET::Token>>
}
impl<ET:EngineTypes> DefaultMouth<ET> {
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
    fn with_list<Fn:FnOnce(&mut Vec<ET::Token>)>(&mut self,f:Fn) {
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
impl<ET:EngineTypes> Mouth<ET> for DefaultMouth<ET> {
    fn new(_aux: &mut EngineAux<ET>, _state: &mut ET::State) -> Self {
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

    fn current_sourceref(&self) -> SourceReference<<ET::File as File>::SourceRefID> {
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
    fn start_ref(&self) -> SourceReference<<ET::File as File>::SourceRefID> {
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


    fn get_args(&mut self) -> [Vec<ET::Token>;9] {
        match std::mem::take(&mut self.args) {
            Some(a) => a,
            None => unreachable!()//array_init::array_init(|_| Vec::new())
        }
        //array_init::array_init(|_| Vec::new())
    }

    fn return_args(&mut self,mut exp:[Vec<ET::Token>;9]) {
        for a in exp.iter_mut() { a.clear() }
        self.args = Some(exp);
    }

    fn endinput(&mut self, aux:&EngineAux<ET>) {
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

    fn push_macro_exp(&mut self, mut exp: MacroExpansion<ET::Token>) {
        self.with_list(|v| exp.consume_rev(v));
        self.return_args(exp.args);
    }

    fn push_exp(&mut self, exp: &TokenList<ET::Token>) {
        self.with_list(|v|v.extend(exp.0.iter().rev().cloned()))
    }

    fn push_vec(&mut self, exp: Vec<ET::Token>) {
        self.with_list(|v| v.extend(exp.into_iter().rev()))
    }

    fn push_string(&mut self, s: StringLineSource<ET::Char>) {
        self.inputs.push(TokenSource::String(InputTokenizer::new(s)));
    }

    fn requeue(&mut self,t:ET::Token) {
        self.with_list(|v| v.push(t))
    }

    fn push_file(&mut self, f: ET::File) {
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

    fn get_next_opt(&mut self,aux:&mut EngineAux<ET>,state:&ET::State) -> Option<ET::Token> {
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
                    let cc: &CategoryCodeScheme<ET::Char> = state.get_catcode_scheme();
                    let endline: Option<ET::Char> = state.get_endline_char();
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

    fn iterate<Fn:FnMut(&mut EngineAux<ET>,ET::Token) -> bool>(&mut self,aux:&mut EngineAux<ET>,state:&ET::State,mut cont:Fn) {
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
                    let cc = state.get_catcode_scheme();
                    let endline = state.get_endline_char();
                    loop {
                        match s.get_next(&aux.error_handler, aux.memory.cs_interner_mut(), cc, endline) {
                            Ok(Some(t)) => if !cont(aux,t) { return },
                            Ok(_) => todo!(),
                            Err(_) => todo!()
                        }
                    }
                }
                Some(TokenSource::File(s,_)) => {
                    let cc = state.get_catcode_scheme();
                    let endline = state.get_endline_char();
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

    fn preview(&self, int:&<ET::CSName as CSName<ET::Char>>::Handler, cc:&CategoryCodeScheme<ET::Char>, esc:Option<ET::Char>) -> String {
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

impl<ET:EngineTypes> DefaultMouth<ET> {
    fn end_file(&mut self,aux:&mut EngineAux<ET>,state:&ET::State) -> ET::Token {
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
            ET::Token::eof()
        } else {
            self.requeue(ET::Token::eof());
            self.push_exp(everyeof);
            if let Some(TokenSource::Vec(v)) = self.inputs.last_mut() {
                v.pop().unwrap()
            } else { unreachable!() }
        }
    }
}

impl<ET:EngineTypes> EngineReferences<'_,ET> {
    pub fn insert_every(&mut self,every:PrimitiveIdentifier) {
        let tks = self.state.get_primitive_tokens(every);
        self.mouth.push_exp(tks);
    }
}