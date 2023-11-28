use std::fmt::Display;
use crate::engine::{EngineAux, EngineReferences, EngineTypes};
use crate::engine::filesystem::{File, FileLineSource};
use crate::engine::utils::memory::{MemoryManager, PRIMITIVES};
use crate::engine::mouth::pretokenized::{MacroExpansion, TokenListIterator};
use crate::engine::mouth::strings::StringTokenizer;
use crate::engine::state::State;
use crate::engine::utils::outputs::Outputs;
use crate::tex::catcodes::CategoryCodeScheme;
use crate::tex::control_sequences::ControlSequenceName;
use crate::tex::input_text::StringLineSource;
use crate::tex::token::Token;

pub mod strings;
pub mod pretokenized;

type C<M> = <<M as Mouth>::Token as Token>::Char;

pub trait Mouth:Sized {
    type Token:Token;
    type File:File<Char=C<Self>>;
    fn get_args(&mut self) -> [Vec<Self::Token>;9];
    fn return_args(&mut self,args:[Vec<Self::Token>;9]);
    fn push_file(&mut self,f:Self::File);
    fn push_exp(&mut self,exp:TokenListIterator<Self::Token>);
    fn push_macro_exp(&mut self,exp:MacroExpansion<Self::Token>);
    fn get_next_opt<ET:EngineTypes<Char = C<Self>,Token = Self::Token,File = Self::File>>(&mut self, aux:&mut EngineAux<ET>, state:&ET::State) -> Option<Self::Token>;
    fn iterate<ET:EngineTypes<Token = Self::Token,File = Self::File>,Fn:FnMut(&mut EngineAux<ET>,Self::Token) -> bool>(&mut self,aux:&mut EngineAux<ET>,cc:&CategoryCodeScheme<C<Self>>,endline:Option<C<Self>>,cont:Fn);
    fn requeue(&mut self,t:Self::Token);
    fn num_exps(&self) -> usize;
    fn line_number(&self) -> usize;
    fn endinput(&mut self);
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
            cont(a,t);
            true
        });
        eg.unwrap()
    }
    fn current_position_fmt<W:std::fmt::Write>(&self,w:W) -> std::fmt::Result;
    #[inline(always)]
    fn current_position(&self) -> MouthPosition<Self> {
        MouthPosition(self)
    }
    fn preview(&self,int:&<<Self::Token as Token>::CS as ControlSequenceName>::Handler,cc:&CategoryCodeScheme<C<Self>>,esc:Option<C<Self>>) -> String;
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
    String(StringTokenizer<T::Char,StringLineSource>),
    File(StringTokenizer<T::Char,F::LineSource>),
    Expansion(MacroExpansion<T>),
    TokenList(TokenListIterator<T>),
    Requeued(T)
}


pub struct DefaultMouth<T:Token,F:File<Char=T::Char>> {
    inputs:Vec<TokenSource<T,F>>,
    args:Vec<[Vec<T>;9]>
}
impl<T:Token,F:File<Char=T::Char>> DefaultMouth<T,F> {
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
}
impl<T:Token,F:File<Char=T::Char>> Mouth for DefaultMouth<T,F> {
    type Token = T;
    type File = F;

    fn get_args(&mut self) -> [Vec<T>;9] {
        match self.args.pop() {
            Some(a) => a,
            None => array_init::array_init(|_| Vec::new())
        }
    }
    fn return_args(&mut self,mut exp:[Vec<T>;9]) {
        for a in exp.iter_mut() { a.clear() }
        self.args.push(exp)
    }

    fn endinput(&mut self) {
        for (i,s) in self.inputs.iter().enumerate().rev() {
            match s {
                TokenSource::File(_) => {
                    self.inputs.remove(i);
                    return
                }
                _ => ()
            }
        }
    }

    fn num_exps(&self) -> usize {
        let mut ret = 0;
        for s in self.inputs.iter().rev() {
            match s {
                TokenSource::Expansion(_) => ret += 1,
                TokenSource::TokenList(_) => ret += 1,
                _ => return ret
            }
        }
        return ret
    }
    fn line_number(&self) -> usize {
        for s in self.inputs.iter().rev() {
            match s {
                TokenSource::File(s) => return s.line(),
                _ => ()
            }
        }
        0
    }
    #[inline(always)]
    fn push_macro_exp(&mut self, exp: MacroExpansion<Self::Token>) {
        self.clean();
        self.inputs.push(TokenSource::Expansion(exp))
    }
    #[inline(always)]
    fn push_exp(&mut self, exp: TokenListIterator<Self::Token>) {
        self.clean();
        self.inputs.push(TokenSource::TokenList(exp))
    }
    #[inline(always)]
    fn requeue(&mut self,t:T) {
        self.clean();
        self.inputs.push(TokenSource::Requeued(t))
    }
    #[inline(always)]
    fn push_file(&mut self, f: F) {
        self.clean();
        let s = f.line_source().unwrap();
        self.inputs.push(TokenSource::File(StringTokenizer::new(s)));
    }
    fn current_position_fmt<W:std::fmt::Write>(&self,mut w:W) -> std::fmt::Result {
        for i in self.inputs.iter().rev() { match i {
            TokenSource::File(s) => return write!(w,"{} l. {} c. {}",s.source.path().display(),s.line(),s.column()),
            _ => ()
        }}
        Ok(())
    }
    fn get_next_opt<ET:EngineTypes<Char=T::Char,Token =T,File = F>>(&mut self,aux:&mut EngineAux<ET>,state:&ET::State) -> Option<T> {
        let cc: &CategoryCodeScheme<T::Char> = state.get_catcode_scheme();
        let endline: Option<T::Char> = state.get_endline_char();
        loop {
            match self.inputs.last_mut() {
                None => return None,
                Some(TokenSource::Requeued(_)) => {
                    if let Some(TokenSource::Requeued(t)) = self.inputs.pop() {
                        return Some(t)
                    } else { unreachable!() }
                }
                Some(TokenSource::TokenList(s)) => {
                    match s.next() {
                        Some(t) => return Some(t),
                        _ => match self.inputs.pop() {
                            Some(TokenSource::TokenList(ls)) => ls.give_back_maybe(&mut aux.memory),
                            _ => unreachable!()
                        }
                    }
                }
                Some(TokenSource::Expansion(s)) => {
                    match s.next() {
                        Some(t) => return Some(t),
                        _ => match self.inputs.pop() {
                            Some(TokenSource::Expansion(ls)) => self.return_args(ls.args), // ls.give_back_maybe(&mut aux.memory),
                            _ => unreachable!()
                        }
                    }
                }
                Some(TokenSource::String(s)) => {
                    match s.get_next(&aux.error_handler, aux.memory.cs_interner_mut(), cc, endline) {
                        Some(t) => return Some(t),
                        _ => {
                            self.inputs.pop();
                            todo!("file end in mouth")
                        }
                    }
                }
                Some(TokenSource::File(s)) => {
                    match s.get_next(&aux.error_handler, aux.memory.cs_interner_mut(), cc, endline) {
                        Some(t) => return Some(t),
                        _ => {
                            let f = match self.inputs.pop() {
                                Some(TokenSource::File(f)) => f,
                                _ => unreachable!()
                            };
                            aux.outputs.file_close(f.source.path().display());
                            let everyeof = state.get_primitive_tokens(PRIMITIVES.everyeof);
                            if everyeof.is_empty() {
                                return Some(T::eof())
                            } else {
                                todo!("everyeof");
                                self.inputs.push(TokenSource::TokenList(TokenListIterator::new(Some(PRIMITIVES.everyeof),everyeof.clone())))
                            }
                        }
                    }
                }
            }
        }
    }

    fn iterate<ET:EngineTypes<Token = T,File = F>,Fn:FnMut(&mut EngineAux<ET>,T) -> bool>(&mut self,aux:&mut EngineAux<ET>,cc:&CategoryCodeScheme<T::Char>,endline:Option<T::Char>,mut cont:Fn) {
        macro_rules! iterate {
            ($iter:ident) => {
                for t in $iter {
                    if !cont(aux,t) { return }
                }
            };
        }
        loop {
            match self.inputs.last_mut() {
                Some(TokenSource::Requeued(t)) => {
                    if let Some(TokenSource::Requeued(t)) = self.inputs.pop() {
                        if t.is_noexpand_marker() {continue}
                        if !cont(aux,t) {return}
                    } else { unreachable!() }
                }
                Some(TokenSource::TokenList(s)) => {
                    iterate!(s);
                    match self.inputs.pop() {
                        Some(TokenSource::TokenList(ls)) => ls.give_back_maybe(&mut aux.memory),
                        _ => unreachable!()
                    }
                }
                Some(TokenSource::Expansion(s)) => {
                    iterate!(s);
                    match self.inputs.pop() {
                        Some(TokenSource::Expansion(ls)) => self.return_args(ls.args), //ls.give_back_maybe(&mut aux.memory),
                        _ => unreachable!()
                    }
                }
                Some(TokenSource::String(s)) => {
                    while let Some(t) = s.get_next(&aux.error_handler, aux.memory.cs_interner_mut(), cc, endline) {
                        if !cont(aux,t) { return }
                    }
                    todo!()
                }
                Some(TokenSource::File(s)) => {
                    while let Some(t) = s.get_next(&aux.error_handler, aux.memory.cs_interner_mut(), cc, endline) {
                        if !cont(aux,t) { return }
                    }
                    todo!()
                }
                None => todo!()
            }
        }
    }
    fn preview(&self,int:&<T::CS as ControlSequenceName>::Handler,cc:&CategoryCodeScheme<T::Char>,esc:Option<T::Char>) -> String {
        let mut str = String::new();
        for src in self.inputs.iter().rev() {
            match src {
                TokenSource::TokenList(s) => s.preview(int,cc,esc,&mut str),
                TokenSource::String(s) => s.preview(&mut 500,&mut str).unwrap(),
                TokenSource::Expansion(s) => s.preview(int,cc,esc,&mut str),
                TokenSource::File(s) => {
                    s.preview(&mut 50,&mut str).unwrap();break
                },
                TokenSource::Requeued(t) => {
                    t.display_fmt(int,cc,esc,&mut str).unwrap()
                }
                _ => ()
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
    pub fn new() -> Self {
        DefaultMouth {
            inputs:Vec::new(),args:Vec::new()
        }
    }
/*
    fn get_argument<Fn:FnMut(T),E:ErrorHandler,M:MemoryManager<T>>(&mut self,eh:&E,mem:&mut M,cc:&CategoryCodeScheme<T::Char>,endline:Option<T::Char>,delim:Option<&[T]>,nopar:Option<T::CS>,mut cont:Fn) {
        match delim {
            Some(d) => self.get_argument_with_delim(d,nopar,cont),
            _ => match self.get_next_opt(eh,mem,cc,endline) {
                Some(t) if t.is_noexpand_marker() => cont(self.get_next_opt(eh,mem,cc,endline).unwrap()),
                // not None because of the noexpand marker, and not begin group, because a begin group
                // character is not expandable
                Some(t) if t.is_begin_group() => self.read_until_endgroup(eh,mem,cc,endline,nopar,cont),
                Some(t) => cont(t),
                _ => todo!()
            }
        }
    }

 */
/*
    fn read_until_endgroup<Fn:FnMut(T),E:ErrorHandler,M:MemoryManager<T>>(&mut self,eh:&E,mem:&mut M,cc:&CategoryCodeScheme<T::Char>,endline:Option<T::Char>,nopar:Option<T::CS>,mut cont:Fn) {
        let mut ingroups = 0;
        self.iterate(eh,mem,cc,endline,|t| {
            if t.is_begin_group() { ingroups += 1;
            } else if t.is_end_group() {
                if ingroups == 0 { return false }
                ingroups -= 1;
            } else if t.is_noexpand_marker() { return true }
            cont(t);
            true
        });
    }

 */

    fn get_argument_with_delim<Fn:FnMut(T)>(&mut self,delim:&[T],nopar:Option<T::CS>,cont:Fn) {
        todo!()
    }
}