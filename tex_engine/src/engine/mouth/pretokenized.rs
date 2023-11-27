use std::fmt::{Display, Write};
use std::marker::PhantomData;
use log::kv::Source;
use crate::engine::utils::memory::{MemoryManager, PrimitiveIdentifier};
use crate::tex::catcodes::{CategoryCode, CommandCode};
use crate::tex::control_sequences::{ControlSequenceName, ControlSequenceNameHandler};
use crate::tex::token::{StandardToken, Token};
use crate::utils::Ptr;
use crate::tex::catcodes::CategoryCodeScheme;
use crate::tex::input_text::Character;
use crate::tex::input_text::CharacterMap;

#[derive(Clone,Debug)]
pub struct TokenList<T:Token>(Ptr<[T]>);
impl<T:Token> PartialEq for TokenList<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}
impl<T:Token> TokenList<T> {
    #[inline(always)]
    pub fn inner(&self) -> &[T] { &*self.0 }
    pub fn give_back_maybe<M:MemoryManager<T>>(self,memory:&mut M) {
        /*match std::rc::Rc::try_unwrap(self.0) {
            Ok(sl) => memory.return_token_vec(sl.into_vec()),
            _ => ()
        }*/
    }
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
    pub fn empty() -> Self {
        Self(Ptr::new([]))
    }
    #[inline(always)]
    pub fn get(&self,i:usize) -> &T {
        &(*self.0)[i]
    }

    pub fn meaning_fmt(&self, int:&<T::CS as ControlSequenceName>::Handler, cc:&CategoryCodeScheme<T::Char>, escapechar:Option<T::Char>, f: &mut std::fmt::Formatter<'_>,double_par:bool) -> std::fmt::Result {
        meaning_fmt(self.0.iter(),int,cc,escapechar,f,double_par)
        /*for t in self.0.iter() {
            match t.is_argument_marker() {
                Some(i) => write!(f,"#{}",(i + 1))?,
                _ => match t.to_enum() {
                    StandardToken::Character(c,CommandCode::Parameter) => write!(f,"{}{}",c.displayable(),c.displayable())?,
                    StandardToken::Character(_,CommandCode::Space) => write!(f," ")?,
                    StandardToken::Character(c,_) => write!(f,"{}",c.displayable())?,
                    StandardToken::ControlSequence(cs) => {
                        let res = int.resolve(&cs);
                        let str = res.as_ref();
                        write!(f, "{}{}", T::Char::displayable_opt(escapechar), str)?;
                        match T::Char::single_char(str) {
                            None => write!(f," ")?,
                            Some(c) => match cc.get(c) {
                                CategoryCode::Letter => write!(f," ")?,
                                _ => ()
                            }
                        }
                    }
                }
            }
        }
        Ok(())*/
    }
    pub fn displayable<'a>(&'a self,int:&'a <T::CS as ControlSequenceName>::Handler,cc:&'a CategoryCodeScheme<T::Char>,escapechar:Option<T::Char>) -> TLMeaning<'a,T> {
        TLMeaning {
            ls:self,
            int,
            cc,
            escapechar
        }
    }
}

pub fn meaning_fmt<'a,T:Token,I:Iterator<Item=&'a T>,W:std::fmt::Write>(iter:I, int:&<T::CS as ControlSequenceName>::Handler, cc:&CategoryCodeScheme<T::Char>, escapechar:Option<T::Char>, mut f: W,double_par:bool) -> std::fmt::Result {
    for t in iter {
        match t.is_argument_marker() {
            Some(i) => write!(f,"#{}",(i + 1))?,
            _ => match t.to_enum() {
                StandardToken::Character(c,CommandCode::Parameter) if double_par => write!(f,"{}{}",c.displayable(),c.displayable())?,
                StandardToken::Character(_,CommandCode::Space) => write!(f," ")?,
                StandardToken::Character(c,_) => write!(f,"{}",c.displayable())?,
                StandardToken::ControlSequence(cs) => {
                    let res = int.resolve(&cs);
                    let str = res.as_ref();
                    write!(f, "{}{}", T::Char::displayable_opt(escapechar), str)?;
                    match T::Char::single_char(str) {
                        None => write!(f," ")?,
                        Some(c) => match cc.get(c) {
                            CategoryCode::Letter => write!(f," ")?,
                            _ => ()
                        }
                    }
                }
            }
        }
    }
    Ok(())
}

pub struct TLMeaning<'a,T:Token> {
    ls:&'a TokenList<T>,
    int:&'a <T::CS as ControlSequenceName>::Handler,
    cc:&'a CategoryCodeScheme<T::Char>,
    escapechar:Option<T::Char>
}
impl<'a,T:Token> Display for TLMeaning<'a,T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        meaning_fmt(self.ls.0.iter(),self.int,self.cc,self.escapechar,f,false)
    }
}

pub struct TLVecMeaning<'a,T:Token> {
    ls:&'a Vec<T>,
    int:&'a <T::CS as ControlSequenceName>::Handler,
    cc:&'a CategoryCodeScheme<T::Char>,
    escapechar:Option<T::Char>
}
impl<'a,T:Token> TLVecMeaning<'a,T> {
    pub fn new(ls:&'a Vec<T>,int:&'a <T::CS as ControlSequenceName>::Handler,cc:&'a CategoryCodeScheme<T::Char>,escapechar:Option<T::Char>) -> Self {
        Self {
            ls,
            int,
            cc,
            escapechar
        }
    }
}
impl<'a,T:Token> Display for TLVecMeaning<'a,T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        meaning_fmt(self.ls.iter(),self.int,self.cc,self.escapechar,f,false)
    }
}

pub struct Tokenizer<'a,T:Token,F:FnMut(T)>(&'a mut F,PhantomData<T>);
impl<'a,T:Token,F:FnMut(T)> Tokenizer<'a,T,F> {
    #[inline(always)]
    pub fn new(f:&'a mut F) -> Self {
        Self(f,PhantomData)
    }
}
impl<'a,T:Token,F:FnMut(T)> std::fmt::Write for Tokenizer<'a,T,F> {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        // todo!("do not use bytes")
        for u in s.as_bytes() {
            if *u == b' ' { (self.0)(T::space()) }
            else {
                (self.0)(T::from_char_cat(T::Char::from(*u),CommandCode::Other))
            }
        }
        Ok(())
    }
}

impl<T:Token> From<Vec<T>> for TokenList<T> {
    fn from(value: Vec<T>) -> Self {
        Self(value.into())
    }
}

pub struct TokenListIterator<T:Token> {
    pub name:Option<PrimitiveIdentifier>,
    ls:TokenList<T>,
    index:usize
}
impl<T:Token> TokenListIterator<T> {
    #[inline(always)]
    pub fn give_back_maybe<M:MemoryManager<T>>(self,memory:&mut M) {
        self.ls.give_back_maybe(memory)
    }
    pub fn preview<W:Write>(&self, int:&<T::CS as ControlSequenceName>::Handler,
                            cc:&CategoryCodeScheme<T::Char>,
                            escapechar:Option<T::Char>,w:W) {
        meaning_fmt(self.ls.0[self.index..].iter(),int,cc,escapechar,w,false).unwrap()
    }
    pub fn new(name:Option<PrimitiveIdentifier>,ls:TokenList<T>) -> Self {
        Self {
            name,
            ls,
            index:0
        }
    }
    pub fn has_next(&self) -> bool {
        self.index < self.ls.0.len()
    }
}
impl<T:Token> Iterator for TokenListIterator<T> {
    type Item = T;
    fn next(&mut self) -> Option<Self::Item> {
        if self.index < self.ls.0.len() {
            let t = self.ls.get(self.index);
            self.index += 1;
            Some(t.clone())
        } else {
            None
        }
    }
}

pub struct MacroExpansion<T:Token> {
    ls:TokenList<T>,index:usize,currarg:Option<(usize,usize)>,args:[Vec<T>;9]
}
impl<T:Token> MacroExpansion<T> {
    #[inline(always)]
    pub fn give_back_maybe<M:MemoryManager<T>>(self,memory:&mut M) {
        self.ls.give_back_maybe(memory);
        for a in self.args {
            memory.return_token_vec(a);
        }
    }
    pub fn new(ls:TokenList<T>,args:[Vec<T>;9]) -> Self {
        Self {
            ls,
            index:0,
            currarg:None,
            args
        }
    }

    pub fn preview<W:Write>(&self, int:&<T::CS as ControlSequenceName>::Handler,
                            cc:&CategoryCodeScheme<T::Char>,
                            escapechar:Option<T::Char>,mut w:W) {
        let mut currarg = self.currarg;
        let mut index = self.index;
        loop {
            match currarg {
                Some((i,j)) if j < self.args[i].len() => {
                    self.args[i][j].display_fmt(int,cc,escapechar,&mut w);
                    currarg = Some((i,j+1));
                }
                Some(_) => currarg = None,
                None if index < self.ls.0.len() => {
                    let t = self.ls.get(index);
                    index += 1;
                    match t.is_argument_marker() {
                        Some(i) => currarg = Some((i.into(),0)),
                        _ => t.display_fmt(int,cc,escapechar,&mut w).unwrap()
                    }
                }
                _ => return
            }
        }
    }
    pub fn has_next(&mut self) -> bool {
        self.index < self.ls.0.len() || match self.currarg {
            Some((i,j)) => j < self.args[i].len(),
            _ => false
        }
    }
}
impl<T:Token> Iterator for MacroExpansion<T> {
    type Item = T;
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.currarg {
                Some((i,j)) if j < self.args[i].len() => {
                    let t = self.args[i][j].clone();
                    self.currarg = Some((i,j+1));
                    return Some(t)
                }
                Some(_) => self.currarg = None,
                None if self.index < self.ls.0.len() => {
                    let t = self.ls.get(self.index);
                    self.index += 1;
                    match t.is_argument_marker() {
                        Some(i) => self.currarg = Some((i.into(),0)),
                        _ => return Some(t.clone())
                    }
                }
                _ => return None
            }
        }
    }
}

pub struct NoParIterator<'a, T:Token,I:Iterator<Item=T>>(pub &'a mut I,&'a <T::CS as ControlSequenceName>::Handler);
impl<T:Token,I:Iterator<Item=T>> Iterator for NoParIterator<'_,T,I> {
    type Item = T;
    fn next(&mut self) -> Option<Self::Item> {
        match self.0.next() {
            Some(t) if t.is_cs(&self.1.par()) => todo!(),
            o => o
        }
    }
}
pub trait HasNoPar<T:Token>:Iterator<Item = T>+Sized {
    fn no_par<'a>(&'a mut self,handler:&'a <T::CS as ControlSequenceName>::Handler) -> NoParIterator<'a,T,Self> {
        NoParIterator(self,handler)
    }
}
impl<T:Token,I:Iterator<Item = T>> HasNoPar<T> for I {}

pub struct ExpansionContainer<T:Token>(Vec<T>);
impl<T:Token> ExpansionContainer<T> {
    #[inline(always)]
    pub fn push(&mut self,t:T) {
        self.0.push(t)
    }
    #[inline(always)]
    pub fn new(v:Vec<T>) -> Self {
        Self(v)
    }
    #[inline(always)]
    pub fn to_iter(self) -> TokenListIterator<T> {
        TokenListIterator::new(None,TokenList::from(self.0))
    }
}