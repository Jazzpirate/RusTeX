use std::fmt::{Arguments, Display, Write};
use std::marker::PhantomData;
use crate::engine::utils::memory::{MemoryManager, PrimitiveIdentifier};
use crate::tex::catcodes::{CategoryCode, CommandCode};
use crate::tex::control_sequences::{ControlSequenceName, ControlSequenceNameHandler, ResolvedCSName};
use crate::tex::token::{StandardToken, Token};
use crate::tex::catcodes::CategoryCodeScheme;
use crate::tex::input_text::Character;
use crate::tex::input_text::CharacterMap;

#[derive(Clone,Debug)]
pub struct TokenList<T:Token>(pub shared_vector::SharedVector<T>);
impl<T:Token> PartialEq for TokenList<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}
impl<T:Token> TokenList<T> {
    #[inline(always)]
    pub fn inner(&self) -> &[T] { &*self.0 }
    pub fn give_back_maybe<M:MemoryManager<T>>(self,_memory:&mut M) {
        /*match std::rc::Rc::try_unwrap(self.0) {
            Ok(sl) => memory.return_token_vec(sl.into_vec()),
            _ => ()
        }*/
    }
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
    #[inline(always)]
    pub fn get(&self,i:usize) -> &T {
        &(*self.0)[i]
    }

    pub fn displayable<'a>(&'a self,int:&'a <T::CS as ControlSequenceName<T::Char>>::Handler,cc:&'a CategoryCodeScheme<T::Char>,escapechar:Option<T::Char>) -> TLMeaning<'a,T> {
        TLMeaning {
            ls:self,
            int,
            cc,
            escapechar
        }
    }
    pub fn meaning_char<W:WriteChars<T::Char,T::CS>>(&self, int:&<T::CS as ControlSequenceName<T::Char>>::Handler, cc:&CategoryCodeScheme<T::Char>, escapechar:Option<T::Char>, f: W,double_par:bool) {
        meaning_char(self.0.iter(),int,cc,escapechar,f,double_par).unwrap();
    }
}

pub fn meaning_tk<'a,T:Token,W:WriteChars<T::Char,T::CS>>(t:T, int:&<T::CS as ControlSequenceName<T::Char>>::Handler, cc:&CategoryCodeScheme<T::Char>, escapechar:Option<T::Char>, mut f: W,double_par:bool) {
    match t.is_argument_marker() {
        Some(i) => write!(f,"#{}",(i + 1)).unwrap(),
        _ => {
            match t.to_enum() {
                StandardToken::Character(c,CommandCode::Parameter) if double_par => {
                    f.push_char(c);
                    f.push_char(c)
                }
                StandardToken::Character(_,CommandCode::Space) => f.push_char(b' '.into()),
                StandardToken::Character(c,_) => f.push_char(c),
                StandardToken::ControlSequence(cs) =>
                    f.push_cs(cs,int,cc,escapechar)
            }
        }
    }
}

pub fn meaning_char<'a,T:Token,I:Iterator<Item=&'a T>,W:WriteChars<T::Char,T::CS>>(iter:I, int:&<T::CS as ControlSequenceName<T::Char>>::Handler, cc:&CategoryCodeScheme<T::Char>, escapechar:Option<T::Char>, mut f: W,double_par:bool) -> std::fmt::Result {
    for t in iter { meaning_tk(t.clone(),int,cc,escapechar,&mut f,double_par) }
    Ok(())
}
pub fn meaning_fmt<'a,T:Token,I:Iterator<Item=&'a T>>(iter:I, int:&<T::CS as ControlSequenceName<T::Char>>::Handler, cc:&CategoryCodeScheme<T::Char>, escapechar:Option<T::Char>, f: &mut std::fmt::Formatter<'_>,double_par:bool) {
    let s = Stringify::<T::Char,T::CS>::new(f);
    meaning_char(iter,int,cc,escapechar,s,double_par).unwrap();
}

pub struct TLMeaning<'a,T:Token> {
    ls:&'a TokenList<T>,
    int:&'a <T::CS as ControlSequenceName<T::Char>>::Handler,
    cc:&'a CategoryCodeScheme<T::Char>,
    escapechar:Option<T::Char>
}
impl<'a,T:Token> Display for TLMeaning<'a,T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let i =self.ls.0.iter();
        meaning_fmt(i,self.int,self.cc,self.escapechar,f,false);
        Ok(())
    }
}

pub struct TLVecMeaning<'a,T:Token> {
    ls:&'a Vec<T>,
    int:&'a <T::CS as ControlSequenceName<T::Char>>::Handler,
    cc:&'a CategoryCodeScheme<T::Char>,
    escapechar:Option<T::Char>
}
impl<'a,T:Token> TLVecMeaning<'a,T> {
    pub fn new(ls:&'a Vec<T>,int:&'a <T::CS as ControlSequenceName<T::Char>>::Handler,cc:&'a CategoryCodeScheme<T::Char>,escapechar:Option<T::Char>) -> Self {
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
        meaning_fmt(self.ls.iter(),self.int,self.cc,self.escapechar,f,false);
        Ok(())
    }
}
pub trait WriteChars<C:Character,CS:ControlSequenceName<C>>: std::fmt::Write {
    fn push_char(&mut self,c:C);
    fn push_cs<I:ControlSequenceNameHandler<C,CS>>(&mut self,cs:CS,int:&I,cc:&CategoryCodeScheme<C>,esc:Option<C>);
}
impl<'a,C:Character,CS:ControlSequenceName<C>,A:WriteChars<C,CS>> WriteChars<C,CS> for &'a mut A {
    fn push_char(&mut self, c: C) { (*self).push_char(c) }
    fn push_cs<I:ControlSequenceNameHandler<C,CS>>(&mut self,cs:CS,int:&I,cc:&CategoryCodeScheme<C>,esc:Option<C>) {
        (*self).push_cs(cs,int,cc,esc)
    }
}
pub struct Stringify<'a,'b,C:Character,CS:ControlSequenceName<C>>(&'a mut std::fmt::Formatter<'b>,PhantomData<C>,PhantomData<CS>);
impl <'a,'b,C:Character,CS:ControlSequenceName<C>> Stringify<'a,'b,C,CS> {
    #[inline(always)]
    pub fn new(f:&'a mut std::fmt::Formatter<'b>) -> Self {
        Self(f,PhantomData,PhantomData)
    }
}
impl<'a,'b,C:Character,CS:ControlSequenceName<C>> std::fmt::Write for Stringify<'a,'b,C,CS> {
    #[inline(always)]
    fn write_char(&mut self, c: char) -> std::fmt::Result {
        self.0.write_char(c)
    }
    #[inline(always)]
    fn write_fmt(&mut self, args: Arguments<'_>) -> std::fmt::Result {
        self.0.write_fmt(args)
    }
    #[inline(always)]
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        self.0.write_str(s)
    }
}
impl<'a,'b,C:Character,CS:ControlSequenceName<C>> WriteChars<C,CS> for Stringify<'a,'b,C,CS> {
    fn push_char(&mut self, c: C) {
        c.display(self.0)
    }
    fn push_cs<I:ControlSequenceNameHandler<C,CS>>(&mut self,cs:CS,int:&I,cc:&CategoryCodeScheme<C>,esc:Option<C>) {
        let res = int.resolve(&cs);
        //let str = res.as_ref();
        write!(self, "{}{}", C::displayable_opt(esc), res).unwrap();
        if res.len() == 1 {
            let c = res.iter().next().unwrap();
            match cc.get(c) {
                CategoryCode::Letter => self.write_char(' ').unwrap(),
                _ => ()
            }
        } else {
            self.write_char(' ').unwrap()
        }
    }
}

pub struct Tokenizer<'a,T:Token,F:FnMut(T)>(&'a mut F,PhantomData<T>);
impl<'a,T:Token,F:FnMut(T)> Tokenizer<'a,T,F> {
    #[inline(always)]
    pub fn new(f:&'a mut F) -> Self {
        Self(f,PhantomData)
    }
}

impl<'a,T:Token,F:FnMut(T)> WriteChars<T::Char,T::CS> for Tokenizer<'a,T,F> {
    fn push_char(&mut self, c:T::Char) { (self.0)(T::from_char_cat(c, CommandCode::Other)) }
    fn push_cs<I:ControlSequenceNameHandler<T::Char,T::CS>>(&mut self,cs:T::CS,int:&I,cc:&CategoryCodeScheme<T::Char>,esc:Option<T::Char>) {
        if let Some(e) = esc {
            (self.0)(T::from_char_cat(e, CommandCode::Other));
        }
        let res = int.resolve(&cs);
        for c in res.iter() {
            if matches!(c.try_into(),Ok(b' ')) {
                (self.0)(T::space());
            } else {
                (self.0)(T::from_char_cat(c, CommandCode::Other));
            }
        }
        if res.len() == 1 {
            let c = res.iter().next().unwrap();
            match cc.get(c) {
                CategoryCode::Letter => (self.0)(T::space()),
                _ => ()
            }
        } else {
            (self.0)(T::space())
        }
    }
}
impl<'a,T:Token,F:FnMut(T)> std::fmt::Write for Tokenizer<'a,T,F> {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        for u in T::Char::string_to_iter(s) {
            if matches!(u.try_into(),Ok(b' ')) { (self.0)(T::space()) }
            else {
                (self.0)(T::from_char_cat(u,CommandCode::Other))
            }
        }
        Ok(())
    }
}

impl<T:Token> From<shared_vector::Vector<T>> for TokenList<T> {
    fn from(value: shared_vector::Vector<T>) -> Self {
        Self(value.into())
    }
}

pub struct TokenListIterator<T:Token> {
    pub name:Option<PrimitiveIdentifier>,
    pub ls:TokenList<T>,
    index:usize
}
impl<T:Token> TokenListIterator<T> {
    #[inline(always)]
    pub fn give_back_maybe<M:MemoryManager<T>>(self,memory:&mut M) {
        self.ls.give_back_maybe(memory)
    }
    /*pub fn preview<W:Write>(&self, int:&<T::CS as ControlSequenceName<T::Char>>::Handler,
                            cc:&CategoryCodeScheme<T::Char>,
                            escapechar:Option<T::Char>,w:W) {
        meaning_fmt(self.ls.0[self.index..].iter(),int,cc,escapechar,w,false).unwrap()
    }*/
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
        let r = self.ls.0.get(self.index).cloned();
        self.index += 1;
        r
    }
}

pub type TokenVecIterator<T> = std::vec::IntoIter<T>;

pub struct MacroExpansion<T:Token> {
    pub ls:TokenList<T>,index:usize,currarg:Option<(usize,usize)>,pub args:[Vec<T>;9]
}
impl<T:Token> MacroExpansion<T> {
    pub fn consume_rev(&mut self, v:&mut Vec<T>) {
        for t in self.ls.0.iter().rev() {
            if let Some(i) = t.is_argument_marker() {
                for t in self.args[i as usize].iter().rev() {
                    v.push(t.clone());
                }
            } else {
                v.push(t.clone());
            }
        }
    }
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

    pub fn preview<W:Write>(&self, int:&<T::CS as ControlSequenceName<T::Char>>::Handler,
                            cc:&CategoryCodeScheme<T::Char>,
                            escapechar:Option<T::Char>,mut w:W) {
        let mut currarg = self.currarg;
        let mut index = self.index;
        loop {
            match currarg {
                Some((i,j)) if j < self.args[i].len() => {
                    self.args[i][j].display_fmt(int,cc,escapechar,&mut w).unwrap();
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
            if let Some((i, j)) = self.currarg {
                if let Some(t) = self.args[i].get(j) {
                    self.currarg = Some((i, j + 1));
                    return Some(t.clone());
                } else {
                    self.currarg = None;
                }
            }
            match self.ls.0.get(self.index) {
                None => return None,
                Some(t) => {
                    self.index += 1;
                    if let Some(i) = t.is_argument_marker() {
                        self.currarg = Some((i.into(), 0));
                    } else {
                        return Some(t.clone());
                    }
                }
            }
        }
    }
}

pub struct ExpansionContainer<T:Token>(shared_vector::Vector<T>);
impl<T:Token> ExpansionContainer<T> {
    #[inline(always)]
    pub fn push(&mut self,t:T) {
        self.0.push(t)
    }
    #[inline(always)]
    pub fn new() -> Self {
        Self(shared_vector::Vector::new())
    }
    #[inline(always)]
    pub fn to_iter(self) -> TokenListIterator<T> {
        TokenListIterator::new(None,TokenList::from(self.0))
    }
}