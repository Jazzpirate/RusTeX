use std::hash::BuildHasher;
use array_init::array_init;
use log::error;
use string_interner::backend::BufferBackend;
use string_interner::StringInterner;
use crate::engine::EngineType;
use crate::tex::token::Token;
use crate::utils::strings::TeXStr;

const VEC_SIZE:usize = 8;

pub struct Memory<ET:EngineType> {
    args:Option<[Vec<Token<ET>>;9]>,
    token_vecs:Vec<Vec<Token<ET>>>,
    strings:Vec<String>,
    pub interner: StringInterner<BufferBackend,ahash::RandomState>,
    pub relax:TeXStr<ET::Char>,
    pub par:TeXStr<ET::Char>,
    pub empty_str:TeXStr<ET::Char>,
}
impl<ET:EngineType> Clone for Memory<ET> {
    fn clone(&self) -> Self { Self::new_with(self.interner.clone())}
}
impl<ET:EngineType> Memory<ET> {
    pub fn print_stats(&self) {
        error!("args: {}",self.args.as_ref().unwrap().len());
        for a in self.args.as_ref().unwrap().iter() {
            error!(" -  {}",a.capacity());
        }
        error!("token_vecs: {}",self.token_vecs.len());
        for a in self.token_vecs.iter() {
            error!(" -  {}",a.capacity());
        }
    }
    pub fn new_with(mut interner:StringInterner<BufferBackend,ahash::RandomState>) -> Self {
        let token_vecs = (0..32).map(|_| Vec::with_capacity(VEC_SIZE)).collect();
        let relax = TeXStr::from_primitive(interner.get_or_intern_static("relax"));
        let par = TeXStr::from_primitive(interner.get_or_intern_static("par"));
        let empty_str = TeXStr::from_primitive(interner.get_or_intern_static(""));
        Memory{
            args:Some(array_init(|_| Vec::with_capacity(VEC_SIZE))),strings:Vec::with_capacity(4),token_vecs,interner,relax,par,empty_str
        }
    }
    pub fn new() -> Self {
        let token_vecs = (0..32).map(|_| Vec::with_capacity(VEC_SIZE)).collect();
        let mut interner = StringInterner::<BufferBackend,ahash::RandomState>::new();
        let relax = TeXStr::from_primitive(interner.get_or_intern_static("relax"));
        let par = TeXStr::from_primitive(interner.get_or_intern_static("par"));
        let empty_str = TeXStr::from_primitive(interner.get_or_intern_static(""));
        Memory{
            args:Some(array_init(|_| Vec::with_capacity(VEC_SIZE))),strings:Vec::with_capacity(4),token_vecs,interner,relax,par,empty_str
        }
    }
    pub fn get_args(&mut self) -> [Vec<Token<ET>>;9] {
        std::mem::take(&mut self.args).unwrap()
    }
    pub fn return_args(&mut self, mut args:[Vec<Token<ET>>;9]) {
        self.args = Some(args);
    }
    pub fn get_token_vec(&mut self) -> Vec<Token<ET>> {
        self.token_vecs.pop().unwrap_or(Vec::with_capacity(VEC_SIZE))
    }
    pub fn get_string(&mut self) -> String {
        self.strings.pop().unwrap_or(String::new())
    }
    pub fn return_string(&mut self, mut s:String) {
        s.clear();
        self.strings.push(s);
    }
    pub fn return_token_vec(&mut self, mut v: Vec<Token<ET>>) {
        v.clear();
        self.token_vecs.push(v);
    }
    /*pub fn get_token_array(&mut self) -> TokenArray<ET> {
        if let Some(a) = self.token_arrays.pop() {
            a
        } else {
            TokenArray::new()
        }
    }*/
    /*pub fn return_token_array(&mut self, mut a: TokenArray<ET>) {
        a.reset();
        self.token_arrays.push(a);
    }*/
    pub fn get_expansion_container(&mut self) -> ExpansionContainer<ET> {
        let array = self.token_vecs.pop().unwrap_or(Vec::with_capacity(VEC_SIZE));
        ExpansionContainer {array}
    }
}

static OFFSET: usize = 5;

#[derive(Clone)]
pub struct TokenArray<ET:EngineType>{array:Vec<(Token<ET>,bool)>}
pub struct ExpansionContainer<ET:EngineType>{array:Vec<Token<ET>>}
impl<ET:EngineType> ExpansionContainer<ET> {
    pub fn push(&mut self, t:Token<ET>,memory:&mut Memory<ET>) {
        self.array.push(t);
    }
    pub fn reset(&mut self,memory:&mut Memory<ET>) {
        self.array.clear();
    }
    #[inline]
    pub fn consume<F,R>(mut self,memory:&mut Memory<ET>,mut f:F) where F:FnMut((Token<ET>,bool)) {
        for t in self.array.drain(..).rev() {
            f((t,true));
        }
        memory.return_token_vec(self.array);
    }
}

/*
pub struct TokenArray<ET:EngineType>{array:[Option<(Token<ET>, bool)>;ARRAY_SIZE],index:usize,max:usize}
impl<ET:EngineType> Clone for TokenArray<ET> {
    fn clone(&self) -> Self { Self::new()}
}
impl<ET:EngineType> TokenArray<ET> {
    pub fn new() -> Self { TokenArray {array:array_init(|_| None),index:OFFSET,max:OFFSET} }
    pub fn reset(&mut self) { self.index = OFFSET; self.max= OFFSET; } // so we have some room for requeueing
    pub fn has_next(&self) -> bool { self.index < self.max }
    pub fn get_next(&mut self) -> Option<(Token<ET>,bool)> {
        if self.index < self.max {
            let t = std::mem::take(&mut self.array[self.index]);
            self.index += 1;
            t
        } else { None }
    }
    pub fn push(&mut self,t:Token<ET>,expand:bool) -> Result<(),Token<ET>> {
        if self.index == 0 {
            Err(t)
        } else {
            self.index -= 1;
            self.array[self.index] = Some((t,expand));
            Ok(())
        }
    }
    pub fn preview(&self) -> String { // TODO memory
        self.array[self.index..self.max].iter().map(|t| match t {
            Some((t,_)) => t.to_string(),
            None => String::from("")
        }).collect::<Vec<String>>().join(" ")
    }
}
pub struct ExpansionContainer<ET:EngineType>{current: TokenArray<ET>,former:Vec<TokenArray<ET>>}
impl<ET:EngineType> ExpansionContainer<ET> {
    pub fn push(&mut self, t:Token<ET>,memory:&mut Memory<ET>) {
        if self.current.max == ARRAY_SIZE {
            self.former.push(std::mem::replace(&mut self.current,memory.get_token_array()));
        }
        self.current.array[self.current.max] = Some((t,true));
        self.current.max += 1;
    }
    pub fn reset(&mut self,memory:&mut Memory<ET>) {
        self.current.reset();
        for mut former in self.former.drain(..) {
            memory.return_token_array(former);
        }
    }
    #[inline]
    pub fn consume<F,R>(mut self,memory:&mut Memory<ET>,mut f:F) where F:FnMut(TokenArray<ET>) {
        let mut current = self.current;
        loop {
            if current.max == current.index {
                memory.return_token_array(current);
                for r in self.former {memory.return_token_array(r);}
                break;
            } else {
                match self.former.pop() {
                    Some(mut former) => {
                        std::mem::swap(&mut current,&mut former);
                        f(former);
                    },
                    None => { f(current); break; }
                }
            }
        }
    }
}

 */