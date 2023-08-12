use crate::engine::EngineType;
use crate::tex::token::Token;

const ARRAY_SIZE:usize = 1024;

pub struct Memory<ET:EngineType> {
    token_arrays:Vec<TokenArray<ET>>,
    token_array_vecs:Vec<Vec<TokenArray<ET>>>,
}
impl<ET:EngineType> Clone for Memory<ET> {
    fn clone(&self) -> Self { Self::new()}
}
impl<ET:EngineType> Memory<ET> {
    pub fn new() -> Self {
        Memory{token_arrays:Vec::with_capacity(32),token_array_vecs:Vec::with_capacity(32)}
    }
    pub fn get_token_array(&mut self) -> TokenArray<ET> {
        if let Some(a) = self.token_arrays.pop() {
            a
        } else {
            TokenArray {array:todo!()/*[None;ARRAY_SIZE]*/,index:0,max:0}
        }
    }
    pub fn return_token_array(&mut self, mut a: TokenArray<ET>) {
        a.reset();
        self.token_arrays.push(a);
    }
    pub fn get_expansion_container(&mut self) -> ExpansionContainer<ET> {
        let array = self.get_token_array();
        let vec = self.token_array_vecs.pop().unwrap_or(Vec::with_capacity(4));
        ExpansionContainer {current:array,former:vec}
    }
}

pub struct TokenArray<ET:EngineType>{array:[Option<(Token<ET>, bool)>;ARRAY_SIZE],index:usize,max:usize}
impl<ET:EngineType> TokenArray<ET> {
    pub fn reset(&mut self) { self.index = 0; self.max= 0; }
    pub fn has_next(&self) -> bool { self.index < self.max }
    pub fn get_next(&mut self) -> Option<(Token<ET>,bool)> {
        if self.index < self.max {
            let t = std::mem::take(&mut self.array[self.index]);
            self.index += 1;
            t
        } else { None }
    }
    pub fn push(&mut self,t:Token<ET>,expand:bool) -> bool {
        if self.index == 0 { false } else {
            self.index -= 1;
            self.array[self.index] = Some((t,expand));
            true
        }
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
    #[inline]
    pub fn consume<F,R>(mut self,memory:&mut Memory<ET>,mut f:F) where F:FnMut(TokenArray<ET>) {
        let mut current = self.current;
        loop {
            if current.max == 0 {
                memory.return_token_array(current);
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