use array_init::array_init;
use crate::engine::EngineType;
use crate::tex::token::Token;

const ARRAY_SIZE:usize = 1024;
const VEC_SIZE:usize = 256;

pub struct Memory<ET:EngineType> {
    token_arrays:Vec<TokenArray<ET>>,
    token_array_vecs:Vec<Vec<TokenArray<ET>>>,
    args:Option<[Vec<Token<ET>>;9]>,
    token_vecs:Vec<Vec<Token<ET>>>,
}
impl<ET:EngineType> Clone for Memory<ET> {
    fn clone(&self) -> Self { Self::new()}
}
impl<ET:EngineType> Memory<ET> {
    pub fn new() -> Self {
        Memory{
            token_arrays:Vec::with_capacity(VEC_SIZE),
            token_array_vecs:Vec::with_capacity(VEC_SIZE),
            args:Some(array_init(|_| Vec::with_capacity(VEC_SIZE))),
            token_vecs:vec!(Vec::with_capacity(VEC_SIZE),Vec::with_capacity(VEC_SIZE))
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
    pub fn return_token_vec(&mut self, mut v: Vec<Token<ET>>) {
        v.clear();
        self.token_vecs.push(v);
    }
    pub fn get_token_array(&mut self) -> TokenArray<ET> {
        if let Some(a) = self.token_arrays.pop() {
            a
        } else {
            TokenArray::new()
        }
    }
    pub fn return_token_array(&mut self, mut a: TokenArray<ET>) {
        a.reset();
        self.token_arrays.push(a);
    }
    pub fn get_expansion_container(&mut self) -> ExpansionContainer<ET> {
        let array = self.get_token_array();
        let vec = self.token_array_vecs.pop().unwrap_or(Vec::with_capacity(8));
        ExpansionContainer {current:array,former:vec}
    }
}

pub struct TokenArray<ET:EngineType>{array:[Option<(Token<ET>, bool)>;ARRAY_SIZE],index:usize,max:usize}
impl<ET:EngineType> TokenArray<ET> {
    pub fn new() -> Self { TokenArray {array:array_init(|_| None),index:3,max:3} }
    pub fn reset(&mut self) { self.index = 3; self.max= 3; } // so we have some room for requeueing
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