
use array_init::array_init;
use log::error;
use string_interner::{StringInterner};
use crate::engine::EngineType;
use crate::tex::commands::tex::NOEXPAND_INTERNAL;
use crate::tex::token::Token;
use crate::utils::collections::HMap;
use crate::utils::strings::{CharType, TeXStr};

pub type Symbol = string_interner::symbol::SymbolU32;
type Backend = string_interner::backend::StringBackend<Symbol>; // string_interner::backend::StringBackend<Symbol>;


pub const VEC_SIZE:usize = 32;
/*
pub struct MyInterner<Char:CharType> {
    ends:Vec<usize>,
    buffer:Vec<Char>,
    hasher:ahash::AHasher,
    map:HMap<u32,()>
}
impl<Char:CharType> MyInterner<Char> {
    pub fn new() -> Self {
        MyInterner {
            ends:Vec::with_capacity(4096),
            buffer:Vec::with_capacity(16*4096),
            hasher:ahash::AHasher::default(),
            map:HMap::default()
        }
    }
    pub fn get_or_intern(&mut self,v:&Vec<Char>) -> u32 {
        let hash = make_hash(self.hasher, string.as_ref());
        let entry = self.map.raw_entry_mut().from_hash(hash, |symbol| {
            // SAFETY: This is safe because we only operate on symbols that
            //         we receive from our backend making them valid.
            string == unsafe { backend.resolve_unchecked(*symbol) }
        });
    }
}

 */

#[derive(Clone)]
pub struct Interner {
    interner:StringInterner<Backend,ahash::RandomState>,
    pub relax:TeXStr,
    pub par:TeXStr,
    pub empty_str:TeXStr,
    pub noexpand_tk:TeXStr
}
impl Interner {
    pub fn new() -> Self {
        let mut interner = StringInterner::<Backend,ahash::RandomState>::new();
        Interner {
            relax:TeXStr::from_primitive(interner.get_or_intern_static("relax")),
            par:TeXStr::from_primitive(interner.get_or_intern_static("par")),
            empty_str:TeXStr::from_primitive(interner.get_or_intern_static("")),
            noexpand_tk:TeXStr::from_primitive(interner.get_or_intern_static(NOEXPAND_INTERNAL)),
            interner
        }
    }
    pub fn resolve(&self,symbol:Symbol) -> &str {
        self.interner.resolve(symbol).unwrap()
    }
    pub fn from_static(&mut self,s:&'static str) -> TeXStr {
        TeXStr::from_primitive(self.interner.get_or_intern_static(s))
    }
    pub fn from_string(&mut self,s:&str) -> TeXStr {
        TeXStr::from_primitive(self.interner.get_or_intern(s))
    }
}
#[derive(Clone)]
pub struct Memory<ET:EngineType> {
    args:Option<[Vec<ET::Token>;9]>,
    token_vecs:Vec<Vec<ET::Token>>,
    strings:Vec<String>,
}

impl<ET:EngineType> Memory<ET> {
    pub fn print_stats(&self) {
        println!("args:");
        for a in self.args.as_ref().unwrap().iter() {
            println!(" -  {}",a.capacity());
        }
        println!("token_vecs: {}",self.token_vecs.len());
        for a in self.token_vecs.iter() {
            println!(" -  {}",a.capacity());
        }
        println!("strings: {}",self.strings.len());
        for s in self.strings.iter() {
            println!(" -  {}",s.capacity());
        }
    }
    pub fn new() -> Self {
        let token_vecs = (0..32).map(|_| Vec::with_capacity(2048)).collect();
        Memory{
            args:Some(array_init(|_| Vec::with_capacity(2097152))),strings:(0..8).map(|_|String::with_capacity(4096)).collect(),token_vecs
        }
    }
    pub fn get_args(&mut self) -> [Vec<ET::Token>;9] {
        self.args.take().unwrap()
    }
    pub fn return_args(&mut self, mut args:[Vec<ET::Token>;9]) {
        /*for a in args.iter_mut() {
            a.clear();
            //a.shrink_to(VEC_SIZE)
        }*/
        self.args = Some(args);
    }
    pub fn get_token_vec(&mut self) -> Vec<ET::Token> {
        self.token_vecs.pop().unwrap_or(Vec::with_capacity(VEC_SIZE))
        //Vec::with_capacity(VEC_SIZE)
    }
    pub fn get_string(&mut self) -> String {
        self.strings.pop().unwrap_or(String::with_capacity(64))
    }
    pub fn return_string(&mut self, mut s:String) {
        s.clear();
        //s.shrink_to(VEC_SIZE);
        self.strings.push(s);
    }
    pub fn return_token_vec(&mut self, mut v: Vec<ET::Token>) {
        v.clear();
        //v.shrink_to(VEC_SIZE);
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

}


/*
pub struct TokenArray<ET:EngineType>{array:[Option<(ET::Token, bool)>;ARRAY_SIZE],index:usize,max:usize}
impl<ET:EngineType> Clone for TokenArray<ET> {
    fn clone(&self) -> Self { Self::new()}
}
impl<ET:EngineType> TokenArray<ET> {
    pub fn new() -> Self { TokenArray {array:array_init(|_| None),index:OFFSET,max:OFFSET} }
    pub fn reset(&mut self) { self.index = OFFSET; self.max= OFFSET; } // so we have some room for requeueing
    pub fn has_next(&self) -> bool { self.index < self.max }
    pub fn get_next(&mut self) -> Option<(ET::Token,bool)> {
        if self.index < self.max {
            let t = std::mem::take(&mut self.array[self.index]);
            self.index += 1;
            t
        } else { None }
    }
    pub fn push(&mut self,t:ET::Token,expand:bool) -> Result<(),ET::Token> {
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
    pub fn push(&mut self, t:ET::Token,memory:&mut Memory<ET>) {
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