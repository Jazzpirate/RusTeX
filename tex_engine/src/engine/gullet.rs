/*! A [`Gullet`] is the part of the TeX engine that reads tokens from the input stream,
    expands macros, and outputs a stream of primitives to be processed by a
    [`Stomach`](crate::engine::stomach::Stomach).
*/
pub mod methods;

use crate::catch;
use crate::engine::mouth::Mouth;
use crate::engine::state::State;
use crate::tex::commands::{GulletCommand, StomachCommand, StomachCommandInner};
use crate::tex::numbers::NumSet;
use crate::tex::token::{BaseToken, Token};
use crate::utils::errors::{ErrorInPrimitive, TeXError};
use crate::utils::map::Map;
use crate::utils::strings::TeXStr;

/// The [`Gullet`] trait defines the interface for the part of the TeX engine that reads tokens from the input stream,
/// and expands macros. It has basically no components other than a [`Mouth`], so it is implemented as a trait
/// to allow for overriding its methods.
pub trait Gullet<T:Token>:Sized+'static {
    type M:Mouth<T>;
    type S:State<T>;
    /// Returns a reference to the [`Mouth`].
    fn mouth(&mut self) -> &mut Self::M;

    fn switch_mouth(&mut self,tks:Vec<T>) -> Self::M;
    fn restore_mouth(&mut self,m:Self::M);

    /// Returns the next primitive to be processed by the [`Stomach`](crate::engine::stomach::Stomach) from
    /// the input stream, after expanding macros as necessary.
    fn get_next_stomach_command(&mut self, state: &mut Self::S) -> Result<Option<StomachCommand<T>>,Box<dyn TeXError<T>>> {
        while let Some((tk,b)) = self.mouth().get_next(state)? {
            if b {
                match catch!(methods::process_token_for_stomach(self, tk.clone(), state) => tk) {
                    Some(cmd) => return Ok(Some(cmd)),
                    None => ()
                }
            } else {
                return Ok(Some(StomachCommand{cause:tk,cmd:StomachCommandInner::Relax }))
            }
        }
        Ok(None)
    }

    /// Reads a number from the input stream.
    fn get_int(&mut self, state:&mut Self::S) -> Result<<<Self::S as State<T>>::NumSet as NumSet>::Int,Box<dyn TeXError<T>>> {
        methods::get_int(self, state)
    }

    /// Reads a dimension from the input stream.
    fn get_dim(&mut self, state:&mut Self::S) -> Result<<<Self::S as State<T>>::NumSet as NumSet>::Dim,Box<dyn TeXError<T>>> {
        methods::get_dim(self, state)
    }

    fn get_expanded_group(&mut self, state:&mut Self::S, expand_protected:bool, keep_the:bool, err_on_unknowns:bool) -> Result<Vec<T>,Box<dyn TeXError<T>>> {
        methods::get_expanded_group(self, state, expand_protected, keep_the, err_on_unknowns)
    }

    /// read a single keyword from the input stream; returns `true` if the keyword is found.
    fn get_keyword<'a>(&mut self, state:&mut Self::S, keyword:&'a str) -> Result<bool,Box<dyn TeXError<T>>> {
        methods::get_keyword(self, state, keyword)
    }

    /// reads one of several optional keywords from the input stream;
    /// returns `None` if none of the keywords are found.
    fn get_keywords<'a>(&mut self, state:&mut Self::S, mut keywords:Vec<&'a str>) -> Result<Option<&'a str>,Box<dyn TeXError<T>>> {
        methods::get_keywords(self, state, keywords)
    }

    fn get_control_sequence(&mut self, state:&mut Self::S) -> Result<BaseToken<T::Char>,Box<dyn TeXError<T>>> {
        methods::get_control_sequence(self, state)
    }

    fn get_string(&mut self,state:&mut Self::S) -> Result<TeXStr<T::Char>,Box<dyn TeXError<T>>> {
        methods::get_string(self, state)
    }

    fn new_conditional(&mut self) -> usize;
    fn close_conditional(&mut self);
    fn pop_conditional(&mut self);


    fn register_primitive(&mut self,name:&'static str,cmd:fn(&mut Self::S,&mut Self,GulletCommand<T>) -> Result<Vec<T>,ErrorInPrimitive<T>>) -> usize;
    fn register_conditional(&mut self,name:&'static str,cmd:fn(&mut Self::S,&mut Self,GulletCommand<T>) -> Result<bool,ErrorInPrimitive<T>>) -> usize;

    fn primitive(&self,idx:usize) -> Option<fn(&mut Self::S,&mut Self,GulletCommand<T>) -> Result<Vec<T>,ErrorInPrimitive<T>>>;
    fn primitive_from_name(&self,name:&'static str) -> Option<fn(&mut Self::S,&mut Self,GulletCommand<T>) -> Result<Vec<T>,ErrorInPrimitive<T>>>;

    fn conditional(&self,idx:usize) -> Option<fn(&mut Self::S,&mut Self,GulletCommand<T>) -> Result<bool,ErrorInPrimitive<T>>>;
    fn conditional_from_name(&self,name:&'static str) -> Option<fn(&mut Self::S,&mut Self,GulletCommand<T>) -> Result<bool,ErrorInPrimitive<T>>>;

    fn register_primitive_int(&mut self,name:&'static str,cmd:fn(&mut Self::S,&mut Self,GulletCommand<T>) -> Result<<<Self::S as State<T>>::NumSet as NumSet>::Int,ErrorInPrimitive<T>>) -> usize;
    fn primitive_int(&self,idx:usize) -> Option<fn(&mut Self::S,&mut Self,GulletCommand<T>) -> Result<<<Self::S as State<T>>::NumSet as NumSet>::Int,ErrorInPrimitive<T>>>;
    fn primitive_int_from_name(&self,name:&'static str) -> Option<fn(&mut Self::S,&mut Self,GulletCommand<T>) -> Result<<<Self::S as State<T>>::NumSet as NumSet>::Int,ErrorInPrimitive<T>>>;

    fn register_primitive_dim(&mut self,name:&'static str,cmd:fn(&mut Self::S,&mut Self,GulletCommand<T>) -> Result<<<Self::S as State<T>>::NumSet as NumSet>::Dim,ErrorInPrimitive<T>>) -> usize;
    fn primitive_dim(&self,idx:usize) -> Option<fn(&mut Self::S,&mut Self,GulletCommand<T>) -> Result<<<Self::S as State<T>>::NumSet as NumSet>::Dim,ErrorInPrimitive<T>>>;
    fn primitive_dim_from_name(&self,name:&'static str) -> Option<fn(&mut Self::S,&mut Self,GulletCommand<T>) -> Result<<<Self::S as State<T>>::NumSet as NumSet>::Dim,ErrorInPrimitive<T>>>;

    fn register_primitive_toks(&mut self,name:&'static str,cmd:fn(&mut Self::S,&mut Self,GulletCommand<T>) -> Result<Vec<T>,ErrorInPrimitive<T>>) -> usize;
    fn primitive_toks(&self,idx:usize) -> Option<fn(&mut Self::S,&mut Self,GulletCommand<T>) -> Result<Vec<T>,ErrorInPrimitive<T>>>;
    fn primitive_toks_from_name(&self,name:&'static str) -> Option<fn(&mut Self::S,&mut Self,GulletCommand<T>) -> Result<Vec<T>,ErrorInPrimitive<T>>>;
}

pub struct TeXGullet<T:Token,M:Mouth<T>,S:State<T>> {
    pub mouth:M,
    in_conditionals:Vec<bool>,
    conditionals:Map<fn(&mut S,&mut Self,GulletCommand<T>) -> Result<bool,ErrorInPrimitive<T>>>,
    primitives:Map<fn(&mut S,&mut Self,GulletCommand<T>) -> Result<Vec<T>,ErrorInPrimitive<T>>>,
    ints:Map<fn(&mut S,&mut Self,GulletCommand<T>) -> Result<<<S as State<T>>::NumSet as NumSet>::Int,ErrorInPrimitive<T>>>,
    dims:Map<fn(&mut S,&mut Self,GulletCommand<T>) -> Result<<<S as State<T>>::NumSet as NumSet>::Dim,ErrorInPrimitive<T>>>,
    toks:Map<fn(&mut S,&mut Self,GulletCommand<T>) -> Result<Vec<T>,ErrorInPrimitive<T>>>,
    phantom_char:std::marker::PhantomData<T>,
    phantom_state:std::marker::PhantomData<S>
}
impl<T:Token,M:Mouth<T>,S:State<T>> TeXGullet<T,M,S> {
    pub fn new(mouth:M) -> Self {
        Self {mouth,
            in_conditionals:Vec::new(),
            conditionals:Map::default(),
            primitives:Map::default(),
            ints:Map::default(),
            dims:Map::default(),
            toks:Map::default(),
            phantom_char:std::marker::PhantomData,
            phantom_state:std::marker::PhantomData
        }
    }
}
impl<T:Token,M:Mouth<T>,S:State<T>> Gullet<T> for TeXGullet<T,M,S> {
    type M = M;
    type S = S;
    fn switch_mouth(&mut self, tks: Vec<T>) -> Self::M {
        let old = std::mem::replace(&mut self.mouth, M::new());
        self.mouth.push_tokens(tks);
        old
    }
    fn restore_mouth(&mut self, m: Self::M) {
        self.mouth = m
    }
    fn mouth(&mut self) -> &mut M { &mut self.mouth }
    fn register_primitive(&mut self, name: &'static str, cmd: fn(&mut Self::S, &mut Self, GulletCommand<T>) -> Result<Vec<T>, ErrorInPrimitive<T>>) -> usize {
        self.primitives.insert(name,cmd)
    }
    fn register_conditional(&mut self, name: &'static str, cmd: fn(&mut Self::S, &mut Self, GulletCommand<T>) -> Result<bool, ErrorInPrimitive<T>>) -> usize {
        self.conditionals.insert(name,cmd)
    }
    fn register_primitive_int(&mut self, name: &'static str, cmd: fn(&mut Self::S, &mut Self, GulletCommand<T>) -> Result<<<Self::S as State<T>>::NumSet as NumSet>::Int, ErrorInPrimitive<T>>) -> usize {
        self.ints.insert(name,cmd)
    }
    fn register_primitive_dim(&mut self, name: &'static str, cmd: fn(&mut Self::S, &mut Self, GulletCommand<T>) -> Result<<<Self::S as State<T>>::NumSet as NumSet>::Dim, ErrorInPrimitive<T>>) -> usize {
        self.dims.insert(name,cmd)
    }
    fn register_primitive_toks(&mut self, name: &'static str, cmd: fn(&mut Self::S, &mut Self, GulletCommand<T>) -> Result<Vec<T>, ErrorInPrimitive<T>>) -> usize {
        self.toks.insert(name,cmd)
    }
    fn primitive(&self, idx: usize) -> Option<fn(&mut Self::S, &mut Self, GulletCommand<T>) -> Result<Vec<T>, ErrorInPrimitive<T>>> {
        self.primitives.get(idx).copied()
    }
    fn primitive_from_name(&self, name: &'static str) -> Option<fn(&mut Self::S, &mut Self, GulletCommand<T>) -> Result<Vec<T>, ErrorInPrimitive<T>>> {
        self.primitives.get_from_name(name).copied()
    }
    fn conditional(&self, idx: usize) -> Option<fn(&mut Self::S, &mut Self, GulletCommand<T>) -> Result<bool, ErrorInPrimitive<T>>> {
        self.conditionals.get(idx).copied()
    }
    fn conditional_from_name(&self, name: &'static str) -> Option<fn(&mut Self::S, &mut Self, GulletCommand<T>) -> Result<bool, ErrorInPrimitive<T>>> {
        self.conditionals.get_from_name(name).copied()
    }
    fn primitive_int(&self, idx: usize) -> Option<fn(&mut Self::S, &mut Self, GulletCommand<T>) -> Result<<<Self::S as State<T>>::NumSet as NumSet>::Int, ErrorInPrimitive<T>>> {
        self.ints.get(idx).copied()
    }
    fn primitive_int_from_name(&self, name: &'static str) -> Option<fn(&mut Self::S, &mut Self, GulletCommand<T>) -> Result<<<Self::S as State<T>>::NumSet as NumSet>::Int, ErrorInPrimitive<T>>> {
        self.ints.get_from_name(name).copied()
    }
    fn primitive_dim(&self, idx: usize) -> Option<fn(&mut Self::S, &mut Self, GulletCommand<T>) -> Result<<<Self::S as State<T>>::NumSet as NumSet>::Dim, ErrorInPrimitive<T>>> {
        self.dims.get(idx).copied()
    }
    fn primitive_dim_from_name(&self, name: &'static str) -> Option<fn(&mut Self::S, &mut Self, GulletCommand<T>) -> Result<<<Self::S as State<T>>::NumSet as NumSet>::Dim, ErrorInPrimitive<T>>> {
        self.dims.get_from_name(name).copied()
    }
    fn primitive_toks(&self, idx: usize) -> Option<fn(&mut Self::S, &mut Self, GulletCommand<T>) -> Result<Vec<T>, ErrorInPrimitive<T>>> {
        self.toks.get(idx).copied()
    }
    fn primitive_toks_from_name(&self, name: &'static str) -> Option<fn(&mut Self::S, &mut Self, GulletCommand<T>) -> Result<Vec<T>, ErrorInPrimitive<T>>> {
        self.toks.get_from_name(name).copied()
    }


    fn new_conditional(&mut self) -> usize {
        let ret = self.in_conditionals.len();
        self.in_conditionals.push(false);
        ret
    }
    fn close_conditional(&mut self) {
        // TODO throw error
        *self.in_conditionals.last_mut().unwrap() = true;
    }
    fn pop_conditional(&mut self) {
        // TODO throw error
        self.in_conditionals.pop();
    }
}