/*! A [`Gullet`] is the part of the TeX engine that reads tokens from the input stream,
    expands macros, and outputs a stream of primitives to be processed by a
    [`Stomach`](crate::engine::stomach::Stomach).
*/
pub mod methods;
pub mod numeric_methods;

use std::marker::PhantomData;
use crate::catch;
use crate::engine::{EngineMut, EngineType};
use crate::engine::gullet::methods::{do_conditional, EngineMutNoGullet};
use crate::engine::mouth::{Mouth, StandardMouth};
use crate::engine::state::State;
use crate::tex::ConditionalBranch;
use crate::tex::fonts::FontStore;
use crate::tex::numbers::{Skip, MuSkip};
use crate::tex::token::{BaseToken, Token};
use crate::tex::commands::{TokenCont, ResolvedToken, CommandSource, StomachCommand, BaseCommand};
use crate::tex::commands::etex::UNLESS;
use crate::tex::commands::methods::expand_def;
use crate::tex::commands::tex::{ELSE, FI};
use crate::utils::errors::TeXError;
use crate::utils::map::Map;
use crate::utils::strings::TeXStr;


/// The [`Gullet`] trait defines the interface for the part of the TeX engine that reads tokens from the input stream,
/// and expands macros. It has basically no components other than a [`Mouth`], so it is implemented as a trait
/// to allow for overriding its methods.
pub trait Gullet<ET:EngineType<Gullet=Self>>:Sized + Clone +'static {

    /// Expands [`Token`]s for as long as possible and returns the [`ResolvedToken`] for the next unexpandable [`Token`] encountered
    /// (or [`None`] if the [`Mouth`] is empty)
    fn get_next_unexpandable(&mut self,engine:&mut EngineMutNoGullet<ET>) -> Result<Option<ResolvedToken<ET>>,TeXError<ET>> {
        methods::get_next_unexpandable(&mut engine.join_gullet(self))
    }

    /// Returns the next primitive to be processed by the [`Stomach`](crate::engine::stomach::Stomach) from
    /// the input stream, after expanding macros as necessary.
    fn get_next_stomach_command(&mut self,engine:&mut EngineMutNoGullet<ET>) -> Result<Option<StomachCommand<ET>>,TeXError<ET>> {
        Ok(match self.get_next_unexpandable(engine)? {
            Some(rt) => Some(StomachCommand::from_resolved(rt)?),
            None => None
        })
    }

    /// Expands the given [`Token`], if expandable, by calling `f` on every element of its expansion and returns [`None`].
    /// If not expandable, returns the [`ResolvedToken`] for `tk`
    fn expand(&mut self,engine:&mut EngineMutNoGullet<ET>,ret:ResolvedToken<ET>) -> Result<Option<ResolvedToken<ET>>,TeXError<ET>>;

    /// Reads a number from the input stream.
    fn get_int(&mut self, engine:&mut EngineMutNoGullet<ET>) -> Result<ET::Int,TeXError<ET>> {
        numeric_methods::get_int::<ET>(&mut engine.join_gullet(self))
    }

    /// Reads a dimension from the input stream.
    fn get_dim(&mut self, engine:&mut EngineMutNoGullet<ET>) -> Result<ET::Dim,TeXError<ET>> {
        numeric_methods::get_dim::<ET>(&mut engine.join_gullet(self))
    }

    /// Reads a skip from the input stream.
    fn get_skip(&mut self, engine:&mut EngineMutNoGullet<ET>) -> Result<Skip<ET::SkipDim>,TeXError<ET>> {
        numeric_methods::get_skip::<ET>(&mut engine.join_gullet(self))
    }

    /// Reads a muskip from the input stream.
    fn get_muskip(&mut self, engine:&mut EngineMutNoGullet<ET>) -> Result<MuSkip<ET::MuDim,ET::MuStretchShrinkDim>,TeXError<ET>> {
        numeric_methods::get_muskip::<ET>(&mut engine.join_gullet(self))
    }

    /// read a single keyword from the input stream; returns `true` if the keyword is found.
    fn get_keyword<'a>(&mut self, engine:&mut EngineMutNoGullet<ET>, keyword:&'a str) -> Result<bool,TeXError<ET>> {
        methods::get_keyword::<ET>(&mut engine.join_gullet(self), keyword)
    }

    /// reads one of several optional keywords from the input stream;
    /// returns `None` if none of the keywords are found.
    fn get_keywords<'a>(&mut self, engine:&mut EngineMutNoGullet<ET>, mut keywords:Vec<&'a str>) -> Result<Option<&'a str>,TeXError<ET>> {
        methods::get_keywords::<ET>(&mut engine.join_gullet(self), keywords)
    }

    fn get_string(&mut self, engine:&mut EngineMutNoGullet<ET>) -> Result<String,TeXError<ET>> {
        methods::get_string::<ET>(&mut engine.join_gullet(self))
    }

    fn get_font(&mut self,engine:&mut EngineMutNoGullet<ET>) -> Result<ET::Font,TeXError<ET>> {
        methods::get_font::<ET>(&mut engine.join_gullet(self))
    }

    fn new_conditional(&mut self,name:&'static str) -> usize;
    fn set_conditional(&mut self,idx:usize,branch:ConditionalBranch);
    fn set_top_conditional(&mut self,branch:ConditionalBranch);
    fn pop_conditional(&mut self);
    fn current_conditional(&self) -> (Option<ConditionalBranch>,usize);
}

pub struct TeXGullet<ET:EngineType<Gullet=Self>> {
    in_conditionals:Vec<ConditionalBranch>,phantom:PhantomData<ET>
}
impl<ET:EngineType<Gullet=Self>> Clone for TeXGullet<ET> {
    fn clone(&self) -> Self { Self {
        in_conditionals: self.in_conditionals.clone(),phantom:PhantomData
    }}
}

impl<ET:EngineType<Gullet=Self>> TeXGullet<ET> {
    pub fn new() -> Self {
        Self {in_conditionals:Vec::new(),phantom:PhantomData}
    }
}
impl<ET:EngineType<Gullet=Self>> Gullet<ET> for TeXGullet<ET> {
    fn new_conditional(&mut self,name:&'static str) -> usize {
        let ret = self.in_conditionals.len();
        self.in_conditionals.push(ConditionalBranch::None(name));
        ret
    }
    fn set_conditional(&mut self,idx:usize,branch:ConditionalBranch) {
        // TODO throw error
        *self.in_conditionals.get_mut(idx).unwrap() = branch;
    }
    fn set_top_conditional(&mut self,branch:ConditionalBranch) {
        // TODO throw error
        *self.in_conditionals.last_mut().unwrap() = branch;
    }
    fn pop_conditional(&mut self) {
        // TODO throw error
        self.in_conditionals.pop();
    }
    fn current_conditional(&self) -> (Option<ConditionalBranch>,usize) {
        (self.in_conditionals.last().copied(),self.in_conditionals.len() - 1)
    }

    fn expand(&mut self, engine:&mut EngineMutNoGullet<ET>, ret: ResolvedToken<ET>) -> Result<Option<ResolvedToken<ET>>, TeXError<ET>> {
        let mut engine = engine.join_gullet(self);
        match ret.command {
            BaseCommand::Def(d) => {
                engine.add_expansion(|engine,rs| {
                    expand_def(&d,engine,ret.source,&mut |engine,t| Ok(rs.push(t,engine.memory)))?;
                    Ok(None)
                })
            }
            // expandable commands that do not expand to new tokens
            BaseCommand::Expandable { name, apply } if name == FI || name == ELSE || name == UNLESS => {
                apply(&mut engine, ret.source, &mut |_,_| Ok(()))?;
                Ok(None)
            }
            BaseCommand::Expandable {apply,..} => {
                engine.add_expansion(|engine,rs| {
                    apply(engine,ret.source,&mut |engine,t| Ok(rs.push(t,engine.memory)))?;
                    Ok(None)
                })
            },
            BaseCommand::Conditional {name,apply} => {
                do_conditional(&mut engine,ret.source, name,apply, false)?;
                Ok(None)
            }
            _ => Ok(Some(ret))
        }
    }
}