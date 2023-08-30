/*! A [`Gullet`] is the part of the TeX engine that reads tokens from the input stream,
    expands macros, and outputs a stream of primitives to be processed by a
    [`Stomach`](crate::engine::stomach::Stomach).
*/
pub mod methods;
pub mod numeric_methods;

use std::marker::PhantomData;
use crate::engine::{EngineRef, EngineType};
use crate::engine::gullet::methods::{do_conditional};
use crate::engine::mouth::Mouth;
use crate::tex::ConditionalBranch;
use crate::tex::numbers::{Skip, MuSkip};
use crate::tex::commands::{ResolvedToken, StomachCommand, BaseCommand};
use crate::tex::commands::methods::expand_def;
use crate::tex::fonts::FontStore;
use crate::utils::errors::TeXError;


/// The [`Gullet`] trait defines the interface for the part of the TeX engine that reads tokens from the input stream,
/// and expands macros. It has basically no components other than a [`Mouth`], so it is implemented as a trait
/// to allow for overriding its methods.
pub trait Gullet<ET:EngineType<Gullet=Self>>:Sized + Clone +'static {

    /// Expands [`Token`]s for as long as possible and returns the [`ResolvedToken`] for the next unexpandable [`Token`] encountered
    /// (or [`None`] if the [`Mouth`] is empty)
    fn get_next_unexpandable(engine:&mut EngineRef<ET>) -> Option<ResolvedToken<ET>> {
        methods::get_next_unexpandable(engine)
    }

    /// Expands [`Token`]s for as long as possible and returns the [`ResolvedToken`] for the next unexpandable [`Token`] encountered
    /// (or [`None`] if the [`Mouth`] is empty)
    fn get_next_unexpandable_same_file(engine:&mut EngineRef<ET>) -> Option<ResolvedToken<ET>> {
        methods::get_next_unexpandable_same_file(engine)
    }

    /// Returns the next primitive to be processed by the [`Stomach`](crate::engine::stomach::Stomach) from
    /// the input stream, after expanding macros as necessary.
    fn get_next_stomach_command(engine:&mut EngineRef<ET>) -> Option<StomachCommand<ET>> {
        match Self::get_next_unexpandable(engine) {
            Some(rt) => Some(StomachCommand::from_resolved(rt,&engine.interner)),
            None => None
        }
    }

    /// Expands the given [`Token`], if expandable, by calling `f` on every element of its expansion and returns [`None`].
    /// If not expandable, returns the [`ResolvedToken`] for `tk`
    fn expand(engine:&mut EngineRef<ET>, ret:ResolvedToken<ET>) -> Option<ResolvedToken<ET>>;

    /// Reads a number from the input stream.
    fn get_int(engine:&mut EngineRef<ET>) -> ET::Int {
        numeric_methods::get_int::<ET>(engine)
    }

    /// Reads a dimension from the input stream.
    fn get_dim(engine:&mut EngineRef<ET>) -> ET::Dim {
        numeric_methods::get_dim::<ET>(engine)
    }

    /// Reads a skip from the input stream.
    fn get_skip(engine:&mut EngineRef<ET>) -> Skip<ET::SkipDim> {
        numeric_methods::get_skip::<ET>(engine)
    }

    /// Reads a muskip from the input stream.
    fn get_muskip(engine:&mut EngineRef<ET>) -> MuSkip<ET::MuDim,ET::MuStretchShrinkDim> {
        numeric_methods::get_muskip::<ET>(engine)
    }

    /// read a single keyword from the input stream; returns `true` if the keyword is found.
    fn get_keyword<'a>(engine:&mut EngineRef<ET>, keyword:&'a str) -> bool {
        methods::get_keyword::<ET>(engine, keyword)
    }

    /// reads one of several optional keywords from the input stream;
    /// returns `None` if none of the keywords are found.
    fn get_keywords<'a>(engine:&mut EngineRef<ET>, keywords:Vec<&'a str>) -> Option<&'a str> {
        methods::get_keywords::<ET>(engine, keywords)
    }

    fn get_string(engine:&mut EngineRef<ET>, string:&mut String) {
        methods::get_string::<ET>(engine,string)
    }

    fn get_font(engine:&mut EngineRef<ET>) -> ET::FontRefType {
        methods::get_font::<ET>(engine)
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
        match self.in_conditionals.get_mut(idx) {
            None =>
                panic!("Conditional index out of bounds"),
            Some(r) => {
                *r = branch;
            }
        }
    }
    fn set_top_conditional(&mut self,branch:ConditionalBranch) {
        match self.in_conditionals.last_mut() {
            None =>
                panic!("Conditional index out of bounds"),
            Some(r) => {
                *r = branch;
            }
        }
    }
    fn pop_conditional(&mut self) {
        match self.in_conditionals.pop() {
            None =>
                panic!("Conditional index out of bounds"),
            Some(_) => ()
        }
    }
    fn current_conditional(&self) -> (Option<ConditionalBranch>,usize) {
        (self.in_conditionals.last().copied(),self.in_conditionals.len() - 1)
    }

    fn expand(engine:&mut EngineRef<ET>, ret: ResolvedToken<ET>) -> Option<ResolvedToken<ET>> {
        use crate::engine::mouth::MouthTrait;
        match ret.command {
            BaseCommand::Def(d) => {
                let mut rs = engine.mouth.get_expansion();
                expand_def(&d,engine,ret.source,&mut rs);
                engine.mouth.push_expansion_norev(rs);
                None
            }
            BaseCommand::ExpandableNoTokens {apply,..} => {
                apply(engine,ret.source);
                None
            }
            BaseCommand::Expandable {apply,..} => {
                let mut rs = engine.mouth.get_expansion();
                apply(engine,ret.source,&mut rs);
                engine.mouth.push_expansion(rs);
                None
            },
            BaseCommand::Conditional {name,apply} => {
                do_conditional(engine,ret.source, name,apply, false);
                None
            }
            _ => Some(ret)
        }
    }
}