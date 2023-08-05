/*! A [`Gullet`] is the part of the TeX engine that reads tokens from the input stream,
    expands macros, and outputs a stream of primitives to be processed by a
    [`Stomach`](crate::engine::stomach::Stomach).
*/
pub mod methods;
pub mod numeric_methods;

use crate::catch;
use crate::engine::EngineType;
use crate::engine::mouth::Mouth;
use crate::engine::state::State;
use crate::tex::ConditionalBranch;
use crate::tex::fonts::FontStore;
use crate::tex::numbers::{Skip, MuSkip};
use crate::tex::token::{BaseToken, Token};
use crate::tex::commands::{TokenCont, ResolvedToken, CommandSource, StomachCommand};
use crate::utils::errors::TeXError;
use crate::utils::map::Map;
use crate::utils::strings::TeXStr;


/// The [`Gullet`] trait defines the interface for the part of the TeX engine that reads tokens from the input stream,
/// and expands macros. It has basically no components other than a [`Mouth`], so it is implemented as a trait
/// to allow for overriding its methods.
pub trait Gullet<ET:EngineType<Gullet=Self>>:Sized + Clone +'static {
    /// Returns a reference to the [`Mouth`].
    fn mouth(&mut self) -> &mut ET::Mouth;

    fn switch_mouth(&mut self,tks:Vec<ET::Token>) -> ET::Mouth;
    fn restore_mouth(&mut self,mouth:ET::Mouth);

    /// Expands [`Token`]s for as long as possible and returns the [`ResolvedToken`] for the next unexpandable [`Token`] encountered
    /// (or [`None`] if the [`Mouth`] is empty)
    fn get_next_unexpandable(&mut self,state:&mut ET::State) -> Result<Option<ResolvedToken<ET>>,TeXError<ET::Token>> {
        methods::get_next_unexpandable(self,state)
    }

    /// Returns the next primitive to be processed by the [`Stomach`](crate::engine::stomach::Stomach) from
    /// the input stream, after expanding macros as necessary.
    fn get_next_stomach_command(&mut self, state: &mut ET::State) -> Result<Option<StomachCommand<ET>>,TeXError<ET::Token>> {
        Ok(match self.get_next_unexpandable(state)? {
            Some(rt) => Some(StomachCommand::from_resolved(rt)?),
            None => None
        })
    }

    /// Reads a number from the input stream.
    fn get_int(&mut self, state:&mut ET::State) -> Result<ET::Int,TeXError<ET::Token>> {
        numeric_methods::get_int::<ET>(self, state)
    }

    /// Reads a dimension from the input stream.
    fn get_dim(&mut self, state:&mut ET::State) -> Result<ET::Dim,TeXError<ET::Token>> {
        numeric_methods::get_dim::<ET>(self, state)
    }

    /// Reads a skip from the input stream.
    fn get_skip(&mut self, state:&mut ET::State) -> Result<Skip<ET::SkipDim>,TeXError<ET::Token>> {
        numeric_methods::get_skip::<ET>(self, state)
    }

    /// Reads a muskip from the input stream.
    fn get_muskip(&mut self, state:&mut ET::State) -> Result<MuSkip<ET::MuDim,ET::MuStretchShrinkDim>,TeXError<ET::Token>> {
        numeric_methods::get_muskip::<ET>(self, state)
    }

    fn get_group(&mut self, state:&mut ET::State,f:TokenCont<ET>) -> Result<(),TeXError<ET::Token>> {
        methods::get_group::<ET>(self, state,f)
    }

    fn get_expanded_group(&mut self, state:&mut ET::State, expand_protected:bool, keep_the:bool, err_on_unknowns:bool, f: TokenCont<ET>) -> Result<(),TeXError<ET::Token>> {
        methods::get_expanded_group::<ET>(self, state, expand_protected, keep_the, err_on_unknowns,f)
    }

    fn get_braced_string(&mut self, state:&mut ET::State) -> Result<Vec<u8>,TeXError<ET::Token>> {
        methods::get_braced_string::<ET>(self, state)
    }

    /// read a single keyword from the input stream; returns `true` if the keyword is found.
    fn get_keyword<'a>(&mut self, state:&mut ET::State, keyword:&'a str) -> Result<bool,TeXError<ET::Token>> {
        methods::get_keyword::<ET>(self, state, keyword)
    }

    /// reads one of several optional keywords from the input stream;
    /// returns `None` if none of the keywords are found.
    fn get_keywords<'a>(&mut self, state:&mut ET::State, mut keywords:Vec<&'a str>) -> Result<Option<&'a str>,TeXError<ET::Token>> {
        methods::get_keywords::<ET>(self, state, keywords)
    }

    fn get_control_sequence(&mut self, state:&mut ET::State) -> Result<ET::Token,TeXError<ET::Token>> {
        methods::get_control_sequence::<ET>(self, state)
    }

    fn get_string(&mut self,state:&mut ET::State) -> Result<String,TeXError<ET::Token>> {
        methods::get_string::<ET>(self, state)
    }

    fn get_font(&mut self,state:&mut ET::State) -> Result<ET::Font,TeXError<ET::Token>> {
        methods::get_font::<ET>(self, state)
    }

    fn new_conditional(&mut self,name:&'static str) -> usize;
    fn set_conditional(&mut self,idx:usize,branch:ConditionalBranch);
    fn set_top_conditional(&mut self,branch:ConditionalBranch);
    fn pop_conditional(&mut self);
    fn current_conditional(&self) -> (Option<ConditionalBranch>,usize);
}

#[derive(Clone)]
pub struct TeXGullet<ET:EngineType<Gullet=Self>> {
    mouth:ET::Mouth,
    in_conditionals:Vec<ConditionalBranch>
}
impl<ET:EngineType<Gullet=Self>> TeXGullet<ET> {
    pub fn new(mouth:ET::Mouth) -> Self {
        Self {mouth, in_conditionals:Vec::new() }
    }
}
impl<ET:EngineType<Gullet=Self>> Gullet<ET> for TeXGullet<ET> {
    fn switch_mouth(&mut self, tks: Vec<ET::Token>) -> ET::Mouth {
        let old = std::mem::replace(&mut self.mouth, ET::Mouth::new());
        self.mouth.push_tokens(tks);
        old
    }
    fn restore_mouth(&mut self, mouth: ET::Mouth) {
        self.mouth = mouth
    }
    fn mouth(&mut self) -> &mut ET::Mouth { &mut self.mouth }

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
}