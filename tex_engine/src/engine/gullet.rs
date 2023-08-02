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
use crate::tex::commands::{GulletCommand, StomachCommand, StomachCommandInner};
use crate::tex::ConditionalBranch;
use crate::tex::fonts::FontStore;
use crate::tex::numbers::{NumSet, Skip, MuSkip};
use crate::tex::token::{BaseToken, Token};
use crate::utils::errors::{ErrorInPrimitive, TeXError};
use crate::utils::map::Map;
use crate::utils::strings::TeXStr;

type GulletFun<ET:EngineType> = fn(&mut ET::State, &mut ET::Gullet, GulletCommand<ET::Token>) -> Result<Vec<ET::Token>,ErrorInPrimitive<ET::Token>>;
type ConditionalFun<ET:EngineType> = fn(&mut ET::State, &mut ET::Gullet, GulletCommand<ET::Token>) -> Result<bool,ErrorInPrimitive<ET::Token>>;
type IntFun<ET:EngineType> = fn(&mut ET::State,&mut ET::Gullet,GulletCommand<ET::Token>) -> Result<ET::Int,ErrorInPrimitive<ET::Token>>;
type DimFun<ET:EngineType> = fn(&mut ET::State,&mut ET::Gullet,GulletCommand<ET::Token>) -> Result<ET::Dim,ErrorInPrimitive<ET::Token>>;
type SkipFun<ET:EngineType> = fn(&mut ET::State,&mut ET::Gullet,GulletCommand<ET::Token>) -> Result<Skip<ET::SkipDim>,ErrorInPrimitive<ET::Token>>;
type MuSkipFun<ET:EngineType> = fn(&mut ET::State,&mut ET::Gullet,GulletCommand<ET::Token>) -> Result<MuSkip<ET::MuDim,ET::MuStretchShrinkDim>,ErrorInPrimitive<ET::Token>>;
type ToksFun<ET:EngineType> = fn(&mut ET::State,&mut ET::Gullet,GulletCommand<ET::Token>) -> Result<Vec<ET::Token>,ErrorInPrimitive<ET::Token>>;
type FontFun<ET:EngineType> = fn(&mut ET::State,&mut ET::Gullet,GulletCommand<ET::Token>) -> Result<usize,ErrorInPrimitive<ET::Token>>;

/// The [`Gullet`] trait defines the interface for the part of the TeX engine that reads tokens from the input stream,
/// and expands macros. It has basically no components other than a [`Mouth`], so it is implemented as a trait
/// to allow for overriding its methods.
pub trait Gullet<ET:EngineType<Gullet=Self>>:Sized + Clone +'static {
    /// Returns a reference to the [`Mouth`].
    fn mouth(&mut self) -> &mut ET::Mouth;

    fn switch_mouth(&mut self,tks:Vec<ET::Token>) -> ET::Mouth;
    fn restore_mouth(&mut self,mouth:ET::Mouth);

    /// Returns the next primitive to be processed by the [`Stomach`](crate::engine::stomach::Stomach) from
    /// the input stream, after expanding macros as necessary.
    fn get_next_stomach_command(&mut self, state: &mut ET::State) -> Result<Option<StomachCommand<ET::Token>>,Box<dyn TeXError<ET::Token>>> {
        while let Some((tk,b)) = self.mouth().get_next::<ET>(state)? {
            if b {
                match catch!(methods::process_token_for_stomach::<ET>(self, tk.clone(), state) => tk) {
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
    fn get_int(&mut self, state:&mut ET::State) -> Result<ET::Int,Box<dyn TeXError<ET::Token>>> {
        numeric_methods::get_int::<ET>(self, state)
    }

    /// Reads a dimension from the input stream.
    fn get_dim(&mut self, state:&mut ET::State) -> Result<ET::Dim,Box<dyn TeXError<ET::Token>>> {
        numeric_methods::get_dim::<ET>(self, state)
    }

    /// Reads a skip from the input stream.
    fn get_skip(&mut self, state:&mut ET::State) -> Result<Skip<ET::SkipDim>,Box<dyn TeXError<ET::Token>>> {
        numeric_methods::get_skip::<ET>(self, state)
    }

    /// Reads a muskip from the input stream.
    fn get_muskip(&mut self, state:&mut ET::State) -> Result<MuSkip<ET::MuDim,ET::MuStretchShrinkDim>,Box<dyn TeXError<ET::Token>>> {
        numeric_methods::get_muskip::<ET>(self, state)
    }

    fn get_expanded_group(&mut self, state:&mut ET::State, expand_protected:bool, keep_the:bool, err_on_unknowns:bool) -> Result<Vec<ET::Token>,Box<dyn TeXError<ET::Token>>> {
        methods::get_expanded_group::<ET>(self, state, expand_protected, keep_the, err_on_unknowns)
    }

    fn get_braced_string(&mut self, state:&mut ET::State) -> Result<Vec<u8>,Box<dyn TeXError<ET::Token>>> {
        methods::get_braced_string::<ET>(self, state)
    }

    /// read a single keyword from the input stream; returns `true` if the keyword is found.
    fn get_keyword<'a>(&mut self, state:&mut ET::State, keyword:&'a str) -> Result<bool,Box<dyn TeXError<ET::Token>>> {
        methods::get_keyword::<ET>(self, state, keyword)
    }

    /// reads one of several optional keywords from the input stream;
    /// returns `None` if none of the keywords are found.
    fn get_keywords<'a>(&mut self, state:&mut ET::State, mut keywords:Vec<&'a str>) -> Result<Option<&'a str>,Box<dyn TeXError<ET::Token>>> {
        methods::get_keywords::<ET>(self, state, keywords)
    }

    fn get_control_sequence(&mut self, state:&mut ET::State) -> Result<BaseToken<ET::Char>,Box<dyn TeXError<ET::Token>>> {
        methods::get_control_sequence::<ET>(self, state)
    }

    fn get_string(&mut self,state:&mut ET::State) -> Result<TeXStr<ET::Char>,Box<dyn TeXError<ET::Token>>> {
        methods::get_string::<ET>(self, state)
    }

    fn get_font(&mut self,state:&mut ET::State) -> Result<usize,Box<dyn TeXError<ET::Token>>> {
        methods::get_font::<ET>(self, state)
    }

    fn new_conditional(&mut self,name:&'static str) -> usize;
    fn set_conditional(&mut self,idx:usize,branch:ConditionalBranch);
    fn set_top_conditional(&mut self,branch:ConditionalBranch);
    fn pop_conditional(&mut self);
    fn current_conditional(&self) -> (Option<ConditionalBranch>,usize);

    fn register_primitive(&mut self,name:&'static str,cmd:GulletFun<ET>) -> usize;
    fn register_conditional(&mut self,name:&'static str,cmd:ConditionalFun<ET>) -> usize;

    fn primitive(&self,idx:usize) -> Option<GulletFun<ET>>;
    fn primitive_from_name(&self,name:&'static str) -> Option<GulletFun<ET>>;

    fn conditional(&self,idx:usize) -> Option<ConditionalFun<ET>>;
    fn conditional_from_name(&self,name:&'static str) -> Option<ConditionalFun<ET>>;

    fn register_primitive_int(&mut self,name:&'static str,cmd:IntFun<ET>) -> usize;
    fn primitive_int(&self,idx:usize) -> Option<IntFun<ET>>;
    fn primitive_int_from_name(&self,name:&'static str) -> Option<IntFun<ET>>;

    fn register_primitive_dim(&mut self,name:&'static str,cmd:DimFun<ET>) -> usize;
    fn primitive_dim(&self,idx:usize) -> Option<DimFun<ET>>;
    fn primitive_dim_from_name(&self,name:&'static str) -> Option<DimFun<ET>>;

    fn register_primitive_skip(&mut self,name:&'static str,cmd:SkipFun<ET>) -> usize;
    fn primitive_skip(&self,idx:usize) -> Option<SkipFun<ET>>;
    fn primitive_skip_from_name(&self,name:&'static str) -> Option<SkipFun<ET>>;

    fn register_primitive_muskip(&mut self,name:&'static str,cmd:MuSkipFun<ET>) -> usize;
    fn primitive_muskip(&self,idx:usize) -> Option<MuSkipFun<ET>>;
    fn primitive_muskip_from_name(&self,name:&'static str) -> Option<MuSkipFun<ET>>;

    fn register_primitive_toks(&mut self,name:&'static str,cmd:ToksFun<ET>) -> usize;
    fn primitive_toks(&self,idx:usize) -> Option<ToksFun<ET>>;
    fn primitive_toks_from_name(&self,name:&'static str) -> Option<ToksFun<ET>>;

    fn register_primitive_font(&mut self,name:&'static str,cmd:FontFun<ET>) -> usize;
    fn primitive_font(&self,idx:usize) -> Option<FontFun<ET>>;
    fn primitive_font_from_name(&self,name:&'static str) -> Option<FontFun<ET>>;

}

#[derive(Clone)]
pub struct TeXGullet<ET:EngineType<Gullet=Self>> {
    pub mouth:ET::Mouth,
    in_conditionals:Vec<ConditionalBranch>,
    conditionals:Map<fn(&mut ET::State,&mut Self,GulletCommand<ET::Token>) -> Result<bool,ErrorInPrimitive<ET::Token>>>,
    primitives:Map<fn(&mut ET::State,&mut Self,GulletCommand<ET::Token>) -> Result<Vec<ET::Token>,ErrorInPrimitive<ET::Token>>>,
    ints:Map<fn(&mut ET::State,&mut Self,GulletCommand<ET::Token>) -> Result<ET::Int,ErrorInPrimitive<ET::Token>>>,
    dims:Map<fn(&mut ET::State,&mut Self,GulletCommand<ET::Token>) -> Result<ET::Dim,ErrorInPrimitive<ET::Token>>>,
    skips:Map<fn(&mut ET::State,&mut Self,GulletCommand<ET::Token>) -> Result<Skip<ET::SkipDim>,ErrorInPrimitive<ET::Token>>>,
    muskips:Map<fn(&mut ET::State,&mut Self,GulletCommand<ET::Token>) -> Result<MuSkip<ET::MuDim,ET::MuStretchShrinkDim>,ErrorInPrimitive<ET::Token>>>,
    toks:Map<fn(&mut ET::State,&mut Self,GulletCommand<ET::Token>) -> Result<Vec<ET::Token>,ErrorInPrimitive<ET::Token>>>,
        fonts:Map<fn(&mut ET::State,&mut Self,GulletCommand<ET::Token>) -> Result<usize,ErrorInPrimitive<ET::Token>>>
}
impl<ET:EngineType<Gullet=Self>> TeXGullet<ET> {
    pub fn new(mouth:ET::Mouth) -> Self {
        Self {mouth,
            in_conditionals:Vec::new(),
            conditionals:Map::default(),
            primitives:Map::default(),
            ints:Map::default(),
            dims:Map::default(),
            skips:Map::default(),
            muskips:Map::default(),
            toks:Map::default(),
            fonts:Map::default(),
        }
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
    fn register_primitive(&mut self, name: &'static str, cmd: GulletFun<ET>) -> usize {
        self.primitives.insert(name,cmd)
    }
    fn register_conditional(&mut self, name: &'static str, cmd: ConditionalFun<ET>) -> usize {
        self.conditionals.insert(name,cmd)
    }
    fn register_primitive_int(&mut self, name: &'static str, cmd: IntFun<ET>) -> usize {
        self.ints.insert(name,cmd)
    }
    fn register_primitive_dim(&mut self, name: &'static str, cmd: DimFun<ET>) -> usize {
        self.dims.insert(name,cmd)
    }
    fn register_primitive_skip(&mut self, name: &'static str, cmd: SkipFun<ET>) -> usize {
        self.skips.insert(name,cmd)
    }
    fn register_primitive_muskip(&mut self, name: &'static str, cmd: MuSkipFun<ET>) -> usize {
        self.muskips.insert(name,cmd)
    }
    fn register_primitive_toks(&mut self, name: &'static str, cmd: ToksFun<ET>) -> usize {
        self.toks.insert(name,cmd)
    }
    fn register_primitive_font(&mut self, name: &'static str, cmd: FontFun<ET>) -> usize {
        self.fonts.insert(name,cmd)
    }
    fn primitive(&self, idx: usize) -> Option<GulletFun<ET>> {
        self.primitives.get(idx).copied()
    }
    fn primitive_from_name(&self, name: &'static str) -> Option<GulletFun<ET>> {
        self.primitives.get_from_name(name).copied()
    }
    fn conditional(&self, idx: usize) -> Option<ConditionalFun<ET>> {
        self.conditionals.get(idx).copied()
    }
    fn conditional_from_name(&self, name: &'static str) -> Option<ConditionalFun<ET>> {
        self.conditionals.get_from_name(name).copied()
    }
    fn primitive_int(&self, idx: usize) -> Option<IntFun<ET>> {
        self.ints.get(idx).copied()
    }
    fn primitive_int_from_name(&self, name: &'static str) -> Option<IntFun<ET>> {
        self.ints.get_from_name(name).copied()
    }
    fn primitive_dim(&self, idx: usize) -> Option<DimFun<ET>> {
        self.dims.get(idx).copied()
    }
    fn primitive_dim_from_name(&self, name: &'static str) -> Option<DimFun<ET>> {
        self.dims.get_from_name(name).copied()
    }
    fn primitive_skip(&self, idx: usize) -> Option<SkipFun<ET>> {
        self.skips.get(idx).copied()
    }
    fn primitive_skip_from_name(&self, name: &'static str) -> Option<SkipFun<ET>> {
        self.skips.get_from_name(name).copied()
    }
    fn primitive_muskip(&self, idx: usize) -> Option<MuSkipFun<ET>> {
        self.muskips.get(idx).copied()
    }
    fn primitive_muskip_from_name(&self, name: &'static str) -> Option<MuSkipFun<ET>> {
        self.muskips.get_from_name(name).copied()
    }
    fn primitive_toks(&self, idx: usize) -> Option<ToksFun<ET>> {
        self.toks.get(idx).copied()
    }
    fn primitive_toks_from_name(&self, name: &'static str) -> Option<ToksFun<ET>> {
        self.toks.get_from_name(name).copied()
    }
    fn primitive_font(&self, idx: usize) -> Option<FontFun<ET>> {
        self.fonts.get(idx).copied()
    }
    fn primitive_font_from_name(&self, name: &'static str) -> Option<FontFun<ET>> {
        self.fonts.get_from_name(name).copied()
    }

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