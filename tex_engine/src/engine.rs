//! Components of a TeX engine, such as [`Mouth`](mouth::NoTracingMouth) and [`State`](state::State)
use std::path::PathBuf;
use log::{debug, info};
use crate::engine::filesystem::{File, FileSystem};
use crate::engine::gullet::{Gullet, TeXGullet};
use crate::engine::mouth::{Mouth, NoTracingMouth, TracingMouth};
use crate::engine::state::{State, TeXState};
use crate::engine::stomach::{Stomach, NoShipoutDefaultStomach};
use crate::tex;
use crate::tex::boxes::{StandardTeXNode, TeXBox, TeXNode};
use crate::tex::fonts::{Font, FontStore, TfmFontStore};
use crate::tex::numbers::{DefaultNumSet, Dim, MuDim, MuStretchShrinkDim, NumSet, SkipDim};
use crate::tex::token::{BaseToken, Token, TokenWithSourceref};
use crate::utils::errors::{InitializationError, TeXError};

pub mod state;
pub mod mouth;
pub mod gullet;
pub mod stomach;
pub mod filesystem;

/// An [`Engine`] combines a [`FileSystem`], [`State`], [`Gullet`] (including [`Mouth`]) and [`Stomach`] to
/// form a complete TeX engine.
pub trait EngineType:Sized+'static {
    type Char:CharType;
    type Token:Token<Char=Self::Char>;
    type File:File<Self::Char>;
    type FileSystem:FileSystem<Self::Char,F=Self::File>;
    type Font:Font;
    type FontStore:FontStore<Char=Self::Char,Font=Self::Font>;
    type Node:TeXNode<Bx=Self::Box>;
    type Box:TeXBox;
    type Int:Int;
    type Dim:Dim;
    type SkipDim:SkipDim<Dim=Self::Dim>;
    type MuDim:MuDim;
    type MuStretchShrinkDim:MuStretchShrinkDim;
    type Numbers:NumSet<Int=Self::Int,Dim=Self::Dim,SkipDim=Self::SkipDim,MuDim=Self::MuDim,MuStretchShrinkDim=Self::MuStretchShrinkDim>;
    type State:State<Self>;
    type Mouth:Mouth<Self::Token>;
    type Gullet:Gullet<Self>;
    type Stomach:Stomach<Self>;
}

pub trait Engine<ET:EngineType> {
    /// All components of an [`Engine`] bundled into a struct - this allows the borrow
    /// checker to prove that the individual components are non-overlapping.
    fn components(&self) -> &EngineStruct<ET>;
    fn components_mut(&mut self) -> &mut EngineStruct<ET>;
    fn initialize(&mut self) -> Result<(),InitializationError<ET::Token>>;

    fn init_file(&mut self,s:&str) -> Result<(),InitializationError<ET::Token>> {
        debug!("Initializing with file {}",s);
        let comps = self.components_mut();
        let file = comps.state.filesystem().get(s);
        let old = comps.state.filesystem().set_pwd(file.path().parent().unwrap().to_path_buf());
        comps.gullet.mouth().push_file(&file);
        comps.state.set_job(s.to_string());
        let state = &mut comps.state;
        let gullet = &mut comps.gullet;
        // should not produce any boxes, so loop until file end
        comps.stomach.next_shipout_box(state,gullet)?;
        comps.state.filesystem().set_pwd(old);
        Ok(())
    }
    fn do_file(&mut self,s:PathBuf) -> Result<Vec<ET::Node>,Box<dyn TeXError<ET::Token>>> {
        debug!("Running file {:?}",s);
        let mut ret = vec!();
        let comps = self.components_mut();
        comps.state.set_job(s.file_name().unwrap().to_str().unwrap().to_string());
        let file = comps.state.filesystem().get(s.to_str().unwrap());
        comps.gullet.mouth().push_file(&file);
        let state = &mut comps.state;
        let gullet = &mut comps.gullet;
        while let Some(b) = comps.stomach.next_shipout_box(state,gullet)? {
            ret.push(b)
        }
        Ok(ret)
    }
}

pub fn new<ET:EngineType>(state:ET::State, gullet: ET::Gullet, stomach:ET::Stomach) -> EngineStruct<ET> {
    EngineStruct {
        state, gullet, stomach,
        phantom_tk:std::marker::PhantomData,
    }
}
/*
pub type TeXEngine<T,Sto,S, M,FS> = EngineStruct<T,NoShipoutDefaultStomach<T,TeXState<T,Sto,FS,TfmFontStore,DefaultNumSet>,TeXGullet<T, M,S>, StandardTeXNode>>;

pub fn new_tex<FS:FileSystem<u8>>(fs:FS,outputs:Outputs) -> TeXEngine<BaseToken<u8>,NoShipoutDefaultStomach<BaseToken<u8>,TeXState,TeXGullet,StandardTeXNode>,TeXState,NoTracingMouth<BaseToken<u8>>,FS> {
    new(TeXState::new(fs,TfmFontStore::new(),outputs), TeXGullet::new(NoTracingMouth::new()), NoShipoutDefaultStomach::new())
}
pub fn new_tex_with_source_references<FS:FileSystem<u8>>(fs:FS,outputs:Outputs) -> TeXEngine<TokenWithSourceref<u8>,TracingMouth<u8>,FS> {
    new(TeXState::new(fs,TfmFontStore::new(),outputs), TeXGullet::new(TracingMouth::new()), NoShipoutDefaultStomach::new())
}

 */

pub struct EngineStruct<ET:EngineType> {
    pub state:ET::State,
    pub gullet: ET::Gullet,
    pub stomach:ET::Stomach,
    phantom_tk:std::marker::PhantomData<ET>
}

use crate::tex::numbers::Int;
use crate::utils::strings::CharType;

impl<ET:EngineType> Engine<ET> for EngineStruct<ET> {
    fn components(&self) -> &EngineStruct<ET> { self }
    fn components_mut(&mut self) -> &mut EngineStruct<ET> {
        self
    }
    fn initialize(&mut self) -> Result<(),InitializationError<ET::Token>> {
        info!("Initializing TeX engine");
        tex::commands::tex::initialize_tex_primitives::<ET>(&mut self.state,&mut self.stomach, &mut self.gullet);
        self.state.set_primitive_int("mag",ET::Int::from_i64(1000)?,true);
        self.state.set_primitive_int("fam",ET::Int::from_i64(-1)?,true);
        Ok(())
    }

}
impl<ET:EngineType> EngineStruct<ET> {
    pub fn initialize_etex(&mut self) -> Result<(),InitializationError<ET::Token>> {
        self.initialize()?;
        self.etex();
        Ok(())
    }
    fn etex(&mut self) -> Result<(),InitializationError<ET::Token>> {
        tex::commands::etex::initialize_etex_primitives::<ET>(&mut self.state,&mut self.stomach, &mut self.gullet);
        Ok(())
    }
    fn pdftex(&mut self) -> Result<(),InitializationError<ET::Token>> {
        tex::commands::pdftex::initialize_pdftex_primitives::<ET>(&mut self.state,&mut self.stomach, &mut self.gullet);
        //state.dimensions_prim.set_locally((crate::commands::registers::PDFPXDIMEN.index - 1) as usize,65536);
        self.init_file("pdftexconfig.tex")
    }
    fn latex(&mut self) -> Result<(),InitializationError<ET::Token>> {
        self.init_file("latex.ltx")
    }
    pub fn initialize_pdftex(&mut self) -> Result<(),InitializationError<ET::Token>> {
        self.initialize()?;
        self.pdftex()
    }
    pub fn initialize_latex(&mut self) -> Result<(),InitializationError<ET::Token>> {
        self.initialize_etex()?;
        self.init_file("latex.ltx")
    }
    pub fn initialize_pdflatex(&mut self) -> Result<(),InitializationError<ET::Token>> {
        self.initialize_etex()?;
        self.pdftex()?;
        self.latex()
    }
}

#[derive(Clone)]
pub struct Outputs {
    pub error: fn(&str),
    pub message: fn(&str),
    pub file_open:fn(&str),
    pub file_close:fn(&str),
    pub write_18:fn(&str),
    pub write_17:fn(&str),
    pub write_16:fn(&str),
    pub write_neg1:fn(&str),
    pub write_other:fn(&str)
}