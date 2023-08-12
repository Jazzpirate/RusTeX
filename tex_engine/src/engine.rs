//! Components of a TeX engine, such as [`Mouth`](mouth::NoTracingMouth) and [`State`](state::State)
use std::fmt::Debug;
use std::path::PathBuf;
use log::{debug, info};
use crate::engine::filesystem::{File, FileSystem};
use crate::engine::gullet::{Gullet, TeXGullet};
use crate::engine::memory::Memory;
use crate::engine::mouth::{Mouth, StandardMouth};
use crate::engine::state::{State, TeXState};
use crate::engine::stomach::{Stomach, NoShipoutDefaultStomach};
use crate::tex;
use crate::tex::nodes::{CustomBox, CustomNode};
use crate::tex::commands::{Assignable, Command, CommandReference};
use crate::tex::fonts::{Font, FontStore, TfmFontStore};
use crate::tex::numbers::{Dim, MuDim, MuStretchShrinkDim, SkipDim};
use crate::tex::token::{BaseToken, Token, TokenReference};

pub mod state;
pub mod mouth;
pub mod gullet;
pub mod stomach;
pub mod filesystem;
pub mod memory;

/// An [`Engine`] combines a [`FileSystem`], [`State`], [`Gullet`] (including [`Mouth`]) and [`Stomach`] to
/// form a complete TeX engine.
pub trait EngineType:Sized+'static + Copy + Clone + Debug {
    type Char:CharType;
    type File:File<Self::Char>;
    type FileSystem:FileSystem<Self::Char,F=Self::File>;
    type Font:Font;
    type FontStore:FontStore<Char=Self::Char,Font=Self::Font>;
    type Node: CustomNode<Self,Bx=Self::Box>;
    type Box: CustomBox<Self>;
    type Int:Int+Assignable<Self>;
    type Dim:Dim+Assignable<Self>;
    type SkipDim:SkipDim<Dim=Self::Dim>;
    type MuDim:MuDim;
    type MuStretchShrinkDim:MuStretchShrinkDim;
    type CommandReference:CommandReference<Self>;
    type TokenReference:TokenReference<Self>;
    type State:State<Self>;
    type Gullet:Gullet<Self>;
    type Stomach:Stomach<Self>;
}

pub struct EngineMut<'a,ET:EngineType> {
    pub state:&'a mut ET::State,
    pub gullet:&'a mut ET::Gullet,
    pub stomach:&'a mut ET::Stomach,
    pub memory:&'a mut Memory<ET>,
}

pub struct EngineRef<'a,ET:EngineType> {
    pub state:&'a ET::State,
    pub gullet:&'a ET::Gullet,
    pub stomach:&'a ET::Stomach,
}

pub trait Engine<ET:EngineType> {
    /// All components of an [`Engine`] bundled into a struct - this allows the borrow
    /// checker to prove that the individual components are non-overlapping.
    fn components(&self) -> EngineRef<ET>;
    fn components_mut(&mut self) -> EngineMut<ET>;
    fn initialize(&mut self) -> Result<(),TeXError<ET>>;

    fn init_file(&mut self,s:&str) -> Result<(),TeXError<ET>> {
        debug!("Initializing with file {}",s);
        let mut comps = self.components_mut();
        let file = comps.state.filesystem().get(s);
        let old = comps.state.filesystem().set_pwd(file.path().parent().unwrap().to_path_buf());
        comps.gullet.mouth().push_file(&file);
        comps.state.set_job(file.path().with_extension("").file_name().unwrap().to_str().unwrap().to_string());
        // should not produce any boxes, so loop until file end
        let (s,mut r) = comps.split_stomach();
        s.next_shipout_box(&mut r)?;
        r.state.filesystem().set_pwd(old);
        Ok(())
    }
    fn do_file(&mut self,s:PathBuf) -> Result<Vec<ET::Node>,TeXError<ET>> {
        debug!("Running file {:?}",s);
        let mut ret = vec!();
        let mut comps = self.components_mut();
        comps.state.set_job(s.with_extension("").file_name().unwrap().to_str().unwrap().to_string());
        let file = comps.state.filesystem().get(s.to_str().unwrap());
        comps.gullet.mouth().push_file(&file);
        let mut ej = comps.gullet.mouth().new_tokensource();
        for t in comps.state.get_primitive_toks("everyjob") { ej.push(t) }
        comps.gullet.mouth().push_tokens(ej);
        let (s,mut r) = comps.split_stomach();
        while let Some(b) = s.next_shipout_box(&mut r)? {
            ret.push(b)
        }
        Ok(ret)
    }
}

pub fn new<ET:EngineType>(state:ET::State, gullet: ET::Gullet, stomach:ET::Stomach) -> EngineStruct<ET> {
    EngineStruct {
        state, gullet, stomach,memory:Memory::new()
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

#[derive(Clone)]
pub struct EngineStruct<ET:EngineType> {
    pub state:ET::State,
    pub gullet: ET::Gullet,
    pub stomach:ET::Stomach,
    memory:Memory<ET>,
}

use crate::tex::numbers::Int;
use crate::utils::errors::TeXError;
use crate::utils::strings::CharType;

impl<ET:EngineType> Engine<ET> for EngineStruct<ET> {
    fn components(&self) -> EngineRef<ET> { EngineRef {state:&self.state, gullet:&self.gullet, stomach:&self.stomach} }
    fn components_mut(&mut self) -> EngineMut<ET> { EngineMut {state:&mut self.state, gullet:&mut self.gullet, stomach:&mut self.stomach, memory:&mut self.memory} }
    fn initialize(&mut self) -> Result<(),TeXError<ET>> {
        info!("Initializing TeX engine");
        tex::commands::tex::initialize_tex_primitives::<ET>(&mut self.components_mut());
        self.state.set_primitive_int("mag",ET::Int::from_i64(1000)?,true);
        self.state.set_primitive_int("fam",ET::Int::from_i64(-1)?,true);
        Ok(())
    }

}
impl<ET:EngineType> EngineStruct<ET> {
    pub fn initialize_etex(&mut self) -> Result<(),TeXError<ET>> {
        self.initialize()?;
        self.etex();
        Ok(())
    }
    pub fn etex(&mut self) -> Result<(),TeXError<ET>> {
        tex::commands::etex::initialize_etex_primitives::<ET>(&mut self.components_mut());
        Ok(())
    }
    pub fn pdftex(&mut self) -> Result<(),TeXError<ET>> {
        tex::commands::pdftex::initialize_pdftex_primitives::<ET>(&mut self.components_mut());
        //state.dimensions_prim.set_locally((crate::commands::registers::PDFPXDIMEN.index - 1) as usize,65536);
        self.init_file("pdftexconfig.tex")
    }
    pub fn latex(&mut self) -> Result<(),TeXError<ET>> {
        self.init_file("latex.ltx")
    }
    pub fn initialize_pdftex(&mut self) -> Result<(),TeXError<ET>> {
        self.initialize()?;
        self.pdftex()
    }
    pub fn initialize_latex(&mut self) -> Result<(),TeXError<ET>> {
        self.initialize_etex()?;
        self.init_file("latex.ltx")
    }
    pub fn initialize_pdflatex(&mut self) -> Result<(),TeXError<ET>> {
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