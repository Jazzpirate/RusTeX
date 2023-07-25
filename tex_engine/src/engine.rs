//! Components of a TeX engine, such as [`Mouth`](mouth::NoTracingMouth) and [`State`](state::State)
use std::path::PathBuf;
use log::{debug, info};
use crate::engine::filesystem::{File, FileSystem};
use crate::engine::gullet::{Gullet, TeXGullet};
use crate::engine::mouth::{Mouth, NoTracingMouth, TracingMouth};
use crate::engine::state::{State, TeXState};
use crate::engine::stomach::{Stomach, NoShipoutDefaultStomach};
use crate::tex;
use crate::tex::boxes::StandardTeXBox;
use crate::tex::numbers::{DefaultNumSet, NumSet};
use crate::tex::token::{BaseToken, Token, TokenWithSourceref};
use crate::utils::errors::{InitializationError, TeXError};

pub mod state;
pub mod mouth;
pub mod gullet;
pub mod stomach;
pub mod filesystem;

/// An [`Engine`] combines a [`FileSystem`], [`State`], [`Gullet`] (including [`Mouth`]) and [`Stomach`] to
/// form a complete TeX engine.
pub trait EngineType {
    type T:Token;
    type FS:FileSystem<<Self::T as Token>::Char>;
    type S:State<Self::T,FS=Self::FS>;
    type M:Mouth<Self::T>;
    type Gu:Gullet<Self::T, M=Self::M,S=Self::S>;
    type Sto:Stomach<Self::T,S=Self::S,Gu=Self::Gu>;
}

pub trait Engine:EngineType {
    /// All components of an [`Engine`] bundled into a struct - this allows the borrow
    /// checker to prove that the individual components are non-overlapping.
    fn components(&self) -> &EngineStruct<Self::T,Self::Sto>;
    fn components_mut(&mut self) -> &mut EngineStruct<Self::T,Self::Sto>;
    fn initialize(&mut self) -> Result<(),InitializationError<Self::T>>;

    fn init_file(&mut self,s:&str) -> Result<(),InitializationError<Self::T>> {
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
    fn do_file(&mut self,s:PathBuf) -> Result<Vec<<Self::Sto as Stomach<Self::T>>::B>,Box<dyn TeXError<Self::T>>> {
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

pub fn new<T:Token,Sto:Stomach<T>
>(state:Sto::S, gullet: Sto::Gu, stomach:Sto) -> EngineStruct<T,Sto> {
    EngineStruct {
        state, gullet, stomach,
        phantom_tk:std::marker::PhantomData,
    }
}

pub type TeXEngine<T,M,FS> = EngineStruct<T,NoShipoutDefaultStomach<T,TeXState<T,FS,DefaultNumSet>,TeXGullet<T, M,TeXState<T,FS,DefaultNumSet>>,StandardTeXBox>>;

pub fn new_tex<FS:FileSystem<u8>>(fs:FS,outputs:Outputs) -> TeXEngine<BaseToken<u8>,NoTracingMouth<BaseToken<u8>>,FS> {
    new(TeXState::new(fs,outputs), TeXGullet::new(NoTracingMouth::new()), NoShipoutDefaultStomach::new())
}
pub fn new_tex_with_source_references<FS:FileSystem<u8>>(fs:FS,outputs:Outputs) -> TeXEngine<TokenWithSourceref<u8>,TracingMouth<u8>,FS> {
    new(TeXState::new(fs,outputs), TeXGullet::new(TracingMouth::new()), NoShipoutDefaultStomach::new())
}

pub struct EngineStruct<
    T:Token,
    Sto:Stomach<T>
> {
    pub state:Sto::S,
    pub gullet: Sto::Gu,
    pub stomach:Sto,
    phantom_tk:std::marker::PhantomData<T>
}
impl <T:Token,Sto:Stomach<T>> EngineType for EngineStruct<T,Sto> {
    type T = T;
    type Sto = Sto;
    type S = Sto::S;
    type Gu = Sto::Gu;
    type FS = <Self::S as State<T>>::FS;
    type M = <Self::Gu as Gullet<T>>::M;
}
use crate::tex::numbers::Int;
impl<T:Token,Sto:Stomach<T>> Engine for EngineStruct<T,Sto> {
    fn components(&self) -> &EngineStruct<Self::T, Self::Sto> { self }
    fn components_mut(&mut self) -> &mut EngineStruct<Self::T, Self::Sto> {
        self
    }
    fn initialize(&mut self) -> Result<(),InitializationError<Self::T>> {
        info!("Initializing TeX engine");
        tex::commands::tex::initialize_tex_primitives(&mut self.state,&mut self.stomach, &mut self.gullet);
        self.state.set_primitive_int("mag",<<Self::S as State<T>>::NumSet as NumSet>::Int::from_i64(1000)?,true);
        self.state.set_primitive_int("fam",<<Self::S as State<T>>::NumSet as NumSet>::Int::from_i64(-1)?,true);
        Ok(())
    }

}
impl<T:Token,Sto:Stomach<T>> EngineStruct<T,Sto> {
    pub fn initialize_etex(&mut self) -> Result<(),InitializationError<T>> {
        self.initialize()?;
        self.etex();
        Ok(())
    }
    fn etex(&mut self) -> Result<(),InitializationError<T>> {
        tex::commands::etex::initialize_etex_primitives(&mut self.state,&mut self.stomach, &mut self.gullet);
        Ok(())
    }
    fn pdftex(&mut self) -> Result<(),InitializationError<T>> {
        tex::commands::pdftex::initialize_pdftex_primitives(&mut self.state,&mut self.stomach, &mut self.gullet);
        //state.dimensions_prim.set_locally((crate::commands::registers::PDFPXDIMEN.index - 1) as usize,65536);
        self.init_file("pdftexconfig.tex")
    }
    fn latex(&mut self) -> Result<(),InitializationError<T>> {
        self.init_file("latex.ltx")
    }
    pub fn initialize_pdftex(&mut self) -> Result<(),InitializationError<T>> {
        self.initialize()?;
        self.pdftex()
    }
    pub fn initialize_latex(&mut self) -> Result<(),InitializationError<T>> {
        self.initialize_etex()?;
        self.init_file("latex.ltx")
    }
    pub fn initialize_pdflatex(&mut self) -> Result<(),InitializationError<T>> {
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