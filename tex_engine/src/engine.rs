//! Components of a TeX engine, such as [`Mouth`](mouth::NoTracingMouth) and [`State`](state::State)
use std::fmt::Debug;
use std::path::PathBuf;
use std::time::Instant;
use chrono::{DateTime, Local};
use log::{debug, info};
use crate::engine::filesystem::{File, FileSystem};
use crate::engine::gullet::Gullet;
use crate::engine::memory::{Interner, Memory};
use crate::engine::mouth::{Mouth, MouthTrait};
use crate::engine::state::{PDFState, State};
use crate::engine::stomach::Stomach;
use crate::tex;
use crate::tex::nodes::CustomNode;
use crate::tex::commands::{Assignable, CommandReference};
use crate::tex::commands::pdftex::PDFTeXNode;
use crate::tex::fonts::{Font, FontStore};
use crate::tex::numbers::{Dim, MuDim, MuStretchShrinkDim, SkipDim};
use crate::tex::token::TokenReference;

pub mod state;
pub mod mouth;
pub mod gullet;
pub mod stomach;
pub mod filesystem;
pub mod memory;

/// An [`Engine`] combines a [`FileSystem`], [`State`], [`Gullet`] (including [`MouthTrait`]) and [`Stomach`] to
/// form a complete TeX engine.
pub trait EngineType:Sized+'static + Copy + Clone + Debug {
    type Char:CharType;
    type File:File<Self::Char>;
    type FileSystem:FileSystem<Self::Char,F=Self::File>;
    type Font:Font<Char=Self::Char>;
    type FontStore:FontStore<Char=Self::Char,Font=Self::Font,RefType=Self::FontRefType>;
    type FontRefType:Copy+PartialEq+Debug;
    type Node: CustomNode<Self>;
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

pub struct EngineRef<ET:EngineType> {
    pub state:ET::State,
    pub mouth:Mouth<ET>,
    pub gullet:ET::Gullet,
    pub stomach:ET::Stomach,
    pub memory:Memory<ET>,
    pub outputs:Outputs,
    pub jobname:String,
    pub start_time:DateTime<Local>,
    pub elapsed:Instant,
    pub filesystem:ET::FileSystem,
    pub fontstore:ET::FontStore,
    pub interner:Interner<ET::Char>,
}

pub trait Engine<ET:EngineType> {

    fn components(&mut self) -> &mut EngineRef<ET>;
    fn initialize(&mut self) -> Result<(),TeXError<ET>>;

    fn init_file(&mut self,s:&str) -> Result<(),TeXError<ET>> {match std::panic::catch_unwind(std::panic::AssertUnwindSafe( ||{
        debug!("Initializing with file {}",s);
        let mut comps = self.components();
        let file = comps.filesystem.get(s);
        comps.jobname = file.path().with_extension("").file_name().unwrap().to_str().unwrap().to_string();
        comps.start_time = Local::now();
        let old = comps.filesystem.set_pwd(file.path().parent().unwrap().to_path_buf());
        comps.mouth.push_file(&file,&mut comps.interner);
        // should not produce any boxes, so loop until file end
        ET::Stomach::next_shipout_box(&mut comps);
        comps.filesystem.set_pwd(old);
    })) {
        Ok(_) => Ok(()),
        Err(e) => match e.downcast::<TeXError<ET>>() {
            Ok(e) => Err(*e),
            Err(e) => Err(TeXError{msg:format!("Panic: {:?}",e),cause:None,source:None})
        }
    }}
    fn do_file(&mut self,s:PathBuf) -> Result<Vec<ET::Node>,TeXError<ET>> {match std::panic::catch_unwind(std::panic::AssertUnwindSafe( ||{
        debug!("Running file {:?}",s);
        let mut ret = vec!();
        let mut comps = self.components();
        comps.jobname = s.with_extension("").file_name().unwrap().to_str().unwrap().to_string();
        comps.start_time = Local::now();
        comps.filesystem.set_pwd(s.parent().unwrap().to_path_buf());
        let file = comps.filesystem.get(s.to_str().unwrap());
        comps.mouth.push_file(&file,&mut comps.interner);
        comps.start_time = Local::now();
        comps.elapsed = std::time::Instant::now();

        match comps.state.get_primitive_toks("everyjob").cloned() {
            None => (),
            Some(v) if v.is_empty() => (),
            Some(v) =>{
                let mut rs = comps.mouth.get_expansion();
                rs.extend(v.into_iter());
                comps.mouth.push_expansion(rs);
            }
        }
        while let Some(b) = ET::Stomach::next_shipout_box(&mut comps) {
            ret.push(b)
        }
        ret
    })) {
        Ok(v) => Ok(v),
        Err(e) => match e.downcast::<TeXError<ET>>() {
            Ok(e) => Err(*e),
            Err(e) => Err(TeXError{msg:format!("Panic: {:?}",e),cause:None,source:None})
        }
    }}
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
/*
pub struct EngineStruct<ET:EngineType> {
    pub state:ET::State,
    pub mouth:Mouth<ET>,
    pub gullet: ET::Gullet,
    pub stomach:ET::Stomach,
    memory:Memory<ET>,
    pub outputs:Outputs,
    jobname:String,
    start_time:DateTime<Local>,
    elapsed_time_from:Instant,
    filesystem:ET::FileSystem,
    fontstore:ET::FontStore,
    pub interner:Interner<ET::Char>
}
*/

use crate::tex::numbers::Int;
use crate::utils::errors::TeXError;
use crate::utils::strings::CharType;

impl<ET:EngineType> Engine<ET> for EngineRef<ET> {
    fn components(&mut self) -> &mut EngineRef<ET> { self }
    fn initialize(&mut self) -> Result<(),TeXError<ET>> {
        info!("Initializing TeX engine");
        tex::commands::tex::initialize_tex_primitives::<ET>(&mut self.components());
        self.state.set_primitive_int("mag",ET::Int::from_i64(1000),true);
        self.state.set_primitive_int("fam",ET::Int::from_i64(-1),true);
        Ok(())
    }

}
impl<ET:EngineType> EngineRef<ET> {
    pub fn new(filesystem:ET::FileSystem,fontstore:ET::FontStore,state:ET::State,gullet: ET::Gullet, stomach:ET::Stomach,outputs:Outputs) -> Self {
        let mut memory = Memory::new();
        let mut interner = Interner::new();
        EngineRef {
            state, gullet, mouth:Mouth::new(&mut memory),stomach,memory,outputs,filesystem,fontstore,
            jobname:"".to_string(),start_time:Local::now(),elapsed:std::time::Instant::now(),interner
        }
    }
    pub fn set_state(&mut self,state:ET::State) {
        self.state = state;
    }
    pub fn initialize_etex(&mut self) -> Result<(),TeXError<ET>> {
        self.initialize()?;
        self.etex()?;
        Ok(())
    }
    pub fn etex(&mut self) -> Result<(),TeXError<ET>> {
        tex::commands::etex::initialize_etex_primitives::<ET>(&mut self.components());
        Ok(())
    }
    pub fn latex(&mut self) -> Result<(),TeXError<ET>> {
        self.init_file("latex.ltx")
    }
    pub fn initialize_latex(&mut self) -> Result<(),TeXError<ET>> {
        self.initialize_etex()?;
        self.init_file("latex.ltx")
    }
}
impl<ET:EngineType> EngineRef<ET> where ET::Node:From<PDFTeXNode<ET>>,ET::State:PDFState<ET> {
    pub fn pdftex(&mut self) -> Result<(),TeXError<ET>> {
        tex::commands::pdftex::initialize_pdftex_primitives::<ET>(&mut self.components());
        //state.dimensions_prim.set_locally((crate::commands::registers::PDFPXDIMEN.index - 1) as usize,65536);
        self.init_file("pdftexconfig.tex")
    }
    pub fn initialize_pdftex(&mut self) -> Result<(),TeXError<ET>> {
        self.initialize()?;
        self.pdftex()
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