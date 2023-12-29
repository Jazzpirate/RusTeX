pub mod commands;
pub mod nodes;

use nodes::{MinimalPDFExtension, PDFExtension, PDFNode};
use crate::engine::{DefaultEngine, EngineReferences, EngineTypes, filesystem, state, TeXEngine, utils};
use crate::engine::filesystem::{File, VirtualFile};
use crate::engine::fontsystem::{TfmFont, TfmFontSystem};
use crate::engine::gullet::DefaultGullet;
use crate::engine::mouth::DefaultMouth;
use crate::engine::stomach::StomachWithShipout;
use crate::engine::utils::memory::InternedCSName;
use crate::engine::utils::outputs::LogOutputs;
use crate::tex;
use crate::tex::input_text::Character;
use crate::tex::nodes::vertical::VNode;
use crate::tex::numerics::{Dim32, MuSkip32, Skip32};
use crate::utils::errors::TeXError;

pub trait PDFTeXEngine: TeXEngine
    where <Self::Types as EngineTypes>::Extension: PDFExtension<Self::Types>,
          <Self::Types as EngineTypes>::CustomNode: From<PDFNode<Self::Types>>,
          <Self::Types as EngineTypes>::File: FileWithMD5 {
    fn do_file_pdf<F:FnMut(&mut EngineReferences<Self::Types>, VNode<Self::Types>)>(&mut self, s:&str, f:F) -> Result<(),TeXError> {
        *self.get_engine_refs().aux.extension.elapsed() = std::time::Instant::now();
        self.do_file_default(s,f)
    }

    fn initialize_pdflatex(&mut self) -> Result<(),TeXError> {
        self.initialize_etex();
        commands::register_pdftex_primitives(self);
        self.init_file("pdftexconfig.tex")?;
        self.load_latex()
    }
}

pub trait FileWithMD5: File {
    fn md5(&self) -> md5::Digest;
}
impl<C:Character> FileWithMD5 for VirtualFile<C> {
    fn md5(&self) -> md5::Digest {
        match &self.source {
            None => match std::fs::read(&self.path) {
                Ok(v) => md5::compute(v),
                Err(_) => md5::compute("")
            }
            Some(s) => {
                let v : Vec<u8> = s.iter().map(|v| v.iter().map(|c| c.to_char().to_string().into_bytes())).flatten().flatten().collect();
                md5::compute(v)
            }
        }
    }
}

/// Example implementation of [`EngineTypes`] for a plain TeX engine.
#[derive(Copy,Clone,Debug)]
pub struct DefaultPDFTeXEngineTypes;
impl EngineTypes for DefaultPDFTeXEngineTypes {
    type Char = u8;
    type CSName = utils::memory::InternedCSName<u8>;//InternedString;
    type Token = super::tex::token::CompactToken;//::StandardToken<u8,Self::CSName>;//
    type ErrorHandler = super::utils::errors::ErrorThrower;
    type Extension = MinimalPDFExtension<Self>;
    type Int = i32;
    type Dim = Dim32;
    type Skip = Skip32<Dim32>;
    type MuSkip = MuSkip32;
    type Num = tex::numerics::DefaultNumSet;
    type State = state::tex_state::TeXState<Self>;
    type Memory = utils::memory::ReuseTokenLists<Self::Token>;
    type File = VirtualFile<u8>;
    type FileSystem = filesystem::NoOutputFileSystem<u8>;
    type Outputs = LogOutputs;
    type Mouth = DefaultMouth<Self::Token,Self::File>;
    type Gullet = DefaultGullet<Self>;
    type CustomNode = PDFNode<Self>;
    type Stomach = StomachWithShipout<Self>;
    type Font = TfmFont<i32,Dim32,InternedCSName<u8>>;
    type FontSystem = TfmFontSystem<i32,Dim32,InternedCSName<u8>>;//InternedString>;
}

pub type PlainPDFTeXEngine = DefaultEngine<DefaultPDFTeXEngineTypes>;


//impl PDFTeXEngine for PlainPDFTeXEngine {}
impl<A> PDFTeXEngine for A where
    A:TeXEngine,
    <A::Types as EngineTypes>::Extension : PDFExtension<A::Types>,
    <A::Types as EngineTypes>::CustomNode: From<PDFNode<A::Types>>,
    <A::Types as EngineTypes>::File: FileWithMD5 {}

