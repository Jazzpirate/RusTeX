use std::sync::Mutex;
use lazy_static::lazy_static;
use tex_engine::commands::Command;
use tex_engine::commands::pdftex::pdftexnodes::{MinimalPDFExtension, PDFNode};
use tex_engine::commands::primitives::register_unexpandable;
use tex_engine::engine::{DefaultEngine, EngineAux, EngineReferences, EngineTypes, filesystem, state, utils};
use tex_engine::engine::filesystem::{File, SourceReference, VirtualFile};
use tex_engine::engine::gullet::DefaultGullet;
use tex_engine::engine::mouth::DefaultMouth;
use tex_engine::engine::stomach::StomachWithShipout;
use tex_engine::engine::utils::memory::InternedCSName;
use tex_engine::engine::utils::outputs::LogOutputs;
use tex_engine::tex;
use tex_engine::tex::nodes::{BoxInfo, TeXNode, TeXBox};
use tex_engine::tex::numerics::{Dim32, MuSkip32, Skip32};
use tex_engine::tex::token::CompactToken;
use tex_engine::utils::errors::{ErrorThrower};
use tex_engine::engine::TeXEngine;
use tex_engine::engine::utils::outputs::Outputs;
use tex_engine::utils::errors::ErrorHandler;
use tex_engine::engine::EngineExtension;
use tex_engine::engine::mouth::Mouth;
use tex_engine::engine::gullet::Gullet;
use tex_engine::engine::stomach::Stomach as StomachT;
use tex_engine::engine::filesystem::FileSystem;
use tex_engine::engine::PDFTeXEngine;
use tex_engine::engine::state::State as OrigState;
use tex_engine::tex::catcodes::CategoryCodeScheme;
use tex_engine::tex::catcodes::CategoryCode;
use tex_engine::tex::types::BoxType;
use crate::nodes::RusTeXNode;
use crate::state::RusTeXState;
use crate::stomach::{CLOSE_FONT, close_font, RusTeXStomach};

pub(crate) type Extension = super::extension::RusTeXExtension;
pub(crate) type Memory = utils::memory::ReuseTokenLists<CompactToken>;
pub(crate) type Font = tex_engine::engine::fontsystem::TfmFont<i32,Dim32,InternedCSName<u8>>;
pub(crate) type Bx = TeXBox<Types>;
pub(crate) type SRef = SourceReference<<<Types as EngineTypes>::File as File>::SourceRefID>;
pub(crate) type Refs<'a,'b> = &'a mut EngineReferences<'b,Types>;
pub(crate) type CSName = InternedCSName<u8>;

#[derive(Clone,Debug,Copy)]
pub struct Types;
impl EngineTypes for Types {
    type Char = u8;
    type CSName = CSName;
    type Token = CompactToken;
    type ErrorHandler = ErrorThrower;
    type Extension = Extension;
    type Int = i32;
    type Dim = Dim32;
    type Skip = Skip32<Dim32>;
    type MuSkip = MuSkip32;
    type Num = tex::numerics::DefaultNumSet;
    type State = RusTeXState;
    type Memory = utils::memory::ReuseTokenLists<Self::Token>;
    type File = VirtualFile<u8>;
    type FileSystem = crate::files::Files;
    type Outputs = LogOutputs;
    type Mouth = DefaultMouth<Self::Token,Self::File>;
    type Gullet = DefaultGullet<Self>;
    type CustomNode = RusTeXNode;
    type Stomach = RusTeXStomach;
    type Font = tex_engine::engine::fontsystem::TfmFont<i32,Dim32,InternedCSName<u8>>;
    type FontSystem = super::fonts::Fontsystem;
}



thread_local! {
    static MAIN_STATE : Mutex<Option<(RusTeXState,Memory)>> = Mutex::new(None);
    static FONT_SYSTEM : Mutex<Option<super::fonts::Fontsystem>> = Mutex::new(None);
}

fn get_state() -> (RusTeXState,Memory) {
    MAIN_STATE.with(|state| {
        let mut guard = state.lock().unwrap();
        match &mut *guard {
            Some((s, m)) =>return  (s.clone(), m.clone()),
            n => {
                let mut engine = DefaultEngine::<Types>::new();
                register_unexpandable(&mut engine,CLOSE_FONT,close_font);
                engine.initialize_pdflatex();
                register_command(&mut engine, true, "LaTeX", "",
                                 "L\\kern-.3em\\raise.5ex\\hbox{\\check@mathfonts\\fontsize\\sf@size\\z@\\math@fontsfalse\\selectfont A}\\kern-.15em\\TeX",
                                 true, false
                );
                register_command(&mut engine, true, "pgfsysdriver", "",
                                 "pgfsys-rustex.def",
                                 false, false
                );
                *n = Some((engine.state.clone(), engine.aux.memory.clone()));
                FONT_SYSTEM.with(|f| f.lock().unwrap().replace(engine.fontsystem.clone()));
                return (engine.state, engine.aux.memory)
            }
        }
    })
}

fn get_engine() -> DefaultEngine<Types> {
    let (mut state,memory) = get_state();
    let fontsystem = FONT_SYSTEM.with(|f| f.lock().unwrap().clone()).unwrap();
    let mut aux = EngineAux {
        memory,
        outputs: LogOutputs::new(),
        error_handler: ErrorThrower::new(),
        start_time:chrono::Local::now(),
        extension: Extension::new(),
        jobname: String::new()
    };
    let mut mouth = DefaultMouth::new(&mut aux,&mut state);
    let gullet = DefaultGullet::new(&mut aux,&mut state,&mut mouth);
    let stomach = RusTeXStomach::new(&mut aux,&mut state);
    DefaultEngine {
        state,
        aux,
        fontsystem,
        filesystem: crate::files::Files::new(tex_engine::utils::PWD.to_path_buf()),
        mouth,gullet,stomach
    }
}
pub trait RusTeXEngineT {
    fn initialize();
    fn get() -> Self;
    fn do_file<S:AsRef<str>>(file:S);
}

lazy_static! {
static ref AT_LETTER_SCHEME : CategoryCodeScheme<u8> = {
    let mut catcodes = [CategoryCode::Other;256];
    catcodes[123] = CategoryCode::BeginGroup;
    catcodes[125] = CategoryCode::EndGroup;
    catcodes[36] = CategoryCode::MathShift;
    catcodes[38] = CategoryCode::AlignmentTab;
    catcodes[35] = CategoryCode::Parameter;
    catcodes[94] = CategoryCode::Superscript;
    catcodes[95] = CategoryCode::Subscript;
    catcodes[126] = CategoryCode::Active;
    catcodes[92] = CategoryCode::Escape;
    catcodes[32] = CategoryCode::Space;
    catcodes[13] = CategoryCode::EOL;
    catcodes[37] = CategoryCode::Comment;
    for i in 65..91 { catcodes[i] = CategoryCode::Letter}
    for i in 97..123 { catcodes[i] = CategoryCode::Letter}
    catcodes
};
}
fn register_command(e: &mut DefaultEngine<Types>,globally:bool,name:&'static str,sig:&'static str,exp:&'static str,protect:bool,long:bool) {
    use tex_engine::engine::utils::memory::MemoryManager;
    use tex_engine::tex::control_sequences::ControlSequenceNameHandler;
    let e = e.get_engine_refs();
    let name = e.aux.memory.cs_interner_mut().new(name);
    let mut cmd = tex_engine::commands::methods::make_macro::<Types,_,_>(
        e.aux.memory.cs_interner_mut(),
        &AT_LETTER_SCHEME,sig,exp);
    if protect { cmd.protected = true }
    if long { cmd.long = true }
    e.state.set_command(&e.aux,name,Some(Command::Macro(cmd)),globally)
}

/*pub struct RusTeXEngine {
    engine: DefaultEngine<Types>
}*/
pub type RusTeXEngine = DefaultEngine<Types>;
impl RusTeXEngineT for RusTeXEngine {
    #[inline(always)]
    fn initialize() { let _ = get_engine(); }
    #[inline(always)]
    fn get() -> Self { get_engine() }
    fn do_file<S:AsRef<str>>(file:S) {
        let mut engine = Self::get();
        engine.do_file_pdf(file.as_ref(),|e,n| super::shipout::shipout_paginated(e,n)).unwrap();
        FONT_SYSTEM.with(|f| f.lock().unwrap().replace(engine.fontsystem));
    }

}
