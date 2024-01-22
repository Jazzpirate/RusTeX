use std::sync::Mutex;
use lazy_static::lazy_static;
use pdfium_render::page_objects_common::PdfPageObjectsCommon;
use tex_engine::commands::{TeXCommand, CommandScope, Macro, PrimitiveCommand};
use tex_engine::commands::primitives::{register_simple_expandable, register_unexpandable};
use tex_engine::engine::{DefaultEngine, EngineAux, EngineReferences, EngineTypes, utils};
use tex_engine::engine::filesystem::{File, SourceReference, VirtualFile};
use tex_engine::engine::gullet::DefaultGullet;
use tex_engine::engine::mouth::DefaultMouth;
use tex_engine::tex;
use tex_engine::tex::numerics::{Dim32, Mu};
use tex_engine::tex::tokens::CompactToken;
use tex_engine::utils::errors::{ErrorThrower, TeXError};
use tex_engine::engine::TeXEngine;
use tex_engine::engine::utils::outputs::Outputs;
use tex_engine::utils::errors::ErrorHandler;
use tex_engine::engine::EngineExtension;
use tex_engine::engine::mouth::Mouth;
use tex_engine::engine::gullet::Gullet;
use tex_engine::engine::stomach::Stomach as StomachT;
use tex_engine::engine::filesystem::FileSystem;
use tex_engine::engine::fontsystem::FontSystem;
use tex_engine::pdflatex::PDFTeXEngine;
use tex_engine::engine::state::State as OrigState;
use tex_engine::tex::catcodes::{AT_LETTER_SCHEME, CategoryCodeScheme, DEFAULT_SCHEME_U8};
use tex_engine::tex::catcodes::CategoryCode;
use tex_engine::tex::nodes::boxes::TeXBox;
use crate::nodes::RusTeXNode;
use crate::output::RusTeXOutput;
use crate::shipout;
use crate::shipout::make_page;
use crate::state::RusTeXState;
use crate::stomach::{CLOSE_FONT, close_font, RusTeXStomach};
use tex_engine::engine::utils::memory::MemoryManager;
use tex_engine::prelude::*;

pub(crate) type Extension = super::extension::RusTeXExtension;
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
    type Extension = Extension;
    type Int = i32;
    type Dim = Dim32;
    type MuDim = Mu;
    type Num = tex::numerics::DefaultNumSet;
    type State = RusTeXState;
    type File = VirtualFile<u8>;
    type FileSystem = crate::files::RusTeXFileSystem;
    type Outputs = RusTeXOutput;
    type Mouth = DefaultMouth<Self>;
    type Gullet = DefaultGullet<Self>;
    type CustomNode = RusTeXNode;
    type Stomach = RusTeXStomach;
    type Font = tex_engine::engine::fontsystem::TfmFont<i32,Dim32,InternedCSName<u8>>;
    type FontSystem = super::fonts::Fontsystem;
}



thread_local! {
    static MAIN_STATE : Mutex<Option<(RusTeXState,MemoryManager<CompactToken>)>> = Mutex::new(None);
    static FONT_SYSTEM : Mutex<Option<super::fonts::Fontsystem>> = Mutex::new(None);
}

fn get_state(log:bool) -> (RusTeXState,MemoryManager<CompactToken>) {
    MAIN_STATE.with(|state| {
        let mut guard = state.lock().unwrap();
        match &mut *guard {
            Some((s, m)) =>return  (s.clone(), m.clone()),
            n => {
                //let start = std::time::Instant::now();
                let mut engine = DefaultEngine::<Types>::new();
                if log { engine.aux.outputs = RusTeXOutput::Print(true);}
                register_simple_expandable(&mut engine,CLOSE_FONT,close_font);
                engine.state.register_primitive(&mut engine.aux,"rustexBREAK",PrimitiveCommand::Unexpandable {
                    scope:CommandScope::Any,
                    apply:|_,_| {
                        println!("HERE!")
                    }
                });
                engine.initialize_pdflatex();
                register_command(&mut engine, true, "LaTeX", "",
                                 "L\\kern-.3em\\raise.5ex\\hbox{\\check@mathfonts\\fontsize\\sf@size\\z@\\math@fontsfalse\\selectfont A}\\kern-.15em\\TeX",
                                 true, false
                );
                crate::pgf::register_pgf(&mut engine);
                *n = Some((engine.state.clone(), engine.aux.memory.clone()));
                FONT_SYSTEM.with(|f| f.lock().unwrap().replace(engine.fontsystem.clone()));
                //println!("Initialized in {:?}", start.elapsed());
                return (engine.state, engine.aux.memory)
            }
        }
    })
}

fn get_engine(log:bool) -> DefaultEngine<Types> {
    let (mut state,mut memory) = get_state(log);
    let fontsystem = FONT_SYSTEM.with(|f| f.lock().unwrap().clone()).unwrap();
    let mut aux = EngineAux {
        outputs: RusTeXOutput::None,
        error_handler: ErrorThrower::new(),
        start_time:chrono::Local::now(),
        extension: Extension::new(&mut memory),
        memory,
        jobname: String::new()
    };
    let mut mouth = DefaultMouth::new(&mut aux,&mut state);
    let gullet = DefaultGullet::new(&mut aux,&mut state,&mut mouth);
    let stomach = RusTeXStomach::new(&mut aux,&mut state);
    DefaultEngine {
        state,
        aux,
        fontsystem,
        filesystem: crate::files::RusTeXFileSystem::new(tex_engine::utils::PWD.to_path_buf()),
        mouth,gullet,stomach
    }
}

pub struct CompilationResult {
    pub out:String,
    pub error:Option<TeXError>,
    pub missing_glyphs: Box<[(String,u8,String)]>,
    pub missing_fonts: Box<[String]>
}

pub trait RusTeXEngineT {
    fn initialize(log:bool);
    fn get() -> Self;
    fn do_file<S:AsRef<str>>(file:S,verbose:bool,log:bool,sourcerefs:bool) -> CompilationResult;
}

pub(crate) fn register_command(e: &mut DefaultEngine<Types>, globally:bool, name:&'static str, sig:&'static str, exp:&'static str, protect:bool, long:bool) {
    let e = e.get_engine_refs();
    let name = e.aux.memory.cs_interner_mut().new(name);
    let mut cmd = Macro::new(
        e.aux.memory.cs_interner_mut(),
        &AT_LETTER_SCHEME,sig,exp);
    if protect { cmd.protected = true }
    if long { cmd.long = true }
    e.state.set_command(&e.aux, name, Some(TeXCommand::Macro(cmd)), globally)
}

/*pub struct RusTeXEngine {
    engine: DefaultEngine<Types>
}*/
pub type RusTeXEngine = DefaultEngine<Types>;
impl RusTeXEngineT for RusTeXEngine {

    fn initialize(log:bool) { let _ = get_engine(log); }

    fn get() -> Self { get_engine(false) }
    fn do_file<S:AsRef<str>>(file:S,verbose:bool,log:bool,sourcerefs:bool) -> CompilationResult {
        use std::fmt::Write;
        let mut engine = Self::get();
        engine.stomach.continuous = true;
        if log { engine.aux.outputs = RusTeXOutput::Print(verbose); }

        let start = std::time::Instant::now();
        let res = match engine.do_file_pdf(file.as_ref(),|e,n| shipout::shipout(e,n)) {
            Ok(_) => None,
            Err(e) => {
                engine.aux.outputs.errmessage(format!("{}\n\nat {}",e,engine.mouth.current_sourceref().display(&engine.filesystem)));
                Some(e)
            }
        };

        //let cap = engine.aux.memory.cs_interner().cap();
        //println!("\n\nCapacity: {} of {} ({:.2}%)",cap,0x8000_0000,(cap as f32 / (0x8000_0000u32 as f32)) * 100.0);

        // ----------------
        let out = std::mem::take(&mut engine.aux.extension.state.output);
        let mut ret = String::new();
        let (fonts,glyphs) = {
            let mut refs = engine.get_engine_refs();
            let (fonts,glyphs) = shipout::split_state(&mut refs, |engine, state| {
                let mut page = make_page(engine, state, |_, state| {
                    for c in out { state.push_child(c) }
                });
                page.classes = vec!("rustex-body".into());
                let topfont = state.fonts.first().unwrap().clone();
                let dp = page.displayable(&engine.fontsystem.glyphmaps, &engine.filesystem, *state.widths.first().unwrap(), &topfont, sourcerefs);
                write!(ret, "{}{}{}", shipout::PREAMBLE,dp, shipout::POSTAMBLE).unwrap();
                for k in dp.get_missings() {
                    state.missing_fonts.insert(k);
                }
                (std::mem::take(&mut state.missing_fonts), std::mem::take(&mut state.missing_glyphs))
            });
            (fonts,glyphs)
        };

        println!("Finished after {:?}", start.elapsed());

        FONT_SYSTEM.with(|f| f.lock().unwrap().replace(engine.fontsystem));

        CompilationResult {
            out:ret,
            error:res,
            missing_glyphs:glyphs.into_iter().collect::<Vec<_>>().into_boxed_slice(),
            missing_fonts:fonts.into_iter().collect::<Vec<_>>().into_boxed_slice()
        }
    }
}