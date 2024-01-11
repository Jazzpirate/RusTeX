use std::sync::Mutex;
use lazy_static::lazy_static;
use log::error;
use tex_engine::commands::{Command, CommandScope, Unexpandable};
use tex_engine::commands::primitives::register_unexpandable;
use tex_engine::engine::{DefaultEngine, EngineAux, EngineReferences, EngineTypes, utils};
use tex_engine::engine::filesystem::{File, SourceReference, VirtualFile};
use tex_engine::engine::gullet::DefaultGullet;
use tex_engine::engine::mouth::DefaultMouth;
use tex_engine::engine::utils::memory::{InternedCSName, PRIMITIVES};
use tex_engine::engine::utils::outputs::LogOutputs;
use tex_engine::tex;
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
use tex_engine::engine::fontsystem::FontSystem;
use tex_engine::pdflatex::PDFTeXEngine;
use tex_engine::engine::state::State as OrigState;
use tex_engine::tex::catcodes::CategoryCodeScheme;
use tex_engine::tex::catcodes::CategoryCode;
use tex_engine::tex::nodes::boxes::TeXBox;
use crate::nodes::RusTeXNode;
use crate::output::RusTeXOutput;
use crate::shipout;
use crate::shipout::make_page;
use crate::state::RusTeXState;
use crate::stomach::{CLOSE_FONT, close_font, RusTeXStomach};
use tex_engine::engine::utils::memory::MemoryManager;

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
    type FileSystem = crate::files::RusTeXFileSystem;
    type Outputs = RusTeXOutput;
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

fn get_state(log:bool) -> (RusTeXState,Memory) {
    MAIN_STATE.with(|state| {
        let mut guard = state.lock().unwrap();
        match &mut *guard {
            Some((s, m)) =>return  (s.clone(), m.clone()),
            n => {
                let start = std::time::Instant::now();
                let mut engine = DefaultEngine::<Types>::new();
                if log { engine.aux.outputs = RusTeXOutput::Print(true);}
                register_unexpandable(&mut engine,CLOSE_FONT,CommandScope::Any,close_font);
                engine.register_primitive(Command::Unexpandable(
                    Unexpandable {
                        name:PRIMITIVES.get("rustexBREAK"),
                        scope:CommandScope::Any,
                        apply:|_,_| {
                            println!("HERE!")
                        }
                    }
                ),"rustexBREAK");
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
    let (mut state,memory) = get_state(log);
    let fontsystem = FONT_SYSTEM.with(|f| f.lock().unwrap().clone()).unwrap();
    let mut aux = EngineAux {
        memory,
        outputs: RusTeXOutput::None,
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
        filesystem: crate::files::RusTeXFileSystem::new(tex_engine::utils::PWD.to_path_buf()),
        mouth,gullet,stomach
    }
}
pub trait RusTeXEngineT {
    fn initialize(log:bool);
    fn get() -> Self;
    fn do_file<S:AsRef<str>>(file:S,verbose:bool,log:bool,sourcerefs:bool) -> String;
}

lazy_static! {
pub(crate) static ref AT_LETTER_SCHEME : CategoryCodeScheme<u8> = {
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
    for i in 64..91 { catcodes[i] = CategoryCode::Letter}
    for i in 97..123 { catcodes[i] = CategoryCode::Letter}
    catcodes
};
}
pub(crate) fn register_command(e: &mut DefaultEngine<Types>, globally:bool, name:&'static str, sig:&'static str, exp:&'static str, protect:bool, long:bool) {
    use tex_engine::engine::utils::memory::MemoryManager;
    use tex_engine::tex::control_sequences::CSHandler;
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
    fn initialize(log:bool) { let _ = get_engine(log); }
    #[inline(always)]
    fn get() -> Self { get_engine(false) }
    fn do_file<S:AsRef<str>>(file:S,verbose:bool,log:bool,sourcerefs:bool) -> String {
        use std::fmt::Write;
        let mut engine = Self::get();
        engine.stomach.continuous = true;
        if log { engine.aux.outputs = RusTeXOutput::Print(verbose); }

        match engine.do_file_pdf(file.as_ref(),|e,n| shipout::shipout(e,n)) {
            Ok(_) => (),
            Err(e) => engine.aux.outputs.errmessage(e)
        }


        let cap = engine.aux.memory.cs_interner().cap();
        println!("\n\nCapacity: {} of {} ({:.2}%)",cap,0x8000_0000,(cap as f64 / (0x8000_0000u32 as f64)) * 100.0);



        // ----------------
        let out = std::mem::take(&mut engine.aux.extension.state.output);
        let mut ret = String::new();
        {
            let mut refs = engine.get_engine_refs();
            shipout::split_state(&mut refs, |engine, state| {
                let mut page = make_page(engine, state, |_, state| {
                    for c in out { state.push_child(c) }
                });
                page.classes = vec!("rustex-body".into());
                let topfont = state.fonts.first().unwrap().clone();
                write!(ret, "{}{}{}", shipout::PREAMBLE,
                       page.displayable(&engine.fontsystem.glyphmaps, &engine.filesystem, *state.widths.first().unwrap(), &topfont, sourcerefs),
                       shipout::POSTAMBLE).unwrap();
            });
        }
        //std::fs::write(crate::shipout::TEST_FILE, &format!("{}{}{}", crate::shipout::PREAMBLE,
        //                                                   page.displayable(&engine.fontsystem.glyphmaps,&engine.filesystem,*state.widths.first().unwrap(),&null,do_refs),
        //                                                   crate::shipout::POSTAMBLE)).unwrap();
        //println!("HERE");
        //state.output = page.children;
        // ----------------


        //engine.do_file_pdf(file.as_ref(),|e,n| super::shipout::shipout_paginated(e,n)).unwrap();


        FONT_SYSTEM.with(|f| f.lock().unwrap().replace(engine.fontsystem));
        ret
    }

}
