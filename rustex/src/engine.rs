use std::path::Path;
use std::sync::Mutex;
use tex_engine::commands::{TeXCommand, Macro};
use tex_engine::engine::{DefaultEngine, EngineAux, EngineReferences, EngineTypes};
use tex_engine::engine::filesystem::{File, SourceReference, VirtualFile};
use tex_engine::engine::gullet::DefaultGullet;
use tex_engine::engine::mouth::DefaultMouth;
use tex_engine::tex;
use tex_engine::tex::numerics::{Dim32, Mu};
use tex_engine::tex::tokens::CompactToken;
use tex_engine::utils::errors::{ErrorThrower, TeXError, TeXResult};
use tex_engine::engine::TeXEngine;
use tex_engine::engine::EngineExtension;
use tex_engine::engine::mouth::Mouth;
use tex_engine::engine::gullet::Gullet;
use tex_engine::engine::stomach::Stomach as StomachT;
use tex_engine::engine::filesystem::FileSystem;
use tex_engine::pdflatex::PDFTeXEngine;
use tex_engine::engine::state::State as OrigState;
use tex_engine::tex::catcodes::AT_LETTER_SCHEME;
use tex_engine::tex::nodes::boxes::TeXBox;
use crate::nodes::RusTeXNode;
use crate::output::RusTeXOutput;
use crate::shipout;
use crate::shipout::make_page;
use crate::state::RusTeXState;
use crate::stomach::RusTeXStomach;
use tex_engine::engine::utils::memory::MemoryManager;
use tex_engine::prelude::*;

pub type Extension = super::extension::RusTeXExtension;
pub(crate) type Font = tex_engine::engine::fontsystem::TfmFont<i32,Dim32,InternedCSName<u8>>;
pub(crate) type Bx = TeXBox<Types>;
pub(crate) type SRef = SourceReference<<<Types as EngineTypes>::File as File>::SourceRefID>;
pub(crate) type Refs<'a,'b> = &'a mut EngineReferences<'b,Types>;
pub(crate) type CSName = InternedCSName<u8>;

#[derive(Clone,Debug,Copy)]
pub struct Types;

pub type Res<R> = TeXResult<R,Types>;

impl EngineTypes for Types {
    type Char = u8;
    type CSName = CSName;
    type Token = CompactToken;
    type Extension = Extension;
    type File = VirtualFile<u8>;
    type FileSystem = crate::files::RusTeXFileSystem;
    type Int = i32;
    type Dim = Dim32;
    type MuDim = Mu;
    type Num = tex::numerics::DefaultNumSet;
    type State = RusTeXState;
    type Outputs = RusTeXOutput;
    type Mouth = DefaultMouth<Self>;
    type Gullet = DefaultGullet<Self>;
    type Stomach = RusTeXStomach;
    type CustomNode = RusTeXNode;
    type Font = tex_engine::engine::fontsystem::TfmFont<i32,Dim32,InternedCSName<u8>>;
    type FontSystem = super::fonts::Fontsystem;
    type ErrorHandler = ErrorThrower<Self>;
}

thread_local! {
    static MAIN_STATE : Mutex<Option<(RusTeXState,MemoryManager<CompactToken>)>> = const { Mutex::new(None) };
    static FONT_SYSTEM : Mutex<Option<super::fonts::Fontsystem>> = const { Mutex::new(None) };
}


fn get_state(log:bool) -> (RusTeXState,MemoryManager<CompactToken>) {
    MAIN_STATE.with(|state| {
        let mut guard = state.lock().unwrap();
        match &mut *guard {
            Some((s, m)) =>(s.clone(), m.clone()),
            n => {
                //let start = std::time::Instant::now();
                let mut engine = DefaultEngine::<Types>::new();
                if log { engine.aux.outputs = RusTeXOutput::Print(true);}
                crate::commands::register_primitives_preinit(&mut engine);
                engine.initialize_pdflatex().unwrap();
                crate::commands::register_primitives_postinit(&mut engine);
                *n = Some((engine.state.clone(), engine.aux.memory.clone()));
                FONT_SYSTEM.with(|f| f.lock().unwrap().replace(engine.fontsystem.clone()));
                //println!("Initialized in {:?}", start.elapsed());
                (engine.state, engine.aux.memory)
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
    pub error:Option<TeXError<Types>>,
    pub missing_glyphs: Box<[(String,u8,String)]>,
    pub missing_fonts: Box<[String]>
}

pub trait RusTeXEngineT {
    fn initialize(log:bool);
    fn get() -> Self;
    fn run<S:AsRef<str>>(&mut self,file:S,sourcerefs:bool) -> CompilationResult;
    fn do_file<S:AsRef<str>>(file:S,verbose:bool,log:bool,sourcerefs:bool) -> CompilationResult;
}

pub(crate) fn register_command(e: &mut DefaultEngine<Types>, globally:bool, name:&'static str, sig:&'static str, exp:&'static str, protect:bool, long:bool) {
    let e = e.get_engine_refs();
    let name = e.aux.memory.cs_interner_mut().from_str(name);
    let mut cmd = Macro::new::<_,_,Types>(
        e.aux.memory.cs_interner_mut(),
        &AT_LETTER_SCHEME,sig,exp).unwrap();
    if protect { cmd.protected = true }
    if long { cmd.long = true }
    e.state.set_command(e.aux, name, Some(TeXCommand::Macro(cmd)), globally)
}

/*pub struct RusTeXEngine {
    engine: DefaultEngine<Types>
}*/
pub type RusTeXEngine = DefaultEngine<Types>;
impl RusTeXEngineT for RusTeXEngine {

    fn initialize(log:bool) { let _ = get_engine(log); }

    fn get() -> Self { get_engine(false) }

    fn run<S:AsRef<str>>(&mut self,file:S,sourcerefs:bool) -> CompilationResult {
        use std::fmt::Write;
        let res = match self.do_file_pdf(file.as_ref(),shipout::shipout) {
            Ok(_) => None,
            Err(e) => {
                self.aux.outputs.errmessage(format!("{}\n\nat {}",e,self.mouth.current_sourceref().display(&self.filesystem)));
                Some(e)
            }
        };
        let out = std::mem::take(&mut self.aux.extension.state.output);
        let mut ret = String::new();
        let (fonts,glyphs) = {
            let mut refs = self.get_engine_refs();
            let (fonts,glyphs) = shipout::state::split_state(&mut refs, |engine, state| {
                let mut page = make_page(engine, state, |_, state| {
                    for c in out { state.push_child(c) };Ok(())
                }).unwrap();
                page.classes = vec!("rustex-body".into());
                let topfont = state.fonts.first().unwrap().clone();

                ret.write_str("<!DOCTYPE html>\n<html lang=\"en\"").unwrap();
                for (k,v) in engine.aux.extension.top.iter() {
                    write!(ret," {}=\"{}\"",k,v).unwrap();
                }
                ret.write_str(" prefix=\"").unwrap();
                for (k,v) in engine.aux.extension.namespaces.iter() {
                    write!(ret,"{}: {} ",k,v).unwrap();
                }
                ret.write_str("\">\n<head>\t<meta charset=\"UTF-8\">\n").unwrap();
                for m in engine.aux.extension.metas.iter() {
                    ret.write_str("\t<meta").unwrap();
                    for (k,v) in m.iter() {
                        write!(ret," {}=\"{}\"",k,v).unwrap();
                    }
                    ret.write_str(">\n").unwrap();
                }
                ret.write_str(&format!("\t<title>{}</title>\n",
                    Path::new(file.as_ref()).file_stem().unwrap().to_str().unwrap()
                )).unwrap();
                ret.write_str("\t<link rel=\"stylesheet\" type=\"text/css\" href=\"file:///home/jazzpirate/work/Software/sTeX/RusTeXNew/rustex/src/resources/rustex.css\">\n").unwrap();
                for c in engine.aux.extension.css_files.iter() {
                    ret.write_str(&format!("\t<link rel=\"stylesheet\" type=\"text/css\" href=\"{}\">\n",c)).unwrap();
                }
                for l in &state.fontlinks {
                    ret.write_str(&format!("\t<link rel=\"stylesheet\" href=\"{}\">\n",l)).unwrap();
                }
                ret.write_str("</head>\n<body>\n").unwrap();

                let dp = page.displayable(&engine.fontsystem.glyphmaps, engine.filesystem, *state.widths.first().unwrap(), &topfont, sourcerefs);
                write!(ret, "{}", dp).unwrap();
                ret.write_str("\n</body>\n</html>").unwrap();
                for k in dp.get_missings() {
                    state.missing_fonts.insert(k);
                }
                (std::mem::take(&mut state.missing_fonts), std::mem::take(&mut state.missing_glyphs))
            });
            (fonts,glyphs)
        };
        CompilationResult {
            out:ret,
            error:res,
            missing_glyphs:glyphs.into_iter().collect::<Vec<_>>().into_boxed_slice(),
            missing_fonts:fonts.into_iter().collect::<Vec<_>>().into_boxed_slice()
        }
    }

    fn do_file<S:AsRef<str>>(file:S,verbose:bool,log:bool,sourcerefs:bool) -> CompilationResult {
        let mut engine = Self::get();
        engine.stomach.continuous = true;
        if log { engine.aux.outputs = RusTeXOutput::Print(verbose); }
        let ret = <Self as RusTeXEngineT>::run(&mut engine,file,sourcerefs);
        FONT_SYSTEM.with(|f| f.lock().unwrap().replace(engine.fontsystem));
        ret
    }
}