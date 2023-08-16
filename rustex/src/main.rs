use std::path::PathBuf;
use ansi_term::Color::*;
use log::warn;
use tex_engine::engine::filesystem::{FileSystem, KpseVirtualFileSystem, VirtualFile};
use tex_engine::engine::gullet::{TeXGullet,Gullet};
use tex_engine::engine::state::{TeXState,State};
use tex_engine::engine::stomach::{NoShipoutDefaultStomach, ShipoutDefaultStomach};
use tex_engine::tex::commands::{BaseCommand, Command};
use tex_engine::tex::fonts::{TfmFont, TfmFontStore};
use tex_engine::tex::numbers::{Dimi32, Fill, MuFill, Mui32};
use tex_engine::tex::token::FileTokenReference;
use tex_engine::utils::Ptr;
use tex_engine::utils::strings::{CharType, TeXStr};
use tex_engine::engine::*;
use tex_engine::engine::memory::Memory;
use tex_engine::engine::mouth::{Mouth,StandardMouth};
use tex_engine::tex::commands::pdftex::PDFTeXNode;

#[derive(Clone,Copy,Debug)]
struct Default();
impl EngineType for Default {
    type Char = u8;
    type File = Ptr<VirtualFile<u8>>;
    type FileSystem = KpseVirtualFileSystem<u8>;
    type Font = TfmFont;
    type FontStore = TfmFontStore;
    type Node = PDFTeXNode<Self>;
    type Int = i32;
    type Dim = Dimi32;
    type SkipDim = Fill;
    type MuDim = Mui32;
    type MuStretchShrinkDim = MuFill;
    type CommandReference = ();
    type TokenReference = ();//FileTokenReference<Self>;
    type Mouth = StandardMouth<Self>;
    type State = TeXState<Self>;
    type Gullet = TeXGullet<Self>;
    type Stomach = ShipoutDefaultStomach<Self>;
}

macro_rules! catch {
    ($engine:ident,$e:expr) => {
        match $e {
            Ok(x) => x,
            Err(e) => {
                println!("{}\n\nat:{}\n   {}...",e.throw_string($engine.components().memory),$engine.components().current_position(),$engine.components().preview(100));
                panic!()
            }
        }
    }
}

fn main() {
    profile()
    //thesis()
}

fn engine() -> EngineStruct<Default> {
    let outputs = Outputs {
        error: |s|  { print!("\n{}",Red.paint(std::format!("{}",s))) },
        message: |s| { print!("{}",Yellow.paint(s)) },
        file_open:|pb| { print!("\n{}",Green.paint(format!("({}",pb))) },
        file_close:|pb| { print!("{}",Green.paint(format!(")"))) },
        write_18:|_| { },
        write_17:|s| { print!("{}",s) },
        write_16:|s| { print!("{}",White.bold().paint(s)) },
        write_neg1:|s| { print!("\n{}",Black.on(Blue).paint(s)) },
        write_other:|s| { print!("\n{}",Black.on(Green).paint(s)) },
    };

    let fs = KpseVirtualFileSystem::new(std::env::current_dir().unwrap());
    let mut memory = Memory::new();
    let fonts = TfmFontStore::new(&mut memory);
    let state = TeXState::new(&fonts);
    let gullet = TeXGullet::new();
    let stomach = ShipoutDefaultStomach::new();
    let mut engine = tex_engine::engine::EngineStruct::<Default>::new(fs,fonts,state,gullet,stomach,outputs);
    engine.memory = memory;
    engine.state.set_command(TeXStr::from_static("rustexBREAK",&mut engine.memory),Some(Command::new(BaseCommand::Unexpandable {
        name: "rustexBREAK",
        apply: |_,_| {
            println!("HERE!");
            //std::env::set_var("RUST_LOG","debug,tex_engine::tex::commands=trace,tex_engine::engine::gullet=trace");
            //env_logger::init();
            env_logger::builder().filter_level(log::LevelFilter::Trace).try_init();
            //trace();
            Ok(())
        },forces_mode:None
    },None)),true);
    catch!(engine,engine.initialize_etex());
    catch!(engine,engine.pdftex());
    engine
}

fn thesis() {
    use std::path::*;
    let mut engine = engine();
    catch!(engine,engine.latex());
    catch!(engine,engine.do_file(PathBuf::from("/home/jazzpirate/work/LaTeX/Papers/19 - Thesis/thesis.tex")));
}

fn profile() {
    let mut engine = engine();
    let state = engine.state.clone();
    catch!(engine,engine.latex());
    engine.set_state(state.clone());
    catch!(engine,engine.latex());
    engine.set_state(state.clone());
    catch!(engine,engine.latex());
    engine.set_state(state.clone());
    catch!(engine,engine.latex());
    engine.set_state(state.clone());
    catch!(engine,engine.latex());
    engine.set_state(state.clone());
    catch!(engine,engine.latex());
    engine.set_state(state.clone());
    catch!(engine,engine.latex());
    engine.set_state(state.clone());
    catch!(engine,engine.latex());
    engine.set_state(state.clone());
    catch!(engine,engine.latex());
    engine.set_state(state);
    catch!(engine,engine.latex());
}