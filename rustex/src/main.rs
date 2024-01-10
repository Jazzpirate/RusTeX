// #![forbid(unsafe_code)]

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;
//static ALLOC: rpmalloc::RpMalloc = rpmalloc::RpMalloc;
pub use RusTeX::engine::{RusTeXEngine,RusTeXEngineT};

use std::env;
use std::path::Path;
use clap::Parser;
use log::info;
use RusTeX::engine::Types;
use RusTeX::files::RusTeXFileSystem;
use RusTeX::output::RusTeXOutput;
use RusTeX::stomach::{CLOSE_FONT, close_font};
use tex_engine::commands::{Command, CommandScope, Unexpandable};
use tex_engine::commands::primitives::register_unexpandable;
use tex_engine::engine::{DefaultEngine, TeXEngine};
use tex_engine::engine::utils::memory::PRIMITIVES;
use tex_engine::pdflatex::commands::register_pdftex_primitives;
use tex_engine::pdflatex::PlainPDFTeXEngine;

fn main() {
    //profile()
    //thesis()
    //run()
    //test()
    notes()
    //test2()
}

fn test2() {
    tex_engine::engine::filesystem::kpathsea::KpathseaBase::new();
}

fn test() {
    //env_logger::builder().filter_level(log::LevelFilter::Info).try_init();
    let ret = RusTeXEngine::do_file("/home/jazzpirate/work/Software/sTeX/RusTeXNew/test/test.tex",false,true,true);
    std::fs::write("/home/jazzpirate/work/Software/sTeX/RusTeXNew/test/test.html", &ret).unwrap();
}

fn thesis() {
    //env_logger::builder().filter_level(log::LevelFilter::Info).try_init();
    let ret = RusTeXEngine::do_file("/home/jazzpirate/work/LaTeX/Papers/19 - Thesis/thesis.tex",false,true,true);
    std::fs::write("/home/jazzpirate/work/Software/sTeX/RusTeXNew/test/thesis.html", &ret).unwrap();
}

fn notes() {
    //env_logger::builder().filter_level(log::LevelFilter::Info).try_init();
    // /home/jazzpirate/work/MathHub/MiKoMH/AI/source/search/slides/ts-ex.en.tex
    let ret = RusTeXEngine::do_file("/home/jazzpirate/work/MathHub/MiKoMH/AI/source/course/notes/notes.tex",true,true,true);
    //let ret = RusTeXEngine::do_file("/home/jazzpirate/work/MathHub/MiKoMH/AI/source/game-play/slides/agenda.en.tex",true,true,true);
    std::fs::write("/home/jazzpirate/work/Software/sTeX/RusTeXNew/test/ainotes.html", &ret).unwrap();
}

fn profile() {
    use tex_engine::engine::filesystem::FileSystem;
    println!("Profiling...");
    //env_logger::builder().filter_level(log::LevelFilter::Info).try_init();
    let mut engine = DefaultEngine::<Types>::new();
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
    engine.aux.outputs = RusTeXOutput::Print(false);
    engine.initialize_etex();
    register_pdftex_primitives(&mut engine);
    engine.init_file("pdftexconfig.tex").unwrap();
    let state = engine.state.clone();
    for _ in 0..30 {
        engine.load_latex();
        engine.reset(state.clone());
        engine.filesystem = RusTeXFileSystem::new(tex_engine::utils::PWD.to_path_buf());
    }
}

#[derive(Parser,Debug)]
#[clap(author, version, about, long_about = None)]
struct Parameters {
    /// Input file (tex)
    #[clap(short, long)]
    input: Option<String>,

    /// Input dir (tex)
    #[clap(short, long)]
    dir: Option<String>,

    /// Input string (tex)
    #[clap(short, long)]
    text: Option<String>,

    /// Output file (html)
    #[clap(short, long)]
    output: Option<String>,
/*
    /// log file (html)
    #[clap(short, long)]
    log: Option<String>,
*/
    /// insert source references
    #[clap(short, long, default_value_t = true)]
    sourcerefs: bool,

    /// console log
    #[clap(short, long, default_value_t = true)]
    console: bool,

    /// verbose
    #[clap(short, long, default_value_t = false)]
    verbose: bool,

    /// profile
    #[clap(short, long, default_value_t = false)]
    profile: bool,
}

fn run() {
    //env_logger::builder().filter_level(log::LevelFilter::Info).try_init();
    let params = Parameters::parse();
    if params.profile {
        profile();
        return;
    }
    match (params.input,params.output) {
        (Some(i),Some(o)) => {
            let ret = RusTeXEngine::do_file(i.as_str(),params.verbose,params.console,params.sourcerefs);
            std::fs::write(o, &ret).unwrap();
        }
        _ => {
            println!("No input/output file given. Testing latex.ltx...");
            RusTeXEngine::initialize(true);
            println!("Done");
        }
    }
}