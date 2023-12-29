// #![forbid(unsafe_code)]

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;
//static ALLOC: rpmalloc::RpMalloc = rpmalloc::RpMalloc;
pub use RusTeX::engine::{RusTeXEngine,RusTeXEngineT};

use std::env;
use std::path::Path;
use clap::Parser;
use log::info;
use tex_engine::engine::TeXEngine;
use tex_engine::pdflatex::commands::register_pdftex_primitives;
use tex_engine::pdflatex::PlainPDFTeXEngine;

fn main() {
    //profile()
    thesis()
    //run()
}

fn thesis() {
    env_logger::builder().filter_level(log::LevelFilter::Info).try_init();
    //RusTeXEngine::do_file("/home/jazzpirate/work/LaTeX/Papers/19 - Thesis/thesis.tex")
    RusTeXEngine::do_file("/home/jazzpirate/test.tex")
}

fn profile() {
    env_logger::builder().filter_level(log::LevelFilter::Info).try_init();
    let mut engine = PlainPDFTeXEngine::new();
    engine.initialize_etex();
    register_pdftex_primitives(&mut engine);
    engine.init_file("pdftexconfig.tex").unwrap();
    let state = engine.state.clone();
    for _ in 0..30 {
        engine.load_latex();
        engine.reset(state.clone());
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

    /// Output file (xhtml)
    #[clap(short, long)]
    output: Option<String>,
}

fn run() {
    env_logger::builder().filter_level(log::LevelFilter::Info).try_init();
    let params = Parameters::parse();
    match params.input {
        Some(i) => {
            RusTeX::engine::RusTeXEngine::do_file(i.as_str());
            // TODO
        }
        None => {
            info!("No input file given. Testing latex.ltx...");
            RusTeX::engine::RusTeXEngine::initialize();
            info!("Done");
        }
    }
}