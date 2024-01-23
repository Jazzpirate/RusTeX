// #![forbid(unsafe_code)]

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use std::collections::HashSet;
//static ALLOC: rpmalloc::RpMalloc = rpmalloc::RpMalloc;
use RusTeX::engine::{RusTeXEngine,RusTeXEngineT};

use std::env;
use std::path::{Path, PathBuf};
use clap::Parser;
use log::info;
use pdfium_render::page_objects_common::PdfPageObjectsCommon;
use RusTeX::engine::Types;
use RusTeX::files::RusTeXFileSystem;
use RusTeX::output::RusTeXOutput;
use tex_engine::commands::{TeXCommand, CommandScope, PrimitiveCommand};
use tex_engine::commands::primitives::{register_simple_expandable, register_unexpandable};
use tex_engine::engine::{DefaultEngine, TeXEngine};
use tex_engine::pdflatex::commands::register_pdftex_primitives;
use tex_engine::pdflatex::PlainPDFTeXEngine;

fn main() {
    //profile()
    //thesis()
    //run()
    //test()
    //temp_test()
    //notes()
    //test2()
    test_all()
}

fn test_all() {
    use std::io::BufRead;
    let files = "/home/jazzpirate/textest.txt";
    let allfiles_reader = std::io::BufReader::new(std::fs::File::open(files).unwrap());
    let mut allfiles = Vec::new();
    for line in allfiles_reader.lines() {
        let line = line.unwrap();
        let (a,b) = line.split_once("|").unwrap();
        if b.trim() == "true" {
            allfiles.push(PathBuf::from(a));
        }
    }
    let files = "/home/jazzpirate/textest_dones.txt";
    let mut dones = Vec::new();
    let donefiles_reader = std::io::BufReader::new(std::fs::File::open(files).unwrap());
    for line in donefiles_reader.lines() {
        let line = line.unwrap();
        dones.push(PathBuf::from(line));
    }
    let allfiles: Vec<_> = allfiles.into_iter().filter(|x| !dones.contains(x)).collect();
    println!("Testing {} files",allfiles.len());
    let mut missing_glyphs = HashSet::new();
    let mut missing_fonts = HashSet::new();
    let len = allfiles.len();
    for (i,f) in allfiles.into_iter().enumerate() {
        println!("Testing file {} of {}: {}",i+1,len,f.display());
        let ret = RusTeXEngine::do_file(f.to_str().unwrap(),false,false,false);
        for gl in ret.missing_glyphs.into_vec().into_iter() {
            missing_glyphs.insert(gl);
        }
        for fnt in ret.missing_fonts.into_vec().into_iter() {
            missing_fonts.insert(fnt);
        }
        match ret.error {
            None => {
                dones.push(f);
                std::fs::write(files, dones.iter().map(|x| x.to_str().unwrap()).collect::<Vec<_>>().join("\n")).unwrap();
                let out = PathBuf::from("/home/jazzpirate/temp/out").join(format!("{}.html",i+1));
                std::fs::write(out, &ret.out).unwrap();
            }
            Some(e) => {
                println!("Errored");
                panic!("Errored: {}\n{}\n\nMissing glyphs: {}\nMissing web fonts: {}",
                       f.display(),e.msg,
                       missing_glyphs.into_iter().map(|(x,c,y)| format!("({},{},{})",x,c,y)).collect::<Vec<_>>().join(", "),
                          missing_fonts.into_iter().map(|x| x.to_string()).collect::<Vec<_>>().join(", ")
                );
            }
        }
    }
    println!("Finished \\o/\n\nMissing glyphs: {}\nMissing web fonts: {}",
           missing_glyphs.into_iter().map(|(x,c,y)| format!("({},{},{})",x,c,y)).collect::<Vec<_>>().join(", "),
           missing_fonts.into_iter().map(|x| x.to_string()).collect::<Vec<_>>().join(", ")
    );
}

fn test() {
    //env_logger::builder().filter_level(log::LevelFilter::Info).try_init();
    let ret = RusTeXEngine::do_file("/home/jazzpirate/work/Software/sTeX/RusTeXNew/test/test.tex",false,true,true);
    std::fs::write("/home/jazzpirate/work/Software/sTeX/RusTeXNew/test/test.html", &ret.out).unwrap();
}

fn temp_test() {
    //env_logger::builder().filter_level(log::LevelFilter::Info).try_init();
    let ret = RusTeXEngine::do_file("/home/jazzpirate/work/Software/sTeX/RusTeXNew/test/numtest.tex",false,true,true);
    //let ret = RusTeXEngine::do_file("/home/jazzpirate/work/LaTeX/Papers/ODK/WP6/MACIS17-interop/gap_singular_mitm_fig.tex",true,true,true);
    //let ret = RusTeXEngine::do_file("/home/jazzpirate/work/LaTeX/Papers/17 - Alignment Translation/macros/kwarc/workplan/workplan-template.tex",true,true,true);
    std::fs::write("/home/jazzpirate/work/Software/sTeX/RusTeXNew/test/numtest.html", &ret.out).unwrap();
}

fn thesis() {
    //env_logger::builder().filter_level(log::LevelFilter::Info).try_init();
    let ret = RusTeXEngine::do_file("/home/jazzpirate/work/LaTeX/Papers/19 - Thesis/thesis.tex",false,true,true);
    std::fs::write("/home/jazzpirate/work/Software/sTeX/RusTeXNew/test/thesis.html", &ret.out).unwrap();
}

fn notes() {
    //env_logger::builder().filter_level(log::LevelFilter::Info).try_init();
    // /home/jazzpirate/work/MathHub/MiKoMH/AI/source/search/slides/ts-ex.en.tex
    let ret = RusTeXEngine::do_file("/home/jazzpirate/work/MathHub/MiKoMH/AI/source/course/notes/notes.tex",false,true,true);
    //let ret = RusTeXEngine::do_file("/home/jazzpirate/work/MathHub/MiKoMH/CompLog/source/kr/tikz/axioms2.tex",true,true,true);
    std::fs::write("/home/jazzpirate/work/Software/sTeX/RusTeXNew/test/ainotes.html", &ret.out).unwrap();
}

fn profile() {
    use tex_engine::engine::filesystem::FileSystem;
    use tex_engine::engine::state::State;
    println!("Profiling...");
    //env_logger::builder().filter_level(log::LevelFilter::Info).try_init();
    let mut engine = DefaultEngine::<Types>::new();
    RusTeX::commands::register_primitives_preinit(&mut engine);
    engine.aux.outputs = RusTeXOutput::Print(false);
    engine.initialize_etex();
    register_pdftex_primitives(&mut engine);
    engine.init_file("pdftexconfig.tex").unwrap();
    let state = engine.state.clone();
    for _ in 0..30 {
        engine.load_latex();
        engine.state = state.clone();
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
            std::fs::write(o, &ret.out).unwrap();
        }
        _ => {
            println!("No input/output file given. Testing latex.ltx...");
            test_latex_ltx();
            println!("Done");
        }
    }
}

fn test_latex_ltx() {
    use tex_engine::engine::state::State;
    let mut engine = DefaultEngine::<Types>::new();
    RusTeX::commands::register_primitives_preinit(&mut engine);

    //engine.state.set_primitive_int(&mut engine.aux,PRIMITIVES.tracingcommands,1,true);
    //engine.state.set_primitive_int(&mut engine.aux,PRIMITIVES.tracingifs,1,true);

    engine.aux.outputs = RusTeXOutput::Print(true);
    engine.initialize_etex();
    register_pdftex_primitives(&mut engine);
    engine.init_file("pdftexconfig.tex").unwrap();
    engine.load_latex();
}