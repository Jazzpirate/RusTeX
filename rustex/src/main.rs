// #![forbid(unsafe_code)]

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;
//static GLOBAL: rpmalloc::RpMalloc = rpmalloc::RpMalloc;

use std::collections::HashSet;
use RusTeX::engine::{RusTeXEngine, RusTeXEngineT, Settings};

use clap::Parser;
use std::path::{Path, PathBuf};
use tex_engine::commands::primitives::{register_simple_expandable, register_unexpandable};
use tex_engine::commands::{CommandScope, PrimitiveCommand, TeXCommand};
use tex_engine::engine::{DefaultEngine, TeXEngine};
use tex_engine::pdflatex::commands::register_pdftex_primitives;
use tex_engine::pdflatex::PlainPDFTeXEngine;
use RusTeX::engine::files::RusTeXFileSystem;
use RusTeX::engine::output::RusTeXOutput;
use RusTeX::engine::Types;

/*
notes   5:31
profile 1:38 / 1:38     ==> 1:43
thesis  0:15
 */
fn main() {
    //profile()
    //thesis()
    run()
    //test()
    //temp_test()
    //notes()
    //test2()
    //test_all()
}

fn test_all() {
    use std::io::BufRead;
    let files = "/home/jazzpirate/textest.txt";
    let allfiles_reader = std::io::BufReader::new(std::fs::File::open(files).unwrap());
    let mut allfiles = Vec::new();
    for line in allfiles_reader.lines() {
        let line = line.unwrap();
        let (a, b) = line.split_once("|").unwrap();
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
    let len = allfiles.len();
    let allfiles: Vec<_> = allfiles
        .into_iter()
        .enumerate()
        .filter(|(_, x)| !dones.contains(x))
        .collect();

    println!("Testing {} files", allfiles.len());
    //let mut missing_glyphs = HashSet::new();
    //let mut missing_fonts = HashSet::new();
    for (i, f) in allfiles.into_iter() {
        println!("Testing file {} of {}: {}", i + 1, len, f.display());
        let mut ret = RusTeXEngine::do_file(f.to_str().unwrap(), Settings::default());
        /*
        let mut glyphs = ret.missing_glyphs.clone().into_vec();
        glyphs.retain(|g| !missing_glyphs.contains(g));
        let mut fonts = ret.missing_fonts.clone().into_vec();
        fonts.retain(|g| !missing_fonts.contains(g));
        if !fonts.is_empty() {
            println!(
                "Missing fonts: {}",
                fonts
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            );
        }
        for f in fonts {
            missing_fonts.insert(f);
        }
        glyphs.sort_by(|a, b| a.2.cmp(&b.2));
        if !glyphs.is_empty() {
            println!(
                "Missing glyphs: {}",
                glyphs
                    .iter()
                    .map(|(x, c, y)| format!("({},{},{})", x, c, y))
                    .collect::<Vec<_>>()
                    .join(", ")
            );
        }
        for g in glyphs {
            missing_glyphs.insert(g);
        }

         */
        match ret.error.take() {
            None => {
                let out =
                    PathBuf::from("/home/jazzpirate/temp/out").join(format!("{}.html", i + 1));
                ret.write_out(&out).unwrap();
                dones.push(f);
                std::fs::write(
                    files,
                    dones
                        .iter()
                        .map(|x| x.to_str().unwrap())
                        .collect::<Vec<_>>()
                        .join("\n"),
                )
                .unwrap();
            }
            Some((e, _)) => {
                println!("Errored");
                panic!(
                    "Errored: {}\n{}\n\n", //Missing glyphs: {}\nMissing web fonts: {}",
                    f.display(),
                    e /*,
                      missing_glyphs
                          .into_iter()
                          .map(|(x, c, y)| format!("({},{},{})", x, c, y))
                          .collect::<Vec<_>>()
                          .join(", "),
                      missing_fonts
                          .into_iter()
                          .map(|x| x.to_string())
                          .collect::<Vec<_>>()
                          .join(", ")
                          */
                );
            }
        }
    }
    //let mut missing_glyphs: Vec<_> = missing_glyphs.into_iter().collect();
    //missing_glyphs.sort_by(|a, b| a.2.cmp(&b.2));
    println!(
        "Finished \\o/\n\n", //Missing glyphs: {}\nMissing web fonts: {}",
                             /*missing_glyphs
                                 .into_iter()
                                 .map(|(x, c, y)| format!("({},{},{})", x, c, y))
                                 .collect::<Vec<_>>()
                                 .join(", "),
                             missing_fonts
                                 .into_iter()
                                 .map(|x| x.to_string())
                                 .collect::<Vec<_>>()
                                 .join(", ")*/
    );
}

fn test() {
    //env_logger::builder().filter_level(log::LevelFilter::Info).try_init();
    let ret = RusTeXEngine::do_file(
        "/home/jazzpirate/work/Software/sTeX/RusTeXNew/test/test.tex",
        Settings {
            verbose: false,
            log: true,
            sourcerefs: true,
            image_options: Default::default(),
        },
    );
    ret.write_out(Path::new(
        "/home/jazzpirate/work/Software/sTeX/RusTeXNew/test/test.html",
    ))
    .unwrap();
}

fn temp_test() {
    //env_logger::builder().filter_level(log::LevelFilter::Info).try_init();
    //let ret = RusTeXEngine::do_file("/home/jazzpirate/work/Software/sTeX/RusTeXNew/test/numtest.tex",false,true,true);
    let ret = RusTeXEngine::do_file(
        //"/home/jazzpirate/work/MathHub/courses/UMR/GdMA/course/source/course/sec/Vorwort.de.tex",
        "/home/jazzpirate/work/MathHub/sTeX/DemoExamples/source/proof.tex",
        //"/home/jazzpirate/work/Software/sTeX/RusTeXNew/test/tmptest.tex",
        Settings {
            verbose: true,
            log: true,
            sourcerefs: true,
            image_options: Default::default(),
        },
    );
    //let ret = RusTeXEngine::do_file("/home/jazzpirate/work/LaTeX/Papers/17 - Alignment Translation/macros/kwarc/workplan/workplan-template.tex",true,true,true);
    //std::fs::write("/home/jazzpirate/work/Software/sTeX/RusTeXNew/test/numtest.html", &ret.out).unwrap();
    ret.write_out(Path::new(
        //"/home/jazzpirate/rustex.out.html"
        "/home/jazzpirate/work/MathHub/sTeX/DemoExamples/source/proof.html"
        //"/home/jazzpirate/work/Software/sTeX/RusTeXNew/test/tmptest.html", //"/home/jazzpirate/work/MathHub/courses/UMR/GdMA/course/source/course/sec/Vorwort.de.tex.html"
    ))
    .unwrap();
}

fn thesis() {
    //env_logger::builder().filter_level(log::LevelFilter::Info).try_init();
    let ret = RusTeXEngine::do_file(
        "/home/jazzpirate/work/LaTeX/Papers/19 - Thesis/thesis.tex",
        Settings {
            verbose: false,
            log: true,
            sourcerefs: true,
            image_options: Default::default(),
        },
    );
    ret.write_out(Path::new(
        "/home/jazzpirate/work/Software/sTeX/RusTeXNew/test/thesis.html",
    ))
    .unwrap();
}

fn notes() {
    //env_logger::builder().filter_level(log::LevelFilter::Info).try_init();
    // /home/jazzpirate/work/MathHub/MiKoMH/AI/source/search/slides/ts-ex.en.tex
    let ret = RusTeXEngine::do_file(
        "/home/jazzpirate/work/MathHub/MiKoMH/AI/source/course/notes/notes.tex",
        Settings {
            verbose: false,
            log: true,
            sourcerefs: true,
            image_options: Default::default(),
        },
    );
    //let ret = RusTeXEngine::do_file("/home/jazzpirate/work/MathHub/MiKoMH/CompLog/source/kr/tikz/axioms2.tex",true,true,true);
    ret.write_out(Path::new(
        "/home/jazzpirate/work/Software/sTeX/RusTeXNew/test/ainotes.html",
    ))
    .unwrap();
}

fn profile() {
    use tex_engine::engine::filesystem::FileSystem;
    println!("Profiling...");
    //env_logger::builder().filter_level(log::LevelFilter::Info).try_init();
    let mut engine = DefaultEngine::<Types>::default();
    RusTeX::engine::commands::register_primitives_preinit(&mut engine);
    engine.aux.outputs = RusTeXOutput::Print(false);
    engine.initialize_etex_primitives();
    register_pdftex_primitives(&mut engine);
    engine.init_file("pdftexconfig.tex").unwrap();
    let state = engine.state.clone();
    for _ in 0..30 {
        let _ = engine.load_latex();
        engine.state = state.clone();
        engine.filesystem = RusTeXFileSystem::new(tex_engine::utils::PWD.to_path_buf());
    }
}

#[derive(Parser, Debug)]
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

    /// kpse
    #[command(subcommand)]
    kpse: Option<KpseArgs>,
}

#[derive(clap::Subcommand,Debug)]
enum KpseArgs {
    Kpse {
        #[clap(long, default_value_t = false)]
        log:bool,
        #[arg(required = true)]
        path:String
    }
}

fn run() {
    //env_logger::builder().filter_level(log::LevelFilter::Info).try_init();
    let params = Parameters::parse();
    if params.profile {
        profile();
        return;
    }
    if let Some(k) = params.kpse {
        kpse(k);
        return;
    }
    match (params.input, params.output) {
        (Some(i), Some(o)) => {
            let ret = RusTeXEngine::do_file(
                i.as_str(),
                Settings {
                    verbose: params.verbose,
                    log: params.console,
                    sourcerefs: params.sourcerefs,
                    image_options: Default::default(),
                },
            );
            ret.write_out(Path::new(&o)).unwrap();
        }
        _ => {
            println!("No input/output file given. Testing latex.ltx...");
            test_latex_ltx();
            println!("Done");
        }
    }
}

fn test_latex_ltx() {
    let mut engine = DefaultEngine::<Types>::default();
    RusTeX::engine::commands::register_primitives_preinit(&mut engine);

    //engine.state.set_primitive_int(&mut engine.aux,PRIMITIVES.tracingcommands,1,true);
    //engine.state.set_primitive_int(&mut engine.aux,PRIMITIVES.tracingifs,1,true);

    engine.aux.outputs = RusTeXOutput::Print(true);
    engine.initialize_etex_primitives();
    register_pdftex_primitives(&mut engine);
    engine.init_file("pdftexconfig.tex").unwrap();
    let _ = engine.load_latex();
}


fn kpse(KpseArgs::Kpse {log,path}:KpseArgs) {
    if log {
        tex_engine::engine::filesystem::kpathsea::LOG_KPATHSEA.store(true, std::sync::atomic::Ordering::Relaxed);
    }
    let kpse = tex_engine::engine::filesystem::kpathsea::Kpathsea::new(
        std::env::current_dir().expect("Could not find current dir")
    );
    if log && path == "ALL" {
        for (k,v) in &kpse.global.pre {
            println!("{k}:   {}",v.display());
        }
        for (k,v) in &kpse.local {
            println!("{k}:   {}",v.display());
        }
        for (k,v) in &kpse.global.post {
            println!("{k}:   {}",v.display());
        }
    } else {
        let r = kpse.kpsewhich(path);
        if r.exists {
            println!("{}",r.path.display());
        }
    }
}