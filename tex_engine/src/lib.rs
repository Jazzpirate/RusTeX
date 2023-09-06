#![forbid(unsafe_code)]

/*!
 ***Work In Progress***

 An implementation of a generic TeX engine, the core of the TeX typesetting system.
 This crate largely follows an object-oriented design for modularity and adaptability,
 with functionality largely implemented in generic traits. This lets the compiler
 optimize the code for the specific types used, while still allowing for easy customization.

 A specific [`Engine`](engine::Engine) combines:
 - a [`Mouth`](engine::mouth::Mouth) that provides [`Token`](tex::token::Token)s to process
 - a [`Gullet`](engine::gullet::Gullet) that expands macros and returns primitives
   ([`StomachCommand`](tex::commands::StomachCommand)s),
 - a [`State`](engine::state::State) that keeps track of the current state of the engine,
   e.g. the current [`CategoryCodeScheme`](tex::catcodes::CategoryCodeScheme), and allows for
   pushing/popping the stack,
 - a [`Stomach`](engine::stomach::Stomach) that processes the [`StomachCommand`](tex::commands::StomachCommand)s
   and returns [`TeXBox`](tex::nodes::CustomNode)es.

   Almost all algorithms, too, are generic over implementations of these traits.
   The default [`Engine`](engine::Engine) for plain TeX/LaTeX can be found
   in [`PlainTeXEngine`](engine::TeXEngine).

*/
pub mod utils;
pub mod tex;
pub mod engine;

#[macro_export]
macro_rules! debug_log {
    (trace=>$($arg:tt)*) => {
      #[cfg(debug_assertions)]
      {log::trace!($($arg)*);}
    };
    (debug=>$($arg:tt)*) => {
      #[cfg(debug_assertions)]
      {log::debug!($($arg)*);}
    };
    (info=>$($arg:tt)*) => {
      #[cfg(debug_assertions)]
      {log::info!($($arg)*);}
    };
    (warn=>$($arg:tt)*) => {
      #[cfg(debug_assertions)]
      {log::warn!($($arg)*);}
    };
    (error=>$($arg:tt)*) => {
       #[cfg(debug_assertions)]
       {log::error!($($arg)*);}
    };
}

#[cfg(test)]
mod tests {
    use std::collections::LinkedList;
    use std::mem::size_of;
    use crate::engine::filesystem::{FileSystem, KpseVirtualFileSystem, VirtualFile};
    use log::{error, warn, info, debug, trace};
    use crate::engine::gullet::{Gullet,TeXGullet};
    use crate::engine::mouth::Mouth;
    //use crate::engine::{Engine, new_tex_with_source_references, Outputs};
    use crate::engine::state::{State, FieldBasedState};
    use crate::engine::stomach::{NoShipoutDefaultStomach, ShipoutDefaultStomach, Stomach};
    use crate::engine::mouth::Mouth;

    use ansi_term::Colour::Green;
    use crate::engine::filesystem::kpathsea::Kpathsea;
    use crate::utils::errors::TeXError;
    use ansi_term::Colour::*;
    use rand::Rng;
    use crate::engine::{Engine, EngineRef, EngineType, Outputs};
    use crate::engine::memory::{Interner, Memory};
    use crate::tex::catcodes::CategoryCode;
    use crate::tex::commands::{BaseCommand, BaseStomachCommand, Command, CommandSource};
    use crate::tex::commands::pdftex::PDFTeXNode;
    use crate::tex::commands::tex::PAR;
    use crate::tex::fonts::{TfmFont, TfmFontStore};
    use crate::tex::numbers::{Dimi32, Fill, MuFill, Mui32};
    use crate::tex::token::{BaseToken, FileTokenReference, Token};
    use crate::utils::Ptr;
    use crate::utils::strings::{CharType, TeXStr};


    #[derive(Clone,Copy,Debug)]
    struct Default();
    impl EngineType for Default {
        type Char = u8;
        type File = VirtualFile<u8>;
        type FileSystem = KpseVirtualFileSystem<u8>;
        type Font = TfmFont;
        type FontStore = TfmFontStore;
        type FontRef = usize;
        type Node = PDFTeXNode<Self>;
        type Int = i32;
        type Dim = Dimi32;
        type SkipDim = Fill;
        type MuDim = Mui32;
        type MuStretchShrinkDim = MuFill;
        type CommandReference = ();
        type TokenReference = ();//FileTokenReference<Self>;
        type State = FieldBasedState<Self>;
        type Gullet = TeXGullet<Self>;
        type Stomach = ShipoutDefaultStomach<Self>;
    }

    macro_rules! measure {
        ($key:ident:$x:expr) => {{
            let measure_start = std::time::Instant::now();
            let ret = $x;
            warn!(target:stringify!($key),"{}", Green.bold().paint(format!("Finished after {:?}",measure_start.elapsed())));
            ret
        }};
    }
    /*lazy_static! {
        static ref INIT: std::sync::Once = std::sync::Once::new();
    }*/

    fn trace() {
        env_logger::builder().filter_level(log::LevelFilter::Trace).try_init();
    }

    fn debug() {
        env_logger::builder().filter_level(log::LevelFilter::Debug).try_init();
    }

    fn info() {
        env_logger::builder().filter_level(log::LevelFilter::Info).try_init();
    }

    fn warn() {
        env_logger::builder().filter_level(log::LevelFilter::Warn).try_init();
    }

    fn error() {
        env_logger::builder().filter_level(log::LevelFilter::Error).try_init();
    }

    /*
    std::env::set_var("RUST_LOG","info,tex_engine::engine::mouth=trace");
    env_logger::init();
    */
    #[test]
    fn sizes () {
        warn();
        use std::mem::size_of;
        type TR = Option<<Default as crate::engine::EngineType>::TokenReference>;
        warn!("catcode: {}, TeXStr: {}, Pair: {}, Reference: {}, Unit: {}",size_of::<CategoryCode>(),size_of::<TeXStr>(),size_of::<(u8,CategoryCode)>(),size_of::<TR>(),size_of::<()>());
        enum BaseTokenA {
            CS(u16),
            Char(u8,CategoryCode),
        }
        warn!("Base A: {} == {}",size_of::<BaseToken<u8>>(),size_of::<BaseTokenA>());
        warn!("Pair: {} == {}",size_of::<(BaseToken<u8>,TR)>(),size_of::<(BaseTokenA,TR)>());
        struct TokenA {
            token:BaseTokenA,
            reference:TR
        }
        warn!("A: {} == {}",size_of::<Token<Default>>(),size_of::<TokenA>());
        enum TokenB {
            CS(TeXStr,TR),
            Char(u8,CategoryCode,TR),
        }
        warn!("B: {} == {}",size_of::<Token<Default>>(),size_of::<TokenB>());
        enum TokenC {
            CS{name:TeXStr,reference:TR},
            Char{char:u8,catcode:CategoryCode,reference:TR},
        }
        warn!("C: {} == {}",size_of::<Token<Default>>(),size_of::<TokenC>());
        enum TokenD {
            CS{name:TeXStr,reference:TR},
            BGroup{char:u8,reference:TR},
            EGroup{char:u8,reference:TR},
            MathShift{char:u8,reference:TR},
            AlignmentTab{char:u8,reference:TR},
            EOL{char:u8,reference:TR},
            Parameter{char:u8,reference:TR},
            Superscript{char:u8,reference:TR},
            Subscript{char:u8,reference:TR},
            Space{reference:TR},
            Letter{char:u8,reference:TR},
            Other{char:u8,reference:TR},
            Active{char:u8,reference:TR},
        }
        warn!("D: {} == {}",size_of::<Token<Default>>(),size_of::<TokenD>());
        enum TokenE {
            CS{name:TeXStr,reference:TR,foo:()},
            Char{char:u8,catcode:CategoryCode,reference:TR,bar:()},
        }
        warn!("E: {} == {}",size_of::<Token<Default>>(),size_of::<TokenE>());
        struct Foo<A> {
            a: A,
        }
        warn!("Test: {}, {}",size_of::<Foo<u8>>(),size_of::<Foo<()>>());

        warn!("-------------------------------------------------------------------------");

        warn!("Command: {}; BaseCommand: {}",size_of::<Command<Default>>(),size_of::<BaseCommand<Default>>());

        struct FnTest<ET:EngineType> {
            foo:fn(EngineRef<ET>,CommandSource<ET>,&mut Vec<ET::Token>)
        }

        struct FnTest2<ET:EngineType> {
            foo:&'static fn(EngineRef<ET>,CommandSource<ET>,&mut Vec<ET::Token>)
        }

        warn!("Test: {}, {}, {}, {}", size_of::<&'static str>(), size_of::<FnTest<Default>>(), size_of::<FnTest<Default>>(), size_of::<FnTest2<Default>>());

        #[derive(Clone)]
        struct TestAA(u64);
        #[derive(Clone)]
        struct TestA(u32,u16);
        #[derive(Clone)]
        struct TestB(u32,u16,u16,u16);
        #[derive(Clone)]
        struct TestC(u64,u32);
        #[derive(Clone)]
        struct TestD(u8,u8,u8,u8,u8,u8,u8,u8,u8,u8,u8,u8,u8,u8,u8,u8);

        warn!("-------------------------------------------------------------------------");

        warn!("Test: {}, {}, {}, {}, {}", size_of::<TestAA>(), size_of::<TestA>(), size_of::<TestB>(),size_of::<TestC>(),size_of::<TestD>());

        measure!(aa:{
            let mut vec = Vec::new();
            let a = TestAA(0);
            for _ in (0..1000) {
                for _ in (0..100000000) {
                    vec.push(a.clone());
                }
                while let Some(x) = vec.pop() {
                    let _ = x;
                }
            }
        });

        measure!(a:{
            let mut vec = Vec::new();
            let a = TestA(0,0);
            for _ in (0..1000) {
                for _ in (0..100000000) {
                    vec.push(a.clone());
                }
                while let Some(x) = vec.pop() {
                    let _ = x;
                }
            }
        });

        measure!(b:{
            let mut vec = Vec::new();
            let b = TestB(0,0,0,0);
            for _ in (0..1000) {
                for _ in (0..100000000) {
                    vec.push(b.clone());
                }
                while let Some(x) = vec.pop() {
                    let _ = x;
                }
            }
        });

        measure!(c:{
            let mut vec = Vec::new();
            let c = TestC(0,0);
            for _ in (0..1000) {
                for _ in (0..100000000) {
                    vec.push(c.clone());
                }
                while let Some(x) = vec.pop() {
                    let _ = x;
                }
            }
        });

        measure!(c:{
            let mut vec = Vec::new();
            let c = TestD(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);
            for _ in (0..1000) {
                for _ in (0..100000000) {
                    vec.push(c.clone());
                }
                while let Some(x) = vec.pop() {
                    let _ = x;
                }
            }
        });
    }

    #[test]
    fn kpsewhich() { measure!(kpsewhich: {
        debug();
        let fs = measure!(kpsewhich_initialization: {
            KpseVirtualFileSystem::<u8>::new(std::env::current_dir().unwrap())
        });
        info!(target:"kpsewhich", "latex.ltx: {:?}",fs.kpsewhich("latex.ltx").path);
        info!(target:"kpsewhich", "expl3-code: {:?}",fs.kpsewhich("expl3-code").path);
        let kpse = Kpathsea::new(std::env::current_dir().unwrap());
        let vv = kpse.kpsewhich("|kpsewhich -var-value MATHHUB");
        info!(target:"kpsewhich", "-var-value MATHHUB: {:?}",vv.path);
        info!(target:"kpsewhich", "returns: {:?}",String::from_utf8(kpse.get(&std::env::current_dir().unwrap(),&vv.path).unwrap()));
    });}

    #[test]
    fn engine() { /*crate::utils::with_stack_size(16 * 1024 * 1024,||*/ measure!(engine: {
        //error();
        //trace();
        //std::env::set_var("RUST_LOG","debug,tex_engine::tex::commands=trace,tex_engine::engine::gullet=trace");
        //env_logger::init();
        warn();

        let outputs = Outputs {
            error: |s|  { warn!("\n{}",Red.paint(std::format!("{}",s))) },
            message: |s| { warn!("{}",Yellow.paint(s)) },
            file_open:|pb| { warn!("\n{}",Green.paint(format!("({}",pb))) },
            file_close:|pb| { warn!("{}",Green.paint(format!(")"))) },
            write_18:|_| { },
            write_17:|s| { warn!("{}",s) },
            write_16:|s| { warn!("{}",White.bold().paint(s)) },
            write_neg1:|s| { warn!("\n{}",Black.on(Blue).paint(s)) },
            write_other:|s| { warn!("\n{}",Black.on(Green).paint(s)) },
        };

        let mut interner = Interner::new();
        let fs = KpseVirtualFileSystem::new(std::env::current_dir().unwrap());
        let fonts = TfmFontStore::new(&mut interner);
        let state = FieldBasedState::new(&fonts);
        let gullet = TeXGullet::new();
        let stomach = ShipoutDefaultStomach::new();
        let mut engine = crate::engine::EngineRef::<Default>::new(fs,fonts,state,gullet,stomach,outputs);
        engine.interner = interner;

        engine.state.set_command(TeXStr::from_static("rustexBREAK",&mut engine.interner),Some(Command::new(BaseCommand::Unexpandable {
            name: "rustexBREAK",
            apply: |_,_| {
                println!("HERE!");
                std::env::set_var("RUST_LOG","debug,tex_engine::tex::commands=trace,tex_engine::engine::gullet=trace");
                env_logger::init();
                //trace();
            },forces_mode:None
        },None)),true);

        match engine.initialize_pdflatex() {
            Ok(_) => {
                engine.components().memory.print_stats();
                error!("Mouth: {}",engine.mouth.stack.capacity());
            },
            Err(e) => {
                (engine.outputs.error)(&format!("{}\n\nat:{}\n   {}...",e.throw_string(&mut engine.interner),engine.components().current_position(),engine.components().preview(100)));
                panic!()
            }
        }
    })/*);*/}

    fn random_token(interner:&mut Interner) -> Token<Default> {
        if rand::random() {
            let mut s = String::new();
            for _ in (0..rand::thread_rng().gen_range(1..=30)) {
                s.push(rand::random())
            }
            Token::new(BaseToken::CS(TeXStr::from_string(&s,interner)),None)
        } else {
            Token::new(BaseToken::Char(rand::random(),CategoryCode::try_from(rand::thread_rng().gen_range(1..=13)).unwrap()),None)
        }
    }
/*
    fn run_one(iterations:usize,length:usize) -> (std::time::Duration,std::time::Duration) {
        let mut ls = LinkedList::new();
        let mut vec = Vec::new();
        let mut ls_timer = std::time::Duration::new(0,0);
        let mut vec_timer = std::time::Duration::new(0,0);
        let mut dummy = 0;
        for _ in (0..iterations) {
            let mut interner: Interner<u8> = Interner::new();
            let interner = &mut interner;
            ls.clear();
            vec.clear();
            let len = rand::thread_rng().gen_range(1..=length);

            let start = std::time::Instant::now();
            for _ in (0..len) {
                ls.push_back(random_token(interner));
            }
            while let Some(next) = ls.pop_front() {
                dummy += <CategoryCode as Into<u8>>::into(next.catcode()) as usize;
            }
            ls_timer += start.elapsed();


            let start = std::time::Instant::now();
            for _ in (0..len) {
                vec.push(random_token(interner));
            }
            vec.reverse();
            while let Some(next) = vec.pop() {
                dummy += <CategoryCode as Into<u8>>::into(next.catcode()) as usize;
            }
            vec_timer += start.elapsed();
            println!("dummy: {}",dummy);
            dummy = 0;
        }
        (ls_timer,vec_timer)
    }

    #[test]
    fn profile() {
        info();
        let (a,b) = run_one(30*500000,20);
        info!(target:"profile","30*500000;20    TokenList: {}, Vec: {}",a.as_secs_f64(),b.as_secs_f64());
        let (a,b) = run_one(1200000,200);
        info!(target:"profile","1200000;200   TokenList: {}, Vec: {}",a.as_secs_f64(),b.as_secs_f64());
        let (a,b) = run_one(300000,2000);
        info!(target:"profile","300000;2000  TokenList: {}, Vec: {}",a.as_secs_f64(),b.as_secs_f64());
        let (a,b) = run_one(30*500000,20);
        info!(target:"profile","30*500000;20    TokenList: {}, Vec: {}",a.as_secs_f64(),b.as_secs_f64());
        let (a,b) = run_one(1200000,200);
        info!(target:"profile","1200000;200   TokenList: {}, Vec: {}",a.as_secs_f64(),b.as_secs_f64());
        let (a,b) = run_one(300000,2000);
        info!(target:"profile","300000;2000  TokenList: {}, Vec: {}",a.as_secs_f64(),b.as_secs_f64());
    }

 */
}