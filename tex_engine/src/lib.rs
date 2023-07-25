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
   and returns [`TeXBox`](tex::boxes::TeXBox)es.

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
    use crate::engine::filesystem::{FileSystem, KpseVirtualFileSystem};
    use log::{error, warn, info, debug, trace};
    use crate::engine::gullet::TeXGullet;
    use crate::engine::mouth::TracingMouth;
    use crate::engine::{Engine, new_tex_with_source_references, Outputs};
    use crate::engine::state::TeXState;
    use crate::engine::stomach::NoShipoutDefaultStomach;
    use crate::tex::boxes::StandardTeXBox;
    use crate::tex::token::TokenWithSourceref;
    use crate::engine::mouth::Mouth;

    use ansi_term::Colour::Green;
    use crate::engine::filesystem::kpathsea::Kpathsea;
    use crate::utils::errors::TeXError;
    use ansi_term::Colour::*;

    macro_rules! measure {
        ($key:ident:$x:expr) => {{
            let measure_start = std::time::Instant::now();
            let ret = $x;
            info!(target:stringify!($key),"{}", Green.bold().paint(format!("Finished after {:?}",measure_start.elapsed())));
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
    fn engine() { measure!(engine: {
        //debug();
        trace();
        //std::env::set_var("RUST_LOG","debug,tex_engine::tex::commands=trace");
        //env_logger::init();

        let outputs = Outputs {
            error: |s|  { println!("{}",Red.paint(std::format!("{}",s))) },
            message: |s| { print!("{}",Yellow.paint(s)) },
            file_open:|pb| { println!("{}",Green.paint(format!("Opening file {:?}",pb))) },
            file_close:|pb| { println!("{}",Green.paint(format!("Closing file {:?}",pb))) },
            write_18:|_| { },
            write_17:|s| { print!("{}",s) },
            write_16:|s| { print!("{}",White.bold().paint(s)) },
            write_neg1:|s| { print!("{}",Black.on(Blue).paint(s)) },
            write_other:|s| { print!("{}",Black.on(Green).paint(s)) },
        };

        let mut engine = new_tex_with_source_references(KpseVirtualFileSystem::<u8>::new(std::env::current_dir().unwrap()),outputs.clone());
        match engine.initialize_pdflatex() {
            Ok(_) => {},
            Err(e) => {
                (outputs.error)(&format!("{}\n\nat:{}",e.throw_string(),engine.gullet.mouth.preview(100)));
                panic!()
            }
        }
    });}
}