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

#[doc(hidden)]
#[cfg(any(test,doctest))]
#[allow(dead_code)]
pub mod test_utils {
    #[macro_export]
    macro_rules! measure {
        ($key:ident:$x:expr) => {{
            let measure_start = std::time::Instant::now();
            let ret = $x;
            log::warn!(target:stringify!($key),"{}", Green.bold().paint(format!("Finished after {:?}",measure_start.elapsed())));
            ret
        }};
    }

    #[allow(unused_must_use)]
    pub fn trace() {
        env_logger::builder().filter_level(log::LevelFilter::Trace).try_init();
    }
    #[allow(unused_must_use)]
    pub fn debug() {
        env_logger::builder().filter_level(log::LevelFilter::Debug).try_init();
    }
    #[allow(unused_must_use)]
    pub fn info() {
        env_logger::builder().filter_level(log::LevelFilter::Info).try_init();
    }
    #[allow(unused_must_use)]
    pub fn warn() {
        env_logger::builder().filter_level(log::LevelFilter::Warn).try_init();
    }
    #[allow(unused_must_use)]
    pub fn error() {
        env_logger::builder().filter_level(log::LevelFilter::Error).try_init();
    }
}


#[cfg(test)]
mod tests {
    use std::fmt::Formatter;
    use ansi_term::Colour::*;
    use crate::tests::test_utils::*;
    use crate::measure;
    use log::*;
    use crate::engine::{DefaultPlainTeXEngineTypes, EngineReferences, PlainPDFTeXEngine, PlainTeXEngine, TeXEngine};
    use crate::engine::gullet::DefaultGullet;
    use crate::engine::mouth::DefaultMouth;
    use crate::engine::mouth::pretokenized::ExpansionContainer;
    use crate::engine::state::{CustomStateChange, State};
    use crate::engine::utils::memory::PRIMITIVES;
    use crate::tex::catcodes::CommandCode;
    use crate::tex::input_text::{Character, StringLineSource};
    use crate::tex::nodes::PreShipoutNodeTrait;
    use crate::tex::token::Token;
    use crate::utils::Ptr;
    use crate::engine::PDFTeXEngine;
    use crate::engine::state::tex_state::TeXState;
    use crate::engine::stomach::StomachWithShipout;
    use crate::tex::nodes::NodeTrait;

    #[test]
    fn kpsewhich() { measure!(kpsewhich: {
        use crate::engine::filesystem::kpathsea::*;
        debug();
        let kpse = Kpathsea::new(std::env::current_dir().unwrap());
        info!(target:"kpsewhich", "latex.ltx: {:?}", kpse.kpsewhich("latex.ltx").path);
        info!(target:"kpsewhich", "article.cls: {:?}",kpse.kpsewhich("article.cls").path);
        info!(target:"kpsewhich", "expl3-code: {:?}",kpse.kpsewhich("expl3-code").path);
        /*for _ in 0..10000 {
            kpse.kpsewhich("latex.ltx").path;
            kpse.kpsewhich("expl3-code").path;
        }*/
        info!(target:"kpsewhich", "-var-value MATHHUB: {:?}",kpse.kpsewhich("|kpsewhich -var-value MATHHUB").path);
        /*
        let fs = measure!(kpsewhich_initialization: {
            KpseVirtualFileSystem::<u8>::new(std::env::current_dir().unwrap())
        });
        info!(target:"kpsewhich", "latex.ltx: {:?}",fs.kpsewhich("latex.ltx").path);
        info!(target:"kpsewhich", "expl3-code: {:?}",fs.kpsewhich("expl3-code").path);
        let kpse = Kpathsea::new(std::env::current_dir().unwrap());
        let vv = kpse.kpsewhich("|kpsewhich -var-value MATHHUB");
        info!(target:"kpsewhich", "-var-value MATHHUB: {:?}",vv.path);
        info!(target:"kpsewhich", "returns: {:?}",String::from_utf8(kpse.get(&std::env::current_dir().unwrap(),&vv.path).unwrap()));

         */
    });}

    #[test]
    fn tokenizer() {
        debug();
        use crate::utils::errors::ErrorThrower;
        use crate::engine::mouth::strings::StringTokenizer;
        use crate::tex::token::StandardToken;
        use crate::tex::catcodes::DEFAULT_SCHEME_U8;
        use crate::utils::Ptr;
        use crate::tex::token::Token;

        type T = StandardToken<u8,Ptr<str>>;
        let eh = ErrorThrower;
        let mut cs_handler = ();
        let cc = &*DEFAULT_SCHEME_U8;

        let string = "\\foo   \n  \n   {a}{!}";
// should yield: [ControlSequence("foo"),BeginGroup,Letter("a"),EndGroup,BeginGroup,Other("!"),EndGroup]
        let input: StringLineSource<u8> = string.into();
        let mut tokenizer = StringTokenizer::new(input);
        let eol = Some(b'\r');
        let next = tokenizer.get_next(&eh,&mut cs_handler,cc,None); // \foo
        assert!(matches!(next,Some(T::ControlSequence(s)) if &*s == "foo"));
        let next = tokenizer.get_next(&eh,&mut cs_handler,cc,eol); // \par
        assert!(matches!(next,Some(T::ControlSequence(s)) if &*s == "par"));
        let next : T = tokenizer.get_next(&eh,&mut cs_handler,cc,eol).unwrap(); // {
        assert_eq!(next.command_code(), CommandCode::BeginGroup);
        let next : T = tokenizer.get_next(&eh,&mut cs_handler,cc,eol).unwrap(); // a
        assert_eq!(next.command_code(), CommandCode::Letter);
        let next : T = tokenizer.get_next(&eh,&mut cs_handler,cc,eol).unwrap(); // }
        assert_eq!(next.command_code(), CommandCode::EndGroup);
        let next : T = tokenizer.get_next(&eh,&mut cs_handler,cc,eol).unwrap(); // {
        assert_eq!(next.command_code(), CommandCode::BeginGroup);
        let next : T = tokenizer.get_next(&eh,&mut cs_handler,cc,eol).unwrap(); // !
        assert_eq!(next.command_code(), CommandCode::Other);
        let next : T = tokenizer.get_next(&eh,&mut cs_handler,cc,eol).unwrap(); // }
        assert_eq!(next.command_code(), CommandCode::EndGroup);
        let next : T = tokenizer.get_next(&eh,&mut cs_handler,cc,eol).unwrap(); // end of line => space
        assert_eq!(next.command_code(), CommandCode::Space);
        assert!(tokenizer.get_next::<T,_>(&eh,&mut cs_handler,cc,eol).is_none()); // EOF
    }

    #[test]
    fn initex() {
        debug();
        let mut engine = PlainPDFTeXEngine::new();
        {
            let refs = engine.get_engine_refs();
           // refs.state.set_primitive_int(&refs.aux,PRIMITIVES.tracingassigns,1,true);
           // refs.state.set_primitive_int(&refs.aux,PRIMITIVES.tracingifs,1,true);
           // refs.state.set_primitive_int(&refs.aux,PRIMITIVES.tracingcommands,1,true);
           // refs.state.set_primitive_int(&refs.aux,PRIMITIVES.tracinggroups,1,true);
           // refs.state.set_primitive_int(&refs.aux,PRIMITIVES.tracingrestores,1,true);
        }
        match engine.initialize_pdflatex() {
            Ok(_) => (),
            Err(e) => {
                panic!("{}",e.msg)
            }
        }
    }

    #[test]
    fn thesis() {
        debug();
        let mut engine = PlainPDFTeXEngine::new();
        engine.initialize_pdflatex().unwrap();
        {
            let refs = engine.get_engine_refs();
            //refs.state.set_primitive_int(&refs.aux,PRIMITIVES.tracingassigns,1,true);
            //refs.state.set_primitive_int(&refs.aux,PRIMITIVES.tracingifs,1,true);
            //refs.state.set_primitive_int(&refs.aux,PRIMITIVES.tracingcommands,1,true);
            //refs.state.set_primitive_int(&refs.aux,PRIMITIVES.tracinggroups,1,true);
            //refs.state.set_primitive_int(&refs.aux,PRIMITIVES.tracingrestores,1,true);
        }
        engine.do_file_pdf("/home/jazzpirate/work/LaTeX/Papers/19 - Thesis/thesis.tex",|b| {
            info!("{}",b.readable());
            todo!()
        }).unwrap();
    }

    #[test]
    fn memory_things() {
        debug();
        type Tk = crate::tex::token::CompactToken;
        type CS = <Tk as Token>::CS;
        type Change = crate::engine::state::StateChange<crate::engine::state::tex_state::TeXState<DefaultPlainTeXEngineTypes>>;
        info!("primitive id: {}b",std::mem::size_of::<crate::engine::utils::memory::PrimitiveIdentifier>());
        info!("control sequence name: {}b",std::mem::size_of::<CS>());
        info!("compact token: {}b",std::mem::size_of::<Tk>());
        info!("standard token: {}b",std::mem::size_of::<crate::tex::token::StandardToken<u8,CS>>());
        info!("command: {}b",std::mem::size_of::<crate::commands::Command<DefaultPlainTeXEngineTypes>>());
        info!("macro: {}b",std::mem::size_of::<crate::commands::Macro<Tk>>());
        info!("fn: {}b",std::mem::size_of::<fn(&mut EngineReferences<DefaultPlainTeXEngineTypes>,&mut ExpansionContainer<Tk>,Tk)>());
        info!("state change: {}b",std::mem::size_of::<Change>());
        info!("custom state change: {}b",std::mem::size_of::<Ptr<dyn CustomStateChange<crate::engine::state::tex_state::TeXState<DefaultPlainTeXEngineTypes>>>>());
        info!("primitive id + int: {}b",std::mem::size_of::<(crate::engine::utils::memory::PrimitiveIdentifier,i32)>());
        info!("--------------------------------------------------------------");
        info!("static string: {}b",std::mem::size_of::<&'static str>());
        info!("ptr: {}b",std::mem::size_of::<Ptr<&'static str>>());
        info!("char: {}b",std::mem::size_of::<char>());

        struct DP(u8);
        impl std::fmt::Display for DP {
            fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
                use crate::tex::input_text::Character;
                self.0.display(f);
                Ok(())
            }
        }

        for a in 0u8..255 {
            info!("{}: {}",a,DP(a))
        }
    }
}