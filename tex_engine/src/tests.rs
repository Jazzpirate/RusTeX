#[doc(hidden)]
#[cfg(any(test,doctest))]
#[allow(dead_code)]
pub mod test_utils {
    #[macro_export]
    macro_rules! measure {
        ($key:ident:$x:expr) => {{
            let measure_start = std::time::Instant::now();
            let ret = $x;
            log::warn!(target:stringify!($key),"Finished after {:?}",measure_start.elapsed());
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
    use crate::tests::test_utils::*;
    use crate::prelude::*;
    use crate::measure;
    use log::*;

    #[test]
    fn kpsewhich() { measure!(kpsewhich: {
        use crate::engine::filesystem::kpathsea::*;
        debug();
        let kpse = Kpathsea::new(std::env::current_dir().unwrap());
        info!(target:"kpsewhich", "latex.ltx: {:?}", kpse.kpsewhich("latex.ltx").path);
        info!(target:"kpsewhich", "article.cls: {:?}",kpse.kpsewhich("article.cls").path);
        info!(target:"kpsewhich", "expl3-code: {:?}",kpse.kpsewhich("expl3-code").path);
        info!(target:"kpsewhich", "-var-value MATHHUB: {:?}",kpse.kpsewhich("|kpsewhich -var-value MATHHUB").path);
    });}

    #[test]
    fn tokenizer() {
        debug();
        use crate::engine::mouth::strings::InputTokenizer;
        use crate::tex::tokens::StandardToken;
        use crate::tex::catcodes::DEFAULT_SCHEME_U8;
        use crate::utils::Ptr;
        use crate::tex::characters::StringLineSource;

        type T = StandardToken<u8,Ptr<str>>;
        let mut cs_handler = ();
        let cc = &DEFAULT_SCHEME_U8;

        let string = "\\foo   \n  \n   {a}{!}";
        let input: StringLineSource<u8> = string.into();
        let mut tokenizer = InputTokenizer::new(input);
        let eol = Some(b'\r');
        let next = tokenizer.get_next(&mut cs_handler,cc,None); // \foo
        assert!(matches!(next,Ok(Some(T::ControlSequence(s))) if &*s == "foo"));
        let next = tokenizer.get_next(&mut cs_handler,cc,eol); // \par
        assert!(matches!(next,Ok(Some(T::ControlSequence(s))) if &*s == "par"));
        let next : T = tokenizer.get_next(&mut cs_handler,cc,eol).unwrap().unwrap(); // {
        assert_eq!(next.command_code(), CommandCode::BeginGroup);
        let next : T = tokenizer.get_next(&mut cs_handler,cc,eol).unwrap().unwrap(); // a
        assert_eq!(next.command_code(), CommandCode::Letter);
        let next : T = tokenizer.get_next(&mut cs_handler,cc,eol).unwrap().unwrap(); // }
        assert_eq!(next.command_code(), CommandCode::EndGroup);
        let next : T = tokenizer.get_next(&mut cs_handler,cc,eol).unwrap().unwrap(); // {
        assert_eq!(next.command_code(), CommandCode::BeginGroup);
        let next : T = tokenizer.get_next(&mut cs_handler,cc,eol).unwrap().unwrap(); // !
        assert_eq!(next.command_code(), CommandCode::Other);
        let next : T = tokenizer.get_next(&mut cs_handler,cc,eol).unwrap().unwrap(); // }
        assert_eq!(next.command_code(), CommandCode::EndGroup);
        let next : T = tokenizer.get_next(&mut cs_handler,cc,eol).unwrap().unwrap(); // end of line => space
        assert_eq!(next.command_code(), CommandCode::Space);
        assert!(tokenizer.get_next::<T>(&mut cs_handler,cc,eol).unwrap().is_none()); // EOF
    }

    const CARLISLE: &str = r#"\let~\catcode~`76~`A13~`F1~`j00~`P2jdefA71F~`7113jdefPALLF
PA''FwPA;;FPAZZFLaLPA//71F71iPAHHFLPAzzFenPASSFthP;A$$FevP
A@@FfPARR717273F737271P;ADDFRgniPAWW71FPATTFvePA**FstRsamP
AGGFRruoPAqq71.72.F717271PAYY7172F727171PA??Fi*LmPA&&71jfi
Fjfi71PAVVFjbigskipRPWGAUU71727374 75,76Fjpar71727375Djifx
:76jelse&U76jfiPLAKK7172F71l7271PAXX71FVLnOSeL71SLRyadR@oL
RrhC?yLRurtKFeLPFovPgaTLtReRomL;PABB71 72,73:Fjif.73.jelse
B73:jfiXF71PU71 72,73:PWs;AMM71F71diPAJJFRdriPAQQFRsreLPAI
I71Fo71dPA!!FRgiePBt'el@ lTLqdrYmu.Q.,Ke;vz vzLqpip.Q.,tz;
;Lql.IrsZ.eap,qn.i. i.eLlMaesLdRcna,;!;h htLqm.MRasZ.ilk,%
s$;z zLqs'.ansZ.Ymi,/sx ;LYegseZRyal,@i;@ TLRlogdLrDsW,@;G
LcYlaDLbJsW,SWXJW ree @rzchLhzsW,;WERcesInW qt.'oL.Rtrul;e
doTsW,Wk;Rri@stW aHAHHFndZPpqar.tridgeLinZpe.LtYer.W,:jbye
    "#;

    #[test]
    fn carlisle() {
        info();
        let mut engine = PlainTeXEngine::new();
        engine.initialize_plain_tex().unwrap();
        engine.mouth.push_string(CARLISLE.into());
        match engine.run(|_,n| {
            info!("{}",n.display());
        }) {
            Ok(_) => (),
            Err(e) => {
                panic!("{}",e.msg)
            }
        }
    }

    #[cfg(feature="pdflatex")]
    #[test]
    fn pdflatex_init() {
        use crate::pdflatex::{PDFTeXEngine,PlainPDFTeXEngine};
        debug();
        let mut engine = PlainPDFTeXEngine::new();
        match engine.initialize_pdflatex() {
            Ok(_) => (),
            Err(e) => {
                panic!("{}",e.msg)
            }
        }
    }

    #[cfg(feature="pdflatex")]
    #[test]
    fn testfile() {
        use crate::utils::PWD;
        use crate::pdflatex::{PDFTeXEngine,PlainPDFTeXEngine};
        use path_dedot::*;
        let testpath = PWD.join("../test/test.tex").parse_dot().unwrap().to_path_buf();
        debug();
        let mut engine = PlainPDFTeXEngine::new();
        match engine.initialize_pdflatex() {
            Ok(_) => (),
            Err(e) => {
                panic!("{}",e.msg)
            }
        }
        engine.do_file_pdf(testpath.to_str().unwrap(),|_,_| {}).unwrap_or_else(|e| {
            //let pos = engine.mouth.display_position().to_string();
            //let cap = engine.aux.memory.cs_interner().cap();
            //error!("{}:\n{}\n\nCapacity: {} of {} ({:.2}%)",pos,engine.get_engine_refs().preview(),cap,0x8000_0000,(cap as f32 / (0x8000_0000u32 as f32)) * 100.0);
            panic!("{}",e.msg);
        });

    }

/*
    #[cfg(feature="pdflatex")]
    #[test]
    fn thesis() {
        debug();
        let mut engine = PlainPDFTeXEngine::new();
        engine.register_primitive(Command::Unexpandable(
            Unexpandable {
                name:PRIMITIVES.get("rustexBREAK"),
                scope:CommandScope::Any,
                apply:|_,_| {
                    println!("HERE!")
                }
            }
        ),"rustexBREAK");
        engine.initialize_pdflatex().unwrap();
        {
            //let refs = engine.get_engine_refs();
            //refs.state.set_primitive_int(&refs.aux,PRIMITIVES.tracingassigns,1,true);
            //refs.state.set_primitive_int(&refs.aux,PRIMITIVES.tracingifs,1,true);
            //refs.state.set_primitive_int(&refs.aux,PRIMITIVES.tracingcommands,1,true);
            //refs.state.set_primitive_int(&refs.aux,PRIMITIVES.tracinggroups,1,true);
            //refs.state.set_primitive_int(&refs.aux,PRIMITIVES.tracingrestores,1,true);
        }
        engine.do_file_pdf("/home/jazzpirate/work/LaTeX/Papers/19 - Thesis/thesis.tex",|_,b| {
            info!("{}",b.readable());
            println!("HERE");
        }).unwrap_or_else(|e| {
            let pos = engine.mouth.display_position().to_string();
            let cap = engine.aux.memory.cs_interner().cap();
            error!("{}:\n{}\n\nCapacity: {} of {} ({:.2}%)",pos,engine.get_engine_refs().preview(),cap,0x8000_0000,(cap as f32 / (0x8000_0000u32 as f32)) * 100.0);
            panic!("{}",e.msg);
        });
    }

 */
/*
    #[test]
    fn memory_things() {
        debug();
        type Tk = crate::tex::token::CompactToken;
        type CS = <Tk as Token>::CS;
        type State = crate::engine::state::tex_state::TeXState<DefaultPlainTeXEngineTypes>;
        type Change = crate::engine::state::StateChange<DefaultPlainTeXEngineTypes,State>;
        type Bx = crate::tex::nodes::boxes::TeXBox<DefaultPlainTeXEngineTypes>;
        info!("primitive id: {}b",std::mem::size_of::<crate::engine::utils::memory::PrimitiveIdentifier>());
        info!("control sequence name: {}b",std::mem::size_of::<CS>());
        info!("compact token: {}b",std::mem::size_of::<Tk>());
        info!("standard token: {}b",std::mem::size_of::<crate::tex::token::StandardToken<u8,CS>>());
        info!("command: {}b",std::mem::size_of::<crate::commands::Command<DefaultPlainTeXEngineTypes>>());
        info!("macro: {}b",std::mem::size_of::<crate::commands::Macro<Tk>>());
        info!("fn: {}b",std::mem::size_of::<fn(&mut EngineReferences<DefaultPlainTeXEngineTypes>,&mut ExpansionContainer<Tk>,Tk)>());
        info!("box: {}b",std::mem::size_of::<Bx>());
        info!("state change: {}b",std::mem::size_of::<Change>());
        info!("custom state change: {}b",std::mem::size_of::<Ptr<dyn CustomStateChange<DefaultPlainTeXEngineTypes,State>>>());
        info!("primitive id + int: {}b",std::mem::size_of::<(crate::engine::utils::memory::PrimitiveIdentifier,i32)>());
        info!("--------------------------------------------------------------");
        info!("static string: {}b",std::mem::size_of::<&'static str>());
        info!("ptr: {}b",std::mem::size_of::<Ptr<&'static str>>());
        info!("char: {}b",std::mem::size_of::<char>());

        /*
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

         */
    }
 */
}