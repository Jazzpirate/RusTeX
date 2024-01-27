#![allow(text_direction_codepoint_in_literal)]

pub mod fontstyles;
pub mod encodings;
mod parsing;
pub mod glyphs;

use crate::glyphs::{Glyph,GlyphList,GlyphI,UNDEFINED};

include!(concat!(env!("OUT_DIR"), "/codegen.rs"));


#[cfg(test)]
mod tests {
    use crate::encodings::EncodingStore;
    use super::*;
    use super::fontstyles::{FontModifiable, FontModifier};
    #[test]
    fn test_glyphmap() {
        assert_eq!(Glyph::get("AEacute").to_string(), "Ǽ");
        assert_eq!(Glyph::get("contourintegral").to_string(), "∮");
        assert_eq!(Glyph::get("bulletinverse").to_string(), "◘");
        assert_eq!(Glyph::get("Gangiacoptic").to_string(), "Ϫ");
        assert_eq!(Glyph::get("zukatakana").to_string(), "ズ");
        assert_eq!("test".make_bold().to_string(), "𝐭𝐞𝐬𝐭");
        assert_eq!("test".make_bold().make_sans().to_string(), "𝘁𝗲𝘀𝘁");
        assert_eq!("test".apply_modifiers(&[FontModifier::SansSerif,FontModifier::Bold]).to_string(), "𝘁𝗲𝘀𝘁");
    }
    fn get_store() -> EncodingStore<String,fn(&str) -> String> {
        encodings::EncodingStore::new(|s| {
            std::str::from_utf8(std::process::Command::new("kpsewhich")
                .args(vec!(s)).output().expect("kpsewhich not found!")
                .stdout.as_slice()).unwrap().trim().to_string()
        })
    }
    #[test]
    fn test_encodings() {
        let mut es = get_store();
        let names = es.all_encs().take(50).map(|e| e.tfm_name.clone()).collect::<Vec<_>>();
        for n in names { es.get_glyphlist(n); }
    }
    #[test]
    fn print_table() {
        env_logger::builder().filter_level(log::LevelFilter::Debug).try_init().unwrap();
        let mut es = get_store();
        log::info!("cmr10:\n{}",es.display_encoding("cmr10").unwrap());
        log::info!("cmbx10:\n{}",es.display_encoding("cmbx10").unwrap());
        /*
        log::info!("ptmr7t:\n{}",es.display_encoding("ptmr7t").unwrap());
        log::info!("ecrm1095:\n{}",es.display_encoding("ecrm1095").unwrap());
        log::info!("ec-lmr10:\n{}",es.display_encoding("ec-lmr10").unwrap());
        log::info!("jkpbitc:\n{}",es.display_encoding("jkpbitc").unwrap());
        log::info!("ot1-stix2textsc:\n{}",es.display_encoding("ot1-stix2textsc").unwrap());
        log::info!("stix-mathbbit-bold:\n{}",es.display_encoding("stix-mathbbit-bold").unwrap());
        log::info!("MnSymbolE10:\n{}",es.display_encoding("MnSymbolE10").unwrap());
         */
    }
/*
    #[test]
    fn vfs() {
        env_logger::builder().filter_level(log::LevelFilter::Debug).try_init().unwrap();
        use tex_engine::engine::filesystem::kpathsea::*;
        let mut store = encodings::EncodingStore::new(|s| {
            match KPATHSEA.which(s).map(|s| s.to_str().map(|s| s.to_string())).flatten() {
                Some(s) => s,
                _ => "".into()
            }
        });
        let vfs = &KPATHSEA.post.clone();
        for v in vfs.values() {
            match v.extension() {
                Some(e) if e == "vf" => {
                    let name = v.file_stem().unwrap().to_str().unwrap();
                    log::info!("{}",v.display());
                    match store.display_encoding(name) {
                        Some(s) => log::info!("{}",s),
                        None => log::info!("Failed!")
                    }
                    print!("");
                }
                _ => ()
            }
        }
    }

 */
}