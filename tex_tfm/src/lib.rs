#![allow(text_direction_codepoint_in_literal)]

pub mod fontstyles;
pub mod encodings;
mod parsing;
pub mod glyphs;

use crate::glyphs::{Glyph,GlyphList,GlyphI,UNDEFINED};

include!(concat!(env!("OUT_DIR"), "/codegen.rs"));


#[cfg(test)]
mod tests {
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
    #[test]
    fn test_encodings() {
        let mut es = encodings::EncodingStore::new(|s| {
            std::str::from_utf8(std::process::Command::new("kpsewhich")
                .args(vec!(s)).output().expect("kpsewhich not found!")
                .stdout.as_slice()).unwrap().trim().to_string()
        });
        let names = es.all_encs().take(50).map(|e| e.tfm_name.clone()).collect::<Vec<_>>();
        for n in names { es.get_glyphlist(n); }
    }
}