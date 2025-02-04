use crate::glyphs::UNDEFINED_LIST;
use crate::parsing::Parser;
use crate::{Glyph, GlyphList};

pub fn parse_enc(f: &str) -> Vec<(Box<str>, GlyphList)> {
    let file = std::fs::read_to_string(f).expect("Error reading .enc file");
    let mut s = Parser::new(&file);
    let mut ret = Vec::new();
    while !s.is_empty() {
        if s.starts_with('%') {
            s.skip_until_endline();
            continue;
        }
        if s.starts_with_str("LIGKERN") {
            s.read_until(';');
            continue;
        }
        if s.starts_with('/') {
            let name: Box<str> = s.read_until('[').trim().into();
            let glyphlist = parse_glyphlist(&mut s);
            ret.push((name, glyphlist));
        }
    }
    ret
}

fn parse_glyphlist(s: &mut Parser<'_>) -> GlyphList {
    let mut ret = UNDEFINED_LIST.clone();
    let mut curr = 0usize;
    while !s.is_empty() {
        if s.starts_with(']') {
            s.skip(1);
            break;
        }
        if s.starts_with('%') {
            s.skip_until_endline();
            continue;
        }
        if s.starts_with('/') {
            assert!(curr <= 255, "Too many glyphs in glyphlist!");
            s.skip(1);
            let name = s.read_until_ws_or('/');
            ret.0[curr] = Glyph::get(name);
            curr += 1;
        }
    }
    s.drop("def");
    assert!(curr > 0, "Empty glyphlist!");
    assert!(curr >= 20, "Too few glyphs in glyphlist!");
    ret
}
