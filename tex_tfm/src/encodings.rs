mod enc_files;
mod pfx_files;
mod vf_files;

use std::fmt::{Display, Write};
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::{Path, PathBuf};
use crate::fontstyles::{FontModifier, ModifierSeq};
use crate::{GlyphList, PATCHED_TABLES};
use crate::glyphs::{Glyph, GlyphI, UNDEFINED_LIST};
use crate::parsing::Parser;

type HMap<A,B> = ahash::HashMap<A,B>;

#[derive(Debug,Clone)]
pub struct FontEnc {
    pub tfm_name: Box<str>,
    //ps_name: Box<str>,
    //fontflags:u32,
    //special: Vec<Box<str>>,
    enc_file: Box<str>,
    pfx_file: Box<str>,
    vf_file: bool,
    pub styles: ModifierSeq,
    glyphlist:Option<usize>,
    pub weblink:Option<(&'static str,&'static str)>
}
#[derive(Debug,Clone)]
pub struct EncodingStore<S:AsRef<str>,F:FnMut(&str) -> S> {
    pdftex_map: HMap<Box<str>,FontEnc>,
    enc_files: HMap<Box<str>,Vec<(Box<str>, usize)>>,
    pfb_files: HMap<Box<str>,usize>,
    glyph_lists:Vec<GlyphList>,
    get:F
}

impl<S:AsRef<str>,F:FnMut(&str) -> S> EncodingStore<S,F> {
    pub fn new(get:F) -> Self {
        let mut map = HMap::default();
        parse_pdftex_map(&mut map);
        include!(concat!(env!("OUT_DIR"), "/codegen_patch.rs"));

        EncodingStore { pdftex_map: map, enc_files: HMap::default(),pfb_files: HMap::default(), glyph_lists:PATCHED_TABLES.to_vec(),get }
    }
    pub fn display_encoding<'a,S2:AsRef<str>>(&'a mut self,name:S2) -> Option<impl Display + 'a> {
        if let Some(u) = self.get_glyphlist_i(&name) {
            let enc = self.pdftex_map.get(name.as_ref()).unwrap();
            Some(DisplayEncoding { enc, list:self.glyph_lists.get(u).unwrap() })
        } else { None }
    }
    pub fn get_info<S2:AsRef<str>>(&self,s:S2) -> Option<&FontEnc> {
        self.pdftex_map.get(s.as_ref())
    }
    pub fn all_encs(&self) -> impl Iterator<Item=&FontEnc> {
        self.pdftex_map.values()
    }
    pub fn get_glyphlist<S2:AsRef<str>>(&mut self,name:S2) -> &GlyphList {
        match self.get_glyphlist_i(name) {
            None => &UNDEFINED_LIST,
            Some(u) => self.glyph_lists.get(u).unwrap()
        }
    }

    fn add_list(e:&mut Vec<GlyphList>,list:GlyphList) -> usize {
        match e.iter().enumerate().find_map(|(i,e)| if *e == list {Some(i)} else {None}) {
            Some(i) => i,
            None => {
                let i = e.len();
                e.push(list);
                i
            }
        }
    }

    fn get_glyphlist_i<'a,S2:AsRef<str>>(&'a mut self,name:S2) -> Option<usize> {
        let mut enc = match self.pdftex_map.get(name.as_ref()) {
            Some(enc) => {
                if let Some(idx) = enc.glyphlist {
                    return Some(idx)
                }
                enc.clone()
            }
            _ => FontEnc {
                tfm_name: name.as_ref().into(),
                enc_file: "".into(),
                pfx_file: "".into(),
                vf_file: false,
                styles: ModifierSeq::empty(),
                glyphlist: None,
                weblink: None
            }
        };
        if !enc.enc_file.is_empty() {
            match self.enc_files.get(&enc.enc_file) {
                Some(e) if e.len() == 1 => {
                    let idx = e[0].1;
                    enc.glyphlist = Some(idx);
                    self.pdftex_map.insert(name.as_ref().into(),enc);
                    return Some(idx)
                }
                Some(_) => enc.enc_file = "".into(),
                None => {
                    let f = (self.get)(enc.enc_file.as_ref());
                    if PathBuf::from(f.as_ref()).exists() {
                        let ls = enc_files::parse_enc(f.as_ref());
                        let retls = ls.into_iter().map(|l| (l.0,Self::add_list(&mut self.glyph_lists,l.1))).collect::<Vec<_>>();
                        self.enc_files.insert(enc.enc_file.clone(),retls.clone());
                        if retls.len() == 1 {
                            enc.glyphlist = Some(retls[0].1);
                            self.pdftex_map.insert(name.as_ref().into(),enc);
                            return Some(retls[0].1)
                        } else {
                            enc.enc_file = "".into();
                        }
                    } else {
                        enc.enc_file = "".into();
                    }
                }
            }
        }
        if !enc.pfx_file.is_empty() {
            match self.pfb_files.get(&enc.pfx_file) {
                Some(idx) => {
                    enc.glyphlist = Some(*idx);
                    self.pdftex_map.insert(name.as_ref().into(),enc);
                    return Some(*idx)
                }
                None => {
                    if enc.pfx_file.ends_with(".pfb") || enc.pfx_file.ends_with(".pfa") {
                        let f = (self.get)(enc.pfx_file.as_ref());
                        if PathBuf::from(f.as_ref()).exists() {
                            if let Some(ls) = pfx_files::parse_pfb(f.as_ref(),&mut enc.styles) {
                                let idx = Self::add_list(&mut self.glyph_lists,ls);
                                self.pfb_files.insert(enc.pfx_file.clone(), idx);
                                enc.glyphlist = Some(idx);
                                self.pdftex_map.insert(name.as_ref().into(),enc);
                                return Some(idx)
                            } else {
                                enc.pfx_file = "".into();
                            }
                        } else {
                            enc.pfx_file = "".into();
                        }
                    } else {
                        enc.pfx_file = "".into();
                    }
                }
            }
        }
        if enc.vf_file {
            self.pdftex_map.insert(name.as_ref().into(),enc);
            return None
        }
        enc.vf_file = true;
        let f = format!("{}.vf",enc.tfm_name);
        let f = (self.get)(&f);
        let p = Path::new(f.as_ref());
        if !p.is_file() {
            self.pdftex_map.insert(name.as_ref().into(),enc);
            return None
        }
        let m = match vf_files::parse_vf(f.as_ref()) {
            None => {
                self.pdftex_map.insert(name.as_ref().into(),enc);
                return None
            }
            Some(m) => m
        };
        let deps = m.deps.into_iter().map(|d| { self.get_glyphlist_i(d) }).collect::<Vec<_>>();
        let mut table = UNDEFINED_LIST.clone();
        for (idx,v) in m.maps.into_iter().enumerate() {
            let mut gls = v.into_iter().map(|(f,i)| match deps.get(f as usize).unwrap() {
                None => vec!(GlyphI::S(0)),
                Some(k) => match self.glyph_lists.get(*k).unwrap().get(i).0 {
                    GlyphI::S(0) => vec!(GlyphI::S(0)),
                    GlyphI::Undefined(_) => vec!(GlyphI::S(0)),
                    GlyphI::S(i) => vec!(GlyphI::S(i)),
                    GlyphI::Unicode(c) => vec!(GlyphI::Unicode(c)),
                    GlyphI::Ls(ls) => ls.into_vec(),
                }
            }).flatten().collect::<Vec<_>>();
            let gl = if gls.len() == 1 {
                Glyph(gls.pop().unwrap())
            } else if gls.is_empty() {
                Glyph(GlyphI::S(0))
            } else {
                Glyph(GlyphI::Ls(gls.into()))
            };
            table.0[idx] = gl;
        }
        let i = Self::add_list(&mut self.glyph_lists,table);
        enc.glyphlist = Some(i);
        self.pdftex_map.insert(name.as_ref().into(),enc);
        Some(i)
    }
}

fn patch(map:&mut HMap<Box<str>,FontEnc>,name:&'static str,modifiers:ModifierSeq,table:Option<usize>,link:Option<(&'static str,&'static str)>) {
    match map.get_mut(name) {
        Some(enc) => {
            enc.styles = modifiers;
            enc.glyphlist = table;
            enc.weblink = link;
        }
        _ => {
            let enc = FontEnc {
                tfm_name: name.into(),
                enc_file: "".into(),
                pfx_file: "".into(),
                vf_file: false,
                styles: modifiers,
                glyphlist: table,
                weblink: link
            };
            map.insert(name.into(),enc);
        }
    }
}

fn parse_pdftex_map(map:&mut HMap<Box<str>,FontEnc>) {
    let enc_file = std::str::from_utf8(std::process::Command::new("kpsewhich")
        .args(vec!("pdftex.map")).output().expect("kpsewhich not found!")
        .stdout.as_slice()).unwrap().trim().to_string();//get("pdftex.map");
    let lines = BufReader::new(File::open(enc_file).unwrap()).lines();
    for l in lines {
        let line = l.unwrap();
        if line.starts_with('%') {
            continue;
        }
        let res = parse_pdftex_map_line(line);
        map.insert(res.tfm_name.clone(),res);
    }
}

fn parse_pdftex_map_line(st:String) -> FontEnc {
    let mut s = Parser::new(&st);
    while s.ends_with('\'') || s.ends_with(' ') {
        s.drop_right(1);
    }
    let tfm_name:Box<str> = s.read_until_ws().into();

    //let mut ps_name = String::new();
    //let mut fontflags = 0;
    //let mut special = vec!();
    let mut enc_file = String::new();
    let mut pfx_file = String::new();
    let mut styles = ModifierSeq::empty();

    macro_rules! modify {
        ($s:expr) => {
            let lc = $s.to_lowercase();
            if lc.contains("bol") {
                styles.add(FontModifier::Bold);
            }
            if lc.contains("ita") {
                styles.add(FontModifier::Italic);
            }
            if lc.contains("obl") {
                styles.add(FontModifier::Oblique);
            }
            if lc.contains("-sc") {
                styles.add(FontModifier::Capitals);
            }
            if lc.contains("blackboard") {
                styles.add(FontModifier::Blackboard);
            }
        }
    }

    modify!(tfm_name);

    while !s.is_empty() {
        if s.starts_with('\"') { // special
            s.skip(1);
            s.read_until('\"');
            continue;
        }
        if s.drop("<[") || s.drop("<<") || s.drop("<") {
            let f:String = s.read_until_ws().into();
            if f.ends_with(".pfb") || f.ends_with(".pfa")|| f.ends_with(".ttf") {
                pfx_file = f;
                modify!(pfx_file);
            } else if f.ends_with(".enc") {
                enc_file = f;
                modify!(enc_file);
            }
            continue
        }
        if s.starts_with_digit() { // fontflags
            s.read_digit();
            continue
        }
        let ps_name = s.read_until_ws();
        modify!(ps_name);
    }

    FontEnc {
        tfm_name,
        //ps_name:ps_name.into(),fontflags,special:special.into(),
        enc_file:enc_file.into(),pfx_file:pfx_file.into(),vf_file:false,
        styles,glyphlist:None,weblink:None
    }
}


pub struct DisplayEncoding<'a> {
    enc:&'a FontEnc,
    list:&'a GlyphList
}
impl<'a> Display for DisplayEncoding<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("| `.tfm` name                          | Modifiers | Table (Optional)  | External Font (Optional)                                             |\n")?;
        f.write_str("|--------------------------------------|-----------|-------------------|----------------------------------------------------------------------|\n")?;
        write!(f,"| {} | ",self.enc.tfm_name)?;
        if self.enc.styles.bold { f.write_char('b')?; }
        if self.enc.styles.italic { f.write_char('i')?; }
        if self.enc.styles.oblique { f.write_char('o')?; }
        if self.enc.styles.sans_serif { f.write_char('s')?; }
        if self.enc.styles.monospaced { f.write_char('m')?; }
        if self.enc.styles.capitals { f.write_char('c')?; }
        if self.enc.styles.script { f.write_char('S')?; }
        if self.enc.styles.blackboard { f.write_char('B')?; }
        if self.enc.styles.fraktur { f.write_char('f')?; }
        write!(f, " | {}_table | ", self.enc.tfm_name)?;
        match self.enc.weblink {
            None => f.write_char('|')?,
            Some((url,name)) => write!(f,"{} {} |",name,url)?
        }
        write!(f,"\n\n- {}_table\n\n",self.enc.tfm_name)?;
        f.write_str("| \\_x\\_  | 0   | 1   | 2    | 3   | 4   | 5   | 6   | 7   | 8   | 9   | A   | B    | C    | D    | E     | F     |\n")?;
        f.write_str("|--------|-----|-----|------|-----|-----|-----|-----|-----|-----|-----|-----|------|------|------|-------|-------|\n")?;
        for i in 0..16 {
            write!(f,"| **{:X}x** | ",i)?;
            for j in 0..16 {
                match &self.list.0[i*16+j].0 {
                    crate::GlyphI::S(0) => f.write_char('|')?,
                    crate::GlyphI::S(g) => write!(f,"/{} | ",crate::GLYPH_NAMES[*g as usize])?,
                    crate::GlyphI::Unicode(c) => write!(f,"\\u{:04X} | ",*c as u32)?,
                    g@crate::GlyphI::Ls(_) => write!(f,"`{}` | ",g.to_string().replace("`","\\`").replace("|","\\|"))?,
                    crate::GlyphI::Undefined(_) => f.write_char('|')?,
                }
            }
            f.write_char('\n')?;
        }
        Ok(())
    }
}
