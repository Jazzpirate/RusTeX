mod enc_files;
mod pfx_files;

use std::fmt::{Display, Write};
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::PathBuf;
use crate::fontstyles::{FontModifier, ModifierSeq};
use crate::GlyphList;
use crate::glyphs::UNDEFINED_LIST;
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
    pub styles: ModifierSeq,
    glyphlist:Option<usize>,
    pub weblink:Option<(&'static str,&'static str)>
}
#[derive(Debug,Clone)]
pub struct EncodingStore<S:AsRef<str>,F:FnMut(&str) -> S> {
    pdftex_map: HMap<Box<str>,FontEnc>,
    enc_files: HMap<Box<str>,Vec<(Box<str>, GlyphList)>>,
    pfb_files: HMap<Box<str>,GlyphList>,
    get:F
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

impl<S:AsRef<str>,F:FnMut(&str) -> S> EncodingStore<S,F> {
    pub fn new(get:F) -> Self {
        let mut map = HMap::default();
        parse_pdftex_map(&mut map);
        include!(concat!(env!("OUT_DIR"), "/codegen_patch.rs"));

        EncodingStore { pdftex_map: map, enc_files: HMap::default(),pfb_files: HMap::default(),get }
    }
    pub fn display_encoding<'a,S2:AsRef<str>>(&'a mut self,name:S2) -> Option<impl Display + 'a> {
        if let Some((enc,list)) = self.get_glyphlist_i(name) {
            Some(DisplayEncoding { enc, list })
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
            Some((_,l)) => l
        }
    }
    fn get_glyphlist_i<S2:AsRef<str>>(&mut self,name:S2) -> Option<(&FontEnc,&GlyphList)> {
        match self.pdftex_map.get_mut(name.as_ref()) {
            None => None,
            Some(enc) if enc.glyphlist.is_some() => {
                Some((&*enc,&crate::PATCHED_TABLES[enc.glyphlist.unwrap()]))
            }
            Some(enc) if !enc.enc_file.is_empty() => {
                match get_from_enc(&mut self.get,&mut self.enc_files, enc) {
                    Ok(r) => Some(r),
                    Err(enc) => match get_from_pfx(&mut self.get,&mut self.pfb_files, enc) {
                        (e,Some(r)) => Some((e,r)),
                        (e,_) => Some((e,&UNDEFINED_LIST))
                    }
                }
            }
            Some(enc) => match get_from_pfx(&mut self.get,&mut self.pfb_files, enc) {
                (e,Some(r)) => Some((e,r)),
                (e,_) => Some((e,&UNDEFINED_LIST))
            }
        }
    }
}

fn patch(map:&mut HMap<Box<str>,FontEnc>,name:&'static str,modifiers:ModifierSeq,table:Option<usize>,link:Option<(&'static str,&'static str)>) {
    match map.get_mut(name) {
        Some(enc) => {
            enc.styles = modifiers;
            enc.glyphlist = table;
            enc.weblink = link;
        }
        _ => ()
    }
}

fn get_from_pfx<'a,S:AsRef<str>,F:FnMut(&str) -> S>(get:&mut F, map:&'a mut HMap<Box<str>,GlyphList>, enc:&'a mut FontEnc) -> (&'a FontEnc,Option<&'a GlyphList>) {
    if enc.pfx_file.is_empty() {
        return (&*enc,None)
    }
    match map.get(&enc.pfx_file) {
        Some(_) => (),
        None => {
            if enc.pfx_file.ends_with(".pfb") || enc.pfx_file.ends_with(".pfa") {
                let f = get(enc.pfx_file.as_ref());
                if PathBuf::from(f.as_ref()).exists() {
                    let ls = pfx_files::parse_pfb(f.as_ref(),&mut enc.styles);
                    map.insert(enc.pfx_file.clone(),ls);
                } else {
                    enc.pfx_file = "".into();
                    return (&*enc,None)
                }
            } else {
                todo!("File ending: {}",enc.pfx_file)
            }
        }
    }
    (&*enc,map.get(&enc.pfx_file))
}

fn get_from_enc<'a,S:AsRef<str>,F:FnMut(&str) -> S>(get:&mut F,map:&'a mut HMap<Box<str>,Vec<(Box<str>,GlyphList)>>,enc:&'a mut FontEnc) -> Result<(&'a FontEnc,&'a GlyphList),&'a mut FontEnc> {
    match map.get(&enc.enc_file) {
        None => {
            let f = get(enc.enc_file.as_ref());
            if PathBuf::from(f.as_ref()).exists() {
                let ls = enc_files::parse_enc(f.as_ref());
                map.insert(enc.enc_file.clone(),ls);
            } else {
                enc.enc_file = "".into();
                return Err(enc)
            }
        },
        _ => ()
    }
    let e = map.get(&enc.enc_file).unwrap();
    if e.len() == 1 {
        Ok((&*enc,&e[0].1))
    } else {
        todo!("Length: {}; names: {:?}",e.len(),e.iter().map(|(n,_)| n.as_ref()).collect::<Vec<_>>())
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
        enc_file:enc_file.into(),pfx_file:pfx_file.into(),
        styles,glyphlist:None,weblink:None
    }
}