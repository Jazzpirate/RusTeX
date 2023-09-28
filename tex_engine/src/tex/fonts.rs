pub mod tfm_files;
//use include_dir::{Dir, include_dir};
//static TFM_FILES : Dir<'_> = include_dir!("$CARGO_MANIFEST_DIR/resources/fontmaps");

use std::fmt::{Debug, Display, Formatter};
use std::path::PathBuf;
use crate::engine::EngineType;
use crate::engine::filesystem::kpathsea::KPATHSEA;
use crate::engine::memory::{Interner, Memory};
use crate::tex::fonts::tfm_files::TfmFile;
use crate::tex::numbers::{Dim, Dimi32, Int};
use crate::throw;
use crate::utils::errors::TeXError;
use crate::utils::collections::HMap;
use crate::utils::{Mut, Ptr};
use crate::utils::strings::{CharType, TeXStr};

pub trait FontStore:'static+Debug {
    type Char:CharType;
    type RefType:Copy+PartialEq+Debug+Default;
    type Font:Font<Char=Self::Char>;
    fn get_new<ET:EngineType<Char=Self::Char,FontStore=Self,Font=Self::Font, FontRef=Self::RefType>>(&mut self, s: &str, macroname:TeXStr) -> Self::RefType;
    fn get(&self,id:Self::RefType) -> &Self::Font;
    fn get_mut(&mut self,id:Self::RefType) -> &mut Self::Font;
}
pub trait Font:Debug+Display {
    type Char:CharType;
    fn set_at(&mut self,at:i64);
    fn get_at(&self) -> i64;
    fn name(&self) -> TeXStr;
    fn exists(&self,char:Self::Char) -> bool;
    fn char_wd<D:Dim>(&self,char:Self::Char) -> D;
    fn char_ht<D:Dim>(&self,char:Self::Char) -> D;
    fn char_dp<D:Dim>(&self,char:Self::Char) -> D;
    fn char_ic<D:Dim>(&self,char:Self::Char) -> D;
    
    fn ligature(&self,char1:Self::Char,char2:Self::Char) -> Option<Self::Char>;

    fn set_hyphenchar(&mut self,hyphenchar:i64);
    fn get_hyphenchar(&self) -> i64;

    fn get_lpcode<I:Int>(&self,char:Self::Char) -> I;
    fn get_rpcode<I:Int>(&self,char:Self::Char) -> I;
    fn set_lpcode<I:Int>(&mut self,char:Self::Char,code:I);
    fn set_rpcode<I:Int>(&mut self,char:Self::Char,code:I);

    fn set_skewchar(&mut self,skewchar:i64);
    fn get_skewchar(&self) -> i64;

    fn set_dim<D:Dim>(&mut self,i:usize,d:D);
    fn get_dim<D:Dim>(&self, i: usize) -> D;
}

#[derive(Debug)]
pub struct TfmFontStore {
    font_files:HMap<PathBuf,Ptr<TfmFile>>,fonts:Vec<TfmFont>
}
impl FontStore for TfmFontStore {
    type Char = u8;
    type Font = TfmFont;
    type RefType = usize;
    fn get(&self, id: Self::RefType) -> &Self::Font {
        &self.fonts[id]
    }
    fn get_mut(&mut self, id: Self::RefType) -> &mut Self::Font {
        &mut self.fonts[id]
    }
    fn get_new<ET:EngineType<Char=Self::Char,FontStore=Self,Font=Self::Font, FontRef=Self::RefType>>(&mut self, s: &str, macroname:TeXStr) -> Self::RefType {
        let path = match KPATHSEA.get(s) {
            None => throw!("Font not found: {}",s),
            Some(res) => res.path
        };
        let file = match self.font_files.get(&path) {
            None => {
                let file = TfmFile::new(path.clone());
                self.font_files.insert(path.clone(),Ptr::new(file));
                self.font_files.get(&path).unwrap().clone()
            },
            Some(file) => file.clone()
        };
        self.fonts.push(TfmFont{
            name:macroname,
            hyphenchar:file.hyphenchar as i64,
            skewchar:file.skewchar as i64,
            file,
            at:None,
            dimens:HMap::default(),
            lps:HMap::default(),
            rps:HMap::default(),
        });
        self.fonts.len()-1
    }
}
impl TfmFontStore {
    pub fn new(interner:&mut Interner) -> Self {
        let null = TfmFile {
            hyphenchar:45,
            skewchar:255,
            dimen:[0.0;256],
            size:0,
            typestr:"nullfont".to_string(),
            widths:[0.0;256],
            heights:[0.0;256],
            depths:[0.0;256],
            ics:[0.0;256],
            ligs:HMap::default(),
            name:"nullfont".to_string(),
            filepath:"nullfont".to_string()
        };
        let font = TfmFont{
            name:TeXStr::from_static("nullfont",interner),
            hyphenchar:null.hyphenchar as i64,
            skewchar:null.skewchar as i64,
            file:Ptr::new(null),
            at:None,
            dimens:HMap::default(),
            lps:HMap::default(),
            rps:HMap::default(),
        };
        let fonts = vec!(font);
        Self{fonts, font_files:HMap::default()}
    }
}
// todo: replace by Arrays, maybe
pub struct TfmFont {
    name:TeXStr,
    file:Ptr<TfmFile>,
    at:Option<i64>,
    dimens:HMap<usize,i64>,
    hyphenchar:i64,
    skewchar:i64,
    lps:HMap<u8,i16>,
    rps:HMap<u8,i16>,
}
impl PartialEq for TfmFont {
    fn eq(&self, other: &Self) -> bool {
        self.file == other.file &&
            self.at == other.at &&
            self.hyphenchar == other.hyphenchar &&
            self.skewchar == other.skewchar &&
            self.dimens == other.dimens
    }
}

impl Debug for TfmFont {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f,"Font({})",self.file.name)
    }
}
impl Display for TfmFont {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f,"{}",self.file.name)?;
        match &self.at {
            None => Ok(()),
            Some(at) => write!(f," at {}",Dimi32(*at as i32))
        }
    }
}

impl Font for TfmFont {
    type Char = u8;
    fn set_at(&mut self,at:i64) {
        self.at = Some(at);
    }
    fn get_at(&self) -> i64 {
        match &self.at {
            Some(at) => *at,
            None => self.file.size
        }
    }
    fn name(&self) -> TeXStr { self.name }
    fn exists(&self, char: Self::Char) -> bool {
        self.file.heights[char as usize] > 0.0 ||
            self.file.widths[char as usize] > 0.0 ||
            self.file.depths[char as usize] > 0.0
    }
    fn char_ht<D: Dim>(&self, char: Self::Char) -> D {
        D::from_sp((self.file.heights[char as usize] * (self.get_at() as f64)).round() as i64)
    }
    fn char_wd<D: Dim>(&self, char: Self::Char) -> D {
        D::from_sp((self.file.widths[char as usize] * (self.get_at() as f64)).round() as i64)
    }
    fn char_dp<D: Dim>(&self, char: Self::Char) -> D {
        D::from_sp((self.file.depths[char as usize] * (self.get_at() as f64)).round() as i64)
    }
    fn char_ic<D: Dim>(&self, char: Self::Char) -> D {
        D::from_sp((self.file.ics[char as usize] * (self.get_at() as f64)).round() as i64)
    }
    fn get_lpcode<I: Int>(&self, char: Self::Char) -> I {
        match self.lps.get(&char) {
            None => I::from_i64(0) ,
            Some(i) => I::from_i64(*i as i64)
        }
    }
    fn get_rpcode<I: Int>(&self, char: Self::Char) -> I {
        match self.rps.get(&char) {
            None => I::from_i64(0) ,
            Some(i) => I::from_i64(*i as i64)
        }
    }
    fn set_lpcode<I: Int>(&mut self, char: Self::Char, code: I) {
        let i = std::cmp::min(std::cmp::max(code.to_i64(),-1000),1000) as i16;
        self.lps.insert(char,i);
    }
    fn set_rpcode<I: Int>(&mut self, char: Self::Char, code: I) {
        let i = std::cmp::min(std::cmp::max(code.to_i64(),-1000),1000) as i16;
        self.rps.insert(char,i);
    }

    fn get_hyphenchar(&self) -> i64 {
        self.hyphenchar
    }
    fn set_hyphenchar(&mut self, hyphenchar: i64) {
        self.hyphenchar = hyphenchar;
    }

    fn get_skewchar(&self) -> i64 {
        self.skewchar
    }
    fn set_skewchar(&mut self, skewchar: i64) {
        self.skewchar = skewchar;
    }

    fn set_dim<D:Dim>(&mut self, i: usize, d: D) {
        self.dimens.insert(i,d.to_sp());
    }
    fn get_dim<D:Dim>(&self, i: usize) -> D {
        D::from_sp(self.dimens.get(&i).map(|c| *c).unwrap_or(
            if i > 0 && i < 256 {
                (self.file.dimen[i] * (self.get_at() as f64)).round() as i64
            } else {
                0
            }
        ))
    }
    fn ligature(&self, char1: Self::Char, char2: Self::Char) -> Option<Self::Char> {
        match self.file.ligs.get(&(char1,char2)) {
            Some(c) => Some(*c),
            _ => None
        }
    }
}