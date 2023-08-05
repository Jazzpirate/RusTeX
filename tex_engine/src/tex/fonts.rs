pub mod tfm_files;
//use include_dir::{Dir, include_dir};
//static TFM_FILES : Dir<'_> = include_dir!("$CARGO_MANIFEST_DIR/resources/fontmaps");

use std::fmt::{Debug, Display, Formatter};
use std::path::PathBuf;
use std::sync::RwLock;
use crate::engine::filesystem::FileSystem;
use crate::engine::filesystem::kpathsea::KPATHSEA;
use crate::engine::state::State;
use crate::tex::fonts::tfm_files::TfmFile;
use crate::tex::numbers::{Dim, Dimi32};
use crate::tex::token::Token;
use crate::throw;
use crate::utils::errors::TeXError;
use crate::utils::map::HMap;
use crate::utils::Ptr;
use crate::utils::strings::CharType;

pub trait FontStore:Clone+'static {
    type Char:CharType;
    type Font:Font<Char=Self::Char>;
    fn get_new<T:Token<Char=Self::Char>>(&mut self,s: &str) -> Result<Self::Font,TeXError<T>>;
    fn null(&self) -> Self::Font;
}
pub trait Font:Debug+Display+Clone + PartialEq {
    type Char:CharType;
    fn set_at(&mut self,at:i64);
    fn get_at(&self) -> i64;

    fn set_hyphenchar(&mut self,hyphenchar:i64);
    fn get_hyphenchar(&self) -> i64;

    fn set_skewchar(&mut self,skewchar:i64);
    fn get_skewchar(&self) -> i64;

    fn set_dim<D:Dim>(&mut self,i:usize,d:D);
    fn get_dim<D:Dim>(&self, i: usize) -> D;
}

#[derive(Clone)]
pub struct TfmFontStore {
    font_files:HMap<PathBuf,Ptr<TfmFile>>,null:TfmFont
}
impl FontStore for TfmFontStore {
    type Char = u8;
    type Font = TfmFont;
    fn get_new<T:Token<Char=Self::Char>>(&mut self, s: &str) -> Result<Self::Font,TeXError<T>> {
        let path = match KPATHSEA.get(s) {
            None => throw!("Font not found: {}",s),
            Some(res) => res.path
        };
        let file = match self.font_files.get(&path) {
            None => {
                let mut file = TfmFile::new(path.clone());
                self.font_files.insert(path.clone(),Ptr::new(file));
                self.font_files.get(&path).unwrap().clone()
            },
            Some(file) => file.clone()
        };
        Ok(Ptr::new(TfmFontInner{
            hyphenchar:RwLock::new(file.hyphenchar as i64),
            skewchar:RwLock::new(file.skewchar as i64),
            file,
            at:RwLock::new(None),
            dimens:RwLock::new(HMap::default()),
            lps:HMap::default(),
            rps:HMap::default(),
        }))
    }
    fn null(&self) -> Self::Font { self.null.clone() }
}
impl TfmFontStore {
    pub fn new() -> Self {
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
            lps:[0;256],
            rps:[0;256],
            ligs:HMap::default(),
            name:"nullfont".to_string(),
            filepath:"nullfont".to_string()
        };
        let font = Ptr::new(TfmFontInner{
            hyphenchar:RwLock::new(null.hyphenchar as i64),
            skewchar:RwLock::new(null.skewchar as i64),
            file:Ptr::new(null),
            at:RwLock::new(None),
            dimens:RwLock::new(HMap::default()),
            lps:HMap::default(),
            rps:HMap::default(),
        });
        Self{null:font, font_files:HMap::default()}
    }
}
// todo: replace by Arrays, maybe
pub struct TfmFontInner {
    file:Ptr<TfmFile>,
    at:RwLock<Option<i64>>,
    dimens:RwLock<HMap<usize,i64>>,
    hyphenchar:RwLock<i64>,
    skewchar:RwLock<i64>,
    lps:HMap<u8,u8>,
    rps:HMap<u8,u8>,
}
impl PartialEq for TfmFontInner {
    fn eq(&self, other: &Self) -> bool {
        self.file == other.file &&
            *self.at.read().unwrap() == *other.at.read().unwrap() &&
            *self.hyphenchar.read().unwrap() == *other.hyphenchar.read().unwrap() &&
            *self.skewchar.read().unwrap() == *other.skewchar.read().unwrap() &&
            *self.dimens.read().unwrap() == *other.dimens.read().unwrap()
    }
}

impl Debug for TfmFontInner {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f,"Font({})",self.file.name)
    }
}
impl Display for TfmFontInner {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f,"{}",self.file.name)?;
        match &*self.at.read().unwrap() {
            None => Ok(()),
            Some(at) => write!(f," at {}",Dimi32(*at as i32))
        }
    }
}
pub type TfmFont = Ptr<TfmFontInner>;
impl Font for TfmFont {
    type Char = u8;
    fn set_at(&mut self,at:i64) {
        *self.at.write().unwrap() = Some(at);
    }
    fn get_at(&self) -> i64 {
        match &*self.at.read().unwrap() {
            Some(at) => *at,
            None => self.file.size
        }
    }

    fn get_hyphenchar(&self) -> i64 {
        self.hyphenchar.read().unwrap().clone()
    }
    fn set_hyphenchar(&mut self, hyphenchar: i64) {
        *self.hyphenchar.write().unwrap() = hyphenchar;
    }

    fn get_skewchar(&self) -> i64 {
        self.skewchar.read().unwrap().clone()
    }
    fn set_skewchar(&mut self, skewchar: i64) {
        *self.skewchar.write().unwrap() = skewchar;
    }

    fn set_dim<D:Dim>(&mut self, i: usize, d: D) {
        self.dimens.write().unwrap().insert(i,d.to_sp());
    }
    fn get_dim<D:Dim>(&self, i: usize) -> D {
        D::from_sp(self.dimens.read().unwrap().get(&i).map(|c| *c).unwrap_or(
            if i > 0 && i < 256 {
                ((self.file.dimen[i] * (self.get_at() as f64)).round() as i64)
            } else {
                0
            }
        ))
    }
}