pub mod tfm_files;
//use include_dir::{Dir, include_dir};
//static TFM_FILES : Dir<'_> = include_dir!("$CARGO_MANIFEST_DIR/resources/fontmaps");

use std::path::PathBuf;
use crate::engine::filesystem::FileSystem;
use crate::engine::filesystem::kpathsea::KPATHSEA;
use crate::engine::state::State;
use crate::tex::fonts::tfm_files::TfmFile;
use crate::tex::numbers::{NumSet,Dim};
use crate::tex::token::Token;
use crate::utils::errors::{OtherError, TeXError};
use crate::utils::map::HMap;
use crate::utils::Ptr;
use crate::utils::strings::CharType;

pub trait FontStore:'static {
    type Char:CharType;
    type F:Font<Char=Self::Char>;
    fn get_new<T:Token<Char=Self::Char>>(&mut self,s: &str) -> Result<usize,Box<dyn TeXError<T>>>;
    fn get(&mut self,i:usize) -> &mut Self::F;
}
pub trait Font{
    type Char:CharType;
    fn set_at(&mut self,at:i64);
    fn get_at(&self) -> i64;
    fn set_dim<NS:NumSet>(&mut self,i:usize,d:NS::Dim);
    fn get_dim<NS: NumSet>(&self, i: usize) -> NS::Dim;
}

pub struct TfmFontStore {
    font_files:HMap<PathBuf,Ptr<TfmFile>>,
    fonts:Vec<TfmFont>
}
impl FontStore for TfmFontStore {
    type Char = u8;
    type F = TfmFont;
    fn get_new<T:Token<Char=Self::Char>>(&mut self, s: &str) -> Result<usize,Box<dyn TeXError<T>>> {
        let path = match KPATHSEA.get(s) {
            None => return Err(OtherError{msg:format!("Font not found: {}",s),cause:None,source:None}.into()),
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
        self.fonts.push(TfmFont{
            file,
            at:None,
            name:s.to_string(),
            dimens:HMap::default(),
            hyphenchar:0,
            skewchar:0,
            lps:HMap::default(),
            rps:HMap::default(),
        });
        Ok(self.fonts.len() - 1)
    }
    fn get(&mut self,i:usize) -> &mut Self::F {
        self.fonts.get_mut(i).unwrap()
    }
}
impl TfmFontStore {
    pub(crate) fn new() -> Self { Self{fonts:vec!(), font_files:HMap::default()} }
}
// todo: replace by Arrays, maybe
pub struct TfmFont {
    file:Ptr<TfmFile>,
    at:Option<i64>,
    name:String,
    dimens:HMap<usize,i64>,
    hyphenchar:u8,
    skewchar:u8,
    lps:HMap<u8,u8>,
    rps:HMap<u8,u8>,
}
impl Font for TfmFont {
    type Char = u8;
    fn set_at(&mut self,at:i64) {
        self.at = Some(at);
    }
    fn get_at(&self) -> i64 {
        match self.at {
            Some(at) => at,
            None => self.file.size
        }
    }
    fn set_dim<NS: NumSet>(&mut self, i: usize, d: NS::Dim) {
        self.dimens.insert(i,d.to_sp());
    }
    fn get_dim<NS: NumSet>(&self, i: usize) -> NS::Dim {
        NS::Dim::from_sp(self.dimens.get(&i).map(|c| *c).unwrap_or(
            if i > 0 && i < 256 {
                ((self.file.dimen[i] * (self.get_at() as f64)).round() as i64)
            } else {
                0
            }
        ))
    }
}