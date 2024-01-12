mod tfm;

use std::collections::BTreeMap;
use std::path::PathBuf;
use std::sync::RwLock;
use crate::commands::Command;
use crate::engine::{EngineAux, EngineReferences, EngineTypes};
use crate::engine::filesystem::{File, FileSystem};
use crate::engine::fontsystem::tfm::TfmFile;
use crate::engine::gullet::ResolvedToken;
use crate::engine::utils::memory::MemoryManager;
use crate::tex::tokens::control_sequences::{CSName,CSHandler};
use crate::tex::input_text::Character;
use crate::tex::numerics::{Numeric, TeXDimen, TeXInt};
use crate::utils::{HMap, Ptr};

pub trait FontSystem:Clone+std::fmt::Debug {
    type Char:Character;
    type CS: CSName<Self::Char>;
    type Int:TeXInt;
    type Font:Font<Char=Self::Char,CS=Self::CS, Dim=Self::Dim,Int=Self::Int>;
    type Dim:TeXDimen;
    fn null(&self) -> Self::Font;
    fn new<ET:EngineTypes<Char=Self::Char,CSName=Self::CS>>(aux:&mut EngineAux<ET>) -> Self;
    fn new_font<S:AsRef<str>,F:FileSystem>(&mut self,path:S,macroname:<Self::Font as Font>::CS,fs:&mut F) -> Self::Font;
}

pub trait Font:Clone+std::fmt::Debug {
    type Char:Character;
    type Dim:TeXDimen;
    type Int:TeXInt;
    type CS: CSName<Self::Char>;
    fn get_at(&self) -> Self::Dim;
    fn has_at_set(&self) -> bool;
    fn set_at(&mut self,d:Self::Dim);
    fn name(&self) -> &Self::CS;
    fn filename(&self) -> &str;
    fn get_dim(&self,idx:u16) -> Self::Dim;
    fn set_dim(&mut self,idx:u16,d:Self::Dim);
    fn get_hyphenchar(&self) -> Self::Int;
    fn set_hyphenchar(&mut self,c:Self::Int);
    fn get_skewchar(&self) -> Self::Int;
    fn set_skewchar(&mut self,c:Self::Int);
    fn has_char(&self,c:Self::Char) -> bool;
    fn display<W:std::fmt::Write>(&self, i:&<Self::CS as CSName<Self::Char>>::Handler, w:W) -> std::fmt::Result;
    fn get_wd(&self,c:Self::Char) -> Self::Dim;
    fn get_ht(&self,c:Self::Char) -> Self::Dim;
    fn get_dp(&self,c:Self::Char) -> Self::Dim;
    fn get_ic(&self,c:Self::Char) -> Self::Dim;
    fn set_ic(&mut self,c:Self::Char,d:Self::Dim);
    fn get_lp(&self,c:Self::Char) -> Self::Int;
    fn set_lp(&mut self,c:Self::Char,d:Self::Int);
    fn get_rp(&self,c:Self::Char) -> Self::Int;
    fn set_rp(&mut self,c:Self::Char,d:Self::Int);
    fn ligature(&self,c1:Self::Char,c2:Self::Char) -> Option<Self::Char>;
}
#[derive(Clone,Debug)]
pub struct TfmFontSystem<I:TeXInt,D:TeXDimen + Numeric<I>,CS: CSName<u8>> {
    files:HMap<PathBuf,Ptr<TfmFile>>,
    null:Ptr<TfmFontI<I,D,CS>>
}

impl<I:TeXInt,D:TeXDimen + Numeric<I>,CS: CSName<u8>> FontSystem for TfmFontSystem<I,D,CS> {
    type Char = u8;
    type Int = I;
    type Font = TfmFont<I,D,CS>;
    type Dim = D;
    type CS=CS;

    fn new<ET: EngineTypes<Char=Self::Char,CSName=Self::CS>>(aux: &mut EngineAux<ET>) -> Self {
        let null_file = TfmFile {
            hyphenchar:45,
            skewchar:255,
            dimen:vec!(),
            size:0,
            typestr:"nullfont".to_string(),
            widths:[0.0;256],
            defined:[false;256],
            heights:[0.0;256],
            depths:[0.0;256],
            ics:[0.0;256],
            ligs:BTreeMap::default(),
            filepath:std::path::PathBuf::from("/nullfont")
        };
        let muts = Mutables::default();
        let null = Ptr::new(TfmFontI{
            file:Ptr::new(null_file),
            muts:RwLock::new(muts),
            name:aux.memory.cs_interner_mut().new("nullfont")
        });
        TfmFontSystem {
            files:HMap::default(),
            null
        }
    }

    fn new_font<S:AsRef<str>,F:FileSystem>(&mut self,path:S,macroname:CS,fs:&mut F) -> Self::Font {
        let path = path.as_ref();
        let f = if path.ends_with(".tfm") {
            fs.get(path)
        } else {
            fs.get(&format!("{}.tfm",path))
        };
        let ff = match self.files.get(f.path()) {
            Some(ff) => ff.clone(),
            None => {
                let ff = Ptr::new(TfmFile::new(f.path().to_path_buf()));
                self.files.insert(f.path().to_path_buf(),ff.clone());
                ff
            }
        };
        let muts = Mutables::default();
        let font = TfmFontI {
            file:ff,
            muts:RwLock::new(muts),
            name:macroname
        };
        Ptr::new(font)
    }

    fn null(&self) -> Self::Font {
        self.null.clone()
    }
}

#[derive(Default)]
struct Mutables<I:TeXInt,D:TeXDimen + Numeric<I>>  {
    at:Option<D>,
    lps:HMap<u8,I>,
    rps:HMap<u8,I>,
    ics:HMap<u8,D>,
    hyphenchar:Option<I>,
    skewchar:Option<I>,
    dimens:Vec<D>
}

pub struct TfmFontI<I:TeXInt,D:TeXDimen + Numeric<I>,CS: CSName<u8>>  {
    file:Ptr<TfmFile>,
    name:CS,
    muts:RwLock<Mutables<I,D>>
}
impl<I:TeXInt,D:TeXDimen + Numeric<I>,CS: CSName<u8>> PartialEq for TfmFontI<I,D,CS> {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}
impl<I:TeXInt,D:TeXDimen + Numeric<I>,CS: CSName<u8>> std::fmt::Debug for TfmFontI<I,D,CS> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f,"Font {:?}",self.name)
    }

}

pub type TfmFont<I,D,CS> = Ptr<TfmFontI<I,D,CS>>;
impl<I:TeXInt,D:TeXDimen + Numeric<I>,CS: CSName<u8>> Font for TfmFont<I,D,CS> {
    type Char = u8;
    type CS = CS;
    type Int = I;
    type Dim = D;
    #[inline(always)]
    fn has_char(&self, c: Self::Char) -> bool { self.file.defined[c as usize] }
    fn get_hyphenchar(&self) -> I {
        match self.muts.read().unwrap().hyphenchar {
            Some(c) => c,
            None => (self.file.hyphenchar as i32).into()
        }
    }
    fn set_hyphenchar(&mut self, c: I) {
        self.muts.write().unwrap().hyphenchar = Some(c);
    }
    fn get_skewchar(&self) -> I {
        match self.muts.read().unwrap().skewchar {
            Some(c) => c,
            None => (self.file.skewchar as i32).into()
        }
    }
    fn set_skewchar(&mut self, c: I) {
        self.muts.write().unwrap().skewchar = Some(c);
    }
    fn get_at(&self) -> Self::Dim {
        let at = self.muts.read().unwrap().at;
        match at {
            Some(d) => d,
            None => D::from_sp(self.file.size as i32)
        }
    }
    fn has_at_set(&self) -> bool {
        self.muts.read().unwrap().at.is_some()
    }
    fn get_dim(&self, idx: u16) -> Self::Dim {
        match self.muts.read().unwrap().dimens.get(idx as usize) {
            Some(d) => *d,
            None => match self.file.dimen.get(idx as usize) {
                Some(d) => self.get_at().scale_float(*d),
                None => D::default()
            }
        }
    }
    fn set_dim(&mut self, idx: u16, d: Self::Dim) {
        let v = &mut self.muts.write().unwrap().dimens;
        if idx as usize >= v.len() {
            v.resize(idx as usize + 1,D::default());
        }
        v[idx as usize] = d;
    }
    #[inline(always)]
    fn set_at(&mut self, d: Self::Dim) {
        self.muts.write().unwrap().at = Some(d);
    }
    #[inline(always)]
    fn name(&self) -> &Self::CS {
        &self.name
    }
    #[inline(always)]
    fn filename(&self) -> &str {
        self.file.name()
    }
    fn display<W:std::fmt::Write>(&self, _i:&<Self::CS as CSName<u8>>::Handler, mut w:W) -> std::fmt::Result {
        let at = self.muts.read().unwrap().at;
        match at {
            Some(d) => write!(w,"{} at {}",self.file.name(),d),
            None => write!(w,"{}",self.file.name())
        }
    }

    fn get_ic(&self, c: Self::Char) -> Self::Dim {
        let v = &mut self.muts.write().unwrap().ics;
        match v.get(&c) {
            Some(d) => *d,
            None => {
                let d = self.file.ics[c as usize];
                self.get_at().scale_float(d)
            }
        }
    }
    fn set_ic(&mut self, c: Self::Char, d: Self::Dim) {
        let v = &mut self.muts.write().unwrap().ics;
        v.insert(c,d);
    }

    fn get_lp(&self, c: Self::Char) -> I {
        let v = &mut self.muts.write().unwrap().lps;
        match v.get(&c) {
            Some(d) => *d,
            None => I::default()
        }
    }
    fn set_lp(&mut self, c: Self::Char, d: I) {
        let v = &mut self.muts.write().unwrap().lps;
        v.insert(c,d);
    }

    fn get_rp(&self, c: Self::Char) -> I {
        let v = &mut self.muts.write().unwrap().rps;
        match v.get(&c) {
            Some(d) => *d,
            None => I::default()
        }
    }
    fn set_rp(&mut self, c: Self::Char, d: I) {
        let v = &mut self.muts.write().unwrap().rps;
        v.insert(c,d);
    }
    #[inline(always)]
    fn get_wd(&self, c: Self::Char) -> Self::Dim {
        let d = self.file.widths[c as usize];
        self.get_at().scale_float(d)
    }
    #[inline(always)]
    fn get_ht(&self, c: Self::Char) -> Self::Dim {
        let d = self.file.heights[c as usize];
        self.get_at().scale_float(d)
    }
    #[inline(always)]
    fn get_dp(&self, c: Self::Char) -> Self::Dim {
        let d = self.file.depths[c as usize];
        self.get_at().scale_float(d)
    }
    #[inline(always)]
    fn ligature(&self,char1:Self::Char,char2:Self::Char) -> Option<Self::Char> {
        self.file.ligs.get(&(char1,char2)).copied()
    }
}

impl<ET:EngineTypes> EngineReferences<'_,ET> {
    pub fn read_font(&mut self) -> <ET::FontSystem as FontSystem>::Font {
        crate::expand_loop!(self,
            ResolvedToken::Cmd {cmd:Some(c),token} => match c {
                Command::Font(f) => return f.clone(),
                Command::FontCmd(f) => {
                    let f = f.read;
                    return f(self,token)
                }
                _ => todo!("error")
            }
            _ => todo!("error")
        );
        todo!("file end")
    }
}