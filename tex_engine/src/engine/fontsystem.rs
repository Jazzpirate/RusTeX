mod tfm;

use std::path::PathBuf;
use std::sync::RwLock;
use crate::commands::Command;
use crate::engine::{EngineAux, EngineReferences, EngineTypes};
use crate::engine::filesystem::{File, FileSystem};
use crate::engine::fontsystem::tfm::TfmFile;
use crate::engine::gullet::ResolvedToken;
use crate::engine::utils::memory::MemoryManager;
use crate::tex::control_sequences::ControlSequenceName;
use crate::tex::input_text::Character;
use crate::tex::numerics::{Numeric, TeXDimen, TeXInt};
use crate::utils::{HMap, Ptr};
use crate::tex::control_sequences::ControlSequenceNameHandler;

pub trait FontSystem:Clone+std::fmt::Debug {
    type Char:Character;
    type CS:ControlSequenceName;
    type Int:TeXInt;
    type Font:Font<Char=Self::Char,CS=Self::CS,D=Self::Dim,Int=Self::Int>;
    type Dim:TeXDimen;
    fn null(&self) -> Self::Font;
    fn new_font<S:AsRef<str>,F:FileSystem>(&mut self,path:S,macroname:<Self::Font as Font>::CS,fs:&mut F) -> Self::Font;
}

pub trait Font:Clone+std::fmt::Debug {
    type Char:Character;
    type D:TeXDimen;
    type Int:TeXInt;
    type CS:ControlSequenceName;
    fn get_at(&self) -> Self::D;
    fn set_at(&mut self,d:Self::D);
    fn name(&self) -> &Self::CS;
    fn get_dim(&self,idx:u16) -> Self::D;
    fn set_dim(&mut self,idx:u16,d:Self::D);
    fn get_hyphenchar(&self) -> Self::Int;
    fn set_hyphenchar(&mut self,c:Self::Int);
    fn get_skewchar(&self) -> Self::Int;
    fn set_skewchar(&mut self,c:Self::Int);
    fn display<W:std::fmt::Write>(&self,i:&<Self::CS as ControlSequenceName>::Handler,w:W) -> std::fmt::Result;
}
#[derive(Clone,Debug)]
pub struct TfmFontSystem<I:TeXInt,D:TeXDimen + Numeric<I>,CS:ControlSequenceName> {
    files:HMap<PathBuf,Ptr<TfmFile>>,
    null:Ptr<TfmFontI<I,D,CS>>
}
impl<I:TeXInt,D:TeXDimen + Numeric<I>,CS:ControlSequenceName> TfmFontSystem<I,D,CS> {
    pub fn new<ET:EngineTypes<FontSystem=Self,CSName = CS>>(aux:&mut EngineAux<ET>) -> Self  {
        use crate::tex::control_sequences::ControlSequenceNameHandler;
        let null_file = TfmFile {
            hyphenchar:45,
            skewchar:255,
            dimen:vec!(),
            size:0,
            typestr:"nullfont".to_string(),
            widths:[0.0;256],
            heights:[0.0;256],
            depths:[0.0;256],
            ics:[0.0;256],
            ligs:HMap::default(),
            filepath:std::path::PathBuf::from("/nullfont")
        };
        let mut muts = Mutables::default();
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
}
impl<I:TeXInt,D:TeXDimen + Numeric<I>,CS:ControlSequenceName> FontSystem for TfmFontSystem<I,D,CS> {
    type Char = u8;
    type Int = I;
    type Font = TfmFont<I,D,CS>;
    type Dim = D;
    type CS=CS;

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
        let mut muts = Mutables::default();
        let mut font = TfmFontI {
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
    heights:HMap<u8,D>,
    depths:HMap<u8,D>,
    widths:HMap<u8,D>,
    lps:HMap<u8,D>,
    rps:HMap<u8,D>,
    ics:HMap<u8,D>,
    hyphenchar:Option<I>,
    skewchar:Option<I>,
    dimens:Vec<D>
}

pub struct TfmFontI<I:TeXInt,D:TeXDimen + Numeric<I>,CS:ControlSequenceName>  {
    file:Ptr<TfmFile>,
    name:CS,
    muts:RwLock<Mutables<I,D>>
}
impl<I:TeXInt,D:TeXDimen + Numeric<I>,CS:ControlSequenceName> std::fmt::Debug for TfmFontI<I,D,CS> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f,"Font {:?}",self.name)
    }

}

pub type TfmFont<I,D,CS> = Ptr<TfmFontI<I,D,CS>>;
impl<I:TeXInt,D:TeXDimen + Numeric<I>,CS:ControlSequenceName> Font for TfmFont<I,D,CS> {
    type Char = u8;
    type CS = CS;
    type Int = I;
    type D = D;
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
    fn get_at(&self) -> Self::D {
        let at = self.muts.read().unwrap().at;
        match at {
            Some(d) => d,
            None => D::from_sp(self.file.size as i32)
        }
    }
    fn get_dim(&self, idx: u16) -> Self::D {
        match self.muts.read().unwrap().dimens.get(idx as usize) {
            Some(d) => *d,
            None => match self.file.dimen.get(idx as usize) {
                Some(d) => self.get_at().scale_float(*d),
                None => D::default()
            }
        }
    }
    fn set_dim(&mut self, idx: u16, d: Self::D) {
        let v = &mut self.muts.write().unwrap().dimens;
        if idx as usize >= v.len() {
            v.resize(idx as usize + 1,D::default());
        }
        v[idx as usize] = d;
    }
    #[inline(always)]
    fn set_at(&mut self, d: Self::D) {
        self.muts.write().unwrap().at = Some(d);
    }
    #[inline(always)]
    fn name(&self) -> &Self::CS {
        &self.name
    }
    fn display<W:std::fmt::Write>(&self,i:&<Self::CS as ControlSequenceName>::Handler,mut w:W) -> std::fmt::Result {
        let at = self.muts.read().unwrap().at;
        match at {
            Some(d) => write!(w,"{} at {}",self.file.name(),d),
            None => write!(w,"{}",self.file.name())
        }
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