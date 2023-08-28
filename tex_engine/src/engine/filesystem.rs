/*! A TeX [`Engine`](crate::engine::Engine) needs to read and write files, and find them
    in the using simple file names relative to e.g. the `TEXMF` tree.

    This module provides [`FileSystem`](FileSystem) traits and implementations for
    [`PhysicalFile`](PhysicalFile) and [`VirtualFile`](VirtualFile) types. The latter
    do not actually modify the local file system.

    For retrieval, a bare bones implementation of [`Kpathsea`](Kpathsea) is provided.
*/

pub mod kpathsea;

use std::collections::hash_map::Entry;
use std::marker::PhantomData;
use std::path::PathBuf;
use kpathsea::Kpathsea;
use crate::engine::EngineType;
use crate::engine::filesystem::kpathsea::KpseResult;
use crate::engine::memory::{Interner, Memory};
use crate::engine::mouth::string_source::StringSource;
use crate::tex::catcodes::{CategoryCode, CategoryCodeScheme};
use crate::tex::token::{BaseToken, Token};
use crate::throw;
use crate::utils::errors::TeXError;
use crate::utils::collections::HMap;
use crate::utils::{Mut, Ptr};
use crate::utils::strings::CharType;

pub trait File<Char:CharType>:Clone {
    fn path(&self) -> &PathBuf;
    fn exists(&self) -> bool;
    fn content_string(&self) -> Option<Ptr<[Box<[u8]>]>>;
    fn open_out(&self);
    fn open_in(&self,interner:&mut Interner<Char>);
    fn close_out(&self);
    fn close_in(&self);
    fn write(&self,string:&str);
    fn eof<ET:EngineType<Char=Char>>(&self,state:&ET::State) -> bool;
    fn read<ET:EngineType<Char=Char>,F:FnMut(Token<ET>)>(&self,interner:&mut Interner<Char>,cc:&CategoryCodeScheme<Char>,endlinechar:Option<Char>,f:F);
    fn readline<ET:EngineType<Char=Char>,F:FnMut(Token<ET>)>(&self,interner:&mut Interner<Char>,f:F);
}

pub trait FileSystem<Char:CharType>:Clone + 'static {
    type F:File<Char>;
    fn new(pwd:PathBuf) -> Self;
    fn get(&mut self,path:&str) -> Self::F;
    fn set_pwd(&mut self, pwd:PathBuf) -> PathBuf;
}
/*
pub struct PhysicalFile<Char:CharType> {path:PathBuf,contents:Ptr<[Box<[u8]>]>,phantom:PhantomData<Char>}
impl<Char:CharType> PhysicalFile<Char> {
    pub fn new(path:PathBuf) -> Self {
        PhysicalFile {
            contents: {
                if path.exists() {
                    Some(std::fs::read(&path).ok().unwrap_or(vec!()))
                } else {
                    None
                }
            },
            phantom: PhantomData,
            path
        }
    }
}
impl<Char:CharType> File<Char> for Ptr<PhysicalFile<Char>> {
    type OptionRef<'a> = &'a Option<Vec<u8>>;
    fn path(&self) -> &PathBuf { &self.path }
    fn exists(&self) -> bool { self.path.exists() }
    fn content_string(&self) -> Self::OptionRef<'_> {
        &self.contents
    }
    fn open_out(&self) {
        todo!("Physical file system not implemented yet")
    }
    fn close_in(&self) {
        todo!("Physical file system not implemented yet")
    }
    fn close_out(&self) {
        todo!("Physical file system not implemented yet")
    }
    fn eof<ET:EngineType<Char=Char>>(&self,state:&ET::State) -> bool {
        todo!("Physical file system not implemented yet")
    }
    fn open_in(&self,interner:&mut Interner<Char>) {
        todo!("Physical file system not implemented yet")
    }
    fn write(&self,_:&str) {
        todo!("Physical file system not implemented yet")
    }
    fn read<ET:EngineType<Char=Char>,F:FnMut(Token<ET>)>(&self,interner:&mut Interner<Char>,cc:&CategoryCodeScheme<Char>,endlinechar:Option<Char>,f:F) -> Result<(),TeXError<ET>> {
        todo!("Physical file system not implemented yet")
    }
}
*/

enum FileState<Char:CharType> {
    OpenIn(StringSource<Char>),
    OpenOut(Vec<Box<[u8]>>),
    Closed(Option<Ptr<[Box<[u8]>]>>)
}
struct VirtualFileI<Char:CharType> {path:PathBuf,state:Mut<FileState<Char>>}
#[derive(Clone)]
pub struct VirtualFile<Char:CharType>(Ptr<VirtualFileI<Char>>);
impl<Char:CharType> File<Char> for VirtualFile<Char> {
    fn path(&self) -> &PathBuf { &self.0.path }
    fn exists(&self) -> bool { match &*self.0.state.borrow() {
        FileState::Closed(o) => o.is_some(),
        _ => true
    } }
    fn content_string(&self) -> Option<Ptr<[Box<[u8]>]>> {
        match &*self.0.state.borrow() {
            FileState::Closed(v@None) => v.clone(),
            FileState::Closed(v) => v.clone(),
            FileState::OpenIn(ss) => Some(ss.string.clone()),
            _ => None
        }
    }
    fn close_out(&self) {
        let s = &mut *self.0.state.borrow_mut();
        match s {
            FileState::OpenOut(v) => *s = FileState::Closed(Some((std::mem::take(v)).into())),
            _ => unreachable!()
        }
    }
    fn open_out(&self) {
        (*self.0.state.borrow_mut()) = FileState::OpenOut(vec!());
    }
    fn open_in(&self,interner:&mut Interner<Char>) {
        let s = &mut *self.0.state.borrow_mut();
        match s {
            FileState::Closed(Some(v)) => {
                let mut ss = StringSource::new(v.clone(),Some(interner.from_string(self.0.path.to_str().unwrap())));
                *s = FileState::OpenIn(ss);
            },
            FileState::Closed(None) => (),
            _ => unreachable!()
        }
    }
    fn close_in(&self) {
        let s = &mut *self.0.state.borrow_mut();
        match s {
            FileState::OpenIn(ss) => *s = FileState::Closed(Some(ss.string.clone())),
            FileState::Closed(_) => (),
            _ => unreachable!()
        }
    }
    fn eof<ET:EngineType<Char=Char>>(&self,state:&ET::State) -> bool {
        match &mut *self.0.state.borrow_mut() {
            FileState::OpenIn(ss) => ss.eof::<ET>(state),
            FileState::Closed(_) => true,
            _ => unreachable!()
        }
    }
    fn read<ET:EngineType<Char=Char>,F:FnMut(Token<ET>)>(&self,interner:&mut Interner<Char>,cc:&CategoryCodeScheme<Char>,endlinechar:Option<Char>,mut f:F) {
        let s = &mut *self.0.state.borrow_mut();
        match s {
            FileState::OpenIn(ss) => ss.read(interner,cc,endlinechar,f),
            FileState::Closed(_) => (),
            _ => unreachable!()
        }
    }

    fn readline<ET:EngineType<Char=Char>,F:FnMut(Token<ET>)>(&self,interner:&mut Interner<Char>,mut f:F) {
        let s = &mut *self.0.state.borrow_mut();
        match s {
            FileState::OpenIn(ss) => ss.readline(interner,f),
            FileState::Closed(_) => (),
            _ => unreachable!()
        }
    }

    fn write(&self, string: &str) {
        let s = &mut *self.0.state.borrow_mut();
        match s {
            FileState::OpenOut(v) => {
                for s in string.split('\n') {
                    v.push(s.as_bytes().into())
                }
            },
            _ => unreachable!()
        }
    }
}
/*
#[derive(Clone)]
pub struct KpsePhysicalFileSystem<Char:CharType> {kpathsea:Kpathsea,phantom:PhantomData<Char>}
impl<Char:CharType> FileSystem<Char> for KpsePhysicalFileSystem<Char> {
    type F = Ptr<PhysicalFile<Char>>;
    fn new(pwd:PathBuf) -> Self { KpsePhysicalFileSystem {kpathsea:Kpathsea::new(pwd),phantom:PhantomData} }
    fn get(&mut self, _path: &str) -> Self::F {
        todo!("Physical file system not implemented yet")
    }
    fn set_pwd(&mut self, _pwd: PathBuf) -> PathBuf {
        todo!("Physical file system not implemented yet")
    }
}
impl<Char:CharType> KpsePhysicalFileSystem<Char> {
    pub fn kpsewhich(&self, path: &str) -> KpseResult {
        self.kpathsea.kpsewhich(path)
    }
}

 */

#[derive(Clone)]
pub struct KpseVirtualFileSystem<Char:CharType> {pwd:PathBuf,kpathsea:Kpathsea,files:HMap<PathBuf,VirtualFile<Char>>}

impl<Char:CharType> KpseVirtualFileSystem<Char> {
    pub fn kpsewhich(&self, path: &str) -> KpseResult {
        self.kpathsea.kpsewhich(path)
    }
}
impl<Char:CharType> FileSystem<Char> for KpseVirtualFileSystem<Char> {
    type F = VirtualFile<Char>;
    fn new(pwd:PathBuf) -> Self { KpseVirtualFileSystem {
        pwd:pwd.clone(),
        kpathsea:Kpathsea::new(pwd),
        files:HMap::default()
    } }
    fn set_pwd(&mut self, pwd: PathBuf) -> PathBuf {
        let old = std::mem::replace(&mut self.kpathsea, Kpathsea::new(pwd));
        old.pwd
    }
    fn get(&mut self, path: &str) -> Self::F {
        let ret = self.kpathsea.kpsewhich(path);
        match self.files.entry(ret.path) {
            Entry::Occupied(e) => e.get().clone(),
            Entry::Vacant(e) => {
                let s: Option<Vec<u8>> = self.kpathsea.get(&self.pwd,e.key());
                let ret = VirtualFile(Ptr::new(VirtualFileI{
                    path:e.key().clone(),
                    state:Mut::new(FileState::Closed(s.map(|s|StringSource::<Char>::from_str(&s))))
                }));
                e.insert(ret.clone());
                ret
            }
        }
    }
}