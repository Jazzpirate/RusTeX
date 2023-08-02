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
use std::sync::RwLock;
use kpathsea::Kpathsea;
use crate::engine::EngineType;
use crate::engine::filesystem::kpathsea::KpseResult;
use crate::engine::mouth::string_source::{StringSource, StringSourceState};
use crate::engine::state::State;
use crate::tex::catcodes::{CategoryCode, CategoryCodeScheme};
use crate::tex::token::{BaseToken, Token};
use crate::utils::errors::{OtherError, TeXError};
use crate::utils::map::HMap;
use crate::utils::Ptr;
use crate::utils::strings::CharType;

pub trait File<Char:CharType>:Clone {
    fn path(&self) -> &PathBuf;
    fn exists(&self) -> bool;
    fn content_string(&self) -> Option<Vec<u8>>;
    fn open_out(&self);
    fn open_in(&self);
    fn close_out(&self);
    fn close_in(&self);
    fn write(&self,string:&str);
    fn eof<ET:EngineType<Char=Char>>(&self,state:&ET::State) -> bool;
    fn read<T:Token<Char=Char>>(&self,cc:&CategoryCodeScheme<Char>,endlinechar:Option<Char>) -> Result<Vec<T>,Box<dyn TeXError<T>>>;
}

pub trait FileSystem<Char:CharType>:Clone + 'static {
    type F:File<Char>;
    fn new(pwd:PathBuf) -> Self;
    fn get(&mut self,path:&str) -> Self::F;
    fn set_pwd(&mut self, pwd:PathBuf) -> PathBuf;
}

pub struct PhysicalFile<Char:CharType> {path:PathBuf,contents:Option<Vec<u8>>,phantom:PhantomData<Char>}
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
    fn path(&self) -> &PathBuf { &self.path }
    fn exists(&self) -> bool { self.path.exists() }
    fn content_string(&self) -> Option<Vec<u8>> {
        self.contents.clone()
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
    fn open_in(&self) {
        todo!("Physical file system not implemented yet")
    }
    fn write(&self,_:&str) {
        todo!("Physical file system not implemented yet")
    }
    fn read<T:Token<Char=Char>>(&self,cc:&CategoryCodeScheme<Char>,endlinechar:Option<Char>) -> Result<Vec<T>,Box<dyn TeXError<T>>> {
        todo!("Physical file system not implemented yet")
    }
}


pub struct VirtualFile<Char:CharType> {path:PathBuf,contents:RwLock<Option<Vec<u8>>>,open:RwLock<Option<StringSource<Char>>>}
impl<Char:CharType> File<Char> for Ptr<VirtualFile<Char>> {
    fn path(&self) -> &PathBuf { &self.path }
    fn exists(&self) -> bool { self.contents.read().unwrap().is_some() }
    fn content_string(&self) -> Option<Vec<u8>> {
        match &*self.contents.read().unwrap() {
            Some(s) => Some(s.clone()),
            None => None
        }
    }
    fn close_out(&self) {}
    fn open_out(&self) {
        let mut w = self.contents.write().unwrap();
        *w = Some(vec!());
    }
    fn open_in(&self) {
        let w = self.contents.read().unwrap();
        let mut open = self.open.write().unwrap();
        let (v,eof) = match &*w {
            None => (vec!(),true),
            Some(v) => (v.clone(),false)
        };
        let mut ss = StringSource::new(v,Some(Ptr::new(self.path.to_str().unwrap().to_string())));
        ss.state.eof = eof;
        *open = Some(ss);
    }
    fn close_in(&self) {
        let mut open = self.open.write().unwrap();
        *open = None;
    }
    fn eof<ET:EngineType<Char=Char>>(&self,state:&ET::State) -> bool {
        let mut open = self.open.write().unwrap();
        open.as_mut().unwrap().eof::<ET>(state)//.peek().is_none()
    }
    fn read<T:Token<Char=Char>>(&self,cc:&CategoryCodeScheme<Char>,endlinechar:Option<Char>) -> Result<Vec<T>,Box<dyn TeXError<T>>> {
        let mut open = self.open.write().unwrap();
        match &mut *open {
            None => Err(OtherError { msg: "File not open".to_string(), source: None, cause: None }.into()),
            Some(m) => {
                let mut ret: Vec<T> = vec!();
                let mut ingroups = 0;
                loop {
                    m.read(cc,endlinechar,&mut |tk:T| match tk.base() {
                        BaseToken::Char(c,CategoryCode::BeginGroup) => {ingroups += 1; ret.push(tk)},
                        BaseToken::Char(c,CategoryCode::EndGroup) => {ingroups += 1; ret.push(tk)},
                        _ => ret.push(tk)
                    })?;
                    if ingroups == 0 {return Ok(ret)}
                }
            }
        }

    }

    fn write(&self, string: &str) {
        let mut write = self.contents.write().unwrap();
        let mut v = write.as_mut().unwrap();
        v.extend(string.as_bytes());
        v.push(b'\n');
    }
}

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

#[derive(Clone)]
pub struct KpseVirtualFileSystem<Char:CharType> {pwd:PathBuf,kpathsea:Kpathsea,files:HMap<PathBuf,Ptr<VirtualFile<Char>>>}
impl<Char:CharType> KpsePhysicalFileSystem<Char> {
    pub fn kpsewhich(&self, path: &str) -> KpseResult {
        self.kpathsea.kpsewhich(path)
    }
}
impl<Char:CharType> KpseVirtualFileSystem<Char> {
    pub fn kpsewhich(&self, path: &str) -> KpseResult {
        self.kpathsea.kpsewhich(path)
    }
}
impl<Char:CharType> FileSystem<Char> for KpseVirtualFileSystem<Char> {
    type F = Ptr<VirtualFile<Char>>;
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
                let ret = Ptr::new(VirtualFile{
                    path:e.key().clone(),
                    contents:RwLock::new(s),
                    open:RwLock::new(None)
                });
                e.insert(ret.clone());
                ret
            }
        }
    }
}