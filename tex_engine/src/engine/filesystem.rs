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
use crate::engine::memory::Memory;
use crate::engine::mouth::string_source::StringSource;
use crate::tex::catcodes::{CategoryCode, CategoryCodeScheme};
use crate::tex::token::{BaseToken, Token};
use crate::throw;
use crate::utils::errors::TeXError;
use crate::utils::map::HMap;
use crate::utils::{Mut, Ptr};
use crate::utils::strings::CharType;

pub trait File<Char:CharType>:Clone {
    type OptionRef<'a>: std::ops::Deref<Target=Option<Vec<u8>>> where Self:'a;
    fn path(&self) -> &PathBuf;
    fn exists(&self) -> bool;
    fn content_string(&self) -> Self::OptionRef<'_>;
    fn open_out(&self);
    fn open_in<ET:EngineType<Char=Char>>(&self,memory:&mut Memory<ET>);
    fn close_out(&self);
    fn close_in(&self);
    fn write(&self,string:&str);
    fn eof<ET:EngineType<Char=Char>>(&self,state:&ET::State) -> bool;
    fn read<ET:EngineType<Char=Char>,F:FnMut(Token<ET>)>(&self,memory:&mut Memory<ET>,cc:&CategoryCodeScheme<Char>,endlinechar:Option<Char>,f:F) -> Result<(),TeXError<ET>>;
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
    fn open_in<ET:EngineType<Char=Char>>(&self,memory:&mut Memory<ET>) {
        todo!("Physical file system not implemented yet")
    }
    fn write(&self,_:&str) {
        todo!("Physical file system not implemented yet")
    }
    fn read<ET:EngineType<Char=Char>,F:FnMut(Token<ET>)>(&self,memory:&mut Memory<ET>,cc:&CategoryCodeScheme<Char>,endlinechar:Option<Char>,f:F) -> Result<(),TeXError<ET>> {
        todo!("Physical file system not implemented yet")
    }
}


pub struct VirtualFile<Char:CharType> {path:PathBuf,contents:Mut<Option<Vec<u8>>>,open:Mut<Option<StringSource<Char>>>}
impl<Char:CharType> File<Char> for Ptr<VirtualFile<Char>> {
    type OptionRef<'a> = core::cell::Ref<'a,Option<Vec<u8>>>;
    fn path(&self) -> &PathBuf { &self.path }
    fn exists(&self) -> bool { self.contents.borrow().is_some() }
    fn content_string(&self) -> Self::OptionRef<'_> {
        self.contents.borrow()
    }
    fn close_out(&self) {}
    fn open_out(&self) {
        let mut w = self.contents.borrow_mut();
        *w = Some(vec!());
    }
    fn open_in<ET:EngineType<Char=Char>>(&self,memory:&mut Memory<ET>) {
        let w = &*self.contents.borrow();
        let mut open = self.open.borrow_mut();
        let (v,eof) = match w {
            None => (vec!(),true),
            Some(v) => (v.clone(),false)
        };
        let mut ss = StringSource::new(v,Some(memory.interner.get_or_intern(self.path.to_str().unwrap().to_string())));
        ss.state.eof = eof;
        *open = Some(ss);
    }
    fn close_in(&self) {
        let mut open = self.open.borrow_mut();
        *open = None;
    }
    fn eof<ET:EngineType<Char=Char>>(&self,state:&ET::State) -> bool {
        let open = &mut *self.open.borrow_mut();
        let open = open.as_mut().unwrap();
        open.eof::<ET>(state)//.peek().is_none()
    }
    fn read<ET:EngineType<Char=Char>,F:FnMut(Token<ET>)>(&self,memory:&mut Memory<ET>,cc:&CategoryCodeScheme<Char>,endlinechar:Option<Char>,mut f:F) -> Result<(),TeXError<ET>> {
        let open = &mut *self.open.borrow_mut();
        match open {
            None => throw!("File not open"),
            Some(m) => {
                let mut ingroups = 0;
                loop {
                    m.read(memory,cc,endlinechar,|tk| match &tk.base {
                        BaseToken::Char(c,CategoryCode::BeginGroup) => {ingroups += 1; f(tk)},
                        BaseToken::Char(c,CategoryCode::EndGroup) => {ingroups += 1; f(tk)},
                        _ => f(tk)
                    })?;
                    if ingroups == 0 {return Ok(())}
                }
            }
        }

    }

    fn write(&self, string: &str) {
        let write = &mut *self.contents.borrow_mut();
        let v = write.as_mut().unwrap();
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
                    contents:Mut::new(s),
                    open:Mut::new(None)
                });
                e.insert(ret.clone());
                ret
            }
        }
    }
}