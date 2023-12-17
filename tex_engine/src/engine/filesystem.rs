/*! Accessing files on `\input`, `\open`/`\write` etc. */

use std::path::{Path, PathBuf};
use crate::debug_log;
use crate::engine::{EngineAux, EngineTypes};
use crate::engine::filesystem::kpathsea::Kpathsea;
use crate::engine::mouth::strings::StringTokenizer;
use crate::engine::utils::outputs::Outputs;
use crate::tex::catcodes::CategoryCodeScheme;
use crate::tex::control_sequences::ControlSequenceName;
use crate::tex::input_text::{Character, TextLine, TextLineSource};
use crate::tex::token::Token;
use crate::utils::{HMap, Ptr};
use crate::utils::errors::ErrorHandler;

pub mod kpathsea;

#[derive(Debug,Copy,Clone)]
pub struct SourceReference<FileId:Copy> {
    pub file: FileId,
    pub line: usize,
    pub column: usize
}

/// A [`FileSystem`] provides access to files.
pub trait FileSystem:Clone {
    /// The type of files provided by this [`FileSystem`].
    type File:File;
    /// Creates a new [`FileSystem`] with the given working directory.
    fn new(pwd:PathBuf) -> Self;
    /// Returns the file with the given name in the file database.
    /// May return nonexistent files in the CWD
    fn get<S:AsRef<str>>(&mut self,path:S) -> Self::File;
    /// Sets the working directory of this [`FileSystem`], returning the old working directory
    /// and updating the file database.
    fn set_pwd(&mut self, pwd:PathBuf) -> PathBuf;

    fn open_out(&mut self,idx:u8,file:Self::File);
    fn open_in(&mut self,idx:u8,file:Self::File);
    fn close_in(&mut self,idx:u8);
    fn close_out(&mut self,idx:u8);
    fn eof(&self,idx:u8) -> bool;
    fn write<ET:EngineTypes,D:std::fmt::Display>(&mut self,idx:i64,string:D,newlinechar:Option<ET::Char>,aux:&mut EngineAux<ET>);
    fn read<T:Token<Char=<Self::File as File>::Char>,E:ErrorHandler,F:FnMut(T)>(&mut self,
                                                  idx:u8,eh:&E,
                                                  handler:&mut <T::CS as ControlSequenceName<T::Char>>::Handler,
                                                  cc:&CategoryCodeScheme<<Self::File as File>::Char>,endline:Option<<Self::File as File>::Char>,cont:F
    );
    fn readline<T:Token<Char=<Self::File as File>::Char>,F:FnMut(T)>(&mut self, idx:u8,cont:F);
}

/// A (virtual or physical) file.
pub trait File:std::fmt::Display+Clone+std::fmt::Debug + 'static {
    /// The type of characters to be read from the file.
    type Char:Character;
    type SourceRefID:Copy+std::fmt::Debug;
    /// The type of line sources to be read from the file.
    type LineSource: FileLineSource<Self::Char>;
    //type Write: WriteOpenFile<Self::Char>;
    fn path(&self) -> &Path;
    fn line_source(self) -> Option<Self::LineSource>;
    //fn write(self) -> Self::Write;
    #[inline(always)]
    fn exists(&self) -> bool {
        self.path().exists()
    }
    fn size(&self) -> usize;
    fn sourceref(&self) -> Self::SourceRefID;
    fn md5(&self) -> md5::Digest;
}

pub trait FileLineSource<C:Character>:TextLineSource<C> {
    fn path(&self) -> &Path;
}

/// A [`FileSystem`] that does not write to the local physical file system.
/// If a file is modified, its contents are kept in memory.
///
pub struct NoOutputFileSystem<C:Character> {
    pub kpse:Kpathsea,
    files:HMap<PathBuf,VirtualFile<C>>,
    write_files:Vec<Option<WritableVirtualFile<C>>>,
    read_files:Vec<Option<StringTokenizer<C,VirtualFileLineSource<C>>>>,
    pub interner:string_interner::StringInterner<string_interner::DefaultBackend<string_interner::symbol::SymbolU32>,ahash::RandomState>
}
impl<C:Character> Clone for NoOutputFileSystem<C> {
    fn clone(&self) -> Self { Self {
        kpse:self.kpse.clone(),
        files:self.files.clone(),
        write_files:self.write_files.clone(),
        read_files:Vec::new(),
        interner:self.interner.clone()
    } }
}
impl<C:Character> FileSystem for NoOutputFileSystem<C> {
    type File = VirtualFile<C>;
    fn new(pwd:PathBuf) -> Self {
        Self {
            //phantom:PhantomData::default(),
            kpse:Kpathsea::new(pwd),
            files:HMap::default(),
            write_files:Vec::new(),
            read_files:Vec::new(),
            interner:string_interner::StringInterner::new()
        }
    }
    fn get<S:AsRef<str>>(&mut self,path:S) -> Self::File {
        let path = self.kpse.kpsewhich(path);
        match self.files.get(&path.path) {
            Some(f) => f.clone(),
            None => {
                let string = if path.path.starts_with(&self.kpse.pwd) {
                    format!("./{}",path.path.strip_prefix(&self.kpse.pwd).unwrap().display())
                } else if self.kpse.global.set.iter().any(|p| path.path.starts_with(p)) {
                    format!("<TEXINPUTS>/{}",path.path.file_name().unwrap().to_str().unwrap())
                } else {
                    path.path.display().to_string()
                };
                let f = VirtualFile {
                    path:path.path,
                    source:None,
                    id:self.interner.get_or_intern(string)
                };
                self.files.insert(f.path.clone(),f.clone());
                f
            }
        }
    }
    fn set_pwd(&mut self, pwd:PathBuf) -> PathBuf {
        let old = std::mem::replace(&mut self.kpse, Kpathsea::new(pwd));
        old.pwd
    }
    fn open_in(&mut self, idx: u8, file: Self::File) {
        while self.read_files.len() <= idx as usize {
            self.read_files.push(None);
        }
        match self.read_files.get_mut(idx as usize) {
            Some(n) =>
                *n = match file.line_source() {
                    Some(src) => Some(StringTokenizer::new( src)),
                    _ => None
                },
            _ => unreachable!()
        }
    }
    fn read<T:Token<Char=C>,E:ErrorHandler,F:FnMut(T)>(&mut self,
                                                  idx:u8,eh:&E,
                                                  handler:&mut <T::CS as ControlSequenceName<C>>::Handler,
        cc:&CategoryCodeScheme<C>,endline:Option<C>,cont:F
    ) {
        match self.read_files.get_mut(idx as usize) {
            Some(Some(f)) => {
                f.read(eh,handler,cc,endline,cont);
            }
            _ => todo!("throw File not open error")
        }
    }
    fn readline<T:Token<Char=C>,F:FnMut(T)>(&mut self, idx:u8,cont:F) {
        match self.read_files.get_mut(idx as usize) {
            Some(Some(f)) => {
                //debug_log!(debug => "readline: {}",f.source.path.display());
                f.readline(cont);
            }
            _ => todo!("throw File not open error")
        }
    }
    fn eof(&self,idx:u8) -> bool {
        match self.read_files.get(idx as usize) {
            Some(Some(f)) => f.eof(),
            _ => true,
        }
    }

    fn close_in(&mut self,idx:u8) {
        if let Some(f) = self.read_files.get_mut(idx as usize) {
            *f = None;
        }
    }
    fn open_out(&mut self, idx: u8, file: Self::File) {
        if idx as usize >= self.write_files.len() {
            self.write_files.resize((idx+1) as usize, None);
        }
        match &mut self.write_files[idx as usize] {
            n@None => *n = Some(WritableVirtualFile::new(file.path,file.id)),
            _ => todo!("throw File already open error")
        }
    }
    fn close_out(&mut self,idx:u8) {
        match self.write_files.get_mut(idx as usize) {
            Some(o) => match std::mem::take(o) {
                Some(f) => {
                    let vf = VirtualFile {
                        path:f.1,
                        source:Some(f.0.into()),id:f.2

                    };
                    self.files.insert(vf.path.clone(),vf);
                }
                _ => ()
            }
            _ => ()
        }
    }
    fn write<ET:EngineTypes,D:std::fmt::Display>(&mut self,idx:i64,string:D,newlinechar:Option<ET::Char>,aux:&mut EngineAux<ET>) {
        if idx < 0 {
            aux.outputs.write_neg1(string)
        } else if idx == 16 {
            aux.outputs.write_16(string)
        } else if idx == 17 {
            aux.outputs.write_17(string)
        } else if idx == 18 {
            aux.outputs.write_18(string)
        } else {
            match self.write_files.get_mut(idx as usize) {
                Some(Some(f)) => {
                    let s = string.to_string().into_bytes();
                    match newlinechar {
                        Some(c) =>
                        match c.try_into() {
                            Ok(u) => {
                                for l in s.split(|b| *b == u) {
                                    f.0.push(C::convert(l.to_vec()));
                                }
                                return
                            }
                            _ => ()
                        }
                        _ => ()
                    }
                    let tl = C::convert(s);
                    f.0.push(tl);
                }
                _ => aux.outputs.write_other(string)
            }
        }
    }
}

#[derive(Clone)]
struct WritableVirtualFile<C:Character>(Vec<Box<[C]>>, PathBuf,string_interner::symbol::SymbolU32);
impl<C:Character> WritableVirtualFile<C> {
    #[inline(always)]
    fn new(p:PathBuf,id:string_interner::symbol::SymbolU32) -> Self {
        Self(Vec::new(),p,id)
    }
}

type VirtualFileContents<C> = Ptr<[TextLine<C>]>;

#[derive(Debug)]
enum VirtualOrPhysicalFile<C:Character> {
    Virtual(VirtualFileContents<C>,usize),
    Physical(std::io::Split<std::io::BufReader<std::fs::File>>)
}
#[derive(Debug)]
pub struct VirtualFileLineSource<C:Character> {
    path:PathBuf,
    source:VirtualOrPhysicalFile<C>
}
impl <C:Character> FileLineSource<C> for VirtualFileLineSource<C> {
    #[inline(always)]
    fn path(&self) -> &Path { &self.path }
}
impl<C:Character> TextLineSource<C> for VirtualFileLineSource<C> {
    fn get_line(&mut self) -> Option<TextLine<C>> {
        match &mut self.source {
            VirtualOrPhysicalFile::Virtual(v,i) => {
                if *i >= v.len() {
                    None
                } else {
                    let ret = v[*i].clone();
                    *i += 1;
                    Some(ret)
                }
            }
            VirtualOrPhysicalFile::Physical(f) => {
                match f.next() {
                    Some(Ok(mut s)) => {
                        if let Some(b'\r') = s.last() {
                            s.pop();
                        }
                        while let Some(b' ') = s.last() {
                            s.pop();
                            if s.last() == Some(&b'\\') {
                                s.push(b' ');
                                break;
                            }
                        }
                        Some(C::convert(s))
                    }
                    _ => None
                }
            }
        }
    }
}

#[derive(Clone,Debug)]
pub struct VirtualFile<C:Character> {
    pub path:PathBuf,
    pub id:string_interner::symbol::SymbolU32,
    pub source:Option<VirtualFileContents<C>>
}
impl<C:Character> std::fmt::Display for VirtualFile<C> {
    #[inline(always)]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.path.display())
    }
}
impl<C:Character> File for VirtualFile<C> {
    type Char = C;
    type LineSource = VirtualFileLineSource<C>;
    type SourceRefID = string_interner::symbol::SymbolU32;
    #[inline(always)]
    fn exists(&self) -> bool {
        self.source.is_some() || self.path().exists()
    }
    #[inline(always)]
    fn sourceref(&self) -> Self::SourceRefID { self.id }
    #[inline(always)]
    fn path(&self) -> &Path { &self.path }
    fn md5(&self) -> md5::Digest {
        match &self.source {
            None => match std::fs::read(&self.path) {
                Ok(v) => md5::compute(v),
                Err(_) => md5::compute("")
            }
            Some(s) => {
                let v : Vec<u8> = s.iter().map(|v| v.iter().map(|c| c.to_char().to_string().into_bytes())).flatten().flatten().collect();
                md5::compute(v)
            }
        }
    }
    fn line_source(self) -> Option<Self::LineSource> {
        use std::io::BufRead;
        match self.source {
            Some(src) => Some(VirtualFileLineSource {
                path:self.path,
                source:VirtualOrPhysicalFile::Virtual(src,0)
            }),
            None => {
                let f = std::fs::File::open(&self.path).ok()?;
                let f = std::io::BufReader::new(f);
                let f = f.split(b'\n');
                Some(VirtualFileLineSource {
                    path:self.path,
                    source:VirtualOrPhysicalFile::Physical(f)
                })
            }
        }
    }
    fn size(&self) -> usize {
        match &self.source {
            Some(src) => {
                let cnt = src.iter().count() + src.len();
                if cnt == 0 { 0 } else { cnt - 1 }
            }
            None => {
                match std::fs::metadata(&self.path) {
                    Ok(md) => md.len() as usize,
                    _ => 0
                }

            }
        }
    }
}

/*

pub trait FileLineSource<C:Character>:TextLineSource<C> {
    fn path(&self) -> &Path;
}

pub struct ReadOpenPhysicalFile<C:Character> {
    path:PathBuf,
    reader:std::io::Split<std::io::BufReader<std::fs::File>>,
    phantom:PhantomData<C>
}
impl<C:Character> FileLineSource<C> for ReadOpenPhysicalFile<C> {
    #[inline(always)]
    fn path(&self) -> &Path { &self.path }
}
impl<C:Character> TextLineSource<C> for ReadOpenPhysicalFile<C> {
    fn get_line(&mut self) -> Option<TextLine<C>> {
        match self.reader.next() {
            Some(Ok(mut s)) => {
                if let Some(b'\r') = s.last() {
                    s.pop();
                }
                while let Some(b' ') = s.last() {
                    s.pop();
                }
                Some(C::convert(s))
            }
            _ => None
        }
    }
}


pub trait WriteOpenFile<C:Character> {
    //fn write_line<I:Iterator<Item = C>>(&mut self,chars:&mut I);
}




impl<C:Character> File for VirtualFile<C> {
    type Char = C;
    type LineSource = ReadOpenVirtualFile<C>;
    type Write = WriteOpenVirtualFile<C>;
    fn path(&self) -> &Path { &self.path }
    fn line_source(self) -> Option<Self::LineSource> {
        ReadOpenVirtualFile {
            path:self.path,
            source:match self.source {
                Some(src) => ROVF::Memory((*src).clone()),
                None => ROVF::Physical()
            }
        }
        match self.source {
        }
    }
    fn write(self) -> Self::Write { todo!() }
}

pub struct ReadOpenVirtualFile<C:Character> {
    path:PathBuf,
    source:ROVF<C>
}
impl<C:Character> FileLineSource<C> for ReadOpenVirtualFile<C> {
    #[inline(always)]
    fn path(&self) -> &Path { &self.path }
}
impl<C:Character> TextLineSource<C> for ReadOpenVirtualFile<C> {
    #[inline(always)]
    fn get_line(&mut self) -> Option<Box<[C]>> { match self.source {
        ROVF::Physical(ref mut f) => f.get_line(),
        ROVF::Memory(ref mut s) => s.get_line()
    } }
}

enum ROVF<C:Character> {
    Physical(ReadOpenPhysicalFile<C>),
    Memory(StringLineSource)
}

pub struct WriteOpenVirtualFile<C:Character> {
    phantom:PhantomData<C>
}

impl<C:Character> WriteOpenFile<C> for WriteOpenVirtualFile<C> {
}

*/