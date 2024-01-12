use std::path::PathBuf;
use tex_engine::engine::{EngineAux, EngineTypes};
use tex_engine::engine::filesystem::{File, FileSystem, NoOutputFileSystem, VirtualFile};
use tex_engine::tex::catcodes::CategoryCodeScheme;
use tex_engine::tex::characters::{StringLineSource, TextLine};
use tex_engine::tex::tokens::Token;
use tex_engine::utils::errors::ErrorHandler;
use tex_engine::utils::Ptr;
use tex_engine::prelude::CSName;

static PGFSYS: &str = include_str!("resources/pgfsys.def");

#[derive(Clone)]
pub struct RusTeXFileSystem{
    pub(crate) inner: NoOutputFileSystem<u8>,
    pub(crate) svg: (<VirtualFile<u8> as File>::SourceRefID,Ptr<[TextLine<u8>]>)
}


impl FileSystem for RusTeXFileSystem {
    type File = VirtualFile<u8>;
    #[inline(always)]
    fn new(pwd: PathBuf) -> Self {
        let mut inner = tex_engine::engine::filesystem::NoOutputFileSystem::new(pwd);
        let id = inner.interner.get_or_intern("<TEXINPUTS>/pgfsys-rustex.def");
        Self{
            inner,
            svg:(id,StringLineSource::make_lines(PGFSYS.as_bytes().iter().copied()).into())
        }
    }
    fn ref_str<'a>(&'a self, id: <Self::File as File>::SourceRefID) -> &'a str {
        self.inner.ref_str(id)
    }

    fn get<S:AsRef<str>>(&mut self,path:S) -> Self::File {
        use tex_engine::tex::characters::*;
        let sr = path.as_ref();
        if sr.ends_with("pgfsys-rustex.def") {
            VirtualFile {
                path:self.inner.kpse.pwd.join("pgfsys-rustex.def"),
                id:self.svg.0,pipe:false,exists:true,
                source:Some(self.svg.1.clone())
            }
        } else {
            self.inner.get(sr)
        }
    }
    #[inline(always)]
    fn set_pwd(&mut self, pwd:PathBuf) -> PathBuf {
        self.inner.set_pwd(pwd)
    }

    #[inline(always)]
    fn open_out(&mut self,idx:u8,file:Self::File) {
        self.inner.open_out(idx,file)
    }
    #[inline(always)]
    fn open_in(&mut self,idx:u8,file:Self::File) {
        self.inner.open_in(idx,file)
    }
    #[inline(always)]
    fn close_in(&mut self,idx:u8) {
        self.inner.close_in(idx)
    }
    #[inline(always)]
    fn close_out(&mut self,idx:u8) {
        self.inner.close_out(idx)
    }
    #[inline(always)]
    fn eof(&self,idx:u8) -> bool {
        self.inner.eof(idx)
    }
    #[inline(always)]
    fn write<ET:EngineTypes,D:std::fmt::Display>(&mut self,idx:i64,string:D,newlinechar:Option<ET::Char>,aux:&mut EngineAux<ET>) {
        self.inner.write(idx,string,newlinechar,aux)
    }
    #[inline(always)]
    fn read<T:Token<Char=<Self::File as File>::Char>,E:ErrorHandler,F:FnMut(T)>(&mut self,
                                                                                idx:u8, eh:&E,
                                                                                handler:&mut <T::CS as CSName<T::Char>>::Handler,
                                                                                cc:&CategoryCodeScheme<<Self::File as File>::Char>, endline:Option<<Self::File as File>::Char>, cont:F
    ) {
        self.inner.read(idx,eh,handler,cc,endline,cont)
    }
    fn readline<T:Token<Char=<Self::File as File>::Char>,F:FnMut(T)>(&mut self, idx:u8,cont:F) {
        self.inner.readline(idx,cont)
    }
}
