use std::path::PathBuf;
use std::rc::Rc;
use tex_engine::engine::{EngineAux, EngineTypes};
use tex_engine::engine::filesystem::{File, FileSystem, VirtualFile};
use tex_engine::tex::catcodes::CategoryCodeScheme;
use tex_engine::tex::control_sequences::ControlSequenceName;
use tex_engine::tex::input_text::{Character, StringLineSource};
use tex_engine::tex::token::Token;
use tex_engine::utils::errors::ErrorHandler;

static PGFSYS: &str = include_str!("resources/pgfsys.def");

#[derive(Clone)]
pub struct Files(tex_engine::engine::filesystem::NoOutputFileSystem<u8>);


impl FileSystem for Files {
    type File = tex_engine::engine::filesystem::VirtualFile<u8>;
    #[inline(always)]
    fn new(pwd: PathBuf) -> Self {
        Self(tex_engine::engine::filesystem::NoOutputFileSystem::new(pwd))
    }
    #[inline(always)]
    fn get<S:AsRef<str>>(&mut self,path:S) -> Self::File {
        use tex_engine::tex::input_text::*;
        let sr = path.as_ref();
        if sr.ends_with("pgfsys-rustex.def") {
            let id = self.0.interner.get_or_intern("<TEXINPUTS>/pgfsys-rustex.def");
            let src: StringLineSource<u8> = PGFSYS.into();
            VirtualFile {
                path:self.0.kpse.pwd.join("pgfsys-rustex.def"),
                id,
                source:Some(src.lines.into())
            }
        } else {
            self.0.get(sr)
        }
    }
    #[inline(always)]
    fn set_pwd(&mut self, pwd:PathBuf) -> PathBuf {
        self.0.set_pwd(pwd)
    }

    #[inline(always)]
    fn open_out(&mut self,idx:u8,file:Self::File) {
        self.0.open_out(idx,file)
    }
    #[inline(always)]
    fn open_in(&mut self,idx:u8,file:Self::File) {
        self.0.open_in(idx,file)
    }
    #[inline(always)]
    fn close_in(&mut self,idx:u8) {
        self.0.close_in(idx)
    }
    #[inline(always)]
    fn close_out(&mut self,idx:u8) {
        self.0.close_out(idx)
    }
    #[inline(always)]
    fn eof(&self,idx:u8) -> bool {
        self.0.eof(idx)
    }
    #[inline(always)]
    fn write<ET:EngineTypes,D:std::fmt::Display>(&mut self,idx:i64,string:D,newlinechar:Option<ET::Char>,aux:&mut EngineAux<ET>) {
        self.0.write(idx,string,newlinechar,aux)
    }
    #[inline(always)]
    fn read<T:Token<Char=<Self::File as File>::Char>,E:ErrorHandler,F:FnMut(T)>(&mut self,
                                                                                idx:u8,eh:&E,
                                                                                handler:&mut <T::CS as ControlSequenceName<T::Char>>::Handler,
                                                                                cc:&CategoryCodeScheme<<Self::File as File>::Char>,endline:Option<<Self::File as File>::Char>,cont:F
    ) {
        self.0.read(idx,eh,handler,cc,endline,cont)
    }
    fn readline<T:Token<Char=<Self::File as File>::Char>,F:FnMut(T)>(&mut self, idx:u8,cont:F) {
        self.0.readline(idx,cont)
    }
}
