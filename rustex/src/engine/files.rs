use std::path::PathBuf;
use tex_engine::engine::filesystem::{File, FileSystem, NoOutputFileSystem, VirtualFile};
use tex_engine::engine::{EngineAux, EngineTypes};
use tex_engine::prelude::CSName;
use tex_engine::tex::characters::{StringLineSource, TextLine};
use tex_engine::utils::errors::TeXResult;
use tex_engine::utils::Ptr;

static PGFSYS: &str = include_str!("../resources/pgfsys.def");
static RUSTEX_DEFS: &str = include_str!("../resources/patches.tex");

#[derive(Clone)]
pub struct RusTeXFileSystem {
    pub(crate) inner: NoOutputFileSystem<u8>,
    pub(crate) svg: (<VirtualFile<u8> as File>::SourceRefID, Ptr<[TextLine<u8>]>),
    pub(crate) defs: (<VirtualFile<u8> as File>::SourceRefID, Ptr<[TextLine<u8>]>),
}
impl RusTeXFileSystem {
    pub fn new_with_envs<I: IntoIterator<Item = (String, String)>>(pwd: PathBuf, envs: I) -> Self {
        let mut ret = Self::new(pwd);
        ret.inner.envs.extend(envs);
        ret
    }

    pub fn add_file(&mut self, path: PathBuf, file_content: &str) {
        self.inner.add_file(path, file_content);
    }

    pub fn add_envs<I: IntoIterator<Item = (String, String)>>(&mut self, envs: I) {
        self.inner.envs.extend(envs);
    }
}

impl FileSystem for RusTeXFileSystem {
    type File = VirtualFile<u8>;

    fn new(pwd: PathBuf) -> Self {
        let mut inner = NoOutputFileSystem::new(pwd);
        let svg_id = inner
            .interner
            .get_or_intern("<TEXINPUTS>/pgfsys-rustex.def");
        let ext_id = inner.interner.get_or_intern("<TEXINPUTS>/rustex_defs.def");
        Self {
            inner,
            svg: (
                Some(svg_id),
                StringLineSource::make_lines(PGFSYS.as_bytes().iter().copied()).into(),
            ),
            defs: (
                Some(ext_id),
                StringLineSource::make_lines(RUSTEX_DEFS.as_bytes().iter().copied()).into(),
            ),
        }
    }
    fn ref_str(&self, id: <Self::File as File>::SourceRefID) -> &str {
        self.inner.ref_str(id)
    }

    fn get<S: AsRef<str>>(&mut self, path: S) -> Self::File {
        let sr = path.as_ref();
        if sr.ends_with("pgfsys-rustex.def") {
            VirtualFile {
                path: self.inner.kpse.pwd.join("pgfsys-rustex.def"),
                id: self.svg.0,
                pipe: false,
                exists: true,
                source: Some(self.svg.1.clone()),
            }
        } else if sr.ends_with("rustex_defs.def") {
            VirtualFile {
                path: self.inner.kpse.pwd.join("rustex_defs.def"),
                id: self.defs.0,
                pipe: false,
                exists: true,
                source: Some(self.defs.1.clone()),
            }
        } else {
            self.inner.get(sr)
        }
    }

    fn set_pwd(&mut self, pwd: PathBuf) -> PathBuf {
        self.inner.set_pwd(pwd)
    }

    fn open_out(&mut self, idx: u8, file: Self::File) {
        self.inner.open_out(idx, file)
    }

    fn open_in(&mut self, idx: u8, file: Self::File) {
        self.inner.open_in(idx, file)
    }

    fn close_in(&mut self, idx: u8) {
        self.inner.close_in(idx)
    }

    fn close_out(&mut self, idx: u8) {
        self.inner.close_out(idx)
    }

    fn eof(&self, idx: u8) -> bool {
        self.inner.eof(idx)
    }

    fn write<ET: EngineTypes, D: std::fmt::Display>(
        &mut self,
        idx: i64,
        string: D,
        newlinechar: Option<ET::Char>,
        aux: &mut EngineAux<ET>,
    ) {
        self.inner.write(idx, string, newlinechar, aux)
    }

    fn read<ET: EngineTypes<Char = <Self::File as File>::Char>, F: FnMut(ET::Token)>(
        &mut self,
        idx: u8,
        handler: &mut <ET::CSName as CSName<ET::Char>>::Handler,
        state: &ET::State,
        cont: F,
    ) -> TeXResult<(), ET> {
        self.inner.read::<ET, F>(idx, handler, state, cont)
    }

    fn readline<ET: EngineTypes<Char = <Self::File as File>::Char>, F: FnMut(ET::Token)>(
        &mut self,
        idx: u8,
        state: &ET::State,
        cont: F,
    ) -> TeXResult<(), ET> {
        self.inner.readline(idx, state, cont)
    }
}
