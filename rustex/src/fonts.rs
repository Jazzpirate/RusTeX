use tex_engine::engine::{EngineAux, EngineTypes};
use tex_engine::engine::filesystem::FileSystem;
use tex_engine::tex::numerics::Dim32;
use tex_engine::engine::fontsystem::{Font, FontSystem as FontSystemT};
use tex_engine::tex::tokens::control_sequences::InternedCSName;

pub(crate) type FontStore = tex_tfm::encodings::EncodingStore<String,fn(&str) -> String>;

fn get(s:&str) -> String {
    match tex_engine::engine::filesystem::kpathsea::KPATHSEA.which(s) {
        Some(p) => p.display().to_string(),
        None => s.to_string()
    }
}
#[derive(Clone,Debug)]
pub struct Fontsystem {
    fs:tex_engine::engine::fontsystem::TfmFontSystem<i32,Dim32,InternedCSName<u8>>,
    pub glyphmaps:FontStore
}
impl FontSystemT for Fontsystem {
    type Char = u8;
    type Int = i32;
    type Font = tex_engine::engine::fontsystem::TfmFont<i32,Dim32,InternedCSName<u8>>;
    type Dim = Dim32;
    type CS = InternedCSName<u8>;
    fn new<ET: EngineTypes<Char=Self::Char, CSName=Self::CS>>(aux: &mut EngineAux<ET>) -> Self {
        Fontsystem {
            fs: tex_engine::engine::fontsystem::TfmFontSystem::new(aux),
            glyphmaps: FontStore::new(get)
        }
    }
    #[inline(always)]
    fn new_font<S: AsRef<str>, F: FileSystem>(&mut self, path: S, macroname: <Self::Font as Font>::CS, fs: &mut F) -> Self::Font {
        self.fs.new_font(path, macroname, fs)
    }
    #[inline(always)]
    fn null(&self) -> Self::Font { self.fs.null() }
}