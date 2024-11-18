/*! Fonts */
mod tfm;

use crate::commands::{PrimitiveCommand, ResolvedToken, TeXCommand};
use crate::engine::filesystem::{File, FileSystem};
use crate::engine::fontsystem::tfm::TfmFile;
use crate::engine::{EngineAux, EngineReferences, EngineTypes};
use crate::prelude::CommandCode;
use crate::tex::characters::Character;
use crate::tex::numerics::{Numeric, TeXDimen, TeXInt};
use crate::tex::tokens::control_sequences::{CSHandler, CSName};
use crate::utils::errors::{TeXError, TeXResult};
use crate::utils::{HMap, Ptr};
use std::path::PathBuf;
use std::sync::RwLock;

/// A font system provides [`Font`]s, which in turn provide various information about [`Character`]s (or, rather, glyphs)
/// in that font.
pub trait FontSystem: Clone + std::fmt::Debug {
    /// The type of [`Character`]s used by this font system.
    type Char: Character;
    /// The type of [control sequences](CSName) used to give a name to a font; returned by e.g. `\the` when followed
    /// by a font.
    type CS: CSName<Self::Char>;
    /// The type of [`TeXInt`]s used by this font system, for e.g. `\skewchar`, `\hyphenchar` etc. - these
    /// numbers may escape the actual character range of the font, so we use [`TeXInt`]s instead of [`Character`]s.
    type Int: TeXInt;
    /// The type of [`Font`]s provided by this font system.
    type Font: Font<Char = Self::Char, CS = Self::CS, Dim = Self::Dim, Int = Self::Int>;
    /// The type of [`TeXDimen`]s used by this font system, for e.g. `\fontdimen`, `\fontcharwd` etc.
    type Dim: TeXDimen;
    /// Returns the `\nullfont`.
    fn null(&self) -> Self::Font;
    /// Creates a new font system.
    fn new<ET: EngineTypes<Char = Self::Char, CSName = Self::CS>>(aux: &mut EngineAux<ET>) -> Self;
    /// Creates a new font with the given macroname and filepath (in plain TeX a `.tfm`-file).
    fn new_font<S: AsRef<str>, F: FileSystem>(
        &mut self,
        path: S,
        macroname: <Self::Font as Font>::CS,
        fs: &mut F,
    ) -> Self::Font;
}

/// A font provides various information about [`Character`]s (or, rather, glyphs) in that font. Since in the [`Stomach`](crate::engine::Stomach)
/// every glyph in the output is directly associated with a [`Font`], we clone them a lot, so they should be cheap to clone;
/// e.g. using `Rc` or `Arc` is a good idea.
pub trait Font: Clone + std::fmt::Debug {
    /// The type of [`Character`]s used by this font.
    type Char: Character;
    /// The type of [`TeXDimen`]s used by this font, for e.g. `\fontdimen`, `\fontcharwd` etc.
    type Dim: TeXDimen;
    /// The type of [`TeXInt`]s used by this font, for e.g. `\skewchar`, `\hyphenchar` etc. - these
    /// numbers may escape the actual character range of the font, so we use [`TeXInt`]s instead of [`Character`]s.
    type Int: TeXInt;
    /// The type of [control sequences](CSName) used to give a name to a font; returned by e.g. `\the` when followed
    /// by a font.
    type CS: CSName<Self::Char>;
    /// The size of the font; initially provided by the font itself, but can be set via e.g. `\font\foo=cmr10 at 12pt`.
    fn get_at(&self) -> Self::Dim;
    /// Returns whether the size of the font has been set via e.g. `at ...`.
    fn has_at_set(&self) -> bool;
    /// Sets the size of the font.
    fn set_at(&mut self, d: Self::Dim);
    /// Returns the name of the font as a [`CSName`]; used by e.g. `\the\font`.
    fn name(&self) -> &Self::CS;
    /// Returns the filename of the font.
    fn filename(&self) -> &str;
    /// Returns the `\fontdimen` at the given index (-1); i.e. `\fontdimen5` returns `get_dim(4)`.
    fn get_dim(&self, idx: u16) -> Self::Dim;
    /// Sets the `\fontdimen` at the given index (-1); i.e. `\fontdimen5=...` calls `set_dim(4)`.
    fn set_dim(&mut self, idx: u16, d: Self::Dim);
    /// Returns the `\hyphenchar` of this font.
    fn get_hyphenchar(&self) -> Self::Int;
    /// Sets the `\hyphenchar` of this font.
    fn set_hyphenchar(&mut self, c: Self::Int);
    /// Returns the `\skewchar` of this font.
    fn get_skewchar(&self) -> Self::Int;
    /// Sets the `\skewchar` of this font.
    fn set_skewchar(&mut self, c: Self::Int);
    /// Returns whether this font has a glyph for the given [`Character`].
    fn has_char(&self, c: Self::Char) -> bool;
    /// Writes a human-readable representation of this font to the given [`std::fmt::Write`]r
    /// - i.e. `<`[`name()`](Self::name)`>[ at <`[`get_at()`](Self::get_at)`>]`
    fn display<W: std::fmt::Write>(
        &self,
        i: &<Self::CS as CSName<Self::Char>>::Handler,
        w: W,
    ) -> std::fmt::Result;
    /// Returns the width the given [`Character`] in this font.
    fn get_wd(&self, c: Self::Char) -> Self::Dim;
    /// Returns the height the given [`Character`] in this font.
    fn get_ht(&self, c: Self::Char) -> Self::Dim;
    /// Returns the depth the given [`Character`] in this font.
    fn get_dp(&self, c: Self::Char) -> Self::Dim;
    /// Returns the litalic correction of the given [`Character`] in this font.
    fn get_ic(&self, c: Self::Char) -> Self::Dim;
    /// Sets the litalic correction of the given [`Character`] in this font.
    fn set_ic(&mut self, c: Self::Char, d: Self::Dim);
    /// Returns the ligature of the given [`Character`]s in this font, if any; e.g. most fonts
    /// combine `-` and `-` into an endash.
    fn ligature(&self, c1: Self::Char, c2: Self::Char) -> Option<Self::Char>;
}

/// A font system for `.tfm`-files, as used by plain TeX, eTeX and pdfTeX for [`Character`]`=u8`
#[derive(Clone, Debug)]
pub struct TfmFontSystem<I: TeXInt, D: TeXDimen + Numeric<I>, CS: CSName<u8>> {
    files: HMap<PathBuf, Ptr<TfmFile>>,
    null: Ptr<TfmFontI<I, D, CS>>,
}

impl<I: TeXInt, D: TeXDimen + Numeric<I>, CS: CSName<u8>> FontSystem for TfmFontSystem<I, D, CS> {
    type Char = u8;
    type Int = I;
    type Font = TfmFont<I, D, CS>;
    type Dim = D;
    type CS = CS;

    fn new<ET: EngineTypes<Char = Self::Char, CSName = Self::CS>>(aux: &mut EngineAux<ET>) -> Self {
        let null_file = TfmFile {
            hyphenchar: 45,
            skewchar: 255,
            dimen: vec![],
            size: 0,
            //typestr:"nullfont".to_string(),
            widths: [0.0; 256],
            defined: [false; 256],
            heights: [0.0; 256],
            depths: [0.0; 256],
            ics: [0.0; 256],
            ligs: HMap::default(),
            filepath: std::path::PathBuf::from("/nullfont"),
        };
        let muts = Mutables::default();
        let null = Ptr::new(TfmFontI {
            file: Ptr::new(null_file),
            muts: RwLock::new(muts),
            name: aux.memory.cs_interner_mut().cs_from_str("nullfont"),
        });
        TfmFontSystem {
            files: HMap::default(),
            null,
        }
    }

    fn new_font<S: AsRef<str>, F: FileSystem>(
        &mut self,
        path: S,
        macroname: CS,
        fs: &mut F,
    ) -> Self::Font {
        let path = path.as_ref();
        let f = if path.ends_with(".tfm") {
            fs.get(path)
        } else {
            fs.get(format!("{path}.tfm"))
        };
        let ff = match self.files.get(f.path()) {
            Some(ff) => ff.clone(),
            None => {
                let ff = Ptr::new(TfmFile::new(f.path().to_path_buf()));
                self.files.insert(f.path().to_path_buf(), ff.clone());
                ff
            }
        };
        let muts = Mutables::default();
        let font = TfmFontI {
            file: ff,
            muts: RwLock::new(muts),
            name: macroname,
        };
        Ptr::new(font)
    }

    fn null(&self) -> Self::Font {
        self.null.clone()
    }
}

#[derive(Default)]
pub(crate) struct Mutables<I: TeXInt, D: TeXDimen + Numeric<I>> {
    at: Option<D>,
    #[cfg(feature = "pdflatex")]
    pub(crate) lps: HMap<u8, I>,
    #[cfg(feature = "pdflatex")]
    pub(crate) rps: HMap<u8, I>,
    ics: HMap<u8, D>,
    hyphenchar: Option<I>,
    skewchar: Option<I>,
    dimens: Vec<D>,
}

/// See [`TfmFont`].
pub struct TfmFontI<I: TeXInt, D: TeXDimen + Numeric<I>, CS: CSName<u8>> {
    file: Ptr<TfmFile>,
    name: CS,
    pub(crate) muts: RwLock<Mutables<I, D>>,
}
impl<I: TeXInt, D: TeXDimen + Numeric<I>, CS: CSName<u8>> PartialEq for TfmFontI<I, D, CS> {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}
impl<I: TeXInt, D: TeXDimen + Numeric<I>, CS: CSName<u8>> std::fmt::Debug for TfmFontI<I, D, CS> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Font {:?}", self.name)
    }
}

/// A [`Font`] represented by a `.tfm`-file, as used by plain TeX, eTeX and pdfTeX for [`Character`]`=u8`;
/// defined as `Rc<`[`TfmFontI`]`>` for clonability.
pub type TfmFont<I, D, CS> = Ptr<TfmFontI<I, D, CS>>;
impl<I: TeXInt, D: TeXDimen + Numeric<I>, CS: CSName<u8>> Font for TfmFont<I, D, CS> {
    type Char = u8;
    type CS = CS;
    type Int = I;
    type Dim = D;

    fn has_char(&self, c: Self::Char) -> bool {
        self.file.defined[c as usize]
    }
    fn get_hyphenchar(&self) -> I {
        match self.muts.read().unwrap().hyphenchar {
            Some(c) => c,
            None => (self.file.hyphenchar as i32).into(),
        }
    }
    fn set_hyphenchar(&mut self, c: I) {
        self.muts.write().unwrap().hyphenchar = Some(c);
    }
    fn get_skewchar(&self) -> I {
        match self.muts.read().unwrap().skewchar {
            Some(c) => c,
            None => (self.file.skewchar as i32).into(),
        }
    }
    fn set_skewchar(&mut self, c: I) {
        self.muts.write().unwrap().skewchar = Some(c);
    }
    fn get_at(&self) -> Self::Dim {
        let at = self.muts.read().unwrap().at;
        match at {
            Some(d) => d,
            None => D::from_sp(self.file.size),
        }
    }
    fn has_at_set(&self) -> bool {
        self.muts.read().unwrap().at.is_some()
    }
    fn get_dim(&self, idx: u16) -> Self::Dim {
        match self.muts.read().unwrap().dimens.get(idx as usize) {
            Some(d) => *d,
            None => match self.file.dimen.get(idx as usize) {
                Some(d) => self.get_at().scale_float(*d as f64),
                None => D::default(),
            },
        }
    }
    fn set_dim(&mut self, idx: u16, d: Self::Dim) {
        let v = &mut self.muts.write().unwrap().dimens;
        if idx as usize >= v.len() {
            v.resize(idx as usize + 1, D::default());
        }
        v[idx as usize] = d;
    }

    fn set_at(&mut self, d: Self::Dim) {
        self.muts.write().unwrap().at = Some(d);
    }

    fn name(&self) -> &Self::CS {
        &self.name
    }

    fn filename(&self) -> &str {
        self.file.name()
    }
    fn display<W: std::fmt::Write>(
        &self,
        _i: &<Self::CS as CSName<u8>>::Handler,
        mut w: W,
    ) -> std::fmt::Result {
        let at = self.muts.read().unwrap().at;
        match at {
            Some(d) => write!(w, "{} at {}", self.file.name(), d),
            None => write!(w, "{}", self.file.name()),
        }
    }

    fn get_ic(&self, c: Self::Char) -> Self::Dim {
        let v = &mut self.muts.write().unwrap().ics;
        match v.get(&c) {
            Some(d) => *d,
            None => {
                let d = self.file.ics[c as usize];
                self.get_at().scale_float(d as f64)
            }
        }
    }
    fn set_ic(&mut self, c: Self::Char, d: Self::Dim) {
        let v = &mut self.muts.write().unwrap().ics;
        v.insert(c, d);
    }

    fn get_wd(&self, c: Self::Char) -> Self::Dim {
        let d = self.file.widths[c as usize].max(0.0);
        self.get_at().scale_float(d as f64)
    }

    fn get_ht(&self, c: Self::Char) -> Self::Dim {
        let d = self.file.heights[c as usize].max(0.0);
        self.get_at().scale_float(d as f64)
    }

    fn get_dp(&self, c: Self::Char) -> Self::Dim {
        let d = self.file.depths[c as usize].max(0.0);
        self.get_at().scale_float(d as f64)
    }

    fn ligature(&self, char1: Self::Char, char2: Self::Char) -> Option<Self::Char> {
        self.file.ligs.get(&(char1, char2)).copied()
    }
}

impl<ET: EngineTypes> EngineReferences<'_, ET> {
    /// reads a font from the input stream (e.g. `\font` for the current font
    /// or a font defined via `\font\foo=...`).
    pub fn read_font(
        &mut self,
        skip_eq: bool,
        token: &ET::Token,
    ) -> TeXResult<<ET::FontSystem as FontSystem>::Font, ET> {
        let mut had_eq = !skip_eq;
        crate::expand_loop!(self,token,
            ResolvedToken::Cmd(Some(c)) => match c {
                TeXCommand::Font(f) => return Ok(f.clone()),
                TeXCommand::Primitive{cmd:PrimitiveCommand::FontCmd{read,..},..} => {
                    return read(self,token)
                }
                _ => {
                    self.general_error("Missing font identifier.".to_string())?;
                    return Ok(self.fontsystem.null())
                }
            }
            ResolvedToken::Tk {char,code:CommandCode::Other} if !had_eq && matches!(char.try_into(),Ok(b'=')) => {
                had_eq = true;
            }
            _ => {
                self.general_error("Missing font identifier.".to_string())?;
                return Ok(self.fontsystem.null())
            }
        );
        TeXError::file_end_while_use(self.aux, self.state, self.mouth, token.clone())?;
        Ok(self.fontsystem.null())
    }
}
