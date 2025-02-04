/*! [`Glyph`] and [`GlyphList`] types.*/
use std::fmt::{Debug, Display, Write};

/// A glyph is a character in some font.
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Glyph(pub(crate) GlyphI);

impl Glyph {
    /// Get the name of this glyph as a [`GlyphName`], e.g.
    /// ```
    /// # use tex_glyphs::glyphs::Glyph;
    /// assert_eq!(&Glyph::get("Gamma").to_string(),"Γ");
    /// ```
    #[must_use]
    pub const fn name(&self) -> GlyphName {
        GlyphName(&self.0)
    }
    /// Get the undefined glyph (i.e. the glyph with name `.undefined`).
    #[must_use]
    pub const fn undefined() -> Self {
        Self(GlyphI::S(0))
    }
    /// Whether this glyph is defined.
    #[must_use]
    pub const fn is_defined(&self) -> bool {
        match self.0 {
            GlyphI::S(i) => i != 0,
            GlyphI::Undefined(_) => false,
            _ => true,
        }
    }

    /// Lookup a glyph by *value*, i.e. `Glyph::lookup("Γ")` returns the glyph with name `Gamma`.
    /// ```
    /// # use tex_glyphs::glyphs::Glyph;
    /// assert_eq!(Glyph::lookup("Γ").unwrap(),Glyph::get("Gamma"));
    /// ```
    #[must_use]
    pub fn lookup(s: &str) -> Option<Self> {
        crate::GLYPH_LOOKUP.get(s).map(|g| Self(GlyphI::S(*g)))
    }

    /// Returns the glyph with the given name or the undefined glyph if no such glyph exists.
    pub fn get<S: AsRef<str>>(s: S) -> Self {
        let s = s.as_ref();
        get_i(s).map_or_else(|| Self(GlyphI::Undefined(s.into())), |g| g)
    }
}
impl Display for Glyph {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.0, f)
    }
}
impl Debug for Glyph {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Glyph({})=\'{}\'", self.name(), self)
    }
}

/// A list of glyphs in some font.
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct GlyphList(pub(crate) [Glyph; 256]);
impl GlyphList {
    /// Get the glyph at the given index.
    #[must_use]
    pub fn get(&self, c: u8) -> Glyph {
        self.0[c as usize].clone()
    }
    /// Whether this is the undefined glyph list, where every glyph is undefined.
    #[must_use]
    pub fn is_defined(&self) -> bool {
        *self == UNDEFINED_LIST
    }
}

/// Utility struct for displaying the name of a [`Glyph`] (e.g. `uni0041`, `A` or `Gamma`).
pub struct GlyphName<'a>(&'a GlyphI);
impl Display for GlyphName<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.0 {
            GlyphI::S(i) => f.write_str(crate::GLYPH_NAMES[*i as usize]),
            GlyphI::Unicode(c) => write!(f, "uni{:04X}", *c as u32),
            GlyphI::Ls(ls) => {
                for g in ls {
                    Display::fmt(&GlyphName(g), f)?;
                }
                Ok(())
            }
            GlyphI::Undefined(s) => Display::fmt(s, f),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub(crate) enum GlyphI {
    S(u16),
    Unicode(char),
    Ls(Box<[GlyphI]>),
    Undefined(Box<str>),
}
impl Display for GlyphI {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::S(i) => Display::fmt(&crate::GLYPH_LIST[*i as usize], f),
            Self::Unicode(c) => f.write_char(*c),
            Self::Ls(ls) => {
                for g in ls {
                    Display::fmt(g, f)?;
                }
                Ok(())
            }
            Self::Undefined(_) => f.write_str("???"),
        }
    }
}

pub(crate) const UNDEFINED: Glyph = Glyph(GlyphI::S(0));
pub(crate) const UNDEFINED_LIST: GlyphList = GlyphList([UNDEFINED; 256]);

#[allow(clippy::cognitive_complexity)]
fn get_i(s: &str) -> Option<Glyph> {
    match crate::GLYPH_MAP.get(s) {
        Some(g) => Some(Glyph(GlyphI::S(*g))),
        None if s.starts_with('.') => get_i(&s[1..]),
        None if s.contains('.') => {
            let Some(i) = s.find('.') else { unreachable!() };
            get_i(&s[..i])
        }
        None if s.ends_with("_SC") => {
            get_i(&s[..s.len() - 3]) // TODO - remove or convert to smallcaps?
        }
        None if s.ends_with("_os") => {
            get_i(&s[..s.len() - 3]) // TODO - what does "osf"/"os" signify?
        }
        None if s.ends_with("_sub")
            || s.ends_with("_sup")
            || s.ends_with("_SUB")
            || s.ends_with("_SUP") =>
        {
            get_i(&s[..s.len() - 4])
        } // TODO unicode does not have subscript/superscript letters (for the most part)
        None if s.ends_with("superior") || s.ends_with("inferior") => get_i(&s[..s.len() - 8]),
        None if s.ends_with("_swash") || s.ends_with("_short") => get_i(&s[..s.len() - 6]),
        None if s.ends_with("_swash1") => get_i(&s[..s.len() - 7]),
        None if s.ends_with("jmn") => get_i(&s[..s.len() - 3]),
        None if s.ends_with("_alt") => get_i(&s[..s.len() - 4]),
        None if s == "alt" => None, //UNDEFINED.clone(),
        None if s.ends_with("alt") => get_i(&s[..s.len() - 3]),
        None if s.ends_with("Greek") || s.ends_with("greek") => get_i(&s[..s.len() - 5]),
        None if s == "text" => None, //UNDEFINED.clone(),
        None if s.ends_with("text") => get_i(&s[..s.len() - 4]),
        None if s.ends_with("display") => get_i(&s[..s.len() - 7]),
        None if s.ends_with("disp") => get_i(&s[..s.len() - 4]),
        None if s.starts_with('_') && s.ends_with('_') => get_i(&s[1..s.len() - 1]),
        None if s.contains('_') => {
            let rets = s
                .split('_')
                .filter(|v| !v.is_empty())
                .map(get_i)
                .collect::<Vec<_>>();
            if rets.iter().any(Option::is_none) {
                return None;
            }
            Some(Glyph(GlyphI::Ls(
                rets.into_iter()
                    .map(|o| o.unwrap_or_else(|| unreachable!()).0)
                    .collect(),
            )))
        }
        None if s.starts_with("uni") => {
            match parse_unicode(&s[3..]) {
                Some(Ok(c)) => Some(Glyph(GlyphI::Unicode(c))),
                Some(Err(ls)) => Some(Glyph(GlyphI::Ls(ls))),
                None => None, //UNDEFINED.clone()
            }
        }
        None if s.starts_with('u') => {
            match parse_unicode(&s[1..]) {
                Some(Ok(c)) => Some(Glyph(GlyphI::Unicode(c))),
                Some(Err(ls)) => Some(Glyph(GlyphI::Ls(ls))),
                None => None, //UNDEFINED.clone()
            }
        }
        None if s.ends_with('1') || s.ends_with('2') || s.ends_with('3') || s.ends_with('4') => {
            get_i(&s[..s.len() - 1])
        }
        None if s == "SSsmall" => get_i("germandbls"),
        None if s.starts_with("aux") => get_i(&s[3..]),
        _ => None,
    }
}

fn parse_one(s: &str) -> Option<char> {
    u32::from_str_radix(s, 16)
        .map(std::char::from_u32)
        .unwrap_or(None)
}
fn parse_unicode(s: &str) -> Option<Result<char, Box<[GlyphI]>>> {
    let mut s = s.trim_start();
    if s.contains(' ') {
        let r = s.split(' ').map(parse_one).collect::<Option<Vec<_>>>();
        return r.map(|v| {
            Err(v
                .into_iter()
                .map(GlyphI::Unicode)
                .collect::<Vec<_>>()
                .into())
        });
    }
    if s.len() == 4 {
        return parse_one(s).map(Ok);
    }
    if s.len() % 4 == 0 {
        let mut v = Vec::new();
        while !s.is_empty() {
            match parse_one(&s[..4]) {
                Some(c) => {
                    s = s[4..].trim_start();
                    v.push(GlyphI::Unicode(c));
                }
                None => return None,
            }
        }
        Some(Err(v.into_boxed_slice()))
    } else {
        parse_one(s).map(Ok)
    }
}
