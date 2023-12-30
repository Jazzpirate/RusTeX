use std::borrow::Cow;
use std::collections::{BTreeMap, BTreeSet};
use std::fmt::Formatter;
use std::thread::current;
use tex_engine::engine::EngineTypes;
use tex_engine::engine::filesystem::File;
use tex_engine::engine::filesystem::SourceReference;
use tex_engine::tex::numerics::{Dim32, Mu};
use tex_tfm::glyphs::Glyph;
use crate::engine::{Font, Types};
use crate::fonts::FontStore;
use crate::shipout::{ShipoutState, ZERO};
use std::fmt::Write;
use tex_engine::engine::fontsystem::Font as FontT;

#[inline(always)]
fn dim_to_px(d:i32) -> f64{
    d as f64 / 65536.0 * 1.5
}

#[inline(always)]
pub(crate) fn dim_to_num(d:i32) -> String {
    format!("{:.5}",dim_to_px(d)).trim_end_matches('0').trim_end_matches('.').to_string()
}

#[inline(always)]
pub(crate) fn dim_to_string(d:Dim32) -> String {
    dim_to_num(d.0) + "px"
}

#[inline(always)]
pub(crate) fn mudim_to_string(d:Mu) -> String {
    format!("{:.5}",dim_to_px(d.0) / 18.0).trim_end_matches('0').trim_end_matches('.').to_string() + "em"
}

type Ref = SourceReference<<<Types as EngineTypes>::File as File>::SourceRefID>;

#[derive(Debug,Clone,PartialEq,Eq)]
pub enum Tag {
    Div,
    Span,
    Section,
    Article,
    Table,
    Tr,
    Td,
    A,
    B,
    I,
    None,
    Math,Mrow,Mi,Mo,Mspace,MUnderOver,MSubSup,MUnder,MOver,MSub,MSup,MText,
    Svg,SVGForeign,G(String)
}
impl Tag {
    fn ismath(&self) -> bool {
        use Tag::*;
        match self {
            Math | Mrow | Mi | Mo | Mspace | MUnderOver | MSubSup | MUnder | MOver | MSub | MSup | MText => true,
            _ => false
        }
    }
    fn is_svg(&self) -> bool {
        use Tag::*;
        match self {
            Svg | SVGForeign | G(_) => true,
            _ => false
        }
    }
}


pub(crate) mod labels {
    use super::Tag;
    #[derive(Clone, Debug, Eq)]
    pub(crate) struct Label {
        pub id: u8,
        pub(crate) cls: Option<&'static str>,
        pub(crate) tag: Tag
    }
    impl PartialEq for Label {
        #[inline(always)]
        fn eq(&self, other: &Self) -> bool {
            self.id == other.id
        }
    }

    pub(crate) const DUMMY: Label = Label { id: 0, cls: None, tag: Tag::None };
    pub(crate) const VBOX_CONTAINER: Label = Label { id: 1, cls: Some("rustex-vbox-container"), tag: Tag::Div };
    pub(crate) const VBOX_HEIGHT_CONTAINER: Label = Label { id: 2, cls: Some("rustex-vbox-height-container"), tag: Tag::Div };
    pub(crate) const VBOX_INNER: Label = Label { id: 3, cls: Some("rustex-vbox"), tag: Tag::Div };
    pub(crate) const HBOX_CONTAINER: Label = Label { id: 4, cls: Some("rustex-hbox-container"), tag: Tag::Div };
    pub(crate) const HBOX_INNER: Label = Label { id: 5, cls: Some("rustex-hbox"), tag: Tag::Div };
    pub(crate) const DEST: Label = Label { id: 6, cls: None, tag: Tag::A };
    pub(crate) const RAISE: Label = Label { id: 7, cls: Some("rustex-raise"), tag: Tag::Div };
    pub(crate) const MOVE_RIGHT: Label = Label { id: 8, cls: Some("rustex-moveright"), tag: Tag::Div };
    pub(crate) const VRULE_CONTAINER: Label = Label { id: 9, cls: Some("rustex-vrule-container"), tag: Tag::Div };
    pub(crate) const VRULE_INNER: Label = Label { id: 10, cls: Some("rustex-vrule"), tag: Tag::Div };
    pub(crate) const HRULE_CONTAINER: Label = Label { id: 11, cls: Some("rustex-hrule-container"), tag: Tag::Div };
    pub(crate) const HRULE_INNER: Label = Label { id: 12, cls: Some("rustex-hrule"), tag: Tag::Div };
    pub(crate) const HSKIP: Label = Label { id: 13, cls: Some("rustex-hskip"), tag: Tag::Div };
    pub(crate) const VSKIP: Label = Label { id: 14, cls: Some("rustex-vskip"), tag: Tag::Div };
    pub(crate) const PARAGRAPH: Label = Label { id: 15, cls: Some("rustex-paragraph"), tag: Tag::Div };
    pub(crate) const FONT_CHANGE: Label = Label { id: 16, cls: None, tag: Tag::None };
    pub(crate) const PAGE: Label = Label { id: 17, cls: Some("rustex-page"), tag: Tag::Article };
    pub(crate) const INNER_PAGE: Label = Label { id: 18, cls: Some("rustex-body"), tag: Tag::Section };
    pub(crate) const VCENTER_CONTAINER: Label = Label { id: 19, cls: Some("rustex-vcenter-container"), tag: Tag::Div };
    pub(crate) const VCENTER_INNER: Label = Label { id: 20, cls: Some("rustex-vcenter"), tag: Tag::Div };
    pub(crate) const HALIGN: Label = Label { id: 21, cls: Some("rustex-halign"), tag: Tag::Table };
    pub(crate) const HALIGN_ROW: Label = Label { id: 22, cls: Some("rustex-halign-row"), tag: Tag::Tr };
    pub(crate) const HALIGN_CELL: Label = Label { id: 23, cls: Some("rustex-halign-cell"), tag: Tag::Td };
    pub(crate) const SPACE: Label = Label { id: 24, cls: Some("rustex-space-in-hbox"), tag: Tag::Span };
    pub(crate) const MATH: Label = Label { id: 25, cls: Some("rustex-math"), tag: Tag::Math };
    pub(crate) const MATH_ROW: Label = Label { id: 26, cls: None, tag: Tag::Mrow };
    pub(crate) const MATH_ORD: Label = Label { id: 27, cls: Some("rustex-math-ord"), tag: Tag::Mi };
    pub(crate) const MATH_OP: Label = Label { id: 28, cls: Some("rustex-math-op"), tag: Tag::Mo };
    pub(crate) const MATH_BIN: Label = Label { id: 29, cls: Some("rustex-math-bin"), tag: Tag::Mo };
    pub(crate) const MATH_REL: Label = Label { id: 30, cls: Some("rustex-math-rel"), tag: Tag::Mo };
    pub(crate) const MATH_PUNCT: Label = Label { id: 31, cls: Some("rustex-math-punct"), tag: Tag::Mo };
    pub(crate) const MATH_OPEN: Label = Label { id: 32, cls: Some("rustex-math-open"), tag: Tag::Mo };
    pub(crate) const MATH_CLOSE: Label = Label { id: 33, cls: Some("rustex-math-close"), tag: Tag::Mo };
    pub(crate) const PARINDENT: Label = Label { id: 34, cls: Some("rustex-parindent"), tag: Tag::Div };
    pub(crate) const MSKIP: Label = Label { id: 35, cls: Some("rustex-mskip"), tag: Tag::Mspace };
    pub(crate) const HKERN_IN_M: Label = Label { id: 36, cls: None, tag: Tag::Mspace };
    pub(crate) const MISSING_GLYPH: Label = Label { id: 37, cls: Some("rustex-missing-glyph"), tag: Tag::None };
    pub(crate) const LINK: Label = Label {id: 38,cls:None,tag: Tag::None};
    pub(crate) const COLOR_CHANGE: Label = Label { id: 39, cls: None, tag: Tag::None };
    pub(crate) const MUNDEROVER: Label = Label { id: 40, cls: Some("rustex-munderover"), tag: Tag::MUnderOver };
    pub(crate) const MSUBSUP: Label = Label { id: 41, cls: Some("rustex-subsup"), tag: Tag::MSubSup };
    pub(crate) const MUNDER: Label = Label { id: 42, cls: Some("rustex-munder"), tag: Tag::MUnder };
    pub(crate) const MOVER: Label = Label { id: 43, cls: Some("rustex-mover"), tag: Tag::MOver };
    pub(crate) const MSUB: Label = Label { id: 44, cls: Some("rustex-msub"), tag: Tag::MSub };
    pub(crate) const MSUP: Label = Label { id: 45, cls: Some("rustex-msup"), tag: Tag::MSup };
    pub(crate) const MATH_ESCAPE: Label = Label { id: 46, cls: Some("rustex-math-escape"), tag: Tag::MText };
    pub(crate) const SVG_WRAP: Label = Label { id: 47, cls: Some("rustex-svg"), tag: Tag::Div };
    pub(crate) const SVG: Label = Label { id: 48, cls: None, tag: Tag::Svg };
    pub(crate) fn svg_g(s:String) -> Label { Label { id: 49, cls: None, tag: Tag::G(s) } }
    pub(crate) const SVG_FOREIGN: Label = Label { id: 50, cls: None, tag: Tag::SVGForeign };
    pub(crate) const SVG_ESCAPE_DIV: Label = Label { id: 50, cls: Some("rustex-foreign"), tag: Tag::Div };
    pub(crate) const PDF_MATRIX: Label = Label { id: 51, cls: Some("rustex-pdfmatrix"), tag: Tag::Span };
    pub(crate) const NOALIGN_H: Label = Label { id: 52, cls: Some("rustex-noalign"), tag: Tag::Div };
}

#[derive(Debug)]
pub enum HTMLChild {
    Node(HTMLNode),
    Text(String),
    Glyph(Glyph),Space,EscapedSpace
}

#[derive(Debug)]
pub struct HTMLNode {
    tag: Tag,pub label:labels::Label,
    pub classes:Vec<Cow<'static,str>>,
    pub attrs:BTreeMap<Cow<'static,str>,String>,
    pub styles:BTreeMap<Cow<'static,str>,Cow<'static,str>>,
    pub children:Vec<HTMLChild>,
    sourceref:Option<(Ref,Ref)>,
    pub width:Option<Dim32>,
    pub(crate) font:Option<Font>,
    pub(crate) force_font:bool,
    pub(crate) allow_newline:bool,
    pub(crate) inner_width:Option<Dim32>,
}

impl HTMLNode {
    #[inline(always)]
    pub(crate) fn reopened(&self) -> Self {
        Self {
            tag:self.tag.clone(),
            label:self.label.clone(),
            classes:self.classes.clone(),
            attrs:self.attrs.clone(),
            styles:self.styles.clone(),
            children:Vec::new(),
            sourceref:self.sourceref,
            width:self.width,
            font:self.font.clone(),
            force_font:self.force_font,
            allow_newline:self.allow_newline,
            inner_width:self.inner_width,
        }
    }
    pub(crate) fn is_empty(&self) -> bool {
        self.children.is_empty()
    }

    #[inline(always)]
    pub fn display<W:std::fmt::Write>(self,store:&mut FontStore,state:&mut ShipoutState,mut f:&mut W) -> std::fmt::Result {
        let urls = &mut state.fontlinks;
        let fnt = state.fonts.first().unwrap();
        let wd = state.widths.first().copied().unwrap();
        self.display_fmt(store,urls,fnt,wd,0,false,false,&mut f)
    }

    fn display_fmt<W:std::fmt::Write>(mut self,store:&mut FontStore,links:&mut BTreeSet<String>,current_font:&Font,top_width:Dim32,indent:usize,mut in_math:bool,mut in_svg:bool,mut f:&mut W) -> std::fmt::Result {
        let mut wd = self.do_width_fmt(top_width);
        let fnt = self.do_font_fmt(store,links,current_font);
        if self.tag != Tag::None {
            in_math = self.tag.ismath();
            in_svg = self.tag.is_svg();
        }
        self.tag_fmt(f,in_math,in_svg,|mut s,f| {
            if !s.classes.is_empty() {
                f.write_str(" class=\"")?;
                let mut i = s.classes.iter();
                write!(f,"{}",i.next().unwrap())?;
                for c in i {
                    write!(f," {}",c)?;
                }
                f.write_char('"')?;
            }
            if !s.attrs.is_empty() {
                for (k,v) in s.attrs.iter() {
                    write!(f," {}=\"{}\"",k,v)?;
                }
            }
            if !s.styles.is_empty() || s.width.is_some() {
                f.write_str(" style=\"")?;
                for (k,v) in s.styles.iter() {
                    write!(f,"{}:{};",k,v)?;
                }
                f.write_char('"')?;
            }
            /*if let Some((start,end)) = self.sourceref {
                write!(f," data-start=\"{}\" data-end=\"{}\"",start,end)?;
            }*/
            f.write_char('>')?;
            if let Some(w) = s.inner_width {
                let pctg = w.0 as f64 / (top_width.0 as f64);
                wd = Some(w);
                f.write_str(&format!("<span style=\"display:contents;--temp-width:calc({:.2} * var(--document-width))\">",pctg))?;
            }
            if wd.is_some() {
                f.write_str("<span class=\"rustex-contents\">")?;
            }
            for c in s.children.into_iter() {
                match c {
                    HTMLChild::Node(n) => {
                        if s.allow_newline {
                            f.write_char('\n')?;
                            for _ in 0..=indent {
                                f.write_str("  ")?;
                            }
                        }
                        n.display_fmt(store,links,&fnt,wd.unwrap_or(top_width),indent+1,in_math,in_svg,f)?
                    }
                    HTMLChild::Text(s) => f.write_str(&s)?,
                    HTMLChild::Glyph(g) => write!(f,"{}",g)?,
                    HTMLChild::Space => f.write_char(' ')?,
                    HTMLChild::EscapedSpace => f.write_str("<span class=\"rustex-space-in-hbox\">&#160;</span>")?
                }
            }
            if s.allow_newline {
                f.write_char('\n')?;
                for _ in 0..indent {
                    f.write_str("  ")?;
                }
            }
            if wd.is_some() {
                f.write_str("</span>")?;
            }
            if s.inner_width.is_some() {
                f.write_str("</span>")?;
            }
            Ok(())
        })
    }

    fn do_width_fmt(&mut self,top_width:Dim32) -> Option<Dim32> {
        match self.width {
            Some(w) if w == ZERO => {
                self.style_str("max-width","0px");
                None
            }
            Some(w) if w < ZERO => {
                self.style_str("max-width","0px");
                self.style("margin-right",dim_to_string(w));
                None
            }
            Some(w) if w == top_width => {
                self.style_str("min-width","var(--document-width)");
                self.style_str("max-width","var(--document-width)");
                None
            }
            Some(w) if w != top_width => {
                let pctg = w.0 as f64 / (top_width.0 as f64);
                self.style("--temp-width",format!("calc({:.2} * var(--document-width))",pctg));
                self.class("rustex-withwidth");
                Some(w)
            }
            _ => None
        }
    }
    fn do_font_fmt(&mut self,store:&mut FontStore,links:&mut BTreeSet<String>,current_font:&Font) -> Font {
        if let Some(ref font) = self.font {
            if self.force_font {
                let new = store.get_info(font.filename());
                match new.map(|f| f.weblink).flatten() {
                    Some((name,link)) => {
                        links.insert(link.to_string());
                        self.styles.insert(Cow::Borrowed("font-family"),Cow::Owned(name.into()));
                    }
                    None if font.filename().ends_with("nullfont") => (),
                    None => {
                        self.children.insert(0,HTMLChild::Text(format!("<!-- Unknown web font for {} -->",font.filename())))
                    }
                }
                if let Some(fi) = new {
                    if fi.styles.capitals {
                        self.styles.insert(Cow::Borrowed("font-variant"), Cow::Borrowed("small-caps"));
                    }
                    if fi.styles.bold {
                        self.styles.insert(Cow::Borrowed("font-weight"), Cow::Borrowed("bold"));
                    }
                    if fi.styles.italic {
                        self.styles.insert(Cow::Borrowed("font-style"), Cow::Borrowed("italic"));
                    } else if fi.styles.oblique {
                        self.styles.insert(Cow::Borrowed("font-style"), Cow::Borrowed("oblique"));
                    }
                }
            } else if font.filename() != current_font.filename() {
                let old = store.get_info(current_font.filename());
                let new = store.get_info(font.filename());
                match (old.map(|f| f.weblink).flatten(),new.map(|f| f.weblink).flatten()) {
                    (Some((name,_)),Some((name2,_))) if name == name2 => (),
                    (_,Some((name,link))) => {
                        links.insert(link.to_string());
                        self.styles.insert(Cow::Borrowed("font-family"),Cow::Owned(name.into()));
                    }
                    (_,None) if font.filename().ends_with("nullfont") => (),
                    (_,None) =>
                        self.children.insert(0,HTMLChild::Text(format!("<!-- Unknown web font for {} -->",font.filename())))
                }
                if let Some(fi) = new {
                    if let Some(old) = old {
                        if fi.styles.capitals && !old.styles.capitals {
                            self.styles.insert(Cow::Borrowed("font-variant"), Cow::Borrowed("small-caps"));
                        } else if old.styles.capitals && !fi.styles.capitals {
                            self.styles.insert(Cow::Borrowed("font-variant"), Cow::Borrowed("normal"));
                        }
                        if fi.styles.bold && !old.styles.bold {
                            self.styles.insert(Cow::Borrowed("font-weight"), Cow::Borrowed("bold"));
                        } else if old.styles.bold && !fi.styles.bold {
                            self.styles.insert(Cow::Borrowed("font-weight"), Cow::Borrowed("normal"));
                        }
                        if fi.styles.italic && !old.styles.italic {
                            self.styles.insert(Cow::Borrowed("font-style"), Cow::Borrowed("italic"));
                        } else if fi.styles.oblique && !old.styles.oblique {
                            self.styles.insert(Cow::Borrowed("font-style"), Cow::Borrowed("oblique"));
                        } else if (old.styles.italic || old.styles.oblique) && !(fi.styles.italic || fi.styles.oblique) {
                            self.styles.insert(Cow::Borrowed("font-style"), Cow::Borrowed("normal"));
                        }
                    } else {
                        if fi.styles.capitals {
                            self.styles.insert(Cow::Borrowed("font-variant"), Cow::Borrowed("small-caps"));
                        }
                        if fi.styles.bold {
                            self.styles.insert(Cow::Borrowed("font-weight"), Cow::Borrowed("bold"));
                        }
                        if fi.styles.italic {
                            self.styles.insert(Cow::Borrowed("font-style"), Cow::Borrowed("italic"));
                        } else if fi.styles.oblique {
                            self.styles.insert(Cow::Borrowed("font-style"), Cow::Borrowed("oblique"));
                        }
                    }
                }
            }
            if !font.filename().ends_with("nullfont") {
                let rel = font.get_at().0 as f32 / (current_font.get_at().0 as f32);
                if rel != 1.0 {
                    self.styles.insert(Cow::Borrowed("font-size"), Cow::Owned(format!("{:.2}%", (rel * 100.0).round())));
                }
            }
            font.clone()
        } else { current_font.clone() }
    }
    fn tag_fmt<W:std::fmt::Write,F:FnOnce(Self,&mut W) -> std::fmt::Result>(self,f:&mut W,in_math:bool,in_svg:bool,cont:F) -> std::fmt::Result {
        macro_rules! go {
            ($tag:ident) => {{
                f.write_str("<")?;
                f.write_str(stringify!($tag))?;
                cont(self,f);
                f.write_str("</")?;
                f.write_str(stringify!($tag))?;
                f.write_str(">")
            }}
        }
        match self.tag {
            Tag::Div => go!(div),
            Tag::Span => go!(span),
            Tag::Section => go!(section),
            Tag::Article => go!(article),
            Tag::A => go!(a),
            Tag::B => go!(b),
            Tag::I => go!(i),
            Tag::Table => go!(table),
            Tag::Tr => go!(tr),
            Tag::Td => go!(td),
            Tag::None if in_math => go!(mrow),
            Tag::None if in_svg => go!(g),
            Tag::None => go!(span),
            Tag::Math => go!(math),
            Tag::Mrow => go!(mrow),
            Tag::Mi => go!(mi),
            Tag::Mo => go!(mo),
            Tag::Mspace => go!(mspace),
            Tag::MUnderOver => go!(munderover),
            Tag::MSubSup => go!(msubsup),
            Tag::MUnder => go!(munder),
            Tag::MOver => go!(mover),
            Tag::MSub => go!(msub),
            Tag::MSup => go!(msup),
            Tag::MText => go!(mtext),
            Tag::Svg => go!(svg),
            Tag::SVGForeign => go!(foreignObject),
            Tag::G(ref s) => {
                let s = s.clone();
                write!(f,"<{}",s)?;
                cont(self,f);
                write!(f,"</{}>",s)
            },
        }
    }

    pub fn push_node(&mut self,mut n:HTMLNode) {
        n.close(self.tag.ismath(),self.tag.is_svg());
        self.children.push(HTMLChild::Node(n));
    }

    pub fn close(&mut self,in_math:bool,in_svg:bool) {
        match self.tag {
            Tag::None => {
                if self.children.len() == 1 {
                    match self.children.pop() {
                        Some(HTMLChild::Node(n)) => {
                            let tag = n.tag.clone();
                            if self.merge(n) {
                                self.tag = tag;
                            } else {
                                if in_math {
                                    self.tag = Tag::Mrow
                                } else if in_svg {
                                    self.tag = Tag::G("g".into())
                                } else {
                                    self.style_str("display", "contents");
                                    self.tag = Tag::Span; // TODO math
                                }
                            }
                        }
                        Some(o) => self.children.push(o),
                        _ => unreachable!()
                    }
                } else if in_math {
                    self.tag = Tag::Mrow
                } else if in_svg {
                    self.tag = Tag::G("g".into())
                } else {
                    self.style_str("display","contents");
                    self.tag = Tag::Span; // TODO math
                }
            }
            _ if self.children.len() == 1 => {
                let c = self.children.pop().unwrap();
                match c {
                    HTMLChild::Node(n) if n.tag == Tag::None => {
                        self.merge(n);
                    },
                    _ => self.children.push(c)
                }
            }
            _ => ()
        }
    }

    fn merge(&mut self,n:HTMLNode) -> bool {
        for k in self.attrs.keys() {
            if n.attrs.contains_key(k) {
                self.children.push(HTMLChild::Node(n));
                return false;
            }
        }
        if let Some(w) = n.width {
            self.width = Some(w);
        }
        if let Some(f) = n.font {
            self.font = Some(f);
        }
        for (k,v) in n.attrs {
            self.attrs.insert(k,v);
        }
        for c in n.classes {
            self.classes.push(c);
        }
        for (k,v) in n.styles {
            self.styles.insert(k,v);
        }
        if let Some(sr) = n.sourceref {
            self.sourceref = Some(sr);
        }
        self.force_font = self.force_font || n.force_font;
        if let Some(iw) = n.inner_width {
            self.inner_width = Some(iw);
        }
        self.children = n.children;
        true
    }

    #[inline(always)]
    pub fn push_text(&mut self,s:String) {
        self.children.push(HTMLChild::Text(s));
    }
    #[inline(always)]
    pub fn push_glyph(&mut self,g:Glyph) {
        self.children.push(HTMLChild::Glyph(g));
    }
    #[inline(always)]
    pub fn push_space(&mut self) {
        self.children.push(HTMLChild::Space);
    }

    #[inline(always)]
    pub fn width(&mut self,w:Dim32) {
        self.width = Some(w);
    }

    #[inline(always)]
    pub fn class(&mut self,cls:&'static str) {
        self.classes.push(Cow::Borrowed(cls));
    }

    #[inline(always)]
    pub fn attr(&mut self,attr:&'static str,value:String) {
        self.attrs.insert(Cow::Borrowed(attr),value);
    }

    #[inline(always)]
    pub fn style(&mut self,attr:&'static str,value:String) {
        self.styles.insert(Cow::Borrowed(attr),Cow::Owned(value));
    }
    #[inline(always)]
    pub fn style_str(&mut self,attr:&'static str,value:&'static str) {
        self.styles.insert(Cow::Borrowed(attr),Cow::Borrowed(value));
    }

    #[inline(always)]
    pub fn set_font(&mut self,font:Font) {
        self.font = Some(font);
    }

    #[inline(always)]
    pub fn sourceref(&mut self,start:Ref,end:Ref) {
        self.sourceref = Some((start,end));
    }
    #[inline(always)]
    pub fn new(label: labels::Label, allow_newline:bool) -> Self {
        let cls = label.cls.clone();
        let mut r = HTMLNode {
            tag:label.tag.clone() ,label,allow_newline,..Self::default()
        };
        if let Some(cls) = cls { r.class(cls); }
        r
    }
}
impl Default for HTMLNode {
    fn default() -> Self {
        HTMLNode {
            label:labels::DUMMY,
            tag: Tag::None,
            classes:vec!(),
            attrs:BTreeMap::new(),
            styles:BTreeMap::new(),
            children:Vec::new(),
            sourceref:None,
            width:None,
            allow_newline:false,
            force_font:false,
            font:None,
            inner_width:None,
        }
    }
}