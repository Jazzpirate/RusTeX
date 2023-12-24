use std::borrow::Cow;
use std::collections::{BTreeMap, BTreeSet};
use std::fmt::Formatter;
use tex_engine::engine::EngineTypes;
use tex_engine::engine::filesystem::File;
use tex_engine::engine::filesystem::SourceReference;
use tex_engine::tex::numerics::{Dim32, Mu};
use tex_tfm::glyphs::Glyph;
use crate::engine::{Font, Types};
use crate::fonts::FontStore;
use crate::shipout::{ShipoutState, ZERO};

#[inline(always)]
fn dim_to_px(d:i32) -> f64{
    d as f64 / 65536.0 * 1.5
}

#[inline(always)]
pub(crate) fn dim_to_string(d:Dim32) -> String {
    format!("{:.5}",dim_to_px(d.0)).trim_end_matches('0').trim_end_matches('.').to_string() + "px"
}

#[inline(always)]
pub(crate) fn mudim_to_string(d:Mu) -> String {
    format!("{:.5}",dim_to_px(d.0) / 18.0).trim_end_matches('0').trim_end_matches('.').to_string() + "em"
}

type Ref = SourceReference<<<Types as EngineTypes>::File as File>::SourceRefID>;

#[derive(Debug,Copy,Clone,PartialEq,Eq)]
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
    Math,Mrow,Mi,Mo,Mspace
}
impl Tag {
    fn ismath(&self) -> bool {
        use Tag::*;
        match self {
            Math | Mrow | Mi | Mo | Mspace => true,
            _ => false
        }
    }
}


pub(crate) mod labels {
    use super::Tag;
    #[derive(Copy, Clone, Debug, Eq)]
    pub(crate) struct Label {
        id: u8,
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
    classes:Vec<Cow<'static,str>>,
    attrs:BTreeMap<Cow<'static,str>,String>,
    pub styles:BTreeMap<Cow<'static,str>,Cow<'static,str>>,
    pub children:Vec<HTMLChild>,
    sourceref:Option<(Ref,Ref)>,
    pub width:Option<Dim32>,
    pub(crate) font:Option<Font>,
    pub(crate) allow_newline:bool,
    pub(crate) inner_width:Option<Dim32>,
}

impl HTMLNode {
    pub(crate) fn is_empty(&self) -> bool {
        self.children.is_empty()
    }

    #[inline(always)]
    pub fn display<W:std::fmt::Write>(self,store:&mut FontStore,state:&mut ShipoutState,mut f:&mut W) -> std::fmt::Result {
        let urls = &mut state.fontlinks;
        let fnt = state.fonts.first().unwrap();
        let wd = state.widths.first().copied().unwrap();
        self.display_fmt(store,urls,fnt,wd,0,&mut f)
    }

    fn display_fmt<W:std::fmt::Write>(mut self,store:&mut FontStore,links:&mut BTreeSet<String>,current_font:&Font,top_width:Dim32,indent:usize,mut f:&mut W) -> std::fmt::Result {
        use std::fmt::Write;
        use tex_engine::engine::fontsystem::Font;
        let mut wd = match self.width {
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
                self.style_str("width","var(--document-width)");
                None
            }
            Some(w) if w != top_width => {
                let pctg = w.0 as f64 / (top_width.0 as f64);
                self.style("--temp-width",format!("calc({:.2} * var(--document-width))",pctg));
                self.class("rustex-withwidth");
                Some(w)
            }
            _ => None
        };
        let fnt = if let Some(ref font) = self.font {
            if font.filename() != current_font.filename() {
                let old = store.get_info(current_font.filename());
                let new = store.get_info(font.filename());
                match (old.map(|f| f.weblink).flatten(),new.map(|f| f.weblink).flatten()) {
                    (Some((name,_)),Some((name2,_))) if name == name2 => (),
                    (_,Some((name,link))) => {
                        links.insert(link.to_string());
                        self.styles.insert(Cow::Borrowed("font-family"),Cow::Owned(name.into()));
                    }
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
            let rel = font.get_at().0 as f32 / (current_font.get_at().0 as f32);
            if rel != 1.0 {
                self.styles.insert(Cow::Borrowed("font-size"),Cow::Owned(format!("{:.2}%",(rel * 100.0).round())));
            }
            font
        } else { current_font };
        match self.tag {
            Tag::Div => f.write_str("<div")?,
            Tag::Span => f.write_str("<span")?,
            Tag::Section => f.write_str("<section")?,
            Tag::Article => f.write_str("<article")?,
            Tag::A => f.write_str("<a")?,
            Tag::B => f.write_str("<b")?,
            Tag::I => f.write_str("<i")?,
            Tag::Table => f.write_str("<table")?,
            Tag::Tr => f.write_str("<tr")?,
            Tag::Td => f.write_str("<td")?,
            Tag::None => f.write_str("<span")?,
            Tag::Math => f.write_str("<math")?,
            Tag::Mrow => f.write_str("<mrow")?,
            Tag::Mi => f.write_str("<mi")?,
            Tag::Mo => f.write_str("<mo")?,
            Tag::Mspace => f.write_str("<mspace")?,
        }
        if !self.classes.is_empty() {
            f.write_str(" class=\"")?;
            let mut i = self.classes.iter();
            write!(f,"{}",i.next().unwrap())?;
            for c in i {
                write!(f," {}",c)?;
            }
            f.write_char('"')?;
        }
        if !self.attrs.is_empty() {
            for (k,v) in self.attrs.iter() {
                write!(f," {}=\"{}\"",k,v)?;
            }
        }
        if !self.styles.is_empty() || self.width.is_some() {
            f.write_str(" style=\"")?;
            for (k,v) in self.styles.iter() {
                write!(f,"{}:{};",k,v)?;
            }
            f.write_char('"')?;
        }
        /*if let Some((start,end)) = self.sourceref {
            write!(f," data-start=\"{}\" data-end=\"{}\"",start,end)?;
        }*/
        f.write_char('>')?;
        if let Some(w) = self.inner_width {
            let pctg = w.0 as f64 / (top_width.0 as f64);
            wd = Some(w);
            f.write_str(&format!("<span style=\"display:contents;--temp-width:calc({:.2} * var(--document-width))\">",pctg))?;
        }
        if wd.is_some() {
            f.write_str("<span class=\"rustex-contents\">")?;
        }
        for c in self.children.into_iter() {
            match c {
                HTMLChild::Node(n) => {
                    if self.allow_newline {
                        f.write_char('\n')?;
                        for _ in 0..=indent {
                            f.write_str("  ")?;
                        }
                    }
                    n.display_fmt(store,links,fnt,wd.unwrap_or(top_width),indent+1,f)?
                }
                HTMLChild::Text(s) => f.write_str(&s)?,
                HTMLChild::Glyph(g) => write!(f,"{}",g)?,
                HTMLChild::Space => f.write_char(' ')?,
                HTMLChild::EscapedSpace => f.write_str("<span class=\"rustex-space-in-hbox\">&#160;</span>")?
            }
        }
        if self.allow_newline {
            f.write_char('\n')?;
            for _ in 0..indent {
                f.write_str("  ")?;
            }
        }
        if wd.is_some() {
            f.write_str("</span>")?;
        }
        if self.inner_width.is_some() {
            f.write_str("</span>")?;
        }
        match self.tag {
            Tag::Div => f.write_str("</div>")?,
            Tag::Span => f.write_str("</span>")?,
            Tag::Section => f.write_str("</section>")?,
            Tag::Article => f.write_str("</article>")?,
            Tag::A => f.write_str("</a>")?,
            Tag::B => f.write_str("</b>")?,
            Tag::I => f.write_str("</i>")?,
            Tag::Table => f.write_str("</table>")?,
            Tag::Tr => f.write_str("</tr>")?,
            Tag::Td => f.write_str("</td>")?,
            Tag::None => f.write_str("</span>")?,
            Tag::Math => f.write_str("</math>")?,
            Tag::Mrow => f.write_str("</mrow>")?,
            Tag::Mi => f.write_str("</mi>")?,
            Tag::Mo => f.write_str("</mo>")?,
            Tag::Mspace => f.write_str("</mspace>")?,
        }
        Ok(())
    }
    pub fn push_node(&mut self,mut n:HTMLNode) {
        n.close(self.tag.ismath());
        self.children.push(HTMLChild::Node(n));
    }

    pub fn close(&mut self,in_math:bool) {
        match self.tag {
            Tag::None => {
                if self.children.len() == 1 {
                    match self.children.pop() {
                        Some(HTMLChild::Node(n)) => {
                            let tag = n.tag;
                            if self.merge(n) {
                                self.tag = tag;
                            } else {
                                if in_math {
                                    self.tag = Tag::Mrow
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
        let mut r = HTMLNode {
            tag:label.tag ,label,allow_newline,..Self::default()
        };
        if let Some(cls) = label.cls { r.class(cls); }
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
            font:None,
            inner_width:None,
        }
    }
}