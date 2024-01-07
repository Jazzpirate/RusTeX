use std::borrow::Cow;
use std::collections::{BTreeMap, BTreeSet};
use std::fmt::{Display, Formatter};
use std::thread::current;
use tex_engine::engine::EngineTypes;
use tex_engine::engine::filesystem::{File, FileSystem};
use tex_engine::engine::filesystem::SourceReference;
use tex_engine::tex::numerics::{Dim32, Mu};
use tex_tfm::glyphs::Glyph;
use crate::engine::{Font, SRef, Types};
use crate::fonts::FontStore;
use crate::shipout::{ShipoutMode, ShipoutState};
use std::fmt::Write;
use std::path::PathBuf;
use tex_engine::engine::fontsystem::Font as FontT;
use tex_engine::pdflatex::nodes::PDFColor;
use crate::files::RusTeXFileSystem;
use super::shipout::nodes::Alignment;

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
    format!("{:.5}",(d.0 as f64) / 18.0 / 65536.0).trim_end_matches('0').trim_end_matches('.').to_string() + "em"
}

type Ref = SourceReference<<<Types as EngineTypes>::File as File>::SourceRefID>;

pub struct Escaper<'a,W:Write>(&'a mut W);
impl<'a,W:Write> Write for Escaper<'a,W> {
    fn write_char(&mut self, c: char) -> std::fmt::Result {
        match c {
            '<' => self.0.write_str("&lt;"),
            '>' => self.0.write_str("&gt;"),
            '&' => self.0.write_str("&amp;"),
            //'"' => self.f.write_str("&quot;"),
            //'\'' => self.f.write_str("&apos;"),
            _ => self.0.write_char(c)
        }
    }
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        for c in s.chars() { self.write_char(c)? }
        Ok(())
    }
}

#[derive(Debug)]
pub enum HTMLChild {
    Node(HTMLNode),
    Text(String),
    Comment(String),
    HRule{width:Option<Dim32>,height:Dim32,bottom:Option<Dim32>,color:PDFColor,start:SRef,end:SRef},
    VRuleS {width:Dim32,font_size:Option<Dim32>,color:PDFColor,start:SRef,end:SRef},
    VRuleC {width:Dim32,height:Dim32,depth:Option<Dim32>,color:PDFColor,start:SRef,end:SRef},
    Image {width:Dim32,height:Dim32,filepath:PathBuf}
    //Glyph(Glyph),Space,EscapedSpace
}
impl HTMLChild {
    pub fn display_fmt(&self,store:&FontStore,files:&RusTeXFileSystem,indent:usize,curr_width:Dim32,in_font:&Font,do_refs:bool,f:&mut Formatter<'_>) -> std::fmt::Result {
        match self {
            HTMLChild::Node(n) => n.display_fmt(store, files, indent, curr_width, in_font,do_refs, f),
            HTMLChild::Text(string) => f.write_str(string),
            HTMLChild::Comment(s) => f.write_str(s),
            HTMLChild::HRule { width, height, bottom, color, start, end } => {
                write!(f, "<div class=\"rustex-hrule\" style=\"height:{};{}\"><div style=\"background:{};height:{};{}\" {}></div></div>",
                       dim_to_string(*height),
                       match width {
                           None => "min-width:100%".to_string(),
                           Some(w) => format!("--rustex-scale-width:{};", (w.0 as f32) / (curr_width.0 as f32))
                       },
                       color,
                       dim_to_string(*height),
                       match bottom {
                           None => "".to_string(),
                           Some(b) => format!("margin-bottom:{};", dim_to_string(*b))
                       },
                       SourceRange(start, end, files,do_refs)
                )
            },
            HTMLChild::VRuleS { width, font_size, color, start, end } => {
                write!(f,"<div class=\"rustex-vrule\" style=\"--rustex-this-width:{};{};background:{};\" {}></div>",
                       dim_to_string(*width),
                    match font_size {
                        Some(font_size) => format!("min-height:{}",dim_to_string(*font_size)),
                        _ => "align-self:stretch".to_string()
                    },
                    color,
                    SourceRange(start,end,files,do_refs)
                )
            }
            HTMLChild::VRuleC { width, height, depth, color, start, end } => {
                write!(f,
                       "<div class=\"rustex-vrule-container\" style=\"height:{};--rustex-this-width:{};\"><div style=\"{}background:{};\"></div></div>",
                        dim_to_string(*height),
                       dim_to_string(*width),
                       /*match depth {
                           None => "-0.5ex".to_string(),
                           Some(d) => dim_to_string(-*d)
                       },*/
                       match depth {
                           None => format!("height:calc(0.5ex + {});margin-bottom:-0.5ex;", dim_to_string(*height)),
                           Some(d) => format!("margin-bottom:{};", dim_to_string(-*d))
                       },
                       color
                )
            }
            HTMLChild::Image { width, height, filepath } => {
                write!(f, "<img src=\"{}\" width=\"{}\" height=\"{}\"/>",
                       filepath.display(),
                       dim_to_string(*width),
                       dim_to_string(*height),
                )
            }
        }
    }
}

#[derive(Debug)]
pub struct HTMLNode {
    pub tag:HTMLTag,
    pub children:Vec<HTMLChild>,
    uses_font:bool,
    pub(crate) uses_color:bool,
    pub color:Option<PDFColor>,
    pub font:Option<(Font,bool)>, // true = absolute, false = relative
    pub width:Option<(Dim32,bool)>, // true = absolute, false = relative
    pub classes:Vec<Cow<'static,str>>,
    pub attrs:BTreeMap<Cow<'static,str>,String>,
    pub styles:BTreeMap<Cow<'static,str>,Cow<'static,str>>,
    pub sourceref:Option<(Ref,Ref)>,
}

impl HTMLNode {
    pub fn reopened(&self) -> Self {
        HTMLNode {
            tag:self.tag.clone(),
            children:Vec::new(),
            uses_font:self.uses_font,
            uses_color:self.uses_color,
            color:self.color.clone(),
            font:self.font.clone(),
            width:self.width.clone(),
            classes:self.classes.clone(),
            attrs:self.attrs.clone(),
            styles:self.styles.clone(),
            sourceref:self.sourceref.clone()
        }
    }
    pub fn new(tag:HTMLTag) -> Self {
        HTMLNode {
            tag,
            children:Vec::new(),
            uses_font:false,
            uses_color:false,
            color:None,font:None,
            width:None,
            classes:Vec::new(),
            attrs:BTreeMap::new(),
            styles:BTreeMap::new(),
            sourceref:None
        }
    }
    pub fn page() -> Self {
        HTMLNode {
            tag:HTMLTag::Page,
            children:Vec::new(),
            uses_font:false,
            uses_color:false,
            color:None,font:None,
            width:None,
            classes:vec!("rustex-page".into()),
            attrs:BTreeMap::new(),
            styles:BTreeMap::new(),
            sourceref:None
        }
    }
    pub fn push_glyph(&mut self,mode:ShipoutMode,g:Glyph) {
        match self.children.last_mut() {
            Some(HTMLChild::Text(string)) => {
                write!(Escaper(string),"{}",g).unwrap();
            }
            _ => {
                let mut s = String::new();
                write!(Escaper(&mut s),"{}",g).unwrap();
                self.push_child(mode,HTMLChild::Text(s));
            }
        }
    }
    pub fn push_space(&mut self,mode:ShipoutMode) {
        match self.children.last_mut() {
            Some(HTMLChild::Text(string)) if string.ends_with(char::is_whitespace) => {
                string.push_str("&nbsp;");
            }
            Some(HTMLChild::Text(string)) => {
                string.push(' ');
            }
            _ => {
                self.push_child(mode,HTMLChild::Text(" ".to_string()));
            }
        }
    }
    pub fn paragraph(start:SRef,end:SRef,width:Dim32) -> Self {
        HTMLNode {
            tag:HTMLTag::Paragraph,
            children:Vec::new(),
            uses_font:false,
            uses_color:false,
            color:None,font:None,
            sourceref:Some((start,end)),
            width:Some((width,false)),
            classes:vec!("rustex-paragraph".into()),
            attrs:BTreeMap::new(),
            styles:BTreeMap::new(),
        }
    }
    pub fn push_comment<D:Display>(&mut self,d:D) {
        match self.children.last_mut() {
            Some(HTMLChild::Comment(string)) => {
                write!(string,"{}",d).unwrap();
            }
            _ => {
                let mut s = String::new();
                write!(s,"{}",d).unwrap();
                self.children.push(HTMLChild::Comment(s));
            }
        }
    }
    pub fn push_string<D:Display>(&mut self,d:D,mode:ShipoutMode,escape:bool) {
        match self.children.last_mut() {
            Some(HTMLChild::Text(string)) => {
                if escape {
                    write!(Escaper(string),"{}",d).unwrap();
                } else {
                    write!(string,"{}",d).unwrap();
                }
            }
            _ => {
                let mut s = String::new();
                if escape {
                    write!(Escaper(&mut s),"{}",d).unwrap();
                } else {
                    write!(s,"{}",d).unwrap();
                }
                self.push_child(mode,HTMLChild::Text(s));
            }
        }
    }
    pub fn push_child(&mut self,mode:ShipoutMode,ch:HTMLChild) {
        if mode == ShipoutMode::SVG {
            self.children.push(ch)
        } else {
            match ch {
                t @ HTMLChild::Text { .. } => {
                    if mode != ShipoutMode::Math && self.font.is_none() { self.uses_font = true }
                    if self.color.is_none() { self.uses_color = true }
                    self.children.push(t);
                }
                HTMLChild::Node(n) => return self.push_open_node(mode, n),
                o => self.children.push(o)
            }
        }
    }

    pub fn push_open_node(&mut self,mode:ShipoutMode,mut n:HTMLNode) {
        if self.color.is_none() {
            self.uses_color = self.uses_color || n.uses_color;
        }
        if self.font.is_none() {
            self.uses_font = self.uses_font || n.uses_font;
        }
        n.close(mode,&mut self.children);
    }
    pub fn close(self,mode:ShipoutMode,parent:&mut Vec<HTMLChild>) {
        match self.tag {
            HTMLTag::FontChange(_) => close_font(self,mode,parent),
            HTMLTag::ColorChange(_) => close_color(self,mode,parent),
            HTMLTag::Annot(_) => close_annot(self,mode,parent),
            _ if self.children.len() == 1 => match &self.children[0] {
                HTMLChild::Node(n) if matches!(n.tag,HTMLTag::FontChange(_)|HTMLTag::ColorChange(_))
                => merge_annotation(self, mode, parent),
                _ => parent.push(HTMLChild::Node(self))
            },
            _ => parent.push(HTMLChild::Node(self))
        }
    }
    pub fn displayable<'a>(&'a self,store:&'a FontStore,files:&'a RusTeXFileSystem,width:Dim32,font:&'a Font,do_refs:bool) -> impl Display + 'a {
        DisplayableNode {node:self,files,store,width,font,do_refs}
    }
    pub fn display_fmt(&self,store:&FontStore,files:&RusTeXFileSystem,indent:usize,curr_width:Dim32,in_font:&Font,do_refs:bool,f:&mut Formatter<'_>) -> std::fmt::Result {
        write!(f,"<{}",self.tag)?;
        let (widthcls,widthsstyle,wd,scaled) = if let Some((wd,abs)) = self.width {
            if wd == Dim32(0) {
                (None,Some("max-width:0;".to_string()),curr_width,false)
            } else if wd < Dim32(0) {
                (None,Some(format!("max-width:0;margin-right:{};",dim_to_string(wd))),curr_width,false)
            } else if abs {
                (Some("rustex-withwidth"),Some(format!("--rustex-this-width:{};",dim_to_num(wd.0))),wd,false)
            } else {
                let pctg = wd.0 as f32 / (curr_width.0 as f32);
                (Some("rustex-scalewidth"),Some(format!("--rustex-scale-width:{};",format!("{:.2}",pctg))),wd,true)
            }
        } else { (None,None,curr_width,false) };

        if !self.classes.is_empty() || widthcls.is_some() {
            f.write_str(" class=\"")?;
            let mut i = self.classes.iter();
            if let Some(c) = widthcls {
                f.write_str(c)?
            } else {
                f.write_str(i.next().unwrap())?;
            }
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
        let (font,font_style,missing) = if let Some((ref f,abs)) = self.font {
            if !abs && in_font == f {
                (in_font,None,None)
            } else {
                let (a, b) = font_attributes(store, &f, if abs { None } else { Some(in_font) });
                (f, a, b)
            }
        } else { (in_font,None,None) };

        if !self.styles.is_empty() || widthsstyle.is_some() || font_style.is_some() || self.color.is_some() {
            f.write_str(" style=\"")?;
            if let Some(s) = font_style {
                f.write_str(&s)?;
            }
            if let Some(wd) = widthsstyle {
                f.write_str(&wd)?;
            }
            if let Some(c) = self.color {
                write!(f,"color:{};",c)?;
            }
            for (k,v) in self.styles.iter() {
                write!(f,"{}:{};",k,v)?;
            }
            f.write_char('"')?;
        }
        if do_refs {
            if let Some((start, end)) = &self.sourceref {
                f.write_char(' ')?;
                do_sourceref(files, start, end, f)?;
            }
        }
        f.write_char('>')?;
        if scaled {
            f.write_str("<span>")?;
        }
        if let Some(missing) = missing {
            f.write_str(&missing)?;
        }
        for c in &self.children {
            if self.tag.allow_linebreak() {
                do_indent(indent+2,f)?;
            }
            c.display_fmt(store,files,indent+2,wd,font,do_refs,f)?;
        }
        if scaled {
            f.write_str("</span>")?;
        }
        if self.tag.allow_linebreak() {
            do_indent(indent,f)?;
        }
        write!(f,"</{}>",self.tag)
    }
}


fn close_annot(mut node:HTMLNode, mode:ShipoutMode, parent:&mut Vec<HTMLChild>) {
    if node.children.len() == 1 {
        match node.children.first().unwrap() {
            HTMLChild::Node(_) => {
                if let Some(HTMLChild::Node(mut n)) = node.children.pop() {
                    if node.attrs.iter().any(|(k,_)| {
                        n.attrs.contains_key(k)
                    }) {
                        node.children.push(HTMLChild::Node(n));
                        parent.push(HTMLChild::Node(node));
                        return
                    }
                    for (k,v) in node.attrs {
                        n.attrs.insert(k,v);
                    }
                    match (node.color,n.color) {
                        (Some(c),None) => {
                            n.color = Some(c);
                            n.uses_color = false;
                        }
                        _ => {}
                    }
                    match (node.font,&n.font) {
                        (Some(c),None) => {
                            n.font = Some(c);
                            n.uses_font = false;
                        }
                        _ => {}
                    }
                    for s in node.styles {
                        n.styles.insert(s.0,s.1);
                    }
                    parent.push(HTMLChild::Node(n));
                } else {unreachable!()}
            }
            _ => parent.push(HTMLChild::Node(node))
        }
    }
}

fn merge_annotation(mut node:HTMLNode, _mode:ShipoutMode, parent:&mut Vec<HTMLChild>) {
    match node.children.pop() {
        Some(HTMLChild::Node(n))
        if matches!(n.tag,HTMLTag::ColorChange(_)|HTMLTag::FontChange(_)) => {
            if n.attrs.iter().any(|(k,_)| {
                node.attrs.contains_key(k)
            }) {
                node.children.push(HTMLChild::Node(n));
                parent.push(HTMLChild::Node(node));
                return
            }
            for (k,v) in n.attrs {
                node.attrs.insert(k,v);
            }
            if n.color.is_some() {
                node.color = n.color;
                node.uses_color = false;
            }
            if n.font.is_some() {
                node.font = n.font;
                node.uses_font = false;
            }
            for s in n.styles {
                node.styles.insert(s.0,s.1);
            }
            node.children = n.children;
            parent.push(HTMLChild::Node(node));
        }
        _ => unreachable!()
    }
}

fn close_font(mut annot:HTMLNode,mode:ShipoutMode,parent:&mut Vec<HTMLChild>) {
    let mut has_text = false;
    let num = annot.children.iter().filter(|c| match c {
        HTMLChild::Node(n) => n.uses_font,
        HTMLChild::Text {..} => {has_text = true ;true},
        _ => false
    }).count();
    match num {
        0 => parent.extend(annot.children),
        1 if !has_text => {
            if let Some(c) = annot.children.iter_mut().find_map(|c| match c {
                HTMLChild::Node(n) if n.uses_font => Some(n),
                _ => None
            }) {
                assert!(c.font.is_none());
                c.font = annot.font;
                c.uses_font = false;
            }
            parent.extend(annot.children);
        }
        _ => match &mut annot.tag {
            HTMLTag::FontChange(m) => {
                *m = mode;
                parent.push(HTMLChild::Node(annot));
            }
            _ => unreachable!()
        }
    }
}

fn close_color(mut annot:HTMLNode,mode:ShipoutMode,parent:&mut Vec<HTMLChild>) {
    let mut has_text = false;
    let num = annot.children.iter().filter(|c| match c {
        HTMLChild::Node(n) => n.uses_color,
        HTMLChild::Text {..} => {has_text = true ;true},
        _ => false
    }).count();
    match num {
        0 => parent.extend(annot.children),
        1 if !has_text => {
            if let Some(c) = annot.children.iter_mut().find_map(|c| match c {
                HTMLChild::Node(n) if n.uses_color => Some(n),
                _ => None
            }) {
                assert!(c.color.is_none());
                c.uses_color = false;
                c.color = annot.color;
            }
            parent.extend(annot.children);
        }
        _ => match &mut annot.tag {
            HTMLTag::ColorChange(m) => {
                *m = mode;
                parent.push(HTMLChild::Node(annot));
            }
            _ => unreachable!()
        }
    }
}


struct DisplayableNode<'a> {
    node:&'a HTMLNode,
    files:&'a RusTeXFileSystem,
    store:&'a FontStore,
    width:Dim32,
    font:&'a Font,
    do_refs:bool
}
impl<'a> Display for DisplayableNode<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.node.display_fmt(self.store,self.files,0,self.width,self.font,self.do_refs,f)
    }
}

#[derive(Debug,PartialEq,Eq,Clone)]
pub enum HTMLTag {
    Page,Paragraph,
    VBoxContainer,VBoxHeight,VBox,
    VTopContainer,VTopHeight,VTop,
    VCenterContainer,VCenter,
    HBoxContainer,HBox,Display,Raise,MoveLeft,
    HAlign,HBody,HRow,HCell,NoAlignH,
    FontChange(ShipoutMode),ColorChange(ShipoutMode),Link(ShipoutMode),
    Annot(ShipoutMode),
    Matrix(ShipoutMode),
    Dest(ShipoutMode),
    Math,MathGroup,Mo,Mi,MUnderOver,MUnder,MOver,MSubSup,MSub,MSup,MFrac,
    MathEscape,
    SvgWrap,SvgG(String),SvgTop,SvgForeign,EscapeSvg
}
impl HTMLTag {
    fn allow_linebreak(&self) -> bool {
        use HTMLTag::*;
        match self {
            HBoxContainer | HBox | Paragraph | Mi | Mo => false,
            FontChange(m) | ColorChange(m) | Link(m) |
            Matrix(m) | Annot(m) => !m.is_h(),
            _ => true
        }
    }
    fn is_math(&self) -> bool {
        use HTMLTag::*;
        match self {
            Math | MathGroup | Mo | Mi | MUnderOver | MUnder | MOver | MSubSup | MSub | MSup | MathEscape | MFrac => true,
            _ => false
        }
    }
}
impl Display for HTMLTag {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use HTMLTag::*;
        match self {
            Page => f.write_str("article"),
            Paragraph | VBoxContainer | VBoxHeight | VBox | VTopContainer | VTopHeight | VTop |
            HBoxContainer | HBox | Display | Raise | MoveLeft | VCenterContainer | VCenter |
            SvgWrap | EscapeSvg => f.write_str("div"),
            Math => f.write_str("math"),
            MathGroup => f.write_str("mrow"),
            MathEscape => f.write_str("mtext"),
            MFrac => f.write_str("mfrac"),
            HAlign => f.write_str("table"),
            HBody => f.write_str("tbody"),
            HRow => f.write_str("tr"),
            HCell => f.write_str("td"),
            NoAlignH => f.write_str("td"),
            SvgG(s) => f.write_str(s),
            SvgTop => f.write_str("svg"),
            SvgForeign => f.write_str("foreignObject"),
            Mo => f.write_str("mo"),
            Mi => f.write_str("mi"),
            MUnderOver => f.write_str("munderover"),
            MUnder => f.write_str("munder"),
            MOver => f.write_str("mover"),
            MSubSup => f.write_str("msubsup"),
            MSub => f.write_str("msub"),
            MSup => f.write_str("msup"),
            FontChange(mode) | ColorChange(mode) | Annot(mode) | Matrix(mode) => {
                if *mode == ShipoutMode::Math { f.write_str("mrow") }
                else if *mode == ShipoutMode::SVG { f.write_str("g") }
                else { f.write_str("span") }
            }
            Link(mode) => {
                if *mode == ShipoutMode::Math { f.write_str("mrow") }
                else if *mode == ShipoutMode::SVG { f.write_str("g") }
                else { f.write_str("a") }
            }
            Dest(mode) => {
                if *mode == ShipoutMode::Math { f.write_str("mspace") }
                else if *mode == ShipoutMode::SVG { f.write_str("g") }
                else { f.write_str("a") }
            }
        }
    }
}

fn do_indent(indent:usize,f:&mut Formatter<'_>) -> std::fmt::Result {
    f.write_char('\n')?;
    for _ in 0..indent { f.write_str("  ")?;}
    Ok(())
}


struct DisplaySourceRef<'a> {
    files:&'a RusTeXFileSystem,
    start:&'a SRef,
    end:&'a SRef
}
impl<'a> Display for DisplaySourceRef<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

fn font_attributes(store:&FontStore, font:&Font,parent:Option<&Font>) -> (Option<String>,Option<String>) {
    match parent {
        Some(f) if !f.filename().ends_with("nullfont") => {
            let old = store.get_info(f.filename());
            let old = if let Some(old) = old { old } else { return simple_font(store,font) };

            let mut first = String::new();
            let mut size = ((font.get_at().0 as f32 / (f.get_at().0 as f32)) * 100.0).round();
            if size != 100.0 {
                write!(first,"font-size:{}%;",size).unwrap();
            }

            let new = store.get_info(font.filename());
            let new = if let Some(new) = new { new } else {
                return (if first.is_empty() {None} else {Some(first)},Some(format!("<!-- Unknown web font for {} -->",font.filename())))
            };
            if new.styles.capitals && !old.styles.capitals {
                first.push_str("font-variant:small-caps;");
            } else if old.styles.capitals && !new.styles.capitals {
                first.push_str("font-variant:normal;");
            }
            if new.styles.bold && !old.styles.bold {
                first.push_str("font-weight:bold;");
            } else if old.styles.bold && !new.styles.bold {
                first.push_str("font-weight:normal;");
            }
            if new.styles.italic && !old.styles.italic {
                first.push_str("font-style:italic;");
            } else if new.styles.oblique && !old.styles.oblique {
                first.push_str("font-style:oblique;");
            } else if (old.styles.italic || old.styles.oblique) && !(new.styles.italic || new.styles.oblique) {
                first.push_str("font-style:normal;");
            }
            let second = match (old.weblink,new.weblink) {
                (Some((name,_)),Some((newname,_))) if name == newname => None,
                (_,Some((name,_))) => {
                    write!(first,"font-family:{};",name).unwrap();
                    None
                }
                (_,None) => Some(format!("<!-- Unknown web font for {} -->",font.filename()))
            };
            (if first.is_empty() { None } else { Some(first) },second)
        },
        _ => simple_font(store,font)
    }
}

fn simple_font(store:&FontStore, font:&Font) -> (Option<String>,Option<String>) {
    match store.get_info(font.filename()) {
        None => (None,if font.filename().ends_with("nullfont") {None} else {Some(format!("<!-- Unknown web font for {} -->",font.filename()))}),
        Some(info) => {
            let mut s = String::new();
            write!(s,"font-size:{};",dim_to_string(font.get_at())).unwrap();
            if info.styles.capitals { s.push_str("font-variant:small-caps;"); }
            else { s.push_str("font-variant:normal;");}
            if info.styles.bold { s.push_str("font-weight:bold;"); }
            else { s.push_str("font-weight:normal;"); }
            if info.styles.italic { s.push_str("font-style:italic;"); }
            else if info.styles.oblique { s.push_str("font-style:oblique;"); }
            else { s.push_str("font-style:normal;");}
            match info.weblink {
                None if s.is_empty() => (None,if font.filename().ends_with("nullfont") {None} else {Some(format!("<!-- Unknown web font for {} -->",font.filename()))}),
                None => (Some(s),if font.filename().ends_with("nullfont") {None} else {Some(format!("<!-- Unknown web font for {} -->",font.filename()))}),
                Some((name,_)) => {
                    write!(s,"font-family:{};",name).unwrap();
                    (Some(s),None)
                }
            }
        }
    }
}

fn do_sourceref(files:&RusTeXFileSystem,start:&SRef,end:&SRef,f:&mut Formatter<'_>) -> std::fmt::Result {
    write!(f,"data-rustex-sourceref=\"{}#({};{}):({};{})\"",files.ref_str(start.file),start.line,start.column,end.line,end.column)
}

struct SourceRange<'a>(&'a SRef,&'a SRef,&'a RusTeXFileSystem,bool);
impl<'a> Display for SourceRange<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.3 {do_sourceref(self.2,self.0,self.1,f) } else {Ok(())}
    }
}

/*
fn close_tag_with_font(store:&FontStore, font:&Font, f:&mut Formatter<'_>) -> std::fmt::Result {
    match store.get_info(font.filename()) {
        None => write!(f,"\"><!-- Unknown web font for {} -->",font.filename()),
        Some(info) => {
            if info.styles.capitals { f.write_str("font-variant:small-caps;")?; }
            if info.styles.bold { f.write_str("font-weight:bold;")?; }
            if info.styles.italic { f.write_str("font-style:italic;")?; }
            else if info.styles.oblique { f.write_str("font-style:oblique;")?; }
            match info.weblink {
                None => write!(f,"\"><!-- Unknown web font for {} -->",font.filename()),
                Some((name,_)) => {
                    write!(f,"font-family:{};\">",name)
                }
            }
        }
    }
}

 */

/*

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
    pub(crate) const VTOP_CONTAINER: Label = Label { id: 53, cls: Some("rustex-vtop-container"), tag: Tag::Div };
    pub(crate) const VTOP_HEIGHT_CONTAINER: Label = Label { id: 54, cls: Some("rustex-vtop-height-container"), tag: Tag::Div };
    pub(crate) const VTOP_INNER: Label = Label { id: 55, cls: Some("rustex-vtop"), tag: Tag::Div };
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
    pub children:Vec<HTMLChild>,
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

 */