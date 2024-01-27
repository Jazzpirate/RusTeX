use std::borrow::Cow;
use std::collections::{BTreeMap, BTreeSet, HashSet};
use std::fmt::{Display, Formatter};
use std::thread::current;
use tex_engine::engine::EngineTypes;
use tex_engine::engine::filesystem::{File, FileSystem};
use tex_engine::engine::filesystem::SourceReference;
use tex_engine::tex::numerics::{Dim32, Mu};
use tex_glyphs::glyphs::Glyph;
use crate::engine::{Font, SRef, Types};
use crate::fonts::FontStore;
use crate::shipout::{ShipoutMode, ShipoutState};
use std::fmt::Write;
use std::path::PathBuf;
use std::sync::Mutex;
use tex_engine::engine::fontsystem::Font as FontT;
use tex_engine::pdflatex::nodes::PDFColor;
use crate::files::RusTeXFileSystem;
use super::shipout::nodes::Alignment;


fn dim_to_px(d:i32) -> f32{
    d as f32 / 65536.0 * 1.5
}


pub(crate) fn dim_to_num(d:i32) -> String {
    format!("{:.5}",dim_to_px(d)).trim_end_matches('0').trim_end_matches('.').to_string()
}


pub(crate) fn dim_to_string(d:Dim32) -> String {
    dim_to_num(d.0) + "px"
}


pub(crate) fn mudim_to_string(d:Mu) -> String {
    format!("{:.5}",(d.0 as f32) / 18.0 / 65536.0).trim_end_matches('0').trim_end_matches('.').to_string() + "em"
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
    pub fn display_fmt(&self,store:&FontStore,files:&RusTeXFileSystem,missings:&mut HashSet<String>,indent:usize,curr_width:Dim32,in_font:&Font,do_refs:bool,f:&mut Formatter<'_>) -> std::fmt::Result {
        match self {
            HTMLChild::Node(n) => n.display_fmt(store, files, missings,indent, curr_width, in_font,do_refs, f),
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
    pub fn displayable<'a>(&'a self,store:&'a FontStore,files:&'a RusTeXFileSystem,width:Dim32,font:&'a Font,do_refs:bool) -> DisplayableNode<'a> {
        DisplayableNode {node:self,files,store,width,font,do_refs,missing_fonts:Mutex::new(HashSet::new())}
    }
    pub fn display_fmt(&self,store:&FontStore,files:&RusTeXFileSystem,missings:&mut HashSet<String>,indent:usize,curr_width:Dim32,in_font:&Font,do_refs:bool,f:&mut Formatter<'_>) -> std::fmt::Result {
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
                let (a, b) = font_attributes(store, &f, missings,if abs { None } else { Some(in_font) });
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
            c.display_fmt(store,files,missings,indent+2,wd,font,do_refs,f)?;
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


pub struct DisplayableNode<'a> {
    node:&'a HTMLNode,
    files:&'a RusTeXFileSystem,
    store:&'a FontStore,
    width:Dim32,
    font:&'a Font,
    missing_fonts:Mutex<HashSet<String>>,
    do_refs:bool
}
impl<'a> DisplayableNode<'a> {
    pub fn get_missings(mut self) -> HashSet<String> {
        std::mem::take(self.missing_fonts.get_mut().unwrap())
    }
}
impl<'a> Display for DisplayableNode<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut hs = self.missing_fonts.lock().unwrap();
        self.node.display_fmt(self.store,self.files,&mut *hs,0,self.width,self.font,self.do_refs,f)
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
    Math,MathGroup,Mo,Mi,MUnderOver,MUnder,MOver,MSubSup,MSub,MSup,MFrac,MSqrt,
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
            Math | MathGroup | Mo | Mi | MUnderOver | MUnder | MOver | MSubSup | MSub | MSup | MathEscape | MFrac | MSqrt => true,
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
            MSqrt => f.write_str("msqrt"),
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

fn font_attributes(store:&FontStore, font:&Font,missings:&mut HashSet<String>,parent:Option<&Font>) -> (Option<String>,Option<String>) {
    match parent {
        Some(f) if !f.filename().ends_with("nullfont") => {
            let old = store.get_info(f.filename());
            let old = if let Some(old) = old { old } else { return simple_font(store,missings,font) };

            let mut first = String::new();
            let mut size = ((font.get_at().0 as f32 / (f.get_at().0 as f32)) * 100.0).round();
            if size != 100.0 {
                write!(first,"font-size:{}%;",size).unwrap();
            }

            let new = store.get_info(font.filename());
            let new = if let Some(new) = new { new } else {
                missings.insert(font.filename().to_string());
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
                (_,None) => {
                    missings.insert(font.filename().to_string());
                    Some(format!("<!-- Unknown web font for {} -->",font.filename()))
                }
            };
            (if first.is_empty() { None } else { Some(first) },second)
        },
        _ => simple_font(store,missings,font)
    }
}

fn simple_font(store:&FontStore,missings:&mut HashSet<String>, font:&Font) -> (Option<String>,Option<String>) {
    match store.get_info(font.filename()) {
        None => (None,if font.filename().ends_with("nullfont") {None} else {
            missings.insert(font.filename().to_string());
            Some(format!("<!-- Unknown web font for {} -->",font.filename()))
        }),
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
                None if s.is_empty() => (None,if font.filename().ends_with("nullfont") {None} else {
                    missings.insert(font.filename().to_string());
                    Some(format!("<!-- Unknown web font for {} -->",font.filename()))
                }),
                None => (Some(s),if font.filename().ends_with("nullfont") {None} else {
                    missings.insert(font.filename().to_string());
                    Some(format!("<!-- Unknown web font for {} -->",font.filename()))
                }),
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