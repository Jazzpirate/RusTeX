pub(crate) mod nodes;
mod annotations;

use std::borrow::Cow;
use std::collections::BTreeSet;
use tex_engine::tex::types::{BoxType, MathClass};
use crate::engine::{Bx, Font, Refs, SRef, Types};
use std::fmt::Write;
use std::vec::IntoIter;
use tex_engine::pdflatex::nodes::{PDFColor, PDFDest, PDFNode};
use tex_engine::engine::{EngineReferences, EngineTypes};
use tex_engine::engine::filesystem::{File, SourceReference};
use tex_engine::engine::utils::memory::{InternedCSName, PRIMITIVES};
use tex_engine::tex::nodes::NodeTrait;
use tex_engine::tex::numerics::{Dim32, Fill, Mu, MuSkip, MuSkip32, Skip32};
use tex_engine::engine::state::State;
use crate::html::{dim_to_num, dim_to_string, HTMLChild, HTMLNode, HTMLTag};
use tex_engine::engine::fontsystem::{Font as FontT, FontSystem};
use tex_engine::tex::nodes::boxes::{BoxInfo, HBoxInfo, TeXBox, ToOrSpread, VBoxInfo};
use tex_engine::tex::nodes::horizontal::HNode;
use tex_engine::tex::nodes::math::{MathFontStyle, MathGroup, MathKernel, MathNode, MathNucleus};
use tex_engine::tex::nodes::vertical::VNode;
use tex_tfm::fontstyles::ModifierSeq;
use tex_tfm::glyphs::Glyph;
use crate::nodes::{LineSkip, RusTeXNode};
use crate::shipout::nodes::{MuAdd, SkipAdd};

pub(crate) struct ExtensibleIter<T> {
    curr:IntoIter<T>,
    next:Vec<IntoIter<T>>

}
impl<T> ExtensibleIter<T> {
    pub fn prefix(&mut self,vec:Vec<T>) {
        let old = std::mem::replace(&mut self.curr,vec.into_iter());
        self.next.push(old);
    }
}
impl<T> From<Vec<T>> for ExtensibleIter<T> {
    fn from(v: Vec<T>) -> Self {
        Self { curr:v.into_iter(),next:Vec::new() }
    }
}
impl<T> Iterator for ExtensibleIter<T> {
    type Item = T;
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(n) = self.curr.next() { return Some(n) }
        while let Some(mut it) = self.next.pop() {
            if let Some(n) = it.next() {
                self.curr = it;
                return Some(n)
            }
        }
        None
    }
}

pub(crate) type VNodes = ExtensibleIter<VNode<Types>>;
pub(crate) type HNodes = ExtensibleIter<HNode<Types>>;
pub(crate) type MNodes = ExtensibleIter<MNode>;
pub(crate) type MNode = MathNode<Types,MathFontStyle<Types>>;

#[derive(Copy,Clone,PartialEq,Eq,Debug)]
pub enum ShipoutMode {Top,V,H{escape:bool},Par,Math,SVG}
impl ShipoutMode {
    pub fn is_v(&self) -> bool {
        match self {
            ShipoutMode::V | ShipoutMode::Top => true,
            _ => false
        }
    }
    pub fn is_h(&self) -> bool {
        match self {
            ShipoutMode::H{..} | ShipoutMode::Par => true,
            _ => false
        }
    }
}

pub(crate) struct ShipoutState {
    pub(crate) done:String,
    pub(crate) output:Vec<HTMLChild>,
    pub(crate) nodes:Vec<HTMLNode>,
    pub(crate) fonts: Vec<Font>,
    pub(crate) colors:Vec<PDFColor>,
    pub(crate) widths: Vec<Dim32>,
    pub(crate) lineskip:Vec<LineSkip>,
    pub(crate) modes:Vec<ShipoutMode>,
    pub(crate) fontlinks:BTreeSet<String>,
    pub(crate) in_content:bool,
    pub(crate) nullfont:Option<Font>
}
impl Default for ShipoutState {
    fn default() -> Self {
        Self {
            done:String::new(),
            output:Vec::new(),
            nodes:Vec::new(),
            fonts:Vec::new(),
            widths:Vec::new(),
            colors:vec!(PDFColor::black()),
            fontlinks:BTreeSet::new(),
            modes:vec!(ShipoutMode::Top),
            lineskip:Vec::new(),
            in_content:false,
            nullfont:None
        }
    }
}
impl ShipoutState {
    fn mode(&self) -> ShipoutMode {
        *self.modes.last().unwrap()
    }
    fn do_in_and<F1:FnOnce(&mut Self)>(&mut self, node:HTMLNode,mode:Option<ShipoutMode>, cont:F1) -> HTMLNode {
        let disc = node.tag;
        self.nodes.push(node);
        if let Some(mode) = mode {
            self.modes.push(mode);
        }
        cont(self);
        let node = annotations::close_all(self.mode(),&mut self.nodes);
        if let Some(_) = mode {
            self.modes.pop();
        }
        if node.tag == disc {
            node
        } else {
            todo!()
        }
    }

    fn do_in<F1:FnOnce(&mut Self)>(&mut self, node:HTMLNode,mode:Option<ShipoutMode>, cont:F1) {
        let r = self.do_in_and(node, mode,cont);
        self.push(r);
    }

    fn push_child(&mut self, child:HTMLChild) {
        let mode = self.mode();
        match self.nodes.last_mut() {
            Some(parent) => parent.push_child(mode,child),
            None => self.output.push(child),
        }
    }
    fn push_space(&mut self) {
        let mode = self.mode();
        match self.nodes.last_mut() {
            Some(parent) => parent.push_space(mode),
            None => unreachable!()
        }
    }
    fn push_string<D:std::fmt::Display>(&mut self, d:D,escape:bool) {
        let mode = self.mode();
        match self.nodes.last_mut() {
            Some(parent) => parent.push_string(d,mode,escape),
            None => unreachable!()
        }
    }
    fn push_comment<D:std::fmt::Display>(&mut self, d:D) {
        match self.nodes.last_mut() {
            Some(parent) => parent.push_comment(d),
            None => unreachable!()
        }
    }

    fn push(&mut self, node:HTMLNode) {
        let mode = self.mode();
        match self.nodes.last_mut() {
            Some(parent) => parent.push_open_node(mode,node),
            None => node.close(mode,&mut self.output),
        }
    }
    #[inline(always)]
    fn push_glyph(&mut self,glyph:Glyph) {
        let mode = self.mode();
        self.nodes.last_mut().unwrap().push_glyph(mode,glyph)
    }
}


fn split_state<R,F:FnOnce(Refs,&mut ShipoutState) -> R>(engine:Refs,f:F) -> R {
    let mut state = std::mem::take(&mut engine.aux.extension.state);
    if state.nullfont.is_none() {
        state.nullfont = Some(engine.fontsystem.null())
    }
    if state.fonts.is_empty() {
        state.fonts.push(engine.state.get_current_font().clone());
    }
    if state.widths.is_empty() {
        state.widths.push(engine.state.get_primitive_dim(PRIMITIVES.hsize));
    }
    let r = f(engine,&mut state);
    engine.aux.extension.state = state;
    r
}


const TEST_FILE: &str = "/home/jazzpirate/work/Software/sTeX/RusTeXNew/test/test.html";
const PREAMBLE: &str = r#"
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <title>example</title>
  <link rel="stylesheet" type="text/css" href="file:///home/jazzpirate/work/Software/sTeX/RusTeXNew/rustex/src/resources/html.css">

  <link rel="stylesheet" type="text/css" href="https://cdn.jsdelivr.net/gh/dreampulse/computer-modern-web-font@master/font/Serif/cmun-serif.css">
  <link rel="stylesheet" type="text/css" href="https://fonts.cdnfonts.com/css/latin-modern-roman">
  <link rel="stylesheet" type="text/css" href="https://fonts.cdnfonts.com/css/latin-modern-sans">
  <link rel="stylesheet" type="text/css" href="https://fonts.cdnfonts.com/css/latin-modern-mono">
</head>
<body>
"#;
const POSTAMBLE: &str = r#"
</body>
</html>
"#;

pub(crate) fn make_page<F:FnOnce(Refs,&mut ShipoutState)>(engine:Refs,state:&mut ShipoutState,f:F,start:SRef,end:SRef) -> HTMLNode {
    let mut page = state.do_in_and(HTMLNode::page(start,end),None,|state| {
        f(engine,state)
    });
    let page_width = engine.state.get_primitive_dim(PRIMITIVES.pdfpagewidth);
    let text_width = engine.state.get_primitive_dim(PRIMITIVES.hsize);
    let top_font = state.fonts.first().unwrap();
    page.font = Some((top_font.clone(),true));
    page.styles.insert("--rustex-text-width".into(),dim_to_num(text_width.0).into());
    page.styles.insert("--rustex-page-width".into(),dim_to_num(page_width.0).into());
    page.styles.insert("line-height".into(),"1.2".into()); // TODO
    page
}

/*
pub(crate) fn make_page<F:FnOnce(Refs,&mut ShipoutState)>(engine:Refs,state:&mut ShipoutState,f:F,start:SRef,end:SRef) -> HTMLNode {
    let mut node = HTMLNode::new(crate::html::labels::PAGE, true);
    node.sourceref(start,end);
    state.do_in(node,|state| {
        f(engine,state)
    },|state,node| if node.label == crate::html::labels::PAGE {state.output.push(HTMLChild::Node(node));None} else {
        todo!()
    });
    let mut page = if let Some(HTMLChild::Node(page)) = state.output.pop() {page} else {unreachable!()};
    let mut inner = HTMLNode::new(crate::html::labels::INNER_PAGE, true);
    inner.children = std::mem::take(&mut page.children);

    let pagewidth = engine.state.get_primitive_dim(PRIMITIVES.pdfpagewidth);
    page.style("max-width",dim_to_string(pagewidth));
    let font = state.fonts.first().unwrap();
    inner.style("max-width",dim_to_string(pagewidth));
    let textwidth = state.widths.first().copied().unwrap();
    let scale = (textwidth.0 as f64) / (pagewidth.0 as f64);
    inner.style("--document-width",format!("calc({:.2} * min(100vw,{}))",scale,dim_to_string(pagewidth)));
    let margin = (pagewidth.0 as f64 - (textwidth.0 as f64)) / (2.0 * pagewidth.0 as f64) * 100.0;
    inner.style("padding-left",format!("{:.2}%",margin));
    inner.style("padding-right",format!("{:.2}%",margin));
    inner.style("line-height",format!("{:.2}",state.lineskip.factor(font) / (font.get_at().0 as f64)));
    page.children.push(HTMLChild::Node(inner));

    page.style("font-size",dim_to_string(font.get_at()));
    let store = &mut engine.fontsystem.glyphmaps;
    let fi = store.get_info(font.filename());
    match fi.map(|f| f.weblink).flatten() {
        Some((name,link)) => {
            state.fontlinks.insert(link.to_string());
            page.style("font-family",name.into());
        }
        _ => {
            page.children.insert(0,HTMLChild::Text(format!("<!-- Unknown web font for {} -->",font.filename())));
        }
    }
    page
}

 */
pub(crate) fn shipout_paginated(engine:Refs, n: VNode<Types>) {
    todo!() /*
    match n {
        VNode::Box(TeXBox::V {children,start,end,..}) => split_state(engine,|engine,state|{
            let vec = children.into_vec();
            let page = make_page(engine,state,|engine,state| {
                do_vlist(engine,state,&mut vec.into(),true)
            },start,end);

            let mut done = std::mem::take(&mut state.done);
            page.display(&mut engine.fontsystem.glyphmaps,state,&mut done).unwrap();


            state.done = done;

            std::fs::write(TEST_FILE,&format!("{}{}{}",PREAMBLE,state.done,POSTAMBLE)).unwrap();
            println!("HERE")
        }),
        _ => unreachable!()
    }
    */
}

fn get_inner_page(bx:TeXBox<Types>) -> Option<Vec<VNode<Types>>> {
    match bx {
        TeXBox::V {children,info:VBoxInfo::VBox {moved_left:Some(_),..},..} => Some(children.into_vec()),
        TeXBox::V {children,..} => {
            for c in children.into_vec().into_iter() { match c {
                VNode::Box(bx) => match get_inner_page(bx) {
                    None => (),
                    Some(v) => return Some(v)
                }
                _ => ()
            }}
            None
        }
        _ => None
    }
}
fn get_page_inner(children:Box<[VNode<Types>]>) -> Vec<VNode<Types>> {
    let mut ret = Vec::new();
    let vec = children.into_vec();
    let mut list:VNodes = vec.into();
    let mut in_page:usize = 0;

    while let Some(c) = list.next() {
        match c {
            VNode::Box(TeXBox::V {children,..}) if in_page == 0 => list.prefix(children.into_vec()),
            VNode::Custom(RusTeXNode::PageBegin) => in_page += 1,
            VNode::Custom(RusTeXNode::PageEnd) => in_page -= 1,
            VNode::Custom(_) | VNode::Whatsit(_) => ret.push(c),
            _ if in_page == 0 => (),
            _ => ret.push(c)
        }
    }
    ret
}
#[inline(always)]
pub(crate) fn shipout(engine:Refs, n: VNode<Types>) {
    match n {
        VNode::Box(TeXBox::V { children, start,end, .. }) => {
            let children = get_page_inner(children);
            split_state(engine, |engine, state| {
                do_vlist(engine, state, &mut children.into(), true);
                let out = std::mem::take(&mut state.output);
                let mut page = make_page(engine,state,|_,state| {
                    for c in out {state.push_child(c)}
                },start,end);
                page.classes = vec!("rustex-body".into());
                let null = engine.fontsystem.null();
                std::fs::write(TEST_FILE,&format!("{}{}{}",PREAMBLE,
                    page.displayable(&engine.fontsystem.glyphmaps,&engine.filesystem,*state.widths.first().unwrap(),&null),
                    POSTAMBLE)).unwrap();
                println!("HERE");
                state.output = page.children;
            });
        }
        _ => unreachable!()
    }
    /*
    match n {
        VNode::Box(TeXBox::V { children, start, end, .. }) => {
            let children = get_page_inner(children);
            split_state(engine, |engine, state| {
                let node = HTMLNode::new(crate::html::labels::PAGE, true);
                state.do_in(node, |state| {
                    do_vlist(engine, state, &mut children.into(), true)
                }, |state, node| if node.label == crate::html::labels::PAGE { state.output.push(HTMLChild::Node(node));None } else {
                    todo!()
                });;
                let mut done = std::mem::take(&mut state.done);
                let nodes = if let Some(HTMLChild::Node(n)) = state.output.pop() { n.children } else { unreachable!() };
                for c in nodes {
                    match c {
                        HTMLChild::Node(n) => {
                            n.display(&mut engine.fontsystem.glyphmaps, state, &mut done).unwrap();
                            done.write_char('\n').unwrap();
                        },
                        _ => ()
                    }
                }
                state.done = done;
                let page = make_page(engine,state,|engine,state| {
                    state.nodes.last_mut().unwrap().push_text(state.done.clone())
                },start,end);

                let mut out = String::new();
                page.display(&mut engine.fontsystem.glyphmaps,state,&mut out).unwrap();

                std::fs::write(TEST_FILE,&format!("{}{}{}",PREAMBLE,out,POSTAMBLE)).unwrap();
                println!("HERE")
            });
        }
        _ => unreachable!()
    }
    */
}

pub(crate) const ZERO_SKIP: Skip32<Dim32> = Skip32 {base:Dim32(0),stretch:None,shrink:None};

fn do_vlist(engine:Refs, state:&mut ShipoutState, children:&mut VNodes,mut empty: bool) {
    let mut currskip = ZERO_SKIP;
    while let Some(c) = children.next() {
        match c {
            VNode::VKern(kn) => { currskip.base = currskip.base + kn; }
            VNode::VSkip(sk) => { currskip.merge(sk) }
            VNode::VFil => { currskip.set_fil() }
            VNode::VFill => { currskip.set_fill() }
            VNode::Vss => { currskip.set_fil() }
            VNode::VFilneg => (),
            VNode::Custom(RusTeXNode::ParagraphBegin{specs,start,end,lineskip,parskip}) => {
                nodes::vskip(state,std::mem::take(&mut currskip));
                let parskip = if empty {None} else {Some(parskip)};
                empty = false;
                nodes::do_paragraph(engine,state,children,specs,start,end,lineskip,parskip);
            }
            VNode::Custom(RusTeXNode::HAlignBegin) => {
                nodes::vskip(state,std::mem::take(&mut currskip));
                empty = false;
                todo!()//nodes::do_halign(engine,state,children);
            }
            VNode::Custom(RusTeXNode::PDFNode(PDFNode::Color(act))) => annotations::do_color(state,engine,act),
            VNode::Custom(RusTeXNode::FontChange(font,true)) => {
                if state.in_content {
                    todo!()
                }
                *state.fonts.last_mut().unwrap() = font;
            }
            VNode::Custom(RusTeXNode::PDFNode(PDFNode::PDFStartLink(link))) =>
                todo!(),//annotations::do_link(link,state,true),
            VNode::Custom(RusTeXNode::PDFNode(PDFNode::PDFEndLink)) =>
                todo!(),//annotations::close_link(state,false,false),
            VNode::Custom(RusTeXNode::FontChange(font,false)) => annotations::do_font(state,&engine.fontsystem.glyphmaps,font),
            VNode::Custom(RusTeXNode::FontChangeEnd) => annotations::close_font(state),
            VNode::Custom(RusTeXNode::PDFNode(PDFNode::PDFOutline(_) | PDFNode::PDFCatalog(_)| PDFNode::PDFSave)) | VNode::Penalty(_) => (),
            c => {
                empty = false;
                nodes::vskip(state,std::mem::take(&mut currskip));
                do_v(engine,state,c)
            }

        }
    }
    nodes::vskip(state,currskip);
}


fn do_hlist(engine:Refs, state:&mut ShipoutState, children:&mut HNodes) {
    let mut currskip = ZERO_SKIP;
    while let Some(c) = children.next() {
        match c {
            HNode::HKern(kn) => { currskip.base = currskip.base + kn; }
            HNode::Space if state.mode() == ShipoutMode::H{escape:true} =>
                state.push_comment(format_args!("<div class=\"rustex-space-in-hbox\"></div>")),
            HNode::Space => state.push_space(),
            HNode::HSkip(sk) => currskip.merge(sk),
            HNode::HFil | HNode::Hss => currskip.set_fil(),
            HNode::HFill => currskip.set_fill(),
            HNode::HFilneg => (),
            HNode::Custom(RusTeXNode::PDFNode(PDFNode::Color(act))) =>  annotations::do_color(state,engine,act),
            HNode::Custom(RusTeXNode::FontChange(font,true)) => {
                if state.in_content {
                    todo!()
                }
                *state.fonts.last_mut().unwrap() = font;
            }
            HNode::Custom(RusTeXNode::FontChange(font,false)) => annotations::do_font(state,&engine.fontsystem.glyphmaps,font),
            HNode::Custom(RusTeXNode::PDFNode(PDFNode::PDFStartLink(link))) =>
                todo!(),//annotations::do_link(link,state,false),
            HNode::Custom(RusTeXNode::PDFNode(PDFNode::PDFEndLink)) =>
                todo!(),//annotations::close_link(state,false,false),
            HNode::Custom(RusTeXNode::FontChangeEnd) => annotations::close_font(state),
            HNode::Custom(RusTeXNode::PGFGBegin{..}|RusTeXNode::PGFGEnd) => (),  // TODO???
            HNode::Custom(RusTeXNode::PDFNode(PDFNode::PDFOutline(_) | PDFNode::PDFCatalog(_) | PDFNode::PDFSave)) | HNode::Penalty(_) => (),
            c => {
                nodes::hskip(state,std::mem::take(&mut currskip));
                do_h(engine,state,c)
            }
        }
    }
    nodes::hskip(state,currskip);
}
/*
fn node_from_class(cls:MathClass) -> HTMLNode {
    use MathClass::*;
    match cls {
        Ord => HTMLNode::new(crate::html::labels::MATH_ORD,true),
        Op => HTMLNode::new(crate::html::labels::MATH_OP,true),
        Bin => HTMLNode::new(crate::html::labels::MATH_BIN,true),
        Rel => HTMLNode::new(crate::html::labels::MATH_REL,true),
        Open => HTMLNode::new(crate::html::labels::MATH_OPEN,true),
        Close => HTMLNode::new(crate::html::labels::MATH_CLOSE,true),
        Punct => HTMLNode::new(crate::html::labels::MATH_PUNCT,true)
    }
}


fn do_mathlist(engine:Refs, state:&mut ShipoutState, children:&mut MNodes) {
    use tex_tfm::fontstyles::FontModifiable;
    let mut currskip = Mu(0);
    let mut currclass : Option<(MathClass,HTMLNode)> = None;
    while let Some(c) = children.next() {
        match c {
            MathNode::MSkip {skip,..} => {
                if let Some((_,node)) = std::mem::take(&mut currclass) {
                    nodes::mskip(state,currskip);
                    state.push(node)
                }
                currskip = Mu(currskip.0 + skip.base.0);
            }
            MathNode::MKern{kern,..} => {
                if let Some((_,node)) = std::mem::take(&mut currclass) {
                    nodes::mskip(state,currskip);
                    state.push(node)
                }
                currskip = Mu(currskip.0 + kern.0);
            }
            MathNode::Space => {
                if let Some((_,node)) = std::mem::take(&mut currclass) {
                    nodes::mskip(state,currskip);
                    state.push(node)
                }
                currskip = Mu(currskip.0 + (65536 / 2));
            }
            MathNode::HSkip(sk) => {
                if sk.base != Dim32(0) {
                    if let Some((_,node)) = std::mem::take(&mut currclass) {
                        nodes::mskip(state,currskip);
                        state.push(node)
                    }
                    let mut node = HTMLNode::new(crate::html::labels::HKERN_IN_M,true);
                    node.style("margin-left",dim_to_string(sk.base));
                    state.push(node)
                }
            },
            MathNode::HKern(d) => {
                if d != Dim32(0) {
                    if let Some((_,node)) = std::mem::take(&mut currclass) {
                        nodes::mskip(state,currskip);
                        state.push(node)
                    }
                    let mut node = HTMLNode::new(crate::html::labels::HKERN_IN_M,true);
                    node.style("margin-left",dim_to_string(d));
                    state.push(node)
                }
            },
            MathNode::Atom(a) if a.sup.is_none() && a.sub.is_none() => match a.nucleus {
                MathNucleus::Simple{kernel:MathKernel::Char {char,style:MathFontStyle{font,..}},cls,..} => {
                    nodes::mskip(state,std::mem::take(&mut currskip));
                    nodes::do_mathchar(engine,state,&mut currclass,char,cls,font);
                }
                MathNucleus::Simple{kernel:MathKernel::List {children,..},cls,..} => {
                    nodes::mskip(state,currskip);
                    if let Some((_,node)) = std::mem::take(&mut currclass) {
                        state.push(node)
                    }
                    let mut node = HTMLNode::new(crate::html::labels::DUMMY,true);
                    if let Some(Cow::Borrowed(s)) = node_from_class(cls).classes.pop() {
                        node.class(s);
                    }
                    state.do_in(node,|state| {
                        do_mathlist(engine,state,&mut children.into_vec().into())
                    },|_,node| if node.label == crate::html::labels::DUMMY {Some(node)} else {
                        todo!()
                    });
                }
                MathNucleus::Inner(MathKernel::List{children:nc,..}) => children.prefix(nc.into_vec()),
                MathNucleus::Inner(MathKernel::Box(bx)) | MathNucleus::Simple {kernel:MathKernel::Box(bx),cls:MathClass::Ord,..} => {
                    if bx.is_empty() {
                        todo!("empty box in math")
                    } else {
                        nodes::mskip(state,currskip);
                        if let Some((_,node)) = std::mem::take(&mut currclass) {
                            state.push(node)
                        }
                        nodes::box_in_math(engine,state,bx)
                    }
                }
                o => todo!(" {:?}",o)
            }
            MathNode::Atom(a) => {
                nodes::mskip(state,currskip);
                if let Some((_,node)) = std::mem::take(&mut currclass) {
                    state.push(node)
                }
                nodes::do_sub_sup(engine,state,a)
            }
            MathNode::Choice(c) => children.prefix(c.0.into_vec()),
            MathNode::Custom(RusTeXNode::PDFNode(PDFNode::Color(act))) => annotations::do_color(state,engine,act,true,false),
            MathNode::Custom(RusTeXNode::PDFNode(PDFNode::PDFStartLink(link))) =>
                annotations::do_link(link,state,false),
            MathNode::Custom(RusTeXNode::PDFNode(PDFNode::PDFEndLink)) =>
                annotations::close_link(state,true,false),
            MathNode::HFil | MathNode::HFill | MathNode::Hss | MathNode::HFilneg => (),
            o => todo!(" {:?}",o)
        }
    }
    nodes::mskip(state,currskip);
    if let Some((_,node)) = currclass { state.push(node) }
}
*/

fn do_v(engine:Refs, state:&mut ShipoutState, n: VNode<Types>) {
    match n {
        VNode::Box(bx@ TeXBox::V { info: VBoxInfo::VBox {raised:Some(_),..} | VBoxInfo::VTop {raised:Some(_),..},..} | bx @ TeXBox::H {info:HBoxInfo::HBox {raised:Some(_),..},..}) =>
            todo!(),//nodes::do_raise(bx,state,true,|n,state| do_v(engine,state,VNode::Box(n),false)),
        VNode::Box(bx@ TeXBox::V { info: VBoxInfo::VBox {moved_left:Some(_),..} | VBoxInfo::VTop{moved_left:Some(_),..},..} | bx @ TeXBox::H{info: HBoxInfo::HBox{moved_left:Some(_),..},..}) =>
            todo!(),//nodes::do_moveleft(bx,state,true,|n,state| do_v(engine,state,VNode::Box(n),false)),
        VNode::Box(bx@ TeXBox::V { info: VBoxInfo::VBox { .. },.. }) => nodes::do_vbox(state,engine,bx),
        VNode::Box(bx@ TeXBox::V { info: VBoxInfo::VTop { .. },.. }) => nodes::do_vtop(state,engine,bx),
        VNode::Box(bx@ TeXBox::H { info: HBoxInfo::HBox { .. },.. }) => nodes::do_hbox(state,engine,bx),
        VNode::Custom(RusTeXNode::PDFNode(PDFNode::PDFDest(PDFDest{id,..}))) => todo!(),//nodes::do_pdfdest(state, id),
        VNode::Whatsit(wi) => wi.call(engine),
        VNode::HRule {ref start,ref end,ref width,..} => {
            let height = n.height();
            let depth = n.depth();
            nodes::hrule(state,*start,*end,*width,height,depth)
        }
        VNode::Custom(RusTeXNode::PDFNode(PDFNode::PDFMatrix {scale, rotate,skewx,skewy})) =>
            todo!(),//annotations::do_matrix(state,scale,rotate,skewx,skewy,false,false),
        VNode::Custom(RusTeXNode::PDFNode(PDFNode::PDFRestore)) => todo!(),//annotations::reset_matrix(state,false,false),
        VNode::Custom(RusTeXNode::PDFNode(PDFNode::XImage(img))) => {
            todo!();
            /*
            let mut attr = String::new();
            if let Some(w) = img.width {
                attr.push_str(&format!(" width=\"{}\"",dim_to_string(w)));
            }
            if let Some(h) = img.height {
                attr.push_str(&format!(" height=\"{}\"",dim_to_string(h)));
            }
            state.push_text(format!("<img src=\"{}\"{}/>",img.filepath.display(),attr));

             */
        }
        VNode::Custom(RusTeXNode::PGFSvg {bx,minx,miny,maxx,maxy}) => todo!(),//nodes::do_svg(engine,state,bx,minx,miny,maxx,maxy),
        VNode::Mark(..) => (),
        _ => panic!("Here: {:?}",n)
    }
}

fn do_h(engine:Refs, state:&mut ShipoutState, n: HNode<Types>) {
    match n {
        HNode::Box(bx@ TeXBox::V { info: VBoxInfo::VBox {raised:Some(_),..} | VBoxInfo::VTop {raised:Some(_),..},..} | bx @ TeXBox::H {info: HBoxInfo::HBox {raised:Some(_),..},..}) =>
            todo!(),//nodes::do_raise(bx,state,false,|n,state| do_h(engine,state,HNode::Box(n),false,escape_space)),
        HNode::Box(bx@ TeXBox::V { info: VBoxInfo::VBox {moved_left:Some(_),..} | VBoxInfo::VTop {moved_left:Some(_),..},..} | bx @ TeXBox::H{info: HBoxInfo::HBox {moved_left:Some(_),..} ,..}) =>
            todo!(),//nodes::do_moveleft(bx,state,false,|n,state| do_h(engine,state,HNode::Box(n),false,escape_space)),
        HNode::Box(bx@ TeXBox::V { info: VBoxInfo::VBox { .. },.. }) => nodes::do_vbox(state,engine,bx),
        HNode::Box(bx@ TeXBox::V { info: VBoxInfo::VTop { .. },.. }) => nodes::do_vtop(state,engine,bx),
        HNode::Box(bx@ TeXBox::H { info: HBoxInfo::HBox { .. },.. }) => nodes::do_hbox(state,engine,bx),
        HNode::Box(TeXBox::H {info:HBoxInfo::ParIndent(d),..}) => {
            if d > Dim32(0) { state.push_comment(format_args!("<div class=\"rustex-parindent\", style=\"margin-left:{}\"></div>",dim_to_string(d))) }
        }
        HNode::Custom(RusTeXNode::PDFNode(PDFNode::PDFDest(PDFDest{id,..}))) => todo!(),//nodes::do_pdfdest(state, id),
        HNode::Whatsit(wi) => wi.call(engine),
        HNode::Char {char,font,..} => {
            if font == engine.fontsystem.null() { return }
            state.in_content = true;
            let glyphtable = engine.fontsystem.glyphmaps.get_glyphlist(font.filename());
            let glyph = glyphtable.get(char);
            if !glyph.is_defined() {
                nodes::do_missing_glyph(char,&font,state);
            } else {
                state.push_glyph(glyph)
            }
        }
        HNode::VRule {width,height,depth,start,end} => {
            nodes::vrule(state,start,end,width.unwrap_or(Dim32(26214)),height,depth)
        }
        HNode::Custom(RusTeXNode::PDFNode(PDFNode::PDFMatrix {scale, rotate,skewx,skewy})) =>
            todo!(),//annotations::do_matrix(state,scale,rotate,skewx,skewy,false,false),
        HNode::Custom(RusTeXNode::PDFNode(PDFNode::PDFRestore)) => todo!(),//annotations::reset_matrix(state,false,false),
        HNode::MathGroup(mg) if mathlist_is_h(&mg.children) => do_math_in_h(mg,state,engine),
        HNode::MathGroup(mg) => todo!(),//nodes::do_math(mg,state,engine),
        HNode::Custom(RusTeXNode::PGFSvg {bx,minx,miny,maxx,maxy}) => todo!(),//nodes::do_svg(engine,state,bx,minx,miny,maxx,maxy),
        HNode::Leaders(_) => (), // TODO?
        HNode::Custom(RusTeXNode::PDFNode(PDFNode::XImage(img))) => {
            todo!();
            /*
            let mut attr = String::new();
            if let Some(w) = img.width {
                attr.push_str(&format!(" width=\"{}\"",dim_to_string(w)));
            }
            if let Some(h) = img.height {
                attr.push_str(&format!(" height=\"{}\"",dim_to_string(h)));
            }
            state.push_text(format!("<img src=\"{}\"{}/>",img.filepath.display(),attr));

             */
        }
        _ => todo!("{:?}",n)
    }
}

pub(crate) fn do_math_in_h(mut bx:MathGroup<Types,MathFontStyle<Types>>,state:&mut ShipoutState,engine:Refs) {
    if bx.display {todo!("display in H")}
    let mut iter: MNodes = bx.children.into_vec().into();
    let mut currskip = ZERO_SKIP;
    while let Some(c) = iter.next() {match c {
        MathNode::HKern(kn) => { currskip.base = currskip.base + kn; }
        MathNode::Space if state.mode() == ShipoutMode::H{escape:true} =>
            state.push_comment(format_args!("<div class=\"rustex-space-in-hbox\"></div>")),
        MathNode::Space => state.push_space(),
        MathNode::HSkip(sk) => currskip.merge(sk),
        MathNode::HFil | MathNode::Hss => currskip.set_fil(),
        MathNode::HFill => currskip.set_fill(),
        MathNode::HFilneg => (),
        MathNode::Custom(RusTeXNode::PDFNode(PDFNode::Color(act))) => annotations::do_color(state,engine,act),
        MathNode::Custom(RusTeXNode::FontChange(font,true)) => {
            if state.in_content {
                todo!()
            }
            *state.fonts.last_mut().unwrap() = font;
        }
        MathNode::Custom(RusTeXNode::FontChange(font,false)) => annotations::do_font(state,&engine.fontsystem.glyphmaps,font),
        MathNode::Custom(RusTeXNode::FontChangeEnd) => annotations::close_font(state),
        MathNode::Custom(RusTeXNode::PDFNode(PDFNode::PDFOutline(_) | PDFNode::PDFCatalog(_))) | MathNode::Penalty(_) => (),
        MathNode::Atom(a) => match a.nucleus {
            MathNucleus::VCenter {start,end,children} =>
                todo!(),//do_vcenter(start,end,children,state,engine),
            MathNucleus::Simple{kernel:MathKernel::Empty,..} | MathNucleus::Inner(MathKernel::Empty) => (),
            MathNucleus::Simple{kernel:MathKernel::Box(b),..} | MathNucleus::Inner(MathKernel::Box(b)) =>
                do_h(engine,state,HNode::Box(b)),
            MathNucleus::Simple{kernel:MathKernel::List{children,..},..} | MathNucleus::Inner(MathKernel::List{children,..}) => {
                iter.prefix(children.into_vec())
            }
            _ => unreachable!()
        }
        o => todo!("math in h: {:?}",o)
    }}
}

fn mathlist_is_h(ls: &[MNode]) -> bool {
    ls.iter().all(|n| match n {
        MathNode::Atom(a) => {
            a.sup.is_none() && a.sub.is_none() && match &a.nucleus {
                MathNucleus::VCenter {..} => true,
                MathNucleus::Simple{kernel,..} | MathNucleus::Inner(kernel) => kernel_is_h(kernel),
                _ => false
            }
        }
        MathNode::HSkip(_) | MathNode::HFil | MathNode::HFill | MathNode::HFilneg | MathNode::Hss | MathNode::Space |
            MathNode::MSkip {..} | MathNode::MKern {..} | MathNode::HKern(_) | MathNode::Penalty(_) |
            MathNode::Mark(..) | MathNode::VRule {..} | MathNode::Whatsit(_) | MathNode::Custom(_) => true,
        _ => false
    })

}

fn kernel_is_h(k:&MathKernel<Types,MathFontStyle<Types>>) -> bool {
    match k {
        MathKernel::Empty => true,
        MathKernel::Box(_) => true,
        MathKernel::Char {..} => false,
        MathKernel::List {children,..} => mathlist_is_h(children),
    }
}