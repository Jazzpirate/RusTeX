mod nodes;
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
use crate::html::{dim_to_string, HTMLChild, HTMLNode, Tag};
use tex_engine::engine::fontsystem::{Font as FontT, FontSystem};
use tex_engine::tex::nodes::boxes::{BoxInfo, HBoxInfo, TeXBox, ToOrSpread, VBoxInfo};
use tex_engine::tex::nodes::horizontal::HNode;
use tex_engine::tex::nodes::math::{MathFontStyle, MathKernel, MathNode, MathNucleus};
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

pub(crate) struct ShipoutState {
    pub(crate) done:String,
    pub(crate) output:Vec<HTMLChild>,
    pub(crate) nodes:Vec<HTMLNode>,
    pub(crate) fonts: Vec<Font>,
    pub(crate) widths: Vec<Dim32>,
    pub(crate) colors:Vec<PDFColor>,
    pub(crate) fontlinks:BTreeSet<String>,
    pub(crate) lineskip:LineSkip,
    pub(crate) in_content:bool,
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
            lineskip:LineSkip::default(),
            in_content:false
        }
    }
}
impl ShipoutState {
    fn do_in<F1:FnOnce(&mut Self),F2:FnOnce(&mut Self,HTMLNode) -> Option<HTMLNode>>(&mut self, node:HTMLNode,f1:F1,f2:F2) {
        if let Some(width) = node.width {
            self.widths.push(width);
        }
        self.nodes.push(node);
        f1(self);
        let node = annotations::close_all(&mut self.nodes);
        if let Some(_) = node.width {
            self.widths.pop().unwrap();
        }
        match f2(self,node) {
            Some(node) => self.push(node,false,false),
            None => ()
        }
    }
    fn push(&mut self, mut node:HTMLNode,in_math:bool,in_svg:bool) {
        match self.nodes.last_mut() {
            Some(parent) => parent.push_node(node),
            None => {
                node.close(in_math,in_svg);
                self.output.push(HTMLChild::Node(node))
            }
        }
    }
    #[inline(always)]
    fn push_glyph(&mut self,glyph:Glyph) {
        self.nodes.last_mut().unwrap().push_glyph(glyph)
    }
    #[inline(always)]
    fn push_space(&mut self) {
        self.nodes.last_mut().unwrap().push_space()
    }
    #[inline(always)]
    fn push_escaped_space(&mut self) {
        self.nodes.last_mut().unwrap().children.push(HTMLChild::EscapedSpace)
    }
    fn curr_width(&self) -> Dim32 {
        *self.widths.last().unwrap()
    }
}


fn split_state<R,F:FnOnce(Refs,&mut ShipoutState) -> R>(engine:Refs,f:F) -> R {
    let mut state = std::mem::take(&mut engine.aux.extension.state);
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


const TEST_FILE: &str = "/home/jazzpirate/Downloads/example.html";
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
pub(crate) fn shipout_paginated(engine:Refs, n: VNode<Types>) {
/*
    println!("{}",n.readable());
    println!("HERE");
*/

    match n {
        VNode::Box(TeXBox::V {children,start,end,..}) => split_state(engine,|engine,state|{
            let vec = children.into_vec();
            let page = make_page(engine,state,|engine,state| {
                do_vlist(engine,state,&mut vec.into(),true)
            },start,end);


            /*
            let mut node = HTMLNode::new(crate::html::labels::PAGE, true);
            node.sourceref(start,end);
            state.do_in(node,|state| {
                do_vlist(engine,state,&mut children.into_vec().into(),true)
            },|_,node| if node.label == crate::html::labels::PAGE {Some(node)} else {
                todo!()
            });

            match state.output.first_mut() {
                Some(HTMLChild::Node(page)) if !page.styles.contains_key("font-family") && !page.styles.contains_key("font-size") => {
                    let pagewidth = engine.state.get_primitive_dim(PRIMITIVES.pdfpagewidth);
                    page.style("max-width",dim_to_string(pagewidth));
                    let font = state.fonts.first().unwrap();

                    let children = std::mem::take(&mut page.children);
                    let mut inner = HTMLNode::new(crate::html::labels::INNER_PAGE, true);
                    inner.children = children;
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
                }
                _ => ()
            }

            let mut done = std::mem::take(&mut state.done);
            for c in std::mem::take(&mut state.output) {
                match c {
                    HTMLChild::Node(n) => {
                        n.display(&mut engine.fontsystem.glyphmaps,state,&mut done).unwrap();
                    },
                    _ => ()
                }

            }
             */
            let mut done = std::mem::take(&mut state.done);
            page.display(&mut engine.fontsystem.glyphmaps,state,&mut done).unwrap();


            state.done = done;

            std::fs::write(TEST_FILE,&format!("{}{}{}",PREAMBLE,state.done,POSTAMBLE)).unwrap();
            println!("HERE")
        }),
        _ => unreachable!()
    }
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
    //return shipout_paginated(engine,n);
    //println!("HERE: {}",n.readable());
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
}

pub(crate) const ZERO: Dim32 = Dim32(0);
pub(crate) const ZERO_SKIP: Skip32<Dim32> = Skip32 {base:ZERO,stretch:None,shrink:None};

fn do_vlist(engine:Refs, state:&mut ShipoutState, children:&mut VNodes, top:bool) {
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
                nodes::do_paragraph(engine,state,children,specs,start,end,lineskip,parskip);
            }
            VNode::Custom(RusTeXNode::HAlignBegin) => {
                nodes::vskip(state,std::mem::take(&mut currskip));
                nodes::do_halign(engine,state,children);
            }
            VNode::Custom(RusTeXNode::PDFNode(PDFNode::Color(act))) => annotations::do_color(state,engine,act,false,false),
            VNode::Custom(RusTeXNode::FontChange(font,true)) => {
                if state.in_content {
                    todo!()
                }
                *state.fonts.last_mut().unwrap() = font;
            }
            VNode::Custom(RusTeXNode::PDFNode(PDFNode::PDFStartLink(link))) =>
                annotations::do_link(link,state,true),
            VNode::Custom(RusTeXNode::PDFNode(PDFNode::PDFEndLink)) =>
                annotations::close_link(state,false,false),
            VNode::Custom(RusTeXNode::FontChange(font,false)) => annotations::do_font(state,font),
            VNode::Custom(RusTeXNode::FontChangeEnd) => annotations::close_font(state,false,false),
            VNode::Custom(RusTeXNode::PDFNode(PDFNode::PDFOutline(_) | PDFNode::PDFCatalog(_))) | VNode::Penalty(_) => (),
            c => {
                nodes::vskip(state,std::mem::take(&mut currskip));
                do_v(engine,state,c,top)
            }

        }
    }
    nodes::vskip(state,currskip);
}

fn do_hlist(engine:Refs, state:&mut ShipoutState, children:&mut HNodes, inpar:bool,escape_space:bool) {
    let mut currskip = ZERO_SKIP;
    while let Some(c) = children.next() {
        match c {
            HNode::HKern(kn) => { currskip.base = currskip.base + kn; }
            HNode::Space if !escape_space => state.push_space(),
            HNode::Space => state.push_escaped_space(),
            HNode::HSkip(sk) => currskip.merge(sk),
            HNode::HFil | HNode::Hss => currskip.set_fil(),
            HNode::HFill => currskip.set_fill(),
            HNode::HFilneg => (),
            HNode::Custom(RusTeXNode::PDFNode(PDFNode::Color(act))) => annotations::do_color(state,engine,act,false,false),
            HNode::Custom(RusTeXNode::FontChange(font,true)) => {
                if state.in_content {
                    todo!()
                }
                *state.fonts.last_mut().unwrap() = font;
            }
            HNode::Custom(RusTeXNode::FontChange(font,false)) => annotations::do_font(state,font),
            HNode::Custom(RusTeXNode::PDFNode(PDFNode::PDFStartLink(link))) =>
                annotations::do_link(link,state,false),
            HNode::Custom(RusTeXNode::PDFNode(PDFNode::PDFEndLink)) =>
                annotations::close_link(state,false,false),
            HNode::Custom(RusTeXNode::FontChangeEnd) => annotations::close_font(state,false,false),
            HNode::Custom(RusTeXNode::PGFGBegin{..}|RusTeXNode::PGFGEnd) => (),  // TODO???
            HNode::Custom(RusTeXNode::PDFNode(PDFNode::PDFOutline(_) | PDFNode::PDFCatalog(_))) | HNode::Penalty(_) => (),
            c => {
                nodes::hskip(state,std::mem::take(&mut currskip));
                do_h(engine,state,c,inpar,escape_space)
            }
        }
    }
    nodes::hskip(state,currskip);
}


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
const ZERO_MATH: Mu = Mu(0);
fn do_mathlist(engine:Refs, state:&mut ShipoutState, children:&mut MNodes) {
    use tex_tfm::fontstyles::FontModifiable;
    let mut currskip = ZERO_MATH;
    let mut currclass : Option<(MathClass,HTMLNode)> = None;
    while let Some(c) = children.next() {
        match c {
            MathNode::MSkip {skip,..} => {
                if let Some((_,node)) = std::mem::take(&mut currclass) {
                    nodes::mskip(state,currskip);
                    state.push(node,true,false)
                }
                currskip = Mu(currskip.0 + skip.base.0);
            }
            MathNode::MKern{kern,..} => {
                if let Some((_,node)) = std::mem::take(&mut currclass) {
                    nodes::mskip(state,currskip);
                    state.push(node,true,false)
                }
                currskip = Mu(currskip.0 + kern.0);
            }
            MathNode::Space => {
                if let Some((_,node)) = std::mem::take(&mut currclass) {
                    nodes::mskip(state,currskip);
                    state.push(node,true,false)
                }
                currskip = Mu(currskip.0 + (65536 / 2));
            }
            MathNode::HSkip(sk) => {
                if sk.base != ZERO {
                    if let Some((_,node)) = std::mem::take(&mut currclass) {
                        nodes::mskip(state,currskip);
                        state.push(node,true,false)
                    }
                    let mut node = HTMLNode::new(crate::html::labels::HKERN_IN_M,true);
                    node.style("margin-left",dim_to_string(sk.base));
                    state.push(node,true,false)
                }
            },
            MathNode::HKern(d) => {
                if d != ZERO {
                    if let Some((_,node)) = std::mem::take(&mut currclass) {
                        nodes::mskip(state,currskip);
                        state.push(node,true,false)
                    }
                    let mut node = HTMLNode::new(crate::html::labels::HKERN_IN_M,true);
                    node.style("margin-left",dim_to_string(d));
                    state.push(node,true,false)
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
                        state.push(node,true,false)
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
                            state.push(node,true,false)
                        }
                        nodes::box_in_math(engine,state,bx)
                    }
                }
                o => todo!(" {:?}",o)
            }
            MathNode::Atom(a) => {
                nodes::mskip(state,currskip);
                if let Some((_,node)) = std::mem::take(&mut currclass) {
                    state.push(node,true,false)
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
    if let Some((_,node)) = currclass { state.push(node,true,false) }
}

fn do_v(engine:Refs, state:&mut ShipoutState, n: VNode<Types>, top:bool) {
    match n {
        VNode::Box(bx@ TeXBox::V { info: VBoxInfo::VBox {raised:Some(_),..} | VBoxInfo::VTop {raised:Some(_),..},..} | bx @ TeXBox::H {info:HBoxInfo::HBox {raised:Some(_),..},..}) =>
            nodes::do_raise(bx,state,true,|n,state| do_v(engine,state,VNode::Box(n),false)),
        VNode::Box(bx@ TeXBox::V { info: VBoxInfo::VBox {moved_left:Some(_),..} | VBoxInfo::VTop{moved_left:Some(_),..},..} | bx @ TeXBox::H{info: HBoxInfo::HBox{moved_left:Some(_),..},..}) =>
            nodes::do_moveleft(bx,state,true,|n,state| do_v(engine,state,VNode::Box(n),false)),
        VNode::Box(bx@ TeXBox::V { info: VBoxInfo::VBox { .. },.. }) => nodes::do_vbox(bx,state,engine,top,true),
        VNode::Box(bx@ TeXBox::H { info: HBoxInfo::HBox { .. },.. }) => nodes::do_hbox(bx,state,engine,top,false),
        VNode::Custom(RusTeXNode::PDFNode(PDFNode::PDFDest(PDFDest{id,..}))) => nodes::do_pdfdest(state, id),
        VNode::Whatsit(wi) => wi.call(engine),
        v@VNode::HRule {..} => {
            let width = v.width();
            let height = v.height();
            let depth = v.depth();
            let VNode::HRule {start,end,..} = v else {unreachable!()};
            nodes::hrule(start,end,width,height,depth,state)
        }
        VNode::Mark(..) => (),
        _ => panic!("Here: {:?}",n)
    }
}

fn do_h(engine:Refs, state:&mut ShipoutState, n: HNode<Types>, par:bool,escape_space:bool) {
    match n {
        HNode::Box(bx@ TeXBox::V { info: VBoxInfo::VBox {raised:Some(_),..} | VBoxInfo::VTop {raised:Some(_),..},..} | bx @ TeXBox::H {info: HBoxInfo::HBox {raised:Some(_),..},..}) =>
            nodes::do_raise(bx,state,false,|n,state| do_h(engine,state,HNode::Box(n),false,escape_space)),
        HNode::Box(bx@ TeXBox::V { info: VBoxInfo::VBox {moved_left:Some(_),..} | VBoxInfo::VTop {moved_left:Some(_),..},..} | bx @ TeXBox::H{info: HBoxInfo::HBox {moved_left:Some(_),..} ,..}) =>
            nodes::do_moveleft(bx,state,false,|n,state| do_h(engine,state,HNode::Box(n),false,escape_space)),
        HNode::Box(bx@ TeXBox::V { info: VBoxInfo::VBox { .. },.. }) => nodes::do_vbox(bx,state,engine,false,false),
        HNode::Box(bx@ TeXBox::H { info: HBoxInfo::HBox { .. },.. }) => nodes::do_hbox(bx,state,engine,false,!par),
        HNode::Box(TeXBox::H {info:HBoxInfo::ParIndent(d),..}) => {
            let mut node = HTMLNode::new(crate::html::labels::PARINDENT,false);
            node.style("margin-left",dim_to_string(d));
            state.push(node,false,false);
        }
        HNode::Custom(RusTeXNode::PDFNode(PDFNode::PDFDest(PDFDest{id,..}))) => nodes::do_pdfdest(state, id),
        HNode::Whatsit(wi) => wi.call(engine),
        HNode::Char {char,font,..} => {
            if font == engine.fontsystem.null() { return }
            state.in_content = true;
            let glyphtable = engine.fontsystem.glyphmaps.get_glyphlist(font.filename());
            let glyph = glyphtable.get(char);
            if !glyph.is_defined() {
                nodes::do_missing_glyph(char,&font,state,false,false);
            } else {
                state.push_glyph(glyph)
            }
        }
        v@HNode::VRule {..} => {
            let width = v.width();
            let height = v.height();
            let depth = v.depth();
            let HNode::VRule {start,end,..} = v else {unreachable!()};
            nodes::vrule(start,end,width,height,depth,state,par)
        }
        HNode::MathGroup(mg) if mathlist_is_h(&mg.children) => nodes::do_math_in_h(mg,state,engine,par,escape_space),
        HNode::MathGroup(mg) => nodes::do_math(mg,state,engine),
        HNode::Custom(RusTeXNode::PGFSvg {bx,minx,miny,maxx,maxy}) => nodes::do_svg(engine,state,bx,minx,miny,maxx,maxy),
        HNode::Leaders(_) => (), // TODO?
        _ => todo!("{:?}",n)
    }
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

fn get_box_dims(bx: &Bx,state:&ShipoutState,scale:fn(&Bx) -> Dim32,top:bool) -> (Option<Dim32>,Option<Dim32>,Option<Dim32>,Option<Dim32>) {
    let wd = match bx.assigned_width() {
        Some(w) if w == state.curr_width() => None,
        Some(w) =>  Some(w),
        _ if top => match bx.width() {
            w if w < ZERO => Some(ZERO),
            _ => None
        }
        _ => None
    };
    let (ht,mut bottom) = match bx.assigned_height() {
        Some(h) if h < ZERO => (Some(ZERO),Some(h)),
        Some(h) => (Some(h),None),
        _ if top && bx.height() < ZERO => (Some(ZERO),None),
        _ => (None,None)
    };
    match (bottom,bx.assigned_depth()) {
        (Some(b),Some(d)) => {
            let s = b + d;
            if s != ZERO { bottom = Some(b);
            } else { bottom = None; }
        }
        (_,Some(d)) if d != ZERO => bottom = Some(d),
        _ => ()
    }
    let to = match bx.to_or_scaled() {
        ToOrSpread::To(d) => Some(d),
        ToOrSpread::Spread(s) => Some(s + scale(bx)),
        _ => None
    };
    (wd,ht,bottom,to)
}