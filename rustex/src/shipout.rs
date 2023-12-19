mod nodes;

use std::collections::BTreeSet;
use tex_engine::tex::nodes::{BoxInfo, KernNode, TeXNode, SkipNode, TeXBox, ToOrSpread, SimpleNode};
use tex_engine::tex::types::BoxType;
use crate::engine::{Bx, Font, Refs, Types};
use std::fmt::Write;
use std::vec::IntoIter;
use tex_engine::commands::pdftex::pdftexnodes::{NumOrName, PDFColor, PDFDest, PDFNode};
use tex_engine::engine::{EngineReferences, EngineTypes};
use tex_engine::engine::filesystem::{File, SourceReference};
use tex_engine::engine::utils::memory::{InternedCSName, PRIMITIVES};
use tex_engine::tex::nodes::NodeTrait;
use tex_engine::tex::numerics::{Dim32, Fill, Skip32};
use tex_engine::engine::state::State;
use crate::html::{dim_to_string, HTMLChild, HTMLNode, Tag};
use tex_engine::engine::fontsystem::Font as FontT;
use tex_tfm::glyphs::Glyph;
use crate::nodes::RusTeXNode;

pub(crate) struct NodeIter {
    curr:IntoIter<TeXNode<Types>>,
    next:Vec<IntoIter<TeXNode<Types>>>
}
impl NodeIter {
    pub fn prefix(&mut self,vec:Vec<TeXNode<Types>>) {
        let old = std::mem::replace(&mut self.curr,vec.into_iter());
        self.next.push(old);
    }
}
impl From<Vec<TeXNode<Types>>> for NodeIter {
    fn from(v: Vec<TeXNode<Types>>) -> Self {
        Self { curr:v.into_iter(),next:Vec::new() }
    }
}
impl Iterator for NodeIter {
    type Item = TeXNode<Types>;
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(n) = self.curr.next() { return Some(n) }
        if let Some(mut it) = self.next.pop() {
            if let Some(n) = it.next() {
                self.curr = it;
                return self.next()
            }
        }
        None
    }

}

#[derive(Default)]
pub(crate) struct ShipoutState {
    pub(crate) output:Vec<HTMLChild>,
    pub(crate) nodes:Vec<HTMLNode>,
    pub(crate) fonts: Vec<Font>,
    pub(crate) widths: Vec<Dim32>,
    pub(crate) color:PDFColor,
    pub(crate) fontlinks:BTreeSet<String>,
    pub(crate) in_content:bool
}
impl ShipoutState {
    fn do_in<F1:FnOnce(&mut Self),F2:FnOnce(&mut Self,HTMLNode) -> Option<HTMLNode>>(&mut self, node:HTMLNode,f1:F1,f2:F2) {
        if let Some(width) = node.width {
            self.widths.push(width);
        }
        self.nodes.push(node);
        f1(self);
        let node = self.nodes.pop().unwrap();
        if let Some(_) = node.width {
            self.widths.pop().unwrap();
        }
        match f2(self,node) {
            Some(node) => self.push(node),
            None => ()
        }
    }
    fn push(&mut self, mut node:HTMLNode) {
        match self.nodes.last_mut() {
            Some(parent) => parent.push_node(node),
            None => {
                node.close();
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

pub(crate) fn shipout_paginated(engine:Refs, n: TeXNode<Types>) {
    match n {
        TeXNode::Box(TeXBox { children, info: BoxInfo {
            tp:BoxType::Vertical,..}, start,end }) => split_state(engine,|engine,state|{
            let mut node = HTMLNode::new(crate::html::labels::PAGE, true);
            node.sourceref(start,end);
            state.do_in(node,|state| {
                do_vlist(engine,state,&mut children.into(),true)
            },|_,node| if node.label == crate::html::labels::PAGE {Some(node)} else {
                todo!()
            });

            match state.output.first_mut() {
                Some(HTMLChild::Node(page)) if !page.styles.contains_key("font-family") && !page.styles.contains_key("font-size") => {
                    let pagewidth = engine.state.get_primitive_dim(PRIMITIVES.pdfpagewidth);
                    page.style("max-width",dim_to_string(pagewidth));

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
                    page.children.push(HTMLChild::Node(inner));

                    let font = state.fonts.first().unwrap();
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

            let mut out = String::new();

            for c in std::mem::take(&mut state.output) {
                match c {
                    HTMLChild::Node(n) => {
                        n.display(&mut engine.fontsystem.glyphmaps,state,&mut out).unwrap();
                    },
                    _ => ()
                }

            }
            println!("{}",out);
            println!("HERE")
        }),
        _ => unreachable!()
    }
}
#[inline(always)]
pub(crate) fn shipout(engine:Refs, n: TeXNode<Types>) {
    split_state(engine,|engine,state|{
        do_v(engine,state,n,true);
    });
}

pub(crate) const ZERO: Dim32 = Dim32(0);
pub(crate) const ZERO_SKIP: Skip32<Dim32> = Skip32 {base:ZERO,stretch:None,shrink:None};


fn do_vlist(engine:Refs, state:&mut ShipoutState, children:&mut NodeIter, top:bool) {
    let mut currskip = ZERO_SKIP;
    while let Some(c) = children.next() {
        match c {
            TeXNode::Kern(kn) => { currskip.base = currskip.base + kn.dim(); }
            TeXNode::Skip(sk) => { currskip = currskip + sk.skip() }
            TeXNode::Custom(RusTeXNode::ParagraphBegin(spec)) => {
                nodes::vskip(state,std::mem::take(&mut currskip));
                nodes::do_paragraph(engine,state,children,spec);
            }
            n => common_list(state,engine,n,|state,engine,c| {
                    nodes::vskip(state,std::mem::take(&mut currskip));
                    do_v(engine,state,c,top)
                })

        }
    }
    nodes::vskip(state,currskip);
}

fn do_hlist(engine:Refs, state:&mut ShipoutState, children:&mut NodeIter, inpar:bool) {
    // todo merge hskips and such, annotations
    let mut currskip = ZERO_SKIP;
    while let Some(c) = children.next() {
        match c {
            TeXNode::Kern(kn) => { currskip.base = currskip.base + kn.dim(); }
            TeXNode::Skip(SkipNode::Space) if inpar => state.push_space(),
            TeXNode::Skip(SkipNode::Space) => {
                todo!()
            }
            TeXNode::Skip(sk) => { currskip = currskip + sk.skip() }
            c => common_list(state,engine,c,|state,engine,c| {
                nodes::hskip(state,std::mem::take(&mut currskip));
                do_h(engine,state,c,inpar)
            })
        }
    }
    nodes::hskip(state,currskip);
}

fn common_list<F:FnOnce(&mut ShipoutState,Refs,TeXNode<Types>)>(state:&mut ShipoutState,engine:Refs,n:TeXNode<Types>,cont:F) {
    match n {
        TeXNode::Custom(RusTeXNode::PDFNode(PDFNode::Color(act))) => {
            if let Some(c) = nodes::do_color(state,engine,act) {
                todo!()
            }
        }
        TeXNode::Custom(RusTeXNode::FontChange(font,true)) => {
            if state.in_content {
                todo!()
            }
            *state.fonts.last_mut().unwrap() = font;
        }
        TeXNode::Custom(RusTeXNode::FontChange(font,false)) => {
            let mut node = HTMLNode::new(crate::html::labels::FONT_CHANGE,false);
            node.set_font(font);
            state.nodes.push(node);
        }
        TeXNode::Custom(RusTeXNode::FontChangeEnd) => nodes::close_font(state),
        TeXNode::Custom(RusTeXNode::PDFNode(PDFNode::PDFOutline(_) | PDFNode::PDFCatalog(_))) | TeXNode::Penalty(_) => (),
        _ => cont(state,engine,n)
    }
}

fn do_v(engine:Refs, state:&mut ShipoutState, n: TeXNode<Types>, top:bool) {
    match n {
        TeXNode::Box(bx@ TeXBox { info: BoxInfo {raised:Some(_),..},..}) =>
            nodes::do_raise(bx,state,true,|n,state| do_v(engine,state,n,false)),
        TeXNode::Box(bx@ TeXBox { info: BoxInfo {moved_left:Some(_),..},..}) =>
            nodes::do_moveleft(bx,state,true,|n,state| do_v(engine,state,n,false)),
        TeXNode::Box(bx@ TeXBox { info: BoxInfo {
            tp:BoxType::Vertical,
            kind:"vbox",..
        },.. }) => nodes::do_vbox(bx,state,engine,top,true),
        TeXNode::Box(bx@ TeXBox { info: BoxInfo {
            tp:BoxType::Horizontal,
            kind:"hbox",..
        },.. }) => nodes::do_hbox(bx,state,engine,top,false),
        TeXNode::Custom(RusTeXNode::PDFNode(PDFNode::PDFDest(PDFDest{id,..}))) => nodes::do_pdfdest(state, id),
        TeXNode::Whatsit(wi) =>
            if let Some(n) = wi.call(engine) { do_v(engine,state,n,top) }
        TeXNode::Simple(v@SimpleNode::HRule {..}) => {
            let width = v.width();
            let height = v.height();
            let depth = v.depth();
            let SimpleNode::HRule {start,end,..} = v else {unreachable!()};
            nodes::hrule(start,end,width,height,depth,state)
        }
        _ => panic!("Here: {:?}",n)
    }
}

fn do_h(engine:Refs, state:&mut ShipoutState, n: TeXNode<Types>, par:bool) {
    match n {
        TeXNode::Box(bx@ TeXBox { info: BoxInfo {raised:Some(_),..},..}) =>
            nodes::do_raise(bx,state,false,|n,state| do_h(engine,state,n,false)),
        TeXNode::Box(bx@ TeXBox { info: BoxInfo {moved_left:Some(_),..},..}) =>
            nodes::do_moveleft(bx,state,false,|n,state| do_h(engine,state,n,false)),
        TeXNode::Box(bx@ TeXBox { info: BoxInfo {
            tp:BoxType::Vertical,
            kind:"vbox",..
        },.. }) => nodes::do_vbox(bx,state,engine,false,false),
        TeXNode::Box(bx@ TeXBox { info: BoxInfo {
            tp:BoxType::Horizontal,
            kind:"hbox",..
        },.. }) => nodes::do_hbox(bx,state,engine,false,!par),
        TeXNode::Custom(RusTeXNode::PDFNode(PDFNode::PDFDest(PDFDest{id,..}))) => nodes::do_pdfdest(state, id),
        TeXNode::Whatsit(wi) =>
            if let Some(n) = wi.call(engine) { do_h(engine,state,n,par) }
        TeXNode::Char {char,font,..} => {
            state.in_content = true;
            let glyphtable = engine.fontsystem.glyphmaps.get_glyphlist(font.filename());
            let glyph = glyphtable.get(char);
            if !glyph.is_defined() {
                todo!("Undefined Glyph")
            } else {
                state.push_glyph(glyph)
            }
        }
        TeXNode::Simple(v@SimpleNode::VRule {..}) => {
            let width = v.width();
            let height = v.height();
            let depth = v.depth();
            let SimpleNode::VRule {start,end,..} = v else {unreachable!()};
            nodes::vrule(start,end,width,height,depth,state,par)
        }
        _ => todo!("{:?}",n)
    }
}

fn get_box_dims(bx: &Bx,state:&ShipoutState,scale:fn(&Bx) -> Dim32,top:bool) -> (Option<Dim32>,Option<Dim32>,Option<Dim32>,Option<Dim32>) {
    let wd = match bx.info.assigned_width {
        Some(w) if w != state.curr_width() => Some(w),
        None if top => match bx.width() {
            w if w < ZERO => Some(ZERO),
            _ => None
        }
        _ => None
    };
    let (ht,mut bottom) = match bx.info.assigned_height {
        Some(h) if h < ZERO => (Some(ZERO),Some(h)),
        Some(h) => (Some(h),None),
        None if top && bx.height() < ZERO => (Some(ZERO),None),
        _ => (None,None)
    };
    match (bottom,bx.info.assigned_depth) {
        (Some(b),Some(d)) => {
            let s = b + d;
            if s != ZERO { bottom = Some(b);
            } else { bottom = None; }
        }
        (_,Some(d)) if d != ZERO => bottom = Some(d),
        _ => ()
    }
    let to = match bx.info.scaled {
        ToOrSpread::To(d) => Some(d),
        ToOrSpread::Spread(s) => Some(s + scale(bx)),
        _ => None
    };
    (wd,ht,bottom,to)
}