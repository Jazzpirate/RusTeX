mod nodes;

use std::collections::BTreeSet;
use tex_engine::tex::nodes::{BoxInfo, KernNode, TeXNode, SkipNode, TeXBox, ToOrSpread, SimpleNode};
use tex_engine::tex::types::{BoxType, MathClass};
use crate::engine::{Bx, Font, Refs, Types};
use std::fmt::Write;
use std::vec::IntoIter;
use tex_engine::commands::pdftex::pdftexnodes::{NumOrName, PDFColor, PDFDest, PDFNode};
use tex_engine::engine::{EngineReferences, EngineTypes};
use tex_engine::engine::filesystem::{File, SourceReference};
use tex_engine::engine::utils::memory::{InternedCSName, PRIMITIVES};
use tex_engine::tex::nodes::NodeTrait;
use tex_engine::tex::numerics::{Dim32, Fill, Mu, MuSkip, MuSkip32, Skip32};
use tex_engine::engine::state::State;
use crate::html::{dim_to_string, HTMLChild, HTMLNode, Tag};
use tex_engine::engine::fontsystem::Font as FontT;
use tex_tfm::fontstyles::ModifierSeq;
use tex_tfm::glyphs::Glyph;
use crate::nodes::{LineSkip, RusTeXNode};

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
        while let Some(mut it) = self.next.pop() {
            if let Some(n) = it.next() {
                self.curr = it;
                return Some(n)
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
    pub(crate) lineskip:LineSkip,
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
                node.close(false);
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

pub(crate) fn shipout_paginated(engine:Refs, n: TeXNode<Types>) {
    match n {
        TeXNode::Box(TeXBox { children, info: BoxInfo::VBox {..}, start,end }) => split_state(engine,|engine,state|{
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
            TeXNode::Skip(sk) => { nodes::merge_skip_node(&mut currskip,sk) }
            TeXNode::Custom(RusTeXNode::ParagraphBegin{specs,start,end,lineskip}) => {
                nodes::vskip(state,std::mem::take(&mut currskip));
                nodes::do_paragraph(engine,state,children,specs,start,end,lineskip);
            }
            TeXNode::Custom(RusTeXNode::HAlignBegin) => {
                nodes::vskip(state,std::mem::take(&mut currskip));
                nodes::do_halign(engine,state,children);
            }
            n => common_list(state,engine,n,|state,engine,c| {
                    nodes::vskip(state,std::mem::take(&mut currskip));
                    do_v(engine,state,c,top)
                })

        }
    }
    nodes::vskip(state,currskip);
}

fn do_hlist(engine:Refs, state:&mut ShipoutState, children:&mut NodeIter, inpar:bool,escape_space:bool) {
    let mut currskip = ZERO_SKIP;
    while let Some(c) = children.next() {
        match c {
            TeXNode::Kern(kn) => { currskip.base = currskip.base + kn.dim(); }
            TeXNode::Skip(SkipNode::Space) if !escape_space => state.push_space(),
            TeXNode::Skip(SkipNode::Space) => state.push_escaped_space(),
            TeXNode::Skip(sk) => { nodes::merge_skip_node(&mut currskip,sk) }
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
fn do_mathlist(engine:Refs, state:&mut ShipoutState, children:&mut NodeIter) {
    use tex_tfm::fontstyles::FontModifiable;
    let mut currskip = ZERO_MATH;
    let mut currclass : Option<(MathClass,HTMLNode)> = None;
    while let Some(c) = children.next() {
        match c {
            TeXNode::Simple(SimpleNode::MathChar(mc)) => {
                nodes::mskip(state,std::mem::take(&mut currskip));
                nodes::do_mathchar(engine,state,&mut currclass,mc);
            }
            TeXNode::Simple(SimpleNode::Delim(d)) => {
                nodes::mskip(state,std::mem::take(&mut currskip));
                nodes::do_mathchar(engine,state,&mut currclass,d.small);
            }
            TeXNode::MathGroup(mg) => {
                children.prefix(mg.children);
            }
            TeXNode::Skip(SkipNode::MSkip(ms,_)) => {
                if let Some((_,node)) = std::mem::take(&mut currclass) {
                    nodes::mskip(state,currskip);
                    state.push(node)
                }
                currskip = Mu(currskip.0 + ms.base.0);
            }
            TeXNode::Kern(KernNode::MKern(mu,_)) => {
                if let Some((_,node)) = std::mem::take(&mut currclass) {
                    nodes::mskip(state,currskip);
                    state.push(node)
                }
                currskip = Mu(currskip.0 + mu.0);
            }
            o => todo!(" {:?}",o)
        }
    }
    nodes::mskip(state,currskip);
    if let Some((_,node)) = currclass { state.push(node) }
}

fn do_v(engine:Refs, state:&mut ShipoutState, n: TeXNode<Types>, top:bool) {
    match n {
        TeXNode::Box(bx@ TeXBox { info: BoxInfo::VBox {raised:Some(_),..} | BoxInfo::HBox {raised:Some(_),..}| BoxInfo::VTop {raised:Some(_),..},..}) =>
            nodes::do_raise(bx,state,true,|n,state| do_v(engine,state,n,false)),
        TeXNode::Box(bx@ TeXBox { info: BoxInfo::VBox {moved_left:Some(_),..} | BoxInfo::HBox{moved_left:Some(_),..}| BoxInfo::VTop{moved_left:Some(_),..},..}) =>
            nodes::do_moveleft(bx,state,true,|n,state| do_v(engine,state,n,false)),
        TeXNode::Box(bx@ TeXBox { info: BoxInfo::VBox { .. },.. }) => nodes::do_vbox(bx,state,engine,top,true),
        TeXNode::Box(bx@ TeXBox { info: BoxInfo::HBox { .. },.. }) => nodes::do_hbox(bx,state,engine,top,false),
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
        TeXNode::Box(bx@ TeXBox { info: BoxInfo::VBox {raised:Some(_),..} | BoxInfo::HBox {raised:Some(_),..}| BoxInfo::VTop {raised:Some(_),..},..}) =>
            nodes::do_raise(bx,state,false,|n,state| do_h(engine,state,n,false)),
        TeXNode::Box(bx@ TeXBox { info: BoxInfo::VBox {moved_left:Some(_),..} | BoxInfo::HBox {moved_left:Some(_),..} | BoxInfo::VTop {moved_left:Some(_),..},..}) =>
            nodes::do_moveleft(bx,state,false,|n,state| do_h(engine,state,n,false)),
        TeXNode::Box(bx@ TeXBox { info: BoxInfo::VBox { .. },.. }) => nodes::do_vbox(bx,state,engine,false,false),
        TeXNode::Box(bx@ TeXBox { info: BoxInfo::VCenter,.. }) => nodes::do_vcenter(bx,state,engine),
        TeXNode::Box(bx@ TeXBox { info: BoxInfo::HBox { .. },.. }) => nodes::do_hbox(bx,state,engine,false,!par),
        TeXNode::Box(TeXBox{info:BoxInfo::ParIndent(d),..}) => {
            let mut node = HTMLNode::new(crate::html::labels::PARINDENT,false);
            node.style("margin-left",dim_to_string(d));
            state.push(node);
        }
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
        TeXNode::MathGroup(mut mg) if mg.children.iter().any(|n| matches!(n,
            TeXNode::Box(TeXBox { info: BoxInfo::VCenter,.. })
        )) && !mg.display => {
            if mg.children.len() == 1 {
                if let Some(TeXNode::Box(bx)) = mg.children.pop() {
                    nodes::do_vcenter(bx, state, engine)
                } else { unreachable!() }
            } else {
                do_hlist(engine, state, &mut mg.children.into(), par,!par)
            }
        },
        TeXNode::MathGroup(mg) => nodes::do_math(mg,state,engine),
        _ => todo!("{:?}",n)
    }
}

fn get_box_dims(bx: &Bx,state:&ShipoutState,scale:fn(&Bx) -> Dim32,top:bool) -> (Option<Dim32>,Option<Dim32>,Option<Dim32>,Option<Dim32>) {
    let wd = match bx.info {
        BoxInfo::HBox { assigned_width: Some(w),..} | BoxInfo::VBox { assigned_width: Some(w),..} | BoxInfo::VTop { assigned_width: Some(w),..}
        if w == state.curr_width() => None,
        BoxInfo::HBox { assigned_width: Some(w),..} | BoxInfo::VBox { assigned_width: Some(w),..} | BoxInfo::VTop { assigned_width: Some(w),..}
            => Some(w),
        _ if top => match bx.width() {
            w if w < ZERO => Some(ZERO),
            _ => None
        }
        _ => None
    };
    let (ht,mut bottom) = match bx.info {
        BoxInfo::HBox { assigned_height: Some(h),..} | BoxInfo::VBox { assigned_height: Some(h),..} | BoxInfo::VTop { assigned_height: Some(h),..}
        if h < ZERO => (Some(ZERO),Some(h)),
        BoxInfo::HBox { assigned_height: Some(h),..} | BoxInfo::VBox { assigned_height: Some(h),..} | BoxInfo::VTop { assigned_height: Some(h),..}
            => (Some(h),None),
        _ if top && bx.height() < ZERO => (Some(ZERO),None),
        _ => (None,None)
    };
    match (bottom,&bx.info) {
        (Some(b),BoxInfo::HBox { assigned_depth: Some(d),..} | BoxInfo::VBox { assigned_depth: Some(d),..} | BoxInfo::VTop { assigned_depth: Some(d),..})
            => {
            let s = b + *d;
            if s != ZERO { bottom = Some(b);
            } else { bottom = None; }
        }
        (_,BoxInfo::HBox { assigned_depth: Some(d),..} | BoxInfo::VBox { assigned_depth: Some(d),..} | BoxInfo::VTop { assigned_depth: Some(d),..})
            if *d != ZERO => bottom = Some(*d),
        _ => ()
    }
    let to = match bx.info {
        BoxInfo::HBox { scaled: ToOrSpread::To(d),..} | BoxInfo::VBox { scaled: ToOrSpread::To(d),..} | BoxInfo::VTop { scaled: ToOrSpread::To(d),..}
            => Some(d),
        BoxInfo::HBox { scaled: ToOrSpread::Spread(s),..} | BoxInfo::VBox { scaled: ToOrSpread::Spread(s),..} | BoxInfo::VTop { scaled: ToOrSpread::Spread(s),..}
            => Some(s + scale(bx)),
        _ => None
    };
    (wd,ht,bottom,to)
}