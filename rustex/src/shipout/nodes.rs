use std::vec::IntoIter;
use tex_engine::commands::pdftex::pdftexnodes::{ColorStackAction, NumOrName, PDFColor, PDFExtension, PDFNode};
use tex_engine::engine::EngineTypes;
use tex_engine::engine::filesystem::{File, SourceReference};
use tex_engine::tex::nodes::{BoxInfo, KernNode, TeXBox, TeXNode, ToOrSpread};
use tex_engine::tex::numerics::{Dim32, Fill, Skip32};
use crate::engine::{Bx, Extension, Font, Refs, SRef, Types};
use crate::html::{dim_to_string, HTMLChild, HTMLNode, Tag};
use crate::shipout::{do_hlist, do_vlist, get_box_dims, NodeIter, nodes, ShipoutState, ZERO, ZERO_SKIP};
use tex_engine::tex::nodes::NodeTrait;
use tex_engine::tex::types::BoxType;
use crate::fonts::FontStore;
use crate::nodes::RusTeXNode;
use tex_engine::engine::fontsystem::Font as FontT;
use tex_engine::engine::stomach::ParLineSpec;

#[derive(PartialEq,Copy,Clone)]
enum FilLevel {
    None,Fil,Fill,Filll
}
impl From<Option<Fill<Dim32>>> for FilLevel {
    fn from(f:Option<Fill<Dim32>>) -> Self {
        match f {
            Some(Fill::fil(_)) => Self::Fil,
            Some(Fill::fill(_)) => Self::Fill,
            _ => Self::None
        }
    }
}
impl FilLevel {
    pub fn add(&mut self,other:FilLevel) {
        use FilLevel::*;
        match (&self,other) {
            (None,o) => *self = o,
            (_,None) => (),
            (Fil,Fill) => *self = Fill,
            (Fil,Filll) => *self = Filll,
            (Fill,Filll) => *self = Filll,
            _ => (),
        }
    }
    pub fn cmp(&self, other: &Self) -> Alignment {
        use FilLevel::*;
        use Alignment::*;
        match (self,other) {
            (None,None) => S,
            (a,b) if a == b => C,
            (None,_) | (Fil,Fill) | (Fill,Filll) => L,
            _ => R
        }
    }
}
#[derive(PartialEq,Copy,Clone)]
pub(crate) enum Alignment {
    L,R,C,S
}
impl From<(FilLevel,FilLevel)> for Alignment {
    #[inline(always)]
    fn from((a,b):(FilLevel,FilLevel)) -> Self {
        a.cmp(&b)
    }
}
impl From<(Skip32<Dim32>,Skip32<Dim32>)> for Alignment {
    #[inline(always)]
    fn from((a,b):(Skip32<Dim32>,Skip32<Dim32>)) -> Self {
        let a: FilLevel = a.stretch.into();
        let b: FilLevel = b.stretch.into();
        a.cmp(&b)
    }
}

pub(crate) fn alignment(mut v:Vec<TeXNode<Types>>) -> (Alignment, Vec<TeXNode<Types>>) {
    let (mut left,mut right) = (FilLevel::None,FilLevel::None);
    let mut repush = Vec::new();
    while let Some(n) = v.pop() {
        match n {
            TeXNode::Kern(_) => repush.push(n),
            TeXNode::Skip(sk) => {
                let isk: Skip32<Dim32> = sk.skip();
                right.add(isk.stretch.into());
                repush.push(TeXNode::Kern(KernNode::HKern(isk.base)));
            }
            TeXNode::Penalty(_) => (),
            TeXNode::Custom(RusTeXNode::PDFNode(PDFNode::PDFDest {..})) => repush.push(n),
            _ => {v.push(n);break}
        }
    }
    v.extend(repush.into_iter().rev());
    let mut nv = vec!();
    let mut it = v.into_iter();
    while let Some(n) = it.next() {
        match n {
            TeXNode::Kern(_) => nv.push(n),
            TeXNode::Skip(sk) => {
                let isk: Skip32<Dim32> = sk.skip();
                left.add(isk.stretch.into());
                nv.push(TeXNode::Kern(KernNode::HKern(isk.base)));
            }
            TeXNode::Penalty(_) => (),
            TeXNode::Custom(RusTeXNode::PDFNode(PDFNode::PDFDest {..})) => nv.push(n),
            _ => {nv.push(n);break}
        }
    }
    nv.extend(it);
    ((left,right).into(),nv)
}

use crate::html::labels::*;

pub(crate) fn do_vbox(bx:Bx, state:&mut ShipoutState, engine:Refs, top:bool, in_v:bool) {
    let (wd,ht,bottom,to) = get_box_dims(&bx,state,|bx| bx.height(),top);
    if wd.is_none() && ht.is_none() && bottom.is_none() && to.is_none() && in_v {
        let mut iter = bx.children.into();
        return do_vlist(engine,state,&mut iter,false);
    }
    let mut node = HTMLNode::new(VBOX_CONTAINER, true);
    if let Some(b) = bottom { node.style("margin-bottom",dim_to_string(b)) }
    if let Some(w) = wd { node.width(w); }
    state.do_in(node,|state| match ht {
        Some(h) => {
            let mut node = HTMLNode::new(VBOX_HEIGHT_CONTAINER, true);
            node.style("height",dim_to_string(h));
            if wd.is_some() { node.style_str("width","100%") }
            state.do_in(node,|state| {
                vbox_inner(wd.is_some(),to,state,bx.children,bx.start,bx.end,engine)
            },|_,node| if node.label == VBOX_HEIGHT_CONTAINER {Some(node)} else {
                todo!()
            })
        }
        None =>
            vbox_inner(wd.is_some(),to,state,bx.children,bx.start,bx.end,engine),
    },|_,node| if node.label == VBOX_CONTAINER {Some(node)} else {
        todo!()
    });
}

fn vbox_inner(has_wd:bool, to:Option<Dim32>, state:&mut ShipoutState, children:Vec<TeXNode<Types>>, start:SRef, end:SRef, engine:Refs) {
    let mut node = HTMLNode::new(VBOX_INNER, true);
    node.sourceref(start,end);
    if has_wd { node.style_str("width","100%") }
    match to {
        Some(d) if d < ZERO => {
            node.style_str("height","0");
            node.style("margin-bottom",dim_to_string(d))
        }
        Some(d) => node.style("height",dim_to_string(d)),
        _ => ()
    }
    state.do_in(node,|state| {
        do_vlist(engine,state,&mut children.into(),false);
    },|_,node| if node.label == VBOX_INNER {Some(node)} else {
        todo!()
    })
}


pub(crate) fn do_hbox(bx:Bx, state:&mut ShipoutState, engine:Refs, top:bool, in_h:bool) {
    let (wd,ht,bottom,to) = get_box_dims(&bx,state,|bx| bx.width(),top);
    if wd.is_none() && ht.is_none() && bottom.is_none() && to.is_none() && in_h {
        return do_hlist(engine,state,&mut bx.children.into(),false);
    }
    if wd.is_none() && ht.is_none() && bottom.is_none() {
        return hbox_inner(to,state,bx.children,bx.start,bx.end,engine);
    }
    let mut node = HTMLNode::new(HBOX_CONTAINER, true);
    if let Some(b) = bottom { node.style("margin-bottom",dim_to_string(b)) }
    if let Some(h) = ht { node.style("height",dim_to_string(h)); }
    if let Some(w) = wd { node.width(w); }
    state.do_in(node,|state| {
        hbox_inner(to,state,bx.children,bx.start,bx.end,engine)
    },|_,node| if node.label == HBOX_CONTAINER {Some(node)} else {
        todo!()
    });
}

fn hbox_inner(to:Option<Dim32>, state:&mut ShipoutState, children:Vec<TeXNode<Types>>, start:SRef, end:SRef, engine:Refs) {
    let (a,children) = alignment(children);
    let mut node = HTMLNode::new(HBOX_INNER, false);
    node.sourceref(start,end);
    if let Some(wd) = to { node.width(wd); }
    match a {
        Alignment::L => {
            node.style_str("justify-content","start");
            node.class("rustex-hbox-no-space");
        },
        Alignment::R => {
            node.style_str("justify-content","end");
            node.class("rustex-hbox-no-space");
        },
        Alignment::C => {
            node.style_str("justify-content","center");
            node.class("rustex-hbox-no-space");
        },
        _ => ()
    }
    state.do_in(node,|state| {
        do_hlist(engine,state,&mut children.into(),false);
    },|_,node| if node.label == HBOX_INNER {Some(node)} else {
        todo!()
    })
}


pub(crate) fn do_pdfdest(state:&mut ShipoutState, id:NumOrName) {
    let mut node = HTMLNode::new(DEST, false);
    let target = match id {
        NumOrName::Num(n) => format!("NUM_{}",n),
        NumOrName::Name(n) => n
    };
    node.attr("name",target.clone());
    node.attr("id",target);
    state.push(node);
}

pub(crate) fn do_raise<F:FnOnce(TeXNode<Types>,&mut ShipoutState)>(mut bx:Bx, state:&mut ShipoutState,inv:bool, f:F) {
    let d = std::mem::take(&mut bx.info.raised).unwrap();
    let mut node = HTMLNode::new(RAISE, inv);
    node.style("bottom",dim_to_string(d));
    state.do_in(node,|state| {
        f(TeXNode::Box(bx), state)
    },|_,node| if node.label == RAISE {Some(node)} else {
        todo!()
    })
}

pub(crate) fn do_moveleft<F:FnOnce(TeXNode<Types>,&mut ShipoutState)>(mut bx:Bx, state:&mut ShipoutState,inv:bool, f:F) {
    let d = std::mem::take(&mut bx.info.moved_left).unwrap();
    let mut node = HTMLNode::new(MOVE_RIGHT, inv);
    node.style("margin-left",dim_to_string(-d));
    state.do_in(node,|state| {
        f(TeXNode::Box(bx), state)
    },|_,node| Some(node))
}

pub(crate) fn hrule(start:SRef, end:SRef, width:Dim32, height:Dim32, depth:Dim32, state:&mut ShipoutState) {
    let ht = height + depth;
    if ht == ZERO {return}
    let mut cont = HTMLNode::new(HRULE_CONTAINER, false);
    // TODO maybe relativize;
    cont.style("height",dim_to_string(ht));
    if width == ZERO {
        cont.style_str("width","100%");
    } else {
        cont.width(width);
    }
    if state.color != PDFColor::black() { todo!() }
    let mut node = HTMLNode::new(HRULE_INNER, false);
    node.style("background",state.color.to_string());
    node.sourceref(start, end);
    node.style_str("width","100%");
    node.style("height",dim_to_string(ht));
    if depth != ZERO {
        node.style("margin-bottom",dim_to_string(-depth));
    }
    cont.push_node(node);
    state.push(cont);
}

pub(crate) fn vrule(start:SRef, end:SRef, width:Dim32, height:Dim32, depth:Dim32, state:&mut ShipoutState, par:bool) {
    if width == ZERO { return }
    let ht = height + depth;
    let mut node = HTMLNode::new(VRULE_INNER, false);
    node.sourceref(start, end);
    // TODO maybe relativize;
    node.style("background",state.color.to_string());
    // height things
    if ht == ZERO {
        node.style("width", dim_to_string(width));
        node.style("min-width", dim_to_string(width));
        if par {
            todo!()
        } else { node.style_str("align-self","stretch") }
        state.push(node)
    } else {
        let mut cont = HTMLNode::new(VRULE_CONTAINER, false);
        cont.style("width", dim_to_string(width));
        cont.style("min-width", dim_to_string(width));
        node.style_str("vertical-align","baseline");
        node.style_str("width","100%");
        cont.style("height",dim_to_string(ht));
        if par && depth == ZERO {
            node.style_str("margin-bottom","-0.5ex");
            node.style("height",format!("calc(0.5ex + {})",dim_to_string(ht)));
        } else {
            node.style("height",dim_to_string(ht));
            node.style("margin-bottom",dim_to_string(-depth));
        }
        cont.push_node(node);
        state.push(cont);
    }
}

pub(crate) fn hskip(state:&mut ShipoutState, skip:Skip32<Dim32>) {
    if skip == ZERO_SKIP { return }
    let mut node = HTMLNode::new(HSKIP, false);
    match skip.stretch {
        Some(Fill::fil(_) | Fill::fill(_)) => node.style_str("margin-right","auto"),
        _ => ()
    }
    node.style("margin-left",dim_to_string(skip.base));
    state.push(node)
}
pub(crate) fn vskip(state:&mut ShipoutState, skip:Skip32<Dim32>) {
    if skip == ZERO_SKIP { return }
    let mut node = HTMLNode::new(VSKIP, false);
    node.class("rustex-vskip");
    match skip.stretch {
        Some(Fill::fil(_) | Fill::fill(_)) => node.style_str("margin-top","auto"),
        _ => ()
    }
    node.style("margin-bottom",dim_to_string(skip.base));
    state.push(node)
}

pub(crate) fn do_color(state:&mut ShipoutState,engine:Refs, color:ColorStackAction) -> Option<PDFColor> {
    let stack = engine.aux.extension.colorstacks();
    match color {
        ColorStackAction::Set(idx,c) => {
            *stack[idx].last_mut().unwrap() = c;
            if *engine.aux.extension.current_colorstack() == idx && state.color != c { todo!() } else { None }
        }
        ColorStackAction::Push(idx,c) => {
            stack[idx].push(c);
            if *engine.aux.extension.current_colorstack() == idx && state.color != c { todo!() } else { None }
        }
        ColorStackAction::Pop(idx) => {
            stack[idx].pop();
            let old = stack[idx].last().unwrap().clone();
            if *engine.aux.extension.current_colorstack() == idx && state.color != old { todo!() } else { None }
        }
        ColorStackAction::Current(idx) => {
            let old = stack[idx].last().unwrap().clone();
            if *engine.aux.extension.current_colorstack() == idx { None } else {
                *engine.aux.extension.current_colorstack() = idx;
                if old != state.color {todo!()} else { None }
            }
        }
    }
}

pub(crate) fn do_paragraph(engine:Refs, state:&mut ShipoutState,children:&mut NodeIter,mut spec:Vec<ParLineSpec<Types>>) {
    let mut children = paragraph_list(children);
    let mut node = HTMLNode::new(PARAGRAPH, false);
    let spec = spec.pop().unwrap();
    if spec.leftskip.base != ZERO {
        node.style("margin-left",dim_to_string(spec.leftskip.base));
    }
    if spec.rightskip.base != ZERO {
        node.style("margin-right",dim_to_string(spec.rightskip.base));
    }
    let wd = spec.target + spec.leftskip.base + spec.rightskip.base;
    node.width(wd);
    let align: Alignment = (spec.leftskip,spec.rightskip).into();
    match align {
        Alignment::L => node.style_str("text-align","left"),
        Alignment::R => node.style_str("text-align","right"),
        Alignment::C => node.style_str("text-align","center"),
        _ => ()
    }
    if spec.target != wd { node.inner_width = Some(spec.target); }

    // TODO: alignment
    state.do_in(node,|state| {
        do_hlist(engine,state,&mut children.into(),true);
    },|_,node| if node.label == PARAGRAPH {Some(node)} else {
        todo!()
    })
}

pub(crate) fn paragraph_list(children:&mut NodeIter) -> Vec<TeXNode<Types>> {
    let mut success = false;
    let mut ret = vec!();
    while let Some(c) = children.next() {
        match c {
            TeXNode::Custom(RusTeXNode::ParagraphEnd) => {
                success = true;
                break;
            }
            TeXNode::Box(bx@ TeXBox { info: BoxInfo {
                tp:BoxType::Horizontal,
                kind:"parline",..
            },.. }) => ret.extend(bx.children.into_iter()),
            o =>
                todo!("{:?}",o)
        }
    }
    if !success { todo!("ERROR!") }
    ret
}

pub(crate) fn close_font(state:&mut ShipoutState) {
    let mut requeue:Vec<HTMLNode> = vec!();
    while let Some(n) = state.nodes.pop() {
        if n.label == FONT_CHANGE {
            if requeue.is_empty() {
                if !n.children.is_empty() {state.push(n);}
                return
            }
            let Some(f) = n.font.clone() else { unreachable!() };
            if !n.children.is_empty() {state.push(n);}
            for mut c in requeue.into_iter().rev() {
                let mut node = HTMLNode::new(FONT_CHANGE, c.allow_newline);
                node.set_font(f.clone());
                node.children = std::mem::take(&mut c.children);
                if !node.children.is_empty() {c.children.push(HTMLChild::Node(node));}
                state.nodes.push(c);
            }
            return
        } else {
            requeue.push(n);
        }
    }
    unreachable!()
}