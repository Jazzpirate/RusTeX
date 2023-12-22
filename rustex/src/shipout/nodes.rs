use std::vec::IntoIter;
use tex_engine::commands::pdftex::pdftexnodes::{ColorStackAction, NumOrName, PDFColor, PDFExtension, PDFNode};
use tex_engine::engine::EngineTypes;
use tex_engine::engine::filesystem::{File, SourceReference};
use tex_engine::tex::nodes::{BoxInfo, KernNode, MathGroup, SkipNode, TeXBox, TeXNode, ToOrSpread};
use tex_engine::tex::numerics::{Dim32, Fill, Skip, Skip32};
use crate::engine::{Bx, Extension, Font, Refs, SRef, Types};
use crate::html::{dim_to_string, HTMLChild, HTMLNode, Tag};
use crate::shipout::{do_hlist, do_vlist, get_box_dims, NodeIter, nodes, ShipoutState, ZERO, ZERO_SKIP};
use tex_engine::tex::nodes::NodeTrait;
use tex_engine::tex::types::BoxType;
use crate::fonts::FontStore;
use crate::nodes::{LineSkip, RusTeXNode};
use tex_engine::engine::fontsystem::Font as FontT;
use tex_engine::engine::stomach::ParLineSpec;
/*
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

 */
#[derive(PartialEq,Copy,Clone)]
pub(crate) enum Alignment {
    L,R,C,S
}
impl Alignment {
    pub fn from(skip1:Skip32<Dim32>,skip2:Skip32<Dim32>) -> Self {
        use Alignment::*;
        match (skip1.stretch,skip2.stretch) {
            (None|Some(Fill::pt(_)),None|Some(Fill::pt(_))) => S,
            (Some(Fill::fil(_)),Some(Fill::fil(_))) => C,
            (Some(Fill::fill(_)),Some(Fill::fill(_))) => C,
            (Some(Fill::fill(_)),_) => R,
            (_,Some(Fill::fill(_))) => L,
            (Some(Fill::fil(_)),_) => R,
            (_,Some(Fill::fil(_))) => L,
        }
    }
}
/*
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

 */

fn merge_skip(sk1:&mut Skip32<Dim32>,sk2:Skip32<Dim32>) {
    let base = sk1.base + sk2.base;
    let stretch = match (sk1.stretch,sk2.stretch) {
        (None|Some(Fill::pt(_)),None|Some(Fill::pt(_))) => None,
        (Some(s1),None|Some(Fill::pt(_))) => Some(s1),
        (None|Some(Fill::pt(_)),Some(s2)) => Some(s2),
        (Some(Fill::fil(a)),Some(Fill::fil(b))) => Some(Fill::fil(a+b)),
        (Some(Fill::fill(a)),Some(Fill::fill(b))) => Some(Fill::fill(a+b)),
        (Some(Fill::fill(a)),_)|(_,Some(Fill::fill(a))) => Some(Fill::fill(a)),
        (Some(Fill::fil(a)),_)|(_,Some(Fill::fil(a))) => Some(Fill::fil(a))
    };
    *sk1 = Skip32 { base,stretch,shrink:None };
}
pub fn merge_skip_node(sk:&mut Skip32<Dim32>,n:SkipNode<Types>) {
    match n {
        SkipNode::HSkip(sk2) | SkipNode::VSkip(sk2) => merge_skip(sk,sk2),
        SkipNode::HFil | SkipNode::VFil => merge_skip(sk,Skip32 { base:ZERO,stretch:Some(Fill::fil(1)),shrink:None }),
        SkipNode::HFill | SkipNode::VFill => merge_skip(sk,Skip32 { base:ZERO,stretch:Some(Fill::fill(1)),shrink:None }),
        SkipNode::Hss | SkipNode::Vss => merge_skip(sk,Skip32 { base:ZERO,stretch:Some(Fill::fill(1)),shrink:None }),
        SkipNode::Space | SkipNode::HFilneg | SkipNode::VFilneg => (),
    }
}

pub(crate) fn alignment(mut v:Vec<TeXNode<Types>>) -> (Alignment, Vec<TeXNode<Types>>) {
    let (mut left,mut right) = (ZERO_SKIP,ZERO_SKIP);
    let mut repush = Vec::new();
    while let Some(n) = v.pop() {
        match n {
            TeXNode::Kern(n) => right.base = right.base + n.dim(),
            TeXNode::Skip(sk) => merge_skip_node(&mut right,sk),
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
            TeXNode::Kern(n) => left.base = left.base + n.dim(),
            TeXNode::Skip(sk) => merge_skip_node(&mut left,sk),
            TeXNode::Penalty(_) => (),
            TeXNode::Custom(RusTeXNode::PDFNode(PDFNode::PDFDest {..})) => nv.push(n),
            _ => {nv.push(n);break}
        }
    }
    if left.base != ZERO {
        nv.insert(0,TeXNode::Kern(KernNode::HKern(left.base)))
    }
    nv.extend(it);
    if right.base != ZERO {
        nv.push(TeXNode::Kern(KernNode::HKern(right.base)))
    }
    (Alignment::from(left,right),nv)
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

pub(crate) fn do_vcenter(mut bx:Bx, state:&mut ShipoutState, engine:Refs) {
    let mut node = HTMLNode::new(VCENTER_CONTAINER, true);
    let do_wd = if bx.width() == ZERO {
        node.width(ZERO);
        true
    } else { false };
    state.do_in(node,|state| {
        let mut node = HTMLNode::new(VCENTER_INNER, true);
        if do_wd {
            node.style_str("width", "100%");
        }
        state.do_in(node,|state| {
            do_vlist(engine,state,&mut bx.children.into(),false);
        },|_,node| if node.label == VCENTER_INNER {Some(node)} else {
            todo!()
        })
    },|_,node| if node.label == VCENTER_CONTAINER {Some(node)} else {
        todo!()
    })

}


pub(crate) fn do_hbox(bx:Bx, state:&mut ShipoutState, engine:Refs, top:bool, in_h:bool) {
    let (wd,ht,bottom,to) = get_box_dims(&bx,state,|bx| bx.width(),top);
    if wd.is_none() && ht.is_none() && bottom.is_none() && to.is_none() && in_h {
        return do_hlist(engine,state,&mut bx.children.into(),false,false);
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
    let esc = match a {
        Alignment::L => {
            node.style_str("justify-content","start");
            false
        },
        Alignment::R => {
            node.style_str("justify-content","end");
            false
        },
        Alignment::C => {
            node.style_str("justify-content","center");
            false
        },
        _ => true
    };
    state.do_in(node,|state| {
        do_hlist(engine,state,&mut children.into(),false,esc);
    },|_,node| if node.label == HBOX_INNER {Some(node)} else {
        todo!()
    })
}

pub(crate) fn do_math(mut bx:MathGroup<Types>, state:&mut ShipoutState, engine:Refs) {
    if bx.display {
        todo!()
    } else {
        let mut node = HTMLNode::new(MATH, true);
        node.sourceref(bx.start,bx.end);
        state.do_in(node,|state| {
            let node = HTMLNode::new(MATH_ROW, true);
            state.do_in(node,|state| {
                super::do_mathlist(engine,state,&mut bx.children.into());
            },|_,node| if node.label == MATH_ROW {Some(node)} else {
                todo!()
            });
        },|_,node| if node.label == MATH {Some(node)} else {
            todo!()
        })

    }
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
    let d = match bx.info {
        BoxInfo::VBox {ref mut raised,..} => std::mem::take(raised).unwrap(),
        BoxInfo::VTop {ref mut raised,..} => std::mem::take(raised).unwrap(),
        BoxInfo::HBox {ref mut raised,..} => std::mem::take(raised).unwrap(),
        _ => unreachable!()
    };
    let mut node = HTMLNode::new(RAISE, inv);
    node.style("bottom",dim_to_string(d));
    state.do_in(node,|state| {
        f(TeXNode::Box(bx), state)
    },|_,node| if node.label == RAISE {Some(node)} else {
        todo!()
    })
}

pub(crate) fn do_moveleft<F:FnOnce(TeXNode<Types>,&mut ShipoutState)>(mut bx:Bx, state:&mut ShipoutState,inv:bool, f:F) {
    let d = match bx.info {
        BoxInfo::VBox {ref mut moved_left,..} => std::mem::take(moved_left).unwrap(),
        BoxInfo::VTop {ref mut moved_left,..} => std::mem::take(moved_left).unwrap(),
        BoxInfo::HBox {ref mut moved_left,..} => std::mem::take(moved_left).unwrap(),
        _ => unreachable!()
    };
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
    node.style("margin-left",dim_to_string(skip.base));
    match skip.stretch {
        Some(Fill::fil(_)) => {
            node.style_str("margin-right","auto");
            state.push(node)
        }
        Some(Fill::fill(_)) => {
            node.style_str("margin-right","auto");
            state.push(node);
            let mut node = HTMLNode::new(HSKIP, false);
            node.style_str("margin-right","auto");
            state.push(node);
        }
        _ => state.push(node)
    }
}
pub(crate) fn vskip(state:&mut ShipoutState, skip:Skip32<Dim32>) {
    if skip == ZERO_SKIP { return }
    let mut node = HTMLNode::new(VSKIP, false);
    node.style("margin-bottom",dim_to_string(skip.base));
    match skip.stretch {
        Some(Fill::fil(_)) => {
            node.style_str("margin-top","auto");
            state.push(node)
        }
        Some(Fill::fill(_)) => {
            node.style_str("margin-top","auto");
            state.push(node);
            let mut node = HTMLNode::new(VSKIP, false);
            node.style_str("margin-top","auto");
            state.push(node);
        },
        _ => state.push(node)
    }
}

pub(crate) fn mskip(state:&mut ShipoutState, skip:Skip32<Dim32>) {
    if skip == ZERO_SKIP { return }
    todo!()
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

pub(crate) fn do_paragraph(engine:Refs, state:&mut ShipoutState,children:&mut NodeIter,mut spec:Vec<ParLineSpec<Types>>,start:SRef,end:SRef,lineskip: LineSkip) {
    let mut children = paragraph_list(children);
    if state.lineskip == LineSkip::default() {
        state.lineskip = lineskip;
    }
    let mut node = HTMLNode::new(PARAGRAPH, false);
    node.sourceref(start,end);
    let spec = spec.pop().unwrap();
    if spec.leftskip.base != ZERO {
        node.style("margin-left",dim_to_string(spec.leftskip.base));
    }
    if spec.rightskip.base != ZERO {
        node.style("margin-right",dim_to_string(spec.rightskip.base));
    }
    let wd = spec.target; //+ spec.leftskip.base + spec.rightskip.base;
    node.width(wd);
    let align: Alignment = Alignment::from(spec.leftskip,spec.rightskip);
    match align {
        Alignment::L => node.style_str("text-align","left"),
        Alignment::R => node.style_str("text-align","right"),
        Alignment::C => node.style_str("text-align","center"),
        _ => ()
    }
    //if spec.target != wd { node.inner_width = Some(spec.target); }

    // TODO: alignment
    state.do_in(node,|state| {
        do_hlist(engine,state,&mut children.into(),true,false);
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
            TeXNode::Box(bx@ TeXBox { info: BoxInfo::ParLine { .. },..}) => ret.extend(bx.children.into_iter()),
            o =>
                todo!("{:?}",o)
        }
    }
    if !success { todo!("ERROR!") }
    ret
}

pub(crate) fn do_halign(engine:Refs, state:&mut ShipoutState,children:&mut NodeIter) {
    let mut num_cols= 0;
    let mut rows = Vec::new();
    while let Some(row) = children.next() {
        match row {
            TeXNode::Custom(RusTeXNode::HAlignEnd) => break,
            TeXNode::Box(bx@ TeXBox { info: BoxInfo::HAlignRow,.. }) => {
                num_cols = num_cols.max(bx.children.len());
                rows.push(TeXNode::Box(bx));
            }
            _ => todo!()
        }
    }
    let mut node = HTMLNode::new(HALIGN, true);
    node.style("grid-template-columns",format!("repeat({},1fr)",num_cols));
    state.do_in(node,|state| {
        for row in rows {
            match row {
                TeXNode::Box(bx@ TeXBox { info: BoxInfo::HAlignRow,.. }) => {
                    let mut cols = 0;
                    let node = HTMLNode::new(HALIGN_ROW, true);
                    state.do_in(node,|state| {
                        for c in bx.children {
                            cols += 1;
                            match c {
                                TeXNode::Box(bx@ TeXBox { info: BoxInfo::HAlignCell {.. },.. }) => {
                                    let node = HTMLNode::new(HALIGN_CELL, true);
                                    state.do_in(node,|state| {
                                        // TODO do moar smart stuff
                                        do_hbox(bx,state,engine,false,false)
                                    },|_,node| if node.label == HALIGN_CELL {Some(node)} else {
                                        todo!()
                                    })
                                }
                                _ => todo!()
                            }
                        }
                    },|_,node| if node.label == HALIGN_ROW {Some(node)} else {
                        todo!()
                    })
                }
                _ => todo!()
            }
        }
    },|_,node| if node.label == HALIGN {Some(node)} else {
        todo!()
    })
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