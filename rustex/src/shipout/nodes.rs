use std::vec::IntoIter;
use tex_engine::commands::pdftex::pdftexnodes::{ColorStackAction, NumOrName, PDFColor, PDFExtension, PDFNode};
use tex_engine::engine::EngineTypes;
use tex_engine::engine::filesystem::{File, SourceReference};
use tex_engine::tex::numerics::{DefaultNumSet, Dim32, Fill, Mu, MuSkip32, Skip, Skip32};
use crate::engine::{Bx, Extension, Font, Refs, SRef, Types};
use crate::html::{dim_to_string, HTMLChild, HTMLNode, mudim_to_string, Tag};
use crate::shipout::{do_hlist, do_vlist, get_box_dims, HNodes, MNodes, node_from_class, nodes, ShipoutState, VNodes, ZERO, ZERO_MATH, ZERO_SKIP};
use tex_engine::tex::nodes::NodeTrait;
use tex_engine::tex::types::{BoxType, MathClass};
use crate::fonts::FontStore;
use crate::nodes::{LineSkip, RusTeXNode};
use tex_engine::engine::fontsystem::Font as FontT;
use tex_engine::engine::stomach::ParLineSpec;
use tex_engine::tex::nodes::boxes::{HBoxInfo, TeXBox, VBoxInfo};
use tex_engine::tex::nodes::horizontal::HNode;
use tex_engine::tex::nodes::math::{MathFontStyle, MathGroup, MathKernel, MathNode, MathNucleus};
use tex_engine::tex::nodes::vertical::VNode;
use tex_tfm::fontstyles::ModifierSeq;
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

pub(crate) trait SkipAdd {
    fn merge(&mut self,sk:Skip32<Dim32>);
    fn set_fil(&mut self);
    fn set_fill(&mut self);
}
impl SkipAdd for Skip32<Dim32> {
    fn merge(&mut self,sk2:Skip32<Dim32>) {
        let base = self.base + sk2.base;
        let stretch = match (self.stretch,sk2.stretch) {
            (Some(Fill::fill(a)),_)|(_,Some(Fill::fill(a))) => Some(Fill::fill(a)),
            (Some(Fill::fil(a)),_)|(_,Some(Fill::fil(a))) => Some(Fill::fil(a)),
            _ => None,
        };
        *self = Skip32 { base,stretch,shrink:None };
    }
    fn set_fil(&mut self) {
        if let Some(Fill::fill(_)) = self.stretch { return }
        self.stretch = Some(Fill::fil(1))
    }
    fn set_fill(&mut self) {
        self.stretch = Some(Fill::fill(1))
    }
}

pub(crate) trait MuAdd {
    fn merge(&mut self,sk:Dim32,f:&Font);
}
impl MuAdd for Mu {
    fn merge(&mut self,sk2:Dim32,f:&Font) {
        let em = f.get_dim(5);
        let nb = (((sk2.0 as f64) * 18.0 / (em.0 as f64)) * 65536.0) as i32;
        self.0 += nb;
    }
}


pub(crate) fn alignment(mut v:Vec<HNode<Types>>) -> (Alignment, Vec<HNode<Types>>) {
    let (mut left,mut right) = (ZERO_SKIP,ZERO_SKIP);
    let mut repush = Vec::new();
    while let Some(n) = v.pop() {
        match n {
            HNode::HKern(n) => right.base = right.base + n,
            HNode::HSkip(sk) => right.merge(sk),
            HNode::HFil | HNode::Hss => right.set_fil(),
            HNode::HFill => right.set_fill(),
            HNode::Penalty(_) | HNode::Mark(..) => (),
            HNode::Custom(RusTeXNode::PDFNode(PDFNode::PDFDest {..})) => repush.push(n),
            _ => {v.push(n);break}
        }
    }
    v.extend(repush.into_iter().rev());
    let mut nv = vec!();
    let mut it = v.into_iter();
    while let Some(n) = it.next() {
        match n {
            HNode::HKern(n) => left.base = left.base + n,
            HNode::HSkip(sk) => left.merge(sk),
            HNode::HFil | HNode::Hss => left.set_fil(),
            HNode::HFill => left.set_fill(),
            HNode::Penalty(_) | HNode::Mark(..) => (),
            HNode::Custom(RusTeXNode::PDFNode(PDFNode::PDFDest {..})) => nv.push(n),
            _ => {nv.push(n);break}
        }
    }
    if left.base != ZERO {
        nv.insert(0,HNode::HKern(left.base))
    }
    nv.extend(it);
    if right.base != ZERO {
        nv.push(HNode::HKern(right.base))
    }
    (Alignment::from(left,right),nv)
}

use crate::html::labels::*;

pub(crate) fn do_vbox(bx:Bx, state:&mut ShipoutState, engine:Refs, top:bool, in_v:bool) {
    let (wd,ht,bottom,to) = get_box_dims(&bx,state,|bx| bx.height(),top);
    let (children,start,end) = if let TeXBox::V {children,start,end,..} = bx { (children.into_vec(),start,end) } else {unreachable!()};
    if wd.is_none() && ht.is_none() && bottom.is_none() && to.is_none() && in_v {
        return do_vlist(engine,state,&mut children.into(),false);
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
                vbox_inner(wd.is_some(),to,state,children,start,end,engine)
            },|_,node| if node.label == VBOX_HEIGHT_CONTAINER {Some(node)} else {
                todo!()
            })
        }
        None =>
            vbox_inner(wd.is_some(),to,state,children,start,end,engine),
    },|_,node| if node.label == VBOX_CONTAINER {Some(node)} else {
        todo!()
    });
}

fn vbox_inner(has_wd:bool, to:Option<Dim32>, state:&mut ShipoutState, children:Vec<VNode<Types>>, start:SRef, end:SRef, engine:Refs) {
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
    let (children,start,end) = if let TeXBox::H {children,start,end,..} = bx { (children.into_vec(),start,end) } else {unreachable!()};
    if wd.is_none() && ht.is_none() && bottom.is_none() && to.is_none() && in_h {
        return do_hlist(engine,state,&mut children.into(),false,false);
    }
    if wd.is_none() && ht.is_none() && bottom.is_none() {
        return hbox_inner(to,state,children,start,end,engine);
    }
    let mut node = HTMLNode::new(HBOX_CONTAINER, true);
    if let Some(b) = bottom { node.style("margin-bottom",dim_to_string(b)) }
    if let Some(h) = ht { node.style("height",dim_to_string(h)); }
    if let Some(w) = wd { node.width(w); }
    state.do_in(node,|state| {
        hbox_inner(to,state,children,start,end,engine)
    },|_,node| if node.label == HBOX_CONTAINER {Some(node)} else {
        todo!()
    });
}

fn hbox_inner(to:Option<Dim32>, state:&mut ShipoutState, children:Vec<HNode<Types>>, start:SRef, end:SRef, engine:Refs) {
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

pub(crate) fn do_math_in_h(mut bx:MathGroup<Types,MathFontStyle<Font>>,state:&mut ShipoutState,engine:Refs, inpar:bool,escape_space:bool) {
    if bx.display {todo!("display in H")}
    let mut iter: MNodes = bx.children.into_vec().into();
    let mut currskip = ZERO_SKIP;
    while let Some(c) = iter.next() {match c {
        MathNode::HKern(kn) => { currskip.base = currskip.base + kn; }
        MathNode::Space if !escape_space => state.push_space(),
        MathNode::Space => state.push_escaped_space(),
        MathNode::HSkip(sk) => currskip.merge(sk),
        MathNode::HFil | MathNode::Hss => currskip.set_fil(),
        MathNode::HFill => currskip.set_fill(),
        MathNode::HFilneg => (),
        MathNode::Custom(RusTeXNode::PDFNode(PDFNode::Color(act))) => {
            if let Some(c) = do_color(state,engine,act) {
                todo!()
            }
        }
        MathNode::Custom(RusTeXNode::FontChange(font,true)) => {
            if state.in_content {
                todo!()
            }
            *state.fonts.last_mut().unwrap() = font;
        }
        MathNode::Custom(RusTeXNode::FontChange(font,false)) => {
            let mut node = HTMLNode::new(FONT_CHANGE,false);
            node.set_font(font);
            state.nodes.push(node);
        }
        MathNode::Custom(RusTeXNode::FontChangeEnd) => close_font(state),
        MathNode::Custom(RusTeXNode::PDFNode(PDFNode::PDFOutline(_) | PDFNode::PDFCatalog(_))) | MathNode::Penalty(_) => (),
        MathNode::Atom(a) => match a.nucleus {
            MathNucleus::VCenter {start,end,children} =>
                do_vcenter(start,end,children,state,engine),
            MathNucleus::Simple{kernel:MathKernel::Empty,..} | MathNucleus::Inner(MathKernel::Empty) => (),
            MathNucleus::Simple{kernel:MathKernel::Box(b),..} | MathNucleus::Inner(MathKernel::Box(b)) =>
                super::do_h(engine,state,HNode::Box(b),inpar,escape_space),
            MathNucleus::Simple{kernel:MathKernel::List{children,..},..} | MathNucleus::Inner(MathKernel::List{children,..}) => {
                iter.prefix(children.into_vec())
            }
            _ => unreachable!()
        }
        o => todo!("math in h: {:?}",o)
    }}
}

pub(crate) fn do_vcenter(start:SRef,end:SRef,children:Box<[VNode<Types>]>, state:&mut ShipoutState, engine:Refs) {
    let mut node = HTMLNode::new(VCENTER_CONTAINER, true);
    /*let do_wd = if bx.width() == ZERO {
        node.width(ZERO);
        true
    } else { false };*/
    state.do_in(node,|state| {
        let mut node = HTMLNode::new(VCENTER_INNER, true);
        /*if do_wd {
            node.style_str("width", "100%");
        }*/
        state.do_in(node,|state| {
            do_vlist(engine,state,&mut children.into_vec().into(),false);
        },|_,node| if node.label == VCENTER_INNER {Some(node)} else {
            todo!()
        })
    },|_,node| if node.label == VCENTER_CONTAINER {Some(node)} else {
        todo!()
    })
}

pub(crate) fn do_math(mut bx:MathGroup<Types,MathFontStyle<Font>>, state:&mut ShipoutState, engine:Refs) {
    if bx.display {
        todo!()
    } else {
        let mut node = HTMLNode::new(MATH, true);
        node.sourceref(bx.start,bx.end);
        state.do_in(node,|state| {
            let node = HTMLNode::new(MATH_ROW, true);
            state.do_in(node,|state| {
                super::do_mathlist(engine,state,&mut bx.children.into_vec().into());
            },|_,node| if node.label == MATH_ROW {Some(node)} else {
                todo!()
            });
        },|_,node| if node.label == MATH {Some(node)} else {
            todo!()
        })

    }
}

pub(crate) fn do_missing_glyph(char:u8,font:&Font,state:&mut ShipoutState) {
    let mut node = HTMLNode::new(MISSING_GLYPH, false);
    node.attr("title",format!("Missing Glyph: {} in font {}",char,font.filename()));
    let missing = tex_tfm::glyphs::Glyph::get("missingglyph");
    node.push_glyph(missing);
    state.push(node);
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

pub(crate) fn do_raise<F:FnOnce(Bx,&mut ShipoutState)>(mut bx:Bx, state:&mut ShipoutState,inv:bool, f:F) {
    let d = match bx {
        TeXBox::H {info: HBoxInfo::HBox { ref mut raised, .. },..} => std::mem::take(raised).unwrap(),
        TeXBox::V{ info: VBoxInfo::VBox { ref mut raised, .. },..} => std::mem::take(raised).unwrap(),
        TeXBox::V{ info: VBoxInfo::VTop { ref mut raised, .. },..} => std::mem::take(raised).unwrap(),
        _ => unreachable!()
    };
    let mut node = HTMLNode::new(RAISE, inv);
    node.style("bottom",dim_to_string(d));
    state.do_in(node,|state| {
        f(bx, state)
    },|_,node| if node.label == RAISE {Some(node)} else {
        todo!()
    })
}

pub(crate) fn do_moveleft<F:FnOnce(Bx,&mut ShipoutState)>(mut bx:Bx, state:&mut ShipoutState,inv:bool, f:F) {
    let d = match bx {
        TeXBox::H {info: HBoxInfo::HBox { ref mut moved_left, .. },..} => std::mem::take(moved_left).unwrap(),
        TeXBox::V{ info: VBoxInfo::VBox { ref mut moved_left, .. },..} => std::mem::take(moved_left).unwrap(),
        TeXBox::V{ info: VBoxInfo::VTop { ref mut moved_left, .. },..} => std::mem::take(moved_left).unwrap(),
        _ => unreachable!()
    };
    let mut node = HTMLNode::new(MOVE_RIGHT, inv);
    node.style("margin-left",dim_to_string(-d));
    state.do_in(node,|state| {
        f(bx, state)
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

pub(crate) fn mskip(state:&mut ShipoutState, mskip:Mu) {
    if mskip == ZERO_MATH { return }
    let mut node = HTMLNode::new(MSKIP, true);
    node.attr("width",mudim_to_string(mskip));
    state.push(node)
}

pub(crate) fn do_mathchar(engine:Refs, state:&mut ShipoutState,current_class: &mut Option<(MathClass,HTMLNode)>,char:u8,cls:MathClass,font:Font) {
    use tex_tfm::fontstyles::FontModifiable;
    state.in_content = true;
    let glyphtable = engine.fontsystem.glyphmaps.get_glyphlist(font.filename());
    let glyph = glyphtable.get(char);
    if !glyph.is_defined() {
        do_missing_glyph(char,&font,state);
    }
    let modifiers = match engine.fontsystem.glyphmaps.get_info(font.filename()) {
        Some(mds) if mds.styles != ModifierSeq::empty() => Some(mds.styles),
        _ => None
    };
    match current_class {
        Some((mc2,ref mut node)) if *mc2 == cls => {
            if let Some(m) = modifiers {
                node.push_text(glyph.to_string().apply(m).to_string());
            } else {
                node.push_glyph(glyph)
            }
        },
        Some((ref mut c,ref mut n)) => {
            *c = cls;
            let mut node = node_from_class(cls);
            if let Some(m) = modifiers {
                node.push_text(glyph.to_string().apply(m).to_string());
            } else {
                node.push_glyph(glyph)
            }
            let old = std::mem::replace(n,node);
            state.push(old)
        }
        r@None => {
            let mut node = node_from_class(cls);
            if let Some(m) = modifiers {
                node.push_text(glyph.to_string().apply(m).to_string());
            } else {
                node.push_glyph(glyph)
            }
            *r = Some((cls,node))
        }
    }
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

pub(crate) fn do_paragraph(engine:Refs, state:&mut ShipoutState,children:&mut VNodes,mut spec:Vec<ParLineSpec<Types>>,start:SRef,end:SRef,lineskip: LineSkip) {
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

pub(crate) fn paragraph_list(children:&mut VNodes) -> Vec<HNode<Types>> {
    let mut success = false;
    let mut ret = vec!();
    while let Some(c) = children.next() {
        match c {
            VNode::Custom(RusTeXNode::ParagraphEnd) => {
                success = true;
                break;
            }
            VNode::Box(TeXBox::H { info: HBoxInfo::ParLine { .. },children,..}) => ret.extend(children.into_vec().into_iter()),
            VNode::VSkip(_) | VNode::VFil | VNode::VFill | VNode::VFilneg | VNode::Vss => (),
            o =>
                todo!("{:?}",o)
        }
    }
    if !success { todo!("ERROR!") }
    ret
}

pub(crate) fn do_halign(engine:Refs, state:&mut ShipoutState,children:&mut VNodes) {
    let mut num_cols= 0;
    let mut rows = Vec::new();
    while let Some(row) = children.next() {
        match row {
            VNode::Custom(RusTeXNode::HAlignEnd) => break,
            VNode::Box(TeXBox::H { info: HBoxInfo::HAlignRow,children,start,end }) => {
                num_cols = num_cols.max(children.len());
                rows.push(VNode::Box(TeXBox::H {info:HBoxInfo::HAlignRow,children,start,end}));
            }
            _ => todo!()
        }
    }
    let mut node = HTMLNode::new(HALIGN, true);
    node.style("grid-template-columns",format!("repeat({},1fr)",num_cols));
    state.do_in(node,|state| {
        for row in rows {
            match row {
                VNode::Box(TeXBox::H { info: HBoxInfo::HAlignRow, children,.. }) => {
                    let mut cols = 0;
                    let node = HTMLNode::new(HALIGN_ROW, true);
                    state.do_in(node,|state| {
                        for c in children.into_vec() {
                            cols += 1;
                            match c {
                                HNode::Box(bx@ TeXBox::H { info: HBoxInfo::HAlignCell {.. },.. }) => {
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