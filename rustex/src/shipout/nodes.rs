use tex_engine::pdflatex::nodes::{NumOrName, PDFColor, PDFNode};
use tex_engine::engine::EngineTypes;
use tex_engine::engine::filesystem::File;
use tex_engine::tex::numerics::{Dim32, Fill, Mu, Skip, Skip32};
use crate::engine::{Bx, Font, Refs, SRef, Types};
use crate::html::{dim_to_num, dim_to_string, HTMLChild, HTMLNode, HTMLTag, mudim_to_string};
use tex_engine::tex::nodes::NodeTrait;
use tex_engine::tex::types::MathClass;
use crate::nodes::{LineSkip, RusTeXNode};
use tex_engine::engine::fontsystem::{Font as FontT, FontSystem};
use tex_engine::engine::stomach::ParLineSpec;
use tex_engine::tex::nodes::boxes::{HBoxInfo, TeXBox, ToOrSpread, VBoxInfo};
use tex_engine::tex::nodes::horizontal::HNode;
use tex_engine::tex::nodes::math::{MathAtom, MathFontStyle, MathGroup, MathKernel, MathNode, MathNucleus};
use tex_engine::tex::nodes::vertical::VNode;
use tex_tfm::fontstyles::ModifierSeq;
use crate::shipout::{do_hlist, do_vlist, MNodes, ShipoutMode, ShipoutState, VNodes, ZERO_SKIP};

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
    if left.base != Dim32(0) {
        nv.insert(0,HNode::HKern(left.base))
    }
    nv.extend(it);
    if right.base != Dim32(0) {
        nv.push(HNode::HKern(right.base))
    }
    (Alignment::from(left,right),nv)
}

fn get_box_dims(bx: &Bx,state:&ShipoutState,scale:fn(&Bx) -> Dim32) -> (Option<Dim32>,Option<Dim32>,Option<Dim32>,Option<Dim32>) {
    let wd = match bx.assigned_width() {
        Some(w) =>  Some(w),
        _ if state.mode() == ShipoutMode::Top => match bx.width() {
            w if w < Dim32(0) => Some(Dim32(0)),
            _ => None
        }
        _ => None
    };
    let (ht,mut bottom) = match bx.assigned_height() {
        Some(h) if h < Dim32(0) => (Some(Dim32(0)),Some(h)),
        Some(h) => (Some(h),None),
        _ if state.mode() == ShipoutMode::Top && bx.height() < Dim32(0) => (Some(Dim32(0)),None),
        _ => (None,None)
    };
    match (bottom,bx.assigned_depth()) {
        (Some(b),Some(d)) => {
            let s = b + d;
            if s != Dim32(0) { bottom = Some(b);
            } else { bottom = None; }
        }
        (_,Some(d)) if d != Dim32(0) => bottom = Some(d),
        _ => ()
    }
    let to = match bx.to_or_scaled() {
        ToOrSpread::To(d) => Some(d),
        ToOrSpread::Spread(s) => Some(s + scale(bx)),
        _ => None
    };
    (wd,ht,bottom,to)
}

// ----------------------------------------------------------------------------------------

pub(crate) fn vskip(state:&mut ShipoutState, skip:Skip32<Dim32>) {
    if skip == ZERO_SKIP { return }
    state.push_child(HTMLChild::VSkip(skip.base))
}
pub(crate) fn hskip(state:&mut ShipoutState, skip:Skip32<Dim32>) {
    if skip == ZERO_SKIP { return }
    state.push_child(HTMLChild::HSkip(skip.base))
}

pub(crate) fn do_missing_glyph(char:u8,font:&Font,state:&mut ShipoutState) {
    let s = match state.mode() {
        ShipoutMode::H{..} | ShipoutMode::Par => "span",
        ShipoutMode::Math => "mtext",
        _ => unreachable!()
    };
    state.push_child(HTMLChild::Comment(
        format!("<{} class=\"rustex-missing\" title=\"Missing Glyph: {} in font {}\"></{}>",s,char,font.filename(),s)
    ));
}

pub(crate) fn do_paragraph(engine:Refs, state:&mut ShipoutState,children:&mut VNodes,mut spec:Vec<ParLineSpec<Types>>,start:SRef,end:SRef,lineskip: LineSkip,parskip:Option<Skip32<Dim32>>) {
    if let Some(parskip) = parskip {
        state.push_child(HTMLChild::VSkip(parskip.base));
    }
    let spec = spec.pop().unwrap();
    let align: Alignment = Alignment::from(spec.leftskip,spec.rightskip);
    let mut node = HTMLNode::paragraph(start,end,spec.target);
    if spec.leftskip.base != Dim32(0) {
        node.styles.insert("margin-left".into(),dim_to_string(spec.leftskip.base).into());
    }
    if spec.rightskip.base != Dim32(0) {
        node.styles.insert("margin-right".into(),dim_to_string(spec.rightskip.base).into());
    }
    match align {
        Alignment::S => (),
        Alignment::L => {node.styles.insert("text-align".into(),"left".into());}
        Alignment::R => {node.styles.insert("text-align".into(),"right".into());}
        Alignment::C => {node.styles.insert("text-align".into(),"center".into());}
    };

    let children = paragraph_list(children);

    state.do_in(
        HTMLNode::paragraph(start,end,spec.target),
        Some(ShipoutMode::Par),|state| {
            state.widths.push(spec.target);
            state.lineskip.push(lineskip);
            do_hlist(engine,state,&mut children.into());
            state.widths.pop();
            state.lineskip.pop();
        }
    );
}

pub(crate) fn paragraph_list(children:&mut VNodes) -> Vec<HNode<Types>> {
    let mut success = false;
    let mut ret = vec!();
    let mut later = vec!();
    while let Some(c) = children.next() {
        match c {
            VNode::Custom(RusTeXNode::ParagraphEnd) => {
                success = true;
                break;
            }
            VNode::Box(TeXBox::H { info: HBoxInfo::ParLine { .. },children,..}) => ret.extend(children.into_vec().into_iter()),
            VNode::VSkip(_) | VNode::VFil | VNode::VFill | VNode::VFilneg | VNode::Vss | VNode::Mark(..) | VNode::VKern(_) => (),
            _ => later.push(c),
        }
    }
    if !success { todo!("ERROR!") }
    children.prefix(later);
    ret
}

pub(crate) fn do_vbox(state:&mut ShipoutState, engine:Refs,bx:Bx) {
    let (wd,ht,bottom,to) = get_box_dims(&bx,state,|bx| bx.height());
    let (children,start,end) = if let TeXBox::V {children,start,end,..} = bx { (children.into_vec(),start,end) } else {unreachable!()};
    /*if wd.is_none() && ht.is_none() && bottom.is_none() && to.is_none() && state.mode().is_v() {
        return do_vlist(engine,state,&mut children.into(),true);
    }*/
    let mut node = HTMLNode::new(HTMLTag::VBoxContainer);
    node.classes.push("rustex-vbox-container".into());
    if let Some(b) = bottom { node.styles.insert("margin-bottom".into(),dim_to_string(b).into()); }
    if let Some(w) = wd { node.width = Some((w,false)); }
    state.do_in(node,None,|state| match ht {
        Some(h) => {
            let mut node = HTMLNode::new(HTMLTag::VBoxHeight);
            node.classes.push("rustex-vbox-height-container".into());
            node.styles.insert("height".into(),dim_to_string(h).into());
            state.do_in(node,None,|state| {
                vbox_inner(state,engine,wd.is_some(),to,children,start,end)
            })
        }
        None =>
            vbox_inner(state,engine,wd.is_some(),to,children,start,end),
    });
}

fn vbox_inner(state:&mut ShipoutState, engine:Refs,has_wd:bool, to:Option<Dim32>, children:Vec<VNode<Types>>, start:SRef, end:SRef) {
    let mut node = HTMLNode::new(HTMLTag::VBox);
    node.classes.push("rustex-vbox".into());
    node.sourceref = Some((start,end));
    match to {
        Some(d) if d < Dim32(0) => {
            node.styles.insert("height".into(),"0".into());
            node.styles.insert("margin-bottom".into(),dim_to_string(d).into());
        }
        Some(d) => {node.styles.insert("height".into(),dim_to_string(d).into());}
        _ => ()
    }
    state.do_in(node,Some(ShipoutMode::V),|state| {
        do_vlist(engine,state,&mut children.into(),true);
    })
}

pub(crate) fn do_vtop(state:&mut ShipoutState, engine:Refs, mut bx:Bx) {
    let wd = bx.assigned_width();
    let ht = bx.assigned_height();
    let dp = bx.assigned_depth();
    let to = bx.to_or_scaled();
    let cht = bx.height();
    let (start,end) = if let TeXBox::V {start,end,..} = bx { (start,end) } else {unreachable!()};
    if wd.is_none() && ht.is_none() && dp.is_none() && to == ToOrSpread::None && state.mode().is_v() {
        let TeXBox::V {children,..} = bx else {unreachable!()};
        return do_vlist(engine,state,&mut children.into_vec().into(),false);
    }
    let mut node = HTMLNode::new(HTMLTag::VTopContainer);
    node.classes.push("rustex-vtop-container".into());
    if let Some(w) = wd { node.width = Some((w,false)); }

    let to = match to {
        ToOrSpread::To(d) => Some(d),
        ToOrSpread::Spread(s) => Some(s + cht),
        _ => None
    };

    state.do_in(node,None,|state| match (ht,dp) {
        (Some(_),_)|(_,Some(_)) => {
            let mut node = HTMLNode::new(HTMLTag::VTopHeight);
            node.classes.push("rustex-vtop-height-container".into());

            let oht = if let TeXBox::V {info:VBoxInfo::VTop {computed_height,computed_depth,..},children,..} = &bx {
                *computed_height.get_or_init(||
                    match children.first() {
                        Some(c@VNode::Box(..)) => c.height(),
                        _ => Dim32(0)
                    }
                )
            } else {unreachable!()};
            if let Some(ah) = ht {
                let ht = ah - oht;
                node.styles.insert("margin-top".into(),dim_to_string(ht).into());
                node.styles.insert("bottom".into(),dim_to_string(ht).into());
            }
            if let Some(dp) = dp {
                let dp = dp + oht;
                node.styles.insert("height".into(),dim_to_string(dp).into());
            }
            let TeXBox::V {children,..} = bx else {unreachable!()};

            state.do_in(node,None,|state| {
                vtop_inner(state,engine,to, children.into_vec(), start, end)
            })
        }
        _ => {
            let TeXBox::V {children,..} = bx else {unreachable!()};
            vtop_inner(state,engine,to, children.into_vec(), start, end)
        },
    })
}

fn vtop_inner(state:&mut ShipoutState, engine:Refs, to:Option<Dim32>, children:Vec<VNode<Types>>, start:SRef, end:SRef) {
    let mut node = HTMLNode::new(HTMLTag::VTop);
    node.classes.push("rustex-vtop".into());
    node.sourceref = Some((start,end));
    match to {
        Some(d) if d < Dim32(0) => {
            node.styles.insert("height".into(),"0".into());
            node.styles.insert("margin-bottom".into(),dim_to_string(d).into());
        }
        Some(d) => {node.styles.insert("height".into(),dim_to_string(d).into());}
        _ => ()
    }
    state.do_in(node,Some(ShipoutMode::V),|state| {
        do_vlist(engine,state,&mut children.into(),true);
    })
}

pub(crate) fn do_hbox(state:&mut ShipoutState, engine:Refs, bx:Bx) {
    let (wd,ht,bottom,to) = get_box_dims(&bx,state,|bx| bx.width());
    let (children,start,end) = if let TeXBox::H {children,start,end,..} = bx { (children.into_vec(),start,end) } else {unreachable!()};
    if wd.is_none() && ht.is_none() && bottom.is_none() && to.is_none() && state.mode().is_h() {
        return do_hlist(engine,state,&mut children.into());
    }
    if wd.is_none() && ht.is_none() && bottom.is_none() {
        return hbox_inner(state,engine,to,children,start,end);
    }
    let mut node = HTMLNode::new(HTMLTag::HBoxContainer);
    node.classes.push("rustex-hbox-container".into());
    if let Some(b) = bottom { node.styles.insert("margin-bottom".into(),dim_to_string(b).into()); }
    if let Some(h) = ht { node.styles.insert("height".into(),dim_to_string(h).into()); }
    if let Some(w) = wd { node.width = Some((w,false)); }
    state.do_in(node,None,|state| {
        hbox_inner(state,engine,to,children,start,end)
    });
}

fn hbox_inner(state:&mut ShipoutState, engine:Refs,to:Option<Dim32>, children:Vec<HNode<Types>>, start:SRef, end:SRef) {
    let (a,children) = alignment(children);
    let mut node = HTMLNode::new(HTMLTag::HBox);
    node.classes.push("rustex-hbox".into());
    node.sourceref = Some((start,end));
    if let Some(wd) = to { node.width = Some((wd,false)); }
    let escape = match a {
        Alignment::L => {
            node.styles.insert("justify-content".into(),"start".into());
            false
        },
        Alignment::R => {
            node.styles.insert("justify-content".into(),"end".into());
            false
        },
        Alignment::C => {
            node.styles.insert("justify-content".into(),"center".into());
            false
        },
        _ => to.is_some(),
    };
    state.do_in(node,Some(ShipoutMode::H {escape}),|state| {
        do_hlist(engine,state,&mut children.into());
    })
}


pub(crate) fn hrule(state:&mut ShipoutState,start:SRef, end:SRef, width:Option<Dim32>, height:Dim32, depth:Dim32) {
    let ht = height + depth;
    if ht == Dim32(0) {return}
    if state.mode() == ShipoutMode::Math { todo!() }
    let bottom = if depth == Dim32(0) {None} else {Some(-depth)};
    state.push_child(HTMLChild::HRule {width,height:ht,bottom,start,end,color:*state.colors.last().unwrap()});
}

pub(crate) fn vrule(state:&mut ShipoutState,start:SRef, end:SRef, width:Dim32, height:Option<Dim32>, depth:Option<Dim32>) {
    let mode = state.mode();
    if mode == ShipoutMode::Math { todo!() }
    if width == Dim32(0) { return }
    match (height,depth) {
        (None,None) =>
            state.push_child(HTMLChild::VRuleS {width,
            font_size:if mode == ShipoutMode::Par {Some(state.fonts.last().unwrap().get_at())} else {None},
            start,end,color:*state.colors.last().unwrap()
        }),
        _ => {
            let ht = height.unwrap_or(Dim32(0)) + depth.unwrap_or(Dim32(0));
            let dp = if depth == None && mode == ShipoutMode::Par {None} else {Some(depth.unwrap_or(Dim32(0)))};
            state.push_child(HTMLChild::VRuleC {width,height:ht,depth:dp,start,end,color:*state.colors.last().unwrap()});
        }
    }
    /*
    let mut node = HTMLNode::new(VRULE_INNER, false);
    node.sourceref(start, end);
    node.style("background",color);
    // height things
    // TODO maybe relativize;
    if ht == ZERO {
        node.style("width", dim_to_string(width));
        node.style("min-width", dim_to_string(width));
        if par {
            todo!()
        } else { node.style_str("align-self","stretch") }
        state.push(node,false,false)
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
        state.push(cont,false,false);
    }

     */
}

/*

use crate::html::labels::*;
use crate::shipout::annotations::close_all;


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

pub(crate) fn do_math(mut bx:MathGroup<Types,MathFontStyle<Types>>, state:&mut ShipoutState, engine:Refs) {
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

pub(crate) fn do_pdfdest(state:&mut ShipoutState, id:NumOrName) {
    let mut node = HTMLNode::new(DEST, false);
    let target = match id {
        NumOrName::Num(n) => format!("NUM_{}",n),
        NumOrName::Name(n) => n
    };
    node.attr("name",target.clone());
    node.attr("id",target);
    state.push(node,false,false);
}

pub(crate) fn do_raise<F:FnOnce(Bx,&mut ShipoutState)>(mut bx:Bx, state:&mut ShipoutState,inv:bool, f:F) {
    let (d,v) = match bx {
        TeXBox::H {info: HBoxInfo::HBox { ref mut raised, .. },..} => (std::mem::take(raised).unwrap(),false),
        TeXBox::V{ info: VBoxInfo::VBox { ref mut raised, .. },..} => (std::mem::take(raised).unwrap(),true),
        TeXBox::V{ info: VBoxInfo::VTop { ref mut raised, .. },..} => (std::mem::take(raised).unwrap(),true),
        _ => unreachable!()
    };
    let mut node = HTMLNode::new(RAISE, inv);
    node.style("bottom",dim_to_string(d));
    if v {
        node.style_str("flex-direction","column")
    } else {
        node.style_str("flex-direction","row")
    }
    state.do_in(node,|state| {
        f(bx, state)
    },|_,node| if node.label == RAISE {Some(node)} else {
        todo!()
    })
}

pub(crate) fn do_moveleft<F:FnOnce(Bx,&mut ShipoutState)>(mut bx:Bx, state:&mut ShipoutState,inv:bool, f:F) {
    let (d,v) = match bx {
        TeXBox::H {info: HBoxInfo::HBox { ref mut moved_left, .. },..} => (std::mem::take(moved_left).unwrap(),false),
        TeXBox::V{ info: VBoxInfo::VBox { ref mut moved_left, .. },..} => (std::mem::take(moved_left).unwrap(),true),
        TeXBox::V{ info: VBoxInfo::VTop { ref mut moved_left, .. },..} => (std::mem::take(moved_left).unwrap(),true),
        _ => unreachable!()
    };
    let mut node = HTMLNode::new(MOVE_RIGHT, inv);
    if v {
        node.style_str("flex-direction","column")
    } else {
        node.style_str("flex-direction","row")
    }
    node.style("margin-left",dim_to_string(-d));
    state.do_in(node,|state| {
        f(bx, state)
    },|_,node| Some(node))
}


pub(crate) fn hskip(state:&mut ShipoutState, skip:Skip32<Dim32>) {
    if skip == ZERO_SKIP { return }
    let mut node = HTMLNode::new(HSKIP, false);
    node.style("margin-left",dim_to_string(skip.base));
    match skip.stretch {
        Some(Fill::fil(_)) => {
            node.style_str("margin-right","auto");
            state.push(node,false,false)
        }
        Some(Fill::fill(_)) => {
            node.style_str("margin-right","auto");
            state.push(node,false,false);
            let mut node = HTMLNode::new(HSKIP, false);
            node.style_str("margin-right","auto");
            state.push(node,false,false);
        }
        _ => state.push(node,false,false)
    }
}

pub(crate) fn mskip(state:&mut ShipoutState, mskip:Mu) {
    if mskip == ZERO_MATH { return }
    let mut node = HTMLNode::new(MSKIP, true);
    node.attr("width",mudim_to_string(mskip));
    state.push(node,true,false)
}

pub(crate) fn do_mathchar(engine:Refs, state:&mut ShipoutState,current_class: &mut Option<(MathClass,HTMLNode)>,char:u8,cls:MathClass,font:Font) {
    use tex_tfm::fontstyles::FontModifiable;
    state.in_content = true;
    let glyphtable = engine.fontsystem.glyphmaps.get_glyphlist(font.filename());
    let glyph = glyphtable.get(char);
    if !glyph.is_defined() {
        do_missing_glyph(char,&font,state,true,false);
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
            state.push(old,true,false)
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


fn wrap<F:FnOnce(&mut ShipoutState)>(state:&mut ShipoutState,f:F) {
    let node = HTMLNode::default();
    state.do_in(node,|state| {
        f(state)
    },|_,mut node| if node.label == DUMMY {
        if node.children.len() == 1 {
            if let Some(HTMLChild::Node(n)) = node.children.pop() {
                Some(n)
            } else {unreachable!()}
        } else {Some(node)}
    } else {
        todo!()
    })
}
pub(crate) fn do_sub_sup(engine:Refs, state:&mut ShipoutState,a: MathAtom<Types,MathFontStyle<Types>>) {
    match (a.nucleus,a.sub,a.sup) {
        (n@MathNucleus::Simple{limits:Some(true),..},Some(sub),Some(sup)) => {
            let node = HTMLNode::new(MUNDEROVER, true);
            state.do_in(node,|state| {
                wrap(state,|state| do_nucleus(engine,state,n));
                wrap(state,|state| super::do_mathlist(engine,state,&mut sub.into_vec().into()));
                wrap(state,|state| super::do_mathlist(engine,state,&mut sup.into_vec().into()));
            },|_,node| if node.label == MUNDEROVER {Some(node)} else {
                todo!()
            })
        }
        (n,Some(sub),Some(sup)) => {
            let node = HTMLNode::new(MSUBSUP, true);
            state.do_in(node,|state| {
                wrap(state,|state| do_nucleus(engine,state,n));
                wrap(state,|state| super::do_mathlist(engine,state,&mut sub.into_vec().into()));
                wrap(state,|state| super::do_mathlist(engine,state,&mut sup.into_vec().into()));
            },|_,node| if node.label == MSUBSUP {Some(node)} else {
                todo!()
            })
        }
        (n@MathNucleus::Simple{limits:Some(true),..},Some(sub),_) => {
            let node = HTMLNode::new(MUNDER, true);
            state.do_in(node,|state| {
                wrap(state,|state| do_nucleus(engine,state,n));
                wrap(state,|state| super::do_mathlist(engine,state,&mut sub.into_vec().into()));
            },|_,node| if node.label == MUNDER {Some(node)} else {
                todo!()
            })
        }
        (n@MathNucleus::Simple{limits:Some(true),..},_,Some(sup)) => {
            let node = HTMLNode::new(MOVER, true);
            state.do_in(node,|state| {
                wrap(state,|state| do_nucleus(engine,state,n));
                wrap(state,|state| super::do_mathlist(engine,state,&mut sup.into_vec().into()));
            },|_,node| if node.label == MOVER {Some(node)} else {
                todo!()
            })
        }
        (n,Some(sub),_) => {
            let node = HTMLNode::new(MSUB, true);
            state.do_in(node,|state| {
                wrap(state,|state| do_nucleus(engine,state,n));
                wrap(state,|state| super::do_mathlist(engine,state,&mut sub.into_vec().into()));
            },|_,node| if node.label == MSUB {Some(node)} else {
                todo!()
            })
        }
        (n,_,Some(sup)) => {
            let node = HTMLNode::new(MSUP, true);
            state.do_in(node,|state| {
                wrap(state,|state| do_nucleus(engine,state,n));
                wrap(state,|state| super::do_mathlist(engine,state,&mut sup.into_vec().into()));
            },|_,node| if node.label == MSUP {Some(node)} else {
                todo!()
            })
        }
        _ => unreachable!()
    };
}

pub(crate) fn do_nucleus(engine:Refs,state:&mut ShipoutState,n:MathNucleus<Types,MathFontStyle<Types>>) {
    use tex_tfm::fontstyles::FontModifiable;
    match n {
        MathNucleus::Simple{kernel:MathKernel::Char {char,style:MathFontStyle{font,..}},cls,..} => {
            state.in_content = true;
            let glyphtable = engine.fontsystem.glyphmaps.get_glyphlist(font.filename());
            let glyph = glyphtable.get(char);
            if !glyph.is_defined() {
                do_missing_glyph(char,&font,state,true,false);
            }
            let modifiers = match engine.fontsystem.glyphmaps.get_info(font.filename()) {
                Some(mds) if mds.styles != ModifierSeq::empty() => Some(mds.styles),
                _ => None
            };
            let mut node = node_from_class(cls);
            if let Some(m) = modifiers {
                node.push_text(glyph.to_string().apply(m).to_string());
            } else {
                node.push_glyph(glyph)
            }
            state.push(node,true,false)
        }
        MathNucleus::Simple {kernel:MathKernel::Empty,..} | MathNucleus::Inner(MathKernel::Empty) => (),
        MathNucleus::Inner(MathKernel::List{children,..}) =>
            super::do_mathlist(engine,state,&mut children.into_vec().into()),
        o => todo!(" {:?}",o)
    }
}

pub(crate) fn box_in_math(engine:Refs,state:&mut ShipoutState,mut bx:Bx) {
    let mut node = HTMLNode::new(MATH_ESCAPE, true);
    node.set_font(state.fonts.last().unwrap().clone());
    node.force_font = true;
    state.do_in(node,|state| {
        match &bx {
            TeXBox::H {..} => {
                let ls = vec!(VNode::Box(bx));
                do_vlist(engine,state,&mut ls.into(),false);
            }
            _ => {
                let ls = vec!(HNode::Box(bx));
                do_hlist(engine,state,&mut ls.into(),false,false);
            }
        }
    },|_,node| if node.label == MATH_ESCAPE {Some(node)} else {
        todo!()
    })
}

enum RowOrNoAlign {
    Row(Vec<HNode<Types>>,SRef,SRef),
    NoAlign(Vec<VNode<Types>>)
}
pub(crate) fn do_halign(engine:Refs, state:&mut ShipoutState,children:&mut VNodes) {
    let mut num_cols= 0;
    let mut rows = Vec::new();
    let mut noalign = Vec::new();
    while let Some(row) = children.next() {
        match row {
            VNode::Custom(RusTeXNode::HAlignEnd) => {
                if !noalign.is_empty() {
                    rows.push(RowOrNoAlign::NoAlign(std::mem::take(&mut noalign)));
                }
                break
            },
            VNode::Box(TeXBox::H { info: HBoxInfo::HAlignRow,children,start,end,.. }) => {
                num_cols = num_cols.max(children.len());
                if !noalign.is_empty() {
                    rows.push(RowOrNoAlign::NoAlign(std::mem::take(&mut noalign)));
                }
                rows.push(RowOrNoAlign::Row(children.into_vec(),start,end));
            }
            _ => noalign.push(row)
        }
    }
    let mut node = HTMLNode::new(HALIGN, true);
    node.style("grid-template-columns",format!("repeat({},1fr)",num_cols));
    state.do_in(node,|state| {
        for row in rows {
            match row {
                RowOrNoAlign::Row(children,.. ) => {
                    let mut cols = 0;
                    let node = HTMLNode::new(HALIGN_ROW, true);
                    state.do_in(node,|state| {
                        for c in children {
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
                        for _ in cols..num_cols {
                            state.push(HTMLNode::new(HALIGN_CELL, true),false,false)
                        }
                    },|_,node| if node.label == HALIGN_ROW {Some(node)} else {
                        todo!()
                    })
                }
                RowOrNoAlign::NoAlign(v) => {
                    let mut node = HTMLNode::new(NOALIGN_H, true);
                    node.style("grid-column",format!("span {}",num_cols));
                    state.do_in(node,|state| {
                        do_vlist(engine,state,&mut v.into(),false)
                    },|_,node| if node.label == NOALIGN_H {Some(node)} else {
                        todo!()
                    })
                }
            }
        }
    },|_,node| if node.label == HALIGN {Some(node)} else {
        todo!()
    })
}


pub(crate) fn do_svg(engine:Refs,state:&mut ShipoutState,bx:TeXBox<Types>,minx:Dim32,miny:Dim32,maxx:Dim32,maxy:Dim32) {
    let node = HTMLNode::new(SVG_WRAP, true);
    state.do_in(node,|state| {
        let mut svg = HTMLNode::new(SVG, true);
        svg.attr("width",dim_to_string(maxx - minx));
        svg.attr("height",dim_to_string(maxy - miny));
        svg.attr("viewBox",format!("{} {} {} {}",dim_to_num(minx.0),dim_to_num(miny.0),dim_to_num(maxx.0 - minx.0),dim_to_num(maxy.0 - miny.0)));
        state.do_in(svg,|state| {
            let mut g = HTMLNode::new(svg_g("g".into()), true);
            g.attr("transform",format!("translate(0,{})",dim_to_num(maxy.0 + miny.0)));
            state.do_in(g,|state| {
                if let TeXBox::H {children,..} = bx {
                    svg_inner(engine,state,&mut children.into_vec().into())
                } else { unreachable!() }
            },|_,node| if node.label.id == 49 {Some(node)} else {
                todo!()
            });
        },|_,node| if node.label == SVG {Some(node)} else {
            todo!()
        })
    },|_,node| if node.label == SVG_WRAP {Some(node)} else {
        todo!()
    })
}

pub fn strtonum(ts : String) -> String {
    (ts.to_string().parse::<f32>().unwrap() * 1.5).to_string()
}

fn svg_inner(engine:Refs,state: &mut ShipoutState, children: &mut HNodes) {
    while let Some(c) = children.next() {
        match c {
            HNode::Custom(RusTeXNode::PGFGBegin {attrs,tag}) => {
                let mut node = HTMLNode::new(svg_g(tag), true);
                for (k,v) in attrs.into_iter() {
                    match k {
                        k if k == "stroke-width" => {
                            node.attr(k, strtonum(v))
                        }
                        k if k == "d" => {
                            node.attr(k, parse_path(v))
                        }
                        k if k == "transform" => {
                            node.attr(k.into(),parse_transform(v))
                        }
                        _ => node.attr(k, v)
                    }
                }
                state.nodes.push(node);
            }
            HNode::Custom(RusTeXNode::PGFGEnd) => {
                let node = close_all(&mut state.nodes);
                state.nodes.last_mut().unwrap().push_node(node);
            }
            HNode::Custom(RusTeXNode::PGFEscape(bx)) => {
                let mut node = HTMLNode::new(SVG_FOREIGN,true);
                let wd = bx.width();
                let ht = bx.height() + bx.depth();
                node.inner_width = Some(wd);
                node.style("width",dim_to_string(wd));
                node.style("height",dim_to_string(ht));
                node.style("translate",format!("0 {}",dim_to_string(-ht)));
                state.do_in(node,|state| {
                    let mut node = HTMLNode::new(SVG_ESCAPE_DIV,true);
                    node.inner_width = Some(wd);
                    state.do_in(node,|state| {
                        do_h(engine,state,HNode::Box(bx),false,false)
                    },|_,node| if node.label == SVG_ESCAPE_DIV {Some(node)} else {
                        todo!()
                    })
                },|_,node| if node.label == SVG_FOREIGN {Some(node)} else {
                    todo!()
                })
            }
            HNode::Char {char,font,..} => {
                if font == engine.fontsystem.null() { continue }
                state.in_content = true;
                let glyphtable = engine.fontsystem.glyphmaps.get_glyphlist(font.filename());
                let glyph = glyphtable.get(char);
                if !glyph.is_defined() {
                    nodes::do_missing_glyph(char,&font,state,false,true);
                } else {
                    state.push_glyph(glyph)
                }
            }
            HNode::Space | HNode::Hss => (),
            HNode::Box(TeXBox::H {children:chs,..}) => children.prefix(chs.into_vec()),
            HNode::Custom(RusTeXNode::FontChange(font,false)) => annotations::do_font(state,font),
            HNode::Custom(RusTeXNode::FontChangeEnd) => annotations::close_font(state,false,true),
            HNode::Custom(RusTeXNode::PDFNode(PDFNode::Color(act))) => annotations::do_color(state,engine,act,false,true),
            o => todo!("svg: {:?}",o)
        }
    }
}

// TODO old code, needs cleanup + optimization

pub fn parse_path(ts : String) -> String {
    fn parse_path_one(s : &str) -> String {
        if s.is_empty() { return "".into() };
        if s.starts_with("M") {
            let (n1, s1) = parse_get_num(s[1..].trim_start());
            let (n2, s2) = parse_get_num(s1.trim_start());
            "M ".to_string() +
                scale(n1).as_str() + " " +
                scale(-n2).as_str() + " " +
                parse_path_one(s2.trim_start()).as_str()
        } else if s.starts_with("L") {
            let (n1, s1) = parse_get_num(s[1..].trim_start());
            let (n2, s2) = parse_get_num(s1.trim_start());
            "L ".to_string() +
                scale(n1).as_str() + " " +
                scale(-n2).as_str() + " " +
                parse_path_one(s2.trim_start()).as_str()
        } else if s.starts_with("C") {
            let (n1, s1) = parse_get_num(s[1..].trim_start());
            let (n2, s2) = parse_get_num(s1.trim_start());
            let (n3, s3) = parse_get_num(s2.trim_start());
            let (n4, s4) = parse_get_num(s3.trim_start());
            let (n5, s5) = parse_get_num(s4.trim_start());
            let (n6, s6) = parse_get_num(s5.trim_start());
            "C ".to_string() +
                scale(n1).as_str() + " " +
                scale(-n2).as_str() + " " +
                scale(n3).as_str() + " " +
                scale(-n4).as_str() + " " +
                scale(n5).as_str() + " " +
                scale(-n6).as_str() + " " +
                parse_path_one(s6.trim_start()).as_str()
        } else if s.starts_with("Z") {
            "Z ".to_string() + parse_path_one(s[1..].trim_start()).as_str()
        } else if s.starts_with("h") {
            let (n1, s1) = parse_get_num(s[1..].trim_start());
            "h ".to_string() +
                scale(n1).as_str() + " " +
                parse_path_one(s1.trim_start()).as_str()
        } else if s.starts_with("v") {
            let (n1, s1) = parse_get_num(s[1..].trim_start());
            "v ".to_string() +
                scale(-n1).as_str() + " " +
                parse_path_one(s1.trim_start()).as_str()
        } else {
            todo!("Foo!")
        }
    }
    parse_path_one(ts.trim())
}

pub fn parse_transform(ts : String) -> String {
    fn parse_one(s : &str) -> String {
        if s.is_empty() { return "".into() };
        if s.starts_with("translate(") {
            let (n1,s1) = parse_get_num(&s[10..]);
            let (n2,s2) = parse_get_num(s1);
            "translate(".to_string() + scale(n1).as_str() +
                "," + scale(-n2).as_str() + ")" + &parse_one(s2)
        } else if s.starts_with("matrix(") {
            let (n1, s1) = parse_get_num(s[7..].trim_start());
            let (n2, s2) = parse_get_num(s1.trim_start());
            let (n3, s3) = parse_get_num(s2.trim_start());
            let (n4, s4) = parse_get_num(s3.trim_start());
            let (n5, s5) = parse_get_num(s4.trim_start());
            let (n6, s6) = parse_get_num(s5.trim_start());
            "matrix(".to_string() +
                n1.to_string().as_str() + "," +
                n2.to_string().as_str() + "," +
                n3.to_string().as_str() + "," +
                n4.to_string().as_str() + "," +
                scale(n5).as_str() + "," +
                scale(-n6).as_str() + ")" + &parse_one(s6)
        } else {
            todo!("Foo!")
        }
    }
    parse_one(ts.trim())
}
fn parse_get_num<'a>(s:&'a str) -> (f32,&'a str) {
    match s.find(|x| x == ' ' || x == ')' || x == ',') {
        Some(i) =>
            (s[..i].parse::<f32>().unwrap(), &s[i + 1..]),
        None => (s.parse::<f32>().unwrap(), "")
    }
}

fn scale(f:f32) -> String {
    (1.5 * f).to_string()
}

 */