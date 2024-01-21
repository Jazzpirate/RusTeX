use tex_engine::pdflatex::nodes::{NumOrName, PDFColor, PDFNode};
use tex_engine::engine::EngineTypes;
use tex_engine::engine::filesystem::File;
use tex_engine::tex::numerics::{Dim32, StretchShrink, Mu, MuStretchShrink, Skip};
use crate::engine::{Bx, Font, Refs, SRef, Types};
use crate::html::{dim_to_num, dim_to_string, HTMLChild, HTMLNode, HTMLTag, mudim_to_string};
use tex_engine::tex::nodes::NodeTrait;
use crate::nodes::{LineSkip, RusTeXNode};
use tex_engine::engine::fontsystem::{Font as FontT, FontSystem};
use tex_engine::engine::stomach::methods::ParLineSpec;
use tex_engine::tex::nodes::boxes::{HBoxInfo, TeXBox, ToOrSpread, VBoxInfo};
use tex_engine::tex::nodes::horizontal::HNode;
use tex_engine::tex::nodes::math::{MathAtom, MathClass, MathFontStyle, MathGroup, MathKernel, MathNode, MathNucleus};
use tex_engine::tex::nodes::vertical::VNode;
use tex_tfm::fontstyles::ModifierSeq;
use crate::shipout::{do_hlist, do_mathlist, do_vlist, HNodes, MNode, MNodes, nodes, ShipoutMode, ShipoutState, VNodes, ZERO_SKIP};
use tex_engine::tex::numerics::TeXDimen;
use tex_tfm::glyphs::GlyphName;

pub(crate) trait MuAdd {
    fn merge(&mut self,sk:Dim32,f:&Font);
}
impl MuAdd for Mu {
    fn merge(&mut self,sk2:Dim32,f:&Font) {
        let em = f.get_dim(5);
        let nb = (((sk2.0 as f32) * 18.0 / (em.0 as f32)) * 65536.0) as i32;
        self.0 += nb;
    }
}
/*
pub(crate) fn mu_to_dim(mu:Mu,f:&Font) -> Dim32 {
    let em = f.get_dim(5);
    Dim32(((mu.0 as f32) * (em.0 as f32) / 65536.0 / 18.0) as i32)
}
pub(crate) fn muskip_to_skip(ms:MuSkip,f:&Font) -> Skip<Dim32> {
    let em : Dim32 = f.get_dim(5);
    let base = em.scale_float((ms.base.0 as f32) / (65536.0 * 18.0));
    let stretch = match ms.stretch {
        Some(MuFill::mu(i)) => Some(Fill::pt(Dim32(i))),
        Some(MuFill::fil(i)) => Some(Fill::fil(i)),
        Some(MuFill::fill(i)) => Some(Fill::fill(i)),
        None => None
    };
    let shrink = match ms.shrink {
        Some(MuFill::mu(i)) => Some(Fill::pt(Dim32(i))),
        Some(MuFill::fil(i)) => Some(Fill::fil(i)),
        Some(MuFill::fill(i)) => Some(Fill::fill(i)),
        None => None
    };
    Skip { base,stretch,shrink }
}
 */

pub(crate) trait SkipAdd {
    fn merge(&mut self,sk:Skip<Dim32>);
    fn set_fil(&mut self);
    fn set_fill(&mut self);
}
impl SkipAdd for Skip<Dim32> {
    fn merge(&mut self,sk2:Skip<Dim32>) {
        let base = self.base + sk2.base;
        let stretch = match (self.stretch,sk2.stretch) {
            (Some(StretchShrink::Fill(a)),_)|(_,Some(StretchShrink::Fill(a))) => Some(StretchShrink::Fill(a)),
            (Some(StretchShrink::Fil(a)),_)|(_,Some(StretchShrink::Fil(a))) => Some(StretchShrink::Fil(a)),
            _ => None,
        };
        *self = Skip { base,stretch,shrink:None };
    }
    fn set_fil(&mut self) {
        if let Some(StretchShrink::Fill(_)) = self.stretch { return }
        self.stretch = Some(StretchShrink::Fil(1))
    }
    fn set_fill(&mut self) {
        self.stretch = Some(StretchShrink::Fill(1))
    }
}

#[derive(PartialEq,Copy,Clone)]
pub(crate) enum Alignment {
    L,R,C,S
}
impl Alignment {
    pub fn from(skip1:Skip<Dim32>,skip2:Skip<Dim32>) -> Self {
        use Alignment::*;
        match (skip1.stretch,skip2.stretch) {
            (None|Some(StretchShrink::Dim(_)),None|Some(StretchShrink::Dim(_))) => S,
            (Some(StretchShrink::Fil(_)),Some(StretchShrink::Fil(_))) => C,
            (Some(StretchShrink::Fill(_)),Some(StretchShrink::Fill(_))) => C,
            (Some(StretchShrink::Filll(_)),Some(StretchShrink::Filll(_))) => C,
            (Some(StretchShrink::Filll(_)),_) => R,
            (_,Some(StretchShrink::Filll(_))) => L,
            (Some(StretchShrink::Fill(_)),_) => R,
            (_,Some(StretchShrink::Fill(_))) => L,
            (Some(StretchShrink::Fil(_)),_) => R,
            (_,Some(StretchShrink::Fil(_))) => L,
        }
    }
}

fn align_i(n:HNode<Types>,remove_space:bool,skip:&mut Skip<Dim32>,v:Option<&mut Vec<HNode<Types>>>,rep:&mut Vec<HNode<Types>>) -> bool {
    match n {
        HNode::HKern(n) => skip.base = skip.base + n,
        HNode::HSkip(sk) => skip.merge(sk),
        HNode::HFil | HNode::Hss => skip.set_fil(),
        HNode::HFill => skip.set_fill(),
        HNode::Penalty(_) | HNode::Mark(..) => (),
        HNode::Space if remove_space => (),
        HNode::Custom(RusTeXNode::PDFNode(PDFNode::PDFDest {..} | PDFNode::Color(..)) | RusTeXNode::FontChange(..) | RusTeXNode::FontChangeEnd) | HNode::Space => rep.push(n),
        _ => {
            match v {
                Some(v) => v.push(n),
                _ => rep.push(n)
            }
            return true
        }
    }
    false
}
pub(crate) fn alignment(mut v:Vec<HNode<Types>>) -> (Alignment, Vec<HNode<Types>>) {
    let (mut left,mut right) = (ZERO_SKIP,ZERO_SKIP);
    let mut repush = Vec::new();
    while let Some(n) = v.pop() { if align_i(n,true,&mut right,Some(&mut v),&mut repush) {break} }
    v.extend(repush.into_iter().rev());
    let mut nv = vec!();
    let mut it = v.into_iter();
    while let Some(n) = it.next() { if align_i(n,false,&mut left,None,&mut nv) {break} }
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

pub(crate) fn class_from_mathclass(cls:MathClass,cramped:bool) -> &'static str {
    use MathClass::*;
    if cramped { return "rustex-math-cramped" }
    match cls {
        Ord => "rustex-math-ord",
        Op => "rustex-math-op",
        Bin => "rustex-math-bin",
        Rel => "rustex-math-rel",
        Open => "rustex-math-open",
        Close => "rustex-math-close",
        Punct => "rustex-math-punct",
    }
}
pub(crate) fn node_from_class(cls:MathClass,cramped:bool) -> HTMLNode {
    use MathClass::*;
    match cls {
        Ord => HTMLNode::new(HTMLTag::Mi),
        o => {
            let mut node = HTMLNode::new(HTMLTag::Mo);
            node.attrs.insert("stretchy".into(), "false".into());
            node.classes.push(class_from_mathclass(o,cramped).into());
            node
        }
    }
}

pub(crate) fn wrap<F:FnOnce(&mut ShipoutState)>(state:&mut ShipoutState,refs:Option<(SRef,SRef)>,f:F) {
    let node = HTMLNode::new(HTMLTag::MathGroup);
    let mut node = state.do_in_and(node, None,|state| f(state));
    match (refs,node.sourceref) {
        (Some(r),None) => node.sourceref = Some(r),
        _ => ()
    }
    if node.children.len() == 1 {
        match node.children.pop() {
            Some(HTMLChild::Node(n)) => state.push(n),
            Some(o) => {
                node.children.push(o);
                state.push(node)
            }
            None => unreachable!()
        }
    }
    else {
        state.push(node)
    }
}

// ----------------------------------------------------------------------------------------

pub(crate) fn do_missing_glyph(state:&mut ShipoutState,name:GlyphName,char:u8,font:&Font) {
    let s = match state.mode() {
        ShipoutMode::H{..} | ShipoutMode::Par => "span",
        ShipoutMode::Math => "mtext",
        _ => unreachable!()
    };
    state.missing_glyphs.insert((name.to_string(),char,font.filename().to_string()));
    state.push_child(HTMLChild::Comment(
        format!("<{} class=\"rustex-missing\" title=\"Missing Glyph: {} (pos. {}) in font {}\"></{}>",s,name,char,font.filename(),s)
    ));
}

pub(crate) fn do_paragraph(engine:Refs, state:&mut ShipoutState,children:&mut VNodes,mut spec:Vec<ParLineSpec<Types>>,start:SRef,end:SRef,lineskip: LineSkip,parskip:Option<Skip<Dim32>>) {
    if let Some(parskip) = parskip {
        state.push_comment(format!("<div class=\"rustex-vskip\" style=\"margin-bottom:{};\"></div>",dim_to_string(parskip.base)))
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

    state.do_in(node,
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
    let mut emergency_break= false;
    let mut ret = vec!();
    let mut later = vec!();
    while let Some(c) = children.next() {
        match c {
            VNode::Custom(RusTeXNode::ParagraphEnd) if ret.is_empty() => {
                emergency_break = true
            }
            VNode::Custom(RusTeXNode::ParagraphEnd) => {
                success = true;
                break;
            }
            VNode::Box(TeXBox::H { info: HBoxInfo::ParLine { .. },children,..}) => ret.extend(children.into_vec().into_iter()),
            VNode::Box(TeXBox::H { info: HBoxInfo::HBox { moved_left:None,raised:None,.. },children,..}) if emergency_break => ret.extend(children.into_vec().into_iter()),
            VNode::VSkip(_) | VNode::VFil | VNode::VFill | VNode::VFilneg | VNode::Vss | VNode::Mark(..) | VNode::VKern(_) => (),
            _ if emergency_break => {
                later.push(c);
                success = true;
                break
            },
            _ => later.push(c),
        }
    }
    if !success && !emergency_break { todo!("ERROR!") }
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

pub(crate) fn do_vcenter(state:&mut ShipoutState, engine:Refs,start:SRef,end:SRef,children:Box<[VNode<Types>]>) {
    let mut node = HTMLNode::new(HTMLTag::VCenterContainer);
    node.sourceref = Some((start,end));
    node.classes.push("rustex-vcenter-container".into());
    state.do_in(node,None,|state| {
        let node = HTMLNode::new(HTMLTag::VCenter);
        state.do_in(node,Some(ShipoutMode::V),|state| {
            do_vlist(engine,state,&mut children.into_vec().into(),false);
        })
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

pub(crate) fn do_math(state:&mut ShipoutState, engine:Refs,mut bx:MathGroup<Types>) {
    match bx.display {
        Some((pre,post)) => {
            let mut node = HTMLNode::new(HTMLTag::Display);
            if pre.base != Dim32(0) {
                node.styles.insert("margin-top".into(),dim_to_string(pre.base).into());
            }
            if post.base != Dim32(0) {
                node.styles.insert("margin-bottom".into(),dim_to_string(post.base).into());
            }
            node.classes.push("rustex-display".into());
            state.do_in(node,None,|state| {
                math_inner(state,engine,bx)
            })
            // TODO eqno/leqno
        }
        None => math_inner(state,engine,bx)
    }
}
fn math_inner(state:&mut ShipoutState, engine:Refs,mut bx:MathGroup<Types>) {
    let mut node = HTMLNode::new(HTMLTag::Math);
    node.classes.push("rustex-math".into());
    node.sourceref = Some((bx.start,bx.end));
    state.do_in(node,Some(ShipoutMode::Math),|state| {
        let node = HTMLNode::new(HTMLTag::MathGroup);
        state.do_in(node,None,|state| {
            let children = bx.children.into_vec();
            do_mathlist(engine,state,&mut children.into());
        });
    })
}

pub(crate) fn math_escape<F:FnOnce(Refs,&mut ShipoutState)>(engine:Refs,state:&mut ShipoutState,width:Dim32,height:Dim32,mode:ShipoutMode,f:F) {
    let mut node = HTMLNode::new(HTMLTag::MathEscape);
    node.classes.push("rustex-math-escape".into());
    node.width = Some((width,true));
    //node.styles.insert("height".into(),dim_to_string(height).into());
    node.font = Some((state.fonts.last().unwrap().clone(),true));
    state.do_in(node,Some(mode),|state| f(engine,state))
}

pub(crate) fn box_in_math(engine:Refs,state:&mut ShipoutState,bx:Bx) {
    let mode = match &bx {
        TeXBox::H { .. } => ShipoutMode::V,
        _ => ShipoutMode::H { escape: false }
    };
    let wd = bx.width();
    let ht = bx.height() + bx.depth();
    math_escape(engine,state,wd,ht,mode,move |engine,state| {
        match &bx {
            TeXBox::H {..} => {
                let ls = vec!(VNode::Box(bx));
                do_vlist(engine,state,&mut ls.into(),true);
            }
            _ => {
                let ls = vec!(HNode::Box(bx));
                do_hlist(engine,state,&mut ls.into());
            }
        }
    })
}

pub(crate) fn do_sub_sup(engine:Refs, state:&mut ShipoutState,a: MathAtom<Types,MathFontStyle<Types>>) {
    match (a.nucleus,a.sub,a.sup) {
        (n@MathNucleus::Simple{limits:Some(true),..},Some(sub),Some(sup)) => {
            let mut node = HTMLNode::new(HTMLTag::MUnderOver);
            node.attrs.insert("displaystyle".into(),"true".into());
            state.do_in(node,None,|state| {
                wrap(state,None,|state| do_nucleus(engine,state,n));
                wrap(state,None,|state| do_mathlist(engine,state,&mut sub.into_vec().into()));
                wrap(state,None,|state| do_mathlist(engine,state,&mut sup.into_vec().into()));
            })
        }
        (n,Some(sub),Some(sup)) => {
            let node = HTMLNode::new(HTMLTag::MSubSup);
            state.do_in(node,None,|state| {
                wrap(state,None,|state| do_nucleus(engine,state,n));
                wrap(state,None,|state| do_mathlist(engine,state,&mut sub.into_vec().into()));
                wrap(state,None,|state| do_mathlist(engine,state,&mut sup.into_vec().into()));
            })
        }
        (n@MathNucleus::Simple{limits:Some(true),..},Some(sub),_) => {
            let mut node = HTMLNode::new(HTMLTag::MUnder);
            node.attrs.insert("displaystyle".into(),"true".into());
            state.do_in(node,None,|state| {
                wrap(state,None,|state| do_nucleus(engine,state,n));
                wrap(state,None,|state| do_mathlist(engine,state,&mut sub.into_vec().into()));
            })
        }
        (n@MathNucleus::Simple{limits:Some(true),..},_,Some(sup)) => {
            let mut node = HTMLNode::new(HTMLTag::MOver);
            node.attrs.insert("displaystyle".into(),"true".into());
            state.do_in(node,None,|state| {
                wrap(state,None,|state| do_nucleus(engine,state,n));
                wrap(state,None,|state| do_mathlist(engine,state,&mut sup.into_vec().into()));
            })
        }
        (n,Some(sub),_) => {
            let node = HTMLNode::new(HTMLTag::MSub);
            state.do_in(node,None,|state| {
                wrap(state,None,|state| do_nucleus(engine,state,n));
                wrap(state,None,|state| do_mathlist(engine,state,&mut sub.into_vec().into()));
            })
        }
        (n,_,Some(sup)) => {
            let node = HTMLNode::new(HTMLTag::MSup);
            let mut node = state.do_in_and(node,None,|state| {
                wrap(state,None,|state| do_nucleus(engine,state,n));
                let sup = sup.into_vec();
                wrap(state,None,|state| do_mathlist(engine,state,&mut sup.into()));
            });
            // This is a hack to fix the rendering of primes
            match node.children.get(1) {
                Some(HTMLChild::Node(n)) if n.tag == HTMLTag::Mi => {
                    if n.children.len() == 1 {
                        match n.children.first() {
                            Some(HTMLChild::Text(s)) if s == "′" || s == "'" || s == "’" || s == "′" => {
                                let Some(HTMLChild::Node(_)) = node.children.pop() else {unreachable!()};
                                let Some(HTMLChild::Node(first)) = node.children.pop() else {unreachable!()};
                                state.push(first);
                                state.push_child(HTMLChild::Comment("<mo>′</mo>".to_string()));
                                return
                            }
                            _ => ()
                        }
                    }
                }
                _ => ()
            }
            // END HACK
            state.push(node);
        }
        _ => unreachable!()
    };
}

pub(crate) fn do_nucleus(engine:Refs,state:&mut ShipoutState,n:MathNucleus<Types,MathFontStyle<Types>>) {
    match n {
        MathNucleus::Simple{kernel,cls,..} => do_mathkernel(engine,state,kernel,Some(cls)),
        MathNucleus::Inner(kernel) => do_mathkernel(engine,state,kernel,None),
        c@MathNucleus::VCenter {..} => {
            let width = c.width();
            let height = c.height() + c.depth();
            if let MathNucleus::VCenter {start,end,children,..} = c {
                // TODO to/spread for VCenter
                math_escape(engine, state,width,height, ShipoutMode::H {escape:false}, |engine, state| {
                    do_vcenter(state, engine, start, end, children)
                })
            } else {unreachable!()}
        },
        MathNucleus::LeftRight {left,right,children,start,end} => {
            let mut node = HTMLNode::new(HTMLTag::MathGroup);
            node.sourceref = Some((start,end));
            state.do_in(node,None,|state| {
                if let Some((c,s)) = left {
                    let mut node = nodes::do_mathchar(engine, state, c, MathClass::Open, s.font);
                    node.attrs.insert("stretchy".into(),"true".into());
                    state.push(node);
                };
                do_mathlist(engine,state,&mut children.into_vec().into());
                if let Some((c,s)) = right {
                    let mut node =nodes::do_mathchar(engine, state, c, MathClass::Close, s.font);
                    node.attrs.insert("stretchy".into(),"true".into());
                    state.push(node);
                };
            });
        }
        MathNucleus::Middle(c,s) => {
            let mut node = nodes::do_mathchar(engine, state, c, MathClass::Op, s.font);
            node.attrs.insert("stretchy".into(),"true".into());
            state.push(node)
        }
        MathNucleus::Underline(k) => {
            let mut node = HTMLNode::new(HTMLTag::Annot(state.mode()));
            node.styles.insert("text-decoration".into(),"underline".into());
            state.do_in(node,None,|state| do_mathkernel(engine,state,k,None));
        }
        MathNucleus::Overline(k) => {
            let mut node = HTMLNode::new(HTMLTag::Annot(state.mode()));
            node.styles.insert("text-decoration".into(),"overline".into());
            state.do_in(node,None,|state| do_mathkernel(engine,state,k,None));
        }
        MathNucleus::Accent {accent:(char,style),inner} => {
            let mut node = HTMLNode::new(HTMLTag::MOver);
            node.attrs.insert("accent".into(),"true".into());
            let mut node = state.do_in_and(node,None,|state| {
                wrap(state,None,|state|
                    do_mathlist(engine,state,&mut inner.into_vec().into())
                );
                do_mathkernel(engine,state,MathKernel::Char{char,style},None);
            });
            match node.children.last_mut() {
                Some(HTMLChild::Node(n)) => {n.attrs.insert("stretchy".into(),"true".into());}
                _ => ()
            }
            state.push(node)
        }
        MathNucleus::Radical {rad:_,inner} => {
            let node = HTMLNode::new(HTMLTag::MSqrt);
            state.do_in(node,None,|state| {
                do_mathlist(engine,state,&mut inner.into_vec().into());
            });
        }
        o => todo!(" {:?}",o)
    }
}

pub(crate) fn do_mathkernel(engine:Refs,state:&mut ShipoutState,kernel:MathKernel<Types,MathFontStyle<Types>>,cls:Option<MathClass>) {
    use tex_tfm::fontstyles::FontModifiable;
    match kernel {
        MathKernel::Char {char,style:MathFontStyle{font,cramped,..}} => {
            let cls = cls.unwrap_or(MathClass::Ord);
            state.in_content = true;
            let glyphtable = engine.fontsystem.glyphmaps.get_glyphlist(font.filename());
            let glyph = glyphtable.get(char);
            if !glyph.is_defined() {
                do_missing_glyph(state,glyph.name(),char,&font);
            }
            let modifiers = match engine.fontsystem.glyphmaps.get_info(font.filename()) {
                Some(mds) if mds.styles != ModifierSeq::empty() => Some(mds.styles),
                _ => None
            };
            let mut node = node_from_class(cls,cramped);
            if let Some(m) = modifiers {
                node.push_string(glyph.to_string().apply(m).to_string(),ShipoutMode::Math,true);
            } else {
                node.push_glyph(ShipoutMode::Math,glyph)
            }
            state.push(node)
        }
        MathKernel::List {children,start,end} if cls.is_some() => {
            let cls = cls.unwrap();
            let mut node = HTMLNode::new(HTMLTag::MathGroup);
            node.classes.push(class_from_mathclass(cls,false).into());
            node.sourceref = Some((start,end));
            let mut node = state.do_in_and(node,None,|state| {
                do_mathlist(engine,state,&mut children.into_vec().into())
            });
            if node.children.len() == 1 && matches!(node.children.first(),Some(HTMLChild::Node(_))) {
                if let Some(HTMLChild::Node(mut nc)) = node.children.pop() {
                    if nc.tag == HTMLTag::Mo {
                        for c in node.classes {
                            if !nc.classes.contains(&c) {
                                nc.classes.push(c)
                            }
                        }
                        state.push(nc)
                    } else {
                        node.children.push(HTMLChild::Node(nc));
                        state.push(node)
                    }
                } else {unreachable!()}
            } else { state.push(node) }
        }
        MathKernel::List{children,start,end} => {
            let children = children.into_vec();
            wrap(state,Some((start,end)),|state| do_mathlist(engine, state, &mut children.into()))
        }
        MathKernel::Box(bx) => {
            if bx.is_empty() {
                let wd = bx.assigned_width();
                let ht = bx.assigned_height();
                let dp = bx.assigned_depth();
                match (wd,ht,dp) {
                    (Some(Dim32(0))|None,Some(Dim32(0))|None,Some(Dim32(0))|None) => (),
                    _ => state.push_comment(
                        format!("<mspace{}{}{}/>",
                                match wd {
                                    None | Some(Dim32(0)) => "".into(),
                                    Some(wd) => format!(" width=\"{}\"", dim_to_string(wd))
                                },
                                match ht {
                                    None | Some(Dim32(0)) => "".into(),
                                    Some(ht) => format!(" height=\"{}\"", dim_to_string(ht))
                                },
                                match dp {
                                    None | Some(Dim32(0)) => "".into(),
                                    Some(dp) => format!(" depth=\"{}\"", dim_to_string(dp))
                                }
                        ))
                }
            } else { match cls {
                Some(cls) if cls != MathClass::Ord => {
                    let mut node = HTMLNode::new(HTMLTag::MathGroup);
                    node.classes.push(class_from_mathclass(cls,false).into());
                    state.do_in(node,None,|state| box_in_math(engine,state,bx))
                },
                _ => box_in_math(engine, state, bx)
            } }
        }
        MathKernel::Empty => (),
    }
}

pub(crate) fn do_mathchar(engine:Refs, state:&mut ShipoutState, char:u8, cls:MathClass, font:Font) -> HTMLNode {
    use tex_tfm::fontstyles::FontModifiable;
    state.in_content = true;
    let glyphtable = engine.fontsystem.glyphmaps.get_glyphlist(font.filename());
    let glyph = glyphtable.get(char);
    if !glyph.is_defined() {
        do_missing_glyph(state,glyph.name(),char,&font);
    }
    let modifiers = match engine.fontsystem.glyphmaps.get_info(font.filename()) {
        Some(mds) if mds.styles != ModifierSeq::empty() => Some(mds.styles),
        _ => None
    };
    let mut node = node_from_class(cls,false);
    if let Some(m) = modifiers {
        node.push_string(glyph.to_string().apply(m).to_string(),ShipoutMode::Math,true);
    } else {
        node.push_glyph(ShipoutMode::Math,glyph)
    }
    node
}
pub(crate) fn merge_mathchar(engine:Refs, state:&mut ShipoutState, current_class: &mut Option<(MathClass,bool, HTMLNode)>, char:u8, cls:MathClass, font:Font, cramped:bool) {
    use tex_tfm::fontstyles::FontModifiable;
    state.in_content = true;
    let glyphtable = engine.fontsystem.glyphmaps.get_glyphlist(font.filename());
    let glyph = glyphtable.get(char);
    if !glyph.is_defined() {
        do_missing_glyph(state,glyph.name(),char,&font);
    }
    let modifiers = match engine.fontsystem.glyphmaps.get_info(font.filename()) {
        Some(mds) if mds.styles != ModifierSeq::empty() => Some(mds.styles),
        _ => None
    };
    match current_class {
        Some((mc2,cr,ref mut node)) if *mc2 == cls && *cr==cramped => {
            if let Some(m) = modifiers {
                node.push_string(glyph.to_string().apply(m).to_string(),ShipoutMode::Math,true);
            } else {
                node.push_glyph(ShipoutMode::Math,glyph)
            }
        },
        Some((ref mut c,cr,ref mut n)) => {
            *c = cls;
            *cr = cramped;
            let mut node = node_from_class(cls, cramped);
            if let Some(m) = modifiers {
                node.push_string(glyph.to_string().apply(m).to_string(),ShipoutMode::Math,true);
            } else {
                node.push_glyph(ShipoutMode::Math,glyph)
            }
            let old = std::mem::replace(n,node);
            state.push(old)
        }
        r@None => {
            let mut node = node_from_class(cls, cramped);
            if let Some(m) = modifiers {
                node.push_string(glyph.to_string().apply(m).to_string(),ShipoutMode::Math,true);
            } else {
                node.push_glyph(ShipoutMode::Math,glyph)
            }
            *r = Some((cls, cramped, node))
        }
    }
}

pub(crate) fn do_over(state:&mut ShipoutState,engine:Refs,top:Box<[MNode]>,bottom:Box<[MNode]>,sep:Option<Dim32>,left:Option<(u8,MathFontStyle<Types>)>,right:Option<(u8,MathFontStyle<Types>)>,start:SRef,end:SRef) {
    if let Some((c,s)) = left {
        let mut node = do_mathchar(engine, state, c, MathClass::Open, s.font);
        node.attrs.insert("stretchy".into(),"true".into());
        state.push(node);
    };
    let mut node = HTMLNode::new(HTMLTag::MFrac);
    node.sourceref = Some((start,end));
    node.classes.push("rustex-math-over".into());
    match sep {
        Some(Dim32(0))|None => (),
        Some(d) => {node.attrs.insert("linethickness".into(),dim_to_string(d).into());}
    }
    state.do_in(node,None,|state| {
        wrap(state,None,|state| do_mathlist(engine,state,&mut top.into_vec().into()));
        wrap(state,None,|state| do_mathlist(engine,state,&mut bottom.into_vec().into()));
    });
    if let Some((c,s)) = right {
        let mut node = do_mathchar(engine, state, c, MathClass::Close, s.font);
        node.attrs.insert("stretchy".into(),"true".into());
        state.push(node);
    };
}


pub(crate) fn do_raise<F:FnOnce(Bx,&mut ShipoutState)>(state:&mut ShipoutState,mut bx:Bx, f:F) {
    let d = match bx {
        TeXBox::H {info: HBoxInfo::HBox { ref mut raised, .. },..} => std::mem::take(raised).unwrap(),
        TeXBox::V{ info: VBoxInfo::VBox { ref mut raised, .. },..} => std::mem::take(raised).unwrap(),
        TeXBox::V{ info: VBoxInfo::VTop { ref mut raised, .. },..} => std::mem::take(raised).unwrap(),
        _ => unreachable!()
    };
    let mut node = HTMLNode::new(HTMLTag::Raise);
    node.classes.push("rustex-raise".into());
    node.styles.insert("--rustex-raise".into(),dim_to_string(d).into());
    /*if v {
        node.style_str("flex-direction","column")
    } else {
        node.style_str("flex-direction","row")
    }*/
    state.do_in(node,None,|state| {
        f(bx, state)
    })
}

pub(crate) fn do_moveleft<F:FnOnce(Bx,&mut ShipoutState)>(state:&mut ShipoutState,mut bx:Bx, f:F) {
    let d = match bx {
        TeXBox::H {info: HBoxInfo::HBox { ref mut moved_left, .. },..} => std::mem::take(moved_left).unwrap(),
        TeXBox::V{ info: VBoxInfo::VBox { ref mut moved_left, .. },..} => std::mem::take(moved_left).unwrap(),
        TeXBox::V{ info: VBoxInfo::VTop { ref mut moved_left, .. },..} => std::mem::take(moved_left).unwrap(),
        _ => unreachable!()
    };
    let mut node = HTMLNode::new(HTMLTag::MoveLeft);
    node.classes.push("rustex-moveleft".into());
    /*if v {
        node.style_str("flex-direction","column")
    } else {
        node.style_str("flex-direction","row")
    }*/
    node.styles.insert("--rustex-moveleft".into(),dim_to_string(d).into());
    state.do_in(node,None,|state| {
        f(bx, state)
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
}

pub(crate) fn do_pdfdest(state:&mut ShipoutState, id:NumOrName) {
    let mut node = HTMLNode::new(HTMLTag::Dest(state.mode()));
    let target = match id {
        NumOrName::Num(n) => format!("NUM_{}",n),
        NumOrName::Name(n) => n
    };
    node.attrs.insert("name".into(),target.clone().into());
    node.attrs.insert("id".into(),target.into());
    state.push(node);
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
    let mut node = HTMLNode::new(HTMLTag::HAlign);
    node.styles.insert("--rustex-align-num".into(),num_cols.to_string().into());
    node.classes.push("rustex-halign".into());
    state.do_in(node,None,|state| {
        state.do_in(HTMLNode::new(HTMLTag::HBody),Some(ShipoutMode::V), |state| {
            for row in rows {
                match row {
                    RowOrNoAlign::Row(children,.. ) => {
                        let mut cols = 0;
                        let node = HTMLNode::new(HTMLTag::HRow);
                        state.do_in(node,None,|state| {
                            for c in children {
                                cols += 1;
                                match c {
                                    HNode::Box(TeXBox::H { info: HBoxInfo::HAlignCell {spans,.. },children,start,end,.. }) => {
                                        let mut node = HTMLNode::new(HTMLTag::HCell);
                                        node.sourceref = Some((start,end));
                                        node.classes.push("rustex-halign-cell".into());
                                        if spans > 0 {
                                            node.styles.insert("grid-column".into(),format!("span {}",spans+1).into());
                                        }
                                        let (a,v) = align_cell(children.into_vec());
                                        state.do_in(node,None,|state| {
                                            let mut node = HTMLNode::new(HTMLTag::HBox);
                                            node.classes.push("rustex-hbox".into());
                                            match a {
                                                Alignment::L => {
                                                    node.styles.insert("justify-content".into(),"start".into());
                                                },
                                                Alignment::R => {
                                                    node.styles.insert("justify-content".into(),"end".into());
                                                },
                                                Alignment::C => {
                                                    node.styles.insert("justify-content".into(),"center".into());
                                                },
                                                _ => (),
                                            };
                                            state.do_in(node,Some(ShipoutMode::H {escape:false}),|state| {
                                                do_hlist(engine,state,&mut v.into());
                                            })
                                        })
                                    }
                                    _ => todo!()
                                }
                            }
                            for _ in cols..num_cols {
                                let mut node = HTMLNode::new(HTMLTag::HCell);
                                node.classes.push("rustex-halign-cell".into());
                                state.push(node)
                            }
                        })
                    }
                    RowOrNoAlign::NoAlign(v) => {
                        let node = HTMLNode::new(HTMLTag::HRow);
                        state.do_in(node,None,|state|{
                            let mut node = HTMLNode::new(HTMLTag::NoAlignH);
                            node.classes.push("rustex-noalign".into());
                            state.do_in(node,None,|state| {
                                do_vlist(engine,state,&mut v.into(),false)
                            })
                        })
                    }
                }
            }
        })
    })
}

fn align_cell(mut v:Vec<HNode<Types>>) -> (Alignment, Vec<HNode<Types>>) {
    let (mut left,mut right) = (ZERO_SKIP,ZERO_SKIP);
    let mut repush = Vec::new();
    while let Some(n) = v.pop() { if halign_i(n,&mut right,Some(&mut v),&mut repush) {break} }
    v.extend(repush.into_iter().rev());
    let mut nv = vec!();
    let mut it = v.into_iter();
    while let Some(n) = it.next() { if halign_i(n,&mut left,None,&mut nv) {break} }
    if left.base != Dim32(0) {
        nv.insert(0,HNode::HKern(left.base))
    }
    nv.extend(it);
    if right.base != Dim32(0) {
        nv.push(HNode::HKern(right.base))
    }
    (Alignment::from(left,right),nv)
}

fn halign_i(n:HNode<Types>,skip:&mut Skip<Dim32>,v:Option<&mut Vec<HNode<Types>>>,rep:&mut Vec<HNode<Types>>) -> bool {
    match n {
        HNode::HKern(n) => skip.base = skip.base + n,
        HNode::HSkip(sk) => skip.merge(sk),
        HNode::HFil | HNode::Hss => skip.set_fil(),
        HNode::HFill => skip.set_fill(),
        HNode::Penalty(_) | HNode::Mark(..) => (),
        ref vr@HNode::VRule {ref height,..} if vr.height() < Dim32(10) && height.is_some() => (),
        vr@HNode::VRule {..} if vr.width() <= Dim32(10) => rep.push(vr),
        HNode::Custom(RusTeXNode::PDFNode(PDFNode::PDFDest {..} | PDFNode::Color(..)) | RusTeXNode::FontChange(..) | RusTeXNode::FontChangeEnd) | HNode::Space => rep.push(n),
        _ => {
            match v {
                Some(v) => v.push(n),
                _ => rep.push(n)
            }
            return true
        }
    }
    false
}

pub(crate) fn do_svg(engine:Refs,state:&mut ShipoutState,bx:TeXBox<Types>,minx:Dim32,miny:Dim32,maxx:Dim32,maxy:Dim32) {
    let mut node = HTMLNode::new(HTMLTag::SvgWrap);
    node.classes.push("rustex-svg".into());
    state.do_in(node,None,|state| {
        let mut svg = HTMLNode::new(HTMLTag::SvgTop);
        svg.attrs.insert("width".into(),dim_to_string(maxx - minx).into());
        svg.attrs.insert("height".into(),dim_to_string(maxy - miny).into());
        svg.attrs.insert("viewBox".into(),format!("{} {} {} {}",
               dim_to_num(minx.0),
               dim_to_num(miny.0),
               dim_to_num(maxx.0 - minx.0),
               dim_to_num(maxy.0 - miny.0)
        ).into());
        state.do_in(svg,Some(ShipoutMode::SVG),|state| {
            let mut g = HTMLNode::new(HTMLTag::SvgG("g".to_string()));
            g.attrs.insert("transform".into(),
                   format!("translate(0,{})",dim_to_num(maxy.0 + miny.0)).into()
            );
            state.do_in(g,None,|state| {
                if let TeXBox::H {children,..} = bx {
                    svg_inner(engine,state,&mut children.into_vec().into())
                } else { unreachable!() }
            });
        })
    })
}

pub fn strtonum(ts : String) -> String {
    (ts.to_string().parse::<f32>().unwrap() * 1.5).to_string()
}

fn svg_inner(engine:Refs,state: &mut ShipoutState, children: &mut HNodes) {
    while let Some(c) = children.next() {
        match c {
            HNode::Custom(RusTeXNode::PGFGBegin {attrs,tag}) => {
                let mut node = HTMLNode::new(HTMLTag::SvgG(tag));
                for (k,v) in attrs.into_iter() {
                    match k {
                        k if k == "stroke-width" => {
                            node.attrs.insert(k.into(), strtonum(v).into());
                        }
                        k if k == "d" => {
                            node.attrs.insert(k.into(), parse_path(v).into());
                        }
                        k if k == "transform" => {
                            node.attrs.insert(k.into(),parse_transform(v).into());
                        }
                        _ => {node.attrs.insert(k.into(), v.into());}
                    }
                }
                state.nodes.push(node);
            }
            HNode::Custom(RusTeXNode::PGFGEnd) => {
                let node = super::annotations::close_all(state.mode(),&mut state.nodes);
                let mode = state.mode();
                state.nodes.last_mut().unwrap().push_open_node(mode,node);
            }
            HNode::Custom(RusTeXNode::PGFEscape(bx)) => {
                let mut node = HTMLNode::new(HTMLTag::SvgForeign);
                let wd = bx.width();
                let ht = bx.height() + bx.depth();
                node.width = Some((wd,true));
                node.styles.insert("width".into(),dim_to_string(wd).into());
                node.styles.insert("height".into(),dim_to_string(ht).into());
                node.styles.insert(
                    "translate".into(),
                    format!("0 {}",dim_to_string(-ht)).into()
                );
                node.classes.push("rustex-foreign".into());
                state.do_in(node,None,|state| {
                    let mut node = HTMLNode::new(HTMLTag::EscapeSvg);
                    node.font = Some((state.fonts.last().unwrap().clone(),true));
                    //node.width = Some((wd,true));
                    state.do_in(node,Some(ShipoutMode::V),|state| {
                        super::do_h(engine,state,HNode::Box(bx))
                    })
                })
            }
            HNode::Char {char,font,..} => {
                if font == engine.fontsystem.null() { continue }
                state.in_content = true;
                let glyphtable = engine.fontsystem.glyphmaps.get_glyphlist(font.filename());
                let glyph = glyphtable.get(char);
                if !glyph.is_defined() {
                    do_missing_glyph(state,glyph.name(),char,&font);
                } else {
                    state.push_glyph(glyph)
                }
            }
            HNode::Space | HNode::Hss => (),
            HNode::Box(TeXBox::H {children:chs,..}) => children.prefix(chs.into_vec()),
            HNode::Custom(RusTeXNode::FontChange(font,false)) =>
                super::annotations::do_font(state,&engine.fontsystem.glyphmaps,font),
            HNode::Custom(RusTeXNode::FontChangeEnd) =>
                super::annotations::close_font(state),
            HNode::Custom(RusTeXNode::PDFNode(PDFNode::Color(act))) =>
                super::annotations::do_color(state,engine,act),
            o => todo!("svg: {:?}",o)
        }
    }
}
use std::fmt::Write;

pub fn parse_path(ts : String) -> String {
    let mut ret = String::new();
    let mut s = ts.trim();
    while !s.is_empty() {
        if s.starts_with("M") {
            s = s[1..].trim_start();
            let n1 = parse_get_num(&mut s);
            let n2 = parse_get_num(&mut s);
            write!(ret,"M {} {} ",scale(n1),scale(-n2)).unwrap();
        } else if s.starts_with("L") {
            s = s[1..].trim_start();
            let n1 = parse_get_num(&mut s);
            let n2 = parse_get_num(&mut s);
            write!(ret,"L {} {} ",scale(n1),scale(-n2)).unwrap();
        } else if s.starts_with("C") {
            s = s[1..].trim_start();
            let n1 = parse_get_num(&mut s);
            let n2 = parse_get_num(&mut s);
            let n3 = parse_get_num(&mut s);
            let n4 = parse_get_num(&mut s);
            let n5 = parse_get_num(&mut s);
            let n6 = parse_get_num(&mut s);
            write!(ret,"C {} {} {} {} {} {} ",scale(n1),scale(-n2),scale(n3),scale(-n4),scale(n5),scale(-n6)).unwrap();
        } else if s.starts_with("Z") {
            write!(ret,"Z ").unwrap();
            s = s[1..].trim_start();
        } else if s.starts_with("h") {
            s = s[1..].trim_start();
            let n1 = parse_get_num(&mut s);
            write!(ret,"h {} ",scale(n1)).unwrap();
        } else if s.starts_with("v") {
            s = s[1..].trim_start();
            let n1 = parse_get_num(&mut s);
            write!(ret,"v {} ",scale(-n1)).unwrap();
        } else {
            todo!("parse_path: {}",s);
        }
    }
    ret
}

pub fn parse_transform(ts : String) -> String {
    let mut ret = String::new();
    let mut s = ts.trim();
    while !s.is_empty() {
        if s.starts_with("translate(") {
            s = &s[10..].trim_start();
            let n1 = parse_get_num(&mut s);
            let n2 = parse_get_num(&mut s);
            write!(ret,"translate({},{})",scale(n1),scale(-n2)).unwrap();
        } else if s.starts_with("matrix(") {
            s = s[7..].trim_start();
            let n1 = parse_get_num(&mut s);
            let n2 = parse_get_num(&mut s);
            let n3 = parse_get_num(&mut s);
            let n4 = parse_get_num(&mut s);
            let n5 = parse_get_num(&mut s);
            let n6 = parse_get_num(&mut s);
            write!(ret,"matrix({},{},{},{},{},{})",n1,n2,n3,n4,scale(n5),scale(-n6)).unwrap();
        } else {
            todo!("parse_transform: {}",s);
        }
    }
    ret
}
fn parse_get_num<'a>(s:&mut &'a str) -> f32 {
    match s.find(|x| x == ' ' || x == ')' || x == ',') {
        Some(i) => {
            let f = s[..i].parse::<f32>().unwrap();
            *s = &s[i+1..].trim_start();
            f
        }
        None => {
            let f = s.parse::<f32>().unwrap();
            *s = "";
            f
        }
    }
}

fn scale(f:f32) -> f32 { 1.5 * f }