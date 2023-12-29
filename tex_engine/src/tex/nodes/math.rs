use std::fmt::{Debug, Formatter};
use crate::engine::EngineTypes;
use crate::engine::filesystem::SourceRef;
use crate::engine::fontsystem::Font;
use crate::engine::mouth::pretokenized::TokenList;
use crate::tex::nodes::{Leaders, NodeTrait, WhatsitNode};
use crate::tex::nodes::boxes::TeXBox;
use crate::tex::nodes::vertical::VNode;
use crate::tex::numerics::{MuSkip, Skip, TeXDimen};
use crate::tex::types::{MathClass, MathStyle, MathStyleType, NodeType};
use crate::tex::numerics::NumSet;

#[derive(Debug,Clone)]
pub enum UnresolvedMathFontStyle<ET:EngineTypes> {
    Forced { style:MathStyleType, cramped:bool, font:ET::Font },
    Unforced { style:MathStyleType, cramped:bool, text_font:ET::Font, script_font:ET::Font, script_script_font:ET::Font }
}

pub trait MathFontStyleT<ET:EngineTypes>:Clone+Debug {
    type Choice:MathChoiceT<ET>;
    fn get_em(&self) -> ET::Dim { self.get_font().get_dim(5) }
    fn get_font(&self) -> &ET::Font;
}

pub trait MathChoiceT<ET:EngineTypes>:Clone+Debug {
    fn readable_fmt(&self, indent: usize, f: &mut Formatter<'_>) -> std::fmt::Result;
    fn width(&self) -> ET::Dim;
    fn height(&self) -> ET::Dim;
    fn depth(&self) -> ET::Dim;
}
#[derive(Clone,Debug)]
pub struct UnresolvedMathChoice<ET:EngineTypes> {
    pub display:Box<[MathNode<ET,UnresolvedMathFontStyle<ET>>]>,
    pub text:Box<[MathNode<ET,UnresolvedMathFontStyle<ET>>]>,
    pub script:Box<[MathNode<ET,UnresolvedMathFontStyle<ET>>]>,
    pub scriptscript:Box<[MathNode<ET,UnresolvedMathFontStyle<ET>>]>,
}
impl<ET:EngineTypes> MathChoiceT<ET> for UnresolvedMathChoice<ET> {
    fn readable_fmt(&self, indent: usize, f: &mut Formatter<'_>) -> std::fmt::Result {
        MathNode::<ET,MathFontStyle<ET>>::readable_do_indent(indent, f)?;
        f.write_str("<unresolved_choice>")?;
        for c in self.display.iter() {
            c.readable_fmt(indent + 2, f)?;
        }
        for c in self.text.iter() {
            c.readable_fmt(indent + 2, f)?;
        }
        for c in self.script.iter() {
            c.readable_fmt(indent + 2, f)?;
        }
        for c in self.scriptscript.iter() {
            c.readable_fmt(indent + 2, f)?;
        }
        MathNode::<ET,MathFontStyle<ET>>::readable_do_indent(indent, f)?;
        f.write_str("</unresolved_choice>")
    }
    fn width(&self) -> ET::Dim {
        self.display.iter().map(|c| c.width()).sum()
    }
    fn height(&self) -> ET::Dim {
        self.display.iter().map(|c| c.height()).max().unwrap_or_default()
    }
    fn depth(&self) -> ET::Dim {
        self.display.iter().map(|c| c.depth()).max().unwrap_or_default()
    }
}

pub struct ResolvedChoice<ET:EngineTypes>(pub Box<[MathNode<ET,MathFontStyle<ET>>]>);
impl<ET:EngineTypes> Debug for ResolvedChoice<ET> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("<resolved_choice>")?;
        for c in self.0.iter() {
            c.readable_fmt(2, f)?;
        }
        f.write_str("</resolved_choice>")
    }
}
impl<ET:EngineTypes> Clone for ResolvedChoice<ET> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}
impl<ET:EngineTypes> MathChoiceT<ET> for ResolvedChoice<ET> {
    fn readable_fmt(&self, indent: usize, f: &mut Formatter<'_>) -> std::fmt::Result {
        for c in self.0.iter() {
            c.readable_fmt(indent, f)?;
        }
        Ok(())
    }
    fn width(&self) -> ET::Dim {
        self.0.iter().map(|c| c.width()).sum()
    }
    fn height(&self) -> ET::Dim {
        self.0.iter().map(|c| c.height()).max().unwrap_or_default()
    }
    fn depth(&self) -> ET::Dim {
        self.0.iter().map(|c| c.depth()).max().unwrap_or_default()
    }
}

#[derive(Debug,Clone)]
pub struct MathFontStyle<ET:EngineTypes> {
    pub style:MathStyleType, pub cramped:bool, pub font:ET::Font
}
impl<ET:EngineTypes> MathFontStyleT<ET> for MathFontStyle<ET> {
    type Choice = ResolvedChoice<ET>;
    #[inline(always)]
    fn get_font(&self) -> &ET::Font { &self.font }
}

impl<ET:EngineTypes> MathFontStyleT<ET> for UnresolvedMathFontStyle<ET> {
    type Choice = UnresolvedMathChoice<ET>;
    fn get_font(&self) -> &ET::Font {
        match self {
            UnresolvedMathFontStyle::Forced { font, .. } => font,
            UnresolvedMathFontStyle::Unforced { style,text_font, script_font, script_script_font,.. } => match style {
                MathStyleType::Script => script_font,
                MathStyleType::ScriptScript => script_script_font,
                _ => text_font
            },
        }
    }
}

#[derive(Clone,Debug)]
pub enum MathNode<ET:EngineTypes,S:MathFontStyleT<ET>> {
    Atom(MathAtom<ET,S>),
    HSkip(ET::Skip),HFil,HFill,HFilneg,Hss,Space,
    MSkip{
        skip:ET::MuSkip,style: S
    },
    MKern{
        kern: <ET::MuSkip as MuSkip>::Base,style: S
    },HKern(ET::Dim),
    Leaders(Leaders<ET>),
    Penalty(i32),
    Mark(usize,TokenList<ET::Token>),
    VRule{
        width:Option<ET::Dim>,
        height:Option<ET::Dim>,
        depth:Option<ET::Dim>,
        start:SourceRef<ET>,end:SourceRef<ET>
    },
    Choice(S::Choice),
    Whatsit(WhatsitNode<ET>),
    Custom(ET::CustomNode),
}
impl<ET:EngineTypes,S:MathFontStyleT<ET>> NodeTrait<ET> for MathNode<ET,S> {
    fn readable_fmt(&self, indent: usize, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            MathNode::Penalty(p) => {
                Self::readable_do_indent(indent,f)?;
                write!(f, "<penalty:{}>",p)
            },
            MathNode::Mark(i, _) => {
                Self::readable_do_indent(indent,f)?;
                write!(f, "<mark:{}>",i)
            },
            MathNode::Choice(c) => c.readable_fmt(indent, f),
            MathNode::Leaders(l) => l.readable_fmt(indent, f),
            MathNode::VRule { width, height, depth, .. } => {
                write!(f, "<vrule")?;
                if let Some(w) = width {
                    write!(f, " width={}",w)?;
                }
                if let Some(h) = height {
                    write!(f, " height={}",h)?;
                }
                if let Some(d) = depth {
                    write!(f, " depth={}",d)?;
                }
                write!(f, ">")
            },
            MathNode::Whatsit(w) => {
                Self::readable_do_indent(indent,f)?;
                write!(f, "{:?}",w)
            }
            MathNode::HSkip(s) => write!(f, "<hskip:{}>",s),
            MathNode::MSkip{skip,..} => write!(f, "<mskip:{}>",skip),
            MathNode::MKern{kern,..} => write!(f, "<mkern:{}>",kern),
            MathNode::HFil => write!(f, "<hfil>"),
            MathNode::HFill => write!(f, "<hfill>"),
            MathNode::HFilneg => write!(f, "<hfilneg>"),
            MathNode::Hss => write!(f, "<hss>"),
            MathNode::Space => write!(f, "<space>"),
            MathNode::HKern(d) => write!(f, "<hkern:{}>",d),
            MathNode::Custom(n) => n.readable_fmt(indent, f),
            MathNode::Atom(a) => a.readable_fmt(indent, f),
        }
    }
    fn height(&self) -> ET::Dim {
        match self {
            MathNode::VRule { height, .. } => height.unwrap_or_default(),
            MathNode::Custom(n) => n.height(),
            MathNode::Atom(a) => a.height(),
            MathNode::Leaders(l) => l.height(),
            MathNode::Choice(c) => c.height(),
            _ => ET::Dim::default(),
        }
    }
    fn width(&self) -> ET::Dim {
        match self {
            MathNode::VRule { width, .. } => width.unwrap_or(ET::Dim::from_sp(26214)),
            MathNode::Custom(n) => n.width(),
            MathNode::HKern(d) => *d,
            MathNode::MKern {kern,style} => ET::Num::mudim_to_dim(*kern,style.get_em()),
            MathNode::MSkip {skip,style} => ET::Num::mudim_to_dim(skip.base(),style.get_em()),
            MathNode::HSkip(s) => s.base(),
            MathNode::Space => ET::Dim::from_sp(65536 * 5), // TODO heuristic; use spacefactor instead
            MathNode::Leaders(l) => l.width(),
            MathNode::Atom(a) => a.width(),
            MathNode::Choice(c) => c.width(),
            _=> ET::Dim::default(),
        }
    }
    fn depth(&self) -> ET::Dim {
        match self {
            MathNode::VRule { depth, .. } => depth.unwrap_or_default(),
            MathNode::Custom(n) => n.depth(),
            MathNode::Atom(a) => a.depth(),
            MathNode::Leaders(l) => l.depth(),
            MathNode::Choice(c) => c.depth(),
            _ => ET::Dim::default(),
        }

    }
    fn nodetype(&self) -> NodeType {
        match self {
            MathNode::Penalty(_) => NodeType::Penalty,
            MathNode::VRule {..} => NodeType::Rule,
            MathNode::HKern(_) | MathNode::MKern {..} => NodeType::Kern,
            MathNode::Mark(_, _) => NodeType::Mark,
            MathNode::Choice(_) => NodeType::Math,
            MathNode::Atom(_) => NodeType::Math,
            MathNode::Whatsit(_) => NodeType::WhatsIt,
            MathNode::HSkip(_) | MathNode::MSkip {..} | MathNode::Space | MathNode::HFil |
            MathNode::HFill | MathNode::HFilneg | MathNode::Hss => NodeType::Glue,
            MathNode::Leaders(_) => NodeType::Glue,
            MathNode::Custom(n) => n.nodetype(),
        }
    }
    fn opaque(&self) -> bool {
        match self {
            MathNode::Mark(_, _) => true,
            MathNode::Custom(n) => n.opaque(),
            _ => false
        }
    }
}

#[derive(Clone,Debug)]
pub struct MathAtom<ET:EngineTypes,S:MathFontStyleT<ET>> {
    pub nucleus:MathNucleus<ET,S>,
    pub sup:Option<Box<[MathNode<ET,S>]>>,
    pub sub:Option<Box<[MathNode<ET,S>]>>,
}
impl<ET:EngineTypes,S:MathFontStyleT<ET>> MathAtom<ET,S> {
    pub fn empty() -> Self {
        Self { nucleus:MathNucleus::Simple { cls:MathClass::Ord, kernel:MathKernel::Empty, limits:None }, sup:None, sub:None }
    }
}
impl <ET:EngineTypes,S:MathFontStyleT<ET>> NodeTrait<ET> for MathAtom<ET,S> {
    fn readable_fmt(&self, indent: usize, f: &mut Formatter<'_>) -> std::fmt::Result {
        Self::readable_do_indent(indent, f)?;
        if self.sub.is_none() || self.sup.is_none() {
            return self.nucleus.readable_fmt(indent, f);
        }
        f.write_str("<atom>")?;
        self.nucleus.readable_fmt(indent + 2, f)?;
        if let Some(sup) = &self.sup {
            Self::readable_do_indent(indent + 2, f)?;
            f.write_str("<sup>")?;
            for c in sup.iter() {
                c.readable_fmt(indent + 4, f)?;
            }
            Self::readable_do_indent(indent + 2, f)?;
            f.write_str("</sup>")?;
        }
        if let Some(sub) = &self.sub {
            Self::readable_do_indent(indent + 2, f)?;
            f.write_str("<sub>")?;
            for c in sub.iter() {
                c.readable_fmt(indent + 4, f)?;
            }
            Self::readable_do_indent(indent + 2, f)?;
            f.write_str("</sub>")?;
        }
        Self::readable_do_indent(indent, f)?;
        f.write_str("</atom>")
    }
    fn height(&self) -> ET::Dim {
        if self.sup.is_none() {
            return self.nucleus.height();
        }
        let h = self.nucleus.height();
        let limits = match self.nucleus {
            MathNucleus::Simple {cls:MathClass::Op,limits:Some(true),..} => true,
            _ => false
        };
        let sup = self.sup.as_ref().unwrap().iter().map(|c| c.height()).max().unwrap_or_default();
        if limits {
            h + sup + self.sup.as_ref().unwrap().iter().map(|c| c.depth()).max().unwrap_or_default() +
                ET::Dim::from_sp(65536 * 5) // TODO heuristic
        } else {
            h + sup.scale_float(0.5) // TODO heuristic
        }
    }
    fn width(&self) -> ET::Dim {
        if self.sup.is_none() && self.sub.is_none() {
            return self.nucleus.width();
        }
        let w = self.nucleus.width();
        let sup = match self.sup {
            Some(ref ls) => ls.iter().map(|c| c.width()).sum(),
            _ => ET::Dim::default()
        };
        let sub = match self.sub {
            Some(ref ls) => ls.iter().map(|c| c.width()).sum(),
            _ => ET::Dim::default()
        };
        let limits = match self.nucleus {
            MathNucleus::Simple {cls:MathClass::Op,limits:Some(true),..} => true,
            _ => false
        };
        if limits {
            w.max(sup).max(sub)
        } else {
            w + sup.max(sub) + ET::Dim::from_sp(65536 * 3) // TODO heuristic
        }
    }
    fn depth(&self) -> ET::Dim {
        if self.sub.is_none() {
            return self.nucleus.depth();
        }
        let h = self.nucleus.depth();
        let limits = match self.nucleus {
            MathNucleus::Simple {cls:MathClass::Op,limits:Some(true),..} => true,
            _ => false
        };
        let sub = self.sub.as_ref().unwrap().iter().map(|c| c.depth()).max().unwrap_or_default();
        if limits {
            h + sub + self.sub.as_ref().unwrap().iter().map(|c| c.height()).max().unwrap_or_default() +
                ET::Dim::from_sp(65536 * 5) // TODO heuristic
        } else {
            h + sub // TODO heuristic
        }
    }
    #[inline(always)]
    fn nodetype(&self) -> NodeType { NodeType::Math }
}

#[derive(Clone,Debug)]
pub enum MathNucleus<ET:EngineTypes,S:MathFontStyleT<ET>> {
    Simple{cls: MathClass,kernel:MathKernel<ET,S>,limits:Option<bool>},
    Inner(MathKernel<ET,S>),
    Overline(MathKernel<ET,S>),
    Underline(MathKernel<ET,S>),
    Accent,Radical,
    VCenter{
        start:SourceRef<ET>,
        end:SourceRef<ET>,
        children:Box<[VNode<ET>]>
    }
}
impl<ET:EngineTypes,S:MathFontStyleT<ET>> NodeTrait<ET> for MathNucleus<ET,S> {
    fn readable_fmt(&self, indent: usize, f: &mut Formatter<'_>) -> std::fmt::Result {
        Self::readable_do_indent(indent, f)?;
        match self {
            MathNucleus::Simple{cls:MathClass::Op,kernel,limits} => {
                write!(f, "<mathop limits={:?}>",limits)?;
                kernel.readable_fmt(indent + 2, f)?;
                Self::readable_do_indent(indent, f)?;
                f.write_str("</mathop>")
            }
            MathNucleus::Simple{cls,kernel,..} => {
                write!(f, "<math{:?}>",cls)?;
                kernel.readable_fmt(indent + 2, f)?;
                Self::readable_do_indent(indent, f)?;
                write!(f,"</math{:?}>",cls)
            }
            MathNucleus::Inner(k) => {
                write!(f, "<mathinner>")?;
                k.readable_fmt(indent + 2, f)?;
                Self::readable_do_indent(indent, f)?;
                f.write_str("</mathinner>")
            }
            MathNucleus::Overline(k) => {
                write!(f, "<overline>")?;
                k.readable_fmt(indent + 2, f)?;
                Self::readable_do_indent(indent, f)?;
                f.write_str("</overline>")
            }
            MathNucleus::Underline(k) => {
                write!(f, "<underline>")?;
                k.readable_fmt(indent + 2, f)?;
                Self::readable_do_indent(indent, f)?;
                f.write_str("</underline>")
            }
            MathNucleus::Accent => {
                todo!()
            }
            MathNucleus::Radical => {
                todo!()
            }
            MathNucleus::VCenter{children,..} => {
                write!(f, "<vcenter>")?;
                for c in children.iter() {
                    c.readable_fmt(indent + 2, f)?;
                }
                Self::readable_do_indent(indent, f)?;
                f.write_str("</vcenter>")
            }
        }
    }
    fn height(&self) -> ET::Dim {
        match self {
            MathNucleus::Simple {cls:MathClass::Op,kernel,..} => (kernel.height() + kernel.depth()).scale_float(0.5),
            MathNucleus::Simple {kernel,..} => kernel.height(),
            MathNucleus::Inner(k) => k.height(),
            MathNucleus::Overline(k) => k.height(),
            MathNucleus::Underline(k) => k.height(),
            MathNucleus::Accent => ET::Dim::default(),
            MathNucleus::Radical => ET::Dim::default(),
            MathNucleus::VCenter{children,..} => children.iter().map(|c| c.height() + c.depth()).sum::<ET::Dim>() +
                -children.iter().last().map(|c| c.depth()).unwrap_or_default()
        }
    }
    fn width(&self) -> ET::Dim {
        match self {
            MathNucleus::Simple{kernel,..} => kernel.width(),
            MathNucleus::Inner(k) => k.width(),
            MathNucleus::Overline(k) => k.width(),
            MathNucleus::Underline(k) => k.width(),
            MathNucleus::Accent => ET::Dim::default(),
            MathNucleus::Radical => ET::Dim::default(),
            MathNucleus::VCenter{children,..} => children.iter().map(|c| c.width()).max().unwrap_or_default()
        }
    }
    fn depth(&self) -> ET::Dim {
        match self {
            MathNucleus::Simple {cls:MathClass::Op,kernel,..} => (kernel.height() + kernel.depth()).scale_float(0.5),
            MathNucleus::Simple {kernel,..} => kernel.depth(),
            MathNucleus::Inner(k) => k.depth(),
            MathNucleus::Overline(k) => k.depth(),
            MathNucleus::Underline(k) => k.depth(),
            MathNucleus::Accent => ET::Dim::default(),
            MathNucleus::Radical => ET::Dim::default(),
            MathNucleus::VCenter{children,..} => children.iter().last().map(|c| c.depth()).unwrap_or_default()
        }
    }
    #[inline(always)]
    fn nodetype(&self) -> NodeType { NodeType::Math }
}

#[derive(Clone,Debug)]
pub enum MathKernel<ET:EngineTypes,S:MathFontStyleT<ET>> {
    Empty,
    Char {
        char:ET::Char,
        style: S
    },
    Box(TeXBox<ET>),
    List{
        start:SourceRef<ET>,
        children:Box<[MathNode<ET,S>]>,
        end:SourceRef<ET>
    }
}
impl<ET:EngineTypes,S:MathFontStyleT<ET>> Default for MathKernel<ET,S> {
    fn default() -> Self { MathKernel::Empty }
}
impl<ET:EngineTypes,S:MathFontStyleT<ET>> NodeTrait<ET> for MathKernel<ET,S> {
    fn readable_fmt(&self, indent: usize, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            MathKernel::Empty => Ok(()),
            MathKernel::Char { char, .. } => {
                Self::readable_do_indent(indent, f)?;
                write!(f, "<char:{}/>",char)
            }
            MathKernel::Box(b) => b.readable_fmt(indent, f),
            MathKernel::List{children,..} => {
                for c in children.iter() {
                    c.readable_fmt(indent + 2, f)?;
                }
                Ok(())
            }
        }
    }
    fn height(&self) -> ET::Dim {
        match self {
            MathKernel::Empty => ET::Dim::default(),
            MathKernel::Char { style,char } => style.get_font().get_ht(*char),
            MathKernel::Box(b) => b.height(),
            MathKernel::List{children,..} => children.iter().map(|c| c.height()).max().unwrap_or_default()
        }
    }
    fn width(&self) -> ET::Dim {
        match self {
            MathKernel::Empty => ET::Dim::default(),
            MathKernel::Char { style,char } => style.get_font().get_wd(*char),
            MathKernel::Box(b) => b.width(),
            MathKernel::List{children,..} => children.iter().map(|c| c.width()).sum()
        }
    }
    fn depth(&self) -> ET::Dim {
        match self {
            MathKernel::Empty => ET::Dim::default(),
            MathKernel::Char { style,char } => style.get_font().get_dp(*char),
            MathKernel::Box(b) => b.depth(),
            MathKernel::List{children,..} => children.iter().map(|c| c.depth()).max().unwrap_or_default()
        }
    }
    fn nodetype(&self) -> NodeType {
        match self {
            MathKernel::Empty => NodeType::Math,
            MathKernel::Char { .. } => NodeType::MathChar,
            MathKernel::Box(b) => b.nodetype(),
            MathKernel::List{..} => NodeType::Math
        }
    }
}

#[derive(Debug,Clone)]
pub struct MathGroup<ET:EngineTypes,S:MathFontStyleT<ET>> {
    pub display:bool,
    pub children:Box<[MathNode<ET,S>]>,
    pub start:SourceRef<ET>,
    pub end:SourceRef<ET>
}

impl<ET:EngineTypes,S:MathFontStyleT<ET>> MathGroup<ET,S> {
    pub fn close(display:bool,start:SourceRef<ET>,end:SourceRef<ET>,children:Vec<MathNode<ET,UnresolvedMathFontStyle<ET>>>) -> MathGroup<ET,MathFontStyle<ET>> {
        let style = if display { MathStyle {
            style:MathStyleType::Display,
            cramped:false,
            forced_from:None
        } } else { MathStyle {
            style:MathStyleType::Text,
            cramped:false,
            forced_from:None
        } };
        let nch = Self::close_i(children,style);
        MathGroup { display,children:nch.into(),start,end }
    }
    fn close_i(ls: Vec<MathNode<ET,UnresolvedMathFontStyle<ET>>>,style:MathStyle) -> Vec<MathNode<ET,MathFontStyle<ET>>> {
        ls.into_iter().map(|n| match n {
            MathNode::HSkip(s) => MathNode::HSkip(s),
            MathNode::HFil => MathNode::HFil,
            MathNode::HFill => MathNode::HFill,
            MathNode::HFilneg => MathNode::HFilneg,
            MathNode::Hss => MathNode::Hss,
            MathNode::Choice(c) => match style {
                MathStyle { style:MathStyleType::Display, .. } => MathNode::Choice(ResolvedChoice(Self::close_i(c.display.into_vec(),style).into())),
                MathStyle { style:MathStyleType::Text, .. } => MathNode::Choice(ResolvedChoice(Self::close_i(c.text.into_vec(),style).into())),
                MathStyle { style:MathStyleType::Script, .. } => MathNode::Choice(ResolvedChoice(Self::close_i(c.script.into_vec(),style).into())),
                MathStyle { style:MathStyleType::ScriptScript, .. } => MathNode::Choice(ResolvedChoice(Self::close_i(c.scriptscript.into_vec(), style).into())),
            }
            MathNode::Space => MathNode::Space,
            MathNode::Leaders(l) => MathNode::Leaders(l),
            MathNode::HKern(d) => MathNode::HKern(d),
            MathNode::Penalty(p) => MathNode::Penalty(p),
            MathNode::Mark(i,tl) => MathNode::Mark(i,tl),
            MathNode::VRule{width,height,depth,start,end} => MathNode::VRule{width,height,depth,start,end},
            MathNode::Whatsit(w) => MathNode::Whatsit(w),
            MathNode::Custom(n) => MathNode::Custom(n),
            MathNode::MSkip {skip,style:unresolved} => MathNode::MSkip {skip,style:Self::resolve_style(style, unresolved)},
            MathNode::MKern {kern,style:unresolved} => MathNode::MKern {kern,style:Self::resolve_style(style, unresolved)},
            MathNode::Atom(a) => MathNode::Atom(MathAtom {
                nucleus: Self::resolve_nucleus(a.nucleus,style),
                sup: match a.sup {
                    None => None,
                    Some(l) => Some(Self::close_i(l.into_vec(),style.sup()).into())
                },
                sub: match a.sub {
                    None => None,
                    Some(l) => Some(Self::close_i(l.into_vec(),style.sub()).into())
                }
            })
        }).collect()
    }
    fn resolve_nucleus(n:MathNucleus<ET,UnresolvedMathFontStyle<ET>>,style:MathStyle) -> MathNucleus<ET,MathFontStyle<ET>> {
        match n {
            MathNucleus::Simple { cls:MathClass::Op, kernel, limits } =>
                MathNucleus::Simple { cls:MathClass::Op, kernel:Self::resolve_kernel(kernel,style), limits:match limits {
                    Some(l) => Some(l),
                    None => if style.style == MathStyleType::Display { Some(true) } else { Some(false) }
                } },
            MathNucleus::Simple { cls, kernel, limits } =>
                MathNucleus::Simple { cls, kernel:Self::resolve_kernel(kernel,style), limits },
            MathNucleus::Inner(k) => MathNucleus::Inner(Self::resolve_kernel(k,style)),
            MathNucleus::Overline(k) => MathNucleus::Overline(Self::resolve_kernel(k,style)),
            MathNucleus::Underline(k) => MathNucleus::Underline(Self::resolve_kernel(k,style)),
            MathNucleus::Accent => MathNucleus::Accent,
            MathNucleus::Radical => MathNucleus::Radical,
            MathNucleus::VCenter{start,end,children} => MathNucleus::VCenter{start,end,children}
        }
    }
    fn resolve_kernel(n:MathKernel<ET,UnresolvedMathFontStyle<ET>>,style:MathStyle) -> MathKernel<ET,MathFontStyle<ET>> {
        match n {
            MathKernel::Empty => MathKernel::Empty,
            MathKernel::Char { char, style:unresolved } => MathKernel::Char { char, style:Self::resolve_style(style, unresolved) },
            MathKernel::Box(b) => MathKernel::Box(b),
            MathKernel::List{start,children,end} => MathKernel::List{start,children:Self::close_i(children.into_vec(),style).into(),end}
        }
    }
    fn resolve_style(style:MathStyle, unresolved:UnresolvedMathFontStyle<ET>) -> MathFontStyle<ET> {
        match unresolved {
            UnresolvedMathFontStyle::Forced { style, cramped, font } => MathFontStyle { style, cramped, font },
            UnresolvedMathFontStyle::Unforced { text_font, script_font, script_script_font,.. } => match style.style {
                MathStyleType::Script => MathFontStyle { style:style.style, cramped:style.cramped, font:script_font },
                MathStyleType::ScriptScript => MathFontStyle { style:style.style, cramped:style.cramped, font:script_script_font },
                _ => MathFontStyle { style:style.style, cramped:style.cramped, font:text_font },
            },
        }
    }
}

#[derive(Debug,Clone)]
pub struct MathChar<ET:EngineTypes> {
    pub char:ET::Char,
    pub cls:MathClass,
    pub style: UnresolvedMathFontStyle<ET>
}
impl<ET:EngineTypes> MathChar<ET> {
    pub fn to_atom(self) -> MathAtom<ET,UnresolvedMathFontStyle<ET>> {
        let kernel = MathNucleus::Simple { cls:self.cls, kernel:MathKernel::Char { char:self.char, style:self.style }, limits:None };
        MathAtom { nucleus: kernel,sup:None,sub:None }
    }
}

#[derive(Clone,Debug)]
pub struct Delimiter<ET:EngineTypes> {
    pub small:MathChar<ET>,
    pub large:MathChar<ET>,
}