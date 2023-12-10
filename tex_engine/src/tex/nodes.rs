use std::cell::{OnceCell, RefCell};
use std::fmt::{Debug, Display, Write};
use std::marker::PhantomData;
use crate::engine::{EngineReferences, EngineTypes};
use crate::engine::filesystem::kpathsea::SourceReference;
use crate::tex::types::{BoxType, NodeType, TeXMode};
use crate::engine::filesystem::File;
use crate::engine::fontsystem::FontSystem;
use crate::engine::mouth::pretokenized::TokenList;
use crate::engine::utils::memory::{PrimitiveIdentifier, PRIMITIVES};
use crate::tex::input_text::Character;
use crate::tex::numerics::{Skip, TeXDimen};
use crate::utils::Ptr;

type SR<ET> = SourceReference<<<ET as EngineTypes>::File as File>::SourceRefID>;

#[derive(Debug,Clone)]
pub struct NodeList<ET:EngineTypes> {
    pub children:Vec<PreShipoutNode<ET>>,
    pub tp:NodeListType<ET>
}

#[derive(Clone,Debug)]
pub enum BoxTarget {
    Register{ index:u16, globally:bool },
    List,
    Out
}

#[derive(Clone,Debug)]
pub enum NodeListType<ET:EngineTypes> {
    Paragraph(SourceReference<<ET::File as File>::SourceRefID>),
    Box(BoxInfo<ET>,SR<ET>,BoxTarget)
}

pub trait NodeTrait<ET:EngineTypes>:Debug+Clone {
    fn height(&self) -> ET::Dim;
    fn depth(&self) -> ET::Dim;
    fn width(&self) -> ET::Dim;
    fn nodetype(&self) -> NodeType;
    fn readable_fmt(&self, indent:usize, f:&mut std::fmt::Formatter<'_>) -> std::fmt::Result;
    #[inline(always)]
    fn readable_do_indent(indent:usize,mut f:&mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char('\n')?;
        for _ in 0..indent {f.write_char(' ')?;}
        Ok(())
    }
    fn readable(&self) -> ReadableNode<ET,Self> where Self:Sized {
        ReadableNode(self,PhantomData)
    }
    fn opaque(&self) -> bool { false }
}
impl<ET:EngineTypes> NodeTrait<ET> for () {
    fn height(&self) -> ET::Dim { ET::Dim::default() }
    fn depth(&self) -> ET::Dim { ET::Dim::default() }
    fn width(&self) -> ET::Dim { ET::Dim::default() }
    fn nodetype(&self) -> NodeType { NodeType::WhatsIt }
    fn readable_fmt(&self, _indent:usize, _f:&mut std::fmt::Formatter<'_>) -> std::fmt::Result { Ok(()) }
    fn opaque(&self) -> bool { true }
}

pub trait PreShipoutNodeTrait<ET:EngineTypes>:NodeTrait<ET> {
    fn as_node(self) -> PreShipoutNode<ET>;
    fn shipout(self,list:&mut Vec<ShipoutNode<ET>>,engine:&mut EngineReferences<ET>);
}

impl<ET:EngineTypes<PreCustomNode=()>> PreShipoutNodeTrait<ET> for () {
    #[inline(always)]
    fn as_node(self) -> PreShipoutNode<ET> { PreShipoutNode::Custom(self) }
    #[inline(always)]
    fn shipout(self, _list: &mut Vec<ShipoutNode<ET>>, _engine: &mut EngineReferences<ET>) {}
}

pub struct ReadableNode<'a,ET:EngineTypes,N: NodeTrait<ET>>(&'a N, PhantomData<ET>);
impl<'a,ET:EngineTypes,N: NodeTrait<ET>> Display for ReadableNode<'a,ET,N> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.readable_fmt(0, f)
    }
}

pub type WhatsitFunction<ET> = Ptr<RefCell<Option<Box<dyn FnOnce(&mut EngineReferences<ET>) -> Option<ShipoutNode<ET>>>>>>;
#[derive(Clone)]
pub struct WhatsitNode<ET:EngineTypes>(String,WhatsitFunction<ET>);
impl<ET:EngineTypes> std::fmt::Debug for WhatsitNode<ET> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f,"<whatsit {}>",self.0)
    }
}
impl<ET:EngineTypes> WhatsitNode<ET> {
    pub fn new(f:Box<dyn FnOnce(&mut EngineReferences<ET>) -> Option<ShipoutNode<ET>>>, name:PrimitiveIdentifier) -> PreShipoutNode<ET> {
        PreShipoutNode::Whatsit(WhatsitNode(PRIMITIVES.printable::<ET::Char>(name, None).to_string(),
                                            Ptr::new(RefCell::new(Some(f)))
        ))
    }
    pub fn call(self,engine: &mut EngineReferences<ET>) -> Option<ShipoutNode<ET>> {
        let f = self.1.replace(None);
        f.map(|f| f(engine)).flatten()
    }
}

#[derive(Clone,Debug)]
pub enum ShipoutNode<ET:EngineTypes> {
    Penalty(i32),
    Mark(usize,TokenList<ET::Token>),
    Skip(SkipNode<ET>),
    Kern(KernNode<ET>),
    Box(TeXBox<ET,Self>),
    Simple(SimpleNode<ET>),
    Custom(ET::ShipoutCustomNode),
    Char { char:ET::Char, font:<ET::FontSystem as FontSystem>::Font, width:ET::Dim, height:ET::Dim, depth:ET::Dim  }
}
pub trait TopNodeTrait<ET:EngineTypes>: NodeTrait<ET> {}
impl<ET:EngineTypes> TopNodeTrait<ET> for ShipoutNode<ET> {}
impl<ET:EngineTypes> TopNodeTrait<ET> for PreShipoutNode<ET> {}

impl<ET:EngineTypes> NodeTrait<ET> for ShipoutNode<ET> {
    fn readable_fmt(&self, indent: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ShipoutNode::Penalty(p) => {
                Self::readable_do_indent(indent,f)?;
                write!(f, "<penalty:{}>",p)
            },
            ShipoutNode::Skip(s) => s.readable_fmt(indent, f),
            ShipoutNode::Kern(k) => k.readable_fmt(indent, f),
            ShipoutNode::Box(b) => b.readable_fmt(indent, f),
            ShipoutNode::Mark(i, _) => {
                Self::readable_do_indent(indent,f)?;
                write!(f, "<mark:{}>",i)
            },
            ShipoutNode::Simple(s) => s.readable_fmt(indent, f),
            ShipoutNode::Char { char, font, .. } =>
                Ok(char.display(f)),
            ShipoutNode::Custom(n) => n.readable_fmt(indent,f)
        }
    }
    fn height(&self) -> ET::Dim {
        match self {
            ShipoutNode::Penalty(_) => ET::Dim::default(),
            ShipoutNode::Skip(s) => s.height(),
            ShipoutNode::Box(b) => b.height(),
            ShipoutNode::Simple(s) => s.height(),
            ShipoutNode::Char { height, .. } => *height,
            ShipoutNode::Kern(k) => k.height(),
            ShipoutNode::Mark(_, _) => ET::Dim::default(),
            ShipoutNode::Custom(n) => n.height()
        }
    }
    fn width(&self) -> ET::Dim {
        match self {
            ShipoutNode::Penalty(_) => ET::Dim::default(),
            ShipoutNode::Skip(s) => s.width(),
            ShipoutNode::Box(b) => b.width(),
            ShipoutNode::Simple(s) => s.width(),
            ShipoutNode::Char { width, .. } => *width,
            ShipoutNode::Kern(k) => k.width(),
            ShipoutNode::Mark(_, _) => ET::Dim::default(),
            ShipoutNode::Custom(n) => n.width(),
        }
    }
    fn depth(&self) -> ET::Dim {
        match self {
            ShipoutNode::Penalty(_) | ShipoutNode::Skip(_) => ET::Dim::default(),
            ShipoutNode::Box(b) => b.depth(),
            ShipoutNode::Simple(s) => s.depth(),
            ShipoutNode::Char { depth, .. } => *depth,
            ShipoutNode::Kern(k) => k.depth(),
            ShipoutNode::Mark(_, _) => ET::Dim::default(),
            ShipoutNode::Custom(n) => n.depth(),
        }
    }
    fn nodetype(&self) -> NodeType {
        match self {
            ShipoutNode::Penalty(_) => NodeType::Penalty,
            ShipoutNode::Skip(_) => NodeType::Glue,
            ShipoutNode::Box(b) => b.nodetype(),
            ShipoutNode::Simple(s) => s.nodetype(),
            ShipoutNode::Char { .. } => NodeType::Char,
            ShipoutNode::Kern(_) => NodeType::Kern,
            ShipoutNode::Mark(_, _) => NodeType::Mark,
            ShipoutNode::Custom(n) => n.nodetype(),
        }
    }
}

#[derive(Clone,Debug)]
pub enum PreShipoutNode<ET:EngineTypes> {
    Penalty(i32),
    Mark(usize,TokenList<ET::Token>),
    Whatsit(WhatsitNode<ET>),
    Skip(SkipNode<ET>),
    Kern(KernNode<ET>),
    Box(TeXBox<ET,Self>),
    Insert,
    VAdjust,
    Math,
    Simple(SimpleNode<ET>),
    Custom(ET::PreCustomNode),
    Char { char:ET::Char, font:<ET::FontSystem as FontSystem>::Font, width:ET::Dim, height:ET::Dim, depth:ET::Dim  }
}
impl<ET:EngineTypes> PreShipoutNode<ET> {
    pub fn discardable(&self) -> bool {
        match self {
            PreShipoutNode::Penalty(_) => true,
            PreShipoutNode::Skip(_) => true,
            PreShipoutNode::Kern(_) => true,
            _ => false
        }
    }

    pub fn shipout_top(self,engine:&mut EngineReferences<ET>) -> Option<ShipoutNode<ET>> {
        match self {
            PreShipoutNode::Penalty(p) => return Some(ShipoutNode::Penalty(p)),
            PreShipoutNode::Mark(i, t) => return Some(ShipoutNode::Mark(i, t)),
            PreShipoutNode::Char { char, font, width, height, depth } => {
                return Some(ShipoutNode::Char { char, font, width, height, depth })
            },
            PreShipoutNode::Whatsit(w) => {
                if let Some(n) = w.call(engine) {
                    return Some(n)
                } else { return None }
            }
            _ => ()
        }
        let mut v = vec!();
        self.shipout(&mut v,engine);
        v.into_iter().next()
    }
}

impl<ET:EngineTypes> NodeTrait<ET> for PreShipoutNode<ET> {
    fn readable_fmt(&self, indent: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PreShipoutNode::Penalty(p) => {
                Self::readable_do_indent(indent,f)?;
                write!(f, "<penalty:{}>",p)
            },
            PreShipoutNode::Skip(s) => s.readable_fmt(indent, f),
            PreShipoutNode::Kern(k) => k.readable_fmt(indent, f),
            PreShipoutNode::Box(b) => b.readable_fmt(indent, f),
            PreShipoutNode::Mark(i, _) => {
                Self::readable_do_indent(indent,f)?;
                write!(f, "<mark:{}>",i)
            },
            PreShipoutNode::Insert => {
                Self::readable_do_indent(indent,f)?;
                f.write_str("<insert>")
            },
            PreShipoutNode::VAdjust => {
                Self::readable_do_indent(indent,f)?;
                f.write_str("<vadjust>")
            },
            PreShipoutNode::Math => {
                Self::readable_do_indent(indent,f)?;
                f.write_str("<math>")
            },
            PreShipoutNode::Simple(s) => s.readable_fmt(indent, f),
            PreShipoutNode::Char { char, font, .. } =>
                Ok(char.display(f)),
            PreShipoutNode::Whatsit(w) => {
                Self::readable_do_indent(indent,f)?;
                write!(f, "{:?}",w)
            }
            PreShipoutNode::Custom(n) => n.readable_fmt(indent,f)
        }
    }
    fn height(&self) -> ET::Dim {
        match self {
            PreShipoutNode::Penalty(_) => ET::Dim::default(),
            PreShipoutNode::Skip(s) => s.height(),
            PreShipoutNode::Box(b) => b.height(),
            PreShipoutNode::Simple(s) => s.height(),
            PreShipoutNode::Char { height, .. } => *height,
            PreShipoutNode::Kern(k) => k.height(),
            PreShipoutNode::Mark(_, _) => ET::Dim::default(),
            PreShipoutNode::Insert | PreShipoutNode::VAdjust | PreShipoutNode::Math => todo!(),
            PreShipoutNode::Whatsit(_) => ET::Dim::default(),
            PreShipoutNode::Custom(n) => n.height()
        }
    }
    fn width(&self) -> ET::Dim {
        match self {
            PreShipoutNode::Penalty(_) => ET::Dim::default(),
            PreShipoutNode::Skip(s) => s.width(),
            PreShipoutNode::Box(b) => b.width(),
            PreShipoutNode::Simple(s) => s.width(),
            PreShipoutNode::Char { width, .. } => *width,
            PreShipoutNode::Kern(k) => k.width(),
            PreShipoutNode::Mark(_, _) => ET::Dim::default(),
            PreShipoutNode::Insert | PreShipoutNode::VAdjust | PreShipoutNode::Math => todo!(),
            PreShipoutNode::Whatsit(_) => ET::Dim::default(),
            PreShipoutNode::Custom(n) => n.width(),
        }
    }
    fn depth(&self) -> ET::Dim {
        match self {
            PreShipoutNode::Penalty(_) | PreShipoutNode::Skip(_) => ET::Dim::default(),
            PreShipoutNode::Box(b) => b.depth(),
            PreShipoutNode::Simple(s) => s.depth(),
            PreShipoutNode::Char { depth, .. } => *depth,
            PreShipoutNode::Kern(k) => k.depth(),
            PreShipoutNode::Mark(_, _) => ET::Dim::default(),
            PreShipoutNode::Insert | PreShipoutNode::VAdjust | PreShipoutNode::Math => todo!(),
            PreShipoutNode::Whatsit(_) => ET::Dim::default(),
            PreShipoutNode::Custom(n) => n.depth(),
        }
    }
    fn nodetype(&self) -> NodeType {
        match self {
            PreShipoutNode::Penalty(_) => NodeType::Penalty,
            PreShipoutNode::Skip(_) => NodeType::Glue,
            PreShipoutNode::Box(b) => b.nodetype(),
            PreShipoutNode::Simple(s) => s.nodetype(),
            PreShipoutNode::Char { .. } => NodeType::Char,
            PreShipoutNode::Kern(_) => NodeType::Kern,
            PreShipoutNode::Insert => NodeType::Insertion,
            PreShipoutNode::VAdjust => NodeType::Adjust,
            PreShipoutNode::Math => NodeType::Math,
            PreShipoutNode::Mark(_, _) => NodeType::Mark,
            PreShipoutNode::Whatsit(_) => NodeType::WhatsIt,
            PreShipoutNode::Custom(n) => n.nodetype(),
        }
    }
}
impl<ET:EngineTypes> PreShipoutNodeTrait<ET> for PreShipoutNode<ET> {
    #[inline(always)]
    fn as_node(self) -> PreShipoutNode<ET> { self }
    fn shipout(self, list: &mut Vec<ShipoutNode<ET>>, engine: &mut EngineReferences<ET>) {
        match self {
            PreShipoutNode::Penalty(p) => list.push(ShipoutNode::Penalty(p)),
            PreShipoutNode::Skip(s) => s.shipout(list,engine),
            PreShipoutNode::Kern(k) => k.shipout(list,engine),
            PreShipoutNode::Box(b) => b.shipout(list,engine),
            PreShipoutNode::Custom(n) => n.shipout(list,engine),
            PreShipoutNode::Mark(i, t) => list.push(ShipoutNode::Mark(i, t)),
            PreShipoutNode::Char { char, font, width, height, depth } => {
                list.push(ShipoutNode::Char { char, font, width, height, depth })
            },
            PreShipoutNode::Insert | PreShipoutNode::VAdjust | PreShipoutNode::Math => todo!(),
            PreShipoutNode::Simple(s) => s.shipout(list,engine),
            PreShipoutNode::Whatsit(w) => {
                if let Some(n) = w.call(engine) {
                    list.push(n)
                }
            }
        }
    }
}

#[derive(Debug,Clone)]
pub enum SimpleNode<ET:EngineTypes> {
    VRule{
        width:Option<ET::Dim>,
        height:Option<ET::Dim>,
        depth:Option<ET::Dim>,
        start:SR<ET>,end:SR<ET>
    },
    HRule{
        width:Option<ET::Dim>,
        height:Option<ET::Dim>,
        depth:Option<ET::Dim>,
        start:SR<ET>,end:SR<ET>
    }
}

impl<ET:EngineTypes> NodeTrait<ET> for SimpleNode<ET> {

    fn readable_fmt(&self, indent: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Self::readable_do_indent(indent,f)?;
        match self {
            SimpleNode::VRule { width, height, depth, .. } => {
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
            SimpleNode::HRule { width, height, depth, .. } => {
                write!(f, "<hrule")?;
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
            }
        }
    }
    fn height(&self) -> ET::Dim {
        match self {
            SimpleNode::VRule { height, .. } => height.unwrap_or_default(),
            SimpleNode::HRule { height, .. } => height.unwrap_or(ET::Dim::from_sp(26214)),
        }
    }
    fn width(&self) -> ET::Dim {
        match self {
            SimpleNode::VRule { width, .. } => width.unwrap_or(ET::Dim::from_sp(26214)),
            SimpleNode::HRule { width, .. } => width.unwrap_or_default(),
        }
    }
    fn depth(&self) -> ET::Dim {
        match self {
            SimpleNode::VRule { depth, .. } => depth.unwrap_or_default(),
            SimpleNode::HRule { depth, .. } => depth.unwrap_or_default(),
        }
    }
    fn nodetype(&self) -> NodeType {
        match self {
            SimpleNode::VRule {..} | SimpleNode::HRule {..} => NodeType::Rule
        }
    }
}
impl<ET:EngineTypes> PreShipoutNodeTrait<ET> for SimpleNode<ET> {
    #[inline(always)]
    fn as_node(self) -> PreShipoutNode<ET> { PreShipoutNode::Simple(self) }
    #[inline(always)]
    fn shipout(self, list: &mut Vec<ShipoutNode<ET>>, engine: &mut EngineReferences<ET>) {
        list.push(ShipoutNode::Simple(self))
    }
}

#[derive(Debug,Clone)]
pub enum KernNode<ET:EngineTypes> {
    VKern(ET::Dim),HKern(ET::Dim),MKern(ET::Dim)
}
impl <ET:EngineTypes> KernNode<ET> {
    pub fn dim(&self) -> ET::Dim { match self {
        KernNode::VKern(d) => *d,
        KernNode::HKern(d) => *d,
        KernNode::MKern(d) => *d
    } }
}
impl<ET:EngineTypes> NodeTrait<ET> for KernNode<ET> {
    fn readable_fmt(&self, indent: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Self::readable_do_indent(indent,f)?;
        match self {
            KernNode::VKern(d) => write!(f, "<vkern:{}>",d),
            KernNode::HKern(d) => write!(f, "<hkern:{}>",d),
            KernNode::MKern(d) => write!(f, "<mkern:{}>",d),
        }
    }
    fn height(&self) -> ET::Dim { match self {
        KernNode::VKern(d) => *d,
        _ => ET::Dim::default()
    } }
    fn width(&self) -> ET::Dim { match self {
        KernNode::HKern(d) => *d,
        _ => ET::Dim::default()
    }}
    fn depth(&self) -> ET::Dim { ET::Dim::default() }
    fn nodetype(&self) -> NodeType { NodeType::Kern }
}
impl<ET:EngineTypes> PreShipoutNodeTrait<ET> for KernNode<ET> {
    #[inline(always)]
    fn as_node(self) -> PreShipoutNode<ET> { PreShipoutNode::Kern(self) }
    #[inline(always)]
    fn shipout(self, list: &mut Vec<ShipoutNode<ET>>, engine: &mut EngineReferences<ET>) {
        list.push(ShipoutNode::Kern(self))
    }
}

#[derive(Debug,Clone)]
pub enum SkipNode<ET:EngineTypes> {
    VSkip(ET::Skip),Space,VFil,VFill,VFilneg,Vss,
    HSkip(ET::Skip),HFil,HFill,HFilneg,Hss
}
impl <ET:EngineTypes> SkipNode<ET> {
    pub fn skip(&self) -> ET::Skip { match self {
        SkipNode::VSkip(s) => *s,
        SkipNode::HSkip(s) => *s,
        _ => ET::Skip::default() // TODO
    } }
}
impl<ET:EngineTypes> NodeTrait<ET> for SkipNode<ET> {
    fn readable_fmt(&self, indent: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Self::readable_do_indent(indent,f)?;
        match self {
            SkipNode::VSkip(s) => write!(f, "<vskip:{}>",s),
            SkipNode::Space => write!(f, "<space>"),
            SkipNode::VFil => write!(f, "<vfil>"),
            SkipNode::VFill => write!(f, "<vfill>"),
            SkipNode::VFilneg => write!(f, "<vfilneg>"),
            SkipNode::Vss => write!(f, "<vss>"),
            SkipNode::HSkip(s) => write!(f, "<hskip:{}>",s),
            SkipNode::HFil => write!(f, "<hfil>"),
            SkipNode::HFill => write!(f, "<hfill>"),
            SkipNode::HFilneg => write!(f, "<hfilneg>"),
            SkipNode::Hss => write!(f, "<hss>"),
        }
    }
    fn height(&self) -> ET::Dim { match self {
        SkipNode::VSkip(s) => s.base(),
        _ => ET::Dim::default()
    }}
    fn width(&self) -> ET::Dim { match self {
        SkipNode::HSkip(s) => s.base(),
        SkipNode::Space => ET::Dim::from_sp(65536),
        _ => ET::Dim::default()
    } }
    #[inline(always)]
    fn depth(&self) -> ET::Dim { ET::Dim::default() }
    fn nodetype(&self) -> NodeType { NodeType::Glue }

}
impl<ET:EngineTypes> PreShipoutNodeTrait<ET> for SkipNode<ET> {
    #[inline(always)]
    fn as_node(self) -> PreShipoutNode<ET> { PreShipoutNode::Skip(self) }
    #[inline(always)]
    fn shipout(self, list: &mut Vec<ShipoutNode<ET>>, engine: &mut EngineReferences<ET>) {
        list.push(ShipoutNode::Skip(self))
    }
}

#[derive(Debug,Clone,PartialEq,Eq)]
pub enum ToOrSpread<D:TeXDimen> {
    None,To(D),Spread(D)
}

#[derive(Debug,Clone)]
pub struct BoxInfo<ET:EngineTypes> {
    pub tp:BoxType,
    pub kind:&'static str,
    pub scaled:ToOrSpread<ET::Dim>,
    pub assigned_width:Option<ET::Dim>,
    pub assigned_height:Option<ET::Dim>,
    pub assigned_depth:Option<ET::Dim>,
    pub moved_left:Option<ET::Dim>,
    pub raised:Option<ET::Dim>,
}

#[derive(Debug,Clone)]
pub struct TeXBox<ET:EngineTypes,T:TopNodeTrait<ET>> {
    pub children:Vec<T>,
    pub info:BoxInfo<ET>,
    pub start:SR<ET>,
    pub end:SR<ET>
}

impl <ET:EngineTypes,T:TopNodeTrait<ET>> NodeTrait<ET> for TeXBox<ET,T> {
    fn readable_fmt(&self, indent: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Self::readable_do_indent(indent, f)?;
        write!(f,"<{}>",self.info.kind)?;
        for c in &self.children {
            c.readable_fmt(indent+2, f)?;
        }
        Self::readable_do_indent(indent, f)?;
        write!(f,"</{}>",self.info.kind)
    }
    fn height(&self) -> ET::Dim {
        self.info.assigned_height.unwrap_or_else(||
            match self.info.tp {
                BoxType::Vertical => self.children.iter().map(|c| c.height()).sum(),
                _ => self.children.iter().map(|c| c.height()).max().unwrap_or_default(),
            }
        )
    }
    fn width(&self) -> ET::Dim {
        self.info.assigned_width.unwrap_or_else(||
            match self.info.tp {
                BoxType::Vertical => self.children.iter().map(|c| c.width()).max().unwrap_or_default(),
                _ => self.children.iter().map(|c| c.width()).sum(),
            }
        )
    }
    fn depth(&self) -> ET::Dim {
        self.info.assigned_depth.unwrap_or_else(||
            match self.info.tp {
                BoxType::Vertical => self.children.last().map(|c| c.depth()).unwrap_or_default(),
                _ => self.children.iter().map(|c| c.depth()).max().unwrap_or_default()
            }
        )
    }
    fn nodetype(&self) -> NodeType { match self.info.tp {
        BoxType::Horizontal => NodeType::HList,
        BoxType::Vertical => NodeType::VList,
        BoxType::InlineMath => NodeType::Math,
        BoxType::DisplayMath => NodeType::Math
    }}
}

impl <ET:EngineTypes> PreShipoutNodeTrait<ET> for TeXBox<ET,PreShipoutNode<ET>> {
    #[inline(always)]
    fn as_node(self) -> PreShipoutNode<ET> { PreShipoutNode::Box(self) }
    #[inline(always)]
    fn shipout(self, list: &mut Vec<ShipoutNode<ET>>, engine: &mut EngineReferences<ET>) {
        let mut nls = vec!();
        for c in self.children {
            c.shipout(&mut nls,engine);
        }
        list.push(ShipoutNode::Box(TeXBox {
            children: nls,
            info: self.info,
            start: self.start,
            end: self.end
        }))
    }
}