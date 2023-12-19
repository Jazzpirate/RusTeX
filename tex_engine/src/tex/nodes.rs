use std::cell::RefCell;
use std::fmt::{Debug, Display, Write};
use std::marker::PhantomData;
use crate::engine::{EngineReferences, EngineTypes};
use crate::engine::filesystem::SourceReference;
use crate::tex::types::{BoxType, NodeType};
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
    pub children:Vec<TeXNode<ET>>,
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
    fn as_node(self) -> TeXNode<ET>;
    fn height(&self) -> ET::Dim;
    fn depth(&self) -> ET::Dim;
    fn width(&self) -> ET::Dim;
    fn nodetype(&self) -> NodeType;
    fn readable_fmt(&self, indent:usize, f:&mut std::fmt::Formatter<'_>) -> std::fmt::Result;
    #[inline(always)]
    fn readable_do_indent(indent:usize,f:&mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char('\n')?;
        for _ in 0..indent {f.write_char(' ')?;}
        Ok(())
    }
    fn readable(&self) -> ReadableNode<ET,Self> where Self:Sized {
        ReadableNode(self,PhantomData)
    }
    fn opaque(&self) -> bool { false }
}
impl<ET:EngineTypes<CustomNode = ()>> NodeTrait<ET> for () {
    fn height(&self) -> ET::Dim { ET::Dim::default() }
    fn depth(&self) -> ET::Dim { ET::Dim::default() }
    fn width(&self) -> ET::Dim { ET::Dim::default() }
    fn nodetype(&self) -> NodeType { NodeType::WhatsIt }
    fn readable_fmt(&self, _indent:usize, _f:&mut std::fmt::Formatter<'_>) -> std::fmt::Result { Ok(()) }
    fn opaque(&self) -> bool { true }
    fn as_node(self) -> TeXNode<ET> { TeXNode::Custom(self) }
}

pub struct ReadableNode<'a,ET:EngineTypes,N: NodeTrait<ET>>(&'a N, PhantomData<ET>);
impl<'a,ET:EngineTypes,N: NodeTrait<ET>> Display for ReadableNode<'a,ET,N> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.readable_fmt(0, f)
    }
}

pub type WhatsitFunction<ET> = Ptr<RefCell<Option<Box<dyn FnOnce(&mut EngineReferences<ET>) -> Option<TeXNode<ET>>>>>>;
#[derive(Clone)]
pub struct WhatsitNode<ET:EngineTypes>(String,WhatsitFunction<ET>);
impl<ET:EngineTypes> std::fmt::Debug for WhatsitNode<ET> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f,"<whatsit {}>",self.0)
    }
}
impl<ET:EngineTypes> WhatsitNode<ET> {
    pub fn new(f:Box<dyn FnOnce(&mut EngineReferences<ET>) -> Option<TeXNode<ET>>>, name:PrimitiveIdentifier) -> TeXNode<ET> {
        TeXNode::Whatsit(WhatsitNode(PRIMITIVES.printable::<ET::Char>(name, None).to_string(),
                                     Ptr::new(RefCell::new(Some(f)))
        ))
    }
    pub fn call(self,engine: &mut EngineReferences<ET>) -> Option<TeXNode<ET>> {
        let f = self.1.replace(None);
        f.map(|f| f(engine)).flatten()
    }
}

#[derive(Clone,Debug)]
pub enum TeXNode<ET:EngineTypes> {
    Penalty(i32),
    Mark(usize,TokenList<ET::Token>),
    Whatsit(WhatsitNode<ET>),
    Skip(SkipNode<ET>),
    Kern(KernNode<ET>),
    Box(TeXBox<ET>),
    Insert,
    VAdjust,
    Math,
    Simple(SimpleNode<ET>),
    Custom(ET::CustomNode),
    Char { char:ET::Char, font:<ET::FontSystem as FontSystem>::Font, width:ET::Dim, height:ET::Dim, depth:ET::Dim  }
}
impl<ET:EngineTypes> TeXNode<ET> {
    pub fn discardable(&self) -> bool {
        match self {
            TeXNode::Penalty(_) => true,
            TeXNode::Skip(_) => true,
            TeXNode::Kern(_) => true,
            _ => false
        }
    }
}

impl<ET:EngineTypes> NodeTrait<ET> for TeXNode<ET> {
    fn readable_fmt(&self, indent: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TeXNode::Penalty(p) => {
                Self::readable_do_indent(indent,f)?;
                write!(f, "<penalty:{}>",p)
            },
            TeXNode::Skip(s) => s.readable_fmt(indent, f),
            TeXNode::Kern(k) => k.readable_fmt(indent, f),
            TeXNode::Box(b) => b.readable_fmt(indent, f),
            TeXNode::Mark(i, _) => {
                Self::readable_do_indent(indent,f)?;
                write!(f, "<mark:{}>",i)
            },
            TeXNode::Insert => {
                Self::readable_do_indent(indent,f)?;
                f.write_str("<insert>")
            },
            TeXNode::VAdjust => {
                Self::readable_do_indent(indent,f)?;
                f.write_str("<vadjust>")
            },
            TeXNode::Math => {
                Self::readable_do_indent(indent,f)?;
                f.write_str("<math>")
            },
            TeXNode::Simple(s) => s.readable_fmt(indent, f),
            TeXNode::Char { char, .. } =>
                Ok(char.display(f)),
            TeXNode::Whatsit(w) => {
                Self::readable_do_indent(indent,f)?;
                write!(f, "{:?}",w)
            }
            TeXNode::Custom(n) => n.readable_fmt(indent, f)
        }
    }
    fn height(&self) -> ET::Dim {
        match self {
            TeXNode::Penalty(_) => ET::Dim::default(),
            TeXNode::Skip(s) => s.height(),
            TeXNode::Box(b) => b.height(),
            TeXNode::Simple(s) => s.height(),
            TeXNode::Char { height, .. } => *height,
            TeXNode::Kern(k) => k.height(),
            TeXNode::Mark(_, _) => ET::Dim::default(),
            TeXNode::Insert | TeXNode::VAdjust | TeXNode::Math => todo!(),
            TeXNode::Whatsit(_) => ET::Dim::default(),
            TeXNode::Custom(n) => n.height()
        }
    }
    fn width(&self) -> ET::Dim {
        match self {
            TeXNode::Penalty(_) => ET::Dim::default(),
            TeXNode::Skip(s) => s.width(),
            TeXNode::Box(b) => b.width(),
            TeXNode::Simple(s) => s.width(),
            TeXNode::Char { width, .. } => *width,
            TeXNode::Kern(k) => k.width(),
            TeXNode::Mark(_, _) => ET::Dim::default(),
            TeXNode::Insert | TeXNode::VAdjust | TeXNode::Math => todo!(),
            TeXNode::Whatsit(_) => ET::Dim::default(),
            TeXNode::Custom(n) => n.width(),
        }
    }
    fn depth(&self) -> ET::Dim {
        match self {
            TeXNode::Penalty(_) | TeXNode::Skip(_) => ET::Dim::default(),
            TeXNode::Box(b) => b.depth(),
            TeXNode::Simple(s) => s.depth(),
            TeXNode::Char { depth, .. } => *depth,
            TeXNode::Kern(k) => k.depth(),
            TeXNode::Mark(_, _) => ET::Dim::default(),
            TeXNode::Insert | TeXNode::VAdjust | TeXNode::Math => todo!(),
            TeXNode::Whatsit(_) => ET::Dim::default(),
            TeXNode::Custom(n) => n.depth(),
        }
    }
    fn nodetype(&self) -> NodeType {
        match self {
            TeXNode::Penalty(_) => NodeType::Penalty,
            TeXNode::Skip(_) => NodeType::Glue,
            TeXNode::Box(b) => b.nodetype(),
            TeXNode::Simple(s) => s.nodetype(),
            TeXNode::Char { .. } => NodeType::Char,
            TeXNode::Kern(_) => NodeType::Kern,
            TeXNode::Insert => NodeType::Insertion,
            TeXNode::VAdjust => NodeType::Adjust,
            TeXNode::Math => NodeType::Math,
            TeXNode::Mark(_, _) => NodeType::Mark,
            TeXNode::Whatsit(_) => NodeType::WhatsIt,
            TeXNode::Custom(n) => n.nodetype(),
        }
    }
    fn opaque(&self) -> bool {
        match self {
            TeXNode::Penalty(_) => false,
            TeXNode::Skip(_) => false,
            TeXNode::Kern(_) => false,
            TeXNode::Box(_) => false,
            TeXNode::Simple(s) => s.opaque(),
            TeXNode::Char { .. } => false,
            TeXNode::Mark(_, _) => true,
            TeXNode::Insert => false,
            TeXNode::VAdjust => true,
            TeXNode::Math => false,
            TeXNode::Whatsit(_) => false,
            TeXNode::Custom(n) => n.opaque(),
        }
    }
    #[inline(always)]
    fn as_node(self) -> TeXNode<ET> { self }
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
    #[inline(always)]
    fn as_node(self) -> TeXNode<ET> { TeXNode::Simple(self) }
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
    #[inline(always)]
    fn as_node(self) -> TeXNode<ET> { TeXNode::Kern(self) }
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
    #[inline(always)]
    fn as_node(self) -> TeXNode<ET> { TeXNode::Skip(self) }
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
pub struct TeXBox<ET:EngineTypes> {
    pub children:Vec<TeXNode<ET>>,
    pub info:BoxInfo<ET>,
    pub start:SR<ET>,
    pub end:SR<ET>
}

impl <ET:EngineTypes> NodeTrait<ET> for TeXBox<ET> {
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
    #[inline(always)]
    fn as_node(self) -> TeXNode<ET> { TeXNode::Box(self) }
}