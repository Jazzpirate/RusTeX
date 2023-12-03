use std::marker::PhantomData;
use crate::engine::EngineTypes;
use crate::engine::filesystem::kpathsea::SourceReference;
use crate::tex::types::{BoxType, NodeType};
use crate::engine::filesystem::File;
use crate::engine::fontsystem::FontSystem;
use crate::engine::mouth::pretokenized::TokenList;
use crate::tex::numerics::{Skip, TeXDimen};
use crate::utils::Ptr;

type SR<ET> = SourceReference<<<ET as EngineTypes>::File as File>::SourceRefID>;

#[derive(Debug,Clone)]
pub struct NodeList<ET:EngineTypes> {
    pub children:Vec<TeXNode<ET>>,
    pub tp:NodeListType<ET>
}

#[derive(Clone,Debug)]
pub enum NodeListType<ET:EngineTypes> {
    Paragraph,Box(BoxInfo<ET>,SR<ET>,Option<(u16,bool)>)
}

pub trait NodeTrait<ET:EngineTypes> {
    fn as_node(self) -> TeXNode<ET>;
    fn height(&self) -> ET::Dim;
    fn depth(&self) -> ET::Dim;
    fn width(&self) -> ET::Dim;
    fn nodetype(&self) -> NodeType;
}

#[derive(Debug,Clone)]
pub enum TeXNode<ET:EngineTypes> {
    Penalty(i32),
    Skip(SkipNode<ET>),
    Kern(KernNode<ET>),
    Box(TeXBox<ET>),
    Mark(usize,TokenList<ET::Token>),
    Insert,
    Simple(SimpleNode<ET>),
    Char { char:ET::Char, font:<ET::FontSystem as FontSystem>::Font, width:ET::Dim, height:ET::Dim, depth:ET::Dim  }
}

impl<ET:EngineTypes> NodeTrait<ET> for TeXNode<ET> {
    #[inline(always)]
    fn as_node(self) -> TeXNode<ET> { self }
    fn height(&self) -> ET::Dim {
        match self {
            TeXNode::Penalty(_) => ET::Dim::default(),
            TeXNode::Skip(s) => s.height(),
            TeXNode::Box(b) => b.height(),
            TeXNode::Simple(s) => s.height(),
            TeXNode::Char { height, .. } => *height,
            TeXNode::Kern(k) => k.height(),
            TeXNode::Mark(_,_) => ET::Dim::default(),
            TeXNode::Insert => todo!()
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
            TeXNode::Mark(_,_) => ET::Dim::default(),
            TeXNode::Insert => todo!()
        }
    }
    fn depth(&self) -> ET::Dim {
        match self {
            TeXNode::Penalty(_) | TeXNode::Skip(_) => ET::Dim::default(),
            TeXNode::Box(b) => b.depth(),
            TeXNode::Simple(s) => s.depth(),
            TeXNode::Char { depth, .. } => *depth,
            TeXNode::Kern(k) => k.depth(),
            TeXNode::Mark(_,_) => ET::Dim::default(),
            TeXNode::Insert => todo!()
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
            TeXNode::Mark(_,_) => NodeType::Mark
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
    #[inline(always)]
    fn as_node(self) -> TeXNode<ET> { TeXNode::Simple(self) }
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
    #[inline(always)]
    fn as_node(self) -> TeXNode<ET> { TeXNode::Kern(self) }
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
    #[inline(always)]
    fn as_node(self) -> TeXNode<ET> { TeXNode::Skip(self) }
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
    #[inline(always)]
    fn as_node(self) -> TeXNode<ET> { TeXNode::Box(self) }
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