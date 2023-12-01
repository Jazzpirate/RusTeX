use std::marker::PhantomData;
use crate::engine::EngineTypes;
use crate::engine::filesystem::kpathsea::SourceReference;
use crate::tex::types::{BoxType, NodeType};
use crate::engine::filesystem::File;

#[derive(Debug,Clone)]
pub struct NodeList<ET:EngineTypes> {
    pub children:Vec<TeXNode<ET>>,
    pub tp:NodeListType<ET>
}

#[derive(Clone,Debug)]
pub enum NodeListType<ET:EngineTypes> {
    Paragraph,Box(BoxInfo<ET>,SourceReference<<ET::File as File>::SourceRefID>,Option<(u16,bool)>)
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
    Box(TeXBox<ET>)
}

impl<ET:EngineTypes> NodeTrait<ET> for TeXNode<ET> {
    #[inline(always)]
    fn as_node(self) -> TeXNode<ET> { self }
    fn height(&self) -> ET::Dim {
        match self {
            TeXNode::Penalty(_) => ET::Dim::default(),
            TeXNode::Skip(s) => s.height(),
            TeXNode::Box(b) => b.height()
        }
    }
    fn width(&self) -> ET::Dim {
        match self {
            TeXNode::Penalty(_) => ET::Dim::default(),
            TeXNode::Skip(s) => s.width(),
            TeXNode::Box(b) => b.width()
        }
    }
    fn depth(&self) -> ET::Dim {
        match self {
            TeXNode::Penalty(_) | TeXNode::Skip(_) => ET::Dim::default(),
            TeXNode::Box(b) => b.depth()
        }
    }
    fn nodetype(&self) -> NodeType {
        match self {
            TeXNode::Penalty(_) => NodeType::Penalty,
            TeXNode::Skip(_) => NodeType::Glue,
            TeXNode::Box(b) => b.nodetype()
        }
    }
}

#[derive(Debug,Clone)]
pub enum SkipNode<ET:EngineTypes> {
    Phantom(PhantomData<ET>)
}
impl<ET:EngineTypes> NodeTrait<ET> for SkipNode<ET> {
    #[inline(always)]
    fn as_node(self) -> TeXNode<ET> { TeXNode::Skip(self) }
    fn height(&self) -> ET::Dim { todo!() }
    fn width(&self) -> ET::Dim { todo!() }
    fn depth(&self) -> ET::Dim { todo!() }
    fn nodetype(&self) -> NodeType { NodeType::Glue }

}

#[derive(Debug,Clone)]
pub struct BoxInfo<ET:EngineTypes> {
    pub tp:BoxType,
    pub kind:&'static str,
    pub to:Option<ET::Dim>,
    pub spread:Option<ET::Dim>,
    pub assigned_width:Option<ET::Dim>,
    pub assigned_height:Option<ET::Dim>,
    pub assigned_depth:Option<ET::Dim>,
}

#[derive(Debug,Clone)]
pub struct TeXBox<ET:EngineTypes> {
    pub children:Vec<TeXNode<ET>>,
    pub info:BoxInfo<ET>,
    pub start:SourceReference<<ET::File as File>::SourceRefID>,
    pub end:SourceReference<<ET::File as File>::SourceRefID>
}

impl <ET:EngineTypes> NodeTrait<ET> for TeXBox<ET> {
    #[inline(always)]
    fn as_node(self) -> TeXNode<ET> { TeXNode::Box(self) }
    fn height(&self) -> ET::Dim { todo!() }
    fn width(&self) -> ET::Dim { todo!() }
    fn depth(&self) -> ET::Dim { todo!() }
    fn nodetype(&self) -> NodeType { match self.info.tp {
        BoxType::Horizontal => NodeType::HList,
        BoxType::Vertical => NodeType::VList,
        BoxType::InlineMath => NodeType::Math,
        BoxType::DisplayMath => NodeType::Math
    }}
}