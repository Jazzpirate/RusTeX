use std::fmt::Write;
use std::marker::PhantomData;
use tex_engine::commands::pdftex::pdftexnodes::{NumOrName, PDFCatalog, PDFColor, PDFDestType, PDFLiteralOption, PDFNode, PDFObj, PDFOutline, PDFXForm, PDFXImage};
use tex_engine::engine::stomach::ParLineSpec;
use tex_engine::tex::nodes::{NodeTrait, ReadableNode, TeXNode};
use tex_engine::tex::numerics::Dim32;
use tex_engine::tex::types::NodeType;
use crate::engine::{Font, Types};
use crate::shipout::ZERO;

#[derive(Debug,Clone)]
pub enum RusTeXNode {
    PDFNode(PDFNode<Types>),
    FontChange(Font,bool),
    FontChangeEnd,
    ParagraphBegin(Vec<ParLineSpec<Types>>),
    ParagraphEnd,
    Br
}
impl NodeTrait<Types> for RusTeXNode {
    fn as_node(self) -> TeXNode<Types> {
        TeXNode::Custom(self)
    }
    fn height(&self) -> Dim32 {
        match self {
            Self::PDFNode(n) => n.height(),
            _ => ZERO
        }
    }
    fn depth(&self) -> Dim32 {
        match self {
            Self::PDFNode(n) => n.depth(),
            _ => ZERO
        }
    }
    fn width(&self) -> Dim32 {
        match self {
            Self::PDFNode(n) => n.width(),
            _ => ZERO
        }
    }
    fn nodetype(&self) -> NodeType {NodeType::WhatsIt}
    fn readable_fmt(&self, indent:usize, f:&mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::PDFNode(n) => n.readable_fmt(indent,f),
            _ => write!(f,"<{:?}>",self)
        }
    }
    fn opaque(&self) -> bool {
        match self {
            Self::PDFNode(n) => n.opaque(),
            _ => true
        }
    }
}
impl From<PDFNode<Types>> for RusTeXNode {
    fn from(value: PDFNode<Types>) -> Self {
        Self::PDFNode(value)
    }
}