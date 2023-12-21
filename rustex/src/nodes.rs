use std::fmt::Write;
use std::marker::PhantomData;
use tex_engine::commands::pdftex::pdftexnodes::{NumOrName, PDFCatalog, PDFColor, PDFDestType, PDFLiteralOption, PDFNode, PDFObj, PDFOutline, PDFXForm, PDFXImage};
use tex_engine::engine::state::State;
use tex_engine::engine::stomach::ParLineSpec;
use tex_engine::engine::utils::memory::PRIMITIVES;
use tex_engine::tex::nodes::{NodeTrait, ReadableNode, TeXNode};
use tex_engine::tex::numerics::{Dim32, Skip32};
use tex_engine::tex::types::NodeType;
use crate::engine::{Font, SRef, Types};
use crate::shipout::ZERO;
use crate::state::RusTeXState;

#[derive(Debug,Clone,Default,PartialEq)]
pub struct LineSkip {
    pub baselineskip:f64,
    pub lineskip:f64,
    pub lineskiplimit:f64,
}
impl LineSkip {
    pub fn get(state:&RusTeXState) -> Self {
        Self {
            baselineskip:state.get_primitive_skip(PRIMITIVES.baselineskip).base.0 as f64,
            lineskip:state.get_primitive_skip(PRIMITIVES.lineskip).base.0 as f64,
            lineskiplimit:state.get_primitive_dim(PRIMITIVES.lineskiplimit).0 as f64,
        }
    }
    pub fn factor(&self,font:&Font) -> f64 {
        use tex_engine::engine::fontsystem::Font;
        let at = font.get_at().0 as f64;
        if self.baselineskip >= (self.lineskiplimit + at) {
            self.baselineskip
        } else { at + self.lineskip }
    }
}

#[derive(Debug,Clone)]
pub enum RusTeXNode {
    PDFNode(PDFNode<Types>),
    FontChange(Font,bool),
    FontChangeEnd,
    ParagraphBegin{
        specs:Vec<ParLineSpec<Types>>,
        start:SRef,
        end:SRef,
        lineskip:LineSkip,
    },
    ParagraphEnd,
    HAlignBegin,HAlignEnd,
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