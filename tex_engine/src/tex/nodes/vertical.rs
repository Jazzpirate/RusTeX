/*! Nodes allowed in vertical lists. */
use crate::engine::EngineTypes;
use crate::engine::filesystem::SourceRef;
use crate::tex::tokens::token_lists::TokenList;
use crate::tex::nodes::{BoxTarget, display_do_indent, Leaders, NodeTrait, NodeType, WhatsitNode};
use crate::tex::nodes::boxes::{TeXBox, ToOrSpread, VBoxInfo};
use crate::tex::numerics::TeXDimen;
use crate::tex::numerics::Skip;

/// A vertical list node.
#[derive(Clone,Debug)]
pub enum VNode<ET:EngineTypes> {
    /// A penalty node, as produced by `\penalty`.
    Penalty(i32),
    /// A mark node, as produced by `\mark`.
    Mark(usize,TokenList<ET::Token>),
    /// A whatsit node, as produced by `\special`, `\write`, etc.
    Whatsit(WhatsitNode<ET>),
    /// A glue node, as produced by `\vskip`.
    VSkip(Skip<ET::Dim>),
    /// A glue node, as produced by `\vfil`.
    VFil,
    /// A glue node, as produced by `\vfill`.
    VFill,
    /// A glue node, as produced by `\vfilneg`.
    VFilneg,
    /// A glue node, as produced by `\vss`.
    Vss,
    /// A kern node, as produced by `\kern`.
    VKern(ET::Dim),
    /// Leaders, as produced by `\leaders` or `\cleaders` or `\xleaders`.
    Leaders(Leaders<ET>),
    /// A box node, as produced by `\hbox`, `\vbox`, `\vtop`, etc.
    Box(TeXBox<ET>),
    /// A rule node, as produced by `\hrule`.
    HRule{
        /// The *provided* width of the rule.
        width:Option<ET::Dim>,
        /// The *provided* height of the rule.
        height:Option<ET::Dim>,
        /// The *provided* depth of the rule.
        depth:Option<ET::Dim>,
        /// The source reference for the start of the rule.
        start:SourceRef<ET>,
        /// The source reference for the end of the rule.
        end:SourceRef<ET>
    },
    /// An insertion node, as produced by `\insert`.
    Insert(usize,Box<[VNode<ET>]>),
    /// A custom node.
    Custom(ET::CustomNode),
}

impl<ET:EngineTypes> VNode<ET> {
    /// Whether this node is discardable.
    pub fn discardable(&self) -> bool {
        use VNode::*;
        matches!(self, Penalty(_) | VSkip(_) | VKern(_) | VFil | VFill | VFilneg | Vss)
    }
}

impl<ET:EngineTypes> NodeTrait<ET> for VNode<ET> {
    fn display_fmt(&self, indent: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VNode::Penalty(p) => {
                display_do_indent(indent,f)?;
                write!(f, "<penalty:{}>",p)
            },
            VNode::Leaders(l) => l.display_fmt(indent, f),
            VNode::VSkip(s) => write!(f, "<vskip:{}>",s),
            VNode::VFil => write!(f, "<vfil>"),
            VNode::VFill => write!(f, "<vfill>"),
            VNode::VFilneg => write!(f, "<vfilneg>"),
            VNode::Vss => write!(f, "<vss>"),
            VNode::VKern(d) => write!(f, "<vkern:{}>",d),
            VNode::HRule { width, height, depth, .. } => {
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
            VNode::Box(b) => b.display_fmt(indent, f),
            VNode::Mark(i, _) => {
                display_do_indent(indent,f)?;
                write!(f, "<mark:{}>",i)
            },
            VNode::Insert(n,ch) => {
                display_do_indent(indent,f)?;
                write!(f,"<insert {}>",n)?;
                for c in ch.iter() {
                    c.display_fmt(indent + 2, f)?;
                }
                display_do_indent(indent,f)?;
                write!(f,"</insert>")
            },
            VNode::Whatsit(w) => {
                display_do_indent(indent,f)?;
                write!(f, "{:?}",w)
            }
            VNode::Custom(n) => n.display_fmt(indent, f)
        }
    }
    fn height(&self) -> ET::Dim {
        match self {
            VNode::VKern(d) => *d,
            VNode::Box(b) => b.height(),
            VNode::Leaders(l) => l.height(),
            VNode::HRule { height, .. } => height.unwrap_or(ET::Dim::from_sp(26214)),
            VNode::Custom(n) => n.height(),
            VNode::VSkip(s) => s.base,
            _ => ET::Dim::default(),
        }
    }
    fn width(&self) -> ET::Dim {
        match self {
            VNode::Box(b) => b.width(),
            VNode::HRule { width, .. } => width.unwrap_or_default(),
            VNode::Custom(n) => n.width(),
            VNode::Leaders(l) => l.width(),
            _ => ET::Dim::default(),
        }
    }
    fn depth(&self) -> ET::Dim {
        match self {
            VNode::Box(b) => b.depth(),
            VNode::HRule { depth, .. } => depth.unwrap_or_default(),
            VNode::Custom(n) => n.depth(),
            VNode::Leaders(l) => l.depth(),
            _ => ET::Dim::default(),
        }
    }
    fn nodetype(&self) -> NodeType {
        match self {
            VNode::Penalty(_) => NodeType::Penalty,
            VNode::HRule {..} => NodeType::Rule,
            VNode::Leaders(_) => NodeType::Glue,
            VNode::Box(b) => b.nodetype(),
            VNode::VKern(_) => NodeType::Kern,
            VNode::Insert(..) => NodeType::Insertion,
            VNode::Mark(_, _) => NodeType::Mark,
            VNode::Whatsit(_) => NodeType::WhatsIt,
            VNode::Custom(n) => n.nodetype(),
            VNode::VSkip(_) | VNode::VFil | VNode::VFill | VNode::VFilneg | VNode::Vss => NodeType::Glue,
        }
    }
    fn opaque(&self) -> bool {
        match self {
            VNode::Mark(_, _) => true,
            VNode::Custom(n) => n.opaque(),
            _ => false
        }
    }
    fn sourceref(&self) -> Option<(&SourceRef<ET>, &SourceRef<ET>)> {
        match self {
            VNode::HRule { start, end, .. } => Some((start, end)),
            VNode::Box(b) => b.sourceref(),
            _ => None
        }
    }
}

/// The kinds of vertical lists that can occur. TODO: rethink this
#[derive(Clone,Debug)]
pub enum VerticalNodeListType<ET:EngineTypes> {
    /// A vertical box
    Box(VBoxInfo<ET>,SourceRef<ET>,BoxTarget<ET>),
    /// `\insert`
    Insert(usize),
    /// `\vcenter`
    VCenter(SourceRef<ET>,ToOrSpread<ET::Dim>),
    /// `\vadjust`
    VAdjust,
    /// A column in a `\valign`. The source ref indicates the start of the column.
    VAlignColumn(SourceRef<ET>),
    /// A cell in a `\valign`. The source ref indicates the start of the cell.
    /// The `u8` indicates the number of *additional* rows spanned by this cell
    /// (so by default 0)
    VAlignCell(SourceRef<ET>,u8),
    /// An `\halign` list
    HAlign,
    /// A page to be output
    Page
}