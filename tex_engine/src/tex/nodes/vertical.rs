use crate::engine::EngineTypes;
use crate::engine::filesystem::SourceRef;
use crate::tex::tokens::token_lists::TokenList;
use crate::tex::nodes::{BoxTarget, display_do_indent, Leaders, NodeTrait, NodeType, WhatsitNode};
use crate::tex::nodes::boxes::{TeXBox, ToOrSpread, VBoxInfo};
use crate::tex::numerics::TeXDimen;
use crate::tex::numerics::Skip;

#[derive(Clone,Debug)]
pub enum VerticalNodeListType<ET:EngineTypes> {
    Box(VBoxInfo<ET>,SourceRef<ET>,BoxTarget<ET>),
    Insert(usize),
    VCenter(SourceRef<ET>,ToOrSpread<ET::Dim>),
    VAdjust,
    VAlignRow(SourceRef<ET>),
    VAlignCell(SourceRef<ET>,u8),
    HAlign,Page
}


#[derive(Clone,Debug)]
pub enum VNode<ET:EngineTypes> {
    Penalty(i32),
    Mark(usize,TokenList<ET::Token>),
    Whatsit(WhatsitNode<ET>),
    VSkip(Skip<ET::Dim>),
    VFil,VFill,VFilneg,Vss,
    VKern(ET::Dim),
    Leaders(Leaders<ET>),
    Box(TeXBox<ET>),
    HRule{
        width:Option<ET::Dim>,
        height:Option<ET::Dim>,
        depth:Option<ET::Dim>,
        start:SourceRef<ET>,end:SourceRef<ET>
    },
    Insert(usize,Box<[VNode<ET>]>),
    Custom(ET::CustomNode),
}

impl<ET:EngineTypes> VNode<ET> {
    pub fn discardable(&self) -> bool {
        use VNode::*;
        match self {
            Penalty(_) | VSkip(_) | VKern(_) | VFil | VFill | VFilneg | Vss  => true,
            _ => false
        }
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
}