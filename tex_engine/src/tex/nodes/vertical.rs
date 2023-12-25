use crate::engine::EngineTypes;
use crate::engine::mouth::pretokenized::TokenList;
use crate::tex::nodes::{NodeTrait, SR, WhatsitNode};
use crate::tex::nodes::boxes::TeXBox;
use crate::tex::types::NodeType;
use crate::tex::numerics::TeXDimen;
use crate::tex::numerics::Skip;

#[derive(Clone,Debug)]
pub enum VNode<ET:EngineTypes> {
    Penalty(i32),
    Mark(usize,TokenList<ET::Token>),
    Whatsit(WhatsitNode<ET>),
    VSkip(ET::Skip),
    VFil,VFill,VFilneg,Vss,
    VKern(ET::Dim),
    Box(TeXBox<ET>),
    HRule{
        width:Option<ET::Dim>,
        height:Option<ET::Dim>,
        depth:Option<ET::Dim>,
        start:SR<ET>,end:SR<ET>
    },
    Insert,
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
    fn readable_fmt(&self, indent: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VNode::Penalty(p) => {
                Self::readable_do_indent(indent,f)?;
                write!(f, "<penalty:{}>",p)
            },
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
            VNode::Box(b) => b.readable_fmt(indent, f),
            VNode::Mark(i, _) => {
                Self::readable_do_indent(indent,f)?;
                write!(f, "<mark:{}>",i)
            },
            VNode::Insert => {
                Self::readable_do_indent(indent,f)?;
                f.write_str("<insert>")
            },
            VNode::Whatsit(w) => {
                Self::readable_do_indent(indent,f)?;
                write!(f, "{:?}",w)
            }
            VNode::Custom(n) => n.readable_fmt(indent, f)
        }
    }
    fn height(&self) -> ET::Dim {
        match self {
            VNode::VKern(d) => *d,
            VNode::Box(b) => b.height(),
            VNode::HRule { height, .. } => height.unwrap_or(ET::Dim::from_sp(26214)),
            VNode::Insert  => todo!(),
            VNode::Custom(n) => n.height(),
            VNode::VSkip(s) => s.base(),
            _ => ET::Dim::default(),
        }
    }
    fn width(&self) -> ET::Dim {
        match self {
            VNode::Box(b) => b.width(),
            VNode::HRule { width, .. } => width.unwrap_or_default(),
            VNode::Insert => todo!(),
            VNode::Custom(n) => n.width(),
            _ => ET::Dim::default(),
        }
    }
    fn depth(&self) -> ET::Dim {
        match self {
            VNode::Box(b) => b.depth(),
            VNode::HRule { depth, .. } => depth.unwrap_or_default(),
            VNode::Insert => todo!(),
            VNode::Custom(n) => n.depth(),
            _ => ET::Dim::default(),
        }
    }
    fn nodetype(&self) -> NodeType {
        match self {
            VNode::Penalty(_) => NodeType::Penalty,
            VNode::HRule {..} => NodeType::Rule,
            VNode::Box(b) => b.nodetype(),
            VNode::VKern(_) => NodeType::Kern,
            VNode::Insert => NodeType::Insertion,
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