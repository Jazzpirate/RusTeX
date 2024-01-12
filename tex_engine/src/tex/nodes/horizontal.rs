use crate::engine::EngineTypes;
use crate::engine::filesystem::SourceRef;
use crate::engine::fontsystem::{Font, FontSystem};
use crate::tex::tokens::token_lists::TokenList;
use crate::tex::nodes::{Leaders, NodeTrait, WhatsitNode};
use crate::tex::nodes::boxes::TeXBox;
use crate::tex::nodes::math::{MathFontStyle, MathGroup};
use crate::tex::nodes::vertical::VNode;
use crate::tex::types::NodeType;
use crate::tex::characters::Character;
use crate::tex::numerics::TeXDimen;
use crate::tex::numerics::Skip;


#[derive(Clone,Debug)]
pub enum HNode<ET:EngineTypes> {
    Penalty(i32),
    Mark(usize,TokenList<ET::Token>),
    Whatsit(WhatsitNode<ET>),
    HSkip(ET::Skip),HFil,HFill,HFilneg,Hss,Space,
    HKern(ET::Dim),
    Leaders(Leaders<ET>),
    Box(TeXBox<ET>),
    VRule{
        width:Option<ET::Dim>,
        height:Option<ET::Dim>,
        depth:Option<ET::Dim>,
        start:SourceRef<ET>,end:SourceRef<ET>
    },
    Insert(usize,Box<[VNode<ET>]>),
    VAdjust(Box<[VNode<ET>]>),
    MathGroup(MathGroup<ET,MathFontStyle<ET>>),
    Char { char:ET::Char, font:<ET::FontSystem as FontSystem>::Font },
    Custom(ET::CustomNode),
}

impl<ET:EngineTypes> NodeTrait<ET> for HNode<ET> {
    fn readable_fmt(&self, indent: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            HNode::Penalty(p) => {
                Self::readable_do_indent(indent,f)?;
                write!(f, "<penalty:{}>",p)
            },
            HNode::Leaders(l) => l.readable_fmt(indent, f),
            HNode::Box(b) => b.readable_fmt(indent, f),
            HNode::Mark(i, _) => {
                Self::readable_do_indent(indent,f)?;
                write!(f, "<mark:{}>",i)
            },
            HNode::VRule { width, height, depth, .. } => {
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
            HNode::Insert(n,ch) => {
                Self::readable_do_indent(indent,f)?;
                write!(f,"<insert {}>",n)?;
                for c in ch.iter() {
                    c.readable_fmt(indent + 2,f)?;
                }
                Self::readable_do_indent(indent,f)?;
                write!(f,"</insert>")
            },
            HNode::VAdjust(ls) => {
                Self::readable_do_indent(indent,f)?;
                f.write_str("<vadjust>")?;
                for c in ls.iter() {
                    c.readable_fmt(indent+2, f)?;
                }
                Self::readable_do_indent(indent,f)?;
                f.write_str("</vadjust>")
            },
            HNode::MathGroup(mg) => {
                mg.readable_fmt(indent, f)
            }
            HNode::Char { char, .. } =>
                Ok(char.display_fmt(f)),
            HNode::Whatsit(w) => {
                Self::readable_do_indent(indent,f)?;
                write!(f, "{:?}",w)
            }
            HNode::HSkip(s) => write!(f, "<hskip:{}>",s),
            HNode::HFil => write!(f, "<hfil>"),
            HNode::HFill => write!(f, "<hfill>"),
            HNode::HFilneg => write!(f, "<hfilneg>"),
            HNode::Hss => write!(f, "<hss>"),
            HNode::Space => write!(f, "<space>"),
            HNode::HKern(d) => write!(f, "<hkern:{}>",d),
            HNode::Custom(n) => n.readable_fmt(indent, f)
        }
    }
    fn height(&self) -> ET::Dim {
        match self {
            HNode::Box(b) => b.height(),
            HNode::VRule { height, .. } => height.unwrap_or_default(),
            HNode::Char { char,font } => font.get_ht(*char),
            HNode::Leaders(l) => l.height(),
            HNode::MathGroup(mg) => mg.height(),
            HNode::Custom(n) => n.height(),
            _ => ET::Dim::default(),
        }
    }
    fn width(&self) -> ET::Dim {
        match self {
            HNode::Box(b) => b.width(),
            HNode::Char { char,font } => font.get_wd(*char),
            HNode::VRule { width, .. } => width.unwrap_or(ET::Dim::from_sp(26214)),
            HNode::Leaders(l) => l.width(),
            HNode::MathGroup(mg) => mg.width(),
            HNode::Custom(n) => n.width(),
            HNode::HKern(d) => *d,
            HNode::HSkip(s) => s.base(),
            HNode::Space => ET::Dim::from_sp(65536 * 5), // TODO heuristic; use spacefactor instead
            _=> ET::Dim::default(),
        }
    }
    fn depth(&self) -> ET::Dim {
        match self {
            HNode::Box(b) => b.depth(),
            HNode::Char { char,font } => font.get_dp(*char),
            HNode::VRule { depth, .. } => depth.unwrap_or_default(),
            HNode::Leaders(l) => l.depth(),
            HNode::MathGroup(mg) => mg.depth(),
            HNode::Custom(n) => n.depth(),
            _ => ET::Dim::default(),
        }
    }
    fn nodetype(&self) -> NodeType {
        match self {
            HNode::Penalty(_) => NodeType::Penalty,
            HNode::VRule {..} => NodeType::Rule,
            HNode::Box(b) => b.nodetype(),
            HNode::Char { .. } => NodeType::Char,
            HNode::HKern(_) => NodeType::Kern,
            HNode::Insert(..) => NodeType::Insertion,
            HNode::VAdjust(_) => NodeType::Adjust,
            HNode::MathGroup{..} => NodeType::Math,
            HNode::Mark(_, _) => NodeType::Mark,
            HNode::Whatsit(_) => NodeType::WhatsIt,
            HNode::Leaders(_) => NodeType::Glue,
            HNode::HSkip(_) | HNode::Space | HNode::HFil | HNode::HFill | HNode::HFilneg | HNode::Hss => NodeType::Glue,
            HNode::Custom(n) => n.nodetype(),
        }
    }
    fn opaque(&self) -> bool {
        match self {
            HNode::Mark(_, _) => true,
            HNode::Custom(n) => n.opaque(),
            _ => false
        }
    }
}