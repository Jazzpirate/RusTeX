/*! Nodes allowed in horizontal lists. */
use crate::engine::EngineTypes;
use crate::engine::filesystem::{File, SourceRef, SourceReference};
use crate::engine::fontsystem::{Font, FontSystem};
use crate::tex::tokens::token_lists::TokenList;
use crate::tex::nodes::{BoxTarget, display_do_indent, Leaders, NodeTrait, NodeType, WhatsitNode};
use crate::tex::nodes::boxes::{HBoxInfo, TeXBox};
use crate::tex::nodes::math::MathGroup;
use crate::tex::nodes::vertical::VNode;
use crate::tex::characters::Character;
use crate::tex::numerics::TeXDimen;
use crate::tex::numerics::Skip;

/// A horizontal list node.
#[derive(Clone,Debug)]
pub enum HNode<ET:EngineTypes> {
    /// A penalty node, as produced by `\penalty`.
    Penalty(i32),
    /// A mark node, as produced by `\mark`.
    Mark(usize,TokenList<ET::Token>),
    /// A whatsit node, as produced by `\special`, `\write`, etc.
    Whatsit(WhatsitNode<ET>),
    /// A glue node, as produced by `\hskip`.
    HSkip(Skip<ET::Dim>),
    /// A glue node, as produced by `\hfil`.
    HFil,
    /// A glue node, as produced by `\hfill`.
    HFill,
    /// A glue node, as produced by `\hfilneg`.
    HFilneg,
    /// A glue node, as produced by `\hss`.
    Hss,
    /// A glue node, as produced by a space character.
    Space,
    /// A kern node, as produced by `\kern`.
    HKern(ET::Dim),
    /// Leaders, as produced by `\leaders` or `\cleaders` or `\xleaders`.
    Leaders(Leaders<ET>),
    /// A box node, as produced by `\hbox`, `\vbox`, `\vtop`, etc.
    Box(TeXBox<ET>),
    /// A rule node, as produced by `\vrule`.
    VRule{
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
    /// A vadjust node, as produced by `\vadjust`; its contents will migrate to the surrounding vertical list eventually.
    VAdjust(Box<[VNode<ET>]>),
    /// A math list, as produced by `$...$` or `$$...$$`.
    MathGroup(MathGroup<ET>),
    /// A character node, as produced by a character.
    Char {
        /// The character.
        char:ET::Char,
        /// The current font
        font:<ET::FontSystem as FontSystem>::Font
    },
    /// A custom node.
    Custom(ET::CustomNode),
}

impl<ET:EngineTypes> NodeTrait<ET> for HNode<ET> {
    fn display_fmt(&self, indent: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            HNode::Penalty(p) => {
                display_do_indent(indent,f)?;
                write!(f, "<penalty:{}>",p)
            },
            HNode::Leaders(l) => l.display_fmt(indent, f),
            HNode::Box(b) => b.display_fmt(indent, f),
            HNode::Mark(i, _) => {
                display_do_indent(indent,f)?;
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
                display_do_indent(indent,f)?;
                write!(f,"<insert {}>",n)?;
                for c in ch.iter() {
                    c.display_fmt(indent + 2, f)?;
                }
                display_do_indent(indent,f)?;
                write!(f,"</insert>")
            },
            HNode::VAdjust(ls) => {
                display_do_indent(indent,f)?;
                f.write_str("<vadjust>")?;
                for c in ls.iter() {
                    c.display_fmt(indent+2, f)?;
                }
                display_do_indent(indent,f)?;
                f.write_str("</vadjust>")
            },
            HNode::MathGroup(mg) => {
                mg.display_fmt(indent, f)
            }
            HNode::Char { char, .. } =>
                Ok(char.display_fmt(f)),
            HNode::Whatsit(w) => {
                display_do_indent(indent,f)?;
                write!(f, "{:?}",w)
            }
            HNode::HSkip(s) => write!(f, "<hskip:{}>",s),
            HNode::HFil => write!(f, "<hfil>"),
            HNode::HFill => write!(f, "<hfill>"),
            HNode::HFilneg => write!(f, "<hfilneg>"),
            HNode::Hss => write!(f, "<hss>"),
            HNode::Space => write!(f, "<space>"),
            HNode::HKern(d) => write!(f, "<hkern:{}>",d),
            HNode::Custom(n) => n.display_fmt(indent, f)
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
            HNode::HSkip(s) => s.base,
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

    fn sourceref(&self) -> Option<(&SourceRef<ET>, &SourceRef<ET>)> {
        match self {
            HNode::VRule { start, end, .. } => Some((start, end)),
            HNode::Box(b) => b.sourceref(),
            HNode::MathGroup(mg) => mg.sourceref(),
            _ => None
        }
    }
}

/// The kinds of horizontal lists that can occur.
/// TODO: rethink this
#[derive(Clone,Debug)]
pub enum HorizontalNodeListType<ET:EngineTypes> {
    /// A paragraph; will ultimately be broken into lines.
    Paragraph(SourceReference<<ET::File as File>::SourceRefID>),
    /// A horizontal box.
    Box(HBoxInfo<ET>,SourceRef<ET>,BoxTarget<ET>),
    /// A `\valign` list
    VAlign,
    /// A row in an `\halign`. The source ref indicates the start of the row.
    HAlignRow(SourceRef<ET>),
    /// A cell in an `\halign`. The source ref indicates the start of the cell.
    /// The `u8` indicates the number of *additional* columns spanned by this cell
    /// (so by default 0).
    HAlignCell(SourceRef<ET>,u8),
}