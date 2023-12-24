use std::cell::RefCell;
use std::fmt::{Debug, Display, Formatter, Write};
use std::marker::PhantomData;
use crate::engine::{EngineReferences, EngineTypes};
use crate::engine::filesystem::SourceReference;
use crate::tex::types::{BoxType, MathClass, NodeType};
use crate::engine::filesystem::File;
use crate::engine::fontsystem::FontSystem;
use crate::engine::mouth::pretokenized::TokenList;
use crate::engine::stomach::ParLineSpec;
use crate::engine::utils::memory::{PrimitiveIdentifier, PRIMITIVES};
use crate::tex::input_text::Character;
use crate::tex::numerics::{MuSkip, Skip, TeXDimen};
use crate::utils::Ptr;
use crate::tex::numerics::NumSet;

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
    Box(BoxInfo<ET>,SR<ET>,BoxTarget),
    Align,
    VAdjust,
    AlignRow(SourceReference<<ET::File as File>::SourceRefID>),
    AlignCell(SourceReference<<ET::File as File>::SourceRefID>),
    Math{start:SourceReference<<ET::File as File>::SourceRefID>,top_display:bool}
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
    VAdjust(Vec<TeXNode<ET>>),
    MathGroup(MathGroup<ET>),
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
            TeXNode::VAdjust(ls) => {
                Self::readable_do_indent(indent,f)?;
                f.write_str("<vadjust>");
                for c in ls {
                    c.readable_fmt(indent+2, f)?;
                }
                Self::readable_do_indent(indent,f)?;
                f.write_str("</vadjust>")
            },
            TeXNode::MathGroup(group) => {
                group.readable_fmt(indent, f)
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
            TeXNode::Insert  => todo!(),
            TeXNode::VAdjust(ls) => ls.iter().map(|c| c.height()).sum(),
            TeXNode::MathGroup(gr) => gr.height(),
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
            TeXNode::Insert => todo!(),
            TeXNode::VAdjust(ls) => ls.iter().map(|c| c.width()).max().unwrap_or_default(),
            TeXNode::MathGroup(gr) => gr.width(),
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
            TeXNode::Insert => todo!(),
            TeXNode::VAdjust(ls) => ls.last().map(|c| c.depth()).unwrap_or_default(),
            TeXNode::MathGroup(gr) => gr.depth(),
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
            TeXNode::VAdjust(_) => NodeType::Adjust,
            TeXNode::MathGroup(_) => NodeType::Math,
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
            TeXNode::VAdjust(_) => false,
            TeXNode::MathGroup(_) => false,
            TeXNode::Whatsit(_) => false,
            TeXNode::Custom(n) => n.opaque(),
        }
    }
    #[inline(always)]
    fn as_node(self) -> TeXNode<ET> { self }
}

#[derive(Debug,Clone)]
pub struct MathGroup<ET:EngineTypes> {
    pub display:bool,
    pub children:Vec<TeXNode<ET>>,
    pub start:SR<ET>,
    pub end:SR<ET>
}
impl<ET:EngineTypes> NodeTrait<ET> for MathGroup<ET> {
    #[inline(always)]
    fn as_node(self) -> TeXNode<ET> { TeXNode::MathGroup(self) }
    fn depth(&self) -> ET::Dim {
        self.children.iter().map(|c| c.depth()).max().unwrap_or_default()
    }
    fn height(&self) -> ET::Dim {
        self.children.iter().map(|c| c.height()).max().unwrap_or_default()
    }
    fn width(&self) -> ET::Dim {
        self.children.iter().map(|c| c.width()).sum()
    }
    #[inline(always)]
    fn nodetype(&self) -> NodeType { NodeType::Math }
    #[inline(always)]
    fn opaque(&self) -> bool { false }
    fn readable_fmt(&self, indent: usize, f: &mut Formatter<'_>) -> std::fmt::Result {
        Self::readable_do_indent(indent,f)?;
        write!(f, "<math>")?;
        for c in &self.children {
            c.readable_fmt(indent+2, f)?;
        }
        Self::readable_do_indent(indent, f)?;
        write!(f, "</math>")
    }
}

#[derive(Debug,Clone)]
pub struct MathChar<ET:EngineTypes> {
    pub char:ET::Char,
    pub font:<ET::FontSystem as FontSystem>::Font,
    pub width:ET::Dim,
    pub height:ET::Dim,
    pub depth:ET::Dim,
    pub cls:MathClass
}
impl<ET:EngineTypes> NodeTrait<ET> for MathChar<ET> {
    fn readable_fmt(&self, indent: usize, f: &mut Formatter<'_>) -> std::fmt::Result {
        Self::readable_do_indent(indent,f)?;
        write!(f, "<mathchar:{}>",self.char)
    }
    fn height(&self) -> ET::Dim { self.height }
    fn width(&self) -> ET::Dim { self.width }
    fn depth(&self) -> ET::Dim { self.depth }
    fn nodetype(&self) -> NodeType { NodeType::MathChar }
    #[inline(always)]
    fn as_node(self) -> TeXNode<ET> { TeXNode::Simple(SimpleNode::MathChar(self)) }
}

#[derive(Clone,Debug)]
pub struct Delimiter<ET:EngineTypes> {
    pub small:MathChar<ET>,
    pub large:MathChar<ET>,
}
impl<ET:EngineTypes> NodeTrait<ET> for Delimiter<ET> {
    fn readable_fmt(&self, indent: usize, f: &mut Formatter<'_>) -> std::fmt::Result {
        Self::readable_do_indent(indent,f)?;
        write!(f, "<delimiter:{}>",self.small.char)
    }
    fn height(&self) -> ET::Dim { self.small.height }
    fn width(&self) -> ET::Dim { self.small.width }
    fn depth(&self) -> ET::Dim { self.small.depth }
    fn nodetype(&self) -> NodeType { NodeType::Math }
    #[inline(always)]
    fn as_node(self) -> TeXNode<ET> { TeXNode::Simple(SimpleNode::Delim(self)) }
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
    },
    MathChar(MathChar<ET>),
    Delim(Delimiter<ET>)
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
            SimpleNode::MathChar(mc) => mc.readable_fmt(indent, f),
            SimpleNode::Delim(d) => d.readable_fmt(indent, f),
        }
    }
    fn height(&self) -> ET::Dim {
        match self {
            SimpleNode::VRule { height, .. } => height.unwrap_or_default(),
            SimpleNode::HRule { height, .. } => height.unwrap_or(ET::Dim::from_sp(26214)),
            SimpleNode::MathChar(mc) => mc.height(),
            SimpleNode::Delim(d) => d.height(),
        }
    }
    fn width(&self) -> ET::Dim {
        match self {
            SimpleNode::VRule { width, .. } => width.unwrap_or(ET::Dim::from_sp(26214)),
            SimpleNode::HRule { width, .. } => width.unwrap_or_default(),
            SimpleNode::MathChar(mc) => mc.width(),
            SimpleNode::Delim(d) => d.width(),
        }
    }
    fn depth(&self) -> ET::Dim {
        match self {
            SimpleNode::VRule { depth, .. } => depth.unwrap_or_default(),
            SimpleNode::HRule { depth, .. } => depth.unwrap_or_default(),
            SimpleNode::MathChar(mc) => mc.depth(),
            SimpleNode::Delim(d) => d.depth(),
        }
    }
    fn nodetype(&self) -> NodeType {
        match self {
            SimpleNode::VRule {..} | SimpleNode::HRule {..} => NodeType::Rule,
            SimpleNode::MathChar(_) => NodeType::MathChar,
            SimpleNode::Delim(_) => NodeType::Math,
        }
    }
    #[inline(always)]
    fn as_node(self) -> TeXNode<ET> { TeXNode::Simple(self) }
}

#[derive(Debug,Clone)]
pub enum KernNode<ET:EngineTypes> {
    VKern(ET::Dim),HKern(ET::Dim),MKern(<ET::MuSkip as MuSkip>::Base,ET::Dim)
}
impl <ET:EngineTypes> KernNode<ET> {
    pub fn dim(&self) -> ET::Dim { match self {
        KernNode::VKern(d) => *d,
        KernNode::HKern(d) => *d,
        KernNode::MKern(b,d) => ET::Num::mudim_to_dim(*b,*d)
    } }
}
impl<ET:EngineTypes> NodeTrait<ET> for KernNode<ET> {
    fn readable_fmt(&self, indent: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Self::readable_do_indent(indent,f)?;
        match self {
            KernNode::VKern(d) => write!(f, "<vkern:{}>",d),
            KernNode::HKern(d) => write!(f, "<hkern:{}>",d),
            KernNode::MKern(b,d) => write!(f, "<mkern:{}>",b),
        }
    }
    fn height(&self) -> ET::Dim { match self {
        KernNode::VKern(d) => *d,
        _ => ET::Dim::default()
    } }
    fn width(&self) -> ET::Dim { match self {
        KernNode::HKern(d) => *d,
        KernNode::MKern(b,d) => ET::Num::mudim_to_dim(*b,*d),
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
    HSkip(ET::Skip),HFil,HFill,HFilneg,Hss,
    MSkip(ET::MuSkip,ET::Dim)
}
impl <ET:EngineTypes> SkipNode<ET> {
    pub fn skip(&self) -> ET::Skip { match self {
        SkipNode::VSkip(s) => *s,
        SkipNode::HSkip(s) => *s,
        SkipNode::MSkip(s,f) => ET::Num::muskip_to_skip(*s,*f),
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
            SkipNode::MSkip(s,_) => write!(f, "<mskip:{}>",s),
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
        SkipNode::MSkip(s,f) => ET::Num::mudim_to_dim(s.base(),*f),
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
pub enum BoxInfo<ET:EngineTypes> {
    HBox {
        scaled:ToOrSpread<ET::Dim>,
        assigned_width:Option<ET::Dim>,
        assigned_height:Option<ET::Dim>,
        assigned_depth:Option<ET::Dim>,
        moved_left:Option<ET::Dim>,
        raised:Option<ET::Dim>,
    },
    ParLine {
        spec:ParLineSpec<ET>,
        ends_with_line_break:bool,
    },
    HAlignRow, HAlignCell {
        to: Option<ET::Dim>,
    },
    ParIndent(ET::Dim),
    VAlignRow,VAlignCell {
        to: Option<ET::Dim>,
    },
    VBox {
        scaled:ToOrSpread<ET::Dim>,
        assigned_width:Option<ET::Dim>,
        assigned_height:Option<ET::Dim>,
        assigned_depth:Option<ET::Dim>,
        moved_left:Option<ET::Dim>,
        raised:Option<ET::Dim>,
    },
    VTop {
        scaled:ToOrSpread<ET::Dim>,
        assigned_width:Option<ET::Dim>,
        assigned_height:Option<ET::Dim>,
        assigned_depth:Option<ET::Dim>,
        moved_left:Option<ET::Dim>,
        raised:Option<ET::Dim>,
    },
    VCenter,Output
}
impl<ET:EngineTypes> Display for BoxInfo<ET> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use BoxInfo::*;
        match self {
            HBox => write!(f, "hbox"),
            ParLine { .. } => write!(f, "parline"),
            HAlignRow => write!(f, "halignrow"),
            HAlignCell { .. } => write!(f, "haligncell"),
            VBox { .. } => write!(f, "vbox"),
            VTop { .. } => write!(f, "vtop"),
            VAlignRow => write!(f, "valignrow"),
            VAlignCell { .. } => write!(f, "valigncell"),
            VCenter => write!(f, "vcenter"),
            Output => write!(f, "output"),
        }
    }
}
impl<ET:EngineTypes> BoxInfo<ET> {
    #[inline(always)]
    pub fn tp(&self) -> BoxType { match self {
        BoxInfo::HBox { .. } | BoxInfo::ParLine { .. } | BoxInfo::HAlignRow | BoxInfo::HAlignCell { .. } => BoxType::Horizontal,
        _ => BoxType::Vertical
    } }
    #[inline(always)]
    pub fn new_cell(tp:BoxType) -> Self {match tp {
        BoxType::Horizontal => BoxInfo::HAlignCell { to: None },
        _ => BoxInfo::HAlignCell { to: None }
    }}
    #[inline(always)]
    pub fn new_row(tp:BoxType) -> Self {match tp {
        BoxType::Horizontal => BoxInfo::HAlignRow,
        _ => BoxInfo::VAlignRow
    }}
    pub fn clone_for_split(&mut self) -> Self {
        match self {
            BoxInfo::VBox { scaled, assigned_width, assigned_height, assigned_depth, moved_left, raised } => {
                *assigned_height = None;
                *assigned_depth = None;
                *scaled = ToOrSpread::None;
                BoxInfo::VBox {
                    scaled: ToOrSpread::None,
                    assigned_width: assigned_width.clone(),
                    assigned_height: None,
                    assigned_depth: None,
                    moved_left: moved_left.clone(),
                    raised: raised.clone(),
                }
            },
            BoxInfo::VTop { scaled, assigned_width, assigned_height, assigned_depth, moved_left, raised } => {
                *assigned_height = None;
                *assigned_depth = None;
                *scaled = ToOrSpread::None;
                BoxInfo::VTop {
                    scaled: ToOrSpread::None,
                    assigned_width: assigned_width.clone(),
                    assigned_height: None,
                    assigned_depth: None,
                    moved_left: moved_left.clone(),
                    raised: raised.clone(),
                }
            },
            BoxInfo::Output => {
                BoxInfo::VBox {
                    scaled: ToOrSpread::None,
                    assigned_width: None,
                    assigned_height: None,
                    assigned_depth: None,
                    moved_left: None,
                    raised: None,
                }
            },
            _ => unreachable!()
        }
    }
    #[inline(always)]
    fn v_height_inner(v:&Vec<TeXNode<ET>>) -> ET::Dim { v.iter().map(|c| c.height()).sum() }
    #[inline(always)]
    fn v_depth_inner(v:&Vec<TeXNode<ET>>) -> ET::Dim { for c in v.iter().rev() {
        if !c.opaque() { return c.depth() }
    } ET::Dim::default() }
    #[inline(always)]
    fn v_width_inner(v:&Vec<TeXNode<ET>>) -> ET::Dim { v.iter().map(|c| c.width()).max().unwrap_or_default() }
    #[inline(always)]
    fn h_height_inner(v:&Vec<TeXNode<ET>>) -> ET::Dim { v.iter().map(|c| c.height()).max().unwrap_or_default() }
    #[inline(always)]
    fn h_depth_inner(v:&Vec<TeXNode<ET>>) -> ET::Dim { v.iter().map(|c| c.depth()).max().unwrap_or_default() }
    #[inline(always)]
    fn h_width_inner(v:&Vec<TeXNode<ET>>) -> ET::Dim { v.iter().map(|c| c.width()).sum() }

    fn get_height(&self,v:&Vec<TeXNode<ET>>) -> ET::Dim {
        match self {
            BoxInfo::HBox { assigned_height, .. } => assigned_height.unwrap_or_else(|| Self::h_height_inner(v)),
            BoxInfo::ParLine { spec,.. } => Self::h_height_inner(v),
            BoxInfo::HAlignRow => Self::h_height_inner(v),
            BoxInfo::HAlignCell { .. } => Self::h_height_inner(v),
            BoxInfo::ParIndent(_) => ET::Dim::default(),
            BoxInfo::VAlignRow => Self::v_height_inner(v),
            BoxInfo::VAlignCell { to } => to.unwrap_or_else(||  Self::v_height_inner(v)),
            BoxInfo::VBox { assigned_height, .. } => assigned_height.unwrap_or_else(|| Self::v_height_inner(v)),
            BoxInfo::VTop { assigned_height, .. } => assigned_height.unwrap_or_else(|| Self::v_height_inner(v)), // TODO
            BoxInfo::VCenter => Self::v_height_inner(v), // TODO
            BoxInfo::Output => Self::v_height_inner(v),
        }
    }
    fn get_width(&self,v:&Vec<TeXNode<ET>>) -> ET::Dim {
        match self {
            BoxInfo::HBox { assigned_width, .. } => assigned_width.unwrap_or_else(|| Self::h_width_inner(v)),
            BoxInfo::ParLine { spec,.. } => spec.leftskip.base() + spec.rightskip.base() + spec.target,
            BoxInfo::HAlignRow => Self::h_width_inner(v),
            BoxInfo::HAlignCell { to } => to.unwrap_or_else(|| Self::h_width_inner(v)),
            BoxInfo::ParIndent(d) => *d,
            BoxInfo::VAlignRow => Self::v_width_inner(v),
            BoxInfo::VAlignCell { .. } => Self::v_width_inner(v),
            BoxInfo::VBox { assigned_width, .. } => assigned_width.unwrap_or_else(|| Self::v_width_inner(v)),
            BoxInfo::VTop { assigned_width, .. } => assigned_width.unwrap_or_else(|| Self::v_width_inner(v)), // TODO
            BoxInfo::VCenter => Self::v_width_inner(v),
            BoxInfo::Output => Self::v_width_inner(v),
        }
    }
    fn get_depth(&self,v:&Vec<TeXNode<ET>>) -> ET::Dim {
        match self {
            BoxInfo::HBox { assigned_depth, .. } => assigned_depth.unwrap_or_else(|| Self::h_depth_inner(v)),
            BoxInfo::ParLine { spec,.. } => Self::h_depth_inner(v),
            BoxInfo::HAlignRow => Self::h_depth_inner(v),
            BoxInfo::HAlignCell { .. } => Self::h_depth_inner(v),
            BoxInfo::ParIndent(_) => ET::Dim::default(),
            BoxInfo::VAlignRow => Self::v_depth_inner(v),
            BoxInfo::VAlignCell { .. } => Self::v_depth_inner(v),
            BoxInfo::VBox { assigned_depth, .. } => assigned_depth.unwrap_or_else(|| Self::v_depth_inner(v)),
            BoxInfo::VTop { assigned_depth, .. } => assigned_depth.unwrap_or_else(|| Self::v_depth_inner(v)), // TODO
            BoxInfo::VCenter => Self::v_depth_inner(v), // TODO
            BoxInfo::Output => Self::v_depth_inner(v),
        }
    }

    pub fn raise(&mut self,d:ET::Dim) {
        match self {
            BoxInfo::HBox { ref mut raised, .. } => *raised = Some(d),
            BoxInfo::VBox { ref mut raised, .. } => *raised = Some(d),
            BoxInfo::VTop { ref mut raised, .. } => *raised = Some(d),
            _ => todo!()
        }
    }
    pub fn move_left(&mut self,d:ET::Dim) {
        match self {
            BoxInfo::HBox { ref mut moved_left, .. } => *moved_left = Some(d),
            BoxInfo::VBox { ref mut moved_left, .. } => *moved_left = Some(d),
            BoxInfo::VTop { ref mut moved_left, .. } => *moved_left = Some(d),
            _ => todo!()
        }
    }
}

/*
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

 */

#[derive(Debug,Clone)]
pub struct TeXBox<ET:EngineTypes> {
    pub children:Vec<TeXNode<ET>>,
    pub info:BoxInfo<ET>,
    pub start:SR<ET>,
    pub end:SR<ET>
}

impl<ET:EngineTypes> TeXBox<ET> {
    pub fn assign_height(&mut self, h:ET::Dim) {
        match self.info {
            BoxInfo::HBox { ref mut assigned_height, .. } => *assigned_height = Some(h),
            BoxInfo::VBox { ref mut assigned_height, .. } => *assigned_height = Some(h),
            BoxInfo::VTop { ref mut assigned_height, .. } => *assigned_height = Some(h),
            _ => todo!()
        }
    }
    pub fn assign_width(&mut self, w:ET::Dim) {
        match self.info {
            BoxInfo::HBox { ref mut assigned_width, .. } => *assigned_width = Some(w),
            BoxInfo::VBox { ref mut assigned_width, .. } => *assigned_width = Some(w),
            BoxInfo::VTop { ref mut assigned_width, .. } => *assigned_width = Some(w),
            _ => todo!()
        }
    }
    pub fn assign_depth(&mut self, d:ET::Dim) {
        match self.info {
            BoxInfo::HBox { ref mut assigned_depth, .. } => *assigned_depth = Some(d),
            BoxInfo::VBox { ref mut assigned_depth, .. } => *assigned_depth = Some(d),
            BoxInfo::VTop { ref mut assigned_depth, .. } => *assigned_depth = Some(d),
            _ => todo!()
        }
    }
}

impl <ET:EngineTypes> NodeTrait<ET> for TeXBox<ET> {
    fn readable_fmt(&self, indent: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Self::readable_do_indent(indent, f)?;
        write!(f,"<{}>",self.info)?;
        for c in &self.children {
            c.readable_fmt(indent+2, f)?;
        }
        Self::readable_do_indent(indent, f)?;
        write!(f,"</{}>",self.info)
    }
    #[inline(always)]
    fn height(&self) -> ET::Dim {
        self.info.get_height(&self.children)
    }
    #[inline(always)]
    fn width(&self) -> ET::Dim {
        self.info.get_width(&self.children)
    }
    #[inline(always)]
    fn depth(&self) -> ET::Dim {
        self.info.get_depth(&self.children)
    }
    fn nodetype(&self) -> NodeType { match self.info.tp() {
        BoxType::Horizontal => NodeType::HList,
        BoxType::Vertical => NodeType::VList,
    }}
    #[inline(always)]
    fn as_node(self) -> TeXNode<ET> { TeXNode::Box(self) }
}