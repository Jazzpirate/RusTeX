use std::fmt::{Display, Formatter};
use crate::engine::EngineTypes;
use crate::engine::filesystem::SourceRef;
use crate::engine::stomach::{ParLineSpec, Stomach};
use crate::tex::nodes::{BoxTarget, HorizontalNodeListType, NodeList, NodeTrait, VerticalNodeListType};
use crate::tex::nodes::horizontal::HNode;
use crate::tex::nodes::vertical::VNode;
use crate::tex::numerics::TeXDimen;
use crate::tex::types::NodeType;
use crate::tex::numerics::Skip;

#[derive(Debug,Clone,PartialEq,Eq)]
pub enum ToOrSpread<D:TeXDimen> {
    None,To(D),Spread(D)
}


#[derive(Debug,Clone,PartialEq,Eq)]
pub enum HBoxInfo<ET:EngineTypes> {
    HBox {
        scaled:ToOrSpread<ET::Dim>,
        assigned_width:Option<ET::Dim>,
        assigned_height:Option<ET::Dim>,
        assigned_depth:Option<ET::Dim>,
        moved_left:Option<ET::Dim>,
        raised:Option<ET::Dim>
    },
    ParLine {
        spec:ParLineSpec<ET>,
        ends_with_line_break:bool,
    },
    HAlignRow, HAlignCell {
        to: Option<ET::Dim>,
    },
    ParIndent(ET::Dim),
}
impl<ET:EngineTypes> Display for HBoxInfo<ET> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use HBoxInfo::*;
        match self {
            HBox {..} => write!(f, "hbox"),
            ParLine { .. } => write!(f, "parline"),
            ParIndent(_) => write!(f, "parindent"),
            HAlignRow => write!(f, "halignrow"),
            HAlignCell { .. } => write!(f, "haligncell"),
        }
    }
}
impl<ET:EngineTypes> HBoxInfo<ET> {
    pub fn open_list(self,start:SourceRef<ET>) -> NodeList<ET> {
        match self {
            HBoxInfo::HBox {..} => NodeList::Horizontal {tp:HorizontalNodeListType::Box(self,start,BoxTarget::none()),children:vec!()},
            HBoxInfo::ParLine {..} => NodeList::Horizontal {tp:HorizontalNodeListType::Paragraph(start),children:vec!()},
            HBoxInfo::HAlignRow => NodeList::Horizontal {tp:HorizontalNodeListType::HAlignRow(start),children:vec!()},
            HBoxInfo::HAlignCell {..} => NodeList::Horizontal {tp:HorizontalNodeListType::HAlignCell(start),children:vec!()},
            HBoxInfo::ParIndent(_) => unreachable!(),
        }
    }
    #[inline(always)]
    fn height_inner(v:&[HNode<ET>]) -> ET::Dim { v.iter().map(|c| c.height()).max().unwrap_or_default() }
    #[inline(always)]
    fn depth_inner(v:&[HNode<ET>]) -> ET::Dim { v.iter().map(|c| c.depth()).max().unwrap_or_default() }
    #[inline(always)]
    fn width_inner(v:&[HNode<ET>]) -> ET::Dim { v.iter().map(|c| c.width()).sum() }

    fn get_height(&self,v:&[HNode<ET>]) -> ET::Dim {
        match self {
            HBoxInfo::HBox { assigned_height, .. } => assigned_height.unwrap_or_else(|| Self::height_inner(v)),
            HBoxInfo::ParLine { .. } => Self::height_inner(v),
            HBoxInfo::HAlignRow => Self::height_inner(v),
            HBoxInfo::HAlignCell { .. } => Self::height_inner(v),
            HBoxInfo::ParIndent(_) => ET::Dim::default(),
        }
    }
    fn get_width(&self,v:&[HNode<ET>]) -> ET::Dim {
        match self {
            HBoxInfo::HBox { assigned_width, .. } => assigned_width.unwrap_or_else(|| Self::width_inner(v)),
            HBoxInfo::ParLine { spec,.. } => spec.leftskip.base() + spec.rightskip.base() + spec.target,
            HBoxInfo::HAlignRow => Self::width_inner(v),
            HBoxInfo::HAlignCell { to } => to.unwrap_or_else(|| Self::width_inner(v)),
            HBoxInfo::ParIndent(d) => *d,
        }
    }
    fn get_depth(&self,v:&[HNode<ET>]) -> ET::Dim {
        match self {
            HBoxInfo::HBox { assigned_depth, .. } => assigned_depth.unwrap_or_else(|| Self::depth_inner(v)),
            HBoxInfo::ParLine { .. } => Self::depth_inner(v),
            HBoxInfo::HAlignRow => Self::depth_inner(v),
            HBoxInfo::HAlignCell { .. } => Self::depth_inner(v),
            HBoxInfo::ParIndent(_) => ET::Dim::default(),
        }
    }

    pub fn raise(&mut self,d:ET::Dim) {
        match self {
            HBoxInfo::HBox { ref mut raised, .. } => *raised = Some(d),
            _ => todo!()
        }
    }
    pub fn move_left(&mut self,d:ET::Dim) {
        match self {
            HBoxInfo::HBox { ref mut moved_left, .. } => *moved_left = Some(d),
            _ => todo!()
        }
    }
}

#[derive(Debug,Clone,PartialEq,Eq)]
pub enum VBoxInfo<ET:EngineTypes> {
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
    },Output
}
impl<ET:EngineTypes> Display for VBoxInfo<ET> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use VBoxInfo::*;
        match self {
            VBox { .. } => write!(f, "vbox"),
            VTop { .. } => write!(f, "vtop"),
            VAlignRow => write!(f, "valignrow"),
            VAlignCell { .. } => write!(f, "valigncell"),
            Output => write!(f, "output"),
        }
    }
}

impl<ET:EngineTypes> VBoxInfo<ET> {
    pub fn open_list(self,start:SourceRef<ET>) -> NodeList<ET> {
        match self {
            VBoxInfo::VBox {..} => NodeList::Vertical {tp:VerticalNodeListType::Box(self,start,BoxTarget::none()),children:vec!()},
            VBoxInfo::VTop {..} => NodeList::Vertical {tp:VerticalNodeListType::Box(self,start,BoxTarget::none()),children:vec!()},
            VBoxInfo::VAlignRow => NodeList::Vertical {tp:VerticalNodeListType::VAlignRow(start),children:vec!()},
            VBoxInfo::VAlignCell {..} => NodeList::Vertical {tp:VerticalNodeListType::VAlignCell(start),children:vec!()},
            VBoxInfo::Output => NodeList::Vertical {tp:VerticalNodeListType::Box(self,start,BoxTarget::none()),children:vec!()},
        }
    }
    pub fn clone_for_split(&mut self) -> Self {
        match self {
            VBoxInfo::VBox { scaled, assigned_width, assigned_height, assigned_depth, moved_left, raised } => {
                *assigned_height = None;
                *assigned_depth = None;
                *scaled = ToOrSpread::None;
                VBoxInfo::VBox {
                    scaled: ToOrSpread::None,
                    assigned_width: assigned_width.clone(),
                    assigned_height: None,
                    assigned_depth: None,
                    moved_left: moved_left.clone(),
                    raised: raised.clone(),
                }
            },
            VBoxInfo::VTop { scaled, assigned_width, assigned_height, assigned_depth, moved_left, raised } => {
                *assigned_height = None;
                *assigned_depth = None;
                *scaled = ToOrSpread::None;
                VBoxInfo::VTop {
                    scaled: ToOrSpread::None,
                    assigned_width: assigned_width.clone(),
                    assigned_height: None,
                    assigned_depth: None,
                    moved_left: moved_left.clone(),
                    raised: raised.clone(),
                }
            },
            VBoxInfo::Output => {
                VBoxInfo::VBox {
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
    fn height_inner(v:&[VNode<ET>]) -> ET::Dim { v.iter().map(|c| c.height() + c.depth()).sum::<ET::Dim>() + -Self::depth_inner(v) }
    #[inline(always)]
    fn depth_inner(v:&[VNode<ET>]) -> ET::Dim { for c in v.iter().rev() {
        if !c.opaque() { return c.depth() }
    } ET::Dim::default() }
    #[inline(always)]
    fn width_inner(v:&[VNode<ET>]) -> ET::Dim { v.iter().map(|c| c.width()).max().unwrap_or_default() }

    fn get_height(&self,v:&[VNode<ET>]) -> ET::Dim {
        match self {
            VBoxInfo::VAlignRow => Self::height_inner(v),
            VBoxInfo::VAlignCell { to } => to.unwrap_or_else(||  Self::height_inner(v)),
            VBoxInfo::VBox { assigned_height, .. } => assigned_height.unwrap_or_else(|| Self::height_inner(v)),
            VBoxInfo::VTop { assigned_height, .. } => assigned_height.unwrap_or_else(|| Self::height_inner(v)), // TODO
            VBoxInfo::Output => Self::height_inner(v),
        }
    }
    fn get_width(&self,v:&[VNode<ET>]) -> ET::Dim {
        match self {
            VBoxInfo::VAlignRow => Self::width_inner(v),
            VBoxInfo::VAlignCell { .. } => Self::width_inner(v),
            VBoxInfo::VBox { assigned_width, .. } => assigned_width.unwrap_or_else(|| Self::width_inner(v)),
            VBoxInfo::VTop { assigned_width, .. } => assigned_width.unwrap_or_else(|| Self::width_inner(v)), // TODO
            VBoxInfo::Output => Self::width_inner(v),
        }
    }
    fn get_depth(&self,v:&[VNode<ET>]) -> ET::Dim {
        match self {
            VBoxInfo::VAlignRow => Self::depth_inner(v),
            VBoxInfo::VAlignCell { .. } => Self::depth_inner(v),
            VBoxInfo::VBox { assigned_depth, .. } => assigned_depth.unwrap_or_else(|| Self::depth_inner(v)),
            VBoxInfo::VTop { assigned_depth, .. } => assigned_depth.unwrap_or_else(|| Self::depth_inner(v)), // TODO
            VBoxInfo::Output => Self::depth_inner(v),
        }
    }

    pub fn raise(&mut self,d:ET::Dim) {
        match self {
            VBoxInfo::VBox { ref mut raised, .. } => *raised = Some(d),
            VBoxInfo::VTop { ref mut raised, .. } => *raised = Some(d),
            _ => todo!()
        }
    }
    pub fn move_left(&mut self,d:ET::Dim) {
        match self {
            VBoxInfo::VBox { ref mut moved_left, .. } => *moved_left = Some(d),
            VBoxInfo::VTop { ref mut moved_left, .. } => *moved_left = Some(d),
            _ => todo!()
        }
    }
}

#[derive(Debug,Clone,PartialEq,Eq)]
pub enum BoxInfo<ET:EngineTypes> {
    H(HBoxInfo<ET>),V(VBoxInfo<ET>)
}
impl<ET:EngineTypes> BoxInfo<ET> {
    pub fn open_list(self,start:SourceRef<ET>) -> NodeList<ET> {
        match self {
            BoxInfo::H(h) => h.open_list(start),
            BoxInfo::V(v) => v.open_list(start),
        }
    }
    pub fn move_left(&mut self,d:ET::Dim) {
        match self {
            BoxInfo::H(h) => h.move_left(d),
            BoxInfo::V(v) => v.move_left(d),
        }
    }
    pub fn raise(&mut self,d:ET::Dim) {
        match self {
            BoxInfo::H(h) => h.raise(d),
            BoxInfo::V(v) => v.raise(d),
        }
    }
}

#[derive(Debug,Clone)]
pub enum TeXBox<ET:EngineTypes> {
    V { info:VBoxInfo<ET>, children:Box<[VNode<ET>]>, start:SourceRef<ET>,end:SourceRef<ET> },
    H { info:HBoxInfo<ET>, children:Box<[HNode<ET>]>, start:SourceRef<ET>,end:SourceRef<ET> },
}

impl<ET:EngineTypes> TeXBox<ET> {
    pub fn is_empty(&self) -> bool {
        match self {
            TeXBox::H { children, .. } => children.is_empty(),
            TeXBox::V { children, .. } => children.is_empty(),
        }
    }
    pub fn assign_height(&mut self, h:ET::Dim) {
        match self {
            TeXBox::H {info: HBoxInfo::HBox { ref mut assigned_height, .. },..} => *assigned_height = Some(h),
            TeXBox::V{ info: VBoxInfo::VBox { ref mut assigned_height, .. },..} => *assigned_height = Some(h),
            TeXBox::V{ info: VBoxInfo::VTop { ref mut assigned_height, .. },..} => *assigned_height = Some(h),
            _ => todo!()
        }
    }
    pub fn assigned_height(&self) -> Option<ET::Dim> {
        match self {
            TeXBox::H {info: HBoxInfo::HBox { assigned_height, .. },..} => *assigned_height,
            TeXBox::V{ info: VBoxInfo::VBox { assigned_height, .. },..} => *assigned_height,
            TeXBox::V{ info: VBoxInfo::VTop { assigned_height, .. },..} => *assigned_height,
            _ => None
        }
    }
    pub fn assign_width(&mut self, w:ET::Dim) {
        match self {
            TeXBox::H{ info: HBoxInfo::HBox { ref mut assigned_width, .. },..} => *assigned_width = Some(w),
            TeXBox::V{ info: VBoxInfo::VBox { ref mut assigned_width, .. },..} => *assigned_width = Some(w),
            TeXBox::V{ info: VBoxInfo::VTop { ref mut assigned_width, .. },..} => *assigned_width = Some(w),
            _ => todo!()
        }
    }
    pub fn assigned_width(&self) -> Option<ET::Dim> {
        match self {
            TeXBox::H {info: HBoxInfo::HBox { assigned_width, .. },..} => *assigned_width,
            TeXBox::V{ info: VBoxInfo::VBox { assigned_width, .. },..} => *assigned_width,
            TeXBox::V{ info: VBoxInfo::VTop { assigned_width, .. },..} => *assigned_width,
            _ => None
        }
    }
    pub fn assign_depth(&mut self, d:ET::Dim) {
        match self {
            TeXBox::H{ info: HBoxInfo::HBox { ref mut assigned_depth, .. },..} => *assigned_depth = Some(d),
            TeXBox::V{ info: VBoxInfo::VBox { ref mut assigned_depth, .. },..} => *assigned_depth = Some(d),
            TeXBox::V{ info: VBoxInfo::VTop { ref mut assigned_depth, .. },..} => *assigned_depth = Some(d),
            _ => todo!()
        }
    }
    pub fn assigned_depth(&self) -> Option<ET::Dim> {
        match self {
            TeXBox::H {info: HBoxInfo::HBox { assigned_depth, .. },..} => *assigned_depth,
            TeXBox::V{ info: VBoxInfo::VBox { assigned_depth, .. },..} => *assigned_depth,
            TeXBox::V{ info: VBoxInfo::VTop { assigned_depth, .. },..} => *assigned_depth,
            _ => None
        }
    }
    pub fn to_or_scaled(&self) -> ToOrSpread<ET::Dim> {
        match self {
            TeXBox::H {info: HBoxInfo::HBox { scaled, .. },..} => scaled.clone(),
            TeXBox::V{ info: VBoxInfo::VBox { scaled, .. },..} => scaled.clone(),
            TeXBox::V{ info: VBoxInfo::VTop { scaled, .. },..} => scaled.clone(),
            _ => ToOrSpread::None
        }
    }
}

impl <ET:EngineTypes> NodeTrait<ET> for TeXBox<ET> {
    fn readable_fmt(&self, indent: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Self::readable_do_indent(indent, f)?;
        match self {
            TeXBox::H { info, children,.. } => {
                write!(f, "<hbox:{}>",info)?;
                for c in children.iter() {
                    c.readable_fmt(indent+2, f)?;
                }
                Self::readable_do_indent(indent, f)?;
                write!(f,"</hbox:{}>",info)
            },
            TeXBox::V { info, children,.. } => {
                write!(f, "<vbox:{}>",info)?;
                for c in children.iter() {
                    c.readable_fmt(indent+2, f)?;
                }
                Self::readable_do_indent(indent, f)?;
                write!(f,"</vbox:{}>",info)
            },
        }
    }
    #[inline(always)]
    fn height(&self) -> ET::Dim {
        match self {
            TeXBox::H { info, children,.. } => info.get_height(&children),
            TeXBox::V { info, children,.. } => info.get_height(&children),
        }
    }
    #[inline(always)]
    fn width(&self) -> ET::Dim {
        match self {
            TeXBox::H { info, children,.. } => info.get_width(&children),
            TeXBox::V { info, children,.. } => info.get_width(&children),
        }
    }
    #[inline(always)]
    fn depth(&self) -> ET::Dim {
        match self {
            TeXBox::H { info, children,.. } => info.get_depth(&children),
            TeXBox::V { info, children,.. } => info.get_depth(&children),
        }
    }
    fn nodetype(&self) -> NodeType { match self{
        TeXBox::H {..} => NodeType::HList,
        TeXBox::V {..} => NodeType::VList,
    }}
}