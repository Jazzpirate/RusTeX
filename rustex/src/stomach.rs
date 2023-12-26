use tex_engine::commands::CommandScope;
use tex_engine::engine::{EngineAux, EngineReferences, EngineTypes};
use tex_engine::engine::filesystem::{File, SourceReference};
use tex_engine::engine::fontsystem::FontSystem;
use tex_engine::engine::state::State;
use tex_engine::engine::stomach::{insert_afterassignment, ParLine, ParLineSpec, split_paragraph_roughly, SplitResult, Stomach, StomachData};
use tex_engine::engine::utils::memory::MemoryManager;
use tex_engine::tex::nodes::{BoxTarget, HorizontalNodeListType, NodeList, VerticalNodeListType};
use tex_engine::tex::numerics::Dim32;
use tex_engine::tex::token::CompactToken;
use tex_engine::tex::types::{BoxType, GroupType};
use crate::engine::{Font, Refs, Types};
use crate::extension::FontChange;
use crate::nodes::{LineSkip, RusTeXNode};
use crate::state::RusTeXState;
use tex_engine::engine::mouth::Mouth;
use tex_engine::tex::nodes::boxes::TeXBox;
use tex_engine::tex::nodes::horizontal::HNode;
use tex_engine::tex::nodes::math::{MathAtom, MathNode, MathNucleus};
use tex_engine::tex::nodes::NodeTrait;
use tex_engine::tex::nodes::vertical::VNode;
use crate::shipout::ZERO;

pub struct RusTeXStomach {
    afterassignment:Option<CompactToken>,
    data:StomachData<Types>,
    prevent_shipout:bool
}
impl Stomach for RusTeXStomach {
    type ET = Types;
    #[inline(always)]
    fn new(_aux: &mut EngineAux<Types>, _state: &mut RusTeXState) -> Self {
        Self { afterassignment:None, data:StomachData::new(), prevent_shipout:false }
    }
    #[inline(always)]
    fn afterassignment(&mut self) -> &mut Option<CompactToken> {
        &mut self.afterassignment
    }
    #[inline(always)]
    fn data_mut(&mut self) -> &mut StomachData<Types> {
        &mut self.data
    }
    #[inline(always)]
    fn split_vertical(engine: Refs, nodes: Vec<VNode<Types>>, target: Dim32) -> SplitResult<Types> {
        vsplit(engine, nodes, target)
    }

    fn do_font(engine: Refs, _token: CompactToken, f: Font, global: bool) {
        let g = global || engine.aux.extension.change_markers.len() == 0;
        Self::add_node(engine,
                       || VNode::Custom(RusTeXNode::FontChange(f.clone(),g)),
                       || HNode::Custom(RusTeXNode::FontChange(f.clone(),g)),
                       || MathNode::Custom(RusTeXNode::FontChange(f.clone(),g))
        );
        if !g {
            engine.aux.extension.change_markers.last_mut().unwrap().push(FontChange(f.clone()));
        }
        engine.state.set_current_font(engine.aux,f,global);
        insert_afterassignment(engine);
    }

    fn end_box(engine:&mut EngineReferences<Self::ET>,bt:BoxType) {
        match engine.stomach.data_mut().open_lists.last() {
            Some(NodeList::Horizontal{tp:HorizontalNodeListType::Paragraph(_),..}) => Self::close_paragraph(engine),
            _ => ()
        }
        match engine.stomach.data_mut().open_lists.pop() {
            Some(NodeList::Vertical {mut children,tp:VerticalNodeListType::VAdjust}) if bt == BoxType::Vertical => {
                for _ in engine.aux.extension.change_markers.last_mut().unwrap().drain(..) {
                    children.push(VNode::Custom(RusTeXNode::FontChangeEnd));
                }
                engine.state.pop(engine.aux,engine.mouth);
                Self::add_node_h(engine,HNode::VAdjust(children.into()))
            }
            Some(NodeList::Vertical {mut children,tp:VerticalNodeListType::Box(info,start,target)})  if bt == BoxType::Vertical => {
                for _ in engine.aux.extension.change_markers.last_mut().unwrap().drain(..) {
                    children.push(VNode::Custom(RusTeXNode::FontChangeEnd));
                }
                engine.state.pop(engine.aux,engine.mouth);
                let bx = TeXBox::V {
                    children:children.into(),info,start,end:engine.mouth.current_sourceref(),
                };
                Self::add_box(engine,bx,target)
            }
            Some(NodeList::Vertical {mut children,tp:VerticalNodeListType::VCenter(start)}) if bt == BoxType::Vertical => {
                for _ in engine.aux.extension.change_markers.last_mut().unwrap().drain(..) {
                    children.push(VNode::Custom(RusTeXNode::FontChangeEnd));
                }
                engine.state.pop(engine.aux,engine.mouth);
                Self::add_node_m(engine,MathNode::Atom(MathAtom {
                    nucleus: MathNucleus::VCenter {children:children.into(),start,end:engine.mouth.current_sourceref()},
                    sub:None,sup:None
                }))
            }
            Some(NodeList::Horizontal {mut children,tp:HorizontalNodeListType::Box(info,start,target)}) if bt == BoxType::Horizontal => {
                for _ in engine.aux.extension.change_markers.last_mut().unwrap().drain(..) {
                    children.push(HNode::Custom(RusTeXNode::FontChangeEnd));
                }
                engine.state.pop(engine.aux,engine.mouth);
                let bx = TeXBox::H {
                    children:children.into(),info,start,end:engine.mouth.current_sourceref(),
                };
                Self::add_box(engine,bx,target)
            }
            o => todo!("throw error: {:?}",o),
        }
    }

    fn split_paragraph(engine: Refs, specs: Vec<ParLineSpec<Types>>, children: Vec<HNode<Types>>, sourceref: SourceReference<<<Self::ET as EngineTypes>::File as File>::SourceRefID>) {
        if children.is_empty() { return }
        let ret = split_paragraph_roughly(engine,specs.clone(),children,sourceref.clone());
        engine.stomach.prevent_shipout = true;
        Self::add_node_v(engine,VNode::Custom(RusTeXNode::ParagraphBegin{specs,start:sourceref,end:engine.mouth.current_sourceref(),lineskip:LineSkip::get(engine.state)}));
        for line in ret {
            match line {
                ParLine::Adjust(n) => Self::add_node_v(engine,n),
                ParLine::Line(bx) => Self::add_node_v(engine,VNode::Box(bx))
            }
        }
        engine.stomach.prevent_shipout = false;
        Self::add_node_v(engine,VNode::Custom(RusTeXNode::ParagraphEnd));
    }

    fn maybe_shipout(engine:&mut EngineReferences<Self::ET>) {
        if engine.stomach.prevent_shipout { return }
        let data = engine.stomach.data_mut();
        if !data.in_output && data.open_lists.is_empty() && data.pagetotal >= data.pagegoal && !data.page.is_empty() {
            Self::do_shipout_output(engine, None)
        }
    }

    fn open_align(engine: Refs, _inner: BoxType, between: BoxType) {
        Self::add_node(engine,|| VNode::Custom(RusTeXNode::HAlignBegin), || HNode::Custom(RusTeXNode::HAlignBegin), || unreachable!());
        engine.state.push(engine.aux,GroupType::Box(between),engine.mouth.line_number());
        engine.stomach.data_mut().open_lists.push(
            if between == BoxType::Vertical {
                NodeList::Vertical {
                    tp: VerticalNodeListType::HAlign,
                    children: vec!()
                }} else {
                NodeList::Horizontal {
                    tp: HorizontalNodeListType::VAlign,
                    children: vec!()
                }
            });
    }
    fn close_align(engine: &mut EngineReferences<Self::ET>) {
        match engine.stomach.data_mut().open_lists.pop() {
            Some(NodeList::Vertical{children,tp:VerticalNodeListType::HAlign}) => {
                engine.state.pop(engine.aux,&mut engine.mouth);
                for c in children {
                    Self::add_node_v(engine, c);
                }
            }
            Some(NodeList::Horizontal{children,tp:HorizontalNodeListType::VAlign}) => {
                engine.state.pop(engine.aux,&mut engine.mouth);
                for c in children {
                    Self::add_node_h(engine, c);
                }
            }
            _ => todo!("throw error")
        };
        Self::add_node(engine,|| VNode::Custom(RusTeXNode::HAlignEnd), || HNode::Custom(RusTeXNode::HAlignEnd), || unreachable!());
    }
}

pub(crate) const CLOSE_FONT:&str = "!\"$%&/(closefont)\\&%$\"!";
pub(crate) fn close_font(engine: Refs, _token: CompactToken) {
    match engine.stomach.data.open_lists.last_mut() {
        Some(NodeList::Vertical{ref mut children,..}) => {
            match children.last() {
                Some(VNode::Custom(RusTeXNode::FontChange(_,_))) => {
                    children.pop();
                }
                _ => children.push(VNode::Custom(RusTeXNode::FontChangeEnd))
            }
        },
        Some(NodeList::Horizontal{ref mut children,..}) => {
            match children.last() {
                Some(HNode::Custom(RusTeXNode::FontChange(_,_))) => {
                    children.pop();
                }
                _ => children.push(HNode::Custom(RusTeXNode::FontChangeEnd))
            }
        },
        Some(NodeList::Math{ref mut children,..}) => {
            match children.last() {
                Some(MathNode::Custom(RusTeXNode::FontChange(_,_))) => {
                    children.pop();
                }
                _ => children.push(MathNode::Custom(RusTeXNode::FontChangeEnd))
            }
        },
        _ => engine.stomach.data.page.push(VNode::Custom(RusTeXNode::FontChangeEnd))
    }
}


pub fn vsplit(engine: Refs, mut nodes: Vec<VNode<Types>>, mut target: Dim32) -> SplitResult<Types> {
    let data = engine.stomach.data_mut();
    data.topmarks.clear();
    std::mem::swap(&mut data.botmarks,&mut data.topmarks);
    data.firstmarks.clear();
    data.splitfirstmarks.clear();
    data.splitbotmarks.clear();
    let mut in_par = None;
    let mut split = nodes.len();
    let iter = nodes.iter().enumerate();
    for (i,n) in iter {
        match n {
            VNode::Custom(r@RusTeXNode::ParagraphBegin{..}) => {
                in_par = Some(r.clone());
            }
            VNode::Mark(i, v) => {
                if !data.firstmarks.contains_key(&i) {
                    data.firstmarks.insert(*i,v.clone());
                }
                data.botmarks.insert(*i,v.clone());
            }
            _ => {
                target = target + (-n.height()); // - n.depth() ?
                if target < ZERO {
                    split = i;
                    break
                }
            }
        }
    };
    let mut rest = nodes.split_off(split);
    if let Some(b) = in_par {
        rest.insert(0,VNode::Custom(b));
        nodes.push(VNode::Custom(RusTeXNode::ParagraphEnd));
    }
    let split_penalty = match rest.first() {
        Some(VNode::Penalty(p)) => {
            let p = *p;
            rest.remove(0);
            Some(p)
        }
        _ => None
    };

    for n in &rest {
        match n {
            VNode::Mark(i, v) => {
                if !data.splitfirstmarks.contains_key(&i) {
                    data.splitfirstmarks.insert(*i,v.clone());
                }
                data.splitbotmarks.insert(*i,v.clone());
            }
            _ => ()
        }
    }
    SplitResult{first:nodes,rest,split_penalty}
}