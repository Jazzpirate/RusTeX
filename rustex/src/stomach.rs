use tex_engine::commands::NodeCommandScope;
use tex_engine::engine::{EngineAux, EngineReferences, EngineTypes};
use tex_engine::engine::filesystem::{File, SourceReference};
use tex_engine::engine::fontsystem::FontSystem;
use tex_engine::engine::state::State;
use tex_engine::engine::stomach::{insert_afterassignment, ParLine, ParLineSpec, split_paragraph_roughly, SplitResult, Stomach, StomachData};
use tex_engine::engine::utils::memory::MemoryManager;
use tex_engine::tex::nodes::{BoxInfo, BoxTarget, NodeList, NodeListType, TeXBox, TeXNode, ToOrSpread};
use tex_engine::tex::numerics::Dim32;
use tex_engine::tex::token::CompactToken;
use tex_engine::tex::types::{BoxType, GroupType};
use crate::engine::{Font, Refs, Types};
use crate::extension::FontChange;
use crate::nodes::{LineSkip, RusTeXNode};
use crate::state::RusTeXState;
use tex_engine::engine::mouth::Mouth;
use tex_engine::tex::nodes::NodeTrait;
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
    fn split_vertical(engine: Refs, nodes: Vec<TeXNode<Types>>, target: Dim32) -> SplitResult<Types> {
        vsplit(engine, nodes, target)
    }

    fn do_font(engine: Refs, _token: CompactToken, f: Font, global: bool) {
        let g = global || engine.aux.extension.change_markers.len() == 0;
        Self::add_node(engine,TeXNode::Custom(RusTeXNode::FontChange(f.clone(),g)));
        if !g {
            engine.aux.extension.change_markers.last_mut().unwrap().push(FontChange(f.clone()));
        }
        engine.state.set_current_font(engine.aux,f,global);
        insert_afterassignment(engine);
    }

    fn end_box(engine:&mut EngineReferences<Self::ET>,bt:BoxType) {
        match engine.stomach.data_mut().open_lists.last() {
            Some(NodeList{tp:NodeListType::Paragraph(_),..}) => Self::close_paragraph(engine),
            _ => ()
        }
        match engine.stomach.data_mut().open_lists.pop() {
            Some(mut ls) => match ls.tp {
                NodeListType::Paragraph(_) => { unreachable!() }
                NodeListType::Box(bi,start,reg) if bi.tp == bt => {
                    for _ in engine.aux.extension.change_markers.last_mut().unwrap().drain(..) {
                        ls.children.push(TeXNode::Custom(RusTeXNode::FontChangeEnd));
                    }
                    engine.state.pop(engine.aux,engine.mouth);
                    let bx = TeXBox {
                        children:ls.children,info:bi,start,end:engine.mouth.current_sourceref(),
                    };
                    match reg {
                        BoxTarget::Register { index, globally } =>
                            engine.state.set_box_register(engine.aux,index,Some(bx),globally),
                        BoxTarget::List => Self::add_node(engine,bx.as_node()),
                        BoxTarget::Out => todo!()
                    }
                }
                _ => todo!("throw error")
            }
            None => todo!("throw error"),
        }
    }

    fn split_paragraph(engine: Refs, specs: Vec<ParLineSpec<Types>>, children: Vec<TeXNode<Types>>, sourceref: SourceReference<<<Self::ET as EngineTypes>::File as File>::SourceRefID>) {
        if children.is_empty() { return }
        let ret = split_paragraph_roughly(engine,specs.clone(),children);
        engine.stomach.prevent_shipout = true;
        Self::add_node(engine,TeXNode::Custom(RusTeXNode::ParagraphBegin{specs,start:sourceref,end:engine.mouth.current_sourceref(),lineskip:LineSkip::get(engine.state)}));
        for line in ret {
            match line {
                ParLine::Adjust(n) => {
                    todo!()
                }
                ParLine::Line{mut contents,broken_early} => {
                    if broken_early {
                        contents.push(TeXNode::Custom(RusTeXNode::Br));
                    }
                    Self::add_node(engine, TeXBox {
                           children: contents,
                           info: BoxInfo {
                               tp: BoxType::Horizontal,
                               kind: "parline",
                               scaled: ToOrSpread::None,
                               assigned_width: None,
                               assigned_height: None,
                               assigned_depth: None,
                               moved_left: None,
                               raised: None
                           },
                           start: sourceref,
                           end: engine.mouth.current_sourceref(),
                       }.as_node()
                    )
                }
            }
        }
        engine.stomach.prevent_shipout = false;
        Self::add_node(engine,TeXNode::Custom(RusTeXNode::ParagraphEnd));
    }

    fn maybe_shipout(engine:&mut EngineReferences<Self::ET>) {
        if engine.stomach.prevent_shipout { return }
        let data = engine.stomach.data_mut();
        if !data.in_output && data.open_lists.is_empty() && data.pagetotal >= data.pagegoal && !data.page.is_empty() {
            Self::do_shipout_output(engine, None)
        }
    }

    fn open_align(engine: Refs, _inner: BoxType, between: BoxType) {
        Self::add_node(engine,TeXNode::Custom(RusTeXNode::HAlignBegin));
        engine.state.push(engine.aux,GroupType::Box(between),engine.mouth.line_number());
        engine.stomach.data_mut().open_lists.push(NodeList {
            tp:NodeListType::Align,
            children:vec!(),
        });
    }
    fn close_align(engine: &mut EngineReferences<Self::ET>) {
        let children = match engine.stomach.data_mut().open_lists.pop() {
            Some(NodeList{children,tp:NodeListType::Align}) => children,
            _ => todo!("throw error")
        };
        engine.state.pop(engine.aux,&mut engine.mouth);
        for c in children {
            Self::add_node(engine,c);
        }
        Self::add_node(engine,TeXNode::Custom(RusTeXNode::HAlignEnd));
    }
}

pub(crate) const CLOSE_FONT:&str = "!\"$%&/(closefont)\\&%$\"!";
pub(crate) fn close_font(engine: Refs, _token: CompactToken) {
    match engine.stomach.data.open_lists.last_mut() {
        Some(NodeList{ref mut children,..}) => {
            match children.last() {
                Some(TeXNode::Custom(RusTeXNode::FontChange(_,_))) => {
                    children.pop();
                }
                _ => children.push(TeXNode::Custom(RusTeXNode::FontChangeEnd))
            }
        },
        _ => engine.stomach.data.page.push(TeXNode::Custom(RusTeXNode::FontChangeEnd))
    }
}


pub fn vsplit(engine: Refs, mut nodes: Vec<TeXNode<Types>>, mut target: Dim32) -> SplitResult<Types> {
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
            TeXNode::Custom(r@RusTeXNode::ParagraphBegin{..}) => {
                in_par = Some(r.clone());
            }
            TeXNode::Mark(i, v) => {
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
        rest.insert(0,TeXNode::Custom(b));
        nodes.push(TeXNode::Custom(RusTeXNode::ParagraphEnd));
    }
    let split_penalty = match rest.first() {
        Some(TeXNode::Penalty(p)) => {
            let p = *p;
            rest.remove(0);
            Some(p)
        }
        _ => None
    };

    for n in &rest {
        match n {
            TeXNode::Mark(i, v) => {
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