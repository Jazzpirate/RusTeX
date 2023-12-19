use tex_engine::engine::{EngineAux, EngineReferences, EngineTypes};
use tex_engine::engine::filesystem::{File, SourceReference};
use tex_engine::engine::fontsystem::FontSystem;
use tex_engine::engine::state::State;
use tex_engine::engine::stomach::{insert_afterassignment, ParLine, ParLineSpec, split_paragraph_roughly, SplitResult, Stomach, StomachData, vsplit_roughly};
use tex_engine::engine::utils::memory::MemoryManager;
use tex_engine::tex::nodes::{BoxInfo, BoxTarget, NodeList, NodeListType, TeXBox, TeXNode, ToOrSpread};
use tex_engine::tex::numerics::Dim32;
use tex_engine::tex::token::CompactToken;
use tex_engine::tex::types::BoxType;
use crate::engine::{Font, Refs, Types};
use crate::extension::FontChange;
use crate::nodes::RusTeXNode;
use crate::state::RusTeXState;
use tex_engine::engine::mouth::Mouth;
use tex_engine::tex::nodes::NodeTrait;

pub struct RusTeXStomach {
    afterassignment:Option<CompactToken>,
    data:StomachData<Types>,
}
impl Stomach for RusTeXStomach {
    type ET = Types;
    #[inline(always)]
    fn new(_aux: &mut EngineAux<Types>, _state: &mut RusTeXState) -> Self {
        Self { afterassignment:None, data:StomachData::new() }
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
        vsplit_roughly(engine, nodes, target)
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

    fn split_paragraph(engine: &mut EngineReferences<Types>, specs: Vec<ParLineSpec<Types>>, children: Vec<TeXNode<Types>>, sourceref: SourceReference<<<Self::ET as EngineTypes>::File as File>::SourceRefID>) {
        if children.is_empty() { return }
        let ret = split_paragraph_roughly(engine,specs.clone(),children,sourceref);
        Self::add_node(engine,TeXNode::Custom(RusTeXNode::ParagraphBegin(specs)));
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
        Self::add_node(engine,TeXNode::Custom(RusTeXNode::ParagraphEnd));
    }
}

pub(crate) const CLOSE_FONT:&str = "!\"$%&/(closefont)\\&%$\"!";
pub(crate) fn close_font(engine: Refs, _token: CompactToken) {
    RusTeXStomach::add_node(engine,TeXNode::Custom(RusTeXNode::FontChangeEnd));
}