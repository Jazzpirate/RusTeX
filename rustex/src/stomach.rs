use tex_engine::commands::{Command, CommandScope};
use tex_engine::commands::methods::make_macro;
use tex_engine::engine::{EngineAux, EngineReferences, EngineTypes};
use tex_engine::engine::filesystem::{File, SourceReference};
use tex_engine::engine::fontsystem::FontSystem;
use tex_engine::engine::state::State;
use tex_engine::engine::stomach::{insert_afterassignment, ParLine, ParLineSpec, split_paragraph_roughly, SplitResult, Stomach, StomachData};
use tex_engine::engine::utils::memory::{MemoryManager, PRIMITIVES};
use tex_engine::tex::nodes::{BoxTarget, HorizontalNodeListType, NodeList, VerticalNodeListType};
use tex_engine::tex::numerics::{Dim32, Skip32};
use tex_engine::tex::token::CompactToken;
use tex_engine::tex::types::{BoxType, GroupType};
use crate::engine::{AT_LETTER_SCHEME, Font, Refs, Types};
use crate::extension::FontChange;
use crate::nodes::{LineSkip, RusTeXNode};
use crate::state::RusTeXState;
use tex_engine::engine::mouth::Mouth;
use tex_engine::engine::stomach::methods::add_node_v;
use tex_engine::tex::nodes::boxes::{TeXBox, ToOrSpread, VBoxInfo};
use tex_engine::tex::nodes::horizontal::HNode;
use tex_engine::tex::nodes::math::{MathAtom, MathNode, MathNucleus};
use tex_engine::tex::nodes::NodeTrait;
use tex_engine::tex::nodes::vertical::VNode;
use crate::shipout::ZERO;
use tex_engine::tex::types::TeXMode;
use tex_engine::tex::numerics::TeXDimen;
use tex_engine::tex::numerics::Skip;
use tex_engine::tex::control_sequences::ControlSequenceNameHandler;

pub struct RusTeXStomach {
    afterassignment:Option<CompactToken>,
    data:StomachData<Types>,
    prevent_shipout:bool,
    pub continuous:bool
}
impl Stomach for RusTeXStomach {
    type ET = Types;
    #[inline(always)]
    fn new(_aux: &mut EngineAux<Types>, _state: &mut RusTeXState) -> Self {
        Self { afterassignment:None, data:StomachData::new(), prevent_shipout:false,continuous:false }
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
        tex_engine::add_node!(Self;engine,
                       VNode::Custom(RusTeXNode::FontChange(f.clone(),g)),
                       HNode::Custom(RusTeXNode::FontChange(f.clone(),g)),
                       MathNode::Custom(RusTeXNode::FontChange(f.clone(),g))
        );
        if !g {
            engine.aux.extension.change_markers.last_mut().unwrap().push(FontChange(f.clone()));
        }
        engine.state.set_current_font(engine.aux,f,global);
        insert_afterassignment(engine);
    }

    fn close_box(engine:&mut EngineReferences<Self::ET>, bt:BoxType) {
        let markers = std::mem::take(engine.aux.extension.change_markers.last_mut().unwrap());
        for _ in markers {
            tex_engine::add_node!(Self;engine,
                            VNode::Custom(RusTeXNode::FontChangeEnd),
                            HNode::Custom(RusTeXNode::FontChangeEnd),
                           MathNode::Custom(RusTeXNode::FontChangeEnd)
            )
        }
        tex_engine::engine::stomach::methods::close_box(engine,bt)
    }

    fn split_paragraph(engine: Refs, specs: Vec<ParLineSpec<Types>>, children: Vec<HNode<Types>>, sourceref: SourceReference<<<Self::ET as EngineTypes>::File as File>::SourceRefID>) {
        if children.is_empty() { return }
        let ret = split_paragraph_roughly(engine,specs.clone(),children,sourceref.clone());
        engine.stomach.prevent_shipout = true;
        Self::add_node_v(engine,VNode::Custom(RusTeXNode::ParagraphBegin{specs,
            start:sourceref,
            end:engine.mouth.current_sourceref(),
            lineskip:LineSkip::get(engine.state),
            parskip:engine.state.get_primitive_skip(PRIMITIVES.parskip)
        }));
        for line in ret {
            match line {
                ParLine::Adjust(n) => Self::add_node_v(engine,n),
                ParLine::Line(bx) => Self::add_node_v(engine,VNode::Box(bx))
            }
        }
        engine.stomach.prevent_shipout = false;
        Self::add_node_v(engine,VNode::Custom(RusTeXNode::ParagraphEnd));
    }
    /*
    fn add_node_v(engine: &mut EngineReferences<Self::ET>, node: VNode<Self::ET>) {
        if engine.stomach.data.in_output
        match node {
            VNode::Custom(RusTeXNode::PageBegin | RusTeXNode::PageEnd) => (),
            _ => add_node_v(engine,node)
        }
    }
     */

    fn maybe_shipout(engine:&mut EngineReferences<Self::ET>,penalty:Option<i32>) {
        if engine.stomach.prevent_shipout { return }
        let continuous = engine.stomach.continuous;
        let data = engine.stomach.data_mut();
        if !data.in_output && data.open_lists.is_empty() && !data.page.is_empty() {
            if continuous {
                data.pagegoal = <Self::ET as EngineTypes>::Dim::from_sp(i32::MAX / 3);
                if data.page_contains_boxes && data.pagetotal > <Self::ET as EngineTypes>::Dim::from_sp(6553600 * 5) {
                    do_shipout(engine,Some(-10000),|_|());
                } else if penalty.is_some() {
                    do_shipout(engine,penalty,|data|data.page.push(VNode::VSkip(Skip32::new(Dim32(655360),None,None))));
                }
            } else if data.pagetotal >= data.pagegoal || penalty.is_some() {
                do_shipout(engine,penalty,|_|());
            }
        }
    }

    fn open_align(engine: Refs, _inner: BoxType, between: BoxType) {
        tex_engine::add_node!(Self;engine,VNode::Custom(RusTeXNode::HAlignBegin),HNode::Custom(RusTeXNode::HAlignBegin),unreachable!());
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
        tex_engine::add_node!(Self;engine,VNode::Custom(RusTeXNode::HAlignEnd),HNode::Custom(RusTeXNode::HAlignEnd),unreachable!());
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
            VNode::Custom(RusTeXNode::ParagraphEnd) => {
                in_par = None;
            }
            VNode::Mark(i, v) => {
                if !data.firstmarks.contains_key(&i) {
                    data.firstmarks.insert(*i,v.clone());
                }
                data.botmarks.insert(*i,v.clone());
            }
            _ => {
                target = target -(n.height() + n.depth()); // - n.depth() ?
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

fn do_shipout<F:FnOnce(&mut StomachData<Types>)>(engine:&mut EngineReferences<Types>,penalty:Option<i32>,f:F) {
    let undefineds = vec![
        // @mkboth: \long#1#2
        engine.aux.memory.cs_interner_mut().new("@oddhead"),
        engine.aux.memory.cs_interner_mut().new("@oddfoot"),
        engine.aux.memory.cs_interner_mut().new("@evenhead"),
        engine.aux.memory.cs_interner_mut().new("@evenfoot"),
    ];
    let mkboth = engine.aux.memory.cs_interner_mut().new("@mkboth");
    let mut m = make_macro::<Types,_,_>(engine.aux.memory.cs_interner_mut(),&AT_LETTER_SCHEME,"#1#2","");
    m.long = true;
    engine.state.set_command(&engine.aux,mkboth,Some(Command::Macro(m)),true);
    let empty = make_macro::<Types,_,_>(engine.aux.memory.cs_interner_mut(), &AT_LETTER_SCHEME, "", "");
    for s in undefineds.into_iter() {
        engine.state.set_command(&engine.aux, s, Some(Command::Macro(empty.clone())), true)
    }
    let data = engine.stomach.data_mut();
    data.page.insert(0,VNode::Custom(RusTeXNode::PageBegin));
    f(data);
    data.page.push(VNode::Custom(RusTeXNode::PageEnd));
    RusTeXStomach::do_shipout_output(engine,penalty)
}