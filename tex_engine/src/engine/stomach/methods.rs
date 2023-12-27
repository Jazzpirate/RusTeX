use crate::engine::{EngineReferences, EngineTypes};
use crate::tex::nodes::{HorizontalNodeListType, NodeList, VerticalNodeListType};
use crate::tex::nodes::boxes::TeXBox;
use crate::tex::nodes::horizontal::HNode;
use crate::tex::nodes::math::{MathAtom, MathNode, MathNucleus};
use crate::tex::nodes::vertical::VNode;
use crate::tex::types::{BoxType, TeXMode};
use crate::engine::stomach::Stomach;
use crate::engine::state::State;
use crate::engine::mouth::Mouth;

#[macro_export]
macro_rules! add_node {
    ($S:ty;$engine:expr,$v:expr,$h:expr,$m:expr) => {
        match $engine.state.get_mode() {
            TeXMode::Vertical |
            TeXMode::InternalVertical => <$S>::add_node_v($engine,$v),
            TeXMode::Horizontal |
            TeXMode::RestrictedHorizontal => <$S>::add_node_h($engine,$h),
            _ => <$S>::add_node_m($engine,$m)
        }
    };
}

pub fn close_box<ET:EngineTypes>(engine:&mut EngineReferences<ET>, bt:BoxType) {
    match engine.stomach.data_mut().open_lists.last() {
        Some(NodeList::Horizontal {tp:HorizontalNodeListType::Paragraph(_),..}) => ET::Stomach::close_paragraph(engine),
        _ => ()
    }
    match engine.stomach.data_mut().open_lists.pop() {
        Some(NodeList::Vertical {children,tp:VerticalNodeListType::VAdjust}) if bt == BoxType::Vertical => {
            engine.state.pop(engine.aux,engine.mouth);
            engine.stomach.data_mut().vadjusts.extend(children.into_iter())
        }
        Some(NodeList::Vertical {children,tp:VerticalNodeListType::Insert(n)}) if bt == BoxType::Vertical => {
            engine.state.pop(engine.aux,engine.mouth);
            match engine.state.get_mode() {
                TeXMode::Vertical => ET::Stomach::add_node_v(engine, VNode::Insert(n, children.into())),
                TeXMode::Horizontal => ET::Stomach::add_node_h(engine, HNode::Insert(n, children.into())),
                _ => engine.stomach.data_mut().inserts.push((n,children.into()))
            }
        }
        Some(NodeList::Vertical {children,tp:VerticalNodeListType::Box(info,start,target)})  if bt == BoxType::Vertical => {
            engine.state.pop(engine.aux,engine.mouth);
            let bx = TeXBox::V {
                children:children.into(),info,start,end:engine.mouth.current_sourceref(),
            };
            ET::Stomach::add_box(engine, bx, target)
        }
        Some(NodeList::Vertical {children,tp:VerticalNodeListType::VCenter(start)}) if bt == BoxType::Vertical => {
            engine.state.pop(engine.aux,engine.mouth);
            ET::Stomach::add_node_m(engine, MathNode::Atom(MathAtom {
                nucleus: MathNucleus::VCenter {children:children.into(),start,end:engine.mouth.current_sourceref()},
                sub:None,sup:None
            }))
        }
        Some(NodeList::Horizontal {children,tp:HorizontalNodeListType::Box(info,start,target)}) if bt == BoxType::Horizontal => {
            engine.state.pop(engine.aux,engine.mouth);
            let bx = TeXBox::H {
                children:children.into(),info,start,end:engine.mouth.current_sourceref(),
            };
            ET::Stomach::add_box(engine, bx, target)
        }
        o =>
            todo!("throw error: {:?}",o),
    }
    match engine.state.get_mode() {
        TeXMode::Vertical => {
            let data = engine.stomach.data_mut();
            let inserts = std::mem::take(&mut data.inserts);
            data.page.extend(inserts.into_iter().map(|(a,b)| VNode::Insert(a,b)));
            let adjusts = std::mem::take(&mut data.vadjusts);
            data.page.extend(adjusts.into_iter());
        }
        TeXMode::Horizontal => {
            let adjusts = std::mem::take(&mut engine.stomach.data_mut().vadjusts);
            ET::Stomach::add_node_h(engine, HNode::VAdjust(adjusts.into()))
        }
        _ => ()
    }
}