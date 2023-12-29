use crate::engine::{EngineReferences, EngineTypes};
use crate::tex::nodes::{HorizontalNodeListType, NodeList, NodeTrait, VerticalNodeListType};
use crate::tex::nodes::boxes::TeXBox;
use crate::tex::nodes::horizontal::HNode;
use crate::tex::nodes::math::{MathAtom, MathNode, MathNucleus};
use crate::tex::nodes::vertical::VNode;
use crate::tex::types::{BoxType, TeXMode};
use crate::engine::stomach::Stomach;
use crate::engine::state::State;
use crate::engine::mouth::Mouth;
use crate::engine::utils::memory::PRIMITIVES;
use crate::tex::numerics::TeXDimen;
use crate::tex::numerics::Skip;

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
                TeXMode::Horizontal if engine.stomach.data_mut().open_lists.len() == 1 => ET::Stomach::add_node_h(engine, HNode::Insert(n, children.into())),
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

pub fn add_node_v<ET:EngineTypes>(engine:&mut EngineReferences<ET>, node: VNode<ET>) {
    let data = engine.stomach.data_mut();
    let pre = match node {
        VNode::Box(ref b@TeXBox::H {..}) => {
            if data.prevdepth > ET::Dim::from_sp(-65536000) {
                let baselineskip = engine.state.get_primitive_skip(PRIMITIVES.baselineskip);
                let lineskiplimit = engine.state.get_primitive_dim(PRIMITIVES.lineskiplimit);
                let ht = b.height();
                let b = ET::Skip::new(baselineskip.base() - data.prevdepth - ht, baselineskip.stretch(), baselineskip.shrink());
                let sk = if b.base() >= lineskiplimit { b }
                else {
                    engine.state.get_primitive_skip(PRIMITIVES.lineskip)
                };
                if sk != ET::Skip::default() {Some(sk)} else {None}
            } else {None}
        }
        _ => None
    };


    if let VNode::HRule {..} = node {
        data.prevdepth = ET::Dim::from_sp(-65536000);
    } else {
        data.prevdepth = node.depth();
    }
    match data.open_lists.last_mut() {
        Some(NodeList::Vertical {children,..}) => {
            if let Some(pre) = pre {
                children.push(VNode::VSkip(pre));
            }
            children.push(node);
            return
        }
        Some(_) => todo!("throw error"),
        _ => ()
    }
    if !data.page_contains_boxes && !data.in_output /*data.pagegoal == <<Self::ET as EngineTypes>::Dim as TeXDimen>::from_sp(i32::MAX)*/ {
        match &node {
            VNode::Box(_) | VNode::Insert(..) => {
                //crate::debug_log!(debug => "Here: {} \n\n {}",node.readable(),engine.mouth.display_position());
                data.page_contains_boxes = true;
                data.pagegoal = engine.state.get_primitive_dim(PRIMITIVES.vsize);
            }
            n if n.discardable() => return,
            _ => ()
        }
    }

    if let Some(pre) = pre {
        data.pagetotal = data.pagetotal + pre.base();
        data.page.push(VNode::VSkip(pre));
    }
    data.pagetotal = data.pagetotal + node.height() + node.depth(); // ?
    if let VNode::Penalty(i) = node {
        if i <= -10000 {
            if data.page_contains_boxes {
                return ET::Stomach::maybe_shipout(engine, Some(i))
            } else { return }
        }
    }
    data.page.push(node);
    ET::Stomach::maybe_shipout(engine, None)
}