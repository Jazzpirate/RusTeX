/*
use tex_engine::pdflatex::nodes::{ActionSpec, ColorStackAction, GotoAction, PDFStartLink};
use crate::engine::{Font, Refs, SRef, Types};
use crate::shipout::html::{HTMLChild, HTMLNode, HTMLTag};
use crate::shipout::ShipoutMode;
use tex_engine::pdflatex::nodes::PDFExtension;
use tex_engine::engine::fontsystem::Font as FontT;
use tex_engine::utils::HMap;
use crate::engine::fonts::FontStore;
use crate::shipout::state::ShipoutState;
use crate::utils::VecMap;

pub(crate) fn close_all<F:Fn(&HTMLTag) -> bool>(mode:ShipoutMode,open: &mut Vec<HTMLNode>,tag:F) -> HTMLNode {
    let mut reopen:Vec<HTMLNode> = vec!();
    loop {
        let mut last = open.pop().unwrap();
        let m = tag(&last.tag);
        if !m && matches!(last.tag, HTMLTag::ColorChange(_)|HTMLTag::FontChange(_)|HTMLTag::Link(_)|HTMLTag::Annot(_,_)|HTMLTag::Invisible(_)) {
            reopen.push(last);
        } else {
            if reopen.is_empty() { return last }
            let mut redo:Vec<HTMLNode> = vec!();
            let mut iter = reopen.into_iter();
            let mut curr = iter.next().unwrap();
            redo.push(curr.reopened());
            for n in iter {
                redo.push(n.reopened());
                let old = std::mem::replace(&mut curr, n);
                curr.push_open_node(mode,old);
            }
            last.push_open_node(mode,curr);
            for n in redo.into_iter().rev() {
                open.push(n);
            }
            return last
        }
    }
}

pub(crate) fn do_font(state:&mut ShipoutState,store:&FontStore,font:Font) {
    if state.fonts.last() == Some(&font) || font.filename().ends_with("nullfont") {
        state.fonts.push(font);
        return
    }
    if let Some(info) = store.get_info(font.filename()) {
        if let Some((_,link)) = info.weblink {
            state.fontlinks.insert(link.to_string());
        }
    }
    state.fonts.push(font.clone());
    let mut node = HTMLNode::new(HTMLTag::FontChange(state.mode()));
    node.font = Some((font,false));
    state.nodes.push(node);
}

pub(crate) fn close_font(state:&mut ShipoutState) {
    let mut requeue:Vec<HTMLNode> = vec!();
    let font = match state.fonts.pop() {
        Some(f) => f,
        _ => return
    };
    if state.fonts.last() == Some(&font) || font.filename().ends_with("nullfont") {
        return
    }
    while let Some(n) = state.nodes.pop() {
        if matches!(n.tag, HTMLTag::FontChange(_)) {
            if requeue.is_empty() {
                if !n.children.is_empty() {state.push(n);}
                return
            }
            let Some(f) = n.font.clone() else { unreachable!() };
            if !n.children.is_empty() {state.push(n);}
            for mut c in requeue.into_iter().rev() {
                let mut node = HTMLNode::new(HTMLTag::FontChange(state.mode()));
                node.font = Some(f.clone());
                node.children = std::mem::take(&mut c.children);
                if !node.children.is_empty() {c.push_child(state.mode(),HTMLChild::Node(node));}
                state.nodes.push(c);
            }
            return
        } else {
            requeue.push(n);
        }
    }
    state.fonts.push(font);
    for c in requeue.into_iter().rev() {
        state.nodes.push(c)
    }
}

pub(crate) fn do_color(state:&mut ShipoutState, engine:Refs, color:ColorStackAction) {
    let stack = engine.aux.extension.colorstacks();
    let curr = state.colors.last().copied().unwrap_or_default();
    match color {
        ColorStackAction::Set(idx,c) => {
            *stack[idx].last_mut().unwrap() = c;
            if *engine.aux.extension.current_colorstack() == idx && curr != c {
                todo!()
            }
        }
        ColorStackAction::Push(idx,c) => {
            stack[idx].push(c);
            if *engine.aux.extension.current_colorstack() == idx {
                state.colors.push(c);
                if  curr != c {
                    let mut node = HTMLNode::new(HTMLTag::ColorChange(state.mode()));
                    node.color = Some(c);
                    state.nodes.push(node);
                }
            }
        }
        ColorStackAction::Pop(idx) => {
            stack[idx].pop();
            let old = stack[idx].last().copied().unwrap_or_default();
            if *engine.aux.extension.current_colorstack() == idx  {
                state.colors.pop();
                if curr != old {
                    let mut requeue: Vec<HTMLNode> = vec!();
                    while let Some(n) = state.nodes.pop() {
                        if matches!(n.tag,HTMLTag::ColorChange(_)) {
                            if requeue.is_empty() {
                                if !n.children.is_empty() { state.push(n); }
                                return
                            }
                            if !n.children.is_empty() { state.push(n); }
                            for mut c in requeue.into_iter().rev() {
                                let mut node = HTMLNode::new(HTMLTag::ColorChange(state.mode()));
                                node.color = Some(curr);
                                node.children = std::mem::take(&mut c.children);
                                if !node.children.is_empty() { c.push_child(state.mode(),HTMLChild::Node(node)); }
                                state.nodes.push(c);
                            }
                            return
                        } else {
                            requeue.push(n);
                        }
                    }
                }
            }
        }
        ColorStackAction::Current(idx) => {
            let old = *stack[idx].last().unwrap();
            if *engine.aux.extension.current_colorstack() != idx  {
                *engine.aux.extension.current_colorstack() = idx;
                if old != curr {
                    todo!()
                }
            }
        }
    }
}

pub(crate) fn do_link(link:PDFStartLink<Types>,state:&mut ShipoutState) {
    match link.action {
        ActionSpec::Goto(GotoAction::Current {target,..}) => {
            let mut node = HTMLNode::new(HTMLTag::Link(state.mode()));
            node.attrs.insert("href".into(),format!("#{}",target.as_name()));
            state.nodes.push(node);
        },
        ActionSpec::User(str) => {
            let url = if str.contains("/URI(") {
                str.split("/URI(").last().unwrap().split(')').next().unwrap()
            } else if str.contains("/F(") {
                str.split("/F(").last().unwrap().split(')').next().unwrap()
            } else {
                ""
            };
            let mut node = HTMLNode::new(HTMLTag::Link(state.mode()));
            node.attrs.insert("href".into(),url.to_string());
            state.nodes.push(node);
        }
        ActionSpec::Goto(_) => todo!(),
        _ => ()
    }
}

pub(crate) fn close_link(state:&mut ShipoutState) {
    let mut requeue:Vec<HTMLNode> = vec!();
    while let Some(n) = state.nodes.pop() {
        if matches!(n.tag,HTMLTag::Link(_)) {
            if requeue.is_empty() {
                if !n.children.is_empty() {state.push(n);}
                return
            }
            let Some(url) = n.attrs.get("href") else { unreachable!() };
            let url = url.to_string();
            if !n.children.is_empty() {state.push(n);}
            for mut c in requeue.into_iter().rev() {
                let mut node = HTMLNode::new(HTMLTag::Link(state.mode()));
                node.attrs.insert("href".into(),url.clone());
                node.children = std::mem::take(&mut c.children);
                if !node.children.is_empty() {c.children.push(HTMLChild::Node(node));}
                state.nodes.push(c);
            }
            return
        } else {
            requeue.push(n);
        }
    }
    unreachable!()
}

pub(crate) fn do_annot(state:&mut ShipoutState,start:SRef,tag:Option<String>,attrs:VecMap<String,String>,styles:VecMap<String,String>) {
    let mut node = HTMLNode::new(HTMLTag::Annot(state.mode(),tag));
    for (k,v) in attrs.into_iter() {
        node.attrs.insert(k.into(),v);
    }
    for (k,v) in styles.into_iter() {
        node.styles.insert(k.into(),v.into());
    }
    node.sourceref = Some((start,start));
    state.nodes.push(node);
}

pub(crate) fn close_annot(state:&mut ShipoutState,end:SRef) {
    let mut requeue:Vec<HTMLNode> = vec!();
    while let Some(mut n) = state.nodes.pop() {
        match &n.tag {
            HTMLTag::Annot(_,tag) => {
                let Some((start,_)) = std::mem::take(&mut n.sourceref) else {unreachable!()};
                n.sourceref = Some((start,end));
                if requeue.is_empty() {
                    if !n.children.is_empty() || tag.is_some() {state.push(n);}
                    return
                }
                let rf = n.sourceref.unwrap();
                let styles = n.styles.clone();
                let attrs = n.attrs.clone();
                let tag = tag.clone();
                if !n.children.is_empty() {state.push(n);}
                for mut c in requeue.into_iter().rev() {
                    let mut node = HTMLNode::new(HTMLTag::Annot(state.mode(),tag.clone()));
                    node.attrs = attrs.clone();
                    node.styles = styles.clone();
                    node.sourceref = Some(rf);
                    node.children = std::mem::take(&mut c.children);
                    if !node.children.is_empty() {c.children.push(HTMLChild::Node(node));}
                    state.nodes.push(c);
                }
                return
            }
            _ => {requeue.push(n);}
        }
    }
    unreachable!()
}

pub(crate) fn do_invisible(state:&mut ShipoutState) {
    let node = HTMLNode::new(HTMLTag::Invisible(state.mode()));
    state.nodes.push(node);
}
pub(crate) fn close_invisible(state:&mut ShipoutState) {
    let mut requeue:Vec<HTMLNode> = vec!();
    while let Some(mut n) = state.nodes.pop() {
        match &n.tag {
            HTMLTag::Invisible(_) => {
                if !n.children.is_empty() {state.push(n);}
                if requeue.is_empty() {
                    return
                }
                for mut c in requeue.into_iter().rev() {
                    let mut node = HTMLNode::new(HTMLTag::Invisible(state.mode()));
                    node.children = std::mem::take(&mut c.children);
                    if !node.children.is_empty() {c.children.push(HTMLChild::Node(node));}
                    state.nodes.push(c);
                }
                return
            }
            _ => {requeue.push(n);}
        }
    }
    unreachable!()
}

pub(crate) fn do_matrix(state:&mut ShipoutState,scale:f32,rotate:f32,skewx:f32,skewy:f32) {
    let mut node = HTMLNode::new(HTMLTag::Matrix(state.mode()));
    node.classes.push("rustex-pdfmatrix".into());
    node.styles.insert(
        "transform".into(),
        format!("matrix({},{},{},{},0,0)",scale,rotate,skewx,skewy).into()
    );
    state.nodes.push(node);
}

pub(crate) fn reset_matrix(state:&mut ShipoutState) {
    let mut requeue:Vec<HTMLNode> = vec!();
    while let Some(n) = state.nodes.pop() {
        if matches!(n.tag,HTMLTag::Matrix(_)) {
            if requeue.is_empty() {
                if !n.children.is_empty() {state.push(n);}
                return
            }
            let Some(transform) = n.styles.get("transform") else { unreachable!() };
            let transform = transform.to_string();
            if !n.children.is_empty() {state.push(n);}
            for mut c in requeue.into_iter().rev() {
                let mut node = HTMLNode::new(HTMLTag::Matrix(state.mode()));
                node.attrs.insert("transform".into(),transform.clone());
                node.classes.push("rustex-pdfmatrix".into());
                node.children = std::mem::take(&mut c.children);
                if !node.children.is_empty() {c.children.push(HTMLChild::Node(node));}
                state.nodes.push(c);
            }
            return
        } else {
            requeue.push(n);
        }
    }
    for c in requeue.into_iter().rev() {
        state.nodes.push(c)
    }
}

 */
