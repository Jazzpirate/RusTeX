use std::collections::BTreeSet;
use tex_engine::pdflatex::nodes::{ActionSpec, ColorStackAction, GotoAction, PDFNode, PDFStartLink};
use crate::engine::{Font, Refs, Types};
use crate::html::{HTMLChild, HTMLNode, HTMLTag};
use crate::shipout::{ShipoutMode, ShipoutState};
use tex_engine::pdflatex::nodes::PDFExtension;
use tex_engine::engine::fontsystem::Font as FontT;
use crate::fonts::FontStore;

pub(crate) fn close_all(mode:ShipoutMode,open: &mut Vec<HTMLNode>) -> HTMLNode {
    let mut reopen:Vec<HTMLNode> = vec!();
    loop {
        let mut last = open.pop().unwrap();
        if matches!(last.tag, HTMLTag::ColorChange(_)) {
            reopen.push(last);
        } else if matches!(last.tag, HTMLTag::FontChange(_)) {
            reopen.push(last);
        } else if matches!(last.tag, HTMLTag::Link(_)) {
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
    let font = state.fonts.pop().unwrap();
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
    unreachable!()
}


pub(crate) fn do_color(state:&mut ShipoutState, engine:Refs, color:ColorStackAction) {
    let stack = engine.aux.extension.colorstacks();
    let curr = *state.colors.last().unwrap();
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
            let old = stack[idx].last().unwrap().clone();
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
            let old = stack[idx].last().unwrap().clone();
            if *engine.aux.extension.current_colorstack() != idx  {
                *engine.aux.extension.current_colorstack() = idx;
                if old != curr {
                    todo!()
                }
            }
        }
    }
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
    let i = state.nodes.iter().enumerate().find_map(|(i,n)|
        if matches!(n.tag,HTMLTag::Matrix(_)) {Some(i)} else {None});
    if let Some(i) = i {
        if i != state.nodes.len() - 1 {
            todo!("reset matrix")
        }
        let matrix = state.nodes.pop().unwrap();
        state.push(matrix)
    }
}


/*


pub(crate) fn do_link(link:PDFStartLink<Types>,state:&mut ShipoutState,in_v:bool) {
    match link.action {
        ActionSpec::Goto(GotoAction::Current {target,..}) => {
            let mut node = HTMLNode::new(LINK,in_v);
            node.attr("href",format!("#{}",target.as_name()));
            state.nodes.push(node);
        },
        ActionSpec::User(str) => {
            let url = if str.contains("/URI(") {
                str.split("/URI(").last().unwrap().split(")").next().unwrap()
            } else if str.contains("/F(") {
                str.split("/F(").last().unwrap().split(")").next().unwrap()
            } else {
                ""
            };
            let mut node = HTMLNode::new(LINK,in_v);
            node.attr("href",url.to_string());
            state.nodes.push(node);
        }
        ActionSpec::Goto(_) => todo!(),
        _ => ()
    }
}

pub(crate) fn close_link(state:&mut ShipoutState,math:bool,svg:bool) {
    let mut requeue:Vec<HTMLNode> = vec!();
    while let Some(n) = state.nodes.pop() {
        if n.label == LINK {
            if requeue.is_empty() {
                if !n.children.is_empty() {state.push(n,math,svg);}
                return
            }
            let Some(url) = n.attrs.get("href") else { unreachable!() };
            let url = url.to_string();
            if !n.children.is_empty() {state.push(n,math,svg);}
            for mut c in requeue.into_iter().rev() {
                let mut node = HTMLNode::new(LINK, c.allow_newline);
                node.attr("href",url.clone());
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

 */