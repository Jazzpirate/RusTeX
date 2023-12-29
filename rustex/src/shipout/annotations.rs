use tex_engine::pdflatex::nodes::{ActionSpec, ColorStackAction, GotoAction, PDFStartLink};
use crate::engine::{Font, Refs, Types};
use crate::html::{HTMLChild, HTMLNode};
use crate::html::labels::{COLOR_CHANGE, FONT_CHANGE, LINK};
use crate::shipout::ShipoutState;
use tex_engine::pdflatex::nodes::PDFExtension;

pub(crate) fn close_all(open: &mut Vec<HTMLNode>) -> HTMLNode {
    let mut reopen:Vec<HTMLNode> = vec!();
    loop {
        let mut last = open.pop().unwrap();
        if last.label == COLOR_CHANGE {
            reopen.push(last);
        } else if last.label == FONT_CHANGE {
            reopen.push(last);
        } else if last.label == LINK {
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
                curr.push_node(old);
            }
            last.push_node(curr);
            for n in redo.into_iter().rev() {
                open.push(n);
            }
            return last
        }
    }
}

pub(crate) fn do_color(state:&mut ShipoutState, engine:Refs, color:ColorStackAction,math:bool,svg:bool) {
    let stack = engine.aux.extension.colorstacks();
    let curr = state.colors.last().unwrap();
    match color {
        ColorStackAction::Set(idx,c) => {
            *stack[idx].last_mut().unwrap() = c;
            if *engine.aux.extension.current_colorstack() == idx && *curr != c {
                todo!()
            }
        }
        ColorStackAction::Push(idx,c) => {
            stack[idx].push(c);
            if *engine.aux.extension.current_colorstack() == idx && *curr != c {
                state.colors.push(c);
                let mut node = HTMLNode::new(COLOR_CHANGE,false);
                node.style("color",c.to_string());
                state.nodes.push(node);
            }
        }
        ColorStackAction::Pop(idx) => {
            stack[idx].pop();
            let old = stack[idx].last().unwrap().clone();
            let curr = *curr;
            if *engine.aux.extension.current_colorstack() == idx && curr != old {
                state.colors.pop();
                let mut requeue:Vec<HTMLNode> = vec!();
                while let Some(n) = state.nodes.pop() {
                    if n.label == COLOR_CHANGE {
                        if requeue.is_empty() {
                            if !n.children.is_empty() {state.push(n,math,svg);}
                            return
                        }
                        let Some(f) = n.font.clone() else { unreachable!() };
                        if !n.children.is_empty() {state.push(n,math,svg);}
                        for mut c in requeue.into_iter().rev() {
                            let mut node = HTMLNode::new(COLOR_CHANGE, c.allow_newline);
                            node.style("color",curr.to_string());
                            node.children = std::mem::take(&mut c.children);
                            if !node.children.is_empty() {c.children.push(HTMLChild::Node(node));}
                            state.nodes.push(c);
                        }
                        return
                    } else {
                        requeue.push(n);
                    }
                }
            }
        }
        ColorStackAction::Current(idx) => {
            let old = stack[idx].last().unwrap().clone();
            if *engine.aux.extension.current_colorstack() != idx  {
                *engine.aux.extension.current_colorstack() = idx;
                if old != *curr {
                    todo!()
                }
            }
        }
    }
}


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

pub(crate) fn do_font(state:&mut ShipoutState,font:Font) {
    let mut node = HTMLNode::new(FONT_CHANGE,false);
    node.set_font(font);
    state.nodes.push(node);
}

pub(crate) fn close_font(state:&mut ShipoutState,math:bool,svg:bool) {
    let mut requeue:Vec<HTMLNode> = vec!();
    while let Some(n) = state.nodes.pop() {
        if n.label == FONT_CHANGE {
            if requeue.is_empty() {
                if !n.children.is_empty() {state.push(n,math,svg);}
                return
            }
            let Some(f) = n.font.clone() else { unreachable!() };
            if !n.children.is_empty() {state.push(n,math,svg);}
            for mut c in requeue.into_iter().rev() {
                let mut node = HTMLNode::new(FONT_CHANGE, c.allow_newline);
                node.set_font(f.clone());
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