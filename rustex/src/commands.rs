use tex_engine::add_node;
use tex_engine::commands::{CommandScope, PrimitiveCommand};
use tex_engine::commands::primitives::{register_simple_expandable, register_unexpandable};
use tex_engine::engine::DefaultEngine;
use tex_engine::engine::state::State;
use tex_engine::tex::nodes::horizontal::HNode;
use tex_engine::tex::nodes::math::MathNode;
use tex_engine::tex::nodes::NodeList;
use tex_engine::tex::nodes::vertical::VNode;
use tex_engine::tex::tokens::CompactToken;
use crate::engine::{Refs, register_command, Res, Types};
use crate::nodes::RusTeXNode;
use tex_engine::engine::stomach::Stomach;
use crate::stomach::RusTeXStomach;
use tex_engine::prelude::*;
use tex_engine::engine::mouth::Mouth;
use tex_engine::utils::HMap;
use crate::utils::VecMap;

pub fn register_primitives_preinit(engine:&mut DefaultEngine<Types>) {
    register_simple_expandable(engine,CLOSE_FONT,close_font);
    engine.state.register_primitive(&mut engine.aux,"rustexBREAK",PrimitiveCommand::Unexpandable {
        scope:CommandScope::Any,
        apply:|_,_| {
            println!("HERE!");Ok(())
        }
    });
}

pub fn register_primitives_postinit(engine:&mut DefaultEngine<Types>) {
    register_command(engine, true, "LaTeX", "",
                     "L\\kern-.3em\\raise.5ex\\hbox{\\check@mathfonts\\fontsize\\sf@size\\z@\\math@fontsfalse\\selectfont A}\\kern-.15em\\TeX",
                     true, false
    );
    crate::pgf::register_pgf(engine);
    register_unexpandable(engine,"rustex@annotateHTML",CommandScope::Any,annot_begin);
    register_unexpandable(engine,"rustex@HTMLNode",CommandScope::Any,node_begin);
    register_unexpandable(engine,"rustex@annotateHTMLEnd",CommandScope::Any,annot_end);
    engine.state.register_primitive(&mut engine.aux,"if@rustex",PrimitiveCommand::Conditional(tex_engine::commands::tex::iftrue));
    register_unexpandable(engine,"rustex@addNamespaceAbbrev",CommandScope::Any, namespace);
    register_unexpandable(engine,"rustex@addMeta",CommandScope::Any, meta);
    register_unexpandable(engine,"rustex@annotateTop",CommandScope::Any,annot_top);
    register_unexpandable(engine,"rustex@cssLink",CommandScope::Any,css_link);
    register_unexpandable(engine,"rustex@HTMLLiteral",CommandScope::Any,html_literal);
    register_unexpandable(engine,"rustex@directHTML",CommandScope::Any,html_literal);
    // if@rustex
    // rustex@directHTML
}

fn html_literal(engine:Refs,token:CompactToken) -> Res<()> {
    let mut lit = String::new();
    engine.read_braced_string(true,true,&token,&mut lit)?;
    let node = RusTeXNode::Literal(lit);
    add_node!(RusTeXStomach;engine, VNode::Custom(node),HNode::Custom(node),MathNode::Custom(node));
    Ok(())
}
fn css_link(engine:Refs,token:CompactToken) -> Res<()> {
    let mut file = String::new();
    engine.read_braced_string(true,true,&token,&mut file)?;
    engine.aux.extension.css_files.push(file);
    Ok(())
}

fn namespace(engine:Refs,token:CompactToken) -> Res<()> {
    let mut key = String::new();
    engine.read_braced_string(true,true,&token,&mut key)?;
    let mut value = String::new();
    engine.read_braced_string(true,true,&token,&mut value)?;
    engine.aux.extension.namespaces.insert(key,value);
    Ok(())
}

fn meta(engine:Refs,token:CompactToken) -> Res<()> {
    let mut attrs = VecMap::default();
    let mut str = String::new();
    engine.read_braced_string(true,true,&token,&mut str)?;
    let mut s = str[..].trim();
    while !s.is_empty() {
        let key = if let Some(i) = s.find('=') {
            let (n,v) = s.split_at(i);
            s = &v[1..].trim_start();
            n.trim()
        } else { todo!() };
        let delim = if s.starts_with('"') {
            s = &s[1..];
            '"'
        } else if s.starts_with('\'') {
            s = &s[1..];
            '\''
        } else { todo!() };
        let val = if let Some(i) = s.find(delim) {
            let (v,r) = s.split_at(i);
            s = r[1..].trim_start();
            v.trim()
        } else { todo!() };
        attrs.insert(key.to_string(),val.to_string());
    }
    engine.aux.extension.metas.push(attrs);
    Ok(())
}

fn annot_top(engine:Refs,token:CompactToken) -> Res<()> {
    let mut str = String::new();
    engine.read_braced_string(true,true,&token,&mut str)?;
    let mut s = str[..].trim();
    let top = &mut engine.aux.extension.top;
    while !s.is_empty() {
        let key = if let Some(i) = s.find('=') {
            let (n,v) = s.split_at(i);
            s = &v[1..].trim_start();
            n.trim()
        } else { todo!() };
        let delim = if s.starts_with('"') {
            s = &s[1..];
            '"'
        } else if s.starts_with('\'') {
            s = &s[1..];
            '\''
        } else { todo!() };
        let val = if let Some(i) = s.find(delim) {
            let (v,r) = s.split_at(i);
            s = r[1..].trim_start();
            v.trim()
        } else { todo!() };
        top.insert(key.to_string(),val.to_string());
    }
    Ok(())
}

fn node_begin(engine:Refs,token:CompactToken) -> Res<()> {
    let mut attrs = VecMap::default();
    let mut styles = VecMap::default();
    let start = engine.mouth.start_ref();
    let mut tag = String::new();
    engine.read_braced_string(true,true,&token,&mut tag)?;
    let mut str = String::new();
    engine.read_braced_string(true,true,&token,&mut str)?;
    let mut s = str[..].trim();
    while !s.is_empty() {
        let style = if s.starts_with("style:") {
            s = &s[6..];
            true
        } else {false};
        let key = if let Some(i) = s.find('=') {
            let (n,v) = s.split_at(i);
            s = &v[1..].trim_start();
            n.trim()
        } else { todo!() };
        let delim = if s.starts_with('"') {
            s = &s[1..];
            '"'
        } else if s.starts_with('\'') {
            s = &s[1..];
            '\''
        } else { todo!() };
        let val = if let Some(i) = s.find(delim) {
            let (v,r) = s.split_at(i);
            s = r[1..].trim_start();
            v.trim()
        } else { todo!() };
        if style {
            styles.insert(key.to_string(),val.to_string());
        } else {
            attrs.insert(key.to_string(),val.to_string());
        }
    }
    let node = RusTeXNode::AnnotBegin{attrs,styles,start,tag:Some(tag)};
    add_node!(RusTeXStomach;engine, VNode::Custom(node),HNode::Custom(node),MathNode::Custom(node));
    Ok(())
}
fn annot_begin(engine:Refs,token:CompactToken) -> Res<()> {
    let mut attrs = VecMap::default();
    let mut styles = VecMap::default();
    let start = engine.mouth.start_ref();
    let mut str = String::new();
    engine.read_braced_string(true,true,&token,&mut str)?;
    let mut s = str[..].trim();
    while !s.is_empty() {
        let style = if s.starts_with("style:") {
            s = &s[6..];
            true
        } else {false};
        let key = if let Some(i) = s.find('=') {
            let (n,v) = s.split_at(i);
            s = &v[1..].trim_start();
            n.trim()
        } else { todo!() };
        let delim = if s.starts_with('"') {
            s = &s[1..];
            '"'
        } else if s.starts_with('\'') {
            s = &s[1..];
            '\''
        } else { todo!() };
        let val = if let Some(i) = s.find(delim) {
            let (v,r) = s.split_at(i);
            s = r[1..].trim_start();
            v.trim()
        } else { todo!() };
        if style {
            styles.insert(key.to_string(),val.to_string());
        } else {
            attrs.insert(key.to_string(),val.to_string());
        }
    }
    let node = RusTeXNode::AnnotBegin{attrs,styles,start,tag:None};
    add_node!(RusTeXStomach;engine, VNode::Custom(node),HNode::Custom(node),MathNode::Custom(node));
    Ok(())
}
fn annot_end(engine:Refs,_token:CompactToken) -> Res<()> {
    let node = RusTeXNode::AnnotEnd(engine.mouth.current_sourceref());
    add_node!(RusTeXStomach;engine, VNode::Custom(node),HNode::Custom(node),MathNode::Custom(node));
    Ok(())
}

pub const CLOSE_FONT:&str = "!\"$%&/(closefont)\\&%$\"!";
pub fn close_font(engine: Refs, _token: CompactToken) -> Res<()> {
    match engine.stomach.data_mut().open_lists.last_mut() {
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
            match children.list_mut().last() {
                Some(MathNode::Custom(RusTeXNode::FontChange(_,_))) => {
                    children.list_mut().pop();
                }
                _ => children.push(MathNode::Custom(RusTeXNode::FontChangeEnd))
            }
        },
        _ => engine.stomach.data_mut().page.push(VNode::Custom(RusTeXNode::FontChangeEnd))
    }
    Ok(())
}