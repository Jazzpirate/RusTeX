use tex_engine::commands::{Command, CommandScope, Expandable, Unexpandable};
use tex_engine::engine::DefaultEngine;
use tex_engine::engine::utils::memory::PRIMITIVES;
use tex_engine::tex::tokens::CompactToken;
use crate::engine::{Refs, register_command, Types};
use tex_engine::engine::TeXEngine;
use tex_engine::tex::nodes::horizontal::HNode;
use tex_engine::utils::HMap;
use crate::nodes::RusTeXNode;
use tex_engine::engine::mouth::Mouth;
use tex_engine::engine::state::State;
use crate::stomach::RusTeXStomach;
use tex_engine::engine::stomach::Stomach;
use tex_engine::prelude::*;
use tex_engine::engine::utils::memory::MemoryManager;

pub(crate) fn register_pgf(engine:&mut DefaultEngine<Types>) {
    crate::engine::register_command(engine, true, "pgfsysdriver", "",
                                    "pgfsys-rustex.def",
                                    false, false
    );
    let all: &[(&'static str,fn(Refs,CompactToken))] = &[
        ("rustex!pgf!hbox",pgfhbox),
        ("rustex!pgf!typesetpicturebox",pgftypesetpicturebox),
        ("rustex!pgf!literal",pgfliteral),
        ("rustex!pgf!gbegin",gbegin),
        ("rustex!pgf!gend",gend),
        ("rustex!pgf!begin", pgfbegin),
        ("rustex!pgf!end", pgfend)
    ];
    for (s,c) in all {
        engine.register_primitive(Command::Unexpandable(
            Unexpandable {
                name:PRIMITIVES.get(s),
                scope:CommandScope::SwitchesToHorizontalOrMath,
                apply:*c
            }
        ),s);
    }
    engine.register_primitive(Command::Expandable(
        Expandable {
            name:PRIMITIVES.get("rustex!pgf!flushpath"),
            expand:pgfflushpath
        }
    ),"rustex!pgf!flushpath");
    register_command(engine, true, "rustex!pgf!get!dimens", "",
                     "=\\pgf@picminx =\\pgf@picminy =\\pgf@picmaxx =\\pgf@picmaxy",
                     true, false
    );
}

fn pgfhbox(engine:Refs,_token:CompactToken) {
    let num = engine.read_register_index(false);
    let bx = engine.state.take_box_register(num).unwrap();
    let node = RusTeXNode::PGFEscape(bx);
    RusTeXStomach::add_node_h(engine, HNode::Custom(node));
    /*
    let mut ch = match bx {
        TeXBox::H {ref mut children,..} => std::mem::take(children).into_vec(),
        _ => {
            let node = RusTeXNode::PGFEscape(bx);
            RusTeXStomach::add_node_h(engine, HNode::Custom(node));
            return
        }
    };
    let mut endidx = 0;
    for (i,b) in ch.iter().enumerate() {
        match b {
            HNode::Custom(RusTeXNode::FontChange(..) | RusTeXNode::FontChangeEnd |
                            RusTeXNode::PDFNode(PDFNode::PDFStartLink(..)) |
                            RusTeXNode::PDFNode(PDFNode::PDFEndLink) |
                            RusTeXNode::PDFNode(PDFNode::Color(..)) |
                          RusTeXNode::PGFGBegin {..} | RusTeXNode::PGFGEnd
            ) => (),
            _ => { endidx = i; break }
        }
    }
    let mut rest = ch.split_off(endidx);
    let mut readd = vec!();
    while let Some(b) = rest.pop() {
        match b {
            HNode::Custom(RusTeXNode::FontChange(..) | RusTeXNode::FontChangeEnd |
                          RusTeXNode::PDFNode(PDFNode::PDFStartLink(..)) |
                          RusTeXNode::PDFNode(PDFNode::PDFEndLink) |
                          RusTeXNode::PDFNode(PDFNode::Color(..)) |
                          RusTeXNode::PGFGBegin {..} | RusTeXNode::PGFGEnd
            ) => readd.push(b),
            _ => { rest.push(b); break }
        }
    }

    for c in ch { RusTeXStomach::add_node_h(engine,c) }
    match bx {
        TeXBox::H {ref mut children,..} => *children = rest.into(),
        _ => unreachable!()
    };
    let node = RusTeXNode::PGFEscape(bx);
    RusTeXStomach::add_node_h(engine, HNode::Custom(node));
    for c in readd.into_iter().rev() { RusTeXStomach::add_node_h(engine,c) }

     */
}
fn pgftypesetpicturebox(engine:Refs, _token:CompactToken) {
    let num = engine.read_register_index(false);
    let bx = engine.state.take_box_register(num).unwrap();
    let getdimens = engine.aux.memory.cs_interner_mut().new("rustex!pgf!get!dimens");
    engine.requeue(CompactToken::from_cs(getdimens));
    let minx = engine.read_dim(true);
    let miny = engine.read_dim(true);
    let maxx = engine.read_dim(true);
    let maxy = engine.read_dim(true);
    let node = RusTeXNode::PGFSvg { bx, minx, miny, maxx, maxy };
    RusTeXStomach::add_node_h(engine, HNode::Custom(node));
    //tex_engine::add_node!(RusTeXStomach;engine, VNode::Custom(node), HNode::Custom(node),MathNode::Custom(node));
}
fn pgfliteral(engine:Refs,_token:CompactToken) { todo!("pgfliteral") }
fn pgfflushpath(engine:Refs, ret:&mut Vec<CompactToken>,_token:CompactToken) {
    let path = engine.aux.memory.cs_interner_mut().new("pgf@sys@svgpath");
    let empty = engine.state.get_command(&engine.aux.memory.cs_interner_mut().new("pgfutil@empty")).cloned();
    match engine.state.get_command(&path) {
        Some(Command::Macro(m)) => ret.extend(m.expansion.0.iter().cloned()),
        _ => todo!("error"),
    };
    engine.state.set_command(engine.aux,path,empty,true);
}
fn gbegin(engine:Refs,_token:CompactToken) {
    let mut attrs : HMap<&'static str,String> = HMap::default();
    let mut key = String::new();
    let start = engine.mouth.start_ref();
    engine.read_braced_string(true,true,&mut key);
    'attr: loop {
        //println!("HERE! {}",engine.preview());
        match engine.read_keywords(&[
            b"about",b"datatype",b"href",b"inlist",b"prefix",b"property",b"rel",
            b"resource",b"rev",b"src",b"typeof",b"content",
            b"clip-path",b"fill-rule",b"opacity",b"stroke-opacity",b"fill-opacity",
            b"transform",b"stroke-dasharray",b"stroke-dashoffset",b"stroke-width",
            b"stroke",b"fill",b"id",b"marker-start",b"marker-end",b"d",b"fill",
            b"visibility",b"stroke-linecap",b"stroke-linejoin",b"href",
            b"fx",b"fy",b"stroke-miterlimit",b"patternUnits",b"patternTransform",
            b"markerUnits",b"orient",b"overflow",b"attributeName",b"from",b"to",b"stop-color",b"style",
            b"animateTransform",b"animateMotion",b"type",b"gradientTransform",b"offset",b"width",b"height",
            b"dur",b"restart",b"repeatCount",b"repeatDur",b"begin",b"end"]) {
            None => {
                break 'attr
            },
            Some(s) if s == b"animateTransform" => {
                attrs.insert("animateTransform","true".into());
            }
            Some(s) if s == b"animateMotion" => {
                attrs.insert("animateMotion","true".into());
            }
            Some(s) => {
                let mut r = String::new();
                engine.read_string(true,&mut r);
                attrs.insert(std::str::from_utf8(s).unwrap(),r.into());
            }
        }
    }
    let bg = RusTeXNode::PGFGBegin {attrs,tag:key };
    RusTeXStomach::add_node_h(engine, HNode::Custom(bg));
}
fn gend(engine:Refs,_token:CompactToken) {
    RusTeXStomach::add_node_h(engine, HNode::Custom(RusTeXNode::PGFGEnd));
}
fn pgfbegin(engine:Refs, _token:CompactToken) { todo!("pgfbegin") }
fn pgfend(engine:Refs, _token:CompactToken) { todo!("pgfend") }