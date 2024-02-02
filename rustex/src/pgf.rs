use tex_engine::add_node;
use tex_engine::commands::{TeXCommand, CommandScope, PrimitiveCommand};
use tex_engine::engine::DefaultEngine;
use tex_engine::tex::tokens::CompactToken;
use crate::engine::{Refs, register_command, Res, Types};
use tex_engine::tex::nodes::horizontal::HNode;
use tex_engine::utils::HMap;
use crate::nodes::RusTeXNode;
use tex_engine::engine::state::State;
use crate::stomach::RusTeXStomach;
use tex_engine::engine::stomach::Stomach;
use tex_engine::prelude::*;
use tex_engine::tex::nodes::boxes::{HBoxInfo, TeXBox, ToOrSpread};
use tex_engine::tex::nodes::math::{MathAtom, MathKernel, MathNode, MathNucleus};
use tex_engine::tex::nodes::vertical::VNode;

pub(crate) fn register_pgf(engine:&mut DefaultEngine<Types>) {
    register_command(engine, true, "pgfsysdriver", "",
                                    "pgfsys-rustex.def",
                                    false, false
    );
    let all: &[(&'static str,fn(Refs,CompactToken) -> Res<()>)] = &[
        ("rustex!pgf!hbox",pgfhbox),
        ("rustex!pgf!literal",pgfliteral),
        ("rustex!pgf!gbegin",gbegin),
        ("rustex!pgf!gend",gend),
        ("rustex!pgf!begin", pgfbegin),
        ("rustex!pgf!end", pgfend)
    ];
    for (s,c) in all {
        engine.state.register_primitive(&mut engine.aux,s,PrimitiveCommand::Unexpandable {
            scope:CommandScope::Any,//SwitchesToHorizontalOrMath,
            apply:*c
        });
    }
    engine.state.register_primitive(&mut engine.aux,"rustex!pgf!typesetpicturebox",PrimitiveCommand::Unexpandable {
        scope:CommandScope::SwitchesToHorizontalOrMath,
        apply:pgftypesetpicturebox
    });
    engine.state.register_primitive(&mut engine.aux,"rustex!pgf!flushpath",PrimitiveCommand::Expandable(pgfflushpath));
    register_command(engine, true, "rustex!pgf!get!dimens", "",
                     "=\\pgf@picminx =\\pgf@picminy =\\pgf@picmaxx =\\pgf@picmaxy",
                     true, false
    );
}

fn pgfhbox(engine:Refs,_token:CompactToken) -> Res<()> {
    let num = engine.read_register_index(false)?;
    let bx = engine.state.take_box_register(num).unwrap();
    let node = RusTeXNode::PGFEscape(bx);
    add_node!(RusTeXStomach;engine, VNode::Custom(node),HNode::Custom(node),MathNode::Custom(node));
    Ok(())
}
fn pgftypesetpicturebox(engine:Refs, _token:CompactToken) -> Res<()> {
    let num = engine.read_register_index(false)?;
    let bx = engine.state.take_box_register(num).unwrap();
    let getdimens = engine.aux.memory.cs_interner_mut().from_str("rustex!pgf!get!dimens");
    engine.requeue(CompactToken::from_cs(getdimens))?;
    let minx = engine.read_dim(true)?;
    let miny = engine.read_dim(true)?;
    let maxx = engine.read_dim(true)?;
    let maxy = engine.read_dim(true)?;
    let (start,end) = if let TeXBox::H {start,end,..} = &bx {
        (start.clone(),end.clone())
    } else {
        unreachable!()
    };
    let node = RusTeXNode::PGFSvg { bx, minx, miny, maxx, maxy };
    match engine.stomach.data_mut().mode() {
        TeXMode::RestrictedHorizontal | TeXMode::Horizontal =>
            RusTeXStomach::add_node_h(engine, HNode::Custom(node)),
        _ => {
            let info = HBoxInfo::new_box(ToOrSpread::None);
            let bx = MathNode::Atom(MathAtom {
                sub:None,sup:None,
                nucleus: MathNucleus::Inner(MathKernel::Box(TeXBox::H {
                    info,children:vec!(HNode::Custom(node)).into(),start,end,preskip:None
                }))
            });
            RusTeXStomach::add_node_m(engine, bx)
        }
    }
    Ok(())
}
fn pgfliteral(_engine:Refs,_token:CompactToken) -> Res<()> { todo!("pgfliteral") }
fn pgfflushpath(engine:Refs, ret:&mut Vec<CompactToken>,_token:CompactToken) -> Res<()> {
    let path = engine.aux.memory.cs_interner_mut().from_str("pgf@sys@svgpath");
    let empty = engine.state.get_command(&engine.aux.memory.cs_interner_mut().from_str("pgfutil@empty")).cloned();
    match engine.state.get_command(&path) {
        Some(TeXCommand::Macro(m)) => ret.extend(m.expansion.0.iter().cloned()),
        _ => todo!("error"),
    };
    engine.state.set_command(engine.aux,path,empty,true);
    Ok(())
}
fn gbegin(engine:Refs,token:CompactToken) -> Res<()> {
    let mut attrs : HMap<&'static str,String> = HMap::default();
    let mut key = String::new();
    //let start = engine.mouth.start_ref();
    engine.read_braced_string(true,true,&token,&mut key)?;
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
            b"dur",b"restart",b"repeatCount",b"repeatDur",b"begin",b"end"])? {
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
                engine.read_string(true,&mut r)?;
                attrs.insert(std::str::from_utf8(s).unwrap(),r.into());
            }
        }
    }
    let bg = RusTeXNode::PGFGBegin {attrs,tag:key };
    add_node!(RusTeXStomach;engine, VNode::Custom(bg),HNode::Custom(bg),MathNode::Custom(bg));
    Ok(())
}
fn gend(engine:Refs,token:CompactToken) -> Res<()> {
    engine.skip_argument(&token)?;
    add_node!(RusTeXStomach;engine, VNode::Custom(RusTeXNode::PGFGEnd),HNode::Custom(RusTeXNode::PGFGEnd),MathNode::Custom(RusTeXNode::PGFGEnd));
    Ok(())
}
fn pgfbegin(_engine:Refs, _token:CompactToken) -> Res<()> { todo!("pgfbegin") }
fn pgfend(_engine:Refs, _token:CompactToken) -> Res<()> { todo!("pgfend") }