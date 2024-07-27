pub(crate) mod nodes;
pub(crate) mod annotations;
pub(crate) mod utils;
pub(crate) mod state;

use crate::engine::{Font, Refs, Res, Types};
use std::vec::IntoIter;
use tex_engine::commands::primitives::PRIMITIVES;
use tex_engine::pdflatex::nodes::{PDFColor, PDFDest, PDFNode};
use tex_engine::tex::nodes::NodeTrait;
use tex_engine::tex::numerics::{Dim32, Mu, Skip};
use tex_engine::engine::state::State;
use crate::html::{dim_to_num, dim_to_string, HTMLChild, HTMLNode, HTMLTag, mudim_to_string};
use tex_engine::engine::fontsystem::{Font as FontT, FontSystem};
use tex_engine::tex::nodes::boxes::{HBoxInfo, TeXBox, ToOrSpread, VBoxInfo};
use tex_engine::tex::nodes::horizontal::HNode;
use tex_engine::tex::nodes::math::{MathClass, MathFontStyle, MathGroup, MathKernel, MathNode, MathNucleus};
use tex_engine::tex::nodes::vertical::VNode;
use tex_engine::utils::HSet;
use tex_glyphs::glyphs::Glyph;
use crate::nodes::{LineSkip, RusTeXNode};
use crate::shipout::state::ShipoutState;
use crate::shipout::utils::{HNodes, MNode, MNodes, ShipoutMode, VNodes};

pub(crate) fn make_page<F:FnOnce(Refs,&mut ShipoutState) -> Res<()>>(engine:Refs,state:&mut ShipoutState,f:F) -> Res<HTMLNode> {
    let mut page = state.do_in_and(HTMLNode::page(),None,|state| {
        f(engine,state)
    })?;
    let page_width = engine.state.get_primitive_dim(PRIMITIVES.pdfpagewidth);
    let text_width = engine.state.get_primitive_dim(PRIMITIVES.hsize);
    let top_font = state.fonts.first().unwrap();
    page.font = Some((top_font.clone(),true));
    page.styles.insert("--rustex-text-width".into(),dim_to_num(text_width.0).into());
    page.styles.insert("--rustex-page-width".into(),dim_to_num(page_width.0).into());
    page.styles.insert("line-height".into(),"1.2".into()); // TODO
    Ok(page)
}

pub(crate) fn shipout(engine:Refs, n: VNode<Types>) -> Res<()> {
    //println!("Here: {}\n\n-------------------------------------------\n\n",n.display());
    match n {
        VNode::Box(TeXBox::V { children, .. }) => {
            let children = get_page_inner(children.into_vec());
            //for c in &children {
            //    println!("{}",c.display());
            //}
            state::split_state(engine, |engine, state| {
                do_vlist(engine, state, &mut children.into(), true)
            })?;
        }
        _ => unreachable!()
    }
    Ok(())
}

fn get_page_inner(children:Vec<VNode<Types>>) -> Vec<VNode<Types>> {
    let mut ret = Vec::new();
    let mut list:VNodes = children.into();
    while let Some(c) = list.next() {
        match c {
            VNode::Box(TeXBox::V {children,..}) if children.iter().any(|n| matches!(n,VNode::Custom(RusTeXNode::PageBegin))) =>
                ret.extend(children.into_vec().into_iter().filter(|p| !matches!(p,VNode::Custom(RusTeXNode::PageBegin|RusTeXNode::PageEnd)))),
            VNode::Box(TeXBox::V {children,..}) =>
                list.prefix(children.into_vec()),
            VNode::Box(TeXBox::H {children,..}) if hbox_works(&children) =>
                get_page_hbox(children, &mut ret,&mut list),
            VNode::VSkip(_) | VNode::VKern(_) | VNode::VFil | VNode::VFill | VNode::VFilneg | VNode::Vss |
            VNode::Penalty(_) | VNode::Mark(..) => (),
            VNode::Custom(RusTeXNode::PDFNode(PDFNode::PDFDest(..) | PDFNode::PDFCatalog(_) | PDFNode::PDFLiteral(_) |
            PDFNode::XForm(..) | PDFNode::Obj(..) | PDFNode::PDFOutline(_))) => (),
            VNode::Custom(RusTeXNode::PageBegin) => for c in list.by_ref() {
                if let VNode::Custom(RusTeXNode::PageEnd) = c { break }
                else { ret.push(c) }
            }
            _ => ret.push(c)
        }
    }
    ret
}

fn get_page_hbox(children:Box<[HNode<Types>]>,ret:&mut Vec<VNode<Types>>,list:&mut VNodes) {
    for c in children.into_vec().into_iter() {
        match c {
            HNode::HSkip(_) | HNode::Hss | HNode::Space | HNode::HKern(_) | HNode::HFil | HNode::HFill | HNode::HFilneg |
            HNode::Penalty(_) | HNode::Mark(_,_) |
            HNode::Custom(RusTeXNode::PDFNode(PDFNode::PDFDest(..) | PDFNode::PDFCatalog(_) | PDFNode::PDFLiteral(_) |
            PDFNode::XForm(..) | PDFNode::Obj(..) | PDFNode::PDFOutline(_))) => (),
            HNode::Custom(n @ (RusTeXNode::PDFNode(PDFNode::Color(_)) | RusTeXNode::FontChange(..)|RusTeXNode::FontChangeEnd)) =>
                ret.push(VNode::Custom(n)),
            HNode::Box(TeXBox::H {children,..}) if hbox_works(&children) => get_page_hbox(children,ret,list),
            HNode::Box(b) => list.prefix(vec!(VNode::Box(b))),
            _ => unreachable!()
        }
    }
}

fn hbox_works(children:&[HNode<Types>]) -> bool {
    children.iter().all(|n| matches!(n, HNode::HSkip(_) | HNode::Hss | HNode::Space | HNode::HKern(_) | HNode::HFil | HNode::HFill | HNode::HFilneg |
        HNode::Penalty(_) | HNode::Mark(_,_) |
        HNode::Custom(RusTeXNode::PDFNode(PDFNode::PDFDest(..) | PDFNode::PDFCatalog(_) | PDFNode::PDFLiteral(_) |
        PDFNode::XForm(..) | PDFNode::Obj(..) | PDFNode::PDFOutline(_))) |
        HNode::Custom(RusTeXNode::PDFNode(PDFNode::Color(_))) |
        HNode::Custom(RusTeXNode::FontChange(..)|RusTeXNode::FontChangeEnd) | HNode::Box(TeXBox::H {..}) | HNode::Box(TeXBox::V {..})))
}

/*
fn get_page_hbox(children:&Box<[HNode<Types>]>) -> bool {
    let children = children.clone().into_vec();
    children.iter().any(|n| match n {
        HNode::HSkip(_) | HNode::Space | HNode::HKern(_) | HNode::HFil | HNode::HFill | HNode::HFilneg |
        HNode::Penalty(_) | HNode::Mark(_,_) |
        HNode::Custom(RusTeXNode::PDFNode(PDFNode::PDFDest(..) | PDFNode::PDFCatalog(_) | PDFNode::PDFLiteral(_) |
                                          PDFNode::XForm(..) | PDFNode::Obj(..) | PDFNode::PDFOutline(_))) => false,
        HNode::Custom(RusTeXNode::PDFNode(PDFNode::Color(_))) => false,
        HNode::Box(TeXBox::H {children,..}) => get_page_hbox(children),
        _ => true
    })
}

 */

pub(crate) const ZERO_SKIP: Skip<Dim32> = Skip {base:Dim32(0),stretch:None,shrink:None};

fn do_vlist(engine:Refs, state:&mut ShipoutState, children:&mut VNodes,mut empty: bool) -> Res<()> {
    while let Some(c) = children.next() {
        match c {
            VNode::VKern(kn) => state.push_comment(format!("<div class=\"rustex-vkern\" style=\"margin-bottom:{};\"></div>",dim_to_string(kn))),
            VNode::VSkip(sk) => state.push_comment(format!("<div class=\"rustex-vskip\" style=\"margin-bottom:{};\"></div>",dim_to_string(sk.base))),
            VNode::VFil => state.push_comment("<div class=\"rustex-vfil\"></div>"),
            VNode::VFill => state.push_comment("<div class=\"rustex-vfill\"></div>"),
            VNode::Vss => state.push_comment("<div class=\"rustex-vss\"></div>"),
            VNode::VFilneg => (),
            VNode::Custom(RusTeXNode::ParagraphBegin{specs,start,end,lineskip,parskip}) => {
                let parskip = if empty {None} else {Some(parskip)};
                empty = false;
                nodes::do_paragraph(engine,state,children,specs,start,end,lineskip,parskip)?;
            }
            VNode::Custom(RusTeXNode::HAlignBegin) => {
                empty = false;
                nodes::do_halign(engine,state,children)?;
            }
            VNode::Custom(RusTeXNode::PDFNode(PDFNode::Color(act))) => annotations::do_color(state,engine,act),
            VNode::Custom(RusTeXNode::FontChange(font,true)) => {
                /*if state.in_content {
                    todo!()
                }*/
                *state.fonts.last_mut().unwrap() = font;
            }
            VNode::Custom(RusTeXNode::PDFNode(PDFNode::PDFStartLink(link))) =>
                annotations::do_link(link,state),
            VNode::Custom(RusTeXNode::PDFNode(PDFNode::PDFEndLink)) =>
                annotations::close_link(state),
            VNode::Custom(RusTeXNode::FontChange(font,false)) => annotations::do_font(state,&engine.fontsystem.glyphmaps,font),
            VNode::Custom(RusTeXNode::FontChangeEnd) => annotations::close_font(state),
            VNode::Custom(RusTeXNode::AnnotBegin {start,attrs,styles,tag}) => annotations::do_annot(state,start,tag,attrs,styles),
            VNode::Custom(RusTeXNode::AnnotEnd(end)) => annotations::close_annot(state,end),
            VNode::Custom(RusTeXNode::PGFGBegin {..}|RusTeXNode::PGFGEnd) => (), // TODO?
            VNode::Custom(RusTeXNode::PGFEscape(bx)) => children.prefix(vec!(VNode::Box(bx))),
            VNode::Custom(RusTeXNode::PDFNode(PDFNode::PDFOutline(_) | PDFNode::PDFPageAttr(_) | PDFNode::PDFPagesAttr(_) |
                                              PDFNode::PDFCatalog(_)| PDFNode::PDFSave| PDFNode::PDFAnnot(_) | PDFNode::PDFLiteral(_) | PDFNode::XForm(_) | PDFNode::Obj(_))) | VNode::Penalty(_) => (),
            c => {
                empty = false;
                do_v(engine,state,c)?;
            }

        }
    }
    Ok(())
}


fn do_hlist(engine:Refs, state:&mut ShipoutState, children:&mut HNodes) -> Res<()> {
    while let Some(c) = children.next() {
        match c {
            HNode::HKern(kn) => {
                if kn!= Dim32(0) {state.push_comment(format!("<div class=\"rustex-hkern\" style=\"margin-left:{};\"></div>",dim_to_string(kn)))}
            }
            HNode::Space if state.mode() == ShipoutMode::H{escape:true} =>
                state.push_comment(format_args!("<div class=\"rustex-space-in-hbox\"></div>")),
            HNode::Space => state.push_space(),
            HNode::HSkip(sk) => {
                state.push_comment(format!("<div class=\"rustex-hskip\" style=\"margin-left:{};\"></div>",dim_to_string(sk.base)))
            }
            HNode::HFil => state.push_comment("<div class=\"rustex-hfil\"></div>"),
            HNode::HFill => state.push_comment("<div class=\"rustex-hfill\"></div>"),
            HNode::Hss => state.push_comment("<div class=\"rustex-hss\"></div>"),
            HNode::HFilneg => (),
            HNode::Custom(RusTeXNode::PDFNode(PDFNode::Color(act))) =>  annotations::do_color(state,engine,act),
            HNode::Custom(RusTeXNode::FontChange(font,true)) => {
                if state.in_content {
                    //TODO ???
                }
                *state.fonts.last_mut().unwrap() = font;
            }
            HNode::Custom(RusTeXNode::PDFNode(PDFNode::PDFStartLink(link))) =>
                annotations::do_link(link,state),
            HNode::Custom(RusTeXNode::PDFNode(PDFNode::PDFEndLink)) =>
                annotations::close_link(state),
            HNode::Custom(RusTeXNode::FontChange(font,false)) => annotations::do_font(state,&engine.fontsystem.glyphmaps,font),
            HNode::Custom(RusTeXNode::FontChangeEnd) => annotations::close_font(state),
            HNode::Custom(RusTeXNode::AnnotBegin {start,attrs,styles,tag}) => annotations::do_annot(state,start,tag,attrs,styles),
            HNode::Custom(RusTeXNode::AnnotEnd(end)) => annotations::close_annot(state,end),
            HNode::Custom(RusTeXNode::PGFGBegin{..}|RusTeXNode::PGFGEnd) => (),  // TODO???
            HNode::Custom(RusTeXNode::PDFNode(PDFNode::PDFOutline(_) | PDFNode::PDFPageAttr(_) | PDFNode::PDFPagesAttr(_) |
                                              PDFNode::PDFCatalog(_) | PDFNode::PDFSave | PDFNode::PDFAnnot(_) | PDFNode::PDFLiteral(_) | PDFNode::XForm(_) | PDFNode::Obj(_))) | HNode::Penalty(_) => (),
            HNode::Custom(RusTeXNode::PGFEscape(bx)) => do_h(engine,state,HNode::Box(bx))?,
            c => do_h(engine,state,c)?
        }
    }
    Ok(())
}

fn do_v(engine:Refs, state:&mut ShipoutState, n: VNode<Types>) -> Res<()> {
    match n {
        VNode::Box(bx@ TeXBox::V { info: VBoxInfo::VBox {raised:Some(_),..} | VBoxInfo::VTop {raised:Some(_),..},..} | bx @ TeXBox::H {info:HBoxInfo::HBox {raised:Some(_),..},..}) =>
            nodes::do_raise(state,bx,|n,state| do_v(engine,state,VNode::Box(n))),
        VNode::Box(bx@ TeXBox::V { info: VBoxInfo::VBox {moved_left:Some(_),..} | VBoxInfo::VTop{moved_left:Some(_),..},..} | bx @ TeXBox::H{info: HBoxInfo::HBox{moved_left:Some(_),..},..}) =>
            nodes::do_moveleft(state,bx,|n,state| do_v(engine,state,VNode::Box(n))),
        VNode::Box(bx@ TeXBox::V { info: VBoxInfo::VBox { .. },.. }) => nodes::do_vbox(state,engine,bx),
        VNode::Box(bx@ TeXBox::V { info: VBoxInfo::VTop { .. },.. }) => nodes::do_vtop(state,engine,bx),
        VNode::Box(bx@ TeXBox::H { info: HBoxInfo::HBox { .. },.. }) => nodes::do_hbox(state,engine,bx),
        VNode::Box(TeXBox::H { info: HBoxInfo::HAlignRow,children,start,end,.. }) => {
            // TODO
            let bx = TeXBox::H {info: HBoxInfo::new_box(ToOrSpread::None),children,start,end,preskip:None};
            nodes::do_hbox(state,engine,bx)
        }
        VNode::Custom(RusTeXNode::PDFNode(PDFNode::PDFDest(PDFDest{id,..}))) => Ok(nodes::do_pdfdest(state, id)),
        VNode::Whatsit(wi) => wi.call(engine),
        VNode::HRule {ref start,ref end,ref width,..} => {
            let height = n.height();
            let depth = n.depth();
            Ok(nodes::hrule(state,*start,*end,*width,height,depth))
        }
        VNode::Custom(RusTeXNode::PDFNode(PDFNode::PDFMatrix {scale, rotate,skewx,skewy})) =>
            Ok(annotations::do_matrix(state,scale,rotate,skewx,skewy)),
        VNode::Custom(RusTeXNode::PDFNode(PDFNode::PDFRestore)) => Ok(annotations::reset_matrix(state)),
        VNode::Custom(RusTeXNode::PDFNode(PDFNode::XImage(_))) => {
            todo!();
            /*
            let mut attr = String::new();
            if let Some(w) = img.width {
                attr.push_str(&format!(" width=\"{}\"",dim_to_string(w)));
            }
            if let Some(h) = img.height {
                attr.push_str(&format!(" height=\"{}\"",dim_to_string(h)));
            }
            state.push_text(format!("<img src=\"{}\"{}/>",img.filepath.display(),attr));

             */
        }
        VNode::Box(TeXBox::H { info: HBoxInfo::HAlignCell{..}|HBoxInfo::ParLine {..},children,start,end,.. }) => {
            // TODO
            let bx = TeXBox::H {info: HBoxInfo::new_box(ToOrSpread::None),children,start,end,preskip:None};
            nodes::do_hbox(state,engine,bx)
        }
        VNode::Custom(RusTeXNode::PGFSvg {bx,minx,miny,maxx,maxy}) =>
            nodes::do_svg(engine,state,bx,minx,miny,maxx,maxy),
        VNode::Leaders(_) => Ok(()), // TODO?
        VNode::Mark(..) | VNode::Custom(RusTeXNode::PageBegin | RusTeXNode::PageEnd | RusTeXNode::HAlignEnd) => Ok(()),
        VNode::Custom(RusTeXNode::Literal(s)) => {state.push_comment(s);Ok(())}
        _ => panic!("Here: {:?}",n)
    }
}

fn do_h(engine:Refs, state:&mut ShipoutState, n: HNode<Types>) -> Res<()> {
    match n {
        HNode::Box(bx@ TeXBox::V { info: VBoxInfo::VBox {raised:Some(_),..} | VBoxInfo::VTop {raised:Some(_),..},..} | bx @ TeXBox::H {info: HBoxInfo::HBox {raised:Some(_),..},..}) =>
            nodes::do_raise(state,bx,|n,state| do_h(engine,state,HNode::Box(n))),
        HNode::Box(bx@ TeXBox::V { info: VBoxInfo::VBox {moved_left:Some(_),..} | VBoxInfo::VTop {moved_left:Some(_),..},..} | bx @ TeXBox::H{info: HBoxInfo::HBox {moved_left:Some(_),..} ,..}) =>
            nodes::do_moveleft(state,bx,|n,state| do_h(engine,state,HNode::Box(n))),
        HNode::Box(bx@ TeXBox::V { info: VBoxInfo::VBox { .. },.. }) => nodes::do_vbox(state,engine,bx),
        HNode::Box(bx@ TeXBox::V { info: VBoxInfo::VTop { .. },.. }) => nodes::do_vtop(state,engine,bx),
        HNode::Box(bx@ TeXBox::H { info: HBoxInfo::HBox { .. },.. }) => nodes::do_hbox(state,engine,bx),
        HNode::Box(TeXBox::H {info:HBoxInfo::ParIndent(d),..}) => {
            if d > Dim32(0) { state.push_comment(format_args!("<div class=\"rustex-parindent\" style=\"margin-left:{}\"></div>",dim_to_string(d))) }
            Ok(())
        }
        HNode::Box(TeXBox::H { info: HBoxInfo::HAlignCell{..}|HBoxInfo::ParLine {..},children,start,end,.. }) => {
            // TODO
            let bx = TeXBox::H {info: HBoxInfo::new_box(ToOrSpread::None),children,start,end,preskip:None};
            nodes::do_hbox(state,engine,bx)
        }
        HNode::Custom(RusTeXNode::PDFNode(PDFNode::PDFDest(PDFDest{id,..}))) => Ok(nodes::do_pdfdest(state, id)),
        HNode::Whatsit(wi) => wi.call(engine),
        HNode::Char {char,font,..} => {
            if font == engine.fontsystem.null() { return Ok(()) }
            state.in_content = true;
            let glyphtable = engine.fontsystem.glyphmaps.get_glyphlist(font.filename());
            let glyph = glyphtable.get(char);
            if !glyph.is_defined() {
                nodes::do_missing_glyph(state,glyph.name(),char,&font);
            } else {
                state.push_glyph(glyph)
            }
            Ok(())
        }
        HNode::Accent {char,font,accent} => {
            if font == engine.fontsystem.null() { return Ok(()) }
            state.in_content = true;
            let glyphtable = engine.fontsystem.glyphmaps.get_glyphlist(font.filename());
            let glyph = glyphtable.get(char);
            let accent = glyphtable.get(accent);
            if !glyph.is_defined() {
                nodes::do_missing_glyph(state,glyph.name(),char,&font);
            } else {
                if accent.is_defined() {
                    state.push_glyph(accent) // TODO properly
                }
                state.push_glyph(glyph)
            }
            Ok(())
        }
        HNode::VRule {width,height,depth,start,end} => {
            nodes::vrule(state,start,end,width.unwrap_or(Dim32(26214)),height,depth);
            Ok(())
        }
        HNode::Custom(RusTeXNode::PDFNode(PDFNode::PDFMatrix {scale, rotate,skewx,skewy})) =>
            Ok(annotations::do_matrix(state,scale,rotate,skewx,skewy)),
        HNode::Custom(RusTeXNode::PDFNode(PDFNode::PDFRestore)) => Ok(annotations::reset_matrix(state)),
        HNode::MathGroup(mg) if mathlist_is_h(&mg.children) => do_math_in_h(state,engine,mg),
        HNode::MathGroup(mg) => nodes::do_math(state,engine,mg),
        HNode::Custom(RusTeXNode::PGFSvg {bx,minx,miny,maxx,maxy}) =>
            nodes::do_svg(engine,state,bx,minx,miny,maxx,maxy),
        HNode::Leaders(_) => Ok(()), // TODO?
        HNode::Custom(RusTeXNode::PDFNode(p@PDFNode::XImage(_))) => {
            let width = p.width();
            let height = p.height();
            let PDFNode::XImage(img) = p else {unreachable!()};
            state.push_child(HTMLChild::Image {
                width,height,filepath:img.filepath
            });
            Ok(())
            /*
            let mut attr = String::new();
            if let Some(w) = img.width {
                attr.push_str(&format!(" width=\"{}\"",dim_to_string(w)));
            }
            if let Some(h) = img.height {
                attr.push_str(&format!(" height=\"{}\"",dim_to_string(h)));
            }
            state.push_text(format!("<img src=\"{}\"{}/>",img.filepath.display(),attr));

             */
        }
        HNode::Custom(RusTeXNode::Literal(s)) => {state.push_comment(s);Ok(())}
        _ => todo!("{:?}",n)
    }
}

pub(crate) fn do_math_in_h(state:&mut ShipoutState,engine:Refs,bx:MathGroup<Types>) -> Res<()> {
    let mut iter: MNodes = bx.children.into_vec().into();
    match bx.display {
        Some((pre,post)) => {
            let mut node = HTMLNode::new(HTMLTag::Display);
            if pre.base != Dim32(0) {
                node.styles.insert("margin-top".into(),dim_to_string(pre.base).into());
            }
            if post.base != Dim32(0) {
                node.styles.insert("margin-bottom".into(),dim_to_string(post.base).into());
            }
            node.classes.push("rustex-display".into());
            state.do_in(node,None,|state| {
                do_mathlist_in_h(state,engine,&mut iter)
            })
        }
        None => do_mathlist_in_h(state,engine,&mut iter)
    }
}
pub(crate) fn do_mathlist_in_h(state:&mut ShipoutState,engine:Refs,iter:&mut MNodes) -> Res<()> {
    while let Some(c) = iter.next() {match c {
        MathNode::HKern(kn) => {
            if kn!= Dim32(0) {state.push_comment(format!("<div class=\"rustex-hkern\" style=\"margin-left:{};\"></div>",dim_to_string(kn)))}
        }
        MathNode::HSkip(sk) => {
            state.push_comment(format!("<div class=\"rustex-hskip\" style=\"margin-left:{};\"></div>",dim_to_string(sk.base)))
        }
        MathNode::HFil => state.push_comment("<div class=\"rustex-hfil\"></div>"),
        MathNode::HFill => state.push_comment("<div class=\"rustex-hfill\"></div>"),
        MathNode::Hss => state.push_comment("<div class=\"rustex-hss\"></div>"),
        MathNode::HFilneg => (),
        MathNode::Space if state.mode() == ShipoutMode::H{escape:true} =>
            state.push_comment(format_args!("<div class=\"rustex-space-in-hbox\"></div>")),
        MathNode::Space => state.push_space(),
        MathNode::MSkip {skip,..} => {
            state.push_comment(format!("<div class=\"rustex-hskip\" style=\"margin-left:{};\"></div>",mudim_to_string(skip.base)))
        }
        MathNode::MKern {kern,..} => {
            state.push_comment(format!("<div class=\"rustex-hkern\" style=\"margin-left:{};\"></div>",mudim_to_string(kern)))
        }
        MathNode::Custom(RusTeXNode::PDFNode(PDFNode::Color(act))) => annotations::do_color(state,engine,act),
        MathNode::Custom(RusTeXNode::FontChange(font,true)) => {
            if state.in_content {
                todo!()
            }
            *state.fonts.last_mut().unwrap() = font;
        }
        MathNode::Custom(RusTeXNode::FontChange(font,false)) => annotations::do_font(state,&engine.fontsystem.glyphmaps,font),
        MathNode::Custom(RusTeXNode::FontChangeEnd) => annotations::close_font(state),
        MathNode::Custom(RusTeXNode::AnnotBegin {start,attrs,styles,tag}) => annotations::do_annot(state,start,tag,attrs,styles),
        MathNode::Custom(RusTeXNode::AnnotEnd(end)) => annotations::close_annot(state,end),
        MathNode::Custom(RusTeXNode::PDFNode(PDFNode::PDFOutline(_) | PDFNode::PDFPageAttr(_) | PDFNode::PDFPagesAttr(_) |
                                             PDFNode::PDFCatalog(_)| PDFNode::PDFAnnot(_) | PDFNode::XForm(_) | PDFNode::PDFLiteral(_) | PDFNode::Obj(_))) | MathNode::Penalty(_) => (),
        MathNode::VRule {width,height,depth,start,end} =>
            nodes::vrule(state,start,end,width.unwrap_or(Dim32(26214)),height,depth),
        MathNode::Custom(RusTeXNode::Literal(s)) => {state.push_comment(s)}
        MathNode::Atom(a) => match a.nucleus {
            MathNucleus::VCenter {start,end,children,..} =>
            // TODO to/spread for VCenter
                nodes::do_vcenter(state,engine,start,end,children)?,
            MathNucleus::Simple{kernel,..} | MathNucleus::Inner(kernel) =>
                do_kernel_in_h(state,engine,kernel,iter)?,
            MathNucleus::Overline(kernel) => {
                let mut node = HTMLNode::new(HTMLTag::Annot(state.mode(),None));
                node.styles.insert("text-decoration".into(),"overline".into());
                match kernel {
                    MathKernel::Empty => (),
                    MathKernel::List {children,..} => state.do_in(node,None,|state|
                        do_mathlist_in_h(state,engine,&mut children.into_vec().into())
                    )?,
                    kernel => state.do_in(node,None,|state|
                            do_kernel_in_h(state,engine,kernel,iter)
                    )?
                }
            }
            MathNucleus::Underline(kernel) => {
                let mut node = HTMLNode::new(HTMLTag::Annot(state.mode(),None));
                node.styles.insert("text-decoration".into(),"underline".into());
                match kernel {
                    MathKernel::Empty => (),
                    MathKernel::List {children,..} => state.do_in(node,None,|state|
                        do_mathlist_in_h(state,engine,&mut children.into_vec().into())
                    )?,
                    kernel => state.do_in(node,None,|state|
                        do_kernel_in_h(state,engine,kernel,iter)
                    )?
                }
            }
            _ => unreachable!()
        }
        o => todo!("math in h: {:?}",o)
    }}
    Ok(())
}

pub(crate) fn do_kernel_in_h(state:&mut ShipoutState,engine:Refs,k:MathKernel<Types,MathFontStyle<Types>>,iter: &mut MNodes) -> Res<()> {
    match k {
        MathKernel::Empty => (),
        MathKernel::List{children,..} => iter.prefix(children.into_vec()),
        MathKernel::Box(b) => do_h(engine,state,HNode::Box(b))?,
        _ => unreachable!()
    }
    Ok(())
}

fn do_mathlist(engine:Refs, state:&mut ShipoutState, children:&mut MNodes) -> Res<()> {
    let mut currclass : Option<(MathClass,bool,HTMLNode)> = None;
    macro_rules! flush {
        () => {
            if let Some((_,_,node)) = std::mem::take(&mut currclass) {
                state.push(node)
            }
        }
    }
    while let Some(c) = children.next() {
        match c {
            MathNode::MSkip {skip,..} => {
                flush!();
                if skip.base.0 < 0 {
                    state.push_comment(format!("<mspace class=\"rustex-mskip\" style=\"margin-left:{}\"></mspace>", mudim_to_string(skip.base)));
                } else {
                    state.push_comment(format!("<mspace class=\"rustex-mskip\" width=\"{}\"></mspace>", mudim_to_string(skip.base)));
                }
            }
            MathNode::MKern{kern,..} => {
                flush!();
                if kern.0 < 0 {
                    state.push_comment(format!("<mspace class=\"rustex-mkern\" style=\"margin-left:{}\"></mspace>", mudim_to_string(kern)))
                } else {
                    state.push_comment(format!("<mspace class=\"rustex-mkern\" width=\"{}\"></mspace>", mudim_to_string(kern)));
                }
            }
            MathNode::Space => {
                flush!();
                state.push_comment(format!("<mspace class=\"rustex-mkern\" width=\"{}\"></mspace>", mudim_to_string(Mu(65536 * 9))));
            }
            MathNode::HSkip(sk) => {
                if sk.base != Dim32(0) {
                    flush!();
                    if sk.base.0 < 0 {
                        state.push_comment(format!("<mspace class=\"rustex-hskip\" style=\"margin-left:{}\"></mspace>", dim_to_string(sk.base)));
                    } else {
                        state.push_comment(format!("<mspace class=\"rustex-hskip\" width=\"{}\"></mspace>", dim_to_string(sk.base)));
                    }
                }
            },
            MathNode::HKern(d) => {
                if d != Dim32(0) {
                    flush!();
                    if d.0 < 0 {
                        state.push_comment(format!("<mspace class=\"rustex-hkern\" style=\"margin-left:{}\"></mspace>", dim_to_string(d)));
                    } else {
                        state.push_comment(format!("<mspace class=\"rustex-hkern\" width=\"{}\"></mspace>", dim_to_string(d)));
                    }
                }
            },
            MathNode::Atom(a) if a.sup.is_none() && a.sub.is_none() => match a.nucleus {
                MathNucleus::Simple{kernel:MathKernel::Char {char,style:MathFontStyle{font,cramped,..}},cls,..} => {
                    nodes::merge_mathchar(engine, state, &mut currclass, char, cls, font,cramped);
                }
                n => {
                    flush!();
                    nodes::do_nucleus(engine,state,n)?;
                }
            }
            MathNode::Over {start,end,left,right,top,bottom,sep} => {
                flush!();
                nodes::do_over(state, engine, top, bottom, sep, left, right, start, end)?;
            }
            MathNode::Atom(a) => {
                flush!();
                nodes::do_sub_sup(engine,state,a)?
            }
            MathNode::Choice(c) => children.prefix(c.0.into_vec()),
            MathNode::Custom(RusTeXNode::PDFNode(PDFNode::Color(act))) => {
                flush!();
                annotations::do_color(state,engine,act)
            },
            MathNode::Custom(RusTeXNode::PDFNode(PDFNode::PDFStartLink(link))) => {
                flush!();
                annotations::do_link(link, state)
            }
            MathNode::Custom(RusTeXNode::PDFNode(PDFNode::PDFEndLink)) => {
                flush!();
                annotations::close_link(state)
            }
            MathNode::VRule {width,height,depth,start,end} => {
                flush!();
                nodes::vrule(state,start,end,width.unwrap_or(Dim32(26214)),height,depth)
            }
            MathNode::Whatsit(wi) => wi.call(engine)?,
            MathNode::Custom(RusTeXNode::FontChange(font,false)) => {flush!();annotations::do_font(state,&engine.fontsystem.glyphmaps,font)}
            MathNode::Custom(RusTeXNode::FontChangeEnd) => {flush!();annotations::close_font(state)}
            MathNode::Custom(RusTeXNode::AnnotBegin {start,attrs,styles,tag}) => {flush!();annotations::do_annot(state,start,tag,attrs,styles)}
            MathNode::Custom(RusTeXNode::AnnotEnd(end)) => {flush!();annotations::close_annot(state,end)}
            MathNode::HFil | MathNode::HFill | MathNode::Hss | MathNode::HFilneg | MathNode::Penalty(_) => (),
            MathNode::Custom(RusTeXNode::PDFNode(PDFNode::PDFOutline(_) | PDFNode::PDFPageAttr(_) | PDFNode::PDFPagesAttr(_) |
                                                 PDFNode::PDFCatalog(_) | PDFNode::PDFSave | PDFNode::PDFAnnot(_) | PDFNode::PDFLiteral(_) | PDFNode::XForm(_) | PDFNode::Obj(_))) => (),
            MathNode::Leaders(_) => (), // TODO?
            MathNode::Custom(RusTeXNode::Literal(s)) => {state.push_comment(s)}
            MathNode::Custom(RusTeXNode::PDFNode(PDFNode::PDFDest(PDFDest{id,..}))) => nodes::do_pdfdest(state, id),
            o => todo!(" {:?}",o)
        }
    }
    if let Some((_,_,node)) = currclass { state.push(node) }
    Ok(())
}

fn mathlist_is_h(ls: &[MNode]) -> bool {
    ls.iter().all(|n| match n {
        MathNode::Atom(a) => {
            a.sup.is_none() && a.sub.is_none() && match &a.nucleus {
                MathNucleus::VCenter {..} => true,
                MathNucleus::Simple{kernel,..} |
                MathNucleus::Inner(kernel) |
                MathNucleus::Underline(kernel) |
                MathNucleus::Overline(kernel) => kernel_is_h(kernel),
                _ => false
            }
        }
        MathNode::Custom(RusTeXNode::PDFNode(PDFNode::PDFOutline(_) | PDFNode::PDFPageAttr(_) | PDFNode::PDFPagesAttr(_) |
                                             PDFNode::PDFCatalog(_) | PDFNode::PDFSave | PDFNode::PDFAnnot(_) | PDFNode::PDFLiteral(_) | PDFNode::XForm(_) | PDFNode::Obj(_))) => true,
        MathNode::HSkip(_) | MathNode::HFil | MathNode::HFill | MathNode::HFilneg | MathNode::Hss | MathNode::Space |
            MathNode::MSkip {..} | MathNode::MKern {..} | MathNode::HKern(_) | MathNode::Penalty(_) |
            MathNode::Mark(..) | MathNode::VRule {..} | MathNode::Whatsit(_) | MathNode::Custom(_) => true,
        _ => false
    })

}

fn kernel_is_h(k:&MathKernel<Types,MathFontStyle<Types>>) -> bool {
    match k {
        MathKernel::Empty => true,
        MathKernel::Box(_) => true,
        MathKernel::Char {..} => false,
        MathKernel::List {children,..} => mathlist_is_h(children),
    }
}