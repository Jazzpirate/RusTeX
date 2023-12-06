pub mod pdftexnodes;

use crate::{cmtodo, cmtodos};
use crate::engine::{EngineExtension, EngineReferences, EngineTypes, TeXEngine};
use crate::engine::filesystem::{File, FileSystem};
use crate::engine::mouth::pretokenized::{ExpansionContainer, Tokenizer};
use crate::tex::catcodes::CommandCode;
use super::primitives::*;
use crate::engine::utils::memory::MemoryManager;
use crate::tex::token::Token;
use crate::engine::gullet::Gullet;
use crate::tex::numerics::NumSet;
use std::fmt::{Display, Formatter, Write};
use crate::commands::pdftex::pdftexnodes::{PDFColor, PDFExtension, PDFLiteralOption, PDFNodeTrait, PDFObj, PDFXForm};
use crate::engine::state::State;
use crate::tex::nodes::{NodeTrait, PreShipoutNode, PreShipoutNodeTrait, ReadableNode, ShipoutNode, TeXBox, TopNodeTrait};
use crate::tex::types::NodeType;
use crate::utils::Ptr;
use crate::engine::stomach::Stomach;


#[inline(always)]
pub fn pdftexversion<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) -> <ET::Num as NumSet>::Int {
    <ET::Num as NumSet>::Int::from(140)
}
#[inline(always)]
pub fn pdfmajorversion<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) -> <ET::Num as NumSet>::Int {
    <ET::Num as NumSet>::Int::from(1)
}

pub fn pdftexrevision<ET:EngineTypes>(engine: &mut EngineReferences<ET>,exp:&mut Vec<ET::Token>,tk:ET::Token) {
    exp.push(ET::Token::from_char_cat(b'2'.into(),CommandCode::Other));
    exp.push(ET::Token::from_char_cat(b'5'.into(),CommandCode::Other));
}

pub fn pdfcolorstack<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token)
    where ET::Extension : PDFExtension<ET> {
    let index = engine.read_int(false).into();
    if index < 0 {todo!("throw error")}
    let index = index as usize;
    let kw = engine.read_keywords(&[b"push",b"pop",b"set",b"current"]);
    match kw {
        Some(b"current") => *engine.aux.extension.current_colorstack() = index,
        Some(b"pop") => {
            let stack = engine.aux.extension.colorstacks();
            if index >= stack.len() {
                todo!("throw error")
            }
            stack[index].pop();
        }
        Some(b"set") => {
            let mut color = String::new();
            engine.read_braced_string(false,&mut color);
            let color = PDFColor::parse(color);
            let stack = engine.aux.extension.colorstacks();
            *stack[index].last_mut().unwrap() = color;
        }
        Some(b"push") => {
            let mut color = String::new();
            engine.read_braced_string(false,&mut color);
            let color = PDFColor::parse(color);
            let stack = engine.aux.extension.colorstacks();
            stack[index].push(color);
        }
        _ => todo!("error")
    }
}


pub fn ifincsname<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) -> bool {
    *engine.gullet.csnames() > 0
}
pub fn ifpdfabsnum<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) -> bool {
    let first = engine.read_int(false);
    let rel = match engine.read_chars(&[b'=',b'<',b'>']) {
        Ok(b) => b,
        _ => todo!("throw error")
    };
    let second = engine.read_int(false);

    let first = if first < <<ET as EngineTypes>::Num as NumSet>::Int::default() {
        -first
    } else {
        first
    };
    let second = if second < <<ET as EngineTypes>::Num as NumSet>::Int::default() {
        -second
    } else {
        second
    };
    match rel {
        b'=' => first == second,
        b'<' => first < second,
        b'>' => first > second,
        _ => unreachable!()
    }
}
pub fn ifpdfabsdim<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) -> bool {
    let first = engine.read_dim(false);
    let rel = match engine.read_chars(&[b'=',b'<',b'>']) {
        Ok(b) => b,
        _ => todo!("throw error")
    };
    let second = engine.read_dim(false);
    let first = if first < <<ET as EngineTypes>::Num as NumSet>::Dim::default() {
        -first
    } else {
        first
    };
    let second = if second < <<ET as EngineTypes>::Num as NumSet>::Dim::default() {
        -second
    } else {
        second
    };
    match rel {
        b'=' => first == second,
        b'<' => first < second,
        b'>' => first > second,
        _ => unreachable!()
    }
}

pub fn pdfcreationdate<ET:EngineTypes>(engine: &mut EngineReferences<ET>,exp:&mut Vec<ET::Token>,tk:ET::Token) {
    use chrono::{Datelike,Timelike};
    let dt = engine.aux.start_time;
    let mut f = |t| exp.push(t);
    let mut tk = Tokenizer::new(&mut f);
    write!(tk,"D:{}{:02}{:02}{:02}{:02}{:02}{}'",
                      dt.year(),dt.month(),dt.day(),dt.hour(),dt.minute(),dt.second(),
                      dt.offset().to_string().replace(":","'")).unwrap();
}

pub fn pdffilesize<ET:EngineTypes>(engine: &mut EngineReferences<ET>,exp:&mut Vec<ET::Token>,tk:ET::Token) {
    let mut filename = engine.aux.memory.get_string();
    engine.read_braced_string(false,&mut filename);
    let file = engine.filesystem.get(&filename);
    engine.aux.memory.return_string(filename);
    if file.exists() {
        for u in file.size().to_string().bytes() {
            exp.push(ET::Token::from_char_cat(u.into(),CommandCode::Other));
        }
    }
}

pub fn pdfglyphtounicode<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) {
    // TODO
    super::tex::skip_argument(engine);
    super::tex::skip_argument(engine);
}

pub fn pdfmatch<ET:EngineTypes>(engine: &mut EngineReferences<ET>,exp:&mut Vec<ET::Token>,tk:ET::Token)
    where ET::Extension : PDFExtension<ET> {
    let icase = engine.read_keyword(b"icase");
    let subcount = if engine.read_keyword(b"subcount") {
        engine.read_int(false).into()
    } else { -1 };
    let mut pattern_string = engine.aux.memory.get_string();
    let mut target_string = engine.aux.memory.get_string();
    if icase {pattern_string.push_str("(?i)");}
    engine.read_braced_string(false,&mut pattern_string);
    engine.read_braced_string(false,&mut target_string);
    let pdfmatches = engine.aux.extension.pdfmatches();
    pdfmatches.clear();

    match regex::Regex::new(&pattern_string) {
        Err(_) => {
            exp.push(ET::Token::from_char_cat(b'-'.into(),CommandCode::Other));
            exp.push(ET::Token::from_char_cat(b'1'.into(),CommandCode::Other));
        }
        Ok(reg) => match reg.captures_iter(&target_string).next() {
            None =>
                exp.push(ET::Token::from_char_cat(b'0'.into(),CommandCode::Other)),
            Some(capture) => {
                let cap = capture.get(0).unwrap();
                pdfmatches.push(format!("{}->{}",cap.start(),cap.as_str()));
                for cap in capture.iter().skip(1) {
                    match cap {
                        None => pdfmatches.push("-1".to_string()),
                        Some(cap) => {
                            pdfmatches.push(format!("{}->{}",cap.start(),cap.as_str()));
                        }
                    }
                }
                exp.push(ET::Token::from_char_cat(b'1'.into(),CommandCode::Other));
            }
        }
    }
}
pub fn parse_pdfobj<ET:EngineTypes>(engine:&mut EngineReferences<ET>) -> usize
    where ET::Extension : PDFExtension<ET> {
    match engine.read_keywords(&[b"reserveobjnum",b"useobjnum",b"stream"]) {
        Some(b"reserveobjnum") => {
            engine.aux.extension.pdfobjs().push(PDFObj(String::new()));
            engine.aux.extension.pdfobjs().len() - 1
        }
        Some(b"useobjnum") => {
            let num = engine.read_int(false).into();
            if num < 0 {todo!("throw error")}
            let num = num as usize;
            if num >= engine.aux.extension.pdfobjs().len() {todo!("throw error")}
            let mut str = String::new();
            engine.read_braced_string(false,&mut str);
            engine.aux.extension.pdfobjs()[num] = PDFObj(str);
            num
        }
        Some(b"stream") => {
            if engine.read_keyword(b"attr") {
                // TODO
            }
            let mut str = String::new();
            engine.read_braced_string(false,&mut str);
            engine.aux.extension.pdfobjs().push(PDFObj(str));
            engine.aux.extension.pdfobjs().len() - 1
        }
        _ => todo!("throw error"),
    }
}
pub fn pdfobj<ET:EngineTypes>(engine:&mut EngineReferences<ET>, token:ET::Token)
                             -> Option<Box<dyn FnOnce(&mut EngineReferences<ET>) -> Option<ShipoutNode<ET>>>>
    where ET::Extension : PDFExtension<ET> {
    parse_pdfobj(engine);
    None
}
pub fn pdfobj_immediate<ET:EngineTypes>(engine:&mut EngineReferences<ET>,token:ET::Token)
    where ET::Extension : PDFExtension<ET>,
          ET::PreCustomNode:PDFNodeTrait<ET> {
    let num = parse_pdfobj(engine);
    let obj = engine.aux.extension.pdfobjs()[num].clone();
    ET::Stomach::add_node(engine,PreShipoutNode::Custom(ET::PreCustomNode::from_pdfobj(obj)));
}

pub fn pdfrefobj<ET:EngineTypes>(engine:&mut EngineReferences<ET>,token:ET::Token)
    where ET::Extension : PDFExtension<ET>,
          ET::PreCustomNode:PDFNodeTrait<ET> {
    let num = engine.read_int(false).into();
    if num < 0 {todo!("throw error")}
    match engine.aux.extension.pdfobjs().get(num as usize) {
        None => todo!("throw error"),
        Some(o) => {
            let o = o.clone();
            ET::Stomach::add_node(engine, PreShipoutNode::Custom(ET::PreCustomNode::from_pdfobj(o)))
        }
    }
}

#[inline(always)]
pub fn pdflastobj<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) -> <ET::Num as NumSet>::Int
    where ET::Extension : PDFExtension<ET> {
    <ET::Num as NumSet>::Int::from((engine.aux.extension.pdfobjs().len() as i32) - 1)
}

pub fn parse_pdfxform<ET:EngineTypes>(engine:&mut EngineReferences<ET>) -> usize
    where ET::Extension : PDFExtension<ET> {
    let mut attr = String::new();
    if engine.read_keyword(b"attr") {
        engine.read_braced_string(true,&mut attr);
    }
    let mut resources = String::new();
    if engine.read_keyword(b"resources") {
        engine.read_braced_string(true,&mut attr);
    }
    let idx = super::tex::read_register(engine);
    let bx = engine.state.take_box_register(idx);
    engine.aux.extension.pdfxforms().push(PDFXForm {
        attr,resources,bx
    });
    engine.aux.extension.pdfxforms().len() - 1
}
pub fn pdfxform<ET:EngineTypes>(engine:&mut EngineReferences<ET>, token:ET::Token)
                              -> Option<Box<dyn FnOnce(&mut EngineReferences<ET>) -> Option<ShipoutNode<ET>>>>
    where ET::Extension : PDFExtension<ET> {
    parse_pdfxform(engine);
    None
}
pub fn pdfxform_immediate<ET:EngineTypes>(engine:&mut EngineReferences<ET>,token:ET::Token)
    where ET::Extension : PDFExtension<ET>,
      ET::PreCustomNode:PDFNodeTrait<ET> {
    let num = parse_pdfxform(engine);
    let form = engine.aux.extension.pdfxforms()[num].clone();
    ET::Stomach::add_node(engine,PreShipoutNode::Custom(ET::PreCustomNode::from_pdfxform(form)));
}

pub fn pdfrefxform<ET:EngineTypes>(engine:&mut EngineReferences<ET>,token:ET::Token)
    where ET::Extension : PDFExtension<ET>,
          ET::PreCustomNode:PDFNodeTrait<ET> {
    let num = engine.read_int(false).into();
    if num < 0 {todo!("throw error")}
    match engine.aux.extension.pdfxforms().get(num as usize) {
        None => todo!("throw error"),
        Some(n) => {
            let n = n.clone();
            ET::Stomach::add_node(engine, PreShipoutNode::Custom(ET::PreCustomNode::from_pdfxform(n)))
        }
    }
}

#[inline(always)]
pub fn pdflastxform<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) -> <ET::Num as NumSet>::Int
    where ET::Extension : PDFExtension<ET> {
    <ET::Num as NumSet>::Int::from((engine.aux.extension.pdfxforms().len() as i32) - 1)
}

pub fn pdfliteral<ET:EngineTypes>(engine:&mut EngineReferences<ET>,token:ET::Token)
    where ET::Extension : PDFExtension<ET>, ET::PreCustomNode:PDFNodeTrait<ET> {
    let shipout = engine.read_keyword(b"shipout");
    let opt = match engine.read_keywords(&[b"direct",b"page"]) {
        Some(b"direct") => PDFLiteralOption::Direct,
        Some(b"page") => PDFLiteralOption::Page,
        _ => PDFLiteralOption::None
    };
    if shipout {
        todo!("shipout pdfliteral")
    } else {
        let mut str = String::new();
        engine.read_braced_string(false,&mut str);
        ET::Stomach::add_node(engine,PreShipoutNode::Custom(ET::PreCustomNode::from_pdfliteral(str,opt)));
    }
}

#[inline(always)]
pub fn pdfshellescape<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) -> <ET::Num as NumSet>::Int {
    <ET::Num as NumSet>::Int::from(2)
}


pub fn pdfstrcmp<ET:EngineTypes>(engine: &mut EngineReferences<ET>,exp:&mut Vec<ET::Token>,tk:ET::Token) {
    let mut first = engine.aux.memory.get_string();
    let mut second = engine.aux.memory.get_string();
    engine.read_braced_string(false,&mut first);
    engine.read_braced_string(false,&mut second);

    if first == second {
        exp.push(ET::Token::from_char_cat(b'0'.into(),CommandCode::Other))
    } else if first < second {
        exp.push(ET::Token::from_char_cat(b'-'.into(),CommandCode::Other));
        exp.push(ET::Token::from_char_cat(b'1'.into(),CommandCode::Other));
    } else {
        exp.push(ET::Token::from_char_cat(b'1'.into(),CommandCode::Other));
    }

    engine.aux.memory.return_string(first);
    engine.aux.memory.return_string(second);
}

const PRIMITIVE_INTS:&[&'static str] = &[
    "pdfadjustspacing",
    "pdfcompresslevel",
    "pdfdecimaldigits",
    "pdfdraftmode",
    "pdfgentounicode",
    "pdfminorversion",
    "pdfobjcompresslevel",
    "pdfoutput",
    "pdfpkresolution",
    "pdfprotrudechars",
    "tracingstacklevels",
];

const PRIMITIVE_DIMS:&[&'static str] = &[
    "leftmarginkern",
    "pdfhorigin",
    "pdflinkmargin",
    "pdfpageheight",
    "pdfpagewidth",
    "pdfvorigin",
    "rightmarginkern"
];

const PRIMITIVE_TOKS:&[&'static str] = &[
    "pdfpageresources",
];

pub fn register_pdftex_primitives<E:TeXEngine>(engine:&mut E)
    where <E::Types as EngineTypes>::Extension : PDFExtension<E::Types>,
    <E::Types as EngineTypes>::PreCustomNode : PDFNodeTrait<E::Types> {

    register_expandable(engine,"pdfcreationdate",pdfcreationdate);
    register_expandable(engine,"pdffilesize",pdffilesize);
    register_expandable(engine,"pdfmatch",pdfmatch);
    register_expandable(engine,"pdfstrcmp",pdfstrcmp);
    register_expandable(engine,"pdftexrevision",pdftexrevision);

    register_int(engine,"pdftexversion",pdftexversion,None);
    register_int(engine,"pdfmajorversion",pdfmajorversion,None);
    register_int(engine,"pdfshellescape",pdfshellescape,None);
    register_int(engine,"pdflastobj",pdflastobj,None);
    register_int(engine,"pdflastxform",pdflastxform,None);

    register_conditional(engine,"ifincsname",ifincsname);
    register_conditional(engine,"ifpdfabsdim",ifpdfabsdim);
    register_conditional(engine,"ifpdfabsnum",ifpdfabsnum);

    register_unexpandable(engine,"pdfglyphtounicode",pdfglyphtounicode);
    register_unexpandable(engine,"pdfcolorstack",pdfcolorstack);
    register_unexpandable(engine,"pdfliteral",pdfliteral);
    register_unexpandable(engine,"pdfrefobj",pdfrefobj);
    register_unexpandable(engine,"pdfrefxform",pdfrefxform);

    register_whatsit(engine,"pdfobj",pdfobj,pdfobj_immediate);
    register_whatsit(engine,"pdfxform",pdfxform,pdfxform_immediate);

    register_primitive_int(engine,PRIMITIVE_INTS);
    register_primitive_dim(engine,PRIMITIVE_DIMS);
    register_primitive_toks(engine,PRIMITIVE_TOKS);

    cmtodos!(engine,
        lpcode,pdfcatalog,pdfcolorstackinit,
        pdfdest,pdfelapsedtime,pdfendlink,pdfescapestring,
        pdffontexpand,pdffontsize,pdflastximage,
        pdfmdfivesum,pdfoutline,
        pdfrefximage,pdfresettimer,pdfrestore,pdfsave,pdfsetmatrix,pdfstartlink,
        pdfximage,rpcode
    );

    cmtodo!(engine,efcode);
    cmtodo!(engine,knaccode);
    cmtodo!(engine,knbccode);
    cmtodo!(engine,knbscode);
    cmtodo!(engine,pdfadjustinterwordglue);
    cmtodo!(engine,pdfappendkern);
    cmtodo!(engine,pdfforcepagebox);
    cmtodo!(engine,pdfgamma);
    cmtodo!(engine,pdfimageapplygamma);
    cmtodo!(engine,pdfimagegamma);
    cmtodo!(engine,pdfimagehicolor);
    cmtodo!(engine,pdfimageresolution);
    cmtodo!(engine,pdfinclusioncopyfonts);
    cmtodo!(engine,pdfinclusionerrorlevel);
    cmtodo!(engine,pdfinfoomitdate);
    cmtodo!(engine,pdfomitcharset);
    cmtodo!(engine,pdfomitinfodict);
    cmtodo!(engine,pdfomitprocset);
    cmtodo!(engine,pdfpagebox);
    cmtodo!(engine,pdfprependkern);
    cmtodo!(engine,pdfsuppressptexinfo);
    cmtodo!(engine,pdfsuppresswarningdupdest);
    cmtodo!(engine,pdfsuppresswarningdupmap);
    cmtodo!(engine,pdfsuppresswarningpagegroup);
    cmtodo!(engine,pdftracingfonts);
    cmtodo!(engine,pdfuniqueresname);
    cmtodo!(engine,shbscode);
    cmtodo!(engine,showstream);
    cmtodo!(engine,stbscode);
    cmtodo!(engine,tagcode);
    cmtodo!(engine,pdflastannot);
    cmtodo!(engine,pdflastlink);
    cmtodo!(engine,pdflastximagecolordepth);
    cmtodo!(engine,pdflastximagepages);
    cmtodo!(engine,pdflastxpos);
    cmtodo!(engine,pdflastypos);
    cmtodo!(engine,pdfrandomseed);
    cmtodo!(engine,pdfretval);
    cmtodo!(engine,pdfdestmargin);
    cmtodo!(engine,pdfeachlinedepth);
    cmtodo!(engine,pdfeachlineheight);
    cmtodo!(engine,pdffirstlineheight);
    cmtodo!(engine,pdfignoreddimen);
    cmtodo!(engine,pdflastlinedepth);
    cmtodo!(engine,pdfpxdimen);
    cmtodo!(engine,pdfthreadmargin);
    cmtodo!(engine,pdfpageattr);
    cmtodo!(engine,pdfpagesattr);
    cmtodo!(engine,pdfpkmode);
    cmtodo!(engine,ifpdfprimitive);
    cmtodo!(engine,pdfescapehex);
    cmtodo!(engine,pdfescapename);
    cmtodo!(engine,pdffiledump);
    cmtodo!(engine,pdffilemoddate);
    cmtodo!(engine,pdffontname);
    cmtodo!(engine,pdffontobjnum);
    cmtodo!(engine,pdfincludechars);
    cmtodo!(engine,pdfinsertht);
    cmtodo!(engine,pdflastmatch);
    cmtodo!(engine,pdfnormaldeviate);
    cmtodo!(engine,pdfpageref);
    cmtodo!(engine,pdftexbanner);
    cmtodo!(engine,pdfunescapehex);
    cmtodo!(engine,pdfuniformdeviate);
    cmtodo!(engine,pdfxformname);
    cmtodo!(engine,pdfximagebbox);

    cmtodo!(engine,letterspacefont);
    cmtodo!(engine,partokenname);
    cmtodo!(engine,pdfannot);
    cmtodo!(engine,pdfcopyfont);
    cmtodo!(engine,pdfendthread);
    cmtodo!(engine,pdffakespace);
    cmtodo!(engine,pdffontattr);
    cmtodo!(engine,pdfinfo);
    cmtodo!(engine,pdfinterwordspaceoff);
    cmtodo!(engine,pdfinterwordspaceon);
    cmtodo!(engine,pdfmapfile);
    cmtodo!(engine,pdfmapline);
    cmtodo!(engine,pdfnames);
    cmtodo!(engine,pdfnobuiltintounicode);
    cmtodo!(engine,pdfnoligatures);
    cmtodo!(engine,pdfprimitive);
    cmtodo!(engine,pdfrunninglinkoff);
    cmtodo!(engine,pdfrunninglinkon);
    cmtodo!(engine,pdfsavepos);
    cmtodo!(engine,pdfsetrandomseed);
    cmtodo!(engine,pdfspacefont);
    cmtodo!(engine,pdfthread);
    cmtodo!(engine,pdftrailer);
    cmtodo!(engine,pdftrailerid);
    cmtodo!(engine,pdfstartthread);
    cmtodo!(engine,quitvmode);

    /*
    register_conditional!(ifincsname,engine,(e,cmd) =>ifincsname::<ET>(e,&cmd));
    register_conditional!(ifpdfabsdim,engine,(e,cmd) =>ifpdfabsdim::<ET>(e,&cmd));
    register_conditional!(ifpdfabsnum,engine,(e,cmd) =>ifpdfabsnum::<ET>(e,&cmd));
    register_value_assign_int!(lpcode,engine);
    register_unexpandable!(pdfcatalog,engine,None,(e,cmd) =>pdfcatalog::<ET>(e,&cmd));
    register_unexpandable!(pdfcolorstack,engine,None,(e,cmd) =>pdfcolorstack::<ET>(e,&cmd));
    register_int!(pdfcolorstackinit,engine,(e,c) => pdfcolorstackinit::<ET>(e,&c));
    register_expandable!(pdfcreationdate,engine,(e,cmd,f) =>pdfcreationdate::<ET>(e,&cmd,f));
    register_unexpandable!(pdfdest,engine,None,(e,cmd) =>pdfdest::<ET>(e,&cmd));
    register_int!(pdfelapsedtime,engine,(e,c) => pdfelapsedtime::<ET>(e,&c));
    register_unexpandable!(pdfendlink,engine,None,(e,cmd) =>pdfendlink::<ET>(e,&cmd));
    register_expandable!(pdfescapestring,engine,(e,cmd,f) =>pdfescapestring::<ET>(e,&cmd,f));
    register_expandable!(pdffilesize,engine,(e,cmd,f) =>pdffilesize::<ET>(e,&cmd,f));
    register_unexpandable!(pdffontexpand,engine,None,(e,cmd) =>pdffontexpand::<ET>(e,&cmd));
    register_expandable!(pdffontsize,engine,(e,cmd,f) =>pdffontsize::<ET>(e,&cmd,f));
    register_unexpandable!(pdfglyphtounicode,engine,None,(e,cmd) =>pdfglyphtounicode::<ET>(e,&cmd));
    register_int!(pdflastobj,engine,(e,c) => pdflastobj::<ET>(e,&c));
    register_int!(pdflastxform,engine,(e,c) => pdflastxform::<ET>(e,&c));
    register_int!(pdflastximage,engine,(e,c) => pdflastximage::<ET>(e,&c));
    register_unexpandable!(pdfliteral,engine,None,(e,cmd) =>pdfliteral::<ET>(e,&cmd));
    register_int!(pdfmajorversion,engine,(_,c) => pdfmajorversion::<ET>(&c));
    register_expandable!(pdfmatch,engine,(e,cmd,f) =>pdfmatch::<ET>(e,&cmd,f));
    register_expandable!(pdfmdfivesum,engine,(e,cmd,f) =>pdfmdfivesum::<ET>(e,&cmd,f));
    register_unexpandable!(pdfobj,engine,None,(e,cmd) =>pdfobj::<ET>(e,&cmd));
    register_unexpandable!(pdfoutline,engine,None,(e,cmd) =>pdfoutline::<ET>(e,&cmd));
    register_whatsit!(pdfrefxform,engine,(e,cmd) =>pdfrefxform::<ET>(e,&cmd));
    register_unexpandable!(pdfrefximage,engine,None,(e,cmd) =>pdfrefximage::<ET>(e,&cmd));
    register_unexpandable!(pdfresettimer,engine,None,(e,cmd) =>pdfresettimer::<ET>(e,&cmd));
    register_unexpandable!(pdfrestore,engine,None,(e,cmd) => pdfrestore::<ET>(e,&cmd));
    register_unexpandable!(pdfsave,engine,None,(e,cmd) => pdfsave::<ET>(e,&cmd));
    register_unexpandable!(pdfsetmatrix,engine,None,(e,cmd) =>pdfsetmatrix::<ET>(e,&cmd));
    register_int!(pdfshellescape,engine,(_,c) => pdfshellescape::<ET>(&c));
    register_unexpandable!(pdfstartlink,engine,None,(e,cmd) =>pdfstartlink::<ET>(e,&cmd));
    register_expandable!(pdfstrcmp,engine,(e,cmd,f) =>pdfstrcmp::<ET>(e,&cmd,f));
    register_expandable!(pdftexrevision,engine,(e,c,f) =>pdftexrevision::<ET>(e,&c,f));
    register_int!(pdftexversion,engine,(_,c) => pdftexversion::<ET>(&c));
    register_unexpandable!(pdfxform,engine,None,(e,cmd) =>pdfxform::<ET>(e,&cmd));
    register_unexpandable!(pdfximage,engine,None,(e,cmd) =>pdfximage::<ET>(e,&cmd));
    register_value_assign_int!(rpcode,engine);
     */
}