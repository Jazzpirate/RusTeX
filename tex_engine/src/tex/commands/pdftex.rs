//! Implementations for the pdfTeX-specific commands.
//! Use [`initialize_pdftex_primitives`] to register all of these.

use std::marker::PhantomData;
use crate::{cmtodo, debug_log, register_conditional, register_dim_assign, register_int, register_int_assign, register_unexpandable, register_expandable, catch_prim, throw, register_tok_assign, cmstodo, register_whatsit, register_value_assign_int};
use crate::engine::{EngineRef, EngineType};
use crate::engine::filesystem::{File, FileSystem};
use crate::engine::gullet::methods::get_keywords;
use crate::engine::gullet::numeric_methods::expand_until_space;
use crate::engine::state::{PDFState, State};
use crate::engine::stomach::Stomach;
use crate::tex::catcodes::CategoryCode;
use crate::tex::numbers::{Int,Dim};
use crate::tex::token::{BaseToken, Token};
use crate::tex::commands::{CommandSource, TokenCont};
use crate::tex::fonts::Font;
use crate::tex::nodes::{CustomNode, HVBox, NodeTrait, TeXNode, Whatsit};
use crate::utils::errors::TeXError;
use crate::utils::strings::CharType;

/// The version number returned by [`\pdftexversion`](pdftexversion) (140).
pub const PDF_TEX_VERSION: i64 = 140;
/// The version number returned by [`\pdfmajorversion`](pdfmajorversion) (1).
pub const PDF_MAJOR_VERSION: i64 = 1;
/// The version number returned by [`\pdftexrevision`](pdftexrevision) (25).
pub const PDFTEX_REVISION: i64 = 25;

// --------------------------------------------------------------------------------------------------

const STACK_ACTION_KEYWORDS : [&'static str;4] = ["push","pop","set","current"];

#[derive(Debug,Clone,Copy)]
pub struct PDFColor{R:u8,G:u8,B:u8}
impl PDFColor {
    pub fn parse<ET:EngineType>(s:&str) -> Self {
        macro_rules! parse {
            ($s:expr) => {
                match $s.parse::<f32>() {
                    Ok(f) => f,
                    _ => throw!("Invalid color specification: {}",s)
                }
            }
        }
        let ls : Vec<_> = s.split(' ').collect();
        if matches!(ls.last(),Some(&"K")) && ls.len() > 4 {
            let third = 1.0 - parse!(ls[3]);
            let r = 255.0*(1.0 - parse!(ls[0])) * third;
            let g = 255.0*(1.0 - parse!(ls[1])) * third;
            let b = 255.0*(1.0 - parse!(ls[2])) * third;
            if r > 255.0 || g > 255.0 || b > 255.0 || r < 0.0 || g < 0.0 || b < 0.0 {
                throw!("Invalid color specification: {}",s);
            }
            PDFColor{R:(r.round() as u8), G:(g.round() as u8), B:(b.round() as u8)}
        } else if matches!(ls.last(),Some(&"RG")) && ls.len() > 3 {
            let r = 255.0 * parse!(ls[0]);
            let g = 255.0 * parse!(ls[1]);
            let b = 255.0 * parse!(ls[2]);
            if r > 255.0 || g > 255.0 || b > 255.0 || r < 0.0 || g < 0.0 || b < 0.0 {
                throw!("Invalid color specification: {}",s);
            }
            PDFColor{R:(r.round() as u8), G:(g.round() as u8), B:(b.round() as u8)}
        } else if matches!(ls.last(),Some(&"G")) && ls.len() > 1 {
            let x = 255.0 * parse!(ls[0]);
            if x > 255.0 || x < 0.0  {
                throw!("Invalid color specification: {}",s);
            }
            let x = (x.round()) as u8;
            PDFColor{R:x, G:x, B:x}
        } else {
            throw!("Invalid color specification: {}",s);
        }
    }
}

#[derive(Debug,Clone)]
pub struct PDFObj {pub content: String} // TODO

#[derive(Debug,Clone)]
pub struct PDFXForm<ET:EngineType> {pub attr: String, pub resources:String, pub bx:HVBox<ET>} // TODO

#[derive(Debug,Clone)]
pub struct PDFColorstack(pub Vec<PDFColor>); // TODO

#[derive(Debug,Clone)]
pub enum PDFTeXNode<ET:EngineType> where ET::Node:From<PDFTeXNode<ET>> {
    PDFCatalog {literal:String,action:Option<ActionSpec>},
    PDFLiteral{literal:String},
    PDFXForm{form:PDFXForm<ET>},
    PDFOutline{attr:String,action:ActionSpec,content:String,count:Option<i64>},
    PDFDest{structnum:Option<i64>,id:NumOrName,desttype:PDFDestType<ET>},
}

#[derive(Debug,Clone)]
pub enum PDFDestType<ET:EngineType> {
    XYZ { zoom:Option<i64>},
    Fitr {width:Option<ET::Dim>, height:Option<ET::Dim>, depth:Option<ET::Dim>},
    Fitbh, Fitbv, Fitb, Fith, Fitv, Fit
}

impl<ET:EngineType> NodeTrait<ET> for PDFTeXNode<ET> where ET::Node:From<PDFTeXNode<ET>> {
    fn as_node(self) -> TeXNode<ET> {
        TeXNode::Custom(ET::Node::from(self))
    }
    fn height(&self) -> ET::Dim {
        ET::Dim::from_sp(0)
    }
    fn depth(&self) -> ET::Dim {
        ET::Dim::from_sp(0)
    }
    fn width(&self) -> ET::Dim {
        ET::Dim::from_sp(0)
    }
    fn nodetype(&self) -> u8 { 9 }
}
impl<ET:EngineType<Node=Self>> CustomNode<ET> for PDFTeXNode<ET> {}
impl<ET:EngineType> NodeTrait<ET> for PDFXForm<ET> where ET::Node:From<PDFTeXNode<ET>> {
    fn as_node(self) -> TeXNode<ET> {
        PDFTeXNode::PDFXForm{form:self}.as_node()
    }
    fn height(&self) -> ET::Dim {
        ET::Dim::from_sp(0)
    }
    fn depth(&self) -> ET::Dim {
        ET::Dim::from_sp(0)
    }
    fn width(&self) -> ET::Dim {
        ET::Dim::from_sp(0)
    }
    fn nodetype(&self) -> u8 { 9 }
}

#[derive(Debug,Clone)]
pub enum ActionSpec {
    User(String),
    Goto(GotoAction),
    Thread{file:Option<String>,target:NumOrName}
}

#[derive(Debug,Clone)]
pub enum GotoAction {
    File{filename:String,struct_:Option<PDFStruct>,page:Option<i64>,target:String,newwindow:Option<bool>},
    Current{struct_:Option<PDFStruct>,page:Option<i64>,target:NumOrName},
}

#[derive(Debug,Clone)]
pub enum NumOrName { Num(i64),Name(String) }

#[derive(Debug,Clone)]
pub enum PDFStruct { Num(i64),Name(String),Other(String) }

pub fn action_spec<ET:EngineType>(engine:&mut EngineRef<ET>) -> ActionSpec {
    engine.skip_whitespace();
    match engine.get_keywords(vec!("user","goto","thread")) {
        Some("user") => {
            let mut ret = String::new();
            engine.skip_whitespace();
            engine.get_braced_string(&mut ret);
            ActionSpec::User(ret)
        }
        Some("goto") => {
            engine.skip_whitespace();
            let file = if engine.get_keyword("file") {
                let mut file = String::new();
                engine.skip_whitespace();
                engine.get_braced_string(&mut file);
                engine.skip_whitespace();
                Some(file)
            } else {None};
            let struct_ = if engine.get_keyword("struct") {
                engine.skip_whitespace();
                match engine.get_keywords(vec!("num","name")) {
                    None => {
                        let mut ret = String::new();
                        engine.skip_whitespace();
                        engine.get_braced_string(&mut ret);
                        Some(PDFStruct::Other(ret))
                    },
                    Some("num") => {
                        engine.skip_whitespace();
                        Some(PDFStruct::Num(engine.get_int().to_i64()))
                    },
                    Some("name") => {
                        let mut ret = String::new();
                        engine.skip_whitespace();
                        engine.get_braced_string(&mut ret);
                        Some(PDFStruct::Name(ret))
                    },
                    _ => unreachable!()
                }
            } else {None};
            match file {
                None => {
                    engine.skip_whitespace();
                    match engine.get_keywords(vec!("num","name")) {
                        None => {
                            engine.skip_whitespace();
                            let page = if engine.get_keyword("page") {
                                Some(engine.get_int().to_i64())
                            } else {None};
                            let mut str = String::new();
                            engine.skip_whitespace();
                            engine.get_braced_string(&mut str);
                            ActionSpec::Goto(GotoAction::Current{
                                struct_,page,target:NumOrName::Name(str)
                            })
                        }
                        Some("num") => ActionSpec::Goto(GotoAction::Current{
                                struct_,page:None,target:NumOrName::Num(engine.get_int().to_i64())
                            }),
                        Some("name") => {
                            let mut str = String::new();
                            engine.skip_whitespace();
                            engine.get_braced_string(&mut str);
                            ActionSpec::Goto(GotoAction::Current{
                                struct_,page:None,target:NumOrName::Name(str)
                            })
                        }
                        _ => unreachable!()
                    }
                }
                Some(filename) => {
                    let (page,target) = if engine.get_keyword("name") {
                        let mut str = String::new();
                        engine.get_braced_string(&mut str);
                        (None,str)
                    } else {
                        let page = if engine.get_keyword("page") {
                            Some(engine.get_int().to_i64())
                        } else {None};
                        let mut str = String::new();
                        engine.get_braced_string(&mut str);
                        (page,str)
                    };
                    let newwindow = match engine.get_keywords(vec!("newwindow","nonewwindow")) {
                        Some("newwindow") => Some(true),
                        Some("nonewwindow") => Some(false),
                        _ => None
                    };
                    ActionSpec::Goto(GotoAction::File{
                        filename,struct_,page,target,newwindow
                    })
                }
            }

        }
        Some("thread") => {
            let file = if engine.get_keyword("file") {
                let mut file = String::new();
                engine.get_braced_string(&mut file);
                Some(file)
            } else {None};

            match engine.get_keywords(vec!("num","name")) {
                None => throw!("Expected one of 'num','name'"),
                Some("num") => ActionSpec::Thread{
                    file,target:NumOrName::Num(engine.get_int().to_i64())
                },
                Some("name") => {
                    let mut str = String::new();
                    engine.get_braced_string(&mut str);
                    ActionSpec::Thread{
                        file,target:NumOrName::Name(str)
                    }
                }
                _ => unreachable!()
            }

        }
        None => throw!("Expected one of 'user','goto','thread'"),
        _ => unreachable!()
    }
}

// --------------------------------------------------------------------------------------------------

/// "ifpdfabsnum"
pub const IFPDFABSNUM : &str = "ifpdfabsnum";
/// `\ifpdfabsnum`: Compare the absolute values of two numbers.
pub fn ifpdfabsnum<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) -> bool {
    debug_log!(trace=>"ifpdfabsnum");
    let i1 = engine.get_int();
    let rel = match engine.is_next_char_one_of(&super::tex::LGE) {
        None => throw!("Expected one of '<','>','='" => cmd.cause),
        Some(r) => r
    };
    let i2 = engine.get_int();
    match rel {
        b'<' => i1.to_i64().abs() < i2.to_i64().abs(),
        b'>' => i1.to_i64().abs()>i2.to_i64().abs(),
        b'=' => i1.to_i64().abs()==i2.to_i64().abs(),
        _ => unreachable!()
    }
}

pub fn ifincsname<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) -> bool {
    debug_log!(trace=>"ifincsname");
    engine.state.current_csname().is_some()
}

/// "ifpdfabsdim"
pub const IFPDFABSDIM : &str = "ifpdfabsdim";
/// `\ifpdfabsdim`: Compare the absolute values of two dimensions.
pub fn ifpdfabsdim<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) -> bool {
    debug_log!(trace=>"ifpdfabsdim");
    let i1 = engine.get_dim();
    let rel = match engine.is_next_char_one_of(&super::tex::LGE) {
        None => throw!("Expected one of '<','>','='" => cmd.cause),
        Some(r) => r
    };
    let i2 = engine.get_dim();
    match rel {
        b'<' => i1.to_sp() < i2.to_sp().abs(),
        b'>' => i1.to_sp().abs()>i2.to_sp().abs(),
        b'=' => i1.to_sp().abs()==i2.to_sp().abs(),
        _ => unreachable!()
    }
}


pub const LPCODE: &str = "lpcode";
pub fn lpcode_assign<ET:EngineType>(engine: &mut EngineRef<ET>, cmd:&CommandSource<ET>, global:bool) {
    debug_log!(trace=>"Assigning lpcode");
    let mut fnt = engine.get_font();
    let char = engine.get_char();
    engine.skip_eq_char();
    let val = engine.get_int();
    fnt.set_lpcode(char,val);
}
pub fn lpcode_get<ET:EngineType>(engine: &mut EngineRef<ET>, cmd:&CommandSource<ET>) -> ET::Int {
    debug_log!(trace=>"Getting lpcode");
    let fnt = engine.get_font();
    let char = engine.get_char();
    fnt.get_lpcode(char)
}


pub fn pdfcatalog<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>)
    where ET::Node:From<PDFTeXNode<ET>>{
    debug_log!(trace=>"\\pdfcatalog");
    let mut literal = String::new();
    engine.get_braced_string(&mut literal);
    let action = if engine.get_keyword("openaction") {
        Some(action_spec(engine))
    } else {None};

    engine.stomach.push_node(engine.state,PDFTeXNode::PDFCatalog {literal,action}.as_node());
}

/// "pdfcolorstack"
pub const PDFCOLORSTACK: &str = "pdfcolorstack";

pub fn pdfcolorstack<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>)
    where ET::Node:From<PDFTeXNode<ET>>, ET::State:PDFState<ET> {
    debug_log!(trace=>"pdfcolorstack");
    let index = engine.get_int().to_i64();
    if index < 0 {throw!("Invalid colorstack index: {}",index => cmd.cause)}
    match engine.get_keywords(STACK_ACTION_KEYWORDS.to_vec()) {
        Some("current") =>
            engine.state.set_current_colorstack(index as usize),
        Some("pop") => {engine.state.get_colorstack(index as usize).0.pop();}
        Some("set") => {
            let mut colorstr = engine.memory.get_string();
            //catch_prim!(expand_until_space::<ET>(engine) => (PDFCOLORSTACK,cmd));
            engine.get_braced_string(&mut colorstr);
            let color = PDFColor::parse::<ET>(colorstr.as_str());
            engine.memory.return_string(colorstr);
            *engine.state.get_colorstack(index as usize).0.last_mut().unwrap() = color;
        }
        Some("push") => {
            let mut colorstr = engine.memory.get_string();
            //catch_prim!(expand_until_space::<ET>(engine) => (PDFCOLORSTACK,cmd));
            engine.get_braced_string(&mut colorstr);
            let color = PDFColor::parse::<ET>(colorstr.as_str());
            engine.memory.return_string(colorstr);
            engine.state.get_colorstack(index as usize).0.push(color);
        }
        _ => throw!("Expected one of 'push','pop','set','current'" => cmd.cause)
    }
}

pub fn pdfcolorstackinit<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) -> ET::Int
    where ET::Node:From<PDFTeXNode<ET>>, ET::State:PDFState<ET> {
    debug_log!(trace=>"pdfcolorstackinit");
    engine.get_keyword("page"); // TODO
    engine.get_keyword("direct");
    let mut colorstr = engine.memory.get_string();
    //catch_prim!(expand_until_space::<ET>(engine) => (PDFCOLORSTACK,cmd));
    engine.get_braced_string(&mut colorstr);
    let color = PDFColor::parse::<ET>(colorstr.as_str());
    engine.memory.return_string(colorstr);
    let idx = engine.state.pdfcolorstacks().len();
    engine.state.pdfcolorstacks().push(PDFColorstack(vec!(color)));
    ET::Int::from_i64(idx as i64)
}

pub fn pdfdest<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>)
    where ET::Node:From<PDFTeXNode<ET>> {
    debug_log!(trace=>"pdfdest");
    engine.skip_whitespace();
    let structnum = if engine.get_keyword("struct") {
        engine.skip_whitespace();
        Some(engine.get_int().to_i64())
    } else {None};
    engine.skip_whitespace();
    let id = match engine.get_keywords(vec!("num","name")) {
        Some("num") => {
            engine.skip_whitespace();
            NumOrName::Num(engine.get_int().to_i64())
        },
        Some("name") => {
            let mut str = String::new();
            engine.skip_whitespace();
            engine.get_braced_string(&mut str);
            NumOrName::Name(str)
        }
        _ => throw!("Expected one of 'num','name'" => cmd.cause),
    };
    engine.skip_whitespace();
    let desttype = match engine.get_keywords(vec!("xyz","fitr","fitbh","fitbv","fitb","fith","fitv", "fit")) {
        Some("xyz") => {
            engine.skip_whitespace();
            let zoom = if engine.get_keyword("zoom") {
                engine.skip_whitespace();
                Some(engine.get_int().to_i64())
            } else {None};
            PDFDestType::XYZ{zoom}
        }
        Some("fitr") => {
            let mut width : Option<ET::Dim> = None;
            let mut height : Option<ET::Dim> = None;
            let mut depth : Option<ET::Dim> = None;
            loop {
                engine.skip_whitespace();
                match engine.get_keywords(vec!("width","height","depth")) {
                    Some("width") => {
                        engine.skip_whitespace();
                        width = Some(engine.get_dim());
                    }
                    Some("height") => {
                        engine.skip_whitespace();
                        height = Some(engine.get_dim());
                    }
                    Some("depth") => {
                        engine.skip_whitespace();
                        depth = Some(engine.get_dim());
                    }
                    _ => break
                }
            }
            PDFDestType::Fitr{width,height,depth}
        }
        Some("fitbh") => PDFDestType::Fitbh,
        Some("fitbv") => PDFDestType::Fitbv,
        Some("fitb") => PDFDestType::Fitb,
        Some("fith") => PDFDestType::Fith,
        Some("fitv") => PDFDestType::Fitv,
        Some("fit") => PDFDestType::Fit,
        _ => throw!("Expected one of 'xyz','fitr','fitbh','fitbv','fitb','fith','fitv','fit'" => cmd.cause)
    };
    engine.stomach.push_node(engine.state,PDFTeXNode::PDFDest{structnum,id,desttype}.as_node());
}

pub fn pdfelapsedtime<ET:EngineType>(engine:&mut EngineRef<ET>,cmd:&CommandSource<ET>) -> ET::Int {
    let r = engine.elapsed.elapsed().as_secs_f64() * 65536.0;
    let ret = if r > i32::MAX.into() {i32::MAX as i64} else {r.round() as i64};
    ET::Int::from_i64(ret)
}

pub fn pdfescapestring<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>, f:TokenCont<ET>) {
    debug_log!(trace=>"pdfescapestring");
    use crate::tex::commands::BaseCommand;
    use crate::engine::mouth::MouthTrait;
    let esc = engine.state.get_escapechar();
    let cc = engine.state.get_catcode_scheme().clone();
    crate::get_expanded_group!(engine,false,false,true,next => engine.token_to_others(&next,true,f)); // TODO:actual escaping
}

/// "pdffilesize"
pub const PDFFILESIZE : &str = "pdffilesize";
/// `\pdffilesize`: Get the size of a file (in bytes).
pub fn pdffilesize<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>, fun:TokenCont<ET>) {
    debug_log!(trace=>"pdffilesize");
    let mut filename = engine.memory.get_string();
    engine.get_braced_string(&mut filename);
    //gullet.get_expanded_group(state,false,false,true, &mut |_,t| Ok(ret.push(t))) => (PDFFILESIZE,cmd));
    // let filename = tokens_to_string(ret,state.get_escapechar(),state.get_catcode_scheme());
    let f = engine.filesystem.get(&filename);
    engine.memory.return_string(filename);
    match f.content_string() {
        None => (),
        Some(v) =>{
            engine.string_to_tokens(v.iter().map(|b| b.len()).sum::<usize>().to_string().as_bytes(),fun)
        }
    }
}


pub fn pdffontexpand<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    debug_log!(trace=>"pdffontexpand"); // TODO
    let _ = engine.get_font();
    let _ = engine.get_int();
    let _ = engine.get_int();
    let _ = engine.get_int();
    engine.get_keyword("autoexpand");
}

pub fn pdffontsize<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>, f:TokenCont<ET>) {
    let fnt = engine.get_font();
    let d = ET::Dim::from_sp(fnt.get_at());
    engine.string_to_tokens(format!("{}",d).as_bytes(),f)
}

/// "pdfglyphtounicode"
pub const PDFGLYPHTOUNICODE : &str = "pdfglyphtounicode";
/// `\pdfglyphtounicode`: Register the unicode codepoint of a glyph.
pub fn pdfglyphtounicode<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    debug_log!(trace=>"\\pdfglyphtounicode");
    // TODO
    let mut v = engine.memory.get_token_vec();
    engine.get_argument(&mut v);
    engine.get_argument(&mut v);
    engine.memory.return_token_vec(v);
}

pub fn pdflastobj<ET:EngineType>(engine:&mut EngineRef<ET>,cmd:&CommandSource<ET>)
                                     -> ET::Int where ET::State:PDFState<ET> {
    ET::Int::from_i64(engine.state.pdfobjs().len() as i64 - 1)
}

pub fn pdflastxform<ET:EngineType>(engine:&mut EngineRef<ET>,cmd:&CommandSource<ET>)
                                 -> ET::Int where ET::State:PDFState<ET> {
    ET::Int::from_i64(engine.state.pdfxforms().len() as i64 - 1)
}

/// "pdfliteral"
pub const PDFLITERAL: &str = "pdfliteral";

pub fn pdfliteral<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>)
    where ET::Node:From<PDFTeXNode<ET>>{
    debug_log!(trace=>"pdfliteral");
    let mut literal = String::new();
    engine.get_braced_string(&mut literal);
    engine.stomach.push_node(engine.state,PDFTeXNode::PDFLiteral {literal}.as_node());
}

/// "pdfobj"
pub const PDFOBJ : &str = "pdfobj";

pub const OBJECT_TYPE_SPEC: [&'static str; 3] = ["reserveobjnum","useobjnum","stream"];
/// `\pdfobj`
pub fn pdfobj<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>)
    where ET::State:PDFState<ET> {
    debug_log!(trace=>"\\pdfobj");
    match engine.get_keywords(OBJECT_TYPE_SPEC.to_vec()) {
        None => throw!("Expected one of 'reserveobjnum','useobjnum','stream'" => cmd.cause),
        Some("reserveobjnum") => engine.state.pdfobjs().push(PDFObj { content:String::new()}), // TODO
        Some("useobjnum") => {
            let i = engine.get_int().to_i64();
            if i < 0 || engine.state.pdfobjs().len() as i64 <= i {throw!("Invalid object number: {}",i => cmd.cause)}
            let mut str = String::new();
            engine.get_braced_string(&mut str);
            *engine.state.pdfobjs().get_mut(i as usize).unwrap() = PDFObj { content:str};
        },
        Some("stream") => {
            engine.skip_whitespace();
            if engine.get_keyword("attr") {
                // TODO
                let mut ret = engine.memory.get_token_vec();
                engine.get_argument(&mut ret);
            }
        },
        _ => unreachable!()
    }
}

pub fn pdfoutline<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>)
    where ET::Node:From<PDFTeXNode<ET>>{
    // TODO
    // \pdfoutline [ ⟨attr spec⟩ ] ⟨action spec⟩ [ count ⟨integer⟩ ] ⟨general text⟩
    debug_log!(trace=>"\\pdfoutline");
    let mut attr = String::new();
    if engine.get_keyword("attr") {
        engine.get_braced_string(&mut attr)
    }
    let action = action_spec(engine);
    let count = if engine.get_keyword("count") {
        Some(engine.get_int().to_i64())
    } else {None};
    let mut content= String::new();
    engine.get_braced_string(&mut content);
    engine.stomach.push_node(engine.state,PDFTeXNode::PDFOutline{attr,action,content,count}.as_node());
}

pub fn pdfrefxform<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>)
                            -> Whatsit<ET> where ET::State:PDFState<ET>,ET::Node:From<PDFTeXNode<ET>> {
    debug_log!(trace=>"\\pdfrefxform");
    let i = engine.get_int().to_i64();
    if i < 0 || engine.state.pdfxforms().len() as i64 <= i {throw!("Invalid xform number: {}",i => cmd.cause)}
    Whatsit::new(Box::new(move |e| {
        let f = e.state.pdfxforms().remove(i as usize).as_node();
        e.stomach.push_node(e.state,f)
    }))
}

pub fn pdfresettimer<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    debug_log!(trace=>"\\pdfresettimer");
    *engine.elapsed = std::time::Instant::now();
}

/// "pdfstrcmp"
pub const PDFSTRCMP : &str = "pdfstrcmp";
/// `\pdfstrcmp`: Compare two strings; return -1, 0, or 1.
pub fn pdfstrcmp<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>, f:TokenCont<ET>) {
    debug_log!(trace=>"pdfstrcmp");
    let mut str1 = engine.memory.get_string();
    let mut str2 = engine.memory.get_string();
    engine.get_braced_string(&mut str1);
    engine.get_braced_string(&mut str2);
    debug_log!(trace=>"pdfstrcmp: {}=={}?",str1,str2);
    if str1==str2 {f(engine,Token::new(BaseToken::Char(ET::Char::from(b'0'),CategoryCode::Other),None))}
        else if str1 < str2 { engine.string_to_tokens("-1".as_bytes(),f)}
        else {f(engine,Token::new(BaseToken::Char(ET::Char::from(b'1'),CategoryCode::Other),None))}
    engine.memory.return_string(str1);
    engine.memory.return_string(str2);
}

/// "pdftexversion"
pub const PDFTEXVERSION : &str = "pdftexversion";
/// ` \pdftexversion`: Return the [`PDF_TEX_VERSION`] as [`Int`].
pub fn pdftexversion<ET:EngineType>(cmd:&CommandSource<ET>) -> ET::Int {
    ET::Int::from_i64(PDF_TEX_VERSION)
}

/// "pdfmajorversion"
pub const PDFMAJORVERSION : &str = "pdfmajorversion";
/// `\pdfmajorversion`: Return the [`PDF_MAJOR_VERSION`] as [`Int`].
pub fn pdfmajorversion<ET:EngineType>(cmd:&CommandSource<ET>) -> ET::Int {
    ET::Int::from_i64(PDF_MAJOR_VERSION)
}

/// "pdfmatch"
pub const PDFMATCH : &str = "pdfmatch";
/// `\pdmatch`
pub fn pdfmatch<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>, f:TokenCont<ET>)
    where ET::State:PDFState<ET>{
    debug_log!(trace=>"pdfmatch");
    let icase = engine.get_keyword("icase");
    let subcount = if engine.get_keyword("subcount") {
        engine.get_int().to_i64()
    } else {-1};
    let mut pattern_string = engine.memory.get_string();
    if icase {pattern_string.push_str("(?i)")}
    engine.get_braced_string(&mut pattern_string);
    let mut target_string = engine.memory.get_string();
    engine.get_braced_string(&mut target_string);

    for s in engine.state.pdfmatches().drain(..) {
        engine.memory.return_string(s);
    }
    match regex::Regex::new(&pattern_string) {
        Ok(reg) => {
            match reg.captures_iter(&target_string).next() {
                None => {
                    engine.memory.return_string(pattern_string);
                    engine.memory.return_string(target_string);
                    f(engine,Token::new(BaseToken::Char(ET::Char::from(b'0'),CategoryCode::Other),None));
                }
                Some(capture) => { // TODO this is not quite right yet, I think
                    let cap = capture.get(0).unwrap();
                    let mut retstr = pattern_string;
                    retstr.clear();
                    retstr.push_str(&cap.start().to_string());
                    retstr.push_str("->");
                    retstr.push_str(cap.as_str());
                    engine.state.pdfmatches().push(retstr);
                    for cap in capture.iter().skip(1) {
                        let mut retstr = engine.memory.get_string();
                        match cap {
                            None => retstr.push_str("-1"),
                            Some(cap) => {
                                retstr.push_str(&cap.start().to_string());
                                retstr.push_str("->");
                                retstr.push_str(cap.as_str());
                            }
                        }
                        engine.state.pdfmatches().push(retstr);
                    }
                    engine.memory.return_string(target_string);
                    f(engine,Token::new(BaseToken::Char(ET::Char::from(b'1'),CategoryCode::Other),None))
                }
            }

        },
        Err(e) => {
            f(engine,Token::new(BaseToken::Char(ET::Char::from(b'-'),CategoryCode::Other),None));
            f(engine,Token::new(BaseToken::Char(ET::Char::from(b'1'),CategoryCode::Other),None))
        }
    }
}

pub fn pdfmdfivesum<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>, f:TokenCont<ET>) {
    use crate::engine::filesystem::File;
    debug_log!(trace=>"pdfmdfivesum");
    let domd = if engine.get_keyword("file") {
        let mut filename = engine.memory.get_string();
        engine.get_braced_string(&mut filename);
        let file = engine.filesystem.get(&filename);
        engine.memory.return_string(filename);
        let x = match file.content_string() {
            None => md5::compute(""),
            Some(v) => {
                let v : Vec<u8> = v.iter().flat_map(|b| b.iter().map(|u|*u)).collect();
                md5::compute(v)
            }
        };
        x
    } else {
        let mut ret = String::new();
        engine.get_braced_string(&mut ret);
        let r = md5::compute(&ret);
        engine.memory.return_string(ret);
        r
    };
    engine.string_to_tokens(format!("{:X}",domd).as_bytes(),f)
}

/// "pdfshellescape"
pub const PDFSHELLESCAPE: &str = "pdfshellescape";
/// `\pdfmshellescape`: 2 (restricted).
pub fn pdfshellescape<ET:EngineType>(cmd:&CommandSource<ET>) -> ET::Int {
    ET::Int::from_i64(2)
}

/// "pdftexrevision"
pub const PDFTEXREVISION : &str = "pdftexrevision";
/// `\pdftexrevision`: expands to the [`PDFTEX_REVISION`] (`25`).
pub fn pdftexrevision<ET:EngineType>(engine:&mut EngineRef<ET>, f:TokenCont<ET>) {
    engine.string_to_tokens(PDFTEX_REVISION.to_string().as_bytes(),f)
}


pub fn pdfxform<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>)
    where ET::State:PDFState<ET> {
    debug_log!(trace=>"\\pdfxform");
    let mut attr = String::new();
    if engine.get_keyword("attr") {
        engine.skip_whitespace();
        engine.get_braced_string(&mut attr)
    }
    let mut resources = String::new();
    if engine.get_keyword("resources") {
        engine.skip_whitespace();
        engine.get_braced_string(&mut attr)
    }
    engine.skip_whitespace();
    let bx = engine.get_int().to_i64();
    if bx < 0 {throw!("Invalid box number: {}",bx => cmd.cause)}
    let bx = engine.state.take_box_register(bx as usize);
    engine.state.pdfxforms().push(PDFXForm{attr,resources,bx});
}


pub const RPCODE: &str = "rpcode";
pub fn rpcode_assign<ET:EngineType>(engine: &mut EngineRef<ET>, cmd:&CommandSource<ET>, global:bool) {
    debug_log!(trace=>"Assigning rpcode");
    let mut fnt = engine.get_font();
    let char = engine.get_char();
    engine.skip_eq_char();
    let val = engine.get_int();
    fnt.set_rpcode(char,val);
}
pub fn rpcode_get<ET:EngineType>(engine: &mut EngineRef<ET>, cmd:&CommandSource<ET>) -> ET::Int {
    debug_log!(trace=>"Getting rpcode");
    let fnt = engine.get_font();
    let char = engine.get_char();
    fnt.get_rpcode(char)
}


/// Initialize a TeX engine with default implementations for all pdfTeX primitives.
pub fn initialize_pdftex_primitives<ET:EngineType>(engine:&mut EngineRef<ET>) where ET::Node:From<PDFTeXNode<ET>>, ET::State:PDFState<ET> {
    register_conditional!(ifincsname,engine,(e,cmd) =>ifincsname::<ET>(e,&cmd));
    register_conditional!(ifpdfabsdim,engine,(e,cmd) =>ifpdfabsdim::<ET>(e,&cmd));
    register_conditional!(ifpdfabsnum,engine,(e,cmd) =>ifpdfabsnum::<ET>(e,&cmd));
    register_dim_assign!(leftmarginkern,engine);
    register_value_assign_int!(lpcode,engine);
    register_int_assign!(pdfadjustspacing,engine);
    register_unexpandable!(pdfcatalog,engine,None,(e,cmd) =>pdfcatalog::<ET>(e,&cmd));
    register_unexpandable!(pdfcolorstack,engine,None,(e,cmd) =>pdfcolorstack::<ET>(e,&cmd));
    register_int!(pdfcolorstackinit,engine,(e,c) => pdfcolorstackinit::<ET>(e,&c));
    register_int_assign!(pdfcompresslevel,engine);
    register_int_assign!(pdfdecimaldigits,engine);
    register_unexpandable!(pdfdest,engine,None,(e,cmd) =>pdfdest::<ET>(e,&cmd));
    register_int_assign!(pdfdraftmode,engine);
    register_int!(pdfelapsedtime,engine,(e,c) => pdfelapsedtime::<ET>(e,&c));
    register_expandable!(pdfescapestring,engine,(e,cmd,f) =>pdfescapestring::<ET>(e,&cmd,f));
    register_expandable!(pdffilesize,engine,(e,cmd,f) =>pdffilesize::<ET>(e,&cmd,f));
    register_unexpandable!(pdffontexpand,engine,None,(e,cmd) =>pdffontexpand::<ET>(e,&cmd));
    register_expandable!(pdffontsize,engine,(e,cmd,f) =>pdffontsize::<ET>(e,&cmd,f));
    register_int_assign!(pdfgentounicode,engine);
    register_unexpandable!(pdfglyphtounicode,engine,None,(e,cmd) =>pdfglyphtounicode::<ET>(e,&cmd));
    register_dim_assign!(pdfhorigin,engine);
    register_int!(pdflastobj,engine,(e,c) => pdflastobj::<ET>(e,&c));
    register_int!(pdflastxform,engine,(e,c) => pdflastxform::<ET>(e,&c));
    register_dim_assign!(pdflinkmargin,engine);
    register_unexpandable!(pdfliteral,engine,None,(e,cmd) =>pdfliteral::<ET>(e,&cmd));
    register_int!(pdfmajorversion,engine,(_,c) => pdfmajorversion::<ET>(&c));
    register_expandable!(pdfmatch,engine,(e,cmd,f) =>pdfmatch::<ET>(e,&cmd,f));
    register_expandable!(pdfmdfivesum,engine,(e,cmd,f) =>pdfmdfivesum::<ET>(e,&cmd,f));
    register_int_assign!(pdfminorversion,engine);
    register_unexpandable!(pdfobj,engine,None,(e,cmd) =>pdfobj::<ET>(e,&cmd));
    register_int_assign!(pdfobjcompresslevel,engine);
    register_unexpandable!(pdfoutline,engine,None,(e,cmd) =>pdfoutline::<ET>(e,&cmd));
    register_int_assign!(pdfoutput,engine);
    register_dim_assign!(pdfpageheight,engine);
    register_tok_assign!(pdfpageresources,engine);
    register_dim_assign!(pdfpagewidth,engine);
    register_int_assign!(pdfpkresolution,engine);
    register_int_assign!(pdfprotrudechars,engine);
    register_whatsit!(pdfrefxform,engine,(e,cmd) =>pdfrefxform::<ET>(e,&cmd));
    register_unexpandable!(pdfresettimer,engine,None,(e,cmd) =>pdfresettimer::<ET>(e,&cmd));
    register_int!(pdfshellescape,engine,(_,c) => pdfshellescape::<ET>(&c));
    register_expandable!(pdfstrcmp,engine,(e,cmd,f) =>pdfstrcmp::<ET>(e,&cmd,f));
    register_expandable!(pdftexrevision,engine,(e,_,f) =>pdftexrevision::<ET>(e,f));
    register_int!(pdftexversion,engine,(_,c) => pdftexversion::<ET>(&c));
    register_dim_assign!(pdfvorigin,engine);
    register_unexpandable!(pdfxform,engine,None,(e,cmd) =>pdfxform::<ET>(e,&cmd));
    register_dim_assign!(rightmarginkern,engine);
    register_value_assign_int!(rpcode,engine);
    register_int_assign!(tracingstacklevels,engine);


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
    cmtodo!(engine,pdflastximage);
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
    cmtodo!(engine,pdfcreationdate);
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
    cmtodo!(engine,pdfendlink);
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
    cmtodo!(engine,pdfrefobj);
    cmtodo!(engine,pdfrefximage);
    cmtodo!(engine,pdfrestore);
    cmtodo!(engine,pdfrunninglinkoff);
    cmtodo!(engine,pdfrunninglinkon);
    cmtodo!(engine,pdfsave);
    cmtodo!(engine,pdfsavepos);
    cmtodo!(engine,pdfsetmatrix);
    cmtodo!(engine,pdfsetrandomseed);
    cmtodo!(engine,pdfspacefont);
    cmtodo!(engine,pdfstartlink);
    cmtodo!(engine,pdfthread);
    cmtodo!(engine,pdftrailer);
    cmtodo!(engine,pdftrailerid);
    cmtodo!(engine,pdfstartthread);
    cmtodo!(engine,pdfximage);
    cmtodo!(engine,quitvmode);

}