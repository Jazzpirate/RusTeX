use std::fmt::Formatter;
use crate::engine::{EngineExtension, EngineReferences, EngineTypes};
use crate::tex::nodes::boxes::TeXBox;
use crate::tex::nodes::NodeTrait;
use crate::tex::numerics::TeXDimen;
use crate::tex::types::NodeType;
use crate::tex::nodes::CustomNodeTrait;

#[derive(Debug,Clone)]
pub enum ActionSpec {
    User(String),
    Goto(GotoAction),
    Thread{file:Option<String>,target:NumOrName}
}

#[derive(Debug,Clone,Copy)]
pub enum PDFBoxSpec {
    MediaBox, CropBox, BleedBox, TrimBox, ArtBox
}

#[derive(Debug,Clone)]
pub enum GotoAction {
    File{filename:String,struct_:Option<PDFStruct>,page:Option<i64>,target:String,newwindow:Option<bool>},
    Current{struct_:Option<PDFStruct>,page:Option<i64>,target:NumOrName},
}

#[derive(Debug,Clone)]
pub enum PDFDestType<D:TeXDimen> {
    XYZ{zoom:Option<i64>},
    Fitr{width:Option<D>, height:Option<D>,depth:Option<D>},
    Fitbh,Fitbv,Fitb,Fith,Fitv,Fit
}

#[derive(Debug,Clone)]
pub enum NumOrName { Num(i64),Name(String) }

#[derive(Debug,Clone)]
pub enum PDFStruct { Num(i64),Name(String),Other(String) }

pub fn num_or_name<ET:EngineTypes>(engine:&mut EngineReferences<ET>) -> Option<NumOrName> {
    match engine.read_keywords(&[b"num",b"name"]) {
        Some(b"num") => Some(NumOrName::Num(engine.read_int(false).into())),
        Some(b"name") => {
            let mut str = String::new();
            engine.read_braced_string(true,&mut str);
            Some(NumOrName::Name(str))
        }
        _ => None
    }
}

pub fn pdfdest_type<ET:EngineTypes>(engine:&mut EngineReferences<ET>) -> PDFDestType<ET::Dim> {
    match engine.read_keywords(&[b"xyz",b"fitr",b"fitbh",b"fitbv",b"fitb",b"fith",b"fitv",b"fit"]) {
        Some(b"xyz") => {
            let zoom = if engine.read_keyword(b"zoom") {
                Some(engine.read_int(false).into())
            } else {None};
            PDFDestType::XYZ{zoom}
        }
        Some(b"fitr") => {
            let mut width = None;
            let mut height = None;
            let mut depth = None;
            loop {
                match engine.read_keywords(&[b"width",b"height",b"depth"]) {
                    Some(b"width") => width = Some(engine.read_dim(false).into()),
                    Some(b"height") => height = Some(engine.read_dim(false).into()),
                    Some(b"depth") => depth = Some(engine.read_dim(false).into()),
                    _ => break
                }
            }
            PDFDestType::Fitr{width,height,depth}
        }
        Some(b"fitbh") => PDFDestType::Fitbh,
        Some(b"fitbv") => PDFDestType::Fitbv,
        Some(b"fitb") => PDFDestType::Fitb,
        Some(b"fith") => PDFDestType::Fith,
        Some(b"fitv") => PDFDestType::Fitv,
        Some(b"fit") => PDFDestType::Fit,
        _ => todo!("error")
    }
}

pub fn action_spec<ET:EngineTypes>(engine:&mut EngineReferences<ET>) -> ActionSpec {
    match engine.read_keywords(&[b"user",b"goto",b"thread"]) {
        Some(b"user") => {
            let mut ret = String::new();
            engine.read_braced_string(true,&mut ret);
            ActionSpec::User(ret)
        }
        Some(b"goto") => {
            let file = if engine.read_keyword(b"file") {
                let mut file = String::new();
                engine.read_braced_string(true,&mut file);
                Some(file)
            } else {None};
            let struct_ = if engine.read_keyword(b"struct") {
                match num_or_name(engine) {
                    None => {
                        let mut ret = String::new();
                        engine.read_braced_string(true,&mut ret);
                        Some(PDFStruct::Other(ret))
                    },
                    Some(NumOrName::Num(i)) => {
                        Some(PDFStruct::Num(i))
                    },
                    Some(NumOrName::Name(s)) => {
                        Some(PDFStruct::Name(s))
                    },
                }
            } else {None};
            match file {
                None => {
                    match num_or_name(engine) {
                        Some(n) => ActionSpec::Goto(GotoAction::Current {
                            struct_,page:None,target:n
                        }),
                        _ => {
                            let page = if engine.read_keyword(b"page") {
                                Some(engine.read_int(false).into())
                            } else {None};
                            let mut str = String::new();
                            engine.read_braced_string(true,&mut str);
                            ActionSpec::Goto(GotoAction::Current{
                                struct_,page,target:NumOrName::Name(str)
                            })
                        }
                    }
                }
                Some(filename) => {
                    let (page,target) = if engine.read_keyword(b"name") {
                        let mut str = String::new();
                        engine.read_braced_string(true,&mut str);
                        (None,str)
                    } else {
                        let page = if engine.read_keyword(b"page") {
                            Some(engine.read_int(false).into())
                        } else {None};
                        let mut str = String::new();
                        engine.read_braced_string(true,&mut str);
                        (page,str)
                    };
                    let newwindow = match engine.read_keywords(&[b"newwindow",b"nonewwindow"]) {
                        Some(b"newwindow") => Some(true),
                        Some(b"nonewwindow") => Some(false),
                        _ => None
                    };
                    ActionSpec::Goto(GotoAction::File{
                        filename,struct_,page,target,newwindow
                    })
                }
            }
        }
        Some(b"thread") => {
            let file = if engine.read_keyword(b"file") {
                let mut file = String::new();
                engine.read_braced_string(true,&mut file);
                Some(file)
            } else {None};

            match num_or_name(engine) {
                Some(n) => ActionSpec::Thread{file,target:n},
                None => todo!("Expected one of 'num','name'"),
            }

        }
        None => todo!("Expected one of 'user','goto','thread'"),
        _ => unreachable!()
    }
}

// ----------------------------------------------------------------------------------------

#[derive(Clone,Debug)]
pub enum PDFNode<ET:EngineTypes> {
    Obj(PDFObj),
    XForm(PDFXForm<ET>),
    XImage(PDFXImage),
    PDFLiteral(PDFLiteral),
    PDFOutline(PDFOutline),
    PDFCatalog(PDFCatalog),
    PDFDest(PDFDest<ET::Dim>),
    Color(ColorStackAction),
    PDFStartLink(PDFStartLink<ET>),
    PDFEndLink
}

#[derive(Copy,Clone,Debug)]
pub enum ColorStackAction {
    Current(usize),
    Push(usize,PDFColor),
    Pop(usize),
    Set(usize,PDFColor)
}

#[derive(Clone,Debug)]
pub struct PDFStartLink<ET:EngineTypes> {
    pub width:Option<ET::Dim>,
    pub height:Option<ET::Dim>,
    pub depth:Option<ET::Dim>,
    pub attr:Option<String>,
    pub action:ActionSpec,
}

#[derive(Clone,Debug)]
pub struct PDFDest<D:TeXDimen> {
    pub structnum:Option<i64>,
    pub id:NumOrName,
    pub dest:PDFDestType<D>
}

#[derive(Clone,Debug)]
pub struct PDFLiteral {
    pub option:PDFLiteralOption,
    pub literal:String
}

impl<ET:EngineTypes> CustomNodeTrait<ET> for PDFNode<ET>
    where ET::CustomNode : From<PDFNode<ET>> {}

impl<ET:EngineTypes> NodeTrait<ET> for PDFNode<ET>
    where ET::CustomNode : From<PDFNode<ET>> {
    fn height(&self) -> ET::Dim { ET::Dim::default() }
    fn depth(&self) -> ET::Dim { ET::Dim::default() }
    fn width(&self) -> ET::Dim { ET::Dim::default() }
    fn nodetype(&self) -> NodeType { NodeType::WhatsIt }
    fn opaque(&self) -> bool { match self {
        PDFNode::Color(_) => true,
        _ => false
    } }
    fn readable_fmt(&self, indent: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Self::readable_do_indent(indent,f)?;
        match self {
            PDFNode::PDFDest(PDFDest {structnum,id,dest}) =>
                write!(f,"<pdfdest structnum=\"{:?}\", id=\"{:?}\", dest=\"{:?}\">",structnum,id,dest),
            PDFNode::PDFCatalog(c) =>
                write!(f,"<pdfcatalog literal=\"{}\", action=\"{:?}\">",c.literal,c.action),
            PDFNode::PDFOutline(o) =>
                write!(f,"<pdfoutline attr=\"{}\", action=\"{:?}\", count=\"{:?}\", content=\"{}\">",o.attr,o.action,o.count,o.content),
            PDFNode::Obj(o) =>
                write!(f,"<pdfobj literal=\"{}\">",o.0),
            PDFNode::XForm(x) => {
                write!(f, "<pdfxform attr=\"{}\", resources=\"{}\">", x.attr, x.resources)?;
                if let Some(bx) = &x.bx {
                    bx.readable_fmt(indent+2,f)?;
                }
                Self::readable_do_indent(indent,f)?;
                write!(f,"</pdfxform>")
            }
            PDFNode::XImage(_) =>
                write!(f,"<pdfximage>"),
            PDFNode::PDFLiteral(PDFLiteral {option,literal}) =>
                write!(f,"<pdfliteral option=\"{:?}\", literal=\"{:?}\">",option,literal),
            PDFNode::Color(c) => {
                match c {
                    ColorStackAction::Current(i) => write!(f,"<pdfcolorstack current=\"{}\">",i),
                    ColorStackAction::Push(i,c) => write!(f,"<pdfcolorstack push=\"{}\", color=\"{:?}\">",i,c),
                    ColorStackAction::Pop(i) => write!(f,"<pdfcolorstack pop=\"{}\">",i),
                    ColorStackAction::Set(i,c) => write!(f,"<pdfcolorstack set=\"{}\", color=\"{:?}\">",i,c),
                }
            }
            PDFNode::PDFStartLink(PDFStartLink {width,height,depth,attr,action}) => {
                write!(f,"<pdfstartlink")?;
                if let Some(w) = width {
                    write!(f," width=\"{:?}\"",w)?;
                }
                if let Some(h) = height {
                    write!(f," height=\"{:?}\"",h)?;
                }
                if let Some(d) = depth {
                    write!(f," depth=\"{:?}\"",d)?;
                }
                if let Some(a) = attr {
                    write!(f," attr=\"{:?}\"",a)?;
                }
                write!(f," action=\"{:?}\">",action)
            }
            PDFNode::PDFEndLink => write!(f,"<pdfendlink>"),
        }
    }
}

#[derive(Copy,Clone,Debug)]
pub enum PDFLiteralOption {
    None,
    Direct,
    Page
}

pub trait PDFExtension<ET:EngineTypes>: EngineExtension {
    fn pdfmatches(&mut self) -> &mut Vec<String>;
    fn elapsed(&mut self) -> &mut std::time::Instant;
    fn colorstacks(&mut self) -> &mut Vec<Vec<PDFColor>>;
    fn current_colorstack(&mut self) -> &mut usize;
    fn pdfobjs(&mut self) -> &mut Vec<PDFObj>;
    fn pdfxforms(&mut self) -> &mut Vec<PDFXForm<ET>>;
    fn pdfximages(&mut self) -> &mut Vec<PDFXImage>;
}

pub struct MinimalPDFExtension<ET:EngineTypes> {
    matches: Vec<String>,
    elapsed:std::time::Instant,//chrono::DateTime<chrono::Local>,
    colorstacks:Vec<Vec<PDFColor>>,
    current_colorstack:usize,
    pdfobjs:Vec<PDFObj>,
    pdfxforms:Vec<PDFXForm<ET>>,
    pdfximages:Vec<PDFXImage>,
}
impl<ET:EngineTypes> EngineExtension for MinimalPDFExtension<ET> {
    fn new() -> Self {
        Self {
            matches: Vec::new(),
            elapsed: std::time::Instant::now(),
            colorstacks:vec!(vec!(PDFColor::black())),
            current_colorstack:0,
            pdfobjs:Vec::new(),
            pdfxforms:Vec::new(),
            pdfximages:Vec::new(),
        }
    }
}
impl<ET:EngineTypes> PDFExtension<ET> for MinimalPDFExtension<ET> {
    #[inline(always)]
    fn pdfmatches(&mut self) -> &mut Vec<String> {
        &mut self.matches
    }
    #[inline(always)]
    fn elapsed(&mut self) -> &mut std::time::Instant {
        &mut self.elapsed
    }
    #[inline(always)]
    fn colorstacks(&mut self) -> &mut Vec<Vec<PDFColor>> { &mut self.colorstacks }
    #[inline(always)]
    fn current_colorstack(&mut self) -> &mut usize { &mut self.current_colorstack }
    #[inline(always)]
    fn pdfobjs(&mut self) -> &mut Vec<PDFObj> { &mut self.pdfobjs }
    #[inline(always)]
    fn pdfxforms(&mut self) -> &mut Vec<PDFXForm<ET>> { &mut self.pdfxforms }
    #[inline(always)]
    fn pdfximages(&mut self) -> &mut Vec<PDFXImage> { &mut self.pdfximages }
}

#[derive(Debug,Clone)]
pub struct PDFObj(pub String);

#[derive(Debug,Clone)]
pub struct PDFXForm<ET:EngineTypes> {
    pub attr:String,
    pub resources:String,
    pub bx:Option<TeXBox<ET>>
}

#[derive(Debug,Clone)]
pub struct PDFXImage();

#[derive(Debug,Clone)]
pub struct PDFOutline {
    pub attr:String,
    pub action:ActionSpec,
    pub count:Option<i64>,
    pub content:String
}

#[derive(Debug,Clone)]
pub struct PDFCatalog {
    pub literal:String,
    pub action:Option<ActionSpec>
}

#[derive(Debug,Clone,Copy,PartialEq,Eq)]
#[allow(non_snake_case)]
pub struct PDFColor{R:u8,G:u8,B:u8}
impl PDFColor {
    #[inline(always)]
    pub fn black() -> Self { PDFColor{R:0,G:0,B:0} }
    pub fn parse<S:AsRef<str>+std::fmt::Display>(s:S) -> Self {
        macro_rules! parse {
            ($s:expr) => {
                match $s.parse::<f32>() {
                    Ok(f) => f,
                    _ => todo!("Invalid color specification: {}",s)
                }
            }
        }
        let ls : Vec<_> = s.as_ref().split(' ').collect();
        if matches!(ls.last(),Some(&"K")) && ls.len() > 4 {
            let third = 1.0 - parse!(ls[3]);
            let r = 255.0*(1.0 - parse!(ls[0])) * third;
            let g = 255.0*(1.0 - parse!(ls[1])) * third;
            let b = 255.0*(1.0 - parse!(ls[2])) * third;
            if r > 255.0 || g > 255.0 || b > 255.0 || r < 0.0 || g < 0.0 || b < 0.0 {
                todo!("Invalid color specification: {}",s);
            }
            PDFColor{R:(r.round() as u8), G:(g.round() as u8), B:(b.round() as u8)}
        } else if matches!(ls.last(),Some(&"RG")) && ls.len() > 3 {
            let r = 255.0 * parse!(ls[0]);
            let g = 255.0 * parse!(ls[1]);
            let b = 255.0 * parse!(ls[2]);
            if r > 255.0 || g > 255.0 || b > 255.0 || r < 0.0 || g < 0.0 || b < 0.0 {
                todo!("Invalid color specification: {}",s);
            }
            PDFColor{R:(r.round() as u8), G:(g.round() as u8), B:(b.round() as u8)}
        } else if matches!(ls.last(),Some(&"G")) && ls.len() > 1 {
            let x = 255.0 * parse!(ls[0]);
            if x > 255.0 || x < 0.0  {
                todo!("Invalid color specification: {}",s);
            }
            let x = (x.round()) as u8;
            PDFColor{R:x, G:x, B:x}
        } else {
            todo!("Invalid color specification: {}",s);
        }
    }
}
impl Default for PDFColor {
    #[inline(always)]
    fn default() -> Self { PDFColor::black() }
}
impl std::fmt::Display for PDFColor {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f,"#{:02x}{:02x}{:02x}",self.R,self.G,self.B)
    }
}
