use crate::engine::{EngineExtension, EngineReferences, EngineTypes};
use crate::tex::nodes::{NodeTrait, PreShipoutNode, PreShipoutNodeTrait, ShipoutNode, TeXBox, TopNodeTrait};
use crate::tex::numerics::TeXDimen;
use crate::tex::types::NodeType;

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
                    _ => unreachable!()
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

pub trait PDFNodeTrait<ET:EngineTypes> {
    fn from_pdfobj(o:PDFObj) -> Self;
    fn from_pdfxform(x:PDFXForm<ET,PreShipoutNode<ET>>) -> Self;
    fn from_pdfximage(x:PDFXImage) -> Self;
    fn from_pdfliteral(s:String,o:PDFLiteralOption) -> Self;
    fn from_pdfoutline(o:PDFOutline) -> Self;
    fn from_pdfcatalog(c:PDFCatalog) -> Self;
    fn from_pdfdest(structnum:Option<i64>,id:NumOrName,dest:PDFDestType<ET::Dim>) -> Self;
}
pub enum PDFNode<ET:EngineTypes,T:TopNodeTrait<ET>> {
    Obj(PDFObj),
    XForm(PDFXForm<ET,T>),
    XImage(PDFXImage),
    PDFLiteral{
        option:PDFLiteralOption,
        literal:String
    },
    PDFOutline(PDFOutline),
    PDFCatalog(PDFCatalog),
    PDFDest {
        structnum:Option<i64>,
        id:NumOrName,
        dest:PDFDestType<ET::Dim>
    }
}
impl<ET:EngineTypes,T:TopNodeTrait<ET>> std::fmt::Debug for PDFNode<ET,T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PDFNode::Obj(o) => write!(f,"PDFNode::Obj({:?})",o),
            PDFNode::XForm(x) => write!(f,"PDFNode::XForm({:?})",x),
            PDFNode::XImage(x) => write!(f,"PDFNode::XImage({:?})",x),
            PDFNode::PDFLiteral{option,literal} => write!(f,"PDFNode::PDFLiteral{{option:{:?},literal:{:?}}}",option,literal),
            PDFNode::PDFOutline(o) => write!(f,"PDFNode::PDFOutline({:?})",o),
            PDFNode::PDFCatalog(c) => write!(f,"PDFNode::PDFCatalog({:?})",c),
            PDFNode::PDFDest { structnum, id, dest } => write!(f,"PDFNode::PDFDest{{structnum:{:?},id:{:?},dest:{:?}}}",structnum,id,dest),
        }
    }

}
impl<ET:EngineTypes,T:TopNodeTrait<ET>> Clone for PDFNode<ET,T> {
    fn clone(&self) -> Self {
        match self {
            PDFNode::Obj(o) => PDFNode::Obj(o.clone()),
            PDFNode::XForm(x) => PDFNode::XForm(x.clone()),
            PDFNode::XImage(x) => PDFNode::XImage(x.clone()),
            PDFNode::PDFLiteral{option,literal} => PDFNode::PDFLiteral{option:*option,literal:literal.clone()},
            PDFNode::PDFOutline(o) => PDFNode::PDFOutline(o.clone()),
            PDFNode::PDFCatalog(c) => PDFNode::PDFCatalog(c.clone()),
            PDFNode::PDFDest { structnum, id, dest } => PDFNode::PDFDest{structnum:*structnum,id:id.clone(),dest:dest.clone()},
        }
    }
}
impl<ET:EngineTypes<PreCustomNode=Self,ShipoutCustomNode=PDFNode<ET,ShipoutNode<ET>>>> PreShipoutNodeTrait<ET> for PDFNode<ET,PreShipoutNode<ET>> {
    fn as_node(self) -> PreShipoutNode<ET> {
        PreShipoutNode::Custom(self)
    }
    fn shipout(self, list: &mut Vec<ShipoutNode<ET>>, engine: &mut EngineReferences<ET>) {
        match self {
            PDFNode::XForm(x) => {
                let mut v = Vec::with_capacity(1);
                for b in x.bx {
                    b.shipout(&mut v,engine);
                }
                list.push(ShipoutNode::Custom(PDFNode::XForm(PDFXForm{
                    attr:x.attr,
                    resources:x.resources,
                    bx:if let Some(ShipoutNode::Box(bx)) = v.into_iter().next() {
                        Some(bx)
                    } else {
                        None
                    }
                })));
            },
            PDFNode::Obj(o) => list.push(ShipoutNode::Custom(PDFNode::Obj(o))),
            PDFNode::PDFLiteral{option,literal} => list.push(ShipoutNode::Custom(PDFNode::PDFLiteral{option,literal})),
            PDFNode::XImage(x) => list.push(ShipoutNode::Custom(PDFNode::XImage(x))),
            PDFNode::PDFOutline(o) => list.push(ShipoutNode::Custom(PDFNode::PDFOutline(o))),
            PDFNode::PDFCatalog(c) => list.push(ShipoutNode::Custom(PDFNode::PDFCatalog(c))),
            PDFNode::PDFDest { structnum, id, dest } => list.push(ShipoutNode::Custom(PDFNode::PDFDest{structnum,id,dest})),
        }
    }
}
impl<ET:EngineTypes> PDFNodeTrait<ET> for PDFNode<ET,PreShipoutNode<ET>> {
    fn from_pdfobj(o: PDFObj) -> Self { PDFNode::Obj(o) }
    fn from_pdfxform(x: PDFXForm<ET,PreShipoutNode<ET>>) -> Self { PDFNode::XForm(x) }
    fn from_pdfximage(x: PDFXImage) -> Self { PDFNode::XImage(x) }
    fn from_pdfliteral(s: String,o:PDFLiteralOption) -> Self { PDFNode::PDFLiteral{option:o,literal:s} }
    fn from_pdfoutline(o: PDFOutline) -> Self { PDFNode::PDFOutline(o) }
    fn from_pdfcatalog(c: PDFCatalog) -> Self { PDFNode::PDFCatalog(c) }
    fn from_pdfdest(structnum: Option<i64>, id: NumOrName, dest: PDFDestType<ET::Dim>) -> Self {
        PDFNode::PDFDest{structnum,id,dest}
    }
}
impl<ET:EngineTypes,T:TopNodeTrait<ET>> NodeTrait<ET> for PDFNode<ET,T> {
    fn height(&self) -> ET::Dim { ET::Dim::default() }
    fn depth(&self) -> ET::Dim { ET::Dim::default() }
    fn width(&self) -> ET::Dim { ET::Dim::default() }
    fn nodetype(&self) -> NodeType { NodeType::WhatsIt }
    fn opaque(&self) -> bool { false }
    fn readable_fmt(&self, indent: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Self::readable_do_indent(indent,f);
        match self {
            PDFNode::PDFDest {structnum,id,dest} =>
                write!(f,"<pdfdest structnum=\"{:?}\", id=\"{:?}\", dest=\"{:?}\">",structnum,id,dest),
            PDFNode::PDFCatalog(c) =>
                write!(f,"<pdfcatalog literal=\"{}\", action=\"{:?}\">",c.literal,c.action),
            PDFNode::PDFOutline(o) =>
                write!(f,"<pdfoutline attr=\"{}\", action=\"{:?}\", count=\"{:?}\", content=\"{}\">",o.attr,o.action,o.count,o.content),
            PDFNode::Obj(o) =>
                write!(f,"<pdfobj literal=\"{}\">",o.0),
            PDFNode::XForm(x) => {
                write!(f, "<pdfxform attr=\"{}\", resources=\"{}\">", x.attr, x.resources);
                if let Some(bx) = &x.bx {
                    bx.readable_fmt(indent+2,f)?;
                }
                Self::readable_do_indent(indent,f);
                write!(f,"</pdfxform>")
            }
            PDFNode::XImage(x) =>
                write!(f,"<pdfximage>"),
            PDFNode::PDFLiteral{option,literal} =>
                write!(f,"<pdfliteral option=\"{:?}\", literal=\"{:?}\">",option,literal),
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
    fn pdfxforms(&mut self) -> &mut Vec<PDFXForm<ET,PreShipoutNode<ET>>>;
    fn pdfximages(&mut self) -> &mut Vec<PDFXImage>;
}

pub struct MinimalPDFExtension<ET:EngineTypes> {
    matches: Vec<String>,
    elapsed:std::time::Instant,//chrono::DateTime<chrono::Local>,
    colorstacks:Vec<Vec<PDFColor>>,
    current_colorstack:usize,
    pdfobjs:Vec<PDFObj>,
    pdfxforms:Vec<PDFXForm<ET,PreShipoutNode<ET>>>,
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
    fn pdfxforms(&mut self) -> &mut Vec<PDFXForm<ET,PreShipoutNode<ET>>> { &mut self.pdfxforms }
    #[inline(always)]
    fn pdfximages(&mut self) -> &mut Vec<PDFXImage> { &mut self.pdfximages }
}

#[derive(Debug,Clone)]
pub struct PDFObj(pub String);

#[derive(Debug,Clone)]
pub struct PDFXForm<ET:EngineTypes,T:TopNodeTrait<ET>> {
    pub attr:String,
    pub resources:String,
    pub bx:Option<TeXBox<ET,T>>
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

#[derive(Debug,Clone,Copy)]
pub struct PDFColor{R:u8,G:u8,B:u8}
impl PDFColor {
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
