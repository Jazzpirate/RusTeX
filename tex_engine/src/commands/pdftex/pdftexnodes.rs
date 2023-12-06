use crate::engine::{EngineExtension, EngineReferences, EngineTypes};
use crate::tex::nodes::{NodeTrait, PreShipoutNode, PreShipoutNodeTrait, ShipoutNode, TeXBox, TopNodeTrait};
use crate::tex::types::NodeType;

pub trait PDFNodeTrait<ET:EngineTypes> {
    fn from_pdfobj(o:PDFObj) -> Self;
    fn from_pdfxform(x:PDFXForm<ET,PreShipoutNode<ET>>) -> Self;
    fn from_pdfximage(x:PDFXImage) -> Self;
    fn from_pdfliteral(s:String,o:PDFLiteralOption) -> Self;
}
pub enum PDFNode<ET:EngineTypes,T:TopNodeTrait<ET>> {
    Obj(PDFObj),
    XForm(PDFXForm<ET,T>),
    XImage(PDFXImage),
    PDFLiteral{
        option:PDFLiteralOption,
        literal:String
    }
}
impl<ET:EngineTypes,T:TopNodeTrait<ET>> std::fmt::Debug for PDFNode<ET,T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PDFNode::Obj(o) => write!(f,"PDFNode::Obj({:?})",o),
            PDFNode::XForm(x) => write!(f,"PDFNode::XForm({:?})",x),
            PDFNode::XImage(x) => write!(f,"PDFNode::XImage({:?})",x),
            PDFNode::PDFLiteral{option,literal} => write!(f,"PDFNode::PDFLiteral{{option:{:?},literal:{:?}}}",option,literal)
        }
    }

}
impl<ET:EngineTypes,T:TopNodeTrait<ET>> Clone for PDFNode<ET,T> {
    fn clone(&self) -> Self {
        match self {
            PDFNode::Obj(o) => PDFNode::Obj(o.clone()),
            PDFNode::XForm(x) => PDFNode::XForm(x.clone()),
            PDFNode::XImage(x) => PDFNode::XImage(x.clone()),
            PDFNode::PDFLiteral{option,literal} => PDFNode::PDFLiteral{option:*option,literal:literal.clone()}
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
        }
    }
}
impl<ET:EngineTypes> PDFNodeTrait<ET> for PDFNode<ET,PreShipoutNode<ET>> {
    fn from_pdfobj(o: PDFObj) -> Self { PDFNode::Obj(o) }
    fn from_pdfxform(x: PDFXForm<ET,PreShipoutNode<ET>>) -> Self { PDFNode::XForm(x) }
    fn from_pdfximage(x: PDFXImage) -> Self { PDFNode::XImage(x) }
    fn from_pdfliteral(s: String,o:PDFLiteralOption) -> Self { PDFNode::PDFLiteral{option:o,literal:s} }
}
impl<ET:EngineTypes,T:TopNodeTrait<ET>> NodeTrait<ET> for PDFNode<ET,T> {
    fn height(&self) -> ET::Dim { ET::Dim::default() }
    fn depth(&self) -> ET::Dim { ET::Dim::default() }
    fn width(&self) -> ET::Dim { ET::Dim::default() }
    fn nodetype(&self) -> NodeType { NodeType::WhatsIt }
    fn opaque(&self) -> bool { false }
    fn readable_fmt(&self, indent: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
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
