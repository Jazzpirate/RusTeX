use pdfium_render::pdfium::Pdfium;
use tex_engine::pdflatex::nodes::{MinimalPDFExtension, PDFAnnot, PDFColor, PDFExtension, PDFObj, PDFXForm, PDFXImage};
use tex_engine::engine::{EngineExtension, EngineTypes};
use crate::engine::{Font, Refs, Types};
use crate::nodes::RusTeXNode;
use crate::shipout::ShipoutState;
use tex_engine::engine::stomach::Stomach as StomachT;
use crate::stomach::RusTeXStomach;

#[derive(Debug,Clone)]
pub(crate) struct FontChange(pub(crate) Font);

pub struct RusTeXExtension {
    pdf: MinimalPDFExtension<Types>,
    pub(crate) state:ShipoutState,
    pub(crate) change_markers:Vec<Vec<FontChange>>
}
impl RusTeXExtension {
    pub(crate) fn push(&mut self) {
        self.change_markers.push(vec!())
    }
    pub(crate) fn pop(&mut self) -> Vec<FontChange> {
        self.change_markers.pop().unwrap()
    }
}
impl EngineExtension for RusTeXExtension {
    fn new() -> Self { Self { pdf: MinimalPDFExtension::new(),state:ShipoutState::default(),change_markers:vec!() } }
}
impl PDFExtension<Types> for RusTeXExtension {

    fn pdfmatches(&mut self) -> &mut Vec<String> {
        self.pdf.pdfmatches()
    }

    fn elapsed(&mut self) -> &mut std::time::Instant {
        self.pdf.elapsed()
    }

    fn colorstacks(&mut self) -> &mut Vec<Vec<PDFColor>> { self.pdf.colorstacks() }

    fn current_colorstack(&mut self) -> &mut usize { self.pdf.current_colorstack() }

    fn pdfobjs(&mut self) -> &mut Vec<PDFObj> { self.pdf.pdfobjs() }

    fn pdfxforms(&mut self) -> &mut Vec<PDFXForm<Types>> { self.pdf.pdfxforms() }

    fn pdfximages(&mut self) -> &mut Vec<PDFXImage<Types>> { self.pdf.pdfximages() }

    fn pdfannots(&mut self) -> &mut Vec<PDFAnnot<Types>> {
        self.pdf.pdfannots()
    }


    fn pdfium_direct(&mut self) -> &mut Option<Pdfium> {
        self.pdf.pdfium_direct()
    }
}