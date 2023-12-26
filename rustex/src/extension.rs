use tex_engine::commands::pdftex::pdftexnodes::{MinimalPDFExtension, PDFColor, PDFExtension, PDFObj, PDFXForm, PDFXImage};
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
    #[inline(always)]
    fn pdfmatches(&mut self) -> &mut Vec<String> {
        self.pdf.pdfmatches()
    }
    #[inline(always)]
    fn elapsed(&mut self) -> &mut std::time::Instant {
        self.pdf.elapsed()
    }
    #[inline(always)]
    fn colorstacks(&mut self) -> &mut Vec<Vec<PDFColor>> { self.pdf.colorstacks() }
    #[inline(always)]
    fn current_colorstack(&mut self) -> &mut usize { self.pdf.current_colorstack() }
    #[inline(always)]
    fn pdfobjs(&mut self) -> &mut Vec<PDFObj> { self.pdf.pdfobjs() }
    #[inline(always)]
    fn pdfxforms(&mut self) -> &mut Vec<PDFXForm<Types>> { self.pdf.pdfxforms() }
    #[inline(always)]
    fn pdfximages(&mut self) -> &mut Vec<PDFXImage> { self.pdf.pdfximages() }
}