use tex_engine::commands::primitives::PRIMITIVES;
use tex_engine::pdflatex::nodes::PDFColor;
use tex_engine::tex::numerics::Dim32;
use tex_engine::utils::HSet;
use tex_glyphs::glyphs::Glyph;
use crate::engine::{Font, Refs, Res};
use crate::html::{HTMLChild, HTMLNode};
use crate::nodes::LineSkip;
use crate::shipout::annotations;
use crate::shipout::utils::ShipoutMode;

pub(crate) struct ShipoutState {
    pub(crate) output:Vec<HTMLChild>,
    pub(crate) nodes:Vec<HTMLNode>,
    pub(crate) fonts: Vec<Font>,
    pub(crate) colors:Vec<PDFColor>,
    pub(crate) widths: Vec<Dim32>,
    pub(crate) lineskip:Vec<LineSkip>,
    pub(crate) modes:Vec<ShipoutMode>,
    pub(crate) fontlinks:HSet<String>,
    pub(crate) in_content:bool,
    pub(crate) nullfont:Option<Font>,
    pub(crate) missing_glyphs: HSet<(String,u8,String)>,
    pub(crate) missing_fonts: HSet<String>
}
impl Default for ShipoutState {
    fn default() -> Self {
        Self {
            output:Vec::new(),
            nodes:Vec::new(),
            fonts:Vec::new(),
            widths:Vec::new(),
            colors:vec!(PDFColor::black()),
            fontlinks:HSet::default(),
            modes:vec!(ShipoutMode::Top),
            lineskip:Vec::new(),
            in_content:false,
            nullfont:None,
            missing_fonts:HSet::default(),
            missing_glyphs:HSet::default()
        }
    }
}
impl ShipoutState {
    pub fn mode(&self) -> ShipoutMode {
        *self.modes.last().unwrap()
    }
    pub fn do_in_and<F1:FnOnce(&mut Self) -> Res<()>>(&mut self, node:HTMLNode,mode:Option<ShipoutMode>, cont:F1) -> Res<HTMLNode> {
        let disc = node.tag.clone();
        self.nodes.push(node);
        if let Some(mode) = mode {
            self.modes.push(mode);
        }
        cont(self)?;
        let node = annotations::close_all(self.mode(),&mut self.nodes,|t| t == &disc);
        if let Some(_) = mode {
            self.modes.pop();
        }
        if node.tag == disc {
            Ok(node)
        } else {
            todo!()
        }
    }

    pub fn do_in<F1:FnOnce(&mut Self) -> Res<()>>(&mut self, node:HTMLNode,mode:Option<ShipoutMode>, cont:F1) -> Res<()> {
        let r = self.do_in_and(node, mode,cont)?;
        self.push(r);Ok(())
    }

    pub fn push_child(&mut self, child:HTMLChild) {
        let mode = self.mode();
        match self.nodes.last_mut() {
            Some(parent) => parent.push_child(mode,child),
            None => self.output.push(child),
        }
    }
    pub fn push_space(&mut self) {
        let mode = self.mode();
        match self.nodes.last_mut() {
            Some(parent) => parent.push_space(mode),
            None => unreachable!()
        }
    }

    pub fn push_comment<D:std::fmt::Display>(&mut self, d:D) {
        match self.nodes.last_mut() {
            Some(parent) => parent.push_comment(d),
            None => self.output.push(HTMLChild::Text(d.to_string()))
        }
    }

    pub fn push(&mut self, node:HTMLNode) {
        let mode = self.mode();
        match self.nodes.last_mut() {
            Some(parent) => parent.push_open_node(mode,node),
            None => node.close(mode,&mut self.output),
        }
    }

    pub fn push_glyph(&mut self,glyph:Glyph) {
        let mode = self.mode();
        self.nodes.last_mut().unwrap().push_glyph(mode,glyph)
    }
}

use tex_engine::engine::{fontsystem::FontSystem,state::State};

pub(crate) fn split_state<R,F:FnOnce(Refs,&mut ShipoutState) -> R>(engine:Refs,f:F) -> R {
    let mut state = std::mem::take(&mut engine.aux.extension.state);
    if state.nullfont.is_none() {
        state.nullfont = Some(engine.fontsystem.null())
    }
    if state.fonts.is_empty() {
        state.fonts.push(engine.state.get_current_font().clone());
    }
    if state.widths.is_empty() {
        state.widths.push(engine.state.get_primitive_dim(PRIMITIVES.hsize));
    }
    let r = f(engine,&mut state);
    engine.aux.extension.state = state;
    r
}