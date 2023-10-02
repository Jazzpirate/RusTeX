/*! A TeX state */

use string_interner::Symbol;
use crate::engine::filesystem::File;
use crate::tex::catcodes::{CategoryCode, CategoryCodeScheme};
use crate::engine::state::fields::{CharField, SingleValueField, VecField, KeyValueField, StateField, HashMapField, BoxField, TokField, TokMapField};
use crate::engine::state::modes::{BoxMode, FontStyle, GroupType, TeXMode};
use crate::tex::commands::{BaseCommand, Command, CommandSource};
use crate::tex::token::{CSLike, Token};
use crate::utils::strings::TeXStr;
use crate::engine::{EngineRef, EngineType};
use crate::engine::memory::{Interner, Memory};
use crate::tex::commands::pdftex::{PDFColorstack, PDFObj, PDFXForm};
use crate::tex::nodes::HVBox;
use crate::tex::fonts::FontStore;
use crate::tex::numbers::{Int, MuSkip, Skip};
use crate::throw;
use crate::utils::errors::TeXError;
use crate::utils::strings::CharType;

pub mod fields;
pub mod modes;
pub mod state_changes;

/// A TeX state
pub trait State<ET:EngineType>:Clone+'static {

    fn push_csname(&mut self) -> usize;
    fn current_csname(&self) -> Option<usize>;
    fn pop_csname(&mut self);

    fn set_afterassignment(&mut self,t:ET::Token);
    fn take_afterassignment(&mut self) -> Option<ET::Token>;
    fn get_parshape(&self) -> Option<&Vec<(ET::Dim,ET::Dim)>>;
    fn set_parshape(&mut self,v:Vec<(ET::Dim,ET::Dim)>,globally:bool);

    /// The current [`TeXMode`]
    fn mode(&self) -> TeXMode;
    fn set_mode(&mut self, mode:TeXMode);

    fn file_openout(&mut self,i:usize,f:ET::File);
    fn file_closeout(&mut self, i: usize);
    fn file_openin(&mut self, i: usize, f: ET::File,interner:&mut Interner);
    fn file_closein(&mut self, i: usize);
    fn get_open_out_file(&self,i:usize) -> Option<ET::File>;
    fn get_open_in_file(&self,i:usize) -> Option<ET::File>;

    /// push a new group onto the stack
    fn stack_push(&mut self, g: GroupType);

    fn stack_pop(&mut self,memory:&mut Memory<ET>) -> Option<(Vec<ET::Token>,GroupType)>;
    fn grouplevel(&self) -> usize;

    fn get_displaymode(&self) -> bool;
    fn set_displaymode(&mut self,value:bool,globally:bool);

    /// get the current group type
    fn get_grouptype(&self) -> GroupType;

    fn get_fontstyle(&self) -> FontStyle;
    fn set_fontstyle(&mut self,fs:FontStyle,globally:bool);

    /// get the current escape character (`\escapechar`)
    fn get_escapechar(&self) -> Option<ET::Char>;
    /// set the current escape character (`\escapechar`)
    fn set_escapechar(&mut self, c: Option<ET::Char>, globally:bool);

    /// get the current endline character (`\endlinechar`)
    fn get_endlinechar(&self) -> Option<ET::Char>;
    /// set the current endline character (`\endlinechar`)
    fn set_endlinechar(&mut self, c: Option<ET::Char>, globally:bool);

    /// get the current newline character (`\newlinechar`)
    fn get_newlinechar(&self) -> Option<ET::Char>;
    /// set the current newline character (`\newlinechar`)
    fn set_newlinechar(&mut self, c: Option<ET::Char>, globally:bool);

    /// get the current [`BaseCommand`] with name `name:`[`TeXStr`]
    fn get_command(&self, name:TeXStr) -> Option<&Command<ET>>;
    /// get the current [`BaseCommand`] for the active character `c`
    fn get_ac_command(&self, c: ET::Char) -> Option<&Command<ET>>;
    /// set the current [`BaseCommand`] with name `name:`[`TeXStr`]
    fn set_command(&mut self, name:TeXStr, cmd:Option<Command<ET>>, globally:bool);
    /// set the current [`BaseCommand`] for the active character `c`
    fn set_ac_command(&mut self, c: ET::Char, cmd:Option<Command<ET>>, globally:bool);

    /// get the current [`CategoryCodeScheme`]
    fn get_catcode_scheme(&self) -> &CategoryCodeScheme<ET::Char>;
    /// set the current [`CategoryCode`] for a character
    fn set_catcode(&mut self, c: ET::Char, cc:CategoryCode, globally:bool);


    /// get the current space factor code for the character
    fn get_sfcode(&self,c:ET::Char) -> ET::Int;
    /// set the current space factor code for a character
    fn set_sfcode(&mut self, c: ET::Char, v:ET::Int, globally:bool);

    /// get the uppercase character for a character
    fn get_uccode(&self, c: ET::Char) -> ET::Char;
    /// set the uppercase character for a character
    fn set_uccode(&mut self, c: ET::Char, uc: ET::Char, globally:bool);

    /// get the lowercase character for a character
    fn get_lccode(&self, c: ET::Char) -> ET::Char;
    /// set the lowercase character for a character
    fn set_lccode(&mut self, c: ET::Char, lc: ET::Char, globally:bool);

    /// get the mathcode for a character
    fn get_mathcode(&self, c: ET::Char) -> ET::Int;
    /// set the mathcode for a character
    fn set_mathcode(&mut self, c: ET::Char, lc: ET::Int, globally:bool);

    /// get the delimiter code for a character
    fn get_delcode(&self, c: ET::Char) -> ET::Int;
    /// set the delimiter code for a character
    fn set_delcode(&mut self, c: ET::Char, lc: ET::Int, globally:bool);

    /// get the value of an integer register
    fn get_int_register(&self,i:usize) -> ET::Int;
    /// set the value of an integer register
    fn set_int_register(&mut self,i:usize,v:ET::Int,globally:bool);

    /// get the value of a dimension register
    fn get_dim_register(&self,i:usize) -> ET::Dim;
    /// set the value of a dimension register
    fn set_dim_register(&mut self,i:usize,v:ET::Dim,globally:bool);

    /// get the value of a skip register
    fn get_skip_register(&self,i:usize) -> Skip<ET::SkipDim>;
    /// set the value of a skip register
    fn set_skip_register(&mut self,i:usize,v:Skip<ET::SkipDim>,globally:bool);

    /// get the value of a skip register
    fn get_muskip_register(&self,i:usize) -> MuSkip<ET::MuDim,ET::MuStretchShrinkDim>;
    /// set the value of a skip register
    fn set_muskip_register(&mut self,i:usize,v:MuSkip<ET::MuDim,ET::MuStretchShrinkDim>,globally:bool);

    /// get the value of a skip register
    fn get_toks_register(&self,i:usize) -> Option<&Vec<ET::Token>>;
    /// set the value of a skip register
    fn set_toks_register(&mut self,i:usize,v:Vec<ET::Token>,globally:bool,memory:&mut Memory<ET>);

    fn get_box_register(&mut self,i:usize) -> Option<&mut HVBox<ET>>;
    fn set_box_register(&mut self,i:usize,v:HVBox<ET>,globally:bool);
    fn take_box_register(&mut self,i:usize) -> HVBox<ET>;

    /// get a primitive integer value
    fn get_primitive_int(&self,name:&'static str) -> ET::Int;
    /// set a primitive integer value
    fn set_primitive_int(&mut self,name:&'static str,v:ET::Int,globally:bool);

    /// get a primitive dimension register
    fn get_primitive_dim(&self, name:&'static str) -> ET::Dim;
    /// set a primitive dimension register
    fn set_primitive_dim(&mut self, name:&'static str, v:ET::Dim, globally:bool);

    /// get a primitive dimension register
    fn get_primitive_skip(&self, name:&'static str) -> Skip<ET::SkipDim>;
    /// set a primitive dimension register
    fn set_primitive_skip(&mut self, name:&'static str, v:Skip<ET::SkipDim>, globally:bool);

    /// get a primitive dimension register
    fn get_primitive_muskip(&self, name:&'static str) -> MuSkip<ET::MuDim,ET::MuStretchShrinkDim>;
    /// set a primitive dimension register
    fn set_primitive_muskip(&mut self, name:&'static str, v:MuSkip<ET::MuDim,ET::MuStretchShrinkDim>, globally:bool);

    /// get a primitive token register
    fn get_primitive_toks(&self, name:&'static str) -> Option<&Vec<ET::Token>>;
    /// set a primitive token register
    fn set_primitive_toks(&mut self, name:&'static str, v:Vec<ET::Token>, globally:bool,memory:&mut Memory<ET>);

    /// get the current font
    fn get_current_font(&self) -> ET::FontRef;
    /// set the current font
    fn set_current_font(&mut self, f:ET::FontRef, globally:bool);

    fn get_textfont(&self, i:usize) -> ET::FontRef;
    fn set_textfont(&mut self, i:usize, f:ET::FontRef, globally:bool);

    fn get_scriptfont(&self, i:usize) -> ET::FontRef;
    fn set_scriptfont(&mut self, i:usize, f:ET::FontRef, globally:bool);

    fn get_scriptscriptfont(&self, i:usize) -> ET::FontRef;
    fn set_scriptscriptfont(&mut self, i:usize, f:ET::FontRef, globally:bool);

    fn push_aftergroup(&mut self, t:ET::Token);
}

impl<ET:EngineType> EngineRef<ET> {
    pub fn set_command_for_tk(&mut self, cs:CSLike<ET::Char>, cmd:Option<Command<ET>>, globally:bool) {
        match cs {
            CSLike::CS(cs) => self.state.set_command(cs,cmd, globally),
            CSLike::ActiveChar(c) => self.state.set_ac_command(c,cmd, globally),
        }
    }

    pub fn set_relax(&mut self,cs:CSLike<ET::Char>,source:&CommandSource<ET>,globally:bool) {
        match cs {
            CSLike::ActiveChar(c) => {
                self.state.set_ac_command(c, Some(Command::new(BaseCommand::Relax,Some(source))), globally)
            }
            CSLike::CS(name) => {
                self.state.set_command(name.clone(), Some(Command::new(BaseCommand::Relax,Some(source))), globally)
            }
        }
    }
}

pub trait PDFState<ET:EngineType>:State<ET> {
    fn pdfmatches(&mut self) -> &mut Vec<String>;
    fn pdfobjs(&mut self) -> &mut Vec<PDFObj>;
    fn pdfxforms(&mut self) -> &mut Vec<PDFXForm<ET>>;
    fn pdfcolorstacks(&mut self) -> &mut Vec<PDFColorstack>;
    fn set_current_colorstack(&mut self,index:usize);
    fn get_colorstack(&mut self,u:usize) -> &mut PDFColorstack;
}

#[derive(Clone)]
pub struct PDFStateWrapper<ET:EngineType,S:State<ET>> {
    pub state:S,
    pdfmatches:Vec<String>,
    pdfobjs:Vec<PDFObj>,
    pdfxforms:Vec<PDFXForm<ET>>,
    pdfcolorstacks:Vec<PDFColorstack>,
    current_colorstack:usize,
}
impl<ET:EngineType,S:State<ET>> PDFStateWrapper<ET,S> {
    pub fn new(state:S) -> Self {
        Self {
            state,
            pdfmatches:vec!(),
            pdfobjs:vec!(),
            pdfxforms:vec!(),
            pdfcolorstacks:vec!(PDFColorstack(vec!())),
            current_colorstack:0,
        }
    }
}

impl<ET:EngineType,S:State<ET>> PDFState<ET> for PDFStateWrapper<ET,S> {
    fn pdfmatches(&mut self) -> &mut Vec<String> { &mut self.pdfmatches }
    fn pdfobjs(&mut self) -> &mut Vec<PDFObj> { &mut self.pdfobjs }
    fn pdfxforms(&mut self) -> &mut Vec<PDFXForm<ET>> { &mut self.pdfxforms }
    fn pdfcolorstacks(&mut self) -> &mut Vec<PDFColorstack> {
        &mut self.pdfcolorstacks
    }
    fn get_colorstack(&mut self, u: usize) -> &mut PDFColorstack {
        &mut self.pdfcolorstacks[u]
    }
    fn set_current_colorstack(&mut self, index: usize) {
        self.current_colorstack = 0
    }
}
impl<ET:EngineType,S:State<ET>> State<ET> for PDFStateWrapper<ET,S> {
    fn get_fontstyle(&self) -> FontStyle { self.state.get_fontstyle() }
    fn set_fontstyle(&mut self, fs: FontStyle,globally:bool) { self.state.set_fontstyle(fs,globally) }
    fn current_csname(&self) -> Option<usize> { self.state.current_csname() }
    fn file_closein(&mut self, i: usize) { self.state.file_closein(i) }
    fn file_closeout(&mut self, i: usize) { self.state.file_closeout(i) }
    fn file_openin(&mut self, i: usize, f: ET::File,interner:&mut Interner) { self.state.file_openin(i,f,interner) }
    fn file_openout(&mut self, i: usize, f: ET::File) { self.state.file_openout(i,f) }
    fn get_displaymode(&self) -> bool { self.state.get_displaymode() }
    fn set_displaymode(&mut self, value: bool, globally: bool) { self.state.set_displaymode(value,globally) }
    fn get_open_in_file(&self, i: usize) -> Option<ET::File> { self.state.get_open_in_file(i) }
    fn get_open_out_file(&self, i: usize) -> Option<ET::File> { self.state.get_open_out_file(i) }
    fn get_box_register(&mut self, i: usize) -> Option<&mut HVBox<ET>> { self.state.get_box_register(i) }
    fn get_catcode_scheme(&self) -> &CategoryCodeScheme<ET::Char> { self.state.get_catcode_scheme() }
    fn get_command(&self, name: TeXStr) -> Option<&Command<ET>> { self.state.get_command(name) }
    fn get_ac_command(&self, c: ET::Char) -> Option<&Command<ET>> { self.state.get_ac_command(c) }
    fn get_current_font(&self) -> ET::FontRef { self.state.get_current_font() }
    fn get_dim_register(&self, i: usize) -> ET::Dim { self.state.get_dim_register(i) }
    fn get_escapechar(&self) -> Option<ET::Char> { self.state.get_escapechar() }
    fn get_endlinechar(&self) -> Option<ET::Char> { self.state.get_endlinechar() }
    fn get_grouptype(&self) -> GroupType { self.state.get_grouptype() }
    fn get_int_register(&self, i: usize) -> ET::Int { self.state.get_int_register(i) }
    fn get_lccode(&self, c: ET::Char) -> ET::Char { self.state.get_lccode(c) }
    fn get_mathcode(&self, c: ET::Char) -> ET::Int { self.state.get_mathcode(c) }
    fn get_muskip_register(&self, i: usize) -> MuSkip<ET::MuDim, ET::MuStretchShrinkDim> { self.state.get_muskip_register(i) }
    fn get_newlinechar(&self) -> Option<ET::Char> { self.state.get_newlinechar() }
    fn get_parshape(&self) -> Option<&Vec<(ET::Dim, ET::Dim)>> { self.state.get_parshape() }
    fn get_primitive_dim(&self, name: &'static str) -> ET::Dim { self.state.get_primitive_dim(name) }
    fn get_primitive_int(&self, name: &'static str) -> ET::Int { self.state.get_primitive_int(name) }
    fn get_primitive_muskip(&self, name: &'static str) -> MuSkip<ET::MuDim, ET::MuStretchShrinkDim> { self.state.get_primitive_muskip(name) }
    fn get_primitive_skip(&self, name: &'static str) -> Skip<ET::SkipDim> { self.state.get_primitive_skip(name) }
    fn get_primitive_toks(&self, name: &'static str) -> Option<&Vec<ET::Token>> { self.state.get_primitive_toks(name) }
    fn get_scriptfont(&self, i: usize) -> ET::FontRef { self.state.get_scriptfont(i) }
    fn get_scriptscriptfont(&self, i: usize) -> ET::FontRef { self.state.get_scriptscriptfont(i) }
    fn get_skip_register(&self, i: usize) -> Skip<ET::SkipDim> { self.state.get_skip_register(i) }
    fn get_sfcode(&self, c: ET::Char) -> ET::Int { self.state.get_sfcode(c) }
    fn get_textfont(&self, i: usize) -> ET::FontRef { self.state.get_textfont(i) }
    fn get_toks_register(&self, i: usize) -> Option<&Vec<ET::Token>> { self.state.get_toks_register(i) }
    fn get_uccode(&self, c: ET::Char) -> ET::Char { self.state.get_uccode(c) }
    fn get_delcode(&self, c: ET::Char) -> ET::Int { self.state.get_delcode(c) }
    fn set_delcode(&mut self, c: ET::Char, lc: ET::Int, globally: bool) { self.state.set_delcode(c,lc,globally) }
    fn grouplevel(&self) -> usize { self.state.grouplevel() }
    fn mode(&self) -> TeXMode { self.state.mode() }
    fn pop_csname(&mut self) { self.state.pop_csname() }
    fn push_aftergroup(&mut self, t: ET::Token) { self.state.push_aftergroup(t) }
    fn push_csname(&mut self) -> usize { self.state.push_csname() }
    fn set_ac_command(&mut self, c: ET::Char, cmd: Option<Command<ET>>, globally: bool) { self.state.set_ac_command(c,cmd,globally) }
    fn set_afterassignment(&mut self, t: ET::Token) { self.state.set_afterassignment(t) }
    fn set_box_register(&mut self, i: usize, v: HVBox<ET>, globally: bool) { self.state.set_box_register(i,v,globally) }
    fn set_catcode(&mut self, c: ET::Char, cc: CategoryCode, globally: bool) { self.state.set_catcode(c,cc,globally) }
    fn set_command(&mut self, name: TeXStr, cmd: Option<Command<ET>>, globally: bool) { self.state.set_command(name,cmd,globally) }
    fn set_current_font(&mut self, f: ET::FontRef, globally: bool) { self.state.set_current_font(f,globally) }
    fn set_dim_register(&mut self, i: usize, v: ET::Dim, globally: bool) { self.state.set_dim_register(i,v,globally) }
    fn set_escapechar(&mut self, c: Option<ET::Char>, globally: bool) { self.state.set_escapechar(c,globally) }
    fn set_endlinechar(&mut self, c: Option<ET::Char>, globally: bool) { self.state.set_endlinechar(c,globally) }
    fn set_int_register(&mut self, i: usize, v: ET::Int, globally: bool) { self.state.set_int_register(i,v,globally) }
    fn set_lccode(&mut self, c: ET::Char, lc: ET::Char, globally: bool) { self.state.set_lccode(c,lc,globally) }
    fn set_mathcode(&mut self, c: ET::Char, lc: ET::Int, globally: bool) { self.state.set_mathcode(c,lc,globally) }
    fn set_mode(&mut self, mode: TeXMode) { self.state.set_mode(mode) }
    fn set_muskip_register(&mut self, i: usize, v: MuSkip<ET::MuDim, ET::MuStretchShrinkDim>, globally: bool) { self.state.set_muskip_register(i,v,globally) }
    fn set_newlinechar(&mut self, c: Option<ET::Char>, globally: bool) { self.state.set_newlinechar(c,globally) }
    fn set_parshape(&mut self, v: Vec<(ET::Dim, ET::Dim)>, globally: bool) { self.state.set_parshape(v,globally) }
    fn set_primitive_dim(&mut self, name: &'static str, v: ET::Dim, globally: bool) { self.state.set_primitive_dim(name,v,globally) }
    fn set_primitive_int(&mut self, name: &'static str, v: ET::Int, globally: bool) { self.state.set_primitive_int(name,v,globally) }
    fn set_primitive_muskip(&mut self, name: &'static str, v: MuSkip<ET::MuDim, ET::MuStretchShrinkDim>, globally: bool) { self.state.set_primitive_muskip(name,v,globally) }
    fn set_primitive_skip(&mut self, name: &'static str, v: Skip<ET::SkipDim>, globally: bool) { self.state.set_primitive_skip(name,v,globally) }
    fn set_primitive_toks(&mut self, name: &'static str, v: Vec<ET::Token>, globally: bool, memory: &mut Memory<ET>) { self.state.set_primitive_toks(name,v,globally,memory) }
    fn set_scriptfont(&mut self, i: usize, f: ET::FontRef, globally: bool) { self.state.set_scriptfont(i,f,globally) }
    fn set_scriptscriptfont(&mut self, i: usize, f: ET::FontRef, globally: bool) { self.state.set_scriptscriptfont(i,f,globally) }
    fn set_skip_register(&mut self, i: usize, v: Skip<ET::SkipDim>, globally: bool) { self.state.set_skip_register(i,v,globally) }
    fn set_sfcode(&mut self, c: ET::Char, v: ET::Int, globally: bool) { self.state.set_sfcode(c,v,globally) }
    fn set_textfont(&mut self, i: usize, f: ET::FontRef, globally: bool) { self.state.set_textfont(i,f,globally) }
    fn set_toks_register(&mut self, i: usize, v: Vec<ET::Token>, globally: bool, memory: &mut Memory<ET>) { self.state.set_toks_register(i,v,globally,memory) }
    fn set_uccode(&mut self, c: ET::Char, uc: ET::Char, globally: bool) { self.state.set_uccode(c,uc,globally) }
    fn stack_pop(&mut self, memory: &mut Memory<ET>) -> Option<(Vec<ET::Token>, GroupType)> { self.state.stack_pop(memory) }
    fn stack_push(&mut self, g: GroupType) { self.state.stack_push(g) }
    fn take_afterassignment(&mut self) -> Option<ET::Token> { self.state.take_afterassignment() }
    fn take_box_register(&mut self, i: usize) -> HVBox<ET> { self.state.take_box_register(i) }
}