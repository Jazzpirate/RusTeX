/*! A TeX state */

use string_interner::Symbol;
use crate::engine::filesystem::File;
use crate::tex::catcodes::{CategoryCode, CategoryCodeScheme};
use crate::engine::state::fields::{CharField, SingleValueField, VecField, KeyValueField, StateField, HashMapField, BoxField, TokField, TokMapField};
use crate::engine::state::modes::{BoxMode, GroupType, TeXMode};
use crate::tex::commands::{BaseCommand, Command, CommandSource};
use crate::tex::token::{BaseToken, Token};
use crate::utils::strings::TeXStr;
use crate::engine::{EngineRef, EngineType};
use crate::engine::memory::{Interner, Memory};
use crate::tex::commands::pdftex::{PDFColorstack, PDFObj, PDFXForm};
use crate::tex::nodes::HVBox;
use crate::tex::fonts::FontStore;
use crate::tex::numbers::{Int, MuSkip, Skip};
use crate::throw;
use crate::utils::errors::TeXError;

pub mod fields;
pub mod modes;

/// A TeX state
pub trait State<ET:EngineType<State=Self>>:Clone+'static {

    fn push_csname(&mut self) -> usize;
    fn current_csname(&self) -> Option<usize>;
    fn pop_csname(&mut self);

    fn set_afterassignment(&mut self,t:Token<ET>);
    fn take_afterassignment(&mut self) -> Option<Token<ET>>;
    fn get_parshape(&self) -> Option<&Vec<(ET::Dim,ET::Dim)>>;
    fn set_parshape(&mut self,v:Vec<(ET::Dim,ET::Dim)>,globally:bool);

    /// The current [`TeXMode`]
    fn mode(&self) -> TeXMode;
    fn set_mode(&mut self, mode:TeXMode);

    fn file_openout(&mut self,i:usize,f:ET::File);
    fn file_closeout(&mut self, i: usize);
    fn file_openin(&mut self, i: usize, f: ET::File,interner:&mut Interner<ET::Char>);
    fn file_closein(&mut self, i: usize);
    fn get_open_out_file(&self,i:usize) -> Option<ET::File>;
    fn get_open_in_file(&self,i:usize) -> Option<ET::File>;

    /// push a new group onto the stack
    fn stack_push(&mut self, g: GroupType);

    fn stack_pop(&mut self,memory:&mut Memory<ET>) -> Option<(Vec<Token<ET>>,GroupType)>;
    fn grouplevel(&self) -> usize;

    /// get the current group type
    fn get_grouptype(&self) -> GroupType;


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
    fn get_command(&self, name:&TeXStr<ET::Char>) -> Option<&Command<ET>>;
    /// get the current [`BaseCommand`] for the active character `c`
    fn get_ac_command(&self, c: &ET::Char) -> Option<&Command<ET>>;
    /// set the current [`BaseCommand`] with name `name:`[`TeXStr`]
    fn set_command(&mut self, name:TeXStr<ET::Char>, cmd:Option<Command<ET>>, globally:bool);
    /// set the current [`BaseCommand`] for the active character `c`
    fn set_ac_command(&mut self, c: ET::Char, cmd:Option<Command<ET>>, globally:bool);

    /// get the current [`CategoryCodeScheme`]
    fn get_catcode_scheme(&self) -> &CategoryCodeScheme<ET::Char>;
    /// set the current [`CategoryCode`] for a character
    fn set_catcode(&mut self, c: ET::Char, cc:CategoryCode, globally:bool);


    /// get the current space factor code for the character
    fn get_sfcode(&self,c:&ET::Char) -> ET::Int;
    /// set the current space factor code for a character
    fn set_sfcode(&mut self, c: ET::Char, v:ET::Int, globally:bool);

    /// get the uppercase character for a character
    fn get_uccode(&self, c: &ET::Char) -> ET::Char;
    /// set the uppercase character for a character
    fn set_uccode(&mut self, c: ET::Char, uc: ET::Char, globally:bool);

    /// get the lowercase character for a character
    fn get_lccode(&self, c: &ET::Char) -> ET::Char;
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
    fn get_toks_register(&self,i:usize) -> Option<&Vec<Token<ET>>>;
    /// set the value of a skip register
    fn set_toks_register(&mut self,i:usize,v:Vec<Token<ET>>,globally:bool,memory:&mut Memory<ET>);

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
    fn get_primitive_toks(&self, name:&'static str) -> Option<&Vec<Token<ET>>>;
    /// set a primitive token register
    fn set_primitive_toks(&mut self, name:&'static str, v:Vec<Token<ET>>, globally:bool,memory:&mut Memory<ET>);

    /// get the current font
    fn get_current_font(&self) -> &ET::Font;
    /// set the current font
    fn set_current_font(&mut self, f:ET::Font, globally:bool);

    fn get_textfont(&self, i:usize) -> Option<&ET::Font>;
    fn set_textfont(&mut self, i:usize, f:ET::Font, globally:bool);

    fn get_scriptfont(&self, i:usize) -> Option<&ET::Font>;
    fn set_scriptfont(&mut self, i:usize, f:ET::Font, globally:bool);

    fn get_scriptscriptfont(&self, i:usize) -> Option<&ET::Font>;
    fn set_scriptscriptfont(&mut self, i:usize, f:ET::Font, globally:bool);

    fn push_aftergroup(&mut self, t:Token<ET>);
}

pub trait PDFState<ET:EngineType<State=Self>>:State<ET> {
    fn pdfmatches(&mut self) -> &mut Vec<String>;
    fn pdfobjs(&mut self) -> &mut Vec<PDFObj>;
    fn pdfxforms(&mut self) -> &mut Vec<PDFXForm<ET>>;
    fn pdfcolorstacks(&mut self) -> &mut Vec<PDFColorstack>;
    fn set_current_colorstack(&mut self,index:usize);
    fn get_colorstack(&mut self,u:usize) -> &mut PDFColorstack;
}

#[derive(Clone)]
pub struct PDFTeXState<ET:EngineType<State=Self>> {
    out_files:Vec<Option<ET::File>>,
    in_files:Vec<Option<ET::File>>,
    csnames:usize,
    afterassignment:Option<Token<ET>>,

    pdfmatches:Vec<String>,
    pdfobjs:Vec<PDFObj>,
    pdfxforms:Vec<PDFXForm<ET>>,
    pdfcolorstacks:Vec<PDFColorstack>,
    current_colorstack:usize,

    current_font:SingleValueField<ET::Font>,
    parshape:SingleValueField<Option<Vec<(ET::Dim,ET::Dim)>>>,

    mode: TeXMode,
    /* filesystem: FS,*/
    grouptype: Vec<(GroupType,Option<TeXMode>)>,
    aftergroups:Vec<Vec<Token<ET>>>,
    endlinechar: SingleValueField<Option<ET::Char>>,
    escapechar: SingleValueField<Option<ET::Char>>,
    newlinechar: SingleValueField<Option<ET::Char>>,

    pub commands: VecField<Option<Command<ET>>>,
    ac_commands: CharField<ET::Char,Option<Command<ET>>>,

    catcodes: CharField<ET::Char,CategoryCode>,
    sfcodes: CharField<ET::Char,ET::Int>,
    ucchar: CharField<ET::Char, ET::Char>,
    lcchar: CharField<ET::Char, ET::Char>,
    mathcodes: CharField<ET::Char,ET::Int>,
    delcodes: CharField<ET::Char,ET::Int>,

    intregisters: VecField<ET::Int>,
    dimregisters: VecField<ET::Dim>,
    skipregisters: VecField<Skip<ET::SkipDim>>,
    muskipregisters: VecField<MuSkip<ET::MuDim,ET::MuStretchShrinkDim>>,
    toksregisters:TokField<ET>,
    boxregisters:BoxField<ET>,

    textfonts:VecField<Option<ET::Font>>,
    scriptfonts:VecField<Option<ET::Font>>,
    scriptscriptfonts:VecField<Option<ET::Font>>,

    primitive_intregisters: HashMapField<&'static str,ET::Int>,
    primitive_dimregisters: HashMapField<&'static str,ET::Dim>,
    primitive_skipregisters: HashMapField<&'static str,Skip<ET::SkipDim>>,
    primitive_muskipregisters: HashMapField<&'static str,MuSkip<ET::MuDim,ET::MuStretchShrinkDim>>,
    primitive_tokregisters: TokMapField<ET>,
}
use crate::utils::strings::CharType;
impl<ET:EngineType<State=Self>> PDFTeXState<ET> {
    pub fn new(fontstore:&ET::FontStore) -> Self {
        let mut state = Self {
            out_files:vec!(),
            in_files:vec!(),
            csnames:0,
            current_font:SingleValueField::new(fontstore.null()),
            afterassignment:None,
            aftergroups:vec!(vec!()),
            parshape:SingleValueField::new(None),
            mode: TeXMode::Vertical,
            pdfmatches:vec!(),
            pdfobjs:vec!(),
            pdfxforms:vec!(),
            pdfcolorstacks:vec!(PDFColorstack(vec!())),
            current_colorstack:0,
            /* filesystem: fs,*/
            grouptype: vec![(GroupType::Top,None)],
            endlinechar: SingleValueField::new(Some(ET::Char::carriage_return())),
            escapechar: SingleValueField::new(Some(ET::Char::backslash())),
            newlinechar: SingleValueField::new(Some(ET::Char::newline())),
            commands: VecField::new(),
            ac_commands: CharField::new(ET::Char::rep_field(None)),
            catcodes: CharField::new(ET::Char::starting_catcode_scheme()),
            sfcodes: CharField::new(ET::Char::rep_field(ET::Int::default())),
            ucchar: CharField::new(ET::Char::ident()),
            lcchar: CharField::new(ET::Char::ident()),
            mathcodes: CharField::new(ET::Char::rep_field(ET::Int::default())),
            delcodes: CharField::new(ET::Char::rep_field(ET::Int::default())),
            intregisters: VecField::new(),
            dimregisters: VecField::new(),
            skipregisters: VecField::new(),
            muskipregisters: VecField::new(),
            toksregisters: TokField::new(),
            boxregisters: BoxField::new(),

            textfonts:VecField::new(),
            scriptfonts:VecField::new(),
            scriptscriptfonts:VecField::new(),

            primitive_intregisters: HashMapField::new(),
            primitive_dimregisters: HashMapField::new(),
            primitive_skipregisters: HashMapField::new(),
            primitive_muskipregisters: HashMapField::new(),
            primitive_tokregisters: TokMapField::new(),
        };
        for i in 97..123 {
            state.ucchar.set_locally((i as u8).into(),((i-32) as u8).into());
            state.lcchar.set_locally(((i-32) as u8).into(),(i as u8).into());
            state.mathcodes.set_locally(ET::Char::from(i-32),
                                        ET::Int::from_i64((i as i64-32) +
                                            (1 * 16 * 16) +
                                            (7 * 16 * 16 * 16))
            );
            state.mathcodes.set_locally(ET::Char::from(i),
                                        ET::Int::from_i64((i as i64) +
                                            (1 * 16 * 16) +
                                            (7 * 16 * 16 * 16))
            );
        }
        for i in 48..58 {
            state.mathcodes.set_locally(ET::Char::from(i),
                                        ET::Int::from_i64((i as i64) +
                                            (0 * 16 * 16) +
                                            (7 * 16 * 16 * 16))
            );
        }
        state
    }
}

impl<ET:EngineType<State=Self>> PDFState<ET> for PDFTeXState<ET> {
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
        self.current_colorstack = index
    }
}

impl<ET:EngineType<State=Self>> State<ET> for PDFTeXState<ET> {
    fn get_current_font(&self) -> &ET::Font {
        self.current_font.get()
    }
    fn set_current_font(&mut self, f:ET::Font, globally: bool) {
        let globaldefs = self.get_primitive_int("globaldefs").to_i64();
        let globally = if globaldefs == 0 {globally} else {globaldefs > 0};
        if globally {
            self.current_font.set_globally(f)
        } else {
            self.current_font.set_locally(f)
        }
    }
    fn grouplevel(&self) -> usize {
        self.grouptype.len()
    }

    fn set_afterassignment(&mut self, t: Token<ET>) {
        self.afterassignment = Some(t)
    }
    fn take_afterassignment(&mut self) -> Option<Token<ET>> {
        self.afterassignment.take()
    }

    fn mode(&self) -> TeXMode { self.mode }
    fn push_csname(&mut self) -> usize {
        self.csnames += 1;
        self.csnames
    }
    fn current_csname(&self) -> Option<usize> {
        match self.csnames {
            0 => None,
            _ => Some(self.csnames)
        }
    }
    fn pop_csname(&mut self) {
        self.csnames -= 1;
    }
    fn file_openin(&mut self, i: usize, f: ET::File,interner:&mut Interner<ET::Char>) {
        if i >= self.in_files.len() {
            self.in_files.resize(i+1,None);
        }
        f.open_in(interner);
        self.in_files[i] = Some(f);
    }
    fn file_closein(&mut self, i: usize) {
        if i >= self.in_files.len() {
            todo!("Error closeout")
            //self.out_files.resize(i,None);
        }
        if let Some(f) = &self.in_files[i] {
            f.close_in();
        }
        self.in_files[i] = None
    }
    fn file_openout(&mut self, i: usize, f: ET::File) {
        f.open_out();
        if i >= self.out_files.len() {
            self.out_files.resize(i+1,None);
        }
        self.out_files[i] = Some(f);
    }
    fn file_closeout(&mut self, i: usize) {
        if i >= self.out_files.len() {
            todo!("Error closeout")
            //self.out_files.resize(i,None);
        }
        if let Some(f) = &self.out_files[i] {
            f.close_out();
        }
        self.out_files[i] = None
    }
    fn get_open_out_file(&self,i:usize) -> Option<ET::File> {
        if i >= self.out_files.len() {
            None
        } else {
            self.out_files[i].clone()
        }
    }
    fn get_open_in_file(&self,i:usize) -> Option<ET::File> {
        if i >= self.in_files.len() {
            None
        } else {
            self.in_files[i].clone()
        }
    }
    // #[inline(always)]
    fn stack_push(&mut self, g: GroupType) {
        match g {
            GroupType::Box(m) => {
                self.grouptype.push((g,Some(self.mode)));
                self.mode = match m {
                    BoxMode::H | BoxMode::LeftRight => TeXMode::RestrictedHorizontal,
                    BoxMode::M => TeXMode::Math,
                    BoxMode::DM => TeXMode::Displaymath,
                    BoxMode::V => TeXMode::InternalVertical,
                    _ => self.mode
                };
            },
            _ => self.grouptype.push((g,None))
        }
        self.current_font.push_stack();
        self.parshape.push_stack();
        self.endlinechar.push_stack();
        self.escapechar.push_stack();
        self.newlinechar.push_stack();

        self.commands.push_stack();
        self.ac_commands.push_stack();

        self.catcodes.push_stack();
        self.sfcodes.push_stack();
        self.ucchar.push_stack();
        self.lcchar.push_stack();
        self.mathcodes.push_stack();
        self.delcodes.push_stack();

        self.intregisters.push_stack();
        self.dimregisters.push_stack();
        self.skipregisters.push_stack();
        self.muskipregisters.push_stack();
        self.toksregisters.push_stack();
        self.boxregisters.push_stack();

        self.textfonts.push_stack();
        self.scriptfonts.push_stack();
        self.scriptscriptfonts.push_stack();

        self.primitive_intregisters.push_stack();
        self.primitive_dimregisters.push_stack();
        self.primitive_skipregisters.push_stack();
        self.primitive_muskipregisters.push_stack();
        self.primitive_tokregisters.push_stack();
        self.aftergroups.push(vec!());
    }
    fn stack_pop(&mut self,memory:&mut Memory<ET>) -> Option<(Vec<Token<ET>>,GroupType)> {
        let gt = match self.grouptype.pop() {
            None => return None,
            Some((gt,Some(m))) => {
                self.mode = m;
                gt
            }
            Some((gt,_)) => gt
        };
        self.current_font.pop_stack();
        self.parshape.pop_stack();
        self.endlinechar.pop_stack();
        self.escapechar.pop_stack();
        self.newlinechar.pop_stack();

        self.commands.pop_stack();
        self.ac_commands.pop_stack();

        self.catcodes.pop_stack();
        self.sfcodes.pop_stack();
        self.ucchar.pop_stack();
        self.lcchar.pop_stack();
        self.mathcodes.pop_stack();
        self.delcodes.pop_stack();

        self.intregisters.pop_stack();
        self.dimregisters.pop_stack();
        self.skipregisters.pop_stack();
        self.muskipregisters.pop_stack();
        self.toksregisters.pop_stack(memory);
        self.boxregisters.pop_stack();

        self.textfonts.pop_stack();
        self.scriptfonts.pop_stack();
        self.scriptscriptfonts.pop_stack();

        self.primitive_intregisters.pop_stack();
        self.primitive_dimregisters.pop_stack();
        self.primitive_skipregisters.pop_stack();
        self.primitive_muskipregisters.pop_stack();
        self.primitive_tokregisters.pop_stack(memory);

        Some((self.aftergroups.pop().unwrap_or(vec!()),gt))
    }

    fn set_mode(&mut self, mode: TeXMode) {
        self.mode = mode
    }

    // #[inline(always)]
    fn get_grouptype(&self) -> GroupType { *self.grouptype.last().map(|(t,_)| t).unwrap_or(&GroupType::Top) }

    fn get_parshape(&self) -> Option<&Vec<(ET::Dim, ET::Dim)>> {
        self.parshape.get().as_ref()
    }
    fn set_parshape(&mut self, v: Vec<(ET::Dim, ET::Dim)>, globally: bool) {
        let globaldefs = self.get_primitive_int("globaldefs").to_i64();
        let globally = if globaldefs == 0 {globally} else {globaldefs > 0};
        if globally {
            self.parshape.set_globally(if v.is_empty() {None} else {Some(v)})
        } else {
            self.parshape.set_locally(if v.is_empty() {None} else {Some(v)})
        }
    }

    // #[inline(always)]
    fn get_escapechar(&self) -> Option<ET::Char> { *self.escapechar.get() }
    // #[inline(always)]
    fn set_escapechar(&mut self, c: Option<ET::Char>, globally: bool) {
        let globaldefs = self.get_primitive_int("globaldefs").to_i64();
        let globally = if globaldefs == 0 {globally} else {globaldefs > 0};
        if globally {
            self.escapechar.set_globally(c)
        } else {
            self.escapechar.set_locally(c)
        }
    }
    fn push_aftergroup(&mut self, t: Token<ET>) {
        self.aftergroups.last_mut().unwrap().push(t)
    }

    // #[inline(always)]
    fn get_endlinechar(&self) -> Option<ET::Char> { *self.endlinechar.get() }
    // #[inline(always)]
    fn set_endlinechar(&mut self, c: Option<ET::Char>, globally: bool) {
        let globaldefs = self.get_primitive_int("globaldefs").to_i64();
        let globally = if globaldefs == 0 {globally} else {globaldefs > 0};
        if globally {
            self.endlinechar.set_globally(c)
        } else {
            self.endlinechar.set_locally(c)
        }
    }

    // #[inline(always)]
    fn get_newlinechar(&self) -> Option<ET::Char> { *self.newlinechar.get() }
    // #[inline(always)]
    fn set_newlinechar(&mut self, c: Option<ET::Char>, globally: bool) {
        let globaldefs = self.get_primitive_int("globaldefs").to_i64();
        let globally = if globaldefs == 0 {globally} else {globaldefs > 0};
        if globally {
            self.newlinechar.set_globally(c)
        } else {
            self.newlinechar.set_locally(c)
        }
    }

    fn get_sfcode(&self, c: &ET::Char) -> ET::Int {
        *self.sfcodes.get(c).unwrap_or(&ET::Int::default())
    }
    fn set_sfcode(&mut self, c: ET::Char, v: ET::Int, globally: bool) {
        let globaldefs = self.get_primitive_int("globaldefs").to_i64();
        let globally = if globaldefs == 0 {globally} else {globaldefs > 0};
        if globally {
            self.sfcodes.set_globally(c,v)
        } else {
            self.sfcodes.set_locally(c,v)
        }
    }

    fn get_mathcode(&self, c: ET::Char) -> ET::Int {
        *self.mathcodes.get(&c).unwrap_or(&ET::Int::default())
    }
    fn set_mathcode(&mut self, c: ET::Char, lc: ET::Int, globally: bool) {
        let globaldefs = self.get_primitive_int("globaldefs").to_i64();
        let globally = if globaldefs == 0 {globally} else {globaldefs > 0};
        if globally {
            self.mathcodes.set_globally(c,lc)
        } else {
            self.mathcodes.set_locally(c,lc)
        }
    }

    fn get_delcode(&self, c: ET::Char) -> ET::Int {
        *self.delcodes.get(&c).unwrap_or(&ET::Int::default())
    }
    fn set_delcode(&mut self, c: ET::Char, lc: ET::Int, globally: bool) {
        let globaldefs = self.get_primitive_int("globaldefs").to_i64();
        let globally = if globaldefs == 0 {globally} else {globaldefs > 0};
        if globally {
            self.delcodes.set_globally(c,lc)
        } else {
            self.delcodes.set_locally(c,lc)
        }
    }

    // #[inline(always)]
    fn get_command(&self, name: &TeXStr<ET::Char>) -> Option<&Command<ET>> {
        match self.commands.get(&name.0.to_usize()) {
            Some(r) => r.as_ref(),
            _ => None
        }//.as_ref().map(|c| *c)
    }
    // #[inline(always)]
    fn set_command(&mut self, name: TeXStr<ET::Char>, cmd: Option<Command<ET>>, globally: bool) {
        let globaldefs = self.get_primitive_int("globaldefs").to_i64();
        let globally = if globaldefs == 0 {globally} else {globaldefs > 0};
        if globally {
            self.commands.set_globally(name.0.to_usize(),cmd)
        } else {
            self.commands.set_locally(name.0.to_usize(),cmd)
        }
    }
    fn get_ac_command(&self, c: &ET::Char) -> Option<&Command<ET>> {
        match self.ac_commands.get(&c) {
            Some(r) => r.as_ref(),
            _ => None
        }
    }
    fn set_ac_command(&mut self, c: ET::Char, cmd: Option<Command<ET>>, globally: bool) {
        let globaldefs = self.get_primitive_int("globaldefs").to_i64();
        let globally = if globaldefs == 0 {globally} else {globaldefs > 0};
        if globally {
            self.ac_commands.set_globally(c,cmd)
        } else {
            self.ac_commands.set_locally(c,cmd)
        }
    }


    // #[inline(always)]
    fn get_catcode_scheme(&self) -> &CategoryCodeScheme<ET::Char> {
        &self.catcodes.charfield
    }
    // #[inline(always)]
    fn set_catcode(&mut self, c: ET::Char, cc: CategoryCode, globally: bool) {
        let globaldefs = self.get_primitive_int("globaldefs").to_i64();
        let globally = if globaldefs == 0 {globally} else {globaldefs > 0};
        if globally {
            self.catcodes.set_globally(c,cc)
        } else {
            self.catcodes.set_locally(c,cc)
        }
    }

    // #[inline(always)]
    fn get_uccode(&self, c: &ET::Char) -> ET::Char {
        *self.ucchar.get(c).unwrap_or(&ET::Char::default())
    }
    // #[inline(always)]
    fn set_uccode(&mut self, c: ET::Char, uc: ET::Char, globally: bool) {
        let globaldefs = self.get_primitive_int("globaldefs").to_i64();
        let globally = if globaldefs == 0 {globally} else {globaldefs > 0};
        if globally {
            self.ucchar.set_globally(c,uc)
        } else {
            self.ucchar.set_locally(c,uc)
        }
    }

    // #[inline(always)]
    fn get_lccode(&self, c: &ET::Char) -> ET::Char {
        *self.lcchar.get(c).unwrap_or(&ET::Char::default())
    }
    // #[inline(always)]
    fn set_lccode(&mut self, c: ET::Char, lc: ET::Char, globally: bool) {
        let globaldefs = self.get_primitive_int("globaldefs").to_i64();
        let globally = if globaldefs == 0 {globally} else {globaldefs > 0};
        if globally {
            self.lcchar.set_globally(c,lc)
        } else {
            self.lcchar.set_locally(c,lc)
        }
    }

    // #[inline(always)]
    fn get_int_register(&self, i: usize) -> ET::Int {
        *self.intregisters.get(&i).unwrap_or(&ET::Int::default())
    }
    // #[inline(always)]
    fn set_int_register(&mut self, i: usize, v: ET::Int, globally: bool) {
        let globaldefs = self.get_primitive_int("globaldefs").to_i64();
        let globally = if globaldefs == 0 {globally} else {globaldefs > 0};
        if globally {
            self.intregisters.set_globally(i,v)
        } else {
            self.intregisters.set_locally(i,v)
        }
    }

    fn get_dim_register(&self, i: usize) -> ET::Dim { *self.dimregisters.get(&i).unwrap_or(&ET::Dim::default()) }
    fn set_dim_register(&mut self, i: usize, v: ET::Dim, globally: bool) {
        let globaldefs = self.get_primitive_int("globaldefs").to_i64();
        let globally = if globaldefs == 0 {globally} else {globaldefs > 0};
        if globally {
            self.dimregisters.set_globally(i,v)
        } else {
            self.dimregisters.set_locally(i,v)
        }
    }

    fn get_skip_register(&self, i: usize) -> Skip<ET::SkipDim> { *self.skipregisters.get(&i).unwrap_or(&Skip::default()) }
    fn set_skip_register(&mut self, i: usize, v: Skip<ET::SkipDim>, globally: bool) {
        let globaldefs = self.get_primitive_int("globaldefs").to_i64();
        let globally = if globaldefs == 0 {globally} else {globaldefs > 0};
        if globally {
            self.skipregisters.set_globally(i,v)
        } else {
            self.skipregisters.set_locally(i,v)
        }
    }

    fn get_muskip_register(&self, i: usize) -> MuSkip<ET::MuDim,ET::MuStretchShrinkDim> { *self.muskipregisters.get(&i).unwrap_or(&MuSkip::default()) }
    fn set_muskip_register(&mut self, i: usize, v: MuSkip<ET::MuDim,ET::MuStretchShrinkDim>, globally: bool) {
        let globaldefs = self.get_primitive_int("globaldefs").to_i64();
        let globally = if globaldefs == 0 {globally} else {globaldefs > 0};
        if globally {
            self.muskipregisters.set_globally(i,v)
        } else {
            self.muskipregisters.set_locally(i,v)
        }
    }

    fn get_toks_register(&self, i: usize) -> Option<&Vec<Token<ET>>> { self.toksregisters.get(i) }
    fn set_toks_register(&mut self, i: usize, v: Vec<Token<ET>>, globally: bool,memory:&mut Memory<ET>) {
        let globaldefs = self.get_primitive_int("globaldefs").to_i64();
        let globally = if globaldefs == 0 {globally} else {globaldefs > 0};
        if globally {
            self.toksregisters.set_globally(i,v,memory)
        } else {
            self.toksregisters.set_locally(i,v,memory)
        }
    }

    fn get_box_register(&mut self, i: usize) -> Option<&mut HVBox<ET>> {
        self.boxregisters.get_mut(i)
    }
    fn take_box_register(&mut self, i: usize) -> HVBox<ET> {
        self.boxregisters.take(i)
    }
    fn set_box_register(&mut self, i: usize, v: HVBox<ET>, globally: bool) {
        let globaldefs = self.get_primitive_int("globaldefs").to_i64();
        let globally = if globaldefs == 0 {globally} else {globaldefs > 0};
        if globally {
            self.boxregisters.set_globally(i,v)
        } else {
            self.boxregisters.set_locally(i,v)
        }
    }

    fn get_primitive_int(&self, name: &'static str) -> ET::Int {
        *self.primitive_intregisters.get(&name).unwrap_or(&ET::Int::default())
    }
    fn set_primitive_int(&mut self, name: &'static str, v: ET::Int, globally: bool) {
        let globaldefs = self.get_primitive_int("globaldefs").to_i64();
        let globally = if globaldefs == 0 {globally} else {globaldefs > 0};
        if globally {
            self.primitive_intregisters.set_globally(name,v)
        } else {
            self.primitive_intregisters.set_locally(name,v)
        }
    }

    fn get_primitive_dim(&self, name: &'static str) -> ET::Dim {
        *self.primitive_dimregisters.get(&name).unwrap_or(&ET::Dim::default())
    }
    fn set_primitive_dim(&mut self, name: &'static str, v: ET::Dim, globally: bool) {
        let globaldefs = self.get_primitive_int("globaldefs").to_i64();
        let globally = if globaldefs == 0 {globally} else {globaldefs > 0};
        if globally {
            self.primitive_dimregisters.set_globally(name,v)
        } else {
            self.primitive_dimregisters.set_locally(name,v)
        }
    }

    fn get_primitive_skip(&self, name: &'static str) -> Skip<ET::SkipDim> {
        *self.primitive_skipregisters.get(&name).unwrap_or(&Skip::default())
    }
    fn set_primitive_skip(&mut self, name: &'static str, v: Skip<ET::SkipDim>, globally: bool) {
        let globaldefs = self.get_primitive_int("globaldefs").to_i64();
        let globally = if globaldefs == 0 {globally} else {globaldefs > 0};
        if globally {
            self.primitive_skipregisters.set_globally(name,v)
        } else {
            self.primitive_skipregisters.set_locally(name,v)
        }
    }

    fn get_primitive_muskip(&self, name: &'static str) -> MuSkip<ET::MuDim,ET::MuStretchShrinkDim> {
        *self.primitive_muskipregisters.get(&name).unwrap_or(&MuSkip::default())
    }
    fn set_primitive_muskip(&mut self, name: &'static str, v: MuSkip<ET::MuDim,ET::MuStretchShrinkDim>, globally: bool) {
        let globaldefs = self.get_primitive_int("globaldefs").to_i64();
        let globally = if globaldefs == 0 {globally} else {globaldefs > 0};
        if globally {
            self.primitive_muskipregisters.set_globally(name,v)
        } else {
            self.primitive_muskipregisters.set_locally(name,v)
        }
    }

    fn get_primitive_toks(&self, name: &'static str) -> Option<&Vec<Token<ET>>> { self.primitive_tokregisters.get(&name) }
    fn set_primitive_toks(&mut self, name: &'static str, v: Vec<Token<ET>>, globally: bool,memory:&mut Memory<ET>) {
        let globaldefs = self.get_primitive_int("globaldefs").to_i64();
        let globally = if globaldefs == 0 {globally} else {globaldefs > 0};
        if globally {
            self.primitive_tokregisters.set_globally(name,v,memory)
        } else {
            self.primitive_tokregisters.set_locally(name,v,memory)
        }
    }

    fn get_textfont(&self, i: usize) -> Option<&ET::Font> {
        match self.textfonts.get(&i) {
            None => None,
            Some(o) => o.as_ref()
        }
    }
    fn get_scriptfont(&self, i: usize) -> Option<&ET::Font> {
        match self.scriptfonts.get(&i) {
            None => None,
            Some(o) => o.as_ref()
        }
    }
    fn get_scriptscriptfont(&self, i: usize) -> Option<&ET::Font> {
        match self.scriptscriptfonts.get(&i) {
            None => None,
            Some(o) => o.as_ref()
        }
    }
    fn set_textfont(&mut self, i: usize, f: ET::Font, globally: bool) {
        let globaldefs = self.get_primitive_int("globaldefs").to_i64();
        let globally = if globaldefs == 0 {globally} else {
            globaldefs > 0
        };
        if globally {
            self.textfonts.set_globally(i,Some(f))
        } else {
            self.textfonts.set_locally(i,Some(f))
        }
    }
    fn set_scriptfont(&mut self, i: usize, f: ET::Font, globally: bool) {
        let globaldefs = self.get_primitive_int("globaldefs").to_i64();
        let globally = if globaldefs == 0 {
            globally
        } else {
            globaldefs > 0
        };
        if globally {
            self.scriptfonts.set_globally(i,Some(f))
        } else {
            self.scriptfonts.set_locally(i,Some(f))
        }
    }
    fn set_scriptscriptfont(&mut self, i: usize, f: ET::Font, globally: bool) {
        let globaldefs = self.get_primitive_int("globaldefs").to_i64();
        let globally = if globaldefs == 0 {
            globally
        } else {
            globaldefs > 0
        };
        if globally {
            self.scriptscriptfonts.set_globally(i,Some(f))
        } else {
            self.scriptscriptfonts.set_locally(i,Some(f))
        }
    }
}

impl<ET:EngineType> EngineRef<ET> {
    pub fn set_command_for_tk(&mut self, tk:Token<ET>, cmd:Option<Command<ET>>, globally:bool) {
        match tk.base {
            BaseToken::CS(cs) => self.state.set_command(cs,cmd, globally),
            BaseToken::Char(c,_) => self.state.set_ac_command(c,cmd, globally),
        }
    }

    pub fn set_relax(&mut self,tk:&Token<ET>,source:&CommandSource<ET>,globally:bool) -> Result<(),TeXError<ET>> {
        match &tk.base {
            BaseToken::Char(c,CategoryCode::Active) => {
                self.state.set_ac_command(*c, Some(Command::new(BaseCommand::Relax,Some(source))), globally)
            }
            BaseToken::CS(name) => {
                self.state.set_command(name.clone(), Some(Command::new(BaseCommand::Relax,Some(source))), globally)
            }
            _ => throw!("Command name expected, got {:?}",tk => source.cause)
        }
        Ok(())
    }
}