/*! A TeX state */

use crate::engine::filesystem::{File, FileSystem};
use crate::tex::catcodes::{CategoryCode, CategoryCodeScheme};
use crate::engine::state::fields::{CharField, SingleValueField, VecField, KeyValueField, StateField, HashMapField, BoxField};
use crate::engine::state::modes::{GroupType, TeXMode};
use crate::tex::commands::Command;
use crate::tex::token::Token;
use crate::utils::errors::{OtherError, TeXError, UndefinedActiveCharacter, UndefinedControlSequence};
use crate::utils::strings::TeXStr;
use chrono::{DateTime,Local};
use crate::engine::{EngineType, Outputs};
use crate::engine::stomach::Stomach;
use crate::tex::boxes::{HVBox, OpenBox, TeXNode};
use crate::tex::fonts::FontStore;
use crate::tex::numbers::{Int, MuSkip, NumSet, Skip};
use crate::utils::Ptr;

pub mod fields;
pub mod modes;

/// A TeX state
pub trait State<ET:EngineType<State=Self>>:Sized+'static {
    /// Should be called at the start of a new job with the jobname (name of the main file)
    fn set_job(&mut self, jobname:String);

    fn get_jobname(&self) -> &str;
    fn get_start_time(&self) -> DateTime<Local>;

    fn outputs(&self) -> &Outputs;

    fn push_csname(&mut self) -> usize;
    fn current_csname(&self) -> Option<usize>;
    fn pop_csname(&mut self);


    fn box_stack(&self) -> &Vec<OpenBox<ET>>;
    fn box_stack_mut(&mut self) -> &mut Vec<OpenBox<ET>>;

    fn set_afterassignment(&mut self,t:ET::Token);
    fn take_afterassignment(&mut self) -> Option<ET::Token>;

    /// The current [`TeXMode`]
    fn mode(&self) -> TeXMode;

    /// get the [`FileSystem`] used by this state
    fn filesystem(&mut self) -> &mut ET::FileSystem;
    fn file_openout(&mut self,i:usize,f:ET::File);
    fn file_closeout(&mut self, i: usize);
    fn file_openin(&mut self,i:usize,f:ET::File);
    fn file_closein(&mut self, i: usize);
    fn get_open_out_file(&self,i:usize) -> Option<ET::File>;
    fn get_open_in_file(&self,i:usize) -> Option<ET::File>;

    fn fontstore(&self) -> &ET::FontStore;
    fn fontstore_mut(&mut self) -> &mut ET::FontStore;

    /// push a new group onto the stack
    fn stack_push(&mut self, g: GroupType);

    fn stack_pop(&mut self) -> Option<(Vec<ET::Token>,GroupType)>;

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

    /// get the current [`Command`] with name `name:`[`TeXStr`]
    fn get_command(&self, name:&TeXStr<ET::Char>) -> Option<Ptr<Command<ET::Token>>>;
    /// get the current [`Command`] for the active character `c`
    fn get_ac_command(&self, c: ET::Char) -> Option<Ptr<Command<ET::Token>>>;
    /// set the current [`Command`] with name `name:`[`TeXStr`]
    fn set_command(&mut self, name:TeXStr<ET::Char>, cmd:Option<Ptr<Command<ET::Token>>>, globally:bool);
    /// set the current [`Command`] for the active character `c`
    fn set_ac_command(&mut self, c: ET::Char, cmd:Option<Ptr<Command<ET::Token>>>, globally:bool);
    /// get the current [`Command`] with name `name:`[`TeXStr`], or return an error if it is not defined
    fn need_command(&self, name:&TeXStr<ET::Char>) -> Result<Ptr<Command<ET::Token>>,UndefinedControlSequence<ET::Token>> {
        self.get_command(name).ok_or(UndefinedControlSequence(name.clone()))
    }
    /// get the current [`Command`] for the active character `c`, or return an error if it is not defined
    fn need_ac_command(&self, c: ET::Char) -> Result<Ptr<Command<ET::Token>>,UndefinedActiveCharacter<ET::Token>> {
        self.get_ac_command(c).ok_or(UndefinedActiveCharacter(c))
    }

    /// get the current [`CategoryCodeScheme`]
    fn get_catcode_scheme(&self) -> &CategoryCodeScheme<ET::Char>;
    /// set the current [`CategoryCode`] for a character
    fn set_catcode(&mut self, c: ET::Char, cc:CategoryCode, globally:bool);


    /// get the current space factor code for the character
    fn get_sfcode(&self,c:&ET::Char) -> ET::Int;
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
    fn get_toks_register(&self,i:usize) -> Vec<ET::Token>;
    /// set the value of a skip register
    fn set_toks_register(&mut self,i:usize,v:Vec<ET::Token>,globally:bool);

    fn get_box_register(&self,i:usize) -> &HVBox<ET>;
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
    fn get_primitive_toks(&self, name:&'static str) -> Vec<ET::Token>;
    /// set a primitive token register
    fn set_primitive_toks(&mut self, name:&'static str, v:Vec<ET::Token>, globally:bool);

    /// get the current font
    fn get_current_font(&self) -> usize;
    /// set the current font
    fn set_current_font(&mut self, index:usize, globally:bool);
}

pub struct TeXState<ET:EngineType<State=Self>> {
    filesystem:ET::FileSystem,
    out_files:Vec<Option<ET::File>>,
    in_files:Vec<Option<ET::File>>,
    csnames:usize,
    fontstore:ET::FontStore,
    box_stack:Vec<OpenBox<ET>>,
    afterassignment:Option<ET::Token>,

    current_font:SingleValueField<usize>,

    outputs:Outputs,

    jobname: Option<String>,
    start_time: Option<DateTime<Local>>,

    mode: TeXMode,
    /* filesystem: FS,*/
    grouptype: Vec<(GroupType,Option<TeXMode>)>,
    aftergroups:Vec<Vec<ET::Token>>,
    endlinechar: SingleValueField<Option<ET::Char>>,
    escapechar: SingleValueField<Option<ET::Char>>,
    newlinechar: SingleValueField<Option<ET::Char>>,

    commands: HashMapField<TeXStr<ET::Char>,Option<Ptr<Command<ET::Token>>>>,
    ac_commands: CharField<ET::Char,Option<Ptr<Command<ET::Token>>>>,

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
    toksregisters:VecField<Vec<ET::Token>>,
    boxregisters:BoxField<ET>,

    primitive_intregisters: HashMapField<&'static str,ET::Int>,
    primitive_dimregisters: HashMapField<&'static str,ET::Dim>,
    primitive_skipregisters: HashMapField<&'static str,Skip<ET::SkipDim>>,
    primitive_muskipregisters: HashMapField<&'static str,MuSkip<ET::MuDim,ET::MuStretchShrinkDim>>,
    primitive_tokregisters: HashMapField<&'static str,Vec<ET::Token>>,
}
use crate::utils::strings::CharType;
impl<ET:EngineType<State=Self>> TeXState<ET> {
    pub fn new(fs:ET::FileSystem,fontstore:ET::FontStore,outputs:Outputs) -> Self {
        let mut state = Self {
            filesystem:fs,
            out_files:vec!(),
            in_files:vec!(),
            csnames:0,
            fontstore,
            outputs,
            afterassignment:None,
            aftergroups:vec!(vec!()),
            box_stack:vec!(),
            current_font:SingleValueField::new(0),

            jobname: None,
            start_time: None,
            mode: TeXMode::Vertical,
            /* filesystem: fs,*/
            grouptype: vec![(GroupType::Top,None)],
            endlinechar: SingleValueField::new(Some(ET::Char::carriage_return())),
            escapechar: SingleValueField::new(Some(ET::Char::backslash())),
            newlinechar: SingleValueField::new(Some(ET::Char::newline())),
            commands: HashMapField::new(),
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
            toksregisters: VecField::new(),
            boxregisters: BoxField::new(),

            primitive_intregisters: HashMapField::new(),
            primitive_dimregisters: HashMapField::new(),
            primitive_skipregisters: HashMapField::new(),
            primitive_muskipregisters: HashMapField::new(),
            primitive_tokregisters: HashMapField::new(),
        };
        for i in 97..123 {
            state.ucchar.set_locally((i as u8).into(),((i-32) as u8).into());
            state.lcchar.set_locally(((i-32) as u8).into(),(i as u8).into());
            state.mathcodes.set_locally(ET::Char::from(i-32),
                                        ET::Int::from_i64::<ET::Token>((i as i64-32) +
                                            (1 * 16 * 16) +
                                            (7 * 16 * 16 * 16)).unwrap()
            );
            state.mathcodes.set_locally(ET::Char::from(i),
                                        ET::Int::from_i64::<ET::Token>((i as i64) +
                                            (1 * 16 * 16) +
                                            (7 * 16 * 16 * 16)).unwrap()
            );
        }
        for i in 48..58 {
            state.mathcodes.set_locally(ET::Char::from(i),
                                        ET::Int::from_i64::<ET::Token>((i as i64) +
                                            (0 * 16 * 16) +
                                            (7 * 16 * 16 * 16)).unwrap()
            );
        }
        state
    }
}
impl<ET:EngineType<State=Self>> State<ET> for TeXState<ET> {
    fn get_current_font(&self) -> usize {
        *self.current_font.get()
    }
    fn set_current_font(&mut self, index:usize, globally: bool) {
        let globaldefs = self.get_primitive_int("globaldefs").to_i64();
        let globally = if globaldefs == 0 {globally} else {globaldefs > 0};
        if globally {
            self.current_font.set_globally(index)
        } else {
            self.current_font.set_locally(index)
        }
    }

    fn box_stack(&self) -> &Vec<OpenBox<ET>> { &self.box_stack }
    fn box_stack_mut(&mut self) -> &mut Vec<OpenBox<ET>> { &mut self.box_stack }

    fn set_afterassignment(&mut self, t: ET::Token) {
        self.afterassignment = Some(t)
    }
    fn take_afterassignment(&mut self) -> Option<ET::Token> {
        self.afterassignment.take()
    }

    fn fontstore(&self) -> &ET::FontStore { &self.fontstore }
    fn fontstore_mut(&mut self) -> &mut ET::FontStore {
        &mut self.fontstore
    }

    fn mode(&self) -> TeXMode { self.mode }
    fn set_job(&mut self, jobname: String) {
        self.jobname = Some(jobname);
        self.start_time = Some(Local::now());
    }
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
    fn outputs(&self) -> &Outputs { &self.outputs }
    fn get_jobname(&self) -> &str {
        self.jobname.as_ref().unwrap().as_str()
    }
    fn get_start_time(&self) -> DateTime<Local> {
        self.start_time.as_ref().unwrap().clone()
    }
    fn filesystem(&mut self) -> &mut ET::FileSystem {
        &mut self.filesystem
    }
    fn file_openin(&mut self, i: usize, f: ET::File) {
        if i >= self.in_files.len() {
            self.in_files.resize(i+1,None);
        }
        f.open_in();
        self.in_files[i] = Some(f);
    }
    fn file_closein(&mut self, i: usize) {
        if i >= self.in_files.len() {
            todo!("Error closeout")
            //self.out_files.resize(i,None);
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
            GroupType::Box(_) => self.grouptype.push((g,Some(self.mode))),
            _ => self.grouptype.push((g,None))
        }
        self.current_font.push_stack();
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

        self.primitive_intregisters.push_stack();
        self.primitive_dimregisters.push_stack();
        self.primitive_skipregisters.push_stack();
        self.primitive_muskipregisters.push_stack();
        self.primitive_tokregisters.push_stack();
        unsafe{self.aftergroups.push(self.aftergroups.last().unwrap_unchecked().clone())};
    }
    fn stack_pop(&mut self) -> Option<(Vec<ET::Token>,GroupType)> {
        let gt = match self.grouptype.pop() {
            None => return None,
            Some((gt,Some(m))) => {
                self.mode = m;
                gt
            }
            Some((gt,_)) => gt
        };
        self.current_font.pop_stack();
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
        self.toksregisters.pop_stack();
        self.boxregisters.pop_stack();

        self.primitive_intregisters.pop_stack();
        self.primitive_dimregisters.pop_stack();
        self.primitive_skipregisters.pop_stack();
        self.primitive_muskipregisters.pop_stack();
        self.primitive_tokregisters.pop_stack();

        Some((self.aftergroups.pop().unwrap_or(vec!()),gt))
    }

    // #[inline(always)]
    fn get_grouptype(&self) -> GroupType { *self.grouptype.last().map(|(t,_)| t).unwrap_or(&GroupType::Top) }

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
        self.sfcodes.get(c)
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
        self.mathcodes.get(&c)
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
        self.delcodes.get(&c)
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
    fn get_command(&self, name: &TeXStr<ET::Char>) -> Option<Ptr<Command<ET::Token>>> {
        self.commands.get(name)
    }
    // #[inline(always)]
    fn set_command(&mut self, name: TeXStr<ET::Char>, cmd: Option<Ptr<Command<ET::Token>>>, globally: bool) {
        let globaldefs = self.get_primitive_int("globaldefs").to_i64();
        let globally = if globaldefs == 0 {globally} else {globaldefs > 0};
        if globally {
            self.commands.set_globally(name,cmd)
        } else {
            self.commands.set_locally(name,cmd)
        }
    }
    fn get_ac_command(&self, c: ET::Char) -> Option<Ptr<Command<ET::Token>>> {
        self.ac_commands.get(&c)
    }
    fn set_ac_command(&mut self, c: ET::Char, cmd: Option<Ptr<Command<ET::Token>>>, globally: bool) {
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
    fn get_uccode(&self, c: ET::Char) -> ET::Char {
        self.ucchar.get(&c)
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
    fn get_lccode(&self, c: ET::Char) -> ET::Char {
        self.lcchar.get(&c)
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
        self.intregisters.get(&i)
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

    fn get_dim_register(&self, i: usize) -> ET::Dim { self.dimregisters.get(&i) }
    fn set_dim_register(&mut self, i: usize, v: ET::Dim, globally: bool) {
        let globaldefs = self.get_primitive_int("globaldefs").to_i64();
        let globally = if globaldefs == 0 {globally} else {globaldefs > 0};
        if globally {
            self.dimregisters.set_globally(i,v)
        } else {
            self.dimregisters.set_locally(i,v)
        }
    }

    fn get_skip_register(&self, i: usize) -> Skip<ET::SkipDim> { self.skipregisters.get(&i) }
    fn set_skip_register(&mut self, i: usize, v: Skip<ET::SkipDim>, globally: bool) {
        let globaldefs = self.get_primitive_int("globaldefs").to_i64();
        let globally = if globaldefs == 0 {globally} else {globaldefs > 0};
        if globally {
            self.skipregisters.set_globally(i,v)
        } else {
            self.skipregisters.set_locally(i,v)
        }
    }

    fn get_muskip_register(&self, i: usize) -> MuSkip<ET::MuDim,ET::MuStretchShrinkDim> { self.muskipregisters.get(&i) }
    fn set_muskip_register(&mut self, i: usize, v: MuSkip<ET::MuDim,ET::MuStretchShrinkDim>, globally: bool) {
        let globaldefs = self.get_primitive_int("globaldefs").to_i64();
        let globally = if globaldefs == 0 {globally} else {globaldefs > 0};
        if globally {
            self.muskipregisters.set_globally(i,v)
        } else {
            self.muskipregisters.set_locally(i,v)
        }
    }

    fn get_toks_register(&self, i: usize) -> Vec<ET::Token> { self.toksregisters.get(&i) }
    fn set_toks_register(&mut self, i: usize, v: Vec<ET::Token>, globally: bool) {
        let globaldefs = self.get_primitive_int("globaldefs").to_i64();
        let globally = if globaldefs == 0 {globally} else {globaldefs > 0};
        if globally {
            self.toksregisters.set_globally(i,v)
        } else {
            self.toksregisters.set_locally(i,v)
        }
    }

    fn get_box_register(&self, i: usize) -> &HVBox<ET> {
        self.boxregisters.get(i)
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
        self.primitive_intregisters.get(&name)
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
        self.primitive_dimregisters.get(&name)
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
        self.primitive_skipregisters.get(&name)
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
        self.primitive_muskipregisters.get(&name)
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

    fn get_primitive_toks(&self, name: &'static str) -> Vec<ET::Token> {
        self.primitive_tokregisters.get(&name)
    }
    fn set_primitive_toks(&mut self, name: &'static str, v: Vec<ET::Token>, globally: bool) {
        let globaldefs = self.get_primitive_int("globaldefs").to_i64();
        let globally = if globaldefs == 0 {globally} else {globaldefs > 0};
        if globally {
            self.primitive_tokregisters.set_globally(name,v)
        } else {
            self.primitive_tokregisters.set_locally(name,v)
        }
    }

}