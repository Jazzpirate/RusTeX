/*! A TeX state */

use crate::engine::filesystem::{File, FileSystem};
use crate::tex::catcodes::{CategoryCode, CategoryCodeScheme};
use crate::engine::state::fields::{CharField, SingleValueField, VecField, KeyValueField, StateField, HashMapField};
use crate::engine::state::modes::{GroupType, TeXGroupType, TeXMode};
use crate::tex::commands::Command;
use crate::tex::token::Token;
use crate::utils::errors::{OtherError, TeXError, UndefinedActiveCharacter, UndefinedControlSequence};
use crate::utils::strings::TeXStr;
use chrono::{DateTime,Local};
use crate::engine::Outputs;
use crate::tex::numbers::NumSet;
use crate::utils::Ptr;

pub mod fields;
pub mod modes;

/// A TeX state
pub trait State<T:Token>:Sized+'static {
    type FS:FileSystem<T::Char>;
    type Gr:GroupType;
    type NumSet:NumSet;
    /// Should be called at the start of a new job with the jobname (name of the main file)
    fn set_job(&mut self, jobname:String);

    fn get_jobname(&self) -> String;
    fn get_start_time(&self) -> DateTime<Local>;

    fn outputs(&self) -> &Outputs;

    /// The current [`TeXMode`]
    fn mode(&self) -> TeXMode;

    /// get the [`FileSystem`] used by this state
    fn filesystem(&mut self) -> &mut Self::FS;
    fn file_openout(&mut self,i:usize,f:<Self::FS as FileSystem<T::Char>>::F);
    fn file_closeout(&mut self, i: usize);
    fn file_openin(&mut self,i:usize,f:<Self::FS as FileSystem<T::Char>>::F);
    fn file_closein(&mut self, i: usize);
    fn get_open_out_file(&self,i:usize) -> Option<<Self::FS as FileSystem<T::Char>>::F>;
    fn get_open_in_file(&self,i:usize) -> Option<<Self::FS as FileSystem<T::Char>>::F>;

    /// push a new group onto the stack
    fn stack_push(&mut self, g: Self::Gr);

    fn stack_pop(&mut self, g:Self::Gr) -> Result<Option<Vec<T>>,Box<dyn TeXError<T>>>;

    /// get the current group type
    fn get_grouptype(&self) -> Self::Gr;


    /// get the current escape character (`\escapechar`)
    fn get_escapechar(&self) -> Option<T::Char>;
    /// set the current escape character (`\escapechar`)
    fn set_escapechar(&mut self, c: Option<T::Char>, globally:bool);

    /// get the current endline character (`\endlinechar`)
    fn get_endlinechar(&self) -> Option<T::Char>;
    /// set the current endline character (`\endlinechar`)
    fn set_endlinechar(&mut self, c: Option<T::Char>, globally:bool);

    /// get the current newline character (`\newlinechar`)
    fn get_newlinechar(&self) -> Option<T::Char>;
    /// set the current newline character (`\newlinechar`)
    fn set_newlinechar(&mut self, c: Option<T::Char>, globally:bool);

    /// get the current [`Command`] with name `name:`[`TeXStr`]
    fn get_command(&self, name:&TeXStr<T::Char>) -> Option<Ptr<Command<T>>>;
    /// get the current [`Command`] for the active character `c`
    fn get_ac_command(&self, c: T::Char) -> Option<Ptr<Command<T>>>;
    /// set the current [`Command`] with name `name:`[`TeXStr`]
    fn set_command(&mut self, name:TeXStr<T::Char>, cmd:Option<Ptr<Command<T>>>, globally:bool);
    /// set the current [`Command`] for the active character `c`
    fn set_ac_command(&mut self, c: T::Char, cmd:Option<Ptr<Command<T>>>, globally:bool);
    /// get the current [`Command`] with name `name:`[`TeXStr`], or return an error if it is not defined
    fn need_command(&self, name:&TeXStr<T::Char>) -> Result<Ptr<Command<T>>,UndefinedControlSequence<T>> {
        self.get_command(name).ok_or(UndefinedControlSequence(name.clone()))
    }
    /// get the current [`Command`] for the active character `c`, or return an error if it is not defined
    fn need_ac_command(&self, c: T::Char) -> Result<Ptr<Command<T>>,UndefinedActiveCharacter<T>> {
        self.get_ac_command(c).ok_or(UndefinedActiveCharacter(c))
    }

    /// get the current [`CategoryCodeScheme`]
    fn get_catcode_scheme(&self) -> &CategoryCodeScheme<T::Char>;
    /// set the current [`CategoryCode`] for a character
    fn set_catcode(&mut self, c: T::Char, cc:CategoryCode, globally:bool);

    /// get the uppercase character for a character
    fn get_ucchar(&self, c: T::Char) -> T::Char;
    /// set the uppercase character for a character
    fn set_ucchar(&mut self, c: T::Char, uc: T::Char, globally:bool);

    /// get the lowercase character for a character
    fn get_lcchar(&self, c: T::Char) -> T::Char;
    /// set the lowercase character for a character
    fn set_lcchar(&mut self, c: T::Char, lc: T::Char, globally:bool);

    /// get the value of an integer register
    fn get_int_register(&self,i:usize) -> <<Self as State<T>>::NumSet as NumSet>::Int;
    /// set the value of an integer register
    fn set_int_register(&mut self,i:usize,v:<<Self as State<T>>::NumSet as NumSet>::Int,globally:bool);



    /// get a primitive integer value
    fn get_primitive_int(&self,name:&'static str) -> <<Self as State<T>>::NumSet as NumSet>::Int;
    /// set a primitive integer value
    fn set_primitive_int(&mut self,name:&'static str,v:<<Self as State<T>>::NumSet as NumSet>::Int,globally:bool);

    /// get a primitive dimension register
    fn get_primitive_dim(&self, name:&'static str) -> <<Self as State<T>>::NumSet as NumSet>::Dim;
    /// set a primitive dimension register
    fn set_primitive_dim(&mut self, name:&'static str, v:<<Self as State<T>>::NumSet as NumSet>::Dim, globally:bool);

    /// get a primitive token register
    fn get_primitive_toks(&self, name:&'static str) -> Vec<T>;
    /// set a primitive token register
    fn set_primitive_toks(&mut self, name:&'static str, v:Vec<T>, globally:bool);
}

pub struct TeXState<T:Token,FS:FileSystem<T::Char>,NS:NumSet> {
    filesystem:FS,
    out_files:Vec<Option<FS::F>>,
    in_files:Vec<Option<FS::F>>,

    outputs:Outputs,

    jobname: Option<String>,
    start_time: Option<DateTime<Local>>,

    mode: TeXMode,
    /* filesystem: FS,*/
    grouptype: Vec<TeXGroupType>,
    aftergroups:Vec<Vec<T>>,
    endlinechar: SingleValueField<Option<T::Char>>,
    escapechar: SingleValueField<Option<T::Char>>,
    newlinechar: SingleValueField<Option<T::Char>>,

    commands: HashMapField<TeXStr<T::Char>,Option<Ptr<Command<T>>>>,
    ac_commands: CharField<T::Char,Option<Ptr<Command<T>>>>,

    catcodes: CharField<T::Char,CategoryCode>,
    ucchar: CharField<T::Char, T::Char>,
    lcchar: CharField<T::Char, T::Char>,

    intregisters: VecField<NS::Int>,

    primitive_intregisters: HashMapField<&'static str,NS::Int>,
    primitive_dimregisters: HashMapField<&'static str,NS::Dim>,
    primitive_tokregisters: HashMapField<&'static str,Vec<T>>,
}
use crate::utils::strings::CharType;
impl<T:Token,FS:FileSystem<T::Char>,NS:NumSet> TeXState<T,FS,NS> {
    pub fn new(fs:FS,outputs:Outputs) -> Self {
        let mut state = Self {
            filesystem:fs,
            out_files:vec!(),
            in_files:vec!(),
            outputs,
            aftergroups:vec!(vec!()),

            jobname: None,
            start_time: None,
            mode: TeXMode::Vertical,
            /* filesystem: fs,*/
            grouptype: vec![TeXGroupType::Top],
            endlinechar: SingleValueField::new(Some(T::Char::carriage_return())),
            escapechar: SingleValueField::new(Some(T::Char::backslash())),
            newlinechar: SingleValueField::new(Some(T::Char::newline())),
            commands: HashMapField::new(),
            ac_commands: CharField::new(T::Char::rep_field(None)),
            catcodes: CharField::new(T::Char::starting_catcode_scheme()),
            ucchar: CharField::new(T::Char::ident()),
            lcchar: CharField::new(T::Char::ident()),
            intregisters: VecField::new(),

            primitive_intregisters: HashMapField::new(),
            primitive_dimregisters: HashMapField::new(),
            primitive_tokregisters: HashMapField::new(),
        };
        for i in 97..123 {
            state.ucchar.set_locally((i as u8).into(),((i-32) as u8).into());
            state.lcchar.set_locally(((i-32) as u8).into(),(i as u8).into());
            /*state.mathcodes.set_locally(i-32,
                                        (i as i32-32) +
                                            (1 * 16 * 16) +
                                            (7 * 16 * 16 * 16)
            );
            state.mathcodes.set_locally(i,
                                        (i as i32) +
                                            (1 * 16 * 16) +
                                            (7 * 16 * 16 * 16)
            );*/
        }
        /*for i in 48..58 {
            state.mathcodes.set_locally(i,
                                        (i as i32) +
                                            (0 * 16 * 16) +
                                            (7 * 16 * 16 * 16)
            );
        }*/
        state
    }
}
impl<T:Token,FS:FileSystem<T::Char>,NS:NumSet> State<T> for TeXState<T,FS,NS> {
    type FS = FS;
    type Gr = TeXGroupType;
    type NumSet=NS;
    fn mode(&self) -> TeXMode { self.mode }
    fn set_job(&mut self, jobname: String) {
        self.jobname = Some(jobname);
        self.start_time = Some(Local::now());
    }
    fn outputs(&self) -> &Outputs { &self.outputs }
    fn get_jobname(&self) -> String {
        self.jobname.as_ref().unwrap().clone()
    }
    fn get_start_time(&self) -> DateTime<Local> {
        self.start_time.as_ref().unwrap().clone()
    }
    fn filesystem(&mut self) -> &mut FS {
        &mut self.filesystem
    }
    fn file_openin(&mut self, i: usize, f: <Self::FS as FileSystem<T::Char>>::F) {
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
    fn file_openout(&mut self, i: usize, f: <Self::FS as FileSystem<T::Char>>::F) {
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
    fn get_open_out_file(&self,i:usize) -> Option<<Self::FS as FileSystem<T::Char>>::F> {
        if i >= self.out_files.len() {
            None
        } else {
            self.out_files[i].clone()
        }
    }
    fn get_open_in_file(&self,i:usize) -> Option<<Self::FS as FileSystem<T::Char>>::F> {
        if i >= self.in_files.len() {
            None
        } else {
            self.in_files[i].clone()
        }
    }
    // #[inline(always)]
    fn stack_push(&mut self, g: TeXGroupType) {
        self.grouptype.push(g);
        self.endlinechar.push_stack();
        self.escapechar.push_stack();
        self.newlinechar.push_stack();

        self.commands.push_stack();
        self.ac_commands.push_stack();

        self.catcodes.push_stack();
        self.ucchar.push_stack();
        self.lcchar.push_stack();

        self.intregisters.push_stack();

        self.primitive_intregisters.push_stack();
        self.primitive_dimregisters.push_stack();
        self.primitive_tokregisters.push_stack();
        unsafe{self.aftergroups.push(self.aftergroups.last().unwrap_unchecked().clone())};
    }
    fn stack_pop(&mut self, g: Self::Gr) -> Result<Option<Vec<T>>, Box<dyn TeXError<T>>> {
        match self.grouptype.pop() {
            None => return Err(OtherError{msg:"There's no group here to end".to_string(),cause:None,source:None}.into()),
            Some(gt) if gt != g => return Err(OtherError{msg:"There's no group here to end".to_string(),cause:None,source:None}.into()),
            _ => ()
        }
        self.endlinechar.pop_stack();
        self.escapechar.pop_stack();
        self.newlinechar.pop_stack();

        self.commands.pop_stack();
        self.ac_commands.pop_stack();

        self.catcodes.pop_stack();
        self.ucchar.pop_stack();
        self.lcchar.pop_stack();

        self.intregisters.pop_stack();

        self.primitive_intregisters.pop_stack();
        self.primitive_dimregisters.pop_stack();
        self.primitive_tokregisters.pop_stack();

        Ok(self.aftergroups.pop())
    }

    // #[inline(always)]
    fn get_grouptype(&self) -> TeXGroupType { *self.grouptype.last().unwrap_or(&TeXGroupType::Top) }

    // #[inline(always)]
    fn get_escapechar(&self) -> Option<T::Char> { *self.escapechar.get() }
    // #[inline(always)]
    fn set_escapechar(&mut self, c: Option<T::Char>, globally: bool) {
        if globally {
            self.escapechar.set_globally(c)
        } else {
            self.escapechar.set_locally(c)
        }
    }

    // #[inline(always)]
    fn get_endlinechar(&self) -> Option<T::Char> { *self.endlinechar.get() }
    // #[inline(always)]
    fn set_endlinechar(&mut self, c: Option<T::Char>, globally: bool) {
        if globally {
            self.endlinechar.set_globally(c)
        } else {
            self.endlinechar.set_locally(c)
        }
    }

    // #[inline(always)]
    fn get_newlinechar(&self) -> Option<T::Char> { *self.newlinechar.get() }
    // #[inline(always)]
    fn set_newlinechar(&mut self, c: Option<T::Char>, globally: bool) {
        if globally {
            self.newlinechar.set_globally(c)
        } else {
            self.newlinechar.set_locally(c)
        }
    }

    // #[inline(always)]
    fn get_command(&self, name: &TeXStr<T::Char>) -> Option<Ptr<Command<T>>> {
        self.commands.get(name)
    }
    // #[inline(always)]
    fn set_command(&mut self, name: TeXStr<T::Char>, cmd: Option<Ptr<Command<T>>>, globally: bool) {
        if globally {
            self.commands.set_globally(name,cmd)
        } else {
            self.commands.set_locally(name,cmd)
        }
    }
    fn get_ac_command(&self, c: T::Char) -> Option<Ptr<Command<T>>> {
        self.ac_commands.get(&c)
    }
    fn set_ac_command(&mut self, c: T::Char, cmd: Option<Ptr<Command<T>>>, globally: bool) {
        if globally {
            self.ac_commands.set_globally(c,cmd)
        } else {
            self.ac_commands.set_locally(c,cmd)
        }
    }


    // #[inline(always)]
    fn get_catcode_scheme(&self) -> &CategoryCodeScheme<T::Char> {
        &self.catcodes.charfield
    }
    // #[inline(always)]
    fn set_catcode(&mut self, c: T::Char, cc: CategoryCode, globally: bool) {
        if globally {
            self.catcodes.set_globally(c,cc)
        } else {
            self.catcodes.set_locally(c,cc)
        }
    }

    // #[inline(always)]
    fn get_ucchar(&self, c: T::Char) -> T::Char {
        self.ucchar.get(&c)
    }
    // #[inline(always)]
    fn set_ucchar(&mut self, c: T::Char, uc: T::Char, globally: bool) {
        if globally {
            self.ucchar.set_globally(c,uc)
        } else {
            self.ucchar.set_locally(c,uc)
        }
    }

    // #[inline(always)]
    fn get_lcchar(&self, c: T::Char) -> T::Char {
        self.lcchar.get(&c)
    }
    // #[inline(always)]
    fn set_lcchar(&mut self, c: T::Char, lc: T::Char, globally: bool) {
        if globally {
            self.lcchar.set_globally(c,lc)
        } else {
            self.lcchar.set_locally(c,lc)
        }
    }

    // #[inline(always)]
    fn get_int_register(&self, i: usize) -> NS::Int {
        self.intregisters.get(&i)
    }
    // #[inline(always)]
    fn set_int_register(&mut self, i: usize, v: NS::Int, globally: bool) {
        if globally {
            self.intregisters.set_globally(i,v)
        } else {
            self.intregisters.set_locally(i,v)
        }
    }

    fn get_primitive_int(&self, name: &'static str) -> <<Self as State<T>>::NumSet as NumSet>::Int {
        self.primitive_intregisters.get(&name)
    }
    fn set_primitive_int(&mut self, name: &'static str, v: <<Self as State<T>>::NumSet as NumSet>::Int, globally: bool) {
        if globally {
            self.primitive_intregisters.set_globally(name,v)
        } else {
            self.primitive_intregisters.set_locally(name,v)
        }
    }

    fn get_primitive_dim(&self, name: &'static str) -> <<Self as State<T>>::NumSet as NumSet>::Dim {
        self.primitive_dimregisters.get(&name)
    }
    fn set_primitive_dim(&mut self, name: &'static str, v: <<Self as State<T>>::NumSet as NumSet>::Dim, globally: bool) {
        if globally {
            self.primitive_dimregisters.set_globally(name,v)
        } else {
            self.primitive_dimregisters.set_locally(name,v)
        }
    }

    fn get_primitive_toks(&self, name: &'static str) -> Vec<T> {
        self.primitive_tokregisters.get(&name)
    }
    fn set_primitive_toks(&mut self, name: &'static str, v: Vec<T>, globally: bool) {
        if globally {
            self.primitive_tokregisters.set_globally(name,v)
        } else {
            self.primitive_tokregisters.set_locally(name,v)
        }
    }

}