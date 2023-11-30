/*! A TeX engine combines all the necessary components into a struct capable of compiling a TeX file into
    some output format.
*/
use std::fmt::Debug;
use crate::engine::gullet::{DefaultGullet, Gullet, ResolvedToken};
use crate::engine::utils::memory::{InternedCSName, InternedString, MemoryManager, PRIMITIVES};
use crate::engine::mouth::{DefaultMouth, Mouth};
use crate::engine::state::State;
use crate::engine::stomach::{Stomach, StomachWithShipout};
use crate::tex;
use crate::tex::catcodes::CommandCode;
use crate::commands::{Assignment, Command, DimCommand, FontCommand, IntCommand, MuSkipCommand, SkipCommand, Unexpandable, Whatsit};
use crate::engine::filesystem::{File, FileSystem, VirtualFile};
use crate::engine::fontsystem::{FontSystem, TfmFontSystem};
use crate::engine::utils::outputs::{LogOutputs, Outputs};
use crate::tex::input_text::Character;
use crate::tex::control_sequences::ControlSequenceName;
use crate::tex::numerics::{Dim32, MuSkip, MuSkip32, Numeric, NumSet, Skip, Skip32, TeXDimen, TeXInt};
use crate::tex::token::{CompactToken, Token};
use crate::utils::errors::{catch, ErrorHandler, TeXError};

pub mod filesystem;
pub mod mouth;
pub mod state;
pub mod gullet;
pub mod stomach;
pub mod utils;
pub mod fontsystem;

/**
    The types that an engine needs to implement. To reduce overhad in function signature,
    we bundle all of them into a single trait and pass that around.
*/
pub trait EngineTypes:Sized+Copy+Clone+Debug {
    type Char: Character;
    type CSName: ControlSequenceName<Self::Char>;
    type Token: Token<Char = Self::Char, CS = Self::CSName>;
    type ErrorHandler: ErrorHandler;
    type Extension: EngineExtension;
    type File: File<Char=Self::Char>;
    type FileSystem: FileSystem<File=Self::File>;
    type Int:TeXInt;
    type Dim:TeXDimen + Numeric<Self::Int>;
    type Skip:Skip<Base = Self::Dim> + Numeric<Self::Int>;
    type MuSkip:MuSkip + Numeric<Self::Int>;
    type Num: crate::tex::numerics::NumSet<Int = Self::Int,Dim=Self::Dim,Skip=Self::Skip,MuSkip=Self::MuSkip>;
    type State: State<ET=Self>;
    type Outputs: Outputs;
    type Mouth: Mouth<Token=Self::Token,File=Self::File>;
    type Memory:MemoryManager<Self::Token>;
    type Gullet:Gullet<ET=Self>;
    type Stomach:Stomach<ET=Self>;
    type FontSystem: FontSystem<Char=Self::Char,Int=Self::Int,Dim=Self::Dim,CS=Self::CSName>;
}
pub struct EngineAux<ET:EngineTypes> {
    pub memory:ET::Memory,
    pub error_handler:ET::ErrorHandler,
    pub outputs:ET::Outputs,
    pub start_time:chrono::DateTime<chrono::Local>,
    pub jobname:String,
    pub extension:ET::Extension
}

/**
    A TeX engine combines all the necessary components into a struct capable of compiling a TeX file into
    some output format. We use public fields instead of accessor methods to convince the borrow checker
    that all the components are *independent*, and avoid "Cannot borrow as mutable because already borrowed as
    immutable" errors.
*/
pub struct EngineReferences<'et,ET:EngineTypes> {
    pub state:&'et mut ET::State,
    pub mouth:&'et mut ET::Mouth,
    pub gullet:&'et mut ET::Gullet,
    pub stomach:&'et mut ET::Stomach,
    pub filesystem:&'et mut ET::FileSystem,
    pub fontsystem:&'et mut ET::FontSystem,
    pub aux:&'et mut EngineAux<ET>
}

/// Example implementation of [`EngineTypes`] for a plain TeX engine.
#[derive(Copy,Clone,Debug)]
pub struct DefaultPlainTeXEngineTypes;
impl EngineTypes for DefaultPlainTeXEngineTypes {
    type Char = u8;
    type CSName = utils::memory::InternedCSName<u8>;//InternedString;
    type Token = super::tex::token::CompactToken;//::StandardToken<Self::CSName,u8>;//
    type ErrorHandler = super::utils::errors::ErrorThrower;
    type Extension = ();
    type Int = i32;
    type Dim = Dim32;
    type Skip = Skip32<Dim32>;
    type MuSkip = MuSkip32;
    type Num = tex::numerics::DefaultNumSet;
    type State = state::tex_state::TeXState<Self>;
    type Memory = utils::memory::ReuseTokenLists<Self::Token>;
    type File = VirtualFile<u8>;
    type FileSystem = filesystem::NoOutputFileSystem<u8>;
    type Outputs = LogOutputs;
    type Mouth = DefaultMouth<Self::Token,Self::File>;
    type Gullet = DefaultGullet<Self>;
    type Stomach = StomachWithShipout<Self>;
    type FontSystem = TfmFontSystem<i32,Dim32,InternedCSName<u8>>;//InternedString>;
}

//type Int<S> = <<<S as TeXEngine>::Types as EngineTypes>::Num as NumSet>::Int;

pub trait TeXEngine:Sized {
    type Types:EngineTypes;
    fn get_engine_refs(&mut self) -> EngineReferences<Self::Types>;
    fn init_file(&mut self,s:&str) -> Result<(),TeXError> {catch( ||{
        log::debug!("Initializing with file {}",s);
        let mut comps = self.get_engine_refs();
        let file = comps.filesystem.get(s);
        comps.aux.jobname = file.path().with_extension("").file_name().unwrap().to_str().unwrap().to_string();
        //comps.start_time = Local::now();
        comps.push_file(file);
        //(comps.outputs.file_open)(file.path().to_str().unwrap());
        //comps.mouth.push_file(&file,&mut comps.interner);
        // should not produce any boxes, so loop until file end
        //ET::Stomach::next_shipout_box(&mut comps);
        //comps.filesystem.set_pwd(old);
        comps.top_loop();
    })}
    fn initialize_plain_tex(&mut self) {
        super::commands::tex::register_tex_primitives(self);
        let mag = PRIMITIVES.mag;
        let fam = PRIMITIVES.fam;
        let refs = self.get_engine_refs();
        refs.state.set_primitive_int(refs.aux,mag, (1000).into(),true);
        refs.state.set_primitive_int(refs.aux,fam, (-1).into(),true);
    }
    fn initialize_etex(&mut self) {
        self.initialize_plain_tex();
        super::commands::etex::register_etex_primitives(self);
    }
    #[inline(always)]
    fn load_latex(&mut self) -> Result<(),TeXError> {
        self.init_file("latex.ltx")
    }

    fn initialize_pdflatex(&mut self) -> Result<(),TeXError> {
        self.initialize_etex();
        super::commands::pdftex::register_pdftex_primitives(self);
        self.init_file("pdftexconfig.tex")?;
        self.load_latex()
    }
}

/// Example implementation of a plain TeX engine.
pub struct PlainTeXEngine {
    aux:EngineAux<DefaultPlainTeXEngineTypes>,
    pub state: state::tex_state::TeXState<DefaultPlainTeXEngineTypes>,
    filesystem: filesystem::NoOutputFileSystem<u8>,
    fontsystem: fontsystem::TfmFontSystem<i32,Dim32,InternedCSName<u8>>,
    pub mouth: mouth::DefaultMouth<<DefaultPlainTeXEngineTypes as EngineTypes>::Token,<filesystem::NoOutputFileSystem<u8> as FileSystem>::File>,
    gullet: gullet::DefaultGullet<DefaultPlainTeXEngineTypes>,
    pub stomach: stomach::StomachWithShipout<DefaultPlainTeXEngineTypes>
}
impl TeXEngine for PlainTeXEngine {
    type Types = DefaultPlainTeXEngineTypes;
    fn get_engine_refs(&mut self) -> EngineReferences<DefaultPlainTeXEngineTypes> {
        EngineReferences {
            aux: &mut self.aux,
            state: &mut self.state,
            filesystem: &mut self.filesystem,
            fontsystem: &mut self.fontsystem,
            mouth: &mut self.mouth,
            gullet: &mut self.gullet,
            stomach: &mut self.stomach
        }
    }
}
impl PlainTeXEngine {
    pub fn reset(&mut self, s:state::tex_state::TeXState<DefaultPlainTeXEngineTypes>) {
        self.state = s;
        self.mouth = DefaultMouth::new();
        self.gullet = DefaultGullet::new();
        self.stomach = StomachWithShipout::new();
    }
    pub fn new() -> Self {
        let mut aux = EngineAux {
            memory: utils::memory::ReuseTokenLists::new(),
            outputs: LogOutputs,
            error_handler: super::utils::errors::ErrorThrower,
            start_time:chrono::Local::now(),
            extension: (),
            jobname: String::new()
        };
        let fontsystem = fontsystem::TfmFontSystem::new(&mut aux);
        PlainTeXEngine {
            state: state::tex_state::TeXState::new(fontsystem.null(),&aux.memory),
            aux,
            fontsystem,
            filesystem: filesystem::NoOutputFileSystem::new(crate::utils::PWD.to_path_buf()),
            mouth: mouth::DefaultMouth::new(),
            gullet: gullet::DefaultGullet::new(),
            stomach: stomach::StomachWithShipout::new()
        }
    }
}

/** Additional components we want to add to a [`EngineReferences`] can be implemented here.
    Notably, `()` extends this trait if we don't need any additional components.
 */
pub trait EngineExtension {}
impl EngineExtension for () {}

impl<ET:EngineTypes> EngineReferences<'_,ET> {
    pub fn trace_command<D:std::fmt::Display,F:FnOnce(&mut Self) -> D>(&mut self, f:F) {
        let trace = self.state.get_primitive_int(PRIMITIVES.tracingcommands) > <ET::Num as NumSet>::Int::default();
        if trace {
            let d = f(self);
            self.aux.outputs.write_neg1(format_args!("{{{}}}",d));
        }
    }

    pub fn top_loop(&mut self) {
        use crate::tex::control_sequences::ControlSequenceNameHandler;
        crate::expand_loop!(self,
            ResolvedToken::Tk { char, code, token } => ET::Stomach::do_char(self, token, char, code),
            ResolvedToken::Cmd {token,cmd:Some(Command::Char {char, code})} => ET::Stomach::do_char(self, token, *char, *code),
            ResolvedToken::Cmd{cmd: None,token} => self.aux.error_handler.undefined(self.aux.memory.cs_interner(),token),
            ResolvedToken::Cmd{cmd: Some(cmd),token} => crate::do_cmd!(self,token,cmd)
        );
    }

    pub fn expand_until_bgroup(&mut self,allow_let:bool) {
        while let Some(tk) = self.get_next() {
            if tk.is_begin_group() {return }
            crate::expand!(self,tk;
                ResolvedToken::Cmd {cmd: Some(Command::Char {code:CommandCode::BeginGroup,..}),..} if allow_let =>
                    return,
                _ => todo!("error")
            );
        }
        todo!("file end error")
    }
}

#[macro_export]
macro_rules! expand_loop {
    ($engine:ident,$($case:tt)*) => {{
        crate::expand_loop!(ET;$engine,$($case)*)
    }};
    ($ET:ty; $engine:ident,$($case:tt)*) => {{
        while let Some(tk) = $engine.get_next() {
            crate::expand!($ET;$engine,tk;$($case)*);
        }
    }}
}

#[macro_export]
macro_rules! expand {
    ($engine:ident,$tk:expr;$($case:tt)*) => {
        crate::expand!(ET;$engine,$tk;$($case)*)
    };
    ($ET:ty; $engine:ident,$tk:expr;$($case:tt)*) => {
        let cmd = $engine.resolve($tk);
        match cmd {
            crate::engine::gullet::ResolvedToken::Cmd{cmd: Some(crate::commands::Command::Macro(m)),token} =>
                <<$ET as EngineTypes>::Gullet as crate::engine::gullet::Gullet>::do_macro($engine,m.clone(),token),
            crate::engine::gullet::ResolvedToken::Cmd{cmd: Some(crate::commands::Command::Conditional(cond)),token} =>
                <<$ET as EngineTypes>::Gullet as crate::engine::gullet::Gullet>::do_conditional($engine,cond.name,token,cond.expand,false),
            crate::engine::gullet::ResolvedToken::Cmd{cmd: Some(crate::commands::Command::Expandable(e)),token} =>
                <<$ET as EngineTypes>::Gullet as crate::engine::gullet::Gullet>::do_expandable($engine,e.name,token,e.expand),
            crate::engine::gullet::ResolvedToken::Cmd{cmd: Some(crate::commands::Command::SimpleExpandable(e)),token} =>
                <<$ET as EngineTypes>::Gullet as crate::engine::gullet::Gullet>::do_simple_expandable($engine,e.name,token,e.expand),
            $($case)*
        }
    }
}

#[macro_export]
macro_rules! do_cmd {
    ($engine:ident,$token:expr,$cmd:ident) => {
        crate::do_cmd!(ET;$engine,$token,$cmd)
    };
    ($ET:ty;$engine:ident,$token:expr,$cmd:ident) => {
        match $cmd {
            crate::commands::Command::CharDef(char)  => <$ET as EngineTypes>::Stomach::do_char($engine, $token, *char, CommandCode::Other),
            crate::commands::Command::Unexpandable(crate::commands::Unexpandable { name, apply }) =>
                <$ET as EngineTypes>::Stomach::do_unexpandable($engine, *name, $token, *apply),
            crate::commands::Command::Assignment(crate::commands::Assignment { name, assign }) =>
                <$ET as EngineTypes>::Stomach::do_assignment($engine, *name, $token, *assign,false),
            crate::commands::Command::Int(crate::commands::IntCommand { name, assign: Some(assign), .. }) =>
                <$ET as EngineTypes>::Stomach::do_assignment($engine, *name, $token, *assign,false),
            crate::commands::Command::Dim(crate::commands::DimCommand { name, assign: Some(assign), .. }) =>
                <$ET as EngineTypes>::Stomach::do_assignment($engine, *name, $token, *assign,false),
            crate::commands::Command::Skip(crate::commands::SkipCommand { name, assign: Some(assign), .. }) =>
                <$ET as EngineTypes>::Stomach::do_assignment($engine, *name, $token, *assign,false),
            crate::commands::Command::MuSkip(crate::commands::MuSkipCommand { name, assign: Some(assign), .. }) =>
                <$ET as EngineTypes>::Stomach::do_assignment($engine, *name, $token, *assign,false),
            crate::commands::Command::FontCmd(crate::commands::FontCommand { name, assign: Some(assign), .. }) =>
                <$ET as EngineTypes>::Stomach::do_assignment($engine, *name, $token, *assign,false),
            crate::commands::Command::Box(crate::commands::BoxCommand { name, read }) =>
                <$ET as EngineTypes>::Stomach::do_box($engine, *name, $token, *read),
            crate::commands::Command::Font(f) =>
                <$ET as EngineTypes>::Stomach::do_font($engine, $token, f.clone(),false),
            crate::commands::Command::IntRegister(u) =>
                <$ET as EngineTypes>::Stomach::assign_int_register($engine, *u,false),
            crate::commands::Command::DimRegister(u) =>
                <$ET as EngineTypes>::Stomach::assign_dim_register($engine, *u,false),
            crate::commands::Command::SkipRegister(u) =>
                <$ET as EngineTypes>::Stomach::assign_skip_register($engine, *u,false),
            crate::commands::Command::MuSkipRegister(u) =>
               <$ET as EngineTypes>::Stomach::assign_muskip_register($engine, *u,false),
            crate::commands::Command::ToksRegister(u) =>
                <$ET as EngineTypes>::Stomach::assign_toks_register($engine, *u,false),
            crate::commands::Command::Whatsit(crate::commands::Whatsit { name, get, .. }) =>
                <$ET as EngineTypes>::Stomach::do_whatsit($engine, $token, *get),
            crate::commands::Command::PrimitiveInt(name) =>
                <$ET as EngineTypes>::Stomach::assign_primitive_int($engine,*name,false),
            crate::commands::Command::PrimitiveDim(name) =>
                <$ET as EngineTypes>::Stomach::assign_primitive_dim($engine,*name,false),
            crate::commands::Command::PrimitiveSkip(name) =>
                <$ET as EngineTypes>::Stomach::assign_primitive_skip($engine,*name,false),
            crate::commands::Command::PrimitiveMuSkip(name) =>
                <$ET as EngineTypes>::Stomach::assign_primitive_muskip($engine,*name,false),
            crate::commands::Command::PrimitiveToks(name) =>
                <$ET as EngineTypes>::Stomach::assign_primitive_toks($engine,*name,false),
            crate::commands::Command::MathChar(_) => todo!("mathchar"),
            crate::commands::Command::Relax => (),
            crate::commands::Command::Int(crate::commands::IntCommand { name, .. }) |
            crate::commands::Command::Dim(crate::commands::DimCommand { name, .. }) |
            crate::commands::Command::Skip(crate::commands::SkipCommand { name, .. }) |
            crate::commands::Command::MuSkip(crate::commands::MuSkipCommand { name, .. }) |
            crate::commands::Command::FontCmd(crate::commands::FontCommand { name, .. }) =>
                todo!("Not allowed in X mode"),
            crate::commands::Command::Macro(_) | Command::Conditional(_) | Command::Expandable(_)  | Command::SimpleExpandable(_) | Command::Char {..} => unreachable!(),
        }
    }
}