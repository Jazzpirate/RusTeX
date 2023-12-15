/*! A TeX engine combines all the necessary components into a struct capable of compiling a TeX file into
    some output format.
*/
use std::fmt::Debug;
use std::marker::PhantomData;
use crate::engine::gullet::{DefaultGullet, Gullet, ResolvedToken};
use crate::engine::utils::memory::{InternedCSName, InternedString, MemoryManager, PRIMITIVES};
use crate::engine::mouth::{DefaultMouth, Mouth};
use crate::engine::state::State;
use crate::engine::stomach::{Stomach, StomachWithShipout};
use crate::{debug_log, tex};
use crate::tex::catcodes::CommandCode;
use crate::commands::{Assignment, Command, DimCommand, FontCommand, IntCommand, MuSkipCommand, SkipCommand, Unexpandable, Whatsit};
use crate::commands::pdftex::pdftexnodes::{MinimalPDFExtension, PDFExtension, PDFNode, PDFNodeTrait};
use crate::engine::filesystem::{File, FileSystem, VirtualFile};
use crate::engine::fontsystem::{FontSystem, TfmFontSystem};
use crate::engine::utils::outputs::{LogOutputs, Outputs};
use crate::tex::input_text::Character;
use crate::tex::control_sequences::ControlSequenceName;
use crate::tex::nodes::{NodeTrait, PreShipoutNode, PreShipoutNodeTrait, ShipoutNode, TopNodeTrait};
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
    type PreCustomNode:NodeTrait<Self>+PreShipoutNodeTrait<Self>;
    type ShipoutCustomNode:NodeTrait<Self>;
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

pub struct Colon<'c,ET:EngineTypes> {
    out:Box<dyn FnMut(ShipoutNode<ET>) + 'c>
}
impl<'c,ET:EngineTypes> Colon<'c,ET> {
    #[inline(always)]
    pub fn new<F:FnMut(ShipoutNode<ET>) + 'c>(f:F) -> Self {
        Colon { out:Box::new(f) }
    }
    #[inline(always)]
    pub fn out(&mut self, n: ShipoutNode<ET>) {
        (self.out)(n)
    }
}
impl <'c,ET:EngineTypes> Default for Colon<'c,ET> {
    #[inline(always)]
    fn default() -> Self {
        Colon { out:Box::new(|_|{}) }
    }
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
    pub colon:Colon<'et,ET>,
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
    type PreCustomNode = ();
    type ShipoutCustomNode = ();
    type Stomach = StomachWithShipout<Self>;
    type FontSystem = TfmFontSystem<i32,Dim32,InternedCSName<u8>>;//InternedString>;
}

pub trait TeXEngine:Sized {
    type Types:EngineTypes;
    fn register_primitive(&mut self,cmd:Command<Self::Types>,name:&str);
    fn get_engine_refs(&mut self) -> EngineReferences<Self::Types>;
    fn init_file(&mut self,s:&str) -> Result<(),TeXError> {catch( ||{
        log::debug!("Initializing with file {}",s);
        let mut comps = self.get_engine_refs();
        let file = comps.filesystem.get(s);
        comps.aux.jobname = file.path().with_extension("").file_name().unwrap().to_str().unwrap().to_string();
        comps.push_file(file);
        comps.aux.start_time = chrono::Local::now();
        comps.top_loop();
    })}
    fn do_file_default<F:FnMut(ShipoutNode<Self::Types>)>(&mut self, s:&str, f:F) -> Result<(),TeXError> {catch( ||{
        log::debug!("Running file {}",s);
        let mut comps = self.get_engine_refs();
        let file = comps.filesystem.get(s);
        comps.filesystem.set_pwd(file.path().parent().unwrap().to_path_buf());
        comps.aux.jobname = file.path().with_extension("").file_name().unwrap().to_str().unwrap().to_string();
        comps.push_file(file);
        comps.mouth.insert_every::<Self::Types>(&comps.state,PRIMITIVES.everyjob);
        comps.aux.start_time = chrono::Local::now();
        //comps.aux.elapsed = std::time::Instant::now();
        debug_log!(debug =>"Here: {}",comps.preview());
        comps.colon = Colon::new(f);
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
}

pub struct DefaultEngine<ET:EngineTypes> {
    pub aux:EngineAux<ET>,
    pub state: ET::State,
    pub filesystem: ET::FileSystem,
    pub fontsystem: ET::FontSystem,
    pub mouth: ET::Mouth,
    pub gullet: ET::Gullet,
    pub stomach: ET::Stomach,
}
impl<ET:EngineTypes> DefaultEngine<ET> {
    pub fn reset(&mut self, s:ET::State) {
        self.state = s;
        self.mouth = ET::Mouth::new(&mut self.aux,&mut self.state);
        self.gullet = ET::Gullet::new(&mut self.aux,&mut self.state,&mut self.mouth);
        self.stomach = ET::Stomach::new(&mut self.aux,&mut self.state);
    }
    pub fn new() -> Self {
        let mut aux = EngineAux {
            memory: ET::Memory::new(),
            outputs: ET::Outputs::new(),
            error_handler: ET::ErrorHandler::new(),
            start_time:chrono::Local::now(),
            extension: ET::Extension::new(),
            jobname: String::new()
        };
        let fontsystem = ET::FontSystem::new(&mut aux);
        let mut state = ET::State::new(fontsystem.null(),&mut aux);
        let mut mouth = ET::Mouth::new(&mut aux,&mut state);
        let gullet = ET::Gullet::new(&mut aux,&mut state,&mut mouth);
        let stomach = ET::Stomach::new(&mut aux,&mut state);
        Self {
            state,
            aux,
            fontsystem,
            filesystem: ET::FileSystem::new(crate::utils::PWD.to_path_buf()),
            mouth,gullet,stomach
        }
    }
}
impl<ET:EngineTypes> TeXEngine for DefaultEngine<ET> {
    type Types = ET;
    fn register_primitive(&mut self,cmd: Command<ET>, name: &str) {
        use crate::tex::control_sequences::ControlSequenceNameHandler;
        let name = self.aux.memory.cs_interner_mut().new(name);
        self.state.set_command(&self.aux,name,Some(cmd),true);
    }
    fn get_engine_refs(&mut self) -> EngineReferences<ET> {
        EngineReferences {
            aux: &mut self.aux,
            state: &mut self.state,
            filesystem: &mut self.filesystem,
            fontsystem: &mut self.fontsystem,
            mouth: &mut self.mouth,
            gullet: &mut self.gullet,
            stomach: &mut self.stomach,
            colon: Colon::default()
        }
    }
}
pub type PlainTeXEngine = DefaultEngine<DefaultPlainTeXEngineTypes>;

pub trait PDFTeXEngine: TeXEngine
    where <<Self as TeXEngine>::Types as EngineTypes>::Extension : PDFExtension<Self::Types>,
          <<Self as TeXEngine>::Types as EngineTypes>::PreCustomNode : PDFNodeTrait<Self::Types> {
    fn do_file_pdf<F:FnMut(ShipoutNode<Self::Types>)>(&mut self, s:&str, f:F) -> Result<(),TeXError> {
        *self.get_engine_refs().aux.extension.elapsed() = std::time::Instant::now();
        self.do_file_default(s,f)
    }

    fn initialize_pdflatex(&mut self) -> Result<(),TeXError> {
        self.initialize_etex();
        super::commands::pdftex::register_pdftex_primitives(self);
        self.init_file("pdftexconfig.tex")?;
        self.load_latex()
    }
}


/// Example implementation of [`EngineTypes`] for a plain TeX engine.
#[derive(Copy,Clone,Debug)]
pub struct DefaultPDFTeXEngineTypes;
impl EngineTypes for DefaultPDFTeXEngineTypes {
    type Char = u8;
    type CSName = utils::memory::InternedCSName<u8>;//InternedString;
    type Token = super::tex::token::CompactToken;//::StandardToken<u8,Self::CSName>;//
    type ErrorHandler = super::utils::errors::ErrorThrower;
    type Extension = MinimalPDFExtension<Self>;
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
    type PreCustomNode = PDFNode<Self,PreShipoutNode<Self>>;
    type ShipoutCustomNode = PDFNode<Self,ShipoutNode<Self>>;
    type Stomach = StomachWithShipout<Self>;
    type FontSystem = TfmFontSystem<i32,Dim32,InternedCSName<u8>>;//InternedString>;
}

pub type PlainPDFTeXEngine = DefaultEngine<DefaultPDFTeXEngineTypes>;
//impl PDFTeXEngine for PlainPDFTeXEngine {}
impl<A> PDFTeXEngine for A where
    A:TeXEngine,
    <A::Types as EngineTypes>::Extension : PDFExtension<A::Types>,
    <A::Types as EngineTypes>::PreCustomNode : PDFNodeTrait<A::Types> {}

/** Additional components we want to add to a [`EngineReferences`] can be implemented here.
    Notably, `()` extends this trait if we don't need any additional components.
 */
pub trait EngineExtension {
    fn new() -> Self;
}
impl EngineExtension for () {
    fn new() -> Self { () }
}

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
        crate::expand_loop!(self.mouth.update_start_ref() => self,
            ResolvedToken::Tk { char, code:CommandCode::Noexpand, token } => {self.get_next();},
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
    ($then:expr => $engine:ident,$($case:tt)*) => {{
        $then;
        while let Some(tk) = $engine.get_next() {
            crate::expand!(ET;$engine,tk;$($case)*);
            $then;
        }
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
        let cmd = <<$ET as EngineTypes>::Gullet as crate::engine::gullet::Gullet>::resolve(
            &$engine.state,$tk
        );
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
            crate::commands::Command::Node(crate::commands::NodeCommand { name, read, scope }) =>
                <$ET as EngineTypes>::Stomach::do_node($engine, *name, $token, *read,*scope),
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
                <$ET as EngineTypes>::Stomach::do_whatsit($engine, *name,$token, *get),
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

/*
/// Example implementation of a plain TeX engine.
pub struct PlainTeXEngine {
    aux:EngineAux<DefaultPlainTeXEngineTypes>,
    pub state: state::tex_state::TeXState<DefaultPlainTeXEngineTypes>,
    filesystem: filesystem::NoOutputFileSystem<u8>,
    fontsystem: fontsystem::TfmFontSystem<i32,Dim32,InternedCSName<u8>>,
    pub mouth: mouth::DefaultMouth<<DefaultPlainTeXEngineTypes as EngineTypes>::Token,<filesystem::NoOutputFileSystem<u8> as FileSystem>::File>,
    gullet: gullet::DefaultGullet<DefaultPlainTeXEngineTypes>,
    pub stomach: stomach::StomachWithShipout<DefaultPlainTeXEngineTypes>,
}
impl TeXEngine for PlainTeXEngine {
    type Types = DefaultPlainTeXEngineTypes;
    fn register_primitive(&mut self,cmd: Command<Self::Types>, name: &str) {
        use crate::tex::control_sequences::ControlSequenceNameHandler;
        let name = self.aux.memory.cs_interner_mut().new(name);
        self.state.set_command(&self.aux,name,Some(cmd),true);
    }
    fn get_engine_refs(&mut self) -> EngineReferences<DefaultPlainTeXEngineTypes> {
        EngineReferences {
            aux: &mut self.aux,
            state: &mut self.state,
            filesystem: &mut self.filesystem,
            fontsystem: &mut self.fontsystem,
            mouth: &mut self.mouth,
            gullet: &mut self.gullet,
            stomach: &mut self.stomach,
            colon: Colon::default()
        }
    }
}
impl PlainTeXEngine {
    pub fn reset(&mut self, s:state::tex_state::TeXState<DefaultPlainTeXEngineTypes>) {
        self.state = s;
        self.mouth = DefaultMouth::new(&mut self.aux,&mut self.state);
        self.gullet = DefaultGullet::new(&mut self.aux,&mut self.state,&mut self.mouth);
        self.stomach = StomachWithShipout::new(&mut self.aux,&mut self.state);
    }
    pub fn new() -> Self {
        let mut aux = EngineAux {
            memory: utils::memory::ReuseTokenLists::new(),
            outputs: LogOutputs,
            error_handler: super::utils::errors::ErrorThrower,
            start_time:chrono::Local::now(),
            //elapsed:std::time::Instant::now(),
            extension: (),
            jobname: String::new()
        };
        let fontsystem = fontsystem::TfmFontSystem::new(&mut aux);
        let mut state = state::tex_state::TeXState::new(fontsystem.null(),&mut aux);
        let mut mouth = DefaultMouth::new(&mut aux,&mut state);
        let gullet = DefaultGullet::new(&mut aux,&mut state,&mut mouth);
        let stomach = StomachWithShipout::new(&mut aux,&mut state);

        PlainTeXEngine {
            aux,
            state,
            fontsystem,
            filesystem: filesystem::NoOutputFileSystem::new(crate::utils::PWD.to_path_buf()),
            mouth,gullet,stomach
        }
    }
}
*/
/*
/// Example implementation of a plain pdfTeX engine.
pub struct PlainPDFTeXEngine {
    aux:EngineAux<DefaultPDFTeXEngineTypes>,
    pub state: state::tex_state::TeXState<DefaultPDFTeXEngineTypes>,
    filesystem: filesystem::NoOutputFileSystem<u8>,
    fontsystem: fontsystem::TfmFontSystem<i32,Dim32,InternedCSName<u8>>,
    pub mouth: mouth::DefaultMouth<<DefaultPDFTeXEngineTypes as EngineTypes>::Token,<filesystem::NoOutputFileSystem<u8> as FileSystem>::File>,
    gullet: gullet::DefaultGullet<DefaultPDFTeXEngineTypes>,
    pub stomach: stomach::StomachWithShipout<DefaultPDFTeXEngineTypes>,
}
impl PDFTeXEngine for PlainPDFTeXEngine {}
impl TeXEngine for PlainPDFTeXEngine {
    type Types = DefaultPDFTeXEngineTypes;
    fn register_primitive(&mut self,cmd: Command<Self::Types>, name: &str) {
        use crate::tex::control_sequences::ControlSequenceNameHandler;
        let name = self.aux.memory.cs_interner_mut().new(name);
        self.state.set_command(&self.aux,name,Some(cmd),true);
    }
    fn get_engine_refs(&mut self) -> EngineReferences<DefaultPDFTeXEngineTypes> {
        EngineReferences {
            aux: &mut self.aux,
            state: &mut self.state,
            filesystem: &mut self.filesystem,
            fontsystem: &mut self.fontsystem,
            mouth: &mut self.mouth,
            gullet: &mut self.gullet,
            stomach: &mut self.stomach,
            colon: Colon::default()
        }
    }
}
impl PlainPDFTeXEngine {
    pub fn reset(&mut self, s:state::tex_state::TeXState<DefaultPDFTeXEngineTypes>) {
        self.state = s;
        self.mouth = DefaultMouth::new(&mut self.aux,&mut self.state);
        self.gullet = DefaultGullet::new(&mut self.aux,&mut self.state,&mut self.mouth);
        self.stomach = StomachWithShipout::new(&mut self.aux,&mut self.state);
    }
    pub fn new() -> Self {
        let mut aux = EngineAux {
            memory: utils::memory::ReuseTokenLists::new(),
            outputs: LogOutputs,
            error_handler: super::utils::errors::ErrorThrower,
            start_time:chrono::Local::now(),
            //elapsed:std::time::Instant::now(),
            extension: MinimalPDFExtension::new(),
            jobname: String::new()
        };
        let fontsystem = fontsystem::TfmFontSystem::new(&mut aux);
        let mut state = state::tex_state::TeXState::new(fontsystem.null(),&mut aux);
        let mut mouth = DefaultMouth::new(&mut aux,&mut state);
        let gullet = DefaultGullet::new(&mut aux,&mut state,&mut mouth);
        let stomach = StomachWithShipout::new(&mut aux,&mut state);
        PlainPDFTeXEngine {
            state,aux,
            fontsystem,
            filesystem: filesystem::NoOutputFileSystem::new(crate::utils::PWD.to_path_buf()),
            mouth,gullet,stomach
        }
    }
}

 */

/*

pub struct EngineSpec<
    IState:State<ET=Self>,
    IMouth:Mouth<Token=Tk,File=FS::File>,
    IGullet:Gullet<ET=Self>,
    IStomach:Stomach<ET=Self>,
    Tk:Token = CompactToken,
    EH:ErrorHandler = super::utils::errors::ErrorThrower,
    EXT:EngineExtension = (),
    NS:NumSet = tex::numerics::DefaultNumSet,
    F:File<Char=Tk::Char> = VirtualFile<u8>,
    FS:FileSystem<File=F> = filesystem::NoOutputFileSystem<u8>,
    Fnt:FontSystem<Char=Tk::Char,CS=Tk::CS,Font=F,Int=NS::Int,Dim=NS::Dim>
        = TfmFontSystem<<NS as NumSet>::Int,<NS as NumSet>::Dim,<Tk as Token>::CS>,
>(
    PhantomData<IState>,PhantomData<IGullet>,PhantomData<IStomach>,
    PhantomData<Tk>,PhantomData<EH>,PhantomData<EXT>,PhantomData<FS>,
    PhantomData<IMouth>,PhantomData<F>,PhantomData<Fnt>,PhantomData<NS>
);

impl<
    IState:State<ET=Self>,
    IMouth:Mouth<Token=Tk,File=FS::File>,
    IGullet:Gullet<ET=Self>,
    IStomach:Stomach<ET=Self>,
    Tk:Token,
    EH:ErrorHandler,
    EXT:EngineExtension,
    NS:NumSet,
    F:File<Char=Tk::Char>,
    FS:FileSystem<File=F>,
    Fnt:FontSystem<Char=Tk::Char,CS=Tk::CS,Font=F,Int=NS::Int,Dim=NS::Dim>,
> EngineTypes for EngineSpec<IState,IMouth,IGullet,IStomach,Tk,EH,EXT,NS,F,FS,Fnt> {
    type Char = Tk::Char;
    type CSName = Tk::CS;//InternedString;
    type Token = Tk;//::StandardToken<Self::CSName,u8>;//
    type ErrorHandler = EH;
    type Extension = EXT;
    type Int = NS::Int;
    type Dim = NS::Dim;
    type Skip = NS::Skip;
    type MuSkip = NS::MuSkip;
    type Num = NS;
    type State = IState;
    type Memory = utils::memory::ReuseTokenLists<Self::Token>;
    type File = F;
    type FileSystem = FS;
    type Outputs = LogOutputs;
    type Mouth = IMouth;
    type Gullet = IGullet;
    type Stomach = IStomach;
    type FontSystem = Fnt;//InternedString>;
}
impl<
    IState:State<ET=Self>,
    IMouth:Mouth<Token=Tk,File=FS::File>,
    IGullet:Gullet<ET=Self>,
    IStomach:Stomach<ET=Self>,
    Tk:Token,
    EH:ErrorHandler,
    EXT:EngineExtension,
    NS:NumSet,
    F:File<Char=Tk::Char>,
    FS:FileSystem<File=F>,
    Fnt:FontSystem<Char=Tk::Char,CS=Tk::CS,Font=F,Int=NS::Int,Dim=NS::Dim>,
> Debug for EngineSpec<IState,IMouth,IGullet,IStomach,Tk,EH,EXT,NS,F,FS,Fnt> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("EngineSpec")
    }
}
impl<
    IState:State<ET=Self>,
    IMouth:Mouth<Token=Tk,File=FS::File>,
    IGullet:Gullet<ET=Self>,
    IStomach:Stomach<ET=Self>,
    Tk:Token,
    EH:ErrorHandler,
    EXT:EngineExtension,
    NS:NumSet,
    F:File<Char=Tk::Char>,
    FS:FileSystem<File=F>,
    Fnt:FontSystem<Char=Tk::Char,CS=Tk::CS,Font=F,Int=NS::Int,Dim=NS::Dim>,
> Clone for EngineSpec<IState,IMouth,IGullet,IStomach,Tk,EH,EXT,NS,F,FS,Fnt> {
    fn clone(&self) -> Self {
        EngineSpec(PhantomData,PhantomData,
                   PhantomData,PhantomData,
        PhantomData,PhantomData,
                   PhantomData,PhantomData,
            PhantomData,PhantomData,
            PhantomData
        )
    }
}

impl<
    IState:State<ET=Self>,
    IMouth:Mouth<Token=Tk,File=FS::File>,
    IGullet:Gullet<ET=Self>,
    IStomach:Stomach<ET=Self>,
    Tk:Token,
    EH:ErrorHandler,
    EXT:EngineExtension,
    NS:NumSet,
    F:File<Char=Tk::Char>,
    FS:FileSystem<File=F>,
    Fnt:FontSystem<Char=Tk::Char,CS=Tk::CS,Font=F,Int=NS::Int,Dim=NS::Dim>,
> std::marker::Copy for EngineSpec<IState,IMouth,IGullet,IStomach,Tk,EH,EXT,NS,F,FS,Fnt> {}


pub struct StandardEngine<
    IState:State<ET=EngineSpec<IState,IMouth,IGullet,IStomach,Tk,EH,EXT,NS,F,FS,Fnt>>,
    IMouth:Mouth<Token=Tk,File=FS::File>,
    IGullet:Gullet<ET=EngineSpec<IState,IMouth,IGullet,IStomach,Tk,EH,EXT,NS,F,FS,Fnt>>,
    IStomach:Stomach<ET=EngineSpec<IState,IMouth,IGullet,IStomach,Tk,EH,EXT,NS,F,FS,Fnt>>,
    Tk:Token = CompactToken,
    EH:ErrorHandler = super::utils::errors::ErrorThrower,
    EXT:EngineExtension = (),
    NS:NumSet = tex::numerics::DefaultNumSet,
    F:File<Char=Tk::Char> = VirtualFile<u8>,
    FS:FileSystem<File=F> = filesystem::NoOutputFileSystem<u8>,
    Fnt:FontSystem<Char=Tk::Char,CS=Tk::CS,Font=F,Int=NS::Int,Dim=NS::Dim>
    = TfmFontSystem<<NS as NumSet>::Int,<NS as NumSet>::Dim,<Tk as Token>::CS>,
> {
    pub aux:EngineAux<EngineSpec<IState,IMouth,IGullet,IStomach,Tk,EH,EXT,NS,F,FS,Fnt>>,
    pub state: IState,
    pub filesystem: FS,
    pub fontsystem: Fnt,
    pub mouth: IMouth,
    pub gullet: IGullet,
    pub stomach: IStomach,
}

impl<
    IState:State<ET=EngineSpec<IState,IMouth,IGullet,IStomach,Tk,EH,EXT,NS,F,FS,Fnt>>,
    IMouth:Mouth<Token=Tk,File=FS::File>,
    IGullet:Gullet<ET=EngineSpec<IState,IMouth,IGullet,IStomach,Tk,EH,EXT,NS,F,FS,Fnt>>,
    IStomach:Stomach<ET=EngineSpec<IState,IMouth,IGullet,IStomach,Tk,EH,EXT,NS,F,FS,Fnt>>,
    Tk:Token,
    EH:ErrorHandler,
    EXT:EngineExtension,
    NS:NumSet,
    F:File<Char=Tk::Char>,
    FS:FileSystem<File=F>,
    Fnt:FontSystem<Char=Tk::Char,CS=Tk::CS,Font=F,Int=NS::Int,Dim=NS::Dim>,
> TeXEngine for StandardEngine<IState,IMouth,IGullet,IStomach,Tk,EH,EXT,NS,F,FS,Fnt> {
    type Types = EngineSpec<IState,IMouth,IGullet,IStomach,Tk,EH,EXT,NS,F,FS,Fnt>;
    fn register_primitive(&mut self,cmd: Command<Self::Types>, name: &str) {
        use crate::tex::control_sequences::ControlSequenceNameHandler;
        let name = self.aux.memory.cs_interner_mut().new(name);
        self.state.set_command(&self.aux,name,Some(cmd),true);
    }
    fn get_engine_refs(&mut self) -> EngineReferences<Self::Types> {
        EngineReferences {
            aux: &mut self.aux,
            state: &mut self.state,
            filesystem: &mut self.filesystem,
            fontsystem: &mut self.fontsystem,
            mouth: &mut self.mouth,
            gullet: &mut self.gullet,
            stomach: &mut self.stomach,
            colon: Colon::default()
        }
    }
}

impl<
    IState:State<ET=EngineSpec<IState,IMouth,IGullet,IStomach,Tk,EH,EXT,NS,F,FS,Fnt>>,
    IMouth:Mouth<Token=Tk,File=FS::File>,
    IGullet:Gullet<ET=EngineSpec<IState,IMouth,IGullet,IStomach,Tk,EH,EXT,NS,F,FS,Fnt>>,
    IStomach:Stomach<ET=EngineSpec<IState,IMouth,IGullet,IStomach,Tk,EH,EXT,NS,F,FS,Fnt>>,
    Tk:Token,
    EH:ErrorHandler,
    EXT:EngineExtension,
    NS:NumSet,
    F:File<Char=Tk::Char>,
    FS:FileSystem<File=F>,
    Fnt:FontSystem<Char=Tk::Char,CS=Tk::CS,Font=F,Int=NS::Int,Dim=NS::Dim>,
> StandardEngine<IState,IMouth,IGullet,IStomach,Tk,EH,EXT,NS,F,FS,Fnt> {
    pub fn reset(&mut self, s:IState) {
        self.state = s;
        self.mouth = IMouth::new(&mut self.aux,&mut self.state);
        self.gullet = IGullet::new(&mut self.aux,&mut self.state,&mut self.mouth);
        self.stomach = IStomach::new(&mut self.aux,&mut self.state);
    }
    pub fn new() -> Self {
        let mut aux = EngineAux {
            memory: utils::memory::ReuseTokenLists::new(),
            outputs: LogOutputs,
            error_handler: EH::new(),
            start_time:chrono::Local::now(),
            extension: EXT::new(),
            jobname: String::new()
        };
        let fontsystem = Fnt::new(&mut aux);
        let mut state = IState::new(fontsystem.null(),&mut aux);
        let mut mouth = IMouth::new(&mut aux,&mut state);
        let gullet = IGullet::new(&mut aux,&mut state,&mut mouth);
        let stomach = IStomach::new(&mut aux,&mut state);
        Self {
            state,
            aux,
            fontsystem,
            filesystem: FS::new(crate::utils::PWD.to_path_buf()),
            mouth,gullet,stomach
        }
    }
}
 */