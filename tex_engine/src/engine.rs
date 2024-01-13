/*! A TeX engine combines all the necessary components into a struct capable of compiling a TeX file into
    some output format.
*/
use std::fmt::Debug;
use crate::engine::gullet::{DefaultGullet, Gullet};
use crate::engine::utils::memory::{MemoryManager, PRIMITIVES};
use crate::engine::mouth::{DefaultMouth, Mouth};
use crate::engine::state::State;
use crate::engine::stomach::{Stomach, StomachWithShipout};
use crate::{tex};
use crate::tex::catcodes::CommandCode;
use crate::commands::{Command, ResolvedToken};
use crate::engine::filesystem::{File, FileSystem, VirtualFile};
use crate::engine::fontsystem::{Font, FontSystem, TfmFont, TfmFontSystem};
use crate::engine::utils::outputs::{LogOutputs, Outputs};
use crate::tex::characters::Character;
use crate::tex::tokens::control_sequences::{CSName, InternedCSName,CSHandler};
use crate::tex::nodes::CustomNodeTrait;
use crate::tex::nodes::vertical::VNode;
use crate::tex::numerics::{Dim32, MuSkip, MuSkip32, Numeric, NumSet, Skip, Skip32, TeXDimen, TeXInt};
use crate::tex::tokens::Token;
use crate::utils::errors::{ErrorHandler, ErrorThrower, TeXError};

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
pub trait EngineTypes:Sized+Copy+Clone+Debug+'static {
    type Char: Character;
    type CSName: CSName<Self::Char>;
    type Token: Token<Char = Self::Char, CS = Self::CSName>;
    type Extension: EngineExtension<Self>;
    type File: File<Char=Self::Char>;
    type FileSystem: FileSystem<File=Self::File>;
    type Int:TeXInt;
    type Dim:TeXDimen + Numeric<Self::Int>;
    type Skip:Skip<Base = Self::Dim> + Numeric<Self::Int>;
    type MuSkip:MuSkip + Numeric<Self::Int>;
    type Num: crate::tex::numerics::NumSet<Int = Self::Int,Dim=Self::Dim,Skip=Self::Skip,MuSkip=Self::MuSkip>;
    type State: State<ET=Self>;
    type Outputs: Outputs;
    type Mouth: Mouth<Self>;
    type Gullet:Gullet<Self>;
    type Stomach:Stomach<ET=Self>;
    type CustomNode:CustomNodeTrait<Self>;
    type Font:Font<Char=Self::Char,Int=Self::Int, Dim=Self::Dim,CS=Self::CSName>;
    type FontSystem: FontSystem<Font=Self::Font,Char=Self::Char,Int=Self::Int,Dim=Self::Dim,CS=Self::CSName>;
}
pub struct EngineAux<ET:EngineTypes> {
    pub memory:MemoryManager<ET::Token>,
    pub error_handler:Box<dyn ErrorHandler<ET>>,
    pub outputs:ET::Outputs,
    pub start_time:chrono::DateTime<chrono::Local>,
    pub jobname:String,
    pub extension:ET::Extension
}

pub struct Colon<'c,ET:EngineTypes> {
    out:Box<dyn FnMut(&mut EngineReferences<ET>, VNode<ET>) + 'c>
}
impl<'c,ET:EngineTypes> Colon<'c,ET> {

    pub fn new<F:FnMut(&mut EngineReferences<ET>, VNode<ET>) + 'c>(f:F) -> Self {
        Colon { out:Box::new(f) }
    }

    pub fn out(&mut self,engine:&mut EngineReferences<ET>, n: VNode<ET>) {
        (self.out)(engine,n)
    }
}
impl <'c,ET:EngineTypes> Default for Colon<'c,ET> {

    fn default() -> Self {
        Colon { out:Box::new(|_,_|{}) }
    }
}

impl <ET:EngineTypes> EngineReferences<'_,ET> {
    pub fn shipout(&mut self,n: VNode<ET>) {
        let mut colon = std::mem::take(&mut self.colon);
        colon.out(self,n);
        self.colon = colon;
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
    type CSName = InternedCSName<u8>;//InternedString;
    type Token = super::tex::tokens::CompactToken;//::StandardToken<Self::CSName,u8>;//
    type Extension = ();
    type Int = i32;
    type Dim = Dim32;
    type Skip = Skip32<Dim32>;
    type MuSkip = MuSkip32;
    type Num = tex::numerics::DefaultNumSet;
    type State = state::tex_state::TeXState<Self>;
    type File = VirtualFile<u8>;
    type FileSystem = filesystem::NoOutputFileSystem<u8>;
    type Outputs = LogOutputs;
    type Mouth = DefaultMouth<Self>;
    type Gullet = DefaultGullet<Self>;
    type CustomNode = ();
    type Stomach = StomachWithShipout<Self>;
    type Font = TfmFont<i32,Dim32,InternedCSName<u8>>;
    type FontSystem = TfmFontSystem<i32,Dim32,InternedCSName<u8>>;//InternedString>;
}

pub trait TeXEngine:Sized {
    type Types:EngineTypes;
    fn register_primitive(&mut self,cmd:Command<Self::Types>,name:&str);
    fn get_engine_refs(&mut self) -> EngineReferences<Self::Types>;
    fn init_file(&mut self,s:&str) -> Result<(),TeXError> {TeXError::catch(|| {
        log::debug!("Initializing with file {}",s);
        let mut comps = self.get_engine_refs();
        let file = comps.filesystem.get(s);
        comps.aux.jobname = file.path().with_extension("").file_name().unwrap().to_str().unwrap().to_string();
        comps.push_file(file);
        comps.aux.start_time = chrono::Local::now();
        comps.top_loop();
    })}
    fn do_file_default<F:FnMut(&mut EngineReferences<Self::Types>, VNode<Self::Types>)>(&mut self, s:&str, f:F) -> Result<(),TeXError> {TeXError::catch(||{
        log::debug!("Running file {}",s);
        let mut comps = self.get_engine_refs();
        let file = comps.filesystem.get(s);
        comps.filesystem.set_pwd(file.path().parent().unwrap().to_path_buf());
        comps.aux.jobname = file.path().with_extension("").file_name().unwrap().to_str().unwrap().to_string();
        comps.push_file(file);
        comps.push_every(PRIMITIVES.everyjob);
        comps.aux.start_time = chrono::Local::now();
        //comps.aux.elapsed = std::time::Instant::now();
        //debug_log!(debug =>"Here: {}",comps.preview());
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
        let mut memory = MemoryManager::new();
        let mut aux = EngineAux {
            outputs: ET::Outputs::new(),
            error_handler: ErrorThrower::new(),
            start_time:chrono::Local::now(),
            extension: ET::Extension::new(&mut memory),
            memory,
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

/** Additional components we want to add to a [`EngineReferences`] can be implemented here.
    Notably, `()` extends this trait if we don't need any additional components.
 */
pub trait EngineExtension<ET:EngineTypes> {
    fn new(memory:&mut MemoryManager<ET::Token>) -> Self;
}
impl<ET:EngineTypes<Extension=()>> EngineExtension<ET> for () {
    fn new(_memory:&mut MemoryManager<ET::Token>) -> Self { () }
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
        crate::expand_loop!(ET::Stomach::every_top(self) => self,
            ResolvedToken::Tk { code:CommandCode::Noexpand,.. } => {self.get_next();},
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
        let cmd = <<$ET as EngineTypes>::Gullet as crate::engine::gullet::Gullet<$ET>>::resolve(
            &$engine.state,$tk
        );
        match cmd {
            crate::commands::ResolvedToken::Cmd{cmd: Some(crate::commands::Command::Macro(m)),token} =>
                <<$ET as EngineTypes>::Gullet as crate::engine::gullet::Gullet<$ET>>::do_macro($engine,m.clone(),token),
            crate::commands::ResolvedToken::Cmd{cmd: Some(crate::commands::Command::Conditional(cond)),token} =>
                <<$ET as EngineTypes>::Gullet as crate::engine::gullet::Gullet<$ET>>::do_conditional($engine,cond.name,token,cond.expand,false),
            crate::commands::ResolvedToken::Cmd{cmd: Some(crate::commands::Command::Expandable(e)),token} =>
                <<$ET as EngineTypes>::Gullet as crate::engine::gullet::Gullet<$ET>>::do_expandable($engine,e.name,token,e.expand),
            crate::commands::ResolvedToken::Cmd{cmd: Some(crate::commands::Command::SimpleExpandable(e)),token} =>
                <<$ET as EngineTypes>::Gullet as crate::engine::gullet::Gullet<$ET>>::do_simple_expandable($engine,e.name,token,e.expand),
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
            crate::commands::Command::Unexpandable(crate::commands::Unexpandable { name,scope, apply }) =>
                <$ET as EngineTypes>::Stomach::do_unexpandable($engine, *name, *scope,$token, *apply),
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
            crate::commands::Command::MathChar(u) =>
                <$ET as EngineTypes>::Stomach::do_mathchar($engine,*u,$token),
            crate::commands::Command::Relax => (),
            crate::commands::Command::Int(crate::commands::IntCommand { .. }) |
            crate::commands::Command::Dim(crate::commands::DimCommand { .. }) |
            crate::commands::Command::Skip(crate::commands::SkipCommand { .. }) |
            crate::commands::Command::MuSkip(crate::commands::MuSkipCommand { .. }) |
            crate::commands::Command::FontCmd(crate::commands::FontCommand { .. }) =>
                crate::engine::do_error($engine,$cmd.clone()),
            crate::commands::Command::Macro(_) | Command::Conditional(_) | Command::Expandable(_)  | Command::SimpleExpandable(_) | Command::Char {..} => unreachable!(),
        }
    }
}

pub fn do_error<ET:EngineTypes>(engine:&mut EngineReferences<ET>,cmd:Command<ET>) {
    todo!("Not allowed in {:?} mode: {}",engine.stomach.data_mut().mode(),cmd.meaning(engine.aux.memory.cs_interner(),engine.state.get_catcode_scheme(),engine.state.get_escape_char()))
}