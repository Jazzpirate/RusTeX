/*! A TeX engine combines all the necessary components into a struct capable of compiling a TeX file into
    some output format.
*/
use std::fmt::Debug;
use crate::engine::gullet::{DefaultGullet, Gullet};
use crate::engine::utils::memory::{MemoryManager};
use crate::engine::mouth::{DefaultMouth, Mouth};
use crate::engine::state::State;
use crate::engine::stomach::{Stomach, DefaultStomach};
use crate::{tex};
use crate::tex::catcodes::CommandCode;
use crate::commands::{TeXCommand, ResolvedToken};
use crate::commands::primitives::PRIMITIVES;
use crate::engine::filesystem::{File, FileSystem, VirtualFile};
use crate::engine::fontsystem::{Font, FontSystem, TfmFont, TfmFontSystem};
use crate::engine::utils::outputs::{LogOutputs, Outputs};
use crate::tex::characters::Character;
use crate::tex::tokens::control_sequences::{CSName, InternedCSName};
use crate::tex::nodes::CustomNodeTrait;
use crate::tex::nodes::vertical::VNode;
use crate::tex::numerics::{Dim32, Mu, MuDim, Numeric, NumSet, TeXDimen, TeXInt};
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
    type MuDim:MuDim + Numeric<Self::Int>;
    type Num: crate::tex::numerics::NumSet<Int = Self::Int,Dim=Self::Dim,MuDim=Self::MuDim>;
    type State: State<Self>;
    type Outputs: Outputs;
    type Mouth: Mouth<Self>;
    type Gullet:Gullet<Self>;
    type Stomach:Stomach<Self>;
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
    type MuDim = Mu;
    type Num = tex::numerics::DefaultNumSet;
    type State = state::tex_state::DefaultState<Self>;
    type File = VirtualFile<u8>;
    type FileSystem = filesystem::NoOutputFileSystem<u8>;
    type Outputs = LogOutputs;
    type Mouth = DefaultMouth<Self>;
    type Gullet = DefaultGullet<Self>;
    type CustomNode = ();
    type Stomach = DefaultStomach<Self>;
    type Font = TfmFont<i32,Dim32,InternedCSName<u8>>;
    type FontSystem = TfmFontSystem<i32,Dim32,InternedCSName<u8>>;//InternedString>;
}

pub trait TeXEngine:Sized {
    type Types:EngineTypes;
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
        crate::expand_loop!(ET::Stomach::every_top(self) => token => {
            if token.is_primitive() == Some(PRIMITIVES.noexpand) {self.get_next();continue}
        }; self,
            ResolvedToken::Tk { char, code } => ET::Stomach::do_char(self, token, char, code),
            ResolvedToken::Cmd(Some(TeXCommand::Char {char, code})) => ET::Stomach::do_char(self, token, *char, *code),
            ResolvedToken::Cmd(None) => match self.aux.error_handler.undefined(self.aux.memory.cs_interner(),self.state,token) {
                Some(txt) => self.mouth.push_string(txt),
                _ => ()
            }
            ResolvedToken::Cmd(Some(cmd)) => crate::do_cmd!(self,token,cmd)
        );
    }

    pub fn expand_until_bgroup(&mut self,allow_let:bool,t:&ET::Token) {
        while let Some(tk) = self.get_next() {
            if tk.command_code() == CommandCode::BeginGroup {return }
            crate::expand!(self,tk;
                ResolvedToken::Cmd(Some(TeXCommand::Char {code:CommandCode::BeginGroup,..})) if allow_let =>
                    return,
                _ => todo!("error")
            );
        }
        match self.aux.error_handler.missing_begingroup(self.aux.memory.cs_interner(),self.state,t.clone()) {
            Some(txt) => self.mouth.push_string(txt),
            _ => ()
        }
    }
}

#[macro_export]
macro_rules! expand_loop {
    ($engine:ident,$tk:ident,$($case:tt)*) => {{
        crate::expand_loop!(ET;$engine,$tk,$($case)*)
    }};
    ($then:expr => $engine:ident,$tk:ident,$($case:tt)*) => {{
        $then;
        while let Some($tk) = $engine.get_next() {
            crate::expand!(ET;$engine,$tk;$($case)*);
            $then;
        }
    }};
    ($then:expr => $tk:ident => $first:expr; $engine:ident,$($case:tt)*) => {{
        $then;
        while let Some($tk) = $engine.get_next() {
            $first;
            crate::expand!(ET;$engine,$tk;$($case)*);
            $then;
        }
    }};
    ($ET:ty; $engine:ident,$tk:ident,$($case:tt)*) => {{
        while let Some($tk) = $engine.get_next() {
            crate::expand!($ET;$engine,$tk;$($case)*);
        }
    }};
    ($ET:ty; $tk:ident => $first:expr; $engine:ident,$($case:tt)*) => {{
        while let Some($tk) = $engine.get_next() {
            $first;
            crate::expand!($ET;$engine,$tk;$($case)*);
        }
    }}
}

#[macro_export]
macro_rules! expand {
    ($engine:ident,$tk:expr;$($case:tt)*) => {
        crate::expand!(ET;$engine,$tk;$($case)*)
    };
    ($ET:ty; $engine:ident,$token:expr;$($case:tt)*) => {
        let cmd = <<$ET as EngineTypes>::Gullet as crate::engine::gullet::Gullet<$ET>>::resolve($engine.state,&$token);
        match cmd {
            crate::commands::ResolvedToken::Cmd(Some(crate::commands::TeXCommand::Macro(m))) =>
                <<$ET as EngineTypes>::Gullet as crate::engine::gullet::Gullet<$ET>>::do_macro($engine,m.clone(),$token),
            crate::commands::ResolvedToken::Cmd(Some(crate::commands::TeXCommand::Primitive{name,cmd:crate::commands::PrimitiveCommand::Conditional(cond)})) =>
                <<$ET as EngineTypes>::Gullet as crate::engine::gullet::Gullet<$ET>>::do_conditional($engine,*name,$token,*cond,false),
            crate::commands::ResolvedToken::Cmd(Some(crate::commands::TeXCommand::Primitive{name,cmd:crate::commands::PrimitiveCommand::Expandable(e)})) =>
                <<$ET as EngineTypes>::Gullet as crate::engine::gullet::Gullet<$ET>>::do_expandable($engine,*name,$token,*e),
            crate::commands::ResolvedToken::Cmd(Some(crate::commands::TeXCommand::Primitive{name,cmd:crate::commands::PrimitiveCommand::SimpleExpandable(e)})) =>
                <<$ET as EngineTypes>::Gullet as crate::engine::gullet::Gullet<$ET>>::do_simple_expandable($engine,*name,$token,*e),
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
            crate::commands::TeXCommand::CharDef(char)  => <$ET as EngineTypes>::Stomach::do_char($engine, $token, *char, CommandCode::Other),
            crate::commands::TeXCommand::Primitive{name,cmd:crate::commands::PrimitiveCommand::Unexpandable { scope, apply }} =>
                <$ET as EngineTypes>::Stomach::do_unexpandable($engine, *name, *scope,$token, *apply),
            crate::commands::TeXCommand::Primitive{name,cmd:crate::commands::PrimitiveCommand::Assignment(assign)} =>
                <$ET as EngineTypes>::Stomach::do_assignment($engine, *name, $token, *assign,false),
            crate::commands::TeXCommand::Primitive{name,cmd:crate::commands::PrimitiveCommand::Int {  assign: Some(assign), .. }} =>
                <$ET as EngineTypes>::Stomach::do_assignment($engine, *name, $token, *assign,false),
            crate::commands::TeXCommand::Primitive{name,cmd:crate::commands::PrimitiveCommand::Dim { assign: Some(assign), .. }} =>
                <$ET as EngineTypes>::Stomach::do_assignment($engine, *name, $token, *assign,false),
            crate::commands::TeXCommand::Primitive{name,cmd:crate::commands::PrimitiveCommand::Skip { assign: Some(assign), .. }} =>
                <$ET as EngineTypes>::Stomach::do_assignment($engine, *name, $token, *assign,false),
            crate::commands::TeXCommand::Primitive{name,cmd:crate::commands::PrimitiveCommand::MuSkip { assign: Some(assign), .. }} =>
                <$ET as EngineTypes>::Stomach::do_assignment($engine, *name, $token, *assign,false),
            crate::commands::TeXCommand::Primitive{name,cmd:crate::commands::PrimitiveCommand::FontCmd { assign: Some(assign), .. }} =>
                <$ET as EngineTypes>::Stomach::do_assignment($engine, *name, $token, *assign,false),
            crate::commands::TeXCommand::Primitive{name,cmd:crate::commands::PrimitiveCommand::Box(read)} =>
                <$ET as EngineTypes>::Stomach::do_box($engine, *name, $token, *read),
            crate::commands::TeXCommand::Font(f) =>
                <$ET as EngineTypes>::Stomach::assign_font($engine, $token, f.clone(),false),
            crate::commands::TeXCommand::IntRegister(u) =>
                <$ET as EngineTypes>::Stomach::assign_int_register($engine, *u,false),
            crate::commands::TeXCommand::DimRegister(u) =>
                <$ET as EngineTypes>::Stomach::assign_dim_register($engine, *u,false),
            crate::commands::TeXCommand::SkipRegister(u) =>
                <$ET as EngineTypes>::Stomach::assign_skip_register($engine, *u,false),
            crate::commands::TeXCommand::MuSkipRegister(u) =>
               <$ET as EngineTypes>::Stomach::assign_muskip_register($engine, *u,false),
            crate::commands::TeXCommand::ToksRegister(u) =>
                <$ET as EngineTypes>::Stomach::assign_toks_register($engine, *u,false),
            crate::commands::TeXCommand::Primitive{name,cmd:crate::commands::PrimitiveCommand::Whatsit { get, .. }} =>
                <$ET as EngineTypes>::Stomach::do_whatsit($engine, *name,$token, *get),
            crate::commands::TeXCommand::Primitive{name,cmd:crate::commands::PrimitiveCommand::PrimitiveInt} =>
                <$ET as EngineTypes>::Stomach::assign_primitive_int($engine,*name,false),
            crate::commands::TeXCommand::Primitive{name,cmd:crate::commands::PrimitiveCommand::PrimitiveDim} =>
                <$ET as EngineTypes>::Stomach::assign_primitive_dim($engine,*name,false),
            crate::commands::TeXCommand::Primitive{name,cmd:crate::commands::PrimitiveCommand::PrimitiveSkip} =>
                <$ET as EngineTypes>::Stomach::assign_primitive_skip($engine,*name,false),
            crate::commands::TeXCommand::Primitive{name,cmd:crate::commands::PrimitiveCommand::PrimitiveMuSkip} =>
                <$ET as EngineTypes>::Stomach::assign_primitive_muskip($engine,*name,false),
            crate::commands::TeXCommand::Primitive{name,cmd:crate::commands::PrimitiveCommand::PrimitiveToks} =>
                <$ET as EngineTypes>::Stomach::assign_primitive_toks($engine,*name,false),
            crate::commands::TeXCommand::MathChar(u) =>
                <$ET as EngineTypes>::Stomach::do_mathchar($engine,*u,Some($token)),
            crate::commands::TeXCommand::Primitive{cmd:crate::commands::PrimitiveCommand::Relax,..} => (),
            crate::commands::TeXCommand::Primitive{cmd:crate::commands::PrimitiveCommand::Int { .. },name} |
            crate::commands::TeXCommand::Primitive{cmd:crate::commands::PrimitiveCommand::Dim { .. },name} |
            crate::commands::TeXCommand::Primitive{cmd:crate::commands::PrimitiveCommand::Skip { .. },name} |
            crate::commands::TeXCommand::Primitive{cmd:crate::commands::PrimitiveCommand::MuSkip { .. },name} |
            crate::commands::TeXCommand::Primitive{cmd:crate::commands::PrimitiveCommand::FontCmd { .. },name} =>
                match $engine.aux.error_handler.not_allowed_in_mode($engine.aux.memory.cs_interner(),$engine.state,$engine.stomach,*name,$token) {
                Some(s) => $engine.mouth.push_string(s),
                _ => ()
            }
            crate::commands::TeXCommand::Macro(_) |
            crate::commands::TeXCommand::Primitive{ cmd:crate::commands::PrimitiveCommand::Conditional { .. } |
                crate::commands::PrimitiveCommand::Expandable { .. } |
                crate::commands::PrimitiveCommand::SimpleExpandable { .. },..
            } | TeXCommand::Char {..} => unreachable!(),
        }
    }
}
