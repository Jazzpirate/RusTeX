//! A [`BaseCommand`] is a TeX primitive or macro that can be expanded or processed.
pub mod tex;
pub mod etex;
pub mod pdftex;
pub mod methods;

use std::fmt::{Debug, Formatter};
use std::hint::unreachable_unchecked;
use crate::engine::{EngineMut, EngineType};
use crate::engine::mouth::Mouth;
use crate::engine::state::State;
use crate::engine::state::modes::BoxMode;
use crate::tex::nodes::{HVBox, TeXNode, Whatsit};
use crate::tex::catcodes::CategoryCode;
use crate::tex::commands::methods::{set_primitive_int, set_dim_register, set_int_register, set_muskip_register, set_skip_register, set_toks_register, set_primitive_dim, set_primitive_skip, set_primitive_muskip, set_primitive_toks};
use crate::tex::numbers::{Dimi32, MuSkip, Skip};
use crate::tex::token::{BaseToken, Token};
use crate::throw;
use crate::utils::errors::TeXError;
use crate::utils::Ptr;
use crate::utils::strings::CharType;

pub struct ResolvedToken<ET:EngineType> {
    pub command:BaseCommand<ET>,
    pub source: CommandSource<ET>,
    pub expand:bool
}
impl<ET:EngineType> Debug for ResolvedToken<ET> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.source.cause,f)
    }
}
impl<ET:EngineType> ResolvedToken<ET> {
    pub fn with_expand(self,expand:bool) -> Self {
        Self {
            command:self.command,
            source:self.source,
            expand
        }
    }
}

pub struct StomachCommand<ET:EngineType> {
    pub command:BaseStomachCommand<ET>,
    pub source:CommandSource<ET>
}

impl <ET:EngineType> StomachCommand<ET> {
    pub fn from_resolved(resolved:ResolvedToken<ET>) -> Result<Self,TeXError<ET>> {
        Ok(Self {
            command:BaseStomachCommand::from_base(resolved.command,&resolved.source)?,
            source:resolved.source
        })
    }
}


#[derive(Clone,Debug)]
pub struct CommandSource<ET:EngineType> {
    pub cause:Token<ET>,
    pub reference:Option<ET::CommandReference>
}

#[derive(Clone,Debug)]
pub struct Command<ET:EngineType> {
    pub base:BaseCommand<ET>,
    pub reference:Option<ET::CommandReference>
}
impl<ET:EngineType> Command<ET> {
    pub fn new(base:BaseCommand<ET>,source: Option<&CommandSource<ET>>) -> Command<ET> {
        Command {
            reference:match source {
                Some(r) => Some(ET::CommandReference::new(&base,r)),
                _ => None
            },base
        }
    }
    pub fn copy_with(self,source:&CommandSource<ET>) -> Command<ET> {
        Command {
            reference:Some(ET::CommandReference::new(&self.base,source)),base:self.base
        }
    }
}

pub trait CommandReference<ET:EngineType>:Clone+Debug {
    fn new(base:&BaseCommand<ET>, source:&CommandSource<ET>) -> Self;
}

impl<ET:EngineType<CommandReference = Self>> CommandReference<ET> for () {
    fn new(base: &BaseCommand<ET>, source: &CommandSource<ET>) -> Self { () }
}

pub type TokenCont<'a,ET:EngineType> = &'a mut dyn FnMut(&mut EngineMut<ET>,Token<ET>) -> Result<(),TeXError<ET>>;
pub type UnexpandableFun<ET:EngineType> = fn(&mut EngineMut<ET>, CommandSource<ET>) -> Result<(),TeXError<ET>>;
pub type AssignmentFun<ET:EngineType> = fn(&mut EngineMut<ET>, CommandSource<ET>, bool) -> Result<(),TeXError<ET>>;
pub type AssignmentFn<ET:EngineType> = Box<dyn Fn(&mut EngineMut<ET>, CommandSource<ET>,bool) -> Result<(),TeXError<ET>>>;
pub type ConditionalFun<ET:EngineType> = fn(&mut EngineMut<ET>, CommandSource<ET>) -> Result<bool,TeXError<ET>>;
pub type ExpandableFun<ET:EngineType> = fn(&mut EngineMut<ET>, CommandSource<ET>, TokenCont<ET>) -> Result<(),TeXError<ET>>;
pub type CloseBoxFun<ET:EngineType> = Box<dyn Fn(&mut EngineMut<ET>,Vec<TeXNode<ET>>) -> Option<HVBox<ET>>>;
pub type BoxFun<ET:EngineType> = fn(&mut EngineMut<ET>, CommandSource<ET>) -> Result<CloseBoxFun<ET> ,TeXError<ET>>;
pub type WhatsitFun<ET:EngineType> = fn(&mut EngineMut<ET>, CommandSource<ET>) -> Result<Whatsit<ET> ,TeXError<ET>>;

pub type ValueFun<ET:EngineType,A> = fn(&mut EngineMut<ET>, CommandSource<ET>) -> Result<A,TeXError<ET>>;
pub type FontFun<ET:EngineType> = fn(&mut EngineMut<ET>, CommandSource<ET>) -> Result<ET::Font,TeXError<ET>>;

pub trait Assignable<ET:EngineType> {
    fn get_register(state:&ET::State,index:usize) -> Self;
    fn set_register(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>, index:usize, global:bool) -> Result<(),TeXError<ET>>;
    fn get_primitive(state:&ET::State,name:&'static str) -> Self;
    fn set_primitive(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>, name:&'static str, global:bool) -> Result<(),TeXError<ET>>;
}

impl<ET:EngineType> Assignable<ET> for Vec<Token<ET>> {
    fn get_register(state:&ET::State,index:usize) -> Self {
        state.get_toks_register(index)
    }
    fn set_register(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>, index:usize, global:bool) -> Result<(),TeXError<ET>> {
        set_toks_register(engine,index,cmd,global)
    }
    fn get_primitive(state:&ET::State,name:&'static str) -> Self {
        state.get_primitive_toks(name)
    }
    fn set_primitive(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>, name:&'static str, global:bool) -> Result<(),TeXError<ET>> {
        set_primitive_toks(engine,cmd,name,global)
    }
}
impl<ET:EngineType<Int=i32>> Assignable<ET> for i32 {
    fn get_register(state:&ET::State,index:usize) -> Self {
        state.get_int_register(index)
    }
    fn set_register(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>, index:usize, global:bool) -> Result<(),TeXError<ET>> {
        set_int_register(engine,index,cmd,global)
    }
    fn get_primitive(state:&ET::State,name:&'static str) -> Self {
        state.get_primitive_int(name)
    }
    fn set_primitive(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>, name:&'static str, global:bool) -> Result<(),TeXError<ET>> {
        set_primitive_int(engine,cmd,name,global)
    }
}
impl<ET:EngineType<Dim=Dimi32>> Assignable<ET> for Dimi32 {
    fn get_register(state:&ET::State,index:usize) -> Self {
        state.get_dim_register(index)
    }
    fn set_register(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>, index:usize, global:bool) -> Result<(),TeXError<ET>> {
        set_dim_register(engine,index,cmd,global)
    }
    fn get_primitive(state:&ET::State,name:&'static str) -> Self {
        state.get_primitive_dim(name)
    }
    fn set_primitive(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>, name:&'static str, global:bool) -> Result<(),TeXError<ET>> {
        set_primitive_dim(engine,cmd,name,global)
    }
}
impl<ET:EngineType> Assignable<ET> for Skip<ET::SkipDim> {
    fn get_register(state:&ET::State,index:usize) -> Self {
        state.get_skip_register(index)
    }
    fn set_register(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>, index:usize, global:bool) -> Result<(),TeXError<ET>> {
        set_skip_register(engine,index,cmd,global)
    }
    fn get_primitive(state:&ET::State,name:&'static str) -> Self {
        state.get_primitive_skip(name)
    }
    fn set_primitive(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>, name:&'static str, global:bool) -> Result<(),TeXError<ET>> {
        set_primitive_skip(engine,cmd,name,global)
    }
}
impl<ET:EngineType> Assignable<ET> for MuSkip<ET::MuDim,ET::MuStretchShrinkDim> {
    fn get_register(state:&ET::State,index:usize) -> Self {
        state.get_muskip_register(index)
    }
    fn set_register(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>, index:usize, global:bool) -> Result<(),TeXError<ET>> {
        set_muskip_register(engine,index,cmd,global)
    }
    fn get_primitive(state:&ET::State,name:&'static str) -> Self {
        state.get_primitive_muskip(name)
    }
    fn set_primitive(engine:&mut EngineMut<ET>, cmd:CommandSource<ET>, name:&'static str, global:bool) -> Result<(),TeXError<ET>> {
        set_primitive_muskip(engine,cmd,name,global)
    }
}

#[derive(Clone)]
pub enum ValueCommand<ET:EngineType,A:Assignable<ET>> {
    Register(usize),
    Primitive(&'static str),
    Value{name:&'static str,get:ValueFun<ET,A>},
    Complex{name:&'static str,get:ValueFun<ET,A>,set:AssignmentFun<ET>}
}
impl<ET:EngineType,A:Assignable<ET>> PartialEq for ValueCommand<ET,A> {
    fn eq(&self, other: &Self) -> bool {
        use ValueCommand::*;
        match (self,other) {
            (Register(a),Register(b)) => a == b,
            (Primitive(a),Primitive(b)) => a == b,
            (Value{name:a,..},Value{name:b,..}) => a == b,
            (Complex{name:a,..},Complex{name:b,..}) => a == b,
            _ => false
        }
    }
}
impl<ET:EngineType,A:Assignable<ET>> Debug for ValueCommand<ET,A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ValueCommand::Register(i) => write!(f, "Register {} ", i),
            ValueCommand::Primitive(s) => write!(f, "\\{}", s),
            ValueCommand::Value{name,..} => write!(f, "\\{}", name),
            ValueCommand::Complex{name,..} => write!(f, "\\{}", name)
        }
    }
}
impl<ET:EngineType,A:Assignable<ET>> ValueCommand<ET,A> {
    pub fn get(&self, engine:&mut EngineMut<ET>, cmd:CommandSource<ET>) -> Result<A,TeXError<ET>> {
        match self {
            ValueCommand::Value{get,..} => get(engine, cmd.clone()),
            ValueCommand::Complex{get,..} => get(engine, cmd.clone()),
            ValueCommand::Primitive(name) => Ok(A::get_primitive(engine.state,name)),
            ValueCommand::Register(index) => Ok(A::get_register(engine.state,*index))
        }
    }
    pub fn set(&self, engine:&mut EngineMut<ET>, cmd:CommandSource<ET>, global:bool) -> Result<(),TeXError<ET>> {
        match self {
            ValueCommand::Value{name,..} => throw!("Not allowed to assign to {}",name),
            ValueCommand::Complex{set,..} => set(engine, cmd,global),
            ValueCommand::Primitive(name) => A::set_primitive(engine,cmd,name,global),
            ValueCommand::Register(index) => A::set_register(engine,cmd,*index,global)
        }
    }
}

/// A command to be expanded in the [`crate::engine::gullet::Gullet`]
/// or processed in the [`crate::engine::stomach::Stomach`].
#[derive(Clone)]
pub enum BaseCommand<ET:EngineType>{
    /// A macro defined via `\def`, `\edef`, `\xdef` or `\gdef`
    Def(Def<ET>),
    /// A conditional, e.g. `\ifnum`, `\ifdim`, `\iftrue`, `\iffalse`, returning `true` or `false`
    Conditional{name:&'static str,apply:ConditionalFun<ET>},
    /// An expandable primitive to be processed in the [`Gullet`](crate::engine::gullet::Gullet), e.g.
    /// `\the`, `\number`, `\romannumeral`, `\string`, `\meaning`
    Expandable {name:&'static str,apply:ExpandableFun<ET>},
    /// A primitive command to be processed in the [`Stomach`](crate::engine::stomach::Stomach)
    Unexpandable {name:&'static str,apply: UnexpandableFun<ET>, starts_paragraph:bool},
    /// A primitive assignment command, e.g. `\chardef`, `\let`, etc. - importantly
    /// unlike [`Unexpandable`](BaseCommand::Unexpandable), the `\afterassignment`-[`Token`]
    /// (if existent) is inserted after one of these.
    Assignment{name:&'static str,apply:AssignmentFun<ET>},
    /// A command producing a [`Whatsit`](crate::tex::nodes::Whatsit), executed during shipout or `\immediate`ly
    Whatsit {name:&'static str,apply:WhatsitFun<ET>},
    /// A command opening a new  [`TeXBox`](crate::tex::nodes::CustomBox), e.g. `\hbox`, `\vbox`, `\vtop`, `\vcenter`
    OpenBox{name:&'static str,mode:BoxMode, apply:BoxFun<ET>},
    /// A character; also e.g. the result of `\let\foo=a`
    Char{char:ET::Char,catcode:CategoryCode},
    /// The result of a `\chardef`, e.g. `\chardef\foo=97`
    CharDef(ET::Char),
    /// A math character; the result of e.g. `\mathchardef\sum="1350`
    MathChar(u32),
    /// An (optionally) assignable [`Int`](crate::tex::numbers::Int) value, e.g. `\eTeXversion`, `\catcode`,
    /// or the result of a `\countdef`
    Int(ValueCommand<ET,ET::Int>),
    /// An (optionally) assignable [`Dim`](crate::tex::numbers::Dim) value, e.g. `\hsize` or the result of a `\dimendef`
    Dim(ValueCommand<ET,ET::Dim>),
    /// An (optionally) assignable [`Skip`](crate::tex::numbers::Skip) value, e.g. `\baselineskip` or the result of a `\skipdef`
    Skip(ValueCommand<ET,Skip<ET::SkipDim>>),
    /// An (optionally) assignable [`MuSkip`](crate::tex::numbers::MuSkip) value, e.g. `\thinmuskip` or the result of a `\muskipdef`
    MuSkip(ValueCommand<ET,MuSkip<ET::MuDim,ET::MuStretchShrinkDim>>),
    /// An (optionally) assignable token value, e.g. `\everypar` or the result of a `\toksdef`
    Toks(ValueCommand<ET,Vec<Token<ET>>>),
    /// A [`Font`](crate::tex::fonts::Font)
    Font(ET::Font),
    FontCommand { name:&'static str,get:FontFun<ET>,set:Option<AssignmentFun<ET>> },
    /// `\relax`
    Relax,
    /// None (throws an error in the [`Gullet`](crate::engine::gullet::Gullet))
    None
}

impl<ET:EngineType> PartialEq for BaseCommand<ET> {
    fn eq(&self, other: &Self) -> bool {
        match (self,other) {
            (BaseCommand::Def(a), BaseCommand::Def(b)) => **a == **b,
            (BaseCommand::Conditional{name:a,..}, BaseCommand::Conditional{name:b,..}) => a == b,
            (BaseCommand::Expandable {name:a,..}, BaseCommand::Expandable {name:c,..}) => a == c,
            (BaseCommand::Unexpandable {name:a,..}, BaseCommand::Unexpandable {name:b,..}) => a == b,
            (BaseCommand::Assignment{name:a,..}, BaseCommand::Assignment{name:c,..}) => a == c,
            (BaseCommand::Whatsit{name:a,..}, BaseCommand::Whatsit{name:c,..}) => a == c,
            (BaseCommand::OpenBox {name:a,..}, BaseCommand::OpenBox {name:c,..}) => a == c,
            (BaseCommand::Char{char:a,catcode:CategoryCode::Space}, BaseCommand::Char{char:c,catcode:CategoryCode::Space}) => true,
            (BaseCommand::Char{char:a,catcode:b}, BaseCommand::Char{char:c,catcode:d}) => a == c && b == d,
            (BaseCommand::MathChar(a), BaseCommand::MathChar(b)) => a == b,
            (BaseCommand::Int(a), BaseCommand::Int(b)) => a == b,
            (BaseCommand::Dim(a), BaseCommand::Dim(b)) =>  a == b,
            (BaseCommand::Skip(a), BaseCommand::Skip(b)) =>  a == b,
            (BaseCommand::MuSkip(a), BaseCommand::MuSkip(b)) =>  a == b,
            (BaseCommand::Toks(a), BaseCommand::Toks(b)) =>  a == b,
            (BaseCommand::Font(a), BaseCommand::Font(b)) => a == b,
            (BaseCommand::FontCommand {name:a,..},BaseCommand::FontCommand {name:b,..}) => a == b,
            (BaseCommand::CharDef(a), BaseCommand::CharDef(b)) => a == b,
            (BaseCommand::Relax, BaseCommand::Relax) => true,
            (BaseCommand::None, BaseCommand::None) => true,
            _ => false
        }
    }
}

impl<ET:EngineType> Debug for BaseCommand<ET> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            BaseCommand::Def(def) => <Def<ET> as Debug>::fmt(def,f),
            BaseCommand::Conditional{name,..} => write!(f, "Conditional {}", name),
            BaseCommand::Expandable {name,..} => write!(f, "Gullet Command {}", name),
            BaseCommand::Unexpandable {name,..} => write!(f, "Stomach Command {}", name),
            BaseCommand::Assignment {name,..} => write!(f, "Assignment {}", name),
            BaseCommand::Whatsit {name,..} => write!(f, "Whatsit {}", name),
            BaseCommand::OpenBox {name,..} => write!(f, "Open Box {}", name),
            BaseCommand::Char{char,catcode} => write!(f, "Character '{}' (catcode {})", (char as &ET::Char).char_str(), catcode),
            BaseCommand::CharDef(char) => write!(f, "Character Definition '{}'", (char as &ET::Char).char_str()),
            BaseCommand::MathChar(n) => write!(f, "Math Character {:X}", n),
            BaseCommand::Int(a) => write!(f, "Int {:?}", a),
            BaseCommand::Dim(a) => write!(f, "Dim {:?}", a),
            BaseCommand::Skip(a) => write!(f, "Skip {:?}", a),
            BaseCommand::MuSkip(a) => write!(f, "MuSkip {:?}", a),
            BaseCommand::Toks(a) => write!(f, "Toks {:?}", a),
            BaseCommand::Font(font) => write!(f, "Font {:?}", font),
            BaseCommand::FontCommand {..} => write!(f, "\\font"),
            BaseCommand::Relax => write!(f, "Relax"),
            BaseCommand::None => write!(f, "None"),
        }
    }
}

pub enum BaseStomachCommand<ET:EngineType> {
    Unexpandable {name:&'static str,apply: UnexpandableFun<ET>, starts_paragraph:bool},
    Assignment {name:Option<&'static str>, set:AssignmentFun<ET>},
    Whatsit {name:&'static str,apply:WhatsitFun<ET>},
    ValueAss(AssignmentFn<ET>),
    OpenBox{name:&'static str,mode:BoxMode, apply:BoxFun<ET>},
    Char(ET::Char),
    MathChar(u32),
    Font(ET::Font),
    Relax,
    Space,
    BeginGroup,
    EndGroup,
    MathShift,
    Superscript,
    Subscript
}

impl<ET:EngineType> Debug for BaseStomachCommand<ET> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            BaseStomachCommand::Unexpandable{name,..} => write!(f, "Stomach Command {}", name),
            BaseStomachCommand::Assignment{name,..} => write!(f, "Assignment {}", name.unwrap_or("")),
            BaseStomachCommand::Whatsit{name,..} => write!(f, "Whatsit {}", name),
            BaseStomachCommand::OpenBox{name,..} => write!(f, "Open Box {}", name),
            BaseStomachCommand::Char(ch) => write!(f, "Character {}", ch.char_str()),
            BaseStomachCommand::MathChar(n) => write!(f, "Math Character {:X}", n),
            BaseStomachCommand::ValueAss(_) => write!(f, "Value Assignment"),
            BaseStomachCommand::Relax => write!(f, "Relax"),
            BaseStomachCommand::Space => write!(f, "Space"),
            BaseStomachCommand::Font(fnt) => write!(f, "Font {}",fnt),
            BaseStomachCommand::BeginGroup => write!(f, "BeginGroup"),
            BaseStomachCommand::EndGroup => write!(f, "EndGroup"),
            BaseStomachCommand::MathShift => write!(f, "MathShift"),
            BaseStomachCommand::Superscript => write!(f, "Superscript"),
            BaseStomachCommand::Subscript => write!(f, "Subscript"),
        }
    }
}

impl<ET:EngineType> BaseStomachCommand<ET> {
    fn from_base(value: BaseCommand<ET>,source:&CommandSource<ET>) -> Result<Self,TeXError<ET>> {
        use BaseCommand::*;
        use CategoryCode::*;
        Ok(match value {
            Def(_) | Conditional{..} | Expandable{..} | Int(ValueCommand::Value {..}) | Dim(ValueCommand::Value {..})
            | Skip(ValueCommand::Value {..})| MuSkip(ValueCommand::Value {..}) | Toks(ValueCommand::Value {..})
            | FontCommand{set:std::option::Option::None,..} =>
                todo!(),
            None => match &source.cause.base {
                BaseToken::Char(c,_) => throw!("Undefined active character {}",c),
                BaseToken::CS(name) => throw!("Undefined control sequence {}",name),
            }
            Unexpandable {name,apply,starts_paragraph} => BaseStomachCommand::Unexpandable {name,apply, starts_paragraph},
            Assignment {apply,name,..} => BaseStomachCommand::Assignment {name:Some(name),set:apply},
            Whatsit {name,apply} => BaseStomachCommand::Whatsit {name,apply:apply},
            OpenBox {name,mode,apply} => BaseStomachCommand::OpenBox {name,mode,apply},
            Int(ass) => {
                BaseStomachCommand::ValueAss(Box::new(
                    move |e,c,gl|ass.set(e,c,gl)))
            },
            Dim(ass) => {
                BaseStomachCommand::ValueAss(Box::new(
                    move |e,c,gl|ass.set(e,c,gl)))
            },
            Skip(ass) => {
                BaseStomachCommand::ValueAss(Box::new(
                    move |e,c,gl|ass.set(e,c,gl)))
            },
            MuSkip(ass) => {
                BaseStomachCommand::ValueAss(Box::new(
                    move |e,c,gl|ass.set(e,c,gl)))
            },
            Toks(ass) => {
                BaseStomachCommand::ValueAss(Box::new(
                    move |e,c,gl|ass.set(e,c,gl)))
            },
            FontCommand {set:Some(set),..} => BaseStomachCommand::Assignment {name:std::option::Option::None,set},
            Font(f) => BaseStomachCommand::Font(f),
            Relax => BaseStomachCommand::Relax,
            MathChar(u) => BaseStomachCommand::MathChar(u),
            Char{char,catcode} => match catcode {
                EOF => BaseStomachCommand::Relax,
                BeginGroup => BaseStomachCommand::BeginGroup,
                EndGroup => BaseStomachCommand::EndGroup,
                MathShift => BaseStomachCommand::MathShift,
                AlignmentTab => todo!(),
                Superscript => BaseStomachCommand::Superscript,
                Subscript => BaseStomachCommand::Subscript,
                Space => BaseStomachCommand::Space,
                Letter | Other => BaseStomachCommand::Char(char),
                Parameter => todo!(),
                Ignored | Invalid | Comment | EOL | Escape | Active => unreachable!()
            }
            CharDef(char) => BaseStomachCommand::Char(char)
        })
    }
}


/// A macro defined via `\def`, `\edef`, `\xdef` or `\gdef`
#[derive(Clone)]
pub struct DefI<ET:EngineType>{
    pub protected:bool,
    pub long:bool,
    pub outer:bool,
    pub endswithbrace:bool,
    pub arity:u8,
    pub signature:Vec<ParamToken<ET>>,
    pub replacement:Vec<ExpToken<ET>>
}
impl<ET:EngineType> PartialEq for DefI<ET> {
    fn eq(&self, other: &Self) -> bool {
        self.protected == other.protected && self.long == other.long && self.outer == other.outer &&
            self.endswithbrace == other.endswithbrace && self.arity == other.arity &&
        self.signature == other.signature && self.replacement == other.replacement
    }
}

pub(crate) type Def<ET> = Ptr<DefI<ET>>;
impl<ET:EngineType> DefI<ET>{
    pub fn simple(replacement:Vec<Token<ET>>) -> Def<ET> {
        Ptr::new(Self {
            protected:false,
            long:false,
            outer:false,
            endswithbrace:false,
            arity:0,
            signature:Vec::new(),
            replacement:replacement.into_iter().map(ExpToken::Token).collect()
        })
    }
}
impl<ET:EngineType> Debug for DefI<ET> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for x in &self.signature {
            write!(f,"{:?}",x)?;
        }
        write!(f,"->")?;
        for x in &self.replacement {
            write!(f,"{:?}",x)?;
        }
        Ok(())
    }
}

#[derive(Clone)]
pub enum ParamToken<ET:EngineType> {
    Param,
    Token(Token<ET>)
}
impl<ET:EngineType> PartialEq for ParamToken<ET> {
    fn eq(&self, other: &Self) -> bool {
        match (self,other) {
            (Self::Param,Self::Param) => true,
            (Self::Token(t1),Self::Token(t2)) => t1 == t2,
            _ => false
        }
    }
}

impl<ET:EngineType> Debug for ParamToken<ET> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Param => write!(f,"#i"),
            Self::Token(t) => write!(f,"{}",t)
        }
    }
}

#[derive(Clone)]
pub enum ExpToken<ET:EngineType> {
    Param(Token<ET>,u8),
    Token(Token<ET>),
    ParamToken(Token<ET>)
}
impl<ET:EngineType> PartialEq for ExpToken<ET> {
    fn eq(&self, other: &Self) -> bool {
        match (self,other) {
            (Self::Param(t1,u1),Self::Param(t2,u2)) => t1 == t2 && u1 == u2,
            (Self::Token(t1),Self::Token(t2)) => t1 == t2,
            (Self::ParamToken(t1),Self::ParamToken(t2)) => t1 == t2,
            _ => false
        }
    }
}
impl<ET:EngineType> Debug for ExpToken<ET> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Param(t,u8) => write!(f,"{}{}",t,u8+1),
            Self::Token(t) => write!(f,"{}",t),
            Self::ParamToken(t) => write!(f,"{}{}",t,t)
        }
    }
}