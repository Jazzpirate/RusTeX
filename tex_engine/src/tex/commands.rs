//! A [`Command`] is a TeX primitive or macro that can be expanded or processed.
pub mod tex;
pub mod etex;
pub mod pdftex;
pub mod methods;

use std::fmt::{Debug, Formatter};
use crate::engine::mouth::Mouth;
use crate::engine::state::State;
use crate::tex::catcodes::CategoryCode;
use crate::tex::token::Token;
use crate::utils::errors::TeXError;
use crate::utils::Ptr;
use crate::utils::strings::CharType;

/// A command to be expanded in the [`crate::engine::gullet::Gullet`]
/// or processed in the [`crate::engine::stomach::Stomach`].
#[derive(Clone,PartialEq)]
pub enum Command<T:Token>{
    /// A macro defined via `\def`, `\edef`, `\xdef` or `\gdef`
    Def(Def<T>),
    /// A primitive command to be processed in the [`crate::engine::stomach::Stomach`]
    Stomach{name:&'static str,index:usize},

    /// A primitive assignable value, e.g. `\tolerance` ([`Int`](Assignable::Int)),
    /// `\hsize` ([`Dim`](Assignable::Dim))
    AssignableValue{name:&'static str,tp:Assignable},
    /// A primitive non-assignable [`crate::tex::numbers::Int`] value, e.g. `\eTeXversion`
    Value{name:&'static str,index:usize,tp:Assignable},
    /// An value register, e.g. resulting from `\countdef`, `\dimdef` etc.
    ValueRegister{index:usize,tp:Assignable},
    /// Assignable, potentially parametric value requiring parsing; e.g. `\catcode`,
    /// `\count`, etc.
    ValueAssignment{name:&'static str,assignment_index:usize,value_index:usize,tp:Assignable},
    /// A primitive assignment command, e.g. `\chardef`, `\let`, etc.
    Assignment{name:&'static str,index:usize},
    /// A conditional, e.g. `\ifnum`, `\ifdim`, `\iftrue`, `\iffalse`, returning `true` or `false`
    Conditional{name:&'static str,index:usize},
    /// An expandable primitive to be processed in the [`crate::engine::gullet::Gullet`], e.g.
    /// `\the`, `\number`, `\romannumeral`, `\string`, `\meaning`
    Gullet{name:&'static str,index:usize},
    /// A character; the result of e.g. `\let\foo=a`
    Char{char:T::Char,catcode:CategoryCode},
    /// A command producing a [`Whatsit`], executed during shipout or `\immediate`ly
    Whatsit {name:&'static str,index:usize},
    /// `\relax`
    Relax
    // ...
}

#[derive(Copy,Clone,Debug,PartialEq)]
pub enum Assignable {
    Int,Dim,Glue,MuGlue,Font,Toks
}

impl<T:Token> Debug for Command<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Command::Def(def) => <Def<T> as Debug>::fmt(def,f),
            Command::ValueRegister{index,tp} => write!(f, "{:?} register {}",tp, index),
            Command::Relax => write!(f, "Relax"),
            Command::Stomach {name,..} => write!(f, "Stomach Command {}", name),
            Command::ValueAssignment {name,tp,..} => write!(f, "{:?} Assignment {}", tp, name),
            Command::Assignment {name,..} => write!(f, "Assignment {}", name),
            Command::AssignableValue {name,tp,..} => write!(f, "{:?} Assignment {}",tp,name),
            Command::Conditional{name,..} => write!(f, "Conditional {}", name),
            Command::Gullet{name,..} => write!(f, "Gullet Command {}", name),
            Command::Char{char,catcode} => write!(f, "Character '{}' (catcode {})", (char as &T::Char).char_str(), catcode),
            Command::Value {name,tp,..} => write!(f, "{:?} Command {}",tp, name),
            Command::Whatsit {name,index} => write!(f,"Whatsit {}",name),
        }
    }
}


#[derive(Debug,Clone)]
pub struct StomachCommand<T:Token>{pub cause:T,pub cmd:StomachCommandInner<T::Char>}
#[derive(Clone)]
pub enum StomachCommandInner<C:CharType> {
    Command{name:&'static str,index:usize},
    AssignableValue{name:&'static str,tp:Assignable},
    Value{name:&'static str,index:usize,tp:Assignable},
    ValueRegister(usize,Assignable),
    ValueAssignment{name:&'static str,assignment_index:usize,value_index:usize,tp:Assignable},
    Assignment {name:&'static str,index:usize},
    Whatsit {name:&'static str,index:usize},
    Relax,
    Char(C,bool),
    Superscript,
    Subscript,
    Space,
    MathShift,
    BeginGroup,
    EndGroup
}

#[derive(Debug,Clone)]
pub struct GulletCommand<T:Token>{pub cause:T}

impl<C:CharType> Debug for StomachCommandInner<C> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            StomachCommandInner::Relax => write!(f,"Relax"),
            StomachCommandInner::Command{name,..} => write!(f,"\\{}",name),
            StomachCommandInner::ValueAssignment {name,..} => write!(f, "\\{}", name),
            StomachCommandInner::Assignment {name,..} => write!(f, "\\{}", name),
            StomachCommandInner::Value{name,tp,..} => write!(f,"\\{}",name),
            StomachCommandInner::AssignableValue {name,tp,..} => write!(f, "{:?} Assignment {}",tp,name),
            StomachCommandInner::ValueRegister(u,tp) => write!(f, "{:?} register {}",tp, u),
            StomachCommandInner::Whatsit {name,index} => write!(f,"Whatsit {}",name),
            StomachCommandInner::Char(c,_) => write!(f,"Character '{}'",c),
            StomachCommandInner::Superscript => write!(f,"Superscript Token"),
            StomachCommandInner::Subscript => write!(f,"Subscript Token"),
            StomachCommandInner::Space => write!(f,"Space Token"),
            StomachCommandInner::MathShift => write!(f,"MathShift Token"),
            StomachCommandInner::BeginGroup => write!(f,"BeginGroup Token"),
            StomachCommandInner::EndGroup => write!(f,"EndGroup Token")
        }
    }
}

/// A macro defined via `\def`, `\edef`, `\xdef` or `\gdef`
#[derive(PartialEq,Clone)]
pub struct Def<T:Token>{
    pub protected:bool,
    pub long:bool,
    pub outer:bool,
    pub endswithbrace:bool,
    pub arity:u8,
    pub signature:Vec<ParamToken<T>>,
    pub replacement:Vec<ExpToken<T>>
}
impl<T:Token> Def<T>{
    /// Expands the [`Def`] into a [`Vec`] of [`Token`]s. `cmd` and `cause` are the command and
    /// token that triggered the expansion, used for constructing the
    /// [`SourceReference`](crate::tex::token::SourceReference)s of the returned [`Token`]s and
    /// error messages.
    pub fn expand<M:Mouth<T>,S:State<T>>(&self, state:&S, mouth:&mut M, cmd:Ptr<Command<T>>, cause:Ptr<T>) -> Result<Vec<T>,Box<dyn TeXError<T>>> {
        methods::exand_def(self,state,mouth,cmd,cause)
    }
}
impl<T:Token> Debug for Def<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f,"{:?}{{{:?}}}",self.signature,self.replacement)
    }
}

#[derive(Clone,PartialEq)]
pub enum ParamToken<T:Token> {
    Param,
    Token(T)
}
impl<T:Token> Debug for ParamToken<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Param => write!(f,"#i"),
            Self::Token(t) => write!(f,"{}",t)
        }
    }
}

#[derive(Clone,PartialEq)]
pub enum ExpToken<T:Token> {
    Param(T,u8),
    Token(T),
    ParamToken(T)
}
impl<T:Token> Debug for ExpToken<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Param(t,u8) => write!(f,"{}{}",t,u8),
            Self::Token(t) => write!(f,"{}",t),
            Self::ParamToken(t) => write!(f,"{}{}",t,t)
        }
    }
}