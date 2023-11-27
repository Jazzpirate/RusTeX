/*! Commands - primitives, macros, etc. The B-book largely calls these
"equivalents", but we use the more standard term "command" instead.
*/

use std::fmt::Display;
use crate::engine::{EngineReferences, EngineTypes};
use crate::engine::fontsystem::FontSystem;
use crate::engine::utils::memory::{PrimitiveIdentifier, PRIMITIVES};
use crate::engine::mouth::pretokenized::{ExpansionContainer, TokenList};
use crate::tex::catcodes::{CategoryCodeScheme, CommandCode};
use crate::tex::control_sequences::{ControlSequenceName, ControlSequenceNameHandler};
use crate::tex::numerics::NumSet;
use crate::tex::token::Token;
use crate::utils::Ptr;
use crate::tex::input_text::Character;
use crate::engine::fontsystem::Font;
use crate::tex::nodes::{BoxInfo, NodeList, TeXBox};

pub mod primitives;
pub mod tex;
pub mod etex;
pub mod pdftex;

/// A command.
#[derive(Clone,Debug)]
pub enum Command<ET:EngineTypes> {
    /// A user defined [`Macro`], to be expanded (unless [protected](Macro::protected))
    Macro(Macro<ET::Token>),
    /// A conditional, e.g. `\ifnum`, `\ifx`, etc.
    Conditional(Conditional<ET>),
    /// An expandable primitive, e.g. `\the`, `\number`, etc.
    Expandable(Expandable<ET>),
    SimpleExpandable(SimpleExpandable<ET>),
    /// A primitive that cannot be expanded, e.g. `\relax`, `\end`, etc.
    Unexpandable(Unexpandable<ET>),
    Assignment(Assignment<ET>),
    Char{char:ET::Char,code:CommandCode},
    CharDef(ET::Char),
    MathChar(u32),
    Int(IntCommand<ET>),
    Dim(DimCommand<ET>),
    Skip(SkipCommand<ET>),
    MuSkip(MuSkipCommand<ET>),
    FontCmd(FontCommand<ET>),
    Font(<ET::FontSystem as FontSystem>::Font),
    IntRegister(u16),
    DimRegister(u16),
    SkipRegister(u16),
    MuSkipRegister(u16),
    ToksRegister(u16),
    Box(BoxCommand<ET>),
    PrimitiveInt(PrimitiveIdentifier),
    PrimitiveDim(PrimitiveIdentifier),
    PrimitiveSkip(PrimitiveIdentifier),
    PrimitiveMuSkip(PrimitiveIdentifier),
    PrimitiveToks(PrimitiveIdentifier),
    Whatsit(Whatsit<ET>),
    Relax
}
impl<ET:EngineTypes> Command<ET> {
    #[inline(always)]
    pub fn meaning<'a>(&'a self,int:&'a <<ET::Token as Token>::CS as ControlSequenceName>::Handler,cc:&'a CategoryCodeScheme<ET::Char>,escapechar:Option<ET::Char>) -> Meaning<'a,ET> {
        Meaning{cmd:self,int,cc,escapechar}
    }
}

pub struct Meaning<'a,ET:EngineTypes>{
    cmd:&'a Command<ET>,
    int:&'a <<ET::Token as Token>::CS as ControlSequenceName>::Handler,
    cc:&'a CategoryCodeScheme<ET::Char>,
    escapechar:Option<ET::Char>
}
impl<'a,ET:EngineTypes> Display for Meaning<'a,ET> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.cmd {
            Command::Macro(m) => m.meaning_fmt(self.int, self.cc, self.escapechar, f),
            Command::Conditional(c) => write!(f,"{}",PRIMITIVES.printable(c.name,self.escapechar)),
            Command::Expandable(e) => write!(f,"{}",PRIMITIVES.printable(e.name,self.escapechar)),
            Command::SimpleExpandable(e) => write!(f,"{}",PRIMITIVES.printable(e.name,self.escapechar)),
            Command::Unexpandable(u) => write!(f,"{}",PRIMITIVES.printable(u.name,self.escapechar)),
            Command::Assignment(a) => write!(f,"{}",PRIMITIVES.printable(a.name,self.escapechar)),
            Command::Char{char,code} => match code {
                CommandCode::BeginGroup => write!(f,"begin-group character {}",char.displayable()),
                CommandCode::EndGroup => write!(f,"end-group character {}",char.displayable()),
                CommandCode::MathShift => write!(f,"math shift character {}",char.displayable()),
                CommandCode::Parameter => write!(f,"macro parameter character {}",char.displayable()),
                CommandCode::Superscript => write!(f,"superscript character {}",char.displayable()),
                CommandCode::Subscript => write!(f,"subscript character {}",char.displayable()),
                CommandCode::Space => write!(f,"blank space  "),
                CommandCode::Letter => write!(f,"the letter {}",char.displayable()),
                _ => write!(f,"the character {}",char.displayable()),
            },
            Command::CharDef(c) => write!(f,"{}char\"{:X}",ET::Char::displayable_opt(self.escapechar),Into::<u64>::into(*c)),
            Command::MathChar(u) => write!(f,"{}mathchar\"{:X}",ET::Char::displayable_opt(self.escapechar),u),
            Command::Int(i) => write!(f,"{}",PRIMITIVES.printable(i.name,self.escapechar)),
            Command::Dim(i) => write!(f,"{}",PRIMITIVES.printable(i.name,self.escapechar)),
            Command::Skip(i) => write!(f,"{}",PRIMITIVES.printable(i.name,self.escapechar)),
            Command::MuSkip(i) => write!(f,"{}",PRIMITIVES.printable(i.name,self.escapechar)),
            Command::FontCmd(i) => write!(f,"{}",PRIMITIVES.printable(i.name,self.escapechar)),
            Command::Box(i) => write!(f,"{}",PRIMITIVES.printable(i.name,self.escapechar)),
            Command::Font(i) => {
                write!(f,"select font ")?;
                i.display(self.int,f)
            },
            Command::IntRegister(i) => write!(f,"{}count{}",ET::Char::displayable_opt(self.escapechar),i),
            Command::DimRegister(i) => write!(f,"{}dimen{}",ET::Char::displayable_opt(self.escapechar),i),
            Command::SkipRegister(i) => write!(f,"{}skip{}",ET::Char::displayable_opt(self.escapechar),i),
            Command::MuSkipRegister(i) => write!(f,"{}muskip{}",ET::Char::displayable_opt(self.escapechar),i),
            Command::ToksRegister(i) => write!(f,"{}toks{}",ET::Char::displayable_opt(self.escapechar),i),
            Command::PrimitiveInt(i) => write!(f,"{}",PRIMITIVES.printable(*i,self.escapechar)),
            Command::PrimitiveDim(i) => write!(f,"{}",PRIMITIVES.printable(*i,self.escapechar)),
            Command::PrimitiveSkip(i) => write!(f,"{}",PRIMITIVES.printable(*i,self.escapechar)),
            Command::PrimitiveMuSkip(i) => write!(f,"{}",PRIMITIVES.printable(*i,self.escapechar)),
            Command::PrimitiveToks(i) => write!(f,"{}",PRIMITIVES.printable(*i,self.escapechar)),
            Command::Whatsit(w) => write!(f,"{}",PRIMITIVES.printable(w.name,self.escapechar)),
            Command::Relax => write!(f,"{}relax",ET::Char::displayable_opt(self.escapechar))
        }
    }
}

#[derive(Clone,Debug)]
pub struct MacroSignature<T:Token> {
    pub arity:u8,
    pub params:TokenList<T>
}

#[derive(Clone,Debug)]
pub struct Macro<T:Token> {
    pub protected:bool,
    pub long:bool,
    pub outer:bool,
    pub expansion:TokenList<T>,
    pub signature:MacroSignature<T>
}
impl<T:Token> Macro<T> {
    #[inline(always)]
    pub fn meaning<'a,ET:EngineTypes<Token=T,Char=T::Char>>(&'a self,int:&'a <T::CS as ControlSequenceName>::Handler,cc:&'a CategoryCodeScheme<T::Char>,escapechar:Option<T::Char>) -> MacroMeaning<'a,ET> {
        MacroMeaning{cmd:self,int,cc,escapechar}
    }
    pub fn meaning_fmt(&self, int:&<T::CS as ControlSequenceName>::Handler, cc:&CategoryCodeScheme<T::Char>, endline:Option<T::Char>, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.protected {
            write!(f,"{}protected ",T::Char::displayable_opt(endline))?;
        }
        if self.long {
            write!(f,"{}long ",T::Char::displayable_opt(endline))?;
        }
        if self.outer {
            write!(f,"{}outer ",T::Char::displayable_opt(endline))?;
        }
        write!(f,"macro:")?;
        self.signature.params.meaning_fmt(int, cc, endline, f,false)?;
        write!(f,"->")?;
        self.expansion.meaning_fmt(int, cc, endline, f,true)
    }
}

pub struct MacroMeaning<'a,ET:EngineTypes>{
    cmd:&'a Macro<ET::Token>,
    int:&'a <<ET::Token as Token>::CS as ControlSequenceName>::Handler,
    cc:&'a CategoryCodeScheme<ET::Char>,
    escapechar:Option<ET::Char>
}
impl<'a,ET:EngineTypes> Display for MacroMeaning<'a,ET> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.cmd.meaning_fmt(self.int, self.cc, self.escapechar, f)
    }
}

#[derive(Clone,Debug)]
pub struct Conditional<ET:EngineTypes> {
    pub name:PrimitiveIdentifier,
    pub expand:fn(&mut EngineReferences<ET>,ET::Token) -> bool
}

#[derive(Clone,Debug)]
pub struct Expandable<ET:EngineTypes> {
    pub name:PrimitiveIdentifier,
    pub expand:fn(&mut EngineReferences<ET>,&mut ExpansionContainer<ET::Token>,ET::Token)
}

#[derive(Clone,Debug)]
pub struct SimpleExpandable<ET:EngineTypes> {
    pub name:PrimitiveIdentifier,
    pub expand:fn(&mut EngineReferences<ET>,ET::Token)
}

#[derive(Clone,Debug)]
pub struct Unexpandable<ET:EngineTypes> {
    pub name:PrimitiveIdentifier,
    pub apply:fn(&mut EngineReferences<ET>,ET::Token)
}

#[derive(Clone,Debug)]
pub struct Assignment<ET:EngineTypes> {
    pub name:PrimitiveIdentifier,
    pub assign:fn(&mut EngineReferences<ET>,ET::Token,bool)
}

#[derive(Clone,Debug)]
pub struct IntCommand<ET:EngineTypes> {
    pub name:PrimitiveIdentifier,
    pub read:fn(&mut EngineReferences<ET>,ET::Token) -> <ET::Num as NumSet>::Int,
    pub assign:Option<for <'a,'b> fn(&'a mut EngineReferences<'b,ET>,ET::Token,bool)>
}

#[derive(Clone,Debug)]
pub struct DimCommand<ET:EngineTypes> {
    pub name:PrimitiveIdentifier,
    pub read:fn(&mut EngineReferences<ET>,ET::Token) -> <ET::Num as NumSet>::Dim,
    pub assign:Option<for <'a,'b> fn(&'a mut EngineReferences<'b,ET>,ET::Token,bool)>
}

#[derive(Clone,Debug)]
pub struct SkipCommand<ET:EngineTypes> {
    pub name:PrimitiveIdentifier,
    pub read:fn(&mut EngineReferences<ET>,ET::Token) -> <ET::Num as NumSet>::Skip,
    pub assign:Option<for <'a,'b> fn(&'a mut EngineReferences<'b,ET>,ET::Token,bool)>
}

#[derive(Clone,Debug)]
pub struct MuSkipCommand<ET:EngineTypes> {
    pub name:PrimitiveIdentifier,
    pub read:fn(&mut EngineReferences<ET>,ET::Token) -> <ET::Num as NumSet>::MuSkip,
    pub assign:Option<for <'a,'b> fn(&'a mut EngineReferences<'b,ET>,ET::Token,bool)>
}

#[derive(Clone,Debug)]
pub struct FontCommand<ET:EngineTypes> {
    pub name:PrimitiveIdentifier,
    pub read:fn(&mut EngineReferences<ET>,ET::Token) -> <ET::FontSystem as FontSystem>::Font,
    pub assign:Option<for <'a,'b> fn(&'a mut EngineReferences<'b,ET>,ET::Token,bool)>
}

#[derive(Clone,Debug)]
pub struct BoxCommand<ET:EngineTypes> {
    pub name:PrimitiveIdentifier,
    pub read:fn(&mut EngineReferences<ET>,ET::Token) -> Result<TeXBox<ET>,(BoxInfo<ET>,Option<(u16,bool)>)>,
}

#[derive(Clone,Debug)]
pub struct Whatsit<ET:EngineTypes> {
    pub name:PrimitiveIdentifier,
    pub get:fn(&mut EngineReferences<ET>, ET::Token)
               -> Ptr<dyn FnOnce(&mut EngineReferences<ET>)>,
    pub immediate:fn(&mut EngineReferences<ET>,ET::Token)
}