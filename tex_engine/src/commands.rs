/*! Commands - primitives, macros, etc. The B-book largely calls these
"equivalents", but we use the more standard term "command" instead.
*/

use std::fmt::Display;
use crate::engine::{EngineReferences, EngineTypes};
use crate::engine::fontsystem::FontSystem;
use crate::engine::utils::memory::{PrimitiveIdentifier, PRIMITIVES};
use crate::engine::mouth::pretokenized::{Stringify, TokenList, WriteChars};
use crate::tex::catcodes::{CategoryCodeScheme, CommandCode};
use crate::tex::control_sequences::ControlSequenceName;
use crate::tex::numerics::NumSet;
use crate::tex::token::Token;
use crate::engine::fontsystem::Font;
use crate::tex::nodes::boxes::{BoxInfo, TeXBox};

pub mod primitives;
pub mod tex;
pub mod etex;
pub mod pdftex;
pub mod methods;

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
    pub fn meaning<'a>(&'a self,int:&'a <<ET::Token as Token>::CS as ControlSequenceName<ET::Char>>::Handler,cc:&'a CategoryCodeScheme<ET::Char>,escapechar:Option<ET::Char>) -> Meaning<'a,ET> {
        Meaning{cmd:self,int,cc,escapechar}
    }
}

pub struct Meaning<'a,ET:EngineTypes>{
    cmd:&'a Command<ET>,
    int:&'a <<ET::Token as Token>::CS as ControlSequenceName<ET::Char>>::Handler,
    cc:&'a CategoryCodeScheme<ET::Char>,
    escapechar:Option<ET::Char>
}
impl<'a,ET:EngineTypes> Meaning<'a,ET> {
    fn write_chars<W:WriteChars<ET::Char,ET::CSName>>(&self,mut f:W) {
        match self.cmd {
            Command::Macro(m) => m.meaning_char(self.int, self.cc, self.escapechar, f),
            Command::Char{char,code} =>
                code.meaning(*char,f),
            Command::CharDef(c) => {
                if let Some(e) = self.escapechar { f.push_char(e)}
                write!(f,"char\"{:X}",Into::<u64>::into(*c)).unwrap();
            },
            Command::MathChar(u) => {
                if let Some(e) = self.escapechar { f.push_char(e)}
                write!(f,"mathchar\"{:X}",u).unwrap();
            },
            Command::Font(i) => {
                write!(f,"select font ").unwrap();
                i.display(self.int,f).unwrap();
            },
            Command::IntRegister(i) => {
                if let Some(e) = self.escapechar { f.push_char(e)}
                write!(f,"count{}",i).unwrap();
            },
            Command::DimRegister(i) => {
                if let Some(e) = self.escapechar { f.push_char(e)}
                write!(f,"dimen{}",i).unwrap();
            },
            Command::SkipRegister(i) => {
                if let Some(e) = self.escapechar { f.push_char(e) }
                write!(f, "skip{}", i).unwrap();
            }
            Command::MuSkipRegister(i) => {
                if let Some(e) = self.escapechar { f.push_char(e)}
                write!(f,"muskip{}",i).unwrap();
            },
            Command::ToksRegister(i) => {
                if let Some(e) = self.escapechar { f.push_char(e)}
                write!(f,"toks{}",i).unwrap();
            },
            Command::Relax => {
                if let Some(e) = self.escapechar { f.push_char(e)}
                f.write_str("relax").unwrap();
            },
            Command::Conditional(Conditional{name,..}) |
            Command::Expandable(Expandable{name,..}) |
            Command::SimpleExpandable(SimpleExpandable{name,..}) |
            Command::Unexpandable(Unexpandable{name,..}) |
            Command::Assignment(Assignment{name,..}) |
            Command::Int(IntCommand{name,..}) |
            Command::Dim(DimCommand{name,..}) |
            Command::Skip(SkipCommand{name,..}) |
            Command::MuSkip(MuSkipCommand{name,..}) |
            Command::FontCmd(FontCommand{name,..}) |
            Command::Box(BoxCommand{name,..}) |
            Command::PrimitiveInt(name) |
            Command::PrimitiveDim(name) |
            Command::PrimitiveSkip(name) |
            Command::PrimitiveMuSkip(name) |
            Command::PrimitiveToks(name) |
            Command::Whatsit(Whatsit{name,..}) => {
                if let Some(e) = self.escapechar { f.push_char(e)}
                PRIMITIVES.with(*name,|s| f.write_str(s).unwrap());
            },
        }
    }
}
impl<'a,ET:EngineTypes> Display for Meaning<'a,ET> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.write_chars(Stringify::new(f));
        Ok(())
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
    pub fn meaning<'a,ET:EngineTypes<Token=T,Char=T::Char>>(&'a self,int:&'a <T::CS as ControlSequenceName<ET::Char>>::Handler,cc:&'a CategoryCodeScheme<T::Char>,escapechar:Option<T::Char>) -> MacroMeaning<'a,ET> {
        MacroMeaning{cmd:self,int,cc,escapechar}
    }
    pub fn meaning_char<F:WriteChars<T::Char,T::CS>>(&self, int:&<T::CS as ControlSequenceName<T::Char>>::Handler, cc:&CategoryCodeScheme<T::Char>, escapechar:Option<T::Char>, mut f: F) {
        if self.protected {
            if let Some(e) = escapechar { f.push_char(e) }
            write!(f,"protected ").unwrap();
        }
        if self.long {
            if let Some(e) = escapechar { f.push_char(e) }
            write!(f,"long ").unwrap();
        }
        if self.outer {
            if let Some(e) = escapechar { f.push_char(e) }
            write!(f,"outer ").unwrap();
        }
        write!(f,"macro:").unwrap();
        self.signature.params.meaning_char(int, cc, escapechar, &mut f, false);
        write!(f,"->").unwrap();
        self.expansion.meaning_char(int, cc, escapechar, &mut f, true)
    }
    pub fn meaning_fmt(&self, int:&<T::CS as ControlSequenceName<T::Char>>::Handler, cc:&CategoryCodeScheme<T::Char>, escapechar:Option<T::Char>, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.meaning_char( int, cc, escapechar,Stringify::new(f));
        Ok(())
    }
}

pub struct MacroMeaning<'a,ET:EngineTypes>{
    cmd:&'a Macro<ET::Token>,
    int:&'a <<ET::Token as Token>::CS as ControlSequenceName<ET::Char>>::Handler,
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
    pub expand:fn(&mut EngineReferences<ET>,&mut Vec<ET::Token>,ET::Token)
}

#[derive(Clone,Debug)]
pub struct SimpleExpandable<ET:EngineTypes> {
    pub name:PrimitiveIdentifier,
    pub expand:fn(&mut EngineReferences<ET>,ET::Token)
}

#[derive(Clone,Debug)]
pub struct Unexpandable<ET:EngineTypes> {
    pub name:PrimitiveIdentifier,
    pub scope: CommandScope,
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
    pub read:fn(&mut EngineReferences<ET>,ET::Token) -> Result<Option<TeXBox<ET>>,BoxInfo<ET>>,
}

#[derive(Clone,Debug,Copy)]
pub enum CommandScope {
    SwitchesToVertical,SwitchesToHorizontal,MathOnly,Any,SwitchesToHorizontalOrMath
}


#[derive(Clone,Debug)]
pub struct Whatsit<ET:EngineTypes> {
    pub name:PrimitiveIdentifier,
    pub get:fn(&mut EngineReferences<ET>, ET::Token)
               -> Option<Box<dyn FnOnce(&mut EngineReferences<ET>)>>,
    pub immediate:fn(&mut EngineReferences<ET>,ET::Token)
}