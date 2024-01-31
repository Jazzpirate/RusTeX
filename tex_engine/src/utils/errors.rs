/*! Recovery from errors.

   An instance of [`ErrorHandler`] provides methods that get called when errors occur during compilation.
   The signatures of these methods reflect where they are called and what needs to be returned in order
   to recover from the error.

    Alternatively, we can throw a [`TeXError`] which will abort compilation. Good practice would be to
    pass our errors around using `Result` and `?`, but that would require a lot of boilerplate code,
    and lots of pattern matching, which is usually perfectly harmless, but in performance critical
    code, it introduces non-ngligible overhead. So instead, we panic! with a [`TeXError`], and catch
    it at the top level of the engine to abort compilation.
 */

use std::fmt::Debug;
use std::marker::PhantomData;
use thiserror::Error;
use crate::commands::primitives::PrimitiveIdentifier;
use crate::engine::{EngineAux, EngineReferences, EngineTypes};
use crate::engine::state::State;
use crate::prelude::{Mouth, TeXMode};
use crate::tex::tokens::control_sequences::CSHandler;
use crate::tex::characters::{Character, StringLineSource};
use crate::tex::tokens::Token;
use crate::tex::tokens::StandardToken;
/*
/// Error type for TeX errors occurring during compilation.
#[derive(Clone)]
pub struct TeXError {
    /// Error message
    pub msg:String,
    source:Option<Box<TeXError>>,
}
impl Debug for TeXError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f,"{}",self.msg)
    }
}
impl Display for TeXError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f,"{}",self.msg)
    }
}
impl Error for TeXError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match &self.source {
            Some(src) => Some(&*src),
            None => None
        }
    }
}
impl TeXError {
    /// Throw a TeX error.
    pub fn throw<D:Display>(msg:D) -> ! {
        std::panic::panic_any::<_>(TeXError {
            msg:msg.to_string(),
            source: None
        })
    }

    /// Catch a TeX error and return a `Result`
    pub fn catch<R,F:FnOnce() -> R>(f:F) -> Result<R,TeXError> {
        let old = std::panic::take_hook();
        let nf = move |p:&PanicInfo<'_>| panic_hook(&old,p);
        std::panic::set_hook(Box::new(nf));
        let r = match std::panic::catch_unwind(std::panic::AssertUnwindSafe(f)) {
            Ok(x) => Ok(x),
            Err(e) => {
                match e.downcast::<TeXError>() {
                    Ok(e) => Err(*e),
                    Err(e) => {
                        //std::panic::set_hook(old.into());
                        std::panic::resume_unwind(e)
                    }
                }
            }
        };
        let _ = std::panic::take_hook();
        //std::panic::set_hook(old);
        r
    }
}

fn panic_hook(old:&(dyn Fn(&std::panic::PanicInfo<'_>) + Send + Sync + 'static),info:&std::panic::PanicInfo<'_>) {
    match info.payload().downcast_ref::<TeXError>() {
        Some(_) => (),
        _ => old(info)
    }
}
*/
#[derive(Debug,Error)]
pub enum TeXError<ET:EngineTypes> {
    #[error(transparent)]
    InvalidCharacter(#[from] InvalidCharacter<ET::Char>),
    #[error("Use of {0} doesn't match its definition")]
    WrongDefinition(String),
    #[error("! File ended while scanning use of `{0}`")]
    FileEndedWhileScanningUseOf(String),
    #[error("Undefined {0}")]
    Undefined(String),
    #[error("! Too many }}'s")]
    TooManyCloseBraces,
    #[error("Emergency stop.")]
    EmergencyStop,
    #[error("! Missing {{ inserted")]
    MissingBegingroup,
    #[error("! Missing }} inserted")]
    MissingEndgroup,
    #[error("Missing number, treated as zero.")]
    MissingNumber,
    #[error("! Missing {} inserted",.0[0])]
    MissingKeyword(&'static [&'static str]),
    #[error(transparent)]
    IncompleteConditional(#[from] IncompleteConditional),
    #[error(transparent)]
    NotAllowedInMode(#[from] NotAllowedInMode),
    #[error(transparent)]
    General(#[from] GeneralError),
    #[error(transparent)]
    Fmt(#[from] std::fmt::Error),
    /*
    FileEndWhileScanningTextOf(ET::Token),
    MissingEndgroup,
    MissingArgument(ET::Token),
    MissingNumber,
    MissingKeyword(&'static[&'static str]),
    EndInsideGroup,
     */
}
pub type TeXResult<A,ET> = Result<A,TeXError<ET>>;

pub trait IntoErr<ET:EngineTypes,Err> {
    fn into_err(self,aux:&EngineAux<ET>,state:&ET::State) -> Err;
}
impl<ET:EngineTypes,Err,A> IntoErr<ET,Err> for A where Err:From<A> {
    fn into_err(self, _aux: &EngineAux<ET>, _state: &ET::State) -> Err {
        self.into()
    }
}

pub trait RecoverableError<ET:EngineTypes>:Sized {
    fn recover<M:Mouth<ET>,Err>(self,aux:&EngineAux<ET>,state:&ET::State,mouth:&mut M) -> Result<(),Err> where Self:IntoErr<ET,Err>;
    #[inline]
    fn throw<M:Mouth<ET>>(self,aux:&EngineAux<ET>,state:&ET::State,mouth:&mut M) -> Result<(),TeXError<ET>> where Self:IntoErr<ET,TeXError<ET>> {
        self.recover(aux,state,mouth).map_err(|e| e.into_err(aux,state))
    }
}
macro_rules! split {
    ($self:ident,$aux:expr,$state:ident,$mouth:ident,$f:ident($($arg:expr),*)) => {{
        let eh = $aux.error_handler.replace(ErrorThrower::new());
        let ret = eh.$f($aux,$state$(,$arg)*);
        $aux.error_handler.replace(eh);
        match ret {
            Ok(Some(src)) => {
                $mouth.push_string(src);
                Ok(())
            },
            Ok(None) => Ok(()),
            _ => Err($self.into_err($aux,$state))
        }
    }};
}

pub enum MouthError<C:Character> {
    InvalidChar(C),
    MouthEmpty,
}
impl<C:Character> From<InvalidCharacter<C>> for MouthError<C> {
    #[inline]
    fn from(e: InvalidCharacter<C>) -> Self {
        MouthError::InvalidChar(e.0)
    }
}
pub type MouthResult<T> = Result<T, MouthError<<T as Token>::Char>>;

#[derive(Debug)]
pub enum GulletError<C:Character> {
    InvalidChar(C),
    MouthEmpty,
    TooManyCloseBraces,
}
impl<C:Character> From<MouthError<C>> for GulletError<C> {
    fn from(e: MouthError<C>) -> Self {
        match e {
            MouthError::InvalidChar(c) => GulletError::InvalidChar(c),
            MouthError::MouthEmpty => GulletError::MouthEmpty
        }
    }
}
impl<C:Character> From<TooManyCloseBraces> for GulletError<C> {
    fn from(_: TooManyCloseBraces) -> Self {
        GulletError::TooManyCloseBraces
    }
}
pub type GulletResult<T> = Result<T, GulletError<<T as Token>::Char>>;

#[derive(Debug,Error)]
#[error("Errror: {0}")]
pub struct GeneralError(pub String);
impl<ET:EngineTypes> RecoverableError<ET> for GeneralError {
    fn recover<M:Mouth<ET>,Err>(self,aux:&EngineAux<ET>,state:&ET::State,mouth:&mut M) -> Result<(),Err> where Self:IntoErr<ET,Err> {
        split!(self,aux,state,mouth,other(&self.0))
    }
}

/// An error indicating that an invalid [`Character`] was encountered
#[derive(Debug,Error)]
#[error("! Text line contains an invalid character.\n{}",.0.display())]
pub struct InvalidCharacter<C:Character>(pub C);
impl<ET:EngineTypes> RecoverableError<ET> for InvalidCharacter<ET::Char> {
    fn recover<M:Mouth<ET>,Err>(self,aux:&EngineAux<ET>,state:&ET::State,mouth:&mut M) -> Result<(),Err> where Self:IntoErr<ET,Err> {
        split!(self,aux,state,mouth,invalid_character(self.0))
    }
}


#[derive(Debug,Error)]
#[error("Incomplete {}; all text was ignored after line {line_no}",.name.display::<u8>(Some(b'\\')))]
pub struct IncompleteConditional {
    pub name:PrimitiveIdentifier,
    pub line_no:usize
}
impl<ET:EngineTypes> RecoverableError<ET> for IncompleteConditional {
    fn recover<M:Mouth<ET>,Err>(self,aux:&EngineAux<ET>,state:&ET::State,mouth:&mut M) -> Result<(),Err> where Self:IntoErr<ET,Err> {
        split!(self,aux,state,mouth,incomplete_conditional(self.name,self.line_no))
    }
}

pub struct WrongDefinition<T:Token> {
    pub(crate) expected:T,
    pub(crate) found:T,
    pub(crate) in_macro:T
}
impl<ET:EngineTypes> IntoErr<ET,TeXError<ET>> for WrongDefinition<ET::Token> {
    fn into_err(self, aux: &EngineAux<ET>, state: &ET::State) -> TeXError<ET> {
        TeXError::WrongDefinition(self.in_macro.display(aux.memory.cs_interner(),state.get_catcode_scheme(),state.get_escape_char()).to_string())
    }
}
impl<ET:EngineTypes> RecoverableError<ET> for WrongDefinition<ET::Token> {
    fn recover<M:Mouth<ET>,Err>(self,aux:&EngineAux<ET>,state:&ET::State,mouth:&mut M) -> Result<(),Err> where Self:IntoErr<ET,Err> {
        split!(self,aux,state,mouth,wrong_definition(&self.found,&self.expected,&self.in_macro))
    }
}

pub struct FileEndWhileUse<T:Token>(pub T);
impl<ET:EngineTypes> IntoErr<ET,TeXError<ET>> for FileEndWhileUse<ET::Token> {
    fn into_err(self, aux: &EngineAux<ET>, state: &ET::State) -> TeXError<ET> {
        TeXError::FileEndedWhileScanningUseOf(self.0.display(aux.memory.cs_interner(), state.get_catcode_scheme(), state.get_escape_char()).to_string())
    }
}
impl<ET:EngineTypes> RecoverableError<ET> for FileEndWhileUse<ET::Token> {
    fn recover<M:Mouth<ET>,Err>(self,aux:&EngineAux<ET>,state:&ET::State,mouth:&mut M) -> Result<(),Err> where Self:IntoErr<ET,Err> {
        split!(self,aux,state,mouth,file_end_while_use(&self.0))
    }
}

pub struct Undefined<T:Token>(pub T);
impl<ET:EngineTypes> IntoErr<ET,TeXError<ET>> for Undefined<ET::Token> {
    fn into_err(self, aux: &EngineAux<ET>, state: &ET::State) -> TeXError<ET> {
        TeXError::Undefined(
            match self.0.to_enum() {
                StandardToken::ControlSequence(cs) =>
                    format!("control sequence {}{}",ET::Char::display_opt(state.get_escape_char()),aux.memory.cs_interner().resolve(&cs)),
                StandardToken::Character(c,_) =>
                    format!("active character {}",c.display()),
                _ => unreachable!()
            }
        )
    }
}
impl<ET:EngineTypes> RecoverableError<ET> for Undefined<ET::Token> {
    fn recover<M:Mouth<ET>,Err>(self,aux:&EngineAux<ET>,state:&ET::State,mouth:&mut M) -> Result<(),Err> where Self:IntoErr<ET,Err> {
        split!(self,aux,state,mouth,undefined(&self.0))
    }
}

pub struct TooManyCloseBraces;
impl<ET:EngineTypes> RecoverableError<ET> for TooManyCloseBraces {
    fn recover<M:Mouth<ET>,Err>(self,aux:&EngineAux<ET>,state:&ET::State,mouth:&mut M) -> Result<(),Err> where Self:IntoErr<ET,Err> {
        split!(self,aux,state,mouth,too_many_closebraces())
    }
}
impl<ET:EngineTypes> From<TooManyCloseBraces> for TeXError<ET> {
    fn from(_: TooManyCloseBraces) -> Self {
        TeXError::TooManyCloseBraces
    }
}

#[derive(Debug,Error)]
#[error("You can't use {} in {mode} mode",.name.display::<u8>(Some(b'\\')))]
pub struct NotAllowedInMode {
    pub name:PrimitiveIdentifier,
    pub mode:TeXMode
}
impl<ET:EngineTypes> RecoverableError<ET> for NotAllowedInMode {
    fn recover<M:Mouth<ET>,Err>(self,aux:&EngineAux<ET>,state:&ET::State,mouth:&mut M) -> Result<(),Err> where Self:IntoErr<ET,Err> {
        split!(self,aux,state,mouth,not_allowed_in_mode(self.name,self.mode))
    }
}

pub struct MissingBegingroup;
impl<ET:EngineTypes> RecoverableError<ET> for MissingBegingroup {
    fn recover<M:Mouth<ET>,Err>(self,aux:&EngineAux<ET>,state:&ET::State,mouth:&mut M) -> Result<(),Err> where Self:IntoErr<ET,Err> {
        split!(self,aux,state,mouth,missing_begingroup())
    }
}
impl<ET:EngineTypes> From<MissingBegingroup> for TeXError<ET> {
    fn from(_: MissingBegingroup) -> Self {
        TeXError::MissingBegingroup
    }
}

pub struct MissingEndgroup;
impl<ET:EngineTypes> RecoverableError<ET> for MissingEndgroup {
    fn recover<M:Mouth<ET>,Err>(self,aux:&EngineAux<ET>,state:&ET::State,mouth:&mut M) -> Result<(),Err> where Self:IntoErr<ET,Err> {
        split!(self,aux,state,mouth,missing_endgroup())
    }
}
impl<ET:EngineTypes> From<MissingEndgroup> for TeXError<ET> {
    fn from(_: MissingEndgroup) -> Self {
        TeXError::MissingEndgroup
    }
}

pub struct MissingNumber;
impl<ET:EngineTypes> RecoverableError<ET> for MissingNumber {
    fn recover<M:Mouth<ET>,Err>(self,aux:&EngineAux<ET>,state:&ET::State,mouth:&mut M) -> Result<(),Err> where Self:IntoErr<ET,Err> {
        split!(self,aux,state,mouth,missing_number())
    }
}
impl<ET:EngineTypes> From<MissingNumber> for TeXError<ET> {
    fn from(_: MissingNumber) -> Self {
        TeXError::MissingNumber
    }
}

pub enum InvalidCharacterOrEOF<C:Character> {
    Invalid(C),
    EOF
}
impl<C:Character> From<InvalidCharacter<C>> for InvalidCharacterOrEOF<C> {
    #[inline]
    fn from(e: InvalidCharacter<C>) -> Self {
        InvalidCharacterOrEOF::Invalid(e.0)
    }
}

pub struct MissingKeyword(pub &'static [&'static str]);
impl<ET:EngineTypes> RecoverableError<ET> for MissingKeyword {
    fn recover<M:Mouth<ET>,Err>(self,aux:&EngineAux<ET>,state:&ET::State,mouth:&mut M) -> Result<(),Err> where Self:IntoErr<ET,Err> {
        split!(self,aux,state,mouth,missing_keyword(self.0))
    }
}
impl<ET:EngineTypes> From<MissingKeyword> for TeXError<ET> {
    fn from(MissingKeyword(els): MissingKeyword) -> Self {
        TeXError::MissingKeyword(els)
    }
}


/// Trait for error recovery, to be implemented for an engine.
pub trait ErrorHandler<ET:EngineTypes> {
    /// "Text line contains an invalid character".
    fn invalid_character(&self, _aux:&EngineAux<ET>, _state:&ET::State, _c:ET::Char) -> Result<Option<StringLineSource<ET::Char>>,()> {
        Err(())
    }
    /// "Use of `\foo` doesn't match its definition".
    fn wrong_definition(&self, _aux:&EngineAux<ET>, _state:&ET::State, _found:&ET::Token, _expected:&ET::Token, _in_macro:&ET::Token) -> Result<Option<StringLineSource<ET::Char>>,()> {
        Err(())
    }
    /// "File ended while scanning use of `\foo`".
    fn file_end_while_use(&self, _aux:&EngineAux<ET>, _state:&ET::State, _in_macro:&ET::Token) -> Result<Option<StringLineSource<ET::Char>>,()> {
        Err(())
    }
    /// "Too many }'s".
    fn too_many_closebraces(&self, _aux:&EngineAux<ET>, _state:&ET::State) -> Result<Option<StringLineSource<ET::Char>>,()> {
        Err(())
    }
    /// "Missing { inserted".
    fn missing_begingroup(&self,_aux:&EngineAux<ET>,_state:&ET::State) -> Result<Option<StringLineSource<ET::Char>>,()> {
        Err(())
    }
    /// "Missing } inserted".
    fn missing_endgroup(&self,_aux:&EngineAux<ET>,_state:&ET::State) -> Result<Option<StringLineSource<ET::Char>>,()> {
        Err(())
    }

    /// "Incomplete \if...; all text was ignored after line `n`".
    fn incomplete_conditional(&self,_aux:&EngineAux<ET>,_state:&ET::State,_name:PrimitiveIdentifier,_line_no:usize) -> Result<Option<StringLineSource<ET::Char>>,()> {
        Err(())
    }

    /// "Undefined `[`control sequence`|`active character`]`"
    fn undefined(&self,_aux:&EngineAux<ET>,_state:&ET::State,_token:&ET::Token) -> Result<Option<StringLineSource<ET::Char>>,()> {
        Err(())
    }

    /// "You can't use `\foo` in `M` mode."
    fn not_allowed_in_mode(&self,_aux:&EngineAux<ET>,_state:&ET::State,_name:PrimitiveIdentifier,_mode:TeXMode) -> Result<Option<StringLineSource<ET::Char>>,()> {
        Err(())
    }
    /// "Missing `x` inserted"
    fn missing_keyword(&self,_aux:&EngineAux<ET>,_state:&ET::State,_kws:&'static[&'static str]) -> Result<Option<StringLineSource<ET::Char>>,()> {
        Err(())
    }

    /// "Missing number, treated as zero."
    fn missing_number(&self,_aux:&EngineAux<ET>,_state:&ET::State) -> Result<Option<StringLineSource<ET::Char>>,()> {
        Err(())
    }

    /// Any other possibly recoverable error.
    fn other(&self,_aux:&EngineAux<ET>,_state:&ET::State,_msg:&str) -> Result<Option<StringLineSource<ET::Char>>,()> {
        Err(())
    }

/*

    /// `\errmessage`
    fn error_message(&self,msg:&str) {
        TeXError::throw(format!("! {}",msg))
    }

    fn emergency_stop(&self,_state:&ET::State) -> ! {
        TeXError::throw(format!("Emergency stop."))
    }

    fn other(&self,_engine:&mut EngineReferences<ET>,msg:&str) {
        TeXError::throw(msg.to_string())
    }

*/

    /*
    /// "Runaway argument? Paragraph ended before `\foo` was complete."
    fn no_par<T:Token,St:AsRef<str>,S:TextLineSource<T::Char>>(&self, _tokenizer:&mut InputTokenizer<T::Char,S>, _name:St, _start:(usize, usize)) -> T {
        //let line = &tokenizer.string.line(start.0)[start.1..];
        //throw!("Runaway argument?\n{}\n! Paragraph ended before \\{} was complete.",InputLinePresenter(line),name.as_ref());
    }
    /// "Runaway argument? File ended while scanning use of `\foo`."
    fn file_end<T:Token,St:AsRef<str>,S:TextLineSource<T::Char>>(&self, _tokenizer:&mut InputTokenizer<T::Char,S>, _name:St, _start:(usize, usize)) -> T {
        //let line = &tokenizer.string.line(start.0)[start.1..];
        //throw!("Runaway argument?\n{}\n! File ended while scanning use of \\{}.",InputLinePresenter(line),name.as_ref());
    }

     */

}


/// Default [`ErrorHandler`] that just throws a [`TeXError`].
pub struct ErrorThrower<ET:EngineTypes>(PhantomData<ET>);
impl<ET:EngineTypes> ErrorHandler<ET> for ErrorThrower<ET> {}
impl<ET:EngineTypes> ErrorThrower<ET> {
    pub fn new() -> Box<dyn ErrorHandler<ET>> { Box::new(Self(PhantomData)) }
}

impl<ET:EngineTypes> EngineReferences<'_,ET> {
    #[inline]
    pub fn general_error(&mut self,msg:String) -> TeXResult<(),ET> {
        GeneralError(msg).recover::<_,TeXError<_>>(self.aux,self.state,self.mouth)
    }
}