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

use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::marker::PhantomData;
use std::panic::PanicInfo;
use crate::commands::primitives::PrimitiveIdentifier;
use crate::engine::{EngineReferences, EngineTypes};
use crate::engine::state::State;
use crate::engine::stomach::Stomach;
use crate::prelude::CSName;
use crate::tex::tokens::control_sequences::CSHandler;
use crate::tex::characters::{Character, StringLineSource};
use crate::tex::tokens::Token;
use crate::tex::tokens::StandardToken;

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

/// Trait for error recovery, to be implemented for an engine.
pub trait ErrorHandler<ET:EngineTypes> {

    /// "Text line contains an invalid character"
    fn invalid_character(&self,_state:&ET::State,c:ET::Char) -> Option<StringLineSource<ET::Char>> {
        TeXError::throw(format!("! Text line contains an invalid character.\n{}",c.display()))
    }

    /// "File ended while scanning use of X"
    fn file_end_while_scanning(&self,state:&ET::State,int:&<ET::CSName as CSName<ET::Char>>::Handler,token:ET::Token) {
        TeXError::throw(format!("! File ended while scanning use of {}",token.display(int,state.get_catcode_scheme(),state.get_escape_char())))
    }

    fn too_many_closebraces(&self) {
        TeXError::throw(format!("Too many }}'s"))
    }

    /// "You can't use `X` in `Y` mode."
    fn not_allowed_in_mode(&self,engine:&mut EngineReferences<ET>,_token:ET::Token,name:PrimitiveIdentifier) {
        TeXError::throw(format!("! You can't use `{}` in {} mode.",name.display(engine.state.get_escape_char()),engine.stomach.data_mut().mode()))
    }

    /// "File ended while scanning text of `X`"
    fn missing_begingroup(&self,engine:&mut EngineReferences<ET>,t:ET::Token) {
        TeXError::throw(format!("! File ended while scanning text of {}",t.display(engine.aux.memory.cs_interner(),engine.state.get_catcode_scheme(),engine.state.get_escape_char())))
    }

    fn missing_endgroup(&self,engine:&mut EngineReferences<ET>) {
        TeXError::throw(format!("! Missing }} inserted"))
    }

    /// "File ended while scanning use of `X`"
    fn missing_argument(&self,engine:&mut EngineReferences<ET>,t:ET::Token) {
        TeXError::throw(format!("! File ended while scanning use of {}",t.display(engine.aux.memory.cs_interner(),engine.state.get_catcode_scheme(),engine.state.get_escape_char())))
    }

    /// "Missing number, treated as zero."
    fn missing_number(&self,_engine:&mut EngineReferences<ET>) {
        TeXError::throw(format!("Missing number"));
    }

    /// "Missing `x` inserted"
    fn missing_keyword(&self,_engine:&mut EngineReferences<ET>,kws:&[&str]) {
        TeXError::throw(format!("Missing keyword: One of {}",kws.iter().map(|s| format!("`{}`",s)).collect::<Vec<_>>().join(", ")));
    }

    fn other(&self,_engine:&mut EngineReferences<ET>,msg:&str) {
        TeXError::throw(msg.to_string())
    }


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

    /// "Undefined `[`control sequence`|`active character`]`"
    fn undefined(&self, engine:&mut EngineReferences<ET>,token:ET::Token) {
        match token.to_enum() {
            StandardToken::ControlSequence(cs) => self.undefined_control_sequence(engine,token,cs),
            StandardToken::Character(c,_) => self.undefined_active_character(engine,token,c),
            _ => unreachable!()
        }
    }

    /// "Undefined control sequence"
    fn undefined_control_sequence(&self,engine:&mut EngineReferences<ET>,_token:ET::Token,csname:ET::CSName) {
        TeXError::throw(format!("Undefined control sequence {}{}",ET::Char::display_opt(engine.state.get_escape_char()),engine.aux.memory.cs_interner().resolve(&csname)))
    }
    /// "Undefined active character"
    fn undefined_active_character(&self,_engine:&mut EngineReferences<ET>,_token:ET::Token,c:ET::Char) {
        TeXError::throw(format!("Undefined active character {}",c.display()))
    }

    /// `\errmessage`
    fn error_message(&self,msg:&str) {
        TeXError::throw(format!("! {}",msg))
    }
}


/// Default [`ErrorHandler`] that just throws a [`TeXError`].
pub struct ErrorThrower<ET:EngineTypes>(PhantomData<ET>);
impl<ET:EngineTypes> ErrorHandler<ET> for ErrorThrower<ET> {}
impl<ET:EngineTypes> ErrorThrower<ET> {
    pub fn new() -> Box<dyn ErrorHandler<ET>> { Box::new(Self(PhantomData)) }
}
/*
impl<ET:EngineTypes> EngineReferences<'_,ET> {
    pub fn throw_error<F:FnOnce(&mut Self)>(&mut self,f:F) {
        match f(self) {
            Some(src) => self.mouth.push_string(src),
            _ => ()
        }
    }
}

 */

/// Convenience macro for throwing a [`TeXError`] that temporarily replaces the [`ErrorHandler`]
/// to make the borrow checker happy.
/// e.g. [`tex_error`](crate::tex_error)`!(engine,`[undefined_control_sequence](ErrorHandler::undefined_control_sequence)`,token,csname)`
#[macro_export]
macro_rules! tex_error {
    ($engine:expr,$e:ident) => {{
        let eh = std::mem::replace(&mut $engine.aux.error_handler,crate::utils::errors::ErrorThrower::new());
        crate::utils::errors::ErrorHandler::$e(&*eh,$engine);
        $engine.aux.error_handler = eh;
    }};
    ($engine:expr,$e:ident,$($arg:expr),*) => {{
        let eh = std::mem::replace(&mut $engine.aux.error_handler,crate::utils::errors::ErrorThrower::new());
        crate::utils::errors::ErrorHandler::$e(&*eh,$engine,$($arg),*);
        $engine.aux.error_handler = eh;
    }};
}