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
use std::panic::PanicInfo;
use crate::engine::mouth::strings::InputTokenizer;
use crate::tex::tokens::control_sequences::{CSName, CSHandler};
use crate::tex::characters::{Character, TextLineSource};
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
pub trait ErrorHandler {
    fn new() -> Self;
    /// Invalid character in input file/string
    fn invalid_character<T:Token,D:Display>(&self,_character:T::Char,text:D) -> Option<T> {
        TeXError::throw(format!("! Text line contains an invalid character.\n{}",text))
    }
    /// "Runaway argument? Paragraph ended before `\foo` was complete."
    fn no_par<T:Token,St:AsRef<str>,S:TextLineSource<T::Char>>(&self, _tokenizer:&mut InputTokenizer<T::Char,S>, _name:St, _start:(usize, usize)) -> T {
        //let line = &tokenizer.string.line(start.0)[start.1..];
        //throw!("Runaway argument?\n{}\n! Paragraph ended before \\{} was complete.",InputLinePresenter(line),name.as_ref());
        todo!()
    }
    /// "Runaway argument? File ended while scanning use of `\foo`."
    fn file_end<T:Token,St:AsRef<str>,S:TextLineSource<T::Char>>(&self, _tokenizer:&mut InputTokenizer<T::Char,S>, _name:St, _start:(usize, usize)) -> T {
        //let line = &tokenizer.string.line(start.0)[start.1..];
        //throw!("Runaway argument?\n{}\n! File ended while scanning use of \\{}.",InputLinePresenter(line),name.as_ref());
        todo!()
    }

    fn undefined<T:Token,R>(&self, csi:&<T::CS as CSName<T::Char>>::Handler, token:T) -> R {
        match token.to_enum() {
            StandardToken::ControlSequence(cs) => self.undefined_control_sequence(csi.resolve(&cs)),
            StandardToken::Character(c,_) => self.undefined_active_character(c)
        }
    }

    /// "Undefined control sequence"
    fn undefined_control_sequence<R,St:Display>(&self,name:St) -> R { // TODO: proper error type
        TeXError::throw(format!("Undefined control sequence \\{}",name))
    }
    /// "Undefined control sequence"
    fn undefined_active_character<R,C:Character>(&self,c:C) -> R { // TODO: proper error type
        TeXError::throw(format!("Undefined active character {}",c))
    }

    /// `\errormsg`
    fn error_message<R,St:Display>(&self,msg:St) -> R { // TODO: proper error type
        TeXError::throw(format!("! {}",msg))
    }
}


/// Default [`ErrorHandler`] that just panics.
pub struct ErrorThrower;
impl ErrorHandler for ErrorThrower {
    fn new() -> Self { Self }
}

#[macro_export]
macro_rules! file_end {
    () => (crate::utils::errors::TeXError::throw("File ended unexpectedly"));
    ($tk:expr) => (crate::utils::errors::TeXError::throw(format!("File ended unexpectedly: {}",$tk)));
}