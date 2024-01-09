/*! Recovery from errors.

   An instance of [`ErrorHandler`] provides methods that get called when errors occur during compilation.
   The signatures of these methods reflect where they are called and what needs to be returned in order
   to recover from the error.
 */

use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use crate::engine::mouth::strings::StringTokenizer;
use crate::tex::control_sequences::{ControlSequenceName, ControlSequenceNameHandler};
use crate::tex::input_text::{Character, TextLineSource};
use crate::tex::token::Token;
use crate::tex::token::StandardToken;

/// Trait for error recovery, to be implemented for an engine.
pub trait ErrorHandler {
    fn new() -> Self;
    /// Invalid character in input file/string
    fn invalid_character<T:Token,D:Display>(&self,_character:T::Char,text:D) -> Option<T> {
        crate::throw!("! Text line contains an invalid character.\n{}",text);
    }
    /// "Runaway argument? Paragraph ended before `\foo` was complete."
    fn no_par<T:Token,St:AsRef<str>,S:TextLineSource<T::Char>>(&self,_tokenizer:&mut StringTokenizer<T::Char,S>,_name:St,_start:(usize,usize)) -> T {
        //let line = &tokenizer.string.line(start.0)[start.1..];
        //throw!("Runaway argument?\n{}\n! Paragraph ended before \\{} was complete.",InputLinePresenter(line),name.as_ref());
        todo!()
    }
    /// "Runaway argument? File ended while scanning use of `\foo`."
    fn file_end<T:Token,St:AsRef<str>,S:TextLineSource<T::Char>>(&self,_tokenizer:&mut StringTokenizer<T::Char,S>,_name:St,_start:(usize,usize)) -> T {
        //let line = &tokenizer.string.line(start.0)[start.1..];
        //throw!("Runaway argument?\n{}\n! File ended while scanning use of \\{}.",InputLinePresenter(line),name.as_ref());
        todo!()
    }

    fn undefined<T:Token,R>(&self,csi:&<T::CS as ControlSequenceName<T::Char>>::Handler,token:T) -> R {
        match token.to_enum() {
            StandardToken::ControlSequence(cs) => self.undefined_control_sequence(csi.resolve(&cs)),
            StandardToken::Character(c,_) => self.undefined_active_character(c)
        }
    }

    /// "Undefined control sequence"
    fn undefined_control_sequence<R,St:Display>(&self,name:St) -> R { // TODO: proper error type
        crate::throw!("Undefined control sequence \\{}",name);
    }
    /// "Undefined control sequence"
    fn undefined_active_character<R,C:Character>(&self,c:C) -> R { // TODO: proper error type
        crate::throw!("Undefined active character {}",c);
    }

    /// `\errormsg`
    fn error_message<R,St:Display>(&self,msg:St) -> R { // TODO: proper error type
        crate::throw!("! {}",msg);
    }
}

/*
struct MyPanicHook{
    old:Box<dyn Fn(&std::panic::PanicInfo<'_>) + Sync + Send>
}
*/

fn panic_hook(old:&(dyn Fn(&std::panic::PanicInfo<'_>) + Send + Sync + 'static),info:&std::panic::PanicInfo<'_>) {
    match info.payload().downcast_ref::<TeXError>() {
        Some(_) => (),
        _ => old(info)
    }
    // TODO keep the stack trace or something:
    /*
    // If this is a double panic, make sure that we print a backtrace
// for this panic. Otherwise only print it if logging is enabled.
let backtrace = if info.force_no_backtrace() {
    None
} else if panic_count::get_count() >= 2 {
    BacktraceStyle::full()
} else {
    crate::panic::get_backtrace_style()
};

// The current implementation always returns `Some`.
let location = info.location().unwrap();

let msg = match info.payload().downcast_ref::<&'static str>() {
    Some(s) => *s,
    None => match info.payload().downcast_ref::<String>() {
        Some(s) => &s[..],
        None => "Box<dyn Any>",
    },
};
let thread = thread_info::current_thread();
let name = thread.as_ref().and_then(|t| t.name()).unwrap_or("<unnamed>");

let write = |err: &mut dyn crate::io::Write| {
    let _ = writeln!(err, "thread '{name}' panicked at {location}:\n{msg}");

    static FIRST_PANIC: AtomicBool = AtomicBool::new(true);

    match backtrace {
        Some(BacktraceStyle::Short) => {
            drop(backtrace::print(err, crate::backtrace_rs::PrintFmt::Short))
        }
        Some(BacktraceStyle::Full) => {
            drop(backtrace::print(err, crate::backtrace_rs::PrintFmt::Full))
        }
        Some(BacktraceStyle::Off) => {
            if FIRST_PANIC.swap(false, Ordering::SeqCst) {
                let _ = writeln!(
                    err,
                    "note: run with `RUST_BACKTRACE=1` environment variable to display a \
                         backtrace"
                );
            }
        }
        // If backtraces aren't supported or are forced-off, do nothing.
        None => {}
    }
};

if let Some(local) = set_output_capture(None) {
    write(&mut *local.lock().unwrap_or_else(|e| e.into_inner()));
    set_output_capture(Some(local));
} else if let Some(mut out) = panic_output() {
    write(&mut out);
}
     */
}

pub fn catch<R,F:FnOnce() -> R>(f:F) -> Result<R,TeXError> {
    let old/*: std::sync::Arc<dyn Fn(&std::panic::PanicInfo<'_>) + Sync + Send>*/ = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |p| panic_hook(&old,p)));
    let r = match std::panic::catch_unwind(std::panic::AssertUnwindSafe(f)) {
        Ok(x) => Ok(x),
        Err(e) => {
            match e.downcast::<TeXError>() {
                Ok(e) => Err(*e),
                Err(e) => {
                    //std::panic::take_hook();
                    //std::panic::set_hook(old.into());
                    std::panic::resume_unwind(e)
                } //Err(TeXError{msg:format!("Panic: {:?}",e),source:None})
            }
        }
    };
    let _ = std::panic::take_hook();
    //std::panic::set_hook(old);
    r
}

/// Default [`ErrorHandler`] that just panics.
pub struct ErrorThrower;
impl ErrorHandler for ErrorThrower {
    fn new() -> Self { Self }
}

#[derive(Clone)]
pub struct TeXError {
    pub msg:String,
    pub source:Option<Box<TeXError>>,
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

#[macro_export]
macro_rules! throw {
    ($arg:expr) => {
        std::panic::panic_any::<_>(TeXError{
            msg:$arg.to_string(),
            source:std::option::Option::None
        })
    };
    ($first:expr,$($arg:expr),*) => {
        std::panic::panic_any::<_>(TeXError{
            msg:format!($first,$($arg),*),
            source:std::option::Option::None
        })
    };
}

#[macro_export]
macro_rules! file_end {
    () => (crate::throw!("File ended unexpectedly"));
    ($tk:expr) => (crate::throw!("File ended unexpectedly" => $tk));
}