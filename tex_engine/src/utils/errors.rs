use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use crate::engine::EngineType;
use crate::engine::memory::Memory;
use crate::tex::commands::Def;
use crate::tex::token::{Token, TokenReference};
use crate::utils::strings::CharType;
#[derive(Clone,Debug)]
pub struct TeXError<ET:EngineType> {
    pub msg:String,
    pub cause:Option<Token<ET>>,
    pub source:Option<Box<TeXError<ET>>>,
}
impl<ET:EngineType> TeXError<ET> {
    pub fn throw_string(self,memory:&mut Memory<ET>) -> String {
        let mut ret = self.msg;
        match self.cause {
            Some(tk) => match tk.sourceref.as_ref().map(|t| t.trace(memory)).flatten() {
                Some(trace) => {
                    ret.push_str(format!(": {} - {}",tk.to_str(memory,Some(ET::Char::backslash())),trace).as_str());
                }
                None => {
                    ret.push_str(format!(": {}",tk.to_str(memory,Some(ET::Char::backslash()))).as_str());
                }
            },
            None => ()
        }
        match self.source {
            Some(src) => {
                ret.push_str(format!("\n  caused by: {}",src.throw_string(memory)).as_str());
            }
            None => {}
        }
        ret
    }
}
impl<ET:EngineType> Display for TeXError<ET> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f,"{}",self.msg)
    }
}
impl<ET:EngineType> Error for TeXError<ET> {
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
        return Err(TeXError{
            msg:$arg.to_string(),
            cause:std::option::Option::None,
            source:std::option::Option::None
        })
    };
    ($first:expr,$($arg:expr),*) => {
        return Err(TeXError{
            msg:format!($first,$($arg),*),
            cause:std::option::Option::None,
            source:std::option::Option::None
        })
    };
    ($arg:expr => $cause:expr) => {
        return Err(TeXError{
            msg:$arg.to_string(),
            cause:Some($cause.clone()),
            source:std::option::Option::None
        })
    };
    ($first:expr,$($arg:expr),* => $cause:expr) => {
        return Err(TeXError{
            msg:format!($first,$($arg),*),
            cause:Some($cause.clone()),
            source:std::option::Option::None
        })
    };
}

#[macro_export]
macro_rules! file_end {
    () => (crate::throw!("File ended unexpectedly"));
    ($tk:expr) => (crate::throw!("File ended unexpectedly" => $tk));
}

#[macro_export]
macro_rules! file_end_prim {
    ($name:expr,$tk:expr) => (crate::throw!("File ended while scanning {}",$name => $tk.cause.clone()));
}

#[macro_export]
macro_rules! catch_prim {
    ($f:expr => ($name:expr,$cause:expr)) => {
        match $f {
            Ok(x) => x,
            Err(e) => {
                return Err(TeXError{
                    msg:format!("Error in \\{}",$name),
                    cause:Some($cause.cause.clone()),
                    source:Some(Box::new(e))
                })
            }
        }
    }
}

#[macro_export]
macro_rules! catch {
    ($f:expr => $cause:expr) => {
        match $f {
            Ok(x) => x,
            Err(mut e) => {
                match e.cause {
                    Some(_) => (),
                    std::option::Option::None => e.cause = Some($cause)
                }
                return Err(e);
            }
        }
    };
    ($f:expr ; $msg:expr) => {
        match $f {
            Ok(x) => x,
            Err(e) => {
                return Err(TeXError{
                    msg:format!($msg),
                    cause:std::option::Option::None,
                    source:Some(Box::new(e))
                })
            }
        }
    };
    ($f:expr ; $msg:expr => $cause:expr) => {
        match $f {
            Ok(x) => x,
            Err(e) => {
                return Err(TeXError{
                    msg:format!($msg),
                    cause:Some($cause),
                    source:Some(Box::new(e))
                })
            }
        }
    }
}


/*
pub trait TeXError<T:Token>:Error {
    fn display(&self, f: &mut Formatter<'_>) -> std::fmt::Result;
    fn debug(&self, f: &mut Formatter<'_>) -> std::fmt::Result {self.display(f)}
    fn token(&self) -> Option<&T>;
    fn error_source(&self) -> Option<&(dyn TeXError<T>+'static)>;
    fn as_error_source(&self) -> Option<&(dyn Error+'static)>;
    fn throw_string(&self) -> String {
        let mut ret = String::new();
        match self.token() {
            Some(tk) => match tk.sourceref_trace() {
                Some(trace) => {
                    ret.push_str(format!("{}: {} - {}",self,tk,trace).as_str());
                }
                None => {
                    ret.push_str(format!("{}: {}",self,tk).as_str());
                }
            },
            None => {
                ret.push_str(format!("{}",self).as_str());
            }
        }
        match self.error_source() {
            Some(src) => {
                ret.push_str(format!("\n  caused by: {}",src.throw_string()).as_str());
            }
            None => {}
        }
        ret
    }
}
impl<T:Token> TeXError<T> for Box<dyn TeXError<T> + 'static> {
    fn display(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        (**self).display(f)
    }
    fn debug(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        (**self).debug(f)
    }
    fn token(&self) -> Option<&T> {
        (**self).token()
    }
    fn error_source(&self) -> Option<&(dyn TeXError<T>+'static)> {
        (**self).error_source()
    }
    fn as_error_source(&self) -> Option<&(dyn Error + 'static)> {
        (**self).as_error_source()
    }
}
impl<T:Token> Error for Box<dyn TeXError<T> + 'static> {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        (**self).as_error_source()
    }
}
macro_rules! error_impl {
    ($name:ident) => {
        impl<T:Token> Display for $name<T> {
            fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
                self.display(f)
            }
        }
        impl<T:Token> Debug for $name<T> {
            fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
                self.debug(f)
            }
        }
        impl<T:Token> std::error::Error for $name<T> {
            fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {self.as_error_source()}
        }
        impl<T:Token> From<$name<T>> for Box<dyn TeXError<T> + 'static> {
            fn from(e: $name<T>) -> Self {
                Box::new(e)
            }
        }
    }
}


pub struct FileEndedUnexpectedly<T:Token>{pub cause: Option<T>}
impl<T:Token> TeXError<T> for FileEndedUnexpectedly<T> {
    fn display(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.cause {
            None => write!(f,"File ended unexpectedly"),
            Some(ref tk) => write!(f,"File ended unexpectedly while reading {}",tk)
        }
    }
    fn token(&self) -> Option<&T> {self.cause.as_ref()}
    fn error_source(&self) -> Option<&(dyn TeXError<T>+'static)> {None}
    fn as_error_source(&self) -> Option<&(dyn Error+'static)> {None}
}
error_impl!(FileEndedUnexpectedly);

#[macro_export]
macro_rules! file_end {
    () => (return Err(crate::utils::errors::FileEndedUnexpectedly{cause:None}.into()));
    ($tk:expr) => (return Err(crate::utils::errors::FileEndedUnexpectedly{cause:Some($tk)}.into()));
}

pub struct ErrorInDef<T:Token>{
    pub def:Def<T>, pub cause:T, pub source:Box<dyn TeXError<T>>}
impl<T:Token> TeXError<T> for ErrorInDef<T> {
    fn display(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f,"Error in definition of {:?}",self.def)
    }
    fn token(&self) -> Option<&T> {Some(&self.cause)}
    fn error_source(&self) -> Option<&(dyn TeXError<T>+'static)> {
        Some(&self.source)
    }
    fn as_error_source(&self) -> Option<&(dyn Error+'static)> {
        Some(&self.source)
    }
}
error_impl!(ErrorInDef);

pub struct ModeError<T:Token>{pub cmd:&'static str,pub mode:TeXMode, pub cause:Option<T>, pub source:Option<Box<dyn TeXError<T>>>}
impl<T:Token> TeXError<T> for ModeError<T> {
    fn display(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f,"You can't use `\\{}' in {} mode.",self.cmd,self.mode)
    }
    fn token(&self) -> Option<&T> {self.cause.as_ref()}
    fn error_source(&self) -> Option<&(dyn TeXError<T>+'static)> {
        if let Some(ref s) = self.source {Some(&*s)} else {None}
    }
    fn as_error_source(&self) -> Option<&(dyn Error+'static)> {
        if let Some(ref s) = self.source {Some(&*s)} else {None}
    }
}
error_impl!(ModeError);

pub struct OtherError<T:Token>{pub msg:String,pub cause:Option<T>,pub source:Option<Box<dyn TeXError<T>>>}
impl<T:Token> TeXError<T> for OtherError<T> {
    fn display(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f,"{}",self.msg)
    }
    fn token(&self) -> Option<&T> {self.cause.as_ref()}
    fn error_source(&self) -> Option<&(dyn TeXError<T>+'static)> {
        if let Some(ref s) = self.source {Some(&*s)} else {None}
    }
    fn as_error_source(&self) -> Option<&(dyn Error+'static)> {
        if let Some(ref s) = self.source {Some(&*s)} else {None}
    }
}
error_impl!(OtherError);

pub struct TeXError<T:Token>{pub name:&'static str,pub msg:Option<String>,pub cause:Option<T>,pub source:Option<Box<dyn TeXError<T>>>}
impl<T:Token> TeXError<T> for TeXError<T> {
    fn display(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.msg {
            Some(ref msg) => write!(f,"Error in {}: {}",self.name,msg),
            None => write!(f,"Error in {}",self.name)
        }
    }
    fn token(&self) -> Option<&T> {self.cause.as_ref()}
    fn error_source(&self) -> Option<&(dyn TeXError<T>+'static)> {
        if let Some(ref s) = self.source {Some(&*s)} else {None}
    }
    fn as_error_source(&self) -> Option<&(dyn Error+'static)> {
        if let Some(ref s) = self.source {Some(&*s)} else {None}
    }
}
error_impl!(ErrorInPrimitive);

macro_rules! catch_prim {
    ($f:expr => ($name:expr,$cause:expr)) => {
        match $f {
            Ok(x) => x,
            Err(e) => return Err(ErrorInPrimitive{name:$name,msg:None,cause:Some($cause.cause),source:Some(e.into())})
        }
    }
}
pub(crate) use catch_prim;

macro_rules! file_end_prim {
    ($name:expr,$cause:expr) => {
        return Err(ErrorInPrimitive{name:$name,msg:None,cause:Some($cause.cause),source:Some(
            crate::utils::errors::FileEndedUnexpectedly{cause:None}.into()
        )})
    }
}
pub(crate) use file_end_prim;
use crate::engine::state::modes::TeXMode;

pub struct NumericalError<T:Token>(pub String,pub PhantomData<T>);
impl<T:Token> TeXError<T> for NumericalError<T> {
    fn display(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f,"{}",self.0)
    }
    fn error_source(&self) -> Option<&(dyn TeXError<T>+'static)> {None}
    fn token(&self) -> Option<&T> {None}
    fn as_error_source(&self) -> Option<&(dyn Error+'static)> {None}
}
error_impl!(NumericalError);

pub struct InitializationError<T:Token>(Box<dyn TeXError<T>>);
impl<T:Token> TeXError<T> for InitializationError<T> {
    fn display(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f,"Error during initialization")
    }
    fn error_source(&self) -> Option<&(dyn TeXError<T>+'static)> {Some(&self.0)}
    fn token(&self) -> Option<&T> {None}
    fn as_error_source(&self) -> Option<&(dyn Error+'static)> {Some(&self.0)}
}
error_impl!(InitializationError);
impl<T:Token> From<Box<dyn TeXError<T>>> for InitializationError<T> {
    fn from(e: Box<dyn TeXError<T>>) -> Self {
        InitializationError(e)
    }
}
impl<T:Token> From<NumericalError<T>> for InitializationError<T> {
    fn from(e: NumericalError<T>) -> Self {
        InitializationError(Box::new(e))
    }
}

pub struct ImplementationError<T:Token>(pub String,pub PhantomData<T>);
impl<T:Token> TeXError<T> for ImplementationError<T> {
    fn display(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f,"Implementation error: {}",self.0)
    }
    fn error_source(&self) -> Option<&(dyn TeXError<T>+'static)> {None}
    fn token(&self) -> Option<&T> {None}
    fn as_error_source(&self) -> Option<&(dyn Error+'static)> {None}
}
error_impl!(ImplementationError);

pub struct UndefinedControlSequence<T:Token>(pub TeXStr<T::Char>);
impl<T:Token> TeXError<T> for UndefinedControlSequence<T> {
    fn display(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f,"Undefined control sequence: {}",self.0)
    }
    fn token(&self) -> Option<&T> {None}
    fn error_source(&self) -> Option<&(dyn TeXError<T>+'static)> {None}
    fn as_error_source(&self) -> Option<&(dyn Error+'static)> {None}
}
error_impl!(UndefinedControlSequence);

pub struct UndefinedActiveCharacter<T:Token>(pub T::Char);
impl<T:Token> TeXError<T> for UndefinedActiveCharacter<T> {
    fn display(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f,"Undefined active character: {}",self.0)
    }
    fn token(&self) -> Option<&T> {None}
    fn error_source(&self) -> Option<&(dyn TeXError<T>+'static)> {None}
    fn as_error_source(&self) -> Option<&(dyn Error+'static)> {None}
}
error_impl!(UndefinedActiveCharacter);

pub struct InvalidCharacter<T:Token>(pub T::Char);
impl<T:Token> TeXError<T> for InvalidCharacter<T> {
    fn display(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f,"Invalid character: {}",self.0)
    }
    fn token(&self) -> Option<&T> {None}
    fn error_source(&self) -> Option<&(dyn TeXError<T>+'static)> {None}
    fn as_error_source(&self) -> Option<&(dyn Error+'static)> {None}
}
error_impl!(InvalidCharacter);

pub struct ExpectedToken<T:Token>{
    pub(crate) expected:T,
    pub(crate) found:T}
impl<T:Token> TeXError<T> for ExpectedToken<T> {
    fn display(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f,"Expected token: {}, found: {}",self.expected,self.found)
    }
    fn token(&self) -> Option<&T> {Some(&self.found)}
    fn error_source(&self) -> Option<&(dyn TeXError<T>+'static)> {None}
    fn as_error_source(&self) -> Option<&(dyn Error+'static)> {None}
}
error_impl!(ExpectedToken);

pub struct UnexpectedEndgroup<T:Token>(pub T);
impl<T:Token> TeXError<T> for UnexpectedEndgroup<T> {
    fn display(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f,"Unexpected endgroup: {}",self.0)
    }
    fn token(&self) -> Option<&T> {Some(&self.0)}
    fn error_source(&self) -> Option<&(dyn TeXError<T>+'static)> {None}
    fn as_error_source(&self) -> Option<&(dyn Error+'static)> {None}
}
error_impl!(UnexpectedEndgroup);

pub struct ExpectedInteger<T:Token>(pub T,pub PhantomData<T>);
impl<T:Token> TeXError<T> for ExpectedInteger<T> {
    fn display(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f,"Expected integer")
    }
    fn token(&self) -> Option<&T> {Some(&self.0)}
    fn error_source(&self) -> Option<&(dyn TeXError<T>+'static)> {None}
    fn as_error_source(&self) -> Option<&(dyn Error+'static)> {None}
}
error_impl!(ExpectedInteger);

pub struct ExpectedUnit<T:Token>(pub PhantomData<T>);
impl<T:Token> TeXError<T> for ExpectedUnit<T> {
    fn display(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f,"Expected unit")
    }
    fn token(&self) -> Option<&T> {None}
    fn error_source(&self) -> Option<&(dyn TeXError<T>+'static)> {None}
    fn as_error_source(&self) -> Option<&(dyn Error+'static)> {None}
}
error_impl!(ExpectedUnit);

pub struct WithToken<T:Token>(pub T,pub Box<dyn TeXError<T>>);
impl<T:Token> TeXError<T> for WithToken<T> {
    fn display(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f,"{}",self.1)
    }
    fn token(&self) -> Option<&T> {Some(&self.0)}
    fn error_source(&self) -> Option<&(dyn TeXError<T>+'static)> {self.1.error_source()}
    fn as_error_source(&self) -> Option<&(dyn Error+'static)> {self.1.as_error_source()}
    fn throw_string(&self) -> String {
        let mut ret = String::new();
        match self.0.sourceref_trace() {
            Some(trace) => {
                ret.push_str(format!("{}: {} - {}",self.1,self.0,trace).as_str());
            }
            None => {
                ret.push_str(format!("{}: {}",self.1,self.0).as_str());
            }
        }
        ret.push_str(format!("\n  caused by: {}",self.1.throw_string()).as_str());
        ret
    }
}
error_impl!(WithToken);

#[macro_export]
macro_rules! catch {
    ($f:expr => $tk:expr) => {
        match $f {
            Ok(x) => x,
            Err(e) => return Err($crate::utils::errors::WithToken($tk,e.into()).into())
        }
    };
    ($f:expr =>? $tk:expr) => {
        match $f {
            Ok(x) => x,
            Err(e) => match $tk {
                None => return Err(e.into()),
                Some(tk) => return Err($crate::utils::errors::WithToken(tk,e.into()).into())
            }
        }
    };
}
 */