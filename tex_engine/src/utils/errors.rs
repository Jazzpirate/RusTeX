use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::marker::PhantomData;
use crate::tex::commands::Def;
use crate::tex::token::Token;
use crate::utils::strings::TeXStr;

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

pub struct ErrorInPrimitive<T:Token>{pub name:&'static str,pub msg:Option<String>,pub cause:Option<T>,pub source:Option<Box<dyn TeXError<T>>>}
impl<T:Token> TeXError<T> for ErrorInPrimitive<T> {
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
    ($f:expr => ($name:expr,$cause:ident)) => {
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

/*

/*
pub enum TeXError<T:Token> {
    UndefinedControlSequence(TeXStr<T::Char>),
    UndefinedActiveCharacter(T::Char),
    ExpectedToken{expected:T,found:T},
    UnexpectedEndgroup(T),
    InvalidCharacter(T::Char),
    FileEndedUnexpectedly,
    IntError(&'static str)
}
 */
impl<T:Token> Debug for TeXError<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        todo!("Debug for TeXError")
    }
}
impl<T:Token> Display for TeXError<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TeXError::UndefinedControlSequence(name) => write!(f,"Undefined control sequence: {}",name),
            TeXError::UndefinedActiveCharacter(c) => write!(f,"Undefined active character: {}",c),
            TeXError::ExpectedToken{expected,found} => write!(f,"Expected {}, found {}",expected,found),
            TeXError::UnexpectedEndgroup(_) => write!(f,"Unexpected end group character"),
            TeXError::InvalidCharacter(c) => write!(f,"Invalid character {}",c),
            TeXError::FileEndedUnexpectedly => write!(f,"File ended unexpectedly"),
        }
    }
}
impl<T:Token> std::error::Error for TeXError<T> {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            TeXError::UndefinedControlSequence(_) => None,
            TeXError::UndefinedActiveCharacter(_) => None,
            TeXError::ExpectedToken{..} => None,
            TeXError::UnexpectedEndgroup(_) => None,
            TeXError::InvalidCharacter(_) => None,
            TeXError::FileEndedUnexpectedly => None,
        }
    }
}
/*
pub struct TeXError<T:Token> {
    pub msg:String,
    source:Box<Option<TeXError<T>>>,
    tk:Option<T>,
    pub textrace:Vec<(String,String)>,
    pub toplinepos:Vec<(String,usize,usize)>
}
*/

impl<T:Token> TeXError<T> {
    pub fn undefined_control_sequence(name:&TeXStr<T::Char>) -> Self {
        TeXError::UndefinedControlSequence(name.clone())//new(format!("Undefined control sequence: {}",name),None)
    }
    pub fn undefined_active_character(c: T::Char) -> Self {
        TeXError::UndefinedActiveCharacter(c)//new(format!("Undefined active character: {}",c),None)
    }
    pub fn expected_token(expected:&T, found:T) -> Self {
        TeXError::ExpectedToken {expected:expected.clone(),found}//new(format!("Expected {}, found {}",expected,found),Some(found))
    }
    pub fn unexpected_endgroup(cause:T) -> Self {
        TeXError::UnexpectedEndgroup(cause)//new(format!("Unexpected endgroup"),Some(cause))
    }
    pub fn invalid_character(c:T::Char) -> Self {
        TeXError::InvalidCharacter(c)//new(format!("Invalid character {}",c),None)
    }
    /*fn backtrace() -> Backtrace {
        let bt = Backtrace::new_unresolved();
        let mut frames = Vec::new();
        for b in bt.frames() {
            frames.push(b.clone())
        }
        frames = Vec::from(frames.get(2..).unwrap());
        Backtrace::from(frames)
    }*/
    /*pub fn new(msg:String, tk:Option<T>) -> Self {
        TeXError {msg,source:Box::new(None),tk,textrace:vec!(),toplinepos:vec!()}
    }
    pub fn derive(self,msg:String) -> Self {
        TeXError {msg,source:Box::new(Some(self)),tk:None,textrace:vec!(),toplinepos:vec!()}
    }*/
    /*pub fn throw(&mut self, int: &mut Interpreter) {
        //self.backtrace.resolve();
        self.textrace = tex_stacktrace(int,self.tk.clone());
        for x in &int.mouths.mouths {
            match x {
                Mouth::File(sm) =>
                    match &sm.source {
                        StringMouthSource::File(f) =>
                            match &f.path {
                                Some(p) =>
                                    self.toplinepos.push((p.to_string(),sm.line,sm.pos)),
                                _ => ()
                            },
                        _ => ()
                    }
                _ => ()
            }
        }
    }*/
}

/*
fn tex_stacktrace(int:&mut Interpreter,tk:Option<Token>) -> Vec<(String,String)> {
    match tk {
        None if int.has_next() => {
            let next = int.next_token();
            tex_stacktrace(int,Some(next))
        },
        None => vec!(),
        Some(tk) => {
            stacktrace(tk)
        }
    }
}
 */
/*

*/

pub fn stacktrace<T:Token>(tk : T) -> Vec<(String, String)> { /*
    let mut currtk = tk;
    let mut ret : Vec<(String,String)> = vec!();
    let mut currtkstr = "".to_string();
    let mut currline = "".to_string();
    loop {
        match currtk.catcode {
            CategoryCode::Escape => {
                currtkstr += "\\";
                currtkstr += &currtk.name().to_string()
            },
            _ => {
                currtkstr += &TeXString(vec!(currtk.char)).to_string()
            }
        }
        match &currtk.reference {
            None => {
                ret.push((std::mem::take(&mut currtkstr),"(No source information available)".to_string()));
                break
            }
            Some(r) => match &**r {
                SourceReference::File(str, (sl, sp), (el, ep)) => {
                    currline += &str.to_string();
                    currline += " (L ";
                    currline += &sl.to_string();
                    currline += ", C ";
                    currline += &sp.to_string();
                    currline += " - L ";
                    currline += &el.to_string();
                    currline += ", C ";
                    currline += &ep.to_string();
                    currline += ")";
                    ret.push((std::mem::take(&mut currtkstr), std::mem::take(&mut currline)));
                    break
                }
                SourceReference::Exp(ntk, cmd) => {
                    currline += "Expanded from ";
                    match ntk.catcode {
                        CategoryCode::Escape => {
                            currline += "\\";
                            currline += &ntk.cmdname().to_string();
                            currline += " => ";
                            currline += &cmd.meaning(&crate::catcodes::DEFAULT_SCHEME).to_string();
                        }
                        _ => ()
                    }
                    currtk = ntk.clone();
                    ret.push((std::mem::take(&mut currtkstr), std::mem::take(&mut currline)))
                }
            }
        }
    }
    ret */
    todo!()
}

 */