use crate::debug_log;
use crate::engine::EngineType;
use crate::engine::gullet::Gullet;
use crate::engine::state::State;
use crate::tex::commands::{Command, Def, ExpToken, ParamToken, StomachCommand, StomachCommandInner};
use crate::tex::token::{BaseToken, Token, TokenList};
use crate::utils::errors::{catch_prim, ErrorInPrimitive, file_end_prim, TeXError, UnexpectedEndgroup};
use crate::engine::mouth::Mouth;
use crate::tex::catcodes::CategoryCode;
use crate::utils::Ptr;

#[macro_export]
macro_rules! cmtodo {
    ($state:ident,$stomach:ident,$gullet:ident,$name:ident) => {
        register_gullet!($name,$state,$stomach,$gullet,(_,g,_) => todo!("{}: {}",stringify!($name),g.mouth().file_line()));
    };
}

#[macro_export]
macro_rules! cmstodo {
    ($state:ident,$stomach:ident,$gullet:ident,$name:ident) => {
        register_stomach!($name,$state,$stomach,$gullet,(_,g,_,_,_) => todo!("{}: {}",stringify!($name),g.mouth().file_line()));
    };
}

#[macro_export]
macro_rules! map_group {
    ($name:expr,$cmd:ident,$state:ident,$mouth:expr,$finish:expr,$tk:ident => $f:expr) => {
        if let Some((tk,b)) = catch_prim!($mouth.get_next::<ET>($state) => ($name,$cmd)) {
            match tk.catcode() {
                CategoryCode::BeginGroup => (),
                _ => return Err(
                    ErrorInPrimitive{name:$name,msg:None,cause:Some($cmd.cause),source:Some(
                        crate::utils::errors::ExpectedToken{expected:ET::Token::new(BaseToken::Char(ET::Char::from(b'{'),CategoryCode::BeginGroup),None),found:tk}.into()
                    )}
                )
            }
        }
        let mut depth = 1;
        while let Some(($tk,_)) = catch_prim!($mouth.get_next::<ET>($state) => ($name,$cmd)) {
            match $tk.catcode() {
                CategoryCode::BeginGroup => {
                    depth += 1;
                    $f;
                }
                CategoryCode::EndGroup => {
                    depth -= 1;
                    if depth == 0 {
                        $finish
                    }
                    if depth < 0 {
                        return Err(
                        ErrorInPrimitive{name:$name,msg:None,cause:Some($cmd.cause),source:Some(
                            crate::utils::errors::UnexpectedEndgroup($tk).into()
                        )})
                    }
                    $f;
                },
                _ => $f
            }
        }
    }
}

pub fn parse_signature<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:StomachCommand<ET::Token>,name:&'static str)
    -> Result<(bool,u8,Vec<ParamToken<ET::Token>>),ErrorInPrimitive<ET::Token>> {
    let mouth = gullet.mouth();
    let mut arity : u8 = 0;
    let mut params : Vec<ParamToken<ET::Token>> = vec!();
    while let Some((next,_)) = catch_prim!(mouth.get_next::<ET>(state) => (name,cmd)) {
        match next.base() {
            BaseToken::Char(_,CategoryCode::Parameter) => {
                match catch_prim!(mouth.get_next::<ET>(state) => (name,cmd)) {
                    None => file_end_prim!(name,cmd),
                    Some((next,_)) => {
                        match next.base() {
                            BaseToken::Char(_,CategoryCode::BeginGroup) => {
                                mouth.requeue(next);
                                return Ok((true, arity, params))
                            }
                            BaseToken::Char(c,_) => {
                                arity += 1;
                                let u = c.to_usize();
                                if u < 48 || u - 48 != (arity as usize) {
                                    return Err(ErrorInPrimitive{name,msg:Some(format!("Expected parameter number {}, got {}",arity,next)),cause:Some(cmd.cause),source:None})
                                }
                                params.push(ParamToken::Param);
                            }
                            _ =>
                                return Err(ErrorInPrimitive{name,msg:Some(format!("Expected parameter number {}, got {}",arity,next)),cause:Some(cmd.cause),source:None})
                        }
                    }
                }
            }
            BaseToken::Char(_,CategoryCode::BeginGroup) => {
                mouth.requeue(next);
                return Ok((false,arity,params))
            }
            BaseToken::Char(_,CategoryCode::EndGroup) =>
                return Err(ErrorInPrimitive{name,msg:None,cause:Some(cmd.cause),source:Some(UnexpectedEndgroup(next).into())}),
            _ => params.push(ParamToken::Token(next))
        }
    }
    file_end_prim!(name,cmd)
}


#[macro_export]
macro_rules! register_int {
    ($name:ident,$state:ident,$stomach:ident,$gullet:ident,($s:tt,$gu:tt,$c:tt) => $f:expr) => {
        $state.set_command(ET::Char::from_str(stringify!($name)),Some(Ptr::new(crate::tex::commands::Command::Value{
            name:stringify!($name),
            tp:crate::tex::commands::Assignable::Int,
            index:$gullet.register_primitive_int(stringify!($name),|$s,$gu,$c| $f)
        })),true);
    };
}

#[macro_export]
macro_rules! register_dim {
    ($name:ident,$state:ident,$stomach:ident,$gullet:ident,($s:tt,$gu:tt,$c:tt) => $f:expr) => {
        $state.set_command(ET::Char::from_str(stringify!($name)),Some(Ptr::new(crate::tex::commands::Command::Value{
            name:stringify!($name),
            tp:crate::tex::commands::Assignable::Dim,
            index:$gullet.register_primitive_dim(stringify!($name),|$s,$gu,$c| $f)
        })),true);
    };
}

#[macro_export]
macro_rules! register_skip {
    ($name:ident,$state:ident,$stomach:ident,$gullet:ident,($s:tt,$gu:tt,$c:tt) => $f:expr) => {
        $state.set_command(ET::Char::from_str(stringify!($name)),Some(Ptr::new(crate::tex::commands::Command::Value{
            name:stringify!($name),
            tp:crate::tex::commands::Assignable::Skip,
            index:$gullet.register_primitive_skip(stringify!($name),|$s,$gu,$c| $f)
        })),true);
    };
}

#[macro_export]
macro_rules! register_muskip {
    ($name:ident,$state:ident,$stomach:ident,$gullet:ident,($s:tt,$gu:tt,$c:tt) => $f:expr) => {
        $state.set_command(ET::Char::from_str(stringify!($name)),Some(Ptr::new(crate::tex::commands::Command::Value{
            name:stringify!($name),
            tp:crate::tex::commands::Assignable::MuSkip,
            index:$gullet.register_primitive_muskip(stringify!($name),|$s,$gu,$c| $f)
        })),true);
    };
}

#[macro_export]
macro_rules! register_int_assign {
    ($name:ident,$state:ident,$stomach:ident,$gullet:ident) => {
        $state.set_command(ET::Char::from_str(stringify!($name)),Some(Ptr::new(crate::tex::commands::Command::AssignableValue{
            name:stringify!($name),
            tp:crate::tex::commands::Assignable::Int
        })),true);
    };
}

#[macro_export]
macro_rules! register_dim_assign {
    ($name:ident,$state:ident,$stomach:ident,$gullet:ident) => {
        $state.set_command(ET::Char::from_str(stringify!($name)),Some(Ptr::new(crate::tex::commands::Command::AssignableValue{
            name:stringify!($name),
            tp:crate::tex::commands::Assignable::Dim
        })),true);
    };
}
#[macro_export]
macro_rules! register_skip_assign {
    ($name:ident,$state:ident,$stomach:ident,$gullet:ident) => {
        $state.set_command(ET::Char::from_str(stringify!($name)),Some(Ptr::new(crate::tex::commands::Command::AssignableValue{
            name:stringify!($name),
            tp:crate::tex::commands::Assignable::Skip
        })),true);
    };
}
#[macro_export]
macro_rules! register_muskip_assign {
    ($name:ident,$state:ident,$stomach:ident,$gullet:ident) => {
        $state.set_command(ET::Char::from_str(stringify!($name)),Some(Ptr::new(crate::tex::commands::Command::AssignableValue{
            name:stringify!($name),
            tp:crate::tex::commands::Assignable::MuSkip
        })),true);
    };
}

#[macro_export]
macro_rules! register_tok_assign {
    ($name:ident,$state:ident,$stomach:ident,$gullet:ident) => {
        $state.set_command(ET::Char::from_str(stringify!($name)),Some(Ptr::new(crate::tex::commands::Command::AssignableValue{
            name:stringify!($name),
            tp:crate::tex::commands::Assignable::Toks
        })),true);
    };
}

#[macro_export]
macro_rules! register_whatsit {
    ($name:ident,$state:ident,$stomach:ident,$gullet:ident,($s:tt,$gu:tt,$sto:tt,$cmd:tt) => $f:expr) => {
        $state.set_command(ET::Char::from_str(stringify!($name)),Some(Ptr::new(crate::tex::commands::Command::Whatsit{
            name:stringify!($name),
            index:$stomach.register_whatsit(stringify!($name),|$s,$gu,$sto,$cmd| $f)
        })),true);
    };
}


#[macro_export]
macro_rules! register_open_box {
    ($name:ident,$state:ident,$stomach:ident,$gullet:ident,$tp:expr,($s:tt,$gu:tt,$sto:tt,$cmd:tt) => $f:expr) => {
        $state.set_command(ET::Char::from_str(stringify!($name)),Some(Ptr::new(crate::tex::commands::Command::OpenBox{
            name:stringify!($name),
            mode:$tp,
            index:$stomach.register_open_box(stringify!($name),|$s,$gu,$sto,$cmd| $f)
        })),true);
    };
}

#[macro_export]
macro_rules! register_stomach {
    ($name:ident,$state:ident,$stomach:ident,$gullet:ident,($s:tt,$gu:tt,$sto:tt,$cmd:tt,$b:tt) => $f:expr) => {
        $state.set_command(ET::Char::from_str(stringify!($name)),Some(Ptr::new(crate::tex::commands::Command::Stomach{
            name:stringify!($name),
            index:$stomach.register_primitive(stringify!($name),|$s,$gu,$sto,$cmd,$b| $f)
        })),true);
    };
}

#[macro_export]
macro_rules! register_gullet {
    ($name:ident,$state:ident,$stomach:ident,$gullet:ident,($s:tt,$gu:tt,$cmd:tt) => $f:expr) => {
        $state.set_command(ET::Char::from_str(stringify!($name)),Some(Ptr::new(crate::tex::commands::Command::Gullet{
            name:stringify!($name),
            index:$gullet.register_primitive(stringify!($name),|$s,$gu,$cmd| $f)
        })),true);
    };
}

#[macro_export]
macro_rules! register_conditional {
    ($name:ident,$state:ident,$stomach:ident,$gullet:ident,($s:tt,$gu:tt,$cmd:tt) => $f:expr) => {
        $state.set_command(ET::Char::from_str(stringify!($name)),Some(Ptr::new(crate::tex::commands::Command::Conditional{
            name:stringify!($name),
            index:$gullet.register_conditional(stringify!($name),|$s,$gu,$cmd| $f)
        })),true);
    };
}

#[macro_export]
macro_rules! register_assign {
    ($name:ident,$state:ident,$stomach:ident,$gullet:ident,($s:tt,$gu:tt,$sto:tt,$cmd:tt,$b:tt) => $f:expr) => {
        $state.set_command(ET::Char::from_str(stringify!($name)),Some(Ptr::new(crate::tex::commands::Command::Assignment{
            name:stringify!($name),
            index:$stomach.register_primitive(stringify!($name),|$s,$gu,$sto,$cmd,$b| $f)
        })),true);
    };
}

#[macro_export]
macro_rules! register_value_assign_int {
    ($name:ident,$state:ident,$stomach:ident,$gullet:ident) => {paste::paste!{
        $state.set_command(ET::Char::from_str(stringify!($name)),Some(Ptr::new(crate::tex::commands::Command::ValueAssignment{
            name:stringify!($name),
            assignment_index:$stomach.register_primitive(stringify!($name),|s,gu,_,cmd,b| [<$name _assign>]::<ET>(s,gu,cmd,b) ),
            value_index:$gullet.register_primitive_int(stringify!($name),|s,gu,cmd| [<$name _get>]::<ET>(s,gu,cmd)),
            tp:Assignable::Int
        })),true);
    }};
}

#[macro_export]
macro_rules! register_value_assign_font {
    ($name:ident,$state:ident,$stomach:ident,$gullet:ident) => {paste::paste!{
        $state.set_command(ET::Char::from_str(stringify!($name)),Some(Ptr::new(crate::tex::commands::Command::ValueAssignment{
            name:stringify!($name),
            assignment_index:$stomach.register_primitive(stringify!($name),|s,gu,_,cmd,b| [<$name _assign>]::<ET>(s,gu,cmd,b) ),
            value_index:$gullet.register_primitive_font(stringify!($name),|s,gu,cmd| [<$name _get>]::<ET>(s,gu,cmd)),
            tp:Assignable::Font
        })),true);
    }};
}

#[macro_export]
macro_rules! register_value_assign_dim {
    ($name:ident,$state:ident,$stomach:ident,$gullet:ident) => {paste::paste!{
        $state.set_command(ET::Char::from_str(stringify!($name)),Some(Ptr::new(crate::tex::commands::Command::ValueAssignment{
            name:stringify!($name),
            assignment_index:$stomach.register_primitive(stringify!($name),|s,gu,_,cmd,b| [<$name _assign>]::<ET>(s,gu,cmd,b) ),
            value_index:$gullet.register_primitive_dim(stringify!($name),|s,gu,cmd| [<$name _get>]::<ET>(s,gu,cmd)),
            tp:Assignable::Dim
        })),true);
    }};
}

#[macro_export]
macro_rules! register_value_assign_skip {
    ($name:ident,$state:ident,$stomach:ident,$gullet:ident) => {paste::paste!{
        $state.set_command(ET::Char::from_str(stringify!($name)),Some(Ptr::new(crate::tex::commands::Command::ValueAssignment{
            name:stringify!($name),
            assignment_index:$stomach.register_primitive(stringify!($name),|s,gu,_,cmd,b| [<$name _assign>]::<ET>(s,gu,cmd,b) ),
            value_index:$gullet.register_primitive_skip(stringify!($name),|s,gu,cmd| [<$name _get>]::<ET>(s,gu,cmd)),
            tp:Assignable::Skip
        })),true);
    }};
}

#[macro_export]
macro_rules! register_value_assign_muskip {
    ($name:ident,$state:ident,$stomach:ident,$gullet:ident) => {paste::paste!{
        $state.set_command(ET::Char::from_str(stringify!($name)),Some(Ptr::new(crate::tex::commands::Command::ValueAssignment{
            name:stringify!($name),
            assignment_index:$stomach.register_primitive(stringify!($name),|s,gu,_,cmd,b| [<$name _assign>]::<ET>(s,gu,cmd,b) ),
            value_index:$gullet.register_primitive_muskip(stringify!($name),|s,gu,cmd| [<$name _get>]::<ET>(s,gu,cmd)),
            tp:Assignable::MuSkip
        })),true);
    }};
}

pub fn assign_primitive_int<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:StomachCommand<ET::Token>,name:&'static str, global:bool) -> Result<(),ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"Assigning {}",name);
    catch_prim!(gullet.mouth().skip_eq_char::<ET>(state) => (name,cmd));
    let i = catch_prim!(gullet.get_int(state) => (name,cmd));
    debug_log!(debug=>"\\{} = {}",name,i);
    state.set_primitive_int(name,i,global);
    Ok(())
}
pub fn assign_primitive_dim<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:StomachCommand<ET::Token>,name:&'static str,global:bool) -> Result<(),ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"Assigning {}",name);
    catch_prim!(gullet.mouth().skip_eq_char::<ET>(state) => (name,cmd));
    let d = catch_prim!(gullet.get_dim(state) => (name,cmd));
    debug_log!(debug=>"\\{} = {}",name,d);
    state.set_primitive_dim(name,d,global);
    Ok(())
}
pub fn assign_primitive_skip<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:StomachCommand<ET::Token>,name:&'static str,global:bool) -> Result<(),ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"Assigning {}",name);
    catch_prim!(gullet.mouth().skip_eq_char::<ET>(state) => (name,cmd));
    let d = catch_prim!(gullet.get_skip(state) => (name,cmd));
    debug_log!(debug=>"\\{} = {}",name,d);
    state.set_primitive_skip(name,d,global);
    Ok(())
}

pub fn assign_primitive_muskip<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:StomachCommand<ET::Token>,name:&'static str,global:bool) -> Result<(),ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"Assigning {}",name);
    catch_prim!(gullet.mouth().skip_eq_char::<ET>(state) => (name,cmd));
    let d = catch_prim!(gullet.get_muskip(state) => (name,cmd));
    debug_log!(debug=>"\\{} = {}",name,d);
    state.set_primitive_muskip(name,d,global);
    Ok(())
}
pub fn assign_primitive_toks<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:StomachCommand<ET::Token>,name:&'static str,global:bool) -> Result<(),ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"Setting {}",name);
    catch_prim!(gullet.mouth().skip_eq_char::<ET>(state) => (name,cmd));
    match catch_prim!(gullet.get_next_stomach_command(state) => (name,cmd)) {
        Some(StomachCommand{cmd:StomachCommandInner::BeginGroup(_),..}) => (),
        _ => return Err(ErrorInPrimitive{
            name,msg:Some("Begin group token expected".to_string()),cause:Some(cmd.cause),source:None
        })
    }
    let tks = catch_prim!(gullet.mouth().read_until_endgroup::<ET>(state) => (name,cmd));
    debug_log!(debug=>"\\{} = {:?}",name,TokenList(tks.clone()));
    state.set_primitive_toks(name,tks,global);
    Ok(())
}

use crate::utils::errors::ErrorInDef;
use crate::utils::strings::CharType;

macro_rules! catch_def {
    ($f:expr => ($def:ident,$cause:ident)) => {
        match $f {
            Ok(x) => x,
            Err(e) => return Err(ErrorInDef{def:$def.clone(),cause:(*$cause).clone(),source:e.into()}.into())
        }
    }
}
macro_rules! file_end_def {
    ($def:ident,$cause:ident) => {
        return Err(ErrorInDef{def:$def.clone(),cause:(*$cause).clone(),source:crate::utils::errors::FileEndedUnexpectedly{cause:None}.into()}.into())
    }
}
macro_rules! expected_def {
    ($def:ident,$cause:ident,$expected:expr,$got:expr) => {
        return Err(ErrorInDef{def:$def.clone(),cause:(*$cause).clone(),source:crate::utils::errors::ExpectedToken{expected:$expected.clone(),found:$got}.into()}.into())
    }
}

pub fn exand_def<ET:EngineType>(d: &Def<ET::Token>, state:&ET::State, mouth:&mut ET::Mouth, cmd:Ptr<Command<ET::Token>>, cause:Ptr<ET::Token>)
    -> Result<Vec<ET::Token>,Box<dyn TeXError<ET::Token>>> {
    debug_log!(debug=>"Expanding {}:{:?}\n - {}",cause,d,mouth.preview(150).replace("\n","\\n"));
    // The simplest cases are covered first. Technically, the general case covers these as well,
    // but it might be more efficient to do them separately (TODO: check whether that makes a difference)
    if d.signature.is_empty() { // => arity=0
        // No arguments, we just expand the replacement, replacing `##` with `#`
        return Ok(expand_simple(d,cause,cmd))
    }
    if d.arity == 0 {
        // No arguments, we just expand the replacement, but need to eat the delimiters in the signature
        for elem in &d.signature {
            match elem {
                ParamToken::Token(delim) => {
                    if let Some((n,_)) = catch_def!(mouth.get_next::<ET>(state) => (d,cause)) {
                        if n != *delim {
                            expected_def!(d,cause,delim,n)
                        }
                    } else {
                        file_end_def!(d,cause)
                    }
                }
                _=> unreachable!() // since arity=0, there can only be tokens
            }
        }
        return Ok(expand_simple(d,cause,cmd))
    }

    // The general case:
    // We parse the arguments according to the signature
    let args = catch_def!(read_arguments::<ET>(d,mouth,state,&cause) => (d,cause));

    // Now we have all the arguments, so we can expand the replacement
    Ok(replace(d,args,cause,cmd))
}

fn expand_simple<T:Token>(d:&Def<T>, token:Ptr<T>, cmd:Ptr<Command<T>>) -> Vec<T> {
    let mut result = Vec::with_capacity(d.replacement.len());
    for r in &d.replacement {
        match r {
            ExpToken::Token(t) => result.push(t.with_ref(&token,&cmd)),
            ExpToken::ParamToken(t) => result.push(t.with_ref(&token,&cmd)),
            _ => unreachable!()
        }
    }
    result
}

fn read_arguments<ET:EngineType>(d:&Def<ET::Token>, mouth:&mut ET::Mouth, state:&ET::State, cause:&ET::Token)
    -> Result<Vec<Vec<ET::Token>>,Box<dyn TeXError<ET::Token>>> {
    let mut args : Vec<Vec<ET::Token>> = Vec::with_capacity(d.arity as usize);
    let mut iter = d.signature.iter().peekable();
    while let Some(next) = iter.next() {
        match next {
            ParamToken::Token(delim) => { // eat the delimiter
                if let Some((n,_)) = catch_def!(mouth.get_next::<ET>(state) => (d,cause)) {
                    if n != *delim {
                        expected_def!(d,cause,delim,n)
                    }
                } else {
                    file_end_def!(d,cause)
                }
            }
            ParamToken::Param => match iter.peek() { // read an argument
                None if d.endswithbrace => {// read until `{`
                    let mut arg = vec!();
                    'L: loop {
                        match if d.long {catch_def!({mouth.get_next::<ET>(state)} => (d,cause))}
                        else {catch_def!({mouth.get_next_nopar::<ET>(state)} => (d,cause))} {
                            Some((t,_)) => {
                                if t.catcode() == CategoryCode::BeginGroup {
                                    mouth.requeue(t);
                                    break 'L;
                                } else {
                                    arg.push(t);
                                }
                            }
                            None => file_end_def!(d,cause)
                        }
                    }
                    args.push(arg)
                }
                None | Some(ParamToken::Param) => { // undelimited argument
                    catch_def!(mouth.skip_whitespace::<ET>(state) => (d,cause));
                    let arg = if d.long {catch_def!(mouth.read_argument::<ET>(state) => (d,cause))}
                        else {catch_def!(mouth.read_argument_nopar::<ET>(state) => (d,cause))};
                    args.push(arg);
                },
                Some(ParamToken::Token(_)) => { // delimited argument
                    let mut delims = vec!();
                    while let Some(ParamToken::Token(t)) = iter.peek() {
                        delims.push(t.clone());
                        iter.next();
                    }
                    let mut arg = vec!();
                    let mut removebraces: Option<i32> = None;
                    let mut depth = 0;
                    'L: loop {
                        match if d.long {catch_def!({mouth.get_next::<ET>(state)} => (d,cause))}
                        else {catch_def!({mouth.get_next_nopar::<ET>(state)} => (d,cause))} {
                            Some((t,_)) if t.catcode() == CategoryCode::BeginGroup => {
                                depth += 1;
                                if arg.len() == 0 {
                                    removebraces = Some(-1);
                                }
                                arg.push(t);
                            }
                            Some((t,_)) if t.catcode() == CategoryCode::EndGroup => {
                                if depth == 0 {
                                    return Err(ErrorInDef{def:d.clone(),cause:(*cause).clone(),source:crate::utils::errors::UnexpectedEndgroup(t).into()}.into())
                                } else {
                                    depth -= 1;
                                    arg.push(t);
                                    if depth == 0 {
                                        match removebraces {
                                            Some(-1) => removebraces = Some(arg.len() as i32),
                                            _ => ()
                                        }
                                    }
                                    if depth == 0 && arg.ends_with(delims.as_slice()) {
                                        for _ in 0..delims.len() {
                                            arg.pop();
                                        }
                                        break 'L;
                                    }
                                }
                            }
                            Some((t,_)) => {
                                arg.push(t);
                                if depth == 0 && arg.ends_with(delims.as_slice()) {
                                    for _ in 0..delims.len() {
                                        arg.pop();
                                    }
                                    break 'L;
                                }
                            }
                            None => file_end_def!(d,cause)
                        }
                    }
                    match removebraces {
                        Some(i) if i != -1 && arg.len()  == (i as usize) => {
                            arg.remove(0);
                            arg.pop();
                        }
                        _ => ()
                    }
                    args.push(arg);
                }
            }
        }
    }
    Ok(args)
}

fn replace<T:Token>(d:&Def<T>, args:Vec<Vec<T>>, cause:Ptr<T>, cmd:Ptr<Command<T>>) -> Vec<T> {
    #[cfg(debug_assertions)]
    {
        debug_log!(debug=>"Arguments:");
        for i in &args {
            debug_log!(debug=>"  - {}",TokenList(i.clone()));
        }
    }
    let mut result: Vec<T> = vec!();
    let mut replacement = d.replacement.iter();
    while let Some(next) = replacement.next() {
        match next {
            ExpToken::Param(_,idx) => {
                let argls:&Vec<T> = unsafe { args.get_unchecked(*idx as usize) }; // safe because otherwise `\def` would have failed
                for t in argls {
                    result.push(t.with_ref(&cause, &cmd))
                }
            }
            ExpToken::ParamToken(t) => result.push(t.with_ref(&cause, &cmd)),
            ExpToken::Token(t) => result.push(t.with_ref(&cause, &cmd))
        }
    }
/*
        match next.catcode() {
            CategoryCode::Parameter => { // #i or ## => replace by argument
                let numorpar = unsafe{replacement.next().unwrap_unchecked()}; // safe because otherwise `\def` would have failed
                match numorpar.catcode() {
                    CategoryCode::Parameter => // `##` => `#`
                        result.push(numorpar.with_ref(&cause, &cmd)),
                    _ => match numorpar.base() { // #i
                        BaseToken::Char(c, _) => {
                            let idx:usize = c.to_usize() - 49; // argument index
                            let argls:&Vec<T> = unsafe { args.get_unchecked(idx) }; // safe because otherwise `\def` would have failed
                            for t in argls {
                                result.push(t.with_ref(&cause, &cmd))
                            }
                        }
                        _ => unreachable!() // otherwise, `\def` would have failed
                    }
                }
            }
            _ => result.push(next.with_ref(&cause, &cmd))
        }
    } */

    result
}