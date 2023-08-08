use std::hint::unreachable_unchecked;
use crate::{catch, catch_prim, debug_log, file_end, file_end_prim, throw};
use crate::engine::EngineType;
use crate::engine::gullet::Gullet;
use crate::engine::gullet::numeric_methods::expand_until_space;
use crate::engine::state::State;
use crate::tex::commands::{Command, BaseCommand, Def, ExpToken, ParamToken, ResolvedToken, TokenCont, ValueCommand, CommandSource};
use crate::tex::token::{BaseToken, Token, TokenList};
use crate::engine::mouth::Mouth;
use crate::tex::catcodes::CategoryCode;
use crate::utils::strings::CharType;

#[macro_export]
macro_rules! cmtodo {
    ($state:ident,$stomach:ident,$gullet:ident,$name:ident) => {
        register_expandable!($name,$state,$stomach,$gullet,(_,g,_,_) => todo!("{}: {}",stringify!($name),g.mouth().file_line()));
    };
}

#[macro_export]
macro_rules! cmstodo {
    ($state:ident,$stomach:ident,$gullet:ident,$name:ident) => {
        register_unexpandable!($name,$state,$stomach,$gullet,(_,g,_) => todo!("{}: {}",stringify!($name),g.mouth().file_line()));
    };
}

pub fn set_int_register<ET:EngineType>(state:&mut ET::State, gullet:&mut ET::Gullet, u:usize, cmd:CommandSource<ET>, global:bool)
                                       -> Result<(),TeXError<ET::Token>> {
    debug_log!(trace=>"Assigning \\count{}",u);
    catch!(gullet.mouth().skip_eq_char::<ET>(state) => cmd.cause);
    let v = catch!(gullet.get_int(state) => cmd.cause);
    debug_log!(debug=>"\\count{} = {}",u,v);
    state.set_int_register(u,v,global);
    Ok(())
}
pub fn set_dim_register<ET:EngineType>(state:&mut ET::State, gullet:&mut ET::Gullet, u:usize, cmd:CommandSource<ET>, global:bool)
                                       -> Result<(),TeXError<ET::Token>> {
    debug_log!(trace=>"Assigning \\dimen{}",u);
    catch!(gullet.mouth().skip_eq_char::<ET>(state) => cmd.cause);
    let v = catch!(gullet.get_dim(state) => cmd.cause);
    debug_log!(debug=>"\\dimen{} = {}",u,v);
    state.set_dim_register(u,v,global);
    Ok(())
}
pub fn set_skip_register<ET:EngineType>(state:&mut ET::State, gullet:&mut ET::Gullet, u:usize, cmd:CommandSource<ET>, global:bool)
                                        -> Result<(),TeXError<ET::Token>> {
    debug_log!(trace=>"Assigning \\skip{}",u);
    catch!(gullet.mouth().skip_eq_char::<ET>(state) => cmd.cause);
    let v = catch!(gullet.get_skip(state) => cmd.cause);
    debug_log!(debug=>"\\skip{} = {}",u,v);
    state.set_skip_register(u,v,global);
    Ok(())
}
pub fn set_muskip_register<ET:EngineType>(state:&mut ET::State, gullet:&mut ET::Gullet, u:usize, cmd:CommandSource<ET>, global:bool)
                                          -> Result<(),TeXError<ET::Token>> {
    debug_log!(trace=>"Assigning \\muskip{}",u);
    catch!(gullet.mouth().skip_eq_char::<ET>(state) => cmd.cause);
    let v = catch!(gullet.get_muskip(state) => cmd.cause);
    debug_log!(debug=>"\\muskip{} = {}",u,v);
    state.set_muskip_register(u,v,global);
    Ok(())
}
pub fn set_toks_register<ET:EngineType>(state:&mut ET::State, gullet:&mut ET::Gullet, u:usize, cmd:CommandSource<ET>, global:bool)
                                        -> Result<(),TeXError<ET::Token>> {
    debug_log!(trace=>"Setting \\toks{}",u);
    catch!(gullet.mouth().skip_eq_char::<ET>(state) => cmd.cause);
    let mut tks = Vec::with_capacity(32);
    expand_until_space::<ET>(gullet,state)?;
    catch!(gullet.get_group(state,&mut |_,t| Ok(tks.push(t))) =>cmd.cause);
    debug_log!(debug=>"\\{} = {:?}",u,TokenList(&tks));
    state.set_toks_register(u,tks,global);
    Ok(())
}

pub fn set_primitive_int<ET:EngineType>(state:&mut ET::State, gullet:&mut ET::Gullet, cmd:CommandSource<ET>, name:&'static str, global:bool) -> Result<(),TeXError<ET::Token>> {
    debug_log!(trace=>"Assigning {}",name);
    catch_prim!(gullet.mouth().skip_eq_char::<ET>(state) => (name,cmd));
    let i = catch_prim!(gullet.get_int(state) => (name,cmd));
    debug_log!(debug=>"\\{} = {}",name,i);
    state.set_primitive_int(name,i,global);
    Ok(())
}
pub fn set_primitive_dim<ET:EngineType>(state:&mut ET::State, gullet:&mut ET::Gullet, cmd:CommandSource<ET>, name:&'static str, global:bool) -> Result<(),TeXError<ET::Token>> {
    debug_log!(trace=>"Assigning {}",name);
    catch_prim!(gullet.mouth().skip_eq_char::<ET>(state) => (name,cmd));
    let d = catch_prim!(gullet.get_dim(state) => (name,cmd));
    debug_log!(debug=>"\\{} = {}",name,d);
    state.set_primitive_dim(name,d,global);
    Ok(())
}
pub fn set_primitive_skip<ET:EngineType>(state:&mut ET::State, gullet:&mut ET::Gullet, cmd:CommandSource<ET>, name:&'static str, global:bool) -> Result<(),TeXError<ET::Token>> {
    debug_log!(trace=>"Assigning {}",name);
    catch_prim!(gullet.mouth().skip_eq_char::<ET>(state) => (name,cmd));
    let d = catch_prim!(gullet.get_skip(state) => (name,cmd));
    debug_log!(debug=>"\\{} = {}",name,d);
    state.set_primitive_skip(name,d,global);
    Ok(())
}

pub fn set_primitive_muskip<ET:EngineType>(state:&mut ET::State, gullet:&mut ET::Gullet, cmd:CommandSource<ET>, name:&'static str, global:bool) -> Result<(),TeXError<ET::Token>> {
    debug_log!(trace=>"Assigning {}",name);
    catch_prim!(gullet.mouth().skip_eq_char::<ET>(state) => (name,cmd));
    let d = catch_prim!(gullet.get_muskip(state) => (name,cmd));
    debug_log!(debug=>"\\{} = {}",name,d);
    state.set_primitive_muskip(name,d,global);
    Ok(())
}
pub fn set_primitive_toks<ET:EngineType>(state:&mut ET::State, gullet:&mut ET::Gullet, cmd:CommandSource<ET>, name:&'static str, global:bool) -> Result<(),TeXError<ET::Token>> {
    debug_log!(trace=>"Setting {}",name);
    catch_prim!(gullet.mouth().skip_eq_char::<ET>(state) => (name,cmd));
    let mut tks = Vec::with_capacity(32);
    expand_until_space::<ET>(gullet,state)?;
    catch_prim!(gullet.get_group(state,&mut |_,t| Ok(tks.push(t))) => (name,cmd));
    debug_log!(debug=>"\\{} = {:?}",name,TokenList(&tks));
    state.set_primitive_toks(name,tks,global);
    Ok(())
}


#[macro_export]
macro_rules! register_int {
    ($name:ident,$state:ident,$stomach:ident,$gullet:ident,($s:tt,$gu:tt,$c:tt) => $f:expr) => {
        $state.set_command(ET::Char::from_str(stringify!($name)),Some(crate::tex::commands::Command::new(crate::tex::commands::BaseCommand::Int(
            crate::tex::commands::ValueCommand::Value{
                name:stringify!($name),
                get:|$s,$gu,$c| $f
            }
        ),None)),true);
    };
}

#[macro_export]
macro_rules! register_dim {
    ($name:ident,$state:ident,$stomach:ident,$gullet:ident,($s:tt,$gu:tt,$c:tt) => $f:expr) => {
        $state.set_command(ET::Char::from_str(stringify!($name)),Some(crate::tex::commands::Command::new(crate::tex::commands::BaseCommand::Dim(
            crate::tex::commands::ValueCommand::Value{
                name:stringify!($name),
                get:|$s,$gu,$c| $f
            }
        ),None)),true);
    };
}

#[macro_export]
macro_rules! register_skip {
    ($name:ident,$state:ident,$stomach:ident,$gullet:ident,($s:tt,$gu:tt,$c:tt) => $f:expr) => {
        $state.set_command(ET::Char::from_str(stringify!($name)),Some(crate::tex::commands::Command::new(crate::tex::commands::BaseCommand::Skip(
            crate::tex::commands::ValueCommand::Value{
                name:stringify!($name),
                get:|$s,$gu,$c| $f
            }
        ),None)),true);
    };
}

#[macro_export]
macro_rules! register_muskip {
    ($name:ident,$state:ident,$stomach:ident,$gullet:ident,($s:tt,$gu:tt,$c:tt) => $f:expr) => {
        $state.set_command(ET::Char::from_str(stringify!($name)),Some(crate::tex::commands::Command::new(crate::tex::commands::BaseCommand::MuSkip(
            crate::tex::commands::ValueCommand::Value{
                name:stringify!($name),
                get:|$s,$gu,$c| $f
            }
        ),None)),true);
    };
}

#[macro_export]
macro_rules! register_int_assign {
    ($name:ident,$state:ident,$stomach:ident,$gullet:ident) => {
        $state.set_command(ET::Char::from_str(stringify!($name)),Some(crate::tex::commands::Command::new(
            crate::tex::commands::BaseCommand::Int(crate::tex::commands::ValueCommand::Primitive(stringify!(name))) /*{
            get:Ptr::new(|s,_,_| Ok(s.get_primitive_int(stringify!($name)))),
            set:Some(Ptr::new(|s,g,c,b| Ok(crate::tex::commands::methods::assign_primitive_int(s,g,c,stringify!($name),b)?))),
            name:stringify!($name),index:None
        }*/,None)),true);
    };
}

#[macro_export]
macro_rules! register_dim_assign {
    ($name:ident,$state:ident,$stomach:ident,$gullet:ident) => {
        $state.set_command(ET::Char::from_str(stringify!($name)),Some(crate::tex::commands::Command::new(
            crate::tex::commands::BaseCommand::Dim(crate::tex::commands::ValueCommand::Primitive(stringify!(name)))/*{
            get:Ptr::new(|s,_,_| Ok(s.get_primitive_dim(stringify!($name)))),
            set:Some(Ptr::new(|s,g,c,b| Ok(crate::tex::commands::methods::assign_primitive_dim(s,g,c,stringify!($name),b)?))),
            name:stringify!($name),index:None
        }*/,None)),true);
    };
}
#[macro_export]
macro_rules! register_skip_assign {
    ($name:ident,$state:ident,$stomach:ident,$gullet:ident) => {
        $state.set_command(ET::Char::from_str(stringify!($name)),Some(crate::tex::commands::Command::new(
            crate::tex::commands::BaseCommand::Skip(crate::tex::commands::ValueCommand::Primitive(stringify!(name)))/*{
            get:Ptr::new(|s,_,_| Ok(s.get_primitive_skip(stringify!($name)))),
            set:Some(Ptr::new(|s,g,c,b| Ok(crate::tex::commands::methods::assign_primitive_skip(s,g,c,stringify!($name),b)?))),
            name:stringify!($name),index:None
        }*/,None)),true);
    };
}
#[macro_export]
macro_rules! register_muskip_assign {
    ($name:ident,$state:ident,$stomach:ident,$gullet:ident) => {
        $state.set_command(ET::Char::from_str(stringify!($name)),Some(crate::tex::commands::Command::new(
            crate::tex::commands::BaseCommand::MuSkip(crate::tex::commands::ValueCommand::Primitive(stringify!(name)))/*{
            get:Ptr::new(|s,_,_| Ok(s.get_primitive_muskip(stringify!($name)))),
            set:Some(Ptr::new(|s,g,c,b| Ok(crate::tex::commands::methods::assign_primitive_muskip(s,g,c,stringify!($name),b)?))),
            name:stringify!($name),index:None
        }*/,None)),true);
    };
}


#[macro_export]
macro_rules! register_tok_assign {
    ($name:ident,$state:ident,$stomach:ident,$gullet:ident) => {
        $state.set_command(ET::Char::from_str(stringify!($name)),Some(crate::tex::commands::Command::new(
            crate::tex::commands::BaseCommand::Toks(crate::tex::commands::ValueCommand::Primitive(stringify!(name)))/*{
            get:Ptr::new(|s,_,_| Ok(s.get_primitive_toks(stringify!($name)).map(|v| v.clone()).unwrap_or(vec!()))),
            set:Some(Ptr::new(|s,g,c,b| Ok(crate::tex::commands::methods::assign_primitive_toks(s,g,c,stringify!($name),b)?))),
            name:stringify!($name),index:None
        }*/,None)),true);
    };
}

#[macro_export]
macro_rules! register_whatsit {
    ($name:ident,$state:ident,$stomach:ident,$gullet:ident,($s:tt,$gu:tt,$cmd:tt) => $f:expr) => {
        $state.set_command(ET::Char::from_str(stringify!($name)),Some(crate::tex::commands::Command::new(crate::tex::commands::BaseCommand::Whatsit{
            name:stringify!($name),
            apply:|$s,$gu,$cmd| $f
        },None)),true);
    };
}


#[macro_export]
macro_rules! register_open_box {
    ($name:ident,$state:ident,$stomach:ident,$gullet:ident,$tp:expr,($s:tt,$gu:tt,$cmd:tt) => $f:expr) => {
        $state.set_command(ET::Char::from_str(stringify!($name)),Some(crate::tex::commands::Command::new(crate::tex::commands::BaseCommand::OpenBox{
            name:stringify!($name),
            mode:$tp,
            apply:|$s,$gu,$cmd| $f
        },None)),true);
    };
}

#[macro_export]
macro_rules! register_unexpandable {
    ($name:ident,$state:ident,$stomach:ident,$gullet:ident,($s:tt,$gu:tt,$cmd:tt) => $f:expr) => {
        $state.set_command(ET::Char::from_str(stringify!($name)),Some(crate::tex::commands::Command::new(crate::tex::commands::BaseCommand::Unexpandable{
            name:stringify!($name),
            apply:|$s,$gu:&mut ET::Gullet,$cmd| $f
        },None)),true);
    };
}

#[macro_export]
macro_rules! register_expandable {
    ($name:ident,$state:ident,$stomach:ident,$gullet:ident,($s:tt,$gu:tt,$cmd:tt,$tk:tt) => $f:expr) => {
        $state.set_command(ET::Char::from_str(stringify!($name)),Some(crate::tex::commands::Command::new(crate::tex::commands::BaseCommand::Expandable{
            name:stringify!($name),
            apply:|$s,$gu:&mut ET::Gullet,$cmd,$tk| $f
        },None)),true);
    };
}

#[macro_export]
macro_rules! register_conditional {
    ($name:ident,$state:ident,$stomach:ident,$gullet:ident,($s:tt,$gu:tt,$cmd:tt) => $f:expr) => {
        $state.set_command(ET::Char::from_str(stringify!($name)),Some(crate::tex::commands::Command::new(crate::tex::commands::BaseCommand::Conditional{
            name:stringify!($name),
            apply:|$s,$gu,$cmd| $f
        },None)),true);
    };
}

#[macro_export]
macro_rules! register_assign {
    ($name:ident,$state:ident,$stomach:ident,$gullet:ident,($s:tt,$gu:tt,$cmd:tt,$b:tt) => $f:expr) => {
        $state.set_command(ET::Char::from_str(stringify!($name)),Some(crate::tex::commands::Command::new(crate::tex::commands::BaseCommand::Assignment{
            name:stringify!($name),
            apply:|$s,$gu,$cmd,$b| Ok($f?)
        },None)),true);
    };
}

#[macro_export]
macro_rules! register_value_assign_int {
    ($name:ident,$state:ident,$stomach:ident,$gullet:ident) => {paste::paste!{
        $state.set_command(ET::Char::from_str(stringify!($name)),Some(crate::tex::commands::Command::new(crate::tex::commands::BaseCommand::Int(
            crate::tex::commands::ValueCommand::Complex{
                get:|s,gu,cmd| [<$name _get>]::<ET>(s,gu,cmd),
                set:|s,gu,cmd,b| Ok([<$name _assign>]::<ET>(s,gu,cmd,b)?),
                name:stringify!($name)
            }
        ),None)),true);
    }};
}

#[macro_export]
macro_rules! register_value_assign_dim {
    ($name:ident,$state:ident,$stomach:ident,$gullet:ident) => {paste::paste!{
        $state.set_command(ET::Char::from_str(stringify!($name)),Some(crate::tex::commands::Command::new(crate::tex::commands::BaseCommand::Dim(
            crate::tex::commands::ValueCommand::Complex{
                get:|s,gu,cmd| [<$name _get>]::<ET>(s,gu,cmd),
                set:|s,gu,cmd,b| Ok([<$name _assign>]::<ET>(s,gu,cmd,b)?),
                name:stringify!($name)
            }
        ),None)),true);
    }};
}

#[macro_export]
macro_rules! register_value_assign_skip {
    ($name:ident,$state:ident,$stomach:ident,$gullet:ident) => {paste::paste!{
        $state.set_command(ET::Char::from_str(stringify!($name)),Some(crate::tex::commands::Command::new(crate::tex::commands::BaseCommand::Skip(
            crate::tex::commands::ValueCommand::Complex{
                get:|s,gu,cmd| [<$name _get>]::<ET>(s,gu,cmd),
                set:|s,gu,cmd,b| Ok([<$name _assign>]::<ET>(s,gu,cmd,b)?),
                name:stringify!($name)
            }
        ),None)),true);
    }};
}

#[macro_export]
macro_rules! register_value_assign_muskip {
    ($name:ident,$state:ident,$stomach:ident,$gullet:ident) => {paste::paste!{
        $state.set_command(ET::Char::from_str(stringify!($name)),Some(crate::tex::commands::Command::new(crate::tex::commands::BaseCommand::MuSkip(
            crate::tex::commands::ValueCommand::Complex{
                get:|s,gu,cmd| [<$name _get>]::<ET>(s,gu,cmd),
                set:|s,gu,cmd,b| Ok([<$name _assign>]::<ET>(s,gu,cmd,b)?),
                name:stringify!($name)
            }
        ),None)),true);
    }};
}

#[macro_export]
macro_rules! register_value_assign_toks {
    ($name:ident,$state:ident,$stomach:ident,$gullet:ident) => {paste::paste!{
        $state.set_command(ET::Char::from_str(stringify!($name)),Some(crate::tex::commands::Command::new(crate::tex::commands::BaseCommand::Toks(
            crate::tex::commands::ValueCommand::Complex{
                get:|s,gu,cmd| [<$name _get>]::<ET>(s,gu,cmd),
                set:|s,gu,cmd,b| Ok([<$name _assign>]::<ET>(s,gu,cmd,b)?),
                name:stringify!($name)
            }
        ),None)),true);
    }};
}


#[macro_export]
macro_rules! register_value_assign_font {
    ($name:ident,$state:ident,$stomach:ident,$gullet:ident) => {paste::paste!{
        $state.set_command(ET::Char::from_str(stringify!($name)),Some(crate::tex::commands::Command::new(crate::tex::commands::BaseCommand::FontCommand{
            get:|s,gu,cmd| [<$name _get>]::<ET>(s,gu,cmd),
            set:Some(|s,gu,cmd,b| Ok([<$name _assign>]::<ET>(s,gu,cmd,b)?)),
            name:stringify!($name)
        },None)),true);
    }};
}

/// Expands the [`Def`] into a [`Vec`] of [`Token`]s. `cmd` and `cause` are the command and
/// token that triggered the expansion, used for constructing the
/// [`SourceReference`](crate::tex::token::SourceReference)s of the returned [`Token`]s and
/// error messages.
pub fn expand_def<ET:EngineType>(d: &Def<ET::Token>, state:&mut ET::State, mouth:&mut ET::Mouth, cmd:CommandSource<ET>,pool:(&mut[Vec<ET::Token>;9],&mut Vec<ET::Token>), f:TokenCont<ET>)
                                 -> Result<(),TeXError<ET::Token>> {
    debug_log!(debug=>"Expanding {}:{:?}\n - {}",cmd.cause,d,mouth.preview(250).replace("\n","\\n"));
    // The simplest cases are covered first. Technically, the general case covers these as well,
    // but it might be more efficient to do them separately (TODO: check whether that makes a difference)
    if d.signature.is_empty() { // => arity=0
        // No arguments, we just expand the replacement, replacing `##` with `#`
        return expand_simple(d,cmd,state,f)
    }
    if d.arity == 0 {
        // No arguments, we just expand the replacement, but need to eat the delimiters in the signature
        for elem in &d.signature {
            match elem {
                ParamToken::Token(delim) => {
                    if let Some((n,_)) = catch!(mouth.get_next::<ET>(state) => cmd.cause) {
                        if n != *delim {
                            throw!("Usage of {} does not match its definition: {} expected, found {}",cmd.cause,delim,n => cmd.cause)
                        }
                    } else {
                        file_end!(cmd.cause)
                    }
                }
                _=> unsafe{ unreachable_unchecked() } // since arity=0, there can only be tokens
            }
        }
        return expand_simple(d,cmd,state,f)
    }

    /*


    // The general case:
    // We parse the arguments according to the signature
    catch_def!(read_arguments::<ET>(d,mouth,state,&cause) => (d,cause));

    // Now we have all the arguments, so we can expand the replacement
    Ok(replace(d,cause,cmd,state.pool_mut()))

     */
    let (mut args,mut delims) = pool;
    read_arguments(d, mouth, state, &cmd, (args,delims))?;
    replace(d, cmd, state, args, f)
}

fn expand_simple<ET:EngineType>(d:&Def<ET::Token>, cmd:CommandSource<ET>,state:&mut ET::State,f:TokenCont<ET>) -> Result<(),TeXError<ET::Token>> {
    for r in &d.replacement {
        match r {
            ExpToken::Token(t) => f(state,t.with_ref(&cmd))?,
            ExpToken::ParamToken(t) => f(state,t.with_ref(&cmd))?,
            _ => unreachable!()
        }
    }
    Ok(())
}

use crate::tex::numbers::{MuSkip, Skip};
use crate::utils::errors::TeXError;

fn read_arguments<'a,ET:EngineType>(d:&Def<ET::Token>, mouth:&mut ET::Mouth, state:&mut ET::State, cmd:&CommandSource<ET>,pool:(&mut[Vec<ET::Token>;9],&mut Vec<ET::Token>))
                                 -> Result<(),TeXError<ET::Token>> {
    let mut argnum = 0;
    let mut iter = d.signature.iter().peekable();
    while let Some(next) = iter.next() {
        match next {
            ParamToken::Token(delim) => { // eat the delimiter
                if let Some((n,_)) = catch!(mouth.get_next::<ET>(state) => cmd.cause.clone()) {
                    if n != *delim {
                        throw!("Usage of {} does not match its definition: {} expected, found {}",cmd.cause,delim,n => cmd.cause)
                    }
                } else {
                    file_end!(cmd.cause.clone())
                }
            }
            ParamToken::Param => match iter.peek() { // read an argument
                None if d.endswithbrace => {// read until `{`
                    let arg = &mut pool.0[argnum];
                    arg.clear();
                    argnum += 1;
                    'L: loop {
                        match if d.long {catch!({mouth.get_next::<ET>(state)} => cmd.cause.clone())}
                        else {catch!({mouth.get_next_nopar::<ET>(state)} => cmd.cause.clone())} {
                            Some((t,_)) => {
                                if t.catcode() == CategoryCode::BeginGroup {
                                    mouth.requeue(t);
                                    break 'L;
                                } else {
                                    arg.push(t);
                                }
                            }
                            None => file_end!(cmd.cause.clone())
                        }
                    }
                }
                None | Some(ParamToken::Param) => { // undelimited argument
                    let arg = &mut pool.0[argnum];
                    arg.clear();
                    argnum += 1;
                    catch!(mouth.skip_whitespace::<ET>(state) => cmd.cause.clone());
                    if d.long {catch!(mouth.read_argument::<ET>(state,&mut|_,t| Ok(arg.push(t))) => cmd.cause.clone())}
                    else {catch!(mouth.read_argument_nopar::<ET>(state,&mut|_,t| Ok(arg.push(t))) => cmd.cause.clone())};
                },
                Some(ParamToken::Token(_)) => { // delimited argument
                    let arg = &mut pool.0[argnum];
                    arg.clear();
                    argnum += 1;
                    let mut delims = &mut*pool.1;
                    delims.clear();
                    while let Some(ParamToken::Token(t)) = iter.peek() {
                        delims.push(t.clone());
                        iter.next();
                    }
                    let mut removebraces: Option<i32> = None;
                    let mut depth = 0;
                    'L: loop {
                        match if d.long {catch!({mouth.get_next::<ET>(state)} => cmd.cause.clone())}
                        else {catch!({mouth.get_next_nopar::<ET>(state)} => cmd.cause.clone())} {
                            Some((t,_)) if t.catcode() == CategoryCode::BeginGroup => {
                                depth += 1;
                                if arg.len() == 0 {
                                    removebraces = Some(-1);
                                }
                                arg.push(t);
                            }
                            Some((t,_)) if t.catcode() == CategoryCode::EndGroup => {
                                if depth == 0 {
                                    throw!("Unexpected end group token: {}",t => cmd.cause.clone())
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
                            None => file_end!(cmd.cause.clone())
                        }
                    }
                    match removebraces {
                        Some(i) if i != -1 && arg.len()  == (i as usize) => {
                            arg.remove(0);
                            arg.pop();
                        }
                        _ => ()
                    }
                }
            }
        }
    }
    Ok(())
}

fn replace<ET:EngineType>(d:&Def<ET::Token>, cmd:CommandSource<ET>, state: &mut ET::State,args:&[Vec<ET::Token>;9],f:TokenCont<ET>) -> Result<(),TeXError<ET::Token>> {
    #[cfg(debug_assertions)]
    {
        debug_log!(debug=>"Arguments:");
        for i in 0..d.arity {
            debug_log!(debug=>"  - {}",TokenList(&args[i as usize].as_vec()));
        }
    }
    let mut replacement = d.replacement.iter();
    while let Some(next) = replacement.next() {
        match next {
            ExpToken::Param(_,idx) => {
                for t in args[*idx as usize].iter() {
                    f(state,t.with_ref(&cmd))?
                }
            }
            ExpToken::ParamToken(t) => f(state,t.with_ref(&cmd))?,
            ExpToken::Token(t) => f(state,t.with_ref(&cmd))?
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

    Ok(())
}

pub fn parse_signature<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:&CommandSource<ET>,name:&'static str)
    -> Result<(bool,u8,Vec<ParamToken<ET::Token>>),TeXError<ET::Token>> {
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
                                    throw!("Expected parameter number {}, got {}",arity,next => cmd.cause)
                                }
                                params.push(ParamToken::Param);
                            }
                            _ =>
                                throw!("Expected parameter number {}, got {}",arity,next => cmd.cause)
                        }
                    }
                }
            }
            BaseToken::Char(_,CategoryCode::BeginGroup) => {
                mouth.requeue(next);
                return Ok((false,arity,params))
            }
            BaseToken::Char(_,CategoryCode::EndGroup) => throw!("Unexpected end of group" => cmd.cause),
            _ => params.push(ParamToken::Token(next))
        }
    }
    file_end_prim!(name,cmd.clone())
}


pub fn set_relax<ET:EngineType>(state:&mut ET::State,tk:&ET::Token,source:&CommandSource<ET>,globally:bool) -> Result<(),TeXError<ET::Token>> {
    match tk.base() {
        BaseToken::Char(c,CategoryCode::Active) => {
            state.set_ac_command(*c, Some(Command::new(BaseCommand::Relax,Some(source))), globally)
        }
        BaseToken::CS(name) => {
            state.set_command(name.clone(), Some(Command::new(BaseCommand::Relax,Some(source))), globally)
        }
        _ => throw!("Command name expected, got {}",tk => source.cause)
    }
    Ok(())
}
