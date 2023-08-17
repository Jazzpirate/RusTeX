use std::hint::unreachable_unchecked;
use crate::{catch, catch_prim, debug_log, expand_until_group, file_end, file_end_prim, throw};
use crate::engine::{EngineRef, EngineType};
use crate::engine::memory::ExpansionContainer;
use crate::engine::state::State;
use crate::tex::commands::{BaseCommand, Def, ExpToken, ParamToken,  CommandSource};
use crate::tex::token::{BaseToken, Token, TokenList};
use crate::engine::mouth::Mouth;
use crate::tex::catcodes::CategoryCode;
use crate::utils::strings::CharType;
use crate::utils::errors::TeXError;

#[macro_export]
macro_rules! cmtodo {
    ($engine:ident,$name:ident) => {
        register_expandable!($name,$engine,(e,_,_) => todo!("{}: {}",stringify!($name),e.current_position()));
    };
}

#[macro_export]
macro_rules! cmstodo {
    ($engine:ident,$name:ident) => {
        register_unexpandable!($name,$engine,None,(e,_) => todo!("{}: {}",stringify!($name),e.current_position()));
    };
}

pub fn set_int_register<ET:EngineType>(engine:&mut EngineRef<ET>, u:usize, cmd:CommandSource<ET>, global:bool)
                                       -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"Assigning \\count{}",u);
    catch!(engine.skip_eq_char() => cmd.cause);
    let v = catch!(engine.get_int() => cmd.cause);
    debug_log!(debug=>"\\count{} = {}",u,v);
    engine.state.set_int_register(u,v,global);
    Ok(())
}
pub fn set_dim_register<ET:EngineType>(engine:&mut EngineRef<ET>, u:usize, cmd:CommandSource<ET>, global:bool)
                                       -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"Assigning \\dimen{}",u);
    catch!(engine.skip_eq_char() => cmd.cause);
    let v = catch!(engine.get_dim() => cmd.cause);
    debug_log!(debug=>"\\dimen{} = {}",u,v);
    engine.state.set_dim_register(u,v,global);
    Ok(())
}
pub fn set_skip_register<ET:EngineType>(engine:&mut EngineRef<ET>, u:usize, cmd:CommandSource<ET>, global:bool)
                                        -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"Assigning \\skip{}",u);
    catch!(engine.skip_eq_char() => cmd.cause);
    let v = catch!(engine.get_skip() => cmd.cause);
    debug_log!(debug=>"\\skip{} = {}",u,v);
    engine.state.set_skip_register(u,v,global);
    Ok(())
}
pub fn set_muskip_register<ET:EngineType>(engine:&mut EngineRef<ET>, u:usize, cmd:CommandSource<ET>, global:bool)
                                          -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"Assigning \\muskip{}",u);
    catch!(engine.skip_eq_char() => cmd.cause);
    let v = catch!(engine.get_muskip() => cmd.cause);
    debug_log!(debug=>"\\muskip{} = {}",u,v);
    engine.state.set_muskip_register(u,v,global);
    Ok(())
}
pub fn set_toks_register<ET:EngineType>(engine:&mut EngineRef<ET>, u:usize, cmd:CommandSource<ET>, global:bool)
                                        -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"Setting \\toks{}",u);
    catch!(engine.skip_eq_char() => cmd.cause);
    let mut tks = engine.memory.get_token_vec();
    expand_until_group!(engine,t => tks.push(t));
    //catch!(engine.expand_until_group(&mut |_,t| Ok(tks.push(t))) =>cmd.cause);
    debug_log!(debug=>"\\{} = {:?}",u,TokenList(&tks).to_str(engine.interner));
    engine.state.set_toks_register(u,tks,global,engine.memory);
    Ok(())
}

pub fn set_primitive_int<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:CommandSource<ET>, name:&'static str, global:bool) -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"Assigning {}",name);
    catch_prim!(engine.skip_eq_char() => (name,cmd));
    let i = catch_prim!(engine.get_int() => (name,cmd));
    debug_log!(debug=>"\\{} = {}",name,i);
    engine.state.set_primitive_int(name,i,global);
    Ok(())
}
pub fn set_primitive_dim<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:CommandSource<ET>, name:&'static str, global:bool) -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"Assigning {}",name);
    catch_prim!(engine.skip_eq_char() => (name,cmd));
    let d = catch_prim!(engine.get_dim() => (name,cmd));
    debug_log!(debug=>"\\{} = {}",name,d);
    engine.state.set_primitive_dim(name,d,global);
    Ok(())
}
pub fn set_primitive_skip<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:CommandSource<ET>, name:&'static str, global:bool) -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"Assigning {}",name);
    catch_prim!(engine.skip_eq_char() => (name,cmd));
    let d = catch_prim!(engine.get_skip() => (name,cmd));
    debug_log!(debug=>"\\{} = {}",name,d);
    engine.state.set_primitive_skip(name,d,global);
    Ok(())
}

pub fn set_primitive_muskip<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:CommandSource<ET>, name:&'static str, global:bool) -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"Assigning {}",name);
    catch_prim!(engine.skip_eq_char() => (name,cmd));
    let d = catch_prim!(engine.get_muskip() => (name,cmd));
    debug_log!(debug=>"\\{} = {}",name,d);
    engine.state.set_primitive_muskip(name,d,global);
    Ok(())
}
pub fn set_primitive_toks<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:CommandSource<ET>, name:&'static str, global:bool) -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"Setting {}",name);
    catch_prim!(engine.skip_eq_char() => (name,cmd));
    let mut tks = engine.memory.get_token_vec();
    expand_until_group!(engine,t => tks.push(t));
    //catch_prim!(engine.expand_until_group(&mut |_,t| Ok(tks.push(t))) => (name,cmd));
    debug_log!(debug=>"\\{} = {:?}",name,TokenList(&tks).to_str(engine.interner));
    if name == "output" {
        if !tks.is_empty() {
            tks.push(Token::new(BaseToken::Char(ET::Char::from(b'}'),CategoryCode::EndGroup),None));
            tks.insert(0,Token::new(BaseToken::Char(ET::Char::from(b'{'),CategoryCode::BeginGroup),None))
        }
    }
    engine.state.set_primitive_toks(name,tks,global,engine.memory);
    Ok(())
}


#[macro_export]
macro_rules! register_int {
    ($name:ident,$engine:ident,($e:tt,$c:tt) => $f:expr) => {
        $engine.state.set_command(ET::Char::from_str(stringify!($name),$engine.interner),Some(crate::tex::commands::Command::new(crate::tex::commands::BaseCommand::Int(
            crate::tex::commands::ValueCommand::Value{
                name:stringify!($name),
                get:|$e,$c| $f
            }
        ),None)),true);
    };
}

#[macro_export]
macro_rules! register_dim {
    ($name:ident,$engine:ident,($e:tt,$c:tt) => $f:expr) => {
        $engine.state.set_command(ET::Char::from_str(stringify!($name),$engine.interner),Some(crate::tex::commands::Command::new(crate::tex::commands::BaseCommand::Dim(
            crate::tex::commands::ValueCommand::Value{
                name:stringify!($name),
                get:|$e,$c| $f
            }
        ),None)),true);
    };
}

#[macro_export]
macro_rules! register_skip {
    ($name:ident,$engine:ident,($e:tt,$c:tt) => $f:expr) => {
        $engine.state.set_command(ET::Char::from_str(stringify!($name),$engine.interner),Some(crate::tex::commands::Command::new(crate::tex::commands::BaseCommand::Skip(
            crate::tex::commands::ValueCommand::Value{
                name:stringify!($name),
                get:|$e,$c| $f
            }
        ),None)),true);
    };
}

#[macro_export]
macro_rules! register_muskip {
    ($name:ident,$engine:ident,($e:tt,$c:tt) => $f:expr) => {
        $engine.state.set_command(ET::Char::from_str(stringify!($name),$engine.interner),Some(crate::tex::commands::Command::new(crate::tex::commands::BaseCommand::MuSkip(
            crate::tex::commands::ValueCommand::Value{
                name:stringify!($name),
                get:|$e,$c| $f
            }
        ),None)),true);
    };
}

#[macro_export]
macro_rules! register_int_assign {
    ($name:ident,$engine:ident) => {
        $engine.state.set_command(ET::Char::from_str(stringify!($name),$engine.interner),Some(crate::tex::commands::Command::new(
            crate::tex::commands::BaseCommand::Int(crate::tex::commands::ValueCommand::Primitive(stringify!($name))) /*{
            get:Ptr::new(|s,_,_| Ok(s.get_primitive_int(stringify!($name)))),
            set:Some(Ptr::new(|s,g,c,b| Ok(crate::tex::commands::methods::assign_primitive_int(s,g,c,stringify!($name),b)?))),
            name:stringify!($name),index:None
        }*/,None)),true);
    };
}

#[macro_export]
macro_rules! register_dim_assign {
    ($name:ident,$engine:ident) => {
        $engine.state.set_command(ET::Char::from_str(stringify!($name),$engine.interner),Some(crate::tex::commands::Command::new(
            crate::tex::commands::BaseCommand::Dim(crate::tex::commands::ValueCommand::Primitive(stringify!($name)))/*{
            get:Ptr::new(|s,_,_| Ok(s.get_primitive_dim(stringify!($name)))),
            set:Some(Ptr::new(|s,g,c,b| Ok(crate::tex::commands::methods::assign_primitive_dim(s,g,c,stringify!($name),b)?))),
            name:stringify!($name),index:None
        }*/,None)),true);
    };
}
#[macro_export]
macro_rules! register_skip_assign {
    ($name:ident,$engine:ident) => {
        $engine.state.set_command(ET::Char::from_str(stringify!($name),$engine.interner),Some(crate::tex::commands::Command::new(
            crate::tex::commands::BaseCommand::Skip(crate::tex::commands::ValueCommand::Primitive(stringify!($name)))/*{
            get:Ptr::new(|s,_,_| Ok(s.get_primitive_skip(stringify!($name)))),
            set:Some(Ptr::new(|s,g,c,b| Ok(crate::tex::commands::methods::assign_primitive_skip(s,g,c,stringify!($name),b)?))),
            name:stringify!($name),index:None
        }*/,None)),true);
    };
}
#[macro_export]
macro_rules! register_muskip_assign {
    ($name:ident,$engine:ident) => {
        $engine.state.set_command(ET::Char::from_str(stringify!($name),$engine.interner),Some(crate::tex::commands::Command::new(
            crate::tex::commands::BaseCommand::MuSkip(crate::tex::commands::ValueCommand::Primitive(stringify!($name)))/*{
            get:Ptr::new(|s,_,_| Ok(s.get_primitive_muskip(stringify!($name)))),
            set:Some(Ptr::new(|s,g,c,b| Ok(crate::tex::commands::methods::assign_primitive_muskip(s,g,c,stringify!($name),b)?))),
            name:stringify!($name),index:None
        }*/,None)),true);
    };
}


#[macro_export]
macro_rules! register_tok_assign {
    ($name:ident,$engine:ident) => {
        $engine.state.set_command(ET::Char::from_str(stringify!($name),$engine.interner),Some(crate::tex::commands::Command::new(
            crate::tex::commands::BaseCommand::Toks(crate::tex::commands::ToksCommand::Primitive(stringify!($name)))/*{
            get:Ptr::new(|s,_,_| Ok(s.get_primitive_toks(stringify!($name)).map(|v| v.clone()).unwrap_or(vec!()))),
            set:Some(Ptr::new(|s,g,c,b| Ok(crate::tex::commands::methods::assign_primitive_toks(s,g,c,stringify!($name),b)?))),
            name:stringify!($name),index:None
        }*/,None)),true);
    };
}

#[macro_export]
macro_rules! register_whatsit {
    ($name:ident,$engine:ident,($e:tt,$cmd:tt) => $f:expr) => {
        $engine.state.set_command(ET::Char::from_str(stringify!($name),$engine.interner),Some(crate::tex::commands::Command::new(crate::tex::commands::BaseCommand::Whatsit{
            name:stringify!($name),
            apply:|$e,$cmd| $f
        },None)),true);
    };
}


#[macro_export]
macro_rules! register_open_box {
    ($name:ident,$engine:ident,$tp:expr,($e:tt,$cmd:tt) => $f:expr) => {
        $engine.state.set_command(ET::Char::from_str(stringify!($name),$engine.interner),Some(crate::tex::commands::Command::new(crate::tex::commands::BaseCommand::OpenBox{
            name:stringify!($name),
            mode:$tp,
            apply:|$e,$cmd| $f
        },None)),true);
    };
}

#[macro_export]
macro_rules! register_unexpandable {
    ($name:ident,$engine:ident,$is_h:expr,($e:tt,$cmd:tt) => $f:expr) => {
        $engine.state.set_command(ET::Char::from_str(stringify!($name),$engine.interner),Some(crate::tex::commands::Command::new(crate::tex::commands::BaseCommand::Unexpandable{
            name:stringify!($name),
            apply:|$e,$cmd| $f,
            forces_mode:$is_h
        },None)),true);
    };
}

#[macro_export]
macro_rules! register_expandable {
    ($name:ident,$engine:ident,($e:tt,$cmd:tt,$tk:tt) => $f:expr) => {
        $engine.state.set_command(ET::Char::from_str(stringify!($name),$engine.interner),Some(crate::tex::commands::Command::new(crate::tex::commands::BaseCommand::Expandable{
            name:stringify!($name),
            apply:|$e,$cmd,$tk| $f
        },None)),true);
    };
}

#[macro_export]
macro_rules! register_conditional {
    ($name:ident,$engine:ident,($e:tt,$cmd:tt) => $f:expr) => {
        $engine.state.set_command(ET::Char::from_str(stringify!($name),$engine.interner),Some(crate::tex::commands::Command::new(crate::tex::commands::BaseCommand::Conditional{
            name:stringify!($name),
            apply:|$e,$cmd| $f
        },None)),true);
    };
}

#[macro_export]
macro_rules! register_assign {
    ($name:ident,$engine:ident,($e:tt,$cmd:tt,$b:tt) => $f:expr) => {
        $engine.state.set_command(ET::Char::from_str(stringify!($name),$engine.interner),Some(crate::tex::commands::Command::new(crate::tex::commands::BaseCommand::Assignment{
            name:stringify!($name),
            apply:|$e,$cmd,$b| Ok($f?)
        },None)),true);
    };
}

#[macro_export]
macro_rules! register_value_assign_int {
    ($name:ident,$engine:ident) => {paste::paste!{
        $engine.state.set_command(ET::Char::from_str(stringify!($name),$engine.interner),Some(crate::tex::commands::Command::new(crate::tex::commands::BaseCommand::Int(
            crate::tex::commands::ValueCommand::Complex{
                get:|e,cmd| [<$name _get>]::<ET>(e,cmd),
                set:|e,cmd,b| Ok([<$name _assign>]::<ET>(e,cmd,b)?),
                name:stringify!($name)
            }
        ),None)),true);
    }};
}

#[macro_export]
macro_rules! register_value_assign_dim {
    ($name:ident,$engine:ident) => {paste::paste!{
        $engine.state.set_command(ET::Char::from_str(stringify!($name),$engine.interner),Some(crate::tex::commands::Command::new(crate::tex::commands::BaseCommand::Dim(
            crate::tex::commands::ValueCommand::Complex{
                get:|e,cmd| [<$name _get>]::<ET>(e,cmd),
                set:|e,cmd,b| Ok([<$name _assign>]::<ET>(e,cmd,b)?),
                name:stringify!($name)
            }
        ),None)),true);
    }};
}

#[macro_export]
macro_rules! register_value_assign_skip {
    ($name:ident,$engine:ident) => {paste::paste!{
        $engine.state.set_command(ET::Char::from_str(stringify!($name),$engine.interner),Some(crate::tex::commands::Command::new(crate::tex::commands::BaseCommand::Skip(
            crate::tex::commands::ValueCommand::Complex{
                get:|e,cmd| [<$name _get>]::<ET>(e,cmd),
                set:|e,cmd,b| Ok([<$name _assign>]::<ET>(e,cmd,b)?),
                name:stringify!($name)
            }
        ),None)),true);
    }};
}

#[macro_export]
macro_rules! register_value_assign_muskip {
    ($name:ident,$engine:ident) => {paste::paste!{
        $engine.state.set_command(ET::Char::from_str(stringify!($name),$engine.interner),Some(crate::tex::commands::Command::new(crate::tex::commands::BaseCommand::MuSkip(
            crate::tex::commands::ValueCommand::Complex{
                get:|e,cmd| [<$name _get>]::<ET>(e,cmd),
                set:|e,cmd,b| Ok([<$name _assign>]::<ET>(e,cmd,b)?),
                name:stringify!($name)
            }
        ),None)),true);
    }};
}

#[macro_export]
macro_rules! register_value_assign_toks {
    ($name:ident,$engine:ident) => {paste::paste!{
        $engine.state.set_command(ET::Char::from_str(stringify!($name),$engine.interner),Some(crate::tex::commands::Command::new(crate::tex::commands::BaseCommand::Toks(
            crate::tex::commands::ToksCommand::Complex{
                get:|e,cmd| [<$name _get>]::<ET>(e,cmd),
                set:|e,cmd,b| Ok([<$name _assign>]::<ET>(e,cmd,b)?),
                name:stringify!($name)
            }
        ),None)),true);
    }};
}


#[macro_export]
macro_rules! register_value_assign_font {
    ($name:ident,$engine:ident) => {paste::paste!{
        $engine.state.set_command(ET::Char::from_str(stringify!($name),$engine.interner),Some(crate::tex::commands::Command::new(crate::tex::commands::BaseCommand::FontCommand{
            get:|e,cmd| [<$name _get>]::<ET>(e,cmd),
            set:Some(|e,cmd,b| Ok([<$name _assign>]::<ET>(e,cmd,b)?)),
            name:stringify!($name)
        },None)),true);
    }};
}

use crate::tex::token::TokenReference;

/// Expands the [`Def`] into a [`Vec`] of [`Token`]s. `cmd` and `cause` are the command and
/// token that triggered the expansion, used for constructing the
/// [`SourceReference`](crate::tex::token::SourceReference)s of the returned [`Token`]s and
/// error messages.
#[inline(never)]
pub fn expand_def<ET:EngineType>(d: &Def<ET>, engine:&mut EngineRef<ET>, cmd:CommandSource<ET>, exp:&mut ExpansionContainer<ET>)
                                 -> Result<(),TeXError<ET>> {
    debug_log!(debug=>"Expanding {}:{}\n - {}",cmd.cause.to_str(engine.interner,Some(ET::Char::backslash())),d.as_str(engine.interner),engine.preview(250).replace("\n","\\n"));
    // The simplest cases are covered first. Technically, the general case covers these as well,
    // but it might be more efficient to do them separately (TODO: check whether that makes a difference)
    if d.signature.is_empty() { // => arity=0
        // No arguments, we just expand the replacement, replacing `##` with `#`
        return expand_simple(d,cmd,engine,exp)
    }
    if d.arity == 0 {
        // No arguments, we just expand the replacement, but need to eat the delimiters in the signature
        for elem in &d.signature {
            match elem {
                ParamToken::Token(delim) => {
                    if let Some((n,_)) = catch!(engine.get_next_token() => cmd.cause) {
                        if n != *delim {
                            throw!("Usage of {} does not match its definition: {} expected, found {}",
                                cmd.cause.to_str(engine.interner,Some(ET::Char::backslash())),
                                delim.to_str(engine.interner,Some(ET::Char::backslash())),
                                n.to_str(engine.interner,Some(ET::Char::backslash())) => cmd.cause)
                        }
                    } else {
                        file_end!(cmd.cause)
                    }
                }
                _=> unsafe{ unreachable_unchecked() } // since arity=0, there can only be tokens
            }
        }
        return expand_simple(d,cmd,engine,exp)
    }

    /*


    // The general case:
    // We parse the arguments according to the signature, and then substitute them into the replacement
     */
    let mut args = engine.memory.get_args();
    read_arguments(d, engine, &cmd, &mut args)?;
    let r = replace(d, cmd, engine, &mut args, exp);
    engine.memory.return_args(args);
    r
}

fn expand_simple<ET:EngineType>(d:&Def<ET>, cmd:CommandSource<ET>, engine:&mut EngineRef<ET>, exp:&mut ExpansionContainer<ET>) -> Result<(),TeXError<ET>> {
    let rf = ET::TokenReference::from_expansion(&cmd);
    for r in &d.replacement {
        match r {
            ExpToken::Token(t) => exp.push(t.clone().with_ref(&rf),engine.memory),
            ExpToken::ParamToken(t) => exp.push(t.clone().with_ref(&rf),engine.memory),
            _ => unreachable!()
        }
    }
    Ok(())
}


fn read_arguments<'a,ET:EngineType>(d:&Def<ET>, engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>, args:&mut [Vec<Token<ET>>;9])
                                    -> Result<(),TeXError<ET>> {
    let mut argnum = 0;
    let mut iter = d.signature.iter().peekable();
    while let Some(next) = iter.next() {
        match next {
            ParamToken::Token(delim) => { // eat the delimiter
                if let Some(n) = catch!(engine.mouth.get_next_simple(engine.state,engine.interner) => cmd.cause.clone()) {
                    if n != *delim {
                        throw!("Usage of {} does not match its definition: {} expected, found {}",
                            cmd.cause.to_str(engine.interner,Some(ET::Char::backslash())),
                            delim.to_str(engine.interner,Some(ET::Char::backslash())),
                            n.to_str(engine.interner,Some(ET::Char::backslash())) => cmd.cause)
                    }
                } else {
                    file_end!(cmd.cause.clone())
                }
            }
            ParamToken::Param => match iter.peek() { // read an argument
                None if d.endswithbrace => {// read until `{`
                    let arg = &mut args[argnum];
                    argnum += 1;
                    'L: loop {
                        match if d.long {catch!({engine.mouth.get_next_simple(engine.state,engine.interner)} => cmd.cause.clone())}
                        else {catch!({engine.mouth.get_next_nopar(engine.state,engine.interner)} => cmd.cause.clone())} {
                            Some(t) => {
                                if t.catcode() == CategoryCode::BeginGroup {
                                    engine.mouth.requeue(t);
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
                    let arg = &mut args[argnum];
                    argnum += 1;
                    catch!(engine.skip_whitespace() => cmd.cause.clone());
                    if d.long {catch!(engine.get_argument(arg) => cmd.cause.clone())}
                    else {
                        catch!(ET::Mouth::get_argument_nopar(engine,arg) => cmd.cause.clone())
                    };
                },
                Some(ParamToken::Token(_)) => { // delimited argument
                    let arg = &mut args[argnum];
                    let mut delims = engine.memory.get_token_vec();
                    argnum += 1;
                    while let Some(ParamToken::Token(t)) = iter.peek() {
                        delims.push(t.clone());
                        iter.next();
                    }
                    let end_marker = delims.pop().unwrap();
                    let mut removebraces: Option<i32> = None;
                    let mut depth = 0;
                    'L: loop {
                        match if d.long {catch!({engine.mouth.get_next_simple(engine.state,engine.interner)} => cmd.cause.clone())}
                        else {catch!({engine.mouth.get_next_nopar(engine.state,engine.interner)} => cmd.cause.clone())} {
                            Some(t) if t.catcode() == CategoryCode::BeginGroup => {
                                depth += 1;
                                if arg.len() == 0 {
                                    removebraces = Some(-1);
                                }
                                arg.push(t);
                            }
                            Some(t) if t.catcode() == CategoryCode::EndGroup => {
                                if depth == 0 {
                                    throw!("Unexpected end group token: {}",t.to_str(engine.interner,Some(ET::Char::backslash())) => cmd.cause.clone())
                                } else {
                                    depth -= 1;
                                    if depth == 0 && t == end_marker  && arg.ends_with(delims.as_slice()) {
                                        for _ in 0..delims.len() {
                                            arg.pop();
                                        }
                                        arg.remove(0);
                                        break 'L;
                                    }
                                    arg.push(t);
                                    if depth == 0 {
                                        match removebraces {
                                            Some(-1) => removebraces = Some(arg.len() as i32),
                                            _ => ()
                                        }
                                    }
                                }
                            }
                            Some(t) => {
                                if depth == 0 && t == end_marker && arg.ends_with(delims.as_slice()) {
                                    for _ in 0..delims.len() {
                                        arg.pop();
                                    }
                                    break 'L;
                                }
                                arg.push(t);
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
                    engine.memory.return_token_vec(delims)
                }
            }
        }
    }
    Ok(())
}

fn replace<ET:EngineType>(d:&Def<ET>, cmd:CommandSource<ET>, engine: &mut EngineRef<ET>, args:&[Vec<Token<ET>>;9], exp:&mut ExpansionContainer<ET>) -> Result<(),TeXError<ET>> {
    let rf = ET::TokenReference::from_expansion(&cmd);
    #[cfg(debug_assertions)]
    {
        debug_log!(debug=>"Arguments:");
        for i in 0..d.arity {
            debug_log!(debug=>"  - {}",TokenList(&args[i as usize]).to_str(engine.interner));
        }
    }
    let mut replacement = d.replacement.iter();
    while let Some(next) = replacement.next() {
        match next {
            ExpToken::Param(_,idx) => {
                for t in args[*idx as usize].iter() {
                    exp.push(t.clone().with_ref(&rf),engine.memory);
                }
            }
            ExpToken::ParamToken(t) => exp.push(t.clone().with_ref(&rf),engine.memory),
            ExpToken::Token(t) => exp.push(t.clone().with_ref(&rf),engine.memory)
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

pub fn parse_signature<ET:EngineType>(engine: &mut EngineRef<ET>, cmd:&CommandSource<ET>, name:&'static str)
                                      -> Result<(bool,u8,Vec<ParamToken<ET>>),TeXError<ET>> {
    let mut arity : u8 = 0;
    let mut params : Vec<ParamToken<ET>> = vec!();
    while let Some((next,_)) = catch_prim!(engine.get_next_token() => (name,cmd)) {
        match &next.base {
            BaseToken::Char(_,CategoryCode::Parameter) => {
                match catch_prim!(engine.get_next_token() => (name,cmd)) {
                    None => file_end_prim!(name,cmd),
                    Some((next,_)) => {
                        match &next.base {
                            BaseToken::Char(_,CategoryCode::BeginGroup) => {
                                engine.mouth.requeue(next);
                                return Ok((true, arity, params))
                            }
                            BaseToken::Char(c,_) => {
                                arity += 1;
                                let u = c.to_usize();
                                if u < 48 || u - 48 != (arity as usize) {
                                    throw!("Expected parameter number {}, got {}",arity,next.to_str(engine.interner,Some(ET::Char::backslash())) => cmd.cause)
                                }
                                params.push(ParamToken::Param);
                            }
                            _ =>
                                throw!("Expected parameter number {}, got {}",arity,next.to_str(engine.interner,Some(ET::Char::backslash())) => cmd.cause)
                        }
                    }
                }
            }
            BaseToken::Char(_,CategoryCode::BeginGroup) => {
                engine.mouth.requeue(next);
                return Ok((false,arity,params))
            }
            BaseToken::Char(_,CategoryCode::EndGroup) => throw!("Unexpected end of group" => cmd.cause),
            _ => params.push(ParamToken::Token(next))
        }
    }
    file_end_prim!(name,cmd.clone())
}
