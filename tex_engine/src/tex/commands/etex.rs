use log::warn;
use crate::engine::gullet::Gullet;
use crate::engine::state::State;
use crate::engine::mouth::Mouth;
use crate::engine::stomach::Stomach;
use crate::{cmtodo, debug_log, register_assign, register_conditional, register_dim, register_int, register_int_assign, register_muskip, register_skip, register_tok_assign, register_expandable, catch_prim, file_end_prim, throw, file_end};
use crate::engine::EngineType;
use crate::engine::gullet::methods::{string_to_tokens, token_to_chars};
use crate::tex::catcodes::CategoryCode;
use crate::tex::commands::{BaseCommand, BaseStomachCommand, Command, CommandSource, DefI, ExpToken, ResolvedToken, TokenCont};
use crate::tex::numbers::{Frac, MuSkip, Numeric, Skip};
use crate::tex::token::{BaseToken, Token, TokenList};
use crate::utils::strings::CharType;
use crate::tex::numbers::Int;
use crate::utils::Ptr;
use crate::tex::commands::Def;
use crate::engine::filesystem::File;
use crate::engine::gullet::numeric_methods::expand_until_space;
use crate::utils::errors::TeXError;
use super::tex::{global,long,outer,def,edef,gdef,xdef,get_csname};

fn next_char<ET:EngineType>(state:&mut ET::State, gullet:&mut ET::Gullet, syms:&'static [u8]) -> Result<Option<u8>,TeXError<ET>> {
    match gullet.get_next_unexpandable(state)? {
        None => file_end!(),
        Some(res) => match res.command {
            BaseCommand::Char {char,..} if char.as_bytes().len() == 1 => {
                let c = char.as_bytes()[0];
                if syms.contains(&c) {
                    Ok(Some(c))
                } else {
                    gullet.mouth().requeue(res.source.cause);
                    Ok(None)
                }
            }
            _ => {
                gullet.mouth().requeue(res.source.cause);
                Ok(None)
            }
        }
    }
}

fn next_char_eq<ET:EngineType>(state:&mut ET::State, gullet:&mut ET::Gullet,sym:u8) -> Result<bool,TeXError<ET>> {
    match gullet.get_next_unexpandable(state)? {
        None => file_end!(),
        Some(res) => match res.command {
            BaseCommand::Char { char, .. } if char.as_bytes() == [sym] => Ok(true),
            _ => {
                gullet.mouth().requeue(res.source.cause);
                Ok(false)
            }
        }
    }
}

fn expr_scale_loop<ET:EngineType>(state:&mut ET::State, gullet:&mut ET::Gullet, cmd:&CommandSource<ET>, name:&'static str)
                                                  -> Result<Frac,TeXError<ET>> {
    let mut stack: Vec<(Option<Frac>,Option<fn(Frac,Frac) -> Frac>)> = vec!((None,None));
    'a: loop {
        catch_prim!(gullet.mouth().skip_whitespace(state) => (name,cmd));
        if catch_prim!(next_char_eq::<ET>(state,gullet,b'(') => (name,cmd.clone())) {
            stack.push((None,None));
            continue 'a;
        }
        let mut first = Frac::new(catch_prim!(gullet.get_int(state) => (name,cmd)).to_i64(),1);
        loop {
            catch_prim!(gullet.mouth().skip_whitespace(state) => (name,cmd.clone()));
            let kw = catch_prim!(
                if stack.len()>1 {next_char::<ET>(state,gullet,&[b'+',b'-',b'*',b'/',b')'])}
                else {next_char::<ET>(state,gullet,&[b'*',b'/'])}
                => (name,cmd.clone()));
            match kw {
                None => {
                    match stack.last_mut() {
                        Some((Some(f),Some(op))) => {
                            first = op(*f,first);
                            *unsafe{stack.last_mut().unwrap_unchecked()} = (None,None);
                        }
                        _ => ()
                    }
                    match stack.pop() {
                        Some((None,None)) if stack.is_empty() => return Ok(first),
                        _ => throw!("Expected ')'" => cmd.cause.clone())
                    }
                }
                Some(b')' ) => {
                    match stack.last_mut() {
                        Some((Some(f),Some(op))) => {
                            first = op(*f,first);
                            *unsafe{stack.last_mut().unwrap_unchecked()} = (None,None);
                        }
                        _ => ()
                    }
                    stack.pop();
                    match stack.last_mut() {
                        Some((Some(f),Some(op))) => {
                            first = op(*f,first);
                            *unsafe{stack.last_mut().unwrap_unchecked()} = (None,None);
                        }
                        _ => ()
                    }
                }
                Some(b'+') => {
                    match stack.last_mut() {
                        Some((Some(f),Some(op))) => {
                            first = op(*f,first);
                            *unsafe{stack.last_mut().unwrap_unchecked()} = (None,None);
                        }
                        _ => ()
                    }
                    *stack.last_mut().unwrap() = (Some(first),Some(|a,b| a + b));
                    continue 'a;
                }
                Some(b'-') => {
                    match stack.last_mut() {
                        Some((Some(f),Some(op))) => {
                            first = op(*f,first);
                            *unsafe{stack.last_mut().unwrap_unchecked()} = (None,None);
                        }
                        _ => ()
                    }
                    *stack.last_mut().unwrap() = (Some(first),Some(|a,b| a - b));
                    continue 'a;
                }
                Some(b'*') => {
                    let ret = catch_prim!(expr_scale_loop::<ET>(state,gullet,cmd,&name) => (name,cmd.clone()));
                    first = first.scale(ret.0,ret.1);
                    match stack.last_mut() {
                        Some((Some(f),Some(op))) => {
                            first = op(*f,first);
                            *unsafe{stack.last_mut().unwrap_unchecked()} = (None,None);
                        }
                        _ => ()
                    }
                }
                Some(b'/') => {
                    let ret = catch_prim!(expr_scale_loop::<ET>(state,gullet,cmd,&name) => (name,cmd.clone()));
                    first = first.scale(ret.1,ret.0);
                    match stack.last_mut() {
                        Some((Some(f),Some(op))) => {
                            first = op(*f,first);
                            *unsafe{stack.last_mut().unwrap_unchecked()} = (None,None);
                        }
                        _ => ()
                    }
                }
                _ => unreachable!()
            }
        }
    }
}

fn expr_loop<ET:EngineType,Num:Numeric>(state:&mut ET::State, gullet:&mut ET::Gullet, cmd:&CommandSource<ET>, name:&'static str,
                                get:fn(&mut ET::State, &mut ET::Gullet) -> Result<Num,TeXError<ET>>)
                                                                  -> Result<Num,TeXError<ET>> {
    let mut stack: Vec<(Option<Num>, Option<fn(Num, Num) -> Num>)> = vec!((None, None));
    'a: loop {
        catch_prim!(gullet.mouth().skip_whitespace(state) => (name,cmd.clone()));
        if catch_prim!(next_char_eq::<ET>(state,gullet,b'(') => (name,cmd.clone())) {
            stack.push((None, None));
            continue 'a;
        }
        let mut first = catch_prim!(get(state,gullet) => (name,cmd.clone()));
        loop {
            catch_prim!(gullet.mouth().skip_whitespace(state) => (name,cmd.clone()));
            let kw = catch_prim!(
                if stack.len()>1 {next_char::<ET>(state,gullet,&[b'+',b'-',b'*',b'/',b')'])}
                else {next_char::<ET>(state,gullet,&[b'+',b'-',b'*',b'/'])}
                => (name,cmd.clone()));
            match kw {
                None => {
                    match stack.last_mut() {
                        Some((Some(second), Some(op))) => {
                            first = op(second.clone(),first);
                            *unsafe{stack.last_mut().unwrap_unchecked()} = (None,None);
                        }
                        _ => ()
                    }
                    match stack.pop() {
                        Some((None, None)) if stack.is_empty() => return Ok(first),
                        _ => throw!("Expected ')'" => cmd.cause.clone())
                    }
                }
                Some(b')') => {
                    match stack.pop() {
                        Some((Some(second), Some(op))) => {
                            first = op(second.clone(),first);
                        }
                        _ => ()
                    }
                }
                Some(b'+') => {
                    match stack.last_mut() {
                        Some((Some(second), Some(op))) => {
                            first = op(second.clone(),first);
                            *unsafe{stack.last_mut().unwrap_unchecked()} = (None,None);
                        }
                        _ => ()
                    }
                    *stack.last_mut().unwrap() = (Some(first), Some(|a, b| a + b));
                    continue 'a;
                }
                Some(b'-') => {
                    match stack.last_mut() {
                        Some((Some(second), Some(op))) => {
                            first = op(second.clone(),first);
                            *unsafe{stack.last_mut().unwrap_unchecked()} = (None,None);
                        }
                        _ => ()
                    }
                    *stack.last_mut().unwrap() = (Some(first), Some(|a, b| a - b));
                    continue 'a;
                }
                Some(b'*') => {
                    let ret = catch_prim!(expr_scale_loop::<ET>(state,gullet,cmd,&name) => (name,cmd.clone()));
                    first = first.scale(ret.0,ret.1);
                    match stack.last_mut() {
                        Some((Some(second), Some(op))) => {
                            first = op(second.clone(),first);
                            *unsafe{stack.last_mut().unwrap_unchecked()} = (None,None);
                        }
                        _ => ()
                    }
                }
                Some(b'/') => {
                    let ret = catch_prim!(expr_scale_loop::<ET>(state,gullet,cmd,&name) => (name,cmd.clone()));
                    first = first.scale(ret.1,ret.0);
                    match stack.last_mut() {
                        Some((Some(second), Some(op))) => {
                            first = op(second.clone(),first);
                            *unsafe{stack.last_mut().unwrap_unchecked()} = (None,None);
                        }
                        _ => ()
                    }
                }
                _ => unreachable!()
            }
        }
    }
}

// --------------------------------------------------------------------------------------------------

pub static DETOKENIZE: &str = "detokenize";
/// `\detokenize`: convert a token list into a string of [`CategoryCode`] [`Other`](CategoryCode::Other)
/// (except for ` `, which gets code [`Space`](CategoryCode::Space)).
pub fn detokenize<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,f:TokenCont<ET>) -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"detokenize");
    let escape = state.get_escapechar();
    let cc = state.get_catcode_scheme().clone();
    catch_prim!(expand_until_space::<ET>(gullet,state) => (DETOKENIZE,cmd));
    catch_prim!(gullet.get_group(state,&mut |s,next| match &next.base {
        BaseToken::Char(c,CategoryCode::Parameter) => {
            token_to_chars::<ET,_>(&next,escape,&cc,true,|t|f(s,t))?;
            token_to_chars::<ET,_>(&next,escape,&cc,true,|t|f(s,t))?;
            Ok(())
        }
        _ => token_to_chars::<ET,_>(&next,escape,&cc,true,|t|f(s,t))
    }) => (DETOKENIZE,cmd));
    Ok(())
}

/// `\dimexpr`: evaluate a dimension expression; returns a [`Dim`].
pub fn dimexpr<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>) -> Result<ET::Dim,TeXError<ET>> {
    debug_log!(trace=>"dimexpr: {}",gullet.mouth().preview(100));
    let ret = expr_loop::<ET,ET::Dim>(state, gullet, &cmd, "dimexpr", |s, g:&mut ET::Gullet| g.get_dim(s))?;
    if let Some((next,_)) = catch_prim!(gullet.mouth().get_next(state) => ("dimexpr",cmd)) {
        match &next.base {
            BaseToken::CS(name) => match state.get_command(name).map(|c| &c.base) {
                Some(BaseCommand::Relax) => (),
                _ => gullet.mouth().requeue(next)
            }
            _ => gullet.mouth().requeue(next)
        }
    }
    Ok(ret)
}

/// `\etexrevision`: expands to the eTeX revision number (`.6`).
pub fn eTeXrevision<ET:EngineType>(state:&mut ET::State,f:TokenCont<ET>) -> Result<(),TeXError<ET>> {
    string_to_tokens::<ET>(".6".as_bytes(),state,f)
}

/// `\eTeXversion`: returns the eTeX version number as an [`Int`] (2).
pub fn eTeXversion<ET:EngineType>(cmd:CommandSource<ET>) -> Result<ET::Int,TeXError<ET>> {
    Ok(catch_prim!(ET::Int::from_i64(2) => ("eTeXversion",cmd)))
}

/// `\expanded`: expands its argument exhaustively.
pub fn expanded<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,f:TokenCont<ET>)
    -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"expanded");
    catch_prim!(gullet.get_expanded_group(state,false,false,false,f) => ("expanded",cmd));
    Ok(())
}

/// `\glueexpr`: evaluate a glue expression; returns a [`Skip`].
pub fn glueexpr<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>)
    -> Result<Skip<ET::SkipDim>,TeXError<ET>> {
    debug_log!(trace=>"glueexpr: {}",gullet.mouth().preview(100));
    let ret = expr_loop::<ET,Skip<ET::SkipDim>>(state, gullet, &cmd, "glueexpr", |s, g:&mut ET::Gullet| g.get_skip(s))?;
    if let Some((next,_)) = catch_prim!(gullet.mouth().get_next(state) => ("glueexpr",cmd)) {
        match &next.base {
            BaseToken::CS(name) => match state.get_command(name).map(|c| &c.base) {
                Some(BaseCommand::Relax) => (),
                _ => gullet.mouth().requeue(next)
            }
            _ => gullet.mouth().requeue(next)
        }
    }
    Ok(ret)
}

/// `\ifcsname`: expands [`Token`]s like [`\csname`](super::tex::csname) would (i.e. until
/// [`\endcsname`](super::tex::endcsname)) and evaluates to true if the resulting control sequence
/// is defined. Unlike [`\csname`](super::tex::csname), does not define the control sequence as
/// [`\relax`](super::tex::relax)
pub fn ifcsname<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>)
    -> Result<bool,TeXError<ET>> {
    debug_log!(trace=>"ifcsname");
    let name = get_csname::<ET>(state,gullet,&cmd,"ifcsname")?;
    Ok(state.get_command(&name).is_some())
}

/// `\ifdefined`: evaluates to true if the next token is a control sequence that is defined.
pub fn ifdefined<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>)
    -> Result<bool,TeXError<ET>> {
    debug_log!(trace=>"ifdefined");
    match catch_prim!(gullet.mouth().get_next(state) => ("ifeof",cmd)) {
        None => file_end_prim!("ifeof",cmd),
        Some((t,_)) => match t.base {
            BaseToken::Char(c,CategoryCode::Active) =>
                Ok(state.get_ac_command(&c).is_some()),
            BaseToken::CS(name) => {
                debug_log!(trace => "ifdefined: {}",name.to_string());
                Ok(state.get_command(&name).is_some())
            }
            _ => throw!("Expected a control sequence, got: {:?}",t => cmd.cause)
        }
    }
}

/// `\muexpr`: evaluate a mu expression; returns a [`MuSkip`].
pub fn muexpr<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>)
    -> Result<MuSkip<ET::MuDim,ET::MuStretchShrinkDim>,TeXError<ET>> {
    debug_log!(trace=>"muexpr: {}",gullet.mouth().preview(100));
    let ret = expr_loop::<ET,MuSkip<ET::MuDim,ET::MuStretchShrinkDim>>(state, gullet, &cmd, "muexpr", |s, g:&mut ET::Gullet| g.get_muskip(s))?;
    if let Some((next,_)) = catch_prim!(gullet.mouth().get_next(state) => ("muexpr",cmd)) {
        match &next.base {
            BaseToken::CS(name) => match state.get_command(name).map(|c| &c.base) {
                Some(BaseCommand::Relax) => (),
                _ => gullet.mouth().requeue(next)
            }
            _ => gullet.mouth().requeue(next)
        }
    }
    Ok(ret)
}

/// `\numexpr`: evaluate a number expression; returns an [`Int`].
pub fn numexpr<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>)
    -> Result<ET::Int,TeXError<ET>> {
    debug_log!(trace=>"numexpr: {}",gullet.mouth().preview(100));
    let ret = expr_loop::<ET,ET::Int>(state, gullet, &cmd, "numexpr", |s, g:&mut ET::Gullet| g.get_int(s))?;
    if let Some((next,_)) = catch_prim!(gullet.mouth().get_next(state) => ("numexpr",cmd)) {
        match &next.base {
            BaseToken::CS(name) => match state.get_command(name).map(|c| &c.base) {
                Some(BaseCommand::Relax) => (),
                _ => gullet.mouth().requeue(next)
            }
            _ => gullet.mouth().requeue(next)
        }
    }
    Ok(ret)
}


/// `\protected`: make the next control sequence protected (i.e. not expandable).
pub fn protected<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,global_:bool,_:bool,long_:bool,outer_:bool)
                                                      -> Result<(),TeXError<ET>> {
    debug_log!(trace => "\\protected");
    match catch_prim!(gullet.get_next_stomach_command(state) => ("protected",cmd)) {
        None => file_end_prim!("protected",cmd),
        Some(c) => match c.command {
            BaseStomachCommand::Assignment {name:Some("global"),..} => global::<ET>(state,gullet,cmd,global_,true,long_,outer_),
            BaseStomachCommand::Assignment {name:Some("protected"),..} => protected::<ET>(state,gullet,cmd,global_,true,long_,outer_),
            BaseStomachCommand::Assignment {name:Some("long"),..} => long::<ET>(state,gullet,cmd,global_,true,long_,outer_),
            BaseStomachCommand::Assignment {name:Some("outer"),..} => outer::<ET>(state,gullet,cmd,global_,true,long_,outer_),
            BaseStomachCommand::Assignment {name:Some("def"),..} => def::<ET>(state,gullet,cmd,global_,true,long_,outer_),
            BaseStomachCommand::Assignment {name:Some("edef"),..} => edef::<ET>(state,gullet,cmd,global_,true,long_,outer_),
            BaseStomachCommand::Assignment {name:Some("gdef"),..} => gdef::<ET>(state,gullet,cmd,global_,true,long_,outer_),
            BaseStomachCommand::Assignment {name:Some("xdef"),..} => xdef::<ET>(state,gullet,cmd,global_,true,long_,outer_),
            _ => throw!("Expected a macro definition after \\protected" => cmd.cause)
        }
    }
}

/// `\readline`: read a line from a file like [`\read`](super::tex::read), but with all characters
/// having [`CategoryCode`] [12 (`Other`)](CategoryCode::Other) (except for ` ` [`Space`](CategoryCode::Space))
pub fn readline<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,globally:bool)
                           -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"readline");
    let i = catch_prim!(gullet.get_int(state) => ("readline",cmd));
    let i : usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => throw!("Invalid file number: {}",i => cmd.cause)
    };
    let file = match state.get_open_in_file(i) {
        None => throw!("File {} not open for reading",i => cmd.cause),
        Some(f) => f
    };
    if !catch_prim!(gullet.get_keyword(state,"to") => ("readline",cmd)) {
        throw!("Expected 'to' after \\read" => cmd.cause)
    }
    let newcmd = catch_prim!(gullet.get_control_sequence(state) => ("readline",cmd));
    let mut ret = vec!();
    catch_prim!(file.read::<ET,_>(&ET::Char::other_catcode_scheme(),state.get_endlinechar(),|t| ret.push(t)) => ("readline",cmd));
    debug_log!(trace=>"readline: {} = {}",newcmd,TokenList(&ret));

    let def = DefI::simple(ret);
    match newcmd.base {
        BaseToken::CS(name) => state.set_command(name,Some(Command::new(
            BaseCommand::Def(def),Some(&cmd)
        )),globally),
        BaseToken::Char(c,_) => state.set_ac_command(c,Some(Command::new(
            BaseCommand::Def(def),Some(&cmd)
        )),globally)
    }
    Ok(())
}

pub static UNEXPANDED: &str = "unexpanded";
/// `\unexpanded`: read a token list from the input stream, but do not expand it (e.g. in [`\edef`](super::tex::edef)).
pub fn unexpanded<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>,f:TokenCont<ET>) -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"unexpanded");
    match catch_prim!(gullet.get_next_stomach_command(state) => (UNEXPANDED,cmd)) {
        None => file_end_prim!("unexpanded",cmd),
        Some(sc) => match sc.command {
            BaseStomachCommand::BeginGroup => (),
            _ => throw!("Expected a begin group after \\unexpanded" => cmd.cause)
        }
    }
    catch_prim!(gullet.mouth().read_until_endgroup(state,f) => (UNEXPANDED,cmd));
    Ok(())
}

/// `\unless`: read a conditional and inverse its result. Notably, this is not itself implemented as a conditional,
/// since otherwise, e.g. `\unless\ifx` in the [false-loop](crate::engine::gullet::methods::false_loop)
/// would be interpreted as *two* conditionals, rather than just one.
pub static UNLESS : &str = "unless";
pub fn unless<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:CommandSource<ET>) -> Result<(),TeXError<ET>> {
    debug_log!(trace=>"unless");
    match catch_prim!(gullet.mouth().get_next(state) => (UNLESS,cmd)) {
        None => file_end_prim!(UNLESS,cmd),
        Some((next,true)) => {
            let ncmd = match &next.base {
                BaseToken::CS(name) => state.get_command(name),
                BaseToken::Char(c,CategoryCode::Active) => state.get_ac_command(c),
                _ => throw!("Expected a conditional after \\unless" => cmd.cause)
            };
            let ncmd = match ncmd {
                None => throw!("Expected a conditional after \\unless" => cmd.cause),
                Some(c) => c.clone()
            };
            match ncmd.base {
                BaseCommand::Conditional {name,apply} => {
                    catch_prim!(crate::engine::gullet::methods::do_conditional::<ET>(gullet,state,
                        CommandSource{cause:next,reference:ncmd.reference},name,apply,true) => (UNLESS,cmd));
                    Ok(())
                }
                _ => throw!("Expected a conditional after \\unless" => cmd.cause)
            }
        }
         _ => throw!("Expected a conditional after \\unless" => cmd.cause)
    }
}

/// Initialize a TeX engine with default implementations for all eTeX primitives.
pub fn initialize_etex_primitives<ET:EngineType>(state:&mut ET::State,stomach:&mut ET::Stomach,gullet:&mut ET::Gullet) {
    register_expandable!(detokenize,state,stomach,gullet,(s,g,c,f) =>detokenize::<ET>(s,g,c,f));
    register_dim!(dimexpr,state,stomach,gullet,(s,g,c) => dimexpr::<ET>(s,g,c));
    register_expandable!(eTeXrevision,state,stomach,gullet,(s,g,c,f) => eTeXrevision::<ET>(s,f));
    register_int!(eTeXversion,state,stomach,gullet,(s,g,c) => eTeXversion::<ET>(c));
    register_tok_assign!(everyeof,state,stomach,gullet);
    register_expandable!(expanded,state,stomach,gullet,(s,g,c,f) => expanded::<ET>(s,g,c,f));
    register_skip!(glueexpr,state,stomach,gullet,(s,g,c) => glueexpr::<ET>(s,g,c));
    register_conditional!(ifcsname,state,stomach,gullet,(s,gu,cmd) =>ifcsname::<ET>(s,gu,cmd));
    register_conditional!(ifdefined,state,stomach,gullet,(s,gu,cmd) =>ifdefined::<ET>(s,gu,cmd));
    register_muskip!(muexpr,state,stomach,gullet,(s,g,c) => muexpr::<ET>(s,g,c));
    register_int!(numexpr,state,stomach,gullet,(s,g,c) => numexpr::<ET>(s,g,c));
    register_assign!(readline,state,stomach,gullet,(s,gu,cmd,global) =>readline::<ET>(s,gu,cmd,global));
    register_int_assign!(savinghyphcodes,state,stomach,gullet);
    register_int_assign!(tracingassigns,state,stomach,gullet);
    register_int_assign!(tracinggroups,state,stomach,gullet);
    register_int_assign!(tracingifs,state,stomach,gullet);
    register_int_assign!(tracingnesting,state,stomach,gullet);
    register_int_assign!(tracingscantokens,state,stomach,gullet);
    register_assign!(protected,state,stomach,gullet,(s,gu,cmd,g) =>protected::<ET>(s,gu,cmd,g,false,false,false));
    register_expandable!(unexpanded,state,stomach,gullet,(s,g,c,f) => unexpanded::<ET>(s,g,c,f));
    register_expandable!(unless,state,stomach,gullet,(s,gu,cmd,f) =>unless::<ET>(s,gu,cmd));

    cmtodo!(state,stomach,gullet,beginL);
    cmtodo!(state,stomach,gullet,beginR);
    cmtodo!(state,stomach,gullet,botmarks);
    cmtodo!(state,stomach,gullet,clubpenalties);
    cmtodo!(state,stomach,gullet,currentgrouplevel);
    cmtodo!(state,stomach,gullet,currentgrouptype);
    cmtodo!(state,stomach,gullet,currentifbranch);
    cmtodo!(state,stomach,gullet,currentiflevel);
    cmtodo!(state,stomach,gullet,currentiftype);
    cmtodo!(state,stomach,gullet,displaywidowpenalties);
    cmtodo!(state,stomach,gullet,endL);
    cmtodo!(state,stomach,gullet,endR);
    cmtodo!(state,stomach,gullet,firstmarks);
    cmtodo!(state,stomach,gullet,fontchardp);
    cmtodo!(state,stomach,gullet,fontcharht);
    cmtodo!(state,stomach,gullet,fontcharic);
    cmtodo!(state,stomach,gullet,fontcharwd);
    cmtodo!(state,stomach,gullet,glueshrink);
    cmtodo!(state,stomach,gullet,glueshrinkorder);
    cmtodo!(state,stomach,gullet,gluestretch);
    cmtodo!(state,stomach,gullet,gluestretchorder);
    cmtodo!(state,stomach,gullet,gluetomu);
    cmtodo!(state,stomach,gullet,iffontchar);
    cmtodo!(state,stomach,gullet,interactionmode);
    cmtodo!(state,stomach,gullet,interlinepenalties);
    cmtodo!(state,stomach,gullet,lastlinefit);
    cmtodo!(state,stomach,gullet,lastnodetype);
    cmtodo!(state,stomach,gullet,marks);
    cmtodo!(state,stomach,gullet,middle);
    cmtodo!(state,stomach,gullet,mutoglue);
    cmtodo!(state,stomach,gullet,pagediscards);
    cmtodo!(state,stomach,gullet,parshapedimen);
    cmtodo!(state,stomach,gullet,parshapeindent);
    cmtodo!(state,stomach,gullet,parshapelength);
    cmtodo!(state,stomach,gullet,predisplaydirection);
    cmtodo!(state,stomach,gullet,savingvdiscards);
    cmtodo!(state,stomach,gullet,scantokens);
    cmtodo!(state,stomach,gullet,showgroups);
    cmtodo!(state,stomach,gullet,showifs);
    cmtodo!(state,stomach,gullet,showtokens);
    cmtodo!(state,stomach,gullet,splitbotmarks);
    cmtodo!(state,stomach,gullet,splitdiscards);
    cmtodo!(state,stomach,gullet,splitfirstmarks);
    cmtodo!(state,stomach,gullet,TeXXeTstate);
    cmtodo!(state,stomach,gullet,topmarks);
    cmtodo!(state,stomach,gullet,widowpenalties);
}