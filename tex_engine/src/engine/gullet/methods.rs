/*! Default implementations for [`Gullet`] methods.
*/
use either::Either;
use crate::commands::{ActiveConditional, CharOrPrimitive, TeXCommand, PrimitiveCommand, ResolvedToken};
use crate::commands::primitives::PRIMITIVES;
use crate::engine::{EngineReferences, EngineTypes};
use crate::engine::mouth::Mouth;
use crate::tex::catcodes::CommandCode;
use crate::tex::numerics::{MuDim, MuSkip, MuStretchShrink, NumSet, Skip, STRETCH_SHRINK_UNITS, StretchShrink};
use crate::engine::state::State;
use crate::engine::gullet::Gullet;
use crate::tex::tokens::token_lists::TokenList;
use crate::tex::tokens::control_sequences::{CSHandler, ResolvedCSName};
use crate::tex::numerics::TeXDimen;
use crate::tex::tokens::{StandardToken, Token};
use crate::tex::characters::Character;
use crate::engine::EngineAux;
use crate::utils::errors::{FileEndWhileUse, RecoverableError, TeXResult, WrongDefinition, IncompleteConditional, Undefined, MissingNumber};

/// processes the parameter signature `params` of a [`Macro`](crate::commands::Macro) by reading the relevant arguments;
/// storing them in `args`
pub fn read_arguments<ET:EngineTypes>(engine:&mut EngineReferences<ET>,args:&mut [Vec<ET::Token>;9],params:TokenList<ET::Token>,long:bool,token:&ET::Token)
    -> TeXResult<(),ET> {
    let mut i = 1usize;
    let inner = &params.0;
    let mut next = &inner[0];
    loop {
        match next.is_argument_marker() {
            Some(a) => match inner.get(i) {
                Some(n) if n.is_argument_marker().is_some() =>
                    {next = n; i += 1;read_argument(engine,&mut args[a as usize],long,token)?},
                None => {
                    return read_argument(engine,&mut args[a as usize],long,token);
                },
                Some(o) => {
                    let mut delim = vec!(o.clone());
                    i += 1;
                    while let Some(n) = inner.get(i) {
                        if n.is_argument_marker().is_some() {break}
                        delim.push(n.clone());
                        i += 1;
                    }
                    read_delimited_argument(engine,&mut args[a as usize],&delim,long,token)?;
                    match inner.get(i) {
                        Some(n) => {next = n; i += 1},
                        _ => return Ok(())
                    }
                }
            },
            _ => match engine.mouth.get_next(engine.aux, engine.state)? {
                Some(o) if o == *next =>
                    match inner.get(i) {
                        Some(n) => {next = n; i += 1},
                        _ => return Ok(())
                    },
                Some(o) =>
                    WrongDefinition {expected:next.clone(),found:o,in_macro:token.clone()}.throw(engine.aux,engine.state,engine.mouth)?,
                None =>
                    FileEndWhileUse(token.clone()).throw(engine.aux,engine.state,engine.mouth)?,
            }
        }
    }
}

fn read_delimited_argument<ET:EngineTypes>(engine:&mut EngineReferences<ET>,arg:&mut Vec<ET::Token>,delim:&[ET::Token],long:bool,token:&ET::Token)
    -> TeXResult<(),ET> {
    let par = engine.aux.memory.cs_interner().par();
    let last = delim.last().unwrap();
    let ends_with_bgroup = last.command_code() == CommandCode::BeginGroup;
    let mut remove_braces:Option<Option<usize>> = None;
    loop {
        let t = engine.need_next(false,token)?;
        match t.command_code() {
            CommandCode::Primitive if t.is_primitive() == Some(PRIMITIVES.noexpand) => continue,
            CommandCode::BeginGroup if !ends_with_bgroup => {
                if arg.is_empty() { remove_braces = Some(None) }
                arg.push(t);
                let r = if long {
                    engine.read_until_endgroup(token,
                                               |_, _, t| {
                                                   arg.push(t);
                                                   Ok(())
                                               }
                    )?
                } else {
                    engine.read_until_endgroup(token,
                                               |_, _, t| if t.is_cs(&par) { todo!("\\par in read_argument") } else { arg.push(t);
                                               Ok(()) }
                    )?
                };
                arg.push(r);
                if let Some(ref mut n @ None) = remove_braces { *n = Some(arg.len()) }
                continue
            }
            CommandCode::Escape if !long && t.is_cs(&par) => {todo!("\\par in read_argument")}
            _ => ()
        }
        if t == *last {
            arg.push(t);
            if arg.ends_with(delim) {
                arg.truncate(arg.len()-delim.len());
                if let Some(Some(n)) = remove_braces {
                    if arg.len() == n {
                        arg.pop();
                        arg.remove(0);
                    }
                }
                if ends_with_bgroup {
                    if let Some(a) = engine.gullet.get_align_data() { a.ingroups -= 1 }
                }
                return Ok(())
            }
        } else {
            arg.push(t);
        }
    }
}

fn read_argument<ET:EngineTypes>(engine:&mut EngineReferences<ET>,arg:&mut Vec<ET::Token>,long:bool,token:&ET::Token)
                                 -> TeXResult<(),ET> {
    let eof = |a:&_,s:&_,m:&mut _| {
        FileEndWhileUse(token.clone()).throw(a,s,m)
    };
    loop {
        let t = match engine.mouth.get_next(engine.aux, engine.state)? {
            Some(t) => t,
            None => {
                FileEndWhileUse(token.clone()).throw(engine.aux,engine.state,engine.mouth)?;continue
            }
        };
        match t.command_code() {
            CommandCode::Primitive if t.is_primitive() == Some(PRIMITIVES.noexpand) => continue,
            CommandCode::Space => continue,
            CommandCode::BeginGroup => {
                if long {
                    engine.mouth.read_until_endgroup(
                        engine.aux,
                        engine.state,
                        |_, t| {
                            arg.push(t);
                            Ok(())
                        },eof
                    )?;
                } else {
                    let par = engine.aux.memory.cs_interner().par();
                    engine.mouth.read_until_endgroup(
                        engine.aux,
                        engine.state,
                        |_,t|
                            if t.is_cs(&par) {todo!("\\par in read_argument")} else {arg.push(t);
                            Ok(())},eof
                    )?;
                }
                return Ok(())
            }
            _ => ()
        }
        arg.push(t);
        return Ok(())
    }
}

/// Default implementation for [`Gullet::expand_until_endgroup`].
pub fn expand_until_endgroup<ET:EngineTypes,Fn:FnMut(&mut EngineAux<ET>,&ET::State,ET::Token) -> TeXResult<(),ET>>
(engine:&mut EngineReferences<ET>,expand_protected:bool,edef_like:bool,token:&ET::Token,mut cont:Fn)
    -> TeXResult<(),ET> {
    let mut ingroups = 0;
    loop {
        let t = engine.need_next(false,token)?;
        match t.command_code() {
            CommandCode::EndGroup => {
                if ingroups == 0 { return Ok(()) }
                ingroups -= 1;
                cont(engine.aux,engine.state,t)?;
                continue
            }
            CommandCode::BeginGroup => {
                ingroups += 1;
                cont(engine.aux,engine.state,t)?;
                continue
            }
            CommandCode::Primitive if t.is_primitive() == Some(PRIMITIVES.noexpand) => {
                let next = engine.need_next(false,token)?;
                cont(engine.aux,engine.state,next)?;
                continue
            }
            CommandCode::Escape | CommandCode::Active => match engine.resolve(&t) {
                ResolvedToken::Tk{..} => {
                    cont(engine.aux,engine.state,t)?
                }
                ResolvedToken::Cmd(Some(TeXCommand::Macro(m))) if m.protected && !expand_protected =>
                    cont(engine.aux,engine.state,t)?,
                ResolvedToken::Cmd(Some(TeXCommand::Macro(m))) =>
                    ET::Gullet::do_macro(engine,m.clone(),t)?,
                ResolvedToken::Cmd(Some(TeXCommand::Primitive{name,cmd:PrimitiveCommand::Conditional(cond)})) =>
                    ET::Gullet::do_conditional(engine,*name,t,*cond,false)?,
                ResolvedToken::Cmd(Some(TeXCommand::Primitive{name,..})) if *name == PRIMITIVES.unexpanded => {
                    engine.expand_until_bgroup(false,&t)?;
                    engine.read_until_endgroup(token,|a,s,t|{
                        if edef_like && t.command_code() == CommandCode::Parameter {
                            cont(a,s,t.clone())?;
                        }
                        cont(a,s,t)
                    })?;
                }
                ResolvedToken::Cmd(Some(TeXCommand::Primitive{name,..})) if *name == PRIMITIVES.the => {
                    engine.do_the(|a, s, _, t| {
                        if t.command_code() == CommandCode::Parameter && edef_like {
                            cont(a,s,t.clone())?;
                        }
                        cont(a,s,t)
                    })?
                }
                ResolvedToken::Cmd(Some(TeXCommand::Primitive {name,cmd:PrimitiveCommand::SimpleExpandable(e)})) =>
                    ET::Gullet::do_simple_expandable(engine, *name, t, *e)?,
                ResolvedToken::Cmd(Some(TeXCommand::Primitive {name,cmd:PrimitiveCommand::Expandable(e)})) =>
                    ET::Gullet::do_expandable(engine,*name,t,*e)?,
                ResolvedToken::Cmd(_) => {
                    cont(engine.aux,engine.state,t)?
                }
            }
            _ => cont(engine.aux,engine.state,t)?
        }
    }
}

pub(crate) fn case_loop<ET:EngineTypes>(engine:&mut EngineReferences<ET>,idx:usize) -> TeXResult<(),ET> {
    let (mut incond,cond) = {
        let conds = engine.gullet.get_conditionals();
        let ic = conds.len() - idx;
        for _ in 0..ic { conds.pop();}
        (ic,match conds.pop() {
            Some(ActiveConditional::Case(c)) => c,
            _ => unreachable!()
        })
    };
    let mut curr_int = <ET::Num as NumSet>::Int::default();
    let one = <ET::Num as NumSet>::Int::from(1);
    let mut conds = std::mem::take(engine.gullet.get_conditionals());
    engine.iterate(|_,state,t| {
        if !t.is_cs_or_active() { return Ok(None) }
        match ET::Gullet::char_or_primitive(state,&t) {
            Some(CharOrPrimitive::Primitive(name)) if name == PRIMITIVES.r#else && incond == 0 => {
                conds.push(
                    ActiveConditional::Else(PRIMITIVES.ifcase)
                );
                Ok(Some(()))
            }
            Some(CharOrPrimitive::Primitive(name)) if name == PRIMITIVES.fi => {
                if incond == 0 {Ok(Some(()))}
                else {incond -= 1; Ok(None)}
            }
            Some(CharOrPrimitive::Primitive(name)) if name == PRIMITIVES.or && incond == 0 => {
                curr_int = curr_int + one;
                if cond == curr_int {
                    conds.push(
                        ActiveConditional::Case(cond)
                    );
                    Ok(Some(()))
                }
                else {Ok(None)}
            }
            Some(CharOrPrimitive::Primitive(name)) => {
                if let Some(TeXCommand::Primitive{cmd:PrimitiveCommand::Conditional(_),..}) = state.primitives().get_id(name) { incond += 1 }
                Ok(None)
            }
            _ => Ok(None)
        }
    },|a,s,m| {
        IncompleteConditional::recover(IncompleteConditional { name: PRIMITIVES.ifcase, line_no: m.line_number() }, a, s, m)
    })?;
    *engine.gullet.get_conditionals() = conds;
    Ok(())
}

/// What to do on a false conditional - skips [`Token`]s until the next `\else` (if `allowelse` is true) or `\fi`
/// If `skipelse` is false, precisely one `\else` is skipped as well (this happens in `\ifcase` when the
/// appropriate case is already done, so the corresponding `\else` should be skipped).
pub fn false_loop<ET:EngineTypes>(engine:&mut EngineReferences<ET>,idx:usize,allowelse:bool,mut skipelse:bool) -> TeXResult<(),ET> {
    let (mut incond,cond) = {
        let conds = engine.gullet.get_conditionals();
        let ic = conds.len() - idx;
        for _ in 0..ic { conds.pop();}
        (ic,conds.pop().unwrap().name())
    };
    let mut conds = std::mem::take(engine.gullet.get_conditionals());
    engine.iterate(|_,state,t| {
        if !t.is_cs_or_active() { return Ok(None) }
        match ET::Gullet::char_or_primitive(state,&t) {
            Some(CharOrPrimitive::Primitive(name)) if name == PRIMITIVES.r#else && incond == 0 => {
                if allowelse {
                    conds.push(
                        ActiveConditional::Else(cond)
                    );
                    Ok(Some(()))
                }
                else if skipelse {
                    skipelse = false;Ok(None)
                }
                else {todo!("throw spurious else in false-loop error")}
            }
            Some(CharOrPrimitive::Primitive(name)) if name == PRIMITIVES.fi => {
                if incond == 0 {Ok(Some(()))}
                else {incond -= 1; Ok(None)}
            }
            Some(CharOrPrimitive::Primitive(name)) => {
                if let Some(TeXCommand::Primitive{cmd:PrimitiveCommand::Conditional(_),..}) = state.primitives().get_id(name) { incond += 1 }
                Ok(None)
            }
            _ => Ok(None)
        }
    },|a,s,m| {
        IncompleteConditional::recover(IncompleteConditional { name: PRIMITIVES.ifcase, line_no: m.line_number() }, a, s, m)
    })?;
    *engine.gullet.get_conditionals() = conds;
    Ok(())
}

/// Default implementation for [`Gullet::read_string`].
pub fn read_string<ET:EngineTypes>(engine:&mut EngineReferences<ET>,skip_eq:bool, ret:&mut String) -> TeXResult<(),ET> {
    let mut quoted = false;
    let mut had_eq = !skip_eq;
    let mut was_quoted = false;
    crate::expand_loop!(engine,token,
        ResolvedToken::Tk {char,code,..} => match code {
            CommandCode::Space if ret.is_empty() && !was_quoted => (),
            CommandCode::Space if quoted => ret.push(' '),
            CommandCode::Space => return Ok(()),
            CommandCode::Other if !had_eq && ret.is_empty() && matches!(char.try_into(),Ok(b'=')) => {
                had_eq = true;
            }
            _ => {
                if matches!(char.try_into(),Ok(b'\"')) {
                    was_quoted = true;
                    quoted = !quoted;
                } else {
                    char.display_fmt(ret);
                }
            }
        }
        ResolvedToken::Cmd(Some(TeXCommand::CharDef(c))) => c.display_fmt(ret),
        ResolvedToken::Cmd(Some(TeXCommand::Char{char,..})) => char.display_fmt(ret),
        ResolvedToken::Cmd(Some(TeXCommand::Primitive{cmd:PrimitiveCommand::Relax,..})) if !quoted => return Ok(()),
        ResolvedToken::Cmd(_) if !quoted => {
            engine.mouth.requeue(token);
            return Ok(())
        }
        o => todo!("unexpandable in read_string: {:?}",o)
    );
    todo!("file end")
}


fn is_ascii_digit(u:u8) -> bool { (48..=57).contains(&u) }
fn is_ascii_oct_digit(u:u8) -> bool { (48..=55).contains(&u) }
fn is_ascii_hex_digit(u:u8) -> bool { is_ascii_digit(u) || (65..=70).contains(&u) || (97..=102).contains(&u) }

pub struct NumContinuation<ET:EngineTypes> {
    pub is_negative:bool,
    pub next:Either<u8,(TeXCommand<ET>,ET::Token)>
}

/// Takes care of the boilerplate for reading a number/dimension/skip.
/// Expands [`Token`]s until an unexpandable [`Token`] is encountered; skips `=` if `skip_eq` is true,
/// returns `true` in the first component if an odd number of `-`-characters is encountered before the first digit,
/// returns in the second component either
/// the ASCII value of the fist simple [`Token`] encountered, or the relevant [`TeXCommand`]
/// in the case of a control sequence. Spaces are skipped.
pub fn read_numeric<ET:EngineTypes>(engine:&mut EngineReferences<ET>, skip_eq:bool)
                                    -> TeXResult<NumContinuation<ET>,ET> {
    let mut is_negative = false;
    let mut had_eq = !skip_eq;
    crate::expand_loop!(token => if token.is_primitive() == Some(PRIMITIVES.noexpand) {continue};engine,
        ResolvedToken::Tk {char,code,..} => match (char.try_into(),code) {
            (_,CommandCode::Space) => (),
            (Ok(b'='),CommandCode::Other) if !had_eq => {
                had_eq = true;
            }
            (Ok(b'-'),CommandCode::Other) => {
                is_negative = !is_negative;
            }
            (Ok(b'+'),CommandCode::Other) => (),
            (Ok(b),CommandCode::Other) => return Ok(NumContinuation{is_negative,next:either::Left(b)}),
            _ => todo!("number expected; got: {} {:?} -- l. {}",(char.try_into().ok().unwrap() as char),code,engine.mouth.line_number())
        }
        ResolvedToken::Cmd(Some(TeXCommand::Char {char,code})) => match ((*char).try_into(),code) {
            (_,CommandCode::Space) => (),
            (Ok(b'='),CommandCode::Other) if !had_eq => {
                had_eq = true;
            }
            (Ok(b'-'),CommandCode::Other) => {
                is_negative = !is_negative;
            }
            (Ok(b'+'),CommandCode::Other) => (),
            (Ok(b),CommandCode::Other) => return Ok(NumContinuation{is_negative,next:either::Left(b)}),
            _ => todo!("number expected")
        }
        ResolvedToken::Cmd(None) =>
            Undefined(token).throw(engine.aux,engine.state,engine.mouth)?,
        ResolvedToken::Cmd(Some(cmd)) => return Ok(NumContinuation{is_negative,next:either::Right((cmd.clone(),token))})
    );
    todo!("file end")
}

/// default implementation for [`Gullet::read_int`]
pub fn read_int<ET:EngineTypes>(engine:&mut EngineReferences<ET>, skip_eq:bool) -> TeXResult<ET::Int,ET> {
    let NumContinuation{is_negative,next} = read_numeric(engine, skip_eq)?;
    match next {
        Either::Left(b) => read_int_byte(engine,is_negative,b),
        Either::Right((cmd,token)) => read_int_command(engine,is_negative,cmd,token)
    }
}

/// reads an integer literal starting with `b`.
pub fn read_int_byte<ET:EngineTypes>(engine:&mut EngineReferences<ET>, is_negative:bool,b:u8) -> TeXResult<ET::Int,ET> {
    match b {
        b'\"' => read_hex_int(engine, is_negative),
        b'\'' => read_oct_int(engine, is_negative),
        b'`' => read_int_char(engine,is_negative),
        b if is_ascii_digit(b) => read_dec_int(engine, is_negative, b),
        _ => todo!("number expected")
    }
}

/// reads a character literal triggered by a backtick, when a number is expected
fn read_int_char<ET:EngineTypes>(engine:&mut EngineReferences<ET>, is_negative:bool) -> TeXResult<ET::Int,ET> {
    let next = loop {match engine.mouth.get_next(engine.aux, engine.state)? {
        Some(t) => break t,
        None => todo!("throw error here"),
    }};
    let ret = match next.to_enum() {
        StandardToken::Character(c, _) => {
            let i: u64 = c.into();
            match <ET::Num as NumSet>::Int::try_from(i as i64) {
                Ok(v) => if is_negative { -v } else { v },
                _ => todo!("throw error here")
            }
        }
        StandardToken::ControlSequence(cs) => {
            let resolved = engine.aux.memory.cs_interner().resolve(&cs);
            if resolved.len() == 1 {
                let c: ET::Char = resolved.iter().next().unwrap();
                if is_negative {
                    -<ET::Num as NumSet>::Int::from(c.into() as i32)
                } else {
                    <ET::Num as NumSet>::Int::from(c.into() as i32)
                }
            } else  {
                todo!("throw error here")
            }
        }
        _ => todo!("throw error here")
    };
    crate::expand_loop!(engine,token,
        ResolvedToken::Tk{code,..} => {
            if code != CommandCode::Space {
                engine.requeue(token)?;
            }
            break
        }
        ResolvedToken::Cmd(Some(TeXCommand::Char {code:CommandCode::Space,..})) => {
            break
        }
        ResolvedToken::Cmd(_) => {
            engine.requeue(token)?;
            break
        }
    );
    Ok(ret)
}

/// reads an integer from some [`TeXCommand`] that should correspond to an integer value (e.g. `\count` or `\lastpenalty`).
pub fn read_int_command<ET:EngineTypes>(engine:&mut EngineReferences<ET>, is_negative:bool, cmd: TeXCommand<ET>, token:ET::Token) -> TeXResult<ET::Int,ET> {
    match cmd {
        TeXCommand::Primitive{cmd:PrimitiveCommand::Int{read,..},..} => {
            if is_negative {Ok(-read(engine,token)?)}
            else {read(engine,token)}
        },
        TeXCommand::Primitive{name,cmd:PrimitiveCommand::PrimitiveInt} => {
            Ok(if is_negative {-engine.state.get_primitive_int(name)}
            else {engine.state.get_primitive_int(name)}
            )
        }
        TeXCommand::IntRegister(u) => {
            let i = engine.state.get_int_register(u);
            Ok(if is_negative {-i}
            else {i})
        }
        TeXCommand::CharDef(c) => {
            let val:u64 = c.into();
            match <ET::Num as NumSet>::Int::try_from(val as i64) {
                Ok(val) => {
                    Ok(if is_negative {-val}
                    else {val})
                }
                _ => todo!("throw error")
            }
        }
        TeXCommand::MathChar(u) => {
            match <ET::Num as NumSet>::Int::try_from(u as i64) {
                Ok(val) => {
                    Ok(if is_negative {-val}
                    else {val})
                }
                _ => todo!("throw error")
            }
        }
        TeXCommand::DimRegister(u) => {
            let base = ET::Num::dim_to_int(engine.state.get_dim_register(u));
            Ok(if is_negative {-base} else {base})
        }
        TeXCommand::Primitive{name,cmd:PrimitiveCommand::PrimitiveDim} => {
            let base = ET::Num::dim_to_int(engine.state.get_primitive_dim(name));
            Ok(if is_negative {-base} else {base})
        }
        TeXCommand::Primitive{cmd:PrimitiveCommand::Dim{read,..},..} => {
            let base = ET::Num::dim_to_int((read)(engine,token)?);
            Ok(if is_negative {-base} else {base})
        }
        TeXCommand::SkipRegister(u) => {
            let base = ET::Num::dim_to_int(engine.state.get_skip_register(u).base);
            Ok(if is_negative {-base} else {base})
        }
        TeXCommand::Primitive{name,cmd:PrimitiveCommand::PrimitiveSkip} => {
            let base = ET::Num::dim_to_int(engine.state.get_primitive_skip(name).base);
            Ok(if is_negative {-base} else {base})
        }
        TeXCommand::Primitive {cmd:PrimitiveCommand::Skip{read,..},..} => {
            let base = ET::Num::dim_to_int((read)(engine,token)?.base);
            Ok(if is_negative {-base} else {base})
        }
        o => todo!("read_int: {:?}",o)
    }
}

fn read_dec_int<ET:EngineTypes>(engine:&mut EngineReferences<ET>, is_negative:bool,first:u8) -> TeXResult<ET::Int,ET> {
    let mut ret = (first - b'0') as i32;
    crate::expand_loop!(engine,token,
        ResolvedToken::Tk{char,code} => match (char.try_into(),code) {
            (Ok(b),CommandCode::Other) if is_ascii_digit(b) => {
                ret = 10*ret + ((b - b'0') as i32);
            }
            (_,CommandCode::Space) => return Ok(if is_negative {- ET::Int::from(ret)} else {ET::Int::from(ret)}),
            _  => {
                engine.requeue(token)?;
                return Ok(if is_negative {- ET::Int::from(ret)} else {ET::Int::from(ret)})
            }
        }
        ResolvedToken::Cmd(Some(TeXCommand::Char {code:CommandCode::Space,..})) => {
            return Ok(if is_negative {- ET::Int::from(ret)} else {ET::Int::from(ret)})
        }
        ResolvedToken::Cmd(_) => {
            engine.requeue(token)?;
            return Ok(if is_negative {- ET::Int::from(ret)} else {ET::Int::from(ret)})
        }
    );
    Ok(if is_negative {- ET::Int::from(ret)} else {ET::Int::from(ret)})
}

fn hex_to_num(b: u8) -> i64 {
    match b {
        b'0'..=b'9' => (b - b'0') as i64,
        b'A'..=b'F' => (10 + b - b'A') as i64,
        b'a'..=b'f' => (10 + b - b'a') as i64,
        _ => unreachable!(),
    }
}

fn read_hex_int<ET:EngineTypes>(engine:&mut EngineReferences<ET>, is_negative:bool) -> TeXResult<ET::Int,ET> {
    let mut ret = 0i64;
    let mut empty = true;
    crate::expand_loop!(engine,token,
        ResolvedToken::Tk{char,code} => match (char.try_into(),code) {
            (Ok(b),CommandCode::Other|CommandCode::Letter) if is_ascii_hex_digit(b) => {
                ret = 16*ret + hex_to_num(b);
                empty = false;
            }
            (_,CommandCode::Space) =>
                return Ok((if is_negative {-ret} else {ret}).try_into().unwrap_or_else(|_| todo!())),
            _ if !empty => {
                engine.requeue(token)?;
                return Ok((if is_negative {-ret} else {ret}).try_into().unwrap_or_else(|_| todo!())) //ret_hex(engine,ret,is_negative)
            }
            _ => {
                todo!("{}:{:?}",char,code);
            }
        }
        ResolvedToken::Cmd(Some(TeXCommand::Char {code:CommandCode::Space,..})) => {
            return Ok((if is_negative {-ret} else {ret}).try_into().unwrap_or_else(|_| todo!()))
        }
        ResolvedToken::Cmd(_) if !empty => {
            engine.mouth.requeue(token);
            return Ok((if is_negative {-ret} else {ret}).try_into().unwrap_or_else(|_| todo!()))
        }
        o => todo!("{:?}; current: {:?}",o,ret)
    );
    if empty {todo!()} else {
        Ok((if is_negative {-ret} else {ret}).try_into().unwrap_or_else(|_| todo!()))
    }
}

fn read_oct_int<ET:EngineTypes>(engine:&mut EngineReferences<ET>, is_negative:bool) -> TeXResult<ET::Int,ET> {
    let mut ret = 0i32;
    let mut empty = true;
    crate::expand_loop!(engine,token,
        ResolvedToken::Tk{char,code} => match (char.try_into(),code) {
            (Ok(b),CommandCode::Other) if is_ascii_oct_digit(b) => {
                ret = 8*ret + ((b - b'0') as i32);
                empty = false
            }
            (_,CommandCode::Space) if !empty => return Ok(if is_negative {- ET::Int::from(ret)} else {ET::Int::from(ret)}),
            _  if !empty => {
                engine.requeue(token)?;
                return Ok(if is_negative {- ET::Int::from(ret)} else {ET::Int::from(ret)})
            }
            _ => {
                todo!("{}:{:?}",char,code);
            }
        }
        ResolvedToken::Cmd(Some(TeXCommand::Char {code:CommandCode::Space,..})) if !empty => {
            return Ok(if is_negative {- ET::Int::from(ret)} else {ET::Int::from(ret)})
        }
        ResolvedToken::Cmd(_) if !empty => {
            engine.mouth.requeue(token);
            return Ok(if is_negative {- ET::Int::from(ret)} else {ET::Int::from(ret)})
        }
        o => todo!("{:?}; current: {:?}",o,ret)
    );
    if empty {todo!()} else {
        Ok(if is_negative {- ET::Int::from(ret)} else {ET::Int::from(ret)})
    }
}

/// Default implementation for [`Gullet::read_dim`].
pub fn read_dim<ET:EngineTypes>(engine:&mut EngineReferences<ET>, skip_eq:bool) -> TeXResult<ET::Dim,ET> {
    let NumContinuation{is_negative,next} = read_numeric(engine, skip_eq)?;
    match next {
        Either::Left(b) => read_dim_byte(engine,is_negative,b),
        Either::Right((cmd,token)) => read_dim_command(engine,is_negative,cmd,token)
    }
}

/// reads a dimension literal starting with `b`.
pub fn read_dim_byte<ET:EngineTypes>(engine:&mut EngineReferences<ET>, is_negative:bool,b:u8) -> TeXResult<ET::Dim,ET> {
    if b == b',' || b == b'.' {
        read_dim_float(engine,is_negative,b'.')
    } else if b == b'`' {
        let i = read_int_char(engine,is_negative)?.into();
        read_unit_or_dim(engine,i as f64)
    }
    else if is_ascii_digit(b) {
        read_dim_float(engine,is_negative,b)
    } else {
        todo!("error?")
    }
}

/// reads a dimension from some [`TeXCommand`] that should correspond to a dimension value (e.g. `\dimen` or `\hsize`).
/// If the command is a skip, the base value is taken. If the command is an integer,
/// it looks for an appropriate unit.
pub fn read_dim_command<ET:EngineTypes>(engine:&mut EngineReferences<ET>, is_negative:bool, cmd: TeXCommand<ET>, token:ET::Token) -> TeXResult<ET::Dim,ET> {
    match cmd {
        TeXCommand::IntRegister(u) => {
            let i = engine.state.get_int_register(u);
            let i = if is_negative {-i} else {i};
            let i = i.into() as f64;
            read_unit_or_dim(engine,i)
        }
        TeXCommand::Primitive{name,cmd:PrimitiveCommand::PrimitiveInt} => {
            let i = engine.state.get_primitive_int(name);
            let i = if is_negative {-i} else {i};
            let i = i.into() as f64;
            read_unit_or_dim(engine,i)
        }
        TeXCommand::Primitive{cmd:PrimitiveCommand::Int{read,..},..} => {
            let i = if is_negative {-read(engine,token)?} else {read(engine,token)?};
            let f = i.into() as f64;
            read_unit_or_dim(engine,f)
        }
        TeXCommand::CharDef(c) => {
            let val = if is_negative {-(c.into() as f64)} else {c.into() as f64};
            read_unit_or_dim(engine,val)
        }
        TeXCommand::MathChar(c) => {
            let val = if is_negative {-(c as f64)} else {c as f64};
            read_unit_or_dim(engine,val)
        }
        TeXCommand::DimRegister(u) => {
            if is_negative {Ok(-engine.state.get_dim_register(u))}
            else {Ok(engine.state.get_dim_register(u))}
        },
        TeXCommand::Primitive {name,cmd:PrimitiveCommand::PrimitiveDim} => {
            let val = engine.state.get_primitive_dim(name);
            if is_negative {Ok(-val)} else {Ok(val)}
        }
        TeXCommand::Primitive{cmd:PrimitiveCommand::Dim{read,..},..} => {
            let val = read(engine,token)?;
            if is_negative {Ok(-val)} else {Ok(val)}
        }
        TeXCommand::SkipRegister(u) => {
            let val = engine.state.get_skip_register(u).base;
            if is_negative {Ok(-val)} else {Ok(val)}
        }
        TeXCommand::Primitive{name,cmd:PrimitiveCommand::PrimitiveSkip} => {
            let val = engine.state.get_primitive_skip(name).base;
            if is_negative {Ok(-val)} else {Ok(val)}
        }
        TeXCommand::Primitive {cmd:PrimitiveCommand::Skip{read,..},..} => {
            let val = read(engine,token)?.base;
            if is_negative {Ok(-val)} else {Ok(val)}
        }
        o => todo!("command in read_dim: {}",o.meaning(engine.aux.memory.cs_interner(),engine.state.get_catcode_scheme(),engine.state.get_escape_char()))
    }
}

fn read_dim_float<ET:EngineTypes>(engine:&mut EngineReferences<ET>, is_negative:bool, first:u8) -> TeXResult<ET::Dim,ET> {
    let mut ret = 0f64;
    let mut in_decimal = first == b'.';
    let mut fac = 10f64;
    if !in_decimal {
        ret = (first - b'0') as f64;
    }
    crate::expand_loop!(engine,token,
        ResolvedToken::Tk {char,code} => match (char.try_into(),code) {
            (Ok(b),CommandCode::Other) if is_ascii_digit(b) => {
                if in_decimal {
                    ret += (b - b'0') as f64 / fac;
                    fac *= 10.0;
                } else {
                    ret = 10.0*ret + ((b - b'0') as f64);
                }
            }
            (Ok(b','|b'.'),CommandCode::Other) => {
                if in_decimal { todo!("throw error") }
                in_decimal = true;
            }
            (_,CommandCode::Space) => {
                let f = if is_negative {-ret} else {ret};
                return read_unit_or_dim(engine,f)
            }
            (Ok(b),CommandCode::Other | CommandCode::Letter) => {
                let f = if is_negative {-ret} else {ret};
                return read_dim_unit(engine,f,Some((b,token)))
            }
            _ => {
                todo!("{}:{:?}",char,code);
            }
        }
        ResolvedToken::Cmd(Some(c)) => {
            let f = if is_negative {-ret} else {ret};
            return read_unit_cmd(engine,f,c.clone(),token)
        }
        _ => todo!("command in read_dim_inner")
    );
    todo!("read_dim_inner")
}

fn read_unit_or_dim<ET:EngineTypes>(engine:&mut EngineReferences<ET>,float:f64) -> TeXResult<ET::Dim,ET> {
    crate::expand_loop!(engine,token,
        ResolvedToken::Tk {char,code} => match (char.try_into(),code) {
            (Ok(b),CommandCode::Other | CommandCode::Letter) => {
                return read_dim_unit(engine,float,Some((b,token)))
            }
            (_,CommandCode::Space) => (),
            _ => {
                todo!("{}:{:?}",char,code);
            }
        }
        ResolvedToken::Cmd(Some(TeXCommand::Char {code:CommandCode::Space,..})) => (),
        ResolvedToken::Cmd(Some(TeXCommand::Char{char,code:CommandCode::Other | CommandCode::Letter})) => match (*char).try_into() {
            Ok(b) => return read_dim_unit(engine,float,Some((b,token))),
            _ => todo!("throw error")
        }
        ResolvedToken::Cmd(Some(cmd)) => return read_unit_cmd(engine,float,cmd.clone(),token),
        ResolvedToken::Cmd(None) =>
            Undefined(token).throw(engine.aux,engine.state,engine.mouth)?,
    );
    todo!("file end")
}

fn read_unit_cmd<ET:EngineTypes>(engine:&mut EngineReferences<ET>, float:f64,cmd:TeXCommand<ET>,token:ET::Token) -> TeXResult<ET::Dim,ET> {
    match cmd {
        TeXCommand::IntRegister(u) => {
            let base = ET::Dim::from_sp(engine.state.get_int_register(u).into() as i32);
            Ok(base.scale_float(float))
        }
        TeXCommand::Primitive{name,cmd:PrimitiveCommand::PrimitiveInt} => {
            let base = ET::Dim::from_sp(engine.state.get_primitive_int(name).into() as i32);
            Ok(base.scale_float(float))
        }
        TeXCommand::Primitive{cmd:PrimitiveCommand::Int{read,..},..} => {
            let base = ET::Dim::from_sp(read(engine,token)?.into() as i32);
            Ok(base.scale_float(float))
        }
        TeXCommand::DimRegister(u) => {
            let base = engine.state.get_dim_register(u);
            Ok(base.scale_float(float))
        }
        TeXCommand::Primitive{name,cmd:PrimitiveCommand::PrimitiveDim} => {
            let base = engine.state.get_primitive_dim(name);
            Ok(base.scale_float(float))
        }
        TeXCommand::Primitive{cmd:PrimitiveCommand::Dim{read,..},..} => {
            let base = read(engine,token)?;
            Ok(base.scale_float(float))
        }
        TeXCommand::SkipRegister(u) => {
            let base = engine.state.get_skip_register(u).base;
            Ok(base.scale_float(float))
        }
        TeXCommand::Primitive{name,cmd:PrimitiveCommand::PrimitiveSkip} => {
            let base = engine.state.get_primitive_skip(name).base;
            Ok(base.scale_float(float))
        }
        TeXCommand::Primitive{cmd:PrimitiveCommand::Skip{read,..},..} => {
            let base = read(engine,token)?.base;
            Ok(base.scale_float(float))
        }
        o => todo!("command in read_unit_or_dim: {:?}",o)
    }
}

fn read_dim_unit<ET:EngineTypes>(engine:&mut EngineReferences<ET>, mut float:f64, mut first:Option<(u8, ET::Token)>) -> TeXResult<ET::Dim,ET> {
    let is_true = match &first {
        Some((b't'|b'T',_)) => {
            read_keyword(engine,b"true",std::mem::take(&mut first))?
        },
        Some(_) => false,
        None => read_keyword(engine,b"true",None)?
    };
    if is_true {
        let mag = engine.state.get_primitive_int(PRIMITIVES.mag);
        float *= Into::<i64>::into(mag) as f64 / 1000.0;
    }
    let units = ET::Dim::UNITS;
    match read_keywords(engine,units,first)? {
        Some(d) => Ok(<ET::Num as NumSet>::Dim::from_float(engine,float,d)),
        _ => todo!("wut: {}",engine.preview())
    }
}

/// Default implementation for [`Gullet::read_skip`].
pub fn read_skip<ET:EngineTypes>(engine:&mut EngineReferences<ET>, skip_eq:bool) -> TeXResult<Skip<ET::Dim>,ET> {
    let NumContinuation{is_negative,next} = read_numeric(engine, skip_eq)?;
    match next {
        Either::Left(b) => read_skip_byte(engine,is_negative,b),
        Either::Right((cmd,token)) => read_skip_command(engine,is_negative,cmd,token)
    }
}

/// reads a skip literal starting with `b`.
pub fn read_skip_byte<ET:EngineTypes>(engine:&mut EngineReferences<ET>, is_negative:bool,b:u8) -> TeXResult<Skip<ET::Dim>,ET> {
    if b == b',' || b == b'.' {
        read_skip_dim(engine,is_negative,b'.')
    } else if is_ascii_digit(b) {
        read_skip_dim(engine,is_negative,b)
    } else {
        todo!("error?")
    }
}

/// reads a skip from some [`TeXCommand`] that should correspond to a skip value (e.g. `\skip` or `\lastskip`).
/// If the command only yields an integer or dimension, the remaining components of the skip are read.
pub fn read_skip_command<ET:EngineTypes>(engine:&mut EngineReferences<ET>, is_negative:bool, cmd: TeXCommand<ET>, token:ET::Token) -> TeXResult<Skip<ET::Dim>,ET> {
    match cmd {
        TeXCommand::IntRegister(u) => {
            let base = engine.state.get_int_register(u);
            let base = if is_negative {-base} else {base};
            let base = read_unit_or_dim(engine,base.into() as f64)?;
            read_skip_ii(engine,base)
        }
        TeXCommand::Primitive{name,cmd:PrimitiveCommand::PrimitiveInt} => {
            let base = engine.state.get_primitive_int(name);
            let base = if is_negative {-base} else {base};
            let base = read_unit_or_dim(engine,base.into() as f64)?;
            read_skip_ii(engine,base)
        }
        TeXCommand::Primitive {cmd:PrimitiveCommand::Int{read,..},..} => {
            let base = read(engine,token)?;
            let base = if is_negative {-base} else {base};
            let base = read_unit_or_dim(engine,base.into() as f64)?;
            read_skip_ii(engine,base)
        }
        TeXCommand::CharDef(c) => {
            let val:u64 = c.into();
            let base = read_unit_or_dim(engine,val as f64)?;
            read_skip_ii(engine,base)
        }
        TeXCommand::MathChar(u) => {
            let base = read_unit_or_dim(engine,u as f64)?;
            read_skip_ii(engine,base)
        }
        TeXCommand::DimRegister(u) => {
            let base = engine.state.get_dim_register(u);
            let base = if is_negative {-base} else {base};
            read_skip_ii(engine,base)
        }
        TeXCommand::Primitive{name,cmd:PrimitiveCommand::PrimitiveDim} => {
            let base = engine.state.get_primitive_dim(name);
            let base = if is_negative {-base} else {base};
            read_skip_ii(engine,base)
        }
        TeXCommand::Primitive{cmd:PrimitiveCommand::Dim{read,..},..} => {
            let base = read(engine,token)?;
            let base = if is_negative {-base} else {base};
            read_skip_ii(engine,base)
        }
        TeXCommand::SkipRegister(u) => {
            let base = engine.state.get_skip_register(u);
            Ok(if is_negative {-base} else {base})
        }
        TeXCommand::Primitive{name,cmd:PrimitiveCommand::PrimitiveSkip} => {
            let val = engine.state.get_primitive_skip(name);
            Ok(if is_negative {-val} else {val})
        }
        TeXCommand::Primitive{cmd:PrimitiveCommand::Skip{read,..},..} => {
            let val = read(engine,token)?;
            Ok(if is_negative {-val} else {val})
        }
        _ => todo!("read skip command: {:?}",cmd)
    }
}

const PLUS: &[u8] = b"plus";
const MINUS: &[u8] = b"minus";


fn read_skip_dim<ET:EngineTypes>(engine:&mut EngineReferences<ET>, is_negative:bool,first:u8) -> TeXResult<Skip<ET::Dim>,ET> {
    let base = read_dim_float(engine,is_negative,first)?;
    read_skip_ii(engine,base)
}

fn read_skip_ii<ET:EngineTypes>(engine:&mut EngineReferences<ET>, base:<ET::Num as NumSet>::Dim) -> TeXResult<Skip<ET::Dim>,ET> {
    match read_keywords(engine,&[PLUS,MINUS],None)? {
        Some(b) if b == PLUS => {
            let stretch = read_stretch(engine)?;
            if read_keyword(engine,MINUS,None)? {
                Ok(Skip::new(base,Some(stretch),Some(read_stretch(engine)?)))
            } else {
                Ok(Skip::new(base, Some(stretch), None))
            }
        }
        Some(b) if b == MINUS => {
            let shrink = read_stretch(engine)?;
            if read_keyword(engine,PLUS,None)? {
                Ok(Skip::new(base,Some(read_stretch(engine)?),Some(shrink)))
            } else {
                Ok(Skip::new(base,None,Some(shrink)))
            }
        }
        _ => Ok(Skip::new(base,None,None))
    }
}

fn read_stretch<ET:EngineTypes>(engine:&mut EngineReferences<ET>) -> TeXResult<StretchShrink<ET::Dim>,ET> {
    let mut is_negative = false;
    crate::expand_loop!(engine,token,
        ResolvedToken::Tk {char,code } => match (char.try_into(),code) {
            (_,CommandCode::Space) => (),
            (Ok(b'-'),CommandCode::Other) => {
                is_negative = !is_negative;
            }
            (Ok(b'+'),CommandCode::Other) => (),
            (Ok(b),CommandCode::Other) if is_ascii_digit(b) => return read_stretch_float(engine,is_negative,b),
            (Ok(b','|b'.'),CommandCode::Other) => return read_stretch_float(engine,is_negative,b'.'),
            _ => todo!("error?")
        }
        ResolvedToken::Cmd(cmd) => match cmd {
            Some(TeXCommand::DimRegister(u)) => {
                let base = engine.state.get_dim_register(*u);
                return Ok(StretchShrink::Dim(if is_negative {-base} else {base}))
            }
            Some(TeXCommand::Primitive{name,cmd:PrimitiveCommand::PrimitiveDim}) => {
                let base = engine.state.get_primitive_dim(*name);
                return Ok(StretchShrink::Dim(if is_negative {-base} else {base}))
            }
            Some(TeXCommand::Primitive{cmd:PrimitiveCommand::Dim{read,..},..}) => {
                let base = read(engine,token)?;
                return Ok(StretchShrink::Dim(if is_negative {-base} else {base}))
            }
            Some(TeXCommand::IntRegister(u)) => {
                let base = engine.state.get_int_register(*u).into() as f64;
                return read_stretch_unit(engine,if is_negative {-base} else {base},None)
            }
            Some(TeXCommand::Primitive{name,cmd:PrimitiveCommand::PrimitiveInt}) => {
                let base = engine.state.get_primitive_int(*name).into() as f64;
                return read_stretch_unit(engine,if is_negative {-base} else {base},None)
            }
            Some(TeXCommand::Primitive{cmd:PrimitiveCommand::Int{read,..},..}) => {
                let base = read(engine,token)?.into() as f64;
                return read_stretch_unit(engine,if is_negative {-base} else {base},None)
            }
            Some(TeXCommand::CharDef(c)) => {
                let base = Into::<u64>::into(*c) as f64;
                return read_stretch_unit(engine,if is_negative {-base} else {base},None)
            }
            o => todo!("command in read_stretch: {:?}",o)
        }
    );
    MissingNumber.throw(engine.aux,engine.state,engine.mouth)?;
    Ok(StretchShrink::Dim(ET::Dim::default()))
}
fn read_stretch_float<ET:EngineTypes>(engine:&mut EngineReferences<ET>, is_negative:bool, first:u8) -> TeXResult<StretchShrink<ET::Dim>,ET> {
    let mut ret = 0f64;
    let mut in_decimal = first == b'.';
    let mut scale = 10f64;
    if !in_decimal {
        ret = (first - b'0') as f64;
    }
    crate::expand_loop!(engine,token,
        ResolvedToken::Tk {char,code} => match (char.try_into(),code) {
            (Ok(b),CommandCode::Other) if is_ascii_digit(b) => {
                if in_decimal {
                    ret += (b - b'0') as f64 / scale;
                    scale *= 10.0;
                } else {
                    ret = 10.0*ret + ((b - b'0') as f64);
                }
            }
            (Ok(b','|b'.'),CommandCode::Other) => {
                if in_decimal { todo!("throw error") }
                in_decimal = true;
            }
            (_,CommandCode::Space) => {
                let f = if is_negative {-ret} else {ret};
                return read_stretch_unit(engine,f,None)
            }
            (Ok(b),CommandCode::Other | CommandCode::Letter) => {
                let f = if is_negative {-ret} else {ret};
                return read_stretch_unit(engine,f,Some((b,token)))
            }
            _ => {
                todo!("{}:{:?}",char,code);
            }
        }
        ResolvedToken::Cmd(Some(TeXCommand::Primitive {name,cmd:PrimitiveCommand::PrimitiveDim})) => {
            let base = engine.state.get_primitive_dim(*name);
            let scale = if is_negative {-ret} else {ret};
            return Ok(StretchShrink::Dim(base.scale_float(scale)))
        }
        ResolvedToken::Cmd(Some(TeXCommand::DimRegister(u))) => {
            let base = engine.state.get_dim_register(*u);
            let scale = if is_negative {-ret} else {ret};
            return Ok(StretchShrink::Dim(base.scale_float(scale)))
        }
        ResolvedToken::Cmd(Some(TeXCommand::Primitive {name,cmd:PrimitiveCommand::PrimitiveSkip})) => {
            let base = engine.state.get_primitive_skip(*name).base;
            let scale = if is_negative {-ret} else {ret};
            return Ok(StretchShrink::Dim(base.scale_float(scale)))
        }
        ResolvedToken::Cmd(Some(TeXCommand::SkipRegister(u))) => {
            let base = engine.state.get_skip_register(*u).base;
            let scale = if is_negative {-ret} else {ret};
            return Ok(StretchShrink::Dim(base.scale_float(scale)))
        }
        o => todo!("command in read_stretch_inner: {:?}",o)
    );
    todo!("read_dim_inner")
}
fn read_stretch_unit<ET:EngineTypes>(engine:&mut EngineReferences<ET>,mut float:f64,mut first:Option<(u8,ET::Token)>) -> TeXResult<StretchShrink<ET::Dim>,ET> {
    let is_true = match &first {
        Some((b't'|b'T',_)) => {
            read_keyword(engine,b"true",std::mem::take(&mut first))?
        },
        Some(_) => false,
        None => read_keyword(engine,b"true",None)?
    };
    if is_true {
        let mag = engine.state.get_primitive_int(PRIMITIVES.mag);
        float *= Into::<i64>::into(mag) as f64 / 1000.0;
    }
    match read_keywords(engine,STRETCH_SHRINK_UNITS,first)? {
        Some(d) => Ok(StretchShrink::from_float(engine,float,d)),
        _ => {
            let ret = read_unit_or_dim(engine,float)?;
            Ok(StretchShrink::Dim(ret))
        }
    }
}

/// Default implementation for [`Gullet::read_muskip`].
pub fn read_muskip<ET:EngineTypes>(engine:&mut EngineReferences<ET>, skip_eq:bool) -> TeXResult<MuSkip<ET::MuDim>,ET> {
    let NumContinuation{is_negative,next} = read_numeric(engine, skip_eq)?;
    match next {
        Either::Left(b) => read_muskip_byte(engine,is_negative,b,|d,e| read_muskip_ii(e,d)),
        Either::Right((cmd,token)) => read_muskip_command(engine,is_negative,cmd,token,|d,e| read_muskip_ii(e,d),Ok)
    }
}

/// Default implementation for [`Gullet::read_mudim`].
pub fn read_mudim<ET:EngineTypes>(engine:&mut EngineReferences<ET>, skip_eq:bool) -> TeXResult<ET::MuDim,ET> {
    let NumContinuation{is_negative,next} = read_numeric(engine, skip_eq)?;
    match next {
        Either::Left(b) => read_muskip_byte(engine,is_negative,b,|d,_| Ok(d)),
        Either::Right((cmd,token)) => read_muskip_command(engine,is_negative,cmd,token,|d,_| Ok(d),|s| Ok(s.base))
    }
}

/// reads a muskip or mudim literal starting with `b`.
pub fn read_muskip_byte<R,ET:EngineTypes>(engine:&mut EngineReferences<ET>, is_negative:bool,b:u8,kern:fn(ET::MuDim,&mut EngineReferences<ET>) -> TeXResult<R,ET>) -> TeXResult<R,ET> {
    if b == b',' || b == b'.' {
        read_muskip_dim(engine,is_negative,b'.',kern)
    } else if is_ascii_digit(b) {
        read_muskip_dim(engine,is_negative,b,kern)
    } else {
        todo!("error?")
    }
}

/// reads a muskip or mudim value from some [`TeXCommand`] that should correspond to a muskip value (e.g. `\muskip` or `\lastskip`).
pub fn read_muskip_command<R,ET:EngineTypes>(engine:&mut EngineReferences<ET>, is_negative:bool, cmd: TeXCommand<ET>, token:ET::Token, kern:fn(ET::MuDim, &mut EngineReferences<ET>) -> TeXResult<R,ET>, skip:fn(MuSkip<ET::MuDim>) -> TeXResult<R,ET>) -> TeXResult<R,ET> {
    match cmd {
        TeXCommand::MuSkipRegister(u) => {
            let base = engine.state.get_muskip_register(u);
            let base = if is_negative {-base} else {base};
            skip(base)
        }
        TeXCommand::Primitive{name,cmd:PrimitiveCommand::PrimitiveMuSkip} => {
            let base = engine.state.get_primitive_muskip(name);
            let base = if is_negative {-base} else {base};
            skip(base)
        }
        TeXCommand::Primitive{cmd:PrimitiveCommand::MuSkip{read,..},..} => {
            let base = read(engine,token)?;
            let base = if is_negative {-base} else {base};
            skip(base)
        }
        TeXCommand::CharDef(c) => {
            let base = c.into() as i64;
            let base = (if is_negative {-base} else {base}) as f64;
            let base = read_mudim_unit(engine,base,None)?;
            kern(base,engine)
        }
        TeXCommand::IntRegister(u) => {
            let base = engine.state.get_int_register(u);
            let base = (if is_negative {-base} else {base}).into() as f64;
            let base = read_mudim_unit(engine,base,None)?;
            kern(base,engine)
        }
        _ => todo!("read skip command: {:?}",cmd)
    }
}


fn read_muskip_dim<R,ET:EngineTypes>(engine:&mut EngineReferences<ET>, is_negative:bool,first:u8,kern:fn(ET::MuDim,&mut EngineReferences<ET>) -> TeXResult<R,ET>) -> TeXResult<R,ET> {
    let base = read_mudim_float(engine,is_negative,first)?;
    kern(base,engine)
}

fn read_mudim_float<ET:EngineTypes>(engine:&mut EngineReferences<ET>, is_negative:bool, first:u8) -> TeXResult<ET::MuDim,ET> {
    let mut ret = 0f64;
    let mut in_decimal = first == b'.';
    let mut fac = 10f64;
    if !in_decimal {
        ret = (first - b'0') as f64;
    }
    crate::expand_loop!(engine,token,
        ResolvedToken::Tk {char,code} => match (char.try_into(),code) {
            (Ok(b),CommandCode::Other) if is_ascii_digit(b) => {
                if in_decimal {
                    ret += (b - b'0') as f64 / fac;
                    fac *= 10.0;
                } else {
                    ret = 10.0*ret + ((b - b'0') as f64);
                }
            }
            (Ok(b','|b'.'),CommandCode::Other) => {
                if in_decimal { todo!("throw error") }
                in_decimal = true;
            }
            (_,CommandCode::Space) => {
                let f = if is_negative {-ret} else {ret};
                return read_mudim_unit(engine,f,None)
            }
            (Ok(b),CommandCode::Other | CommandCode::Letter) => {
                let f = if is_negative {-ret} else {ret};
                return read_mudim_unit(engine,f,Some((b,token)))
            }
            _ => {
                todo!("{}:{:?}",char,code);
            }
        }
        _ => todo!("command in read_dim_inner")
    );
    todo!("read_dim_inner")
}

fn read_mudim_unit<ET:EngineTypes>(engine:&mut EngineReferences<ET>, float:f64, first:Option<(u8, ET::Token)>) -> TeXResult<ET::MuDim,ET> {
    let units = ET::MuDim::UNITS;
    match read_keywords(engine,units,first)? {
        Some(d) => Ok(ET::MuDim::from_float(engine,float,d)),
        _ => todo!("wut")
    }
}

pub(crate) fn read_muskip_ii<ET:EngineTypes>(engine:&mut EngineReferences<ET>, base:ET::MuDim) -> TeXResult<MuSkip<ET::MuDim>,ET> {
    match read_keywords(engine,&[PLUS,MINUS],None)? {
        Some(b) if b == PLUS => {
            let stretch = read_mustretch(engine)?;
            Ok(if read_keyword(engine,MINUS,None)? {
                MuSkip::new(base,Some(stretch),Some(read_mustretch(engine)?))
            } else {
                MuSkip::new(base, Some(stretch), None)
            })
        }
        Some(b) if b == MINUS => {
            let shrink = read_mustretch(engine)?;
            Ok(if read_keyword(engine,PLUS,None)? {
                MuSkip::new(base,Some(read_mustretch(engine)?),Some(shrink))
            } else {
                MuSkip::new(base,None,Some(shrink))
            })
        }
        _ => Ok(MuSkip::new(base,None,None))
    }
}

fn read_mustretch<ET:EngineTypes>(engine:&mut EngineReferences<ET>) -> TeXResult<MuStretchShrink<ET::MuDim>,ET> {
    let mut is_negative = false;
    crate::expand_loop!(engine,token,
        ResolvedToken::Tk {char,code} => match (char.try_into(),code) {
            (_,CommandCode::Space) => (),
            (Ok(b'-'),CommandCode::Other) => {
                is_negative = !is_negative;
            }
            (Ok(b'+'),CommandCode::Other) => (),
            (Ok(b),CommandCode::Other) if is_ascii_digit(b) => return read_mustretch_float(engine,is_negative,b),
            (Ok(b','|b'.'),CommandCode::Other) => return read_mustretch_float(engine,is_negative,b'.'),
            _ => todo!("error?")
        }
        _ => todo!("command in read_skip")
    );
    MissingNumber.throw(engine.aux,engine.state,engine.mouth)?;
    Ok(MuStretchShrink::Mu(ET::MuDim::default()))
}
fn read_mustretch_float<ET:EngineTypes>(engine:&mut EngineReferences<ET>, is_negative:bool, first:u8) -> TeXResult<MuStretchShrink<ET::MuDim>,ET> {
    let mut ret = 0f64;
    let mut in_decimal = first == b'.';
    let mut fac = 10f64;
    if !in_decimal {
        ret = (first - b'0') as f64;
    }
    crate::expand_loop!(engine,token,
        ResolvedToken::Tk {char,code} => match (char.try_into(),code) {
            (Ok(b),CommandCode::Other) if is_ascii_digit(b) => {
                if in_decimal {
                    ret += (b - b'0') as f64 / fac;
                    fac *= 10.0;
                } else {
                    ret = 10.0*ret + ((b - b'0') as f64);
                }
            }
            (Ok(b','|b'.'),CommandCode::Other) => {
                if in_decimal { todo!("throw error") }
                in_decimal = true;
            }
            (_,CommandCode::Space) => {
                let f = if is_negative {-ret} else {ret};
                return read_mustretch_unit(engine,f,None)
            }
            (Ok(b),CommandCode::Other | CommandCode::Letter) => {
                let f = if is_negative {-ret} else {ret};
                return read_mustretch_unit(engine,f,Some((b,token)))
            }
            _ => {
                todo!("{}:{:?}",char,code);
            }
        }
        _ => todo!("command in read_dim_inner")
    );
    todo!("read_dim_inner")
}
fn read_mustretch_unit<ET:EngineTypes>(engine:&mut EngineReferences<ET>,float:f64,first:Option<(u8,ET::Token)>) -> TeXResult<MuStretchShrink<ET::MuDim>,ET> {
    match read_keywords(engine,STRETCH_SHRINK_UNITS,first)? {
        Some(d) => Ok(MuStretchShrink::from_float(engine,float,d)),
        _ => match read_keywords(engine,ET::MuDim::UNITS,None)? {
            Some(d) => Ok(MuStretchShrink::from_float(engine,float,d)),
            _ => todo!("wut")
        }
    }
}


/// Default implementation for [`Gullet::read_keyword`].
pub fn read_keyword<ET:EngineTypes>(engine:&mut EngineReferences<ET>,kw:&[u8],first:Option<(u8,ET::Token)>) -> TeXResult<bool,ET> {
    let mut ret = arrayvec::ArrayVec::<_,20>::new();//engine.aux.memory.get_bytes();
    let mut read = arrayvec::ArrayVec::<_,20>::new();//engine.aux.memory.get_token_vec();
    if let Some((b,t)) = first {
        ret.push(b.to_ascii_lowercase());
        read.push(t);
    }
    crate::expand_loop!(engine,token,
        ResolvedToken::Tk {char,code} => match (char.try_into(),code) {
            (_,CommandCode::Space) if ret.is_empty() => (),
            (Ok(b),_) => {
                ret.push(b.to_ascii_lowercase());
                read.push(token);
                if !kw.starts_with(&ret) {
                    for t in read.into_iter().rev() {engine.requeue(t)?}
                    //engine.aux.memory.return_bytes(ret);
                    //engine.aux.memory.return_token_vec(read);
                    return Ok(false)
                }
                if kw.len() == ret.len() {
                    //engine.aux.memory.return_token_vec(read);
                    //engine.aux.memory.return_bytes(ret);
                    return Ok(true)
                }
            }
            _ => {
                read.push(token);
                for t in read.into_iter().rev() {engine.requeue(t)?}
                //engine.aux.memory.return_bytes(ret);
                //engine.aux.memory.return_token_vec(read);
                return Ok(false)
            }
        }
        ResolvedToken::Cmd(_) => {
            read.push(token);
            for t in read.into_iter().rev() {engine.requeue(t)?}
            //engine.aux.memory.return_bytes(ret);
            //engine.aux.memory.return_token_vec(read);
            return Ok(false)
        }
    );
    todo!("read_keyword")
}

/// Default implementation for [`Gullet::read_keywords`].
pub fn read_keywords<'a,ET:EngineTypes>(engine:&mut EngineReferences<ET>,kws:&[&'a[u8]],first:Option<(u8,ET::Token)>) -> TeXResult<Option<&'a[u8]>,ET> {
    let mut ret = arrayvec::ArrayVec::<_,20>::new();//engine.aux.memory.get_bytes();
    let mut read = arrayvec::ArrayVec::<_,20>::new();//engine.aux.memory.get_token_vec();
    if let Some((b,t)) = first {
        ret.push(b.to_ascii_lowercase());
        read.push(t);
    }
    crate::expand_loop!(engine,token,
        ResolvedToken::Tk {char,code} => match (char.try_into(),code) {
            (_,CommandCode::Space) if ret.is_empty() => (),
            (Ok(b),_) => {
                ret.push(b.to_ascii_lowercase());
                let curr = kws.iter().filter(|k| k.starts_with(&ret)).collect::<Vec<_>>();
                if curr.is_empty() {
                    ret.pop();
                    match kws.iter().find(|e| **e == ret.as_slice()) {
                        Some(w) => {
                            engine.requeue(token)?;
                            //engine.aux.memory.return_token_vec(read);
                            //engine.aux.memory.return_bytes(ret);
                            return Ok(Some(w))
                        }
                        None => {
                            engine.requeue(token)?;
                            for t in read.into_iter().rev() {engine.requeue(t)?}
                            //engine.aux.memory.return_bytes(ret);
                            //engine.aux.memory.return_token_vec(read);
                            return Ok(None)
                        }
                    }
                }
                read.push(token);
                if curr.len() == 1 && curr[0].len() == ret.len() {
                    //engine.aux.memory.return_token_vec(read);
                    //engine.aux.memory.return_bytes(ret);
                    return Ok(Some(curr[0]))
                }
            }
            _ => {
                let curr = kws.iter().filter(|k| k.starts_with(&ret)).collect::<Vec<_>>();
                match curr.iter().enumerate().find(|(_,b)| ***b == ret.as_slice()) {
                    Some((i,_)) => {
                        engine.requeue(token)?;
                        //engine.aux.memory.return_token_vec(read);
                        //engine.aux.memory.return_bytes(ret);
                        return Ok(Some(curr[i]))
                    }
                    _ => {
                        engine.requeue(token)?;
                        for t in read.into_iter().rev() {engine.requeue(t)?}
                        //engine.aux.memory.return_bytes(ret);
                        //engine.aux.memory.return_token_vec(read);
                        return Ok(None)
                    }
                }
            }
        }
        ResolvedToken::Cmd(_) => {
            let curr = kws.iter().filter(|k| k.starts_with(&ret)).collect::<Vec<_>>();
            match curr.iter().enumerate().find(|(_,b)| ***b == ret.as_slice()) {
                Some((i,_)) => {
                    engine.mouth.requeue(token);
                    //engine.aux.memory.return_token_vec(read);
                    //engine.aux.memory.return_bytes(ret);
                    return Ok(Some(curr[i]))
                }
                _ => {
                    engine.mouth.requeue(token);
                    for t in read.into_iter().rev() {engine.requeue(t)?}
                    //engine.aux.memory.return_bytes(ret);
                    //engine.aux.memory.return_token_vec(read);
                    return Ok(None)
                }
            }
        }
    );
    todo!("read_keyword")
}

/// Default implementation for [`Gullet::read_chars`].
pub fn read_chars<ET:EngineTypes>(engine:&mut EngineReferences<ET>,kws:&[u8]) -> TeXResult<Either<u8,ET::Token>,ET> {
    crate::expand_loop!(engine,token,
        ResolvedToken::Tk {char,code} => match (char.try_into(),code) {
            (_,CommandCode::Space) => (),
            (Ok(b),_) if kws.contains(&b) => {
                return Ok(Either::Left(b))
            }
            _ => {
                return Ok(Either::Right(token))
            }
        }
        ResolvedToken::Cmd(_) => {
            return Ok(Either::Right(token))
        }
    );
    todo!("read_chars")
}

/// Either a [`CSName`](crate::tex::tokens::control_sequences::CSName) or an active character
pub enum CSOrActiveChar<T:Token>{
    Active(T::Char),
    Name(T::CS)
}
impl<ET:EngineTypes> EngineReferences<'_,ET> {
    /// Reads a control sequence or active character from the [`Mouth`] and returns it as a
    /// [`CSOrActiveChar`].
    pub fn read_control_sequence(&mut self,in_token:&ET::Token) -> TeXResult<CSOrActiveChar<ET::Token>,ET> {
        loop {
            let token = self.need_next(false,in_token)?;
            match self.resolve(&token) {
                ResolvedToken::Cmd(Some(TeXCommand::Primitive {name,cmd:PrimitiveCommand::Expandable(f)})) =>
                    ET::Gullet::do_expandable(self,*name,token,*f)?,
                ResolvedToken::Cmd(Some(TeXCommand::Primitive{name,cmd:PrimitiveCommand::SimpleExpandable(f)})) =>
                    ET::Gullet::do_simple_expandable(self,*name,token,*f)?,
                ResolvedToken::Cmd(Some(TeXCommand::Primitive{name,cmd:PrimitiveCommand::Conditional(f)})) =>
                    ET::Gullet::do_conditional(self,*name,token,*f,false)?,
                ResolvedToken::Cmd(_) => {
                    let ret = match token.to_enum() {
                        StandardToken::Character(c, CommandCode::Active) => CSOrActiveChar::Active(c),
                        StandardToken::ControlSequence(cs) => CSOrActiveChar::Name(cs),
                        _ => unreachable!()
                    };
                    self.set_command(&ret, Some(TeXCommand::Primitive {name:PRIMITIVES.relax,cmd:PrimitiveCommand::Relax}), false);
                    return Ok(ret)
                }
                ResolvedToken::Tk {code:CommandCode::Space,..} => (),
                o => todo!("read_control_sequence: {:?}",o)
            }
        }
    }
}