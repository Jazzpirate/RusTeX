/*! Default implementations for [`Gullet`] methods.
*/
use crate::commands::{ActiveConditional, CharOrPrimitive, TeXCommand, PrimitiveCommand, ResolvedToken};
use crate::commands::primitives::PRIMITIVES;
use crate::engine::{EngineReferences, EngineTypes};
use crate::engine::mouth::Mouth;
use crate::tex::catcodes::CommandCode;
use crate::tex::numerics::{MuDim, MuSkip, MuStretchShrink, Numeric, NumSet, Skip, STRETCH_SHRINK_UNITS, StretchShrink};
use crate::engine::state::State;
use crate::tex_error;
use crate::engine::gullet::Gullet;
use crate::tex::tokens::token_lists::TokenList;
use crate::tex::tokens::control_sequences::{CSHandler, ResolvedCSName};
use crate::tex::numerics::TeXDimen;
use crate::tex::tokens::{StandardToken, Token};
use crate::tex::characters::Character;
use crate::engine::EngineAux;

/// processes the parameter signature `params` of a [`Macro`](crate::commands::Macro) by reading the relevant arguments;
/// storing them in `args`
pub fn read_arguments<ET:EngineTypes>(engine:&mut EngineReferences<ET>,args:&mut [Vec<ET::Token>;9],params:TokenList<ET::Token>,long:bool,token:&ET::Token) {
    let mut i = 1usize;
    let inner = &params.0;
    let mut next = &inner[0];
    loop {
        match next.is_argument_marker() {
            Some(a) => match inner.get(i) {
                Some(n) if n.is_argument_marker().is_some() =>
                    {next = n; i += 1;read_argument(engine,&mut args[a as usize],long,token)},
                None => {
                    read_argument(engine,&mut args[a as usize],long,token);
                    return
                },
                Some(o) => {
                    let mut delim = vec!(o.clone());
                    i += 1;
                    while let Some(n) = inner.get(i) {
                        if n.is_argument_marker().is_some() {break}
                        delim.push(n.clone());
                        i += 1;
                    }
                    read_delimited_argument(engine,&mut args[a as usize],&delim,long,token);
                    match inner.get(i) {
                        Some(n) => {next = n; i += 1},
                        _ => return ()
                    }
                }
            },
            _ => match engine.mouth.get_next_opt(engine.aux,engine.state) {
                Some(o) if o == *next =>
                    match inner.get(i) {
                        Some(n) => {next = n; i += 1},
                        _ => return ()
                    },
                Some(_) => tex_error!(engine,wrong_definition,token.clone()),
                _ => tex_error!(engine,missing_argument,token.clone())
            }
        }
    }
}

fn read_delimited_argument<ET:EngineTypes>(engine:&mut EngineReferences<ET>,arg:&mut Vec<ET::Token>,delim:&Vec<ET::Token>,long:bool,token:&ET::Token) {
    let par = engine.aux.memory.cs_interner().par();
    let last = delim.last().unwrap();
    let ends_with_bgroup = last.command_code() == CommandCode::BeginGroup;
    let mut remove_braces:Option<Option<usize>> = None;
    while let Some(t) = engine.get_next() {
        match t.command_code() {
            CommandCode::Primitive if t.is_primitive() == Some(PRIMITIVES.noexpand) => continue,
            CommandCode::BeginGroup if !ends_with_bgroup => {
                if arg.is_empty() { remove_braces = Some(None) }
                arg.push(t);
                let r = if long {
                    engine.read_until_endgroup(&token,
                        |_, _, t| arg.push(t)
                    )
                } else {
                    engine.read_until_endgroup(&token,
                        |_, _, t| if t.is_cs(&par) { todo!("\\par in read_argument") } else { arg.push(t) }
                    )
                };
                arg.push(r);
                match remove_braces {
                    Some(ref mut n @ None) => *n = Some(arg.len()),
                    _ => ()
                }
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
                    match engine.gullet.get_align_data() {
                        Some(a) => a.ingroups -= 1,
                        _ => ()
                    }
                }
                return
            }
        } else {
            arg.push(t);
        }
    }
    tex_error!(engine,missing_argument,token.clone())
}

fn read_argument<ET:EngineTypes>(engine:&mut EngineReferences<ET>,arg:&mut Vec<ET::Token>,long:bool,token:&ET::Token) {
    while let Some(t) = engine.mouth.get_next_opt(engine.aux,engine.state) {
        match t.command_code() {
            CommandCode::Primitive if t.is_primitive() == Some(PRIMITIVES.noexpand) => continue,
            CommandCode::Space => continue,
            CommandCode::BeginGroup => {
                if long {
                    engine.mouth.read_until_endgroup(
                        engine.aux,
                        engine.state,token,
                        |_, t| arg.push(t)
                    );
                } else {
                    let par = engine.aux.memory.cs_interner().par();
                    engine.mouth.read_until_endgroup(
                        engine.aux,
                        engine.state,token,
                        |_,t|
                            if t.is_cs(&par) {todo!("\\par in read_argument")} else {arg.push(t)}
                    );
                }
                return
            }
            _ => ()
        }
        arg.push(t);
        return
    }
    tex_error!(engine,missing_argument,token.clone())
}
/*
pub fn skip_arguments<ET:EngineTypes>(engine:&mut EngineReferences<ET>,params:TokenList<ET::Token>,long:bool,token:&ET::Token) {
    let mut i = 1usize;
    let inner = &params.0;
    let mut next = &inner[0];
    loop {
        match next.is_argument_marker() {
            Some(a) => match inner.get(i) {
                Some(n) if n.is_argument_marker().is_some() =>
                    {next = n; i += 1;
                        skip_argument(engine, long, token)},
                None => {
                    skip_argument(engine, long, token);
                    return
                },
                Some(o) => {
                    let mut delim = vec!(o.clone());
                    i += 1;
                    while let Some(n) = inner.get(i) {
                        if n.is_argument_marker().is_some() {break}
                        delim.push(n.clone());
                        i += 1;
                    }
                    read_delimited_argument(engine, &mut Vec::new(),&delim, long, token);
                    match inner.get(i) {
                        Some(n) => {next = n; i += 1},
                        _ => return ()
                    }
                }
            },
            _ => match engine.get_next() {
                Some(o) if o == *next =>
                    match inner.get(i) {
                        Some(n) => {next = n; i += 1},
                        _ => return ()
                    },
                _ => tex_error!(engine,missing_argument,token.clone())
            }
        }
    }
}

fn skip_argument<ET:EngineTypes>(engine:&mut EngineReferences<ET>,long:bool,token:&ET::Token) {
    while let Some(t) = engine.mouth.get_next_opt(engine.aux,engine.state) {
        match t.command_code() {
            CommandCode::Primitive if t.is_primitive() == Some(PRIMITIVES.noexpand) => continue,
            CommandCode::Space => continue,
            CommandCode::BeginGroup => {
                if long {
                    engine.mouth.read_until_endgroup(
                        engine.aux,
                        engine.state,
                        |_, t| {}
                    );
                } else {
                    let par = engine.aux.memory.cs_interner().par();
                    engine.mouth.read_until_endgroup(
                        engine.aux,
                        engine.state,
                        |_,t|
                            if t.is_cs(&par) {todo!("\\par in read_argument")} else {}
                    );
                }
                return
            }
            _ => ()
        }
        return
    }
    tex_error!(engine,missing_argument,token.clone())
}

 */

/// Default implementation for [`Gullet::expand_until_endgroup`].
pub fn expand_until_endgroup<ET:EngineTypes,Fn:FnMut(&mut EngineAux<ET>,&ET::State,ET::Token)>(engine:&mut EngineReferences<ET>,expand_protected:bool,edef_like:bool,token:&ET::Token,mut cont:Fn) {
    let mut ingroups = 0;
    while let Some(t) = engine.get_next() {
        match t.command_code() {
            CommandCode::EndGroup => {
                if ingroups == 0 { return }
                ingroups -= 1;
                cont(engine.aux,engine.state,t);
                continue
            }
            CommandCode::BeginGroup => {
                ingroups += 1;
                cont(engine.aux,engine.state,t);
                continue
            }
            CommandCode::Primitive if t.is_primitive() == Some(PRIMITIVES.noexpand) => {
                let next = engine.mouth.get_next_opt(engine.aux,engine.state).unwrap();
                cont(engine.aux,engine.state,next);
                continue
            }
            CommandCode::Escape | CommandCode::Active => match engine.resolve(&t) {
                ResolvedToken::Tk{..} => {
                    cont(engine.aux,engine.state,t)
                }
                ResolvedToken::Cmd(Some(TeXCommand::Macro(m))) if m.protected && !expand_protected =>
                    cont(engine.aux,engine.state,t),
                ResolvedToken::Cmd(Some(TeXCommand::Macro(m))) =>
                    ET::Gullet::do_macro(engine,m.clone(),t),
                ResolvedToken::Cmd(Some(TeXCommand::Primitive{name,cmd:PrimitiveCommand::Conditional(cond)})) =>
                    ET::Gullet::do_conditional(engine,*name,t,*cond,false),
                ResolvedToken::Cmd(Some(TeXCommand::Primitive{name,..})) if *name == PRIMITIVES.noexpand => {
                    let next = engine.get_next().unwrap();
                    cont(engine.aux,engine.state,next);
                }
                ResolvedToken::Cmd(Some(TeXCommand::Primitive{name,..})) if *name == PRIMITIVES.unexpanded => {
                    engine.expand_until_bgroup(false,&t);
                    engine.read_until_endgroup(token,|a,s,t|{
                        if edef_like && t.command_code() == CommandCode::Parameter {
                            cont(a,s,t.clone());
                        }
                        cont(a,s,t)
                    });
                }
                ResolvedToken::Cmd(Some(TeXCommand::Primitive{name,..})) if *name == PRIMITIVES.the => {
                    engine.do_the(|a, s, _, t| {
                        if t.command_code() == CommandCode::Parameter && edef_like {
                            cont(a,s,t.clone());
                        }
                        cont(a,s,t)
                    })
                }
                ResolvedToken::Cmd(Some(TeXCommand::Primitive{name,..})) if *name == PRIMITIVES.noexpand => {
                    match engine.get_next() {
                        Some(t) if t.command_code() != CommandCode::EndGroup =>
                            cont(engine.aux,engine.state,t),
                        _ => todo!("throw error")
                    }
                }
                ResolvedToken::Cmd(Some(TeXCommand::Primitive {name,cmd:PrimitiveCommand::SimpleExpandable(e)})) =>
                    ET::Gullet::do_simple_expandable(engine, *name, t, *e),
                ResolvedToken::Cmd(Some(TeXCommand::Primitive {name,cmd:PrimitiveCommand::Expandable(e)})) =>
                    ET::Gullet::do_expandable(engine,*name,t,*e),
                ResolvedToken::Cmd(_) => {
                    cont(engine.aux,engine.state,t)
                }
            }
            _ => cont(engine.aux,engine.state,t)
        }
    }
    todo!("throw error")
}

pub(crate) fn case_loop<ET:EngineTypes>(engine:&mut EngineReferences<ET>,idx:usize,token:&ET::Token) {
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
    engine.iterate(token,|_,state,t| {
        if !t.is_cs_or_active() { return true }
        match ET::Gullet::char_or_primitive(state,&t) {
            Some(CharOrPrimitive::Primitive(name)) if name == PRIMITIVES.r#else && incond == 0 => {
                conds.push(
                    ActiveConditional::Else(PRIMITIVES.ifcase)
                );
                false
            }
            Some(CharOrPrimitive::Primitive(name)) if name == PRIMITIVES.fi => {
                if incond == 0 {false}
                else {incond -= 1; true}
            }
            Some(CharOrPrimitive::Primitive(name)) if name == PRIMITIVES.or && incond == 0 => {
                curr_int = curr_int + one;
                if cond == curr_int {
                    conds.push(
                        ActiveConditional::Case(cond)
                    );
                    false
                }
                else {true}
            }
            Some(CharOrPrimitive::Primitive(name)) => {
                match state.primitives().get_id(name) {
                    Some(TeXCommand::Primitive{cmd:PrimitiveCommand::Conditional(_),..}) => incond += 1,
                    _ => ()
                }
                true
            }
            _ => true
        }
    });
    *engine.gullet.get_conditionals() = conds;
}

/// What to do on a false conditional - skips [`Token`]s until the next `\else` (if `allowelse` is true) or `\fi`
/// If `skipelse` is false, precisely one `\else` is skipped as well (this happens in `\ifcase` when the
/// appropriate case is already done, so the corresponding `\else` should be skipped).
pub fn false_loop<ET:EngineTypes>(engine:&mut EngineReferences<ET>,idx:usize,allowelse:bool,mut skipelse:bool,token:&ET::Token) {
    let (mut incond,cond) = {
        let conds = engine.gullet.get_conditionals();
        let ic = conds.len() - idx;
        for _ in 0..ic { conds.pop();}
        (ic,conds.pop().unwrap().name())
    };
    let mut conds = std::mem::take(engine.gullet.get_conditionals());
    engine.iterate(token,|_,state,t| {
        if !t.is_cs_or_active() { return true }
        match ET::Gullet::char_or_primitive(state,&t) {
            Some(CharOrPrimitive::Primitive(name)) if name == PRIMITIVES.r#else && incond == 0 => {
                if allowelse {
                    conds.push(
                        ActiveConditional::Else(cond)
                    );
                    false
                }
                else if skipelse {
                    skipelse = false;true
                }
                else {todo!("throw spurious else in false-loop error")}
            }
            Some(CharOrPrimitive::Primitive(name)) if name == PRIMITIVES.fi => {
                if incond == 0 {false}
                else {incond -= 1; true}
            }
            Some(CharOrPrimitive::Primitive(name)) => {
                match state.primitives().get_id(name) {
                    Some(TeXCommand::Primitive{cmd:PrimitiveCommand::Conditional(_),..}) => incond += 1,
                    _ => ()
                }
                true
            }
            _ => true
        }
    });
    *engine.gullet.get_conditionals() = conds;
}

/// Default implementation for [`Gullet::read_string`].
pub fn read_string<ET:EngineTypes>(engine:&mut EngineReferences<ET>,skip_eq:bool, ret:&mut String) {
    let mut quoted = false;
    let mut had_eq = !skip_eq;
    let mut was_quoted = false;
    crate::expand_loop!(engine,token,
        ResolvedToken::Tk {char,code,..} => match code {
            CommandCode::Space if ret.is_empty() && !was_quoted => (),
            CommandCode::Space if quoted => ret.push(' '),
            CommandCode::Space => return,
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
        ResolvedToken::Cmd(Some(TeXCommand::Primitive{cmd:PrimitiveCommand::Relax,..})) if !quoted => return,
        ResolvedToken::Cmd(_) if !quoted => {
            engine.mouth.requeue(token);
            return
        }
        o => todo!("unexpandable in read_string: {:?}",o)
    );
}


fn is_ascii_digit(u:u8) -> bool { u >= 48 && u <= 57 }
fn is_ascii_oct_digit(u:u8) -> bool { u >= 48 && u <= 55 }
fn is_ascii_hex_digit(u:u8) -> bool { is_ascii_digit(u) || (u >= 65 && u <= 70) || (u >= 97 && u <= 102) }

/// Takes care of the boilerplate for reading a number/dimension/skip.
/// Expands [`Token`]s until an unexpandable [`Token`] is encountered; skips `=` if `skip_eq` is true,
/// returns `true` in the first component if an odd number of `-`-characters is encountered before the first digit,
/// returns in the second component either
/// the ASCII value of the fist simple [`Token`] encountered, or the relevant [`TeXCommand`]
/// in the case of a control sequence. Spaces are skipped.
pub fn read_numeric<ET:EngineTypes>(engine:&mut EngineReferences<ET>, skip_eq:bool)
                                    -> (bool,Result<u8,(TeXCommand<ET>, ET::Token)>) {
    let mut is_negative = false;
    let mut had_eq = !skip_eq;
    crate::expand_loop!(engine,token,
        ResolvedToken::Tk {char,code,..} => match (char.try_into(),code) {
            (_,CommandCode::Space) => (),
            (Ok(b'='),CommandCode::Other) if !had_eq => {
                had_eq = true;
            }
            (Ok(b'-'),CommandCode::Other) => {
                is_negative = !is_negative;
            }
            (Ok(b'+'),CommandCode::Other) => (),
            (Ok(b),CommandCode::Other) => return (is_negative,Ok(b)),
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
            (Ok(b),CommandCode::Other) => return (is_negative,Ok(b)),
            _ => todo!("number expected")
        }
        ResolvedToken::Cmd(None) =>
            tex_error!(engine,undefined,token.clone()),
        ResolvedToken::Cmd(Some(cmd)) => return (is_negative,Err((cmd.clone(),token)))
    );
    todo!("file end")
}

/// default implementation for [`Gullet::read_int`]
pub fn read_int<ET:EngineTypes>(engine:&mut EngineReferences<ET>, skip_eq:bool) -> <ET::Num as NumSet>::Int {
    let (is_negative,r) = read_numeric(engine, skip_eq);
    match r {
        Ok(b) => read_int_byte(engine,is_negative,b),
        Err((cmd,token)) => read_int_command(engine,is_negative,cmd,token)
    }
}

/// reads an integer literal starting with `b`.
pub fn read_int_byte<ET:EngineTypes>(engine:&mut EngineReferences<ET>, is_negative:bool,b:u8) -> <ET::Num as NumSet>::Int {
    match b {
        b'\"' => read_hex_int(engine, is_negative),
        b'\'' => read_oct_int(engine, is_negative),
        b'`' => read_int_char(engine,is_negative),
        b if is_ascii_digit(b) => read_dec_int(engine, is_negative, b),
        _ => todo!("number expected")
    }
}

/// reads a character literal triggered by a backtick, when a number is expected
fn read_int_char<ET:EngineTypes>(engine:&mut EngineReferences<ET>, is_negative:bool) -> <ET::Num as NumSet>::Int {
    let next = engine.mouth.get_next_opt(engine.aux, engine.state);
    let ret = match next {
        Some(t) => match t.to_enum() {
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
        },
        None => todo!("throw error here")
    };
    crate::expand_loop!(engine,token,
        ResolvedToken::Tk{code,..} => {
            if code != CommandCode::Space {
                engine.requeue(token);
            }
            break
        }
        ResolvedToken::Cmd(Some(TeXCommand::Char {code:CommandCode::Space,..})) => {
            break
        }
        ResolvedToken::Cmd(_) => {
            engine.requeue(token);
            break
        }
    );
    ret
}

/// reads an integer from some [`TeXCommand`] that should correspond to an integer value (e.g. `\count` or `\lastpenalty`).
pub fn read_int_command<ET:EngineTypes>(engine:&mut EngineReferences<ET>, is_negative:bool, cmd: TeXCommand<ET>, token:ET::Token) -> <ET::Num as NumSet>::Int {
    match cmd {
        TeXCommand::Primitive{cmd:PrimitiveCommand::Int{read,..},..} => {
            if is_negative {-read(engine,token)}
            else {read(engine,token)}
        },
        TeXCommand::Primitive{name,cmd:PrimitiveCommand::PrimitiveInt} => {
            if is_negative {-engine.state.get_primitive_int(name)}
            else {engine.state.get_primitive_int(name)}
        }
        TeXCommand::IntRegister(u) => {
            let i = engine.state.get_int_register(u);
            if is_negative {-i}
            else {i}
        }
        TeXCommand::CharDef(c) => {
            let val:u64 = c.into();
            match <ET::Num as NumSet>::Int::try_from(val as i64) {
                Ok(val) => {
                    if is_negative {-val}
                    else {val}
                }
                _ => todo!("throw error")
            }
        }
        TeXCommand::MathChar(u) => {
            match <ET::Num as NumSet>::Int::try_from(u as i64) {
                Ok(val) => {
                    if is_negative {-val}
                    else {val}
                }
                _ => todo!("throw error")
            }
        }
        TeXCommand::DimRegister(u) => {
            let base = ET::Num::dim_to_int(engine.state.get_dim_register(u));
            if is_negative {-base} else {base}
        }
        TeXCommand::Primitive{name,cmd:PrimitiveCommand::PrimitiveDim} => {
            let base = ET::Num::dim_to_int(engine.state.get_primitive_dim(name));
            if is_negative {-base} else {base}
        }
        TeXCommand::Primitive{cmd:PrimitiveCommand::Dim{read,..},..} => {
            let base = ET::Num::dim_to_int((read)(engine,token));
            if is_negative {-base} else {base}
        }
        TeXCommand::SkipRegister(u) => {
            let base = ET::Num::dim_to_int(engine.state.get_skip_register(u).base);
            if is_negative {-base} else {base}
        }
        TeXCommand::Primitive{name,cmd:PrimitiveCommand::PrimitiveSkip} => {
            let base = ET::Num::dim_to_int(engine.state.get_primitive_skip(name).base);
            if is_negative {-base} else {base}
        }
        TeXCommand::Primitive {cmd:PrimitiveCommand::Skip{read,..},..} => {
            let base = ET::Num::dim_to_int((read)(engine,token).base);
            if is_negative {-base} else {base}
        }
        o => todo!("read_int: {:?}",o)
    }
}

fn read_dec_int<ET:EngineTypes>(engine:&mut EngineReferences<ET>, is_negative:bool,first:u8) -> <ET::Num as NumSet>::Int {
    let mut ret = (first - b'0') as i32;
    crate::expand_loop!(engine,token,
        ResolvedToken::Tk{char,code} => match (char.try_into(),code) {
            (Ok(b),CommandCode::Other) if is_ascii_digit(b) => {
                ret = 10*ret + ((b - b'0') as i32);
            }
            (_,CommandCode::Space) => return if is_negative {- ET::Int::from(ret)} else {ET::Int::from(ret)},
            _  => {
                engine.requeue(token);
                return if is_negative {- ET::Int::from(ret)} else {ET::Int::from(ret)}
            }
        }
        ResolvedToken::Cmd(Some(TeXCommand::Char {code:CommandCode::Space,..})) => {
            return if is_negative {- ET::Int::from(ret)} else {ET::Int::from(ret)}
        }
        ResolvedToken::Cmd(_) => {
            engine.mouth.requeue(token);
            return if is_negative {- ET::Int::from(ret)} else {ET::Int::from(ret)}
        }
    );
    if is_negative {- ET::Int::from(ret)} else {ET::Int::from(ret)}
}

fn hex_to_num(b: u8) -> i64 {
    match b {
        b'0'..=b'9' => (b - b'0') as i64,
        b'A'..=b'F' => (10 + b - b'A') as i64,
        b'a'..=b'f' => (10 + b - b'a') as i64,
        _ => unreachable!(),
    }
}

fn read_hex_int<ET:EngineTypes>(engine:&mut EngineReferences<ET>, is_negative:bool) -> <ET::Num as NumSet>::Int {
    let mut ret = 0i64;
    let mut empty = true;
    crate::expand_loop!(engine,token,
        ResolvedToken::Tk{char,code} => match (char.try_into(),code) {
            (Ok(b),CommandCode::Other|CommandCode::Letter) if is_ascii_hex_digit(b) => {
                ret = 16*ret + hex_to_num(b);
                empty = false;
            }
            (_,CommandCode::Space) =>
                return (if is_negative {-ret} else {ret}).try_into().unwrap_or_else(|_| todo!()),
            _ if !empty => {
                engine.requeue(token);
                return (if is_negative {-ret} else {ret}).try_into().unwrap_or_else(|_| todo!()) //ret_hex(engine,ret,is_negative)
            }
            _ => {
                todo!("{}:{:?}",char,code);
            }
        }
        ResolvedToken::Cmd(Some(TeXCommand::Char {code:CommandCode::Space,..})) => {
            return (if is_negative {-ret} else {ret}).try_into().unwrap_or_else(|_| todo!())
        }
        ResolvedToken::Cmd(_) if !empty => {
            engine.mouth.requeue(token);
            return (if is_negative {-ret} else {ret}).try_into().unwrap_or_else(|_| todo!())
        }
        o => todo!("{:?}; current: {:?}",o,ret)
    );
    if empty {todo!()} else {
        (if is_negative {-ret} else {ret}).try_into().unwrap_or_else(|_| todo!())
    }
}

fn read_oct_int<ET:EngineTypes>(engine:&mut EngineReferences<ET>, is_negative:bool) -> <ET::Num as NumSet>::Int {
    let mut ret = 0i32;
    let mut empty = true;
    crate::expand_loop!(engine,token,
        ResolvedToken::Tk{char,code} => match (char.try_into(),code) {
            (Ok(b),CommandCode::Other) if is_ascii_oct_digit(b) => {
                ret = 8*ret + ((b - b'0') as i32);
                empty = false
            }
            (_,CommandCode::Space) if !empty => return if is_negative {- ET::Int::from(ret)} else {ET::Int::from(ret)},
            _  if !empty => {
                engine.requeue(token);
                return if is_negative {- ET::Int::from(ret)} else {ET::Int::from(ret)}
            }
            _ => {
                todo!("{}:{:?}",char,code);
            }
        }
        ResolvedToken::Cmd(Some(TeXCommand::Char {code:CommandCode::Space,..})) if !empty => {
            return if is_negative {- ET::Int::from(ret)} else {ET::Int::from(ret)}
        }
        ResolvedToken::Cmd(_) if !empty => {
            engine.mouth.requeue(token);
            return if is_negative {- ET::Int::from(ret)} else {ET::Int::from(ret)}
        }
        o => todo!("{:?}; current: {:?}",o,ret)
    );
    if empty {todo!()} else {
        if is_negative {- ET::Int::from(ret)} else {ET::Int::from(ret)}
    }
}

/// Default implementation for [`Gullet::read_dim`].
pub fn read_dim<ET:EngineTypes>(engine:&mut EngineReferences<ET>, skip_eq:bool) -> <ET::Num as NumSet>::Dim {
    let (is_negative,r) = read_numeric(engine, skip_eq);
    match r {
        Ok(b) => read_dim_byte(engine,is_negative,b),
        Err((cmd,token)) => read_dim_command(engine,is_negative,cmd,token)
    }
}

/// reads a dimension literal starting with `b`.
pub fn read_dim_byte<ET:EngineTypes>(engine:&mut EngineReferences<ET>, is_negative:bool,b:u8) -> <ET::Num as NumSet>::Dim {
    if b == b',' || b == b'.' {
        read_dim_float(engine,is_negative,b'.')
    } else  if b == b'`' {
        let i = read_int_char(engine,is_negative).into();
        read_unit_or_dim(engine,i as f32)
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
pub fn read_dim_command<ET:EngineTypes>(engine:&mut EngineReferences<ET>, is_negative:bool, cmd: TeXCommand<ET>, token:ET::Token) -> <ET::Num as NumSet>::Dim {
    match cmd {
        TeXCommand::IntRegister(u) => {
            let i = engine.state.get_int_register(u);
            let i = if is_negative {-i} else {i};
            let i = i.into() as f32;
            read_unit_or_dim(engine,i)
        }
        TeXCommand::Primitive{name,cmd:PrimitiveCommand::PrimitiveInt} => {
            let i = engine.state.get_primitive_int(name);
            let i = if is_negative {-i} else {i};
            let i = i.into() as f32;
            read_unit_or_dim(engine,i)
        }
        TeXCommand::Primitive{cmd:PrimitiveCommand::Int{read,..},..} => {
            let i = if is_negative {-read(engine,token)} else {read(engine,token)};
            let f = i.into() as f32;
            read_unit_or_dim(engine,f)
        }
        TeXCommand::CharDef(c) => {
            let val = if is_negative {-(c.into() as f32)} else {c.into() as f32};
            read_unit_or_dim(engine,val)
        }
        TeXCommand::MathChar(c) => {
            let val = if is_negative {-(c as f32)} else {c as f32};
            read_unit_or_dim(engine,val)
        }
        TeXCommand::DimRegister(u) => {
            if is_negative {return -engine.state.get_dim_register(u)}
            else {return engine.state.get_dim_register(u)}
        },
        TeXCommand::Primitive {name,cmd:PrimitiveCommand::PrimitiveDim} => {
            let val = engine.state.get_primitive_dim(name);
            if is_negative {return -val} else {return val}
        }
        TeXCommand::Primitive{cmd:PrimitiveCommand::Dim{read,..},..} => {
            let val = read(engine,token);
            if is_negative {return -val} else {return val}
        }
        TeXCommand::SkipRegister(u) => {
            let val = engine.state.get_skip_register(u).base;
            if is_negative {return -val} else {return val}
        }
        TeXCommand::Primitive{name,cmd:PrimitiveCommand::PrimitiveSkip} => {
            let val = engine.state.get_primitive_skip(name).base;
            if is_negative {return -val} else {return val}
        }
        TeXCommand::Primitive {cmd:PrimitiveCommand::Skip{read,..},..} => {
            let val = read(engine,token).base;
            if is_negative {return -val} else {return val}
        }
        o => todo!("command in read_dim: {}",o.meaning(engine.aux.memory.cs_interner(),engine.state.get_catcode_scheme(),engine.state.get_escape_char()))
    }
}

fn read_dim_float<ET:EngineTypes>(engine:&mut EngineReferences<ET>, is_negative:bool, first:u8) -> <ET::Num as NumSet>::Dim {
    let mut ret = 0f32;
    let mut in_decimal = first == b'.';
    let mut fac = 10f32;
    if !in_decimal {
        ret = (first - b'0') as f32;
    }
    crate::expand_loop!(engine,token,
        ResolvedToken::Tk {char,code} => match (char.try_into(),code) {
            (Ok(b),CommandCode::Other) if is_ascii_digit(b) => {
                if in_decimal {
                    ret += (b - b'0') as f32 / fac;
                    fac *= 10.0;
                } else {
                    ret = 10.0*ret + ((b - b'0') as f32);
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
            let base = read_dim_command(engine,false,c.clone(),token);
            let f = if is_negative {-ret} else {ret};
            return base.scale_float(f)
        }
        _ => todo!("command in read_dim_inner")
    );
    todo!("read_dim_inner")
}

fn read_unit_or_dim<ET:EngineTypes>(engine:&mut EngineReferences<ET>,float:f32) -> <ET::Num as NumSet>::Dim {
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
        ResolvedToken::Cmd(Some(cmd)) => match cmd {
            TeXCommand::DimRegister(u) => {
                let base = engine.state.get_dim_register(*u);
                let scale = (float * 65536.0).round() as i32;
                return base.scale(scale.into(),65536.into())
            }
            TeXCommand::Primitive{name,cmd:PrimitiveCommand::PrimitiveSkip} => {
                let base = engine.state.get_primitive_skip(*name).base;
                let scale = (float * 65536.0).round() as i32;
                return base.scale(scale.into(),65536.into())
            }
            o => todo!("command in read_unit_or_dim: {:?}",o)
        }
        ResolvedToken::Cmd(None) =>
            tex_error!(engine,undefined,token.clone()),
    );
    todo!("file end")
}

fn read_dim_unit<ET:EngineTypes>(engine:&mut EngineReferences<ET>, mut float:f32, mut first:Option<(u8, ET::Token)>) -> <ET::Num as NumSet>::Dim {
    let is_true = match &first {
        Some((b't'|b'T',_)) => {
            read_keyword(engine,b"true",std::mem::take(&mut first))
        },
        Some(_) => false,
        None => read_keyword(engine,b"true",None)
    };
    if is_true {
        let mag = engine.state.get_primitive_int(PRIMITIVES.mag);
        float *= Into::<i64>::into(mag) as f32 / 1000.0;
    }
    let units = ET::Dim::UNITS;
    match read_keywords(engine,units,first) {
        Some(d) => <ET::Num as NumSet>::Dim::from_float(engine,float,d),
        _ => todo!("wut: {}",engine.preview())
    }
}

/// Default implementation for [`Gullet::read_skip`].
pub fn read_skip<ET:EngineTypes>(engine:&mut EngineReferences<ET>, skip_eq:bool) -> Skip<ET::Dim> {
    let (is_negative,r) = read_numeric(engine, skip_eq);
    match r {
        Ok(b) => read_skip_byte(engine,is_negative,b),
        Err((cmd,token)) => read_skip_command(engine,is_negative,cmd,token)
    }
}

/// reads a skip literal starting with `b`.
pub fn read_skip_byte<ET:EngineTypes>(engine:&mut EngineReferences<ET>, is_negative:bool,b:u8) -> Skip<ET::Dim> {
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
pub fn read_skip_command<ET:EngineTypes>(engine:&mut EngineReferences<ET>, is_negative:bool, cmd: TeXCommand<ET>, token:ET::Token) -> Skip<ET::Dim> {
    match cmd {
        TeXCommand::IntRegister(u) => {
            let base = engine.state.get_int_register(u);
            let base = if is_negative {-base} else {base};
            let base = read_unit_or_dim(engine,base.into() as f32);
            read_skip_ii(engine,base)
        }
        TeXCommand::Primitive{name,cmd:PrimitiveCommand::PrimitiveInt} => {
            let base = engine.state.get_primitive_int(name);
            let base = if is_negative {-base} else {base};
            let base = read_unit_or_dim(engine,base.into() as f32);
            read_skip_ii(engine,base)
        }
        TeXCommand::Primitive {cmd:PrimitiveCommand::Int{read,..},..} => {
            let base = read(engine,token);
            let base = if is_negative {-base} else {base};
            let base = read_unit_or_dim(engine,base.into() as f32);
            read_skip_ii(engine,base)
        }
        TeXCommand::CharDef(c) => {
            let val:u64 = c.into();
            let base = read_unit_or_dim(engine,val as f32);
            read_skip_ii(engine,base)
        }
        TeXCommand::MathChar(u) => {
            let base = read_unit_or_dim(engine,u as f32);
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
            let base = read(engine,token);
            let base = if is_negative {-base} else {base};
            read_skip_ii(engine,base)
        }
        TeXCommand::SkipRegister(u) => {
            let base = engine.state.get_skip_register(u);
            if is_negative {-base} else {base}
        }
        TeXCommand::Primitive{name,cmd:PrimitiveCommand::PrimitiveSkip} => {
            let val = engine.state.get_primitive_skip(name);
            if is_negative {return -val} else {return val}
        }
        TeXCommand::Primitive{cmd:PrimitiveCommand::Skip{read,..},..} => {
            let val = read(engine,token);
            if is_negative {return -val} else {return val}
        }
        _ => todo!("read skip command: {:?}",cmd)
    }
}

const PLUS: &[u8] = b"plus";
const MINUS: &[u8] = b"minus";


fn read_skip_dim<ET:EngineTypes>(engine:&mut EngineReferences<ET>, is_negative:bool,first:u8) -> Skip<ET::Dim> {
    let base = read_dim_float(engine,is_negative,first);
    read_skip_ii(engine,base)
}

fn read_skip_ii<ET:EngineTypes>(engine:&mut EngineReferences<ET>, base:<ET::Num as NumSet>::Dim) -> Skip<ET::Dim> {
    match read_keywords(engine,&[PLUS,MINUS],None) {
        Some(b) if b == PLUS => {
            let stretch = read_stretch(engine);
            if read_keyword(engine,MINUS,None) {
                Skip::new(base,Some(stretch),Some(read_stretch(engine)))
            } else {
                Skip::new(base, Some(stretch), None)
            }
        }
        Some(b) if b == MINUS => {
            let shrink = read_stretch(engine);
            if read_keyword(engine,PLUS,None) {
                Skip::new(base,Some(read_stretch(engine)),Some(shrink))
            } else {
                Skip::new(base,None,Some(shrink))
            }
        }
        _ => Skip::new(base,None,None)
    }
}

fn read_stretch<ET:EngineTypes>(engine:&mut EngineReferences<ET>) -> StretchShrink<ET::Dim> {
    let mut is_negative = false;
    crate::expand_loop!(engine,token,
        ResolvedToken::Tk {char,code } => match (char.try_into(),code) {
            (_,CommandCode::Space) => (),
            (Ok(b'-'),CommandCode::Other) => {
                is_negative =!is_negative;
            }
            (Ok(b'+'),CommandCode::Other) => (),
            (Ok(b),CommandCode::Other) if is_ascii_digit(b) => return read_stretch_float(engine,is_negative,b),
            (Ok(b','|b'.'),CommandCode::Other) => return read_stretch_float(engine,is_negative,b'.'),
            _ => todo!("error?")
        }
        ResolvedToken::Cmd(cmd) => match cmd {
            Some(TeXCommand::DimRegister(u)) => {
                let base = engine.state.get_dim_register(*u);
                return StretchShrink::Dim(if is_negative {-base} else {base})
            }
            Some(TeXCommand::Primitive{name,cmd:PrimitiveCommand::PrimitiveDim}) => {
                let base = engine.state.get_primitive_dim(*name);
                return StretchShrink::Dim(if is_negative {-base} else {base})
            }
            Some(TeXCommand::IntRegister(u)) => {
                let base = engine.state.get_int_register(*u).into() as f32;
                return read_stretch_unit(engine,if is_negative {-base} else {base},None)
            }
            Some(TeXCommand::Primitive{name,cmd:PrimitiveCommand::PrimitiveInt}) => {
                let base = engine.state.get_primitive_int(*name).into() as f32;
                return read_stretch_unit(engine,if is_negative {-base} else {base},None)
            }
            o => todo!("command in read_stretch: {:?}",o)
        }
    );
    tex_error!(engine,missing_number);
    StretchShrink::Dim(ET::Dim::default())
}
fn read_stretch_float<ET:EngineTypes>(engine:&mut EngineReferences<ET>, is_negative:bool, first:u8) -> StretchShrink<ET::Dim> {
    let mut ret = 0f32;
    let mut in_decimal = first == b'.';
    let mut scale = 10f32;
    if !in_decimal {
        ret = (first - b'0') as f32;
    }
    crate::expand_loop!(engine,token,
        ResolvedToken::Tk {char,code} => match (char.try_into(),code) {
            (Ok(b),CommandCode::Other) if is_ascii_digit(b) => {
                if in_decimal {
                    ret += (b - b'0') as f32 / scale;
                    scale *= 10.0;
                } else {
                    ret = 10.0*ret + ((b - b'0') as f32);
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
        ResolvedToken::Cmd(Some(TeXCommand::DimRegister(u))) => {
            let base = engine.state.get_dim_register(*u);
            let scale = if is_negative {-ret} else {ret};
            return StretchShrink::Dim(base.scale_float(scale))
        }
        o => todo!("command in read_stretch_inner: {:?}",o)
    );
    todo!("read_dim_inner")
}
fn read_stretch_unit<ET:EngineTypes>(engine:&mut EngineReferences<ET>,mut float:f32,mut first:Option<(u8,ET::Token)>) -> StretchShrink<ET::Dim> {
    let is_true = match &first {
        Some((b't'|b'T',_)) => {
            read_keyword(engine,b"true",std::mem::take(&mut first))
        },
        Some(_) => false,
        None => read_keyword(engine,b"true",None)
    };
    if is_true {
        let mag = engine.state.get_primitive_int(PRIMITIVES.mag);
        float *= Into::<i64>::into(mag) as f32 / 1000.0;
    }
    match read_keywords(engine,STRETCH_SHRINK_UNITS,first) {
        Some(d) => StretchShrink::from_float(engine,float,d),
        _ => match read_keywords(engine,ET::Dim::UNITS,None) {
            Some(d) => StretchShrink::from_float(engine,float,d),
            _ => todo!("wut: {}\n  (l.{})",engine.preview(),engine.mouth.line_number())
        }
    }
}

/// Default implementation for [`Gullet::read_muskip`].
pub fn read_muskip<ET:EngineTypes>(engine:&mut EngineReferences<ET>, skip_eq:bool) -> MuSkip<ET::MuDim> {
    let (is_negative,r) = read_numeric(engine, skip_eq);
    match r {
        Ok(b) => read_muskip_byte(engine,is_negative,b,|d,e| read_muskip_ii(e,d)),
        Err((cmd,token)) => read_muskip_command(engine,is_negative,cmd,token,|d,e| read_muskip_ii(e,d),|s| s)
    }
}

/// Default implementation for [`Gullet::read_mudim`].
pub fn read_mudim<ET:EngineTypes>(engine:&mut EngineReferences<ET>, skip_eq:bool) -> ET::MuDim {
    let (is_negative,r) = read_numeric(engine, skip_eq);
    match r {
        Ok(b) => read_muskip_byte(engine,is_negative,b,|d,_| d),
        Err((cmd,token)) => read_muskip_command(engine,is_negative,cmd,token,|d,_| d,|s| s.base)
    }
}

/// reads a muskip or mudim literal starting with `b`.
pub fn read_muskip_byte<R,ET:EngineTypes>(engine:&mut EngineReferences<ET>, is_negative:bool,b:u8,kern:fn(ET::MuDim,&mut EngineReferences<ET>) -> R) -> R {
    if b == b',' || b == b'.' {
        read_muskip_dim(engine,is_negative,b'.',kern)
    } else if is_ascii_digit(b) {
        read_muskip_dim(engine,is_negative,b,kern)
    } else {
        todo!("error?")
    }
}

/// reads a muskip or mudim value from some [`TeXCommand`] that should correspond to a muskip value (e.g. `\muskip` or `\lastskip`).
pub fn read_muskip_command<R,ET:EngineTypes>(engine:&mut EngineReferences<ET>, is_negative:bool, cmd: TeXCommand<ET>, token:ET::Token, kern:fn(ET::MuDim, &mut EngineReferences<ET>) -> R, skip:fn(MuSkip<ET::MuDim>) -> R) -> R {
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
            let base = read(engine,token);
            let base = if is_negative {-base} else {base};
            skip(base)
        }
        TeXCommand::CharDef(c) => {
            let base = c.into() as i64;
            let base = (if is_negative {-base} else {base}) as f32;
            let base = read_mudim_unit(engine,base,None);
            kern(base,engine)
        }
        TeXCommand::IntRegister(u) => {
            let base = engine.state.get_int_register(u);
            let base = (if is_negative {-base} else {base}).into() as f32;
            let base = read_mudim_unit(engine,base,None);
            kern(base,engine)
        }
        _ => todo!("read skip command: {:?}",cmd)
    }
}


fn read_muskip_dim<R,ET:EngineTypes>(engine:&mut EngineReferences<ET>, is_negative:bool,first:u8,kern:fn(ET::MuDim,&mut EngineReferences<ET>) -> R) -> R {
    let base = read_mudim_float(engine,is_negative,first);
    kern(base,engine)
}

fn read_mudim_float<ET:EngineTypes>(engine:&mut EngineReferences<ET>, is_negative:bool, first:u8) -> ET::MuDim {
    let mut ret = 0f32;
    let mut in_decimal = first == b'.';
    let mut fac = 10f32;
    if !in_decimal {
        ret = (first - b'0') as f32;
    }
    crate::expand_loop!(engine,token,
        ResolvedToken::Tk {char,code} => match (char.try_into(),code) {
            (Ok(b),CommandCode::Other) if is_ascii_digit(b) => {
                if in_decimal {
                    ret += (b - b'0') as f32 / fac;
                    fac *= 10.0;
                } else {
                    ret = 10.0*ret + ((b - b'0') as f32);
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

fn read_mudim_unit<ET:EngineTypes>(engine:&mut EngineReferences<ET>, float:f32, first:Option<(u8, ET::Token)>) -> ET::MuDim {
    let units = ET::MuDim::UNITS;
    match read_keywords(engine,units,first) {
        Some(d) => ET::MuDim::from_float(engine,float,d),
        _ => todo!("wut")
    }
}

pub(crate) fn read_muskip_ii<ET:EngineTypes>(engine:&mut EngineReferences<ET>, base:ET::MuDim) -> MuSkip<ET::MuDim> {
    match read_keywords(engine,&[PLUS,MINUS],None) {
        Some(b) if b == PLUS => {
            let stretch = read_mustretch(engine);
            if read_keyword(engine,MINUS,None) {
                MuSkip::new(base,Some(stretch),Some(read_mustretch(engine)))
            } else {
                MuSkip::new(base, Some(stretch), None)
            }
        }
        Some(b) if b == MINUS => {
            let shrink = read_mustretch(engine);
            if read_keyword(engine,PLUS,None) {
                MuSkip::new(base,Some(read_mustretch(engine)),Some(shrink))
            } else {
                MuSkip::new(base,None,Some(shrink))
            }
        }
        _ => MuSkip::new(base,None,None)
    }
}

fn read_mustretch<ET:EngineTypes>(engine:&mut EngineReferences<ET>) -> MuStretchShrink<ET::MuDim> {
    let mut is_negative = false;
    crate::expand_loop!(engine,token,
        ResolvedToken::Tk {char,code} => match (char.try_into(),code) {
            (_,CommandCode::Space) => (),
            (Ok(b'-'),CommandCode::Other) => {
                is_negative =!is_negative;
            }
            (Ok(b'+'),CommandCode::Other) => (),
            (Ok(b),CommandCode::Other) if is_ascii_digit(b) => return read_mustretch_float(engine,is_negative,b),
            (Ok(b','|b'.'),CommandCode::Other) => return read_mustretch_float(engine,is_negative,b'.'),
            _ => todo!("error?")
        }
        _ => todo!("command in read_skip")
    );
    tex_error!(engine,missing_number);
    MuStretchShrink::Mu(ET::MuDim::default())
}
fn read_mustretch_float<ET:EngineTypes>(engine:&mut EngineReferences<ET>, is_negative:bool, first:u8) -> MuStretchShrink<ET::MuDim> {
    let mut ret = 0f32;
    let mut in_decimal = first == b'.';
    let mut fac = 10f32;
    if !in_decimal {
        ret = (first - b'0') as f32;
    }
    crate::expand_loop!(engine,token,
        ResolvedToken::Tk {char,code} => match (char.try_into(),code) {
            (Ok(b),CommandCode::Other) if is_ascii_digit(b) => {
                if in_decimal {
                    ret += (b - b'0') as f32 / fac;
                    fac *= 10.0;
                } else {
                    ret = 10.0*ret + ((b - b'0') as f32);
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
fn read_mustretch_unit<ET:EngineTypes>(engine:&mut EngineReferences<ET>,float:f32,first:Option<(u8,ET::Token)>) -> MuStretchShrink<ET::MuDim> {
    match read_keywords(engine,STRETCH_SHRINK_UNITS,first) {
        Some(d) => MuStretchShrink::from_float(engine,float,d),
        _ => match read_keywords(engine,ET::MuDim::UNITS,None) {
            Some(d) => MuStretchShrink::from_float(engine,float,d),
            _ => todo!("wut")
        }
    }
}


/// Default implementation for [`Gullet::read_keyword`].
pub fn read_keyword<ET:EngineTypes>(engine:&mut EngineReferences<ET>,kw:&[u8],first:Option<(u8,ET::Token)>) -> bool {
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
                    for t in read.into_iter().rev() {engine.requeue(t)}
                    //engine.aux.memory.return_bytes(ret);
                    //engine.aux.memory.return_token_vec(read);
                    return false
                }
                if kw.len() == ret.len() {
                    //engine.aux.memory.return_token_vec(read);
                    //engine.aux.memory.return_bytes(ret);
                    return true
                }
            }
            _ => {
                read.push(token);
                for t in read.into_iter().rev() {engine.requeue(t)}
                //engine.aux.memory.return_bytes(ret);
                //engine.aux.memory.return_token_vec(read);
                return false
            }
        }
        ResolvedToken::Cmd(_) => {
            read.push(token);
            for t in read.into_iter().rev() {engine.requeue(t)}
            //engine.aux.memory.return_bytes(ret);
            //engine.aux.memory.return_token_vec(read);
            return false
        }
    );
    todo!("read_keyword")
}

/// Default implementation for [`Gullet::read_keywords`].
pub fn read_keywords<'a,ET:EngineTypes>(engine:&mut EngineReferences<ET>,kws:&[&'a[u8]],first:Option<(u8,ET::Token)>) -> Option<&'a[u8]> {
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
                if curr.len() == 0 {
                    ret.pop();
                    match kws.iter().find(|e| **e == ret.as_slice()) {
                        Some(w) => {
                            engine.requeue(token);
                            //engine.aux.memory.return_token_vec(read);
                            //engine.aux.memory.return_bytes(ret);
                            return Some(w)
                        }
                        None => {
                            engine.requeue(token);
                            for t in read.into_iter().rev() {engine.mouth.requeue(t)}
                            //engine.aux.memory.return_bytes(ret);
                            //engine.aux.memory.return_token_vec(read);
                            return None
                        }
                    }
                }
                read.push(token);
                if curr.len() == 1 && curr[0].len() == ret.len() {
                    //engine.aux.memory.return_token_vec(read);
                    //engine.aux.memory.return_bytes(ret);
                    return Some(curr[0])
                }
            }
            _ => {
                let curr = kws.iter().filter(|k| k.starts_with(&ret)).collect::<Vec<_>>();
                match curr.iter().enumerate().find(|(_,b)| ***b == ret.as_slice()) {
                    Some((i,_)) => {
                        engine.requeue(token);
                        //engine.aux.memory.return_token_vec(read);
                        //engine.aux.memory.return_bytes(ret);
                        return Some(curr[i])
                    }
                    _ => {
                        engine.requeue(token);
                        for t in read.into_iter().rev() {engine.requeue(t)}
                        //engine.aux.memory.return_bytes(ret);
                        //engine.aux.memory.return_token_vec(read);
                        return None
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
                    return Some(curr[i])
                }
                _ => {
                    engine.mouth.requeue(token);
                    for t in read.into_iter().rev() {engine.requeue(t)}
                    //engine.aux.memory.return_bytes(ret);
                    //engine.aux.memory.return_token_vec(read);
                    return None
                }
            }
        }
    );
    todo!("read_keyword")
}

/// Default implementation for [`Gullet::read_chars`].
pub fn read_chars<ET:EngineTypes>(engine:&mut EngineReferences<ET>,kws:&[u8]) -> Result<u8,ET::Token> {
    crate::expand_loop!(engine,token,
        ResolvedToken::Tk {char,code} => match (char.try_into(),code) {
            (_,CommandCode::Space) => (),
            (Ok(b),_) if kws.contains(&b) => {
                return Ok(b)
            }
            _ => {
                return Err(token)
            }
        }
        ResolvedToken::Cmd(_) => {
            return Err(token)
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
    pub fn read_control_sequence(&mut self) -> CSOrActiveChar<ET::Token> {
        while let Some(token) = self.get_next() {
            match self.resolve(&token) {
                ResolvedToken::Cmd(Some(TeXCommand::Primitive {name,cmd:PrimitiveCommand::Expandable(f)})) =>
                    ET::Gullet::do_expandable(self,*name,token,*f),
                ResolvedToken::Cmd(Some(TeXCommand::Primitive{name,cmd:PrimitiveCommand::SimpleExpandable(f)})) =>
                    ET::Gullet::do_simple_expandable(self,*name,token,*f),
                ResolvedToken::Cmd(Some(TeXCommand::Primitive{name,cmd:PrimitiveCommand::Conditional(f)})) =>
                    ET::Gullet::do_conditional(self,*name,token,*f,false),
                ResolvedToken::Cmd(_) => {
                    let ret = match token.to_enum() {
                        StandardToken::Character(c, CommandCode::Active) => CSOrActiveChar::Active(c),
                        StandardToken::ControlSequence(cs) => CSOrActiveChar::Name(cs),
                        _ => unreachable!()
                    };
                    self.set_command(&ret, Some(TeXCommand::Primitive {name:PRIMITIVES.relax,cmd:PrimitiveCommand::Relax}), false);
                    return ret
                }
                ResolvedToken::Tk {code:CommandCode::Space,..} => (),
                o => todo!("read_control_sequence: {:?}",o)
            }
        }
        todo!("file ended unexpectedly")
    }
}