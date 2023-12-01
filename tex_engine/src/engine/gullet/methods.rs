use crate::commands::{Command, IntCommand, SimpleExpandable};
use crate::engine::{EngineReferences, EngineTypes};
use crate::engine::mouth::Mouth;
use crate::tex::catcodes::CommandCode;
use crate::tex::numerics::{MuSkip, Numeric, NumSet, Skip};
use crate::engine::state::State;
use crate::engine::utils::memory::{MemoryManager, PRIMITIVES};
use crate::file_end;
use std::str::FromStr;
use crate::engine::gullet::{ActiveConditional, Gullet, ResolvedToken};
use crate::engine::mouth::pretokenized::TokenList;
use crate::tex::control_sequences::{ControlSequenceNameHandler, ResolvedCSName};
use crate::tex::numerics::TeXDimen;
use crate::tex::token::{StandardToken, Token};
use crate::utils::errors::ErrorHandler;
use crate::tex::input_text::Character;
use crate::engine::EngineAux;
use crate::tex::numerics::TeXInt;

pub fn read_arguments<ET:EngineTypes>(engine:&mut EngineReferences<ET>,args:&mut [Vec<ET::Token>;9],params:TokenList<ET::Token>,long:bool) {
    let mut i = 1usize;
    let inner = params.inner();
    let mut next = &inner[0];
    loop {
        match next.is_argument_marker() {
            Some(a) => match inner.get(i) {
                Some(n) if n.is_argument_marker().is_some() =>
                    {next = n; i += 1;read_argument(engine,&mut args[a as usize],long)},
                None => {
                    read_argument(engine,&mut args[a as usize],long);
                    return
                },
                Some(o) => {
                    let mut delim = engine.aux.memory.get_token_vec();
                    delim.push(o.clone());
                    i += 1;
                    while let Some(n) = inner.get(i) {
                        if n.is_argument_marker().is_some() {break}
                        delim.push(n.clone());
                        i += 1;
                    }
                    read_delimited_argument(engine,&mut args[a as usize],&delim,long);
                    engine.aux.memory.return_token_vec(delim);
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
                _ => todo!("error")
            }
        }
    }
}

fn read_delimited_argument<ET:EngineTypes>(engine:&mut EngineReferences<ET>,arg:&mut Vec<ET::Token>,delim:&Vec<ET::Token>,long:bool) {
    let par = engine.aux.memory.cs_interner().par();
    let last = delim.last().unwrap();
    let ends_with_bgroup = last.is_begin_group();
    let mut remove_braces:Option<Option<usize>> = None;
    while let Some(t) = engine.get_next() {
        if t.is_noexpand_marker() { continue }
        if t.is_begin_group() && !ends_with_bgroup {
            if arg.is_empty() { remove_braces = Some(None) }
            arg.push(t);
            let r = if long { engine.read_until_endgroup(
                |_,t| arg.push(t)
            ) } else { engine.read_until_endgroup(
                |_,t| if t.is_cs(&par) {todo!("\\par in read_argument")} else {arg.push(t)}
            ) };
            arg.push(r);
            match remove_braces {
                Some(ref mut n@None) => *n = Some(arg.len()),
                _ => ()
            }
            continue
        }
        if !long {
            if t.is_cs(&par) {todo!("\\par in read_argument")}
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
                return
            }
        } else {
            arg.push(t);
        }
    }
    todo!("file end")
}

fn read_argument<ET:EngineTypes>(engine:&mut EngineReferences<ET>,arg:&mut Vec<ET::Token>,long:bool) {
    while let Some(t) = engine.mouth.get_next_opt(engine.aux,engine.state) {
        if t.is_noexpand_marker() { continue }
        if t.is_space() { continue }
        if t.is_begin_group() {
            if long {
                engine.mouth.read_until_endgroup(
                    engine.aux,
                    engine.state.get_catcode_scheme(),
                    engine.state.get_endline_char(),
                    |_, t| arg.push(t)
                );
            } else {
                let par = engine.aux.memory.cs_interner().par();
                engine.mouth.read_until_endgroup(
                    engine.aux,
                    engine.state.get_catcode_scheme(),
                    engine.state.get_endline_char(),
                    |_,t|
                        if t.is_cs(&par) {todo!("\\par in read_argument")} else {arg.push(t)}
                );
            }
            return
        }
        arg.push(t);
        return
    }
}

pub fn expand_until_endgroup<ET:EngineTypes,Fn:FnMut(&mut EngineAux<ET>,&ET::State,&mut ET::Gullet,ET::Token)>(engine:&mut EngineReferences<ET>,expand_protected:bool,edef_like:bool,mut cont:Fn) {
    match engine.gullet.get_align_data() {
        None => (),
        Some(d) => {
            if d.ingroups == 0 { todo!() }
            d.ingroups -= 1
        }
    }
    let mut ingroups = 0;
    'A: while let Some(t) = engine.mouth.get_next_opt(engine.aux,engine.state) {
        if t.is_end_group()  {
            if ingroups == 0 { return }
            ingroups -= 1;
            cont(engine.aux,engine.state,engine.gullet,t);
            continue
        }
        if t.is_begin_group() {
            ingroups += 1;
            cont(engine.aux,engine.state,engine.gullet,t);
            continue
        }
        if t.is_noexpand_marker() {
            let next = engine.mouth.get_next_opt(engine.aux,engine.state).unwrap();
            cont(engine.aux,engine.state,engine.gullet,next);
            continue
        }
        if !t.is_cs_or_active() {
            cont(engine.aux,engine.state,engine.gullet,t);
            continue
        }
        match engine.resolve(t) {
            ResolvedToken::Tk{token,char,code} => {
                cont(engine.aux,engine.state,engine.gullet, token)
            }
            ResolvedToken::Cmd{cmd: Some(Command::Macro(m)),token} if m.protected && !expand_protected =>
                cont(engine.aux,engine.state,engine.gullet,token),
            ResolvedToken::Cmd{cmd: Some(Command::Macro(m)),token} =>
                ET::Gullet::do_macro(engine,m.clone(),token),
            ResolvedToken::Cmd{cmd: Some(Command::Conditional(cond)),token} =>
                ET::Gullet::do_conditional(engine,cond.name,token,cond.expand,false),
            ResolvedToken::Cmd{cmd: Some(Command::Expandable(e)),token}
                if e.name == PRIMITIVES.unexpanded => {
                engine.expand_until_bgroup(false);
                engine.mouth.read_until_endgroup(engine.aux,engine.state.get_catcode_scheme(),engine.state.get_endline_char(),|a,t|{
                    if edef_like && t.is_param() {
                        cont(a,engine.state,engine.gullet,t.clone());
                    }
                    cont(a,engine.state,engine.gullet,t)
                });
            }
            ResolvedToken::Cmd{cmd: Some(Command::Expandable(e)),token}
                if e.name == PRIMITIVES.the => {
                crate::commands::tex::do_the(engine,|a,s,g,t| {
                    if edef_like && t.is_param() {
                        cont(a,s,g,t.clone());
                    }
                    cont(a,s,g,t)
                })
            }
            ResolvedToken::Cmd{cmd: Some(Command::SimpleExpandable(e)),token}
                if e.name == PRIMITIVES.noexpand => {
                match engine.mouth.get_next_opt(engine.aux,engine.state) {
                    Some(t) if !t.is_end_group() =>
                        cont(engine.aux,engine.state,engine.gullet,t),
                    _ => todo!("throw error")
                }
            }
            ResolvedToken::Cmd{cmd: Some(Command::SimpleExpandable(e)),token} =>
                ET::Gullet::do_simple_expandable(engine, e.name, token, e.expand),
            ResolvedToken::Cmd{cmd: Some(Command::Expandable(e)),token} =>
                ET::Gullet::do_expandable(engine,e.name,token,e.expand),
            ResolvedToken::Cmd{token,..} => {
                cont(engine.aux,engine.state,engine.gullet,token)
            }
        }
    }
    todo!("throw error")
}

pub fn case_loop<ET:EngineTypes>(engine:&mut EngineReferences<ET>,idx:usize) {
    let (mut incond,cond) = {
        let conds = engine.gullet.get_conditionals();
        let ic = conds.len() - idx;
        for i in 0..ic { conds.pop();}
        (ic,match conds.pop() {
            Some(ActiveConditional::Case(c)) => c,
            _ => unreachable!()
        })
    };
    let mut curr_int = <ET::Num as NumSet>::Int::default();
    let one = <ET::Num as NumSet>::Int::from(1);
    let state = &*engine.state;
    let endline = state.get_endline_char();
    let cc = state.get_catcode_scheme();
    let gullet = &mut *engine.gullet;
    engine.mouth.iterate(engine.aux,cc,endline,move |_,t| {
        if !t.is_cs_or_active() { return true }
        match gullet.resolve(state,t) {
            ResolvedToken::Cmd {cmd:Some(Command::Conditional(c)),..} =>
                {incond += 1;true},
            ResolvedToken::Cmd {cmd:Some(Command::SimpleExpandable(SimpleExpandable{name,..})),..}
            if *name == PRIMITIVES.else_ && incond == 0 => {
                gullet.get_conditionals().push(
                    ActiveConditional::Else(PRIMITIVES.ifcase)
                );
                false
            }
            ResolvedToken::Cmd {cmd:Some(Command::SimpleExpandable(SimpleExpandable{name,..})),..}
            if *name == PRIMITIVES.fi => {
                if incond == 0 {false}
                else {incond -= 1; true}
            }
            ResolvedToken::Cmd {cmd:Some(Command::SimpleExpandable(SimpleExpandable{name,..})),..}
            if *name == PRIMITIVES.or && incond == 0 => {
                curr_int = curr_int + one;
                if cond == curr_int {
                    gullet.get_conditionals().push(
                        ActiveConditional::Case(cond)
                    );
                    false
                }
                else {true}
            }
            _ => true
        }
    })
}

pub fn false_loop<ET:EngineTypes>(engine:&mut EngineReferences<ET>,idx:usize,allowelse:bool,mut skipelse:bool) {
    let (mut incond,cond) = {
        let conds = engine.gullet.get_conditionals();
        let ic = conds.len() - idx;
        for i in 0..ic { conds.pop();}
        (ic,conds.pop().unwrap().name())
    };
    let state = &*engine.state;
    let endline = state.get_endline_char();
    let cc = state.get_catcode_scheme();
    let gullet = &mut *engine.gullet;
    engine.mouth.iterate(engine.aux,cc,endline,move |_,t| {
        if !t.is_cs_or_active() { return true }
        match gullet.resolve(state,t) {
            ResolvedToken::Cmd {cmd:Some(Command::Conditional(c)),..} =>
                {incond += 1;true},
            ResolvedToken::Cmd {cmd:Some(Command::SimpleExpandable(SimpleExpandable{name,..})),..}
                if *name == PRIMITIVES.else_ && incond == 0 => {
                if allowelse {
                    gullet.get_conditionals().push(
                        ActiveConditional::Else(cond)
                    );
                    false
                }
                else if skipelse {
                    skipelse = false;true
                }
                else {todo!("throw spurious else in false-loop error")}
            }
            ResolvedToken::Cmd {cmd:Some(Command::SimpleExpandable(SimpleExpandable{name,..})),..}
                if *name == PRIMITIVES.fi => {
                if incond == 0 {false}
                else {incond -= 1; true}
            }
            _ => true
        }
    })
}

pub enum ACOrCS<T:Token>{
    Active(T::Char),
    Name(T::CS)
}
impl<ET:EngineTypes> EngineReferences<'_,ET> {
    pub fn read_control_sequence(&mut self) -> ACOrCS<ET::Token> {
        while let Some(tk) = self.get_next() {
            match self.resolve(tk) {
                ResolvedToken::Cmd {cmd:Some(Command::Expandable(f)),token} =>
                    ET::Gullet::do_expandable(self,f.name,token,f.expand),
                ResolvedToken::Cmd {cmd:Some(Command::SimpleExpandable(f)),token} =>
                    ET::Gullet::do_simple_expandable(self,f.name,token,f.expand),
                ResolvedToken::Cmd {cmd:Some(Command::Conditional(f)),token} =>
                    ET::Gullet::do_conditional(self,f.name,token,f.expand,false),
                ResolvedToken::Cmd {token,..} => {
                    let ret = match token.to_enum() {
                        StandardToken::Character(c, CommandCode::Active) => ACOrCS::Active(c),
                        StandardToken::ControlSequence(cs) => ACOrCS::Name(cs),
                        _ => unreachable!()
                    };
                    self.set_command(&ret,Some(Command::Relax),false);
                    return ret
                }
                ResolvedToken::Tk {code:CommandCode::Space,..} => (),
                o => todo!("read_control_sequence: {:?}",o)
            }
        }
        todo!("file ended unexpectedly")
    }
}

pub fn read_string<ET:EngineTypes>(engine:&mut EngineReferences<ET>,skip_eq:bool, ret:&mut String) {
    let mut quoted = false;
    let mut had_eq = !skip_eq;
    crate::expand_loop!(engine,
        ResolvedToken::Tk {char,code,..} => match code {
            CommandCode::Space if ret.is_empty() => (),
            CommandCode::Space if quoted => ret.push(' '),
            CommandCode::Space => return,
            CommandCode::Other if !had_eq && ret.is_empty() && matches!(char.try_into(),Ok(b'=')) => {
                had_eq = true;
            }
            _ => {
                if matches!(char.try_into(),Ok(b'\"')) {
                    quoted = !quoted;
                } else {
                    char.display(ret);
                }
            }
        }
        ResolvedToken::Cmd {cmd:Some(Command::Relax),..} if !quoted => return,
        ResolvedToken::Cmd {token,..} if !quoted => {
            engine.mouth.requeue(token);
            return
        }
        o => todo!("unexpandable in read_string: {:?}",o)
    );
    crate::file_end!()
}

#[inline(always)]
fn is_ascii_digit(u:u8) -> bool {
    u >= 48 && u <= 57
}
#[inline(always)]
fn is_ascii_oct_digit(u:u8) -> bool {
    u >= 48 && u <= 55
}
#[inline(always)]
fn is_ascii_hex_digit(u:u8) -> bool {
    is_ascii_digit(u) || (u >= 65 && u <= 70) || (u >= 97 && u <= 102)
}

pub fn read_numeric<ET:EngineTypes>(engine:&mut EngineReferences<ET>, skip_eq:bool)
                                    -> (bool,Result<u8,(Command<ET>,ET::Token)>) {
    let mut is_negative = false;
    let mut had_eq = !skip_eq;
    crate::expand_loop!(engine,
        ResolvedToken::Tk {char,code,token} => match (char.try_into(),code) {
            (_,CommandCode::Space) => (),
            (Ok(b'='),CommandCode::Other) => {
                if had_eq { todo!("throw error") }
                had_eq = true;
            }
            (Ok(b'-'),CommandCode::Other) => {
                is_negative =!is_negative;
            }
            (Ok(b'+'),CommandCode::Other) => (),
            (Ok(b),CommandCode::Other) => return (is_negative,Ok(b)),
            _ => todo!("number expected; got: {} {:?} -- l. {}",(char.try_into().ok().unwrap() as char),code,engine.mouth.line_number())
        }
        ResolvedToken::Cmd {cmd:Some(Command::Char {char,code}),token} => match ((*char).try_into(),code) {
            (_,CommandCode::Space) => (),
            (Ok(b'='),CommandCode::Other) => {
                if had_eq { todo!("throw error") }
                had_eq = true;
            }
            (Ok(b'-'),CommandCode::Other) => {
                is_negative =!is_negative;
            }
            (Ok(b'+'),CommandCode::Other) => (),
            (Ok(b),CommandCode::Other) => return (is_negative,Ok(b)),
            _ => todo!("number expected")
        }
        ResolvedToken::Cmd {cmd:None,token} => engine.aux.error_handler.undefined(engine.aux.memory.cs_interner(),token),
        ResolvedToken::Cmd {cmd:Some(cmd),token} => return (is_negative,Err((cmd.clone(),token)))
    );
    todo!("file end")
}

pub fn read_int<ET:EngineTypes>(engine:&mut EngineReferences<ET>, skip_eq:bool) -> <ET::Num as NumSet>::Int {
    let (is_negative,r) = read_numeric(engine, skip_eq);
    match r {
        Ok(b) => read_int_byte(engine,is_negative,b),
        Err((cmd,token)) => read_int_command(engine,is_negative,cmd,token)
    }
}

pub fn read_int_byte<ET:EngineTypes>(engine:&mut EngineReferences<ET>, is_negative:bool,b:u8) -> <ET::Num as NumSet>::Int {
    match b {
        b'\"' => read_hex_int(engine, is_negative),
        b'\'' => read_oct_int(engine, is_negative),
        b'`' => read_int_char(engine,is_negative),
        b if is_ascii_digit(b) => read_dec_int(engine, is_negative, b),
        _ => todo!("number expected")
    }
}

pub fn read_int_char<ET:EngineTypes>(engine:&mut EngineReferences<ET>, is_negative:bool) -> <ET::Num as NumSet>::Int {
    let next = engine.mouth.get_next_opt(engine.aux, engine.state);
    crate::expand_loop!(engine,
        ResolvedToken::Tk{char,code,token} => {
            if code != CommandCode::Space {
                engine.requeue(token);
            }
            break
        }
        ResolvedToken::Cmd {cmd:Some(Command::Char {code:CommandCode::Space,..}),..} => {
            break
        }
        ResolvedToken::Cmd {token,..} => {
            engine.mouth.requeue(token);
            break
        }
    );
    match next {
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
        },
        None => todo!("throw error here")
    }
}

pub fn read_int_command<ET:EngineTypes>(engine:&mut EngineReferences<ET>, is_negative:bool,cmd:Command<ET>,token:ET::Token) -> <ET::Num as NumSet>::Int {
    match cmd {
        Command::Int(IntCommand{read,..}) => {
            if is_negative {-read(engine,token)}
            else {read(engine,token)}
        },
        Command::PrimitiveInt(id) => {
            if is_negative {-engine.state.get_primitive_int(id)}
            else {engine.state.get_primitive_int(id)}
        }
        Command::IntRegister(u) => {
            let i = engine.state.get_int_register(u);
            if is_negative {-i}
            else {i}
        }
        Command::CharDef(c) => {
            let val:u64 = c.into();
            match <ET::Num as NumSet>::Int::try_from(val as i64) {
                Ok(val) => {
                    if is_negative {-val}
                    else {val}
                }
                _ => todo!("throw error")
            }
        }
        Command::MathChar(u) => {
            match <ET::Num as NumSet>::Int::try_from(u as i64) {
                Ok(val) => {
                    if is_negative {-val}
                    else {val}
                }
                _ => todo!("throw error")
            }
        }
        Command::DimRegister(u) => {
            let base = ET::Num::dim_to_int(engine.state.get_dim_register(u));
            if is_negative {-base} else {base}
        }
        Command::Dim(dc) => {
            let base = ET::Num::dim_to_int((dc.read)(engine,token));
            if is_negative {-base} else {base}
        }
        o => todo!("read_int: {:?}",o)
    }
}

fn ret_hex<ET:EngineTypes>(engine:&mut EngineReferences<ET>,v:Vec<u8>,is_negative:bool) -> <ET::Num as NumSet>::Int {
    let i = match <ET::Num as NumSet>::Int::from_str_radix(std::str::from_utf8(&v).unwrap(),16) {
        Ok(i) => i,
        Err(_) => todo!("throw error")
    };
    engine.aux.memory.return_bytes(v);
    return if is_negative { -i } else { i }
}

fn ret_float<ET:EngineTypes>(engine:&mut EngineReferences<ET>,v:Vec<u8>,is_negative:bool) -> f64 {
    let f = f64::from_str(std::str::from_utf8(&v).unwrap()).unwrap();
    engine.aux.memory.return_bytes(v);
    return if is_negative { -f } else { f }
}

pub fn read_dec_int<ET:EngineTypes>(engine:&mut EngineReferences<ET>, is_negative:bool,first:u8) -> <ET::Num as NumSet>::Int {
    let mut ret = (first - b'0') as i32;
    crate::expand_loop!(engine,
        ResolvedToken::Tk{token,char,code} => match (char.try_into(),code) {
            (Ok(b),CommandCode::Other) if is_ascii_digit(b) => {
                ret = 10*ret + ((b - b'0') as i32);
            }
            (_,CommandCode::Space) => return if is_negative {- ET::Int::from(ret)} else {ET::Int::from(ret)},
            _  => {
                engine.requeue(token);
                return if is_negative {- ET::Int::from(ret)} else {ET::Int::from(ret)}
            }
        }
        ResolvedToken::Cmd {cmd:Some(Command::Char {code:CommandCode::Space,..}),..} => {
            return if is_negative {- ET::Int::from(ret)} else {ET::Int::from(ret)}
        }
        ResolvedToken::Cmd {token,..} => {
            engine.mouth.requeue(token);
            return if is_negative {- ET::Int::from(ret)} else {ET::Int::from(ret)}
        }
        o => todo!("{:?}; current: {:?}",o,ret)
    );
    file_end!()
}

pub fn read_hex_int<ET:EngineTypes>(engine:&mut EngineReferences<ET>, is_negative:bool) -> <ET::Num as NumSet>::Int {
    let mut ret = engine.aux.memory.get_bytes();
    crate::expand_loop!(engine,
        ResolvedToken::Tk{token,char,code} => match (char.try_into(),code) {
            (Ok(b),CommandCode::Other|CommandCode::Letter) if is_ascii_hex_digit(b) => {
                ret.push(b);
            }
            (_,CommandCode::Space) => return ret_hex(engine,ret,is_negative),
            _ if !ret.is_empty() => {
                engine.requeue(token);
                return ret_hex(engine,ret,is_negative)
            }
            _ => {
                todo!("{}:{:?}",char,code);
            }
        }
        ResolvedToken::Cmd {cmd:Some(Command::Char {code:CommandCode::Space,..}),..} => {
            return ret_hex(engine,ret,is_negative)
        }
        ResolvedToken::Cmd {token,..} if !ret.is_empty() => {
            engine.mouth.requeue(token);
            return ret_hex(engine,ret,is_negative)
        }
        o => todo!("{:?}; current: {:?}",o,ret)
    );
    file_end!()
}

pub fn read_oct_int<ET:EngineTypes>(engine:&mut EngineReferences<ET>, is_negative:bool) -> <ET::Num as NumSet>::Int {
    todo!("read_oct_int")
}

pub fn read_dim<ET:EngineTypes>(engine:&mut EngineReferences<ET>, skip_eq:bool) -> <ET::Num as NumSet>::Dim {
    let (is_negative,r) = read_numeric(engine, skip_eq);
    match r {
        Ok(b) => read_dim_byte(engine,is_negative,b),
        Err((cmd,token)) => read_dim_command(engine,is_negative,cmd,token)
    }
}

pub fn read_dim_byte<ET:EngineTypes>(engine:&mut EngineReferences<ET>, is_negative:bool,b:u8) -> <ET::Num as NumSet>::Dim {
    if b == b',' || b == b'.' {
        read_dim_float(engine,is_negative,b'.')
    } else if is_ascii_digit(b) {
        read_dim_float(engine,is_negative,b)
    } else {
        todo!("error?")
    }
}

pub fn read_dim_command<ET:EngineTypes>(engine:&mut EngineReferences<ET>, is_negative:bool,cmd:Command<ET>,token:ET::Token) -> <ET::Num as NumSet>::Dim {
    match cmd {
        Command::DimRegister(u) => {
            if is_negative {return -engine.state.get_dim_register(u)}
            else {return engine.state.get_dim_register(u)}
        },
        Command::IntRegister(u) => {
            let i = engine.state.get_int_register(u);
            let i = if is_negative {-i} else {i};
            let i = i.into() as f64;
            read_unit_or_dim(engine,i)
        }
        Command::CharDef(c) => {
            let val = if is_negative {-(c.into() as f64)} else {c.into() as f64};
            read_unit_or_dim(engine,val)
        }
        Command::Int(ic) => {
            let i = if is_negative {-(ic.read)(engine,token)} else {(ic.read)(engine,token)};
            let f = i.into() as f64;
            read_unit_or_dim(engine,f)
        }
        Command::Dim(dc) => {
            let val = (dc.read)(engine,token);
            if is_negative {return -val} else {return val}
        }
        Command::PrimitiveDim(dc) => {
            let val = engine.state.get_primitive_dim(dc);
            if is_negative {return -val} else {return val}
        }
        Command::PrimitiveSkip(dc) => {
            let val = engine.state.get_primitive_skip(dc).base();
            if is_negative {return -val} else {return val}
        }
        o => todo!("command in read_dim: {:?}",o)
    }
}

fn read_dim_float<ET:EngineTypes>(engine:&mut EngineReferences<ET>, is_negative:bool, first:u8) -> <ET::Num as NumSet>::Dim {
    let mut ret = engine.aux.memory.get_bytes();
    let mut in_decimal = first == b'.' && {ret.push(b'0');true};
    ret.push(first);
    crate::expand_loop!(engine,
        ResolvedToken::Tk {char,code,token} => match (char.try_into(),code) {
            (Ok(b),CommandCode::Other) if is_ascii_digit(b) => {
                ret.push(b);
            }
            (Ok(b','|b'.'),CommandCode::Other) => {
                if in_decimal { todo!("throw error") }
                in_decimal = true;
                ret.push(b'.');
            }
            (_,CommandCode::Space) => {
                let f = ret_float(engine,ret,is_negative);
                return read_dim_unit(engine,f,None)
            }
            (Ok(b),CommandCode::Other | CommandCode::Letter) => {
                let f = ret_float(engine,ret,is_negative);
                return read_dim_unit(engine,f,Some((b,token)))
            }
            _ => {
                todo!("{}:{:?}",char,code);
            }
        }
        ResolvedToken::Cmd {cmd:Some(c),token} => {
            let base = read_dim_command(engine,false,c.clone(),token);
            let f = ret_float(engine,ret,is_negative);
            return base.scale_float(f)
        }
        _ => todo!("command in read_dim_inner")
    );
    todo!("read_dim_inner")
}

pub fn read_unit_or_dim<ET:EngineTypes>(engine:&mut EngineReferences<ET>,float:f64) -> <ET::Num as NumSet>::Dim {
    crate::expand_loop!(engine,
        ResolvedToken::Tk {char,code,token} => match (char.try_into(),code) {
            (Ok(b),CommandCode::Other | CommandCode::Letter) => {
                return read_dim_unit(engine,float,Some((b,token)))
            }
            (_,CommandCode::Space) => (),
            _ => {
                todo!("{}:{:?}",char,code);
            }
        }
        ResolvedToken::Cmd {cmd:Some(Command::Char {code:CommandCode::Space,..}),..} => (),
        ResolvedToken::Cmd {cmd:Some(Command::Char{char,code:(CommandCode::Other | CommandCode::Letter)}),token} => match (*char).try_into() {
            Ok(b) => return read_dim_unit(engine,float,Some((b,token))),
            _ => todo!("throw error")
        }
        ResolvedToken::Cmd {cmd:Some(cmd),token} => match cmd {
            Command::DimRegister(u) => {
                let base = engine.state.get_dim_register(*u);
                let scale = (float * 65536.0).round() as i32;
                return base.scale(scale.into(),65536.into())
            }
            o => todo!("command in read_unit_or_dim: {:?}",o)
        }
        ResolvedToken::Cmd {cmd:None,token} =>
            engine.aux.error_handler.undefined(engine.aux.memory.cs_interner(),token)
    );
    todo!("file end")
}

pub fn read_dim_unit<ET:EngineTypes>(engine:&mut EngineReferences<ET>, mut float:f64, mut first:Option<(u8, ET::Token)>) -> <ET::Num as NumSet>::Dim {
    let is_true = match &first {
        Some((b't'|b'T',t)) => {
            read_keyword(engine,b"true",std::mem::take(&mut first))
        },
        Some(_) => false,
        None => read_keyword(engine,b"true",None)
    };
    if is_true {
        let mag = engine.state.get_primitive_int(PRIMITIVES.mag);
        float *= Into::<i64>::into(mag) as f64 / 1000.0;
    }
    let units = <ET::Num as NumSet>::Dim::units();
    match read_keywords(engine,units,first) {
        Some(d) => <ET::Num as NumSet>::Dim::from_float(engine,float,d),
        _ => todo!("wut: {}",engine.preview())
    }
}


pub fn read_skip<ET:EngineTypes>(engine:&mut EngineReferences<ET>, skip_eq:bool) -> <ET::Num as NumSet>::Skip {
    let (is_negative,r) = read_numeric(engine, skip_eq);
    match r {
        Ok(b) => read_skip_byte(engine,is_negative,b),
        Err((cmd,token)) => read_skip_command(engine,is_negative,cmd,token)
    }
}

pub fn read_skip_byte<ET:EngineTypes>(engine:&mut EngineReferences<ET>, is_negative:bool,b:u8) -> <ET::Num as NumSet>::Skip {
    if b == b',' || b == b'.' {
        read_skip_dim(engine,is_negative,b'.')
    } else if is_ascii_digit(b) {
        read_skip_dim(engine,is_negative,b)
    } else {
        todo!("error?")
    }
}

pub fn read_skip_command<ET:EngineTypes>(engine:&mut EngineReferences<ET>, is_negative:bool,cmd:Command<ET>,token:ET::Token) -> <ET::Num as NumSet>::Skip {
    match cmd {
        Command::DimRegister(u) => {
            let base = engine.state.get_dim_register(u);
            let base = if is_negative {-base} else {base};
            read_skip_ii(engine,base)
        }
        Command::SkipRegister(u) => {
            let base = engine.state.get_skip_register(u);
            if is_negative {-base} else {base}
        }
        Command::PrimitiveSkip(dc) => {
            let val = engine.state.get_primitive_skip(dc);
            if is_negative {return -val} else {return val}
        }
        _ => todo!("read skip command: {:?}",cmd)
    }
}
/*
pub fn read_skip_old<ET:EngineTypes>(engine:&mut EngineReferences<ET>, skip_eq:bool) -> <ET::Num as NumSet>::Skip {
    let mut is_negative = false;
    let mut had_eq = !skip_eq;
    crate::expand_loop!(engine,
        ResolvedToken::Tk{char,code,..} => match (char.try_into(),code) {
            (_,CommandCode::Space) => (),
            (Ok(b'='),CommandCode::Other) => {
                if had_eq { todo!("throw error") }
                had_eq = true;
            }
            (Ok(b'-'),CommandCode::Other) => {
                is_negative =!is_negative;
            }
            (Ok(b'+'),CommandCode::Other) => (),
            (Ok(b),CommandCode::Other) if is_ascii_digit(b) => return read_skip_dim(engine,is_negative,b),
            (Ok(b','|b'.'),CommandCode::Other) => return read_skip_dim(engine,is_negative,b'.'),
            o => todo!("error?")
        }
        o => todo!("command in read_skip")
    );
    crate::file_end!()
}
*/

type Sk<ET> = <<ET as EngineTypes>::Num as NumSet>::Skip;
type Str<ET> = <<<ET as EngineTypes>::Num as NumSet>::Skip as Skip>::Stretch;
type Shr<ET> = <<<ET as EngineTypes>::Num as NumSet>::Skip as Skip>::Shrink;

const PLUS: &[u8] = b"plus";
const MINUS: &[u8] = b"minus";

#[inline(always)]
fn read_skip_dim<ET:EngineTypes>(engine:&mut EngineReferences<ET>, is_negative:bool,first:u8) -> Sk<ET> {
    let base = read_dim_float(engine,is_negative,first);
    read_skip_ii(engine,base)
}

fn read_skip_ii<ET:EngineTypes>(engine:&mut EngineReferences<ET>, base:<ET::Num as NumSet>::Dim) -> Sk<ET> {
    match read_keywords(engine,&[PLUS,MINUS],None) {
        Some(b) if b == PLUS => {
            let stretch = read_stretch(engine);
            if read_keyword(engine,MINUS,None) {
                Sk::<ET>::new(base,Some(stretch),Some(read_shrink(engine)))
            } else {
                Sk::<ET>::new(base, Some(stretch), None)
            }
        }
        Some(b) if b == MINUS => {
            let shrink = read_shrink(engine);
            if read_keyword(engine,PLUS,None) {
                Sk::<ET>::new(base,Some(read_stretch(engine)),Some(shrink))
            } else {
                Sk::<ET>::new(base,None,Some(shrink))
            }
        }
        _ => <ET::Num as NumSet>::Skip::new(base,None,None)
    }
}

fn read_stretch<ET:EngineTypes>(engine:&mut EngineReferences<ET>) -> Str<ET> {
    let mut is_negative = false;
    crate::expand_loop!(engine,
        ResolvedToken::Tk {char,code,..} => match (char.try_into(),code) {
            (_,CommandCode::Space) => (),
            (Ok(b'-'),CommandCode::Other) => {
                is_negative =!is_negative;
            }
            (Ok(b'+'),CommandCode::Other) => (),
            (Ok(b),CommandCode::Other) if is_ascii_digit(b) => return read_stretch_float(engine,is_negative,b),
            (Ok(b','|b'.'),CommandCode::Other) => return read_stretch_float(engine,is_negative,b'.'),
            o => todo!("error?")
        }
        o => todo!("command in read_skip")
    );
    crate::file_end!()
}
fn read_stretch_float<ET:EngineTypes>(engine:&mut EngineReferences<ET>, is_negative:bool, first:u8) -> Str<ET> {
    let mut ret = engine.aux.memory.get_bytes();
    let mut in_decimal = first == b'.' && {ret.push(b'0');true};
    ret.push(first);
    crate::expand_loop!(engine,
        ResolvedToken::Tk {token,char,code} => match (char.try_into(),code) {
            (Ok(b),CommandCode::Other) if is_ascii_digit(b) => {
                ret.push(b);
            }
            (Ok(b','|b'.'),CommandCode::Other) => {
                if in_decimal { todo!("throw error") }
                in_decimal = true;
                ret.push(b'.');
            }
            (_,CommandCode::Space) => {
                let f = ret_float(engine,ret,is_negative);
                return read_stretch_unit(engine,f,None)
            }
            (Ok(b),CommandCode::Other | CommandCode::Letter) => {
                let f = ret_float(engine,ret,is_negative);
                return read_stretch_unit(engine,f,Some((b,token)))
            }
            _ => {
                todo!("{}:{:?}",char,code);
            }
        }
        ResolvedToken::Cmd {cmd:Some(Command::DimRegister(u)),..} => {
            let base = engine.state.get_dim_register(*u);
            let scale = ret_float(engine,ret,is_negative);
            return Sk::<ET>::stretch_from_dimen(engine,scale,base)
        }
        o => todo!("command in read_stretch_inner: {:?}",o)
    );
    todo!("read_dim_inner")
}
fn read_stretch_unit<ET:EngineTypes>(engine:&mut EngineReferences<ET>,mut float:f64,mut first:Option<(u8,ET::Token)>) -> Str<ET> {
    let is_true = match &first {
        Some((b't'|b'T',t)) => {
            read_keyword(engine,b"true",std::mem::take(&mut first))
        },
        Some(_) => false,
        None => read_keyword(engine,b"true",None)
    };
    if is_true {
        let mag = engine.state.get_primitive_int(PRIMITIVES.mag);
        float *= Into::<i64>::into(mag) as f64 / 1000.0;
    }
    let units = Sk::<ET>::stretch_units();
    match read_keywords(engine,units,first) {
        Some(d) => Sk::<ET>::stretch_from_float(engine,float,d),
        _ => todo!("wut: {}\n  (l.{})",engine.preview(),engine.mouth.line_number())
    }
}

fn read_shrink<ET:EngineTypes>(engine:&mut EngineReferences<ET>) -> Shr<ET> {
    let mut is_negative = false;
    crate::expand_loop!(engine,
        ResolvedToken::Tk{char,code,..} => match (char.try_into(),code) {
            (_,CommandCode::Space) => (),
            (Ok(b'-'),CommandCode::Other) => {
                is_negative =!is_negative;
            }
            (Ok(b'+'),CommandCode::Other) => (),
            (Ok(b),CommandCode::Other) if is_ascii_digit(b) => return read_shrink_float(engine,is_negative,b),
            (Ok(b','|b'.'),CommandCode::Other) => return read_shrink_float(engine,is_negative,b'.'),
            o => todo!("error?")
        }
        o => todo!("command in read_skip")
    );
    crate::file_end!()
}
fn read_shrink_float<ET:EngineTypes>(engine:&mut EngineReferences<ET>, is_negative:bool, first:u8) -> Shr<ET> {
    let mut ret = engine.aux.memory.get_bytes();
    let mut in_decimal = first == b'.' && {ret.push(b'0');true};
    ret.push(first);
    crate::expand_loop!(engine,
        ResolvedToken::Tk {token,char,code} => match (char.try_into(),code) {
            (Ok(b),CommandCode::Other) if is_ascii_digit(b) => {
                ret.push(b);
            }
            (Ok(b','|b'.'),CommandCode::Other) => {
                if in_decimal { todo!("throw error") }
                in_decimal = true;
                ret.push(b'.');
            }
            (_,CommandCode::Space) => {
                let f = ret_float(engine,ret,is_negative);
                return read_shrink_unit(engine,f,None)
            }
            (Ok(b),CommandCode::Other | CommandCode::Letter) => {
                let f = ret_float(engine,ret,is_negative);
                return read_shrink_unit(engine,f,Some((b,token)))
            }
            _ => {
                todo!("{}:{:?}",char,code);
            }
        }
        ResolvedToken::Cmd {cmd:Some(Command::DimRegister(u)),..} => {
            let base = engine.state.get_dim_register(*u);
            let scale = ret_float(engine,ret,is_negative);
            return Sk::<ET>::shrink_from_dimen(engine,scale,base)
        }
        _ => todo!("command in read_dim_inner")
    );
    todo!("read_dim_inner")
}
fn read_shrink_unit<ET:EngineTypes>(engine:&mut EngineReferences<ET>,mut float:f64,mut first:Option<(u8,ET::Token)>) -> Shr<ET> {
    let is_true = match &first {
        Some((b't'|b'T',t)) => {
            read_keyword(engine,b"true",std::mem::take(&mut first))
        },
        Some(_) => false,
        None => read_keyword(engine,b"true",None)
    };
    if is_true {
        let mag = engine.state.get_primitive_int(PRIMITIVES.mag);
        float *= Into::<i64>::into(mag) as f64 / 1000.0;
    }
    let units = Sk::<ET>::shrink_units();
    match read_keywords(engine,units,first) {
        Some(d) => Sk::<ET>::shrink_from_float(engine,float,d),
        _ => todo!("wut")
    }
}

type MS<ET> = <<ET as EngineTypes>::Num as NumSet>::MuSkip;
type MB<ET> = <<<ET as EngineTypes>::Num as NumSet>::MuSkip as MuSkip>::Base;
type MSt<ET> = <<<ET as EngineTypes>::Num as NumSet>::MuSkip as MuSkip>::Stretch;
type MSh<ET> = <<<ET as EngineTypes>::Num as NumSet>::MuSkip as MuSkip>::Shrink;

pub fn read_muskip<ET:EngineTypes>(engine:&mut EngineReferences<ET>, skip_eq:bool) -> MS<ET> {
    let (is_negative,r) = read_numeric(engine, skip_eq);
    match r {
        Ok(b) => read_muskip_byte(engine,is_negative,b),
        Err((cmd,token)) => read_muskip_command(engine,is_negative,cmd,token)
    }
}

pub fn read_muskip_byte<ET:EngineTypes>(engine:&mut EngineReferences<ET>, is_negative:bool,b:u8) -> MS<ET> {
    if b == b',' || b == b'.' {
        read_muskip_dim(engine,is_negative,b'.')
    } else if is_ascii_digit(b) {
        read_muskip_dim(engine,is_negative,b)
    } else {
        todo!("error?")
    }
}

pub fn read_muskip_command<ET:EngineTypes>(engine:&mut EngineReferences<ET>, is_negative:bool,cmd:Command<ET>,token:ET::Token) -> MS<ET> {
    match cmd {
        _ => todo!("read skip command: {:?}",cmd)
    }
}

#[inline(always)]
fn read_muskip_dim<ET:EngineTypes>(engine:&mut EngineReferences<ET>, is_negative:bool,first:u8) -> MS<ET> {
    let base = read_mudim_float(engine,is_negative,first);
    read_muskip_ii(engine,base)
}

fn read_mudim_float<ET:EngineTypes>(engine:&mut EngineReferences<ET>, is_negative:bool, first:u8) -> MB<ET> {
    let mut ret = engine.aux.memory.get_bytes();
    let mut in_decimal = first == b'.' && {ret.push(b'0');true};
    ret.push(first);
    crate::expand_loop!(engine,
        ResolvedToken::Tk {char,code,token} => match (char.try_into(),code) {
            (Ok(b),CommandCode::Other) if is_ascii_digit(b) => {
                ret.push(b);
            }
            (Ok(b','|b'.'),CommandCode::Other) => {
                if in_decimal { todo!("throw error") }
                in_decimal = true;
                ret.push(b'.');
            }
            (_,CommandCode::Space) => {
                let f = ret_float(engine,ret,is_negative);
                return read_mudim_unit(engine,f,None)
            }
            (Ok(b),CommandCode::Other | CommandCode::Letter) => {
                let f = ret_float(engine,ret,is_negative);
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

pub fn read_mudim_unit<ET:EngineTypes>(engine:&mut EngineReferences<ET>, float:f64, mut first:Option<(u8, ET::Token)>) -> MB<ET> {
    let units = MS::<ET>::units();
    match read_keywords(engine,units,first) {
        Some(d) => MS::<ET>::from_float(engine,float,d),
        _ => todo!("wut")
    }
}

fn read_muskip_ii<ET:EngineTypes>(engine:&mut EngineReferences<ET>, base:MB<ET>) -> MS<ET> {
    match read_keywords(engine,&[PLUS,MINUS],None) {
        Some(b) if b == PLUS => {
            let stretch = read_mustretch(engine);
            if read_keyword(engine,MINUS,None) {
                MS::<ET>::new(base,Some(stretch),Some(read_mushrink(engine)))
            } else {
                MS::<ET>::new(base, Some(stretch), None)
            }
        }
        Some(b) if b == MINUS => {
            let shrink = read_mushrink(engine);
            if read_keyword(engine,PLUS,None) {
                MS::<ET>::new(base,Some(read_mustretch(engine)),Some(shrink))
            } else {
                MS::<ET>::new(base,None,Some(shrink))
            }
        }
        _ => MS::<ET>::new(base,None,None)
    }
}

fn read_mustretch<ET:EngineTypes>(engine:&mut EngineReferences<ET>) -> MSt<ET> {
    let mut is_negative = false;
    crate::expand_loop!(engine,
        ResolvedToken::Tk {char,code,..} => match (char.try_into(),code) {
            (_,CommandCode::Space) => (),
            (Ok(b'-'),CommandCode::Other) => {
                is_negative =!is_negative;
            }
            (Ok(b'+'),CommandCode::Other) => (),
            (Ok(b),CommandCode::Other) if is_ascii_digit(b) => return read_mustretch_float(engine,is_negative,b),
            (Ok(b','|b'.'),CommandCode::Other) => return read_mustretch_float(engine,is_negative,b'.'),
            o => todo!("error?")
        }
        o => todo!("command in read_skip")
    );
    crate::file_end!()
}
fn read_mustretch_float<ET:EngineTypes>(engine:&mut EngineReferences<ET>, is_negative:bool, first:u8) -> MSt<ET> {
    let mut ret = engine.aux.memory.get_bytes();
    let mut in_decimal = first == b'.' && {ret.push(b'0');true};
    ret.push(first);
    crate::expand_loop!(engine,
        ResolvedToken::Tk {token,char,code} => match (char.try_into(),code) {
            (Ok(b),CommandCode::Other) if is_ascii_digit(b) => {
                ret.push(b);
            }
            (Ok(b','|b'.'),CommandCode::Other) => {
                if in_decimal { todo!("throw error") }
                in_decimal = true;
                ret.push(b'.');
            }
            (_,CommandCode::Space) => {
                let f = ret_float(engine,ret,is_negative);
                return read_mustretch_unit(engine,f,None)
            }
            (Ok(b),CommandCode::Other | CommandCode::Letter) => {
                let f = ret_float(engine,ret,is_negative);
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
fn read_mustretch_unit<ET:EngineTypes>(engine:&mut EngineReferences<ET>,mut float:f64,mut first:Option<(u8,ET::Token)>) -> MSt<ET> {
    let units = MS::<ET>::stretch_units();
    match read_keywords(engine,units,first) {
        Some(d) => MS::<ET>::stretch_from_float(engine,float,d),
        _ => todo!("wut")
    }
}

fn read_mushrink<ET:EngineTypes>(engine:&mut EngineReferences<ET>) -> MSh<ET> {
    let mut is_negative = false;
    crate::expand_loop!(engine,
        ResolvedToken::Tk{char,code,..} => match (char.try_into(),code) {
            (_,CommandCode::Space) => (),
            (Ok(b'-'),CommandCode::Other) => {
                is_negative =!is_negative;
            }
            (Ok(b'+'),CommandCode::Other) => (),
            (Ok(b),CommandCode::Other) if is_ascii_digit(b) => return read_mushrink_float(engine,is_negative,b),
            (Ok(b','|b'.'),CommandCode::Other) => return read_mushrink_float(engine,is_negative,b'.'),
            o => todo!("error?")
        }
        o => todo!("command in read_skip")
    );
    crate::file_end!()
}

fn read_mushrink_float<ET:EngineTypes>(engine:&mut EngineReferences<ET>, is_negative:bool, first:u8) -> MSh<ET> {
    let mut ret = engine.aux.memory.get_bytes();
    let mut in_decimal = first == b'.' && {ret.push(b'0');true};
    ret.push(first);
    crate::expand_loop!(engine,
        ResolvedToken::Tk {token,char,code} => match (char.try_into(),code) {
            (Ok(b),CommandCode::Other) if is_ascii_digit(b) => {
                ret.push(b);
            }
            (Ok(b','|b'.'),CommandCode::Other) => {
                if in_decimal { todo!("throw error") }
                in_decimal = true;
                ret.push(b'.');
            }
            (_,CommandCode::Space) => {
                let f = ret_float(engine,ret,is_negative);
                return read_mushrink_unit(engine,f,None)
            }
            (Ok(b),CommandCode::Other | CommandCode::Letter) => {
                let f = ret_float(engine,ret,is_negative);
                return read_mushrink_unit(engine,f,Some((b,token)))
            }
            _ => {
                todo!("{}:{:?}",char,code);
            }
        }
        _ => todo!("command in read_dim_inner")
    );
    todo!("read_dim_inner")
}

fn read_mushrink_unit<ET:EngineTypes>(engine:&mut EngineReferences<ET>,mut float:f64,mut first:Option<(u8,ET::Token)>) -> MSh<ET> {
    let units = MS::<ET>::shrink_units();
    match read_keywords(engine,units,first) {
        Some(d) => MS::<ET>::shrink_from_float(engine,float,d),
        _ => todo!("wut")
    }
}


pub fn read_keyword<ET:EngineTypes>(engine:&mut EngineReferences<ET>,kw:&[u8],first:Option<(u8,ET::Token)>) -> bool {
    let mut ret = arrayvec::ArrayVec::<_,10>::new();//engine.aux.memory.get_bytes();
    let mut read = arrayvec::ArrayVec::<_,10>::new();//engine.aux.memory.get_token_vec();
    if let Some((b,t)) = first {
        ret.push(b.to_ascii_lowercase());
        read.push(t);
    }
    crate::expand_loop!(engine,
        ResolvedToken::Tk {token,char,code} => match (char.try_into(),code) {
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
        ResolvedToken::Cmd {token,..} => {
            read.push(token);
            for t in read.into_iter().rev() {engine.requeue(t)}
            //engine.aux.memory.return_bytes(ret);
            //engine.aux.memory.return_token_vec(read);
            return false
        }
    );
    todo!("read_keyword")
}


pub fn read_keywords<'a,ET:EngineTypes>(engine:&mut EngineReferences<ET>,kws:&[&'a[u8]],first:Option<(u8,ET::Token)>) -> Option<&'a[u8]> {
    let mut ret = arrayvec::ArrayVec::<_,10>::new();//engine.aux.memory.get_bytes();
    let mut read = arrayvec::ArrayVec::<_,10>::new();//engine.aux.memory.get_token_vec();
    if let Some((b,t)) = first {
        ret.push(b.to_ascii_lowercase());
        read.push(t);
    }
    crate::expand_loop!(engine,
        ResolvedToken::Tk {token,char,code} => match (char.try_into(),code) {
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
        ResolvedToken::Cmd {token,..} => {
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


pub fn read_chars<ET:EngineTypes>(engine:&mut EngineReferences<ET>,kws:&[u8]) -> Result<u8,ET::Token> {
    crate::expand_loop!(engine,
        ResolvedToken::Tk {char,code,token} => match (char.try_into(),code) {
            (_,CommandCode::Space) => (),
            (Ok(b),_) if kws.contains(&b) => {
                return Ok(b)
            }
            _ => {
                return Err(token)
            }
        }
        ResolvedToken::Cmd {token,..} => {
            return Err(token)
        }
    );
    todo!("read_chars")
}