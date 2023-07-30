use crate::engine::gullet::Gullet;
use crate::engine::state::State;
use crate::engine::mouth::Mouth;
use crate::engine::stomach::Stomach;
use crate::{cmtodo, debug_log, register_assign, register_conditional, register_dim, register_gullet, register_int, register_int_assign, register_muskip, register_skip, register_tok_assign};
use crate::engine::EngineType;
use crate::tex::catcodes::CategoryCode;
use crate::tex::commands::{Command, GulletCommand, StomachCommand, StomachCommandInner};
//use crate::tex::commands::tex::get_csname;
use crate::tex::numbers::{Frac, MuSkip, Numeric, NumSet, Skip};
use crate::tex::token::{BaseToken, Token};
use crate::utils::errors::{catch_prim, ErrorInPrimitive, file_end_prim, TeXError};
use crate::utils::strings::CharType;
use crate::tex::numbers::Int;
use crate::utils::Ptr;

/*
pub fn expr_loop<T:Token,S:State<T>,Gu:Gullet<T,S=S>,Num:Numeric>(state:&mut S, gullet:&mut Gu, cmd:&GulletCommand<T>, name:&'static str, get:fn(&mut S, &mut Gu) -> Result<Num,Box<dyn TeXError<T>>>)
                                                                  -> Result<Num,ErrorInPrimitive<T>> {
    let mut stack: Vec<(Option<Num>, Option<fn(Num, Num) -> Num>)> = vec!((None, None));
    'a: loop {
        catch_prim!(gullet.mouth().skip_whitespace(state) => (name,cmd.clone()));
        if catch_prim!(gullet.get_keyword(state,"(") => (name,cmd.clone())) {
            stack.push((None, None));
            continue 'a;
        }
        let mut first = catch_prim!(get(state,gullet) => (name,cmd.clone()));
        match stack.last_mut() {
            Some((Some(second), Some(op))) => {
                first = op(second.clone(),first);
                *unsafe{stack.last_mut().unwrap_unchecked()} = (None,None);
            }
            _ => ()
        }
        loop {
            catch_prim!(gullet.mouth().skip_whitespace(state) => (name,cmd.clone()));
            let kw = catch_prim!(
                if stack.len()>1 {gullet.get_keywords(state,vec!("+","-","*","/",")"))}
                else {gullet.get_keywords(state,vec!("+","-","*","/"))}
                => ("numexpr",cmd.clone()));
            match kw {
                None => {
                    match stack.pop() {
                        Some((None, None)) if stack.is_empty() => return Ok(first),
                        _ => return Err(ErrorInPrimitive { name, msg: Some("Expected ')'".to_string()), cause: Some(cmd.cause.clone()), source: None })
                    }
                }
                Some(r) if r == ")" => {
                    stack.pop();
                    match stack.last_mut() {
                        Some((Some(second), Some(op))) => {
                            first = op(first, second.clone());
                            *unsafe{stack.last_mut().unwrap_unchecked()} = (None,None);
                        }
                        _ => ()
                    }
                }
                Some(r) if r == "+" => {
                    *stack.last_mut().unwrap() = (Some(first), Some(|a, b| a + b));
                    continue 'a;
                }
                Some(r) if r == "-" => {
                    *stack.last_mut().unwrap() = (Some(first), Some(|a, b| a - b));
                    continue 'a;
                }
                Some(r) if r == "*" => {
                    let ret = catch_prim!(expr_loop(state,gullet,cmd,&name,|s,g| {
                        Ok(Frac::new(g.get_int(s)?.to_i64(),1))
                    }) => (name,cmd.clone()));
                    first = first.scale(ret.0,ret.1);
                }
                Some(r) if r == "/" => {
                    let ret = catch_prim!(expr_loop(state,gullet,cmd,&name,|s,g| {
                        Ok(Frac::new(g.get_int(s)?.to_i64(),1))
                    }) => (name,cmd.clone()));
                    first = first.scale(ret.1,ret.0);
                }
                _ => unreachable!()
            }
        }
    }
}


pub fn dimexpr<T:Token,S:State<T>,Gu:Gullet<T,S=S>>(state:&mut S,gullet:&mut Gu,cmd:GulletCommand<T>) -> Result<<S::NumSet as NumSet>::Dim,ErrorInPrimitive<T>> {
    debug_log!(trace=>"dimexpr: {}",gullet.mouth().preview(100));
    let ret = expr_loop(state, gullet, &cmd, "dimexpr", |s, g| g.get_dim(s))?;
    if let Some((next,_)) = catch_prim!(gullet.mouth().get_next(state) => ("dimexpr",cmd)) {
        match next.base() {
            BaseToken::CS(name) => match state.get_command(name).as_deref() {
                Some(Command::Relax) => (),
                _ => gullet.mouth().requeue(next)
            }
            _ => gullet.mouth().requeue(next)
        }
    }
    Ok(ret)
}

pub fn etexrevision<T:Token,S:State<T>,Gu:Gullet<T,S=S>>(_:&mut S,_gullet:&mut Gu,_cmd:GulletCommand<T>) -> Result<Vec<T>,ErrorInPrimitive<T>> {
    Ok(T::from_str(".6".to_string()))
}
pub fn etexversion<T:Token,S:State<T>,Gu:Gullet<T,S=S>>(_:&mut S,_gullet:&mut Gu,cmd:GulletCommand<T>) -> Result<<S::NumSet as NumSet>::Int,ErrorInPrimitive<T>> {
    Ok(catch_prim!(<S::NumSet as NumSet>::Int::from_i64(2) => ("eTeXversion",cmd)))
}

pub fn expanded<T:Token,S:State<T>,Gu:Gullet<T,S=S>>(state:&mut S,gullet:&mut Gu,cmd:GulletCommand<T>) -> Result<Vec<T>,ErrorInPrimitive<T>> {
    debug_log!(trace=>"expanded");
    Ok(catch_prim!(gullet.get_expanded_group(state,false,false,false) => ("expanded",cmd)))
}

pub fn glueexpr<T:Token,S:State<T>,Gu:Gullet<T,S=S>>(state:&mut S,gullet:&mut Gu,cmd:GulletCommand<T>) -> Result<Skip<<S::NumSet as NumSet>::SkipDim>,ErrorInPrimitive<T>> {
    debug_log!(trace=>"glueexpr: {}",gullet.mouth().preview(100));
    let ret = expr_loop(state, gullet, &cmd, "glueexpr", |s, g| g.get_skip(s))?;
    if let Some((next,_)) = catch_prim!(gullet.mouth().get_next(state) => ("glueexpr",cmd)) {
        match next.base() {
            BaseToken::CS(name) => match state.get_command(name).as_deref() {
                Some(Command::Relax) => (),
                _ => gullet.mouth().requeue(next)
            }
            _ => gullet.mouth().requeue(next)
        }
    }
    Ok(ret)
}

pub fn ifcsname<T:Token,Gu:Gullet<T>>(state:&mut Gu::S,gullet:&mut Gu,cmd:GulletCommand<T>) -> Result<bool,ErrorInPrimitive<T>> {
    debug_log!(trace=>"ifcsname");
    let name = get_csname(state,gullet,cmd,"ifcsname")?;
    Ok(state.get_command(&name).is_some())
}

pub fn ifdefined<T:Token,Gu:Gullet<T>>(state:&mut Gu::S,gullet:&mut Gu,cmd:GulletCommand<T>) -> Result<bool,ErrorInPrimitive<T>> {
    debug_log!(trace=>"ifdefined");
    match catch_prim!(gullet.mouth().get_next(state) => ("ifeof",cmd)) {
        None => file_end_prim!("ifeof",cmd),
        Some((t,_)) => match t.base() {
            BaseToken::Char(c,CategoryCode::Active) =>
                Ok(state.get_ac_command(*c).is_some()),
            BaseToken::CS(name) => {
                debug_log!(trace => "ifdefined: {}",name.to_string());
                Ok(state.get_command(name).is_some())
            }
            _ => Err(ErrorInPrimitive{name:"ifdefined",msg:Some(format!("Expected a control sequence, got: {:?}",t)),cause:Some(cmd.cause),source:None})
        }
    }
}

pub fn muexpr<T:Token,S:State<T>,Gu:Gullet<T,S=S>>(state:&mut S,gullet:&mut Gu,cmd:GulletCommand<T>) -> Result<MuSkip<<S::NumSet as NumSet>::MuDim,<S::NumSet as NumSet>::MuStretchShrinkDim>,ErrorInPrimitive<T>> {
    debug_log!(trace=>"muexpr: {}",gullet.mouth().preview(100));
    let ret = expr_loop(state, gullet, &cmd, "muexpr", |s, g| g.get_muskip(s))?;
    if let Some((next,_)) = catch_prim!(gullet.mouth().get_next(state) => ("muexpr",cmd)) {
        match next.base() {
            BaseToken::CS(name) => match state.get_command(name).as_deref() {
                Some(Command::Relax) => (),
                _ => gullet.mouth().requeue(next)
            }
            _ => gullet.mouth().requeue(next)
        }
    }
    Ok(ret)
}

pub fn numexpr<T:Token,S:State<T>,Gu:Gullet<T,S=S>>(state:&mut S,gullet:&mut Gu,cmd:GulletCommand<T>) -> Result<<S::NumSet as NumSet>::Int,ErrorInPrimitive<T>> {
    debug_log!(trace=>"numexpr: {}",gullet.mouth().preview(100));
    let ret = expr_loop(state, gullet, &cmd, "numexpr", |s, g| g.get_int(s))?;
    if let Some((next,_)) = catch_prim!(gullet.mouth().get_next(state) => ("numexpr",cmd)) {
        match next.base() {
            BaseToken::CS(name) => match state.get_command(name).as_deref() {
                Some(Command::Relax) => (),
                _ => gullet.mouth().requeue(next)
            }
            _ => gullet.mouth().requeue(next)
        }
    }
    Ok(ret)
}

use super::tex::{global,long,outer,def,edef,gdef,xdef};

pub fn protected<T:Token,Sto:Stomach<T>>(stomach:&mut Sto,state:&mut Sto::S,gullet:&mut Sto::Gu,cmd:StomachCommand<T>,global_:bool,_:bool,long_:bool,outer_:bool)
                                                      -> Result<(),ErrorInPrimitive<T>> {
    debug_log!(trace => "\\protected");
    match catch_prim!(gullet.get_next_stomach_command(state) => ("protected",cmd)) {
        None => file_end_prim!("protected",cmd),
        Some(c) => match c.cmd {
            StomachCommandInner::Assignment {name:"global",..} => global(stomach,state,gullet,cmd,global_,true,long_,outer_),
            StomachCommandInner::Assignment {name:"protected",..} => protected(stomach,state,gullet,cmd,global_,true,long_,outer_),
            StomachCommandInner::Assignment {name:"long",..} => long(stomach,state,gullet,cmd,global_,true,long_,outer_),
            StomachCommandInner::Assignment {name:"outer",..} => outer(stomach,state,gullet,cmd,global_,true,long_,outer_),
            StomachCommandInner::Assignment {name:"def",..} => def(state,gullet,cmd,global_,true,long_,outer_),
            StomachCommandInner::Assignment {name:"edef",..} => edef(state,gullet,cmd,global_,true,long_,outer_),
            StomachCommandInner::Assignment {name:"gdef",..} => gdef(state,gullet,cmd,global_,true,long_,outer_),
            StomachCommandInner::Assignment {name:"xdef",..} => xdef(state,gullet,cmd,global_,true,long_,outer_),
            _ => return Err(ErrorInPrimitive{name:"protected",msg:Some("Expected a macro definition after \\protected".to_string()),cause:Some(cmd.cause),source:None})
        }
    }
}

pub fn unexpanded<T:Token,S:State<T>,Gu:Gullet<T,S=S>>(state:&mut S,gullet:&mut Gu,cmd:GulletCommand<T>) -> Result<Vec<T>,ErrorInPrimitive<T>> {
    debug_log!(trace=>"unexpanded");
    match catch_prim!(gullet.get_next_stomach_command(state) => ("unexpanded",cmd)) {
        None => file_end_prim!("unexpanded",cmd),
        Some(sc) => match sc.cmd {
            StomachCommandInner::BeginGroup(_) => (),
            _ => return Err(ErrorInPrimitive{name:"unexpanded",msg:Some("Expected a begin group after \\unexpanded".to_string()),cause:Some(cmd.cause),source:None})
        }
    }
    Ok(catch_prim!(gullet.mouth().read_until_endgroup(state) => ("unexpanded",cmd)))
}

 */

pub fn initialize_etex_primitives<ET:EngineType>(state:&mut ET::State,stomach:&mut ET::Stomach,gullet:&mut ET::Gullet) {
    /*
    register_dim!(dimexpr,state,stomach,gullet,(s,g,c) => dimexpr(s,g,c));
    register_gullet!(eTeXrevision,state,stomach,gullet,(s,g,c) => etexrevision(s,g,c));
    register_int!(eTeXversion,state,stomach,gullet,(s,g,c) => etexversion(s,g,c));
    register_tok_assign!(everyeof,state,stomach,gullet);
    register_gullet!(expanded,state,stomach,gullet,(s,g,c) => expanded(s,g,c));
    register_skip!(glueexpr,state,stomach,gullet,(s,g,c) => glueexpr(s,g,c));
    register_conditional!(ifcsname,state,stomach,gullet,(s,gu,cmd) =>ifcsname(s,gu,cmd));
    register_conditional!(ifdefined,state,stomach,gullet,(s,gu,cmd) =>ifdefined(s,gu,cmd));
    register_muskip!(muexpr,state,stomach,gullet,(s,g,c) => muexpr(s,g,c));
    register_int!(numexpr,state,stomach,gullet,(s,g,c) => numexpr(s,g,c));
    register_int_assign!(tracingassigns,state,stomach,gullet);
    register_int_assign!(tracinggroups,state,stomach,gullet);
    register_int_assign!(tracingifs,state,stomach,gullet);
    register_int_assign!(tracingnesting,state,stomach,gullet);
    register_int_assign!(tracingscantokens,state,stomach,gullet);
    register_assign!(protected,state,stomach,gullet,(s,gu,sto,cmd,g) =>protected(sto,s,gu,cmd,g,false,false,false));
    register_gullet!(unexpanded,state,stomach,gullet,(s,g,c) => unexpanded(s,g,c));

     */

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
    cmtodo!(state,stomach,gullet,readline);
    cmtodo!(state,stomach,gullet,savinghyphcodes);
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
    cmtodo!(state,stomach,gullet,unless);
    cmtodo!(state,stomach,gullet,widowpenalties);
}