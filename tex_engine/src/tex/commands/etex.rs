use crate::engine::gullet::Gullet;
use crate::engine::state::State;
use crate::engine::mouth::Mouth;
use crate::engine::stomach::Stomach;
use crate::{cmtodo, debug_log, register_assign, register_conditional, register_dim, register_gullet, register_int, register_int_assign, register_muskip, register_skip, register_tok_assign};
use crate::engine::EngineType;
use crate::engine::gullet::methods::string_to_tokens;
use crate::tex::catcodes::CategoryCode;
use crate::tex::commands::{Command, GulletCommand, StomachCommand, StomachCommandInner};
use crate::tex::commands::tex::get_csname;
use crate::tex::numbers::{Frac, MuSkip, Numeric, NumSet, Skip};
use crate::tex::token::{BaseToken, Token};
use crate::utils::errors::{catch_prim, ErrorInPrimitive, file_end_prim, TeXError};
use crate::utils::strings::CharType;
use crate::tex::numbers::Int;
use crate::utils::Ptr;

pub fn expr_scale_loop<ET:EngineType>(state:&mut ET::State, gullet:&mut ET::Gullet, cmd:&GulletCommand<ET::Token>, name:&'static str)
                                                  -> Result<Frac,ErrorInPrimitive<ET::Token>> {
    let mut stack: Vec<(Option<Frac>,Option<fn(Frac,Frac) -> Frac>)> = vec!((None,None));
    'a: loop {
        catch_prim!(gullet.mouth().skip_whitespace::<ET>(state) => (name,cmd.clone()));
        if catch_prim!(gullet.get_keyword(state,"(") => (name,cmd.clone())) {
            stack.push((None,None));
            continue 'a;
        }
        let mut first = Frac::new(catch_prim!(gullet.get_int(state) => (name,cmd.clone())).to_i64(),1);
        loop {
            catch_prim!(gullet.mouth().skip_whitespace::<ET>(state) => (name,cmd.clone()));
            let kw = catch_prim!(
                if stack.len()>1 {gullet.get_keywords(state,vec!("+","-","*","/",")"))}
                else {gullet.get_keywords(state,vec!("*","/"))}
                => ("numexpr",cmd.clone()));
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
                        _ => return Err(ErrorInPrimitive { name, msg: Some("Expected ')'".to_string()), cause: Some(cmd.cause.clone()), source: None })
                    }
                }
                Some(r) if r == ")" => {
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
                Some(r) if r == "+" => {
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
                Some(r) if r == "-" => {
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
                Some(r) if r == "*" => {
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
                Some(r) if r == "/" => {
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

pub fn expr_loop<ET:EngineType,Num:Numeric>(state:&mut ET::State, gullet:&mut ET::Gullet, cmd:&GulletCommand<ET::Token>, name:&'static str,
                                get:fn(&mut ET::State, &mut ET::Gullet) -> Result<Num,Box<dyn TeXError<ET::Token>>>)
                                                                  -> Result<Num,ErrorInPrimitive<ET::Token>> {
    let mut stack: Vec<(Option<Num>, Option<fn(Num, Num) -> Num>)> = vec!((None, None));
    'a: loop {
        catch_prim!(gullet.mouth().skip_whitespace::<ET>(state) => (name,cmd.clone()));
        if catch_prim!(gullet.get_keyword(state,"(") => (name,cmd.clone())) {
            stack.push((None, None));
            continue 'a;
        }
        let mut first = catch_prim!(get(state,gullet) => (name,cmd.clone()));
        loop {
            catch_prim!(gullet.mouth().skip_whitespace::<ET>(state) => (name,cmd.clone()));
            let kw = catch_prim!(
                if stack.len()>1 {gullet.get_keywords(state,vec!("+","-","*","/",")"))}
                else {gullet.get_keywords(state,vec!("+","-","*","/"))}
                => ("numexpr",cmd.clone()));
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
                        _ => return Err(ErrorInPrimitive { name, msg: Some("Expected ')'".to_string()), cause: Some(cmd.cause.clone()), source: None })
                    }
                }
                Some(r) if r == ")" => {
                    match stack.pop() {
                        Some((Some(second), Some(op))) => {
                            first = op(second.clone(),first);
                        }
                        _ => ()
                    }
                }
                Some(r) if r == "+" => {
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
                Some(r) if r == "-" => {
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
                Some(r) if r == "*" => {
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
                Some(r) if r == "/" => {
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


pub fn dimexpr<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:GulletCommand<ET::Token>) -> Result<ET::Dim,ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"dimexpr: {}",gullet.mouth().preview(100));
    let ret = expr_loop::<ET,ET::Dim>(state, gullet, &cmd, "dimexpr", |s, g:&mut ET::Gullet| g.get_dim(s))?;
    if let Some((next,_)) = catch_prim!(gullet.mouth().get_next::<ET>(state) => ("dimexpr",cmd)) {
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

pub fn etexrevision<ET:EngineType>() -> Result<Vec<ET::Token>,ErrorInPrimitive<ET::Token>> {
    Ok(string_to_tokens(".6".as_bytes()))
}

pub fn etexversion<ET:EngineType>(cmd:GulletCommand<ET::Token>) -> Result<ET::Int,ErrorInPrimitive<ET::Token>> {
    Ok(catch_prim!(ET::Int::from_i64(2) => ("eTeXversion",cmd)))
}

pub fn expanded<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:GulletCommand<ET::Token>) -> Result<Vec<ET::Token>,ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"expanded");
    Ok(catch_prim!(gullet.get_expanded_group(state,false,false,false) => ("expanded",cmd)))
}

pub fn glueexpr<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:GulletCommand<ET::Token>) -> Result<Skip<ET::SkipDim>,ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"glueexpr: {}",gullet.mouth().preview(100));
    let ret = expr_loop::<ET,Skip<ET::SkipDim>>(state, gullet, &cmd, "glueexpr", |s, g:&mut ET::Gullet| g.get_skip(s))?;
    if let Some((next,_)) = catch_prim!(gullet.mouth().get_next::<ET>(state) => ("glueexpr",cmd)) {
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

pub fn ifcsname<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:GulletCommand<ET::Token>) -> Result<bool,ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"ifcsname");
    let name = get_csname::<ET>(state,gullet,cmd,"ifcsname")?;
    Ok(state.get_command(&name).is_some())
}

pub fn ifdefined<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:GulletCommand<ET::Token>) -> Result<bool,ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"ifdefined");
    match catch_prim!(gullet.mouth().get_next::<ET>(state) => ("ifeof",cmd)) {
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

pub fn muexpr<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:GulletCommand<ET::Token>)
    -> Result<MuSkip<ET::MuDim,ET::MuStretchShrinkDim>,ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"muexpr: {}",gullet.mouth().preview(100));
    let ret = expr_loop::<ET,MuSkip<ET::MuDim,ET::MuStretchShrinkDim>>(state, gullet, &cmd, "muexpr", |s, g:&mut ET::Gullet| g.get_muskip(s))?;
    if let Some((next,_)) = catch_prim!(gullet.mouth().get_next::<ET>(state) => ("muexpr",cmd)) {
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

pub fn numexpr<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:GulletCommand<ET::Token>) -> Result<ET::Int,ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"numexpr: {}",gullet.mouth().preview(100));
    let ret = expr_loop::<ET,ET::Int>(state, gullet, &cmd, "numexpr", |s, g:&mut ET::Gullet| g.get_int(s))?;
    if let Some((next,_)) = catch_prim!(gullet.mouth().get_next::<ET>(state) => ("numexpr",cmd)) {
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

pub fn protected<ET:EngineType>(stomach:&mut ET::Stomach,state:&mut ET::State,gullet:&mut ET::Gullet,cmd:StomachCommand<ET::Token>,global_:bool,_:bool,long_:bool,outer_:bool)
                                                      -> Result<(),ErrorInPrimitive<ET::Token>> {
    debug_log!(trace => "\\protected");
    match catch_prim!(gullet.get_next_stomach_command(state) => ("protected",cmd)) {
        None => file_end_prim!("protected",cmd),
        Some(c) => match c.cmd {
            StomachCommandInner::Assignment {name:"global",..} => global::<ET>(stomach,state,gullet,cmd,global_,true,long_,outer_),
            StomachCommandInner::Assignment {name:"protected",..} => protected::<ET>(stomach,state,gullet,cmd,global_,true,long_,outer_),
            StomachCommandInner::Assignment {name:"long",..} => long::<ET>(stomach,state,gullet,cmd,global_,true,long_,outer_),
            StomachCommandInner::Assignment {name:"outer",..} => outer::<ET>(stomach,state,gullet,cmd,global_,true,long_,outer_),
            StomachCommandInner::Assignment {name:"def",..} => def::<ET>(state,gullet,cmd,global_,true,long_,outer_),
            StomachCommandInner::Assignment {name:"edef",..} => edef::<ET>(state,gullet,cmd,global_,true,long_,outer_),
            StomachCommandInner::Assignment {name:"gdef",..} => gdef::<ET>(state,gullet,cmd,global_,true,long_,outer_),
            StomachCommandInner::Assignment {name:"xdef",..} => xdef::<ET>(state,gullet,cmd,global_,true,long_,outer_),
            _ => return Err(ErrorInPrimitive{name:"protected",msg:Some("Expected a macro definition after \\protected".to_string()),cause:Some(cmd.cause),source:None})
        }
    }
}

pub fn unexpanded<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:GulletCommand<ET::Token>) -> Result<Vec<ET::Token>,ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"unexpanded");
    match catch_prim!(gullet.get_next_stomach_command(state) => ("unexpanded",cmd)) {
        None => file_end_prim!("unexpanded",cmd),
        Some(sc) => match sc.cmd {
            StomachCommandInner::BeginGroup(_) => (),
            _ => return Err(ErrorInPrimitive{name:"unexpanded",msg:Some("Expected a begin group after \\unexpanded".to_string()),cause:Some(cmd.cause),source:None})
        }
    }
    Ok(catch_prim!(gullet.mouth().read_until_endgroup::<ET>(state) => ("unexpanded",cmd)))
}

pub fn unless<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,cmd:GulletCommand<ET::Token>) -> Result<Vec<ET::Token>,ErrorInPrimitive<ET::Token>> {
    debug_log!(trace=>"unless");
    match catch_prim!(gullet.mouth().get_next::<ET>(state) => ("unless",cmd)) {
        None => file_end_prim!("unless",cmd),
        Some((next,true)) => {
            let ncmd = match next.base() {
                BaseToken::CS(name) => state.get_command(name),
                BaseToken::Char(c,CategoryCode::Active) => state.get_ac_command(*c),
                _ => return Err(ErrorInPrimitive{name:"unless",msg:Some("Expected a conditional after \\unless".to_string()),cause:Some(cmd.cause),source:None})
            };
            match ncmd.as_deref() {
                Some(Command::Conditional {name,index}) => {
                    catch_prim!(crate::engine::gullet::methods::do_conditional::<ET>(gullet,state,next,name,*index,true) => ("unless",cmd));
                    Ok(vec!())
                }
                _ => Err(ErrorInPrimitive{name:"unless",msg:Some("Expected a conditional after \\unless".to_string()),cause:Some(cmd.cause),source:None}),
            }
        }
        _ => Err(ErrorInPrimitive{name:"unless",msg:Some("Expected a conditional after \\unless".to_string()),cause:Some(cmd.cause),source:None})
    }
}

pub fn initialize_etex_primitives<ET:EngineType>(state:&mut ET::State,stomach:&mut ET::Stomach,gullet:&mut ET::Gullet) {
    register_dim!(dimexpr,state,stomach,gullet,(s,g,c) => dimexpr::<ET>(s,g,c));
    register_gullet!(eTeXrevision,state,stomach,gullet,(s,g,c) => etexrevision::<ET>());
    register_int!(eTeXversion,state,stomach,gullet,(s,g,c) => etexversion::<ET>(c));
    register_tok_assign!(everyeof,state,stomach,gullet);
    register_gullet!(expanded,state,stomach,gullet,(s,g,c) => expanded::<ET>(s,g,c));
    register_skip!(glueexpr,state,stomach,gullet,(s,g,c) => glueexpr::<ET>(s,g,c));
    register_conditional!(ifcsname,state,stomach,gullet,(s,gu,cmd) =>ifcsname::<ET>(s,gu,cmd));
    register_conditional!(ifdefined,state,stomach,gullet,(s,gu,cmd) =>ifdefined::<ET>(s,gu,cmd));
    register_muskip!(muexpr,state,stomach,gullet,(s,g,c) => muexpr::<ET>(s,g,c));
    register_int!(numexpr,state,stomach,gullet,(s,g,c) => numexpr::<ET>(s,g,c));
    register_int_assign!(tracingassigns,state,stomach,gullet);
    register_int_assign!(tracinggroups,state,stomach,gullet);
    register_int_assign!(tracingifs,state,stomach,gullet);
    register_int_assign!(tracingnesting,state,stomach,gullet);
    register_int_assign!(tracingscantokens,state,stomach,gullet);
    register_assign!(protected,state,stomach,gullet,(s,gu,sto,cmd,g) =>protected::<ET>(sto,s,gu,cmd,g,false,false,false));
    register_gullet!(unexpanded,state,stomach,gullet,(s,g,c) => unexpanded::<ET>(s,g,c));
    register_gullet!(unless,state,stomach,gullet,(s,gu,cmd) =>unless::<ET>(s,gu,cmd));

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
    cmtodo!(state,stomach,gullet,widowpenalties);
}