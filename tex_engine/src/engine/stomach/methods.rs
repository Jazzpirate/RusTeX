use std::marker::PhantomData;
use crate::{catch, debug_log};
use crate::engine::EngineType;
use crate::engine::gullet::Gullet;
use crate::engine::state::modes::GroupType;
use crate::engine::state::State;
use crate::engine::stomach::Stomach;
use crate::engine::mouth::Mouth;
use crate::tex::commands::{Assignable, methods, StomachCommand, StomachCommandInner};
use crate::tex::token::{Token, TokenList};
use crate::utils::errors::{ImplementationError, ModeError, OtherError, TeXError, UnexpectedEndgroup};

pub fn digest<ET:EngineType>(stomach:&mut ET::Stomach, state:&mut ET::State, gullet:&mut ET::Gullet, cmd:StomachCommand<ET::Token>)
    -> Result<(),Box<dyn TeXError<ET::Token>>> {
    debug_log!(trace=>"digesting command \"{:?}\" ({:?})",cmd.cmd,cmd.cause);
    use super::StomachCommandInner::*;
    match cmd.cmd {
        Command{name,index} => match stomach.command(index) {
            Some(f) => Ok(f(state,gullet,stomach,cmd,false)?),
            None => Err(ImplementationError(format!("Missing implementation for primitive command {}",name),PhantomData).into())
        }
        ValueAssignment {name,assignment_index,..} => match stomach.command(assignment_index) {
            Some(f) => Ok(f(state,gullet,stomach,cmd,false)?), // TODO global!
            None => Err(ImplementationError(format!("Missing implementation for primitive command {}",name),PhantomData).into())
        }
        Value{name,..} => {
            Err(ModeError{cmd:name.clone(),mode:state.mode(),cause:Some(cmd.cause),source:None}.into())
        }
        ValueRegister(u,Assignable::Int) => assign_int_register::<ET>(state,gullet,u,cmd,false), // TODO global!
        ValueRegister(u,Assignable::Dim) => assign_dim_register::<ET>(state,gullet,u,cmd,false), // TODO global!
        ValueRegister(u,Assignable::Skip) => assign_skip_register::<ET>(state,gullet,u,cmd,false), // TODO global!
        ValueRegister(u,Assignable::MuSkip) => assign_muskip_register::<ET>(state,gullet,u,cmd,false), // TODO global!
        ValueRegister(u,Assignable::Toks) => assign_toks_register::<ET>(state,gullet,u,cmd,false), // TODO global!
        ValueRegister(_,tp) => todo!("Value Register {:?}",tp),
        Assignment {name,index} => match stomach.command(index) {
            Some(f) => Ok(f(state, gullet, stomach, cmd, false)?), // TODO global!
            None => Err(ImplementationError(format!("Missing implementation for primitive command {}", name), PhantomData).into())
        }
        AssignableValue {name,tp:Assignable::Int} => Ok(methods::assign_primitive_int::<ET>(state,gullet,cmd,name,false)?), // TODO global!
        AssignableValue {name,tp:Assignable::Dim} => Ok(methods::assign_primitive_dim::<ET>(state,gullet,cmd,name,false)?), // TODO global!
        AssignableValue {name,tp:Assignable::Skip} => Ok(methods::assign_primitive_skip::<ET>(state,gullet,cmd,name,false)?), // TODO global!
        AssignableValue {name,tp:Assignable::Toks} => Ok(methods::assign_primitive_toks::<ET>(state,gullet,cmd,name,false)?), // TODO global!
        AssignableValue {name,tp} => todo!("Assignable Value: {:?}",tp),
        OpenBox {..} => todo!("OpenBox"),
        Whatsit {name,index} => todo!("Whatsits"),
        Relax => Ok(()),
        Char{..} =>
            todo!("Character in digest"),
        MathChar(_) => todo!("Mathchar in digest"),
        Superscript(_) => todo!("Superscript in digest"),
        Subscript(_) => todo!("Subscript in digest"),
        Space if state.mode().is_vertical() => Ok(()),
        Space => todo!("Space in H mode"),
        MathShift(_) => todo!("MathShift in digest"),
        BeginGroup(_) => Ok(state.stack_push(GroupType::Token)),
        EndGroup(_) => match state.stack_pop() {
            Some((v,GroupType::Token)) => {
                gullet.mouth().push_tokens(v);
                Ok(())
            }
            Some((v,GroupType::Box(b))) => {
                match state.box_stack_mut().pop() {
                    Some(crate::tex::boxes::OpenBox::Box {list,mode,on_close}) if mode == b => {
                        match on_close(stomach,state,gullet,list) {
                            Some(b) => unsafe{state.box_stack_mut().last_mut().unwrap_unchecked()}.ls().push(b),
                            None => {}
                        }
                        Ok(())
                    }
                    Some(crate::tex::boxes::OpenBox::Paragraph {list}) =>
                        todo!("Close paragraph"),
                    _ => Err(OtherError{msg:"Unexpected box on stack".to_string(),cause:Some(cmd.cause), source: None }.into())
                }
            }
            _ => Err(UnexpectedEndgroup(cmd.cause).into()),
        }
    }
}

pub fn assign_int_register<ET:EngineType>(state:&mut ET::State, gullet:&mut ET::Gullet, u:usize,cmd:StomachCommand<ET::Token>,global:bool)
    -> Result<(),Box<dyn TeXError<ET::Token>>> {
    debug_log!(trace=>"Assigning \\count{}",u);
    catch!(gullet.mouth().skip_eq_char::<ET>(state) => cmd.cause);
    let v = catch!(gullet.get_int(state) => cmd.cause);
    debug_log!(debug=>"\\count{} = {}",u,v);
    state.set_int_register(u,v,global);
    Ok(())
}
pub fn assign_dim_register<ET:EngineType>(state:&mut ET::State, gullet:&mut ET::Gullet, u:usize,cmd:StomachCommand<ET::Token>,global:bool)
                                                            -> Result<(),Box<dyn TeXError<ET::Token>>> {
    debug_log!(trace=>"Assigning \\dimen{}",u);
    catch!(gullet.mouth().skip_eq_char::<ET>(state) => cmd.cause);
    let v = catch!(gullet.get_dim(state) => cmd.cause);
    debug_log!(debug=>"\\dimen{} = {}",u,v);
    state.set_dim_register(u,v,global);
    Ok(())
}
pub fn assign_skip_register<ET:EngineType>(state:&mut ET::State, gullet:&mut ET::Gullet, u:usize,cmd:StomachCommand<ET::Token>,global:bool)
                                                            -> Result<(),Box<dyn TeXError<ET::Token>>> {
    debug_log!(trace=>"Assigning \\skip{}",u);
    catch!(gullet.mouth().skip_eq_char::<ET>(state) => cmd.cause);
    let v = catch!(gullet.get_skip(state) => cmd.cause);
    debug_log!(debug=>"\\skip{} = {}",u,v);
    state.set_skip_register(u,v,global);
    Ok(())
}
pub fn assign_muskip_register<ET:EngineType>(state:&mut ET::State, gullet:&mut ET::Gullet, u:usize,cmd:StomachCommand<ET::Token>,global:bool)
                                                             -> Result<(),Box<dyn TeXError<ET::Token>>> {
    debug_log!(trace=>"Assigning \\muskip{}",u);
    catch!(gullet.mouth().skip_eq_char::<ET>(state) => cmd.cause);
    let v = catch!(gullet.get_muskip(state) => cmd.cause);
    debug_log!(debug=>"\\muskip{} = {}",u,v);
    state.set_muskip_register(u,v,global);
    Ok(())
}
pub fn assign_toks_register<ET:EngineType>(state:&mut ET::State,gullet:&mut ET::Gullet,u:usize,cmd:StomachCommand<ET::Token>,global:bool)
    -> Result<(),Box<dyn TeXError<ET::Token>>> {
    debug_log!(trace=>"Setting \\toks{}",u);
    catch!(gullet.mouth().skip_eq_char::<ET>(state) => cmd.cause);
    match catch!(gullet.get_next_stomach_command(state) => cmd.cause) {
        Some(StomachCommand{cmd:StomachCommandInner::BeginGroup(_),..}) => (),
        _ => return Err(OtherError{
            msg:"Begin group token expected".to_string(),cause:Some(cmd.cause),source:None
        }.into())
    }
    let tks = catch!(gullet.mouth().read_until_endgroup::<ET>(state) => cmd.cause);
    debug_log!(debug=>"\\toks{} = {:?}",u,TokenList(tks.clone()));
    state.set_toks_register(u,tks,global);
    Ok(())
}