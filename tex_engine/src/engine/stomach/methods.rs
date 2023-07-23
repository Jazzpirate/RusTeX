use std::marker::PhantomData;
use crate::{catch, debug_log};
use crate::engine::gullet::Gullet;
use crate::engine::state::modes::GroupType;
use crate::engine::state::State;
use crate::engine::stomach::Stomach;
use crate::engine::mouth::Mouth;
use crate::tex::commands::{Assignable, methods, StomachCommand};
use crate::tex::commands::Command::MathChar;
use crate::tex::token::Token;
use crate::utils::errors::{ImplementationError, ModeError, TeXError};

pub fn digest<T:Token,Sto:Stomach<T>>(stomach:&mut Sto, state:&mut Sto::S, gullet:&mut Sto::Gu, cmd:StomachCommand<T>)
    -> Result<(),Box<dyn TeXError<T>>> {
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
        ValueRegister(u,Assignable::Int) => assign_int_register(state,gullet,u,cmd,false), // TODO global!
        ValueRegister(u,_) => todo!("ValueRegister assignment"),
        Assignment {name,index} => match stomach.command(index) {
            Some(f) => Ok(f(state, gullet, stomach, cmd, false)?), // TODO global!
            None => Err(ImplementationError(format!("Missing implementation for primitive command {}", name), PhantomData).into())
        }
        AssignableValue {name,tp:Assignable::Int} => Ok(methods::assign_primitive_int(state,gullet,cmd,name,false)?), // TODO global!
        AssignableValue {name,tp:Assignable::Dim} => Ok(methods::assign_primitive_dim(state,gullet,cmd,name,false)?), // TODO global!
        AssignableValue {name,tp:Assignable::Toks} => Ok(methods::assign_primitive_toks(state,gullet,cmd,name,false)?), // TODO global!
        Whatsit {name,index} => todo!("Whatsits"),
        AssignableValue {name,tp} => todo!("Assignable Value"),
        Relax => Ok(()),
        Char(x,_) => todo!("Character in digest"),
        MathChar(_) => todo!("Mathchar in digest"),
        Superscript => todo!("Superscript in digest"),
        Subscript => todo!("Subscript in digest"),
        Space if state.mode().is_vertical() => Ok(()),
        Space => todo!("Space in H mode"),
        MathShift => todo!("MathShift in digest"),
        BeginGroup => Ok(state.stack_push(<<Sto::S as State<T>>::Gr as GroupType>::from_catcode_token())),
        EndGroup => todo!("EndGroup in digest")
    }
}

fn assign_int_register<T:Token,S:State<T>,Gu:Gullet<T,S=S>>(state:&mut S, gullet:&mut Gu, u:usize,cmd:StomachCommand<T>,global:bool)
    -> Result<(),Box<dyn TeXError<T>>> {
    debug_log!(trace=>"Assigning \\count{}",u);
    catch!(gullet.mouth().skip_eq_char(state) => cmd.cause);
    let v = catch!(gullet.get_int(state) => cmd.cause);
    debug_log!(debug=>"\\count{} = {}",u,v);
    state.set_int_register(u,v,global);
    Ok(())
}