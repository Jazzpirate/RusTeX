use std::marker::PhantomData;
use crate::{catch, debug_log, throw};
use crate::engine::EngineType;
use crate::engine::gullet::Gullet;
use crate::engine::state::modes::GroupType;
use crate::engine::state::State;
use crate::engine::stomach::Stomach;
use crate::engine::mouth::Mouth;
use crate::tex::commands::{methods, BaseStomachCommand, StomachCommand};
use crate::tex::token::{Token, TokenList};
use crate::utils::errors::TeXError;

pub fn digest<ET:EngineType>(stomach:&mut ET::Stomach, state:&mut ET::State, gullet:&mut ET::Gullet, cmd:StomachCommand<ET>)
    -> Result<(),TeXError<ET::Token>> {
    use BaseStomachCommand::*;
    debug_log!(trace=>"digesting command \"{:?}\" ({:?})",cmd.command,cmd.source.cause);
    match cmd.command {
        Unexpandable {name,apply} =>
           Ok(apply(state,gullet,cmd.source)?),
        Assignment {name,set} => {
            set(state,gullet,cmd.source,false)?;
            match state.take_afterassignment() {
                Some(t) => gullet.mouth().requeue(t),
                _ => ()
            }
            Ok(())
        }
        ValueAss(set) => {
            set(state,gullet,cmd.source,false)?;
            match state.take_afterassignment() {
                Some(t) => gullet.mouth().requeue(t),
                _ => ()
            }
            Ok(())
        }
        Font(f) => {
            state.set_current_font(f,false);
            match state.take_afterassignment() {
                Some(t) => gullet.mouth().requeue(t),
                _ => ()
            }
            Ok(())
        }
        OpenBox {..} => todo!("OpenBox"),
        Whatsit {name,..} => todo!("Whatsits"),
        Relax => Ok(()),
        Char{..} => {
            let mode = state.mode();
            todo!("Character in digest: {:?} at {}",mode,gullet.mouth().file_line())
        }
        MathChar(_) => todo!("Mathchar in digest"),
        Superscript => todo!("Superscript in digest"),
        Subscript => todo!("Subscript in digest"),
        Space if state.mode().is_vertical() => Ok(()),
        Space => todo!("Space in H mode"),
        MathShift => todo!("MathShift in digest"),
        BeginGroup => Ok(state.stack_push(GroupType::Token)),
        EndGroup => match state.stack_pop() {
            Some((v,GroupType::Token)) => {
                let mut ret = gullet.mouth().new_tokensource();
                for t in v {ret.push(t)}
                gullet.mouth().push_tokens(ret);
                Ok(())
            }
            Some((v,GroupType::Box(b))) => {
                match state.box_stack_mut().pop() {
                    Some(crate::tex::boxes::OpenBox::Box {list,mode,on_close}) if mode == b => {
                        match on_close(state,gullet,list) {
                            Some(b) => unsafe{state.box_stack_mut().last_mut().unwrap_unchecked()}.ls().push(b),
                            None => {}
                        }
                        Ok(())
                    }
                    Some(crate::tex::boxes::OpenBox::Paragraph {list}) =>
                        todo!("Close paragraph"),
                    _ =>throw!("Unexpected box on stack" => cmd.source.cause)
                }
            }
            _ => throw!("Unexpected end group" => cmd.source.cause)
        }
    }
}
