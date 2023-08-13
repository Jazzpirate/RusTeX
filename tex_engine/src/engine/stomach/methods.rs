use std::marker::PhantomData;
use crate::{catch, debug_log, throw};
use crate::engine::{EngineMut, EngineType};
use crate::engine::gullet::Gullet;
use crate::engine::memory::Memory;
use crate::engine::state::modes::{GroupType, TeXMode};
use crate::engine::state::State;
use crate::engine::stomach::Stomach;
use crate::engine::mouth::Mouth;
use crate::tex::commands::{methods, BaseStomachCommand, StomachCommand};
use crate::tex::token::{Token, TokenList};
use crate::utils::errors::TeXError;

pub fn digest<ET:EngineType>(engine:&mut EngineMut<ET>, cmd:StomachCommand<ET>)
    -> Result<(),TeXError<ET>> {
    use BaseStomachCommand::*;
    debug_log!(trace=>"digesting command \"{:?}\" ({:?})",cmd.command,cmd.source.cause);
    match cmd.command {
        Unexpandable {name,apply,starts_paragraph} => {
            if starts_paragraph  {
                match engine.state.mode() {
                    TeXMode::Vertical | TeXMode::InternalVertical =>
                        todo!(),
                    _ => ()
                }
            }
            Ok(apply(engine, cmd.source)?)
        }
        Assignment {name,set} => {
            set(engine,cmd.source,false)?;
            match engine.state.take_afterassignment() {
                Some(t) => engine.mouth.requeue(t,engine.memory),
                _ => ()
            }
            Ok(())
        }
        ValueAss(set) => {
            set(engine,cmd.source,false)?;
            match engine.state.take_afterassignment() {
                Some(t) => engine.mouth.requeue(t,engine.memory),
                _ => ()
            }
            Ok(())
        }
        Font(f) => {
            engine.state.set_current_font(f,false);
            match engine.state.take_afterassignment() {
                Some(t) => engine.mouth.requeue(t,engine.memory),
                _ => ()
            }
            Ok(())
        }
        OpenBox {..} => todo!("OpenBox"),
        Whatsit {name,..} => todo!("Whatsits"),
        Relax => Ok(()),
        Char{..} => {
            let mode = engine.state.mode();
            todo!("Character in digest: {:?} at {}\n{}",mode,engine.current_position(),engine.preview(50))
        }
        MathChar(_) => todo!("Mathchar in digest"),
        Superscript => todo!("Superscript in digest"),
        Subscript => todo!("Subscript in digest"),
        Space if engine.state.mode().is_vertical() => Ok(()),
        Space => todo!("Space in H mode"),
        MathShift => todo!("MathShift in digest"),
        BeginGroup => Ok(engine.state.stack_push(GroupType::Token)),
        EndGroup => match engine.state.stack_pop() {
            Some((v,GroupType::Token)) => {
                engine.add_expansion(|engine,rs| {
                    for t in v {rs.push(t,engine.memory)}
                    Ok(())
                })
            }
            Some((v,GroupType::Box(b))) => {
                match engine.state.shipout_data_mut().box_stack.pop() {
                    Some(crate::tex::nodes::OpenBox::Box {list,mode,on_close}) if mode == b => {
                        match on_close(engine,list) {
                            Some(b) => engine.state.push_node(b),
                            None => {}
                        }
                        Ok(())
                    }
                    Some(crate::tex::nodes::OpenBox::Paragraph {list}) =>
                        todo!("Close paragraph"),
                    _ =>throw!("Unexpected box on stack" => cmd.source.cause)
                }
            }
            _ => throw!("Unexpected end group" => cmd.source.cause)
        }
    }
}

impl<ET:EngineType> EngineMut<'_,ET> {
    pub fn split_stomach(&mut self) -> (&mut ET::Stomach,EngineMutNoStomach<ET>) {
        (self.stomach,EngineMutNoStomach {
            mouth: self.mouth,
            state: self.state,
            gullet: self.gullet,
            memory: self.memory,
        })
    }
}

pub struct EngineMutNoStomach<'a,ET:EngineType> {
    pub state:&'a mut ET::State,
    pub gullet:&'a mut ET::Gullet,
    pub memory:&'a mut Memory<ET>,
    pub mouth:&'a mut ET::Mouth,
}

impl<ET:EngineType> EngineMutNoStomach<'_,ET> {
    pub fn join_stomach<'b>(&'b mut self,stomach:&'b mut ET::Stomach) -> EngineMut<'b,ET> {
        EngineMut {
            state: self.state,
            stomach,
            mouth: self.mouth,
            memory: self.memory,
            gullet:self.gullet,
        }
    }
}