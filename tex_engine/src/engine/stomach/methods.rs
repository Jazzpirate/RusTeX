use crate::{debug_log, throw};
use crate::engine::{EngineRef, EngineType};
use crate::engine::state::modes::{GroupType, TeXMode};
use crate::engine::state::State;
use crate::engine::stomach::Stomach;
use crate::engine::mouth::Mouth;
use crate::tex::commands::{BaseStomachCommand, StomachCommand};
use crate::tex::nodes::{HorV, NodeTrait, TeXNode};
use crate::utils::errors::TeXError;

pub fn digest<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:StomachCommand<ET>) {
    use BaseStomachCommand::*;
    debug_log!(trace=>"digesting command \"{:?}\" ({:?})",cmd.command,cmd.source.cause);
    match cmd.command {
        Unexpandable {name,apply,forces_mode} => {
            match forces_mode {
                None => (),
                Some(mode) => {
                    let currmode = engine.state.mode();
                    match (currmode,mode) {
                        (TeXMode::InternalVertical | TeXMode::Vertical,HorV::Vertical) => (),
                        (TeXMode::Horizontal | TeXMode::RestrictedHorizontal | TeXMode::Math | TeXMode::Displaymath,HorV::Horizontal) => (),
                        _ => todo!()
                    }
                }
            }
            if engine.state.mode() == TeXMode::Vertical {
                ET::Stomach::maybe_shipout(engine,false)
            }
            apply(engine, cmd.source)
        }
        Assignment {name,set} => {
            set(engine,cmd.source,false);
            match engine.state.take_afterassignment() {
                Some(t) => engine.mouth.requeue(t),
                _ => ()
            }
        }
        ValueAss(set) => {
            set(engine,cmd.source,false);
            match engine.state.take_afterassignment() {
                Some(t) => engine.mouth.requeue(t),
                _ => ()
            }
        }
        Font(f) => {
            engine.state.set_current_font(f,false);
            match engine.state.take_afterassignment() {
                Some(t) => engine.mouth.requeue(t),
                _ => ()
            }
        }
        OpenBox {name, apply, mode} => {
            let on_close = apply(engine,cmd.source);
            engine.stomach.shipout_data_mut().box_stack.push(
                crate::tex::nodes::OpenBox::Box {
                    list:vec!(), mode, on_close
                }
            );
        }
        Whatsit {name,apply} => {
            let wi = apply(engine,cmd.source);
            engine.stomach.push_node(TeXNode::Whatsit(wi));
        },
        Relax => (),
        Char{..} => {
            let mode = engine.state.mode();
            todo!("Character in digest: {:?} at {}\n{}",mode,engine.current_position(),engine.preview(50))
        }
        MathChar(_) => todo!("Mathchar in digest"),
        Superscript => todo!("Superscript in digest"),
        Subscript => todo!("Subscript in digest"),
        Space if engine.state.mode().is_vertical() => (),
        Space => todo!("Space in H mode"),
        MathShift => todo!("MathShift in digest"),
        BeginGroup => engine.state.stack_push(GroupType::Token),
        EndGroup => match engine.state.stack_pop(engine.memory) {
            Some((v,GroupType::Token)) => {
                if !v.is_empty() {
                    engine.add_expansion(|engine, rs| {
                        for t in v { rs.push(t, engine.memory) }
                    })
                }
            }
            Some((v,GroupType::Box(b))) => {
                if !v.is_empty() {
                    engine.add_expansion(|engine, rs| {
                        for t in v { rs.push(t, engine.memory) }
                    })
                }
                match engine.stomach.shipout_data_mut().box_stack.pop() {
                    Some(crate::tex::nodes::OpenBox::Box {list,mode,on_close}) if mode == b => {
                        match on_close(engine,list) {
                            Some(b) => engine.stomach.push_node(b.as_node()),
                            None => {}
                        }
                    }
                    Some(crate::tex::nodes::OpenBox::Paragraph {list}) =>
                        todo!("Close paragraph"),
                    _ =>throw!("Unexpected box on stack" => cmd.source.cause)
                }
                if engine.state.mode() == TeXMode::Vertical {
                    ET::Stomach::maybe_shipout(engine,false)
                }
            }
            _ => throw!("Unexpected end group" => cmd.source.cause)
        }
    }
}