pub mod methods;

use std::marker::PhantomData;
use std::os::linux::raw::stat;
use crate::debug_log;
use crate::engine::EngineType;
use crate::engine::gullet::Gullet;
use crate::engine::state::State;
use crate::tex::nodes::{OpenBox, CustomNode, Whatsit, HVBox, TeXNode};
use crate::tex::commands::{CommandSource, StomachCommand};
use crate::tex::token::Token;
use crate::utils::errors::TeXError;
use crate::utils::map::Map;

pub trait Stomach<ET:EngineType<Stomach=Self>>:Sized + Clone+'static {
    fn digest(&mut self,state:&mut ET::State, gullet:&mut ET::Gullet, cmd:StomachCommand<ET>) -> Result<(),TeXError<ET>>;

    fn maybe_shipout(&mut self,state:&mut ET::State,force:bool) -> Option<ET::Node> {
        let sd = state.shipout_data();
        if force || sd.pagetotal >= sd.pagegoal {
            if sd.page.is_empty() { return None }
            todo!("shipout")
        } else { None }
    }

    fn next_shipout_box(&mut self, state:&mut ET::State, gullet:&mut ET::Gullet) -> Result<Option<ET::Node>,TeXError<ET>> {
        loop {
            match self.maybe_shipout(state,false) {
                Some(b) => {
                    debug_log!(trace=>"Shipout box");
                    return Ok(Some(b))
                },
                None => {
                    match gullet.get_next_stomach_command(state)? {
                        Some(cmd) => {
                            self.digest(state,gullet,cmd)?
                        }
                        None => {
                            debug_log!(trace=>"No more commands; force shipout box");
                            return Ok(self.maybe_shipout(state,true))
                        }
                    }
                }
            }
        }
    }
}
/*
// TODO
pub struct ShipoutDefaultStomach<T:Token,S:State<T>,Gu:Gullet<T>,B: TeXNode>{
    commands:Map<fn(&mut S,&mut S::FS,&mut Gu,&mut Self,StomachCommand<T>,bool) -> Result<(),TeXError<T>>>,
    //whatsit_cmds:Map<fn(&mut S,&mut Gu,&mut Self,StomachCommand<T>) -> Result<Whatsit<T,Self>,TeXError<T>>>,
    phantom_box:std::marker::PhantomData<B>
}

 */

#[derive(Clone)]
pub struct NoShipoutDefaultStomach<ET:EngineType>(PhantomData<ET>);

impl<ET:EngineType<Stomach=Self>> NoShipoutDefaultStomach<ET> {
    pub fn new() -> Self { Self(PhantomData) }
}
impl<ET:EngineType<Stomach=Self>> Stomach<ET> for NoShipoutDefaultStomach<ET> {
    fn digest(&mut self,state:&mut ET::State, gullet:&mut ET::Gullet, cmd:StomachCommand<ET>) -> Result<(),TeXError<ET>> {
        methods::digest::<ET>(self,state,gullet,cmd)
    }
}