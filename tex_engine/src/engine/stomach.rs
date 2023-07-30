pub mod methods;

use crate::debug_log;
use crate::engine::EngineType;
use crate::engine::gullet::Gullet;
use crate::engine::state::State;
use crate::tex::boxes::{StomachNode, OpenBox, TeXNode, Whatsit};
use crate::tex::commands::{StomachCommand,StomachCommandInner};
use crate::tex::token::Token;
use crate::utils::errors::{ErrorInPrimitive, TeXError};
use crate::utils::map::Map;

type StomachFun<ET:EngineType> = fn(&mut ET::State, &mut ET::Gullet, &mut ET::Stomach, StomachCommand<ET::Token>, bool) -> Result<(),ErrorInPrimitive<ET::Token>>;
type WhatsitFun<ET:EngineType> = fn(&mut ET::State,&mut ET::Gullet,&mut ET::Stomach,StomachCommand<ET::Token>) -> Result<Whatsit<ET>,ErrorInPrimitive<ET::Token>>;
type BoxReturn<ET:EngineType> = Box<dyn FnOnce(&mut ET::Stomach, &mut ET::State, &mut ET::Gullet,Vec<StomachNode<ET>>) -> Option<StomachNode<ET>>>;
type BoxFun<ET:EngineType> = fn(&mut ET::State,&mut ET::Gullet,&mut ET::Stomach,StomachCommand<ET::Token>) -> Result<BoxReturn<ET> ,ErrorInPrimitive<ET::Token>>;

pub trait Stomach<ET:EngineType<Stomach=Self>>:Sized+'static {
    fn register_primitive(&mut self, name:&'static str, cmd: StomachFun<ET>) -> usize;
    fn register_whatsit(&mut self,name:&'static str,cmd:WhatsitFun<ET>) -> usize;
    fn register_open_box(&mut self,name:&'static str,cmd:BoxFun<ET>) -> usize;
    fn command(&self,index:usize) -> Option<StomachFun<ET>>;
    fn command_from_name(&self,name:&'static str) -> Option<StomachFun<ET>>;

    fn get_whatsit_cmd(&self,index:usize) -> Option<WhatsitFun<ET>>;
    fn get_box_cmd(&self,index:usize) -> Option<BoxFun<ET>>;

    fn digest(&mut self,state:&mut ET::State, gullet:&mut ET::Gullet, cmd:StomachCommand<ET::Token>) -> Result<(),Box<dyn TeXError<ET::Token>>>;

    fn maybe_shipout(&mut self,state:&mut ET::State,force:bool) -> Option<ET::Node> {
        if state.box_stack().len() > 1 { return None }
        match state.box_stack_mut().first_mut() {
            None => None,
            Some(OpenBox::Top { list }) if !list.is_empty() => match list.remove(0) {
                StomachNode::Node(b) => Some(b),
                StomachNode::Whatsit(_) => {
                    todo!("Handle whatsit");
                    self.maybe_shipout(state,force)
                }
                _ => todo!()
            },
            _ => None
        }
    }

    fn next_shipout_box(&mut self, state:&mut ET::State, gullet:&mut ET::Gullet) -> Result<Option<ET::Node>,Box<dyn TeXError<ET::Token>>> {
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
    commands:Map<fn(&mut S,&mut S::FS,&mut Gu,&mut Self,StomachCommand<T>,bool) -> Result<(),ErrorInPrimitive<T>>>,
    //whatsit_cmds:Map<fn(&mut S,&mut Gu,&mut Self,StomachCommand<T>) -> Result<Whatsit<T,Self>,ErrorInPrimitive<T>>>,
    phantom_box:std::marker::PhantomData<B>
}

 */

pub struct NoShipoutDefaultStomach<ET:EngineType>{
    commands:Map<StomachFun<ET>>,
    whatsit_cmds:Map<WhatsitFun<ET>>,
    box_cmds:Map<BoxFun<ET>>,
}

impl<ET:EngineType<Stomach=Self>> NoShipoutDefaultStomach<ET> {
    pub fn new() -> Self { Self {
        commands:Map::default(),
        whatsit_cmds:Map::default(),
        box_cmds:Map::default(),
    } }
}
impl<ET:EngineType<Stomach=Self>> Stomach<ET> for NoShipoutDefaultStomach<ET> {
    fn register_primitive(&mut self, name: &'static str, cmd: StomachFun<ET>) -> usize {
        self.commands.insert(name,cmd)
    }
    fn register_whatsit(&mut self, name: &'static str, cmd: WhatsitFun<ET>) -> usize {
        self.whatsit_cmds.insert(name,cmd)
    }
    fn register_open_box(&mut self, name: &'static str, cmd: BoxFun<ET>) -> usize {
        self.box_cmds.insert(name,cmd)
    }
    fn get_box_cmd(&self, index: usize) -> Option<BoxFun<ET>> {
        self.box_cmds.get(index).copied()
    }
    fn get_whatsit_cmd(&self, index: usize) -> Option<WhatsitFun<ET>> {
        self.whatsit_cmds.get(index).copied()
    }
    fn command_from_name(&self, name: &'static str) -> Option<StomachFun<ET>> {
        self.commands.get_from_name(name).copied()
    }
    fn command(&self, index: usize) -> Option<StomachFun<ET>> {
        self.commands.get(index).copied()
    }

    fn digest(&mut self,state:&mut ET::State, gullet:&mut ET::Gullet, cmd:StomachCommand<ET::Token>) -> Result<(),Box<dyn TeXError<ET::Token>>> {
        methods::digest::<ET>(self,state,gullet,cmd)
    }
}