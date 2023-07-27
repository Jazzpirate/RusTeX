pub mod methods;

use crate::debug_log;
use crate::engine::gullet::Gullet;
use crate::engine::state::State;
use crate::tex::boxes::{BoxOrWhatsit, OpenBox, TeXBox, Whatsit};
use crate::tex::commands::{StomachCommand,StomachCommandInner};
use crate::tex::token::Token;
use crate::utils::errors::{ErrorInPrimitive, TeXError};
use crate::utils::map::Map;

pub trait Stomach<T:Token>:Sized+'static {
    type S:State<T>;
    type Gu:Gullet<T,S=Self::S>;
    type B:TeXBox;
    fn register_primitive(&mut self,name:&'static str,cmd:fn(&mut Self::S,&mut Self::Gu,&mut Self,StomachCommand<T>,bool) -> Result<(),ErrorInPrimitive<T>>) -> usize;
    fn register_whatsit(&mut self,name:&'static str,cmd:fn(&mut Self::S,&mut Self::Gu,&mut Self,StomachCommand<T>) -> Result<Whatsit<T,Self>,ErrorInPrimitive<T>>) -> usize;
    fn stack(&self) -> &Vec<OpenBox<T,Self,Self::B>>;
    fn stack_mut(&mut self) -> &mut Vec<OpenBox<T,Self,Self::B>>;
    fn command(&self,index:usize) -> Option<fn(&mut Self::S,&mut Self::Gu,&mut Self,StomachCommand<T>,bool) -> Result<(),ErrorInPrimitive<T>>>;
    fn command_from_name(&self,name:&'static str) -> Option<fn(&mut Self::S,&mut Self::Gu,&mut Self,StomachCommand<T>,bool) -> Result<(),ErrorInPrimitive<T>>>;

    fn get_whatsit_cmd(&self,index:usize) -> Option<fn(&mut Self::S,&mut Self::Gu,&mut Self,StomachCommand<T>) -> Result<Whatsit<T,Self>,ErrorInPrimitive<T>>>;

    fn digest(&mut self,state:&mut Self::S, gullet:&mut Self::Gu, cmd:StomachCommand<T>) -> Result<(),Box<dyn TeXError<T>>>;

    fn maybe_shipout(&mut self,force:bool) -> Option<Self::B> {
        if self.stack().len() > 1 { return None }
        match self.stack_mut().first_mut() {
            None => None,
            Some(OpenBox::Top { list }) if !list.is_empty() => match list.remove(0) {
                BoxOrWhatsit::Box(b) => Some(b),
                BoxOrWhatsit::Whatsit(_) => {
                    todo!("Handle whatsit");
                    self.maybe_shipout(force)
                }
            },
            _ => None
        }
    }

    fn next_shipout_box(&mut self, state:&mut Self::S, gullet:&mut Self::Gu) -> Result<Option<Self::B>,Box<dyn TeXError<T>>> {
        loop {
            match self.maybe_shipout(false) {
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
                            return Ok(self.maybe_shipout(true))
                        }
                    }
                }
            }
        }
    }
}

// TODO
pub struct ShipoutDefaultStomach<T:Token,S:State<T>,Gu:Gullet<T>,B:TeXBox>{
    commands:Map<fn(&mut S,&mut S::FS,&mut Gu,&mut Self,StomachCommand<T>,bool) -> Result<(),ErrorInPrimitive<T>>>,
    phantom_box:std::marker::PhantomData<B>
}

pub struct NoShipoutDefaultStomach<T:Token,S:State<T>,Gu:Gullet<T,S=S>,B:TeXBox>{
    commands:Map<fn(&mut S,&mut Gu,&mut Self,StomachCommand<T>,bool) -> Result<(),ErrorInPrimitive<T>>>,
    whatsit_cmds:Map<fn(&mut S,&mut Gu,&mut Self,StomachCommand<T>) -> Result<Whatsit<T,Self>,ErrorInPrimitive<T>>>,
    stack:Vec<OpenBox<T,Self,B>>,
}

impl<T:Token,S:State<T>,Gu:Gullet<T,S=S>,B:TeXBox> NoShipoutDefaultStomach<T,S,Gu,B> {
    pub fn new() -> Self { Self {
        commands:Map::default(),
        whatsit_cmds:Map::default(),
        stack:Vec::new(),
    } }
}
impl<T:Token,S:State<T>,Gu:Gullet<T,S=S>,B:TeXBox> Stomach<T> for NoShipoutDefaultStomach<T,S,Gu,B> {
    type S=S;
    type Gu=Gu;
    type B = B;
    fn register_primitive(&mut self, name: &'static str, cmd: fn(&mut Self::S, &mut Self::Gu, &mut Self, StomachCommand<T>,bool) -> Result<(),ErrorInPrimitive<T>>) -> usize {
        self.commands.insert(name,cmd)
    }
    fn register_whatsit(&mut self, name: &'static str, cmd: fn(&mut Self::S, &mut Self::Gu, &mut Self, StomachCommand<T>) -> Result<Whatsit<T, Self>, ErrorInPrimitive<T>>) -> usize {
        self.whatsit_cmds.insert(name,cmd)
    }
    fn get_whatsit_cmd(&self, index: usize) -> Option<fn(&mut Self::S, &mut Self::Gu, &mut Self, StomachCommand<T>) -> Result<Whatsit<T, Self>, ErrorInPrimitive<T>>> {
        self.whatsit_cmds.get(index).copied()
    }
    fn command_from_name(&self, name: &'static str) -> Option<fn(&mut Self::S, &mut Self::Gu, &mut Self, StomachCommand<T>,bool) -> Result<(),ErrorInPrimitive<T>>> {
        self.commands.get_from_name(name).copied()
    }
    fn command(&self, index: usize) -> Option<fn(&mut Self::S, &mut Self::Gu, &mut Self, StomachCommand<T>, bool) -> Result<(), ErrorInPrimitive<T>>> {
        self.commands.get(index).copied()
    }
    fn stack(&self) -> &Vec<OpenBox<T,Self,Self::B>> { &self.stack }
    fn stack_mut(&mut self) -> &mut Vec<OpenBox<T,Self,Self::B>> { &mut self.stack }

    fn digest(&mut self,state:&mut Self::S, gullet:&mut Self::Gu, cmd:StomachCommand<T>) -> Result<(),Box<dyn TeXError<T>>> {
        methods::digest(self,state,gullet,cmd)
    }
}