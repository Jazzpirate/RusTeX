pub mod methods;

use std::marker::PhantomData;
use crate::debug_log;
use crate::engine::{EngineRef, EngineType};
use crate::engine::gullet::Gullet;
use crate::engine::state::State;
use crate::tex::nodes::{OpenBox, CustomNode, Whatsit, HVBox, TeXNode, SimpleNode};
use crate::tex::commands::{CommandSource, StomachCommand};
use crate::tex::token::Token;
use crate::tex::numbers::Dim;
use crate::utils::errors::TeXError;
use crate::utils::map::Map;


#[derive(Clone,Debug)]
pub struct ShipoutData<ET:EngineType> {
    pub box_stack:Vec<OpenBox<ET>>,
    pub page:Vec<TeXNode<ET>>,
    pub pagegoal:ET::Dim,
    pub pagetotal:ET::Dim,
    pub prevdepth: ET::Dim
}
impl<ET:EngineType> ShipoutData<ET> {
    pub fn new() -> Self {
        ShipoutData {
            box_stack:Vec::with_capacity(1024),
            page:Vec::new(),
            pagegoal:ET::Dim::from_sp(i32::MAX as i64),
            pagetotal:ET::Dim::from_sp(0),
            prevdepth:ET::Dim::from_sp(-65536000)
        }
    }
    pub fn get_page(&mut self) -> Vec<TeXNode<ET>> {
        let ret = std::mem::replace(&mut self.page,Vec::with_capacity(1024));
        // TODO moar
        self.pagetotal = ET::Dim::from_sp(0);
        self.pagegoal = ET::Dim::from_sp(i32::MAX as i64);
        ret
    }
}

pub trait Stomach<ET:EngineType<Stomach=Self>>:Sized + Clone+'static {
    fn shipout_data(&self) -> &ShipoutData<ET>;
    fn shipout_data_mut(&mut self) -> &mut ShipoutData<ET>;
    fn open_box(&mut self,bx:OpenBox<ET>) {
        self.shipout_data_mut().box_stack.push(bx)
    }

    fn shipout(&mut self,bx:HVBox<ET>) {
        todo!("shipout")
    }

    fn push_node(&mut self,node:TeXNode<ET>) {
        let mut sd = self.shipout_data_mut();
        match sd.box_stack.last_mut() {
            Some(b) => {
                if b.is_vertical() {
                    match &node {
                        TeXNode::Simple(SimpleNode::Rule{..}) => sd.prevdepth = ET::Dim::from_sp(-65536000),
                        o => sd.prevdepth = o.depth()
                    }
                }
                b.ls().push(node)
            },
            _ => {
                sd.pagetotal = sd.pagetotal + node.height();
                match &node {
                    TeXNode::Simple(SimpleNode::Rule{..}) => sd.prevdepth = ET::Dim::from_sp(-65536000),
                    o => sd.prevdepth = o.depth()
                }
                sd.page.push(node)
            }
        }
    }

    fn digest(engine:&mut EngineRef<ET>, cmd:StomachCommand<ET>) -> Result<(),TeXError<ET>>;

    fn maybe_shipout(engine:&mut EngineRef<ET>, force:bool) -> Option<ET::Node> {
        let sd = engine.stomach.shipout_data();
        if force || sd.pagetotal >= sd.pagegoal {
            if sd.page.is_empty() { return None }
            todo!("shipout: {}>={}\nPage: {:?}",sd.pagetotal,sd.pagegoal,sd.page)
        } else { None }
    }

    fn next_shipout_box(engine:&mut EngineRef<ET>) -> Result<Option<ET::Node>,TeXError<ET>> {
        loop {
            match Self::maybe_shipout(engine,false) {
                Some(b) => {
                    debug_log!(trace=>"Shipout box");
                    return Ok(Some(b))
                },
                None => {
                    match engine.get_next_stomach_command()? {
                        Some(cmd) => {
                            Self::digest(engine,cmd)?
                        }
                        None => {
                            debug_log!(trace=>"No more commands; force shipout box");
                            return Ok(Self::maybe_shipout(engine,true))
                        }
                    }
                }
            }
        }
    }
}

#[derive(Clone)]
pub struct ShipoutDefaultStomach<ET:EngineType>{
    shipout_data:ShipoutData<ET>
}

impl<ET:EngineType<Stomach=Self>> ShipoutDefaultStomach<ET> {
    pub fn new() -> Self { Self{
        shipout_data:ShipoutData::new(),
    } }
}
impl<ET:EngineType<Stomach=Self>> Stomach<ET> for ShipoutDefaultStomach<ET> {
    fn digest(engine:&mut EngineRef<ET>, cmd:StomachCommand<ET>) -> Result<(),TeXError<ET>> {
        methods::digest::<ET>(engine,cmd)
    }
    fn shipout_data(&self) -> &ShipoutData<ET> { &self.shipout_data }
    fn shipout_data_mut(&mut self) -> &mut ShipoutData<ET> { &mut self.shipout_data }
}


#[derive(Clone)]
pub struct NoShipoutDefaultStomach<ET:EngineType> {
    shipout_data:ShipoutData<ET>
}

impl<ET:EngineType<Stomach=Self>> NoShipoutDefaultStomach<ET> {
    pub fn new() -> Self { Self{
    shipout_data:ShipoutData::new(),
} }
}
impl<ET:EngineType<Stomach=Self>> Stomach<ET> for NoShipoutDefaultStomach<ET> {
    fn digest(engine:&mut EngineRef<ET>, cmd:StomachCommand<ET>) -> Result<(),TeXError<ET>> {
        methods::digest::<ET>(engine,cmd)
    }
    fn shipout_data(&self) -> &ShipoutData<ET> { &self.shipout_data }
    fn shipout_data_mut(&mut self) -> &mut ShipoutData<ET> { &mut self.shipout_data }
}