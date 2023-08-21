pub mod methods;

use crate::debug_log;
use crate::engine::{EngineRef, EngineType};
use crate::engine::state::State;
use crate::tex::nodes::{OpenBox, HVBox, TeXNode, SimpleNode};
use crate::tex::commands::StomachCommand;
use crate::tex::numbers::{Dim, Int};
use crate::utils::errors::TeXError;


#[derive(Clone,Debug)]
pub struct ShipoutData<ET:EngineType> {
    pub box_stack:Vec<OpenBox<ET>>,
    pub page:Vec<TeXNode<ET>>,
    pub pagegoal:ET::Dim,
    pub pagetotal:ET::Dim,
    pub prevdepth: ET::Dim,
    pub spacefactor: i32,
    pub to_ship:Option<ET::Node>
}
impl<ET:EngineType> ShipoutData<ET> {
    pub fn new() -> Self {
        ShipoutData {
            box_stack:Vec::with_capacity(1024),
            page:Vec::new(),
            pagegoal:ET::Dim::from_sp(i32::MAX as i64),
            pagetotal:ET::Dim::from_sp(0),
            prevdepth:ET::Dim::from_sp(-65536000),
            spacefactor:1000,
            to_ship:None
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
use crate::tex::nodes::NodeTrait;
pub trait Stomach<ET:EngineType<Stomach=Self>>:Sized + Clone+'static {
    fn shipout_data(&self) -> &ShipoutData<ET>;
    fn shipout_data_mut(&mut self) -> &mut ShipoutData<ET>;
    fn open_box(&mut self,bx:OpenBox<ET>) {
        if !bx.is_vertical() {
            self.shipout_data_mut().spacefactor = 1000;
        }
        self.shipout_data_mut().box_stack.push(bx)
    }

    fn shipout(&mut self,bx:HVBox<ET>) {
        todo!("shipout")
    }

    fn push_node(&mut self,state:&ET::State,node:TeXNode<ET>) {
        let sd = self.shipout_data_mut();
        match node {
            TeXNode::Simple(SimpleNode::Char {char,..}) => {
                let sf = state.get_sfcode(&char).to_i64();
                if sf > 1000 && sd.spacefactor < 1000 {
                    sd.spacefactor = 1000;
                } else {
                    sd.spacefactor = sf as i32;
                }
            }
            _ => sd.spacefactor = 1000
        }
        match sd.box_stack.last_mut() {
            Some(b) => {
                if b.is_vertical() {
                    match &node {
                        TeXNode::Simple(SimpleNode::Rule{..}) => sd.prevdepth = ET::Dim::from_sp(-65536000),
                        o => sd.prevdepth = o.depth()
                    }
                }
                b.ls_mut().push(node)
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

    fn digest(engine:&mut EngineRef<ET>, cmd:StomachCommand<ET>);

    fn maybe_shipout(engine:&mut EngineRef<ET>, force:bool) {
        let sd = engine.stomach.shipout_data();
        if (force || (  sd.box_stack.is_empty()  &&sd.pagetotal >= sd.pagegoal)) && !sd.page.is_empty() {
            todo!("shipout: {}>={}\nPage: {:?}",sd.pagetotal,sd.pagegoal,sd.page)
        }
    }

    fn next_shipout_box(engine:&mut EngineRef<ET>) -> Option<ET::Node> {
        loop {
            match engine.stomach.shipout_data().to_ship {
                None =>
                    match engine.get_next_stomach_command() {
                        Some(cmd) => {
                            Self::digest(engine,cmd)
                        }
                        None => {
                            debug_log!(trace=>"No more commands; force shipout box");
                            Self::maybe_shipout(engine,true);
                            return std::mem::take(&mut engine.stomach.shipout_data_mut().to_ship)
                        }
                    }
                Some(_) => todo!()
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
    fn digest(engine:&mut EngineRef<ET>, cmd:StomachCommand<ET>) {
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
    fn digest(engine:&mut EngineRef<ET>, cmd:StomachCommand<ET>) {
        methods::digest::<ET>(engine,cmd)
    }
    fn shipout_data(&self) -> &ShipoutData<ET> { &self.shipout_data }
    fn shipout_data_mut(&mut self) -> &mut ShipoutData<ET> { &mut self.shipout_data }
}