pub mod methods;

use crate::debug_log;
use crate::engine::{EngineRef, EngineType};
use crate::engine::state::State;
use crate::tex::nodes::{OpenBox, HVBox, TeXNode, SimpleNode};
use crate::tex::commands::StomachCommand;
use crate::tex::fonts::{Font, FontStore};
use crate::tex::numbers::{Dim, Int, Skip};
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
    fn digest(engine:&mut EngineRef<ET>, cmd:StomachCommand<ET>);
    fn split_paragraph(fs:&ET::FontStore,state:&ET::State, nodes:Vec<TeXNode<ET>>, linespecs: Vec<LineSpec<ET>>) -> Vec<Vec<TeXNode<ET>>>;
    fn split_vertical(fs:&ET::FontStore,state:&ET::State, nodes:Vec<TeXNode<ET>>, target:ET::Dim) -> (Vec<TeXNode<ET>>,Vec<TeXNode<ET>>);

    fn shipout(&mut self,bx:HVBox<ET>) {
        /* INSERTS:
        1. read vbox => \box n = b

        g = \pagegoal
        t = \pagetotal
        q = \insertpenalties (accumulated for the current page)
        d = \pagedepth (<= \maxdepth)
        z = \pageshrink
        x = b.height() + b.depth()
        f = \count n / 1000

        if (\box n is empty) {
            g -= h*f + w
        } else {}
    */
        todo!("shipout")
    }

    fn open_box(&mut self,bx:OpenBox<ET>) {
        if !bx.is_vertical() {
            self.shipout_data_mut().spacefactor = 1000;
        }
        self.shipout_data_mut().box_stack.push(bx)
    }

    fn open_paragraph(engine:&mut EngineRef<ET>, cmd:StomachCommand<ET>) {
        methods::open_paragraph(engine,cmd)
    }
    fn close_paragraph(engine: &mut EngineRef<ET>) {
        methods::close_paragraph(engine)
    }

    fn push_node(&mut self,fs:&ET::FontStore,state:&ET::State,node:TeXNode<ET>) {
        let sd = self.shipout_data_mut();
        match node {
            TeXNode::Simple(SimpleNode::Char {char,..}) => {
                let sf = state.get_sfcode(char).to_i64();
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
                        o => sd.prevdepth = o.depth(fs)
                    }
                } else {
                    match (b.ls_mut().last_mut(),&node) {
                        (Some(TeXNode::Simple(SimpleNode::Char {char,font})),
                            TeXNode::Simple(SimpleNode::Char {char:char2,font:font2})) if *font == *font2 => {
                            if let Some(r) = fs.get(*font).ligature(*char,*char2) {
                                *char = r
                            } else {
                                b.ls_mut().push(node)
                            }
                        } // TODO no OpenKernel when group closes!
                        (Some(TeXNode::OpenKernel(k)),_) => {
                            match b.ls_mut().pop() {
                                Some(TeXNode::OpenKernel(k)) =>
                                    // TODO limits
                                    b.ls_mut().push(k.merge(node,false)),
                                _ => unreachable!()
                            };

                        }
                        _ => b.ls_mut().push(node)
                    }
                }
            },
            _ => {
                sd.pagetotal = sd.pagetotal + node.height(fs);
                match &node {
                    TeXNode::Simple(SimpleNode::Rule{..}) => sd.prevdepth = ET::Dim::from_sp(-65536000),
                    o => sd.prevdepth = o.depth(fs)
                }
                sd.page.push(node)
            }
        }
    }

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
    fn split_paragraph(fs:&ET::FontStore,state: &ET::State, nodes: Vec<TeXNode<ET>>, linespecs: Vec<LineSpec<ET>>) -> Vec<Vec<TeXNode<ET>>> {
        methods::split_paragraph_roughly(fs,nodes,linespecs)
    }
    fn split_vertical(fs:&ET::FontStore,state: &ET::State, nodes: Vec<TeXNode<ET>>, target: ET::Dim) -> (Vec<TeXNode<ET>>, Vec<TeXNode<ET>>) {
        methods::split_vertical_roughly(fs,state,nodes,target)
    }
}

#[derive(Clone,Copy,Debug)]
pub struct LineSpec<ET:EngineType> {
    left_skip:Skip<ET::SkipDim>,
    right_skip:Skip<ET::SkipDim>,
    target:ET::Dim,
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
    fn close_paragraph(engine: &mut EngineRef<ET>) {
        todo!()
    }
    fn split_paragraph(fs:&ET::FontStore,state: &ET::State, nodes: Vec<TeXNode<ET>>, linespecs: Vec<LineSpec<ET>>) -> Vec<Vec<TeXNode<ET>>> {
        methods::split_paragraph_roughly(fs,nodes,linespecs)
    }
    fn split_vertical(fs:&ET::FontStore,state: &ET::State, nodes: Vec<TeXNode<ET>>, target: ET::Dim) -> (Vec<TeXNode<ET>>, Vec<TeXNode<ET>>) {
        methods::split_vertical_roughly(fs,state,nodes,target)
    }
}