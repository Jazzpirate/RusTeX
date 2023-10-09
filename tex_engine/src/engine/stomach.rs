pub mod methods;

use std::default;
use log::debug;
use crate::debug_log;
use crate::engine::{EngineRef, EngineType};
use crate::engine::mouth::Mouth;
use crate::engine::state::modes::TeXMode;
use crate::engine::state::State;
use crate::tex::nodes::{OpenBox, HVBox, TeXNode, SimpleNode, VBox, ReverseNodeIterator};
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
    pub pagestretch:ET::Dim,
    pub pagefilstretch:ET::Dim,
    pub pagefillstretch:ET::Dim,
    pub pagefilllstretch:ET::Dim,
    pub pageshrink:ET::Dim,
    pub pagedepth:ET::Dim,
    pub prevdepth: ET::Dim,
    pub spacefactor: i32,
    pub to_ship:Option<TeXNode<ET>>,
    pub topmarks:HMap<usize,Vec<ET::Token>>,
    pub firstmarks:HMap<usize,Vec<ET::Token>>,
    pub botmarks:HMap<usize,Vec<ET::Token>>,
    pub splitfirstmarks:HMap<usize,Vec<ET::Token>>,
    pub splitbotmarks:HMap<usize,Vec<ET::Token>>
}
impl<ET:EngineType> ShipoutData<ET> {
    pub fn new() -> Self {
        ShipoutData {
            box_stack:Vec::with_capacity(1024),
            page:Vec::new(),
            pagegoal:ET::Dim::from_sp(i32::MAX as i64),
            pagetotal:ET::Dim::from_sp(0),
            prevdepth:ET::Dim::from_sp(-65536000),
            pagestretch:ET::Dim::from_sp(0),
            pagefilstretch:ET::Dim::from_sp(0),
            pagefillstretch:ET::Dim::from_sp(0),
            pagefilllstretch:ET::Dim::from_sp(0),
            pageshrink:ET::Dim::from_sp(0),
            pagedepth:ET::Dim::from_sp(0),
            spacefactor:1000,
            to_ship:None,
            topmarks:HMap::default(),
            firstmarks:HMap::default(),
            botmarks:HMap::default(),
            splitfirstmarks:HMap::default(),
            splitbotmarks:HMap::default(),
        }
    }
    pub fn get_page(&mut self) -> Vec<TeXNode<ET>> {
        let ret = std::mem::replace(&mut self.page,Vec::with_capacity(1024));
        // TODO moar
        self.pagetotal = ET::Dim::from_sp(0);
        self.pagegoal = ET::Dim::from_sp(i32::MAX as i64);
        self.pagestretch = ET::Dim::from_sp(0);
        self.pagefilstretch = ET::Dim::from_sp(0);
        self.pagefillstretch = ET::Dim::from_sp(0);
        self.pagefilllstretch = ET::Dim::from_sp(0);
        self.pageshrink = ET::Dim::from_sp(0);
        self.pagedepth = ET::Dim::from_sp(0);
        ret
    }
}
use crate::tex::nodes::NodeTrait;
use crate::utils::collections::HMap;

pub trait Stomach<ET:EngineType<Stomach=Self>>:Sized + Clone+'static {
    fn shipout_data(&self) -> &ShipoutData<ET>;
    fn shipout_data_mut(&mut self) -> &mut ShipoutData<ET>;
    fn digest(engine:&mut EngineRef<ET>, cmd:StomachCommand<ET>);
    fn split_paragraph(engine:&EngineRef<ET>, nodes:Vec<TeXNode<ET>>, linespecs: Vec<LineSpec<ET>>) -> Vec<ParLine<ET>>;
    fn split_vertical(engine:&mut EngineRef<ET>, nodes: Vec<TeXNode<ET>>, target: ET::Dim) -> (Vec<TeXNode<ET>>, Vec<TeXNode<ET>>,i32);

    fn shipout(&mut self,bx:HVBox<ET>) {
        self.shipout_data_mut().to_ship = Some(TeXNode::Box(bx).into())
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

    fn push_node(engine:&mut EngineRef<ET>,node:TeXNode<ET>) {
        let sd = engine.stomach.shipout_data_mut();
        match node {
            TeXNode::Simple(SimpleNode::Char {char,..}) => {
                let sf = engine.state.get_sfcode(char).to_i64();
                if sf > 1000 && sd.spacefactor < 1000 {
                    sd.spacefactor = 1000;
                } else {
                    sd.spacefactor = sf as i32;
                }
            }
            TeXNode::Penalty(i) if i <= -10000 && sd.box_stack.is_empty() => {
                debug_log!(trace=>"Forced shipout");
                sd.page.push(node);
                return Self::maybe_shipout(engine,true);
            }
            _ => sd.spacefactor = 1000
        }
        match sd.box_stack.last_mut() {
            Some(b) => {
                if b.is_vertical() {
                    match &node {
                        TeXNode::Simple(SimpleNode::Rule{..}) => sd.prevdepth = ET::Dim::from_sp(-65536000),
                        o => sd.prevdepth = o.depth(&engine.fontstore)
                    }
                } else {
                    match (b.ls_mut().last_mut(),&node) {
                        (Some(TeXNode::Simple(SimpleNode::Char {char,font,cls:None})),
                            TeXNode::Simple(SimpleNode::Char {char:char2,font:font2,cls:None})) if *font == *font2 => {
                            if let Some(r) = engine.fontstore.get(*font).ligature(*char,*char2) {
                                *char = r
                            } else {
                                b.ls_mut().push(node)
                            }
                        }
                        /*(Some(TeXNode::OpenKernel(k)),_) => {
                            match b.ls_mut().pop() {
                                Some(TeXNode::OpenKernel(k)) =>
                                    // TODO limits
                                    b.ls_mut().push(k.merge(node,false)),
                                _ => unreachable!()
                            };
                        }*/
                        _ => b.ls_mut().push(node)
                    }
                }
            },
            _ => {
                debug!("page total {} += {} (goal {})",sd.pagetotal,node.height(&engine.fontstore),sd.pagegoal);
                if sd.pagegoal == ET::Dim::from_sp(i32::MAX as i64) {
                    match node {
                        TeXNode::Box(_)|TeXNode::Insert(..) => {
                            sd.pagegoal = engine.state.get_primitive_dim("vsize");
                            debug!("=> new goal {}",sd.pagegoal);
                        }
                        _ => ()
                    }
                }
                sd.pagetotal = sd.pagetotal + node.height(&engine.fontstore);
                match &node {
                    TeXNode::Simple(SimpleNode::Rule{..}) => sd.prevdepth = ET::Dim::from_sp(-65536000),
                    o => sd.prevdepth = o.depth(&engine.fontstore)
                }
                sd.page.push(node);
                ET::Stomach::maybe_shipout(engine,false);
            }
        }
    }

    fn maybe_shipout(engine:&mut EngineRef<ET>, force:bool) {
        if !force {
            debug!("Shipout? {} >= {}",engine.stomach.shipout_data().pagetotal,engine.stomach.shipout_data().pagegoal);
        }
        if (force || (  engine.stomach.shipout_data().box_stack.is_empty()  && engine.stomach.shipout_data().pagetotal >= engine.stomach.shipout_data().pagegoal)) && !engine.stomach.shipout_data().page.is_empty() {
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
            debug_log!(debug=>"SHIPOUT");
            let pagegoal = engine.stomach.shipout_data().pagegoal; // TODO check what's going on here
            let page = engine.stomach.shipout_data_mut().get_page();
            // TODO more precisely
            engine.state.set_primitive_int("badness",ET::Int::from_i64(10),true);



            let (mut first,rest,penalty) = if (force) {
                (page,vec!(),None)
            } else {
                let (f,r,p) = ET::Stomach::split_vertical(engine,page,pagegoal);
                (f,r,Some(p))
            };
            let mut deletes = vec!();
            for (i,b) in first.iter_mut().enumerate() { match b {
                TeXNode::Insert(n,v) => {
                    engine.state.set_box_register(*n, HVBox::V(VBox { children: std::mem::take(v), kind: "insert", ..Default::default() }), false);
                    deletes.push(i);
                }
                _ => ()
            }}
            for i in deletes {
                first.remove(i);
            }
            if let Some(p) = penalty {
                engine.state.set_primitive_int("outputpenalty",ET::Int::from_i64(p as i64),true);
            }
/*
            std::env::set_var("RUST_LOG","debug,tex_engine::tex::commands=trace,tex_engine::engine::gullet=trace,,tex_engine::engine::state=trace");
            env_logger::try_init();
*/

            engine.state.set_box_register(255,HVBox::V(VBox {
                kind:"page",
                children:first,
                ..Default::default()
            }),false);

            for r in rest { Self::push_node(engine,r) }
            let mode = engine.state.mode();

            engine.with_mouth(vec!(),|engine| {
                engine.mouth.insert_every(&engine.state,"output");
                engine.state.set_mode(TeXMode::InternalVertical);
                loop {
                    match engine.get_next_stomach_command() {
                        None => break,
                        Some(cmd) => Self::digest(engine,cmd)
                    }
                }
            });
            engine.state.set_mode(mode);
        }
    }

    fn next_shipout_box(engine:&mut EngineRef<ET>) -> Option<TeXNode<ET>> {
        loop {
            match &mut engine.stomach.shipout_data_mut().to_ship {
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
                s@Some(_) => return std::mem::take(s)
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

pub enum ParLine<ET:EngineType> {
    Line(Vec<TeXNode<ET>>),
    Between(TeXNode<ET>)
}

impl<ET:EngineType<Stomach=Self>> Stomach<ET> for ShipoutDefaultStomach<ET> {
    fn digest(engine:&mut EngineRef<ET>, cmd:StomachCommand<ET>) {
        methods::digest::<ET>(engine,cmd)
    }
    fn shipout_data(&self) -> &ShipoutData<ET> { &self.shipout_data }
    fn shipout_data_mut(&mut self) -> &mut ShipoutData<ET> { &mut self.shipout_data }
    fn split_paragraph(engine:&EngineRef<ET>, nodes: Vec<TeXNode<ET>>, linespecs: Vec<LineSpec<ET>>) -> Vec<ParLine<ET>> {
        methods::split_paragraph_roughly(engine,nodes,linespecs)
    }
    fn split_vertical(engine:&mut EngineRef<ET>, nodes: Vec<TeXNode<ET>>, target: ET::Dim) -> (Vec<TeXNode<ET>>, Vec<TeXNode<ET>>,i32) {
        methods::split_vertical_roughly(engine,nodes,target)
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
    fn split_paragraph(engine:&EngineRef<ET>, nodes: Vec<TeXNode<ET>>, linespecs: Vec<LineSpec<ET>>) -> Vec<ParLine<ET>> {
        methods::split_paragraph_roughly(engine,nodes,linespecs)
    }
    fn split_vertical(engine:&mut EngineRef<ET>, nodes: Vec<TeXNode<ET>>, target: ET::Dim) -> (Vec<TeXNode<ET>>, Vec<TeXNode<ET>>,i32) {
        methods::split_vertical_roughly(engine,nodes,target)
    }
}