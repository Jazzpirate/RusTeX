use crate::commands::NodeCommandScope;
use crate::engine::{EngineAux, EngineReferences, EngineTypes};
use crate::engine::fontsystem::FontSystem;
use crate::engine::gullet::ResolvedToken;
use crate::engine::mouth::Mouth;
use crate::engine::mouth::pretokenized::TokenList;
use crate::engine::state::State;
use crate::engine::utils::memory::{MemoryManager, PrimitiveIdentifier, PRIMITIVES};
use crate::tex::catcodes::CommandCode;
use crate::tex::nodes::{BoxInfo, NodeList, NodeListType, SimpleNode, TeXBox, TeXNode};
use crate::tex::numerics::NumSet;
use crate::tex::types::{BoxType, GroupType, TeXMode};
use crate::utils::Ptr;
use crate::tex::numerics::TeXDimen;
use crate::tex::nodes::NodeTrait;
use crate::tex::token::Token;
use crate::commands::Command;
use crate::utils::errors::ErrorHandler;

type Tk<S> = <<S as Stomach>::ET as EngineTypes>::Token;
type Ch<S> = <<S as Stomach>::ET as EngineTypes>::Char;
type Int<E> = <<E as EngineTypes>::Num as NumSet>::Int;
type Fnt<E> = <<E as EngineTypes>::FontSystem as FontSystem>::Font;

fn afterassignment<ET:EngineTypes>(engine:&mut EngineReferences<ET>) {
    match std::mem::take(engine.stomach.afterassignment()) {
        Some(t) => engine.requeue(t),
        _ => ()
    }
}

pub trait Stomach {
    type ET:EngineTypes<Stomach = Self>;
    fn afterassignment(&mut self) -> &mut Option<Tk<Self>>;
    fn data_mut(&mut self) -> &mut StomachData<Self::ET>;

    #[inline(always)]
    fn do_unexpandable(
        engine:&mut EngineReferences<Self::ET>,
        name:PrimitiveIdentifier,
        token:Tk<Self>,
        apply:fn(&mut EngineReferences<Self::ET>,Tk<Self>)
    ) {
        engine.trace_command(|engine| PRIMITIVES.printable(name,engine.state.get_escape_char()));
        apply(engine,token)
    }

    fn do_assignment(
        engine:&mut EngineReferences<Self::ET>,
        name:PrimitiveIdentifier,
        token:Tk<Self>,
        assign:fn(&mut EngineReferences<Self::ET>,Tk<Self>,bool),
        global:bool
    ) {
        engine.trace_command(|engine| PRIMITIVES.printable(name,engine.state.get_escape_char()));
        assign(engine,token,global);
        afterassignment(engine);
    }
    fn end_box(engine:&mut EngineReferences<Self::ET>,bt:BoxType) {
        match engine.stomach.data_mut().open_lists.pop() {
            Some(ls) => match ls.tp {
                NodeListType::Paragraph => {
                    todo!("close paragraph")
                }
                NodeListType::Box(bi,start,reg) if bi.tp == bt => {
                    engine.state.pop(engine.aux,engine.mouth);
                    let bx = TeXBox {
                        children:ls.children,info:bi,start,end:engine.mouth.current_sourceref(),
                    };
                    match reg {
                        Some((reg,global)) =>
                            engine.state.set_box_register(engine.aux,reg,Some(bx),global),
                        None =>
                            Self::add_node(engine,bx.as_node())
                    }
                }
                _ => todo!("throw error")
            }
            None => todo!("throw error"),
        }
    }
    fn do_char(engine:&mut EngineReferences<Self::ET>,token:Tk<Self>,char:Ch<Self>,code:CommandCode) {
        match code {
            CommandCode::EOF => (),
            CommandCode::Space if engine.state.get_mode().is_vertical() => (),
            CommandCode::BeginGroup =>
                engine.state.push(engine.aux,GroupType::Character,engine.mouth.line_number()),
            CommandCode::EndGroup => {
                match engine.state.get_group_type() {
                    Some(GroupType::Character) =>
                        engine.state.pop(engine.aux,engine.mouth),
                    Some(GroupType::Box(bt)) =>
                    Self::end_box(engine,bt),
                    _ => todo!("throw error / close box")
                }
            }
            _ => todo!("{} > {:?}",char,code)
            // todo!("check ligatures, do spacefactor")
        }
    }
    fn do_box(engine:&mut EngineReferences<Self::ET>,name:PrimitiveIdentifier,token:Tk<Self>,bx:fn(&mut EngineReferences<Self::ET>,Tk<Self>) -> Result<Option<TeXBox<Self::ET>>,(BoxInfo<Self::ET>,Option<(u16,bool)>)>) {
        todo!("box in stomach")
    }
    fn add_node(engine:&mut EngineReferences<Self::ET>,node:TeXNode<Self::ET>) {
        let data = engine.stomach.data_mut();
        data.spacefactor = 1000;
        match data.open_lists.last_mut() {
            Some(NodeList{tp:NodeListType::Box(BoxInfo{tp:BoxType::Vertical,..},..),children}) => {
                match node {
                    TeXNode::Simple(SimpleNode::VRule {..}) =>
                        data.prevdepth = <<Self::ET as EngineTypes>::Dim as TeXDimen>::from_sp(-65536000),
                    _ => ()
                }
                children.push(node)
            }
            Some(ls) => {
                data.prevdepth = node.depth();
                ls.children.push(node)
            },
            None => {
                if data.pagegoal == <<Self::ET as EngineTypes>::Dim as TeXDimen>::from_sp(i32::MAX) {
                    match node {
                        TeXNode::Box(_) | TeXNode::Insert =>
                            data.pagegoal = engine.state.get_primitive_dim(PRIMITIVES.vsize),
                        _ => ()
                    }
                }
                data.pagetotal = data.pagetotal + node.height();// + node.depth() ?
                match node {
                    TeXNode::Simple(SimpleNode::VRule {..}) =>
                        data.prevdepth = <<Self::ET as EngineTypes>::Dim as TeXDimen>::from_sp(-65536000),
                    TeXNode::Penalty(i) if i <= -10000 => {
                        data.page.push(node);
                        return Self::maybe_shipout(engine,true)
                    }
                    _ => data.prevdepth = node.depth()
                }
                data.page.push(node);
                Self::maybe_shipout(engine,false)
            }
        }
    }
    fn maybe_switch_mode(engine:&mut EngineReferences<Self::ET>,scope:NodeCommandScope,token:Tk<Self>) -> bool {
        match (scope,engine.state.get_mode()) {
            (NodeCommandScope::Any, _) => true,
            (NodeCommandScope::SwitchesToHorizontal, TeXMode::Horizontal | TeXMode::RestrictedHorizontal) => true,
            (NodeCommandScope::SwitchesToVertical, TeXMode::Vertical | TeXMode::InternalVertical) => true,
            (NodeCommandScope::MathOnly, TeXMode::InlineMath | TeXMode::DisplayMath) => true,
            (NodeCommandScope::MathOnly, _) => todo!("throw error"),
            _ => todo!("switch modes maybe")
        }
    }
    fn do_node(engine:&mut EngineReferences<Self::ET>,name:PrimitiveIdentifier,token:Tk<Self>,read:fn(&mut EngineReferences<Self::ET>,Tk<Self>) -> TeXNode<Self::ET>,scope:NodeCommandScope) {
        engine.trace_command(|engine| PRIMITIVES.printable(name,engine.state.get_escape_char()));
        if Self::maybe_switch_mode(engine,scope,token.clone()) {
            let node = read(engine, token);
            Self::add_node(engine, node)
        }
    }
    fn do_font(engine:&mut EngineReferences<Self::ET>,token:Tk<Self>,f:Fnt<Self::ET>,global:bool) {
        engine.state.set_current_font(engine.aux,f,global);
        afterassignment(engine);
    }
    fn assign_int_register(engine:&mut EngineReferences<Self::ET>,register:u16,global:bool) {
        let val = engine.read_int(true);
        engine.state.set_int_register(engine.aux,register,val,global);
        afterassignment(engine);
    }
    fn assign_dim_register(engine:&mut EngineReferences<Self::ET>,register:u16,global:bool) {
        let val = engine.read_dim(true);
        engine.state.set_dim_register(engine.aux,register,val,global);
        afterassignment(engine);
    }
    fn assign_skip_register(engine:&mut EngineReferences<Self::ET>,register:u16,global:bool) {
        let val = engine.read_skip(true);
        engine.state.set_skip_register(engine.aux,register,val,global);
        afterassignment(engine);
    }
    fn assign_muskip_register(engine:&mut EngineReferences<Self::ET>,register:u16,global:bool) {
        let val = engine.read_muskip(true);
        engine.state.set_muskip_register(engine.aux,register,val,global);
        afterassignment(engine);
    }
    fn assign_toks_register(engine:&mut EngineReferences<Self::ET>,register:u16,global:bool) {
        let mut had_eq = false;
        crate::expand_loop!(Self::ET; engine,
            ResolvedToken::Tk{char,code,..} => match (char.try_into(),code) {
                (_,CommandCode::Space) => (),
                (Ok(b'='),CommandCode::Other) if !had_eq => {
                    if had_eq { todo!("throw error") }
                    had_eq = true;
                }
                (_,CommandCode::BeginGroup) => {
                    let mut tks = shared_vector::Vector::new();
                    engine.read_until_endgroup(|_,t| tks.push(t));
                    engine.state.set_toks_register(engine.aux,register,TokenList::from(tks),global);
                    afterassignment(engine);
                    return ()
                }
                _ => todo!("throw error")
            }
            _ => todo!("throw error")
        )
    }
    fn assign_primitive_int(engine:&mut EngineReferences<Self::ET>,name:PrimitiveIdentifier,global:bool) {
        engine.trace_command(|engine| format!("{}", PRIMITIVES.printable(name,engine.state.get_escape_char())));
        let val = engine.read_int(true);
        engine.state.set_primitive_int(engine.aux,name,val,global);
        afterassignment(engine);
    }
    fn assign_primitive_dim(engine:&mut EngineReferences<Self::ET>,name:PrimitiveIdentifier,global:bool) {
        engine.trace_command(|engine| format!("{}", PRIMITIVES.printable(name,engine.state.get_escape_char())));
        let val = engine.read_dim(true);
        engine.state.set_primitive_dim(engine.aux,name,val,global);
        afterassignment(engine);
    }
    fn assign_primitive_skip(engine:&mut EngineReferences<Self::ET>,name:PrimitiveIdentifier,global:bool) {
        engine.trace_command(|engine| format!("{}", PRIMITIVES.printable(name,engine.state.get_escape_char())));
        let val = engine.read_skip(true);
        engine.state.set_primitive_skip(engine.aux,name,val,global);
        afterassignment(engine);
    }
    fn assign_primitive_muskip(engine:&mut EngineReferences<Self::ET>,name:PrimitiveIdentifier,global:bool) {
        engine.trace_command(|engine| format!("{}", PRIMITIVES.printable(name,engine.state.get_escape_char())));
        let val = engine.read_muskip(true);
        engine.state.set_primitive_muskip(engine.aux,name,val,global);
        afterassignment(engine);
    }
    fn assign_primitive_toks(engine:&mut EngineReferences<Self::ET>,name:PrimitiveIdentifier,global:bool) {
        let mut had_eq = false;
        crate::expand_loop!(Self::ET; engine,
            ResolvedToken::Tk{char,code,..} => match (char.try_into(),code) {
                (_,CommandCode::Space) => (),
                (Ok(b'='),CommandCode::Other) if !had_eq => {
                    if had_eq { todo!("throw error") }
                    had_eq = true;
                }
                (_,CommandCode::BeginGroup) => {
                    let mut tks = shared_vector::Vector::new();
                    if name == PRIMITIVES.output {
                        tks.push(Tk::<Self>::from_char_cat(b'{'.into(),CommandCode::BeginGroup));
                        engine.read_until_endgroup(|_,t| tks.push(t));
                        tks.push(Tk::<Self>::from_char_cat(b'}'.into(),CommandCode::EndGroup));
                    } else {
                        engine.read_until_endgroup(|_,t| tks.push(t));
                    }
                    engine.state.set_primitive_tokens(engine.aux,name,TokenList::from(tks),global);
                    afterassignment(engine);
                    return ()
                }
                _ => todo!("throw error")
            }
            _ => todo!("throw error")
        )
    }
    fn do_whatsit(engine:&mut EngineReferences<Self::ET>,token:Tk<Self>,read:fn(&mut EngineReferences<Self::ET>,Tk<Self>)
                                                                                -> Ptr<dyn FnOnce(&mut EngineReferences<Self::ET>)>) {
        todo!()
    }

    fn maybe_shipout(engine:&mut EngineReferences<Self::ET>,force:bool) {
        if force || {
            let data = engine.stomach.data_mut();
            data.open_lists.is_empty() && data.pagetotal >= data.pagegoal && !data.page.is_empty()
        } {
            Self::do_shipout(engine,force)
        }
    }

    fn do_shipout(engine:&mut EngineReferences<Self::ET>,forced:bool) {
        let data = engine.stomach.data_mut();
        let page = std::mem::take(&mut data.page);
        // TODO more precisely
        engine.state.set_primitive_int(engine.aux,PRIMITIVES.badness,(10).into(),true);

        let SplitResult{mut first,rest,split_penalty} = if (forced) { SplitResult{first:page,rest:vec!(),split_penalty:None} } else {
            let goal = data.pagegoal;
            Self::split_vertical(engine,page,goal)
        };
        let data = engine.stomach.data_mut();


        for (i,b) in first.iter_mut().enumerate() { match b {
            TeXNode::Insert => todo!(),
            _ => ()
        }}

        if let Some(p) = split_penalty {
            engine.state.set_primitive_int(engine.aux,PRIMITIVES.outputpenalty,p.into(),true);
        }

        engine.state.set_box_register(engine.aux,255,Some(TeXBox {
            children:first,
            info:BoxInfo {
                tp:BoxType::Vertical,
                kind: "output",
                to: None,
                spread: None,
                assigned_width: None,
                assigned_height: None,
                assigned_depth: None,
            },
            start:engine.mouth.current_sourceref(),
            end:engine.mouth.current_sourceref(),
        }),false);

        for r in rest{ Self::add_node(engine,r) }
        let mode = engine.state.get_mode();
        engine.mouth.insert_every::<Self::ET>(engine.state,PRIMITIVES.output);
        engine.get_next(); // '{':BeginGroup
        engine.state.push(engine.aux,GroupType::Character,engine.mouth.line_number());
        engine.state.set_mode(TeXMode::InternalVertical);
        let depth = engine.state.get_group_level();
        while let Some(next) = engine.get_next() {
            if engine.state.get_group_level() == depth && next.is_end_group() {
                engine.state.pop(engine.aux,engine.mouth);
                return
            }
            crate::expand!(Self::ET;engine,next;
                ResolvedToken::Tk { char, code, token } => Self::do_char(engine, token, char, code),
                ResolvedToken::Cmd {token,cmd:Some(Command::Char {char, code})} => Self::do_char(engine, token, *char, *code),
                ResolvedToken::Cmd{cmd: None,token} => engine.aux.error_handler.undefined(engine.aux.memory.cs_interner(),token),
                ResolvedToken::Cmd{cmd: Some(cmd),token} => crate::do_cmd!(Self::ET;engine,token,cmd)
            );
        }
        todo!("file end")
    }

    fn split_vertical(engine:&mut EngineReferences<Self::ET>, nodes: Vec<TeXNode<Self::ET>>, target: <Self::ET as EngineTypes>::Dim) -> SplitResult<Self::ET>;
}

pub struct SplitResult<ET:EngineTypes> {
    pub first:Vec<TeXNode<ET>>,
    pub rest:Vec<TeXNode<ET>>,
    pub split_penalty:Option<i32>
}

#[derive(Clone,Debug)]
pub struct StomachData<ET:EngineTypes> {
    pub page:Vec<TeXNode<ET>>,
    pub open_lists:Vec<NodeList<ET>>,
    pub pagegoal:ET::Dim,
    pub pagetotal:ET::Dim,
    pub pagestretch:ET::Dim,
    pub pagefilstretch:ET::Dim,
    pub pagefillstretch:ET::Dim,
    pub pagefilllstretch:ET::Dim,
    pub pageshrink:ET::Dim,
    pub pagedepth:ET::Dim,
    pub prevdepth:ET::Dim,
    pub spacefactor:i32,
}
impl <ET:EngineTypes> StomachData<ET> {
    pub fn new() -> Self {
        StomachData {
            page:vec!(),
            open_lists:vec!(),
            pagegoal:ET::Dim::from_sp(i32::MAX),
            pagetotal:ET::Dim::default(),
            pagestretch:ET::Dim::default(),
            pagefilstretch:ET::Dim::default(),
            pagefillstretch:ET::Dim::default(),
            pagefilllstretch:ET::Dim::default(),
            pageshrink:ET::Dim::default(),
            pagedepth:ET::Dim::default(),
            prevdepth:ET::Dim::from_sp(-65536000),
            spacefactor:1000
        }
    }
}

pub struct StomachWithShipout<ET:EngineTypes<Stomach=Self>> {
    afterassignment:Option<Tk<Self>>,
    data:StomachData<ET>,
}
impl<ET:EngineTypes<Stomach=Self>> StomachWithShipout<ET> {
    pub fn new() -> Self { StomachWithShipout {
        afterassignment:None,
        data:StomachData::new()
    } }
}
impl<ET:EngineTypes<Stomach=Self>> Stomach for StomachWithShipout<ET> {
    type ET = ET;
    #[inline(always)]
    fn afterassignment(&mut self) -> &mut Option<Tk<Self>> {
        &mut self.afterassignment
    }
    #[inline(always)]
    fn data_mut(&mut self) -> &mut StomachData<Self::ET> {
        &mut self.data
    }

    fn split_vertical(engine: &mut EngineReferences<Self::ET>, nodes: Vec<TeXNode<Self::ET>>, target: <Self::ET as EngineTypes>::Dim) -> SplitResult<ET> {
        todo!()
    }
}