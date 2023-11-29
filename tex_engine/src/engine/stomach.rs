use crate::engine::{EngineReferences, EngineTypes};
use crate::engine::fontsystem::FontSystem;
use crate::engine::gullet::ResolvedToken;
use crate::engine::mouth::Mouth;
use crate::engine::mouth::pretokenized::TokenList;
use crate::engine::state::State;
use crate::engine::utils::memory::{MemoryManager, PrimitiveIdentifier, PRIMITIVES};
use crate::tex::catcodes::CommandCode;
use crate::tex::nodes::{BoxInfo, NodeList, NodeListType, TeXBox, TeXNode};
use crate::tex::numerics::NumSet;
use crate::tex::types::GroupType;
use crate::utils::Ptr;
use crate::tex::numerics::TeXDimen;

type Tk<S> = <<S as Stomach>::ET as EngineTypes>::Token;
type Ch<S> = <<S as Stomach>::ET as EngineTypes>::Char;
type Int<E> = <<E as EngineTypes>::Num as NumSet>::Int;
type Fnt<E> = <<E as EngineTypes>::FontSystem as FontSystem>::Font;

fn afterassignment<ET:EngineTypes>(engine:&mut EngineReferences<ET>) {
    match std::mem::take(engine.stomach.afterassignment()) {
        Some(t) => engine.mouth.requeue(t),
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
                    Some(GroupType::Box(bt)) => {
                        match engine.stomach.data_mut().open_lists.pop() {
                            Some(ls) => match ls.tp {
                                NodeListType::Paragraph => {
                                    todo!("close paragraph")
                                }
                                NodeListType::Box(bi,reg) if bi.tp == bt => {
                                    engine.state.pop(engine.aux,engine.mouth);
                                    let bx = TeXBox {
                                        children:ls.children,info:bi
                                    };
                                    match reg {
                                        Some((reg,global)) =>
                                            engine.state.set_box_register(engine.aux,reg,Some(bx),global),
                                        None => {
                                            let data = engine.stomach.data_mut();
                                            match data.open_lists.last_mut() {
                                                Some(ls) => ls.children.push(todo!("box as node")),
                                                None => {
                                                    data.page.push(todo!("box as node"));
                                                    todo!("update goal etc")
                                                }
                                            }
                                        }
                                    }
                                }
                                _ => todo!("throw error")
                            }
                            None => todo!("throw error"),
                        }
                    }
                    _ => todo!("throw error / close box")
                }
            }
            _ => todo!("{} > {:?}",char,code)
        }
    }
    fn do_box(engine:&mut EngineReferences<Self::ET>,name:PrimitiveIdentifier,token:Tk<Self>,bx:fn(&mut EngineReferences<Self::ET>,Tk<Self>) -> Result<TeXBox<Self::ET>,(BoxInfo<Self::ET>,Option<(u16,bool)>)>) {
        todo!("box in stomach")
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
                    let cc = engine.state.get_catcode_scheme();
                    let endline = engine.state.get_endline_char();
                    engine.mouth.read_until_endgroup(engine.aux,cc,endline,|_,t| tks.push(t));
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
                    let cc = engine.state.get_catcode_scheme();
                    let endline = engine.state.get_endline_char();
                    engine.mouth.read_until_endgroup(engine.aux,cc,endline,|_,t| tks.push(t));
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
}