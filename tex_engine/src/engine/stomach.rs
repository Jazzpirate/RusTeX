pub mod methods;

use crate::commands::{CommandScope, Unexpandable};
use crate::engine::{EngineAux, EngineReferences, EngineTypes};
use crate::engine::fontsystem::FontSystem;
use crate::engine::gullet::ResolvedToken;
use crate::engine::mouth::Mouth;
use crate::engine::mouth::pretokenized::TokenList;
use crate::engine::state::State;
use crate::engine::utils::memory::{MemoryManager, PrimitiveIdentifier, PRIMITIVES};
use crate::tex::catcodes::CommandCode;
use crate::tex::nodes::{BoxTarget, NodeList, WhatsitNode, HorizontalNodeListType, VerticalNodeListType, MathNodeListType, ListTarget};
use crate::tex::numerics::NumSet;
use crate::tex::types::{BoxType, GroupType, MathClass, TeXMode};
use crate::utils::HMap;
use crate::tex::numerics::TeXDimen;
use crate::tex::token::Token;
use crate::commands::Command;
use crate::commands::methods::get_mathchar;
use crate::engine::filesystem::{File, SourceRef};
use crate::engine::filesystem::SourceReference;
use crate::utils::errors::ErrorHandler;
use crate::engine::fontsystem::Font;
use crate::tex::nodes::boxes::{BoxInfo, HBoxInfo, TeXBox, ToOrSpread, VBoxInfo};
use crate::tex::nodes::horizontal::HNode;
use crate::tex::nodes::math::{MathAtom, MathFontStyle, MathGroup, MathKernel, MathNode, MathNucleus, UnresolvedMathFontStyle};
use crate::tex::nodes::NodeTrait;
use crate::tex::nodes::vertical::VNode;
use crate::tex::numerics::Skip;

type Tk<S> = <<S as Stomach>::ET as EngineTypes>::Token;
type Ch<S> = <<S as Stomach>::ET as EngineTypes>::Char;
type St<S> = <<S as Stomach>::ET as EngineTypes>::State;
type Int<E> = <<E as EngineTypes>::Num as NumSet>::Int;
type Fnt<E> = <<E as EngineTypes>::FontSystem as FontSystem>::Font;

pub fn insert_afterassignment<ET:EngineTypes>(engine:&mut EngineReferences<ET>) {
    match std::mem::take(engine.stomach.afterassignment()) {
        Some(t) => engine.requeue(t),
        _ => ()
    }
}

pub trait Stomach {
    type ET:EngineTypes<Stomach = Self>;
    fn new(aux:&mut EngineAux<Self::ET>,state:&mut St<Self>) -> Self;
    fn afterassignment(&mut self) -> &mut Option<Tk<Self>>;
    fn data_mut(&mut self) -> &mut StomachData<Self::ET>;

    #[inline(always)]
    fn do_unexpandable(
        engine:&mut EngineReferences<Self::ET>,
        name:PrimitiveIdentifier,
        scope: CommandScope,
        token:Tk<Self>,
        apply:fn(&mut EngineReferences<Self::ET>,Tk<Self>)
    ) {
        if Self::maybe_switch_mode(engine,scope,token.clone()) {
            engine.trace_command(|engine| PRIMITIVES.printable(name, engine.state.get_escape_char()));
            apply(engine, token)
        }
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
        insert_afterassignment(engine);
    }

    fn do_mathchar(engine:&mut EngineReferences<Self::ET>,code:u32,_token:Tk<Self>) {
        if !engine.state.get_mode().is_math() { todo!("throw error") }
        let ret = get_mathchar(engine, code, None);
        Self::add_node_m(engine,MathNode::Atom(ret.to_atom()));
    }

    fn add_box(engine:&mut EngineReferences<Self::ET>,bx:TeXBox<Self::ET>,target:BoxTarget<Self::ET>) {
        if target.is_some() {
            target.call(engine,bx)
        } else {
            match engine.state.get_mode() {
                TeXMode::Horizontal | TeXMode::RestrictedHorizontal => {
                    Self::add_node_h(engine,HNode::Box(bx))
                }
                TeXMode::Vertical | TeXMode::InternalVertical => {
                    Self::add_node_v(engine,VNode::Box(bx))
                }
                _ => Self::add_node_m(engine,MathNode::Atom(MathAtom {
                    nucleus: MathNucleus::Simple{kernel:MathKernel::Box(bx),limits:None,cls:MathClass::Ord},
                    sub:None,sup:None
                }))
            }
        }
    }

    #[inline(always)]
    fn close_box(engine:&mut EngineReferences<Self::ET>, bt:BoxType) {
        methods::close_box(engine,bt)
    }
    fn do_char(engine:&mut EngineReferences<Self::ET>,token:Tk<Self>,char:Ch<Self>,code:CommandCode) {
        match code {
            CommandCode::EOF => (),
            CommandCode::Space if engine.state.get_mode().is_horizontal() =>
                Self::add_node_h(engine,HNode::Space),
            CommandCode::Space => (),
            CommandCode::BeginGroup if engine.state.get_mode().is_math() => {
                engine.state.push(engine.aux,GroupType::Math {
                    display: engine.state.get_mode() == TeXMode::DisplayMath
                },engine.mouth.line_number());
                engine.stomach.data_mut().open_lists.push(NodeList::new_math(engine.mouth.start_ref()));
            },
            CommandCode::EndGroup if engine.state.get_mode().is_math() => {
                match engine.stomach.data_mut().open_lists.pop() {
                    Some(NodeList::Math {children,start,tp:MathNodeListType::Target(t)}) if t.is_some() => {
                        engine.state.pop(engine.aux,engine.mouth);
                        t.call(engine,children,start);
                    }
                    Some(NodeList::Math {children,start,tp:MathNodeListType::Target(t)}) => {
                        engine.state.pop(engine.aux,engine.mouth);
                        Self::add_node_m(engine,MathNode::Atom(MathAtom{
                            nucleus:MathNucleus::Inner(MathKernel::List {children:children.into(),start,end:engine.mouth.current_sourceref()}),
                            sub:None,sup:None
                        }))
                    }
                    _ => todo!("error")
                }
            },
            CommandCode::BeginGroup =>
                engine.state.push(engine.aux,GroupType::Character,engine.mouth.line_number()),
            CommandCode::EndGroup => {
                match engine.state.get_group_type() {
                    Some(GroupType::Character) =>
                        engine.state.pop(engine.aux,engine.mouth),
                    Some(GroupType::Box(bt)) =>
                        Self::close_box(engine, bt),
                    o => todo!("throw error; group type {:?}",o)
                }
            }
            CommandCode::Other | CommandCode::Letter if engine.state.get_mode().is_horizontal() =>
                Self::do_word(engine,char),
            CommandCode::Other | CommandCode::Letter | CommandCode::MathShift if engine.state.get_mode().is_vertical() =>
                Self::open_paragraph(engine,token),
            CommandCode::MathShift if engine.state.get_mode().is_math() => match engine.stomach.data_mut().open_lists.pop() {
                Some(NodeList::Math{children,start,tp:MathNodeListType::Top {display}}) => {
                    if display {match engine.get_next() {
                        Some(tk) if tk.command_code() == CommandCode::MathShift => (),
                        _ => todo!("throw error")
                    }}
                    engine.state.pop(engine.aux,engine.mouth);
                    let group = MathGroup::<Self::ET,MathFontStyle<Self::ET>>::close(display,start,engine.mouth.current_sourceref(),children);
                    Self::add_node_h(engine,HNode::MathGroup(group));
                }
                _ => todo!("error")
            }
            CommandCode::MathShift => {
                let (display,every) = match engine.state.get_mode() {
                    TeXMode::Horizontal => {
                        match engine.get_next() {
                            Some(tk) if tk.command_code() == CommandCode::MathShift => (true,PRIMITIVES.everydisplay),
                            Some(o) => {
                                engine.requeue(o);(false,PRIMITIVES.everymath)
                            }
                            _ => todo!("file end")
                        }
                    }
                    _ => (false,PRIMITIVES.everymath)
                };
                engine.stomach.data_mut().open_lists.push(NodeList::Math {children:vec!(),start:engine.mouth.start_ref(),tp:MathNodeListType::Top{display}});
                engine.state.push(engine.aux,GroupType::Math{display},engine.mouth.line_number());
                engine.state.set_primitive_int(engine.aux,PRIMITIVES.fam,(-1).into(),false);
                engine.mouth.insert_every::<Self::ET>(engine.state,every);
            }
            CommandCode::Other | CommandCode::Letter => {
                let code = engine.state.get_mathcode(char);
                if code == 32768 {
                    engine.mouth.requeue(Tk::<Self>::from_char_cat(char,CommandCode::Active));
                } else {
                    let ret = get_mathchar(engine, code, Some(char));
                    Self::add_node_m(engine,MathNode::Atom(ret.to_atom()));
                }
            }
            CommandCode::Superscript if engine.state.get_mode().is_math() => {
                Self::do_superscript(engine)
            }
            CommandCode::Subscript if engine.state.get_mode().is_math() => {
                Self::do_subscript(engine)
            }
            _ => todo!("{} > {:?}",char,code)
        }
    }

    #[inline(always)]
    fn do_superscript(engine:&mut EngineReferences<Self::ET>) {
        do_xscript(engine,Script::Super)
    }

    #[inline(always)]
    fn do_subscript(engine:&mut EngineReferences<Self::ET>) {
        do_xscript(engine,Script::Sub)
    }

    fn do_word(engine:&mut EngineReferences<Self::ET>,char:Ch<Self>) {
        // TODO trace
        let mut current = char;
        macro_rules! char {
            ($c:expr) => {{
                let font = engine.state.get_current_font().clone();
                match font.ligature(current,$c) {
                    Some(c) => current = c,
                    None => {
                        engine.stomach.add_char(engine.state,current,font);
                        current = $c;
                    }
                }
            }};
        }
        macro_rules! end {
            ($e:expr) => {{
                let font = engine.state.get_current_font().clone();
                engine.stomach.add_char(engine.state,current,font);
                engine.stomach.data_mut().spacefactor = 1000;
                return $e
            }}
        }
        crate::expand_loop!(Self::ET;engine,
            ResolvedToken::Tk { code:CommandCode::Noexpand, .. } => {engine.get_next();},
            ResolvedToken::Tk { char, code:CommandCode::Letter|CommandCode::Other, .. } =>
                char!(char),
            ResolvedToken::Cmd{ cmd:Some(Command::Char {char,code:CommandCode::Letter|CommandCode::Other}),.. } =>
                char!(*char),
            ResolvedToken::Cmd{ cmd:Some(Command::Unexpandable (Unexpandable{name,..})),.. } if *name == PRIMITIVES.char => {
                let char = engine.read_charcode(false);
                char!(char)
            }
            ResolvedToken::Tk { code:CommandCode::Space, .. } |
            ResolvedToken::Cmd{ cmd:Some(Command::Char {code:CommandCode::Space,..}),.. } =>
                end!(Self::add_node_h(engine,HNode::Space)),
            ResolvedToken::Tk { char, code, token } =>
                end!(Self::do_char(engine,token,char,code)),
            ResolvedToken::Cmd{ cmd:Some(Command::Char {char, code}),token} =>
                end!(Self::do_char(engine,token,*char,*code)),
            ResolvedToken::Cmd{cmd: None,token} => engine.aux.error_handler.undefined(engine.aux.memory.cs_interner(),token),
            ResolvedToken::Cmd{cmd: Some(cmd),token} => {
                end!(crate::do_cmd!(Self::ET;engine,token,cmd))
            }
        );
        todo!()
    }
    fn add_char(&mut self,state:&St<Self>,char:Ch<Self>,font:<<Self::ET as EngineTypes>::FontSystem as FontSystem>::Font) {
        let width = font.get_wd(char);
        let height = font.get_ht(char);
        let depth = font.get_dp(char);
        let sf = state.get_sfcode(char);
        let data = self.data_mut();
        if sf > 1000 && data.spacefactor < 1000 {
            data.spacefactor = 1000;
        } else { data.spacefactor = sf as i32; }
        match self.data_mut().open_lists.last_mut() {
            Some(NodeList::Horizontal {children,..}) => {
                children.push(HNode::Char {
                    char,font,width,height,depth
                });
            }
            _ => todo!("throw error")
        }
    }
    fn do_box(engine:&mut EngineReferences<Self::ET>,_name:PrimitiveIdentifier,token:Tk<Self>,bx:fn(&mut EngineReferences<Self::ET>,Tk<Self>) -> Result<Option<TeXBox<Self::ET>>,BoxInfo<Self::ET>>) {
        match bx(engine,token) {
            Ok(Some(bx)) => Self::add_box(engine,bx,BoxTarget::none()),
            Ok(None) => (),
            Err(bi) =>
                engine.stomach.data_mut().open_lists.push(bi.open_list(engine.mouth.start_ref()))
        }
    }
    fn maybe_switch_mode(engine:&mut EngineReferences<Self::ET>, scope: CommandScope, token:Tk<Self>) -> bool {
        match (scope,engine.state.get_mode()) {
            (CommandScope::Any, _) => true,
            (CommandScope::SwitchesToHorizontal | CommandScope::SwitchesToHorizontalOrMath, TeXMode::Horizontal | TeXMode::RestrictedHorizontal) => true,
            (CommandScope::SwitchesToVertical, TeXMode::Vertical | TeXMode::InternalVertical) => true,
            (CommandScope::MathOnly| CommandScope::SwitchesToHorizontalOrMath, TeXMode::InlineMath | TeXMode::DisplayMath) => true,
            (CommandScope::SwitchesToHorizontal| CommandScope::SwitchesToHorizontalOrMath,TeXMode::Vertical | TeXMode::InternalVertical) => {
                Self::open_paragraph(engine,token);
                false
            }
            (CommandScope::MathOnly, _) => todo!("throw error"),
            (a,b) => {
                let mut str = String::new();
                token.display_fmt(engine.aux.memory.cs_interner(),engine.state.get_catcode_scheme(),engine.state.get_escape_char(),&mut str).unwrap();
                todo!("switch modes maybe: {:?} in {:?}: {}",a,b,str)
            }
        }
    }

    fn do_font(engine:&mut EngineReferences<Self::ET>,_token:Tk<Self>,f:Fnt<Self::ET>,global:bool) {
        engine.state.set_current_font(engine.aux,f,global);
        insert_afterassignment(engine);
    }
    fn assign_int_register(engine:&mut EngineReferences<Self::ET>,register:u16,global:bool) {
        let val = engine.read_int(true);
        engine.state.set_int_register(engine.aux,register,val,global);
        insert_afterassignment(engine);
    }
    fn assign_dim_register(engine:&mut EngineReferences<Self::ET>,register:u16,global:bool) {
        let val = engine.read_dim(true);
        engine.state.set_dim_register(engine.aux,register,val,global);
        insert_afterassignment(engine);
    }
    fn assign_skip_register(engine:&mut EngineReferences<Self::ET>,register:u16,global:bool) {
        let val = engine.read_skip(true);
        engine.state.set_skip_register(engine.aux,register,val,global);
        insert_afterassignment(engine);
    }
    fn assign_muskip_register(engine:&mut EngineReferences<Self::ET>,register:u16,global:bool) {
        let val = engine.read_muskip(true);
        engine.state.set_muskip_register(engine.aux,register,val,global);
        insert_afterassignment(engine);
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
                    engine.read_until_endgroup(|_,_,t| tks.push(t));
                    engine.state.set_toks_register(engine.aux,register,TokenList::from(tks),global);
                    insert_afterassignment(engine);
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
        insert_afterassignment(engine);
    }
    fn assign_primitive_dim(engine:&mut EngineReferences<Self::ET>,name:PrimitiveIdentifier,global:bool) {
        engine.trace_command(|engine| format!("{}", PRIMITIVES.printable(name,engine.state.get_escape_char())));
        let val = engine.read_dim(true);
        engine.state.set_primitive_dim(engine.aux,name,val,global);
        insert_afterassignment(engine);
    }
    fn assign_primitive_skip(engine:&mut EngineReferences<Self::ET>,name:PrimitiveIdentifier,global:bool) {
        engine.trace_command(|engine| format!("{}", PRIMITIVES.printable(name,engine.state.get_escape_char())));
        let val = engine.read_skip(true);
        engine.state.set_primitive_skip(engine.aux,name,val,global);
        insert_afterassignment(engine);
    }
    fn assign_primitive_muskip(engine:&mut EngineReferences<Self::ET>,name:PrimitiveIdentifier,global:bool) {
        engine.trace_command(|engine| format!("{}", PRIMITIVES.printable(name,engine.state.get_escape_char())));
        let val = engine.read_muskip(true);
        engine.state.set_primitive_muskip(engine.aux,name,val,global);
        insert_afterassignment(engine);
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
                        engine.read_until_endgroup(|_,_,t| tks.push(t));
                        tks.push(Tk::<Self>::from_char_cat(b'}'.into(),CommandCode::EndGroup));
                    } else {
                        engine.read_until_endgroup(|_,_,t| tks.push(t));
                    }
                    engine.state.set_primitive_tokens(engine.aux,name,TokenList::from(tks),global);
                    insert_afterassignment(engine);
                    return ()
                }
                _ => todo!("throw error")
            }
            _ => todo!("throw error")
        )
    }
    fn do_whatsit(engine:&mut EngineReferences<Self::ET>,name:PrimitiveIdentifier,token:Tk<Self>,read:fn(&mut EngineReferences<Self::ET>,Tk<Self>)
                                                                                -> Option<Box<dyn FnOnce(&mut EngineReferences<Self::ET>)>>) {
        if let Some(ret) = read(engine,token) {
            let wi = WhatsitNode::new(ret, name);
            match engine.state.get_mode() {
                TeXMode::Vertical | TeXMode::InternalVertical => Self::add_node_v(engine,VNode::Whatsit(wi)),
                TeXMode::Horizontal | TeXMode::RestrictedHorizontal => Self::add_node_h(engine,HNode::Whatsit(wi)),
                _ => Self::add_node_m(engine,MathNode::Whatsit(wi))
            }
        }
    }

    fn open_align(engine:&mut EngineReferences<Self::ET>,_inner:BoxType,between:BoxType) {
        engine.state.push(engine.aux,GroupType::Box(between),engine.mouth.line_number());
        engine.stomach.data_mut().open_lists.push(
        if between == BoxType::Vertical {
            NodeList::Vertical {
                tp: VerticalNodeListType::HAlign,
                children: vec!()
            }} else {
            NodeList::Horizontal {
                tp: HorizontalNodeListType::VAlign,
                children: vec!()
            }
        });
    }

    fn close_align(engine:&mut EngineReferences<Self::ET>) {
        match engine.stomach.data_mut().open_lists.pop() {
            Some(NodeList::Vertical{children,tp:VerticalNodeListType::HAlign}) => {
                engine.state.pop(engine.aux,&mut engine.mouth);
                for c in children {
                    Self::add_node_v(engine, c);
                }
            }
            Some(NodeList::Horizontal{children,tp:HorizontalNodeListType::VAlign}) => {
                engine.state.pop(engine.aux,&mut engine.mouth);
                for c in children {
                    Self::add_node_h(engine, c);
                }
            }
            _ => todo!("throw error")
        };
    }

    fn add_node_m(engine:&mut EngineReferences<Self::ET>,node: MathNode<Self::ET,UnresolvedMathFontStyle<Self::ET>>) {
        match engine.stomach.data_mut().open_lists.last_mut() {
            Some(NodeList::Math {children,..}) => {
                children.push(node);
                return
            }
            _ => todo!("throw error")
        }
    }

    fn add_node_h(engine:&mut EngineReferences<Self::ET>,node: HNode<Self::ET>) {
        match engine.stomach.data_mut().open_lists.last_mut() {
            Some(NodeList::Horizontal {children,..}) => {
                children.push(node);
                return
            }
            _ => todo!("throw error")
        }
    }

    fn add_node_v(engine:&mut EngineReferences<Self::ET>,node: VNode<Self::ET>) {
        let data = engine.stomach.data_mut();
        if let VNode::HRule {..} = node {
            data.prevdepth = <<Self::ET as EngineTypes>::Dim as TeXDimen>::from_sp(-65536000);
        } else {
            data.prevdepth = node.depth();
        }
        match data.open_lists.last_mut() {
            Some(NodeList::Vertical {children,..}) => {
                children.push(node);
                return
            }
            Some(_) => todo!("throw error"),
            _ => ()
        }
        if !data.page_contains_boxes && !data.in_output /*data.pagegoal == <<Self::ET as EngineTypes>::Dim as TeXDimen>::from_sp(i32::MAX)*/ {
            match &node {
                VNode::Box(_) | VNode::Insert(..) => {
                    //crate::debug_log!(debug => "Here: {} \n\n {}",node.readable(),engine.mouth.display_position());
                    data.page_contains_boxes = true;
                    data.pagegoal = engine.state.get_primitive_dim(PRIMITIVES.vsize);
                }
                n if n.discardable() => return,
                _ => ()
            }
        }
        data.pagetotal = data.pagetotal + node.height();// + node.depth() ?
        if let VNode::Penalty(i) = node {
            if i <= -10000 {
                if data.page_contains_boxes {
                    return Self::do_shipout_output(engine, Some(i))
                } else { return }
            }
        }
        data.page.push(node);
        Self::maybe_shipout(engine)
    }
/*
    fn add_node(engine:&mut EngineReferences<Self::ET>,node: TeXNode<Self::ET>) {
        let data = engine.stomach.data_mut();
        match data.open_lists.last_mut() {
            Some(NodeList{tp:NodeListType::Box(bi,..),children}) if bi.tp() == BoxType::Vertical => {
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
                if !data.page_contains_boxes && !data.in_output /*data.pagegoal == <<Self::ET as EngineTypes>::Dim as TeXDimen>::from_sp(i32::MAX)*/ {
                    match &node {
                        TeXNode::Box(_) | TeXNode::Insert => {
                            //crate::debug_log!(debug => "Here: {} \n\n {}",node.readable(),engine.mouth.display_position());
                            data.page_contains_boxes = true;
                            data.pagegoal = engine.state.get_primitive_dim(PRIMITIVES.vsize);
                        }
                        n if n.discardable() => return,
                        _ => ()
                    }
                }
                data.pagetotal = data.pagetotal + node.height();// + node.depth() ?
                match node {
                    TeXNode::Simple(SimpleNode::VRule {..}) =>
                        data.prevdepth = <<Self::ET as EngineTypes>::Dim as TeXDimen>::from_sp(-65536000),
                    TeXNode::Penalty(i) if i <= -10000 => {
                        if data.page_contains_boxes {
                            return Self::do_shipout_output(engine, Some(i))
                        } else { return }
                    }
                    _ => data.prevdepth = node.depth()
                }
                data.page.push(node);
                Self::maybe_shipout(engine)
            }
        }
    }

 */

    fn maybe_shipout(engine:&mut EngineReferences<Self::ET>) {
        let data = engine.stomach.data_mut();
        if !data.in_output && data.open_lists.is_empty() && data.pagetotal >= data.pagegoal && !data.page.is_empty() {
            Self::do_shipout_output(engine, None)
        }
    }

    fn do_shipout_output(engine:&mut EngineReferences<Self::ET>, forced:Option<i32>) {
        let data = engine.stomach.data_mut();
        let page = std::mem::take(&mut data.page);
        data.pagegoal = <Self::ET as EngineTypes>::Dim::from_sp(i32::MAX);
        data.pagetotal = <Self::ET as EngineTypes>::Dim::default();
        data.page_contains_boxes = false;
        // TODO more precisely
        engine.state.set_primitive_int(engine.aux,PRIMITIVES.badness,(10).into(),true);

        let SplitResult{mut first,rest,split_penalty} = match forced {
            Some(p) => SplitResult{first:page.into(),rest:vec!().into(),split_penalty:Some(p)},
            _ => {
                let goal = data.pagegoal;
                Self::split_vertical(engine,page,goal)
            }
        };

        engine.state.push(engine.aux,GroupType::Character,engine.mouth.line_number());
        engine.state.set_mode(TeXMode::InternalVertical);

        for r in rest.into_iter() { Self::add_node_v(engine,r) }

        let data = engine.stomach.data_mut();
        data.in_output = true;
        data.deadcycles += 1;
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

        let mut deletes = vec!();
        for (i,b) in first.iter_mut().enumerate() { match b {
            VNode::Insert(n,v) => {
                engine.state.set_box_register(engine.aux,*n as u16,Some(TeXBox::V {
                    info: VBoxInfo::VBox {
                        scaled: ToOrSpread::None,
                        assigned_width: None,
                        assigned_height: None,
                        assigned_depth: None,
                        moved_left: None,
                        raised: None,
                    },
                    children:std::mem::replace(v, vec!().into()),
                    start:engine.mouth.current_sourceref(),
                    end:engine.mouth.current_sourceref(),
                }),false);
                deletes.push(i)
            }
            _ => ()
        }}
        for i in deletes { first.remove(i); }

        if let Some(p) = split_penalty {
            engine.state.set_primitive_int(engine.aux,PRIMITIVES.outputpenalty,p.into(),true);
        }

        let bx = TeXBox::V {
            children:first.into(),
            info:VBoxInfo::Output,
            start:engine.mouth.current_sourceref(),
            end:engine.mouth.current_sourceref(),
        };

        //crate::debug_log!(debug => "Here: {} \n\n {}",bx.readable(),engine.mouth.display_position());

        engine.state.set_box_register(engine.aux,255,Some(bx),false);

        engine.mouth.insert_every::<Self::ET>(engine.state,PRIMITIVES.output);
        engine.get_next(); // '{':BeginGroup

        //crate::debug_log!(debug => "Here: {} at {}",engine.mouth.display_position(),engine.preview());

        let depth = engine.state.get_group_level();
        while let Some(next) = engine.get_next() {
            if engine.state.get_group_level() == depth && next.is_end_group() {
                engine.state.pop(engine.aux,engine.mouth);
                engine.stomach.data_mut().in_output = false;
                return
            }
            crate::expand!(Self::ET;engine,next;
                ResolvedToken::Tk { code:CommandCode::Noexpand, .. } => {engine.get_next();},
                ResolvedToken::Tk { char, code, token } => Self::do_char(engine, token, char, code),
                ResolvedToken::Cmd {token,cmd:Some(Command::Char {char, code})} => Self::do_char(engine, token, *char, *code),
                ResolvedToken::Cmd{cmd: None,token} => engine.aux.error_handler.undefined(engine.aux.memory.cs_interner(),token),
                ResolvedToken::Cmd{cmd: Some(cmd),token} => crate::do_cmd!(Self::ET;engine,token,cmd)
            );
        }
        todo!("file end")
    }

    fn split_vertical(engine:&mut EngineReferences<Self::ET>, nodes: Vec<VNode<Self::ET>>, target: <Self::ET as EngineTypes>::Dim) -> SplitResult<Self::ET>;

    fn open_paragraph(engine:&mut EngineReferences<Self::ET>,token:Tk<Self>) {
        let sref = engine.mouth.start_ref();
        engine.stomach.data_mut().open_lists.push(NodeList::Horizontal {
            tp:HorizontalNodeListType::Paragraph(sref),
            children:vec!()
        });
        engine.state.set_mode(TeXMode::Horizontal);
        match engine.resolve(token) {
            ResolvedToken::Cmd{cmd:Some(Command::Unexpandable(u)),..}
                if u.name == PRIMITIVES.indent => {
                Self::add_node_h(engine,HNode::Box(TeXBox::H {children:vec!().into(),info:HBoxInfo::ParIndent(engine.state.get_primitive_dim(PRIMITIVES.parindent)), start:sref, end:sref}))
            },
            ResolvedToken::Cmd{cmd:Some(Command::Unexpandable(u)),..}
                if u.name == PRIMITIVES.noindent => (),
            ResolvedToken::Cmd {token,..} | ResolvedToken::Tk {token,..} => {
                engine.mouth.requeue(token);
                Self::add_node_h(engine,HNode::Box(TeXBox::H {children:vec!().into(),info:HBoxInfo::ParIndent(engine.state.get_primitive_dim(PRIMITIVES.parindent)), start:sref, end:sref}))
            }
        }
        engine.mouth.insert_every::<Self::ET>(engine.state,PRIMITIVES.everypar)
    }

    fn close_paragraph(engine:&mut EngineReferences<Self::ET>) {
        let ls = &mut engine.stomach.data_mut().open_lists;
        match ls.pop() {
            Some(NodeList::Horizontal{tp:HorizontalNodeListType::Paragraph(sourceref),children}) => {
                if ls.is_empty() {
                    engine.state.set_mode(TeXMode::Vertical)
                } else {
                    engine.state.set_mode(TeXMode::InternalVertical)
                }
                if children.is_empty() {
                    let _ = engine.state.take_parshape();
                    engine.state.set_primitive_int(engine.aux,PRIMITIVES.hangafter,Int::<Self::ET>::default(),false);
                    engine.state.set_primitive_dim(engine.aux,PRIMITIVES.hangindent,<Self::ET as EngineTypes>::Dim::default(),false);
                    return
                }
                let spec = ParLineSpec::make(engine.state,engine.aux);
                Self::split_paragraph(engine,spec,children,sourceref);
            }
            _ => todo!("throw error")
        }
    }

    fn split_paragraph(engine:&mut EngineReferences<Self::ET>, specs:Vec<ParLineSpec<Self::ET>>, children:Vec<HNode<Self::ET>>, sourceref:SourceReference<<<Self::ET as EngineTypes>::File as File>::SourceRefID>) {
        if children.is_empty() { return }
        let ret = split_paragraph_roughly(engine,specs,children,sourceref);
        for line in ret {
            match line {
                ParLine::Adjust(n) => Self::add_node_v(engine,n),
                ParLine::Line(bx) => Self::add_node_v(engine,VNode::Box(bx))
            }
        }
    }
}

impl<ET:EngineTypes> EngineReferences<'_,ET> {
    pub fn read_box(&mut self,skip_eq:bool) -> Result<Option<TeXBox<ET>>, BoxInfo<ET>> {
        let mut read_eq = !skip_eq;
        crate::expand_loop!(self,
            ResolvedToken::Tk {char,code:CommandCode::Other,..} if !read_eq && matches!(char.try_into(),Ok(b'=')) => read_eq = true,
            ResolvedToken::Tk { code:CommandCode::Space,..} => (),
            ResolvedToken::Cmd {cmd:Some(Command::Box(b)),token} =>
                return (b.read)(self,token),
            _ => todo!("error")
        );
        todo!("file end")
    }
}
#[derive(Debug,Clone,Hash,Eq,PartialEq)]
pub struct ParLineSpec<ET:EngineTypes> {
    pub leftskip:ET::Skip,
    pub rightskip:ET::Skip,
    pub target:ET::Dim
}
impl<ET:EngineTypes> ParLineSpec<ET> {
    pub fn make(state:&mut ET::State,aux:&mut EngineAux<ET>) -> Vec<ParLineSpec<ET>> {
        let parshape = state.take_parshape(); // (left-indent,width)*
        let hangindent = state.get_primitive_dim(PRIMITIVES.hangindent); // left-indent
        let hangafter = state.get_primitive_int(PRIMITIVES.hangafter).into();
        state.set_primitive_int(aux,PRIMITIVES.hangafter,ET::Int::default(),false);
        state.set_primitive_dim(aux,PRIMITIVES.hangindent,ET::Dim::default(),false);
        let leftskip = state.get_primitive_skip(PRIMITIVES.leftskip);
        let rightskip = state.get_primitive_skip(PRIMITIVES.rightskip);
        let hsize = state.get_primitive_dim(PRIMITIVES.hsize);
        if parshape.is_empty() {
            if hangindent == ET::Dim::default() || hangafter == 0 {
                vec!(ParLineSpec {
                    target:hsize + -(leftskip.base() + rightskip.base()),
                    leftskip,rightskip
                })
            } else {
                if hangafter < 0 {
                    let mut r: Vec<ParLineSpec<ET>> = (0..-hangafter).map(|_| ParLineSpec {
                        target:hsize + -(leftskip.base() + rightskip.base() + hangindent),
                        leftskip:leftskip.add(hangindent),rightskip
                    }).collect();
                    r.push(ParLineSpec {
                        target:hsize + -(leftskip.base() + rightskip.base()),
                        leftskip,rightskip
                    });
                    r
                } else {
                    let mut r: Vec<ParLineSpec<ET>> = (0..hangafter).map(|_| ParLineSpec {
                        target:hsize + -(leftskip.base() + rightskip.base()),
                        leftskip,rightskip
                    }).collect();
                    r.push(ParLineSpec {
                        target:hsize + -(leftskip.base() + rightskip.base() + hangindent),
                        leftskip:leftskip.add(hangindent),rightskip
                    });
                    r
                }
            }
        } else {
            parshape.into_iter().map(|(i,l)| {
                let left = leftskip.add(i);
                let target = l + -(leftskip.base() + rightskip.base());
                let right = rightskip.add(hsize + -(l + i));
                ParLineSpec {
                    leftskip:left,rightskip:right,target
                }
            }).collect()
        }
    }
}

pub struct SplitResult<ET:EngineTypes> {
    pub first:Vec<VNode<ET>>,
    pub rest:Vec<VNode<ET>>,
    pub split_penalty:Option<i32>
}

#[derive(Clone,Debug)]
pub struct StomachData<ET:EngineTypes> {
    pub page:Vec<VNode<ET>>,
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
    pub topmarks:HMap<usize,TokenList<ET::Token>>,
    pub firstmarks:HMap<usize,TokenList<ET::Token>>,
    pub botmarks:HMap<usize,TokenList<ET::Token>>,
    pub splitfirstmarks:HMap<usize,TokenList<ET::Token>>,
    pub splitbotmarks:HMap<usize,TokenList<ET::Token>>,
    pub page_contains_boxes:bool,
    pub in_output:bool,
    pub deadcycles:usize,
    pub vadjusts:Vec<VNode<ET>>,
    pub inserts:Vec<(usize,Box<[VNode<ET>]>)>
}
impl <ET:EngineTypes> StomachData<ET> {
    /*pub fn get_list(&mut self) -> &mut Vec<TeXNode<ET>> {
        match self.open_lists.last_mut() {
            Some(ls) => &mut ls.children,
            None => &mut self.page
        }
    }*/
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
            spacefactor:1000,
            topmarks:HMap::default(),
            firstmarks:HMap::default(),
            botmarks:HMap::default(),
            splitfirstmarks:HMap::default(),
            splitbotmarks:HMap::default(),
            page_contains_boxes:false,
            in_output:false,
            deadcycles:0,
            vadjusts:vec!(),
            inserts:vec!()
        }
    }
}

pub struct StomachWithShipout<ET:EngineTypes<Stomach=Self>> {
    afterassignment:Option<Tk<Self>>,
    data:StomachData<ET>,
}
impl<ET:EngineTypes<Stomach=Self>> Stomach for StomachWithShipout<ET> {
    type ET = ET;
    fn new(_aux: &mut EngineAux<Self::ET>, _state: &mut St<Self>) -> Self {
        StomachWithShipout {
            afterassignment:None,
            data:StomachData::new()
        }
    }
    #[inline(always)]
    fn afterassignment(&mut self) -> &mut Option<Tk<Self>> {
        &mut self.afterassignment
    }
    #[inline(always)]
    fn data_mut(&mut self) -> &mut StomachData<Self::ET> {
        &mut self.data
    }

    #[inline(always)]
    fn split_vertical(engine: &mut EngineReferences<Self::ET>, nodes: Vec<VNode<Self::ET>>, target: <Self::ET as EngineTypes>::Dim) -> SplitResult<ET> {
        vsplit_roughly(engine, nodes, target)
    }
}

pub fn vsplit_roughly<ET:EngineTypes>(engine: &mut EngineReferences<ET>, mut nodes: Vec<VNode<ET>>, mut target: ET::Dim) -> SplitResult<ET> {
    let data = engine.stomach.data_mut();
    data.topmarks.clear();
    std::mem::swap(&mut data.botmarks,&mut data.topmarks);
    data.firstmarks.clear();
    data.splitfirstmarks.clear();
    data.splitbotmarks.clear();
    let mut split = nodes.len();
    let iter = nodes.iter().enumerate();
    for (i,n) in iter {
        match n {
            VNode::Mark(i, v) => {
                if !data.firstmarks.contains_key(&i) {
                    data.firstmarks.insert(*i,v.clone());
                }
                data.botmarks.insert(*i,v.clone());
            }
            _ => {
                target = target + (-n.height()); // - n.depth() ?
                if target < ET::Dim::default() {
                    split = i;
                    break
                }
            }
        }
    };
    let mut rest = nodes.split_off(split);
    let split_penalty = match rest.first() {
        Some(VNode::Penalty(p)) => {
            let p = *p;
            rest.remove(0);
            Some(p)
        }
        _ => None
    };

    for n in &rest {
        match n {
            VNode::Mark(i, v) => {
                if !data.splitfirstmarks.contains_key(&i) {
                    data.splitfirstmarks.insert(*i,v.clone());
                }
                data.splitbotmarks.insert(*i,v.clone());
            }
            _ => ()
        }
    }
    SplitResult{first:nodes,rest:rest,split_penalty}
}

pub enum ParLine<ET:EngineTypes> {
    Line(TeXBox<ET>),//{contents:Vec<TeXNode<ET>>, broken_early:bool },
    Adjust(VNode<ET>)
}

pub fn split_paragraph_roughly<ET:EngineTypes>(_engine:&mut EngineReferences<ET>, specs:Vec<ParLineSpec<ET>>, children:Vec<HNode<ET>>,start:SourceReference<<ET::File as File>::SourceRefID>) -> Vec<ParLine<ET>> {
    let mut ret : Vec<ParLine<ET>> = Vec::new();
    let mut hgoals = specs.into_iter();
    let mut nodes = children.into_iter();
    let mut line_spec = hgoals.next().unwrap();
    let mut target = line_spec.target;
    let mut currstart = start;
    let mut currend = currstart.clone();
    'A:loop {
        let mut line = vec!();
        let mut reinserts = vec!();

        macro_rules! next_line {
            ($b:literal) => {
                if !line.is_empty() {
                    let start = currstart.clone();
                    currstart = currend.clone();
                    ret.push(ParLine::Line(TeXBox::H {children:line.into(),start,end:currend.clone(),info:HBoxInfo::ParLine {spec:line_spec.clone(),ends_with_line_break:$b}}));
                }
                for c in reinserts {
                    ret.push(ParLine::Adjust(c));
                }
                match hgoals.next() {
                    None => (),
                    Some(e) => line_spec = e
                }
                target = line_spec.target;
            };
        }

        while target > ET::Dim::default() {
            match nodes.next() {
                None => {
                    if !line.is_empty() {
                        let start = currstart.clone();
                        currstart = currend.clone();
                        ret.push(ParLine::Line(TeXBox::H {children:line.into(),start,end:currend.clone(),info:HBoxInfo::ParLine {spec:line_spec.clone(),ends_with_line_break:false}}));
                    }
                    for c in reinserts {
                        ret.push(ParLine::Adjust(c));
                    }
                    break 'A
                }
                Some(HNode::Mark(i, m)) =>
                    reinserts.push(VNode::Mark(i,m)),
                Some(HNode::Insert(n,ch)) =>
                    reinserts.push(VNode::Insert(n,ch)),
                Some(HNode::VAdjust(ls)) => reinserts.extend(ls.into_vec().into_iter()),
                Some(HNode::MathGroup(MathGroup {display:true,..})) => todo!(),
                Some(HNode::Penalty(i)) if i <= -10000 => {
                    next_line!(true); // TODO mark somehow
                    continue 'A
                }
                Some(HNode::Penalty(_)) => (),
                Some(node) => {
                    target = target + (-node.width());
                    line.push(node);
                }
            }
        }
        next_line!(false);
    }
    ret
}

pub enum Script {
    Super,Sub
}
impl Script {
    pub fn invalid<ET:EngineTypes>(&self, a:&MathAtom<ET,UnresolvedMathFontStyle<ET>>) -> bool {
        match self {
            Script::Super => a.sup.is_some(),
            Script::Sub => a.sub.is_some()
        }
    }
    pub fn tp<ET:EngineTypes>(&self) -> ListTarget<ET,MathNode<ET,UnresolvedMathFontStyle<ET>>> {
        match self {
            Script::Super => ListTarget::<ET,_>::new(
                |engine,children,_| if let Some(NodeList::Math{children:ch,..}) = engine.stomach.data_mut().open_lists.last_mut() {
                    if let Some(MathNode::Atom(a)) = ch.last_mut() {
                        a.sup = Some(children.into())
                    } else {unreachable!()}
                } else {unreachable!()}
            ),
            _ => ListTarget::<ET,_>::new(
                |engine,children,_| if let Some(NodeList::Math{children:ch,..}) = engine.stomach.data_mut().open_lists.last_mut() {
                    if let Some(MathNode::Atom(a)) = ch.last_mut() {
                        a.sub = Some(children.into())
                    } else {unreachable!()}
                } else {unreachable!()}
            )
        }
    }
}

fn do_xscript<ET:EngineTypes>(engine:&mut EngineReferences<ET>,script:Script) {
    match engine.stomach.data_mut().open_lists.last_mut() {
        Some(NodeList::Math { children, .. }) => {
            match children.last() {
                Some(MathNode::Atom(a)) if script.invalid(a) => {
                    todo!("throw double xscript error")
                },
                Some(MathNode::Atom(_)) => (),
                _ => children.push(MathNode::Atom(MathAtom::empty())),
            }
        }
        _ => unreachable!()
    }
    engine.read_char_or_math_group(|| script.tp())
}

impl<ET:EngineTypes> EngineReferences<'_,ET> {
    pub fn read_char_or_math_group<F2:FnOnce() -> ListTarget<ET,MathNode<ET,UnresolvedMathFontStyle<ET>>>>(&mut self,tp:F2) {
        crate::expand_loop!(self,
            ResolvedToken::Tk {code:CommandCode::Space,..} => (),
            ResolvedToken::Tk {code:CommandCode::BeginGroup,..} => {
                self.state.push(self.aux,GroupType::Math {
                    display:self.state.get_mode() == TeXMode::DisplayMath
                },self.mouth.line_number());
                let list = NodeList::Math{children:vec!(),start:self.mouth.start_ref(),
                    tp:MathNodeListType::Target(tp())};
                self.stomach.data_mut().open_lists.push(list);
                return
            },
            ResolvedToken::Cmd {cmd:Some(Command::Relax),..} => (),
            ResolvedToken::Tk {..} => {
                todo!("char in xscript")
            },
            ResolvedToken::Cmd {cmd:Some(c),..} => {
                todo!("Here: {}",c.meaning(self.aux.memory.cs_interner(),self.state.get_catcode_scheme(),self.state.get_escape_char()))
            }
            o => todo!("??? {:?}",o)
        )
    }
}
