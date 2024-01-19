use crate::commands::primitives::{PrimitiveIdentifier, PRIMITIVES};
use crate::commands::{TeXCommand, PrimitiveCommand, ResolvedToken};
use crate::engine::{EngineAux, EngineReferences, EngineTypes};
use crate::engine::filesystem::{File, SourceReference};
use crate::tex::nodes::{BoxTarget, ListTarget, NodeList, NodeTrait};
use crate::tex::nodes::boxes::{BoxType, HBoxInfo, TeXBox, ToOrSpread, VBoxInfo};
use crate::tex::nodes::horizontal::{HNode, HorizontalNodeListType};
use crate::tex::nodes::math::{MathAtom, MathClass, MathGroup, MathKernel, MathNode, MathNodeList, MathNodeListType, MathNucleus, UnresolvedMathFontStyle};
use crate::tex::nodes::vertical::{VerticalNodeListType, VNode};
use crate::engine::stomach::{Stomach, TeXMode};
use crate::engine::state::{GroupType, State};
use crate::engine::mouth::Mouth;
use crate::prelude::{CommandCode, TokenList};
use crate::tex::numerics::TeXDimen;
use crate::tex::numerics::Skip;
use crate::tex::tokens::Token;
use crate::engine::fontsystem::Font;
use crate::tex_error;

#[macro_export]
macro_rules! add_node {
    ($S:ty;$engine:expr,$v:expr,$h:expr,$m:expr) => {
        match $engine.stomach.data_mut().mode() {
            TeXMode::Vertical |
            TeXMode::InternalVertical => <$S>::add_node_v($engine,$v),
            TeXMode::Horizontal |
            TeXMode::RestrictedHorizontal => <$S>::add_node_h($engine,$h),
            _ => <$S>::add_node_m($engine,$m)
        }
    };
}

/// inserts the `\afterassignment` [`Token`]
pub fn insert_afterassignment<ET:EngineTypes>(engine:&mut EngineReferences<ET>) {
    match std::mem::take(engine.stomach.afterassignment()) {
        Some(t) => engine.requeue(t),
        _ => ()
    }
}

/// Default implementation for [`Stomach::assign_toks_register`].
pub fn assign_toks_register<ET:EngineTypes>(engine:&mut EngineReferences<ET>,token:ET::Token,register:usize,global:bool) {
    let mut had_eq = false;
    crate::expand_loop!(ET; engine,tk,
        ResolvedToken::Tk{char,code} => match (char.try_into(),code) {
            (_,CommandCode::Space) => (),
            (Ok(b'='),CommandCode::Other) if !had_eq => {
                if had_eq { todo!("throw error") }
                had_eq = true;
            }
            (_,CommandCode::BeginGroup) => {
                let mut tks = shared_vector::Vector::new();
                engine.read_until_endgroup(&token,|_,_,t| tks.push(t));
                engine.state.set_toks_register(engine.aux,register,TokenList::from(tks),global);
                insert_afterassignment(engine);
                return ()
            }
            _ => todo!("throw error")
        }
        ResolvedToken::Cmd(Some(TeXCommand::Primitive{name,cmd:PrimitiveCommand::PrimitiveToks})) => {
            let tks = engine.state.get_primitive_tokens(*name).clone();
            engine.state.set_toks_register(engine.aux,register,tks,global);
            insert_afterassignment(engine);
            return ()
        }
        ResolvedToken::Cmd(Some(TeXCommand::ToksRegister(u))) => {
            let tks = engine.state.get_toks_register(*u).clone();
            engine.state.set_toks_register(engine.aux,register,tks,global);
            insert_afterassignment(engine);
            return ()
        }
        _ => todo!("throw error")
    )
}

/// Default implementation for [`Stomach::assign_primitive_toks`].
pub fn assign_primitive_toks<ET:EngineTypes>(engine:&mut EngineReferences<ET>,token:ET::Token,name:PrimitiveIdentifier,global:bool) {
    let mut had_eq = false;
    crate::expand_loop!(ET; engine,tk,
        ResolvedToken::Tk{char,code} => match (char.try_into(),code) {
            (_,CommandCode::Space) => (),
            (Ok(b'='),CommandCode::Other) if !had_eq => {
                if had_eq { todo!("throw error") }
                had_eq = true;
            }
            (_,CommandCode::BeginGroup) => {
                let mut tks = shared_vector::Vector::new();
                if name == PRIMITIVES.output {
                    tks.push(ET::Token::from_char_cat(b'{'.into(),CommandCode::BeginGroup));
                    engine.read_until_endgroup(&token,|_,_,t| tks.push(t));
                    tks.push(ET::Token::from_char_cat(b'}'.into(),CommandCode::EndGroup));
                } else {
                    engine.read_until_endgroup(&token,|_,_,t| tks.push(t));
                }
                engine.state.set_primitive_tokens(engine.aux,name,TokenList::from(tks),global);
                insert_afterassignment(engine);
                return ()
            }
            _ => todo!("throw error")
        }
        ResolvedToken::Cmd(Some(TeXCommand::Primitive{name,cmd:PrimitiveCommand::PrimitiveToks})) => {
            let tks = engine.state.get_primitive_tokens(*name);
            let tks = if *name == PRIMITIVES.output {
                let mut ntk = shared_vector::Vector::new();
                ntk.push(ET::Token::from_char_cat(b'{'.into(),CommandCode::BeginGroup));
                ntk.extend_from_slice(tks.0.as_slice());
                ntk.push(ET::Token::from_char_cat(b'}'.into(),CommandCode::EndGroup));
                ntk.into()
            } else {
                tks.clone()
            };
            engine.state.set_primitive_tokens(engine.aux,*name,tks,global);
            insert_afterassignment(engine);
            return ()
        }
        ResolvedToken::Cmd(Some(TeXCommand::ToksRegister(u))) => {
            let tks = engine.state.get_toks_register(*u);
            let tks = if name == PRIMITIVES.output {
                let mut ntk = shared_vector::Vector::new();
                ntk.push(ET::Token::from_char_cat(b'{'.into(),CommandCode::BeginGroup));
                ntk.extend_from_slice(tks.0.as_slice());
                ntk.push(ET::Token::from_char_cat(b'}'.into(),CommandCode::EndGroup));
                ntk.into()
            } else {
                tks.clone()
            };
            engine.state.set_primitive_tokens(engine.aux,name,tks,global);
            insert_afterassignment(engine);
            return ()
        }
        _ => todo!("throw error")
    )
}

pub(crate) fn add_box<ET:EngineTypes>(engine:&mut EngineReferences<ET>,bx:TeXBox<ET>,target:BoxTarget<ET>) {
    if target.is_some() {
        target.call(engine,bx)
    } else {
        match engine.stomach.data_mut().mode() {
            TeXMode::Horizontal | TeXMode::RestrictedHorizontal => {
                ET::Stomach::add_node_h(engine, HNode::Box(bx))
            }
            TeXMode::Vertical | TeXMode::InternalVertical => {
                ET::Stomach::add_node_v(engine, VNode::Box(bx))
            }
            _ => ET::Stomach::add_node_m(engine, MathNode::Atom(MathAtom {
                nucleus: MathNucleus::Simple{kernel:MathKernel::Box(bx),limits:None,cls:MathClass::Ord},
                sub:None,sup:None
            }))
        }
    }
}

/// Default implementation for [`Stomach::do_char`].
pub fn do_char<ET:EngineTypes>(engine:&mut EngineReferences<ET>,token:ET::Token,char:ET::Char,code:CommandCode) {
    match code {
        CommandCode::EOF => (),
        CommandCode::Space if engine.stomach.data_mut().mode().is_horizontal() =>
            ET::Stomach::add_node_h(engine, HNode::Space),
        CommandCode::Space => (),
        CommandCode::BeginGroup if engine.stomach.data_mut().mode().is_math() => {
            engine.state.push(engine.aux,GroupType::Math {
                display: engine.stomach.data_mut().mode() == TeXMode::DisplayMath
            },engine.mouth.line_number());
            engine.stomach.data_mut().open_lists.push(NodeList::new_math(engine.mouth.start_ref()));
        },
        CommandCode::EndGroup if engine.stomach.data_mut().mode().is_math() => close_group_in_m(engine),
        CommandCode::BeginGroup =>
            engine.state.push(engine.aux,GroupType::Character,engine.mouth.line_number()),
        CommandCode::EndGroup => {
            match engine.state.get_group_type() {
                Some(GroupType::Character) =>
                    engine.state.pop(engine.aux,engine.mouth),
                Some(GroupType::Box(bt)) =>
                    ET::Stomach::close_box(engine, bt),
                o => tex_error!(engine,other,"Extra }, or forgotten \\endgroup")
            }
        }
        CommandCode::Other | CommandCode::Letter if engine.stomach.data_mut().mode().is_horizontal() =>
            do_word(engine, char),
        CommandCode::Other | CommandCode::Letter | CommandCode::MathShift if engine.stomach.data_mut().mode().is_vertical() =>
            ET::Stomach::open_paragraph(engine, token),
        CommandCode::MathShift if engine.stomach.data_mut().mode().is_math() =>
            close_math(engine),
        CommandCode::MathShift => open_math(engine),
        CommandCode::Other | CommandCode::Letter => {
            let code = engine.state.get_mathcode(char);
            ET::Stomach::do_mathchar(engine,code,Some(token))
        }
        CommandCode::Superscript if engine.stomach.data_mut().mode().is_math() => {
            do_superscript(engine)
        }
        CommandCode::Subscript if engine.stomach.data_mut().mode().is_math() => {
            do_subscript(engine)
        }
        _ => todo!("{} > {:?}",char,code)
    }
}


fn do_word<ET:EngineTypes>(engine:&mut EngineReferences<ET>,char:ET::Char) {
    // TODO trace
    let mut current = char;
    macro_rules! char {
        ($c:expr) => {{
            let font = engine.state.get_current_font().clone();
            match font.ligature(current,$c) {
                Some(c) => {
                    current = c;
                }
                None => {
                    add_char::<ET>(engine.stomach,engine.state,current,font);
                    current = $c;
                }
            }
        }};
    }
    macro_rules! end {
        ($e:expr) => {{
            let font = engine.state.get_current_font().clone();
            add_char::<ET>(engine.stomach,engine.state,current,font);
            $e;
            engine.stomach.data_mut().spacefactor = 1000;
            return
        }}
    }
    crate::expand_loop!(ET;token => {
        if token.is_primitive() == Some(PRIMITIVES.noexpand) { engine.get_next(); continue}
    };engine,
        ResolvedToken::Tk { char, code:CommandCode::Letter|CommandCode::Other } =>
            char!(char),
        ResolvedToken::Cmd(Some(TeXCommand::Char {char,code:CommandCode::Letter|CommandCode::Other})) =>
            char!(*char),
        ResolvedToken::Cmd(Some(TeXCommand::Primitive{name,..})) if *name == PRIMITIVES.char => {
            let char = engine.read_charcode(false);
            char!(char)
        }
        ResolvedToken::Tk { code:CommandCode::Space, .. } |
        ResolvedToken::Cmd(Some(TeXCommand::Char {code:CommandCode::Space,..})) =>
            end!(ET::Stomach::add_node_h(engine,HNode::Space)),
        ResolvedToken::Tk { char, code } =>
            end!(ET::Stomach::do_char(engine,token,char,code)),
        ResolvedToken::Cmd(Some(TeXCommand::Char {char, code})) =>
            end!(ET::Stomach::do_char(engine,token,*char,*code)),
        ResolvedToken::Cmd(None) => {
            tex_error!(engine,undefined,token.clone());
            end!(())
        }
        ResolvedToken::Cmd(Some(cmd)) => {
            end!(crate::do_cmd!(ET;engine,token,cmd))
        }
    );
    todo!()
}

fn add_char<ET:EngineTypes>(slf:&mut ET::Stomach,state:&ET::State,char:ET::Char,font: ET::Font) {
    let sf = state.get_sfcode(char);
    let data = slf.data_mut();
    if sf > 1000 && data.spacefactor < 1000 {
        data.spacefactor = 1000;
    } else { data.spacefactor = sf as i32; }
    match slf.data_mut().open_lists.last_mut() {
        Some(NodeList::Horizontal {children,..}) => {
            children.push(HNode::Char {
                char,font
            });
        }
        _ => todo!("throw error")
    }
}

fn open_math<ET:EngineTypes>(engine:&mut EngineReferences<ET>) {
    let (display,every) = match engine.stomach.data_mut().mode() {
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
    engine.stomach.data_mut().open_lists.push(NodeList::Math {children:MathNodeList::new(),start:engine.mouth.start_ref(),tp:MathNodeListType::Top{display}});
    engine.state.push(engine.aux,GroupType::Math{display},engine.mouth.line_number());
    engine.state.set_primitive_int(engine.aux,PRIMITIVES.fam,(-1).into(),false);
    engine.push_every(every);
}

fn close_math<ET:EngineTypes>(engine:&mut EngineReferences<ET>) {
    match engine.stomach.data_mut().open_lists.pop() {
        Some(NodeList::Math{children,start,tp:MathNodeListType::Top {display}}) => {
            if display {match engine.get_next() {
                Some(tk) if tk.command_code() == CommandCode::MathShift => (),
                _ => todo!("throw error")
            }}
            let (children,eqno) = children.close(start,engine.mouth.current_sourceref());
            let group = MathGroup::close(engine.state,
                if display {Some((
                    engine.state.get_primitive_skip(PRIMITIVES.abovedisplayskip),
                    engine.state.get_primitive_skip(PRIMITIVES.belowdisplayskip)
                ))} else {None},
                start,engine.mouth.current_sourceref(),children,eqno);
            engine.state.pop(engine.aux,engine.mouth);
            ET::Stomach::add_node_h(engine, HNode::MathGroup(group));
        }
        _ => todo!("error")
    }
}

fn close_group_in_m<ET:EngineTypes>(engine:&mut EngineReferences<ET>) {
    let ls = engine.stomach.data_mut().open_lists.pop();
    match ls {
        Some(NodeList::Math {children,start,tp:MathNodeListType::Target(t)}) if t.is_some() => {
            engine.state.pop(engine.aux,engine.mouth);
            let (children,None) = children.close(start,engine.mouth.current_sourceref()) else { unreachable!() };
            t.call(engine,children,start);
        }
        Some(NodeList::Math {children,start,tp:MathNodeListType::Target(_)}) => {
            match children {
                MathNodeList::Simple(v) =>
                    ET::Stomach::add_node_m(engine, MathNode::Atom(MathAtom{
                        nucleus:MathNucleus::Inner(MathKernel::List {children:v.into(),start,end:engine.mouth.current_sourceref()}),
                        sub:None,sup:None
                    })),
                MathNodeList::Over {top,sep,bottom,left,right} =>
                    ET::Stomach::add_node_m(engine, MathNode::Over {
                        start,end:engine.mouth.current_sourceref(),
                        top:top.into(),bottom:bottom.into(),sep,left,right
                    }),
                MathNodeList::EqNo {..} => todo!("throw error")
            }
            engine.state.pop(engine.aux,engine.mouth);
        }
        _ => todo!("error")
    }
}

fn do_superscript<ET:EngineTypes>(engine:&mut EngineReferences<ET>) {
    do_xscript(engine,Script::Super)
}

fn do_subscript<ET:EngineTypes>(engine:&mut EngineReferences<ET>) {
    do_xscript(engine,Script::Sub)
}

pub(crate) enum Script {
    Super,Sub
}
impl Script {
    pub fn invalid<ET:EngineTypes>(&self, a:&MathAtom<ET,UnresolvedMathFontStyle<ET>>) -> bool {
        match self {
            Script::Super => a.sup.is_some(),
            Script::Sub => a.sub.is_some()
        }
    }
    pub fn merge<ET:EngineTypes>(self,n:MathNode<ET,UnresolvedMathFontStyle<ET>>,a:&mut MathAtom<ET,UnresolvedMathFontStyle<ET>>) {
        match self {
            Script::Sub => a.sub = Some(vec!(n).into()),
            Script::Super => a.sup = Some(vec!(n).into())
        }
    }
    pub fn tp<ET:EngineTypes>(&self) -> ListTarget<ET,MathNode<ET,UnresolvedMathFontStyle<ET>>> {
        match self {
            Script::Super => ListTarget::<ET,_>::new(
                |engine,children,_| if let Some(NodeList::Math{children:ch,..}) = engine.stomach.data_mut().open_lists.last_mut() {
                    if let Some(MathNode::Atom(a)) = ch.list_mut().last_mut() {
                        a.sup = Some(children.into())
                    } else {unreachable!()}
                } else {unreachable!()}
            ),
            _ => ListTarget::<ET,_>::new(
                |engine,children,_| if let Some(NodeList::Math{children:ch,..}) = engine.stomach.data_mut().open_lists.last_mut() {
                    if let Some(MathNode::Atom(a)) = ch.list_mut().last_mut() {
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
            match children.list_mut().last() {
                Some(MathNode::Atom(a)) if script.invalid(a) => {
                    todo!("throw double xscript error")
                },
                Some(MathNode::Atom(_)) => (),
                _ => children.push(MathNode::Atom(MathAtom::empty())),
            }
        }
        _ => unreachable!()
    }
    engine.read_char_or_math_group(|script,engine,c| {
        match engine.stomach.data_mut().open_lists.last_mut() {
            Some(NodeList::Math { children, .. }) => {
                match children.list_mut().last_mut() {
                    Some(MathNode::Atom(a)) =>
                        script.merge(MathNode::Atom(c.to_atom()),a),
                    _ => unreachable!()
                }
            }
            _ => unreachable!()
        }
    },|script| script.tp(),script)
}

/// Default implementation for [`Stomach::close_box`].
pub fn close_box<ET:EngineTypes>(engine:&mut EngineReferences<ET>, bt:BoxType) {
    match engine.stomach.data_mut().open_lists.last() {
        Some(NodeList::Horizontal {tp:HorizontalNodeListType::Paragraph(_),..}) => ET::Stomach::close_paragraph(engine),
        _ => ()
    }
    match engine.stomach.data_mut().open_lists.pop() {
        Some(NodeList::Vertical {children,tp:VerticalNodeListType::VAdjust}) if bt == BoxType::Vertical => {
            engine.state.pop(engine.aux,engine.mouth);
            engine.stomach.data_mut().vadjusts.extend(children.into_iter())
        }
        Some(NodeList::Vertical {children,tp:VerticalNodeListType::Insert(n)}) if bt == BoxType::Vertical => {
            engine.state.pop(engine.aux,engine.mouth);
            match engine.stomach.data_mut().mode() {
                TeXMode::Vertical => ET::Stomach::add_node_v(engine, VNode::Insert(n, children.into())),
                TeXMode::Horizontal if engine.stomach.data_mut().open_lists.len() == 1 => ET::Stomach::add_node_h(engine, HNode::Insert(n, children.into())),
                _ => engine.stomach.data_mut().inserts.push((n,children.into()))
            }
        }
        Some(NodeList::Vertical {children,tp:VerticalNodeListType::Box(info,start,target)})  if bt == BoxType::Vertical => {
            engine.state.pop(engine.aux,engine.mouth);
            let bx = TeXBox::V {
                children:children.into(),info,start,end:engine.mouth.current_sourceref(),
            };
            add_box(engine, bx, target)
        }
        Some(NodeList::Vertical {children,tp:VerticalNodeListType::VCenter(start,scaled)}) if bt == BoxType::Vertical => {
            engine.state.pop(engine.aux,engine.mouth);
            ET::Stomach::add_node_m(engine, MathNode::Atom(MathAtom {
                nucleus: MathNucleus::VCenter {children:children.into(),start,end:engine.mouth.current_sourceref(),scaled},
                sub:None,sup:None
            }))
        }
        Some(NodeList::Horizontal {children,tp:HorizontalNodeListType::Box(info,start,target)}) if bt == BoxType::Horizontal => {
            engine.state.pop(engine.aux,engine.mouth);
            let bx = TeXBox::H {
                children:children.into(),info,start,end:engine.mouth.current_sourceref(),preskip:None
            };
            add_box(engine, bx, target)
        }
        o =>
            todo!("throw error: {:?}",o),
    }
    match engine.stomach.data_mut().mode() {
        TeXMode::Vertical => {
            let data = engine.stomach.data_mut();
            let inserts = std::mem::take(&mut data.inserts);
            data.page.extend(inserts.into_iter().map(|(a,b)| VNode::Insert(a,b)));
            let adjusts = std::mem::take(&mut data.vadjusts);
            data.page.extend(adjusts.into_iter());
        }
        TeXMode::Horizontal => {
            let adjusts = std::mem::take(&mut engine.stomach.data_mut().vadjusts);
            ET::Stomach::add_node_h(engine, HNode::VAdjust(adjusts.into()))
        }
        _ => ()
    }
}

/// Default implementation for [`Stomach::add_node_v`].
pub fn add_node_v<ET:EngineTypes>(engine:&mut EngineReferences<ET>, mut node: VNode<ET>) {
    let data = engine.stomach.data_mut();
    let prevdepth = data.prevdepth;

    if let VNode::HRule {..} = node {
        data.prevdepth = ET::Dim::from_sp(-65536000);
    } else {
        data.prevdepth = node.depth();
    }

    let ht = node.height();

    let pre = match node {
        VNode::Box(TeXBox::H {ref mut preskip,..}) => {
            if prevdepth > ET::Dim::from_sp(-65536000) {
                let baselineskip = engine.state.get_primitive_skip(PRIMITIVES.baselineskip);
                let lineskiplimit = engine.state.get_primitive_dim(PRIMITIVES.lineskiplimit);
                let b = Skip::new(baselineskip.base - prevdepth - ht, baselineskip.stretch, baselineskip.shrink);
                let sk = if b.base >= lineskiplimit { b }
                else {
                    engine.state.get_primitive_skip(PRIMITIVES.lineskip)
                };
                if sk != Skip::default() {
                    *preskip = Some(sk);
                    Some(sk)
                } else {None}
            } else {None}
        }
        _ => None
    };

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
            VNode::Box(_) | VNode::Insert(..) | VNode::HRule {..} => {
                //crate::debug_log!(debug => "Here: {} \n\n {}",node.readable(),engine.mouth.display_position());
                data.page_contains_boxes = true;
                data.pagegoal = engine.state.get_primitive_dim(PRIMITIVES.vsize);
            }
            n if n.discardable() => return,
            _ => ()
        }
    }

    if let Some(pre) = pre {
        data.pagetotal = data.pagetotal + pre.base;
    }
    data.pagetotal = data.pagetotal + ht + node.depth(); // ?
    if let VNode::Penalty(i) = node {
        data.lastpenalty = i;
        if i <= -10000 {
            if data.page_contains_boxes {
                return ET::Stomach::maybe_do_output(engine, Some(i))
            } else { return }
        }
    }
    let disc = node.discardable();
    data.page.push(node);
    if disc && data.lastpenalty < 1000 {ET::Stomach::maybe_do_output(engine, None)}
}

/// Default implementation for [`Stomach::do_output`].
pub fn do_output<ET:EngineTypes>(engine:&mut EngineReferences<ET>, caused_penalty:Option<i32>) {
    let data = engine.stomach.data_mut();
    let page = std::mem::take(&mut data.page);
    let goal = data.pagegoal;
    data.pagetotal = <ET as EngineTypes>::Dim::default();
    data.page_contains_boxes = false;
    // TODO more precisely
    engine.state.set_primitive_int(engine.aux,PRIMITIVES.badness,(10).into(),true);

    let SplitResult{mut first,rest,split_penalty} = match caused_penalty {
        Some(p) => SplitResult{first:page.into(),rest:vec!().into(),split_penalty:Some(p)},
        _ => ET::Stomach::split_vertical(engine, page, goal)
    };

    let split_penalty = split_penalty.unwrap_or(0);

    engine.state.push(engine.aux,GroupType::Character,engine.mouth.line_number());

    let data = engine.stomach.data_mut();

    data.open_lists.push(NodeList::Vertical {
        tp: VerticalNodeListType::Page,
        children: vec!()
    });

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
            let children: Box<[VNode<ET>]> = match engine.state.take_box_register(*n) {
                Some(TeXBox::V {children,..}) => {
                    let mut c = children.into_vec();
                    c.extend(std::mem::replace(v, vec!().into()).into_vec().into_iter());
                    c.into()
                },
                _ => std::mem::replace(v, vec!().into())
            };
            engine.state.set_box_register(engine.aux,*n,Some(TeXBox::V {
                info: VBoxInfo::new_box(ToOrSpread::None),
                children,
                start:engine.mouth.current_sourceref(),
                end:engine.mouth.current_sourceref(),
            }),true);
            deletes.push(i)
        }
        _ => ()
    }}
    let mut j = 0;
    for i in deletes { first.remove(i - j);j += 1; }

    engine.state.set_primitive_int(engine.aux,PRIMITIVES.outputpenalty,split_penalty.into(),true);

    let bx = TeXBox::V {
        children:first.into(),
        info:VBoxInfo::new_box(ToOrSpread::None),
        start:engine.mouth.current_sourceref(),
        end:engine.mouth.current_sourceref(),
    };

    //crate::debug_log!(debug => "Here: {} \n\n {}",bx.readable(),engine.mouth.display_position());

    engine.state.set_box_register(engine.aux,255,Some(bx),false);

    engine.push_every(PRIMITIVES.output);
    engine.get_next(); // '{':BeginGroup

    //crate::debug_log!(debug => "Here: {} at {}",engine.mouth.display_position(),engine.preview());

    let depth = engine.state.get_group_level();
    while let Some(next) = engine.get_next() {
        //println!("HERE: {}",engine.preview());
        if engine.state.get_group_level() == depth && next.command_code() == CommandCode::EndGroup {
            engine.state.pop(engine.aux,engine.mouth);
            match engine.stomach.data_mut().open_lists.pop() {
                Some(NodeList::Vertical {children,tp:VerticalNodeListType::Page}) => {
                    engine.stomach.data_mut().pagegoal = <ET as EngineTypes>::Dim::from_sp(i32::MAX);
                    for c in children {
                        ET::Stomach::add_node_v(engine, c);
                    }
                    for r in rest.into_iter() { ET::Stomach::add_node_v(engine, r) }
                }
                _ => todo!("throw error")
            }
            engine.stomach.data_mut().in_output = false;
            return
        }
        if next.is_primitive() == Some(PRIMITIVES.noexpand) {
            engine.get_next();continue
        }
        crate::expand!(ET;engine,next;
            ResolvedToken::Tk { char, code } => do_char(engine, next, char, code),
            ResolvedToken::Cmd(Some(TeXCommand::Char {char, code})) => do_char(engine, next, *char, *code),
            ResolvedToken::Cmd(None) =>tex_error!(engine,undefined,next),
            ResolvedToken::Cmd(Some(cmd)) => crate::do_cmd!(ET;engine,next,cmd)
        );
    }
    todo!("file end")
}

/// The result of splitting a vertical list
pub struct SplitResult<ET:EngineTypes> {
    /// The nodes before the split
    pub first:Vec<VNode<ET>>,
    /// The remaining nodes after the split
    pub rest:Vec<VNode<ET>>,
    /// The penalty at the split (currently not properly implemented)
    pub split_penalty:Option<i32>
}

/// Default implementation for [`Stomach::split_vertical`]. Not TeX accurate,
/// but should be good enough for most purposes, if we don't insinst on precise pagination
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
            VNode::Insert(_,bx) => {
                target = target - bx.iter().map(|c| c.height() + c.depth()).sum(); // - n.depth() ?
                if target < ET::Dim::default() {
                    split = i;
                    break
                }
            }
            _ => {
                target = target - (n.height() + n.depth()); // - n.depth() ?
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

/// Specification of a (target)line in a paragraph
#[derive(Debug,Clone,Eq,PartialEq)]
pub struct ParLineSpec<ET:EngineTypes> {
    /// `\leftskip`
    pub leftskip:Skip<ET::Dim>,
    /// `\rightskip`
    pub rightskip:Skip<ET::Dim>,
    /// The target width of the line
    pub target:ET::Dim
}
impl<ET:EngineTypes> ParLineSpec<ET> {
    /// Creates the relevant [`ParLineSpec`]s from the current state, by looking
    /// at the current `\parshape`, `\hangindent`, `\hangafter`, `\hsize`,
    /// `\leftskip` and `\rightskip`.
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
                    target:hsize + -(leftskip.base + rightskip.base),
                    leftskip,rightskip
                })
            } else {
                if hangafter < 0 {
                    let mut r: Vec<ParLineSpec<ET>> = (0..-hangafter).map(|_| ParLineSpec {
                        target:hsize - (leftskip.base + rightskip.base + hangindent),
                        leftskip:leftskip + hangindent,rightskip
                    }).collect();
                    r.push(ParLineSpec {
                        target:hsize - (leftskip.base + rightskip.base),
                        leftskip,rightskip
                    });
                    r
                } else {
                    let mut r: Vec<ParLineSpec<ET>> = (0..hangafter).map(|_| ParLineSpec {
                        target:hsize - (leftskip.base + rightskip.base),
                        leftskip,rightskip
                    }).collect();
                    r.push(ParLineSpec {
                        target:hsize - (leftskip.base + rightskip.base + hangindent),
                        leftskip:leftskip + hangindent,rightskip
                    });
                    r
                }
            }
        } else {
            parshape.into_iter().map(|(i,l)| {
                let left = leftskip + i;
                let target = l - (leftskip.base + rightskip.base);
                let right = rightskip + (hsize - (l + i));
                ParLineSpec {
                    leftskip:left,rightskip:right,target
                }
            }).collect()
        }
    }
}

/// The result of breaking a paragraph into lines - either an actual line (horizontal box)
/// or vertical material inserted via `\vadjust`.
pub enum ParLine<ET:EngineTypes> {
    Line(TeXBox<ET>),//{contents:Vec<TeXNode<ET>>, broken_early:bool },
    Adjust(VNode<ET>)
}

/// Rough implementation of paragraph breaking
pub fn split_paragraph_roughly<ET:EngineTypes>(engine:&mut EngineReferences<ET>, specs:Vec<ParLineSpec<ET>>, children:Vec<HNode<ET>>,start:SourceReference<<ET::File as File>::SourceRefID>) -> Vec<ParLine<ET>> {
    let mut ret : Vec<ParLine<ET>> = Vec::new();
    let mut hgoals = specs.into_iter();
    let mut nodes = children.into_iter();
    let mut line_spec = hgoals.next().unwrap();
    let mut target = line_spec.target;
    let mut currstart = start;
    let mut currend = currstart.clone();
    let mut curr_height = ET::Dim::default();
    let mut curr_depth = ET::Dim::default();
    'A:loop {
        let mut line = vec!();
        let mut reinserts = vec!();

        macro_rules! next_line {
            ($b:literal) => {
                if !line.is_empty() {
                    let start = std::mem::replace(&mut currstart,currend.clone());
                    ret.push(ParLine::Line(TeXBox::H {children:line.into(),start,end:engine.mouth.current_sourceref(),info:HBoxInfo::ParLine {
                        spec:line_spec.clone(),
                        ends_with_line_break:$b,
                        inner_height:std::mem::take(&mut curr_height),
                        inner_depth:std::mem::take(&mut curr_depth)
                    },preskip:None}));
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

        loop {
            match nodes.next() {
                None => {
                    if !line.is_empty() {
                        let start = std::mem::replace(&mut currstart,currend.clone());
                        ret.push(ParLine::Line(TeXBox::H {children:line.into(),start,end:currend.clone(),info:HBoxInfo::ParLine {
                            spec:line_spec.clone(),
                            ends_with_line_break:false,
                            inner_height:std::mem::take(&mut curr_height),
                            inner_depth:std::mem::take(&mut curr_depth)
                        },preskip:None}));
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
                Some(HNode::MathGroup(g@MathGroup {display:Some(_),..})) => {
                    let ht = g.height();
                    let dp = g.depth();
                    let (a,b) = g.display.unwrap();
                    next_line!(false);
                    ret.push(ParLine::Line(TeXBox::H {start:g.start.clone(),end:g.end.clone(),children:vec!(HNode::MathGroup(g)).into(),info:HBoxInfo::ParLine {
                        spec:line_spec.clone(),
                        ends_with_line_break:false,
                        inner_height:ht + dp + a.base + b.base,
                        inner_depth:ET::Dim::default()
                    },preskip:None}));
                    continue 'A
                }
                Some(HNode::Penalty(i)) if i <= -10000 => {
                    next_line!(true); // TODO mark somehow
                    continue 'A
                }
                Some(HNode::Penalty(_)) => (),
                Some(n@HNode::Space | n@HNode::Hss | n@HNode::HSkip(_) | n@HNode::HFil | n@HNode::HFill) if target <= ET::Dim::default() => {
                    line.push(n);
                    break
                }
                Some(node) => {
                    match node.sourceref() {
                        Some((_,b)) => currend = b.clone(),
                        _ => ()
                    }
                    target = target + (-node.width());
                    curr_height = curr_height.max(node.height());
                    curr_depth = curr_depth.max(node.depth());
                    line.push(node);
                }
            }
        }
        next_line!(false);
    }
    ret
}