use crate::{catch, debug_log, throw};
use crate::engine::{EngineRef, EngineType};
use crate::engine::state::modes::{BoxMode, FontStyle, GroupType, TeXMode};
use crate::engine::state::State;
use crate::engine::stomach::{LineSpec, Stomach};
use crate::engine::mouth::Mouth;
use crate::tex::catcodes::CategoryCode;
use crate::tex::commands::{BaseStomachCommand, StomachCommand};
use crate::tex::nodes::{HBox, HorV, MathClass, NodeTrait, OpenBox, OpenKernel, SimpleNode, SkipNode, TeXNode};
use crate::tex::numbers::{Dim, Int, Skip};
use crate::utils::errors::TeXError;
use crate::utils::strings::CharType;
use crate::tex::token::Token;
use std::fmt::Write;
use crate::utils::collections::HMap;

pub fn digest<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:StomachCommand<ET>) {
    use BaseStomachCommand::*;
    debug_log!(trace=>"digesting command {:?} ({})",cmd.command,cmd.source.cause.printable(&engine.interner));
    match cmd.command {
        Assignment {name,set} => {
            set(engine,cmd.source,false);
            match engine.state.take_afterassignment() {
                Some(t) => engine.mouth.requeue(t),
                _ => ()
            }
        }
        ValueAss(set) => {
            set(engine,cmd.source,false);
            match engine.state.take_afterassignment() {
                Some(t) => engine.mouth.requeue(t),
                _ => ()
            }
        }
        Font(f) => {
            engine.state.set_current_font(f,false);
            match engine.state.take_afterassignment() {
                Some(t) => engine.mouth.requeue(t),
                _ => ()
            }
        }
        OpenBox {name, apply, mode} => {
            let on_close = apply(engine,cmd.source);
            engine.stomach.open_box(crate::tex::nodes::OpenBox::Box {
                list:vec!(), mode, on_close
            });
        }
        FinishedBox {name,get} => {
            let b = get(engine,cmd.source);
            ET::Stomach::push_node(engine,b.as_node());
        }
        Whatsit {name,apply} => {
            let wi = apply(engine,cmd.source);
            ET::Stomach::push_node(engine,TeXNode::Whatsit(wi));
        },
        Relax => (),
        _ => match engine.state.mode() {
            TeXMode::Math | TeXMode::Displaymath => digest_math(engine,cmd),
            _ => digest_hv(engine,cmd)
        }
    }
}

fn digest_hv<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:StomachCommand<ET>) {
    use BaseStomachCommand::*;
    match cmd.command {
        ProvidesNode {name,get,ref forces_mode} => {
            match forces_mode {
                None => (),
                Some(mode) => {
                    let currmode = engine.state.mode();
                    match (currmode,mode) {
                        (TeXMode::InternalVertical | TeXMode::Vertical ,HorV::Vertical) => (),
                        (TeXMode::Horizontal | TeXMode::RestrictedHorizontal,HorV::Horizontal) => (),
                        (TeXMode::RestrictedHorizontal,HorV::Vertical) =>
                            throw!("Not allowed in restricted horizontal mode: {}",cmd.source.cause.printable(&engine.interner) => cmd.source.cause),
                        (TeXMode::Vertical|TeXMode::InternalVertical,HorV::Horizontal) => {
                            return ET::Stomach::open_paragraph(engine,cmd);
                        }
                        (TeXMode::Horizontal,HorV::Vertical) => {
                            engine.mouth.requeue(cmd.source.cause);
                            return ET::Stomach::close_paragraph(engine);
                        }
                        _ => throw!("TODO: Switching modes from {:?} to {:?}", currmode, mode => cmd.source.cause)
                    }
                }
            }
            if engine.state.mode() == TeXMode::Vertical {
                ET::Stomach::maybe_shipout(engine,false)
            }
            let node = get(engine, cmd.source);
            ET::Stomach::push_node(engine,node);
            if engine.state.mode() == TeXMode::Vertical {
                ET::Stomach::maybe_shipout(engine,false)
            }
        }
        Unexpandable {name,apply,ref forces_mode} => {
            match forces_mode {
                None => (),
                Some(mode) => {
                    let currmode = engine.state.mode();
                    match (currmode,mode) {
                        (TeXMode::InternalVertical | TeXMode::Vertical ,HorV::Vertical) => (),
                        (TeXMode::Horizontal | TeXMode::RestrictedHorizontal,HorV::Horizontal) => (),
                        (TeXMode::RestrictedHorizontal,HorV::Vertical) =>
                            throw!("Not allowed in restricted horizontal mode: {}",cmd.source.cause.printable(&engine.interner) => cmd.source.cause),
                        (TeXMode::Vertical|TeXMode::InternalVertical,HorV::Horizontal) => {
                            return ET::Stomach::open_paragraph(engine,cmd);
                        }
                        (TeXMode::Horizontal,HorV::Vertical) => {
                            engine.mouth.requeue(cmd.source.cause);
                            return ET::Stomach::close_paragraph(engine);
                        }
                        _ => throw!("TODO: Switching modes from {:?} to {:?}", currmode, mode => cmd.source.cause)
                    }
                }
            }
            apply(engine, cmd.source);
            if engine.state.mode() == TeXMode::Vertical {
                ET::Stomach::maybe_shipout(engine,false)
            }
        }
        Char(char) => match engine.state.mode() {
            TeXMode::Horizontal | TeXMode::RestrictedHorizontal => {
                ET::Stomach::push_node(engine,SimpleNode::Char {char, font:engine.state.get_current_font().clone(),cls:None}.as_node());
            }
            _ => {
                ET::Stomach::open_paragraph(engine,cmd);
            }
        }
        Superscript => throw!("Superscript character not allowed in non-math mode"),
        Subscript => throw!("Subscript character not allowed in non-math mode"),
        Space if engine.state.mode().is_vertical() => (),
        Space => ET::Stomach::push_node(engine,SkipNode::Space.as_node()),
        MathShift => match engine.state.mode() {
            TeXMode::RestrictedHorizontal => do_math(engine),
            TeXMode::Horizontal => {
                match engine.get_next_token() {
                    None => throw!("Unexpected end of input" => cmd.source.cause),
                    Some((t,_)) if t.is_mathshift() => do_display_math(engine),
                    Some((o,_)) => {
                        engine.mouth.requeue(o);
                        do_math(engine)
                    }
                }
            }
            _ => ET::Stomach::open_paragraph(engine,cmd)
        }
        MathChar(num) => throw!("Math character only allowed in math mode" => cmd.source.cause),
        BeginGroup => engine.state.stack_push(GroupType::Token),
        EndGroup => {
            match (engine.state.get_grouptype(),engine.state.mode()) {
                (GroupType::Box(_),TeXMode::Horizontal) => {
                    match engine.stomach.shipout_data().box_stack.last() {
                        Some(crate::tex::nodes::OpenBox::Paragraph {..}) => ET::Stomach::close_paragraph(engine),
                        _ => throw!("There should be a paragraph to close here!")
                    }
                }
                _ => ()
            }
            match engine.state.stack_pop(&mut engine.memory) {
                Some((v,GroupType::Token)) => {
                    if !v.is_empty() {
                        let mut rs = engine.mouth.get_expansion();
                        rs.extend(v.into_iter());
                        engine.mouth.push_expansion(rs);
                    }
                }
                Some((v,GroupType::Box(b))) => {
                    if !v.is_empty() {
                        let mut rs = engine.mouth.get_expansion();
                        rs.extend(v.into_iter());
                        engine.mouth.push_expansion(rs);
                    }
                    match engine.stomach.shipout_data_mut().box_stack.pop() {
                        Some(crate::tex::nodes::OpenBox::Box {list,mode,on_close}) if mode == b => {
                            match on_close(engine,list) {
                                Some(b) => ET::Stomach::push_node(engine,b.as_node()),
                                None => {}
                            }
                        }
                        _ =>throw!("Unexpected box on stack" => cmd.source.cause)
                    }
                    if engine.stomach.shipout_data().box_stack.is_empty() {
                        ET::Stomach::maybe_shipout(engine,false)
                    }
                }
                _ => throw!("Unexpected end group" => cmd.source.cause)
            }
        }
        Assignment {..} | ValueAss(_) | Font(_) | OpenBox {..} | FinishedBox {..} | Whatsit {..} | Relax => unreachable!()
    }
}
fn digest_math<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:StomachCommand<ET>) {
    use BaseStomachCommand::*;
    debug_log!(debug => "Here: {}",engine.preview(500));
    match cmd.command {
        ProvidesNode {name,get,ref forces_mode} => {
            match forces_mode {
                None | Some(HorV::Horizontal) => (),
                Some(HorV::Vertical) => throw!("Not allowed in math mode" => cmd.source.cause)
            }
            let node = get(engine, cmd.source);
            ET::Stomach::push_node(engine,node);
        }
        Unexpandable {name,apply,ref forces_mode} => {
            match forces_mode {
                None | Some(HorV::Horizontal) => (),
                Some(HorV::Vertical) => throw!("Not allowed in math mode" => cmd.source.cause)
            }
            apply(engine, cmd.source)
        }
        Char(char) => {
            let mc = engine.state.get_mathcode(char);
            let num = mc.to_i64() as u32;
            if num == 32768 {
                engine.mouth.requeue(ET::Token::new_char_from_command(char,CategoryCode::Active,&cmd.source));
                return ()
            }
            let (char,font,cls) = do_mathchar::<ET>(&engine.state,num,Some(char));
            ET::Stomach::push_node(engine,SimpleNode::Char {char, font,cls:Some(cls)}.as_node());
        }
        Space => (),
        MathShift => match engine.state.mode() {
            TeXMode::Math => {
                match engine.state.stack_pop(&mut engine.memory) {
                    Some((v,GroupType::Box(BoxMode::M))) => {
                        if !v.is_empty() {
                            let mut rs = engine.mouth.get_expansion();
                            rs.extend(v.into_iter());
                            engine.mouth.push_expansion(rs);
                        }
                        match engine.stomach.shipout_data_mut().box_stack.pop() {
                            Some(crate::tex::nodes::OpenBox::Math {list:ls,display,..}) => {
                                ET::Stomach::push_node(engine,TeXNode::Math{ls,display,cls:None})
                            }
                            _ =>throw!("Unexpected box on stack" => cmd.source.cause)
                        }
                    }
                    _ => throw!("Unexpected end of math mode" => cmd.source.cause)
                }
            }
            TeXMode::Displaymath => {
                match engine.get_next_token() {
                    None => throw!("Unexpected end of input" => cmd.source.cause),
                    Some((t,_)) if t.is_mathshift() => (),
                    Some((o,_)) => {
                        throw!("Unexpected token in displaymath: {}",o.printable(&engine.interner) => cmd.source.cause)
                    }
                }
                match engine.state.stack_pop(&mut engine.memory) {
                    Some((v,GroupType::Box(BoxMode::M))) => {
                        if !v.is_empty() {
                            let mut rs = engine.mouth.get_expansion();
                            rs.extend(v.into_iter());
                            engine.mouth.push_expansion(rs);
                        }
                        match engine.stomach.shipout_data_mut().box_stack.pop() {
                            Some(crate::tex::nodes::OpenBox::Math {list:ls,display,..}) => {
                                ET::Stomach::push_node(engine,TeXNode::Math{ls,display,cls:None})
                            }
                            _ =>throw!("Unexpected box on stack" => cmd.source.cause)
                        }
                    }
                    _ => throw!("Unexpected end of math mode" => cmd.source.cause)
                }
            }
            _ => unreachable!()
        }
        MathChar(num) => {
            let (char,font,cls) = do_mathchar::<ET>(&engine.state,num,None);
            ET::Stomach::push_node(engine,SimpleNode::Char {char, font, cls:Some(cls)}.as_node());
        }
        Superscript => {
            let last = engine.stomach.shipout_data_mut().box_stack.last_mut().unwrap().ls_mut().last_mut();
            match last {
                Some(TeXNode::OpenKernel(_)) =>
                    throw!("Unexpected superscript character: {:?}",last),
                Some(TeXNode::Simple(SimpleNode::WithScripts {superscript:Some(_),..})) =>
                    throw!("Double superscript"),
                None => {
                    ET::Stomach::push_node(engine,TeXNode::OpenKernel(OpenKernel::Superscript(Box::new(
                        TeXNode::Math {ls:vec!(),display:false,cls:None} // TODO display
                    ))))
                }
                Some(n) => {
                    let old = std::mem::replace(n, TeXNode::Mark(0,vec!()));
                    *n = TeXNode::OpenKernel(OpenKernel::Superscript(Box::new(old)))
                }
            }
        }
        Subscript => {
            let last = engine.stomach.shipout_data_mut().box_stack.last_mut().unwrap().ls_mut().last_mut();
            match last {
                Some(TeXNode::OpenKernel(_)) =>
                    throw!("Unexpected subscript character"),
                Some(TeXNode::Simple(SimpleNode::WithScripts {subscript:Some(_),..})) =>
                    throw!("Double subscript"),
                None => {
                    ET::Stomach::push_node(engine,TeXNode::OpenKernel(OpenKernel::Subscript(Box::new(
                        TeXNode::Math {ls:vec!(),display:false,cls:None} // TODO display
                    ))))
                }
                Some(n) => {
                    let old = std::mem::replace(n, TeXNode::Mark(0,vec!()));
                    *n = TeXNode::OpenKernel(OpenKernel::Subscript(Box::new(old)))
                }
            }
        }
        BeginGroup => {
            let mode = match engine.state.mode() {
                TeXMode::Math => BoxMode::M,
                TeXMode::Displaymath => BoxMode::DM,
                _ => unreachable!()
            };
            engine.state.stack_push(GroupType::Box(mode));
            engine.stomach.open_box(crate::tex::nodes::OpenBox::Math {list:vec!(),display:engine.state.get_displaymode(),cls:None});
        }
        EndGroup => {
            match engine.state.stack_pop(&mut engine.memory) {
                Some((v,GroupType::Box(b))) => {
                    if !v.is_empty() {
                        let mut rs = engine.mouth.get_expansion();
                        rs.extend(v.into_iter());
                        engine.mouth.push_expansion(rs);
                    }
                    match engine.stomach.shipout_data_mut().box_stack.pop() {
                        Some(crate::tex::nodes::OpenBox::Math {list:ls,display,cls}) => {
                            ET::Stomach::push_node(engine,TeXNode::Math{ls,display,cls})
                        }
                        o =>throw!("Unexpected box on stack: {:?}",o => cmd.source.cause)
                    }
                }
                _ => throw!("Unexpected end group" => cmd.source.cause)
            }
        }
        Assignment {..} | ValueAss(_) | Font(_) | OpenBox {..} | FinishedBox {..} | Whatsit {..} | Relax => unreachable!()
    }
}

pub fn do_mathchar<ET:EngineType>(state:&ET::State,mathcode:u32,char:Option<ET::Char>) -> (ET::Char,ET::FontRef,MathClass) {
    let (mut cls,mut fam,pos) = {
        if mathcode == 0 {
            (0,0,match char {
                Some(c) => c.as_byte(),
                _ => 0
            })
        } else {
            let char = (mathcode & 0xFF) as u8;           // num % (16 * 16)
            let fam = ((mathcode >> 8) & 0xF) as usize;      // (rest % 16)
            let rest_fam_shifted = (mathcode >> 12) & 0xF;  // (((rest - fam) / 16) % 16)
            (rest_fam_shifted as u8, fam, char)
        }
    };
    if cls == 7 {
        let i = state.get_primitive_int("fam").to_i64();
        match i {//.registers_prim.get(&(crate::commands::registers::FAM.index - 1)) {
            i if i < 0 || i > 15 => {
                cls = 0;
            }
            i => {
                cls = 0;
                fam = i as usize;
            }
        }
    }
    let mode = state.get_fontstyle();
    let font = match mode {
        FontStyle::Text => state.get_textfont(fam),
        FontStyle::Script => state.get_scriptfont(fam),
        FontStyle::Scriptscript => state.get_scriptscriptfont(fam),
    };
    (ET::Char::from(pos),font,MathClass::from(cls))
}

pub fn open_paragraph<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:StomachCommand<ET>) {
    engine.stomach.open_box(OpenBox::Paragraph {list:vec!()});
    engine.state.set_mode(TeXMode::Horizontal);
    match cmd.command {
        BaseStomachCommand::Unexpandable {name:"indent",apply,..} => {
            apply(engine,cmd.source);
        },
        BaseStomachCommand::Unexpandable {name:"noindent",..} => (),
        _ => {
            crate::tex::commands::tex::indent(engine,&cmd.source);
            engine.mouth.requeue(cmd.source.cause);
        }
    }
    engine.mouth.insert_every(&engine.state,"everypar");
}

pub fn do_math<ET:EngineType>(engine:&mut EngineRef<ET>) {
    engine.state.stack_push(GroupType::Box(BoxMode::M));
    engine.stomach.open_box(OpenBox::Math {list:vec!(),display:false,cls:None});
    engine.state.set_displaymode(false,false);
    engine.mouth.insert_every(&engine.state,"everymath");
}
pub fn do_display_math<ET:EngineType>(engine:&mut EngineRef<ET>) {
    engine.state.stack_push(GroupType::Box(BoxMode::DM));
    engine.stomach.open_box(OpenBox::Math {list:vec!(),display:true,cls:None});
    engine.state.set_displaymode(true,false);
    engine.mouth.insert_every(&engine.state,"everydisplay");
}

pub fn close_paragraph<ET:EngineType>(engine:&mut EngineRef<ET>) {
    use crate::tex::numbers::Dim;
    let par = match engine.stomach.shipout_data_mut().box_stack.pop() {
        Some(OpenBox::Paragraph {list}) => list,
        _ => unreachable!()
    };
    if engine.stomach.shipout_data().box_stack.is_empty() {
        engine.state.set_mode(TeXMode::Vertical)
    } else {
        engine.state.set_mode(TeXMode::InternalVertical)
    }
    if par.is_empty() {
        let parshape = engine.state.get_parshape().cloned();
        engine.state.set_primitive_int("hangafter",ET::Int::from_i64(0),false);
        engine.state.set_primitive_dim("hangindent",ET::Dim::from_sp(0),false);
        engine.state.set_parshape(vec!(),false);
        return ()
    }
    let leftskip = engine.state.get_primitive_skip("leftskip");
    let rightskip = engine.state.get_primitive_skip("rightskip");
    let hsize = engine.state.get_primitive_dim("hsize");
    let hangindent = engine.state.get_primitive_dim("hangindent");
    let hangafter = engine.state.get_primitive_int("hangafter").to_i64();
    let parshape = engine.state.get_parshape().cloned();
    engine.state.set_primitive_int("hangafter",ET::Int::from_i64(0),false);
    engine.state.set_primitive_dim("hangindent",ET::Dim::from_sp(0),false);
    engine.state.set_parshape(vec!(),false);

    let specs: Vec<LineSpec<ET>> = match &parshape {
        Some(v) => {
            v.iter().map(|(_,l)| LineSpec{
                target:*l - (leftskip.base + rightskip.base),left_skip:leftskip,right_skip:rightskip
            }).collect()
            //let lines = ET::Stomach::split_paragraph(&engine.fontstore,&engine.state,par,htargets);
            //todo!()
        }
        None if hangindent != ET::Dim::default() && hangafter != 0 => {
            if hangafter < 0 {
                let mut r: Vec<LineSpec<ET>> = (0..-hangafter).map(|x| LineSpec{target:hsize - (leftskip.base + rightskip.base + hangindent),left_skip:leftskip,right_skip:rightskip}).collect();
                r.push(LineSpec{target:hsize - (leftskip.base + rightskip.base),left_skip:leftskip,right_skip:rightskip});
                r
            } else {
                let mut r: Vec<LineSpec<ET>> =
                    (0..hangafter).map(|x| LineSpec{target:hsize - (leftskip.base + rightskip.base),left_skip:leftskip,right_skip:rightskip}).collect();
                r.push(LineSpec{target:hsize - (leftskip.base + rightskip.base + hangindent),left_skip:leftskip,right_skip:rightskip});
                r
            }
            //let lines = ET::Stomach::split_paragraph(&engine.fontstore,&engine.state,par,htargets);
            //todo!()
        }
        _ => vec!(LineSpec{
                target:hsize - (leftskip.base + rightskip.base),left_skip:leftskip,right_skip:rightskip
            })
    };
    let lines = ET::Stomach::split_paragraph(&engine.fontstore,&engine.state,par,specs);
    for mut line in lines {
        line.insert(0,SkipNode::Skip{skip:leftskip,axis:HorV::Horizontal}.as_node());
        line.push(SkipNode::Skip{skip:rightskip,axis:HorV::Horizontal}.as_node());
        ET::Stomach::push_node(engine,HBox {
            kind:"paragraphline",
            children:line,
            ..Default::default()
        }.as_node())
    }
}

pub fn knuth_plass<ET:EngineType>(nodes:Vec<TeXNode<ET>>, mut linespecs: Vec<LineSpec<ET>>) -> Vec<Vec<TeXNode<ET>>> {
    todo!()
}

pub fn split_paragraph_roughly<ET:EngineType>(fs:&ET::FontStore,nodes:Vec<TeXNode<ET>>, mut linespecs: Vec<LineSpec<ET>>) -> Vec<Vec<TeXNode<ET>>> {
    let mut lines = vec!();
    let mut hgoal = ET::Dim::default();
    let mut hgoals = linespecs.into_iter();
    let mut iter = nodes.into_iter();
    let mut reinserts : Vec<TeXNode<ET>> = vec!();
    // TODO vadjust,marks
    'A:loop {
        let mut goal = match hgoals.next() {
            Some(v) => {
                hgoal = v.target;
                v.target
            },
            None => hgoal.clone()
        };
        lines.push(vec!());
        while goal > ET::Dim::default() {
            match iter.next() {
                None => break 'A,
                Some(n@TeXNode::VAdjust(_)) => {
                    reinserts.push(n)
                }
                Some(n@TeXNode::Mark(_,_)) => {
                    reinserts.push(n)
                }
                Some(node) => {
                    // TODO penalty etc
                    goal = goal - node.width(fs);
                    lines.last_mut().unwrap().push(node);
                }
            }
        }
        if !reinserts.is_empty() {
            lines.push(std::mem::take(&mut reinserts))
        }
    }
    lines
}

pub fn split_vertical_roughly<ET:EngineType>(engine: &mut EngineRef<ET>, mut nodes: Vec<TeXNode<ET>>, mut target: ET::Dim) -> (Vec<TeXNode<ET>>, Vec<TeXNode<ET>>) {
    let mut nodes = nodes.into_iter();
    let mut result = vec!();
    let mut rest = vec!();

    let sd = engine.stomach.shipout_data_mut();
    let topmarks = std::mem::take(&mut sd.botmarks);
    sd.topmarks = topmarks;
    sd.firstmarks.clear();
    sd.botmarks.clear();

    while target > ET::Dim::default() {
        match nodes.next() {
            None => break,
            Some(TeXNode::Mark(i,v)) => {
                if !sd.firstmarks.contains_key(&i) {
                    sd.firstmarks.insert(i,v.clone());
                }
                sd.botmarks.insert(i,v.clone());
                result.push(TeXNode::Mark(i,v));
            }
            Some(node) => { // TODO marks
                target = target - node.height(&engine.fontstore);
                if target < ET::Dim::default() {
                    rest.push(node);
                    break
                } else {
                    result.push(node);
                }
            }
        }
    }

    sd.splitfirstmarks.clear();
    sd.splitbotmarks.clear();

    for n in nodes {
        match n {
            TeXNode::Mark(i,ref v) => {
                if !sd.splitfirstmarks.contains_key(&i) {
                    sd.splitfirstmarks.insert(i,v.clone());
                }
                sd.splitbotmarks.insert(i,v.clone());
            }
            _ => ()
        }
        rest.push(n);
    }
    (result,rest)
}