use crate::engine::state::State;
use crate::engine::mouth::MouthTrait;
use crate::{cmtodo, debug_log, register_assign, register_conditional, register_dim, register_int, register_int_assign, register_muskip, register_skip, register_tok_assign, register_expandable, catch_prim, file_end_prim, throw, file_end, expand_until_group, get_expanded_group, register_expandable_notk};
use crate::engine::{EngineRef, EngineType};
use crate::tex::catcodes::CategoryCode;
use crate::tex::commands::{BaseCommand, BaseStomachCommand, Command, CommandSource, Def, TokenCont};
use crate::tex::numbers::{Frac, MuSkip, Numeric, Skip};
use crate::tex::token::{BaseToken, TokenList};
use crate::utils::strings::CharType;
use crate::tex::numbers::Int;
use crate::engine::filesystem::File;
use crate::engine::stomach::Stomach;
use crate::tex::fonts::Font;
use crate::tex::nodes::NodeTrait;
use crate::utils::errors::TeXError;
use super::tex::{global,long,outer,def,edef,gdef,xdef,get_csname};

static PMTDC:[u8;5] = [b'+',b'-',b'*',b'/',b')'];
static PMTD:[u8;4] = [b'+',b'-',b'*',b'/'];
static TD:[u8;2] = [b'*',b'/'];
fn expr_scale_loop<ET:EngineType>(engine: &mut EngineRef<ET>, cmd:&CommandSource<ET>, name:&'static str) -> Frac {
    let mut stack: Vec<(Option<Frac>,Option<fn(Frac,Frac) -> Frac>)> = vec!((None,None));
    'a: loop {
        engine.skip_whitespace();
        if engine.is_next_char(b'(') {
            stack.push((None,None));
            continue 'a;
        }
        let mut first = Frac::new(engine.get_int().to_i64(),1);
        loop {
            engine.skip_whitespace();
            let kw = if stack.len()>1 {engine.is_next_char_one_of(&PMTDC)}
                else {engine.is_next_char_one_of(&TD)};
            match kw {
                None => {
                    match stack.last_mut() {
                        Some((Some(f),Some(op))) => {
                            first = op(*f,first);
                            *stack.last_mut().unwrap() = (None,None);
                        }
                        _ => ()
                    }
                    match stack.pop() {
                        Some((None,None)) if stack.is_empty() => return first,
                        _ => throw!("Expected ')'" => cmd.cause.clone())
                    }
                }
                Some(b')' ) => {
                    match stack.last_mut() {
                        Some((Some(f),Some(op))) => {
                            first = op(*f,first);
                            *stack.last_mut().unwrap() = (None,None);
                        }
                        _ => ()
                    }
                    stack.pop();
                    match stack.last_mut() {
                        Some((Some(f),Some(op))) => {
                            first = op(*f,first);
                            *stack.last_mut().unwrap() = (None,None);
                        }
                        _ => ()
                    }
                }
                Some(b'+') => {
                    match stack.last_mut() {
                        Some((Some(f),Some(op))) => {
                            first = op(*f,first);
                            *stack.last_mut().unwrap() = (None,None);
                        }
                        _ => ()
                    }
                    *stack.last_mut().unwrap() = (Some(first),Some(|a,b| a + b));
                    continue 'a;
                }
                Some(b'-') => {
                    match stack.last_mut() {
                        Some((Some(f),Some(op))) => {
                            first = op(*f,first);
                            *stack.last_mut().unwrap() = (None,None);
                        }
                        _ => ()
                    }
                    *stack.last_mut().unwrap() = (Some(first),Some(|a,b| a - b));
                    continue 'a;
                }
                Some(b'*') => {
                    let ret = expr_scale_loop::<ET>(engine,cmd,&name);
                    first = first.scale(ret.0,ret.1);
                    match stack.last_mut() {
                        Some((Some(f),Some(op))) => {
                            first = op(*f,first);
                            *stack.last_mut().unwrap() = (None,None);
                        }
                        _ => ()
                    }
                }
                Some(b'/') => {
                    let ret = expr_scale_loop::<ET>(engine,cmd,&name);
                    first = first.scale(ret.1,ret.0);
                    match stack.last_mut() {
                        Some((Some(f),Some(op))) => {
                            first = op(*f,first);
                            *stack.last_mut().unwrap() = (None,None);
                        }
                        _ => ()
                    }
                }
                _ => unreachable!()
            }
        }
    }
}

fn expr_loop<ET:EngineType,Num:Numeric>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>, name:&'static str,
                                        get:fn(&mut EngineRef<ET>) -> Num) -> Num {
    let mut stack: Vec<(Option<Num>, Option<fn(Num, Num) -> Num>)> = vec!((None, None));
    'a: loop {
        engine.skip_whitespace();
        if engine.is_next_char(b'(') {
            stack.push((None, None));
            continue 'a;
        }
        let mut first = get(engine);
        loop {
            engine.skip_whitespace();
            let kw = if stack.len()>1 {engine.is_next_char_one_of(&PMTDC)}
                else {engine.is_next_char_one_of(&PMTD)};
            match kw {
                None => {
                    match stack.last_mut() {
                        Some((Some(second), Some(op))) => {
                            first = op(second.clone(),first);
                            *stack.last_mut().unwrap() = (None,None);
                        }
                        _ => ()
                    }
                    match stack.pop() {
                        Some((None, None)) if stack.is_empty() => return first,
                        _ => throw!("Expected ')'" => cmd.cause.clone())
                    }
                }
                Some(b')') => {
                    match stack.pop() {
                        Some((Some(second), Some(op))) => {
                            first = op(second.clone(),first);
                        }
                        _ => ()
                    }
                }
                Some(b'+') => {
                    match stack.last_mut() {
                        Some((Some(second), Some(op))) => {
                            first = op(second.clone(),first);
                            *stack.last_mut().unwrap() = (None,None);
                        }
                        _ => ()
                    }
                    *stack.last_mut().unwrap() = (Some(first), Some(|a, b| a + b));
                    continue 'a;
                }
                Some(b'-') => {
                    match stack.last_mut() {
                        Some((Some(second), Some(op))) => {
                            first = op(second.clone(),first);
                            *stack.last_mut().unwrap() = (None,None);
                        }
                        _ => ()
                    }
                    *stack.last_mut().unwrap() = (Some(first), Some(|a, b| a - b));
                    continue 'a;
                }
                Some(b'*') => {
                    let ret = expr_scale_loop::<ET>(engine,cmd,&name);
                    first = first.scale(ret.0,ret.1);
                    match stack.last_mut() {
                        Some((Some(second), Some(op))) => {
                            first = op(second.clone(),first);
                            *stack.last_mut().unwrap() = (None,None);
                        }
                        _ => ()
                    }
                }
                Some(b'/') => {
                    let ret = expr_scale_loop::<ET>(engine,cmd,&name);
                    first = first.scale(ret.1,ret.0);
                    match stack.last_mut() {
                        Some((Some(second), Some(op))) => {
                            first = op(second.clone(),first);
                            *stack.last_mut().unwrap() = (None,None);
                        }
                        _ => ()
                    }
                }
                _ => unreachable!()
            }
        }
    }
}

// --------------------------------------------------------------------------------------------------

pub const DETOKENIZE: &str = "detokenize";
/// `\detokenize`: convert a token list into a string of [`CategoryCode`] [`Other`](CategoryCode::Other)
/// (except for ` `, which gets code [`Space`](CategoryCode::Space)).
pub fn detokenize<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>, f:TokenCont<ET>) {
    debug_log!(trace=>"detokenize");
    let esc = engine.state.get_escapechar();
    let cc = engine.state.get_catcode_scheme().clone();

    expand_until_group!(engine,next => match &next.base {
        BaseToken::Char(c,CategoryCode::Parameter) => {
            engine.token_to_others(&next,true,f);
            engine.token_to_others(&next,true,f);
        }
        _ => engine.token_to_others(&next,true,f)
    });
}

pub const DIMEXPR: &str = "dimexpr";
/// `\dimexpr`: evaluate a dimension expression; returns a [`Dim`].
pub fn dimexpr<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) -> ET::Dim {
    debug_log!(trace=>"dimexpr: {}",engine.preview(100));
    let ret = expr_loop::<ET,ET::Dim>(engine, &cmd, DIMEXPR, |e| e.get_dim());
    if let Some((next,_)) = engine.get_next_token() {
        match &next.base {
            BaseToken::CS(name) => match engine.state.get_command(name).map(|c| &c.base) {
                Some(BaseCommand::Relax) => (),
                _ => engine.mouth.requeue(next)
            }
            _ => engine.mouth.requeue(next)
        }
    }
    ret
}

pub const E_TEX_REVISION: &str = ".6";
pub const E_TEX_VERSION: i64 = 2;

/// `\etexrevision`: expands to the eTeX revision number (`.6`).
pub fn eTeXrevision<ET:EngineType>(engine:&mut EngineRef<ET>, f:TokenCont<ET>) {
    engine.string_to_tokens(E_TEX_REVISION.as_bytes(),f)
}

pub const ETEXVERSION: &str = "eTeXversion";
/// `\eTeXversion`: returns the eTeX version number as an [`Int`] (2).
pub fn eTeXversion<ET:EngineType>(cmd:&CommandSource<ET>) -> ET::Int {
    ET::Int::from_i64(E_TEX_VERSION)
}

pub const EXPANDED: &str = "expanded";
/// `\expanded`: expands its argument exhaustively.
pub fn expanded<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>, f:TokenCont<ET>) {
    debug_log!(trace=>"expanded");

    get_expanded_group!(engine,false,false,false,t => f(engine,t));
    //catch_prim!(engine.get_expanded_group(false,false,false,f) => (EXPANDED,cmd));
}


pub fn fontchardp<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) -> ET::Dim {
    let fnt = engine.get_font();
    let char = engine.get_char();
    fnt.char_dp(char)
}
pub fn fontcharht<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) -> ET::Dim {
    let fnt = engine.get_font();
    let char = engine.get_char();
    fnt.char_ht(char)
}
pub fn fontcharic<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) -> ET::Dim {
    let fnt = engine.get_font();
    let char = engine.get_char();
    fnt.char_ic(char)
}
pub fn fontcharwd<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) -> ET::Dim {
    let fnt = engine.get_font();
    let char = engine.get_char();
    fnt.char_wd(char)
}

pub const GLUEEXPR: &str = "glueexpr";
/// `\glueexpr`: evaluate a glue expression; returns a [`Skip`].
pub fn glueexpr<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) -> Skip<ET::SkipDim> {
    debug_log!(trace=>"glueexpr: {}",engine.preview(100));
    let ret = expr_loop::<ET,Skip<ET::SkipDim>>(engine, &cmd, GLUEEXPR, |e| e.get_skip());
    if let Some((next,_)) = engine.get_next_token() {
        match &next.base {
            BaseToken::CS(name) => match engine.state.get_command(name).map(|c| &c.base) {
                Some(BaseCommand::Relax) => (),
                _ => engine.mouth.requeue(next)
            }
            _ => engine.mouth.requeue(next)
        }
    }
    ret
}

/// `\ifcsname`: expands [`Token`]s like [`\csname`](super::tex::csname) would (i.e. until
/// [`\endcsname`](super::tex::endcsname)) and evaluates to true if the resulting control sequence
/// is defined. Unlike [`\csname`](super::tex::csname), does not define the control sequence as
/// [`\relax`](super::tex::relax)
pub fn ifcsname<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) -> bool {
    debug_log!(trace=>"ifcsname");
    let name = get_csname::<ET>(engine,&cmd,"ifcsname");
    engine.state.get_command(&name).is_some()
}

pub const IFDEFINED: &str = "ifdefined";
/// `\ifdefined`: evaluates to true if the next token is a control sequence that is defined.
pub fn ifdefined<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) -> bool {
    debug_log!(trace=>"ifdefined");
    match engine.get_next_token() {
        None => file_end_prim!("ifdefined",cmd),
        Some((t,_)) => match t.base {
            BaseToken::Char(c,CategoryCode::Active) =>
                engine.state.get_ac_command(&c).is_some(),
            BaseToken::CS(name) => {
                debug_log!(trace => "ifdefined: {}",name.to_str(&engine.interner));
                engine.state.get_command(&name).is_some()
            }
            _ => throw!("Expected a control sequence, got: {:?}",t => cmd.cause)
        }
    }
}

pub const IFFONTCHAR: &str = "iffontchar";
/// `\iffontchar`
pub fn iffontchar<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) -> bool {
    debug_log!(trace=>"iffontchar");
    let fnt = engine.get_font();
    let char = engine.get_char();
    fnt.exists(char)
}

pub fn lastnodetype<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) -> ET::Int {
    let ls = match engine.stomach.shipout_data().box_stack.last() {
        None => &engine.stomach.shipout_data().page,
        Some(b) => b.ls()
    };
    match ls.last() {
        None => ET::Int::from_i64(-1),
        Some(e) => ET::Int::from_i64(e.nodetype() as i64)
    }
    /*
    -1: none (empty list)    8: disc node
    0: char node             9: whatsit node
    1: hlist node           10: math node
    2: vlist node           11: glue node
    3: rule node            12: kern node
    4: ins node             13: penalty node
    5: mark node            14: unset node
    6: adjust node          15: math mode nodes
    7: ligature node
     */
}

pub const MUEXPR: &str = "muexpr";
/// `\muexpr`: evaluate a mu expression; returns a [`MuSkip`].
pub fn muexpr<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>)
                             -> MuSkip<ET::MuDim,ET::MuStretchShrinkDim> {
    debug_log!(trace=>"muexpr: {}",engine.preview(100));
    let ret = expr_loop::<ET,MuSkip<ET::MuDim,ET::MuStretchShrinkDim>>(engine, &cmd, MUEXPR, |e| e.get_muskip());
    if let Some((next,_)) = engine.get_next_token() {
        match &next.base {
            BaseToken::CS(name) => match engine.state.get_command(name).map(|c| &c.base) {
                Some(BaseCommand::Relax) => (),
                _ => engine.mouth.requeue(next)
            }
            _ => engine.mouth.requeue(next)
        }
    }
    ret
}

pub const NUMEXPR: &str = "numexpr";
/// `\numexpr`: evaluate a number expression; returns an [`Int`].
pub fn numexpr<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) -> ET::Int {
    debug_log!(trace=>"numexpr: {}",engine.preview(100));
    let ret = expr_loop::<ET,ET::Int>(engine, &cmd, NUMEXPR, |e| e.get_int());
    if let Some((next,_)) = engine.get_next_token() {
        match &next.base {
            BaseToken::CS(name) => match engine.state.get_command(name).map(|c| &c.base) {
                Some(BaseCommand::Relax) => (),
                _ => engine.mouth.requeue(next)
            }
            _ => engine.mouth.requeue(next)
        }
    }
    ret
}


/// `\protected`: make the next control sequence protected (i.e. not expandable).
pub fn protected<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>, global_:bool, _:bool, long_:bool, outer_:bool) {
    debug_log!(trace => "\\protected");
    match engine.get_next_stomach_command() {
        None => file_end_prim!("protected",cmd),
        Some(c) => match c.command {
            BaseStomachCommand::Assignment {name:Some("global"),..} => global::<ET>(engine,cmd,global_,true,long_,outer_),
            BaseStomachCommand::Assignment {name:Some("protected"),..} => protected::<ET>(engine,cmd,global_,true,long_,outer_),
            BaseStomachCommand::Assignment {name:Some("long"),..} => long::<ET>(engine,cmd,global_,true,long_,outer_),
            BaseStomachCommand::Assignment {name:Some("outer"),..} => outer::<ET>(engine,cmd,global_,true,long_,outer_),
            BaseStomachCommand::Assignment {name:Some("def"),..} => def::<ET>(engine,cmd,global_,true,long_,outer_),
            BaseStomachCommand::Assignment {name:Some("edef"),..} => edef::<ET>(engine,cmd,global_,true,long_,outer_),
            BaseStomachCommand::Assignment {name:Some("gdef"),..} => gdef::<ET>(engine,cmd,global_,true,long_,outer_),
            BaseStomachCommand::Assignment {name:Some("xdef"),..} => xdef::<ET>(engine,cmd,global_,true,long_,outer_),
            _ => throw!("Expected a macro definition after \\protected" => cmd.cause)
        }
    }
}

pub const READLINE: &str = "readline";
/// `\readline`: read a line from a file like [`\read`](super::tex::read), but with all characters
/// having [`CategoryCode`] [12 (`Other`)](CategoryCode::Other) (except for ` ` [`Space`](CategoryCode::Space))
pub fn readline<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>, globally:bool) {
    debug_log!(trace=>"readline");
    let i = engine.get_int();
    let i : usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => throw!("Invalid file number: {}",i => cmd.cause)
    };
    let file = match engine.state.get_open_in_file(i) {
        None => throw!("File {} not open for reading",i => cmd.cause),
        Some(f) => f
    };
    if !engine.get_keyword("to") {
        throw!("Expected 'to' after \\read" => cmd.cause)
    }
    let newcmd = engine.get_control_sequence();
    let mut ret = vec!();
    file.readline::<ET,_>(&mut engine.interner,|t| ret.push(t));
    debug_log!(trace=>"readline: {} = {}",newcmd.to_str(&engine.interner,Some(ET::Char::backslash())),TokenList(&ret).to_str(&engine.interner));

    let def = Command::new(BaseCommand::Def(Def::simple(ret)),Some(&cmd));
    engine.set_command_for_tk(newcmd,Some(def),globally);
}


pub const SCANTOKENS: &str = "scantokens";
/// `\scantokens`: convert a token list into a string and reparse/tokenize it.
pub fn scantokens<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    use crate::utils::strings::AllCharsTrait;
    debug_log!(trace=>"scantokens");
    let esc = engine.state.get_escapechar();
    let cc = engine.state.get_catcode_scheme().clone();
    let mut ret = String::new();
    expand_until_group!(engine,t => match &t.base {
        BaseToken::Char(_,CategoryCode::Space) => {
            ret.push(' ');
        }
        BaseToken::Char(c,_) => {
            ret.push(c.as_char())
        }
        BaseToken::CS(name) => {
            if let Some(c) = engine.state.get_escapechar() { ret.push(c.as_char()) }
            let str = name.to_str(&engine.interner);
            if str.len() == 1 && *engine.state.get_catcode_scheme().get(&ET::Char::tokenize(str)[0]) != CategoryCode::Letter {
                ret.push_str(str);
            } else {
                ret.push_str(str);
                ret.push(' ')
            }
        }
    });
    engine.mouth.push_string(&ret.into_bytes());
    debug_log!(trace => "scantokens: {}",engine.preview(250));
}

pub const UNEXPANDED: &str = "unexpanded";
/// `\unexpanded`: read a token list from the input stream, but do not expand it (e.g. in [`\edef`](super::tex::edef)).
pub fn unexpanded<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>, f:TokenCont<ET>) {
    debug_log!(trace=>"unexpanded");
    expand_until_group!(engine,tk => f(engine,tk));
    //catch_prim!(engine.expand_until_group(f) => (UNEXPANDED,cmd));
}

/// `\unless`: read a conditional and inverse its result. Notably, this is not itself implemented as a conditional,
/// since otherwise, e.g. `\unless\ifx` in the [false-loop](crate::engine::gullet::methods::false_loop)
/// would be interpreted as *two* conditionals, rather than just one.
pub const UNLESS : &str = "unless";
pub fn unless<ET:EngineType>(engine:&mut EngineRef<ET>, cmd:&CommandSource<ET>) {
    debug_log!(trace=>"unless");
    match engine.get_next_token() {
        None => file_end_prim!(UNLESS,cmd),
        Some((next,true)) => {
            let ncmd = match &next.base {
                BaseToken::CS(name) => engine.state.get_command(name),
                BaseToken::Char(c,CategoryCode::Active) => engine.state.get_ac_command(c),
                _ => throw!("Expected a conditional after \\unless" => cmd.cause)
            };
            let ncmd = match ncmd {
                None => throw!("Expected a conditional after \\unless" => cmd.cause),
                Some(c) => c.clone()
            };
            match ncmd.base {
                BaseCommand::Conditional {name,apply} => {
                    crate::engine::gullet::methods::do_conditional::<ET>(engine,
                        CommandSource{cause:next,reference:ncmd.reference},name,apply,true);
                }
                _ => throw!("Expected a conditional after \\unless" => cmd.cause)
            }
        }
         _ => throw!("Expected a conditional after \\unless" => cmd.cause)
    }
}

/// Initialize a TeX engine with default implementations for all eTeX primitives.
pub fn initialize_etex_primitives<ET:EngineType>(engine:&mut EngineRef<ET>) {
    register_expandable!(detokenize,engine,(e,c,f) =>detokenize::<ET>(e,&c,f));
    register_dim!(dimexpr,engine,(e,c) => dimexpr::<ET>(e,&c));
    register_expandable!(eTeXrevision,engine,(e,_,f) => eTeXrevision::<ET>(e,f));
    register_int!(eTeXversion,engine,(e,c) => eTeXversion::<ET>(&c));
    register_tok_assign!(everyeof,engine);
    register_expandable!(expanded,engine,(e,c,f) => expanded::<ET>(e,&c,f));
    register_dim!(fontchardp,engine,(e,c) => fontchardp::<ET>(e,&c));
    register_dim!(fontcharht,engine,(e,c) => fontcharht::<ET>(e,&c));
    register_dim!(fontcharic,engine,(e,c) => fontcharic::<ET>(e,&c));
    register_dim!(fontcharwd,engine,(e,c) => fontcharwd::<ET>(e,&c));
    register_skip!(glueexpr,engine,(e,c) => glueexpr::<ET>(e,&c));
    register_conditional!(ifcsname,engine,(eu,cmd) =>ifcsname::<ET>(eu,&cmd));
    register_conditional!(ifdefined,engine,(eu,cmd) =>ifdefined::<ET>(eu,&cmd));
    register_conditional!(iffontchar,engine,(e,cmd) =>iffontchar::<ET>(e,&cmd));
    register_int!(lastnodetype,engine,(e,c) => lastnodetype::<ET>(e,&c));
    register_muskip!(muexpr,engine,(e,c) => muexpr::<ET>(e,&c));
    register_int!(numexpr,engine,(e,c) => numexpr::<ET>(e,&c));
    register_assign!(readline,engine,(eu,cmd,global) =>readline::<ET>(eu,&cmd,global));
    register_int_assign!(savinghyphcodes,engine);
    register_expandable_notk!(scantokens,engine,(e,c) =>scantokens::<ET>(e,&c));
    register_int_assign!(tracingassigns,engine);
    register_int_assign!(tracinggroups,engine);
    register_int_assign!(tracingifs,engine);
    register_int_assign!(tracingnesting,engine);
    register_int_assign!(tracingscantokens,engine);
    register_assign!(protected,engine,(eu,cmd,g) =>protected::<ET>(eu,&cmd,g,false,false,false));
    register_expandable!(unexpanded,engine,(e,c,f) => unexpanded::<ET>(e,&c,f));
    register_expandable_notk!(unless,engine,(eu,cmd) =>unless::<ET>(eu,&cmd));

    cmtodo!(engine,beginL);
    cmtodo!(engine,beginR);
    cmtodo!(engine,botmarks);
    cmtodo!(engine,clubpenalties);
    cmtodo!(engine,currentgrouplevel);
    cmtodo!(engine,currentgrouptype);
    cmtodo!(engine,currentifbranch);
    cmtodo!(engine,currentiflevel);
    cmtodo!(engine,currentiftype);
    cmtodo!(engine,displaywidowpenalties);
    cmtodo!(engine,endL);
    cmtodo!(engine,endR);
    cmtodo!(engine,firstmarks);
    cmtodo!(engine,glueshrink);
    cmtodo!(engine,glueshrinkorder);
    cmtodo!(engine,gluestretch);
    cmtodo!(engine,gluestretchorder);
    cmtodo!(engine,gluetomu);
    cmtodo!(engine,interactionmode);
    cmtodo!(engine,interlinepenalties);
    cmtodo!(engine,lastlinefit);
    cmtodo!(engine,marks);
    cmtodo!(engine,middle);
    cmtodo!(engine,mutoglue);
    cmtodo!(engine,pagediscards);
    cmtodo!(engine,parshapedimen);
    cmtodo!(engine,parshapeindent);
    cmtodo!(engine,parshapelength);
    cmtodo!(engine,predisplaydirection);
    cmtodo!(engine,savingvdiscards);
    cmtodo!(engine,showgroups);
    cmtodo!(engine,showifs);
    cmtodo!(engine,showtokens);
    cmtodo!(engine,splitbotmarks);
    cmtodo!(engine,splitdiscards);
    cmtodo!(engine,splitfirstmarks);
    cmtodo!(engine,TeXXeTstate);
    cmtodo!(engine,topmarks);
    cmtodo!(engine,widowpenalties);
}