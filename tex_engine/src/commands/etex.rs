use crate::{cmtodo, cmtodos};
use crate::commands::{Command, Macro, MacroSignature};
use crate::engine::{EngineReferences, EngineTypes, TeXEngine};
use crate::engine::gullet::ResolvedToken;
use crate::engine::mouth::Mouth;
use crate::engine::mouth::pretokenized::{Tokenizer, TokenList};
use crate::engine::state::State;
use crate::engine::utils::memory::{MemoryManager, PRIMITIVES};
use crate::tex::catcodes::CommandCode;
use crate::tex::control_sequences::ControlSequenceNameHandler;
use crate::tex::token::{StandardToken, Token};
use super::primitives::*;
use crate::commands::ExpansionContainer;
use std::fmt::Write;
use std::process::Output;
use crate::engine::gullet::Gullet;
use crate::tex::numerics::{Numeric, NumSet};
use crate::engine::filesystem::FileSystem;
use crate::engine::utils::outputs::Outputs;
use crate::tex::nodes::NodeTrait;
use crate::commands::NodeList;
use crate::engine::stomach::Stomach;

pub fn eTeXversion<ET:EngineTypes>(engine:&mut EngineReferences<ET>,tk:ET::Token) -> ET::Int {
    2.into()
}
pub fn eTeXrevision<ET:EngineTypes>(engine: &mut EngineReferences<ET>,exp:&mut Vec<ET::Token>,tk:ET::Token) {
    exp.push(ET::Token::from_char_cat(b'.'.into(),CommandCode::Other));
    exp.push(ET::Token::from_char_cat(b'6'.into(),CommandCode::Other));
}

fn expr_inner<ET:EngineTypes,R:Numeric<<ET::Num as NumSet>::Int>,
>(engine:&mut EngineReferences<ET>,
  byte:fn(&mut EngineReferences<ET>,bool,u8) -> R,
  cmd:fn(&mut EngineReferences<ET>,bool,Command<ET>,ET::Token) -> R
) -> R {
    let (is_negative,r) = crate::engine::gullet::methods::read_numeric(engine, false);
    match r {
        Ok(b) => if b == b'(' {
            let (int,ret) = expr_loop(engine, byte, cmd);
            match ret.to_enum() {
                StandardToken::Character(char,CommandCode::Other) if matches!(char.try_into(),Ok(b')')) =>
                    if is_negative {return -int} else {return int},
                _ => todo!("throw error")
            }
        } else { byte(engine,is_negative,b)},
        Err((c,token)) => cmd(engine,is_negative,c,token)
    }
}

struct Summand<ET:EngineTypes,R:Numeric<ET::Int>>{base:R,times:Vec<ET::Int>,div:Vec<ET::Int>}
impl<ET:EngineTypes,R:Numeric<ET::Int>> Summand<ET,R> {
    fn reduce_trace<O:Outputs>(v:Vec<Self>,trace:bool,output: &O) -> R {
        if trace {
            let mut res = String::new();
            for x in v.iter() {
                write!(res,"{} +",x).unwrap();
            }
            output.write_17(format_args!("\\<X>expr: {}",res))
        }
        let r = v.into_iter().map(|x| x.resolve()).reduce(|a,b| a + b).unwrap();
        if trace {
            output.write_17(format_args!("        = {}",r));
        }
        r
    }
    fn reduce(v:Vec<Self>) -> R {
        v.into_iter().map(|x| x.resolve()).reduce(|a,b| a + b).unwrap()
    }
    fn new(r:R) -> Self {
        Self {base:r,times:vec!(),div:vec!()}
    }
    fn resolve(self) -> R {
        let times = if self.times.is_empty() {
            ET::Int::from(1)
        } else {
            self.times.into_iter().reduce(|a,b| a * b).unwrap()
        };
        let div = if self.div.is_empty() {
            ET::Int::from(1)
        } else {
            self.div.into_iter().reduce(|a,b| a * b).unwrap()
        };
        self.base.scale(times,div)
    }
    fn mult(&mut self,i:ET::Int) {
        self.times.push(i)
    }
    fn div(&mut self,i:ET::Int) {
        self.div.push(i)
    }
}
impl<ET:EngineTypes,R:Numeric<ET::Int>> std::fmt::Display for Summand<ET,R> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f,"({}",self.base)?;
        for x in self.times.iter() {
            write!(f," *{}",x)?;
        }
        for x in self.div.iter() {
            write!(f," /{}",x)?;
        }
        write!(f,")")
    }
}

fn expr_loop<ET:EngineTypes,R:Numeric<<ET::Num as NumSet>::Int>>(
    engine:&mut EngineReferences<ET>,
  byte:fn(&mut EngineReferences<ET>,bool,u8) -> R,
  cmd:fn(&mut EngineReferences<ET>,bool,Command<ET>,ET::Token) -> R
) -> (R,ET::Token) {
    let mut prev : Option<R> = None;
    let mut curr = Summand::<ET,R>::new(expr_inner(engine,byte,cmd));
    loop {
        match engine.read_chars(&[b'+',b'-',b'*',b'/']) {
            Err(r) => {
                match prev {
                    Some(p) => return (p + curr.resolve(),r),
                    _ => return (curr.resolve(),r)
                }
                //return (Summand::reduce(adds),r)
            },
            Ok(b'+') => {
                let old = std::mem::replace(&mut curr,Summand::new(expr_inner(engine,byte,cmd))).resolve();
                prev = match prev {
                    Some(s) => Some(s + old),
                    None => Some(old)
                }
            },
            Ok(b'-') => {
                let old = std::mem::replace(&mut curr,Summand::new(-expr_inner(engine,byte,cmd))).resolve();
                prev = match prev {
                    Some(s) => Some(s + old),
                    None => Some(old)
                }
            },
            Ok(b'*') => {
                curr.mult(expr_inner(engine,
                                           crate::engine::gullet::methods::read_int_byte,
                                           crate::engine::gullet::methods::read_int_command
                ))
            },
            Ok(b'/') => {
                curr.div(expr_inner(engine,
                                           crate::engine::gullet::methods::read_int_byte,
                                           crate::engine::gullet::methods::read_int_command
                ))
            },
            Ok(_) => unreachable!()
        }
    }
}

pub fn currentgrouplevel<ET:EngineTypes>(engine:&mut EngineReferences<ET>,tk:ET::Token) -> ET::Int {
    (engine.state.get_group_level() as i32).into()
}

pub fn detokenize<ET:EngineTypes>(engine: &mut EngineReferences<ET>,exp:&mut Vec<ET::Token>,tk:ET::Token) {
    engine.expand_until_bgroup(false);
    let mut f = |t| exp.push(t);
    let escapechar = engine.state.get_escape_char().map(|c| ET::Token::from_char_cat(c,CommandCode::Other));
    engine.read_until_endgroup(|a,t| {
        if t.is_space() {f(t)}
        else if t.is_param() {
            f(t.clone());f(t)
        }
        else {
            match t.to_enum() {
                StandardToken::Character(c, _) =>
                    f(ET::Token::from_char_cat(c, CommandCode::Other)),
                StandardToken::ControlSequence(cs) => {
                    if let Some(e) = &escapechar { f(e.clone()) }
                    let name = a.memory.cs_interner().resolve(&cs);
                    let mut tokenizer = Tokenizer::new(&mut f);
                    write!(tokenizer, "{}", name).unwrap();
                }
            }
        }
    });
}

pub fn expanded<ET:EngineTypes>(engine: &mut EngineReferences<ET>,exp:&mut Vec<ET::Token>,tk:ET::Token) {
    match engine.get_next() {
        Some(t) if t.is_begin_group() => {
            ET::Gullet::expand_until_endgroup(engine,false,false,|a,s,g,t| exp.push(t));
        }
        _ => todo!("throw errors")
    }
}

pub fn ifcsname<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) -> bool {
    let name = super::tex::do_csname(engine);
    engine.state.get_command(&name).is_some()
}

pub fn ifdefined<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) -> bool {
    match engine.get_next() {
        Some(t) => match t.to_enum() {
            StandardToken::Character(c,CommandCode::Active) =>
                engine.state.get_ac_command(c).is_some(),
            StandardToken::ControlSequence(name) =>
                engine.state.get_command(&name).is_some(),
            _ => todo!("Expected active character or control sequence")
        }
        _ => todo!("file end")
    }
}

pub fn iffontchar<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) -> bool {
    todo!()
}

pub fn numexpr<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) -> <ET::Num as NumSet>::Int {
    let (i,r) = expr_loop(engine,
                          crate::engine::gullet::methods::read_int_byte,
                          crate::engine::gullet::methods::read_int_command
    );
    if !r.is_cs_or_active() {
        engine.requeue(r)
    } else {
        match engine.resolve(r) {
            ResolvedToken::Cmd { cmd: Some(Command::Relax), .. } => (),
            ResolvedToken::Cmd { token, .. } => engine.requeue(token),
            ResolvedToken::Tk { token, .. } => engine.requeue(token)
        }
    }
    i
}

pub fn dimexpr<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) -> <ET::Num as NumSet>::Dim {
    let (i,r) = expr_loop(engine,
                          crate::engine::gullet::methods::read_dim_byte,
                          crate::engine::gullet::methods::read_dim_command
    );
    if !r.is_cs_or_active() {
        engine.requeue(r)
    } else {
        match engine.resolve(r) {
            ResolvedToken::Cmd { cmd: Some(Command::Relax), .. } => (),
            ResolvedToken::Cmd { token, .. } => engine.requeue(token),
            ResolvedToken::Tk { token, .. } => engine.requeue(token)
        }
    }
    i
}

pub fn glueexpr<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) -> <ET::Num as NumSet>::Skip {
    let (i,r) = expr_loop(engine,
                          crate::engine::gullet::methods::read_skip_byte,
                          crate::engine::gullet::methods::read_skip_command
    );
    if !r.is_cs_or_active() {
        engine.requeue(r)
    } else {
        match engine.resolve(r) {
            ResolvedToken::Cmd { cmd: Some(Command::Relax), .. } => (),
            ResolvedToken::Cmd { token, .. } => engine.requeue(token),
            ResolvedToken::Tk { token, .. } => engine.requeue(token)
        }
    }
    i
}

pub fn muexpr<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) -> <ET::Num as NumSet>::MuSkip {
    let (i,r) = expr_loop(engine,
                          crate::engine::gullet::methods::read_muskip_byte,
                          crate::engine::gullet::methods::read_muskip_command
    );
    if !r.is_cs_or_active() {
        engine.requeue(r)
    } else {
        match engine.resolve(r) {
            ResolvedToken::Cmd { cmd: Some(Command::Relax), .. } => (),
            ResolvedToken::Cmd { token, .. } => engine.requeue(token),
            ResolvedToken::Tk { token, .. } => engine.requeue(token)
        }
    }
    i
}

pub fn lastnodetype<ET:EngineTypes>(engine:&mut EngineReferences<ET>,tk:ET::Token) -> ET::Int {
    let data = engine.stomach.data_mut();
    let ls = data.get_list();
    match ls.last() {
        None => (-1).into(),
        Some(n) => (n.nodetype().to_u8() as i32).into()
    }
}

pub fn protected<ET:EngineTypes>(engine:&mut EngineReferences<ET>,tk:ET::Token,outer:bool,long:bool,protected:bool,globally:bool) {
    crate::expand_loop!(engine,
        ResolvedToken::Cmd {cmd:Some(Command::Assignment(a)),token} => match a.name {
            n if n == PRIMITIVES.outer => return super::tex::outer(engine,token,outer,long,true,globally),
            n if n == PRIMITIVES.long => return super::tex::long(engine,token,outer,long,true,globally),
            n if n == PRIMITIVES.protected => return self::protected(engine,token,outer,long,true,globally),
            n if n == PRIMITIVES.global => return super::tex::global(engine,token,outer,long,true,globally),
            n if n == PRIMITIVES.def => return super::tex::def(engine,token,outer,long,true,globally),
            n if n == PRIMITIVES.edef => return super::tex::edef(engine,token,outer,long,true,globally),
            n if n == PRIMITIVES.xdef => return super::tex::xdef(engine,token,outer,long,true,globally),
            n if n == PRIMITIVES.gdef => return super::tex::gdef(engine,token,outer,long,true,globally),
            _ => todo!("throw error")
        }
        _ => todo!("throw error")
    )
}

pub fn readline<ET:EngineTypes>(engine:&mut EngineReferences<ET>,token:ET::Token,globally:bool) {
    let idx = super::tex::read_file_index(engine);
    if !engine.read_keyword("to".as_bytes()) {
        todo!("throw error")
    }
    let cs = engine.read_control_sequence();
    let mut ret = shared_vector::Vector::new();
    engine.filesystem.readline(idx,|t| ret.push(t));
    let m = Macro {
        long:false,outer:false,protected:false,
        expansion:ret.into(),
        signature:MacroSignature {
            arity:0,
            params:engine.aux.memory.empty().into()
        }
    };
    engine.set_command(&cs,Some(Command::Macro(m)),globally)
}

pub fn unexpanded<ET:EngineTypes>(engine: &mut EngineReferences<ET>,exp:&mut Vec<ET::Token>,tk:ET::Token) {
    engine.expand_until_bgroup(false);
    engine.read_until_endgroup(|a,t|{
        exp.push(t)
    });
}

pub fn unless<ET:EngineTypes>(engine: &mut EngineReferences<ET>,tk:ET::Token) {
    match engine.get_next() {
        None => todo!("file end"),
        Some(t) => match ET::Gullet::resolve(engine.state,t) {
            ResolvedToken::Cmd {cmd:Some(Command::Conditional(cnd)),token} => {
                ET::Gullet::do_conditional(engine,cnd.name,token,cnd.expand,true)
            }
            _ => todo!("throw error")
        }
    }
}

pub fn marks<ET:EngineTypes>(engine:&mut EngineReferences<ET>,tk:ET::Token) {
    let i = engine.read_int(false).into();
    if i < 0 {
        todo!("throw error")
    }
    super::tex::do_marks(engine,i as usize)
}

pub fn topmarks<ET:EngineTypes>(engine:&mut EngineReferences<ET>,exp:&mut Vec<ET::Token>,token:ET::Token) {
    let i = engine.read_int(false).into();
    if i < 0 {
        todo!("throw error")
    }
    super::tex::get_marks(engine,exp,|d| &mut d.topmarks,i as usize)
}
pub fn firstmarks<ET:EngineTypes>(engine:&mut EngineReferences<ET>,exp:&mut Vec<ET::Token>,token:ET::Token) {
    let i = engine.read_int(false).into();
    if i < 0 {
        todo!("throw error")
    }
    super::tex::get_marks(engine,exp,|d| &mut d.firstmarks,i as usize)
}
pub fn botmarks<ET:EngineTypes>(engine:&mut EngineReferences<ET>,exp:&mut Vec<ET::Token>,token:ET::Token) {
    let i = engine.read_int(false).into();
    if i < 0 {
        todo!("throw error")
    }
    super::tex::get_marks(engine,exp,|d| &mut d.botmarks,i as usize)
}
pub fn splitfirstmarks<ET:EngineTypes>(engine:&mut EngineReferences<ET>,exp:&mut Vec<ET::Token>,token:ET::Token) {
    let i = engine.read_int(false).into();
    if i < 0 {
        todo!("throw error")
    }
    super::tex::get_marks(engine,exp,|d| &mut d.splitfirstmarks,i as usize)
}
pub fn splitbotmarks<ET:EngineTypes>(engine:&mut EngineReferences<ET>,exp:&mut Vec<ET::Token>,token:ET::Token) {
    let i = engine.read_int(false).into();
    if i < 0 {
        todo!("throw error")
    }
    super::tex::get_marks(engine,exp,|d| &mut d.splitbotmarks,i as usize)
}

const PRIMITIVE_INTS:&[&'static str] = &[
    "savinghyphcodes",
    "tracingassigns",
    "tracinggroups",
    "tracingifs",
    "tracingnesting",
    "tracingscantokens"
];

const PRIMITIVE_TOKS:&[&'static str] = &[
    "everyeof",
];

pub fn register_etex_primitives<E:TeXEngine>(engine:&mut E) {
    register_primitive_int(engine,PRIMITIVE_INTS);
    register_primitive_toks(engine,PRIMITIVE_TOKS);

    register_int(engine,"numexpr",numexpr,None);
    register_dim(engine,"dimexpr",dimexpr,None);
    register_skip(engine,"glueexpr",glueexpr,None);
    register_muskip(engine,"muexpr",muexpr,None);

    register_int(engine,"currentgrouplevel",currentgrouplevel,None);
    register_int(engine,"lastnodetype",lastnodetype,None);
    register_int(engine,"eTeXversion",eTeXversion,None);

    register_assignment(engine,"protected",|e,cmd,g|protected(e,cmd,false,false,false,g));
    register_assignment(engine,"readline",readline);

    register_conditional(engine,"ifcsname",ifcsname);
    register_conditional(engine,"ifdefined",ifdefined);
    register_conditional(engine,"iffontchar",iffontchar);

    register_expandable(engine,"detokenize",detokenize);
    register_expandable(engine,"expanded",expanded);
    register_expandable(engine,"unexpanded",unexpanded);
    register_expandable(engine,"eTeXrevision",eTeXrevision);

    register_unexpandable(engine,"marks",marks);

    register_simple_expandable(engine,"unless",unless);

    register_expandable(engine,"topmarks",topmarks);
    register_expandable(engine,"firstmarks",firstmarks);
    register_expandable(engine,"botmarks",botmarks);
    register_expandable(engine,"splitfirstmarks",splitfirstmarks);
    register_expandable(engine,"splitbotmarks",splitbotmarks);

    cmtodos!(engine,
        fontchardp,fontcharht,fontcharic,fontcharwd,
        scantokens
    );

    cmtodo!(engine,beginL);
    cmtodo!(engine,beginR);
    cmtodo!(engine,clubpenalties);
    cmtodo!(engine,currentgrouptype);
    cmtodo!(engine,currentifbranch);
    cmtodo!(engine,currentiflevel);
    cmtodo!(engine,currentiftype);
    cmtodo!(engine,displaywidowpenalties);
    cmtodo!(engine,endL);
    cmtodo!(engine,endR);
    cmtodo!(engine,glueshrink);
    cmtodo!(engine,glueshrinkorder);
    cmtodo!(engine,gluestretch);
    cmtodo!(engine,gluestretchorder);
    cmtodo!(engine,gluetomu);
    cmtodo!(engine,interactionmode);
    cmtodo!(engine,interlinepenalties);
    cmtodo!(engine,lastlinefit);
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
    cmtodo!(engine,splitdiscards);
    cmtodo!(engine,TeXXeTstate);
    cmtodo!(engine,widowpenalties);
/*
    register_int!(currentgrouplevel,engine,(e,c) => currentgrouplevel::<ET>(e,&c));
    register_expandable!(detokenize,engine,(e,c,f) =>detokenize::<ET>(e,&c,f));
    register_dim!(dimexpr,engine,(e,c) => dimexpr::<ET>(e,&c));
    register_expandable!(eTeXrevision,engine,(e,c,f) => eTeXrevision::<ET>(e,&c,f));
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
    register_unexpandable!(marks,engine,None,(e,cmd) =>marks::<ET>(e,&cmd));
    register_expandable!(topmarks,engine,(e,c,f) =>topmarks::<ET>(e,&c,f));
    register_expandable!(firstmarks,engine,(e,c,f) =>firstmarks::<ET>(e,&c,f));
    register_expandable!(botmarks,engine,(e,c,f) =>botmarks::<ET>(e,&c,f));
    register_expandable!(splitfirstmarks,engine,(e,c,f) =>splitfirstmarks::<ET>(e,&c,f));
    register_expandable!(splitbotmarks,engine,(e,c,f) =>splitbotmarks::<ET>(e,&c,f));
    register_muskip!(muexpr,engine,(e,c) => muexpr::<ET>(e,&c));
    register_int!(numexpr,engine,(e,c) => numexpr::<ET>(e,&c));
    register_assign!(readline,engine,(eu,cmd,global) =>readline::<ET>(eu,&cmd,global));
    register_expandable_notk!(scantokens,engine,(e,c) =>scantokens::<ET>(e,&c));
    register_assign!(protected,engine,(eu,cmd,g) =>protected::<ET>(eu,&cmd,g,false,false,false));
    register_expandable!(unexpanded,engine,(e,c,f) => unexpanded::<ET>(e,&c,f));
    register_expandable_notk!(unless,engine,(eu,cmd) =>unless::<ET>(eu,&cmd));


 */
}