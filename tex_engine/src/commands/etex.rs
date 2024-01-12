use crate::cmtodo;
use crate::commands::{Command, CommandScope, Macro, MacroSignature};
use crate::engine::{EngineReferences, EngineTypes, TeXEngine};
use crate::engine::gullet::ResolvedToken;
use crate::engine::mouth::Mouth;
use crate::tex::tokens::token_lists::{Otherize, CharWrite};
use crate::engine::state::State;
use crate::engine::utils::memory::{MemoryManager, PRIMITIVES};
use crate::tex::catcodes::{CategoryCode, CommandCode};
use crate::tex::tokens::control_sequences::{CSHandler,ResolvedCSName};
use crate::tex::tokens::{StandardToken, Token};
use super::primitives::*;
use crate::engine::gullet::Gullet;
use crate::tex::numerics::{Numeric, NumSet};
use crate::engine::filesystem::FileSystem;
use crate::engine::fontsystem::Font;
use crate::engine::stomach::Stomach;
use crate::tex::nodes::{NodeList, NodeTrait};
use crate::tex::input_text::CharacterMap;
use crate::tex::nodes::math::{MathNode, MathNucleus};
use crate::tex::types::GroupType;
use crate::tex::nodes::math::MathAtom;
use crate::utils::errors::TeXError;

#[allow(non_snake_case)]
pub fn eTeXversion<ET:EngineTypes>(_engine:&mut EngineReferences<ET>,_tk:ET::Token) -> ET::Int {
    2.into()
}
#[allow(non_snake_case)]
pub fn eTeXrevision<ET:EngineTypes>(_engine: &mut EngineReferences<ET>,exp:&mut Vec<ET::Token>,_tk:ET::Token) {
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

pub fn currentgrouplevel<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token) -> ET::Int {
    (engine.state.get_group_level() as i32).into()
}

pub fn detokenize<ET:EngineTypes>(engine: &mut EngineReferences<ET>,exp:&mut Vec<ET::Token>,_tk:ET::Token) {
    engine.expand_until_bgroup(false);
    let mut f = |t| exp.push(t);
    let escapechar = engine.state.get_escape_char();
    engine.read_until_endgroup(|a,st,t| {
        if t.is_space() {f(t)}
        else if t.is_param() {
            f(t.clone());f(t)
        }
        else {
            let mut tokenizer = Otherize::new(&mut f);
            match t.to_enum() {
                StandardToken::Character(c, _) =>
                    tokenizer.push_char(c),
                StandardToken::ControlSequence(cs) => {
                    tokenizer.push_cs(cs,a.memory.cs_interner(),st.get_catcode_scheme(),escapechar)
                }
            }
        }
    });
}

pub fn expanded<ET:EngineTypes>(engine: &mut EngineReferences<ET>,exp:&mut Vec<ET::Token>,_tk:ET::Token) {
    match engine.get_next() {
        Some(t) if t.is_begin_group() => {
            ET::Gullet::expand_until_endgroup(engine,false,false,|_,_,t| exp.push(t));
        }
        _ => todo!("throw errors")
    }
}

pub fn fontchardp<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) -> <ET::Num as NumSet>::Dim {
    let fnt = engine.read_font();
    let char = engine.read_charcode(false);
    fnt.get_dp(char)
}
pub fn fontcharht<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) -> <ET::Num as NumSet>::Dim {
    let fnt = engine.read_font();
    let char = engine.read_charcode(false);
    fnt.get_ht(char)
}
pub fn fontcharwd<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) -> <ET::Num as NumSet>::Dim {
    let fnt = engine.read_font();
    let char = engine.read_charcode(false);
    fnt.get_wd(char)
}
pub fn fontcharic<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) -> <ET::Num as NumSet>::Dim {
    let fnt = engine.read_font();
    let char = engine.read_charcode(false);
    fnt.get_ic(char)
}

pub fn ifcsname<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) -> bool {
    let name = super::methods::do_csname(engine);
    engine.state.get_command(&name).is_some()
}

pub fn ifdefined<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) -> bool {
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

pub fn iffontchar<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) -> bool {
    let font = engine.read_font();
    let char = engine.read_charcode(false);
    font.has_char(char)
}

pub fn numexpr<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) -> <ET::Num as NumSet>::Int {
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

pub fn dimexpr<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) -> <ET::Num as NumSet>::Dim {
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

pub fn glueexpr<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) -> <ET::Num as NumSet>::Skip {
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

pub fn muexpr<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) -> <ET::Num as NumSet>::MuSkip {
    fn muskip_byte<ET:EngineTypes>(engine: &mut EngineReferences<ET>,is_negative:bool,b:u8) -> <ET::Num as NumSet>::MuSkip {
        crate::engine::gullet::methods::read_muskip_byte(
            engine,is_negative,b,
            |d,e| crate::engine::gullet::methods::read_muskip_ii(e, d)
        )
    }
    fn muskip_cmd<ET:EngineTypes>(engine: &mut EngineReferences<ET>,is_negative:bool,cmd:Command<ET>,tk:ET::Token) -> <ET::Num as NumSet>::MuSkip {
        crate::engine::gullet::methods::read_muskip_command(
            engine,is_negative,cmd,tk,|d,e| crate::engine::gullet::methods::read_muskip_ii(e, d),|s| s
        )
    }
    let (i,r) = expr_loop(engine,
                          muskip_byte,muskip_cmd
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

pub fn lastnodetype<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token) -> ET::Int {
    let data = engine.stomach.data_mut();
    match data.open_lists.last() {
        None => match data.page.last() {
            None => (-1).into(),
            Some(n) => (n.nodetype().to_u8() as i32).into()
        },
        Some(NodeList::Vertical {children,..}) => match children.last() {
            None => (-1).into(),
            Some(n) => (n.nodetype().to_u8() as i32).into()
        },
        Some(NodeList::Horizontal {children,..}) => match children.last() {
            None => (-1).into(),
            Some(n) => (n.nodetype().to_u8() as i32).into()
        },
        Some(NodeList::Math {children,..}) => match children.list().last() {
            None => (-1).into(),
            Some(n) => (n.nodetype().to_u8() as i32).into()
        }
    }
}

pub fn protected<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token,outer:bool,long:bool,_protected:bool,globally:bool) {
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

pub fn readline<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token,globally:bool) {
    let idx = super::methods::read_file_index(engine);
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


pub fn scantokens<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) {
    engine.expand_until_bgroup(false);
    let mut ret: Vec<Box<[ET::Char]>> = vec!();
    let mut curr = vec!();
    let mut f = |c:ET::Char| if matches!(c.try_into(),Ok(b'\n')) {
        if curr.last().copied() == Some(b'\r'.into()) {
            curr.pop();
        }
        ret.push(std::mem::take(&mut curr).into());
    } else {curr.push(c)};
    let escapechar = engine.state.get_escape_char();
    engine.read_until_endgroup(|a,state,t| {
        match t.to_enum() {
            StandardToken::Character(c,_) => f(c),
            StandardToken::ControlSequence(cs) => {
                if let Some(esc) = escapechar {
                    f(esc);
                }
                let res = a.memory.cs_interner().resolve(&cs);
                for c in res.iter() {
                    f(c)
                }
                if res.len() == 1 {
                    let c = res.iter().next().unwrap();
                    match state.get_catcode_scheme().get(c) {
                        CategoryCode::Letter => f(b' '.into()),
                        _ => ()
                    }
                } else {
                    f(b' '.into())
                }
            }
        }
    });
    if !curr.is_empty() {ret.push(curr.into())}
    engine.mouth.push_string(ret.into());
}


pub fn unexpanded<ET:EngineTypes>(engine: &mut EngineReferences<ET>,exp:&mut Vec<ET::Token>,_tk:ET::Token) {
    engine.expand_until_bgroup(false);
    engine.read_until_endgroup(|_,_,t|{
        exp.push(t)
    });
}

pub fn unless<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) {
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

pub fn middle<ET:EngineTypes>(engine: &mut EngineReferences<ET>,_tk:ET::Token) {
    match engine.state.get_group_type() {
        Some(GroupType::LeftRight) => (),
        _ => todo!("error?")
    }
    let del = match super::methods::read_opt_delimiter(engine) {
        None => todo!("error?"),
        Some(c) => c
    };
    ET::Stomach::add_node_m(engine,MathNode::Atom(MathAtom {
        sub:None,sup:None,nucleus:MathNucleus::Middle(del.small.char,del.small.style)
    }))
}

pub fn marks<ET:EngineTypes>(engine:&mut EngineReferences<ET>,_tk:ET::Token) {
    let i = engine.read_int(false).into();
    if i < 0 {
        todo!("throw error")
    }
    super::methods::do_marks(engine, i as usize)
}

pub fn topmarks<ET:EngineTypes>(engine:&mut EngineReferences<ET>,exp:&mut Vec<ET::Token>,_tk:ET::Token) {
    let i = engine.read_int(false).into();
    if i < 0 {
        todo!("throw error")
    }
    super::methods::get_marks(engine, exp, |d| &mut d.topmarks, i as usize)
}
pub fn firstmarks<ET:EngineTypes>(engine:&mut EngineReferences<ET>,exp:&mut Vec<ET::Token>,_tk:ET::Token) {
    let i = engine.read_int(false).into();
    if i < 0 {
        todo!("throw error")
    }
    super::methods::get_marks(engine, exp, |d| &mut d.firstmarks, i as usize)
}
pub fn botmarks<ET:EngineTypes>(engine:&mut EngineReferences<ET>,exp:&mut Vec<ET::Token>,_tk:ET::Token) {
    let i = engine.read_int(false).into();
    if i < 0 {
        todo!("throw error")
    }
    super::methods::get_marks(engine, exp, |d| &mut d.botmarks, i as usize)
}
pub fn splitfirstmarks<ET:EngineTypes>(engine:&mut EngineReferences<ET>,exp:&mut Vec<ET::Token>,_tk:ET::Token) {
    let i = engine.read_int(false).into();
    if i < 0 {
        todo!("throw error")
    }
    super::methods::get_marks(engine, exp, |d| &mut d.splitfirstmarks, i as usize)
}
pub fn splitbotmarks<ET:EngineTypes>(engine:&mut EngineReferences<ET>,exp:&mut Vec<ET::Token>,_tk:ET::Token) {
    let i = engine.read_int(false).into();
    if i < 0 {
        todo!("throw error")
    }
    super::methods::get_marks(engine, exp, |d| &mut d.splitbotmarks, i as usize)
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

    register_dim(engine,"fontchardp",fontchardp,None);
    register_dim(engine,"fontcharht",fontcharht,None);
    register_dim(engine,"fontcharwd",fontcharwd,None);
    register_dim(engine,"fontcharic",fontcharic,None);

    register_assignment(engine,"protected",|e,cmd,g|protected(e,cmd,false,false,false,g));
    register_assignment(engine,"readline",readline);

    register_conditional(engine,"ifcsname",ifcsname);
    register_conditional(engine,"ifdefined",ifdefined);
    register_conditional(engine,"iffontchar",iffontchar);

    register_expandable(engine,"detokenize",detokenize);
    register_expandable(engine,"expanded",expanded);
    register_expandable(engine,"unexpanded",unexpanded);
    register_expandable(engine,"eTeXrevision",eTeXrevision);

    register_unexpandable(engine,"marks",CommandScope::Any,marks);
    register_unexpandable(engine, "middle", CommandScope::MathOnly, middle);

    register_simple_expandable(engine,"unless",unless);
    register_simple_expandable(engine,"scantokens",scantokens);

    register_expandable(engine,"topmarks",topmarks);
    register_expandable(engine,"firstmarks",firstmarks);
    register_expandable(engine,"botmarks",botmarks);
    register_expandable(engine,"splitfirstmarks",splitfirstmarks);
    register_expandable(engine,"splitbotmarks",splitbotmarks);

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