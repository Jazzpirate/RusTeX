use crate::engine::gullet::Gullet;
use crate::engine::state::State;
use crate::engine::mouth::Mouth;
use crate::engine::stomach::Stomach;
use crate::{debug_log, register_assign, register_conditional, register_gullet, register_int, register_tok_assign};
use crate::tex::catcodes::CategoryCode;
use crate::tex::commands::{Assignable, Command, GulletCommand, StomachCommand, StomachCommandInner};
use crate::tex::numbers::NumSet;
use crate::tex::token::{BaseToken, Token};
use crate::utils::errors::{catch_prim, ErrorInPrimitive, file_end_prim};
use crate::utils::strings::CharType;
use crate::tex::numbers::Int;
use crate::utils::Ptr;


/* TODO

beginL
beginR
botmarks
clubpenalties
currentgrouplevel
currentgrouptype
currentifbranch
currentiflevel
currentiftype
dimexpr
displaywidowpenalties
endL
endR
expanded
firstmarks
fontchardp
fontcharht
fontcharic
fontcharwd
glueexpr
glueshrink
glueshrinkorder
gluestretchorder
gluestretch
gluetomu
ifcsname
iffontchar
interactionmode
interlinepenalties
lastlinefit
lastnodetype
marks
middle
muexpr
mutoglue
numexpr
pagediscards
parshapedimen
parshapeindent
parshapelength
predisplaydirection
readline
savinghyphcodes
savingvdiscards
showgroups
showifs
showtokens
splitdiscards
scantokens
splitbotmarks
splitfirstmarks
TeXXeTstate
topmarks
tracingassigns
tracinggroups
tracingifs
tracingnesting
tracingscantokens
unless
widowpenalties
 */
pub fn etexrevision<T:Token,S:State<T>,Gu:Gullet<T,S=S>>(state:&mut S,_gullet:&mut Gu,_cmd:GulletCommand<T>) -> Result<Vec<T>,ErrorInPrimitive<T>> {
    Ok(T::from_str(".6".to_string()))
}
pub fn etexversion<T:Token,S:State<T>,Gu:Gullet<T,S=S>>(state:&mut S,_gullet:&mut Gu,cmd:GulletCommand<T>) -> Result<<S::NumSet as NumSet>::Int,ErrorInPrimitive<T>> {
    Ok(catch_prim!(<S::NumSet as NumSet>::Int::from_i64(2) => ("eTeXversion",cmd)))
}

pub fn expanded<T:Token,S:State<T>,Gu:Gullet<T,S=S>>(state:&mut S,gullet:&mut Gu,cmd:GulletCommand<T>) -> Result<Vec<T>,ErrorInPrimitive<T>> {
    debug_log!(trace=>"expanded");
    Ok(catch_prim!(gullet.get_expanded_group(state,false,false,false) => ("expanded",cmd)))
}

pub fn ifdefined<T:Token,Gu:Gullet<T>>(state:&mut Gu::S,gullet:&mut Gu,cmd:GulletCommand<T>) -> Result<bool,ErrorInPrimitive<T>> {
    debug_log!(trace=>"ifdefined");
    match catch_prim!(gullet.mouth().get_next(state) => ("ifeof",cmd)) {
        None => file_end_prim!("ifeof",cmd),
        Some((t,_)) => match t.base() {
            BaseToken::Char(c,CategoryCode::Active) =>
                Ok(state.get_ac_command(*c).is_some()),
            BaseToken::CS(name) => {
                debug_log!(trace => "ifdefined: {}",name.to_string());
                Ok(state.get_command(name).is_some())
            }
            _ => Err(ErrorInPrimitive{name:"ifdefined",msg:Some(format!("Expected a control sequence, got: {:?}",t)),cause:Some(cmd.cause),source:None})
        }
    }
}

use super::tex::{global,long,outer,def,edef,gdef,xdef};

pub fn protected<T:Token,Sto:Stomach<T>>(stomach:&mut Sto,state:&mut Sto::S,gullet:&mut Sto::Gu,cmd:StomachCommand<T>,global_:bool,protected_:bool,long_:bool,outer_:bool)
                                                      -> Result<(),ErrorInPrimitive<T>> {
    debug_log!(trace => "\\protected");
    match catch_prim!(gullet.get_next_stomach_command(state) => ("protected",cmd)) {
        None => file_end_prim!("protected",cmd),
        Some(c) => match c.cmd {
            StomachCommandInner::Assignment {name:"global",..} => global(stomach,state,gullet,cmd,global_,true,long_,outer_),
            StomachCommandInner::Assignment {name:"protected",..} => protected(stomach,state,gullet,cmd,global_,true,long_,outer_),
            StomachCommandInner::Assignment {name:"long",..} => long(stomach,state,gullet,cmd,global_,true,long_,outer_),
            StomachCommandInner::Assignment {name:"outer",..} => outer(stomach,state,gullet,cmd,global_,true,long_,outer_),
            StomachCommandInner::Assignment {name:"def",..} => def(state,gullet,cmd,global_,true,long_,outer_),
            StomachCommandInner::Assignment {name:"edef",..} => edef(state,gullet,cmd,global_,true,long_,outer_),
            StomachCommandInner::Assignment {name:"gdef",..} => gdef(state,gullet,cmd,global_,true,long_,outer_),
            StomachCommandInner::Assignment {name:"xdef",..} => xdef(state,gullet,cmd,global_,true,long_,outer_),
            _ => return Err(ErrorInPrimitive{name:"protected",msg:Some("Expected a macro definition after \\protected".to_string()),cause:Some(cmd.cause),source:None})
        }
    }
}

pub fn unexpanded<T:Token,S:State<T>,Gu:Gullet<T,S=S>>(state:&mut S,_gullet:&mut Gu,_cmd:GulletCommand<T>) -> Result<Vec<T>,ErrorInPrimitive<T>> {
    todo!("unexpanded")
}

pub fn initialize_etex_primitives<T:Token,S:State<T>,Gu:Gullet<T,S=S>,Sto:Stomach<T,S=S,Gu=Gu>>(state:&mut S,stomach:&mut Sto,gullet:&mut Gu) {
    register_gullet!(eTeXrevision,state,stomach,gullet,(s,g,c) => etexrevision(s,g,c));
    register_int!(eTeXversion,state,stomach,gullet,(s,g,c) => etexversion(s,g,c));
    register_tok_assign!(everyeof,state,stomach,gullet);
    register_gullet!(expanded,state,stomach,gullet,(s,g,c) => expanded(s,g,c));
    register_conditional!(ifdefined,state,stomach,gullet,(s,gu,cmd) =>ifdefined(s,gu,cmd));
    register_assign!(protected,state,stomach,gullet,(s,gu,sto,cmd,g) =>protected(sto,s,gu,cmd,g,false,false,false));
    register_gullet!(unexpanded,state,stomach,gullet,(s,g,c) => unexpanded(s,g,c));
}