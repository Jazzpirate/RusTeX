//! TeX primitive [`Command`]s

use std::marker::PhantomData;
use crate::{debug_log, register_assign, register_conditional, register_gullet, register_int_assign, register_stomach, register_tok_assign, map_group, register_int, register_whatsit, register_value_assign_int, register_value_assign_dim, register_value_assign_muskip, register_value_assign_skip};
use crate::engine::filesystem::{File, FileSystem};
use crate::engine::gullet::Gullet;
use crate::engine::gullet::methods::{tokens_to_string, do_expandable, do_conditional, string_to_tokens};
use crate::engine::state::State;
use crate::engine::mouth::Mouth;
use crate::engine::state::modes::GroupType;
use crate::engine::stomach::Stomach;
use crate::tex::catcodes::CategoryCode;
use crate::tex::commands::{Assignable, Command, Def, ExpToken, GulletCommand, ParamToken, StomachCommand, StomachCommandInner};
use crate::tex::commands::methods::parse_signature;
use crate::tex::numbers::{Int, NumSet};
use crate::tex::token::{BaseToken, Token, TokenList};
use crate::utils::errors::{catch_prim, ErrorInPrimitive, file_end_prim, ExpectedToken, UnexpectedEndgroup, ImplementationError};
use crate::utils::Ptr;
use crate::utils::strings::{AllCharsTrait, CharType};
use chrono::{Datelike, Timelike};
use crate::engine::gullet;
use crate::tex::boxes::Whatsit;
use super::etex::protected;

/* TODO

SPACE
\/
\-

lastpenalty
parshape
inputlineno
hyphenchar
skewchar
badness
spacefactor
prevgraf
deadcycles
insertpenalties
mathcode
lccode
uccode
sfcode
delcode
textfont
scriptfont
scriptscriptfont
lastkern
fontdimen
prevdepth
pagegoal
pagetotal
pagestretch
pagefilstretch
pagefillstretch
pagefilllstretch
pageshrink
pagedepth
ht
wd
dp
lastskip

pretolerance
tolerance
hbadness
vbadness
linepenalty
hyphenpenalty
exhyphenpenalty
binoppenalty
relpenalty
clubpenalty
widowpenalty
displaywidowpenalty
brokenpenalty
predisplaypenalty
postdisplaypenalty
interlinepenalty
floatingpenalty
outputpenalty
doublehyphendemerits
finalhyphendemerits
adjdemerits
looseness
pausing
holdinginserts
tracingonline
tracingmacros
tracingstats
tracingparagraphs
tracingpages
tracingoutput
tracinglostchars
tracingcommands
tracingrestores
language
uchyph
lefthyphenmin
righthyphenmin
globaldefs
defaulthyphenchar
defaultskewchar
maxdeadcycles
hangafter
fam
delimiterfactor
showboxbreadth
showboxdepth
errorcontextlines

hfuzz
vfuzz
overfullrule
emergencystretch
hsize
vsize
maxdepth
splitmaxdepth
boxmaxdepth
lineskiplimit
delimitershortfall
nulldelimiterspace
scriptspace
mathsurround
predisplaysize
displaywidth
displayindent
parindent
hangindent
hoffset
voffset

baselineskip
lineskip
parskip
abovedisplayskip
abovedisplayshortskip
belowdisplayskip
belowdisplayshortskip
leftskip
rightskip
topskip
splittopskip
tabskip
spaceskip
xspaceskip
parfillskip

thinmuskip
medmuskip
thickmuskip

output
everypar
everymath
everydisplay
everyhbox
everyvbox
everyjob
everycr

setbox
font
futurelet

fontdimen
hyphenchar
skewchar
hyphenation
patterns
errorstopmode
scrollmode
nonstopmode
batchmode

box
copy
lastbox
vsplit
hbox
vbox
vtop

show
showbox
showlists
showthe

shipout
ignorespaces
afterassignment
aftergroup
uppercase
lowercase
special
penalty
kern
mkern
unpenalty
unkern
unskip
mark
topmark
firstmark
botmark
splitfirstmark
splitbotmark
insert
vadjust

vskip
vfil
vfill
vss
vfilneg

leaders
cleaders
xleaders
vrule
hrule

moveleft
moveright
unvbox
unvcopy
unhbox
unhcopy
halign
valign
indent
noindent

noboundary
hfil
hfill
hfilneg
hss
accent
discretionary
dump
raise
lower
setlanguage

mathchar
delimiter
mathcode
fam
nonscript
vcenter
mathord
mathop
mathbin
mathrel
mathopen
mathclose
mathpunct
mathinner
underline
overline
mathaccent
radical
displaylimits
limits
nolimits
mathchoice
displaystyle
textstyle
scriptstyle
scriptscriptstyle
left
right
over
atop
above
overwithdelims
atopwithdelims
abovewithdelims
eqno
leqno


or


----------------------------------------------------------------------------------------------------

above
abovewithdelims
accent
afterassignment
aftergroup
atop
atopwithdelims
batchmode
bigskip
botmark
box
bye
char
csname
cleaders
copy
cr
crcr
delcode
delimiter
detokenize
discretionary
displaylimits
displaystyle
dp
dump
endcsname
endinput
eqno
errorstopmode
firstmark
font
fontdimen
fontname
futurelet
halign
hangafter
hangindent
hbox
hfil
hfill
hfilneg
holdinginserts
hrule
hskip
hss
ht
hyphenation
hyphenchar
ignorespaces
indent
inputlineno
insert
italiccorr
jobname
kern
lastbox
lastkern
lastpenalty
lastskip
lccode
leaders
left
leqno
limits
looseness
lower
lowercase
mark
mathaccent
mathbin
mathchar
mathchoice
mathclose
mathcode
mathinner
mathop
mathopen
mathord
mathpunct
mathrel
medskip
mkern
moveleft
moveright
mskip
noboundary
noalign
noindent
nolimits
nonscript
nonstopmode
nullfont
number
omit
over
overline
overwithdelims
pagegoal
parshape
pausing
penalty
radical
raise
right
romannumeral
scriptfont
scriptscriptfont
scriptstyle
scriptscriptstyle
scrollmode
setbox
setlanguage
sfcode
shipout
show
showbox
showlists
showthe
skewchar
smallskip
span
special
splitbotmark
splitfirstmark
string
textfont
textstyle
topmark
uccode
underline
unhbox
unhcopy
unkern
unpenalty
unskip
unvbox
unvcopy
uppercase
vadjust
valign
vbox
vcenter
vfil
vfill
vfilneg
vrule
vskip
vsplit
vss
vtop
wd
xleaders
 */


// TODO check division by 0
macro_rules! modify_in_place {
    ($name:ident,$state:ident,$gullet:ident,$cmd:ident,$global:ident,$opstr:expr,$op:tt) => {
        debug_log!(trace=>"\\{}",stringify!($name));
        catch_prim!($gullet.mouth().skip_whitespace($state) => (stringify!($name),$cmd));
        match catch_prim!($gullet.get_next_stomach_command($state) => (stringify!($name),$cmd)) {
            None => file_end_prim!("advance",$cmd),
            Some(ocmd) => match ocmd.cmd {
                StomachCommandInner::ValueRegister(u,crate::tex::commands::Assignable::Int) => {
                    catch_prim!($gullet.get_keyword($state,"by") => (stringify!($name),$cmd));
                    debug_log!(debug => "  \\<register>{}",u);
                    let i = catch_prim!(($gullet.get_int($state)) => (stringify!($name),$cmd));
                    let ov = $state.get_int_register(u);
                    debug_log!(debug => "  =={}{}{}",ov,$opstr,i);
                    let nv : <S::NumSet as NumSet>::Int = ov $op i;
                    debug_log!(debug => "  ={}",nv);
                    $state.set_int_register(u,nv,$global);
                    return Ok(())
                }
                StomachCommandInner::AssignableValue {name,tp:crate::tex::commands::Assignable::Int} => {
                    debug_log!(debug => "  \\{}",name);
                    let ov = $state.get_primitive_int(name);
                    let i = catch_prim!(($gullet.get_int($state)) => (stringify!($name),$cmd));
                    debug_log!(debug => "  =={}{}{}",ov,$opstr,i);
                    let nv : <S::NumSet as NumSet>::Int = ov $op i;
                    debug_log!(debug => "  ={}",nv);
                    $state.set_primitive_int(name,nv,$global);
                    return Ok(())
                }
                StomachCommandInner::ValueAssignment {name:"count",..} => {
                    catch_prim!($gullet.get_keyword($state,"by") => (stringify!($name),$cmd));
                    let i = catch_prim!(($gullet.get_int($state)) => (stringify!($name),$cmd));
                    let u = match i.try_into() {
                        Ok(u) => u,
                        _ => return Err(ErrorInPrimitive{name:"count",msg:Some(format!("Not a valid register")),cause:Some($cmd.cause),source:None})
                    };
                    debug_log!(debug => "  \\count{}",u);
                    let i = catch_prim!(($gullet.get_int($state)) => (stringify!($name),$cmd));
                    let ov = $state.get_int_register(u);
                    debug_log!(debug => "  =={}{}{}",ov,$opstr,i);
                    let nv : <S::NumSet as NumSet>::Int = ov $op i;
                    debug_log!(debug => "  ={}",nv);
                    $state.set_int_register(u,nv,$global);
                    return Ok(())
                }
                _ => return Err(ErrorInPrimitive{name:stringify!($name),msg:Some(format!("expected register after \\{}",stringify!($name))),cause:Some($cmd.cause),source:None})
            }
        }
    }
}

pub fn SPACE<T:Token,Sto:Stomach<T>>(_stomach:&mut Sto,state:&mut Sto::S,_cmd:StomachCommand<T>)
                                     -> Result<(),ErrorInPrimitive<T>> {
    todo!("\\ ")
}

pub fn advance<T:Token,S:State<T>,Gu:Gullet<T,S=S>>(state:&mut S,gullet:&mut Gu,cmd:StomachCommand<T>,global:bool)
                                                     -> Result<(),ErrorInPrimitive<T>> {
    modify_in_place!(advance,state,gullet,cmd,global,"+",+);
}

pub fn begingroup<T:Token,Sto:Stomach<T>>(_stomach:&mut Sto,state:&mut Sto::S,_cmd:StomachCommand<T>)
     -> Result<(),ErrorInPrimitive<T>> {
    state.stack_push(<<Sto::S as State<T>>::Gr as GroupType>::from_begingroup_cs());
    Ok(())
}

pub fn catcode_assign<T:Token,S:State<T>,Gu:Gullet<T,S=S>>(state:&mut S,gullet:&mut Gu,cmd:StomachCommand<T>,global:bool) -> Result<(),ErrorInPrimitive<T>> {
    debug_log!(trace=>"Assigning category code");
    let i = catch_prim!(gullet.get_int(state) => ("catcode",cmd));
    let c: T::Char = match T::Char::from_i64(i.to_i64()) {
        Some(i) => i,
        None => return Err(ErrorInPrimitive{name:"catcode",msg:Some(format!("Not a valid character: {}",i)),cause:Some(cmd.cause),source:None})
    };
    catch_prim!(gullet.mouth().skip_eq_char(state) => ("catcode",cmd));
    let v = catch_prim!(gullet.get_int(state) => ("catcode",cmd)).to_i64();
    if v < 0 || v > 15 {
        return Err(ErrorInPrimitive{name:"catcode",msg:Some(format!("Invalid category code: {}",v)),cause:Some(cmd.cause),source:None})
    }
    let cc: CategoryCode = unsafe{(v as u8).try_into().unwrap_unchecked()};
    debug_log!(debug=>"\\catcode '{}' = {}",c.char_str(),cc);
    state.set_catcode(c,cc,global);
    Ok(())
}
pub fn catcode_get<T:Token,S:State<T>,Gu:Gullet<T,S=S>>(state:&mut S,gullet:&mut Gu,cmd:GulletCommand<T>) -> Result<<S::NumSet as NumSet>::Int,ErrorInPrimitive<T>> {
    debug_log!(trace=>"Getting category code");
    let i = catch_prim!(gullet.get_int(state) => ("catcode",cmd));
    let c: T::Char = match T::Char::from_i64(i.to_i64()) {
        Some(i) => i,
        None => return Err(ErrorInPrimitive{name:"catcode",msg:Some(format!("Not a valid character: {}",i)),cause:Some(cmd.cause),source:None})
    };
    let cc = *state.get_catcode_scheme().get(c);
    let v : u8 = cc.into();
    debug_log!(debug=>"\\catcode '{}' == {}",c.char_str(),cc);
    Ok(v.into())
}

pub fn chardef<T:Token,S:State<T>,Gu:Gullet<T,S=S>>(state:&mut S,gullet:&mut Gu,cmd:StomachCommand<T>,global:bool) -> Result<(),ErrorInPrimitive<T>> {
    debug_log!(trace=>"chardef");
    let name = catch_prim!(gullet.get_control_sequence(state) => ("chardef",cmd));
    catch_prim!(gullet.mouth().skip_eq_char(state) => ("chardef",cmd));
    let num = catch_prim!(gullet.get_int(state) => ("chardef",cmd));
    let char = match T::Char::from_i64(num.to_i64()) {
        Some(i) => i,
        None => return Err(ErrorInPrimitive{name:"chardef",msg:Some(format!("Not a valid character: {}",num)),cause:Some(cmd.cause),source:None})
    };
    match name {
        BaseToken::Char(c,_) => {
            debug_log!(debug=>"\\chardef: {} = {}",c.char_str(),char.char_str());
            state.set_ac_command(c, Some(Ptr::new(Command::Char{char,catcode:CategoryCode::Other})), global);
        }
        BaseToken::CS(name) => {
            debug_log!(debug=>"\\chardef: \\{} = {}",name,char.char_str());
            state.set_command(name, Some(Ptr::new(Command::Char{char,catcode:CategoryCode::Other})), global);
        }
    }
    Ok(())
}


pub fn closein<T:Token,Sto:Stomach<T>>(state: &mut Sto::S, gullet:&mut Sto::Gu, stomach:&mut Sto, cmd:StomachCommand<T>) -> Result<(), ErrorInPrimitive<T>> {
    debug_log!(trace=>"\\closein");
    let i = catch_prim!(gullet.get_int(state) => ("closein",cmd));
    let i : usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => return Err(ErrorInPrimitive{name:"closein",msg:Some(format!("Invalid file number: {}",i)),cause:Some(cmd.cause),source:None})
    };
    state.file_closein(i); // TODO error?
    Ok(())
}

pub fn closeout<T:Token,Sto:Stomach<T>>(state: &mut Sto::S, gullet:&mut Sto::Gu, stomach:&mut Sto, cmd:StomachCommand<T>) -> Result<Whatsit<T, Sto>, ErrorInPrimitive<T>> {
    debug_log!(trace=>"\\closeout");
    let i = catch_prim!(gullet.get_int(state) => ("closeout",cmd));
    let i : usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => return Err(ErrorInPrimitive{name:"closeout",msg:Some(format!("Invalid file number: {}",i)),cause:Some(cmd.cause),source:None})
    };
    let apply = Box::new(move |_stomach:&mut Sto,state:&mut Sto::S,_gullet:&mut Sto::Gu| {
        state.file_closeout(i); // TODO error?
        Ok(())
    });
    Ok(Whatsit { apply })
}

pub fn count_assign<T:Token,S:State<T>,Gu:Gullet<T,S=S>>(state:&mut S,gullet:&mut Gu,cmd:StomachCommand<T>,global:bool) -> Result<(),ErrorInPrimitive<T>> {
    debug_log!(trace=>"Assigning \\count");
    let i = catch_prim!(gullet.get_int(state) => ("count",cmd));
    let i:usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => return Err(ErrorInPrimitive{name:"count",msg:Some(format!("Not a valid register: {}",i)),cause:Some(cmd.cause),source:None})
    };
    catch_prim!(gullet.mouth().skip_eq_char(state) => ("count",cmd));
    let v = catch_prim!(gullet.get_int(state) => ("count",cmd));
    debug_log!(debug=>"\\count{} = {}",i,v);
    state.set_int_register(i,v,global);
    Ok(())
}
pub fn count_get<T:Token,S:State<T>,Gu:Gullet<T,S=S>>(state:&mut S,gullet:&mut Gu,cmd:GulletCommand<T>) -> Result<<S::NumSet as NumSet>::Int,ErrorInPrimitive<T>> {
    debug_log!(trace=>"Getting \\count");
    let i = catch_prim!(gullet.get_int(state) => ("count",cmd));
    let i:usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => return Err(ErrorInPrimitive{name:"count",msg:Some(format!("Not a valid register: {}",i)),cause:Some(cmd.cause),source:None})
    };
    let v = state.get_int_register(i);
    debug_log!(debug=>"\\count{} == {}",i,v);
    Ok(v)
}

pub fn countdef<T:Token,S:State<T>,Gu:Gullet<T,S=S>>(state:&mut S,gullet:&mut Gu,cmd:StomachCommand<T>,global:bool)
    -> Result<(),ErrorInPrimitive<T>> {
    debug_log!(trace=>"countdef");
    let name = catch_prim!(gullet.get_control_sequence(state) => ("countdef",cmd));
    match &name {
        BaseToken::Char(c,_) => {
            state.set_ac_command(*c,Some(Ptr::new(Command::Relax)),false)
        }
        BaseToken::CS(name) => {
            state.set_command(name.clone(),Some(Ptr::new(Command::Relax)),false)
        }
    }
    catch_prim!(gullet.mouth().skip_eq_char(state) => ("countdef",cmd));
    let num = catch_prim!(gullet.get_int(state) => ("countdef",cmd));
    if num.to_i64() < 0 {
        return Err(ErrorInPrimitive{name:"countdef",msg:Some(format!("Invalid count register index: {}",num)),cause:Some(cmd.cause),source:None})
    }
    let num = num.to_i64() as usize;
    match name {
        BaseToken::Char(c,_) => {
            debug_log!(debug=>"\\countdef: {} = \\count{}",c.char_str(),num);
            state.set_ac_command(c, Some(Ptr::new(Command::ValueRegister{index:num,tp:Assignable::Int})), global);
        }
        BaseToken::CS(name) => {
            debug_log!(debug=>"\\countdef: \\{} = {}",name,num);
            state.set_command(name, Some(Ptr::new(Command::ValueRegister{index:num,tp:Assignable::Int})), global);
        }
    }
    Ok(())
}

pub fn day<T:Token,S:State<T>,Gu:Gullet<T,S=S>>(state:&mut S,_gullet:&mut Gu,cmd:GulletCommand<T>) -> Result<<S::NumSet as NumSet>::Int,ErrorInPrimitive<T>> {
    Ok(catch_prim!(<S::NumSet as NumSet>::Int::from_i64(
        state.get_start_time().day() as i64
    ) => ("day",cmd)))
}


pub fn def<T:Token,S:State<T>,Gu:Gullet<T,S=S>>(state:&mut S,gullet:&mut Gu,cmd:StomachCommand<T>,global:bool,protected:bool,long:bool,outer:bool)
    -> Result<(),ErrorInPrimitive<T>> {
    debug_log!(trace=>"def");
    let csO = catch_prim!(gullet.mouth().get_next(state) => ("def",cmd));
    let cs = match &csO {
        None => file_end_prim!("def",cmd),
        Some((t,_)) => t.base()
    };
    match cs {
        BaseToken::Char(_,CategoryCode::Active) => (),
        BaseToken::CS(_) => (),
        _ => return Err(ErrorInPrimitive{name:"def",msg:Some(format!("Command expected after \\def")),cause:Some(csO.unwrap().0),source:None})
    }
    let (endswithbrace,arity,signature) = parse_signature(state,gullet,cmd.clone(),"def")?;
    let mut replacement: Vec<ExpToken<T>> = Vec::with_capacity(50);
    map_group!("def",cmd,state,gullet.mouth(),{
        let def = Command::Def(Def{protected,long,outer,endswithbrace,arity,signature,replacement},cmd.cause.clone());
        match cs {
            BaseToken::Char(c,_) => {
                debug_log!(debug=>"\\def: {} = {:?}",c,def);
                state.set_ac_command(*c,Some(Ptr::new(def)),global)
            }
            BaseToken::CS(name) => {
                debug_log!(debug=>"\\def: \\{} = {:?}",name,def);
                state.set_command(name.clone(),Some(Ptr::new(def)),global)
            }
        }
        return Ok(())
    },tk => {
        match tk.base() {
            BaseToken::Char(c,CategoryCode::Parameter) => {
                match catch_prim!(gullet.mouth().get_next(state) => ("def",cmd)) {
                    None => file_end_prim!("def",cmd),
                    Some((stk,_)) => match stk.base() {
                        BaseToken::Char(_,CategoryCode::Parameter) =>
                            replacement.push(ExpToken::ParamToken(tk)),
                        BaseToken::Char(c,_) => {
                            let u = c.to_usize();
                            if u < 48 || u - 48 > (arity as usize) {
                                return Err(ErrorInPrimitive{name:"def",msg:Some(format!("Illegal parameter number {}",u-48)),cause:Some(cmd.cause),source:None})
                            }
                            replacement.push(ExpToken::Param(tk,(u-49) as u8))
                        }
                        _ =>
                        return Err(ErrorInPrimitive{name:"def",msg:Some(format!("Expected number after #, got {}",stk)),cause:Some(cmd.cause),source:None})
                    }
                }
            }
            _ => replacement.push(ExpToken::Token(tk))
        }
    });
    file_end_prim!("def",cmd)
}


pub fn dimen_assign<T:Token,S:State<T>,Gu:Gullet<T,S=S>>(state:&mut S,gullet:&mut Gu,cmd:StomachCommand<T>,global:bool) -> Result<(),ErrorInPrimitive<T>> {
    debug_log!(trace=>"Assigning \\dimen");
    let i = catch_prim!(gullet.get_int(state) => ("dimen",cmd));
    let i:usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => return Err(ErrorInPrimitive{name:"dimen",msg:Some(format!("Not a valid register: {}",i)),cause:Some(cmd.cause),source:None})
    };
    catch_prim!(gullet.mouth().skip_eq_char(state) => ("dimen",cmd));
    let v = catch_prim!(gullet.get_dim(state) => ("dimen",cmd));
    debug_log!(debug=>"\\dimen{} = {}",i,v);
    state.set_dim_register(i,v,global);
    Ok(())
}
pub fn dimen_get<T:Token,S:State<T>,Gu:Gullet<T,S=S>>(state:&mut S,gullet:&mut Gu,cmd:GulletCommand<T>) -> Result<<S::NumSet as NumSet>::Dim,ErrorInPrimitive<T>> {
    debug_log!(trace=>"Getting \\dimen");
    let i = catch_prim!(gullet.get_int(state) => ("dimen",cmd));
    let i:usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => return Err(ErrorInPrimitive{name:"dimen",msg:Some(format!("Not a valid register: {}",i)),cause:Some(cmd.cause),source:None})
    };
    let v = state.get_dim_register(i);
    debug_log!(debug=>"\\dimen{} == {}",i,v);
    Ok(v)
}

pub fn dimendef<T:Token,S:State<T>,Gu:Gullet<T,S=S>>(state:&mut S,gullet:&mut Gu,cmd:StomachCommand<T>,global:bool)
                                                     -> Result<(),ErrorInPrimitive<T>> {
    debug_log!(trace=>"dimendef");
    let name = catch_prim!(gullet.get_control_sequence(state) => ("dimendef",cmd));
    match &name {
        BaseToken::Char(c,_) => {
            state.set_ac_command(*c,Some(Ptr::new(Command::Relax)),false)
        }
        BaseToken::CS(name) => {
            state.set_command(name.clone(),Some(Ptr::new(Command::Relax)),false)
        }
    }
    catch_prim!(gullet.mouth().skip_eq_char(state) => ("dimendef",cmd));
    let num = catch_prim!(gullet.get_int(state) => ("dimendef",cmd));
    if num.to_i64() < 0 {
        return Err(ErrorInPrimitive{name:"dimendef",msg:Some(format!("Invalid dimen register index: {}",num)),cause:Some(cmd.cause),source:None})
    }
    let num = num.to_i64() as usize;
    match name {
        BaseToken::Char(c,_) => {
            debug_log!(debug=>"\\dimendef: {} = \\dimen{}",c.char_str(),num);
            state.set_ac_command(c, Some(Ptr::new(Command::ValueRegister{index:num,tp:Assignable::Dim})), global);
        }
        BaseToken::CS(name) => {
            debug_log!(debug=>"\\dimendef: \\{} = {}",name,num);
            state.set_command(name, Some(Ptr::new(Command::ValueRegister{index:num,tp:Assignable::Dim})), global);
        }
    }
    Ok(())
}

pub fn divide<T:Token,S:State<T>,Gu:Gullet<T,S=S>>(state:&mut S,gullet:&mut Gu,cmd:StomachCommand<T>,global:bool)
    -> Result<(),ErrorInPrimitive<T>> {
    modify_in_place!(divide,state,gullet,cmd,global,"/",/);
}

pub fn edef<T:Token,S:State<T>,Gu:Gullet<T,S=S>>(state:&mut S,gullet:&mut Gu,cmd:StomachCommand<T>,global:bool,protected:bool,long:bool,outer:bool)
                                                -> Result<(),ErrorInPrimitive<T>> {
    debug_log!(trace=>"edef");
    let csO = catch_prim!(gullet.mouth().get_next(state) => ("edef",cmd));
    let cs = match &csO {
        None => file_end_prim!("edef",cmd),
        Some((t,_)) => t.base()
    };
    match cs {
        BaseToken::Char(_,CategoryCode::Active) => (),
        BaseToken::CS(_) => (),
        _ => return Err(ErrorInPrimitive{name:"edef",msg:Some(format!("Command expected after \\edef")),cause:Some(csO.unwrap().0),source:None})
    }
    let (endswithbrace,arity,signature) = parse_signature(state,gullet,cmd.clone(),"edef")?;
    let mut replacement: Vec<ExpToken<T>> = Vec::with_capacity(50);

    macro_rules! expand_group_with_unknowns {
        ($state:ident,$gullet:ident,$finish:expr,($tk:ident,$expand:ident) => $f:expr;$($branch:tt)*) => {
            if let Some((tk,b)) = catch_prim!($gullet.mouth().get_next($state) => ("edef",cmd)) {
                match tk.catcode() {
                    CategoryCode::BeginGroup => (),
                    _ =>
                    return Err(ErrorInPrimitive{name:"edef",msg:None,cause:Some(cmd.cause),source:Some(
                        ExpectedToken{expected:T::new(BaseToken::Char(T::Char::from(b'{'),CategoryCode::BeginGroup),None),found:tk}.into()
                    )})
                }
            }
            let mut depth = 1;
            while let Some(($tk,$expand)) = catch_prim!($gullet.mouth().get_next($state) => ("edef",cmd)) {
                match $tk.catcode() {
                    CategoryCode::BeginGroup => {
                        depth += 1;
                        $f;
                    }
                    CategoryCode::EndGroup => {
                        depth -= 1;
                        if depth == 0 { $finish }
                        if depth < 0 {
                            return Err(ErrorInPrimitive{name:"edef",msg:None,cause:Some(cmd.cause),source:Some(
                                UnexpectedEndgroup($tk).into()
                            )})
                        }
                        $f;
                    },
                    _ => {
                        match $tk.base() {
                            BaseToken::CS(n) => {
                                match $state.get_command(n) {
                                    None => $f,
                                    Some(_) if !$expand => $f,
                                    Some(ncmd) => match &*ncmd {
                                        $($branch)*,
                                        Command::Gullet {name,index} => {
                                            catch_prim!(do_expandable($gullet,$state,$tk,name,*index) => ("edef",cmd));
                                        }
                                        Command::Def(def,_) => {
                                            let v = catch_prim!(def.expand($state,$gullet.mouth(),ncmd.clone(),Ptr::new($tk)) => ("edef",cmd));
                                            if !v.is_empty() {
                                                $gullet.mouth().push_tokens(v);
                                            }
                                        }
                                        Command::Conditional {name,index} => {
                                            catch_prim!(do_conditional($gullet,$state,$tk,name,*index) => ("edef",cmd));
                                        }
                                        _ => $f
                                    }
                                }
                            }
                            BaseToken::Char(c, CategoryCode::Active) => {
                                match $state.get_ac_command(*c) {
                                    None => $f,
                                    Some(_) if !$expand => $f,
                                    Some(ncmd) => match &*ncmd {
                                        $($branch)*,
                                        Command::Gullet {name,index} => {
                                            catch_prim!(do_expandable($gullet,$state,$tk,name,*index) => ("edef",cmd));
                                        }
                                        Command::Def(def,_) => {
                                            let v = catch_prim!(def.expand($state,$gullet.mouth(),ncmd.clone(),Ptr::new($tk)) => ("edef",cmd));
                                            if !v.is_empty() {
                                                $gullet.mouth().push_tokens(v);
                                            }
                                        }
                                        Command::Conditional {name,index} => {
                                            catch_prim!(do_conditional($gullet,$state,$tk,name,*index) => ("edef",cmd));
                                        }
                                        _ => $f
                                    }
                                }
                            }
                            _ => $f
                        }
                    }
                }
            }
            file_end_prim!("edef",cmd)
        }
    }

    expand_group_with_unknowns!(state,gullet,{
        let def = Command::Def(Def{protected,long,outer,endswithbrace,arity,signature,replacement},cmd.cause.clone());
        match cs {
            BaseToken::Char(c,_) => {
                debug_log!(debug=>"\\edef: {} = {:?}",c,def);
                state.set_ac_command(*c,Some(Ptr::new(def)),global)
            }
            BaseToken::CS(name) => {
                debug_log!(debug=>"\\edef: \\{} = {:?}",name,def);
                state.set_command(name.clone(),Some(Ptr::new(def)),global)
            }
        }
        return Ok(())
    },(tk,expand) => {
        match tk.base() {
            BaseToken::Char(c,CategoryCode::Parameter) => {
                match catch_prim!(gullet.mouth().get_next(state) => ("edef",cmd)) {
                    None => file_end_prim!("edef",cmd),
                    Some((stk,_)) => match stk.base() {
                        BaseToken::Char(_,CategoryCode::Parameter) =>
                            replacement.push(ExpToken::ParamToken(tk)),
                        BaseToken::Char(c,_) => {
                            let u = c.to_usize();
                            if u < 48 || u - 48 > (arity as usize) {
                                return Err(ErrorInPrimitive{name:"edef",msg:Some(format!("Illegal parameter number {}",u-48)),cause:Some(cmd.cause),source:None})
                            }
                            replacement.push(ExpToken::Param(tk,(u-49) as u8))
                        }
                        _ =>
                        return Err(ErrorInPrimitive{name:"edef",msg:Some(format!("Expected number after #, got {}",stk)),cause:Some(cmd.cause),source:None})
                    }
                }
            }
            _ => replacement.push(ExpToken::Token(tk))
        }
    };
            Command::Gullet {name:"the",..} => {
                for t in catch_prim!(the(state,gullet,GulletCommand{cause:cmd.cause.clone()}) => ("edef",cmd)) {
                    replacement.push(ExpToken::Token(t));
                }
            }
            Command::Gullet {name:"unexpanded",..} => todo!("'unexpanded' in expansion"),
            Command::Gullet {name:"noexpand",..} => {
                match catch_prim!(gullet.mouth().get_next(state) => ("edef",cmd)) {
                    Some((tk,_)) => replacement.push(ExpToken::Token(tk)),
                    None => return Err(ErrorInPrimitive{name:"edef",msg:None,cause:Some(cmd.cause),source:Some(
                        UnexpectedEndgroup(tk).into()
                    )})
                }
            }
            Command::Def(def,_) if def.protected => replacement.push(ExpToken::Token(tk))
    );
    file_end_prim!("edef",cmd)
}

pub fn else_<T:Token,S:State<T>,Gu:Gullet<T,S=S>>(state:&mut S,gullet:&mut Gu,cmd:GulletCommand<T>) -> Result<Vec<T>,ErrorInPrimitive<T>> {
    catch_prim!(crate::engine::gullet::methods::else_loop(gullet,state) => ("else",cmd));
    Ok(vec![])
}

pub fn end<T:Token,Sto:Stomach<T>>(_stomach:&mut Sto,_state:&mut Sto::S,_cmd:StomachCommand<T>) -> Result<(),ErrorInPrimitive<T>> {
    todo!("end")
}

pub fn endgroup<T:Token,Sto:Stomach<T>>(_stomach:&mut Sto,state:&mut Sto::S,cmd:StomachCommand<T>) -> Result<(),ErrorInPrimitive<T>> {
    catch_prim!(state.stack_pop(<Sto::S as State<T>>::Gr::from_begingroup_cs()) => ("endgroup",cmd));
    Ok(())
}

pub fn endlinechar_assign<T:Token,S:State<T>,Gu:Gullet<T,S=S>>(state:&mut S,gullet:&mut Gu,cmd:StomachCommand<T>,global:bool) -> Result<(),ErrorInPrimitive<T>> {
    debug_log!(trace=>"Assigning \\endlinechar");
    catch_prim!(gullet.mouth().skip_eq_char(state) => ("endlinechar",cmd));
    let i = catch_prim!(gullet.get_int(state) => ("endlinechar",cmd));
    let c = match i.to_i64() {
        -1|255 => None,
        j => match T::Char::from_i64(j) {
            Some(c) => Some(c),
            None => return Err(ErrorInPrimitive{name:"endlinechar",msg:Some(format!("Not a valid character: {}",j)),cause:Some(cmd.cause),source:None})
        }
    };
    debug_log!(debug=>"\\endlinechar = {:?}",c.map(|c| c.char_str()));
    state.set_endlinechar(c,global);
    Ok(())
}
pub fn endlinechar_get<T:Token,S:State<T>,Gu:Gullet<T,S=S>>(state:&mut S,gullet:&mut Gu,cmd:GulletCommand<T>) -> Result<<S::NumSet as NumSet>::Int,ErrorInPrimitive<T>> {
    debug_log!(trace=>"Getting \\endlinechar");
    let c = match state.get_endlinechar() {
        None => -1,
        Some(c) => c.to_usize() as i64
    };
    debug_log!(debug=>"\\endlinechar == {:?}",c);
    Ok(<S::NumSet as NumSet>::Int::from_i64::<T>(c).unwrap())
}

// \errhelp

pub fn errmessage<T:Token,S:State<T>,Gu:Gullet<T,S=S>>(state:&mut S,gullet:&mut Gu,cmd:StomachCommand<T>) -> Result<(),ErrorInPrimitive<T>> {
    debug_log!(debug=>"errmessage");
    catch_prim!(gullet.mouth().skip_whitespace(state) => ("errmessage",cmd));
    let ret = catch_prim!(gullet.get_expanded_group(state,false,false,true) => ("errmessage",cmd));
    let errmsg = tokens_to_string(ret,state.get_escapechar());
    let eh = state.get_primitive_toks("errhelp");
    // TODO errhelp
    Err(ErrorInPrimitive{
        name:"errmessage",
        msg:Some(errmsg),
        cause:Some(cmd.cause),
        source:None
    }.into())
}

pub fn escapechar_assign<T:Token,S:State<T>,Gu:Gullet<T,S=S>>(state:&mut S,gullet:&mut Gu,cmd:StomachCommand<T>,global:bool) -> Result<(),ErrorInPrimitive<T>> {
    debug_log!(trace=>"Assigning \\escapechar");
    catch_prim!(gullet.mouth().skip_eq_char(state) => ("escapechar",cmd));
    let i = catch_prim!(gullet.get_int(state) => ("escapechar",cmd));
    let c = match i.to_i64() {
        -1|255 => None,
        j => match T::Char::from_i64(j) {
            Some(c) => Some(c),
            None => return Err(ErrorInPrimitive{name:"escapechar",msg:Some(format!("Not a valid character: {}",j)),cause:Some(cmd.cause),source:None})
        }
    };
    debug_log!(debug=>"\\escapechar = {:?}",c.map(|c| c.char_str()));
    state.set_escapechar(c,global);
    Ok(())
}
pub fn escapechar_get<T:Token,Gu:Gullet<T>>(state:&mut Gu::S,gullet:&mut Gu,cmd:GulletCommand<T>) -> Result<<<Gu::S as State<T>>::NumSet as NumSet>::Int,ErrorInPrimitive<T>> {
    debug_log!(trace=>"Getting \\escapechar");
    let c = match state.get_escapechar() {
        None => -1,
        Some(c) => c.to_usize() as i64
    };
    debug_log!(debug=>"\\escapechar == {}",c);
    Ok(<<Gu::S as State<T>>::NumSet as NumSet>::Int::from_i64::<T>(c).unwrap())
}

pub fn expandafter<T:Token,Gu:Gullet<T>>(state:&mut Gu::S,gullet:&mut Gu,cmd:GulletCommand<T>) -> Result<Vec<T>,ErrorInPrimitive<T>> {
    debug_log!(trace=>"expandafter");
    let (first,exp) = match catch_prim!(gullet.mouth().get_next(state) => ("expandafter",cmd)){
        None => file_end_prim!("expandafter",cmd),
        Some((t,b)) => (t,b)
    };
    match catch_prim!(gullet.mouth().get_next(state) => ("expandafter",cmd)){
        None => file_end_prim!("expandafter",cmd),
        Some((t,false)) => {
            gullet.mouth().push_noexpand(t);
        }
        Some((t,_)) => {
            if let Some(ncmd) = match t.base() {
                BaseToken::CS(n) => Some(catch_prim!(state.need_command(&n) => ("expandafter",cmd))),
                BaseToken::Char(c, CategoryCode::Active) =>
                    Some(catch_prim!(state.need_ac_command(*c) => ("expandafter",cmd))),
                _ => {
                    gullet.mouth().requeue(t.clone());
                    None
                }
            } {
                match &*ncmd {
                    Command::Conditional{name,index} =>
                        catch_prim!(crate::engine::gullet::methods::do_conditional(gullet,state,t,name,*index) => ("expandafter",cmd)),
                    Command::Gullet {name,index} =>
                        catch_prim!(crate::engine::gullet::methods::do_expandable(gullet,state,t,name,*index) => ("expandafter",cmd)),
                    _ => gullet.mouth().requeue(t)
                }
            }
        }
    };
    if exp {
        gullet.mouth().requeue(first);
    } else {
        gullet.mouth().push_noexpand(first);
    }
    Ok(vec!())
}

pub fn fi<T:Token,Gu:Gullet<T>>(_state:&mut Gu::S,gullet:&mut Gu,_cmd:GulletCommand<T>) -> Result<Vec<T>,ErrorInPrimitive<T>> {
    debug_log!(trace=>"...end of conditional.");
    gullet.pop_conditional();
    Ok(vec![])
}


pub fn gdef<T:Token,S:State<T>,Gu:Gullet<T,S=S>>(state:&mut S,gullet:&mut Gu,cmd:StomachCommand<T>,global:bool,protected:bool,long:bool,outer:bool)
                                                 -> Result<(),ErrorInPrimitive<T>> {
    def(state,gullet,cmd,true,protected,long,outer)
}

pub fn global<T:Token,Sto:Stomach<T>>(stomach:&mut Sto,state:&mut Sto::S,gullet:&mut Sto::Gu,cmd:StomachCommand<T>,global_:bool,protected_:bool,long_:bool,outer_:bool)
                                                 -> Result<(),ErrorInPrimitive<T>> {
    debug_log!(trace => "\\global");
    match catch_prim!(gullet.get_next_stomach_command(state) => ("global",cmd)) {
        None => file_end_prim!("global",cmd),
        Some(c) => match c.cmd {
            StomachCommandInner::Assignment {name:"global",..} => global(stomach,state,gullet,cmd,true,protected_,long_,outer_),
            StomachCommandInner::Assignment {name:"protected",..} => protected(stomach,state,gullet,cmd,true,protected_,long_,outer_),
            StomachCommandInner::Assignment {name:"long",..} => long(stomach,state,gullet,cmd,true,protected_,long_,outer_),
            StomachCommandInner::Assignment {name:"outer",..} => outer(stomach,state,gullet,cmd,true,protected_,long_,outer_),
            StomachCommandInner::Assignment {name:"def",..} => def(state,gullet,cmd,true,protected_,long_,outer_),
            StomachCommandInner::Assignment {name:"edef",..} => edef(state,gullet,cmd,true,protected_,long_,outer_),
            StomachCommandInner::Assignment {name:"gdef",..} => gdef(state,gullet,cmd,true,protected_,long_,outer_),
            StomachCommandInner::Assignment {name:"xdef",..} => xdef(state,gullet,cmd,true,protected_,long_,outer_),
            StomachCommandInner::Assignment {index,..} => {
                match stomach.command(index) {
                    None => Err(ErrorInPrimitive{name:"global",msg:Some(format!("Invalid assignment: {}",index)),cause:Some(cmd.cause),source:None}),
                    Some(f) => f(state,gullet,stomach,c,true)
                }
            }
            _ => todo!("global: {:?}",c)
        }
    }
}

pub fn ifeof<T:Token,Gu:Gullet<T>>(state:&mut Gu::S,gullet:&mut Gu,cmd:GulletCommand<T>) -> Result<bool,ErrorInPrimitive<T>> {
    debug_log!(trace=>"ifeof");
    let i = catch_prim!(gullet.get_int(state) => ("ifeof",cmd));
    let i : usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => return Err(ErrorInPrimitive{name:"openin",msg:Some(format!("Invalid file number: {}",i)),cause:Some(cmd.cause),source:None})
    };
    if i == 18 { return Ok(false) }
    let f = match state.get_open_in_file(i) {
        None => return Err(ErrorInPrimitive{name:"openin",msg:Some(format!("No in file open at index: {}",i)),cause:Some(cmd.cause),source:None}.into()),
        Some(f) => f
    };
    Ok(f.eof())
}

pub fn ifnum<T:Token,Gu:Gullet<T>>(state:&mut Gu::S,gullet:&mut Gu,cmd:GulletCommand<T>) -> Result<bool,ErrorInPrimitive<T>> {
    debug_log!(trace=>"ifnum");
    let i1 = catch_prim!(gullet.get_int(state) => ("ifnum",cmd));
    let rel = match catch_prim!(gullet.get_keywords(state,vec!["<",">","="]) => ("ifnum",cmd)) {
        None => return Err(ErrorInPrimitive{name:"ifnum",msg:Some("Expected one of '<','>','='".to_string()),cause:Some(cmd.cause),source:None}),
        Some(r) => r
    };
    let i2 = catch_prim!(gullet.get_int(state) => ("ifnum",cmd));
    match rel {
        "<" => Ok(i1<i2),
        ">" => Ok(i1>i2),
        "=" => Ok(i1==i2),
        _ => unreachable!()
    }
}

pub fn ifx<T:Token,Gu:Gullet<T>>(state:&mut Gu::S,gullet:&mut Gu,cmd:GulletCommand<T>) -> Result<bool,ErrorInPrimitive<T>> {
    debug_log!(trace=>"ifx");
    let (t1,exp1) = match catch_prim!(gullet.mouth().get_next(state) => ("ifx",cmd)) {
        None => file_end_prim!("ifx",cmd),
        Some(t) => t
    };
    let (t2,exp2) = match catch_prim!(gullet.mouth().get_next(state) => ("ifx",cmd)) {
        None => file_end_prim!("ifx",cmd),
        Some(t) => t
    };
    debug_log!(trace=>"ifx: {} == {}?",t1,t2);
    match (t1.base(),t2.base()) {
        (BaseToken::Char(c1,CategoryCode::Active),BaseToken::Char(c2,CategoryCode::Active)) => {
            let cmd1 = state.get_ac_command(*c1);
            let cmd2 = state.get_ac_command(*c2);
            Ok(ifx_eq_cmd(cmd1,exp1,cmd2,exp2))
        }
        (BaseToken::CS(name),BaseToken::Char(c2,CategoryCode::Active)) =>{
            let cmd1 = state.get_command(name);
            let cmd2 = state.get_ac_command(*c2);
            Ok(ifx_eq_cmd(cmd1,exp1,cmd2,exp2))
        }
        (BaseToken::Char(c1,CategoryCode::Active),BaseToken::CS(name)) =>{
            let cmd1 = state.get_ac_command(*c1);
            let cmd2 = state.get_command(name);
            Ok(ifx_eq_cmd(cmd1,exp1,cmd2,exp2))
        }
        (BaseToken::CS(name1),BaseToken::CS(name2)) =>{
            let cmd1 = state.get_command(name1);
            let cmd2 = state.get_command(name2);
            Ok(ifx_eq_cmd(cmd1,exp1,cmd2,exp2))
        }
        (BaseToken::Char(c1,cc1),BaseToken::Char(c2,cc2)) =>
            Ok(c1==c2 && cc1 == cc2),
        (BaseToken::Char(c1,cc1),BaseToken::CS(name)) =>
            Ok(match state.get_command(name).as_deref() {
                Some(Command::Char{char,catcode}) => *char == *c1 && *catcode == *cc1,
                _ => false
            }),
        (BaseToken::CS(name),BaseToken::Char(c2,cc2)) =>
            Ok(match state.get_command(name).as_deref() {
                Some(Command::Char{char,catcode}) => *char == *c2 && *catcode == *cc2,
                _ => false
            })
    }
}

fn ifx_eq_cmd<T:Token>(cmd1:Option<Ptr<Command<T>>>,expand1:bool,cmd2:Option<Ptr<Command<T>>>,expand2:bool) -> bool {
    if !expand1 || !expand2 {
        todo!("\\noexpand commands in ifx")
    }
    debug_log!(debug=>"ifx_eq_cmd: {:?} == {:?}?",cmd1,cmd2);
    cmd1 == cmd2
}


pub fn immediate<T:Token,Sto:Stomach<T>+'static>(state:&mut Sto::S,gullet:&mut Sto::Gu,stomach:&mut Sto,cmd:StomachCommand<T>)
                                                 -> Result<(),ErrorInPrimitive<T>> {
    debug_log!(trace=>"immediate");
    match catch_prim!(gullet.get_next_stomach_command(state) => ("immediate",cmd)) {
        None => file_end_prim!("immediate",cmd),
        Some(sc) => match sc.cmd {
            StomachCommandInner::Whatsit {name,index} => {
                let fun = match stomach.get_whatsit_cmd(index) {
                    Some(f) => f,
                    None => return Err(ErrorInPrimitive{name:"immediate",msg:None,cause:Some(cmd.cause),source:Some(
                        ImplementationError(format!("Missing implementation for whatsit command {}",name),PhantomData).into()
                    )})
                };
                let wi = catch_prim!(fun(state,gullet,stomach,sc) => ("immediate",cmd));
                catch_prim!((wi.apply)(stomach,state,gullet) => ("immediate",cmd));
                Ok(())
            }
            _ => {
                gullet.mouth().requeue(sc.cause);
                Ok(())
            }
        }
    }
}

pub fn input<T:Token,Gu:Gullet<T>>(state:&mut Gu::S,gullet:&mut Gu,cmd:GulletCommand<T>) -> Result<Vec<T>,ErrorInPrimitive<T>> {
    debug_log!(trace=>"input");
    let filename = catch_prim!(gullet.get_string(state) => ("input",cmd)).to_string();
    debug_log!(trace=>"input: {}",filename);
    let file = state.filesystem().get(&filename);
    debug_log!(trace=>"input resolved: {:?}",file.path());
    if !file.exists() {
        Err(ErrorInPrimitive{name:"input",msg:Some(format!("I can't find file `{}'",filename)),cause:Some(cmd.cause),source:None})
    } else {
        gullet.mouth().push_file(&file);
        Ok(vec!())
    }
}

pub fn let_<T:Token,Gu:Gullet<T>>(state:&mut Gu::S,gullet:&mut Gu,cmd:StomachCommand<T>,globally:bool) -> Result<(),ErrorInPrimitive<T>> {
    debug_log!(trace=>"let");
    let csO = catch_prim!(gullet.mouth().get_next(state) => ("let",cmd));
    let cs = match &csO {
        None => file_end_prim!("let",cmd),
        Some((t,_)) => t.base()
    };
    match cs {
        BaseToken::Char(c,CategoryCode::Active) => {
            catch_prim!(gullet.mouth().skip_eq_char(state) => ("let",cmd));
            let csO = catch_prim!(gullet.mouth().get_next(state) => ("let",cmd));
            let cs = match &csO {
                None => file_end_prim!("let",cmd),
                Some((t,_)) => t.base()
            };
            let cmd = match cs {
                BaseToken::Char(c,CategoryCode::Active) => state.get_ac_command(*c),
                BaseToken::CS(name) => state.get_command(name),
                BaseToken::Char(c,cc) =>
                    Some(Ptr::new(Command::Char{char:*c,catcode:*cc}))
            };
            debug_log!(debug=>"let: {} = {:?}",c,cmd);
            state.set_ac_command(*c,cmd,globally);
        }
        BaseToken::CS(name) => {
            catch_prim!(gullet.mouth().skip_eq_char(state) => ("let",cmd));
            let csO = catch_prim!(gullet.mouth().get_next(state) => ("let",cmd));
            let cs = match &csO {
                None => file_end_prim!("let",cmd),
                Some((t,_)) => t.base()
            };
            let cmd = match cs {
                BaseToken::Char(c,CategoryCode::Active) => state.get_ac_command(*c),
                BaseToken::CS(name) => state.get_command(name),
                BaseToken::Char(c,cc) =>
                    Some(Ptr::new(Command::Char{char:*c,catcode:*cc}))
            };
            debug_log!(debug=>"let: \\{} = {:?}",name,cmd);
            state.set_command(name.clone(),cmd,globally);
        }
        _ => return Err(ErrorInPrimitive{name:"let",msg:Some("Expected a control sequence".to_string()),cause:Some(csO.unwrap().0),source:None})
    }
    Ok(())
}

pub fn long<T:Token,Sto:Stomach<T>>(stomach:&mut Sto, state:&mut Sto::S,gullet:&mut Sto::Gu,cmd:StomachCommand<T>,global_:bool,protected_:bool,long_:bool,outer_:bool)
                                                -> Result<(),ErrorInPrimitive<T>> {
    debug_log!(trace => "\\long");
    match catch_prim!(gullet.get_next_stomach_command(state) => ("long",cmd)) {
        None => file_end_prim!("long",cmd),
        Some(c) => match c.cmd {
            StomachCommandInner::Assignment {name:"global",..} => global(stomach,state,gullet,cmd,global_,protected_,true,outer_),
            StomachCommandInner::Assignment {name:"protected",..} => protected(stomach,state,gullet,cmd,global_,protected_,true,outer_),
            StomachCommandInner::Assignment {name:"long",..} => long(stomach,state,gullet,cmd,global_,protected_,true,outer_),
            StomachCommandInner::Assignment {name:"outer",..} => outer(stomach,state,gullet,cmd,global_,protected_,true,outer_),
            StomachCommandInner::Assignment {name:"def",..} => def(state,gullet,cmd,global_,protected_,true,outer_),
            StomachCommandInner::Assignment {name:"edef",..} => edef(state,gullet,cmd,global_,protected_,true,outer_),
            StomachCommandInner::Assignment {name:"gdef",..} => gdef(state,gullet,cmd,global_,protected_,true,outer_),
            StomachCommandInner::Assignment {name:"xdef",..} => xdef(state,gullet,cmd,global_,protected_,true,outer_),
            _ => return Err(ErrorInPrimitive{name:"long",msg:Some("Expected a macro definition after \\long".to_string()),cause:Some(cmd.cause),source:None})
        }
    }
}

// \mag           : Int

pub fn mathchardef<T:Token,Gu:Gullet<T>>(state:&mut Gu::S,gullet:&mut Gu,cmd:StomachCommand<T>,globally:bool) -> Result<(),ErrorInPrimitive<T>> {
    debug_log!(trace=>"mathchardef");
    let csO = catch_prim!(gullet.mouth().get_next(state) => ("mathchardef",cmd));
    let cs = match &csO {
        None => file_end_prim!("mathchardef",cmd),
        Some((t,_)) => t.base()
    };
    match cs {
        BaseToken::Char(_,CategoryCode::Active) => (),
        BaseToken::CS(_) => (),
        _ => return Err(ErrorInPrimitive{name:"mathchardef",msg:Some(format!("Command expected after \\mathchardef")),cause:Some(csO.unwrap().0),source:None})
    }
    catch_prim!(gullet.mouth().skip_eq_char(state) => ("mathchardef",cmd));
    let i = catch_prim!(gullet.get_int(state) => ("mathchardef",cmd)).to_i64();
    if i < 0 {
        return Err(ErrorInPrimitive{name:"mathchardef",msg:Some(format!("Invalid math char: {}",i)),cause:Some(csO.unwrap().0),source:None})
    }
    match cs {
        BaseToken::Char(c,_) => state.set_ac_command(*c,Some(Ptr::new(Command::MathChar(i as u32))),globally),
        BaseToken::CS(name) => state.set_command(name.clone(),Some(Ptr::new(Command::MathChar(i as u32))),globally)
    }
    Ok(())
}

pub fn meaning<T:Token,Gu:Gullet<T>>(state:&mut Gu::S,gullet:&mut Gu,cmd:GulletCommand<T>) -> Result<Vec<T>,ErrorInPrimitive<T>> {
    debug_log!(trace=>"meaning");
    match catch_prim!(gullet.mouth().get_next(state) => ("meaning",cmd)) {
        None => file_end_prim!("meaning",cmd),
        Some((_,false)) => {
            match state.get_escapechar() {
                None => Ok(string_to_tokens("relax".as_bytes())),
                Some(c) => {
                    let mut string = vec!();
                    for u in c.as_bytes() {string.push(u)}
                    for u in "relax".as_bytes() {string.push(*u)}
                    Ok(string_to_tokens(&string))
                }
            }
        }
        Some((t,_)) => match t.base() {
            BaseToken::CS(name) => Ok(meaning_cmd(state.get_command(name),state.get_escapechar())),
            BaseToken::Char(c,CategoryCode::Active) => Ok(meaning_cmd(state.get_ac_command(*c),state.get_escapechar())),
            BaseToken::Char(c,cc) => Ok(meaning_char(*c,*cc)),
        }
    }
}

pub fn meaning_char<T:Token>(c:T::Char,cc:CategoryCode) -> Vec<T> {
    match cc {
        CategoryCode::BeginGroup => string_to_tokens(&format!("begin-group character {}",c.char_str()).as_bytes()),
        CategoryCode::EndGroup => string_to_tokens(&format!("end-group character {}",c.char_str()).as_bytes()),
        CategoryCode::MathShift => string_to_tokens(&format!("math shift character {}",c.char_str()).as_bytes()),
        CategoryCode::AlignmentTab => string_to_tokens(&format!("alignment tab character {}",c.char_str()).as_bytes()),
        CategoryCode::Parameter => string_to_tokens(&format!("macro parameter character {}",c.char_str()).as_bytes()),
        CategoryCode::Superscript => string_to_tokens(&format!("superscript character {}",c.char_str()).as_bytes()),
        CategoryCode::Subscript => string_to_tokens(&format!("subscript character {}",c.char_str()).as_bytes()),
        CategoryCode::Space => string_to_tokens(&format!("blank space {}",c.char_str()).as_bytes()),
        CategoryCode::Letter => string_to_tokens(&format!("the letter {}",c.char_str()).as_bytes()),
        _ => string_to_tokens(&format!("the character {}",c).as_bytes()),
    }
}

pub fn meaning_cmd<T:Token>(cmd:Option<Ptr<Command<T>>>,escapechar:Option<T::Char>) -> Vec<T> {
    match cmd {
        None => string_to_tokens("undefined".as_bytes()),
        Some(cmd) => {
            match &*cmd {
                Command::Def(d,_) => {
                    let esc = match escapechar {
                        None => vec!(),
                        Some(c) => c.as_bytes()
                    };
                    let mut ret = vec!();
                    if d.protected {
                        ret.extend(esc.clone());
                        ret.extend("protected ".as_bytes());
                    }
                    if d.long {
                        ret.extend(esc.clone());
                        ret.extend("long ".as_bytes());
                    }
                    if d.outer {
                        ret.extend(esc.clone());
                        ret.extend("outer ".as_bytes());
                    }
                    ret.extend("macro:".as_bytes());
                    let mut i = 0;
                    for s in &d.signature {
                        match s {
                            ParamToken::Token(t) => ret.extend(tokens_to_string(vec!(t.clone()),escapechar).as_bytes()),
                            ParamToken::Param => {
                                i += 1;
                                ret.extend(format!("#{}",i).as_bytes());
                            }
                        }
                    }
                    if d.endswithbrace { ret.push(b'#'); }
                    ret.push(b'-'); ret.push(b'>');
                    for t in &d.replacement {
                        match t {
                            ExpToken::Token(t) => ret.extend(tokens_to_string(vec!(t.clone()),escapechar).as_bytes()),
                            ExpToken::ParamToken(t) => ret.extend(tokens_to_string(vec!(t.clone(),t.clone()),escapechar).as_bytes()),
                            ExpToken::Param(t,i) => {
                                ret.extend(tokens_to_string(vec!(t.clone()),escapechar).as_bytes());
                                ret.extend(i.to_string().as_bytes());
                            }
                        }
                    }
                    debug_log!(debug=>"meaning_cmd: {}",std::str::from_utf8(&ret).unwrap());
                    string_to_tokens(&ret)
                }
                Command::Stomach {name,..} => {
                    match escapechar {
                        None => string_to_tokens(name.as_bytes()),
                        Some(c) => {
                            let mut string = vec!();
                            for u in c.as_bytes() {string.push(u)}
                            for u in name.as_bytes() {string.push(*u)}
                            string_to_tokens(&string)
                        }
                    }
                }
                Command::AssignableValue {name,..} => {
                    match escapechar {
                        None => string_to_tokens(name.as_bytes()),
                        Some(c) => {
                            let mut string = vec!();
                            for u in c.as_bytes() {string.push(u)}
                            for u in name.as_bytes() {string.push(*u)}
                            string_to_tokens(&string)
                        }
                    }
                }
                Command::Value {name,..} => {
                    match escapechar {
                        None => string_to_tokens(name.as_bytes()),
                        Some(c) => {
                            let mut string = vec!();
                            for u in c.as_bytes() {string.push(u)}
                            for u in name.as_bytes() {string.push(*u)}
                            string_to_tokens(&string)
                        }
                    }
                }
                Command::ValueRegister {tp:Assignable::Int,index} => {
                    let mut string = vec!();
                    match escapechar {
                        None => (),
                        Some(c) => {
                            for u in c.as_bytes() {string.push(u)}
                        }
                    }
                    for u in "count".as_bytes() {string.push(*u)}
                    for u in index.to_string().as_bytes() {string.push(*u)}
                    string_to_tokens(&string)
                }
                Command::ValueRegister {..} => todo!("meaning_cmd: ValueRegister"),
                Command::ValueAssignment {name,..} => {
                    match escapechar {
                        None => string_to_tokens(name.as_bytes()),
                        Some(c) => {
                            let mut string = vec!();
                            for u in c.as_bytes() {string.push(u)}
                            for u in name.as_bytes() {string.push(*u)}
                            string_to_tokens(&string)
                        }
                    }
                }
                Command::Assignment {name,..} => {
                    match escapechar {
                        None => string_to_tokens(name.as_bytes()),
                        Some(c) => {
                            let mut string = vec!();
                            for u in c.as_bytes() {string.push(u)}
                            for u in name.as_bytes() {string.push(*u)}
                            string_to_tokens(&string)
                        }
                    }
                }
                Command::Relax => {
                    match escapechar {
                        None => string_to_tokens("relax".as_bytes()),
                        Some(c) => {
                            let mut string = vec!();
                            for u in c.as_bytes() {string.push(u)}
                            for u in "relax".as_bytes() {string.push(*u)}
                            string_to_tokens(&string)
                        }
                    }
                }
                Command::Conditional {name,..} => {
                    match escapechar {
                        None => string_to_tokens(name.as_bytes()),
                        Some(c) => {
                            let mut string = vec!();
                            for u in c.as_bytes() {string.push(u)}
                            for u in name.as_bytes() {string.push(*u)}
                            string_to_tokens(&string)
                        }
                    }
                }
                Command::Gullet {name,..} => {
                    match escapechar {
                        None => string_to_tokens(name.as_bytes()),
                        Some(c) => {
                            let mut string = vec!();
                            for u in c.as_bytes() {string.push(u)}
                            for u in name.as_bytes() {string.push(*u)}
                            string_to_tokens(&string)
                        }
                    }
                }
                Command::Char {char,catcode} => meaning_char(*char,*catcode),
                Command::MathChar(index) => {
                    let mut string = vec!();
                    match escapechar {
                        None => (),
                        Some(c) => {
                            for u in c.as_bytes() {string.push(u)}
                        }
                    }
                    for u in "mathchar\"".as_bytes() {string.push(*u)}
                    for u in format!("{:X}", index).as_bytes() {string.push(*u)}
                    string_to_tokens(&string)
                }
                Command::Whatsit {name,..} => {
                    match escapechar {
                        None => string_to_tokens(name.as_bytes()),
                        Some(c) => {
                            let mut string = vec!();
                            for u in c.as_bytes() {string.push(u)}
                            for u in name.as_bytes() {string.push(*u)}
                            string_to_tokens(&string)
                        }
                    }
                }
            }
        }
    }
}

pub fn message<T:Token,S:State<T>,Gu:Gullet<T,S=S>>(state:&mut S,gullet:&mut Gu,cmd:StomachCommand<T>) -> Result<(),ErrorInPrimitive<T>> {
    debug_log!(debug=>"message");
    catch_prim!(gullet.mouth().skip_whitespace(state) => ("message",cmd));
    let ret = catch_prim!(gullet.get_expanded_group(state,false,false,true) => ("message",cmd));
    let msg = tokens_to_string(ret,state.get_escapechar());
    (state.outputs().message)(&msg);
    Ok(())
}

pub fn month<T:Token,S:State<T>,Gu:Gullet<T,S=S>>(state:&mut S,_gullet:&mut Gu,cmd:GulletCommand<T>) -> Result<<S::NumSet as NumSet>::Int,ErrorInPrimitive<T>> {
    Ok(catch_prim!(<S::NumSet as NumSet>::Int::from_i64(
        state.get_start_time().month() as i64
    ) => ("month",cmd)))
}

pub fn multiply<T:Token,S:State<T>,Gu:Gullet<T,S=S>>(state:&mut S,gullet:&mut Gu,cmd:StomachCommand<T>,global:bool)
                                                   -> Result<(),ErrorInPrimitive<T>> {
    modify_in_place!(multiply,state,gullet,cmd,global,"*",*);
}


pub fn muskip_assign<T:Token,S:State<T>,Gu:Gullet<T,S=S>>(state:&mut S,gullet:&mut Gu,cmd:StomachCommand<T>,global:bool) -> Result<(),ErrorInPrimitive<T>> {
    debug_log!(trace=>"Assigning \\muskip");
    let i = catch_prim!(gullet.get_int(state) => ("muskip",cmd));
    let i:usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => return Err(ErrorInPrimitive{name:"muskip",msg:Some(format!("Not a valid register: {}",i)),cause:Some(cmd.cause),source:None})
    };
    catch_prim!(gullet.mouth().skip_eq_char(state) => ("muskip",cmd));
    let v = catch_prim!(gullet.get_muskip(state) => ("muskip",cmd));
    debug_log!(debug=>"\\muskip{} = {}",i,v);
    state.set_muskip_register(i,v,global);
    Ok(())
}
pub fn muskip_get<T:Token,S:State<T>,Gu:Gullet<T,S=S>>(state:&mut S,gullet:&mut Gu,cmd:GulletCommand<T>) -> Result<<S::NumSet as NumSet>::MuSkip,ErrorInPrimitive<T>> {
    debug_log!(trace=>"Getting \\muskip");
    let i = catch_prim!(gullet.get_int(state) => ("muskip",cmd));
    let i:usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => return Err(ErrorInPrimitive{name:"muskip",msg:Some(format!("Not a valid register: {}",i)),cause:Some(cmd.cause),source:None})
    };
    let v = state.get_muskip_register(i);
    debug_log!(debug=>"\\muskip{} == {}",i,v);
    Ok(v)
}

pub fn muskipdef<T:Token,S:State<T>,Gu:Gullet<T,S=S>>(state:&mut S,gullet:&mut Gu,cmd:StomachCommand<T>,global:bool)
                                                     -> Result<(),ErrorInPrimitive<T>> {
    debug_log!(trace=>"muskipdef");
    let name = catch_prim!(gullet.get_control_sequence(state) => ("muskipdef",cmd));
    match &name {
        BaseToken::Char(c,_) => {
            state.set_ac_command(*c,Some(Ptr::new(Command::Relax)),false)
        }
        BaseToken::CS(name) => {
            state.set_command(name.clone(),Some(Ptr::new(Command::Relax)),false)
        }
    }
    catch_prim!(gullet.mouth().skip_eq_char(state) => ("muskipdef",cmd));
    let num = catch_prim!(gullet.get_int(state) => ("muskipdef",cmd));
    if num.to_i64() < 0 {
        return Err(ErrorInPrimitive{name:"muskipdef",msg:Some(format!("Invalid muskip register index: {}",num)),cause:Some(cmd.cause),source:None})
    }
    let num = num.to_i64() as usize;
    match name {
        BaseToken::Char(c,_) => {
            debug_log!(debug=>"\\muskipdef: {} = \\muskip{}",c.char_str(),num);
            state.set_ac_command(c, Some(Ptr::new(Command::ValueRegister{index:num,tp:Assignable::MuSkip })), global);
        }
        BaseToken::CS(name) => {
            debug_log!(debug=>"\\muskipdef: \\{} = {}",name,num);
            state.set_command(name, Some(Ptr::new(Command::ValueRegister{index:num,tp:Assignable::MuSkip})), global);
        }
    }
    Ok(())
}


pub fn newlinechar_assign<T:Token,S:State<T>,Gu:Gullet<T,S=S>>(state:&mut S,gullet:&mut Gu,cmd:StomachCommand<T>,global:bool) -> Result<(),ErrorInPrimitive<T>> {
    debug_log!(trace=>"Assigning \\newlinechar");
    catch_prim!(gullet.mouth().skip_eq_char(state) => ("newlinechar",cmd));
    let i = catch_prim!(gullet.get_int(state) => ("newlinechar",cmd));
    let c = match i.to_i64() {
        -1|255 => None,
        j => match T::Char::from_i64(j) {
            Some(c) => Some(c),
            None => return Err(ErrorInPrimitive{name:"newlinechar",msg:Some(format!("Not a valid character: {}",j)),cause:Some(cmd.cause),source:None})
        }
    };
    debug_log!(debug=>"\\newlinechar = {:?}",c.map(|c| c.char_str()));
    state.set_newlinechar(c,global);
    Ok(())
}
pub fn newlinechar_get<T:Token,S:State<T>,Gu:Gullet<T,S=S>>(state:&mut S,gullet:&mut Gu,cmd:GulletCommand<T>) -> Result<<S::NumSet as NumSet>::Int,ErrorInPrimitive<T>> {
    debug_log!(trace=>"Getting \\newlinechar");
    let c = match state.get_newlinechar() {
        None => -1,
        Some(c) => c.to_usize() as i64
    };
    debug_log!(debug=>"\\newlinechar == {}",c);
    Ok(<S::NumSet as NumSet>::Int::from_i64::<T>(c).unwrap())
}

// invariant: adds token as nonexpanded to the gullet iff the original token was expandable
// in the first place
pub fn noexpand<T:Token,Gu:Gullet<T>>(state:&mut Gu::S,gullet:&mut Gu,cmd:GulletCommand<T>) -> Result<Vec<T>,ErrorInPrimitive<T>> {
    debug_log!(trace=>"\\noexpand");
    match catch_prim!(gullet.mouth().get_next(state) => ("noexpand",cmd)) {
        None => file_end_prim!("noexpand",cmd),
        Some((t,_)) => match t.base() {
            BaseToken::Char(c,CategoryCode::Active) => {
                match state.get_ac_command(*c) {
                    None => gullet.mouth().requeue(t),
                    Some(ac) => {
                        match &*ac {
                            Command::Def(_,_) => gullet.mouth().push_noexpand(t),
                            Command::Gullet {..} => gullet.mouth().push_noexpand(t),
                            Command::Conditional {..} => gullet.mouth().push_noexpand(t),
                            _ => gullet.mouth().requeue(t)
                        }
                    }
                }
            }
            BaseToken::CS(name) => {
                match state.get_command(name) {
                    None => gullet.mouth().requeue(t),
                    Some(ac) => {
                        match &*ac {
                            Command::Def(_,_) => gullet.mouth().push_noexpand(t),
                            Command::Gullet {..} => gullet.mouth().push_noexpand(t),
                            Command::Conditional {..} => gullet.mouth().push_noexpand(t),
                            _ => gullet.mouth().requeue(t)
                        }
                    }
                }
            }
            _ => gullet.mouth().requeue(t)
        }
    }
    Ok(vec!())
}

pub fn number<T:Token,S:State<T>,Gu:Gullet<T,S=S>>(state:&mut S,gullet:&mut Gu,cmd:GulletCommand<T>) -> Result<Vec<T>,ErrorInPrimitive<T>> {
    debug_log!(trace=>"\\number");
    let num = catch_prim!(gullet.get_int(state) => ("number",cmd));
    let ret =
        num.to_i64().to_string().as_bytes().into_iter().map(|c| T::new(BaseToken::Char(T::Char::from(*c),
            if *c == 32 {CategoryCode::Space} else {CategoryCode::Other}
        ),None)).collect();
    Ok(ret)
}


pub fn openin<T:Token,Sto:Stomach<T>>(state: &mut Sto::S, gullet:&mut Sto::Gu, stomach:&mut Sto, cmd:StomachCommand<T>) -> Result<(), ErrorInPrimitive<T>> {
    debug_log!(trace=>"\\openin");
    let i = catch_prim!(gullet.get_int(state) => ("openin",cmd));
    let i : usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => return Err(ErrorInPrimitive{name:"openin",msg:Some(format!("Invalid file number: {}",i)),cause:Some(cmd.cause),source:None})
    };
    gullet.mouth().skip_eq_char(state);
    let filename = catch_prim!(gullet.get_string(state) => ("openin",cmd)).to_string();
    let f = state.filesystem().get(&filename);
    state.file_openin(i,f); // TODO error?
    Ok(())
}

pub fn openout<T:Token,Sto:Stomach<T>>(state: &mut Sto::S, gullet:&mut Sto::Gu, stomach:&mut Sto, cmd:StomachCommand<T>) -> Result<Whatsit<T, Sto>, ErrorInPrimitive<T>> {
    debug_log!(trace=>"\\openout");
    let i = catch_prim!(gullet.get_int(state) => ("openout",cmd));
    let i : usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => return Err(ErrorInPrimitive{name:"openout",msg:Some(format!("Invalid file number: {}",i)),cause:Some(cmd.cause),source:None})
    };
    gullet.mouth().skip_eq_char(state);
    let filename = catch_prim!(gullet.get_string(state) => ("openout",cmd)).to_string();
    let apply = Box::new(move |_stomach:&mut Sto,state:&mut Sto::S,_gullet:&mut Sto::Gu| {
        let f = state.filesystem().get(&filename);
        state.file_openout(i,f); // TODO error?
        Ok(())
    });
    Ok(Whatsit { apply })
}

pub fn outer<T:Token,Sto:Stomach<T>>(stomach:&mut Sto,state:&mut Sto::S,gullet:&mut Sto::Gu,cmd:StomachCommand<T>,global_:bool,protected_:bool,long_:bool,outer_:bool)
                                                      -> Result<(),ErrorInPrimitive<T>> {
    debug_log!(trace => "\\outer");
    match catch_prim!(gullet.get_next_stomach_command(state) => ("outer",cmd)) {
        None => file_end_prim!("outer",cmd),
        Some(c) => match c.cmd {
            StomachCommandInner::Assignment {name:"global",..} => global(stomach,state,gullet,cmd,global_,protected_,long_,true),
            StomachCommandInner::Assignment {name:"protected",..} => protected(stomach,state,gullet,cmd,global_,protected_,long_,true),
            StomachCommandInner::Assignment {name:"long",..} => long(stomach,state,gullet,cmd,global_,protected_,long_,true),
            StomachCommandInner::Assignment {name:"outer",..} => outer(stomach,state,gullet,cmd,global_,protected_,long_,true),
            StomachCommandInner::Assignment {name:"def",..} => def(state,gullet,cmd,global_,protected_,long_,true),
            StomachCommandInner::Assignment {name:"edef",..} => edef(state,gullet,cmd,global_,protected_,long_,true),
            StomachCommandInner::Assignment {name:"gdef",..} => gdef(state,gullet,cmd,global_,protected_,long_,true),
            StomachCommandInner::Assignment {name:"xdef",..} => xdef(state,gullet,cmd,global_,protected_,long_,true),
            _ => return Err(ErrorInPrimitive{name:"outer",msg:Some("Expected a macro definition after \\outer".to_string()),cause:Some(cmd.cause),source:None})
        }
    }
}

pub fn par<T:Token,Sto:Stomach<T>>(_stomach:&mut Sto,state:&mut Sto::S,_cmd:StomachCommand<T>) -> Result<(),ErrorInPrimitive<T>> {
    debug_log!(trace=>"par");
    if state.mode().is_vertical() {Ok(())} else {
        todo!("par in horizontal mode")
    }
}


pub fn read<T:Token,Gu:Gullet<T>>(state:&mut Gu::S,gullet:&mut Gu,cmd:StomachCommand<T>,globally:bool) -> Result<(),ErrorInPrimitive<T>> {
    debug_log!(trace=>"read");
    let i = catch_prim!(gullet.get_int(state) => ("read",cmd));
    let i : usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => return Err(ErrorInPrimitive{name:"read",msg:Some(format!("Invalid file number: {}",i)),cause:Some(cmd.cause),source:None})
    };
    let file = match state.get_open_in_file(i) {
        None => return Err(ErrorInPrimitive{name:"read",msg:Some(format!("File {} not open for reading",i)),cause:Some(cmd.cause),source:None}),
        Some(f) => f
    };
    if !catch_prim!(gullet.get_keyword(state,"to") => ("read",cmd)) {
        return Err(ErrorInPrimitive{name:"read",msg:Some("Expected 'to' after \\read".to_string()),cause:Some(cmd.cause),source:None})
    }
    let newcmd = catch_prim!(gullet.get_control_sequence(state) => ("read",cmd));
    let ret = catch_prim!(file.read(state) => ("read",cmd)).into_iter().map(|tk| ExpToken::Token(tk)).collect();
    let def = Def {
        protected: false,
        long: false,
        outer: false,
        endswithbrace:false,
        replacement: ret,
        arity:0,
        signature:vec!()
    };
    match newcmd {
        BaseToken::CS(name) => state.set_command(name,Some(Ptr::new(
            Command::Def(def,cmd.cause.clone())
        )),globally),
        BaseToken::Char(c,_) => state.set_ac_command(c,Some(Ptr::new(
            Command::Def(def,cmd.cause.clone())
        )),globally)
    }
    Ok(())
}


pub fn skip_assign<T:Token,S:State<T>,Gu:Gullet<T,S=S>>(state:&mut S,gullet:&mut Gu,cmd:StomachCommand<T>,global:bool) -> Result<(),ErrorInPrimitive<T>> {
    debug_log!(trace=>"Assigning \\skip");
    let i = catch_prim!(gullet.get_int(state) => ("skip",cmd));
    let i:usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => return Err(ErrorInPrimitive{name:"skip",msg:Some(format!("Not a valid register: {}",i)),cause:Some(cmd.cause),source:None})
    };
    catch_prim!(gullet.mouth().skip_eq_char(state) => ("skip",cmd));
    let v = catch_prim!(gullet.get_skip(state) => ("skip",cmd));
    debug_log!(debug=>"\\skip{} = {}",i,v);
    state.set_skip_register(i,v,global);
    Ok(())
}
pub fn skip_get<T:Token,S:State<T>,Gu:Gullet<T,S=S>>(state:&mut S,gullet:&mut Gu,cmd:GulletCommand<T>) -> Result<<S::NumSet as NumSet>::Skip,ErrorInPrimitive<T>> {
    debug_log!(trace=>"Getting \\skip");
    let i = catch_prim!(gullet.get_int(state) => ("skip",cmd));
    let i:usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => return Err(ErrorInPrimitive{name:"skip",msg:Some(format!("Not a valid register: {}",i)),cause:Some(cmd.cause),source:None})
    };
    let v = state.get_skip_register(i);
    debug_log!(debug=>"\\skip{} == {}",i,v);
    Ok(v)
}

pub fn skipdef<T:Token,S:State<T>,Gu:Gullet<T,S=S>>(state:&mut S,gullet:&mut Gu,cmd:StomachCommand<T>,global:bool)
                                                     -> Result<(),ErrorInPrimitive<T>> {
    debug_log!(trace=>"skipdef");
    let name = catch_prim!(gullet.get_control_sequence(state) => ("skipdef",cmd));
    match &name {
        BaseToken::Char(c,_) => {
            state.set_ac_command(*c,Some(Ptr::new(Command::Relax)),false)
        }
        BaseToken::CS(name) => {
            state.set_command(name.clone(),Some(Ptr::new(Command::Relax)),false)
        }
    }
    catch_prim!(gullet.mouth().skip_eq_char(state) => ("skipdef",cmd));
    let num = catch_prim!(gullet.get_int(state) => ("skipdef",cmd));
    if num.to_i64() < 0 {
        return Err(ErrorInPrimitive{name:"skipdef",msg:Some(format!("Invalid skip register index: {}",num)),cause:Some(cmd.cause),source:None})
    }
    let num = num.to_i64() as usize;
    match name {
        BaseToken::Char(c,_) => {
            debug_log!(debug=>"\\skipdef: {} = \\skip{}",c.char_str(),num);
            state.set_ac_command(c, Some(Ptr::new(Command::ValueRegister{index:num,tp:Assignable::Skip })), global);
        }
        BaseToken::CS(name) => {
            debug_log!(debug=>"\\skipdef: \\{} = {}",name,num);
            state.set_command(name, Some(Ptr::new(Command::ValueRegister{index:num,tp:Assignable::Skip})), global);
        }
    }
    Ok(())
}

pub fn the<T:Token,S:State<T>,Gu:Gullet<T,S=S>>(state:&mut S,gullet:&mut Gu,cmd:GulletCommand<T>) -> Result<Vec<T>,ErrorInPrimitive<T>> {
    debug_log!(trace => "\\the");
    let next = catch_prim!(gullet.get_next_stomach_command(state) => ("the",cmd));
    match next {
        None => file_end_prim!("the",cmd),
        Some(c) => match c.cmd {
            StomachCommandInner::ValueRegister(i,Assignable::Int) => {
                let val = state.get_int_register(i);
                let str = format!("{}",val);
                debug_log!(debug => "the: {}",str);
                let ret = gullet::methods::string_to_tokens::<T>(&str.as_bytes());
                Ok(ret)
            }
            StomachCommandInner::ValueRegister(_,_) => todo!(),
            StomachCommandInner::AssignableValue {..} => todo!(),
            StomachCommandInner::Value {name,index,tp:Assignable::Int} => {
                match gullet.primitive_int(index) {
                    None =>return Err(ErrorInPrimitive{name:"the",msg:None,cause:Some(cmd.cause),source:Some(
                        ImplementationError(format!("Missing implementation for {}",name),PhantomData).into()
                    )}),
                    Some(fun) => {
                        let val = catch_prim!(fun(state,gullet,cmd.clone()) => ("the",cmd));
                        let str = format!("{}",val);
                        debug_log!(debug => "the: {}",str);
                        let ret = gullet::methods::string_to_tokens::<T>(&str.as_bytes());
                        Ok(ret)
                    }
                }
            }
            StomachCommandInner::ValueAssignment {name,value_index,tp:Assignable::Int,..} => {
                match gullet.primitive_int(value_index) {
                    None =>return Err(ErrorInPrimitive{name:"the",msg:None,cause:Some(cmd.cause),source:Some(
                        ImplementationError(format!("Missing implementation for {}",name),PhantomData).into()
                    )}),
                    Some(fun) => {
                        let val = catch_prim!(fun(state,gullet,cmd.clone()) => ("the",cmd));
                        let str = format!("{}",val);
                        debug_log!(debug => "the: {}",str);
                        let ret = gullet::methods::string_to_tokens::<T>(&str.as_bytes());
                        Ok(ret)
                    }
                }
            }
            StomachCommandInner::Assignment {name:"toks",..} => todo!("toks in \\the"),
            StomachCommandInner::Value {name,index,tp:_} => {
                todo!()
            }
            StomachCommandInner::ValueAssignment {..} => todo!(),
            _ => return Err(ErrorInPrimitive{name:"the",msg:Some("Expected a value after \\the".to_string()),cause:Some(cmd.cause),source:None})
        }
    }
}

pub fn time<T:Token,S:State<T>,Gu:Gullet<T,S=S>>(state:&mut S,_gullet:&mut Gu,cmd:GulletCommand<T>) -> Result<<S::NumSet as NumSet>::Int,ErrorInPrimitive<T>> {
    let t = state.get_start_time();
    Ok(catch_prim!(<S::NumSet as NumSet>::Int::from_i64( ((t.hour() * 60) + t.minute()) as i64 ) => ("time",cmd)))
}


pub fn toks<T:Token,S:State<T>,Gu:Gullet<T,S=S>>(state:&mut S,gullet:&mut Gu,cmd:StomachCommand<T>,global:bool) -> Result<(),ErrorInPrimitive<T>> {
    debug_log!(trace=>"Assigning \\toks");
    let i = catch_prim!(gullet.get_int(state) => ("toks",cmd));
    let i:usize = match i.clone().try_into() {
        Ok(i) => i,
        Err(_) => return Err(ErrorInPrimitive{name:"toks",msg:Some(format!("Not a valid register: {}",i)),cause:Some(cmd.cause),source:None})
    };
    catch_prim!(gullet.mouth().skip_eq_char(state) => ("toks",cmd));
    match catch_prim!(gullet.mouth().get_next(state) => ("toks",cmd)) {
        None => file_end_prim!("toks",cmd),
        Some((t,_)) if t.catcode() == CategoryCode::BeginGroup => (),
        Some((o,_)) => return Err(ErrorInPrimitive{name:"toks",msg:Some(format!("Expected begin group token after \\toks, got: {}",o)),cause:Some(cmd.cause),source:None})
    }
    let v = catch_prim!(gullet.mouth().read_until_endgroup(state) => ("toks",cmd));
    debug_log!(debug=>"\\toks{} = {}",i,TokenList(v.clone()));
    state.set_toks_register(i,v,global);
    Ok(())
}

pub fn toksdef<T:Token,S:State<T>,Gu:Gullet<T,S=S>>(state:&mut S,gullet:&mut Gu,cmd:StomachCommand<T>,global:bool)
                                                      -> Result<(),ErrorInPrimitive<T>> {
    debug_log!(trace=>"toksdef");
    let name = catch_prim!(gullet.get_control_sequence(state) => ("toksdef",cmd));
    match &name {
        BaseToken::Char(c,_) => {
            state.set_ac_command(*c,Some(Ptr::new(Command::Relax)),false)
        }
        BaseToken::CS(name) => {
            state.set_command(name.clone(),Some(Ptr::new(Command::Relax)),false)
        }
    }
    catch_prim!(gullet.mouth().skip_eq_char(state) => ("toksdef",cmd));
    let num = catch_prim!(gullet.get_int(state) => ("toksdef",cmd));
    if num.to_i64() < 0 {
        return Err(ErrorInPrimitive{name:"toksdef",msg:Some(format!("Invalid toks register index: {}",num)),cause:Some(cmd.cause),source:None})
    }
    let num = num.to_i64() as usize;
    match name {
        BaseToken::Char(c,_) => {
            debug_log!(debug=>"\\toksdef: {} = \\toks{}",c.char_str(),num);
            state.set_ac_command(c, Some(Ptr::new(Command::ValueRegister{index:num,tp:Assignable::Toks })), global);
        }
        BaseToken::CS(name) => {
            debug_log!(debug=>"\\toksdef: \\{} = {}",name,num);
            state.set_command(name, Some(Ptr::new(Command::ValueRegister{index:num,tp:Assignable::Toks})), global);
        }
    }
    Ok(())
}


pub fn write<T:Token,Sto:Stomach<T>>(state: &mut Sto::S, gullet:&mut Sto::Gu, stomach:&mut Sto, cmd:StomachCommand<T>) -> Result<Whatsit<T, Sto>, ErrorInPrimitive<T>> {
    debug_log!(trace=>"\\write");
    let i = catch_prim!(gullet.get_int(state) => ("write",cmd));
    let i = i.to_i64();

    match catch_prim!(gullet.mouth().get_next(state) => ("write",cmd)) {
        None => file_end_prim!("write",cmd),
        Some((t,_)) if t.catcode() == CategoryCode::BeginGroup => (),
        Some((o,_)) => return Err(ErrorInPrimitive{name:"write",msg:Some(format!("Expected begin group token after \\write, got: {}",o)),cause:Some(cmd.cause),source:None})
    }
    let mut tks = catch_prim!(gullet.mouth().read_until_endgroup(state) => ("write",cmd));

    let apply = Box::new(move |_stomach:&mut Sto,state:&mut Sto::S,gullet:&mut Sto::Gu| {
        tks.push(T::new(BaseToken::Char(T::Char::from(b'}'),CategoryCode::EndGroup),None));
        tks.insert(0,T::new(BaseToken::Char(T::Char::from(b'{'),CategoryCode::BeginGroup),None));
        let old = gullet.switch_mouth(tks);
        let new = gullet.get_expanded_group(state,false,false,true)?;
        let string = tokens_to_string(new,state.get_escapechar());
        if i == 18 {
            (state.outputs().write_18)(&string)
        }
        else if i == 17 {
            (state.outputs().write_17)(&string)
        }
        else if i < 0 {
            (state.outputs().write_neg1)(&string)
        }
        else {
            match state.get_open_out_file(i as usize) {
                None =>
                    (state.outputs().write_other)(&string),
                Some(f) => f.write(&string)
            }
        }
        gullet.restore_mouth(old);
        Ok(())
    });
    Ok(Whatsit { apply })
}


pub fn xdef<T:Token,S:State<T>,Gu:Gullet<T,S=S>>(state:&mut S,gullet:&mut Gu,cmd:StomachCommand<T>,global:bool,protected:bool,long:bool,outer:bool)
                                                 -> Result<(),ErrorInPrimitive<T>> {
    edef(state,gullet,cmd,true,protected,long,outer)
}

pub fn year<T:Token,S:State<T>,Gu:Gullet<T,S=S>>(state:&mut S,_gullet:&mut Gu,cmd:GulletCommand<T>) -> Result<<S::NumSet as NumSet>::Int,ErrorInPrimitive<T>> {
    Ok(catch_prim!(<S::NumSet as NumSet>::Int::from_i64(
        state.get_start_time().year() as i64
    ) => ("year",cmd)))
}


// --------------------------------------------------------------------------------------------------


pub fn initialize_tex_primitives<T:Token,Sto:Stomach<T>>(state:&mut Sto::S,stomach:&mut Sto,gullet:&mut Sto::Gu) {

    register_assign!(advance,state,stomach,gullet,(s,gu,_,cmd,global) =>advance(s,gu,cmd,global));
    register_stomach!(begingroup,state,stomach,gullet,(s,_,sto,cmd,_) =>begingroup(sto,s,cmd));
    register_value_assign_int!(catcode,state,stomach,gullet);
    register_assign!(chardef,state,stomach,gullet,(s,gu,_,cmd,global) =>chardef(s,gu,cmd,global));
    register_stomach!(closein,state,stomach,gullet,(s,gu,sto,cmd,_) =>closein(s,gu,sto,cmd));
    register_whatsit!(closeout,state,stomach,gullet,(s,gu,sto,cmd) =>closeout(s,gu,sto,cmd));
    register_value_assign_int!(count,state,stomach,gullet);
    register_assign!(countdef,state,stomach,gullet,(s,gu,_,cmd,global) =>countdef(s,gu,cmd,global));
    register_int!(day,state,stomach,gullet,(s,g,c) => day(s,g,c));
    register_assign!(def,state,stomach,gullet,(s,gu,_,cmd,global) =>def(s,gu,cmd,global,false,false,false));
    register_value_assign_dim!(dimen,state,stomach,gullet);
    register_assign!(dimendef,state,stomach,gullet,(s,gu,_,cmd,global) =>dimendef(s,gu,cmd,global));
    register_assign!(divide,state,stomach,gullet,(s,gu,_,cmd,global) =>divide(s,gu,cmd,global));
    register_assign!(edef,state,stomach,gullet,(s,gu,_,cmd,global) =>edef(s,gu,cmd,global,false,false,false));
    register_gullet!(else,state,stomach,gullet,(s,gu,cmd) =>else_(s,gu,cmd));
    register_stomach!(end,state,stomach,gullet,(s,_,sto,cmd,_) =>end(sto,s,cmd));
    register_stomach!(endgroup,state,stomach,gullet,(s,_,sto,cmd,_) =>endgroup(sto,s,cmd));
    register_value_assign_int!(endlinechar,state,stomach,gullet);
    register_tok_assign!(errhelp,state,stomach,gullet);

    let em = stomach.register_primitive("errmessage",|s,gu,_sto,cmd,_global|
        errmessage(s,gu,cmd));
    state.set_command(T::Char::from_str("errmessage"),Some(Ptr::new(Command::Stomach {
        name:"errmessage",
        index:em
    })),true);
    state.set_command(T::Char::from_str("LaTeX3 error:"),Some(Ptr::new(Command::Stomach {
        name:"LaTeX3 error:",
        index:em
    })),true);

    register_value_assign_int!(escapechar,state,stomach,gullet);
    register_gullet!(expandafter,state,stomach,gullet,(s,g,c) => expandafter(s,g,c));
    register_gullet!(fi,state,stomach,gullet,(s,gu,cmd) =>fi(s,gu,cmd));
    register_assign!(gdef,state,stomach,gullet,(s,gu,_,cmd,global) =>gdef(s,gu,cmd,global,false,false,false));
    register_assign!(global,state,stomach,gullet,(s,gu,sto,cmd,g) =>global(sto,s,gu,cmd,g,false,false,false));

    register_conditional!(if,state,stomach,gullet,(s,gu,cmd) =>todo!("if"));
    register_conditional!(ifcase,state,stomach,gullet,(s,gu,cmd) =>todo!("ifcase"));
    register_conditional!(ifcat,state,stomach,gullet,(s,gu,cmd) =>todo!("ifcat"));
    register_conditional!(ifdim,state,stomach,gullet,(s,gu,cmd) =>todo!("ifdim"));
    register_conditional!(ifeof,state,stomach,gullet,(s,gu,cmd) =>ifeof(s,gu,cmd));

    register_conditional!(iffalse,state,stomach,gullet,(s,gu,cmd) => Ok(false));

    register_conditional!(ifhbox,state,stomach,gullet,(s,gu,cmd) =>todo!("ifhbox"));
    register_conditional!(ifhmode,state,stomach,gullet,(s,gu,cmd) =>todo!("ifhmode"));
    register_conditional!(ifinner,state,stomach,gullet,(s,gu,cmd) =>todo!("ifinner"));
    register_conditional!(ifmmode,state,stomach,gullet,(s,gu,cmd) =>todo!("ifmmode"));

    register_conditional!(ifnum,state,stomach,gullet,(s,gu,cmd) =>ifnum(s,gu,cmd));

    register_conditional!(ifodd,state,stomach,gullet,(s,gu,cmd) =>todo!("ifodd"));

    register_conditional!(iftrue,state,stomach,gullet,(s,gu,cmd) => Ok(true));

    register_conditional!(ifvbox,state,stomach,gullet,(s,gu,cmd) =>todo!("ifvbox"));
    register_conditional!(ifvmode,state,stomach,gullet,(s,gu,cmd) =>todo!("ifvmode"));
    register_conditional!(ifvoid,state,stomach,gullet,(s,gu,cmd) =>todo!("ifvoid"));

    register_conditional!(ifx,state,stomach,gullet,(s,gu,cmd) =>ifx(s,gu,cmd));
    register_stomach!(immediate,state,stomach,gullet,(s,gu,sto,cmd,_) =>immediate(s,gu,sto,cmd));
    register_gullet!(input,state,stomach,gullet,(s,gu,cmd) =>input(s,gu,cmd));
    register_assign!(long,state,stomach,gullet,(s,gu,sto,cmd,g) =>long(sto,s,gu,cmd,g,false,false,false));
    register_assign!(let,state,stomach,gullet,(s,gu,_,cmd,global) =>let_(s,gu,cmd,global));
    register_int_assign!(mag,state,stomach,gullet);
    register_assign!(mathchardef,state,stomach,gullet,(s,gu,_,cmd,global) =>mathchardef(s,gu,cmd,global));
    register_gullet!(meaning,state,stomach,gullet,(s,g,c) => meaning(s,g,c));
    register_stomach!(message,state,stomach,gullet,(s,gu,_,cmd,_) =>message(s,gu,cmd));
    register_int!(month,state,stomach,gullet,(s,g,c) => month(s,g,c));
    register_assign!(multiply,state,stomach,gullet,(s,gu,_,cmd,global) =>multiply(s,gu,cmd,global));
    register_value_assign_muskip!(muskip,state,stomach,gullet);
    register_assign!(muskipdef,state,stomach,gullet,(s,gu,_,cmd,global) =>muskipdef(s,gu,cmd,global));
    register_value_assign_int!(newlinechar,state,stomach,gullet);
    register_gullet!(noexpand,state,stomach,gullet,(s,g,c) => noexpand(s,g,c));
    register_gullet!(number,state,stomach,gullet,(s,g,c) => number(s,g,c));
    register_stomach!(openin,state,stomach,gullet,(s,gu,sto,cmd,_) =>openin(s,gu,sto,cmd));
    register_whatsit!(openout,state,stomach,gullet,(s,gu,sto,cmd) =>openout(s,gu,sto,cmd));
    register_assign!(outer,state,stomach,gullet,(s,gu,sto,cmd,g) =>outer(sto,s,gu,cmd,g,false,false,false));

    state.set_command(T::Char::par_token(),Some(Ptr::new(Command::Stomach {
        name:"par",
        index:stomach.register_primitive("par",|s,_gu,sto,cmd,_global| par(sto,s,cmd))
    })),true);

    register_assign!(read,state,stomach,gullet,(s,gu,_,cmd,global) =>read(s,gu,cmd,global));
    state.set_command(T::Char::relax_token(),Some(Ptr::new(Command::Relax)),true);
    register_value_assign_skip!(skip,state,stomach,gullet);
    register_assign!(skipdef,state,stomach,gullet,(s,gu,_,cmd,global) =>skipdef(s,gu,cmd,global));
    register_gullet!(the,state,stomach,gullet,(s,g,c) => the(s,g,c));
    register_int!(time,state,stomach,gullet,(s,g,c) => time(s,g,c));
    register_assign!(toks,state,stomach,gullet,(s,gu,_,cmd,global) =>toks(s,gu,cmd,global));
    register_assign!(toksdef,state,stomach,gullet,(s,gu,_,cmd,global) =>toksdef(s,gu,cmd,global));
    register_whatsit!(write,state,stomach,gullet,(s,gu,sto,cmd) =>write(s,gu,sto,cmd));
    register_assign!(xdef,state,stomach,gullet,(s,gu,_,cmd,global) =>xdef(s,gu,cmd,global,false,false,false));
    register_int!(year,state,stomach,gullet,(s,g,c) => year(s,g,c));
}