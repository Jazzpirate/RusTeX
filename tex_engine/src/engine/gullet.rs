
pub mod methods;

use std::fmt::{Display, Formatter};
use std::marker::PhantomData;
use crate::commands::{Command, Macro, Unexpandable};
use crate::engine::{EngineAux, EngineReferences, EngineTypes};
use crate::engine::mouth::Mouth;
use crate::tex::tokens::token_lists::{MacroExpansion, TokenList, TokenListDisplay};
use crate::engine::state::State;
use crate::engine::utils::memory::{PrimitiveIdentifier, PRIMITIVES};
use crate::engine::utils::outputs::Outputs;
use crate::tex::catcodes::CommandCode;
use crate::tex::characters::Character;
use crate::tex::numerics::{MuSkip, NumSet};
use crate::tex::tokens::{StandardToken, Token};
use crate::tex::tokens::control_sequences::CSHandler;
use crate::tex::types::BoxType;

type M<G> = <<G as Gullet>::ET as EngineTypes>::Mouth;
type A<G> = EngineAux<<G as Gullet>::ET>;
type S<G> = <<G as Gullet>::ET as EngineTypes>::State;
type T<G> = <<G as Gullet>::ET as EngineTypes>::Token;
type C<G> = <<G as Gullet>::ET as EngineTypes>::Char;
type Int<G> = <<<G as Gullet>::ET as EngineTypes>::Num as NumSet>::Int;
type Dim<G> = <<<G as Gullet>::ET as EngineTypes>::Num as NumSet>::Dim;
type Skip<G> = <<<G as Gullet>::ET as EngineTypes>::Num as NumSet>::Skip;
type MSkip<G> = <<<G as Gullet>::ET as EngineTypes>::Num as NumSet>::MuSkip;

pub trait Gullet {
    type ET:EngineTypes<Gullet=Self>;

    fn new(aux:&mut EngineAux<Self::ET>,state:&mut S<Self>,mouth:&mut M<Self>) -> Self;

    fn push_align(&mut self,ad:AlignData<T<Self>,Skip<Self>>);
    fn pop_align(&mut self) -> Option<AlignData<T<Self>,Skip<Self>>>;
    fn get_align_data(&mut self) -> Option<&mut AlignData<T<Self>,Skip<Self>>>;
    fn get_conditional(&self) -> Option<ActiveConditional<Self::ET>>;
    fn get_conditionals(&mut self) -> &mut Vec<ActiveConditional<Self::ET>>;
    fn csnames(&mut self) -> &mut usize;

    fn iterate<Fn:FnMut(&mut A<Self>,&S<Self>,&mut Self,T<Self>) -> bool>(&mut self,mouth:&mut M<Self>,aux:&mut A<Self>,state:&S<Self>,mut f:Fn) {
        match self.get_align_data() {
            None => mouth.iterate(aux,state,|a,t|f(a,state,self,t)),
            Some(_) => {
                mouth.iterate(aux,state,|aux,t| {
                    let data = self.get_align_data().unwrap();
                    if t.is_begin_group() {
                        data.ingroups += 1;
                        f(aux, state, self, t)
                    } else if t.is_end_group() {
                        if data.ingroups == 0 { todo!() }
                        data.ingroups -= 1;
                        f(aux, state, self, t)
                    } else if data.ingroups == data.groupval() && t.is_align_tab() {
                        todo!()
                    } else if data.ingroups == data.groupval() && t.is_cs_or_active() {
                        match Self::resolve(state, t) {
                            ResolvedToken::Cmd { cmd: Some(Command::Unexpandable(Unexpandable { name, .. })), .. }
                            if *name == PRIMITIVES.cr || *name == PRIMITIVES.crcr => {
                                todo!()
                            }
                            ResolvedToken::Cmd { cmd: Some(Command::Unexpandable(Unexpandable { name, .. })), .. }
                            if *name == PRIMITIVES.span => {
                                todo!()
                            }
                            ResolvedToken::Tk { token, .. } | ResolvedToken::Cmd { token, .. } =>
                                f(aux, state, self, token)
                        }
                    } else {
                        f(aux, state, self, t)
                    }
                });
            }
        }
    }

    fn get_next_opt(&mut self,
        mouth:&mut M<Self>,
        aux:&mut A<Self>,
        state:&<Self::ET as EngineTypes>::State) -> Option<T<Self>> {
        match self.get_align_data() {
            None => mouth.get_next_opt(aux,state),
            Some(a) => match mouth.get_next_opt(aux,state) {
                Some(t) if t.is_begin_group()  => {
                    a.ingroups += 1; Some(t)
                },
                Some(t) if a.ingroups == a.groupval() && t.is_align_tab() => {
                    methods::do_align(mouth,aux,a);
                    self.get_next_opt(mouth,aux,state)
                }
                Some(t) if t.is_end_group() => {
                    if a.ingroups == 0 { todo!("throw error") }
                    a.ingroups -= 1;
                    Some(t)
                },
                Some(t) if a.ingroups == a.groupval() && t.is_cs_or_active() => {
                    match Self::resolve(state,t) {
                        ResolvedToken::Cmd {cmd:Some(Command::Unexpandable(Unexpandable {name,..})),..}
                            if *name == PRIMITIVES.cr || *name == PRIMITIVES.crcr => {
                            methods::do_cr(mouth,aux,state,a);
                            return self.get_next_opt(mouth,aux,state)
                        }
                        ResolvedToken::Cmd { cmd: Some(Command::Unexpandable(Unexpandable { name, .. })), .. }
                            if *name == PRIMITIVES.span => {
                            a.span = true;
                            methods::do_align(mouth,aux,a);
                            self.get_next_opt(mouth,aux,state)
                        }
                        ResolvedToken::Tk{token,..} | ResolvedToken::Cmd {token,..} =>
                            Some(token)
                    }
                }
                o => o
            }
        }
    }

    fn read_until_endgroup<Fn:FnMut(&mut EngineAux<Self::ET>,&<Self::ET as EngineTypes>::State,T<Self>)>(&mut self,
                                                                           mouth:&mut M<Self>,
                                                                           aux:&mut A<Self>,
                                                                           state:&<Self::ET as EngineTypes>::State,mut cont:Fn) -> T<Self> {
        match self.get_align_data() {
            None => (),
            Some(d) => {
                if d.ingroups == 0 { todo!() }
                d.ingroups -= 1
            }
        }
        mouth.read_until_endgroup(aux,state,|a,t|cont(a,state,t))
    }
    fn requeue(&mut self,mouth:&mut M<Self>,t:T<Self>) {
        if let Some(data) = self.get_align_data() {
            if t.is_begin_group() {
                if data.ingroups == 0 { todo!() }
                data.ingroups -= 1;
            }
            else if t.is_end_group() {
                data.ingroups += 1;
            }
        }
        mouth.requeue(t)
    }


    fn read_int(engine:&mut EngineReferences<Self::ET>,skip_eq:bool) -> Int<Self> {
        methods::read_int(engine,skip_eq)
    }

    fn read_dim(engine:&mut EngineReferences<Self::ET>,skip_eq:bool) -> Dim<Self> {
        methods::read_dim(engine,skip_eq)
    }

    fn read_skip(engine:&mut EngineReferences<Self::ET>,skip_eq:bool) -> Skip<Self> {
        methods::read_skip(engine,skip_eq)
    }

    fn read_muskip(engine:&mut EngineReferences<Self::ET>,skip_eq:bool) -> MSkip<Self> {
        methods::read_muskip(engine,skip_eq)
    }

    fn read_mudim(engine:&mut EngineReferences<Self::ET>,skip_eq:bool) -> <MSkip<Self> as MuSkip>::Base {
        methods::read_mukern(engine,skip_eq)
    }


    fn read_chars(engine:& mut EngineReferences<Self::ET>,kws:&[u8]) -> Result<u8,T<Self>> {
        methods::read_chars(engine,kws)
    }
    fn resolve<'a>(state:&'a S<Self>,token:T<Self>) -> ResolvedToken<'a,Self::ET> {
        match token.to_enum() {
            StandardToken::Character(c,CommandCode::Active) =>
                ResolvedToken::Cmd{token,cmd:state.get_ac_command(c)},
            StandardToken::Character(c,o) => return ResolvedToken::Tk{token,char:c,code:o},
            StandardToken::ControlSequence(cs) =>
                ResolvedToken::Cmd{token,cmd:state.get_command(&cs)}
        }
    }

    fn do_expandable(engine: &mut EngineReferences<Self::ET>,name:PrimitiveIdentifier,token:T<Self>,f:fn(&mut EngineReferences<Self::ET>,&mut Vec<T<Self>>,T<Self>)) {
        engine.trace_command(|engine| format!("{}", PRIMITIVES.printable(name,engine.state.get_escape_char())));
        let mut exp = Vec::new();// ExpansionContainer::new(engine.aux.memory.get_token_vec());
        f(engine,&mut exp,token);
        engine.mouth.push_vec(exp);
    }
    fn do_macro(engine: &mut EngineReferences<Self::ET>,m:Macro<T<Self>>,token:T<Self>);

    fn do_simple_expandable(engine: &mut EngineReferences<Self::ET>,name:PrimitiveIdentifier,token:T<Self>,f:fn(&mut EngineReferences<Self::ET>,T<Self>)) {
        engine.trace_command(|engine| format!("{}", PRIMITIVES.printable(name,engine.state.get_escape_char())));
        f(engine,token)
    }

    fn read_string(engine:&mut EngineReferences<Self::ET>,skip_eq:bool,target:&mut String) {
        methods::read_string(engine,skip_eq,target)
    }

    fn read_keyword(engine:&mut EngineReferences<Self::ET>,kw:&[u8]) -> bool {
        methods::read_keyword(engine,kw,None)
    }

    fn read_keywords<'a>(engine:&mut EngineReferences<Self::ET>,kw:&'a[&'a[u8]]) -> Option<&'a[u8]>     {
        methods::read_keywords(engine,kw,None)
    }

    fn do_conditional(engine:&mut EngineReferences<Self::ET>,name:PrimitiveIdentifier,token:T<Self>,f:fn(&mut EngineReferences<Self::ET>,T<Self>) -> bool,unless:bool) {
        let trace = engine.state.get_primitive_int(PRIMITIVES.tracingifs) > Int::<Self>::default();
        let index = engine.gullet.get_conditionals().len();
        engine.gullet.get_conditionals().push(ActiveConditional::Unfinished(name));
        if trace {
            //crate::debug_log!(error => "Here: {}",engine.preview());
            engine.aux.outputs.write_neg1(format_args!("{{{}: (level {}) entered on line {}}}",PRIMITIVES.printable(name,engine.state.get_escape_char()),index+1,engine.mouth.line_number()));
        }
        let mut ret = f(engine,token);
        if unless { ret = !ret }
        if ret {
            if trace {
                engine.aux.outputs.write_neg1("{true}");
            }
            if name != PRIMITIVES.ifcase {
                match engine.gullet.get_conditionals().get_mut(index) {
                    Some(u@ActiveConditional::Unfinished(_)) =>
                        *u = ActiveConditional::True(name),
                    _ => todo!("throw proper error")
                }
            }
        } else {
            if trace {
                engine.aux.outputs.write_neg1("{false}");
            }
            if name == PRIMITIVES.ifcase {
                methods::case_loop(engine, index + 1);
            } else {
                methods::false_loop(engine, index + 1, true,false);
            }
            if trace {
                if engine.gullet.get_conditionals().len() > index {
                    engine.aux.outputs.write_neg1(
                        format_args!("{{{}else: {} (level {}) entered on line {}}}",
                                     <C<Self> as Character>::displayable_opt(engine.state.get_escape_char()),
                                     PRIMITIVES.printable(name,engine.state.get_escape_char()),
                                     index+1,engine.mouth.line_number()
                        ));
                } else {
                    engine.aux.outputs.write_neg1(
                        format_args!("{{{}fi: {} (level {}) entered on line {}}}",
                                     <C<Self> as Character>::displayable_opt(engine.state.get_escape_char()),
                                     PRIMITIVES.printable(name,engine.state.get_escape_char()),
                                     index+1,engine.mouth.line_number()
                        ));
                }
            }
        }
    }
    fn expand_until_endgroup<Fn:FnMut(&mut EngineAux<Self::ET>,&S<Self>,T<Self>)>(engine:&mut EngineReferences<Self::ET>,expand_protected:bool,edef_like:bool,cont:Fn);
}

#[derive(Copy,Clone,Eq,PartialEq,Debug)]
pub enum ActiveConditional<ET:EngineTypes> {
    Unfinished(PrimitiveIdentifier),
    Case(<ET::Num as NumSet>::Int),
    True(PrimitiveIdentifier),
    Else(PrimitiveIdentifier),
}
impl<ET:EngineTypes> ActiveConditional<ET> {
    pub fn name(&self) -> PrimitiveIdentifier {
        match self {
            ActiveConditional::Unfinished(n) => *n,
            ActiveConditional::Case(_) => PRIMITIVES.ifcase,
            ActiveConditional::True(n) => *n,
            ActiveConditional::Else(n) => *n,
        }
    }
}

#[derive(Debug)]
pub enum ResolvedToken<'a,ET:EngineTypes> {
    Tk{token:ET::Token,char:ET::Char,code:CommandCode},
    Cmd{token:ET::Token,cmd:Option<&'a Command<ET>>},
}


#[derive(Clone,Debug)]
pub struct AlignColumn<T:Token,Sk: crate::tex::numerics::Skip> {
    pub left: TokenList<T>,
    pub right: TokenList<T>,
    pub inbraces:u8,
    pub tabskip: Sk,
}
impl <T:Token,Sk: crate::tex::numerics::Skip> AlignColumn<T,Sk> {

    pub fn new(left:shared_vector::Vector<T>,right:shared_vector::Vector<T>,tabskip:Sk,inbraces:u8) -> Self {
        Self { left:left.into(),inbraces,right:right.into(),tabskip }
    }
}

pub struct AlignData<T:Token,Sk: crate::tex::numerics::Skip> {
    pub ingroups:u8,
    pub currindex:usize,
    pub recindex:Option<usize>,
    pub columns:shared_vector::SharedVector<AlignColumn<T,Sk>>,
    pub omit:bool,
    pub span:bool,
    pub inner_mode:BoxType,
    pub outer_mode:BoxType
}
impl<T:Token,Sk: crate::tex::numerics::Skip> AlignData<T,Sk> {
    pub fn groupval(&self) -> u8 {
        if self.omit { 0 } else { self.columns[self.currindex].inbraces }
    }
    pub fn dummy() -> Self {
        Self {
            ingroups:125,
            currindex:0,
            recindex:None,
            columns:shared_vector::vector!(AlignColumn::new(shared_vector::Vector::new(),shared_vector::Vector::new(),Sk::default(),0)).into(),
            omit:false,
            span:false,
            inner_mode:BoxType::Horizontal,
            outer_mode:BoxType::Vertical
        }
    }
}


impl<ET:EngineTypes> EngineReferences<'_,ET> {


    pub fn requeue(&mut self,t:ET::Token) {
        self.gullet.requeue(self.mouth,t)
    }


    pub fn read_until_endgroup<Fn:FnMut(&mut EngineAux<ET>,&ET::State,ET::Token)>(&mut self, cont:Fn) -> ET::Token {
        self.gullet.read_until_endgroup(self.mouth,self.aux,self.state,cont)
    }


    pub fn expand_until_endgroup<Fn:FnMut(&mut EngineAux<ET>,&ET::State,ET::Token)>(&mut self,expand_protected:bool,edef_like:bool,mut cont:Fn) {
        ET::Gullet::expand_until_endgroup(self,expand_protected,edef_like,|a,s,t| cont(a,s,t))
    }


    pub fn get_next(&mut self) -> Option<ET::Token> {
        self.gullet.get_next_opt(self.mouth,self.aux,self.state)
    }

    pub fn read_int(&mut self,skip_eq:bool) -> <ET::Num as NumSet>::Int {
        ET::Gullet::read_int(self,skip_eq)
    }

    pub fn read_string(&mut self,skip_eq:bool,target:&mut String) {
        ET::Gullet::read_string(self,skip_eq,target)
    }

    pub fn read_keyword(&mut self,kw:&[u8]) -> bool {
        ET::Gullet::read_keyword(self,kw)
    }


    pub fn read_keywords<'a>(&mut self,kw:&'a[&'a[u8]]) -> Option<&'a[u8]> {
        ET::Gullet::read_keywords(self,kw)
    }

    pub fn read_charcode(&mut self,skip_eq:bool) -> ET::Char {
        let i:i64 = ET::Gullet::read_int(self,skip_eq).into();
        if i < 0 {
            todo!("negative charcode")
        }
        match ET::Char::try_from(i as u64) {
            Ok(c) => c,
            _ => todo!("charcode too large: {}",i)
        }
    }

    pub fn read_braced_string(&mut self,skip_ws:bool, expand_protected:bool, mut str:&mut String) {
        loop {
            match self.get_next() {
                Some(t) if t.is_begin_group() => break,
                Some(t) if t.is_space() && skip_ws => (),
                Some(o) => todo!("should be begingroup: {:?}",o.to_enum()),
                None => todo!("file end")
            }
        }
        ET::Gullet::expand_until_endgroup(self,expand_protected,false,|a,s,t| {
            t.display_fmt(a.memory.cs_interner(),s.get_catcode_scheme(),
                          s.get_escape_char(),&mut str).unwrap();
        });
    }


    pub fn read_dim(&mut self,skip_eq:bool) -> <ET::Num as NumSet>::Dim {
        ET::Gullet::read_dim(self,skip_eq)
    }

    pub fn read_skip(&mut self,skip_eq:bool) -> <ET::Num as NumSet>::Skip {
        ET::Gullet::read_skip(self,skip_eq)
    }

    pub fn read_muskip(&mut self,skip_eq:bool) -> <ET::Num as NumSet>::MuSkip {
        ET::Gullet::read_muskip(self,skip_eq)
    }

    pub fn read_mudim(&mut self,skip_eq:bool) -> <<ET::Num as NumSet>::MuSkip as MuSkip>::Base {
        ET::Gullet::read_mudim(self,skip_eq)
    }

    pub fn read_chars(&mut self,kws:&[u8]) -> Result<u8,ET::Token> {
        ET::Gullet::read_chars(self,kws)
    }

    pub fn resolve(&self,token:ET::Token) -> ResolvedToken<ET> {
        ET::Gullet::resolve(self.state,token)
    }
}


pub struct DefaultGullet<ET:EngineTypes> {
    align_data:Vec<AlignData<ET::Token,ET::Skip>>,
    conditionals:Vec<ActiveConditional<ET>>,
    csnames:usize,
    phantom:PhantomData<ET>
}
struct Dots(usize);
impl Display for Dots {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for _ in 0..self.0 {
            write!(f,".")?;
        }
        Ok(())
    }
}
impl<ET:EngineTypes<Gullet=Self>> Gullet for DefaultGullet<ET> {
    type ET = ET;

    fn new(_aux: &mut EngineAux<Self::ET>, _state: &mut S<Self>, _mouth: &mut M<Self>) -> Self {
        DefaultGullet {
            align_data:Vec::new(),
            phantom:PhantomData::default(),
            conditionals:Vec::new(),
            csnames:0
        }
    }


    fn csnames(&mut self) -> &mut usize { &mut self.csnames }


    fn push_align(&mut self, ad: AlignData<T<Self>,Skip<Self>>) {
        self.align_data.push(ad)
    }


    fn pop_align(&mut self) -> Option<AlignData<T<Self>, Skip<Self>>> {
        self.align_data.pop()
    }

    fn expand_until_endgroup<Fn: FnMut(&mut EngineAux<Self::ET>, &S<Self>, T<Self>)>(engine: &mut EngineReferences<Self::ET>, expand_protected: bool, edef_like: bool, cont: Fn) {
        methods::expand_until_endgroup(engine,expand_protected,edef_like,cont);
    }

    fn do_macro(engine: &mut EngineReferences<Self::ET>, m: Macro<T<Self>>, token: T<Self>) {
        let trace = engine.state.get_primitive_int(PRIMITIVES.tracingcommands) > Int::<Self>::default();
        if trace {
            match token.to_enum() {
                StandardToken::ControlSequence(cs) => {
                    engine.aux.outputs.write_neg1(format_args!("~.{}{}{} {}",
                                                               Dots(engine.mouth.num_exps()),
                                                               ET::Char::displayable_opt(engine.state.get_escape_char()),
                                                               engine.aux.memory.cs_interner().resolve(&cs),
                                                               m.meaning::<ET>(engine.aux.memory.cs_interner(), engine.state.get_catcode_scheme(), engine.state.get_escape_char())
                    ));
                    engine.aux.outputs.write_neg1(format_args!("Here: {}",engine.preview()));
                    if engine.aux.memory.cs_interner().resolve(&cs).to_string() == "lst@ReplaceIn@" {
                        println!("HERE!!!!")
                    }
                }
                StandardToken::Character(c,_) => {
                    engine.aux.outputs.write_neg1(format_args!("~.{}{} {}",
                                                               Dots(engine.mouth.num_exps()),
                                                               c.display(),
                                                               m.meaning::<ET>(engine.aux.memory.cs_interner(), engine.state.get_catcode_scheme(), engine.state.get_escape_char())
                    ));
                    //engine.aux.outputs.write_neg1(format_args!("Here: {}",engine.preview()));
                }
            }
            //crate::debug_log!(error => "Here: {}",engine.preview());
        }
        if m.signature.params.is_empty() {
            engine.mouth.push_exp(&m.expansion);
            return;
        }
        let mut args = engine.mouth.get_args();
        methods::read_arguments(engine,&mut args,m.signature.params,m.long);
        if trace {
            for i in 0..m.signature.arity {
                engine.aux.outputs.write_neg1(
                    format_args!("#{}<-{}",
                                 i+1,
                        TokenListDisplay::from_vec(&args[i as usize], engine.aux.memory.cs_interner(), engine.state.get_catcode_scheme(), engine.state.get_escape_char(),false)
                ));

            }
        }
        if m.signature.arity == 0 {
            engine.mouth.return_args(args);
            engine.mouth.push_exp(&m.expansion);
        } else {
            engine.mouth.push_macro_exp(MacroExpansion::new(m.expansion,args))
        }
    }

    fn get_align_data(&mut self) -> Option<&mut AlignData<T<Self>,Skip<Self>>> {
        self.align_data.last_mut()
    }

    fn get_conditional(&self) -> Option<ActiveConditional<ET>> {
        self.conditionals.last().cloned()
    }

    fn get_conditionals(&mut self) -> &mut Vec<ActiveConditional<ET>> {
        &mut self.conditionals
    }
}