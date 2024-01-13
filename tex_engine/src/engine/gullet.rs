pub mod methods;

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

/// A [`Gullet`] is the part of the engine that reads tokens from the input stream and expands them;
/// including conditionals etc.
/// Additionally, it takes care of reading keywords, strings (e.g. filenames in `\input`),
/// integers, dimensions, skips...
/// Basically, all processing of [`Token`]s that does not result in scoped [`State`] changes or
/// [nodes](crate::tex::nodes::NodeTrait) in the [`Stomach`](crate::engine::stomach::Stomach).
///
/// As part of that, it has to do some bookkeeping already when reading [`Token`]s from the
/// [`Mouth`] and therefore implements wrapper methods around its methods as well.
pub trait Gullet<ET:EngineTypes> {
    /// Instantiate a new Gullet
    fn new(aux:&mut EngineAux<ET>,state:&mut ET::State,mouth:&mut ET::Mouth) -> Self;

    fn push_align(&mut self,ad:AlignData<ET::Token,ET::Skip>);
    fn pop_align(&mut self) -> Option<AlignData<ET::Token,ET::Skip>>;
    fn get_align_data(&mut self) -> Option<&mut AlignData<ET::Token,ET::Skip>>;
    fn get_conditional(&self) -> Option<ActiveConditional<ET>>;
    fn get_conditionals(&mut self) -> &mut Vec<ActiveConditional<ET>>;
    fn csnames(&mut self) -> &mut usize;

    fn iterate<Fn:FnMut(&mut EngineAux<ET>,&ET::State,&mut Self,ET::Token) -> bool>(&mut self,mouth:&mut ET::Mouth,aux:&mut EngineAux<ET>,state:&ET::State,mut f:Fn) {
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

    fn get_next_opt(&mut self, mouth:&mut ET::Mouth, aux:&mut EngineAux<ET>, state:&ET::State) -> Option<ET::Token> {
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

    fn read_until_endgroup<Fn:FnMut(&mut EngineAux<ET>,&ET::State,ET::Token)>(&mut self, mouth:&mut ET::Mouth, aux:&mut EngineAux<ET>, state:&ET::State,mut cont:Fn) -> ET::Token {
        match self.get_align_data() {
            None => (),
            Some(d) => {
                if d.ingroups == 0 { todo!() }
                d.ingroups -= 1
            }
        }
        mouth.read_until_endgroup(aux,state,|a,t|cont(a,state,t))
    }

    fn requeue(&mut self,mouth:&mut ET::Mouth,t:ET::Token) {
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

    fn read_int(engine:&mut EngineReferences<ET>,skip_eq:bool) -> ET::Int {
        methods::read_int(engine,skip_eq)
    }

    fn read_dim(engine:&mut EngineReferences<ET>,skip_eq:bool) -> ET::Dim {
        methods::read_dim(engine,skip_eq)
    }

    fn read_skip(engine:&mut EngineReferences<ET>,skip_eq:bool) -> ET::Skip {
        methods::read_skip(engine,skip_eq)
    }

    fn read_muskip(engine:&mut EngineReferences<ET>,skip_eq:bool) -> ET::MuSkip {
        methods::read_muskip(engine,skip_eq)
    }

    fn read_mudim(engine:&mut EngineReferences<ET>,skip_eq:bool) -> <ET::MuSkip as MuSkip>::Base {
        methods::read_mukern(engine,skip_eq)
    }

    fn read_chars(engine:& mut EngineReferences<ET>,kws:&[u8]) -> Result<u8,ET::Token> {
        methods::read_chars(engine,kws)
    }

    fn resolve<'a>(state:&'a ET::State,token:ET::Token) -> ResolvedToken<'a,ET> {
        match token.to_enum() {
            StandardToken::Character(c,CommandCode::Active) =>
                ResolvedToken::Cmd{token,cmd:state.get_ac_command(c)},
            StandardToken::Character(c,o) => return ResolvedToken::Tk{token,char:c,code:o},
            StandardToken::ControlSequence(cs) =>
                ResolvedToken::Cmd{token,cmd:state.get_command(&cs)}
        }
    }

    fn do_expandable(engine: &mut EngineReferences<ET>,name:PrimitiveIdentifier,token:ET::Token,f:fn(&mut EngineReferences<ET>,&mut Vec<ET::Token>,ET::Token)) {
        engine.trace_command(|engine| format!("{}", PRIMITIVES.printable(name,engine.state.get_escape_char())));
        let mut exp = Vec::new();// ExpansionContainer::new(engine.aux.memory.get_token_vec());
        f(engine,&mut exp,token);
        engine.mouth.push_vec(exp);
    }
    fn do_macro(engine: &mut EngineReferences<ET>,m:Macro<ET::Token>,token:ET::Token);

    fn do_simple_expandable(engine: &mut EngineReferences<ET>,name:PrimitiveIdentifier,token:ET::Token,f:fn(&mut EngineReferences<ET>,ET::Token)) {
        engine.trace_command(|engine| format!("{}", PRIMITIVES.printable(name,engine.state.get_escape_char())));
        f(engine,token)
    }

    fn read_string(engine:&mut EngineReferences<ET>,skip_eq:bool,target:&mut String) {
        methods::read_string(engine,skip_eq,target)
    }

    fn read_keyword(engine:&mut EngineReferences<ET>,kw:&[u8]) -> bool {
        methods::read_keyword(engine,kw,None)
    }

    fn read_keywords<'a>(engine:&mut EngineReferences<ET>,kw:&'a[&'a[u8]]) -> Option<&'a[u8]>     {
        methods::read_keywords(engine,kw,None)
    }

    fn do_conditional(engine:&mut EngineReferences<ET>,name:PrimitiveIdentifier,token:ET::Token,f:fn(&mut EngineReferences<ET>,ET::Token) -> bool,unless:bool) {
        let trace = engine.state.get_primitive_int(PRIMITIVES.tracingifs) > ET::Int::default();
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
                                     ET::Char::displayable_opt(engine.state.get_escape_char()),
                                     PRIMITIVES.printable(name,engine.state.get_escape_char()),
                                     index+1,engine.mouth.line_number()
                        ));
                } else {
                    engine.aux.outputs.write_neg1(
                        format_args!("{{{}fi: {} (level {}) entered on line {}}}",
                                     ET::Char::displayable_opt(engine.state.get_escape_char()),
                                     PRIMITIVES.printable(name,engine.state.get_escape_char()),
                                     index+1,engine.mouth.line_number()
                        ));
                }
            }
        }
    }
    fn expand_until_endgroup<Fn:FnMut(&mut EngineAux<ET>,&ET::State,ET::Token)>(engine:&mut EngineReferences<ET>,expand_protected:bool,edef_like:bool,cont:Fn);
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

    pub fn iterate<Fn:FnMut(&mut EngineAux<ET>,&ET::State,&mut ET::Gullet,ET::Token) -> bool>(&mut self,mut f:Fn) {
        self.gullet.iterate(self.mouth,self.aux,self.state,|a,s,g,t|f(a,s,g,t))
    }

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
impl<ET:EngineTypes> Gullet<ET> for DefaultGullet<ET> {

    fn new(_aux: &mut EngineAux<ET>, _state: &mut ET::State, _mouth: &mut ET::Mouth) -> Self {
        DefaultGullet {
            align_data:Vec::new(),
            phantom:PhantomData::default(),
            conditionals:Vec::new(),
            csnames:0
        }
    }


    fn csnames(&mut self) -> &mut usize { &mut self.csnames }


    fn push_align(&mut self, ad: AlignData<ET::Token,ET::Skip>) {
        self.align_data.push(ad)
    }


    fn pop_align(&mut self) -> Option<AlignData<ET::Token, ET::Skip>> {
        self.align_data.pop()
    }

    fn expand_until_endgroup<Fn: FnMut(&mut EngineAux<ET>, &ET::State, ET::Token)>(engine: &mut EngineReferences<ET>, expand_protected: bool, edef_like: bool, cont: Fn) {
        methods::expand_until_endgroup(engine,expand_protected,edef_like,cont);
    }

    fn do_macro(engine: &mut EngineReferences<ET>, m: Macro<ET::Token>, token: ET::Token) {
        let trace = engine.state.get_primitive_int(PRIMITIVES.tracingcommands) > ET::Int::default();
        if trace {
            match token.to_enum() {
                StandardToken::ControlSequence(cs) => {
                    engine.aux.outputs.write_neg1(format_args!("~.{}{} {}",
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
                    engine.aux.outputs.write_neg1(format_args!("~.{} {}",
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

    fn get_align_data(&mut self) -> Option<&mut AlignData<ET::Token,ET::Skip>> {
        self.align_data.last_mut()
    }

    fn get_conditional(&self) -> Option<ActiveConditional<ET>> {
        self.conditionals.last().cloned()
    }

    fn get_conditionals(&mut self) -> &mut Vec<ActiveConditional<ET>> {
        &mut self.conditionals
    }
}