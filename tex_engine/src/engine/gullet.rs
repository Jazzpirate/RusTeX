/*! A [`Gullet`] is the part of the engine that reads tokens from the input stream and expands them;
    including conditionals etc.
 */
pub mod methods;
pub mod hvalign;

use std::marker::PhantomData;
use crate::commands::{ActiveConditional, CharOrPrimitive, TeXCommand, Macro, ResolvedToken};
use crate::commands::primitives::{PrimitiveIdentifier, PRIMITIVES};
use crate::engine::{EngineAux, EngineReferences, EngineTypes};
use crate::engine::gullet::hvalign::AlignData;
use crate::engine::mouth::Mouth;
use crate::tex::tokens::token_lists::MacroExpansion;
use crate::engine::state::State;
use crate::engine::utils::outputs::Outputs;
use crate::tex::catcodes::CommandCode;
use crate::tex::characters::Character;
use crate::tex::numerics::{MuSkip, NumSet, Skip};
use crate::tex::tokens::{StandardToken, Token};
use crate::tex::tokens::control_sequences::CSHandler;
use crate::tex_error;

/// A [`Gullet`] is the part of the engine that reads tokens from the input stream and expands them;
/// including conditionals etc.
/// Additionally, it takes care of reading keywords, strings (e.g. filenames in `\input`),
/// integers, dimensions, skips...
/// Basically, all processing of [`Token`]s that does not result in scoped [`State`] changes or
/// [nodes](crate::tex::nodes::NodeTrait) in the [`Stomach`](crate::engine::stomach::Stomach).
/// The canonical implementation of a [`Gullet`] is [`DefaultGullet`].
///
/// As part of that, it has to do some bookkeeping already when reading [`Token`]s from the
/// [`Mouth`] and therefore implements wrapper methods around its methods as well.
///
/// Note that we do not require `ET:`[`EngineTypes`]`<`[`Gullet`](EngineTypes::Gullet)`=Self>` - this allows for
/// implementing your own Gullet by just wrapping an existing implementation in a new wrapper struct and pass on functionality
/// to the inner Gullet, which would otherwise
/// fail since `ET::Gullet` would be the outer wrapper struct, not the inner one.
pub trait Gullet<ET:EngineTypes> {
    /// Instantiate a new Gullet
    fn new(aux:&mut EngineAux<ET>,state:&mut ET::State,mouth:&mut ET::Mouth) -> Self;

    /// Push a new [`AlignData`] onto the stack, i.e. on `\halign` or `\valign`
    fn push_align(&mut self,ad:AlignData<ET::Token,ET::Dim>);
    /// Pop the last [`AlignData`] from the stack, i.e. at the end of `\halign` or `\valign`
    fn pop_align(&mut self) -> Option<AlignData<ET::Token,ET::Dim>>;
    /// Inspect the current [`AlignData`], if any (i.e. if we are in an `\halign` or `\valign`)
    fn get_align_data(&mut self) -> Option<&mut AlignData<ET::Token,ET::Dim>>;
    /// Inspect the current [`ActiveConditional`], if any (i.e. if we are in an `\if` or similar)
    fn get_conditional(&self) -> Option<ActiveConditional<ET::Int>>;
    /// Get a mutable reference to the stack of [`ActiveConditional`]s
    fn get_conditionals(&mut self) -> &mut Vec<ActiveConditional<ET::Int>>;
    /// Get a mutable reference to the counter for the number of `\csname`s we are currently in
    fn csnames(&mut self) -> &mut usize;

    /// Wrapper around [`Mouth::iterate`] that, in case we are in an `\halign` or `\valign`,
    /// make sure to replace `&`, `\cr` etc. with the appropriate tokens.
    /// See also [`EngineReferences::iterate`].
    fn iterate<Fn:FnMut(&mut EngineAux<ET>,&ET::State,ET::Token) -> bool>(&mut self,mouth:&mut ET::Mouth,aux:&mut EngineAux<ET>,state:&ET::State,token:&ET::Token,mut f:Fn) {
        match self.get_align_data() {
            None => mouth.iterate(aux,state,token,|a,t|f(a,state,t)),
            Some(data) => {
                mouth.iterate(aux,state,token,|aux,t| {
                    match t.command_code() {
                        CommandCode::BeginGroup => {
                            data.ingroups += 1;
                            f(aux,state,t)
                        }
                        CommandCode::EndGroup => {
                            if data.ingroups == 0 { aux.error_handler.too_many_closebraces() }
                            data.ingroups -= 1;
                            f(aux,state,t)
                        }
                        /*
                        CommandCode::AlignmentTab if data.ingroups == data.groupval() => {
                            todo!("fuck")
                        }
                        CommandCode::Escape | CommandCode::Active | CommandCode::Primitive if data.ingroups == data.groupval() => match Self::char_or_primitive(state,&t) {
                            Some(CharOrPrimitive::Primitive(name)) if name == PRIMITIVES.cr || name == PRIMITIVES.crcr => {
                                todo!("fuck")
                            }
                            Some(CharOrPrimitive::Primitive(name)) if name == PRIMITIVES.span => {
                                todo!("fuck")
                            }
                            _ => f(aux,state,t)
                        }
                         */
                        _ => f(aux,state,t)
                    }
                });
            }
        }
    }

    /// Wrapper around [`Mouth::get_next_opt`] that, in case we are in an `\halign` or `\valign`,
    /// make sure to replace `&`, `\cr` etc. with the appropriate tokens.
    /// See also [`EngineReferences::get_next`].
    fn get_next_opt(&mut self, mouth:&mut ET::Mouth, aux:&mut EngineAux<ET>, state:&ET::State,noexpand:bool) -> Option<ET::Token> {
        match self.get_align_data() {
            None => mouth.get_next_opt(aux,state),
            Some(a) => match mouth.get_next_opt(aux,state) {
                Some(t) => match t.command_code() {
                    CommandCode::BeginGroup => { a.ingroups += 1; Some(t) }
                    CommandCode::EndGroup => {
                        if a.ingroups == 0 { aux.error_handler.too_many_closebraces() }
                        a.ingroups -= 1;
                        Some(t)
                    }
                    CommandCode::AlignmentTab if !noexpand && a.ingroups == a.groupval() => {
                        a.on_alignment_tab(mouth,aux);
                        self.get_next_opt(mouth,aux,state,false)
                    }
                    CommandCode::Escape | CommandCode::Active | CommandCode::Primitive if !noexpand && a.ingroups == a.groupval() => match Self::char_or_primitive(state,&t) {
                        Some(CharOrPrimitive::Primitive(name)) if name == PRIMITIVES.cr || name == PRIMITIVES.crcr => {
                            a.on_cr(mouth,aux,state);
                            return self.get_next_opt(mouth,aux,state,false)
                        }
                        Some(CharOrPrimitive::Primitive(name)) if !noexpand && name == PRIMITIVES.span => {
                            a.span = true;
                            a.on_alignment_tab(mouth,aux);
                            self.get_next_opt(mouth,aux,state,false)
                        }
                        _ => Some(t)
                    }
                    _ => Some(t)
                }
                None => None
            }
        }
    }

    /// Wrapper around [`Mouth::read_until_endgroup`] that, in case we are in an `\halign` or `\valign`,
    /// make sure to replace `&`, `\cr` etc. with the appropriate tokens.
    /// See also [`EngineReferences::read_until_endgroup`].
    fn read_until_endgroup<Fn:FnMut(&mut EngineAux<ET>,&ET::State,ET::Token)>(&mut self, mouth:&mut ET::Mouth, aux:&mut EngineAux<ET>, state:&ET::State,token:&ET::Token,mut cont:Fn) -> ET::Token {
        match self.get_align_data() {
            None => (),
            Some(d) => {
                if d.ingroups == 0 { aux.error_handler.too_many_closebraces() }
                d.ingroups -= 1
            }
        }
        mouth.read_until_endgroup(aux,state,token,|a,t|cont(a,state,t))
    }

    /// Wrapper around [`Mouth::requeue`] that makes sure to update the [`AlignData`] if we are in an
    /// `\halign` or `\valign`.
    /// See also [`EngineReferences::requeue`].
    fn requeue(&mut self,aux:&EngineAux<ET>,mouth:&mut ET::Mouth,t:ET::Token) {
        if let Some(data) = self.get_align_data() {
            let cc = t.command_code();
            if cc == CommandCode::BeginGroup {
                if data.ingroups == 0 { aux.error_handler.too_many_closebraces() }
                data.ingroups -= 1;
            }
            else if cc == CommandCode::EndGroup {
                data.ingroups += 1;
            }
        }
        mouth.requeue(t)
    }

    /// Read an integer from the input stream. See also [`EngineReferences::read_int`].
    fn read_int(engine:&mut EngineReferences<ET>,skip_eq:bool) -> ET::Int {
        methods::read_int(engine,skip_eq)
    }

    /// Read a dimension value from the input stream. See also [`EngineReferences::read_dim`].
    fn read_dim(engine:&mut EngineReferences<ET>,skip_eq:bool) -> ET::Dim {
        methods::read_dim(engine,skip_eq)
    }

    /// Read a skip value from the input stream. See also [`EngineReferences::read_skip`].
    fn read_skip(engine:&mut EngineReferences<ET>,skip_eq:bool) -> Skip<ET::Dim> {
        methods::read_skip(engine,skip_eq)
    }

    /// Read a muskip value from the input stream. See also [`EngineReferences::read_muskip`].
    fn read_muskip(engine:&mut EngineReferences<ET>,skip_eq:bool) -> MuSkip<ET::MuDim> {
        methods::read_muskip(engine,skip_eq)
    }

    /// Read a mudim value from the input stream (for `\mkern`). See also [`EngineReferences::read_mudim`].
    fn read_mudim(engine:&mut EngineReferences<ET>,skip_eq:bool) -> ET::MuDim {
        methods::read_mudim(engine, skip_eq)
    }

    /// Read a string from the input stream. See also [`EngineReferences::read_string`].
    fn read_string(engine:&mut EngineReferences<ET>,skip_eq:bool,target:&mut String) {
        methods::read_string(engine,skip_eq,target)
    }

    /// Check whether the input stream starts with the given keyword. See also [`EngineReferences::read_keyword`].
    fn read_keyword(engine:&mut EngineReferences<ET>,kw:&[u8]) -> bool {
        methods::read_keyword(engine,kw,None)
    }

    /// Check whether the input stream starts with one of the given keywords. See also [`EngineReferences::read_keywords`].
    fn read_keywords<'a>(engine:&mut EngineReferences<ET>,kw:&'a[&'a[u8]]) -> Option<&'a[u8]>     {
        methods::read_keywords(engine,kw,None)
    }

    /// Check whether the next character is one of the provided ones. See also [`EngineReferences::read_chars`].
    fn read_chars(engine:& mut EngineReferences<ET>,kws:&[u8]) -> Result<u8,ET::Token> {
        methods::read_chars(engine,kws)
    }

    /// Inspect the given [`Token`] and return its current definition, if any.
    /// See also [`EngineReferences::resolve`].
    fn resolve<'a>(state:&'a ET::State,token:&ET::Token) -> ResolvedToken<'a,ET> {
        match token.to_enum() {
            StandardToken::Character(c,CommandCode::Active) =>
                ResolvedToken::Cmd(state.get_ac_command(c)),
            StandardToken::Character(c,o) => return ResolvedToken::Tk{char:c,code:o},
            StandardToken::ControlSequence(cs) =>
                ResolvedToken::Cmd(state.get_command(&cs)),
            StandardToken::Primitive(id) =>
                ResolvedToken::Cmd(state.primitives().get_id(id))
                //ResolvedToken::Cmd{token,cmd:state.primitives().get_id(id).map(|p| Command::Primitive {name:id,cmd:p.clone()}) }
        }
    }

    /// Inspects the [`Token`] and returns either the [`PrimitiveIdentifier`] or the [`Character`] and [`CommandCode`]
    /// or None if the token is neither a primitive nor a character. Useful for e.g. quick checks in an `\halign` whether the
    /// token is a specific primitive (`\cr`,`\crcr`,`\span`) or a character (`&`, space).
    fn char_or_primitive(state:&ET::State,token:&ET::Token) -> Option<CharOrPrimitive<ET>> {
        match token.to_enum() {
            StandardToken::Primitive(id) => Some(CharOrPrimitive::Primitive(id)),
            StandardToken::Character(c,CommandCode::Active) => match state.get_ac_command(c) {
                Some(TeXCommand::Primitive {name,..}) => Some(CharOrPrimitive::Primitive(*name)),
                Some(TeXCommand::Char {char,code}) => Some(CharOrPrimitive::Char(*char, *code)),
                Some(TeXCommand::CharDef(c)) => Some(CharOrPrimitive::Char(*c, CommandCode::Other)),
                _ => None
            }
            StandardToken::ControlSequence(cs) => match state.get_command(&cs) {
                Some(TeXCommand::Primitive {name,..}) => Some(CharOrPrimitive::Primitive(*name)),
                Some(TeXCommand::Char {char,code}) => Some(CharOrPrimitive::Char(*char, *code)),
                Some(TeXCommand::CharDef(c)) => Some(CharOrPrimitive::Char(*c, CommandCode::Other)),
                _ => None
            }
            StandardToken::Character(c,code) => Some(CharOrPrimitive::Char(c,code))
        }
    }

    /// Expand the given expandable [`Token`] with its given expansion function. See also
    /// [`Expandable`](crate::commands::PrimitiveCommand::Expandable).
    fn do_expandable(engine: &mut EngineReferences<ET>,name:PrimitiveIdentifier,token:ET::Token,f:fn(&mut EngineReferences<ET>,&mut Vec<ET::Token>,ET::Token)) {
        engine.trace_command(|engine| format!("{}", name.display(engine.state.get_escape_char())));
        let mut exp = Vec::new();// ExpansionContainer::new(engine.aux.memory.get_token_vec());
        f(engine,&mut exp,token);
        engine.mouth.push_vec(exp);
    }
    /// Expand the given [`Macro`] with its given expansion function.
    fn do_macro(engine: &mut EngineReferences<ET>,m:Macro<ET::Token>,token:ET::Token);

    /// Expand the given expandable [`Token`] with its given simple expansion function. See also
    /// [`SimpleExpandable`](crate::commands::PrimitiveCommand::SimpleExpandable).
    fn do_simple_expandable(engine: &mut EngineReferences<ET>,name:PrimitiveIdentifier,token:ET::Token,f:fn(&mut EngineReferences<ET>,ET::Token)) {
        engine.trace_command(|engine| format!("{}", name.display(engine.state.get_escape_char())));
        f(engine,token)
    }

    /// Expand the given conditional. See also [`Conditional`](crate::commands::PrimitiveCommand::Conditional)
    fn do_conditional(engine:&mut EngineReferences<ET>,name:PrimitiveIdentifier,token:ET::Token,f:fn(&mut EngineReferences<ET>,ET::Token) -> bool,unless:bool) {
        let trace = engine.state.get_primitive_int(PRIMITIVES.tracingifs) > ET::Int::default();
        let index = engine.gullet.get_conditionals().len();
        engine.gullet.get_conditionals().push(ActiveConditional::Unfinished(name));
        if trace {
            //crate::debug_log!(error => "Here: {}",engine.preview());
            engine.aux.outputs.write_neg1(format_args!("{{{}: (level {}) entered on line {}}}",name.display(engine.state.get_escape_char()),index+1,engine.mouth.line_number()));
        }
        let mut ret = f(engine,token.clone());
        if unless { ret = !ret }
        if ret {
            if trace {
                engine.aux.outputs.write_neg1("{true}");
            }
            if name != PRIMITIVES.ifcase {
                match engine.gullet.get_conditionals().get_mut(index) {
                    Some(u@ActiveConditional::Unfinished(_)) =>
                        *u = ActiveConditional::True(name),
                    _ => unreachable!()
                }
            }
        } else {
            if trace {
                engine.aux.outputs.write_neg1("{false}");
            }
            if name == PRIMITIVES.ifcase {
                methods::case_loop(engine,index + 1, &token);
            } else {
                methods::false_loop(engine, index + 1, true,false,&token);
            }
            if trace {
                if engine.gullet.get_conditionals().len() > index {
                    engine.aux.outputs.write_neg1(
                        format_args!("{{{}else: {} (level {}) entered on line {}}}",
                                     ET::Char::display_opt(engine.state.get_escape_char()),
                                     name.display(engine.state.get_escape_char()),
                                     index+1, engine.mouth.line_number()
                        ));
                } else {
                    engine.aux.outputs.write_neg1(
                        format_args!("{{{}fi: {} (level {}) entered on line {}}}",
                                     ET::Char::display_opt(engine.state.get_escape_char()),
                                     name.display(engine.state.get_escape_char()),
                                     index+1, engine.mouth.line_number()
                        ));
                }
            }
        }
    }

    /// Expand [`Token`]s until encountering an [`EndGroup`](CommandCode::EndGroup)-token.
    /// See also [`EngineReferences::expand_until_endgroup`].
    fn expand_until_endgroup<Fn:FnMut(&mut EngineAux<ET>,&ET::State,ET::Token)>(engine:&mut EngineReferences<ET>,expand_protected:bool,edef_like:bool,token:&ET::Token,cont:Fn) {
        methods::expand_until_endgroup(engine,expand_protected,edef_like,token,cont);
    }
}

/// Default implementation of a [`Gullet`].
pub struct DefaultGullet<ET:EngineTypes> {
    align_data:Vec<AlignData<ET::Token,ET::Dim>>,
    conditionals:Vec<ActiveConditional<ET::Int>>,
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
    fn push_align(&mut self, ad: AlignData<ET::Token,ET::Dim>) {
        self.align_data.push(ad)
    }
    fn pop_align(&mut self) -> Option<AlignData<ET::Token, ET::Dim>> {
        self.align_data.pop()
    }
    fn do_macro(engine: &mut EngineReferences<ET>, m: Macro<ET::Token>, token: ET::Token) {
        let trace = engine.state.get_primitive_int(PRIMITIVES.tracingcommands) > ET::Int::default();
        if trace {
            match token.to_enum() {
                StandardToken::ControlSequence(cs) => {
                    engine.aux.outputs.write_neg1(format_args!("~.{}{} {}",
                                                               ET::Char::display_opt(engine.state.get_escape_char()),
                                                               engine.aux.memory.cs_interner().resolve(&cs),
                                                               m.meaning(engine.aux.memory.cs_interner(), engine.state.get_catcode_scheme(), engine.state.get_escape_char())
                    ));
                    //engine.aux.outputs.write_neg1(format_args!("Here: {}",engine.preview()));
                }
                StandardToken::Character(c,_) => {
                    engine.aux.outputs.write_neg1(format_args!("~.{} {}",
                                                               c.display(),
                                                               m.meaning(engine.aux.memory.cs_interner(), engine.state.get_catcode_scheme(), engine.state.get_escape_char())
                    ));
                    //engine.aux.outputs.write_neg1(format_args!("Here: {}",engine.preview()));
                }
                _ => unreachable!()
            }
            //crate::debug_log!(error => "Here: {}",engine.preview());

/*
            match engine.gullet.get_align_data() {
                Some(d) => {
                    let ig = d.ingroups;
                    let val = d.groupval();
                    println!("Here: {} == {}",ig,val);
                }
                _ => ()
            }
*/

        }
        if m.signature.params.is_empty() {
            engine.mouth.push_exp(&m.expansion);
            return;
        }
        let mut args = engine.mouth.get_args();
        methods::read_arguments(engine,&mut args,m.signature.params,m.long,&token);
        if trace {
            for i in 0..m.signature.arity {
                engine.aux.outputs.write_neg1(
                    format_args!("#{}<-{}",
                                 i+1,
                                 crate::tex::tokens::token_lists::TokenListDisplay::from_vec(&args[i as usize], engine.aux.memory.cs_interner(), engine.state.get_catcode_scheme(), engine.state.get_escape_char(),false)
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
    fn get_align_data(&mut self) -> Option<&mut AlignData<ET::Token,ET::Dim>> {
        self.align_data.last_mut()
    }
    fn get_conditional(&self) -> Option<ActiveConditional<ET::Int>> {
        self.conditionals.last().cloned()
    }
    fn get_conditionals(&mut self) -> &mut Vec<ActiveConditional<ET::Int>> {
        &mut self.conditionals
    }
}

impl<ET:EngineTypes> EngineReferences<'_,ET> {
    /// Yields [`Token`]s from the input stream until and passes them on to `cont` until `cont` returns `false`.
    pub fn iterate<Fn:FnMut(&mut EngineAux<ET>,&ET::State,ET::Token) -> bool>(&mut self,token:&ET::Token,mut cont:Fn) {
        self.gullet.iterate(self.mouth,self.aux,self.state,token,|a,s,t| cont(a, s, t))
    }

    /// Push the provided [`Token`] back onto the input stream.
    pub fn requeue(&mut self,t:ET::Token) {
        self.gullet.requeue(self.aux,self.mouth,t)
    }

    /// Yields [`Token`]s from the input stream until an [`EndGroup`](CommandCode::EndGroup)-token is encountered.
    pub fn read_until_endgroup<Fn:FnMut(&mut EngineAux<ET>,&ET::State,ET::Token)>(&mut self,token:&ET::Token, cont:Fn) -> ET::Token {
        self.gullet.read_until_endgroup(self.mouth,self.aux,self.state,token,cont)
    }
    /// Yields [`Token`]s from the input stream until an [`EndGroup`](CommandCode::EndGroup)-token is encountered, expanding
    /// all expandable tokens along the way. If `expand_protected` is `true`, protected tokens are expanded as well.
    /// If `edef_like` is `true`, the expansion is done in `\edef`-mode, i.e. tokens expanded from `\the` are not expanded
    /// further and [`Parameter`](CommandCode::Parameter)-tokens expanded from `\the` are doubled.
    pub fn expand_until_endgroup<Fn:FnMut(&mut EngineAux<ET>,&ET::State,ET::Token)>(&mut self,expand_protected:bool,edef_like:bool,token:&ET::Token,mut cont:Fn) {
        ET::Gullet::expand_until_endgroup(self,expand_protected,edef_like,token,|a,s,t| cont(a,s,t))
    }
    /// Yields the next [`Token`] from the input stream.
    pub fn get_next(&mut self,noexpand:bool) -> Option<ET::Token> {
        self.gullet.get_next_opt(self.mouth,self.aux,self.state,noexpand)
    }
    /// Read an integer value from the input stream.
    pub fn read_int(&mut self,skip_eq:bool) -> <ET::Num as NumSet>::Int {
        ET::Gullet::read_int(self,skip_eq)
    }
    /// Read a string from the input stream.
    pub fn read_string(&mut self,skip_eq:bool,target:&mut String) {
        ET::Gullet::read_string(self,skip_eq,target)
    }
    /// Check whether the input stream starts with the given keyword.
    pub fn read_keyword(&mut self,kw:&[u8]) -> bool {
        ET::Gullet::read_keyword(self,kw)
    }
    /// Check whether the input stream starts with one of the given keywords.
    pub fn read_keywords<'a>(&mut self,kw:&'a[&'a[u8]]) -> Option<&'a[u8]> {
        ET::Gullet::read_keywords(self,kw)
    }
    /// Read a character code from the input stream.
    pub fn read_charcode(&mut self,skip_eq:bool) -> ET::Char {
        let i:i64 = ET::Gullet::read_int(self,skip_eq).into();
        if i < 0 {
            tex_error!(self,other,&format!("Bad character code ({})",i));
            return ET::Char::from(0)
        }
        match ET::Char::try_from(i as u64) {
            Ok(c) => c,
            _ => {
                tex_error!(self,other,&format!("Bad character code ({})",i));
                ET::Char::from(0)
            }
        }

    }
    /// Read a braced string from the input stream (unlike [`read_string`](EngineReferences::read_string)
    /// which does not require braces). Compare e.g. `\message{Hello World}` (which would use `read_braced_string`)
    /// and `\input myfile.tex` (which would use [`read_string`](EngineReferences::read_string)).
    pub fn read_braced_string(&mut self,skip_ws:bool, expand_protected:bool,token:&ET::Token, mut str:&mut String) {
        loop {
            match self.get_next(false) {
                Some(t) => match t.command_code() {
                    CommandCode::BeginGroup => break,
                    CommandCode::Space if skip_ws => (),
                    _ => tex_error!(self,missing_begingroup,token.clone()),
                }
                None => self.aux.error_handler.file_end_while_scanning(self.state,&self.aux.memory.cs_interner(),token.clone())
            }
        }
        ET::Gullet::expand_until_endgroup(self,expand_protected,false,token,|a,s,t| {
            t.display_fmt(a.memory.cs_interner(),s.get_catcode_scheme(),
                          s.get_escape_char(),&mut str).unwrap();
        });
    }

    /// Read a dimension value from the input stream.
    pub fn read_dim(&mut self,skip_eq:bool) -> ET::Dim {
        ET::Gullet::read_dim(self,skip_eq)
    }
    /// Read a skip value from the input stream.
    pub fn read_skip(&mut self,skip_eq:bool) -> Skip<ET::Dim> {
        ET::Gullet::read_skip(self,skip_eq)
    }
    /// Read a muskip value from the input stream.
    pub fn read_muskip(&mut self,skip_eq:bool) -> MuSkip<ET::MuDim> {
        ET::Gullet::read_muskip(self,skip_eq)
    }
    /// Read a mudim value from the input stream (for `\mkern`).
    pub fn read_mudim(&mut self,skip_eq:bool) -> ET::MuDim {
        ET::Gullet::read_mudim(self,skip_eq)
    }
    /// Check whether the next character is one of the provided ones. Returns the character if so,
    /// otherwise returns the inspected [`Token`] to be further processed or [Self::requeue]d.
    pub fn read_chars(&mut self,kws:&[u8]) -> Result<u8,ET::Token> {
        ET::Gullet::read_chars(self,kws)
    }
    /// Inspect the given [`Token`] and return its current definition, if any.
    pub fn resolve(&self,token:&ET::Token) -> ResolvedToken<'_,ET> {
        ET::Gullet::resolve(self.state,token)
    }
}