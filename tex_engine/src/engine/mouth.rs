/*! A [`Mouth`] provides a stream of [`Token`]s to be processed by an engine; either by tokenizing
    a file or by returning the [`Token`]s expanded by a macro
 */

use crate::commands::primitives::{PrimitiveIdentifier, PRIMITIVES};
use crate::engine::{EngineAux, EngineReferences, EngineTypes};
use crate::engine::filesystem::{File, FileLineSource};
use crate::engine::filesystem::SourceReference;
use crate::tex::tokens::token_lists::MacroExpansion;
use crate::engine::mouth::strings::InputTokenizer;
use crate::engine::state::State;
use crate::engine::utils::outputs::Outputs;
use crate::prelude::{CommandCode, TokenList};
use crate::tex::catcodes::CategoryCodeScheme;
use crate::tex::tokens::control_sequences::CSName;
use crate::tex::characters::StringLineSource;
use crate::tex::tokens::Token;

pub mod strings;

/// A [`Mouth`] provides a stream of [`Token`]s to be processed by an engine; either by tokenizing
/// a file or by returning the [`Token`]s expanded by a macro. Since a TeX engine spends most of its
/// time processing and expanding [`Token`]s, the [`Mouth`] is likely the most performance critical component of the engine.
///
/// [`DefaultMouth`] is the default implementation of [`Mouth`] that has been quite well optimized.
///
/// During a run, most methods provided by the [`Mouth`] should *not* be called directly, since they circumvent the [Gullet](crate::engine::Gullet),
/// which needs to occasionally do some bookkeeping (e.g. counting braces in an `\halign`).
/// Instead, the [`Mouth`] should if possible be accessed through the [`EngineReferences`]
/// or the [`Gullet`](crate::engine::gullet::Gullet) only.
///
/// Note that we do not require `ET:`[`EngineTypes`]`<`[`Mouth`](EngineTypes::Mouth)`=Self>` - this allows for
/// implementing your own Mouth by just wrapping an existing implementation in a new wrapper struct and pass on functionality
/// to the inner Mouth, which would otherwise
/// fail since `ET::Mouth` would be the outer wrapper struct, not the inner one.
pub trait Mouth<ET:EngineTypes> {
    /// Create a new [`Mouth`]. May use [`EngineAux`] and [`State`], although in practice, the default implementation doesn't.
    fn new(aux:&mut EngineAux<ET>,state:&mut ET::State) -> Self;
    /// Push a file to the [`Mouth`]. The [`Mouth`] will tokenize the file contents and return the [`Token`]s lazily.
    fn push_file(&mut self,f:ET::File);
    /// Push a string to the [`Mouth`]. The [`Mouth`] will tokenize the string and return the [`Token`]s lazily.
    fn push_string(&mut self,s:StringLineSource<ET::Char>);
    /// Push a [`TokenList`] to the [`Mouth`] - e.g. from a token register or `\everypar`.
    fn push_exp(&mut self,exp:&TokenList<ET::Token>);
    /// Push a [`Vec`] of [`Token`]s to the [`Mouth`]. This is occasionally useful for things like `\write`.
    fn push_vec(&mut self, exp: Vec<ET::Token>);
    /// Push a [`MacroExpansion`] (with arguments already read) to the [`Mouth`]. The [`Mouth`] will return the [`Token`]s lazily,
    /// resolving the parameter tokens in the expansion in the process.
    fn push_macro_exp(&mut self,exp:MacroExpansion<ET::Token>);
    /// Push a [`Token`] back to the [`Mouth`]. This is useful for e.g. `\futurelet`, `\expandafter`, or when
    /// reading keywords, numbers, dimensions, etc. that often read "too far ahead" and need to back up.
    /// This method should not be called directly, but rather through [`EngineReferences::requeue`]
    /// or [`Gullet::requeue`](crate::engine::gullet::Gullet::requeue).
    fn requeue(&mut self,t:ET::Token);
    /// Get the next [`Token`] from the [`Mouth`]. Returns `None` if the [`Mouth`] is empty.
    /// This method should not be called directly, but rather through [`EngineReferences::get_next`]
    /// or [`Gullet::get_next_opt`](crate::engine::gullet::Gullet::get_next_opt).
    fn get_next_opt(&mut self, aux:&mut EngineAux<ET>, state:&ET::State) -> Option<ET::Token>;
    /// Iterate over the [`Token`]s in the [`Mouth`] until `cont` returns `false`. Can be faster than repeatedly calling
    /// [`get_next_opt`](Self::get_next_opt), but
    /// blocking both state changes and expanding macros. Useful for e.g. reading macro arguments or the expansion list
    /// in `\def`.
    /// This method should not be called directly, but rather through [`EngineReferences::iterate`]
    /// or [`Gullet::iterate`](crate::engine::gullet::Gullet::iterate).
    fn iterate<Fn:FnMut(&mut EngineAux<ET>,ET::Token) -> bool>(&mut self,aux:&mut EngineAux<ET>,state:&ET::State,token:&ET::Token,cont:Fn);
    /// `\endinput` - removes the last file from the [`Mouth`] without inserting `\everyeof` or an [`EOF`](crate::tex::catcodes::CommandCode::EOF)
    /// [`Token`].
    fn endinput(&mut self, aux:&EngineAux<ET>);
    /// clears the mouth at the end of a run, if desired.
    fn finish(&mut self);

    /// Get the current [`SourceReference`] of the [`Mouth`] (file/line/column).
    fn current_sourceref(&self) -> SourceReference<<ET::File as File>::SourceRefID>;
    /// The mouth (can) track(s) two [`SourceReference`]s: the current one (see [`current_sourceref`](Self::current_sourceref))
    /// and the position
    /// of the last [`Token`] encountered in the top-loop of an engine run that was not the result of an expansion.
    /// The latter is returned by this function. The intuition being that this one indicates the start of the macro
    ///responsible for what is currently happening, even if the Mouth is already further along because more [`Token`]s
    ///have been eaten as arguments to a macro etc.
    fn start_ref(&self) -> SourceReference<<ET::File as File>::SourceRefID>;
    /// Tells the mouth to update the start reference to the current [`SourceReference`] (see [`start_ref`](Self::start_ref)).
    fn update_start_ref(&mut self);
    /// The current line number in the top-most file in the [`Mouth`].
    fn line_number(&self) -> usize;
    /// We (can) reuse an array of Token vectors for macro arguments and reuse it to avoid frequent memory allocations.
    /// This method provides such an array. If it is not pushed to the mouth using [`push_macro_exp`](Self::push_macro_exp),
    /// later, it should be given back to the mouth using [`return_args`](Self::return_args) later.
    fn get_args(&mut self) -> [Vec<ET::Token>;9];
    /// Return the array of Token vectors to the mouth. Should only be called with an array that was previously obtained
    /// using [`get_args`](Self::get_args).
    fn return_args(&mut self,args:[Vec<ET::Token>;9]);

    /// Convenience method reading [`Token`]s in the [`Mouth`] until the next [EndGroup](crate::tex::catcodes::CommandCode::EndGroup)
    ///[`Token`] is encountered and returns that. Useful whenever a group is to be taken; e.g. when reading macro arguments.
    /// This method should not be called directly, but rather through [`EngineReferences::read_until_endgroup`]
    /// or [`Gullet::read_until_endgroup`](crate::engine::gullet::Gullet::read_until_endgroup).
    fn read_until_endgroup<Fn:FnMut(&mut EngineAux<ET>,ET::Token)>(&mut self,aux:&mut EngineAux<ET>,state:&ET::State,token:&ET::Token,mut cont:Fn) -> ET::Token {
        let mut ingroups = 0;
        let mut eg:Option<ET::Token> = None;
        self.iterate(aux,state,token,|a,t| {
            match t.command_code() {
                CommandCode::BeginGroup => ingroups += 1,
                CommandCode::EndGroup => {
                    if ingroups == 0 { eg = Some(t);  return false }
                    ingroups -= 1;
                }
                CommandCode::Primitive if t.is_primitive() == Some(PRIMITIVES.noexpand) => return true,
                _ => (),
            }
            cont(a,t);
            true
        });
        eg.unwrap()
    }

    /// For debugging purposes, this method returns a string representation of the upcoming stuff in the [`Mouth`].
    fn preview(&self, int:&<ET::CSName as CSName<ET::Char>>::Handler, cc:&CategoryCodeScheme<ET::Char>, esc:Option<ET::Char>) -> String;
}

enum TokenSource<T:Token,F:File<Char=T::Char>> {
    String(InputTokenizer<T::Char,StringLineSource<T::Char>>),
    File(InputTokenizer<T::Char,F::LineSource>, F::SourceRefID),
    Vec(Vec<T>)
}

/// The default implementation of [`Mouth`]. Well optimized to be fast, but at the cost of not keeping track
/// of "the depth of current macro expansions" - I found that to be an acceptable loss, since afaict, the only thing that
/// is done with that information
/// is to print a corresponding number of `.`s if `\tracingcommands` is set. By omitting that and just concatenating
/// all expansions into a single Vec from which to `pop()`, we gain a massive speedup.
pub struct DefaultMouth<ET:EngineTypes> {
    inputs:Vec<TokenSource<ET::Token,ET::File>>,
    args:Option<[Vec<ET::Token>;9]>,
    start_ref:Vec<SourceReference<<ET::File as File>::SourceRefID>>,
    vecs:Vec<Vec<ET::Token>>
}

impl<ET:EngineTypes> Mouth<ET> for DefaultMouth<ET> {
    fn new(_aux: &mut EngineAux<ET>, _state: &mut ET::State) -> Self {
        Self {
            inputs:Vec::new(),args:Some(array_init::array_init(|_| Vec::new())),
            start_ref:vec!(),vecs:vec!()
        }
    }

    fn finish(&mut self) {
        for s in self.inputs.drain(..) { match s {
          TokenSource::Vec(mut v) => {
              v.clear(); self.vecs.push(v)
          }
            _ => ()
        } }
        self.start_ref.clear();
    }

    fn current_sourceref(&self) -> SourceReference<<ET::File as File>::SourceRefID> {
        for s in self.inputs.iter().rev() {
            match s {
                TokenSource::File(f,id) =>
                    return SourceReference {
                        file:*id,
                        line:f.line(),
                        column:f.column()
                    },
                _ => ()
            }
        }
        self.start_ref.last().copied().unwrap_or_default()
    }
    fn start_ref(&self) -> SourceReference<<ET::File as File>::SourceRefID> {
        self.start_ref.last().copied().unwrap_or_default()
    }
    fn update_start_ref(&mut self) {
        match self.inputs.last() {
            Some(TokenSource::File(f,id)) => {
                let rf = SourceReference {
                    file:*id,
                    line:f.line(),
                    column:f.column()
                };
                match self.start_ref.last_mut() {
                    None => self.start_ref.push(rf),
                    Some(s) => *s = rf
                }
            }
            Some(TokenSource::Vec(v)) if v.is_empty() => {
                if let Some(TokenSource::Vec(v)) = self.inputs.pop() {
                    self.vecs.push(v)
                } else {unreachable!()}
                self.update_start_ref()
            }
            _ => ()
        }
    }


    fn get_args(&mut self) -> [Vec<ET::Token>;9] {
        match std::mem::take(&mut self.args) {
            Some(a) => a,
            None => unreachable!()//array_init::array_init(|_| Vec::new())
        }
        //array_init::array_init(|_| Vec::new())
    }

    fn return_args(&mut self,mut exp:[Vec<ET::Token>;9]) {
        for a in exp.iter_mut() { a.clear() }
        self.args = Some(exp);
    }

    fn endinput(&mut self, aux:&EngineAux<ET>) {
        for (i,s) in self.inputs.iter().enumerate().rev() {
            match s {
                TokenSource::File(f,_) => {
                    aux.outputs.file_close(f.source.path().display());
                    self.inputs.remove(i);
                    self.start_ref.pop();
                    return
                }
                _ => ()
            }
        }
    }

    fn line_number(&self) -> usize {
        for s in self.inputs.iter().rev() {
            match s {
                TokenSource::File(s,_) => return s.line(),
                _ => ()
            }
        }
        0
    }

    fn push_macro_exp(&mut self, mut exp: MacroExpansion<ET::Token>) {
        self.with_list(|v| exp.consume_rev(v));
        self.return_args(exp.args);
    }

    fn push_exp(&mut self, exp: &TokenList<ET::Token>) {
        self.with_list(|v|v.extend(exp.0.iter().rev().cloned()))
    }

    fn push_vec(&mut self, exp: Vec<ET::Token>) {
        self.with_list(|v| v.extend(exp.into_iter().rev()))
    }

    fn push_string(&mut self, s: StringLineSource<ET::Char>) {
        self.inputs.push(TokenSource::String(InputTokenizer::new(s)));
    }

    fn requeue(&mut self,t:ET::Token) {
        self.with_list(|v| v.push(t))
    }

    fn push_file(&mut self, f: ET::File) {
        //self.clean();
        let id = f.sourceref();
        let s = f.line_source().unwrap();
        let rf = SourceReference {
            file:id,
            line:0,
            column:0
        };
        self.inputs.push(TokenSource::File(InputTokenizer::new(s), id));
        self.start_ref.push(rf);
    }

    fn get_next_opt(&mut self,aux:&mut EngineAux<ET>,state:&ET::State) -> Option<ET::Token> {
        while let Some(src) = self.inputs.last_mut() {
            match src {
                TokenSource::Vec(v) => {
                    match v.pop() {
                        Some(t) => return Some(t),
                        _ => {
                            if let Some(TokenSource::Vec(v)) = self.inputs.pop() {
                                self.vecs.push(v);
                            } else {unreachable!()}
                        }
                    }
                }
                TokenSource::String(s) => {
                    match s.get_next(aux.memory.cs_interner_mut(), state.get_catcode_scheme(), state.get_endline_char()) {
                        Ok(Some(t)) => return Some(t),
                        Ok(_) => return Some(self.end_file(aux,state)),
                        Err(ic) => {
                            match aux.error_handler.invalid_character(state, ic.0){
                                Some(s) => self.push_string(s),
                                _ => ()
                            }
                            continue
                        }
                    }
                }
                TokenSource::File(s,_) => {
                    let cc: &CategoryCodeScheme<ET::Char> = state.get_catcode_scheme();
                    let endline: Option<ET::Char> = state.get_endline_char();
                    match s.get_next(aux.memory.cs_interner_mut(), cc, endline) {
                        Ok(Some(t)) => return Some(t),
                        Ok(_) => return Some(self.end_file(aux,state)),
                        Err(ic) => {
                            match aux.error_handler.invalid_character(state, ic.0){
                                Some(s) => self.push_string(s),
                                _ => ()
                            }
                            continue
                        }
                    }
                }
            }
        }
        None
    }

    fn iterate<Fn:FnMut(&mut EngineAux<ET>,ET::Token) -> bool>(&mut self,aux:&mut EngineAux<ET>,state:&ET::State,token:&ET::Token,mut cont:Fn) {
        'top: loop {
            match self.inputs.last_mut() {
                Some(TokenSource::Vec(v)) => {
                    while let Some(t) = v.pop() {
                        if !cont(aux,t) {return}
                    }
                    if let Some(TokenSource::Vec(v)) = self.inputs.pop() {
                        self.vecs.push(v);
                    } else {unreachable!()}
                }
                Some(TokenSource::String(s)) => {
                    let cc = state.get_catcode_scheme();
                    let endline = state.get_endline_char();
                    loop {
                        match s.get_next(aux.memory.cs_interner_mut(), cc, endline) {
                            Ok(Some(t)) => if !cont(aux,t) { return },
                            Ok(_) => aux.error_handler.file_end_while_scanning(state,aux.memory.cs_interner(),token.clone()),
                            Err(ic) => {
                                match aux.error_handler.invalid_character(state, ic.0){
                                    Some(s) => self.push_string(s),
                                    _ => ()
                                }
                                continue 'top
                            }
                        }
                    }
                }
                Some(TokenSource::File(s,_)) => {
                    let cc = state.get_catcode_scheme();
                    let endline = state.get_endline_char();
                    loop {
                        match s.get_next(aux.memory.cs_interner_mut(), cc, endline) {
                            Ok(Some(t)) => if !cont(aux,t) { return },
                            Ok(_) => aux.error_handler.file_end_while_scanning(state,aux.memory.cs_interner(),token.clone()),
                            Err(ic) => {
                                match aux.error_handler.invalid_character(state, ic.0){
                                    Some(s) => self.push_string(s),
                                    _ => ()
                                }
                                continue 'top
                            }
                        }
                    }
                }
                None => aux.error_handler.file_end_while_scanning(state,aux.memory.cs_interner(),token.clone()),
            }
        }
    }

    fn preview(&self, int:&<ET::CSName as CSName<ET::Char>>::Handler, cc:&CategoryCodeScheme<ET::Char>, esc:Option<ET::Char>) -> String {
        let mut str = String::new();
        for src in self.inputs.iter().rev() {
            match src {
                //TokenSource::TokenList(s) => s.preview(int,cc,esc,&mut str),
                TokenSource::String(s) => s.preview(&mut 1000,&mut str).unwrap(),
                //TokenSource::Expansion(s) => s.preview(int,cc,esc,&mut str),
                TokenSource::File(s,_) => {
                    s.preview(&mut 1000,&mut str).unwrap();break
                },
                TokenSource::Vec(v) => {
                    for t in v.iter().rev().take(1000) {
                        t.display_fmt(int,cc,esc,&mut str).unwrap()
                    }
                }
                /*TokenSource::Requeued(t) => {
                    t.display_fmt(int,cc,esc,&mut str).unwrap()
                }*/
            }
        }
        str
    }
}

impl<ET:EngineTypes> DefaultMouth<ET> {
    /// Trivial conversion between different compatible [`EngineTypes`].
    pub fn into<ET2:EngineTypes<Token=ET::Token,File=ET::File>>(self) -> DefaultMouth<ET2> {
        DefaultMouth { inputs:self.inputs,args:self.args,start_ref:self.start_ref,vecs:self.vecs }
    }
    /// Less trivial conversion between different [`EngineTypes`] with compatible [`Token`]s.
    pub fn into_tokens<ET2:EngineTypes<Char=ET::Char,File=ET::File>,F:FnMut(ET::Token) -> ET2::Token>(self,mut token:F) -> DefaultMouth<ET2> {
        DefaultMouth {
            inputs:self.inputs.into_iter().map(|s| match s {
                TokenSource::String(s) => TokenSource::String(s),
                TokenSource::File(s,id) => TokenSource::File(s,id),
                TokenSource::Vec(v) => TokenSource::Vec(v.into_iter().map(|t|token(t)).collect())
            }).collect(),
            args:self.args.map(|[a0,a1,a2,a3,a4,a5,a6,a7,a8]| [
                a0.into_iter().map(|t| token(t)).collect(),
                a1.into_iter().map(|t| token(t)).collect(),
                a2.into_iter().map(|t| token(t)).collect(),
                a3.into_iter().map(|t| token(t)).collect(),
                a4.into_iter().map(|t| token(t)).collect(),
                a5.into_iter().map(|t| token(t)).collect(),
                a6.into_iter().map(|t| token(t)).collect(),
                a7.into_iter().map(|t| token(t)).collect(),
                a8.into_iter().map(|t| token(t)).collect()
            ]),
            start_ref:self.start_ref,
            vecs:self.vecs.into_iter().map(|v| v.into_iter().map(|t| token(t)).collect()).collect()
        }
    }
    fn with_list<Fn:FnOnce(&mut Vec<ET::Token>)>(&mut self,f:Fn) {
        match self.inputs.last_mut() {
            Some(TokenSource::Vec(v)) => f(v),
            _ => {
                let v = match self.vecs.pop() {
                    Some(mut v) => {f(&mut v);v}
                    None => {
                        let mut v = Vec::new();
                        f(&mut v);
                        v
                    }
                };
                self.inputs.push(TokenSource::Vec(v));
            }
        };
    }
    fn end_file(&mut self,aux:&mut EngineAux<ET>,state:&ET::State) -> ET::Token {
        match self.inputs.pop() {
            Some(TokenSource::File(f,_)) => {
                self.start_ref.pop();
                aux.outputs.file_close(f.source.path().display());
            }
            Some(TokenSource::String(_)) => (),//aux.outputs.file_close(""),
            _ => unreachable!()
        };
        let everyeof = state.get_primitive_tokens(PRIMITIVES.everyeof);
        if everyeof.is_empty() {
            ET::Token::eof()
        } else {
            self.requeue(ET::Token::eof());
            self.push_exp(everyeof);
            if let Some(TokenSource::Vec(v)) = self.inputs.last_mut() {
                v.pop().unwrap()
            } else { unreachable!() }
        }
    }
}

impl<ET:EngineTypes> EngineReferences<'_,ET> {
    /// Push a file to the [`Mouth`] (see [`Mouth::push_file`]).
    pub fn push_file(&mut self,f:ET::File) {
        self.mouth.push_file(f);
    }
    /// Insert the value of a primitive token list (e.g. `\everypar`) into the [`Mouth`].
    pub fn push_every(&mut self, every:PrimitiveIdentifier) {
        let tks = self.state.get_primitive_tokens(every);
        self.mouth.push_exp(tks);
    }
    /// Useful for debugging (see [`Mouth::preview`]).
    pub fn preview(&self) -> String {
        self.mouth.preview(self.aux.memory.cs_interner(),self.state.get_catcode_scheme(),self.state.get_escape_char())
    }
}