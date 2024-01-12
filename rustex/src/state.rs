use tex_engine::commands::Command;
use tex_engine::engine::{EngineAux, EngineTypes, state};
use tex_engine::engine::state::{State, StateChangeTracker, StateStack};
use tex_engine::engine::utils::memory::PrimitiveIdentifier;
use tex_engine::tex::catcodes::{CategoryCode, CategoryCodeScheme};
use tex_engine::tex::numerics::{Dim32, MuSkip32, Skip32};
use tex_engine::tex::tokens::CompactToken;
use tex_engine::tex::types::{GroupType, MathStyle, TeXMode};
use crate::engine::{CSName, Font, Types};
use crate::stomach::CLOSE_FONT;
use tex_engine::tex::tokens::Token;
use tex_engine::engine::utils::memory::MemoryManager;
use tex_engine::engine::mouth::Mouth;
use tex_engine::tex::nodes::boxes::TeXBox;
use tex_engine::prelude::*;

#[derive(Clone)]
pub struct RusTeXState(state::tex_state::TeXState<Types>);
impl StateChangeTracker<state::tex_state::TeXState<Types>> for RusTeXState {

    #[inline(always)]
    fn stack(&mut self) -> &mut StateStack<Types,state::tex_state::TeXState<Types>> { self.0.stack() }
}
impl State for RusTeXState {
    type ET = Types;
    #[inline(always)]
    fn new(nullfont: Font, aux: &mut EngineAux<Types>) -> Self {
        Self(state::tex_state::TeXState::new(nullfont, aux))
    }

    #[inline(always)]
    fn aftergroup(&mut self, token: CompactToken) { self.0.aftergroup(token) }

    #[inline(always)]
    fn push(&mut self, aux: &mut EngineAux<Self::ET>, group_type: GroupType, line_number: usize) {
        self.0.push(aux,group_type,line_number);
        aux.extension.push();
    }

    #[inline(always)]
    fn pop(&mut self, aux: &mut EngineAux<Self::ET>, mouth: &mut <Self::ET as EngineTypes>::Mouth) {
        self.0.pop(aux,mouth);
        let closefont = CompactToken::from_cs(
            aux.memory.cs_interner_mut().new(CLOSE_FONT)
        );
        for _ in aux.extension.pop() {
            mouth.requeue(closefont);
        }
    }

    #[inline(always)]
    fn get_group_type(&self) -> Option<GroupType> {
        self.0.get_group_type()
    }

    #[inline(always)]
    fn get_group_level(&self) -> usize {
        self.0.get_group_level()
    }

    #[inline(always)]
    fn get_current_font(&self) -> &Font {
        self.0.get_current_font()
    }

    #[inline(always)]
    fn get_textfont(&self, i: usize) -> &<Self::ET as EngineTypes>::Font {
        self.0.get_textfont(i)
    }
    #[inline(always)]
    fn set_textfont(&mut self, aux: &mut EngineAux<Self::ET>, idx: usize, fnt: <Self::ET as EngineTypes>::Font, globally: bool) {
        self.0.set_textfont(aux,idx,fnt,globally)
    }
    #[inline(always)]
    fn get_scriptfont(&self, i: usize) -> &<Self::ET as EngineTypes>::Font {
        self.0.get_scriptfont(i)
    }
    #[inline(always)]
    fn set_scriptfont(&mut self, aux: &mut EngineAux<Self::ET>, idx: usize, fnt: <Self::ET as EngineTypes>::Font, globally: bool) {
        self.0.set_scriptfont(aux,idx,fnt,globally)
    }
    #[inline(always)]
    fn get_scriptscriptfont(&self, i: usize) -> &<Self::ET as EngineTypes>::Font {
        self.0.get_scriptscriptfont(i)
    }
    #[inline(always)]
    fn set_scriptscriptfont(&mut self, aux: &mut EngineAux<Self::ET>, idx: usize, fnt: <Self::ET as EngineTypes>::Font, globally: bool) {
        self.0.set_scriptscriptfont(aux,idx,fnt,globally)
    }

    #[inline(always)]
    fn set_current_font(&mut self, aux: &mut EngineAux<Types>, fnt: Font, globally: bool) {
        self.0.set_current_font(aux,fnt,globally)
    }

    #[inline(always)]
    fn get_catcode_scheme(&self) -> &CategoryCodeScheme<u8> {
        self.0.get_catcode_scheme()
    }

    #[inline(always)]
    fn set_catcode(&mut self, aux: &EngineAux<Types>, c: u8, cc: CategoryCode, globally: bool) {
        self.0.set_catcode(aux,c,cc,globally)
    }

    #[inline(always)]
    fn get_sfcode(&self, c: u8) -> u16 {
        self.0.get_sfcode(c)
    }

    #[inline(always)]
    fn set_sfcode(&mut self, aux: &EngineAux<Types>, c: u8, sfcode: u16, globally: bool) {
        self.0.set_sfcode(aux,c,sfcode,globally)
    }

    #[inline(always)]
    fn get_lccode(&self, c: u8) -> u8 {
        self.0.get_lccode(c)
    }

    #[inline(always)]
    fn set_lccode(&mut self, aux: &EngineAux<Types>, c: u8, lccode: u8, globally: bool) {
        self.0.set_lccode(aux,c,lccode,globally)
    }

    #[inline(always)]
    fn get_uccode(&self, c: u8) -> u8 {
        self.0.get_uccode(c)
    }

    #[inline(always)]
    fn set_uccode(&mut self, aux: &EngineAux<Types>, c: u8, uccode: u8, globally: bool) {
        self.0.set_uccode(aux,c,uccode,globally)
    }

    #[inline(always)]
    fn get_delcode(&self, c: u8) -> i32 {
        self.0.get_delcode(c)
    }

    #[inline(always)]
    fn set_delcode(&mut self, aux: &EngineAux<Types>, c: u8, delcode: i32, globally: bool) {
        self.0.set_delcode(aux,c,delcode,globally)
    }
    #[inline(always)]

    fn get_mathcode(&self, c: u8) -> u32 {
        self.0.get_mathcode(c)
    }

    #[inline(always)]
    fn set_mathcode(&mut self, aux: &EngineAux<Types>, c: u8, mathcode: u32, globally: bool) {
        self.0.set_mathcode(aux,c,mathcode,globally)
    }


    #[inline(always)]
    fn get_endline_char(&self) -> Option<u8> { self.0.get_endline_char() }

    #[inline(always)]
    fn set_endline_char(&mut self, aux: &EngineAux<Types>, c: Option<u8>, globally: bool) {
        self.0.set_endline_char(aux,c,globally)
    }

    #[inline(always)]
    fn get_escape_char(&self) -> Option<u8> { self.0.get_escape_char() }

    #[inline(always)]
    fn set_escape_char(&mut self, aux: &EngineAux<Types>, c: Option<u8>, globally: bool) {
        self.0.set_escape_char(aux,c,globally)
    }

    #[inline(always)]
    fn get_newline_char(&self) -> Option<u8> { self.0.get_newline_char() }

    #[inline(always)]
    fn set_newline_char(&mut self, aux: &EngineAux<Types>, c: Option<u8>, globally: bool) {
        self.0.set_newline_char(aux,c,globally)
    }

    #[inline(always)]
    fn get_parshape(&self) -> &Vec<(Dim32, Dim32)> { self.0.get_parshape() }

    #[inline(always)]
    fn take_parshape(&mut self) -> Vec<(Dim32, Dim32)> { self.0.take_parshape() }

    #[inline(always)]
    fn set_parshape(&mut self, aux: &EngineAux<Types>, parshape: Vec<(Dim32, Dim32)>, globally: bool) {
        self.0.set_parshape(aux,parshape,globally)
    }

    #[inline(always)]
    fn get_int_register(&self, idx: u16) -> i32 {
        self.0.get_int_register(idx)
    }

    #[inline(always)]
    fn set_int_register(&mut self, aux: &EngineAux<Types>, idx: u16, v: i32, globally: bool) {
        self.0.set_int_register(aux,idx,v,globally)
    }

    #[inline(always)]
    fn get_primitive_int(&self, name: PrimitiveIdentifier) -> i32 {
        self.0.get_primitive_int(name)
    }

    #[inline(always)]
    fn set_primitive_int(&mut self, aux: &EngineAux<Types>, name: PrimitiveIdentifier, v: i32, globally: bool) {
        self.0.set_primitive_int(aux,name,v,globally)
    }

    #[inline(always)]
    fn get_dim_register(&self, idx: u16) -> Dim32 {
        self.0.get_dim_register(idx)
    }

    #[inline(always)]
    fn set_dim_register(&mut self, aux: &EngineAux<Types>, idx: u16, v: Dim32, globally: bool) {
        self.0.set_dim_register(aux,idx,v,globally)
    }

    #[inline(always)]
    fn get_skip_register(&self, idx: u16) -> Skip32<Dim32> {
        self.0.get_skip_register(idx)
    }

    #[inline(always)]
    fn set_skip_register(&mut self, aux: &EngineAux<Types>, idx: u16, v: Skip32<Dim32>, globally: bool) {
        self.0.set_skip_register(aux,idx,v,globally)
    }

    #[inline(always)]
    fn get_muskip_register(&self, idx: u16) -> MuSkip32 {
        self.0.get_muskip_register(idx)
    }

    #[inline(always)]
    fn set_muskip_register(&mut self, aux: &EngineAux<Types>, idx: u16, v: MuSkip32, globally: bool) {
        self.0.set_muskip_register(aux,idx,v,globally)
    }

    #[inline(always)]
    fn get_toks_register(&self, idx: u16) -> &TokenList<CompactToken> {
        self.0.get_toks_register(idx)
    }

    #[inline(always)]
    fn set_toks_register(&mut self, aux: &EngineAux<Self::ET>, idx: u16, v: TokenList<CompactToken>, globally: bool) {
        self.0.set_toks_register(aux,idx,v,globally)
    }

    #[inline(always)]
    fn get_box_register(&self, idx: u16) -> Option<&TeXBox<Types>> {
        self.0.get_box_register(idx)
    }

    #[inline(always)]
    fn get_box_register_mut(&mut self, idx: u16) -> Option<&mut TeXBox<Types>> {
        self.0.get_box_register_mut(idx)
    }

    #[inline(always)]
    fn take_box_register(&mut self, idx: u16) -> Option<TeXBox<Types>> {
        self.0.take_box_register(idx)
    }

    #[inline(always)]
    fn set_box_register(&mut self, aux: &EngineAux<Types>, idx: u16, v: Option<TeXBox<Types>>, globally: bool) {
        self.0.set_box_register(aux,idx,v,globally)
    }

    #[inline(always)]
    fn get_primitive_dim(&self, name: PrimitiveIdentifier) -> Dim32 {
        self.0.get_primitive_dim(name)
    }

    #[inline(always)]
    fn set_primitive_dim(&mut self, aux: &EngineAux<Types>, name: PrimitiveIdentifier, v: Dim32, globally: bool) {
        self.0.set_primitive_dim(aux,name,v,globally)
    }

    #[inline(always)]
    fn get_primitive_skip(&self, name: PrimitiveIdentifier) -> Skip32<Dim32> {
        self.0.get_primitive_skip(name)
    }

    #[inline(always)]
    fn set_primitive_skip(&mut self, aux: &EngineAux<Types>, name: PrimitiveIdentifier, v: Skip32<Dim32>, globally: bool) {
        self.0.set_primitive_skip(aux,name,v,globally)
    }

    #[inline(always)]
    fn get_primitive_muskip(&self, name: PrimitiveIdentifier) -> MuSkip32 {
        self.0.get_primitive_muskip(name)
    }

    #[inline(always)]
    fn set_primitive_muskip(&mut self, aux: &EngineAux<Types>, name: PrimitiveIdentifier, v: MuSkip32, globally: bool) {
        self.0.set_primitive_muskip(aux,name,v,globally)
    }

    #[inline(always)]
    fn get_primitive_tokens(&self, name: PrimitiveIdentifier) -> &TokenList<CompactToken> {
        self.0.get_primitive_tokens(name)
    }

    #[inline(always)]
    fn set_primitive_tokens(&mut self, aux: &EngineAux<Types>, name: PrimitiveIdentifier, v: TokenList<CompactToken>, globally: bool) {
        self.0.set_primitive_tokens(aux,name,v,globally)
    }

    #[inline(always)]
    fn get_command(&self, name: &CSName) -> Option<&Command<Types>> {
        self.0.get_command(name)
    }

    #[inline(always)]
    fn set_command(&mut self, aux: &EngineAux<Types>, name: CSName, cmd: Option<Command<Types>>, globally: bool) {
        self.0.set_command(aux,name,cmd,globally)
    }

    #[inline(always)]
    fn get_ac_command(&self, c: u8) -> Option<&Command<Types>> {
        self.0.get_ac_command(c)
    }

    #[inline(always)]
    fn set_ac_command(&mut self, aux: &EngineAux<Types>, c: u8, cmd: Option<Command<Types>>, globally: bool) {
        self.0.set_ac_command(aux,c,cmd,globally)
    }
}