use crate::{debug_log, throw};
use crate::engine::EngineType;
use crate::engine::memory::{Interner, Memory};
use crate::engine::state::modes::{BoxMode, GroupType, TeXMode};
use crate::engine::state::State;
use crate::tex::catcodes::{CategoryCode, CategoryCodeScheme};
use crate::tex::commands::Command;
use crate::tex::nodes::HVBox;
use crate::tex::numbers::{MuSkip, Skip};
use crate::tex::token::Token;
use crate::utils::collections::HMap;
use crate::utils::strings::{CharType, TeXStr};
use crate::engine::TeXError;
use crate::utils::strings::AllCharsTrait;
use crate::tex::numbers::*;
use crate::engine::filesystem::*;
use string_interner::Symbol;

#[derive(Clone)]
pub struct ChangeBasedState<ET:EngineType> {
    out_files:Vec<Option<ET::File>>,
    in_files:Vec<Option<ET::File>>,
    csnames:usize,
    afterassignment:Option<ET::Token>,
    mode: TeXMode,

    stack:Vec<StackLevel<ET>>,
    changes:Vec<StateChange<ET>>,

    current_font:ET::FontRef,
    parshape:Option<Vec<(ET::Dim,ET::Dim)>>,
    endlinechar: Option<ET::Char>,
    escapechar: Option<ET::Char>,
    newlinechar: Option<ET::Char>,
    pub commands: Vec<Option<Command<ET>>>,
    ac_commands: <ET::Char as CharType>::Allchars<Option<Command<ET>>>,
    catcodes: <ET::Char as CharType>::Allchars<CategoryCode>,
    sfcodes: <ET::Char as CharType>::Allchars<ET::Int>,
    uccodes: <ET::Char as CharType>::Allchars<ET::Char>,
    lccodes: <ET::Char as CharType>::Allchars<ET::Char>,
    mathcodes: <ET::Char as CharType>::Allchars<ET::Int>,
    delcodes: <ET::Char as CharType>::Allchars<ET::Int>,
    intregisters: Vec<ET::Int>,
    dimregisters: Vec<ET::Dim>,
    skipregisters: Vec<Skip<ET::SkipDim>>,
    muskipregisters: Vec<MuSkip<ET::MuDim,ET::MuStretchShrinkDim>>,
    toksregisters:Vec<Vec<ET::Token>>,
    boxregisters:Vec<HVBox<ET>>,
    textfonts:Vec<ET::FontRef>,
    scriptfonts:Vec<ET::FontRef>,
    scriptscriptfonts:Vec<ET::FontRef>,
    primitive_intregisters: HMap<&'static str,ET::Int>,
    primitive_dimregisters: HMap<&'static str,ET::Dim>,
    primitive_skipregisters: HMap<&'static str,Skip<ET::SkipDim>>,
    primitive_muskipregisters: HMap<&'static str,MuSkip<ET::MuDim,ET::MuStretchShrinkDim>>,
    primitive_tokregisters: HMap<&'static str,Vec<ET::Token>>,
}

impl<ET:EngineType> ChangeBasedState<ET> {
    pub fn new(fontstore:&ET::FontStore) -> Self {
        let mut state = Self {
            out_files:vec!(),
            in_files:vec!(),
            csnames:0,
            current_font:ET::FontRef::default(),
            afterassignment:None,
            mode: TeXMode::Vertical,

            stack: vec![],
            changes: vec![],

            parshape:None,
            endlinechar: Some(ET::Char::carriage_return()),
            escapechar: Some(ET::Char::backslash()),
            newlinechar: Some(ET::Char::newline()),
            commands: vec!(),
            ac_commands: ET::Char::rep_field(None),
            catcodes: ET::Char::starting_catcode_scheme(),
            sfcodes: ET::Char::rep_field(ET::Int::default()),
            uccodes: ET::Char::ident(),
            lccodes: ET::Char::ident(),
            mathcodes: ET::Char::rep_field(ET::Int::default()),
            delcodes: ET::Char::rep_field(ET::Int::default()),
            intregisters: vec!(),
            dimregisters: vec!(),
            skipregisters: vec!(),
            muskipregisters: vec!(),
            toksregisters: vec!(),
            boxregisters: vec!(),
            textfonts:vec!(),
            scriptfonts:vec!(),
            scriptscriptfonts:vec!(),

            primitive_intregisters: HMap::default(),
            primitive_dimregisters: HMap::default(),
            primitive_skipregisters: HMap::default(),
            primitive_muskipregisters: HMap::default(),
            primitive_tokregisters: HMap::default(),
        };
        for i in 97u8..123u8 {
            state.uccodes.set(i.into(), (i-32).into());
            state.lccodes.set((i-32).into(), i.into());
            state.mathcodes.set((i-32).into(),
                                ET::Int::from_i64((i as i64-32) +
                                    (1 * 16 * 16) +
                                    (7 * 16 * 16 * 16))
            );
            state.mathcodes.set(i.into(),
                                ET::Int::from_i64((i as i64) +
                                    (1 * 16 * 16) +
                                    (7 * 16 * 16 * 16))
            );
        }
        for i in 48u8..58u8 {
            state.mathcodes.set(i.into(),
                                ET::Int::from_i64((i as i64) +
                                    (0 * 16 * 16) +
                                    (7 * 16 * 16 * 16))
            );
        }
        state
    }
}

macro_rules! change {
    ($self:ident,$globally:ident,$change:expr) => {
        match $self.stack.last() {
            None => (),
            Some(ch) => {
                let globaldefs = $self.get_primitive_int("globaldefs");
                let globally = if globaldefs == ET::Int::default() {$globally} else {globaldefs.to_i64() > 0};
                if globally {
                    StackLevel::forget($change,&mut $self.changes,&mut $self.stack);
                } else {
                    ch.change($change,&mut $self.changes);
                }
            }
        }
    }
}

impl<ET:EngineType> State<ET> for ChangeBasedState<ET> {
    fn get_current_font(&self) -> ET::FontRef {
        self.current_font
    }
    fn set_current_font(&mut self, f:ET::FontRef, globally: bool) {
        let old = std::mem::replace(&mut self.current_font,f);
        change!(self,globally,StateChange::CurrentFont(old));
    }
    fn grouplevel(&self) -> usize {
        self.stack.len()
    }

    fn set_afterassignment(&mut self, t: ET::Token) {
        self.afterassignment = Some(t)
    }
    fn take_afterassignment(&mut self) -> Option<ET::Token> {
        self.afterassignment.take()
    }

    fn mode(&self) -> TeXMode { self.mode }
    fn push_csname(&mut self) -> usize {
        self.csnames += 1;
        self.csnames
    }
    fn current_csname(&self) -> Option<usize> {
        match self.csnames {
            0 => None,
            _ => Some(self.csnames)
        }
    }
    fn pop_csname(&mut self) {
        self.csnames -= 1;
    }
    fn file_openin(&mut self, i: usize, f: ET::File,interner:&mut Interner) {
        if i >= self.in_files.len() {
            self.in_files.resize(i+1,None);
        }
        f.open_in(interner);
        self.in_files[i] = Some(f);
    }
    fn file_closein(&mut self, i: usize) {
        if i >= self.in_files.len() {
            todo!("Error closeout")
            //self.out_files.resize(i,None);
        }
        if let Some(f) = &self.in_files[i] {
            f.close_in();
        }
        self.in_files[i] = None
    }
    fn file_openout(&mut self, i: usize, f: ET::File) {
        f.open_out();
        if i >= self.out_files.len() {
            self.out_files.resize(i+1,None);
        }
        self.out_files[i] = Some(f);
    }
    fn file_closeout(&mut self, i: usize) {
        if i >= self.out_files.len() {
            todo!("Error closeout")
            //self.out_files.resize(i,None);
        }
        if let Some(f) = &self.out_files[i] {
            f.close_out();
        }
        self.out_files[i] = None
    }
    fn get_open_out_file(&self,i:usize) -> Option<ET::File> {
        if i >= self.out_files.len() {
            None
        } else {
            self.out_files[i].clone()
        }
    }
    fn get_open_in_file(&self,i:usize) -> Option<ET::File> {
        if i >= self.in_files.len() {
            None
        } else {
            self.in_files[i].clone()
        }
    }

    // #[inline(always)]
    fn stack_push(&mut self, g: GroupType) {
        match g {
            GroupType::Box(m) => {
                self.stack.push(StackLevel::new(g,Some(self.mode),self.changes.len()));
                self.mode = match m {
                    BoxMode::H | BoxMode::LeftRight => TeXMode::RestrictedHorizontal,
                    BoxMode::M => TeXMode::Math,
                    BoxMode::DM => TeXMode::Displaymath,
                    BoxMode::V => TeXMode::InternalVertical,
                    _ => self.mode
                };
            },
            _ => self.stack.push(StackLevel::new(g,None,self.changes.len()))
        }
    }
    fn stack_pop(&mut self,memory:&mut Memory<ET>) -> Option<(Vec<ET::Token>,GroupType)> {
        match self.stack.pop() {
            None => throw!("No group here to end"),
            Some(StackLevel{group_type,mode_switch,aftergroup,start}) => {
                if let Some(m) = mode_switch {
                    self.mode = m;
                }
                if start < self.changes.len() {
                    debug_log!(trace => "Rerolling {} changes (start: {} of {})",self.changes.len()-start,start,self.changes.len());
                    let changes = self.changes.drain(start..).rev();
                    for change in changes {
                        use StateChange::*;
                        match change {
                            CurrentFont(v) => self.current_font = v,
                            ParShape(v) => self.parshape = v,
                            Endlinechar(v) => self.endlinechar = v,
                            Escapechar(v) => self.escapechar = v,
                            Newlinechar(v) => self.newlinechar = v,
                            Command(name,v) => self.commands[name] = v,
                            AcCommand(c,v) => self.ac_commands.set(c,v),
                            Catcode(c,v) => self.catcodes.set(c,v),
                            SfCode(c,v) => self.sfcodes.set(c,v),
                            UCCode(c,v) => self.uccodes.set(c,v),
                            LCCode(c,v) => self.lccodes.set(c,v),
                            MathCode(c,v) => self.mathcodes.set(c,v),
                            DelCode(c,v) => self.delcodes.set(c,v),
                            IntRegister(i,v) => self.intregisters[i] = v,
                            DimRegister(i,v) => self.dimregisters[i] = v,
                            SkipRegister(i,v) => self.skipregisters[i] = v,
                            MuSkipRegister(i,v) => self.muskipregisters[i] = v,
                            ToksRegister(i,v) => {
                                let old = std::mem::replace(&mut self.toksregisters[i],v);
                                memory.return_token_vec(old)
                            },
                            BoxRegister(i,v) => self.boxregisters[i] = v,
                            TextFont(i,v) => self.textfonts[i] = v,
                            ScriptFont(i,v) => self.scriptfonts[i] = v,
                            ScriptScriptFont(i,v) => self.scriptscriptfonts[i] = v,
                            PrimitiveIntRegister(name,v) => {self.primitive_intregisters.insert(name,v);}
                            PrimitiveDimRegister(name,v) => {self.primitive_dimregisters.insert(name,v);}
                            PrimitiveSkipRegister(name,v) => {self.primitive_skipregisters.insert(name,v);}
                            PrimitiveMuSkipRegister(name,v) => {self.primitive_muskipregisters.insert(name,v);}
                            PrimitiveToksRegister(name,v) => {
                                if let Some(v) = self.primitive_tokregisters.insert(name,v) {
                                    memory.return_token_vec(v)
                                }
                            },
                        }
                    }
                }
                Some((aftergroup,group_type))
            }
        }
    }

    fn set_mode(&mut self, mode: TeXMode) {
        self.mode = mode
    }

    // #[inline(always)]
    fn get_grouptype(&self) -> GroupType {
        match self.stack.last() {
            None => GroupType::Top,
            Some(StackLevel { group_type, .. }) => *group_type
        }
    }

    fn get_parshape(&self) -> Option<&Vec<(ET::Dim, ET::Dim)>> {
        self.parshape.as_ref()
    }
    fn get_escapechar(&self) -> Option<ET::Char> { self.escapechar }
    fn get_endlinechar(&self) -> Option<ET::Char> { self.endlinechar }
    fn get_newlinechar(&self) -> Option<ET::Char> { self.newlinechar }
    fn get_sfcode(&self, c: ET::Char) -> ET::Int { *self.sfcodes.get(c) }
    fn get_mathcode(&self, c: ET::Char) -> ET::Int { *self.mathcodes.get(c)}
    fn get_delcode(&self, c: ET::Char) -> ET::Int { *self.delcodes.get(c)}
    fn get_command(&self, name: TeXStr) -> Option<&Command<ET>> {
        match self.commands.get(name.0.to_usize()) {
            Some(r) => r.as_ref(),
            _ => None
        }
    }
    fn get_ac_command(&self, c: ET::Char) -> Option<&Command<ET>> { self.ac_commands.get(c).as_ref()}
    fn get_catcode_scheme(&self) -> &CategoryCodeScheme<ET::Char> { &self.catcodes }
    fn get_uccode(&self, c: ET::Char) -> ET::Char { *self.uccodes.get(c) }
    fn get_lccode(&self, c: ET::Char) -> ET::Char { *self.lccodes.get(c) }
    fn get_int_register(&self, i: usize) -> ET::Int {
        *self.intregisters.get(i).unwrap_or(&ET::Int::default())
    }
    fn get_dim_register(&self, i: usize) -> ET::Dim {
        *self.dimregisters.get(i).unwrap_or(&ET::Dim::default())
    }
    fn get_skip_register(&self, i: usize) -> Skip<ET::SkipDim> {
        *self.skipregisters.get(i).unwrap_or(&Skip::default())
    }
    fn get_muskip_register(&self, i: usize) -> MuSkip<ET::MuDim,ET::MuStretchShrinkDim> {
        *self.muskipregisters.get(i).unwrap_or(&MuSkip::default())
    }
    fn get_primitive_int(&self, name: &'static str) -> ET::Int {
        *self.primitive_intregisters.get(&name).unwrap_or(&ET::Int::default())
    }
    fn get_toks_register(&self, i: usize) -> Option<&Vec<ET::Token>> { self.toksregisters.get(i) }
    fn get_box_register(&mut self, i: usize) -> Option<&mut HVBox<ET>> {
        self.boxregisters.get_mut(i)
    }
    fn get_primitive_dim(&self, name: &'static str) -> ET::Dim {
        *self.primitive_dimregisters.get(&name).unwrap_or(&ET::Dim::default())
    }
    fn get_primitive_skip(&self, name: &'static str) -> Skip<ET::SkipDim> {
        *self.primitive_skipregisters.get(&name).unwrap_or(&Skip::default())
    }
    fn get_primitive_muskip(&self, name: &'static str) -> MuSkip<ET::MuDim,ET::MuStretchShrinkDim> {
        *self.primitive_muskipregisters.get(&name).unwrap_or(&MuSkip::default())
    }
    fn get_primitive_toks(&self, name: &'static str) -> Option<&Vec<ET::Token>> { self.primitive_tokregisters.get(&name) }

    fn get_textfont(&self, i: usize) -> ET::FontRef {
        *self.textfonts.get(i).unwrap_or(&ET::FontRef::default())
    }
    fn get_scriptfont(&self, i: usize) -> ET::FontRef {
        *self.scriptfonts.get(i).unwrap_or(&ET::FontRef::default())
    }
    fn get_scriptscriptfont(&self, i: usize) -> ET::FontRef {
        *self.scriptscriptfonts.get(i).unwrap_or(&ET::FontRef::default())
    }

    fn take_box_register(&mut self, i: usize) -> HVBox<ET> {
        match self.boxregisters.get_mut(i) {
            Some(b) => std::mem::replace(b,HVBox::Void),
            None => HVBox::Void
        }
    }


    fn set_parshape(&mut self, v: Vec<(ET::Dim, ET::Dim)>, globally: bool) {
        let v = std::mem::replace(&mut self.parshape,Some(v));
        change!(self,globally,StateChange::ParShape(v));
    }
    fn set_escapechar(&mut self, c: Option<ET::Char>, globally: bool) {
        let c = std::mem::replace(&mut self.escapechar,c);
        change!(self,globally,StateChange::Escapechar(c));
    }
    fn set_endlinechar(&mut self, c: Option<ET::Char>, globally: bool) {
        let c = std::mem::replace(&mut self.endlinechar,c);
        change!(self,globally,StateChange::Endlinechar(c));
    }
    fn set_newlinechar(&mut self, c: Option<ET::Char>, globally: bool) {
        let c = std::mem::replace(&mut self.newlinechar,c);
        change!(self,globally,StateChange::Newlinechar(c));
    }
    fn set_sfcode(&mut self, c: ET::Char, v: ET::Int, globally: bool) {
        let v = self.sfcodes.replace(c,v);
        change!(self,globally,StateChange::SfCode(c,v));
    }
    fn set_mathcode(&mut self, c: ET::Char, mc: ET::Int, globally: bool) {
        let v = self.mathcodes.replace(c,mc);
        change!(self,globally,StateChange::MathCode(c,v));
    }
    fn set_delcode(&mut self, c: ET::Char, lc: ET::Int, globally: bool) {
        let v = self.delcodes.replace(c,lc);
        change!(self,globally,StateChange::DelCode(c,v));
    }
    fn set_command(&mut self, name: TeXStr, cmd: Option<Command<ET>>, globally: bool) {
        let index = name.0.to_usize();
        if self.commands.len() <= index {
            self.commands.resize(index+1,None);
        }
        let old = std::mem::replace(&mut self.commands[index],cmd);
        change!(self,globally,StateChange::Command(index,old));
    }
    fn set_ac_command(&mut self, c: ET::Char, cmd: Option<Command<ET>>, globally: bool) {
        let v = self.ac_commands.replace(c,cmd);
        change!(self,globally,StateChange::AcCommand(c,v));
    }
    fn set_catcode(&mut self, c: ET::Char, cc: CategoryCode, globally: bool) {
        let v = self.catcodes.replace(c,cc);
        change!(self,globally,StateChange::Catcode(c,v));
    }
    fn set_uccode(&mut self, c: ET::Char, uc: ET::Char, globally: bool) {
        let v = self.uccodes.replace(c, uc);
        change!(self,globally,StateChange::UCCode(c,v));
    }
    fn set_lccode(&mut self, c: ET::Char, lc: ET::Char, globally: bool) {
        let v = self.lccodes.replace(c, lc);
        change!(self,globally,StateChange::LCCode(c,v));
    }
    fn set_int_register(&mut self, i: usize, v: ET::Int, globally: bool) {
        if self.intregisters.len() <= i {
            self.intregisters.resize(i+1,ET::Int::default());
        }
        let old = std::mem::replace(&mut self.intregisters[i],v);
        change!(self,globally,StateChange::IntRegister(i,old));
    }
    fn set_dim_register(&mut self, i: usize, v: ET::Dim, globally: bool) {
        if self.dimregisters.len() <= i {
            self.dimregisters.resize(i+1,ET::Dim::default());
        }
        let old = std::mem::replace(&mut self.dimregisters[i],v);
        change!(self,globally,StateChange::DimRegister(i,old));
    }
    fn set_skip_register(&mut self, i: usize, v: Skip<ET::SkipDim>, globally: bool) {
        if self.skipregisters.len() <= i {
            self.skipregisters.resize(i+1,Skip::default());
        }
        let old = std::mem::replace(&mut self.skipregisters[i],v);
        change!(self,globally,StateChange::SkipRegister(i,old));
    }
    fn set_muskip_register(&mut self, i: usize, v: MuSkip<ET::MuDim,ET::MuStretchShrinkDim>, globally: bool) {
        if self.muskipregisters.len() <= i {
            self.muskipregisters.resize(i+1,MuSkip::default());
        }
        let old = std::mem::replace(&mut self.muskipregisters[i],v);
        change!(self,globally,StateChange::MuSkipRegister(i,old));
    }
    fn set_box_register(&mut self, i: usize, v: HVBox<ET>, globally: bool) {
        if self.boxregisters.len() <= i {
            self.boxregisters.resize(i+1,HVBox::Void);
        }
        let old = std::mem::replace(&mut self.boxregisters[i],v);
        change!(self,globally,StateChange::BoxRegister(i,old));
    }
    fn set_primitive_int(&mut self, name: &'static str, v: ET::Int, globally: bool) {
        let old = self.primitive_intregisters.insert(name,v).unwrap_or(ET::Int::default());
        change!(self,globally,StateChange::PrimitiveIntRegister(name,old));
    }
    fn set_primitive_dim(&mut self, name: &'static str, v: ET::Dim, globally: bool) {
        let old = self.primitive_dimregisters.insert(name,v).unwrap_or(ET::Dim::default());
        change!(self,globally,StateChange::PrimitiveDimRegister(name,old));
    }
    fn set_primitive_skip(&mut self, name: &'static str, v: Skip<ET::SkipDim>, globally: bool) {
        let old = self.primitive_skipregisters.insert(name,v).unwrap_or(Skip::default());
        change!(self,globally,StateChange::PrimitiveSkipRegister(name,old));
    }
    fn set_primitive_muskip(&mut self, name: &'static str, v: MuSkip<ET::MuDim,ET::MuStretchShrinkDim>, globally: bool) {
        let old = self.primitive_muskipregisters.insert(name,v).unwrap_or(MuSkip::default());
        change!(self,globally,StateChange::PrimitiveMuSkipRegister(name,old));
    }
    fn set_textfont(&mut self, i: usize, f: ET::FontRef, globally: bool) {
        if self.textfonts.len() <= i {
            self.textfonts.resize(i+1,ET::FontRef::default());
        }
        let old = std::mem::replace(&mut self.textfonts[i],f);
        change!(self,globally,StateChange::TextFont(i,old));
    }
    fn set_scriptfont(&mut self, i: usize, f: ET::FontRef, globally: bool) {
        if self.scriptfonts.len() <= i {
            self.scriptfonts.resize(i+1,ET::FontRef::default());
        }
        let old = std::mem::replace(&mut self.scriptfonts[i],f);
        change!(self,globally,StateChange::ScriptFont(i,old));
    }
    fn set_scriptscriptfont(&mut self, i: usize, f: ET::FontRef, globally: bool) {
        if self.scriptscriptfonts.len() <= i {
            self.scriptscriptfonts.resize(i+1,ET::FontRef::default());
        }
        let old = std::mem::replace(&mut self.scriptscriptfonts[i],f);
        change!(self,globally,StateChange::ScriptScriptFont(i,old));
    }

    fn set_toks_register(&mut self, i: usize, v: Vec<ET::Token>, globally: bool,memory:&mut Memory<ET>) {
        if self.toksregisters.len() <= i {
            self.toksregisters.resize(i+1,memory.get_token_vec());
        }
        let old = std::mem::replace(&mut self.toksregisters[i],v);
        match self.stack.last() {
            None => memory.return_token_vec(old),
            Some(ch) => {
                let globaldefs = self.get_primitive_int("globaldefs");
                let globally = if globaldefs == ET::Int::default() {globally} else {globaldefs.to_i64() > 0};
                if globally {
                    let mut idx = 0;
                    let mut j = 0;
                    while let Some(e) = self.changes.get(idx) {
                        match e {
                            StateChange::ToksRegister(i2,_) if *i2 == i => {
                                match self.changes.remove(idx) {
                                    StateChange::ToksRegister(_,v) => {
                                        memory.return_token_vec(v);
                                        for s in &mut self.stack[j..] {
                                            if s.start > idx {
                                                s.start -= 1;
                                            } else {
                                                j += 1;
                                            }
                                        }
                                    }
                                    _ => unreachable!()
                                }
                            }
                            _ => idx += 1
                        }
                    }
                } else {
                    match ch.change(StateChange::ToksRegister(i,old),&mut self.changes) {
                        Some(StateChange::ToksRegister(_,v)) => memory.return_token_vec(v),
                        None => (),
                        _ => unreachable!()
                    }
                }
            }
        }
    }
    fn set_primitive_toks(&mut self, name: &'static str, v: Vec<ET::Token>, globally: bool,memory:&mut Memory<ET>) {
        let old = self.primitive_tokregisters.insert(name,v);
        match self.stack.last() {
            None => if let Some(old) = old {memory.return_token_vec(old)},
            Some(ch) => {
                let globaldefs = self.get_primitive_int("globaldefs");
                let globally = if globaldefs == ET::Int::default() {globally} else {globaldefs.to_i64() > 0};
                if globally {
                    let mut i = 0;
                    let mut j = 0;
                    while let Some(e) = self.changes.get(i) {
                        match e {
                            StateChange::PrimitiveToksRegister(nname,_) if *nname == name => {
                                match self.changes.remove(i) {
                                    StateChange::PrimitiveToksRegister(_,v) => {
                                        memory.return_token_vec(v);
                                        for s in &mut self.stack[j..] {
                                            if s.start > i {
                                                s.start -= 1;
                                            } else {
                                                j += 1;
                                            }
                                        }
                                    }
                                    _ => unreachable!()
                                }
                            }
                            _ => i += 1
                        }
                    }
                } else {
                    match ch.change(StateChange::PrimitiveToksRegister(name,old.unwrap_or(memory.get_token_vec())),&mut self.changes) {
                        Some(StateChange::PrimitiveToksRegister(_,v)) => memory.return_token_vec(v),
                        None => (),
                        _ => unreachable!()
                    }
                }
            }
        }
    }

    fn push_aftergroup(&mut self, t: ET::Token) {
        match self.stack.last_mut() {
            Some(StackLevel{aftergroup,..}) => {
                aftergroup.push(t)
            },
            _ => ()
        }
    }

}

#[derive(Clone)]
enum StateChange<ET:EngineType> {
    CurrentFont(ET::FontRef),
    ParShape(Option<Vec<(ET::Dim, ET::Dim)>>),
    Endlinechar(Option<ET::Char>),
    Escapechar(Option<ET::Char>),
    Newlinechar(Option<ET::Char>),
    Command(usize,Option<Command<ET>>),
    AcCommand(ET::Char,Option<Command<ET>>),
    Catcode(ET::Char,CategoryCode),
    SfCode(ET::Char,ET::Int),
    UCCode(ET::Char, ET::Char),
    LCCode(ET::Char, ET::Char),
    MathCode(ET::Char,ET::Int),
    DelCode(ET::Char,ET::Int),
    IntRegister(usize,ET::Int),
    DimRegister(usize,ET::Dim),
    SkipRegister(usize,Skip<ET::SkipDim>),
    MuSkipRegister(usize,MuSkip<ET::MuDim,ET::MuStretchShrinkDim>),
    ToksRegister(usize,Vec<ET::Token>),
    BoxRegister(usize,HVBox<ET>),
    TextFont(usize,ET::FontRef),
    ScriptFont(usize,ET::FontRef),
    ScriptScriptFont(usize,ET::FontRef),
    PrimitiveIntRegister(&'static str,ET::Int),
    PrimitiveDimRegister(&'static str,ET::Dim),
    PrimitiveSkipRegister(&'static str,Skip<ET::SkipDim>),
    PrimitiveMuSkipRegister(&'static str,MuSkip<ET::MuDim,ET::MuStretchShrinkDim>),
    PrimitiveToksRegister(&'static str,Vec<ET::Token>)
}

impl<ET:EngineType> StateChange<ET> {
    fn equiv(&self,other:&Self) -> bool {
        use StateChange::*;
        match (self,other) {
            (CurrentFont(_),CurrentFont(_)) => true,
            (ParShape(_),ParShape(_)) => true,
            (Endlinechar(_),Endlinechar(_)) => true,
            (Escapechar(_),Escapechar(_)) => true,
            (Newlinechar(_),Newlinechar(_)) => true,
            (Command(i,_),Command(j,_)) => i == j,
            (AcCommand(c,_),AcCommand(d,_)) => c == d,
            (Catcode(c,_),Catcode(d,_)) => c == d,
            (SfCode(c,_),SfCode(d,_)) => c == d,
            (UCCode(c, _), UCCode(d, _)) => c == d,
            (LCCode(c, _), LCCode(d, _)) => c == d,
            (MathCode(c,_),MathCode(d,_)) => c == d,
            (DelCode(c,_),DelCode(d,_)) => c == d,
            (IntRegister(i,_),IntRegister(j,_)) => i == j,
            (DimRegister(i,_),DimRegister(j,_)) => i == j,
            (SkipRegister(i,_),SkipRegister(j,_)) => i == j,
            (MuSkipRegister(i,_),MuSkipRegister(j,_)) => i == j,
            (ToksRegister(i,_),ToksRegister(j,_)) => i == j,
            (BoxRegister(i,_),BoxRegister(j,_)) => i == j,
            (TextFont(i,_),TextFont(j,_)) => i == j,
            (ScriptFont(i,_),ScriptFont(j,_)) => i == j,
            (ScriptScriptFont(i,_),ScriptScriptFont(j,_)) => i == j,
            (PrimitiveIntRegister(n,_),PrimitiveIntRegister(m,_)) => n == m,
            (PrimitiveDimRegister(n,_),PrimitiveDimRegister(m,_)) => n == m,
            (PrimitiveSkipRegister(n,_),PrimitiveSkipRegister(m,_)) => n == m,
            (PrimitiveMuSkipRegister(n,_),PrimitiveMuSkipRegister(m,_)) => n == m,
            (PrimitiveToksRegister(n,_),PrimitiveToksRegister(m,_)) => n == m,
            _ => false
        }
    }
}

#[derive(Clone)]
struct StackLevel<ET:EngineType> {
    group_type:GroupType,
    mode_switch:Option<TeXMode>,
    aftergroup:Vec<ET::Token>,
    start:usize,
}
impl<ET:EngineType> StackLevel<ET> {
    fn new(group_type:GroupType,old_mode:Option<TeXMode>,start:usize) -> Self {
        Self {
            group_type,
            mode_switch:old_mode,
            aftergroup:Vec::new(),
            start
        }
    }
    fn change(&self,change:StateChange<ET>,ls:&mut Vec<StateChange<ET>>) -> Option<StateChange<ET>> {
        if self.start >= ls.len() {
            ls.push(change);return None
        }
        let slice = &ls[self.start..];
        if slice.iter().any(|c| change.equiv(c)) {
            return Some(change)
        }
        ls.push(change);None
    }
    fn forget(change:StateChange<ET>,ls:&mut Vec<StateChange<ET>>,stack:&mut Vec<Self>) {
        let mut i = 0;
        let mut j = 0;
        while let Some(e) = ls.get(i) {
            if e.equiv(&change) {
                ls.remove(i);
                for s in &mut stack[j..] {
                    if s.start > i {
                        s.start -= 1;
                    } else {
                        j += 1;
                    }
                }
            } else {
                i += 1;
            }
        }
    }
}