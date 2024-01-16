/*! The [`State`] of a TeX engine keeps track of scoped (by TeX groups)
    values. */
pub mod tex_state;

use crate::engine::{EngineAux, EngineReferences, EngineTypes};
use crate::tex::catcodes::{CategoryCode, CategoryCodeScheme};
use crate::commands::{Command, PrimitiveCommand};
use crate::commands::primitives::{PrimitiveCommands, PrimitiveIdentifier, PRIMITIVES};
use crate::engine::gullet::methods::CSOrActiveChar;
use crate::tex::tokens::token_lists::TokenList;
use crate::tex::nodes::boxes::{BoxType, TeXBox};
use crate::tex::nodes::math::UnresolvedMathFontStyle;

/// The type of a group, e.g. `{...}`, `\begingroup...\endgroup`, `$...$`.
#[derive(Clone,Copy,Eq,PartialEq,Debug)]
pub enum GroupType {
    /// A group delimited by `{` and `}`.
    Character,
    /// A group delimited by `\begingroup` and `\endgroup`.
    ControlSequence,
    /// A box (e.g. `\hbox` or `\vbox`), or a math group.
    Box(BoxType),
    /// A math group delimited by `$` (inline) or `$$` (display).
    Math{display:bool},
    /// An inner math group delimited by `\left` and `\right`.
    LeftRight
}

/// A TeX state; holds all the different parameters, equivalents, registers etc.
/// The canoncial implementation is [`TeXState`](tex_state::TeXState).
///
/// Note that we do not require `ET:`[`EngineTypes`]`<`[`State`](EngineTypes::State)`=Self>` - this allows for
/// implementing your own State by just wrapping an existing implementation in a new wrapper struct and pass on functionality
/// to the inner State, which would otherwise
/// fail since `ET::State` would be the outer wrapper struct, not the inner one.
pub trait State<ET:EngineTypes>:Sized+Clone {
    /// Instantiate a new [`State`]
    fn new(nullfont:ET::Font,aux:&mut EngineAux<ET>) -> Self;
    /// convert the provided integer value into a legal index for a register (or None if out of range)
    fn register_index(i:ET::Int) -> Option<usize> {
        let idx: i64 = i.into();
        if idx < 0 || idx > u16::MAX.into() {
            None
        } else {
            Some(idx as usize)
        }
    }

    /// Set the current `\aftergroup` [`Token`]
    fn aftergroup(&mut self,token:ET::Token);

    /// Get the current set of mathfonts (text, script, scriptscript)
    /// for the given family number, where `fam` is 0..15
    /// TODO: due for an overhaul
    fn get_mathfonts(&self,fam:u8) -> UnresolvedMathFontStyle<ET> {
        UnresolvedMathFontStyle {
            text_font:self.get_textfont(fam).clone(),
            script_font:self.get_scriptfont(fam).clone(),
            script_script_font:self.get_scriptscriptfont(fam).clone()
        }
    }
    /// register a new [`PrimitiveCommand`]. Should only be called during engine initialization.
    fn register_primitive(&mut self,aux:&mut EngineAux<ET>,name:&'static str,cmd:PrimitiveCommand<ET>);
    /// return the set of all registered [`PrimitiveCommands`].
    fn primitives(&self) -> &PrimitiveCommands<ET>;
    /// push a new group level to the scoping stack; `line_number` is used for `\tracinggroups`
    fn push(&mut self,aux:&mut EngineAux<ET>, group_type: GroupType,line_number:usize);
    /// pop a group level from the scoping stack. Needs the mouth to insert the `\aftergroup` [`Token`] (if set)
    fn pop(&mut self,aux:&mut EngineAux<ET>,mouth: &mut ET::Mouth);
    /// The current [`GroupType] (i.e. `\currentgrouptype`).
    fn get_group_type(&self) -> Option<GroupType>;
    /// The current group level/depth (0 being the top level; i.e. `\currentgrouplevel`)
    fn get_group_level(&self) -> usize;
    /// Get the current [`Font`](crate::engine::fontsystem::Font)
    fn get_current_font(&self) -> &ET::Font;
    /// Set the current [`Font`](crate::engine::fontsystem::Font)
    fn set_current_font(&mut self,aux:&mut EngineAux<ET>,fnt:ET::Font,globally:bool);
    /// Get the current `\textfont` for the given family number, where `fam` is 0..15
    fn get_textfont(&self, fam:u8) -> &ET::Font;
    /// Set the current `\textfont` for the given family number, where `fam` is 0..15
    fn set_textfont(&mut self,aux:&mut EngineAux<ET>,fam:u8,fnt:ET::Font,globally:bool);
    /// Get the current `\scriptfont` for the given family number, where `fam` is 0..15
    fn get_scriptfont(&self, fam:u8) -> &ET::Font;
    /// Set the current `\scriptfont` for the given family number, where `fam` is 0..15
    fn set_scriptfont(&mut self,aux:&mut EngineAux<ET>,fam:u8,fnt:ET::Font,globally:bool);
    /// Get the current `\scriptscriptfont` for the given family number, where `fam` is 0..15
    fn get_scriptscriptfont(&self, fam:u8) -> &ET::Font;
    /// Set the current `\scriptscriptfont` for the given family number, where `fam` is 0..15
    fn set_scriptscriptfont(&mut self,aux:&mut EngineAux<ET>,fam:u8,fnt:ET::Font,globally:bool);

    /// Get the current [`CategoryCodeScheme`]
    fn get_catcode_scheme(&self) -> &CategoryCodeScheme<ET::Char>;
    /// Set the current [`CategoryCode`] for a character
    fn set_catcode(&mut self,aux:&EngineAux<ET>, c: ET::Char, cc:CategoryCode, globally:bool);
    /// Get the current space factor code `\sfcode` for a character
    fn get_sfcode(&self,c:ET::Char) -> u16;
    /// Set the current space factor code `\sfcode` for a character
    fn set_sfcode(&mut self,aux:&EngineAux<ET>, c: ET::Char, sfcode:u16, globally:bool);
    /// Get the current lower case code `\lccode` for a character
    fn get_lccode(&self,c:ET::Char) -> ET::Char;
    /// Set the current lower case code `\lccode` for a character
    fn set_lccode(&mut self,aux:&EngineAux<ET>, c: ET::Char, lccode:ET::Char, globally:bool);
    /// Get the current upper case code `\uccode` for a character
    fn get_uccode(&self,c:ET::Char) -> ET::Char;
    /// Set the current upper case code `\uccode` for a character
    fn set_uccode(&mut self,aux:&EngineAux<ET>, c: ET::Char, uccode:ET::Char, globally:bool);
    /// Get the current delimiter code `\delcode` for a character
    fn get_delcode(&self,c:ET::Char) -> ET::Int;
    /// Set the current delimiter code `\delcode` for a character
    fn set_delcode(&mut self,aux:&EngineAux<ET>, c: ET::Char, delcode:ET::Int, globally:bool);
    /// Get the current math code `\mathcode` for a character
    fn get_mathcode(&self,c:ET::Char) -> u32;
    /// Set the current math code `\mathcode` for a character
    fn set_mathcode(&mut self,aux:&EngineAux<ET>, c: ET::Char, mathcode:u32, globally:bool);
    /// Get the current endline character
    fn get_endline_char(&self) -> Option<ET::Char>;
    /// Set the current endline character
    fn set_endline_char(&mut self,aux:&EngineAux<ET>, c: Option<ET::Char>, globally:bool);
    /// Get the current escape character
    fn get_escape_char(&self) -> Option<ET::Char>;
    /// Set the current escape character
    fn set_escape_char(&mut self,aux:&EngineAux<ET>, c: Option<ET::Char>, globally:bool);
    /// Get the current newline character
    fn get_newline_char(&self) -> Option<ET::Char>;
    /// Set the current newline character
    fn set_newline_char(&mut self,aux:&EngineAux<ET>, c: Option<ET::Char>, globally:bool);
    /// Get the current `\parshape`
    fn get_parshape(&self) -> &Vec<(ET::Dim,ET::Dim)>;
    /// Get the current `\parshape` and set it to its default value
    fn take_parshape(&mut self) -> Vec<(ET::Dim,ET::Dim)>;
    /// Set the current `\parshape`
    fn set_parshape(&mut self,aux:&EngineAux<ET>, parshape:Vec<(ET::Dim,ET::Dim)>, globally:bool);
    /// Get an integer register value
    fn get_int_register(&self,idx:usize) -> ET::Int;
    /// Set an integer register value
    fn set_int_register(&mut self,aux:&EngineAux<ET>,idx:usize,v:ET::Int,globally:bool);
    /// Get a primitive integer value
    fn get_primitive_int(&self,name:PrimitiveIdentifier) -> ET::Int;
    /// Set a primitive integer value
    fn set_primitive_int(&mut self,aux:&EngineAux<ET>,name:PrimitiveIdentifier,v:ET::Int,globally:bool);
    /// Get a dimen register value
    fn get_dim_register(&self,idx:usize) -> ET::Dim;
    /// Set a dimen register value
    fn set_dim_register(&mut self,aux:&EngineAux<ET>,idx:usize,v:ET::Dim,globally:bool);
    /// Get a skip register value
    fn get_skip_register(&self,idx:usize) -> ET::Skip;
    /// Set a skip register value
    fn set_skip_register(&mut self,aux:&EngineAux<ET>,idx:usize,v:ET::Skip,globally:bool);
    /// Get a muskip register value
    fn get_muskip_register(&self,idx:usize) -> ET::MuSkip;
    /// Set a muskip register value
    fn set_muskip_register(&mut self,aux:&EngineAux<ET>,idx:usize,v:ET::MuSkip,globally:bool);
    /// Get a token register value
    fn get_toks_register(&self,idx:usize) -> &TokenList<ET::Token>;
    /// Set a token register value
    fn set_toks_register(&mut self,aux:&EngineAux<ET>,idx:usize,v:TokenList<ET::Token>,globally:bool);
    /// Get a box register value
    fn get_box_register(&self,idx:usize) -> Option<&TeXBox<ET>>;
    /// Get a box register value mutably (to e.g. change `\ht`, `\wd`, `\dp`, etc.)
    fn get_box_register_mut(&mut self,idx:usize) -> Option<&mut TeXBox<ET>>;
    /// Take a box register value; replacing it with `None` (i.e. void box)
    fn take_box_register(&mut self,idx:usize) -> Option<TeXBox<ET>>;
    /// Set a box register value
    fn set_box_register(&mut self, aux:&EngineAux<ET>, idx:usize, v:Option<TeXBox<ET>>, globally:bool);
    /// Get a primitive dimension value
    fn get_primitive_dim(&self,name:PrimitiveIdentifier) -> ET::Dim;
    /// Set a primitive dimension value
    fn set_primitive_dim(&mut self,aux:&EngineAux<ET>,name:PrimitiveIdentifier,v:ET::Dim,globally:bool);
    /// Get a primitive skip value
    fn get_primitive_skip(&self,name:PrimitiveIdentifier) -> ET::Skip;
    /// Set a primitive skip value
    fn set_primitive_skip(&mut self,aux:&EngineAux<ET>,name:PrimitiveIdentifier,v:ET::Skip,globally:bool);
    /// Get a primitive muskip value
    fn get_primitive_muskip(&self,name:PrimitiveIdentifier) -> ET::MuSkip;
    /// Set a primitive muskip value
    fn set_primitive_muskip(&mut self,aux:&EngineAux<ET>,name:PrimitiveIdentifier,v:ET::MuSkip,globally:bool);
    /// Get a primitive token list
    fn get_primitive_tokens(&self,name:PrimitiveIdentifier) -> &TokenList<ET::Token>;
    /// Set a primitive token list
    fn set_primitive_tokens(&mut self,aux:&EngineAux<ET>,name:PrimitiveIdentifier,v:TokenList<ET::Token>,globally:bool);
    /// Get the current definition for the control sequence name
    fn get_command(&self, name:&ET::CSName) -> Option<&Command<ET>>;
    /// Set the current definition for the control sequence name
    fn set_command(&mut self,aux:&EngineAux<ET>, name:ET::CSName, cmd:Option<Command<ET>>, globally:bool);
    /// Get the current definition for the active character
    fn get_ac_command(&self, c:ET::Char) -> Option<&Command<ET>>;
    /// Set the current definition for the active character
    fn set_ac_command(&mut self,aux:&EngineAux<ET>, c:ET::Char, cmd:Option<Command<ET>>, globally:bool);
}

/// Convenience trait for tracking state changes in a [`StateStack`] and rolling them back
/// if necessary at the end of a group.
pub trait StateChangeTracker<ET:EngineTypes>:State<ET> {
    /// Get the current [`StateStack`]
    fn stack(&mut self) -> &mut StateStack<ET>;
    /// Change a field of the state, and add the change to the [`StateStack`]. Also takes care of inspecting
    /// and considering the current `\globaldefs` value.
    fn change_field<F:FnOnce(&mut Self,bool) -> StateChange<ET>>(&mut self,globally:bool,f:F) {
        let globaldefs = self.get_primitive_int(PRIMITIVES.globaldefs);
        let zero = ET::Int::default();
        let global = if globaldefs == zero { globally } else { globaldefs > zero };
        let change = f(self,global);
        if global {
            self.stack().add_change_globally(change)
        } else {
            self.stack().add_change_locally(change)
        }
    }
}

/// A change to a [`State`], to be potentially rolled back when a group ends.
#[derive(Clone)]
pub enum StateChange<ET:EngineTypes> {
    Catcode{char:ET::Char,old:CategoryCode},
    SfCode{char:ET::Char,old:u16},
    DelCode{char:ET::Char,old:ET::Int},
    LcCode{char:ET::Char,old:ET::Char},
    UcCode{char:ET::Char,old:ET::Char},
    MathCode{char:ET::Char,old:u32},
    ParShape{old:Vec<(ET::Dim,ET::Dim)>},
    CurrentFont(ET::Font),
    TextFont{idx:u8,old:ET::Font},
    ScriptFont{idx:u8,old:ET::Font},
    ScriptScriptFont{idx:u8,old:ET::Font},
    EndlineChar{old:Option<ET::Char>},
    EscapeChar{old:Option<ET::Char>},
    NewlineChar{old:Option<ET::Char>},
    IntRegister{idx:usize,old:ET::Int},
    DimRegister{idx:usize,old:ET::Dim},
    SkipRegister{idx:usize,old:ET::Skip},
    MuSkipRegister{idx:usize,old:ET::MuSkip},
    BoxRegister{idx:usize,old:Option<TeXBox<ET>>},
    ToksRegister{idx:usize,old:TokenList<ET::Token>},
    PrimitiveInt{name:PrimitiveIdentifier,old:ET::Int},
    PrimitiveDim{name:PrimitiveIdentifier,old:ET::Dim},
    PrimitiveToks{name:PrimitiveIdentifier,old:TokenList<ET::Token>},
    PrimitiveSkip{name:PrimitiveIdentifier,old:ET::Skip},
    PrimitiveMuSkip{name:PrimitiveIdentifier,old:ET::MuSkip},
    Command{name:ET::CSName,old:Option<Command<ET>>},
    AcCommand{ char:ET::Char,old:Option<Command<ET>>},
    // /// A custom state change, to be implemented by the engine, if additional state change types are needed
    //Custom{ change:Ptr<Mutex<Option<Box<dyn CustomStateChange<ET>>>>>},
}
/*
/// A custom state change, to be implemented by the engine, if additional state change types are needed.
pub trait CustomStateChange<ET:EngineTypes> {
    /// Check if this state change is equivalent to another one, i.e. if it needs to be rolled back
    /// when a group ends, or is superseded by a previous change
    fn equiv(&self,other:&dyn CustomStateChange<ET>) -> bool;
    fn restore(&mut self,aux:&EngineAux<ET>,state:&mut ET::State,trace:bool);
} */

impl<ET:EngineTypes> StateChange<ET> {
    /// Check if this state change is equivalent to another one, i.e. if it needs to be rolled back
    /// when a group ends, or is superseded by a previous change
    pub fn equiv(&self,other:&Self) -> bool {
        if std::mem::discriminant(self) != std::mem::discriminant(other) { return false; }
        match (self,other) {
            (StateChange::Catcode{char:c1,..},StateChange::Catcode{char:c2,..}) => c1 == c2,
            (StateChange::SfCode {char:c1,..},StateChange::SfCode{char:c2,..}) => c1 == c2,
            (StateChange::LcCode {char:c1,..},StateChange::LcCode{char:c2,..}) => c1 == c2,
            (StateChange::UcCode {char:c1,..},StateChange::UcCode{char:c2,..}) => c1 == c2,
            (StateChange::MathCode {char:c1,..},StateChange::MathCode{char:c2,..}) => c1 == c2,
            (StateChange::CurrentFont(_),StateChange::CurrentFont(_)) => true,
            (StateChange::TextFont{idx:i1,..},StateChange::TextFont{idx:i2,..}) => i1 == i2,
            (StateChange::ScriptFont{idx:i1,..},StateChange::ScriptFont{idx:i2,..}) => i1 == i2,
            (StateChange::ScriptScriptFont{idx:i1,..},StateChange::ScriptScriptFont{idx:i2,..}) => i1 == i2,
            (StateChange::EndlineChar{..},StateChange::EndlineChar{..}) => true,
            (StateChange::EscapeChar{..},StateChange::EscapeChar{..}) => true,
            (StateChange::NewlineChar{..},StateChange::NewlineChar{..}) => true,
            (StateChange::ParShape{..},StateChange::ParShape{..}) => true,
            (StateChange::DelCode {char:c1,..},StateChange::DelCode{char:c2,..}) => c1 == c2,
            (StateChange::IntRegister {idx:i1,..},StateChange::IntRegister{idx:i2,..}) => i1 == i2,
            (StateChange::DimRegister {idx:i1,..},StateChange::DimRegister{idx:i2,..}) => i1 == i2,
            (StateChange::SkipRegister {idx:i1,..},StateChange::SkipRegister{idx:i2,..}) => i1 == i2,
            (StateChange::MuSkipRegister {idx:i1,..},StateChange::MuSkipRegister{idx:i2,..}) => i1 == i2,
            (StateChange::ToksRegister {idx:i1,..},StateChange::ToksRegister{idx:i2,..}) => i1 == i2,
            (StateChange::BoxRegister {idx:i1,..},StateChange::BoxRegister{idx:i2,..}) => i1 == i2,
            (StateChange::PrimitiveInt{name:n1,..},StateChange::PrimitiveInt{name:n2,..}) => n1 == n2,
            (StateChange::PrimitiveDim{name:n1,..},StateChange::PrimitiveDim{name:n2,..}) => n1 == n2,
            (StateChange::PrimitiveSkip{name:n1,..},StateChange::PrimitiveSkip{name:n2,..}) => n1 == n2,
            (StateChange::PrimitiveMuSkip{name:n1,..},StateChange::PrimitiveMuSkip{name:n2,..}) => n1 == n2,
            (StateChange::PrimitiveToks{name:n1,..},StateChange::PrimitiveToks{name:n2,..}) => n1 == n2,
            (StateChange::Command{name:c1,..},StateChange::Command{name:c2,..}) => c1 == c2,
            (StateChange::AcCommand{ char:c1,..},StateChange::AcCommand{ char:c2,..}) => c1 == c2,
            //(StateChange::Custom{ change:e1},StateChange::Custom{ change:e2}) => e1.equiv(&***e2),
            _ => false
        }
    }
}

/// A level of the [`StateStack`], to be rolled back when a group ends
#[derive(Clone)]
pub struct StackLevel<ET:EngineTypes> {
    /// The type of the group
    pub group_type:GroupType,
    /// The `\aftergroup` [`Token`] to be inserted when the group ends
    pub aftergroup:Vec<ET::Token>,
    /// The changes to be rolled back when the group ends
    pub changes:Vec<StateChange<ET>>,
    //keep_track:Vec<C>
}

/// A stack of [`StateChange`]s, to be rolled back when a group ends
pub struct StateStack<ET:EngineTypes> {
    pub stack:Vec<StackLevel<ET>>,
    vecs:Vec<Vec<StateChange<ET>>>
}
impl<ET:EngineTypes> Clone for StateStack<ET> {
    fn clone(&self) -> Self {
        Self {
            stack:self.stack.clone(),
            vecs:vec!()
        }
    }

}
impl<ET:EngineTypes> StateStack<ET> {
    /// Memory management; gives back a Vec<StateChange> to the pool to avoid frequent allocations
    pub fn give_back(&mut self,mut lvl:StackLevel<ET>) {
        lvl.changes.clear();
        self.vecs.push(lvl.changes);
    }
    /// Create a new [`StateStack`]
    pub fn new() -> Self { Self { stack:vec!(),vecs:vec!() } }
    /// Push a new stack level onto the stack with the given [`GroupType`], as a new group begins
    pub fn push(&mut self,group_type:GroupType) {
        let lvl = StackLevel {
            group_type,
            aftergroup:vec!(),
            changes:match self.vecs.pop() {
                Some(v) => v,
                _ => Vec::new()
            }
        };
        self.stack.push(lvl);
    }
    /// Register a global state change, never to be rolled back;
    /// if there is a stack level, remove any equivalent previous changes
    pub fn add_change_globally(&mut self,change:StateChange<ET>) {
        if self.stack.is_empty() { return; }
        for lvl in &mut self.stack {
            lvl.changes.retain(|c| !c.equiv(&change));
        }
    }
    /// Register a local state change, to be rolled back when the current group ends
    pub fn add_change_locally(&mut self,change:StateChange<ET>) {
        match self.stack.last_mut() {
            Some(lvl) => {
                if lvl.changes.iter().all(|c| !c.equiv(&change)) {
                    lvl.changes.push(change);
                }
            }
            None => ()
        }
    }
}

impl<ET:EngineTypes> EngineReferences<'_,ET> {
    /// Set the current definition of the provided control sequence name or active character
    pub fn set_command(&mut self, name:&CSOrActiveChar<ET::Token>, cmd:Option<Command<ET>>, globally:bool) {
        match name {
            CSOrActiveChar::Active(c) => self.state.set_ac_command(self.aux, *c, cmd, globally),
            CSOrActiveChar::Name(cs) => self.state.set_command(self.aux, cs.clone(), cmd, globally)
        }
    }
}