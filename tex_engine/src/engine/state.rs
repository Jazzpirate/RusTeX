/*! The [`State`] of a TeX engine. */
pub mod tex_state;

use crate::engine::{EngineAux, EngineReferences, EngineTypes};
use crate::engine::utils::memory::{PrimitiveIdentifier, PRIMITIVES};
use crate::tex::catcodes::{CategoryCode, CategoryCodeScheme};
use crate::commands::Command;
use crate::engine::gullet::methods::ACOrCS;
use crate::engine::mouth::pretokenized::TokenList;
use crate::tex::numerics::NumSet;
use crate::tex::token::Token;
use crate::tex::types::{GroupType, TeXMode};
use crate::utils::{Ptr, ReusableVectorFactory};
use crate::engine::FontSystem;
use crate::tex::nodes::TeXBox;

type Ch<S> = <<S as State>::ET as EngineTypes>::Char;
type Int<S> = <<<S as State>::ET as EngineTypes>::Num as NumSet>::Int;
type Dim<S> = <<<S as State>::ET as EngineTypes>::Num as NumSet>::Dim;
type Skip<S> = <<<S as State>::ET as EngineTypes>::Num as NumSet>::Skip;
type MuSkip<S> = <<<S as State>::ET as EngineTypes>::Num as NumSet>::MuSkip;
type CS<S> = <<<S as State>::ET as EngineTypes>::Token as Token>::CS;
type T<S> = <<S as State>::ET as EngineTypes>::Token;
type Fnt<S> = <<<S as State>::ET as EngineTypes>::FontSystem as FontSystem>::Font;

/// A TeX state; holds all the different parameters, equivalents, registers etc.
pub trait State:Sized+Clone {
    type ET:EngineTypes<State=Self>;

    fn push(&mut self,aux:&EngineAux<Self::ET>, group_type: GroupType,line_number:usize);
    fn pop(&mut self,aux:&EngineAux<Self::ET>,mouth: &mut <Self::ET as EngineTypes>::Mouth);
    fn get_group_type(&self) -> Option<GroupType>;
    fn get_group_level(&self) -> usize;

    fn get_current_font(&self) -> &Fnt<Self>;
    fn set_current_font(&mut self,aux:&EngineAux<Self::ET>,fnt:Fnt<Self>,globally:bool);

    /// get the current [`CategoryCodeScheme`]
    fn get_catcode_scheme(&self) -> &CategoryCodeScheme<Ch<Self>>;
    /// set the current [`CategoryCode`] for a character
    fn set_catcode(&mut self,aux:&EngineAux<Self::ET>, c: Ch<Self>, cc:CategoryCode, globally:bool);

    fn get_sfcode(&self,c:Ch<Self>) -> u16;
    fn set_sfcode(&mut self,aux:&EngineAux<Self::ET>, c: Ch<Self>, sfcode:u16, globally:bool);

    fn get_lccode(&self,c:Ch<Self>) -> Ch<Self>;
    fn set_lccode(&mut self,aux:&EngineAux<Self::ET>, c: Ch<Self>, lccode:Ch<Self>, globally:bool);

    fn get_uccode(&self,c:Ch<Self>) -> Ch<Self>;
    fn set_uccode(&mut self,aux:&EngineAux<Self::ET>, c: Ch<Self>, uccode:Ch<Self>, globally:bool);

    fn get_delcode(&self,c:Ch<Self>) -> Int<Self>;
    fn set_delcode(&mut self,aux:&EngineAux<Self::ET>, c: Ch<Self>, delcode:Int<Self>, globally:bool);

    fn get_mathcode(&self,c:Ch<Self>) -> u32;
    fn set_mathcode(&mut self,aux:&EngineAux<Self::ET>, c: Ch<Self>, mathcode:u32, globally:bool);

    fn get_mode(&self) -> TeXMode;
    fn set_mode(&mut self,mode:TeXMode);

    fn get_endline_char(&self) -> Option<Ch<Self>>;
    fn set_endline_char(&mut self,aux:&EngineAux<Self::ET>, c: Option<Ch<Self>>, globally:bool);
    fn get_escape_char(&self) -> Option<Ch<Self>>;
    fn set_escape_char(&mut self,aux:&EngineAux<Self::ET>, c: Option<Ch<Self>>, globally:bool);
    fn get_newline_char(&self) -> Option<Ch<Self>>;
    fn set_newline_char(&mut self,aux:&EngineAux<Self::ET>, c: Option<Ch<Self>>, globally:bool);

    /// get an integer register value
    fn get_int_register(&self,idx:u16) -> Int<Self>;
    /// set an integer register value
    fn set_int_register(&mut self,aux:&EngineAux<Self::ET>,idx:u16,v:Int<Self>,globally:bool);
    /// get a primitive integer value
    fn get_primitive_int(&self,name:PrimitiveIdentifier) -> Int<Self>;
    /// set a primitive integer value
    fn set_primitive_int(&mut self,aux:&EngineAux<Self::ET>,name:PrimitiveIdentifier,v:Int<Self>,globally:bool);
    /// get a dimen register value
    fn get_dim_register(&self,idx:u16) -> Dim<Self>;
    /// set a dimen register value
    fn set_dim_register(&mut self,aux:&EngineAux<Self::ET>,idx:u16,v:Dim<Self>,globally:bool);
    /// get a skip register value
    fn get_skip_register(&self,idx:u16) -> Skip<Self>;
    /// set a skip register value
    fn set_skip_register(&mut self,aux:&EngineAux<Self::ET>,idx:u16,v:Skip<Self>,globally:bool);
    /// get a muskip register value
    fn get_muskip_register(&self,idx:u16) -> MuSkip<Self>;
    /// set a muskip register value
    fn set_muskip_register(&mut self,aux:&EngineAux<Self::ET>,idx:u16,v:MuSkip<Self>,globally:bool);
    /// get a token register value
    fn get_toks_register(&self,idx:u16) -> &TokenList<T<Self>>;
    /// set a token register value
    fn set_toks_register(&mut self,aux:&EngineAux<Self::ET>,idx:u16,v:TokenList<T<Self>>,globally:bool);

    fn get_box_register(&self,idx:u16) -> Option<&TeXBox<Self::ET>>;
    fn get_box_register_mut(&mut self,idx:u16) -> Option<&mut TeXBox<Self::ET>>;
    fn take_box_register(&mut self,idx:u16) -> Option<TeXBox<Self::ET>>;
    fn set_box_register(&mut self,aux:&EngineAux<Self::ET>,idx:u16,v:Option<TeXBox<Self::ET>>,globally:bool);

    /// get a primitive dimension value
    fn get_primitive_dim(&self,name:PrimitiveIdentifier) -> Dim<Self>;
    /// set a primitive dimension value
    fn set_primitive_dim(&mut self,aux:&EngineAux<Self::ET>,name:PrimitiveIdentifier,v:Dim<Self>,globally:bool);
    /// get a primitive skip value
    fn get_primitive_skip(&self,name:PrimitiveIdentifier) -> Skip<Self>;
    /// set a primitive skip value
    fn set_primitive_skip(&mut self,aux:&EngineAux<Self::ET>,name:PrimitiveIdentifier,v:Skip<Self>,globally:bool);
    /// get a primitive muskip value
    fn get_primitive_muskip(&self,name:PrimitiveIdentifier) -> MuSkip<Self>;
    /// set a primitive muskip value
    fn set_primitive_muskip(&mut self,aux:&EngineAux<Self::ET>,name:PrimitiveIdentifier,v:MuSkip<Self>,globally:bool);
    /// get a primitive token list
    fn get_primitive_tokens(&self,name:PrimitiveIdentifier) -> &TokenList<T<Self>>;
    /// set a primitive token list
    fn set_primitive_tokens(&mut self,aux:&EngineAux<Self::ET>,name:PrimitiveIdentifier,v:TokenList<T<Self>>,globally:bool);

    fn get_command(&self, name:&CS<Self>) -> Option<&Command<Self::ET>>;
    fn set_command(&mut self,aux:&EngineAux<Self::ET>, name:CS<Self>, cmd:Option<Command<Self::ET>>, globally:bool);

    fn get_ac_command(&self, c:Ch<Self>) -> Option<&Command<Self::ET>>;
    fn set_ac_command(&mut self,aux:&EngineAux<Self::ET>, c:Ch<Self>, cmd:Option<Command<Self::ET>>, globally:bool);
}

/// Convenience trait for tracking state changes in a [`StateStack`] and rolling them back
/// if necessary at the end of a group.
pub trait StateChangeTracker:State {
    /// get the current [`StateStack`]
    fn stack(&mut self) -> &mut StateStack<Self>;
    /// change a field of the state, and add the change to the [`StateStack`]
    fn change_field<F:FnOnce(&mut Self,bool) -> StateChange<Self>>(&mut self,globally:bool,f:F) {
        let globaldefs = self.get_primitive_int(PRIMITIVES.globaldefs);
        let zero = Int::<Self>::default();
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
pub enum StateChange<S:State> {
    /// A change to a [`CategoryCode`]
    Catcode{char:Ch<S>,old:CategoryCode},
    SfCode{char:Ch<S>,old:u16},
    DelCode{char:Ch<S>,old:Int<S>},
    LcCode{char:Ch<S>,old:Ch<S>},
    UcCode{char:Ch<S>,old:Ch<S>},
    MathCode{char:Ch<S>,old:u32},
    /// A change to the [`TeXMode`], rolled back when a box group ends
    TeXMode{old:TeXMode},
    CurrentFont(Fnt<S>),
    EndlineChar{old:Option<Ch<S>>},
    EscapeChar{old:Option<Ch<S>>},
    NewlineChar{old:Option<Ch<S>>},
    IntRegister{idx:u16,old:Int<S>},
    DimRegister{idx:u16,old:Dim<S>},
    SkipRegister{idx:u16,old:Skip<S>},
    MuSkipRegister{idx:u16,old:MuSkip<S>},
    BoxRegister{idx:u16,old:Option<TeXBox<S::ET>>},
    ToksRegister{idx:u16,old:TokenList<T<S>>},
    /// A change to a primitive integer value
    PrimitiveInt{name:PrimitiveIdentifier,old:Int<S>},
    /// A change to a primitive dimension value
    PrimitiveDim{name:PrimitiveIdentifier,old:Dim<S>},
    /// A change to a primitive token list
    PrimitiveToks{name:PrimitiveIdentifier,old:TokenList<T<S>>},
    PrimitiveSkip{name:PrimitiveIdentifier,old:Skip<S>},
    PrimitiveMuSkip{name:PrimitiveIdentifier,old:MuSkip<S>},
    Command{name:CS<S>,old:Option<Command<S::ET>>},
    AcCommand{ char:Ch<S>,old:Option<Command<S::ET>>},
    // /// A custom state change, to be implemented by the engine, if additional state change types are needed
    //Custom{ change:Ptr<Box<dyn CustomStateChange<S>>>},
}

/// A custom state change, to be implemented by the engine, if additional state change types are needed.
pub trait CustomStateChange<S:State>:'static {
    /// Check if this state change is equivalent to another one, i.e. if it needs to be rolled back
    /// when a group ends, or is superseded by a previous change
    fn equiv(&self,other:&dyn CustomStateChange<S>) -> bool;
    fn restore(self:Box<Self>,aux:&EngineAux<S::ET>,state:&mut S,trace:bool);
}

impl<S:State> StateChange<S> {
    /// Check if this state change is equivalent to another one, i.e. if it needs to be rolled back
    /// when a group ends, or is superseded by a previous change
    pub fn equiv(&self,other:&StateChange<S>) -> bool {
        match (self,other) {
            (StateChange::Catcode{char:c1,..},StateChange::Catcode{char:c2,..}) => c1 == c2,
            (StateChange::SfCode {char:c1,..},StateChange::SfCode{char:c2,..}) => c1 == c2,
            (StateChange::LcCode {char:c1,..},StateChange::LcCode{char:c2,..}) => c1 == c2,
            (StateChange::UcCode {char:c1,..},StateChange::UcCode{char:c2,..}) => c1 == c2,
            (StateChange::MathCode {char:c1,..},StateChange::MathCode{char:c2,..}) => c1 == c2,
            (StateChange::TeXMode{..},StateChange::TeXMode{..}) => true,
            (StateChange::CurrentFont(_),StateChange::CurrentFont(_)) => true,
            (StateChange::EndlineChar{..},StateChange::EndlineChar{..}) => true,
            (StateChange::EscapeChar{..},StateChange::EscapeChar{..}) => true,
            (StateChange::NewlineChar{..},StateChange::NewlineChar{..}) => true,
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

impl<ET:EngineTypes> EngineReferences<'_,ET> {
    pub fn set_command(&mut self,name:&ACOrCS<ET::Token>,cmd:Option<Command<ET>>,globally:bool) {
        match name {
            ACOrCS::Active(c) => self.state.set_ac_command(self.aux,*c,cmd,globally),
            ACOrCS::Name(cs) => self.state.set_command(self.aux,cs.clone(),cmd,globally)
        }
    }
}


#[derive(Clone)]
pub struct StackLevel<S:State> {
    pub group_type:GroupType,
    pub aftergroup:Option<Vec<<S::ET as EngineTypes>::Token>>,
    pub changes:Vec<StateChange<S>>,
    //keep_track:Vec<C>
}

/// A stack of [`StateChange`]s, to be rolled back when a group ends
pub struct StateStack<S:State> {
    pub stack:Vec<StackLevel<S>>,
    factory:ReusableVectorFactory<StateChange<S>>
}
impl<S:State> Clone for StateStack<S> {
    fn clone(&self) -> Self {
        Self {
            stack:self.stack.clone(),
            factory:ReusableVectorFactory::new(8,4)
        }
    }

}
impl<S:State> StateStack<S> {
    pub fn give_back(&mut self,lvl:StackLevel<S>) {
        self.factory.give_back(lvl.changes);
    }
    /// Create a new [`StateStack`]
    pub fn new() -> Self { Self { stack:vec!(),factory:ReusableVectorFactory::new(8,8) } }
    /// Push a new stack level onto the stack with the given [`GroupType`], as a new group begins
    pub fn push(&mut self,group_type:GroupType,current_mode:&mut TeXMode) {
        let mut lvl = StackLevel {
            group_type,
            aftergroup:None,
            changes:self.factory.get()
        };
        match group_type {
            GroupType::Box(bt) => {
                lvl.changes.push(StateChange::TeXMode{old:*current_mode});
                *current_mode = bt.into();
            },
            _ => ()
        }
        self.stack.push(lvl);
    }
    /// register a global state change, never to be rolled back;
    /// if there is a stack level, remove any equivalent previous changes
    pub fn add_change_globally(&mut self,change:StateChange<S>) {
        if self.stack.is_empty() { return; }
        for lvl in &mut self.stack {
            lvl.changes.retain(|c| !c.equiv(&change));
        }
    }
    /// register a local state change, to be rolled back when the current group ends
    pub fn add_change_locally(&mut self,change:StateChange<S>) {
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

/*
#[macro_export]
macro_rules! change_state_field {
    ($self:ident,$globally:ident,$stack:expr => $change:expr) => {
        let globaldefs = $self.get_primitive_int(crate::engine::memory::globaldefs_id());
        let zero = <<Self as crate::engine::state::State>::Num as crate::tex::numerics::NumSet>::Int::default();
        let global = if globaldefs == zero { $globally } else { globaldefs > zero };
        let change = $change;
        if global {
            $stack.add_change_globally(change)
        } else {
            $stack.add_change_locally(change)
        }
    };
}
*/