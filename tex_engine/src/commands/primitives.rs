/*! Methods for registering primitives */
use crate::commands::{Assignment, BoxCommand, Command, Conditional, DimCommand, Expandable, FontCommand, IntCommand, MuSkipCommand, CommandScope, SimpleExpandable, SkipCommand, Unexpandable, Whatsit};
use crate::engine::utils::memory::{PrimitiveIdentifier, PRIMITIVES};
use crate::engine::{EngineReferences, EngineTypes, TeXEngine};
use crate::engine::fontsystem::FontSystem;
use crate::engine::state::State;
use crate::tex::tokens::control_sequences::CSHandler;
use crate::tex::nodes::boxes::{BoxInfo, TeXBox};
use crate::tex::numerics::NumSet;
use crate::utils::HMap;

type Tk<E> = <<E as TeXEngine>::Types as EngineTypes>::Token;
type Int<E> = <<<E as TeXEngine>::Types as EngineTypes>::Num as NumSet>::Int;
type Dim<E> = <<<E as TeXEngine>::Types as EngineTypes>::Num as NumSet>::Dim;
type Skip<E> = <<<E as TeXEngine>::Types as EngineTypes>::Num as NumSet>::Skip;
type MuSkip<E> = <<<E as TeXEngine>::Types as EngineTypes>::Num as NumSet>::MuSkip;
type Font<E> = <<<E as TeXEngine>::Types as EngineTypes>::FontSystem as FontSystem>::Font;

#[macro_export]
macro_rules! cmtodos {
    ($engine:ident,$($name:ident),*) => {
        $(cmtodo!($engine,$name);)*
    };
}

#[macro_export]
macro_rules! cmstodos {
    ($engine:ident,$($name:ident),*) => {
        $(cmstodo!($engine,$name);)*
    };
}

#[macro_export]
macro_rules! cmtodo {
    ($engine:ident,$name:ident) => {{
        let id = crate::engine::utils::memory::PRIMITIVES.get(stringify!($name));
        let command = crate::commands::Command::SimpleExpandable(crate::commands::SimpleExpandable{
            name:id,
            expand:|e,_| crate::utils::errors::TeXError::throw(format!("Not yet implemented: \\{} at {}",
                stringify!($name),
                crate::engine::mouth::Mouth::current_sourceref(e.mouth).display(e.filesystem)
            ))
        });
        let refs = $engine.get_engine_refs();
        refs.state.register_primitive(refs.aux,id,stringify!($name),command);
    }};
}

#[macro_export]
macro_rules! cmstodo {
    ($engine:ident,$name:ident) => {{
        let id = crate::engine::utils::memory::PRIMITIVES.get(stringify!($name));
        let command = crate::commands::Command::Unexpandable(crate::commands::Unexpandable{
            name:id,
            scope:crate::commands::CommandScope::Any,
            apply:|e,_| crate::utils::errors::TeXError::throw(format!("Not yet implemented: \\{} at {}",
                stringify!($name),
                crate::engine::mouth::Mouth::current_sourceref(e.mouth).display(e.filesystem)
            ))
        });
        let refs = $engine.get_engine_refs();
        refs.state.register_primitive(refs.aux,id,stringify!($name),command);
    }};
}

/// Creates a new [`Expandable`] primitive and registers it with the engine.
pub fn register_expandable<E:TeXEngine>(
    engine:&mut E,
    name:&'static str,
    f:fn(&mut EngineReferences<E::Types>,&mut Vec<Tk<E>>,Tk<E>)) {
    let id = PRIMITIVES.get(name);
    let command = Command::Expandable(Expandable{
        name:id,
        expand:f
    });
    let refs = engine.get_engine_refs();
    refs.state.register_primitive(refs.aux,id,name,command);
}

/// Creates a new [`SimpleExpandable`] primitive and registers it with the engine.
pub fn register_simple_expandable<E:TeXEngine>(
    engine:&mut E,
    name:&'static str,
    f:fn(&mut EngineReferences<E::Types>,Tk<E>)) {
    let id = PRIMITIVES.get(name);
    let command = Command::SimpleExpandable(SimpleExpandable{
        name:id,
        expand:f
    });
    let refs = engine.get_engine_refs();
    refs.state.register_primitive(refs.aux,id,name,command);
}

/// Creates a new [`Conditional`] primitive and registers it with the engine.
pub fn register_conditional<E:TeXEngine>(
    engine:&mut E,
    name:&'static str,
    f:fn(&mut EngineReferences<E::Types>,Tk<E>) -> bool) {
    let id = PRIMITIVES.get(name);
    let command = Command::Conditional(Conditional{
        name:id,
        expand:f
    });
    let refs = engine.get_engine_refs();
    refs.state.register_primitive(refs.aux,id,name,command);
}

/// Creates a new [`Unexpandable`] primitive and registers it with the engine.
pub fn register_unexpandable<E:TeXEngine>(
    engine:&mut E,
    name:&'static str,
    scope: CommandScope,
    f:fn(&mut EngineReferences<E::Types>,Tk<E>)) {
    let id = PRIMITIVES.get(name);
    let command = Command::Unexpandable(Unexpandable{
        name:id,scope,
        apply:f
    });
    let refs = engine.get_engine_refs();
    refs.state.register_primitive(refs.aux,id,name,command)
}

/// Creates a new primitive named integer value and registers it with the engine.
pub fn register_primitive_int<E:TeXEngine>(engine:&mut E,names:&[&'static str]) {
    let refs = engine.get_engine_refs();
    for name in names {
        let id = PRIMITIVES.get(*name);
        refs.state.register_primitive(refs.aux,id,name,Command::PrimitiveInt(id));
    }
}

/// Creates a new primitive command that yields (and optionally assigns) an
///integer value and registers it with the engine.
pub fn register_int<E:TeXEngine>(engine:&mut E,name:&'static str,
                                 read:fn(&mut EngineReferences<E::Types>,Tk<E>) -> Int<E>,
    assign:Option<for<'a,'b> fn(&'a mut EngineReferences<'b,E::Types>,Tk<E>,bool)>
) {
    let refs = engine.get_engine_refs();
    let id = PRIMITIVES.get(name);
    let command = Command::Int(IntCommand{
        name:id,read,assign
    });
    refs.state.register_primitive(refs.aux,id,name,command);
}

/// Creates a new primitive command that yields (and optionally assigns) a
/// dimension value and registers it with the engine.
pub fn register_dim<E:TeXEngine>(engine:&mut E,name:&'static str,
                                 read:fn(&mut EngineReferences<E::Types>,Tk<E>) -> Dim<E>,
                                 assign:Option<for<'a,'b> fn(&'a mut EngineReferences<'b,E::Types>,Tk<E>,bool)>
) {
    let refs = engine.get_engine_refs();
    let id = PRIMITIVES.get(name);
    let command = Command::Dim(DimCommand{
        name:id,read,assign
    });
    refs.state.register_primitive(refs.aux,id,name,command);
}

/// Creates a new primitive command that yields (and optionally assigns) a
/// skip value and registers it with the engine.
pub fn register_skip<E:TeXEngine>(engine:&mut E,name:&'static str,
                                 read:fn(&mut EngineReferences<E::Types>,Tk<E>) -> Skip<E>,
                                 assign:Option<for<'a,'b> fn(&'a mut EngineReferences<'b,E::Types>,Tk<E>,bool)>
) {
    let refs = engine.get_engine_refs();
    let id = PRIMITIVES.get(name);
    let command = Command::Skip(SkipCommand{
        name:id,read,assign
    });
    refs.state.register_primitive(refs.aux,id,name,command);
}

/// Creates a new primitive command that yields (and optionally assigns) a
/// muskip value and registers it with the engine.
pub fn register_muskip<E:TeXEngine>(engine:&mut E,name:&'static str,
                                  read:fn(&mut EngineReferences<E::Types>,Tk<E>) -> MuSkip<E>,
                                  assign:Option<for<'a,'b> fn(&'a mut EngineReferences<'b,E::Types>,Tk<E>,bool)>
) {
    let refs = engine.get_engine_refs();
    let id = PRIMITIVES.get(name);
    let command = Command::MuSkip(MuSkipCommand{
        name:id,read,assign
    });
    refs.state.register_primitive(refs.aux,id,name,command);
}

/// Creates a new primitive command that yields (and optionally assigns) a
/// font value and registers it with the engine.
pub fn register_font<E:TeXEngine>(engine:&mut E,name:&'static str,
                                    read:fn(&mut EngineReferences<E::Types>,Tk<E>) -> Font<E>,
                                    assign:Option<for<'a,'b> fn(&'a mut EngineReferences<'b,E::Types>,Tk<E>,bool)>
) {
    let refs = engine.get_engine_refs();
    let id = PRIMITIVES.get(name);
    let command = Command::FontCmd(FontCommand{
        name:id,read,assign
    });
    refs.state.register_primitive(refs.aux,id,name,command);
}

/// Creates a new primitive command that yields a
/// box and registers it with the engine.
pub fn register_box<E:TeXEngine>(engine:&mut E,name:&'static str,
                                  read:fn(&mut EngineReferences<E::Types>,Tk<E>) -> Result<Option<TeXBox<E::Types>>,BoxInfo<E::Types>>
) {
    let refs = engine.get_engine_refs();
    let id = PRIMITIVES.get(name);
    let command = Command::Box(BoxCommand{
        name:id,read
    });
    refs.state.register_primitive(refs.aux,id,name,command);
}

/// Creates a new primitive assignment command.
pub fn register_assignment<E:TeXEngine>(engine:&mut E,name:&'static str,
                                 assign:for<'a,'b> fn(&'a mut EngineReferences<'b,E::Types>,Tk<E>,bool)
) {
    let refs = engine.get_engine_refs();
    let id = PRIMITIVES.get(name);
    let command = Command::Assignment(Assignment{
        name:id,assign
    });
    refs.state.register_primitive(refs.aux,id,name,command);
}

/// Creates a new primitive named dimension and registers it with the engine.
pub fn register_primitive_dim<E:TeXEngine>(engine:&mut E,names:&[&'static str]) {
    let refs = engine.get_engine_refs();
    for name in names {
        let id = PRIMITIVES.get(*name);
        refs.state.register_primitive(refs.aux,id,name,Command::PrimitiveDim(id));
    }
}

/// Creates a new primitive named skip and registers it with the engine.
pub fn register_primitive_skip<E:TeXEngine>(engine:&mut E,names:&[&'static str]) {
    let refs = engine.get_engine_refs();
    for name in names {
        let id = PRIMITIVES.get(*name);
        refs.state.register_primitive(refs.aux,id,name,Command::PrimitiveSkip(id));
    }
}

/// Creates a new primitive named skip and registers it with the engine.
pub fn register_primitive_muskip<E:TeXEngine>(engine:&mut E,names:&[&'static str]) {
    let refs = engine.get_engine_refs();
    for name in names {
        let id = PRIMITIVES.get(*name);
        refs.state.register_primitive(refs.aux,id,name,Command::PrimitiveMuSkip(id));
    }
}

/// Creates a new primitive named token register and registers it with the engine.
pub fn register_primitive_toks<E:TeXEngine>(engine:&mut E,names:&[&'static str]) {
    let refs = engine.get_engine_refs();
    for name in names {
        let id = PRIMITIVES.get(*name);
        refs.state.register_primitive(refs.aux,id,name,Command::PrimitiveToks(id));
    }
}

/// Creates a new [`Whatsit`] primitive and registers it with the engine.
pub fn register_whatsit<E:TeXEngine>(
    engine:&mut E,
    name:&'static str,
    get:fn(&mut EngineReferences<E::Types>, Tk<E>)
             -> Option<Box<dyn FnOnce(&mut EngineReferences<E::Types>)>>,
    immediate:fn(&mut EngineReferences<E::Types>,Tk<E>)) {
    let id = PRIMITIVES.get(name);
    let command = Command::Whatsit(Whatsit{
        name:id,get,immediate
    });
    let refs = engine.get_engine_refs();
    refs.state.register_primitive(refs.aux,id,name,command);
}

#[derive(Clone)]
pub struct PrimitiveCommands<ET:EngineTypes> {
    commands: Vec<Command<ET>>,
    names:HMap<&'static str,u16>
}
impl<ET:EngineTypes> PrimitiveCommands<ET> {
    pub fn new() -> Self {
        Self {
            commands: Vec::new(),
            names: HMap::default()
        }
    }
    pub fn register(&mut self,name:&'static str,id:PrimitiveIdentifier,cmd: Command<ET>) {
        let idx = id.as_u16() as usize;
        if idx >= self.commands.len() {
            self.commands.resize(idx+1,Command::Relax);
        }
        self.names.insert(name,idx as u16);
    }
    pub fn get_id(&self,id:PrimitiveIdentifier) -> Option<&Command<ET>> {
        let idx = id.as_u16() as usize;
        self.commands.get(idx)
    }
    pub fn get_name(&self,s:&str) -> Option<PrimitiveIdentifier> {
        self.names.get(s).map(|&u| PrimitiveIdentifier::try_from_u16(u)).flatten()
    }
}