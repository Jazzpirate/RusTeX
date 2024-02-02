/*! Methods for [`PrimitiveCommand`]s and for registering new ones */
use std::sync::RwLock;
use either::Either;
use lazy_static::lazy_static;
use crate::commands::{TeXCommand, CommandScope, PrimitiveCommand};
use crate::engine::{EngineReferences, EngineTypes, TeXEngine};
use crate::engine::state::State;
use crate::prelude::Character;
use crate::tex::nodes::boxes::{BoxInfo, TeXBox};
use crate::tex::numerics::{MuSkip, Skip};
use crate::utils::HMap;


macro_rules! cmtodos {
    ($engine:ident,$($name:ident),*) => {
        $(cmtodo!($engine,$name);)*
    };
}

macro_rules! cmstodos {
    ($engine:ident,$($name:ident),*) => {
        $(cmstodo!($engine,$name);)*
    };
}

macro_rules! cmtodo {
    ($engine:ident,$name:ident) => {{
        let command = crate::commands::PrimitiveCommand::SimpleExpandable(
            |e,_| e.general_error(format!("Not yet implemented: \\{} at {}",
                stringify!($name),
                crate::engine::mouth::Mouth::current_sourceref(e.mouth).display(e.filesystem)
            ))
        );
        let refs = $engine.get_engine_refs();
        refs.state.register_primitive(refs.aux,stringify!($name),command);
    }};
}

macro_rules! cmstodo {
    ($engine:ident,$name:ident) => {{
        let command = crate::commands::PrimitiveCommand::Unexpandable {
            scope:crate::commands::CommandScope::Any,
            apply:|e,_| e.general_error(format!("Not yet implemented: \\{} at {}",
                stringify!($name),
                crate::engine::mouth::Mouth::current_sourceref(e.mouth).display(e.filesystem)
            ))
        };
        let refs = $engine.get_engine_refs();
        refs.state.register_primitive(refs.aux,stringify!($name),command);
    }};
}

pub(crate) use cmtodos;
pub(crate) use cmstodos;
pub(crate) use cmtodo;
pub(crate) use cmstodo;
use crate::utils::errors::TeXResult;

/// Creates a new expandable primitive and registers it with the engine.
pub fn register_expandable<E:TeXEngine>(
    engine:&mut E,
    name:&'static str,
    f:fn(&mut EngineReferences<E::Types>,&mut Vec<<E::Types as EngineTypes>::Token>,<E::Types as EngineTypes>::Token) -> TeXResult<(),E::Types>) {
    let command = PrimitiveCommand::Expandable(f);
    let refs = engine.get_engine_refs();
    refs.state.register_primitive(refs.aux,name,command);
}

/// Creates a new simple expandable primitive (which does not produce new tokens) and registers it with the engine.
pub fn register_simple_expandable<E:TeXEngine>(
    engine:&mut E,
    name:&'static str,
    f:fn(&mut EngineReferences<E::Types>,<E::Types as EngineTypes>::Token) -> TeXResult<(),E::Types>) {
    let command = PrimitiveCommand::SimpleExpandable(f);
    let refs = engine.get_engine_refs();
    refs.state.register_primitive(refs.aux,name,command);
}

/// Creates a new conditional primitive and registers it with the engine.
pub fn register_conditional<E:TeXEngine>(
    engine:&mut E,
    name:&'static str,
    f:fn(&mut EngineReferences<E::Types>,<E::Types as EngineTypes>::Token) -> TeXResult<bool,E::Types>) {
    let command = PrimitiveCommand::Conditional(f);
    let refs = engine.get_engine_refs();
    refs.state.register_primitive(refs.aux,name,command);
}

/// Creates a new unexpandable primitive and registers it with the engine.
pub fn register_unexpandable<E:TeXEngine>(
    engine:&mut E,
    name:&'static str,
    scope: CommandScope,
    f:fn(&mut EngineReferences<E::Types>,<E::Types as EngineTypes>::Token) -> TeXResult<(),E::Types>) {
    let command = PrimitiveCommand::Unexpandable { scope, apply:f };
    let refs = engine.get_engine_refs();
    refs.state.register_primitive(refs.aux,name,command)
}

/// Creates a new primitive named integer value and registers it with the engine.
pub fn register_primitive_int<E:TeXEngine>(engine:&mut E,names:&[&'static str]) {
    let refs = engine.get_engine_refs();
    for name in names {
        refs.state.register_primitive(refs.aux,name,PrimitiveCommand::PrimitiveInt);
    }
}

/// Creates a new primitive command that yields (and optionally assigns) an
///integer value and registers it with the engine.
pub fn register_int<E:TeXEngine>(engine:&mut E,name:&'static str,
                                 read:fn(&mut EngineReferences<E::Types>,<E::Types as EngineTypes>::Token) -> TeXResult<<E::Types as EngineTypes>::Int,E::Types>,
    assign:Option<for<'a,'b> fn(&'a mut EngineReferences<'b,E::Types>,<E::Types as EngineTypes>::Token,bool) -> TeXResult<(),E::Types>>
) {
    let refs = engine.get_engine_refs();
    let command = PrimitiveCommand::Int { read,assign };
    refs.state.register_primitive(refs.aux,name,command);
}

/// Creates a new primitive command that yields (and optionally assigns) a
/// dimension value and registers it with the engine.
pub fn register_dim<E:TeXEngine>(engine:&mut E,name:&'static str,
                                 read:fn(&mut EngineReferences<E::Types>,<E::Types as EngineTypes>::Token) -> TeXResult<<E::Types as EngineTypes>::Dim,E::Types>,
                                 assign:Option<for<'a,'b> fn(&'a mut EngineReferences<'b,E::Types>,<E::Types as EngineTypes>::Token,bool) -> TeXResult<(),E::Types>>
) {
    let refs = engine.get_engine_refs();
    let command = PrimitiveCommand::Dim { read,assign };
    refs.state.register_primitive(refs.aux,name,command);
}

/// Creates a new primitive command that yields (and optionally assigns) a
/// skip value and registers it with the engine.
pub fn register_skip<E:TeXEngine>(engine:&mut E,name:&'static str,
                                 read:fn(&mut EngineReferences<E::Types>,<E::Types as EngineTypes>::Token) -> TeXResult<Skip<<E::Types as EngineTypes>::Dim>,E::Types>,
                                 assign:Option<for<'a,'b> fn(&'a mut EngineReferences<'b,E::Types>,<E::Types as EngineTypes>::Token,bool) -> TeXResult<(),E::Types>>
) {
    let refs = engine.get_engine_refs();
    let command = PrimitiveCommand::Skip { read,assign };
    refs.state.register_primitive(refs.aux,name,command);
}

/// Creates a new primitive command that yields (and optionally assigns) a
/// muskip value and registers it with the engine.
pub fn register_muskip<E:TeXEngine>(engine:&mut E,name:&'static str,
                                  read:fn(&mut EngineReferences<E::Types>,<E::Types as EngineTypes>::Token) -> TeXResult<MuSkip<<E::Types as EngineTypes>::MuDim>,E::Types>,
                                  assign:Option<for<'a,'b> fn(&'a mut EngineReferences<'b,E::Types>,<E::Types as EngineTypes>::Token,bool) -> TeXResult<(),E::Types>>
) {
    let refs = engine.get_engine_refs();
    let command = PrimitiveCommand::MuSkip { read,assign };
    refs.state.register_primitive(refs.aux,name,command);
}

/// Creates a new primitive command that yields (and optionally assigns) a
/// font value and registers it with the engine.
pub fn register_font<E:TeXEngine>(engine:&mut E,name:&'static str,
                                    read:fn(&mut EngineReferences<E::Types>,<E::Types as EngineTypes>::Token) -> TeXResult<<E::Types as EngineTypes>::Font,E::Types>,
                                    assign:Option<for<'a,'b> fn(&'a mut EngineReferences<'b,E::Types>,<E::Types as EngineTypes>::Token,bool) -> TeXResult<(),E::Types>>
) {
    let refs = engine.get_engine_refs();
    let command = PrimitiveCommand::FontCmd { read,assign };
    refs.state.register_primitive(refs.aux,name,command);
}

/// Creates a new primitive command that yields a
/// box and registers it with the engine.
pub fn register_box<E:TeXEngine>(engine:&mut E,name:&'static str,
                                  read:fn(&mut EngineReferences<E::Types>,<E::Types as EngineTypes>::Token) -> TeXResult<Either<Option<TeXBox<E::Types>>,BoxInfo<E::Types>>,E::Types>
) {
    let refs = engine.get_engine_refs();
    let command = PrimitiveCommand::Box(read);
    refs.state.register_primitive(refs.aux,name,command);
}

/// Creates a new primitive assignment command.
pub fn register_assignment<E:TeXEngine>(engine:&mut E,name:&'static str,
                                 assign:for<'a,'b> fn(&'a mut EngineReferences<'b,E::Types>,<E::Types as EngineTypes>::Token,bool) -> TeXResult<(),E::Types>
) {
    let refs = engine.get_engine_refs();
    let command = PrimitiveCommand::Assignment(assign);
    refs.state.register_primitive(refs.aux,name,command);
}

/// Creates a new primitive named dimension and registers it with the engine.
pub fn register_primitive_dim<E:TeXEngine>(engine:&mut E,names:&[&'static str]) {
    let refs = engine.get_engine_refs();
    for name in names {
        refs.state.register_primitive(refs.aux,name,PrimitiveCommand::PrimitiveDim);
    }
}

/// Creates a new primitive named skip and registers it with the engine.
pub fn register_primitive_skip<E:TeXEngine>(engine:&mut E,names:&[&'static str]) {
    let refs = engine.get_engine_refs();
    for name in names {
        refs.state.register_primitive(refs.aux,name,PrimitiveCommand::PrimitiveSkip);
    }
}

/// Creates a new primitive named skip and registers it with the engine.
pub fn register_primitive_muskip<E:TeXEngine>(engine:&mut E,names:&[&'static str]) {
    let refs = engine.get_engine_refs();
    for name in names {
        refs.state.register_primitive(refs.aux,name,PrimitiveCommand::PrimitiveMuSkip);
    }
}

/// Creates a new primitive named token register and registers it with the engine.
pub fn register_primitive_toks<E:TeXEngine>(engine:&mut E,names:&[&'static str]) {
    let refs = engine.get_engine_refs();
    for name in names {
        refs.state.register_primitive(refs.aux,name,PrimitiveCommand::PrimitiveToks);
    }
}

/// Creates a new "Whatsit" primitive and registers it with the engine.
pub fn register_whatsit<E:TeXEngine>(
    engine:&mut E,
    name:&'static str,
    get:fn(&mut EngineReferences<E::Types>, <E::Types as EngineTypes>::Token)
             -> TeXResult<Option<Box<dyn FnOnce(&mut EngineReferences<E::Types>) -> TeXResult<(),E::Types>>>,E::Types>,
    immediate:fn(&mut EngineReferences<E::Types>,<E::Types as EngineTypes>::Token) -> TeXResult<(),E::Types>,
    the:Option<fn(&mut EngineReferences<E::Types>,<E::Types as EngineTypes>::Token) -> TeXResult<Vec<<E::Types as EngineTypes>::Token>,E::Types>>
) {
    let command = PrimitiveCommand::Whatsit { get,immediate, the };
    let refs = engine.get_engine_refs();
    refs.state.register_primitive(refs.aux,name,command);
}

/// A store for all primitive commands.
#[derive(Clone)]
pub struct PrimitiveCommands<ET:EngineTypes> {
    commands: Vec<TeXCommand<ET>>,
    names:HMap<&'static str,u16>
}
impl<ET:EngineTypes> PrimitiveCommands<ET> {
    /// Creates a new store for primitive commands. Usually kept in an engine's [`State`] component.
    pub fn new() -> Self {
        Self {
            commands: Vec::new(),
            names: HMap::default()
        }
    }
    /// Registers a new primitive command.
    pub fn register(&mut self,name:&'static str,cmd: PrimitiveCommand<ET>) -> PrimitiveIdentifier {
        let id = PRIMITIVES.new_id(name);
        let idx = id.as_u16() as usize;
        if idx >= self.commands.len() {
            self.commands.resize(idx+1, TeXCommand::Primitive{name:id,cmd:PrimitiveCommand::Relax});
        }
        self.commands[idx] = TeXCommand::Primitive{name:id,cmd};
        self.names.insert(name,idx as u16);
        id
    }
    /// Return the primitive command with the given identifier.
    pub fn get_id(&self,id:PrimitiveIdentifier) -> Option<&TeXCommand<ET>> {
        let idx = id.as_u16() as usize;
        self.commands.get(idx)
    }
    /// Return the [`PrimitiveIdentifier`] of the primitive command with the given name.
    pub fn get_name(&self,s:&str) -> Option<PrimitiveIdentifier> {
        self.names.get(s).and_then(|&u| PrimitiveIdentifier::try_from_u16(u))
    }
}

/// We always intern the names for primitive commands/macros, for efficiency; in particular for equality checks.
/// Uses `u16` internally, i.e. allowing for up to 65536 primitives.
///
/// It is never necessary to instantiate a new [`PrimitiveInterner`]; instead, use the global [`PRIMITIVES`](static@PRIMITIVES) instance.
pub struct PrimitiveInterner {
    interner:RwLock<string_interner::StringInterner<string_interner::backend::StringBackend<string_interner::symbol::SymbolU16>, ahash::RandomState>>,
    pub globaldefs:PrimitiveIdentifier,
    pub relax:PrimitiveIdentifier,
    pub mag:PrimitiveIdentifier,
    pub fam:PrimitiveIdentifier,
    pub ifcase:PrimitiveIdentifier,
    pub tracingifs:PrimitiveIdentifier,
    pub tracingassigns:PrimitiveIdentifier,
    pub tracingcommands:PrimitiveIdentifier,
    pub tracinggroups:PrimitiveIdentifier,
    pub tracingrestores:PrimitiveIdentifier,
    pub r#else:PrimitiveIdentifier,
    pub fi:PrimitiveIdentifier,
    pub or:PrimitiveIdentifier,
    pub global:PrimitiveIdentifier,
    pub long:PrimitiveIdentifier,
    pub outer:PrimitiveIdentifier,
    pub protected:PrimitiveIdentifier,
    pub def:PrimitiveIdentifier,
    pub edef:PrimitiveIdentifier,
    pub xdef:PrimitiveIdentifier,
    pub gdef:PrimitiveIdentifier,
    pub everyeof:PrimitiveIdentifier,
    pub everyhbox:PrimitiveIdentifier,
    pub everyvbox:PrimitiveIdentifier,
    pub everyjob:PrimitiveIdentifier,
    pub count:PrimitiveIdentifier,
    pub noexpand:PrimitiveIdentifier,
    pub unexpanded:PrimitiveIdentifier,
    pub endcsname:PrimitiveIdentifier,
    pub the:PrimitiveIdentifier,
    pub toks:PrimitiveIdentifier,
    pub vsize:PrimitiveIdentifier,
    pub output:PrimitiveIdentifier,
    pub badness:PrimitiveIdentifier,
    pub outputpenalty:PrimitiveIdentifier,
    pub dimen:PrimitiveIdentifier,
    pub skip:PrimitiveIdentifier,
    pub everypar:PrimitiveIdentifier,
    pub indent:PrimitiveIdentifier,
    pub noindent:PrimitiveIdentifier,
    pub hangindent:PrimitiveIdentifier,
    pub hangafter:PrimitiveIdentifier,
    pub leftskip:PrimitiveIdentifier,
    pub rightskip:PrimitiveIdentifier,
    pub hsize:PrimitiveIdentifier,
    pub pdfpagewidth:PrimitiveIdentifier,
    pub everymath:PrimitiveIdentifier,
    pub everydisplay:PrimitiveIdentifier,
    pub char:PrimitiveIdentifier,
    pub tabskip:PrimitiveIdentifier,
    pub cr:PrimitiveIdentifier,
    pub crcr:PrimitiveIdentifier,
    pub everycr:PrimitiveIdentifier,
    pub span:PrimitiveIdentifier,
    pub noalign:PrimitiveIdentifier,
    pub omit:PrimitiveIdentifier,
    pub baselineskip:PrimitiveIdentifier,
    pub lineskip:PrimitiveIdentifier,
    pub lineskiplimit:PrimitiveIdentifier,
    pub parindent:PrimitiveIdentifier,
    pub hrule:PrimitiveIdentifier,
    pub vrule:PrimitiveIdentifier,
    pub vskip:PrimitiveIdentifier,
    pub hskip:PrimitiveIdentifier,
    pub vfil:PrimitiveIdentifier,
    pub hfil:PrimitiveIdentifier,
    pub vfill:PrimitiveIdentifier,
    pub hfill:PrimitiveIdentifier,
    pub parskip:PrimitiveIdentifier,
    pub delimiter:PrimitiveIdentifier,
    pub abovedisplayskip:PrimitiveIdentifier,
    pub belowdisplayskip:PrimitiveIdentifier,
    pub iffalse:PrimitiveIdentifier,
    pub iftrue:PrimitiveIdentifier,
    pub year:PrimitiveIdentifier,
    pub month:PrimitiveIdentifier,
    pub day:PrimitiveIdentifier,
    pub time:PrimitiveIdentifier,
}
impl PrimitiveInterner {
    fn new() -> Self {
        let mut interner = string_interner::StringInterner::<string_interner::backend::StringBackend<string_interner::symbol::SymbolU16>, ahash::RandomState>::new();
        let globaldefs = PrimitiveIdentifier(interner.get_or_intern_static("globaldefs"));
        let relax = PrimitiveIdentifier(interner.get_or_intern_static("relax"));
        let mag = PrimitiveIdentifier(interner.get_or_intern_static("mag"));
        let fam = PrimitiveIdentifier(interner.get_or_intern_static("fam"));
        let ifcase = PrimitiveIdentifier(interner.get_or_intern_static("ifcase"));
        let tracingifs = PrimitiveIdentifier(interner.get_or_intern_static("tracingifs"));
        let tracingassigns = PrimitiveIdentifier(interner.get_or_intern_static("tracingassigns"));
        let tracingcommands = PrimitiveIdentifier(interner.get_or_intern_static("tracingcommands"));
        let tracinggroups = PrimitiveIdentifier(interner.get_or_intern_static("tracinggroups"));
        let tracingrestores = PrimitiveIdentifier(interner.get_or_intern_static("tracingrestores"));
        let r#else = PrimitiveIdentifier(interner.get_or_intern_static("else"));
        let fi = PrimitiveIdentifier(interner.get_or_intern_static("fi"));
        let or = PrimitiveIdentifier(interner.get_or_intern_static("or"));
        let global = PrimitiveIdentifier(interner.get_or_intern_static("global"));
        let long = PrimitiveIdentifier(interner.get_or_intern_static("long"));
        let outer = PrimitiveIdentifier(interner.get_or_intern_static("outer"));
        let protected = PrimitiveIdentifier(interner.get_or_intern_static("protected"));
        let def = PrimitiveIdentifier(interner.get_or_intern_static("def"));
        let edef = PrimitiveIdentifier(interner.get_or_intern_static("edef"));
        let xdef = PrimitiveIdentifier(interner.get_or_intern_static("xdef"));
        let gdef = PrimitiveIdentifier(interner.get_or_intern_static("gdef"));
        let everyeof = PrimitiveIdentifier(interner.get_or_intern_static("everyeof"));
        let everyhbox = PrimitiveIdentifier(interner.get_or_intern_static("everyhbox"));
        let everyvbox = PrimitiveIdentifier(interner.get_or_intern_static("everyvbox"));
        let everyjob = PrimitiveIdentifier(interner.get_or_intern_static("everyjob"));
        let count = PrimitiveIdentifier(interner.get_or_intern_static("count"));
        let noexpand = PrimitiveIdentifier(interner.get_or_intern_static("noexpand"));
        let endcsname = PrimitiveIdentifier(interner.get_or_intern_static("endcsname"));
        let unexpanded = PrimitiveIdentifier(interner.get_or_intern_static("unexpanded"));
        let the = PrimitiveIdentifier(interner.get_or_intern_static("the"));
        let toks = PrimitiveIdentifier(interner.get_or_intern_static("toks"));
        let vsize = PrimitiveIdentifier(interner.get_or_intern_static("vsize"));
        let output = PrimitiveIdentifier(interner.get_or_intern_static("output"));
        let badness = PrimitiveIdentifier(interner.get_or_intern_static("badness"));
        let outputpenalty = PrimitiveIdentifier(interner.get_or_intern_static("outputpenalty"));
        let dimen = PrimitiveIdentifier(interner.get_or_intern_static("dimen"));
        let skip = PrimitiveIdentifier(interner.get_or_intern_static("skip"));
        let everypar = PrimitiveIdentifier(interner.get_or_intern_static("everypar"));
        let indent = PrimitiveIdentifier(interner.get_or_intern_static("indent"));
        let noindent = PrimitiveIdentifier(interner.get_or_intern_static("noindent"));
        let hangindent = PrimitiveIdentifier(interner.get_or_intern_static("hangindent"));
        let hangafter = PrimitiveIdentifier(interner.get_or_intern_static("hangafter"));
        let leftskip = PrimitiveIdentifier(interner.get_or_intern_static("leftskip"));
        let rightskip = PrimitiveIdentifier(interner.get_or_intern_static("rightskip"));
        let hsize = PrimitiveIdentifier(interner.get_or_intern_static("hsize"));
        let pdfpagewidth = PrimitiveIdentifier(interner.get_or_intern_static("pdfpagewidth"));
        let everymath = PrimitiveIdentifier(interner.get_or_intern_static("everymath"));
        let everydisplay = PrimitiveIdentifier(interner.get_or_intern_static("everydisplay"));
        let char = PrimitiveIdentifier(interner.get_or_intern_static("char"));
        let tabskip = PrimitiveIdentifier(interner.get_or_intern_static("tabskip"));
        let cr = PrimitiveIdentifier(interner.get_or_intern_static("cr"));
        let crcr = PrimitiveIdentifier(interner.get_or_intern_static("crcr"));
        let everycr = PrimitiveIdentifier(interner.get_or_intern_static("everycr"));
        let span = PrimitiveIdentifier(interner.get_or_intern_static("span"));
        let noalign = PrimitiveIdentifier(interner.get_or_intern_static("noalign"));
        let omit = PrimitiveIdentifier(interner.get_or_intern_static("omit"));
        let baselineskip = PrimitiveIdentifier(interner.get_or_intern_static("baselineskip"));
        let lineskip = PrimitiveIdentifier(interner.get_or_intern_static("lineskip"));
        let lineskiplimit = PrimitiveIdentifier(interner.get_or_intern_static("lineskiplimit"));
        let parindent = PrimitiveIdentifier(interner.get_or_intern_static("parindent"));
        let hrule = PrimitiveIdentifier(interner.get_or_intern_static("hrule"));
        let vrule = PrimitiveIdentifier(interner.get_or_intern_static("vrule"));
        let vskip = PrimitiveIdentifier(interner.get_or_intern_static("vskip"));
        let hskip = PrimitiveIdentifier(interner.get_or_intern_static("hskip"));
        let vfil = PrimitiveIdentifier(interner.get_or_intern_static("vfil"));
        let hfil = PrimitiveIdentifier(interner.get_or_intern_static("hfil"));
        let vfill = PrimitiveIdentifier(interner.get_or_intern_static("vfill"));
        let hfill = PrimitiveIdentifier(interner.get_or_intern_static("hfill"));
        let parskip = PrimitiveIdentifier(interner.get_or_intern_static("parskip"));
        let delimiter = PrimitiveIdentifier(interner.get_or_intern_static("delimiter"));
        let abovedisplayskip = PrimitiveIdentifier(interner.get_or_intern_static("abovedisplayskip"));
        let belowdisplayskip = PrimitiveIdentifier(interner.get_or_intern_static("belowdisplayskip"));
        let iffalse = PrimitiveIdentifier(interner.get_or_intern_static("iffalse"));
        let iftrue = PrimitiveIdentifier(interner.get_or_intern_static("iftrue"));
        let year = PrimitiveIdentifier(interner.get_or_intern_static("year"));
        let month = PrimitiveIdentifier(interner.get_or_intern_static("month"));
        let day = PrimitiveIdentifier(interner.get_or_intern_static("day"));
        let time = PrimitiveIdentifier(interner.get_or_intern_static("time"));
        PrimitiveInterner{
            interner:RwLock::new(interner),
            globaldefs, relax, mag, fam, ifcase, tracingifs, tracingassigns, tracingcommands,
            tracinggroups, r#else, fi, or, global, long, outer, protected, def, edef, xdef,
            gdef,everyeof,count,tracingrestores,noexpand,endcsname,unexpanded,the,toks,
            everyhbox,everyvbox,everyjob,vsize,output,badness,outputpenalty,dimen,skip,
            everypar,indent,noindent,hangindent,hangafter,leftskip,rightskip,hsize,
            pdfpagewidth,everymath,everydisplay,char,tabskip,cr,crcr,everycr,span,
            noalign,omit,baselineskip,lineskip,lineskiplimit,parindent,hrule,vrule,
            vskip,hskip,vfil,hfil,vfill,hfill,parskip,delimiter,abovedisplayskip,
            belowdisplayskip,iffalse,iftrue,year,month,day,time
        }
    }

    fn new_id(&self, s:&'static str) -> PrimitiveIdentifier {
        let mut lock = self.interner.write().unwrap();
        PrimitiveIdentifier(lock.get_or_intern_static(s))
    }

}
lazy_static!(
    /// The global [`PrimitiveInterner`].
    pub static ref PRIMITIVES:PrimitiveInterner = PrimitiveInterner::new();
);

/// A `Copy` identifier for a primitive command. Small and fast to compare.
#[derive(Copy,Clone,PartialEq,Eq,Hash,Debug)]
pub struct PrimitiveIdentifier(string_interner::symbol::SymbolU16);
impl PrimitiveIdentifier {
    /// Returns a struct implementing [`Display`](std::fmt::Display) for the given [`PrimitiveIdentifier`], and
    /// optional `\escapechar` that will be prefixed - e.g.
    /// `println!(`[`PRIMITIVES`](static@PRIMITIVES)`.the.`[`display`](Self::display)`(Some('\\'))`
    /// will print `\the`.
    pub fn display<C:Character>(self,escapechar:Option<C>) -> impl std::fmt::Display {
        PrintableIdentifier(self,escapechar)
    }
    /// Returns the `u16` value of the identifier.
    pub fn as_u16(&self) -> u16 {
        use string_interner::Symbol;
        self.0.to_usize() as u16
    }
    /// Returns the identifier for the given `u16` value, if it exists.
    pub fn try_from_u16(u:u16) -> Option<Self> {
        use string_interner::Symbol;
        string_interner::symbol::SymbolU16::try_from_usize(u as usize).map(PrimitiveIdentifier)
    }
}

struct PrintableIdentifier<C:Character>(PrimitiveIdentifier,Option<C>);
impl<C:Character> std::fmt::Display for PrintableIdentifier<C> {
    fn fmt(&self,f:&mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let lock = PRIMITIVES.interner.read().unwrap();
        match self.1 {
            None => (),
            Some(c) => c.display_fmt(f)
        }
        write!(f,"{}",lock.resolve(self.0.0).unwrap())
    }
}