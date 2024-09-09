/*! [TeX Nodes](NodeTrait) that end up in the final document.

Split into three types: [vertical](vertical::VNode), [horizontal](horizontal::HNode) and [math](math::MathNode)
nodes, each with a corresponding [`NodeList`] type.
*/
pub mod vertical;
pub mod horizontal;
pub mod math;
pub mod boxes;

use std::convert::Infallible;
use std::fmt::{Debug, Display, Formatter, Write};
use std::marker::PhantomData;
use crate::commands::primitives::PrimitiveIdentifier;
use crate::engine::{EngineReferences, EngineTypes};
use crate::engine::filesystem::SourceRef;
use crate::tex::nodes::boxes::TeXBox;
use crate::tex::nodes::horizontal::{HNode, HorizontalNodeListType};
use crate::tex::nodes::math::{MathNode, MathNodeList, MathNodeListType, UnresolvedMathFontStyle};
use crate::tex::nodes::vertical::{VerticalNodeListType, VNode};
use crate::tex::numerics::Skip;
use crate::utils::errors::TeXResult;
use crate::utils::Ptr;

/// Common trait for all nodes that end up in the final document.
pub trait NodeTrait<ET:EngineTypes>:Debug+Clone {
    /// Returns the height of the node.
    fn height(&self) -> ET::Dim;
    /// Returns the depth of the node.
    fn depth(&self) -> ET::Dim;
    /// Returns the width of the node.
    fn width(&self) -> ET::Dim;
    /// Returns the type of the node, as returned by `\lastnodetype`.
    fn nodetype(&self) -> NodeType;
    /// Produces a human-readable string; since nodes are deeply nested, takes an
    /// additional `indent` value to indent the string
    fn display_fmt(&self, indent:usize, f:&mut std::fmt::Formatter<'_>) -> std::fmt::Result;

    /// Returns a helper struct that implements [`Display`] and uses [`Self::display_fmt`]
    /// to yield a human-readable string.
    fn display(&self) -> DisplayNode<ET,Self> { DisplayNode(self, PhantomData) }
    /// Whether this node is "opaque"; meaning: When considering a list of nodes (e.g. in `\unskip`
    /// or `\lastbox`, this node should not be considered. Useful for annotation/marker nodes
    /// some engine wants to insert, without impacting algorithms that inspect e.g. the last node
    /// of the current list.
    fn opaque(&self) -> bool { false }
    fn sourceref(&self) -> Option<(&SourceRef<ET>,&SourceRef<ET>)> { None }
}

/// Produces a `\n` followed by `indent`-many spaces - i.e. does the indentation for
/// [`crate::tex::nodes::NodeTrait::display_fmt`].
pub fn display_do_indent(indent:usize, f:&mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.write_char('\n')?;
    for _ in 0..indent {f.write_char(' ')?;}
    Ok(())
}

/// Helper struct that implements [`Display`] and uses [`NodeTrait::display_fmt`] to yield a
/// human-readable string.
pub struct DisplayNode<'a,ET:EngineTypes,N: NodeTrait<ET>>(&'a N, PhantomData<ET>);
impl<'a,ET:EngineTypes,N: NodeTrait<ET>> Display for DisplayNode<'a,ET,N> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.display_fmt(0, f)
    }
}

/// Trait to implement for engine-specific new node types. Needs to implement [`NodeTrait`]
/// and [`Into`]`<`[`ET::CustomNode`](EngineTypes::CustomNode)`>`. Is implemented by `()` for engines that do not have
/// additional node types beyond the default ones.
pub trait CustomNodeTrait<ET:EngineTypes>:NodeTrait<ET> where Self:Into<ET::CustomNode> {
    /// Return this node as a [`VNode`].
    fn into_v(self) -> VNode<ET> { VNode::Custom(self.into()) }
    /// Return this node as an [`HNode`].
    fn into_h(self) -> HNode<ET> { HNode::Custom(self.into()) }
    /// Return this node as a [`MathNode`].
    fn into_math(self) -> MathNode<ET,UnresolvedMathFontStyle<ET>> { MathNode::Custom(self.into()) }
}

impl<ET:EngineTypes<CustomNode = Infallible>> NodeTrait<ET> for Infallible {
    fn height(&self) -> ET::Dim { ET::Dim::default() }
    fn depth(&self) -> ET::Dim { ET::Dim::default() }
    fn width(&self) -> ET::Dim { ET::Dim::default() }
    fn nodetype(&self) -> NodeType { NodeType::WhatsIt }
    fn display_fmt(&self, _indent:usize, _f:&mut std::fmt::Formatter<'_>) -> std::fmt::Result { Ok(()) }
    fn opaque(&self) -> bool { true }
}
impl<ET:EngineTypes<CustomNode = Infallible>> CustomNodeTrait<ET> for Infallible {}

#[cfg(feature="multithreaded")]
pub type WhatsitFunction<ET> = dyn FnOnce(&mut EngineReferences<ET>) -> TeXResult<(),ET> + Send + Sync;
#[cfg(not(feature="multithreaded"))]
pub type WhatsitFunction<ET> = dyn FnOnce(&mut EngineReferences<ET>) -> TeXResult<(),ET>;

#[cfg(feature="multithreaded")]
type WhatsitF<ET> = Ptr<std::sync::RwLock<Option<Box<WhatsitFunction<ET>>>>>;
#[cfg(not(feature="multithreaded"))]
type WhatsitF<ET> = Ptr<std::cell::RefCell<Option<Box<WhatsitFunction<ET>>>>>;
/// A Whatsit [node](NodeTrait), essentially representing a callback to the engine to be executed
/// at shipout, as produced by e.g. `\special` or `\write`.
#[derive(Clone)]
pub struct WhatsitNode<ET:EngineTypes>(String, WhatsitF<ET>);
impl<ET:EngineTypes> std::fmt::Debug for WhatsitNode<ET> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f,"<whatsit {}>",self.0)
    }
}
impl<ET:EngineTypes> WhatsitNode<ET> {
    /// Create a new Whatsit node produce by the primitive
    /// [Whatsit command](crate::commands::PrimitiveCommand::Whatsit) with the given name,
    /// with the function provided, to be executed at shipout.
    pub fn new(f:Box<WhatsitFunction<ET>>, name:PrimitiveIdentifier) -> Self {
        #[cfg(feature="multithreaded")]
        let i = std::sync::RwLock::new(Some(f));
        #[cfg(not(feature="multithreaded"))]
        let i = std::cell::RefCell::new(Some(f));
        WhatsitNode(name.display::<ET::Char>(None).to_string(), Ptr::new(i))
    }
    /// Run this Whatsit node's function at shipout, if it has not been run yet.
    /// If it has been run already, this is a no-op.

    #[cfg(feature="multithreaded")]
    pub fn call(self,engine: &mut EngineReferences<ET>) -> TeXResult<(),ET> {
        let mut lock = self.1.write().unwrap();
        if let Some(f) = std::mem::take(&mut *lock) {
            f(engine)
        } else {Ok(())}
    }
    #[cfg(not(feature="multithreaded"))]
    pub fn call(self,engine: &mut EngineReferences<ET>) -> TeXResult<(),ET> {
        if let Some(f) = self.1.replace(None) {
            f(engine)
        } else {Ok(())}
    }
}

/// A `\leaders` node, as produced by `\leaders`, `\cleaders` or `\xleaders`.
#[derive(Clone,Debug)]
pub struct Leaders<ET:EngineTypes> {
    /// `\leaders`, `\cleaders` or `\xleaders`.
    pub tp:LeaderType,
    /// The node to be repeated for the given skip length; a box or a rule.
    pub body:LeaderBody<ET>,
    /// The skip length along which the body should be repeated.
    pub skip:LeaderSkip<ET>,
}
impl<ET:EngineTypes> NodeTrait<ET> for Leaders<ET> {
    fn display_fmt(&self, indent: usize, f: &mut Formatter<'_>) -> std::fmt::Result {
        display_do_indent(indent,f)?;
        write!(f, "<leaders")?;
        match self.tp {
            LeaderType::Normal => {},
            LeaderType::C => write!(f, " type=c")?,
            LeaderType::X => write!(f, " type=x")?
        }
        match self.skip {
            LeaderSkip::HSkip(s) => write!(f, " hskip={}",s)?,
            LeaderSkip::HFil => write!(f, " hfil")?,
            LeaderSkip::HFill => write!(f, " hfill")?,
            LeaderSkip::VSkip(s) => write!(f, " vskip={}",s)?,
            LeaderSkip::VFil => write!(f, " vfil")?,
            LeaderSkip::VFill => write!(f, " vfill")?,
        }
        write!(f, ">")?;
        //self.bx.readable_fmt(indent+2, f)?;
        display_do_indent(indent,f)?;
        write!(f, "</leaders>")
    }
    fn height(&self) -> ET::Dim {
        if self.skip.is_h() {
            ET::Dim::default()//self.bx.height()
        } else {
            match self.skip {
                LeaderSkip::VSkip(s) => s.base,
                _ => ET::Dim::default(),
            }
        }
    }
    fn width(&self) -> ET::Dim {
        if self.skip.is_h() {
            match self.skip {
                LeaderSkip::HSkip(s) => s.base,
                _ => ET::Dim::default(),
            }
        } else {
            ET::Dim::default()//self.bx.width()
        }
    }
    fn depth(&self) -> ET::Dim {
        ET::Dim::default()
        //self.bx.depth()
    }
    fn nodetype(&self) -> NodeType {
        NodeType::Glue
    }
}

/// The type of a [`Leaders`] node
/// (i.e. whether it was produced by `\leaders`, `\cleaders` or `\xleaders`).
#[derive(Debug,Clone,Copy,PartialEq,Eq)]
pub enum LeaderType {
    Normal,C,X
}

/// The body of a [`Leaders`] node;
/// either a box or a rule.
#[derive(Clone,Debug)]
pub enum LeaderBody<ET:EngineTypes> {
    Box(TeXBox<ET>),
    Rule { width:Option<ET::Dim>, height:Option<ET::Dim>, depth:Option<ET::Dim> }
}

/// The skip length along which a [`Leaders`] node
/// should be repeated.
#[derive(Clone,Debug)]
pub enum LeaderSkip<ET:EngineTypes> {
    HSkip(Skip<ET::Dim>),HFil,HFill,
    VSkip(Skip<ET::Dim>), VFil,VFill
}
impl<ET:EngineTypes> LeaderSkip<ET> {
    /// Whether this is a horizontal skip.
    pub fn is_h(&self) -> bool {
        use LeaderSkip::*;
        matches!(self, HSkip(_) | HFil | HFill)
    }
}

/// The type of a [`Node`](crate::tex::nodes::NodeTrait), as returned
/// by `\lastnodetype`
#[derive(Clone,Copy,Eq,PartialEq,Debug)]
pub enum NodeType {
    /// A character node, e.g. `a` or `1`.
    Char = 0,
    /// A horizontal list node, e.g. `\hbox{...}`.
    HList = 1,
    /// A vertical list node, e.g. `\vbox{...}`.
    VList = 2,
    /// A rule node, e.g. `\hrule`.
    Rule = 3,
    /// An insertion node as produced by `\insert`.
    Insertion = 4,
    /// A mark node as produced by `\mark`.
    Mark = 5,
    /// An adjust node as produced by `\vadjust`.
    Adjust = 6,
    /// A ligature node as produced by the ligaturing algorithm.
    Ligature = 7,
    /// A discretionary node as produced by the discretionary algorithm.
    Discretionary = 8,
    /// A whatsit node, e.g. `\special`.
    WhatsIt = 9,
    /// A math node.
    Math = 10,
    /// A glue node, e.g. `\hskip`.
    Glue = 11,
    /// A kern node, e.g. `\kern`.
    Kern = 12,
    /// A penalty node as produced by `\penalty`.
    Penalty = 13,
    /// An unset node, e.g. `\unskip`.
    Unset = 14,
    /// A math character node, e.g. `a` or `1` in math mode.
    MathChar = 15,
}
impl NodeType {
    /// Returns the numeric value of the node type, as returned by `\lastnodetype`.
    pub fn to_u8(&self) -> u8 {
        *self as u8
    }
}

/// A node list, i.e. an open list of [`Node`](crate::tex::nodes::NodeTrait)s
/// currently being built.
#[derive(Clone,Debug)]
pub enum NodeList<ET:EngineTypes> {
    /// A vertical list.
    Vertical{tp:VerticalNodeListType<ET>,children:Vec<VNode<ET>>},
    /// A horizontal list.
    Horizontal{tp:HorizontalNodeListType<ET>,children:Vec<HNode<ET>>},
    /// A math list.
    Math{children:MathNodeList<ET>,start:SourceRef<ET>,tp:MathNodeListType<ET>},
}
impl<ET:EngineTypes> NodeList<ET> {
    /// Create a new math list.
    pub fn new_math(start:SourceRef<ET>) -> Self {
        NodeList::Math{children:MathNodeList::default(),start,tp:MathNodeListType::Target(ListTarget::none())}
    }
}

/// Once a box is closed, something is supposed to happen to it; most commonly, it is just added to the parent
/// node list; but occasionally, something else should happen to it - e.g. `\setbox5\hbox{...}` implies that
/// after the node list of the box is closed, it should be put into the box register with index 5 instead.
/// This struct abstracts over that by carrying a continuation function to be called when the box is closed.
///
/// TODO: rethink this in light of [`ListTarget`].
pub struct BoxTarget<ET:EngineTypes>(Option<Box<dyn FnOnce(&mut EngineReferences<ET>,TeXBox<ET>) -> TeXResult<(),ET>>>);
impl<ET:EngineTypes> crate::tex::nodes::BoxTarget<ET> {
    /// Create a new box target from the given continuation function.
    pub fn new<F:FnOnce(&mut EngineReferences<ET>,TeXBox<ET>) -> TeXResult<(),ET> + 'static>(f:F) -> Self { crate::tex::nodes::BoxTarget(Some(Box::new(f))) }
    /// Execute the continuation function (once the box is closed)
    pub fn call(self,engine: &mut EngineReferences<ET>,bx:TeXBox<ET>) -> TeXResult<(),ET> {
        match self.0 {
            Some(f) => f(engine,bx),
            None => unreachable!()
        }
    }
    /// Create a trivial box target
    pub fn none() -> Self { crate::tex::nodes::BoxTarget(None) }
    /// Whether the continuation function has not yet been called
    pub fn is_some(&self) -> bool { self.0.is_some() }
}
impl<ET:EngineTypes> Debug for crate::tex::nodes::BoxTarget<ET> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            None => f.write_str("BoxTarget(None)"),
            Some(_) => f.write_str("BoxTarget(Some(_))")
        }
    }
}
impl<ET:EngineTypes> Clone for crate::tex::nodes::BoxTarget<ET> {
    fn clone(&self) -> Self { crate::tex::nodes::BoxTarget(None) }
}

/// Once a node list is closed, something is supposed to happen to it; most commonly, it is a box-list;
/// but occasionally, something else should happen to it - e.g. `\vadjust{...}` adds the list's contents
/// as a vertical adjustment, `a^{...}` adds it to the superscript-field of the `a`.character etc.
/// This struct abstracts over that by carrying a continuation function to be called when the list is closed.
///
/// TODO: rethink this in light of [`BoxTarget`].
pub struct ListTarget<ET:EngineTypes,N>(Option<Box<dyn FnOnce(&mut EngineReferences<ET>,Vec<N>,SourceRef<ET>) -> TeXResult<(),ET>>>);
impl<ET:EngineTypes,N> ListTarget<ET,N> {
    /// Create a new list target from the given continuation function.
    pub fn new<F:FnOnce(&mut EngineReferences<ET>,Vec<N>,SourceRef<ET>) -> TeXResult<(),ET> + 'static>(f:F) -> Self { ListTarget(Some(Box::new(f))) }
    /// Execute the continuation function (once the list is closed)
    pub fn call(self,engine: &mut EngineReferences<ET>,v:Vec<N>,start:SourceRef<ET>) -> TeXResult<(),ET> {
        match self.0 {
            Some(f) => f(engine,v,start),
            None => unreachable!()
        }
    }
    /// Create a trivial list target
    pub fn none() -> Self { ListTarget(None) }
    /// Whether the continuation function has not yet been called
    pub fn is_some(&self) -> bool { self.0.is_some() }
}
impl<ET:EngineTypes,N> Debug for ListTarget<ET,N> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            None => f.write_str("ListTarget(None)"),
            Some(_) => f.write_str("ListTarget(Some(_))")
        }
    }
}
impl<ET:EngineTypes,N> Clone for ListTarget<ET,N> {
    fn clone(&self) -> Self { ListTarget(None) }
}