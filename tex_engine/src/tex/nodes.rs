/*! [TeX Nodes](NodeTrait) that end up in the final document.

Split into three types: [vertical](vertical::VNode), [horizontal](horizontal::HNode) and [math](math::MathNode)
nodes, each with a corresponding [`NodeList`] type.
*/
pub mod vertical;
pub mod horizontal;
pub mod math;
pub mod boxes;

use std::cell::RefCell;
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

    /// Produces a `\n` followed by `indent`-many spaces - i.e. does the indentation for
    /// [`Self::display_fmt`].
    fn readable_do_indent(indent:usize,f:&mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char('\n')?;
        for _ in 0..indent {f.write_char(' ')?;}
        Ok(())
    }
    /// Returns a helper struct that implements [`Display`] and uses [`Self::display_fmt`]
    /// to yield a human-readable string.
    fn display(&self) -> ReadableNode<ET,Self> where Self:Sized {
        ReadableNode(self,PhantomData)
    }
    /// Whether this node is "opaque"; meaning: When considering a list of nodes (e.g. in `\unskip`
    /// or `\lastbox`, this node should not be considered. Useful for annotation/marker nodes
    /// some engine wants to insert, without impacting algorithms that inspect e.g. the last node
    /// of the current list.
    fn opaque(&self) -> bool { false }
}

/// Helper struct that implements [`Display`] and uses [`NodeTrait::display_fmt`] to yield a
/// human-readable string.
pub struct ReadableNode<'a,ET:EngineTypes,N: NodeTrait<ET>>(&'a N, PhantomData<ET>);
impl<'a,ET:EngineTypes,N: NodeTrait<ET>> Display for ReadableNode<'a,ET,N> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.display_fmt(0, f)
    }
}

/// Trait to implement for engine-specific new node types. Needs to implement [`NodeTrait`]
/// and [`Into`]`<ET::CustomNode>`. Is implemented by `()` for engines that do not have
/// additional node types beyond the default ones.
pub trait CustomNodeTrait<ET:EngineTypes>:NodeTrait<ET> where Self:Into<ET::CustomNode> {
    /// Return this node as a [`VNode`].
    fn as_v(self) -> VNode<ET> { VNode::Custom(self.into()) }
    /// Return this node as an [`HNode`].
    fn as_h(self) -> HNode<ET> { HNode::Custom(self.into()) }
    /// Return this node as a [`MathNode`].
    fn as_math(self) -> MathNode<ET,UnresolvedMathFontStyle<ET>> { MathNode::Custom(self.into()) }
}

impl<ET:EngineTypes<CustomNode = ()>> NodeTrait<ET> for () {
    fn height(&self) -> ET::Dim { ET::Dim::default() }
    fn depth(&self) -> ET::Dim { ET::Dim::default() }
    fn width(&self) -> ET::Dim { ET::Dim::default() }
    fn nodetype(&self) -> NodeType { NodeType::WhatsIt }
    fn display_fmt(&self, _indent:usize, _f:&mut std::fmt::Formatter<'_>) -> std::fmt::Result { Ok(()) }
    fn opaque(&self) -> bool { true }
}
impl<ET:EngineTypes<CustomNode = ()>> CustomNodeTrait<ET> for () {}


type WhatsitFunction<ET> = Ptr<RefCell<Option<Box<dyn FnOnce(&mut EngineReferences<ET>)>>>>;
/// A Whatsit [node](NodeTrait), essentially representing a callback to the engine to be executed
/// at shipout, as produced by e.g. `\special` or `\write`.
#[derive(Clone)]
pub struct WhatsitNode<ET:EngineTypes>(String,WhatsitFunction<ET>);
impl<ET:EngineTypes> std::fmt::Debug for WhatsitNode<ET> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f,"<whatsit {}>",self.0)
    }
}
impl<ET:EngineTypes> WhatsitNode<ET> {
    /// Create a new Whatsit node produce by the primitive
    /// [Whatsit command](crate::commands::PrimitiveCommand::Whatsit) with the given name,
    /// with the function provided, to be executed at shipout.
    pub fn new(f:Box<dyn FnOnce(&mut EngineReferences<ET>)>, name:PrimitiveIdentifier) -> Self {
        WhatsitNode(name.display::<ET::Char>(None).to_string(),
                    Ptr::new(RefCell::new(Some(f)))
        )
    }
    /// Run this Whatsit node's function at shipout, if it has not been run yet.
    /// If it has been run already, this is a no-op.
    pub fn call(self,engine: &mut EngineReferences<ET>) {
        if let Some(f) = self.1.replace(None) {
            f(engine);
        }
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
        Self::readable_do_indent(indent,f)?;
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
        Self::readable_do_indent(indent,f)?;
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
        match self {
            HSkip(_) | HFil | HFill => true,
            _ => false
        }
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
        NodeList::Math{children:MathNodeList::new(),start,tp:MathNodeListType::Target(ListTarget::none())}
    }
}

/// Once a box is closed, something is supposed to happen to it; most commonly, it is just added to the parent
/// node list; but occasionally, something else should happen to it - e.g. `\setbox5\hbox{...}` implies that
/// after the node list of the box is closed, it should be put into the box register with index 5 instead.
/// This struct abstracts over that by carrying a continuation function to be called when the box is closed.
///
/// TODO: rethink this in light of [`ListTarget`]. `.clone()` will not clone the function itself
pub struct BoxTarget<ET:EngineTypes>(Option<Box<dyn FnOnce(&mut EngineReferences<ET>,TeXBox<ET>)>>);
impl<ET:EngineTypes> crate::tex::nodes::BoxTarget<ET> {
    /// Create a new box target from the given continuation function.
    pub fn new<F:FnOnce(&mut EngineReferences<ET>,TeXBox<ET>) + 'static>(f:F) -> Self { crate::tex::nodes::BoxTarget(Some(Box::new(f))) }
    /// Execute the continuation function (once the box is closed)
    pub fn call(self,engine: &mut EngineReferences<ET>,bx:TeXBox<ET>) {
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
/// TODO: rethink this in light of [`BoxTarget`]. `.clone()` will not clone the function itself
pub struct ListTarget<ET:EngineTypes,N:NodeTrait<ET>>(Option<Box<dyn FnOnce(&mut EngineReferences<ET>,Vec<N>,SourceRef<ET>)>>);
impl<ET:EngineTypes,N:NodeTrait<ET>> ListTarget<ET,N> {
    /// Create a new list target from the given continuation function.
    pub fn new<F:FnOnce(&mut EngineReferences<ET>,Vec<N>,SourceRef<ET>) + 'static>(f:F) -> Self { ListTarget(Some(Box::new(f))) }
    /// Execute the continuation function (once the list is closed)
    pub fn call(self,engine: &mut EngineReferences<ET>,v:Vec<N>,start:SourceRef<ET>) {
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
impl<ET:EngineTypes,N:NodeTrait<ET>> Debug for ListTarget<ET,N> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            None => f.write_str("ListTarget(None)"),
            Some(_) => f.write_str("ListTarget(Some(_))")
        }
    }
}
impl<ET:EngineTypes,N:NodeTrait<ET>> Clone for ListTarget<ET,N> {
    fn clone(&self) -> Self { ListTarget(None) }
}
