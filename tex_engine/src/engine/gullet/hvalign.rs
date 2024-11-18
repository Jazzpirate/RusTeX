/*! Data structures for `\halign` and `\valign` */

use crate::commands::methods::{END_TEMPLATE, END_TEMPLATE_ROW};
use crate::commands::primitives::PRIMITIVES;
use crate::engine::mouth::Mouth;
use crate::engine::state::State;
use crate::engine::{EngineAux, EngineTypes};
use crate::prelude::{CommandCode, Token};
use crate::tex::nodes::boxes::BoxType;
use crate::tex::numerics::Skip;
use crate::tex::tokens::control_sequences::CSHandler;

/// Specification on a column in an `\halign` (or a row in a `\valign`); in particular:
/// - the alignment template for the column (or row)
/// - the [tabskip](crate::tex::numerics::Skip) to be inserted between the columns (or rows),
/// - the number of braces to inserted at the beginning of the column (or row).
///
/// The latter is important for the [`Gullet`](crate::engine::gullet::Gullet) to know when an [`AlignmentTab`](crate::tex::catcodes::CommandCode::AlignmentTab)
/// [`Token`] (or a `\cr`) should be passed on or replaced by the relevant template tokens
#[derive(Debug)]
pub struct AlignColumn<T: Token, D: crate::tex::numerics::TeXDimen> {
    /// The tokens to be inserted at the beginning of the column (or row)
    pub left: Box<[T]>,
    /// The tokens to be inserted at the end of the column (or row)
    pub right: Box<[T]>,
    /// The number of braces that are opened by the template tokens
    pub inbraces: u8,
    /// The [tabskip](crate::tex::numerics::Skip) to be inserted between the columns (or rows)
    pub tabskip: Skip<D>,
}
impl<T: Token, D: crate::tex::numerics::TeXDimen> AlignColumn<T, D> {
    /// Create a new [`AlignColumn`] with the given template tokens, tabskip and number of braces
    pub fn new(mut left: Vec<T>, mut right: Vec<T>, tabskip: Skip<D>, inbraces: u8) -> Self {
        left.reverse();
        right.reverse();
        Self {
            left: left.into(),
            inbraces,
            right: right.into(),
            tabskip,
        }
    }
}

/// Data structure for a currently open `\halign` (or `\valign`)
pub struct AlignData<T: Token, D: crate::tex::numerics::TeXDimen> {
    pub token: T,
    /// The number of braces that are currently open
    pub ingroups: u8,
    /// The index of the current column (or row)
    pub currindex: usize,
    /// The index of the column (or row) to be repeated, if the number of columns (or rows) in a particular
    /// row exceeds the number of columns (or rows) in the template
    pub repeat_index: Option<usize>,
    /// The columns (or rows) of the template
    pub columns: Box<[AlignColumn<T, D>]>,
    /// Whether the current column (or row) should omit the template
    pub omit: bool,
    /// Whether the current column (or row) spans across the next column (or rows)
    pub span: bool,
    /// The mode of the current column (or row); this is either [`BoxType::Horizontal`] for an `\halign`
    /// or [`BoxType::Vertical`] for a `\valign`
    pub inner_mode: BoxType,
    /// The mode between rows (or columns); this is either [`BoxType::Vertical`] for an `\halign`
    /// or [`BoxType::Horizontal`] for a `\valign`
    pub outer_mode: BoxType,
}
impl<T: Token, D: crate::tex::numerics::TeXDimen> AlignData<T, D> {
    /// the number of open braces at which an [`AlignmentTab`](crate::tex::catcodes::CommandCode::AlignmentTab)
    /// [`Token`] (or a `\cr`) should cause the template tokens to be inserted into the input stream
    pub fn groupval(&self) -> u8 {
        if self.omit {
            0
        } else {
            self.columns[self.currindex].inbraces
        }
    }
    pub fn check_token(&mut self, t: &T) -> Option<bool> {
        match t.command_code() {
            CommandCode::BeginGroup => {
                self.ingroups += 1;
                Some(true)
            }
            CommandCode::EndGroup => {
                if self.ingroups == 0 {
                    Some(false)
                } else {
                    self.ingroups -= 1;
                    Some(true)
                }
            }
            _ => None,
        }
    }

    /// A dummy [`AlignData`] that makes sure that [`AlignmentTab`](crate::tex::catcodes::CommandCode::AlignmentTab)
    /// [`Token`]s (and `\cr`s) are not replaced by a template; can be pushed to the [`Gullet`](crate::engine::gullet::Gullet) at the begin
    /// of an `\halign` (or `\valign`) before the template is fully processed to avoid already open
    /// [`AlignData`]s to be used.
    pub fn dummy() -> Self {
        Self {
            token: T::space(),
            ingroups: 125,
            currindex: 0,
            repeat_index: None,
            columns: Box::new([AlignColumn::new(Vec::new(), Vec::new(), Skip::default(), 0)]),
            omit: false,
            span: false,
            inner_mode: BoxType::Horizontal,
            outer_mode: BoxType::Vertical,
        }
    }
    /// Push the end-template tokens of the current column (or row) to the [`Mouth`];
    /// this is called when an [`AlignmentTab`](crate::tex::catcodes::CommandCode::AlignmentTab)
    /// [`Token`] (or a `\span`) is encountered and the number of currently open braces matches the current column's
    /// `inbraces` value.
    pub fn on_alignment_tab<ET: EngineTypes<Token = T, Dim = D>>(
        &self,
        mouth: &mut ET::Mouth,
        aux: &mut EngineAux<ET>,
    ) -> T {
        let end_align =
            <ET::Token as Token>::from_cs(aux.memory.cs_interner_mut().cs_from_str(END_TEMPLATE));
        let ls = &*self.columns[self.currindex].right;
        if self.omit || ls.is_empty() {
            end_align
        } else {
            mouth.requeue(end_align);
            let next = ls.last().unwrap().clone();
            mouth.push_slice_rev(&ls[..ls.len() - 1]);
            next
        }
    }

    /// Push the end-template tokens of the current column (or row) to the [`Mouth`]
    /// and insert `\everycr`;
    /// this is called when a `\cr` or `\crcr` is encountered and the number of currently open braces matches the current column's
    /// `inbraces` value.
    pub fn on_cr<ET: EngineTypes<Token = T, Dim = D>>(
        &self,
        mouth: &mut ET::Mouth,
        aux: &mut EngineAux<ET>,
        state: &ET::State,
    ) -> T {
        let everycr = state.get_primitive_tokens(PRIMITIVES.everycr);
        mouth.push_exp(everycr);
        let end = <ET::Token as Token>::from_cs(
            aux.memory.cs_interner_mut().cs_from_str(END_TEMPLATE_ROW),
        );
        let ls = &*self.columns[self.currindex].right;
        if self.omit || ls.is_empty() {
            end
        } else {
            mouth.requeue(end);
            let next = ls.last().unwrap().clone();
            mouth.push_slice_rev(&ls[..ls.len() - 1]);
            next
        }
    }
}
