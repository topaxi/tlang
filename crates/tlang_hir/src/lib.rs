#![feature(associated_type_defaults)]

pub mod fold;
mod hir;
pub mod span_index;
pub mod visit;
pub use visit::Visitor;

pub use hir::*;
pub use span_index::{SpanIndex, SpanNode};
pub use tlang_span::HirId;
