#![feature(associated_type_defaults)]

mod hir;
pub mod visit;
pub use visit::Visitor;

pub use hir::*;
