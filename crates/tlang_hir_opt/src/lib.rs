#![feature(box_patterns)]
pub mod constant_folding;
pub mod constant_propagation;
pub mod dead_code_elimination;
pub mod hir_opt;

pub use constant_folding::ConstantFolder;
pub use constant_propagation::ConstantPropagator;
pub use dead_code_elimination::DeadCodeEliminator;
pub use hir_opt::{HirOptimizer, HirPass};
