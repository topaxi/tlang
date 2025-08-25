#![feature(box_patterns)]
pub mod constant_folding;
pub mod hir_opt;
pub mod slot_allocation;
pub mod symbol_resolution;

pub use constant_folding::{ConstantFolder, ConstantFolding, ConstantPropagator};
pub use hir_opt::{HirOptimizer, HirPass};
pub use slot_allocation::{ScopeDataUpdater, SlotAllocation, SlotAllocator};
pub use symbol_resolution::SymbolResolution;
