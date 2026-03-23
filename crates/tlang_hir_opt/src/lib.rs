#![feature(box_patterns)]
pub mod anf_transform;
pub mod constant_folding;
pub mod hir_opt;
pub mod slot_allocation;
pub mod symbol_resolution;
pub mod tail_call_validation;

pub use anf_transform::{AnfFilter, AnfTransform, FullAnfFilter};
pub use constant_folding::{ConstantFolder, ConstantFolding, ConstantPropagator};
pub use hir_opt::{HirOptimizer, HirPass};
pub use slot_allocation::{ScopeDataUpdater, SlotAllocation, SlotAllocator};
pub use symbol_resolution::SymbolResolution;
pub use tail_call_validation::TailPositionAnalysis;
