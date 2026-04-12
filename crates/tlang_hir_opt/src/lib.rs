#![feature(box_patterns)]
pub mod anf_transform;
pub mod constant_folding;
pub mod dead_code_elimination;
pub mod free_variable_analysis;
pub mod hir_opt;
pub mod slot_allocation;
pub mod symbol_resolution;
pub mod tail_call_validation;
pub mod unused_symbol_detection;

pub use anf_transform::{AnfFilter, AnfTransform, FullAnfFilter};
pub use constant_folding::{ConstantFolder, ConstantFolding, ConstantPropagator};
pub use dead_code_elimination::DeadCodeElimination;
pub use free_variable_analysis::FreeVariableAnalysis;
pub use hir_opt::{DefaultOptimizations, HirOptError, HirOptimizer, HirPass};
pub use slot_allocation::{ScopeDataUpdater, SlotAllocation, SlotAllocator};
pub use symbol_resolution::SymbolResolution;
pub use tail_call_validation::TailPositionAnalysis;
pub use unused_symbol_detection::UnusedSymbolDetector;
