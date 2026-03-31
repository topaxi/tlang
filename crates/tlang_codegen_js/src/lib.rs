mod binary_operator_generator;
mod builtins;
mod enum_generator;
pub mod error;
mod expr_generator;
mod function_generator;
pub mod generator;
mod hir_passes;
mod js;
pub mod js_boolean_return_simplification;
pub mod js_hir_opt;
mod name_map;
mod pattern_match_generator;
mod stmt_generator;
mod struct_generator;

pub use error::{CodegenError, CodegenWarning};
pub use hir_passes::{
    BooleanReturnSimplification, JsAnfReturnOpt, JsAnfTransform, TailCallSelfReferenceValidation,
};
pub use js_hir_opt::JsHirOptimizer;
