mod binary_operator_generator;
pub mod boolean_return_simplification;
mod builtins;
mod enum_generator;
pub mod error;
mod expr_generator;
mod function_generator;
pub mod generator;
mod js;
pub mod js_anf_return_opt;
pub mod js_anf_transform;
mod js_boolean_return_simplification;
pub mod js_hir_opt;
mod name_map;
mod pattern_match_generator;
mod stmt_generator;
mod struct_generator;

pub use error::CodegenError;
