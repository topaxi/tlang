#![feature(box_patterns)]
mod binary_operator_generator;
mod enum_generator;
mod expr_generator;
mod function_generator;
pub mod generator;
pub mod hir_js_pass;
mod js;
mod pattern_match_generator;
mod scope;
mod stmt_generator;
mod struct_generator;

pub use hir_js_pass::HirJsPass;
