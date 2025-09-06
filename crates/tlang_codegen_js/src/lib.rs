#![feature(box_patterns)]
mod binary_operator_generator;
mod enum_generator;
mod expr_generator;
mod function_generator;
pub mod generator;
pub mod hir_js_pass;
pub mod hir_js_opt_group;
mod js;
mod pattern_match_generator;
pub mod return_statement_pass;
mod scope;
pub mod simplified_hir_js_pass;
mod stmt_generator;
mod struct_generator;

pub use hir_js_pass::HirJsPass;
pub use hir_js_opt_group::create_hir_js_opt_group;
pub use return_statement_pass::ReturnStatementPass;
pub use simplified_hir_js_pass::SimplifiedHirJsPass;
