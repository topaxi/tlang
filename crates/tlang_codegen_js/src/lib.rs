#![feature(box_patterns)]
mod binary_operator_generator;
mod enum_generator;
mod expr_generator;
mod function_generator;
pub mod generator;
pub mod hir_js_opt_group;
mod js;
pub mod js_expr_utils;
mod pattern_match_generator;
pub mod return_statement_pass;
mod scope;
pub mod simplified_hir_js_pass;
mod stmt_generator;
mod struct_generator;

pub use hir_js_opt_group::create_hir_js_opt_group;
pub use js_expr_utils::{expr_can_render_as_js_expr, if_else_can_render_as_ternary};
pub use return_statement_pass::ReturnStatementPass;
pub use simplified_hir_js_pass::SimplifiedHirJsPass;
