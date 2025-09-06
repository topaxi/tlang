use tlang_hir_opt::hir_opt::{HirOptGroup, HirPass};

use crate::return_statement_pass::ReturnStatementPass;
use crate::simplified_hir_js_pass::SimplifiedHirJsPass;

/// Creates a HIR optimization group for JavaScript code generation
/// This group combines multiple passes to prepare HIR for JavaScript generation:
/// 1. SimplifiedHirJsPass - flattens expressions that cannot be represented in JavaScript
/// 2. ReturnStatementPass - transforms function completion expressions to use explicit return statements
pub fn create_hir_js_opt_group() -> HirOptGroup {
    let passes: Vec<Box<dyn HirPass>> = vec![
        Box::new(SimplifiedHirJsPass::new()),
        Box::new(ReturnStatementPass::new()),
    ];

    HirOptGroup::new("HirJsOptGroup", passes)
}