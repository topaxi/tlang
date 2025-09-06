use tlang_hir_opt::hir_opt::{HirOptGroup, HirPass};

use crate::return_statement_pass::ReturnStatementPass;
use crate::simplified_hir_js_pass::SimplifiedHirJsPass;

/// Creates a HIR optimization group for JavaScript code generation
/// This group combines multiple passes to prepare HIR for JavaScript generation:
/// 1. ReturnStatementPass - transforms function completion expressions to use explicit return statements
/// 2. SimplifiedHirJsPass - flattens expressions that cannot be represented in JavaScript
pub fn create_hir_js_opt_group() -> HirOptGroup {
    let passes: Vec<Box<dyn HirPass>> = vec![
        Box::new(ReturnStatementPass::new()),
        Box::new(SimplifiedHirJsPass::new()),
    ];

    HirOptGroup::new("HirJsOptGroup", passes)
}
