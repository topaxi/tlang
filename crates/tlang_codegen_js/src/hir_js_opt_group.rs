use tlang_hir_opt::hir_opt::{HirOptGroup, HirPass};

use crate::return_statement_pass::ReturnStatementPass;
use crate::simplified_hir_js_pass::SimplifiedHirJsPass;
use crate::refactored_hir_js_pass::RefactoredHirJsPass;

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

/// Creates a HIR optimization group using the refactored generalized approach
/// This group uses the new expression transformation framework for better maintainability
pub fn create_refactored_hir_js_opt_group() -> HirOptGroup {
    let passes: Vec<Box<dyn HirPass>> = vec![
        Box::new(ReturnStatementPass::new()),
        Box::new(RefactoredHirJsPass::new()),
    ];

    HirOptGroup::new("RefactoredHirJsOptGroup", passes)
}
