use tlang_hir_opt::hir_opt::{HirOptGroup, HirPass};

use crate::return_statement_pass::ReturnStatementPass;
use crate::refactored_hir_js_pass::RefactoredHirJsPass;

/// Creates a HIR optimization group for JavaScript code generation
/// This group combines multiple passes to prepare HIR for JavaScript generation:
/// 1. ReturnStatementPass - transforms function completion expressions to use explicit return statements
/// 2. RefactoredHirJsPass - flattens expressions that cannot be represented in JavaScript using a generalized framework
pub fn create_hir_js_opt_group() -> HirOptGroup {
    let passes: Vec<Box<dyn HirPass>> = vec![
        Box::new(ReturnStatementPass::new()),
        Box::new(RefactoredHirJsPass::new()),
    ];

    HirOptGroup::new("HirJsOptGroup", passes)
}


