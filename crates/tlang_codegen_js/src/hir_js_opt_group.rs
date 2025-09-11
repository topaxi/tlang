use tlang_hir_opt::hir_opt::{HirOptGroup, HirPass};

use crate::return_statement_pass::ReturnStatementPass;
use crate::anf_transformer::AnfTransformer;

/// Creates a HIR optimization group for JavaScript code generation
/// This group combines multiple passes to prepare HIR for JavaScript generation:
/// 1. ReturnStatementPass - transforms function completion expressions to use explicit return statements
/// 2. AnfTransformer - converts complex expressions to ANF form with temp variables
pub fn create_hir_js_opt_group() -> HirOptGroup {
    let passes: Vec<Box<dyn HirPass>> = vec![
        Box::new(ReturnStatementPass::new()),
        Box::new(AnfTransformer::new()),
    ];

    HirOptGroup::new("HirJsOptGroup", passes)
}


