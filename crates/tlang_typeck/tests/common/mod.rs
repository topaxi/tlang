#![allow(dead_code)]

use tlang_ast_lowering::lower_to_hir;
use tlang_defs::DefKind;
use tlang_hir as hir;
use tlang_hir_opt::HirPass;
use tlang_hir_opt::hir_opt::HirOptContext;
use tlang_parser::Parser;
use tlang_semantics::SemanticAnalyzer;
use tlang_typeck::TypeChecker;

fn compile(source: &str) -> hir::LowerResult {
    let (mut ast, parse_meta) = Parser::from_source(source).parse().unwrap();
    let mut semantic_analyzer = SemanticAnalyzer::default();
    semantic_analyzer.add_builtin_symbols(&[
        ("println", DefKind::Function(u16::MAX)),
        ("log", DefKind::Function(u16::MAX)),
    ]);
    semantic_analyzer.analyze(&mut ast).unwrap();
    lower_to_hir(
        &ast,
        &parse_meta.constant_pool_node_ids,
        semantic_analyzer.symbol_id_allocator(),
        semantic_analyzer.root_symbol_table(),
        semantic_analyzer.symbol_tables().clone(),
    )
    .expect("lowering should succeed")
}

/// Compile source, run the optimizer **then** the type checker, and return
/// the type checker along with any diagnostics it produced.
pub fn typecheck(source: &str) -> (TypeChecker, Vec<tlang_diagnostics::Diagnostic>) {
    let (mut module, meta) = compile(source);
    let mut ctx: HirOptContext = meta.into();

    // Run default optimizations first (symbol resolution, etc.) but
    // **without** dead code elimination — DCE would remove unused bindings
    // before the type checker can inspect them.
    let mut optimizer = tlang_hir_opt::hir_opt::HirOptimizer::from(
        tlang_hir_opt::DefaultOptimizations::default().without("DeadCodeElimination"),
    );
    optimizer
        .optimize_hir(&mut module, &mut ctx)
        .expect("HIR optimization failed");

    // Run the type checker.
    let mut tc = TypeChecker::new();
    tc.optimize_hir(&mut module, &mut ctx)
        .expect("type checker failed");

    let diags = ctx.diagnostics;
    (tc, diags)
}

/// Convenience: run the type checker and return only the diagnostic messages.
pub fn typecheck_errors(source: &str) -> Vec<String> {
    let (_, diags) = typecheck(source);
    diags.into_iter().map(|d| d.message().to_string()).collect()
}

/// Convenience: run the type checker and assert no errors were produced.
pub fn typecheck_ok(source: &str) {
    let errs = typecheck_errors(source);
    assert!(errs.is_empty(), "expected no errors, got: {errs:?}");
}
