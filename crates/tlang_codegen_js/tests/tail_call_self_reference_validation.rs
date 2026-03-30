use tlang_codegen_js::generator::CodegenJS;
use tlang_codegen_js::js_hir_opt::JsHirOptimizer;
use tlang_diagnostics::Severity;
use tlang_hir_opt::HirPass;
use tlang_parser::Parser;
use tlang_semantics::SemanticAnalyzer;

mod common;

/// Compile source through the full JS pipeline and return only the
/// self-reference validation diagnostics.
fn run_validation(source: &str) -> Vec<tlang_diagnostics::Diagnostic> {
    let mut parser = Parser::from_source(source);
    let (mut ast, parse_meta) = parser.parse().expect("parse should succeed");

    let mut semantic_analyzer = SemanticAnalyzer::default();
    semantic_analyzer.add_builtin_symbols(CodegenJS::get_standard_library_symbols());
    semantic_analyzer
        .analyze(&mut ast)
        .expect("semantic analysis should succeed");

    let (mut module, meta) = tlang_ast_lowering::lower_to_hir(
        &ast,
        &parse_meta.constant_pool_node_ids,
        semantic_analyzer.symbol_id_allocator(),
        semantic_analyzer.root_symbol_table(),
        semantic_analyzer.symbol_tables().clone(),
    )
    .expect("lowering should succeed");

    let mut ctx = meta.into();
    let mut optimizer = JsHirOptimizer::default();
    optimizer
        .optimize_hir(&mut module, &mut ctx)
        .expect("optimizer should not fail");

    ctx.diagnostics
        .into_iter()
        .filter(|d| d.message().contains("not self-referencing"))
        .collect()
}

// ── No warnings ────────────────────────────────────────────────────────────

#[test]
fn no_warning_for_self_referencing_tail_call() {
    let source = r#"
        fn foo(n) { rec foo(n) }
    "#;
    assert!(run_validation(source).is_empty());
}

#[test]
fn no_warning_for_self_referencing_tail_call_in_if_branch() {
    let source = r#"
        fn foo(n) {
            if n == 0 { 0 }
            else { rec foo(n - 1) }
        }
    "#;
    assert!(run_validation(source).is_empty());
}

#[test]
fn no_warning_for_self_referencing_multi_arity_tail_call() {
    let source = r#"
        fn binary_search(list, target) { binary_search(list, target, 0, 10) }
        fn binary_search(_, _, low, high) if low > high; { -1 }
        fn binary_search(list, target, low, high) {
            let mid = (low + high) / 2;
            if list[mid] == target { mid }
            else if list[mid] < target { rec binary_search(list, target, mid + 1, high) }
            else { rec binary_search(list, target, low, mid - 1) }
        }
    "#;
    assert!(run_validation(source).is_empty());
}

#[test]
fn no_warning_for_regular_call() {
    let source = r#"
        fn foo(n) { n }
        fn bar(n) { foo(n) }
    "#;
    assert!(run_validation(source).is_empty());
}

// ── Warnings ───────────────────────────────────────────────────────────────

#[test]
fn warns_on_mutual_tail_call() {
    let source = r#"
        fn is_even(0) { true }
        fn is_even(n) { rec is_odd(n - 1) }

        fn is_odd(0) { false }
        fn is_odd(n) { rec is_even(n - 1) }
    "#;
    let diagnostics = run_validation(source);
    assert_eq!(diagnostics.len(), 2);
    for d in &diagnostics {
        assert_eq!(d.severity(), Severity::Warning);
        assert!(
            d.message().contains("not self-referencing"),
            "unexpected message: {}",
            d.message()
        );
    }
}

#[test]
fn warns_on_single_mutual_tail_call() {
    let source = r#"
        fn foo(n) { rec bar(n) }
        fn bar(n) { n }
    "#;
    let diagnostics = run_validation(source);
    assert_eq!(diagnostics.len(), 1);
    assert_eq!(diagnostics[0].severity(), Severity::Warning);
    assert!(
        diagnostics[0].message().contains("not self-referencing"),
        "unexpected message: {}",
        diagnostics[0].message()
    );
}

#[test]
fn no_warning_when_one_branch_is_self_ref_and_other_is_mutual() {
    // Only the mutual call should warn; the self-referencing one is fine.
    let source = r#"
        fn foo(n) {
            if n == 0 { rec foo(n) }
            else { rec bar(n) }
        }
        fn bar(n) { n }
    "#;
    let diagnostics = run_validation(source);
    assert_eq!(diagnostics.len(), 1);
    assert!(
        diagnostics[0].message().contains("not self-referencing"),
        "unexpected message: {}",
        diagnostics[0].message()
    );
}
