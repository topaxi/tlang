use tlang_diagnostics::Severity;
use tlang_hir_opt::HirPass;
use tlang_hir_opt::hir_opt::HirOptContext;
use tlang_hir_opt::tail_call_validation::TailPositionAnalysis;

mod common;

fn run_validation(source: &str) -> Vec<tlang_diagnostics::Diagnostic> {
    let (mut module, meta) = common::compile(source);
    let mut ctx: HirOptContext = meta.into();
    TailPositionAnalysis::default()
        .optimize_hir(&mut module, &mut ctx)
        .expect("TailPositionAnalysis is infallible");
    ctx.diagnostics
}

// ── No warnings ────────────────────────────────────────────────────────────

#[test]
fn no_warning_for_tail_call_in_function_completion_expr() {
    let source = r#"
        fn foo(n) { rec foo(n) }
    "#;
    assert!(run_validation(source).is_empty());
}

#[test]
fn no_warning_for_tail_call_in_return_stmt() {
    let source = r#"
        fn foo(n) {
            return rec foo(n);
        }
    "#;
    assert!(run_validation(source).is_empty());
}

#[test]
fn no_warning_for_tail_call_in_if_branch() {
    let source = r#"
        fn foo(n) {
            if n == 0 { 0 }
            else { rec foo(n) }
        }
    "#;
    assert!(run_validation(source).is_empty());
}

#[test]
fn no_warning_for_tail_call_in_match_arm() {
    let source = r#"
        fn foo(n) {
            match n {
                0 => 0,
                _ => rec foo(n),
            }
        }
    "#;
    assert!(run_validation(source).is_empty());
}

#[test]
fn no_warning_for_regular_call_anywhere() {
    let source = r#"
        fn foo(n) { n }
        fn bar(n) {
            let x = foo(n);
            x
        }
    "#;
    assert!(run_validation(source).is_empty());
}

// ── Warnings ───────────────────────────────────────────────────────────────

#[test]
fn warns_on_tail_call_in_let_binding() {
    let source = r#"
        fn foo(n) { n }
        fn bar(n) {
            let x = rec foo(n);
            x
        }
    "#;
    let diagnostics = run_validation(source);
    assert_eq!(diagnostics.len(), 1);
    assert_eq!(diagnostics[0].severity(), Severity::Warning);
    assert!(
        diagnostics[0]
            .message()
            .contains("`rec` call is not in tail position"),
        "unexpected message: {}",
        diagnostics[0].message()
    );
}

#[test]
fn warns_on_tail_call_as_function_argument() {
    let source = r#"
        fn foo(n) { n }
        fn bar(n) {
            foo(rec foo(n))
        }
    "#;
    let diagnostics = run_validation(source);
    assert_eq!(diagnostics.len(), 1);
    assert_eq!(diagnostics[0].severity(), Severity::Warning);
}

#[test]
fn warns_on_tail_call_inside_non_tail_if() {
    // The if/else is itself in a let binding (non-tail), so the rec inside it
    // is also not in tail position.
    let source = r#"
        fn foo(n) { n }
        fn bar(n) {
            let x = if n > 0 { rec foo(n) } else { n };
            x
        }
    "#;
    let diagnostics = run_validation(source);
    assert_eq!(diagnostics.len(), 1);
    assert_eq!(diagnostics[0].severity(), Severity::Warning);
}

#[test]
fn no_duplicate_warnings_for_nested_tail_if() {
    // The if/else is itself the tail expression, so rec inside IS in tail position.
    let source = r#"
        fn foo(n) {
            if n > 0 { rec foo(n) } else { n }
        }
    "#;
    assert!(run_validation(source).is_empty());
}

#[test]
fn multiple_misplaced_rec_calls_each_warn() {
    let source = r#"
        fn foo(n) { n }
        fn bar(n) {
            let a = rec foo(n);
            let b = rec foo(n);
            a + b
        }
    "#;
    let diagnostics = run_validation(source);
    assert_eq!(diagnostics.len(), 2);
    for d in &diagnostics {
        assert_eq!(d.severity(), Severity::Warning);
    }
}
