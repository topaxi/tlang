use tlang_diagnostics::Severity;
use tlang_hir_opt::HirPass;
use tlang_hir_opt::hir_opt::HirOptContext;
use tlang_hir_opt::unused_symbol_detection::UnusedSymbolDetector;

mod common;

/// Compile source, optimize HIR, run the type checker, then run
/// `UnusedSymbolDetector`, returning the detector diagnostics after clearing
/// diagnostics from earlier phases.
fn run_detection(source: &str) -> Vec<tlang_diagnostics::Diagnostic> {
    let (mut module, meta) = common::compile(source);
    let mut ctx: HirOptContext = meta.into();

    // Run default optimizations (symbol resolution, etc.) without DCE.
    let mut optimizer = tlang_hir_opt::hir_opt::HirOptimizer::from(
        tlang_hir_opt::DefaultOptimizations::default().without("DeadCodeElimination"),
    );
    optimizer
        .optimize_hir(&mut module, &mut ctx)
        .expect("HIR optimization failed");

    // Run the type checker so that expressions have resolved types.
    let mut tc = tlang_typeck::TypeChecker::new();
    tc.optimize_hir(&mut module, &mut ctx)
        .expect("type checker failed");

    // Clear diagnostics from previous passes so we only see the detection pass.
    ctx.diagnostics.clear();

    // Run the unused-symbol detection pass.
    let mut detector = UnusedSymbolDetector::default();
    detector
        .optimize_hir(&mut module, &mut ctx)
        .expect("UnusedSymbolDetector is infallible");

    ctx.diagnostics
}

fn warning_messages(source: &str) -> Vec<String> {
    run_detection(source)
        .into_iter()
        .filter(|d| d.severity() == Severity::Warning)
        .map(|d| d.message().to_string())
        .collect()
}

// ── Struct fields ──────────────────────────────────────────────────────

#[test]
fn no_warning_for_used_struct_field() {
    let warnings = warning_messages(
        r#"
        struct Point { x: i64, y: i64 }
        let p = Point { x: 1, y: 2 };
        let _ = p.x;
        let _ = p.y;
        "#,
    );
    assert!(
        warnings.is_empty(),
        "expected no warnings, got: {warnings:?}"
    );
}

#[test]
fn warning_for_unused_struct_field() {
    let warnings = warning_messages(
        r#"
        struct Point { x: i64, y: i64 }
        let p = Point { x: 1, y: 2 };
        let _ = p.x;
        "#,
    );
    assert!(
        warnings.iter().any(|w| w.contains("Point::y")),
        "expected warning for unused field `Point::y`, got: {warnings:?}"
    );
    assert!(
        !warnings.iter().any(|w| w.contains("Point::x")),
        "field `Point::x` should NOT be reported as unused, got: {warnings:?}"
    );
}

// ── Dot-methods ────────────────────────────────────────────────────────

#[test]
fn no_warning_for_used_dot_method() {
    let warnings = warning_messages(
        r#"
        struct Vector { x: i64 }
        fn Vector.magnitude(self) -> i64 { self.x }
        let v = Vector { x: 3 };
        let _ = v.magnitude();
        "#,
    );
    // Filter out any struct field warnings (x is accessed via self.x).
    let method_warnings: Vec<_> = warnings
        .iter()
        .filter(|w| w.contains("Vector.magnitude") || w.contains("Vector::magnitude"))
        .collect();
    assert!(
        method_warnings.is_empty(),
        "expected no warnings for used method, got: {method_warnings:?}"
    );
}

#[test]
fn warning_for_unused_dot_method() {
    let warnings = warning_messages(
        r#"
        struct Vector { x: i64 }
        fn Vector.unused_method(self) -> i64 { self.x }
        let _v = Vector { x: 3 };
        "#,
    );
    assert!(
        warnings.iter().any(|w| w.contains("Vector::unused_method")),
        "expected warning for unused canonical method alias, got: {warnings:?}"
    );
    assert!(
        !warnings.iter().any(|w| w.contains("Vector.unused_method")),
        "dot-method alias should not be reported independently, got: {warnings:?}"
    );
}

// ── Struct method aliases (::) ─────────────────────────────────────────

#[test]
fn no_warning_for_used_struct_method_alias() {
    let warnings = warning_messages(
        r#"
        struct Vector { x: i64 }
        fn Vector.get_x(self) -> i64 { self.x }
        let v = Vector { x: 5 };
        let _ = v.get_x();
        "#,
    );
    let alias_warnings: Vec<_> = warnings
        .iter()
        .filter(|w| w.contains("Vector::get_x"))
        .collect();
    assert!(
        alias_warnings.is_empty(),
        "expected no warnings for used method alias, got: {alias_warnings:?}"
    );
}

// ── Guard: pass only runs once ─────────────────────────────────────────

#[test]
fn pass_runs_only_once() {
    let (mut module, meta) = common::compile("struct S { x: i64 }");
    let mut ctx: HirOptContext = meta.into();

    // Run optimizer + type checker.
    let mut optimizer = tlang_hir_opt::hir_opt::HirOptimizer::from(
        tlang_hir_opt::DefaultOptimizations::default().without("DeadCodeElimination"),
    );
    optimizer.optimize_hir(&mut module, &mut ctx).expect("opt");
    let mut tc = tlang_typeck::TypeChecker::new();
    tc.optimize_hir(&mut module, &mut ctx).expect("tc");
    ctx.diagnostics.clear();

    let mut detector = UnusedSymbolDetector::default();
    detector
        .optimize_hir(&mut module, &mut ctx)
        .expect("first run");
    let first_count = ctx.diagnostics.len();

    // Running again should produce no additional diagnostics.
    detector
        .optimize_hir(&mut module, &mut ctx)
        .expect("second run");
    assert_eq!(
        ctx.diagnostics.len(),
        first_count,
        "second run should not produce additional diagnostics"
    );
}

// ── Multiple fields: mixed used/unused ─────────────────────────────────

#[test]
fn mixed_used_unused_fields() {
    let warnings = warning_messages(
        r#"
        struct Rect { width: i64, height: i64, color: i64 }
        let r = Rect { width: 10, height: 20, color: 3 };
        let _ = r.width;
        "#,
    );
    assert!(
        warnings.iter().any(|w| w.contains("Rect::height")),
        "expected warning for unused field `Rect::height`, got: {warnings:?}"
    );
    assert!(
        warnings.iter().any(|w| w.contains("Rect::color")),
        "expected warning for unused field `Rect::color`, got: {warnings:?}"
    );
    assert!(
        !warnings.iter().any(|w| w.contains("Rect::width")),
        "field `Rect::width` should NOT be reported as unused, got: {warnings:?}"
    );
}

#[test]
fn no_warning_for_struct_fields_used_in_fn_param_pattern() {
    let warnings = warning_messages(
        r#"
        struct Point { x: i64, y: i64 }
        fn get_x(Point { x, y: _ }) { x }
        let p = Point { x: 3, y: 4 };
        let _ = get_x(p);
        "#,
    );
    let field_warnings: Vec<_> = warnings
        .iter()
        .filter(|w| w.contains("Point::x") || w.contains("Point::y"))
        .collect();
    assert!(
        field_warnings.is_empty(),
        "expected no warnings for destructured parameter fields, got: {field_warnings:?}"
    );
}

#[test]
fn no_warning_for_struct_fields_used_in_match_pattern() {
    let warnings = warning_messages(
        r#"
        struct Point { x: i64, y: i64 }
        fn classify(p) {
            match p {
                Point { x: 0, y: 0 } => "origin",
                Point { x, y } => x + y,
            }
        }
        let p = Point { x: 3, y: 4 };
        let _ = classify(p);
        "#,
    );
    let field_warnings: Vec<_> = warnings
        .iter()
        .filter(|w| w.contains("Point::x") || w.contains("Point::y"))
        .collect();
    assert!(
        field_warnings.is_empty(),
        "expected no warnings for destructured match fields, got: {field_warnings:?}"
    );
}

// ── Underscore-prefixed fields suppressed ──────────────────────────────

#[test]
fn no_warning_for_underscore_prefixed_field() {
    let warnings = warning_messages(
        r#"
        struct Point { _internal: i64, x: i64 }
        let p = Point { _internal: 0, x: 1 };
        let _ = p.x;
        "#,
    );
    assert!(
        !warnings.iter().any(|w| w.contains("_internal")),
        "underscore-prefixed field should be suppressed, got: {warnings:?}"
    );
}

// ── Method call with multiple args ─────────────────────────────────────

#[test]
fn no_warning_for_used_method_with_args() {
    let warnings = warning_messages(
        r#"
        struct Vector { x: i64 }
        fn Vector.add(self, other: Vector) -> Vector {
            Vector { x: self.x + other.x }
        }
        let v1 = Vector { x: 1 };
        let v2 = Vector { x: 2 };
        let _ = v1.add(v2);
        "#,
    );
    let method_warnings: Vec<_> = warnings
        .iter()
        .filter(|w| w.contains("Vector.add") || w.contains("Vector::add"))
        .collect();
    assert!(
        method_warnings.is_empty(),
        "expected no warnings for used method, got: {method_warnings:?}"
    );
}

#[test]
fn no_warning_for_method_used_via_qualified_alias_reference() {
    let warnings = warning_messages(
        r#"
        struct Vector { x: i64 }
        fn Vector.get_x(self) -> i64 { self.x }

        fn map([], _) { [] }
        fn map([x, ...xs], f) { [f(x), ...map(xs, f)] }

        let vectors = [Vector { x: 1 }, Vector { x: 2 }];
        let _ = map(vectors, Vector::get_x);
        "#,
    );
    let method_warnings: Vec<_> = warnings
        .iter()
        .filter(|w| w.contains("Vector.get_x") || w.contains("Vector::get_x"))
        .collect();
    assert!(
        method_warnings.is_empty(),
        "expected no warnings for method alias reference, got: {method_warnings:?}"
    );
}
