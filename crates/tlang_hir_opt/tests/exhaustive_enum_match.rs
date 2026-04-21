mod common;

use insta::assert_snapshot;
use tlang_hir::{ExprKind, StmtKind};

/// When every variant of a user-defined enum is covered by explicit arms
/// plus a trailing catch-all clause (whose type-checked type matches the enum),
/// the catch-all is unreachable and should be removed.
#[test]
fn exhaustive_match_removes_wildcard() {
    let source = r#"
        enum Color {
            Red,
            Green,
            Blue,
        }

        fn name(Color::Red) { "red" }
        fn name(Color::Green) { "green" }
        fn name(Color::Blue) { "blue" }
        fn name(_) { "unknown" }
    "#;
    let mut pass = tlang_hir_opt::ExhaustiveEnumMatch::default();
    let hir = common::compile_typecheck_and_optimize(source, &mut pass);
    let pretty = common::pretty_print(&hir);

    // The wildcard arm (from the catch-all clause) should be gone.
    assert!(
        !pretty.contains("_ =>"),
        "wildcard arm should be removed:\n{pretty}"
    );
    assert_snapshot!(pretty);
}

/// A partial match (not all variants covered) should keep the catch-all.
#[test]
fn partial_match_keeps_wildcard() {
    let source = r#"
        enum Color {
            Red,
            Green,
            Blue,
        }

        fn name(Color::Red) { "red" }
        fn name(Color::Green) { "green" }
        fn name(_) { "fallback" }
    "#;
    let mut pass = tlang_hir_opt::ExhaustiveEnumMatch::default();
    let hir = common::compile_typecheck_and_optimize(source, &mut pass);
    let pretty = common::pretty_print(&hir);

    assert!(
        pretty.contains("_ =>"),
        "wildcard arm should be kept:\n{pretty}"
    );
    assert_snapshot!(pretty);
}

/// When an arm has a guard, the wildcard should be preserved even if all
/// variants appear to be covered — the guard might fail.
#[test]
fn guarded_arm_keeps_wildcard() {
    let source = r#"
        enum Light {
            On,
            Off,
        }

        fn describe(Light::On) { "on" }
        fn describe(Light::Off) if 1 == 0 { "off (impossible)" }
        fn describe(_) { "fallback" }
    "#;
    let mut pass = tlang_hir_opt::ExhaustiveEnumMatch::default();
    let hir = common::compile_typecheck_and_optimize(source, &mut pass);
    let pretty = common::pretty_print(&hir);

    assert!(
        pretty.contains("_ =>"),
        "wildcard arm should be kept when guard present:\n{pretty}"
    );
    assert_snapshot!(pretty);
}

/// Enums with data fields should also have their wildcard removed when
/// exhaustive, provided all payload sub-patterns are catch-all.
#[test]
fn exhaustive_match_with_data_fields() {
    let source = r#"
        enum Shape {
            Circle(f64),
            Rect(f64, f64),
        }

        fn area(Shape::Circle(r)) { r }
        fn area(Shape::Rect(w, h)) { w * h }
        fn area(_) { 0 }
    "#;
    let mut pass = tlang_hir_opt::ExhaustiveEnumMatch::default();
    let hir = common::compile_typecheck_and_optimize(source, &mut pass);
    let pretty = common::pretty_print(&hir);

    assert!(
        !pretty.contains("_ =>"),
        "wildcard arm should be removed:\n{pretty}"
    );
    assert_snapshot!(pretty);
}

/// A single-variant enum that is fully covered should also have the
/// wildcard removed.
#[test]
fn single_variant_enum_removes_wildcard() {
    let source = r#"
        enum Wrapper {
            Value(i64),
        }

        fn unwrap(Wrapper::Value(v)) { v }
        fn unwrap(_) { 0 }
    "#;
    let mut pass = tlang_hir_opt::ExhaustiveEnumMatch::default();
    let hir = common::compile_typecheck_and_optimize(source, &mut pass);
    let pretty = common::pretty_print(&hir);

    assert!(
        !pretty.contains("_ =>"),
        "wildcard arm should be removed:\n{pretty}"
    );
    assert_snapshot!(pretty);
}

/// No wildcard means nothing to remove — the pass should be a no-op.
#[test]
fn no_wildcard_is_noop() {
    let source = r#"
        enum Color {
            Red,
            Green,
            Blue,
        }

        fn name(Color::Red) { "red" }
        fn name(Color::Green) { "green" }
        fn name(Color::Blue) { "blue" }
    "#;
    let mut pass = tlang_hir_opt::ExhaustiveEnumMatch::default();
    let hir = common::compile_typecheck_and_optimize(source, &mut pass);
    let pretty = common::pretty_print(&hir);

    // Should have exactly 3 arms, no wildcard.
    assert!(!pretty.contains("_ =>"), "no wildcard expected:\n{pretty}");
    assert_snapshot!(pretty);
}

/// A catch-all using a bare identifier (not `_`) should also be removed
/// when the match is exhaustive.
#[test]
fn exhaustive_match_removes_identifier_catchall() {
    let source = r#"
        enum AB {
            A,
            B,
        }

        fn label(AB::A) { "a" }
        fn label(AB::B) { "b" }
        fn label(other) { "?" }
    "#;
    let mut pass = tlang_hir_opt::ExhaustiveEnumMatch::default();
    let hir = common::compile_typecheck_and_optimize(source, &mut pass);
    let pretty = common::pretty_print(&hir);

    // The `other =>` arm is also a catch-all and should be removed.
    assert!(
        !pretty.contains("other =>"),
        "identifier catch-all should be removed:\n{pretty}"
    );
    assert_snapshot!(pretty);
}

/// A data-carrying variant arm with a restrictive payload pattern (e.g. a
/// constant) should not be treated as covering the entire variant; the
/// wildcard must be preserved.
#[test]
fn restrictive_payload_pattern_keeps_wildcard() {
    let source = r#"
        enum Num {
            V(i64),
            W,
        }

        fn classify(Num::V(0)) { "zero" }
        fn classify(Num::W) { "w" }
        fn classify(_) { "other" }
    "#;
    let mut pass = tlang_hir_opt::ExhaustiveEnumMatch::default();
    let hir = common::compile_typecheck_and_optimize(source, &mut pass);
    let pretty = common::pretty_print(&hir);

    assert!(
        pretty.contains("_ =>"),
        "wildcard arm should be kept for restrictive payload patterns:\n{pretty}"
    );
    assert_snapshot!(pretty);
}

/// A catch-all explicitly annotated with `unknown` type should never be
/// removed, even when all enum variants are explicitly covered. The
/// `unknown` annotation means the function accepts values of any type.
#[test]
fn unknown_typed_catchall_keeps_wildcard() {
    let source = r#"
        enum SafeHtml { Html(String) }

        fn render(SafeHtml::Html(v)) { v }
        fn render(v: unknown) { "fallback" }
    "#;
    let mut pass = tlang_hir_opt::ExhaustiveEnumMatch::default();
    let hir = common::compile_typecheck_and_optimize(source, &mut pass);
    let pretty = common::pretty_print(&hir);

    assert!(
        pretty.contains("v =>"),
        "unknown-typed catch-all should be kept:\n{pretty}"
    );
    assert_snapshot!(pretty);
}

#[test]
fn exhaustive_match_without_catchall_is_marked_exhaustive() {
    let source = r#"
        enum Color {
            Red,
            Blue,
        }

        let value = match Color::Red {
            Color::Red => "red",
            Color::Blue => "blue",
        };
    "#;

    let mut pass = tlang_hir_opt::ExhaustiveEnumMatch::default();
    let hir = common::compile_typecheck_and_optimize(source, &mut pass);

    let match_expr = hir
        .block
        .stmts
        .iter()
        .find_map(|stmt| match &stmt.kind {
            StmtKind::Let(_, expr, _) => Some(expr.as_ref()),
            _ => None,
        })
        .expect("expected let statement containing match");

    match &match_expr.kind {
        ExprKind::Match(_, _, metadata) => {
            assert!(metadata.exhaustive, "match should be marked exhaustive");
        }
        other => panic!("expected match expression, got {other:?}"),
    }
}
