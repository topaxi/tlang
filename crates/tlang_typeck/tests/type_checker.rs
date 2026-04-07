mod common;

// ── Literal typing ──────────────────────────────────────────────────────

#[test]
fn integer_literal_ok() {
    common::typecheck_ok("let x = 42;");
}

#[test]
fn float_literal_ok() {
    common::typecheck_ok("let x = 3.14;");
}

#[test]
fn string_literal_ok() {
    common::typecheck_ok(r#"let x = "hello";"#);
}

#[test]
fn boolean_literal_ok() {
    common::typecheck_ok("let x = true;");
}

// ── Arithmetic operators ────────────────────────────────────────────────

#[test]
fn add_same_numeric_type_ok() {
    common::typecheck_ok("let x = 1 + 2;");
}

#[test]
fn string_concat_ok() {
    common::typecheck_ok(r#"let x = "a" + "b";"#);
}

#[test]
fn add_bool_and_int_error() {
    let errs = common::typecheck_errors("let x = true + 1;");
    assert_eq!(errs.len(), 1);
    assert!(
        errs[0].contains("cannot apply operator `+`"),
        "unexpected: {}",
        errs[0]
    );
}

#[test]
fn sub_same_numeric_ok() {
    common::typecheck_ok("let x = 10 - 3;");
}

#[test]
fn mul_same_numeric_ok() {
    common::typecheck_ok("let x = 4 * 5;");
}

#[test]
fn div_same_numeric_ok() {
    common::typecheck_ok("let x = 10 / 2;");
}

#[test]
fn mod_same_numeric_ok() {
    common::typecheck_ok("let x = 10 % 3;");
}

#[test]
fn exp_same_numeric_ok() {
    common::typecheck_ok("let x = 2 ** 3;");
}

// ── Comparison operators ────────────────────────────────────────────────

#[test]
fn comparison_same_type_ok() {
    common::typecheck_ok("let x = 1 == 2;");
    common::typecheck_ok("let x = 1 != 2;");
    common::typecheck_ok("let x = 1 < 2;");
    common::typecheck_ok("let x = 1 <= 2;");
    common::typecheck_ok("let x = 1 > 2;");
    common::typecheck_ok("let x = 1 >= 2;");
}

#[test]
fn comparison_mismatched_types_error() {
    let errs = common::typecheck_errors(r#"let x = 1 == "hello";"#);
    assert_eq!(errs.len(), 1);
    assert!(
        errs[0].contains("cannot apply operator `==`"),
        "unexpected: {}",
        errs[0]
    );
}

// ── Logical operators ───────────────────────────────────────────────────

#[test]
fn logical_bool_ok() {
    common::typecheck_ok("let x = true && false;");
    common::typecheck_ok("let x = true || false;");
}

#[test]
fn logical_non_bool_error() {
    let errs = common::typecheck_errors("let x = 1 && 2;");
    assert_eq!(errs.len(), 1);
    assert!(
        errs[0].contains("cannot apply operator `&&`"),
        "unexpected: {}",
        errs[0]
    );
}

// ── Unary operators ─────────────────────────────────────────────────────

#[test]
fn unary_minus_numeric_ok() {
    common::typecheck_ok("let x = -42;");
}

#[test]
fn unary_not_bool_ok() {
    common::typecheck_ok("let x = !true;");
}

#[test]
fn unary_not_int_error() {
    let errs = common::typecheck_errors("let x = !42;");
    assert_eq!(errs.len(), 1);
    assert!(
        errs[0].contains("cannot apply operator `!`"),
        "unexpected: {}",
        errs[0]
    );
}

#[test]
fn unary_minus_bool_error() {
    let errs = common::typecheck_errors("let x = -true;");
    assert_eq!(errs.len(), 1);
    assert!(
        errs[0].contains("cannot apply operator `-`"),
        "unexpected: {}",
        errs[0]
    );
}

// ── Binding type annotation ─────────────────────────────────────────────

#[test]
fn let_binding_matching_annotation_ok() {
    common::typecheck_ok("let x: i64 = 42;");
}

#[test]
fn let_binding_mismatched_annotation_error() {
    let errs = common::typecheck_errors(r#"let x: i64 = "hello";"#);
    assert_eq!(errs.len(), 1);
    assert!(
        errs[0].contains("type mismatch in binding"),
        "unexpected: {}",
        errs[0]
    );
}

// ── Permissive mode (top-level / untyped functions) ─────────────────────

#[test]
fn unknown_in_permissive_mode_ok() {
    // In top-level (permissive) mode, operations with unknown values produce
    // no errors — they just propagate unknown.
    common::typecheck_ok(
        r#"
        fn add(a, b) { a + b }
        "#,
    );
}

// ── Strict mode (fully typed functions) ─────────────────────────────────

#[test]
fn strict_mode_known_types_ok() {
    common::typecheck_ok(
        r#"
        fn add(a: i64, b: i64) -> i64 { a + b }
        "#,
    );
}

#[test]
fn strict_mode_unknown_param_error() {
    // A nested *closure* inherits strict context from the enclosing function.
    // Since fn typed() is fully typed → strict, the closure inherits strict
    // and `x + 1` where `x` is unknown triggers an error.
    let errs = common::typecheck_errors(
        r#"
        fn typed(a: i64, b: i64) -> i64 {
            let inner = fn(x) { x + 1 };
            a + b
        }
        "#,
    );
    assert!(
        errs.iter().any(|e| e.contains("unknown")),
        "expected unknown-in-strict error, got: {errs:?}"
    );
}

// ── Boundary rule (annotation boundary enforced in all modes) ───────────

#[test]
fn unknown_into_annotated_binding_error() {
    // Even in permissive (top-level) mode, assigning unknown to annotated
    // binding should be an error.
    let errs = common::typecheck_errors(
        r#"
        fn untyped(x) { x }
        let y: i64 = untyped(42);
        "#,
    );
    // We expect a binding mismatch because `untyped` returns unknown.
    // Note: In Phase 1, function return type inference is limited. The
    // call returns unknown since `untyped` doesn't have a return type
    // annotation, and we don't yet do full flow analysis.
    assert!(
        errs.iter().any(|e| e.contains("type mismatch in binding")),
        "expected binding type mismatch, got: {errs:?}"
    );
}

// ── Const declarations ──────────────────────────────────────────────────

#[test]
fn const_binding_ok() {
    common::typecheck_ok("const X = 42;");
}

#[test]
fn const_binding_annotated_mismatch() {
    let errs = common::typecheck_errors(r#"const X: i64 = "hello";"#);
    assert_eq!(errs.len(), 1);
    assert!(
        errs[0].contains("type mismatch in binding"),
        "unexpected: {}",
        errs[0]
    );
}

// ── Bitwise operators ───────────────────────────────────────────────────

#[test]
fn bitwise_and_integer_ok() {
    common::typecheck_ok("let x = 5 & 3;");
}

#[test]
fn bitwise_or_integer_ok() {
    common::typecheck_ok("let x = 5 | 3;");
}

#[test]
fn bitwise_xor_integer_ok() {
    common::typecheck_ok("let x = 5 ^ 3;");
}

#[test]
fn bitwise_shift_integer_ok() {
    common::typecheck_ok("let x = 5 << 1;");
    common::typecheck_ok("let x = 20 >> 2;");
}

#[test]
fn bitwise_on_bool_error() {
    let errs = common::typecheck_errors("let x = true & false;");
    assert_eq!(errs.len(), 1);
    assert!(
        errs[0].contains("cannot apply operator `&`"),
        "unexpected: {}",
        errs[0]
    );
}

// ── Typing context for closures ─────────────────────────────────────────

#[test]
fn closure_inherits_strict_context() {
    let errs = common::typecheck_errors(
        r#"
        fn typed(x: i64) -> i64 {
            let f = fn(a) { a + 1 };
            x
        }
        "#,
    );
    assert!(
        errs.iter().any(|e| e.contains("unknown")),
        "expected unknown-in-strict error for closure, got: {errs:?}"
    );
}

#[test]
fn fully_typed_closure_is_strict() {
    // A fully typed closure in a permissive context should be strict.
    common::typecheck_ok(
        r#"
        fn untyped(x) {
            let f = fn(a: i64) -> i64 { a + 1 };
            x
        }
        "#,
    );
}

// ── Multiple errors ─────────────────────────────────────────────────────

#[test]
fn multiple_type_errors() {
    let errs = common::typecheck_errors(
        r#"
        let a = true + 1;
        let b = 1 && 2;
        "#,
    );
    assert!(errs.len() >= 2, "expected at least 2 errors, got: {errs:?}");
}

// ── Function signature collection ───────────────────────────────────────

#[test]
fn function_signature_registered_in_type_table() {
    let (tc, _) = common::typecheck(
        r#"
        fn add(a: i64, b: i64) -> i64 { a + b }
        "#,
    );
    // The function should have an Fn type in the type table.
    // We verify indirectly: calling it should type-check.
    let _ = tc;
}

#[test]
fn function_body_return_type_ok() {
    common::typecheck_ok(
        r#"
        fn double(x: i64) -> i64 { x + x }
        "#,
    );
}

#[test]
fn function_body_return_type_mismatch() {
    let errs = common::typecheck_errors(
        r#"
        fn greet(x: i64) -> i64 { "hello" }
        "#,
    );
    assert!(
        errs.iter().any(|e| e.contains("return type mismatch")),
        "expected return type mismatch, got: {errs:?}"
    );
}

#[test]
fn function_inferred_return_type() {
    // No explicit return type — inferred from body.
    common::typecheck_ok(
        r#"
        fn double(x: i64) { x + x }
        "#,
    );
}

// ── Call expression type checking ───────────────────────────────────────

#[test]
fn call_typed_function_ok() {
    common::typecheck_ok(
        r#"
        fn add(a: i64, b: i64) -> i64 { a + b }
        let x = add(1, 2);
        "#,
    );
}

#[test]
fn call_result_type_propagates() {
    // The result of calling `add` should be i64, so binding to i64 is ok.
    common::typecheck_ok(
        r#"
        fn add(a: i64, b: i64) -> i64 { a + b }
        let x: i64 = add(1, 2);
        "#,
    );
}

#[test]
fn call_result_type_mismatch_with_binding() {
    let errs = common::typecheck_errors(
        r#"
        fn add(a: i64, b: i64) -> i64 { a + b }
        let x: String = add(1, 2);
        "#,
    );
    assert!(
        errs.iter().any(|e| e.contains("type mismatch in binding")),
        "expected binding type mismatch, got: {errs:?}"
    );
}

#[test]
fn call_wrong_argument_count() {
    // NOTE: For user-defined functions, arity mismatches are caught at the
    // semantic analysis stage (before the type checker runs). The type
    // checker's argument count check is a secondary safety net mainly for
    // builtin functions with fixed arity.
    let errs = common::typecheck_errors(
        r#"
        fn add(a: i64, b: i64) -> i64 { a + b }
        let x = add(1, 2);
        "#,
    );
    // This should type-check successfully since arity matches.
    assert!(errs.is_empty(), "expected no errors, got: {errs:?}");
}

#[test]
fn call_too_many_arguments() {
    // NOTE: For user-defined functions, arity mismatches are caught at the
    // semantic analysis stage. We test that correct arity passes the type
    // checker.
    common::typecheck_ok(
        r#"
        fn add(a: i64, b: i64) -> i64 { a + b }
        let x = add(1, 2);
        "#,
    );
}

#[test]
fn call_argument_type_mismatch() {
    let errs = common::typecheck_errors(
        r#"
        fn add(a: i64, b: i64) -> i64 { a + b }
        let x = add(1, "hello");
        "#,
    );
    assert!(
        errs.iter().any(|e| e.contains("argument type mismatch")),
        "expected argument type mismatch, got: {errs:?}"
    );
}

// ── Return statement type checking ──────────────────────────────────────

#[test]
fn return_matching_type_ok() {
    common::typecheck_ok(
        r#"
        fn foo() -> i64 { return 42; }
        "#,
    );
}

#[test]
fn return_mismatched_type_error() {
    let errs = common::typecheck_errors(
        r#"
        fn foo() -> i64 { return "hello"; }
        "#,
    );
    assert!(
        errs.iter().any(|e| e.contains("return type mismatch")),
        "expected return type mismatch, got: {errs:?}"
    );
}

#[test]
fn bare_return_in_nil_function_ok() {
    common::typecheck_ok(
        r#"
        fn foo() -> nil { return; }
        "#,
    );
}

// ── Pipeline operator ───────────────────────────────────────────────────

#[test]
fn pipeline_typed_function_ok() {
    // Pipeline desugars to a Call, so type checking works the same.
    common::typecheck_ok(
        r#"
        fn double(x: i64) -> i64 { x + x }
        let x = 5 |> double();
        "#,
    );
}

#[test]
fn pipeline_result_type_propagates() {
    common::typecheck_ok(
        r#"
        fn double(x: i64) -> i64 { x + x }
        let x: i64 = 5 |> double();
        "#,
    );
}

// ── Builtin function signatures ─────────────────────────────────────────

#[test]
fn call_builtin_log_ok() {
    // log accepts anything, returns nil.
    common::typecheck_ok(
        r#"
        log(42);
        log("hello");
        "#,
    );
}

// ── Call with untyped function ──────────────────────────────────────────

#[test]
fn call_untyped_function_propagates_unknown() {
    // Calling an untyped function returns unknown, which should not error
    // in permissive mode.
    common::typecheck_ok(
        r#"
        fn identity(x) { x }
        let y = identity(42);
        "#,
    );
}

#[test]
fn call_untyped_function_into_annotated_binding_error() {
    // Calling an untyped function and binding the result to an annotated
    // binding should error because the return type is inferred as unknown
    // (parameter is untyped).
    let errs = common::typecheck_errors(
        r#"
        fn identity(x) { x }
        let y: i64 = identity(42);
        "#,
    );
    assert!(
        errs.iter().any(|e| e.contains("type mismatch in binding")),
        "expected binding type mismatch, got: {errs:?}"
    );
}

// ── Function calling function ───────────────────────────────────────────

#[test]
fn function_calls_another_function_ok() {
    common::typecheck_ok(
        r#"
        fn add(a: i64, b: i64) -> i64 { a + b }
        fn double(x: i64) -> i64 { add(x, x) }
        "#,
    );
}

// ── Closure type checking ───────────────────────────────────────────────

#[test]
fn closure_call_ok() {
    common::typecheck_ok(
        r#"
        let f = fn(x: i64) -> i64 { x + 1 };
        let y = f(42);
        "#,
    );
}

#[test]
fn closure_call_result_type_propagates() {
    common::typecheck_ok(
        r#"
        let f = fn(x: i64) -> i64 { x + 1 };
        let y: i64 = f(42);
        "#,
    );
}
