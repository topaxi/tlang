mod common;

// ── Literal typing ──────────────────────────────────────────────────────

#[test]
fn integer_literal_ok() {
    common::typecheck_ok("let x = 42;");
}

#[test]
fn integer_literal_uses_typed_binding_context() {
    common::typecheck_ok("let x: i32 = 42;");
}

#[test]
fn integer_literal_uses_call_argument_context() {
    common::typecheck_ok(
        r#"
        enum Expr { Number(i32) }
        let _ = Expr::Number(42);
        "#,
    );
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
fn arithmetic_coerces_mixed_numeric_types() {
    common::typecheck_ok(
        r#"
        let sum: i64 = 0 + (1 as isize);
        let ratio: f64 = (1 as isize) / 2.0;
        "#,
    );
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

#[test]
fn let_binding_undeclared_type_annotation_error() {
    let errs = common::typecheck_errors("let x: int = 42;");
    assert_eq!(errs.len(), 1);
    assert!(
        errs[0].contains("Use of undeclared type `int`"),
        "unexpected: {}",
        errs[0]
    );
}

#[test]
fn struct_field_undeclared_type_annotation_error() {
    let errs = common::typecheck_errors("struct Circle { radius: int }");
    assert_eq!(errs.len(), 1);
    assert!(
        errs[0].contains("Use of undeclared type `int`"),
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

#[test]
fn unresolved_generic_closure_param_in_permissive_mode_ok() {
    common::typecheck_ok(
        r#"
        fn use_fn<T>(f: fn(T) -> i64) { 0 }
        use_fn(fn(x) { x * 2 });
        "#,
    );
}

#[test]
fn unresolved_generic_closure_param_comparison_in_permissive_mode_ok() {
    common::typecheck_ok(
        r#"
        fn use_pred<T>(f: fn(T) -> bool) { true }
        use_pred(fn(x) { x > 5 });
        "#,
    );
}

#[test]
fn generic_call_accepts_distinct_type_var_ids() {
    common::typecheck_ok(
        r#"
        protocol Ord {}

        fn max_of<T: Ord>(a: T, b: T) -> T {
            if a >= b { a } else { b }
        }

        fn clamp<T: Ord>(value: T, lo: T, hi: T) -> T {
            value |> max_of(lo) |> max_of(hi)
        }
        "#,
    );
}

#[test]
fn float_literal_adopts_expected_numeric_type_in_call_context() {
    common::typecheck_ok(
        r#"
        enum Expense { Food(f32) }
        let x = Expense::Food(45.50);
        "#,
    );
}

#[test]
fn list_slice_allows_single_start_argument() {
    common::typecheck_ok(
        r#"
        let xs = [1, 2, 3, 4];
        let tail = xs.slice(1);
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

#[test]
fn for_loop_item_infers_from_typed_iterable_variable() {
    common::typecheck_ok(
        r#"
        enum Tree {
            Empty,
            Node { value: isize, left: Tree, right: Tree },
        }

        impl Iterable<isize> for Tree {
            fn iter(self) {
                Iterable::iter([])
            }
        }

        let tree = Tree::Empty;
        let total = for x in tree; with sum = 0 as isize {
            let item: isize = x;
            sum
        };
        "#,
    );
}

#[test]
fn for_loop_accumulator_coerces_mixed_numeric_types() {
    common::typecheck_ok(
        r#"
        enum Tree {
            Empty,
            Node { value: isize, left: Tree, right: Tree },
        }

        impl Iterable<isize> for Tree {
            fn iter(self) {
                Iterable::iter([])
            }
        }

        let tree = Tree::Empty;
        let total: i64 = for x in tree; with sum = 0 {
            sum + x
        };
        "#,
    );
}

#[test]
fn loop_accumulator_infers_from_annotated_result_binding() {
    common::typecheck_ok(
        r#"
        enum Tree {
            Empty,
            Node { value: isize, left: Tree, right: Tree },
        }

        impl Iterable<isize> for Tree {
            fn iter(self) {
                Iterable::iter([])
            }
        }

        let tree = Tree::Empty;
        let evens: List<isize> = for x in tree; with acc = [] {
            if x % 2 == 0 { [...acc, x] } else { acc }
        };
        "#,
    );
}

#[test]
fn loop_accumulator_infers_from_function_return_type() {
    common::typecheck_ok(
        r#"
        enum Tree {
            Empty,
            Node { value: isize, left: Tree, right: Tree },
        }

        impl Iterable<isize> for Tree {
            fn iter(self) {
                Iterable::iter([])
            }
        }

        fn evens(tree: Tree) -> List<isize> {
            for x in tree; with acc = [] {
                if x % 2 == 0 { [...acc, x] } else { acc }
            }
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
    common::typecheck_ok(
        r#"
        fn add(a: i64, b: i64) -> i64 { a + b }
        let result = add(1, 2);
        "#,
    );
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

#[test]
fn function_inferred_return_type_from_if_else_enum_paths() {
    common::typecheck_ok(
        r#"
        enum Tree {
            Empty,
            Node { value: isize, left: Tree, right: Tree },
        }

        fn insert(flag: bool, value: isize) {
            if flag {
                Tree::Node { value, left: Tree::Empty, right: Tree::Empty }
            } else {
                Tree::Node { value, left: Tree::Empty, right: Tree::Empty }
            }
        }

        let tree: Tree = insert(true, 1 as isize);
        "#,
    );
}

#[test]
fn recursive_multi_clause_function_infers_return_type() {
    common::typecheck_ok(
        r#"
        enum Expr {
            Value(isize),
            Add(Expr, Expr),
            Subtract(Expr, Expr),
            Multiply(Expr, Expr),
            Divide(Expr, Expr),
        }

        fn evaluate(Expr::Value(val)) { val }
        fn evaluate(Expr::Add(left, right)) { evaluate(left) + evaluate(right) }
        fn evaluate(Expr::Subtract(left, right)) { evaluate(left) - evaluate(right) }
        fn evaluate(Expr::Multiply(left, right)) { evaluate(left) * evaluate(right) }
        fn evaluate(Expr::Divide(left, right)) { evaluate(left) / evaluate(right) }

        let _: isize = evaluate(Expr::Value(1 as isize));
        "#,
    );
}

#[test]
fn loop_expression_result_infers_let_binding_type() {
    common::typecheck_ok(
        r#"
        let sum: i64 = for x in [1, 2, 3]; with acc = 0 {
            acc + x
        };
        "#,
    );
}

#[test]
fn loop_expression_result_flows_into_call_arguments() {
    common::typecheck_ok(
        r#"
        fn takes_i64(x: i64) -> i64 { x }

        let result = takes_i64(for x in [1, 2, 3]; with acc = 0 {
            acc + x
        });
        let _: i64 = result;
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
fn call_correct_argument_count_ok() {
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
fn call_with_matching_argument_count_ok() {
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
fn infer_return_type_from_return_statement_without_trailing_expression() {
    common::typecheck_ok(
        r#"
        fn foo() { return 42; }
        let x: i64 = foo();
        "#,
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

#[test]
fn call_builtin_math_min_variadic_ok() {
    common::typecheck_ok(
        r#"
        let x = math::min(1.0, 2.0, 3.0);
        "#,
    );
}

#[test]
fn call_builtin_math_min_variadic_type_mismatch_error() {
    let errs = common::typecheck_errors(
        r#"
        let x = math::min(1.0, "x");
        "#,
    );
    assert!(
        errs.iter()
            .any(|e| e.contains("argument type mismatch") && e.contains("math::min")),
        "expected variadic builtin argument type error, got: {errs:?}"
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

#[test]
fn typed_recursive_higher_order_function_infers_closure_types() {
    common::typecheck_ok(
        r#"
        fn map<T, U>([]: List<T>, _: fn(T) -> U) -> List<U> { [] }
        fn map<T, U>([x, ...xs]: List<T>, f: fn(T) -> U) -> List<U> { [f(x), ...map(xs, f)] }

        let ys = map([1, 2, 3], fn(x) { x + 1 });
        "#,
    );
}

#[test]
fn closure_with_generic_expected_return_stays_permissive() {
    common::typecheck_ok(
        r#"
        fn map<T, U>([]: List<T>, _: fn(T) -> U) -> List<U> { [] }
        fn map<T, U>([x, ...xs]: List<T>, f: fn(T) -> U) -> List<U> { [f(x), ...map(xs, f)] }
        fn identity(x) { x }

        let ys = map([1, 2, 3], fn(n) { identity(n) == identity(n) });
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

#[test]
fn closure_body_return_type_mismatch() {
    let errs = common::typecheck_errors(
        r#"
        let f = fn(x: i64) -> i64 { "hello" };
        "#,
    );
    assert!(
        errs.iter().any(|e| e.contains("return type mismatch")),
        "expected return type mismatch for closure, got: {errs:?}"
    );
}

// ── Struct declaration typing ───────────────────────────────────────────

#[test]
fn struct_declaration_registered() {
    // Struct construction: the constructor produces the struct type.
    common::typecheck_ok(
        r#"
        struct Point { x: isize, y: isize }
        let p = Point { x: 1, y: 2 };
        "#,
    );
}

#[test]
fn struct_field_access_ok() {
    // Accessing a typed field on a struct should resolve to the field's type.
    common::typecheck_ok(
        r#"
        struct Point { x: isize, y: isize }
        let p = Point { x: 1, y: 2 };
        let x_val = p.x;
        "#,
    );
}

#[test]
fn struct_construction_type_is_path() {
    // The type of a struct construction expression should be the struct type.
    common::typecheck_ok(
        r#"
        struct Point { x: isize, y: isize }
        let p = Point { x: 1, y: 2 };
        "#,
    );
}

// ── Enum declaration typing ─────────────────────────────────────────────

#[test]
fn enum_declaration_registered() {
    common::typecheck_ok(
        r#"
        enum Color { Red, Green, Blue }
        let c = Color::Red;
        "#,
    );
}

#[test]
fn enum_variant_with_params_ok() {
    common::typecheck_ok(
        r#"
        enum Shape {
            Circle { radius: f64 },
            Rectangle { width: f64, height: f64 },
        }
        let s = Shape::Circle { radius: 5.0 };
        "#,
    );
}

// ── Implements expression ───────────────────────────────────────────────

#[test]
fn implements_expression_is_bool() {
    // `implements` should always produce a bool.
    common::typecheck_ok(
        r#"
        enum Animal {
            Dog,
            Cat,
        }
        protocol Greet {
            fn greet(self)
        }
        let dog = Animal::Dog;
        let check = dog implements Greet;
        "#,
    );
}

// ── Method resolution ───────────────────────────────────────────────────

#[test]
fn struct_method_call_ok() {
    common::typecheck_ok(
        r#"
        struct Point { x: isize, y: isize }
        fn Point.get_x(self) -> isize { self.x }
        let p = Point { x: 3, y: 4 };
        let x = Point::get_x(p);
        "#,
    );
}

#[test]
fn index_access_type_flows_in_strict_context() {
    common::typecheck_ok(
        r#"
        fn head_plus_one(xs: List<i64>) -> i64 {
            xs[0] + 1
        }
        "#,
    );
}

#[test]
fn dot_method_reference_matches_expected_fn_type() {
    common::typecheck_ok(
        r#"
        struct Point { x: i64 }
        fn Point.get_x(self) -> i64 { self.x }
        fn apply_getter(getter: fn(Point) -> i64, point: Point) -> i64 { getter(point) }
        let point = Point { x: 3 };
        let value: i64 = apply_getter(Point::get_x, point);
        "#,
    );
}

#[test]
fn dot_method_call_return_type_flows_in_strict_context() {
    common::typecheck_ok(
        r#"
        struct Point { x: i64 }
        fn Point.get_x(self) -> i64 { self.x }
        fn increment(point: Point) -> i64 { point.get_x() + 1 }
        "#,
    );
}

// ── Struct binding type propagation ─────────────────────────────────────

#[test]
fn struct_construction_binding_type_ok() {
    // Binding the result of struct construction to an annotated binding
    // should work since the constructor returns the struct type.
    common::typecheck_ok(
        r#"
        struct Point { x: isize, y: isize }
        let p = Point { x: 1, y: 2 };
        let q = p;
        "#,
    );
}

#[test]
fn struct_field_access_type_propagates_to_binding() {
    // Field access type should propagate through to annotated bindings.
    common::typecheck_ok(
        r#"
        struct Point { x: isize, y: isize }
        let p = Point { x: 1, y: 2 };
        let x: isize = p.x;
        "#,
    );
}

#[test]
fn struct_field_access_type_mismatch() {
    // Accessing a field and binding it to wrong type should error.
    let errs = common::typecheck_errors(
        r#"
        struct Point { x: isize, y: isize }
        let p = Point { x: 1, y: 2 };
        let x: String = p.x;
        "#,
    );
    assert!(
        errs.iter().any(|e| e.contains("type mismatch in binding")),
        "expected binding type mismatch, got: {errs:?}"
    );
}

// ── Enum variant type propagation ───────────────────────────────────────

#[test]
fn enum_discriminant_variant_type_ok() {
    // Discriminant enum variants should have the enum type.
    common::typecheck_ok(
        r#"
        enum Color { Red, Green, Blue }
        let c = Color::Red;
        let d = Color::Green;
        "#,
    );
}

#[test]
fn enum_constructor_variant_type_ok() {
    // Enum variants with parameters should produce the enum type.
    common::typecheck_ok(
        r#"
        enum Shape {
            Circle { radius: f64 },
            Rectangle { width: f64, height: f64 },
        }
        let c = Shape::Circle { radius: 1.0 };
        let r = Shape::Rectangle { width: 2.0, height: 3.0 };
        "#,
    );
}

// ── Implements expression type checking ─────────────────────────────────

#[test]
fn implements_expression_binding_to_bool_ok() {
    // The result of `implements` should be assignable to bool.
    common::typecheck_ok(
        r#"
        enum Animal {
            Dog,
            Cat,
        }
        protocol Greet {
            fn greet(self)
        }
        let dog = Animal::Dog;
        let check: bool = dog implements Greet;
        "#,
    );
}

#[test]
fn implements_expression_binding_to_non_bool_error() {
    // Binding `implements` result to a non-bool type should error.
    let errs = common::typecheck_errors(
        r#"
        enum Animal {
            Dog,
            Cat,
        }
        protocol Greet {
            fn greet(self)
        }
        let dog = Animal::Dog;
        let check: i64 = dog implements Greet;
        "#,
    );
    assert!(
        errs.iter().any(|e| e.contains("type mismatch in binding")),
        "expected binding type mismatch, got: {errs:?}"
    );
}

// ── Method with return type inference ───────────────────────────────────

#[test]
fn struct_method_return_type_used_in_binding() {
    common::typecheck_ok(
        r#"
        struct Point { x: isize, y: isize }
        fn Point.get_x(self) -> isize { self.x }
        let p = Point { x: 3, y: 4 };
        let x: isize = Point::get_x(p);
        "#,
    );
}

// ── Negative tests: struct/enum construction errors ─────────────────────

#[test]
fn struct_field_access_unknown_field_error() {
    // Accessing a field that doesn't exist on the struct should error.
    let errs = common::typecheck_errors(
        r#"
        struct Point { x: isize, y: isize }
        let p = Point { x: 1, y: 2 };
        let z = p.z;
        "#,
    );
    assert!(
        errs.iter()
            .any(|e| e.contains("unknown field `z` on struct `Point`")),
        "expected unknown field error, got: {errs:?}"
    );
}

// ── Protocol declaration typing ─────────────────────────────────────────

#[test]
fn protocol_declaration_no_errors() {
    common::typecheck_ok(
        r#"
        protocol Greet {
            fn greet(self)
        }
        "#,
    );
}

#[test]
fn protocol_with_return_type_no_errors() {
    common::typecheck_ok(
        r#"
        protocol Greet {
            fn greet(self) -> String
        }
        "#,
    );
}

#[test]
fn protocol_with_constraints_no_errors() {
    common::typecheck_ok(
        r#"
        protocol PartialEq {
            fn eq(self, other)
        }
        protocol Eq : PartialEq {}
        "#,
    );
}

// ── Impl block validation ───────────────────────────────────────────────

#[test]
fn impl_block_all_methods_present_ok() {
    common::typecheck_ok(
        r#"
        protocol Greet {
            fn greet(self)
        }
        enum Animal {
            Dog(String),
            Cat(String),
        }
        impl Greet for Animal {
            fn greet(Animal::Dog(name)) { "Woof! I'm " + name }
            fn greet(Animal::Cat(name)) { "Meow! I'm " + name }
        }
        "#,
    );
}

#[test]
fn impl_block_missing_method_error() {
    let errs = common::typecheck_errors(
        r#"
        protocol Greet {
            fn greet(self)
        }
        protocol Display {
            fn to_string(self)
        }
        enum Animal {
            Dog(String),
        }
        impl Greet for Animal {
            fn greet(Animal::Dog(name)) { "Woof! " + name }
        }
        impl Display for Animal {}
        "#,
    );
    assert!(
        errs.iter()
            .any(|e| e.contains("missing method `to_string`") && e.contains("Display")),
        "expected missing method error, got: {errs:?}"
    );
}

#[test]
fn impl_block_missing_constraint_caught_by_semantic_analysis() {
    // Note: Missing constraint protocols are caught by the semantic
    // analyzer's ProtocolConstraintValidator pass before the type checker
    // runs. The type checker has complementary constraint checking for
    // robustness. This test verifies the semantic pass catches the error.
    let (mut ast, _parse_meta) = tlang_parser::Parser::from_source(
        r#"
        protocol PartialEq {
            fn eq(self, other)
        }
        protocol Eq : PartialEq {}
        protocol Ord : Eq {
            fn cmp(self, other)
        }
        struct Point { x: isize, y: isize }
        impl Ord for Point {
            fn cmp(a, b) { 0 }
        }
        "#,
    )
    .parse()
    .unwrap();
    let mut semantic_analyzer = tlang_semantics::SemanticAnalyzer::default();
    let result = semantic_analyzer.analyze(&mut ast);
    assert!(
        result.is_err(),
        "expected semantic analysis to catch missing constraints"
    );
    let diagnostics = result.unwrap_err();
    assert!(
        diagnostics.iter().any(|d| d.message().contains("Eq")),
        "expected missing constraint for Eq, got: {diagnostics:?}"
    );
}

#[test]
fn impl_block_with_apply_method_ok() {
    common::typecheck_ok(
        r#"
        protocol Greet {
            fn greet(self)
        }
        enum Animal {
            Dog(String),
            Cat(String),
        }
        impl Greet for Animal {
            apply greet;
            fn greet(Animal::Dog(name)) { "Woof! " + name }
            fn greet(Animal::Cat(name)) { "Meow! " + name }
        }
        "#,
    );
}

#[test]
fn impl_block_apply_conflicts_with_existing_method_error() {
    let errs = common::typecheck_errors(
        r#"
        protocol Greet {
            fn greet(self)
        }
        enum Animal {
            Dog(String),
            Cat(String),
        }
        fn Animal.greet(self) {
            Greet::greet(self)
        }
        impl Greet for Animal {
            apply greet;
            fn greet(Animal::Dog(name)) { "Woof! " + name }
            fn greet(Animal::Cat(name)) { "Meow! " + name }
        }
        "#,
    );
    assert!(
        errs.iter().any(|e| e.contains("apply greet")
            && e.contains("already defined")
            && e.contains("Animal")),
        "expected apply conflict error, got: {errs:?}"
    );
}

#[test]
fn impl_block_constraints_satisfied_ok() {
    common::typecheck_ok(
        r#"
        protocol PartialEq {
            fn eq(self, other)
        }
        protocol Eq : PartialEq {}
        protocol Ord : Eq {
            fn cmp(self, other)
        }
        struct Point { x: isize, y: isize }
        impl PartialEq for Point {
            fn eq(a, b) { a.x == b.x && a.y == b.y }
        }
        impl Eq for Point {}
        impl Ord for Point {
            fn cmp(a, b) { 0 }
        }
        "#,
    );
}

// ── Protocol method call typing ─────────────────────────────────────────

#[test]
fn protocol_method_call_ok() {
    common::typecheck_ok(
        r#"
        protocol Greet {
            fn greet(self)
        }
        enum Animal {
            Dog,
        }
        impl Greet for Animal {
            fn greet(self) { "Woof!" }
        }
        let dog = Animal::Dog;
        Greet::greet(dog);
        "#,
    );
}

#[test]
fn protocol_method_call_with_return_type_propagates() {
    common::typecheck_ok(
        r#"
        protocol Greet {
            fn greet(self) -> String
        }
        enum Animal {
            Dog,
        }
        impl Greet for Animal {
            fn greet(self) -> String { "Woof!" }
        }
        let dog = Animal::Dog;
        let greeting: String = Greet::greet(dog);
        "#,
    );
}

// ── Default method bodies ───────────────────────────────────────────────

#[test]
fn protocol_default_method_not_required_in_impl() {
    common::typecheck_ok(
        r#"
        protocol Display {
            fn to_string(self) -> String
            fn display(self) { log(Display::to_string(self)) }
        }
        struct Point { x: isize, y: isize }
        impl Display for Point {
            fn to_string(self) -> String { "point" }
        }
        "#,
    );
}

// ── Empty impl for constraint protocol ──────────────────────────────────

#[test]
fn empty_impl_for_constraint_protocol_with_no_methods_ok() {
    common::typecheck_ok(
        r#"
        protocol PartialEq {
            fn eq(self, other)
        }
        protocol Eq : PartialEq {}
        struct Point { x: isize, y: isize }
        impl PartialEq for Point {
            fn eq(a, b) { true }
        }
        impl Eq for Point {}
        "#,
    );
}

// ── Associated types ────────────────────────────────────────────────────

#[test]
fn protocol_with_associated_type_no_errors() {
    common::typecheck_ok(
        r#"
        protocol Functor {
            type Wrapped
            fn map(self, f)
        }
        "#,
    );
}

#[test]
fn impl_associated_type_binding_ok() {
    common::typecheck_ok(
        r#"
        protocol Functor {
            type Wrapped
            fn map(self, f)
        }
        struct MyBox { value: i64 }
        impl Functor for MyBox {
            type Wrapped = MyBox
            fn map(self, f) { MyBox { value: f(self.value) } }
        }
        "#,
    );
}

#[test]
fn impl_missing_associated_type_error() {
    let errs = common::typecheck_errors(
        r#"
        protocol Functor {
            type Wrapped
            fn map(self, f)
        }
        struct MyBox { value: i64 }
        impl Functor for MyBox {
            fn map(self, f) { MyBox { value: f(self.value) } }
        }
        "#,
    );
    assert!(
        errs.iter()
            .any(|e| e.contains("missing associated type `Wrapped`") && e.contains("Functor")),
        "expected missing associated type error, got: {errs:?}"
    );
}

#[test]
fn impl_unexpected_associated_type_error() {
    // Like the constraint test above, semantic analysis catches this before
    // the type checker runs. We verify the semantic pass diagnoses it.
    let (mut ast, _parse_meta) = tlang_parser::Parser::from_source(
        r#"
        protocol Greet {
            fn greet(self)
        }
        struct Dog { name: String }
        impl Greet for Dog {
            type Output = String
            fn greet(self) { "Woof!" }
        }
        "#,
    )
    .parse()
    .unwrap();
    let mut semantic_analyzer = tlang_semantics::SemanticAnalyzer::default();
    let result = semantic_analyzer.analyze(&mut ast);
    assert!(
        result.is_err(),
        "expected semantic analysis to catch unexpected associated type"
    );
    let diagnostics = result.unwrap_err();
    assert!(
        diagnostics
            .iter()
            .any(|d| d.message().contains("unexpected associated type `Output`")),
        "expected unexpected associated type error, got: {diagnostics:?}"
    );
}

// ── Blanket impls and where clauses ─────────────────────────────────────

#[test]
fn blanket_impl_with_where_clause_ok() {
    common::typecheck_ok(
        r#"
        protocol Iterator {
            fn next(self)
        }
        protocol Functor {
            fn map(self, f)
        }
        impl<I> Functor for I
        where
            I: Iterator
        {
            fn map(self, f) { f(self) }
        }
        "#,
    );
}

#[test]
fn blanket_impl_where_clause_unknown_bound_error() {
    let errs = common::typecheck_errors(
        r#"
        protocol Functor {
            fn map(self, f)
        }
        impl<I> Functor for I
        where
            I: NonExistentProtocol
        {
            fn map(self, f) { f(self) }
        }
        "#,
    );
    assert!(
        errs.iter()
            .any(|e| e.contains("Use of undeclared type `NonExistentProtocol`")),
        "expected undeclared type error, got: {errs:?}"
    );
}

#[test]
fn blanket_impl_does_not_require_direct_constraint_check() {
    // A blanket impl should not require constraint protocols to be
    // checked against the type parameter (since constraints are deferred
    // to instantiation sites).
    common::typecheck_ok(
        r#"
        protocol PartialEq {
            fn eq(self, other)
        }
        protocol Eq : PartialEq {}
        impl<T> Eq for T {
            fn eq(self, other) { true }
        }
        "#,
    );
}

// ── List literal type inference ─────────────────────────────────────────

#[test]
fn list_literal_inferred_as_list_ok() {
    common::typecheck_ok("let a = [1, 2, 3];");
}

#[test]
fn empty_list_literal_inferred_as_list_ok() {
    common::typecheck_ok("let a = [];");
}

#[test]
fn list_literal_annotation_matches_ok() {
    common::typecheck_ok("let a: List = [1, 2, 3];");
}

#[test]
fn bare_list_annotation_is_not_assignable_to_typed_list() {
    let errs = common::typecheck_errors(
        r#"
        fn id(xs: List) -> List { xs }

        fn typed(xs: List<i64>) -> List<i64> {
            id(xs)
        }
        "#,
    );
    assert!(
        errs.iter().any(|e| e.contains("return type mismatch")),
        "expected return type mismatch, got: {errs:?}"
    );
}

#[test]
fn list_literal_annotation_mismatch_error() {
    let errs = common::typecheck_errors("let a: i64 = [1, 2];");
    assert!(
        errs.iter().any(|e| e.contains("type mismatch in binding")),
        "expected binding type mismatch, got: {errs:?}"
    );
}

#[test]
fn list_literal_return_type_mismatch_error() {
    let errs = common::typecheck_errors(
        r#"
        fn get_number() -> i64 { [1, 2, 3] }
        "#,
    );
    assert!(
        errs.iter().any(|e| e.contains("return type mismatch")),
        "expected return type mismatch, got: {errs:?}"
    );
}

#[test]
fn dict_literal_return_type_mismatch_error() {
    let errs = common::typecheck_errors(
        r#"
        fn get_number() -> i64 { return {key: "value"}; }
        "#,
    );
    assert!(
        errs.iter().any(|e| e.contains("return type mismatch")),
        "expected return type mismatch, got: {errs:?}"
    );
}

// ── builtin_types registry ──────────────────────────────────────────────

#[test]
fn builtin_types_list_lookup_returns_prim_ty_res() {
    use tlang_hir::TyKind;
    use tlang_typeck::builtin_types;

    let ty = builtin_types::lookup("List").expect("List should be a known builtin type");
    match ty {
        TyKind::Path(path, type_args) => {
            assert!(type_args.is_empty());
            assert!(path.res.is_prim_ty(), "List path should carry Res::PrimTy");
            assert_eq!(path.join("::"), "List");
        }
        other => panic!("expected TyKind::Path for List, got {other:?}"),
    }
}

#[test]
fn builtin_types_dict_lookup_returns_prim_ty_res() {
    use tlang_hir::TyKind;
    use tlang_typeck::builtin_types;

    let ty = builtin_types::lookup("Dict").expect("Dict should be a known builtin type");
    match ty {
        TyKind::Path(path, type_args) => {
            assert!(type_args.is_empty());
            assert!(path.res.is_prim_ty(), "Dict path should carry Res::PrimTy");
            assert_eq!(path.join("::"), "Dict");
        }
        other => panic!("expected TyKind::Path for Dict, got {other:?}"),
    }
}

#[test]
fn builtin_types_unknown_name_returns_none() {
    use tlang_typeck::builtin_types;
    assert!(builtin_types::lookup("Foo").is_none());
    assert!(builtin_types::lookup("i64").is_none());
}

#[test]
fn dict_literal_inferred_as_dict_ok() {
    common::typecheck_ok("let a = {};");
    common::typecheck_ok(r#"let b = {key: "value"};"#);
}

#[test]
fn dict_literal_annotation_matches_ok() {
    common::typecheck_ok("let a: Dict = {};");
}

#[test]
fn dict_literal_annotation_mismatch_error() {
    let errs = common::typecheck_errors("let a: i64 = {};");
    assert!(
        errs.iter().any(|e| e.contains("type mismatch in binding")),
        "expected binding type mismatch, got: {errs:?}"
    );
}

// ── Closure typing ──────────────────────────────────────────────────────

#[test]
fn closure_type_is_fn() {
    // A closure with annotated params should produce a Fn type that
    // can be used as a callee.
    common::typecheck_ok(
        r#"
        let f = fn(x: i64) -> i64 { x + 1 };
        let y: i64 = f(42);
        "#,
    );
}

#[test]
fn closure_unannotated_params_remain_unknown() {
    // Without calling context, unannotated closure params remain unknown
    // and the closure body operates on unknown types.
    common::typecheck_ok(
        r#"
        let f = fn(x) { x };
        "#,
    );
}

#[test]
fn closure_return_type_inferred_from_body() {
    // When no return type is annotated, the closure's return type is
    // inferred from the body's trailing expression.
    common::typecheck_ok(
        r#"
        let f = fn(x: i64) { x + 1 };
        let y: i64 = f(42);
        "#,
    );
}

#[test]
fn closure_multi_param_typed() {
    // Multiple typed parameters produce the correct Fn type.
    common::typecheck_ok(
        r#"
        let add = fn(a: i64, b: i64) -> i64 { a + b };
        let result: i64 = add(1, 2);
        "#,
    );
}

#[test]
fn closure_called_directly_ok() {
    // A typed closure called directly.
    common::typecheck_ok(
        r#"
        let inc = fn(x: i64) -> i64 { x + 1 };
        let result: i64 = inc(10);
        "#,
    );
}

// ── Function type annotations & bidirectional inference ─────────────────

#[test]
fn fn_type_annotation_on_binding() {
    // A binding with a function-type annotation should accept a closure.
    common::typecheck_ok(
        r#"
        let f: fn(i64) -> i64 = fn(x: i64) { x + 1 };
        let result: i64 = f(42);
        "#,
    );
}

#[test]
fn fn_type_annotation_on_param_enables_call() {
    // A function parameter with a function-type annotation should be callable
    // inside the function body.
    common::typecheck_ok(
        r#"
        fn call_fn(f: fn(i64) -> i64, x: i64) -> i64 { f(x) }
        let result: i64 = call_fn(fn(x: i64) { x + 1 }, 42);
        "#,
    );
}

#[test]
fn bidirectional_inference_fills_closure_params() {
    // When a closure is passed to a function that expects fn(i64) -> i64,
    // the unannotated closure parameter `x` should be inferred as i64.
    common::typecheck_ok(
        r#"
        fn call_fn(f: fn(i64) -> i64, x: i64) -> i64 { f(x) }
        let result: i64 = call_fn(fn(x) { x + 1 }, 42);
        "#,
    );
}

#[test]
fn bidirectional_inference_multi_param() {
    // Multiple closure params inferred from the callee's function type.
    common::typecheck_ok(
        r#"
        fn combine(f: fn(i64, i64) -> i64, a: i64, b: i64) -> i64 { f(a, b) }
        let result: i64 = combine(fn(x, y) { x + y }, 1, 2);
        "#,
    );
}

#[test]
fn bidirectional_inference_preserves_annotated_closure_params() {
    // Explicit type annotations on closure params are not overridden
    // by the expected function type.
    common::typecheck_ok(
        r#"
        fn call_fn(f: fn(i64) -> i64, x: i64) -> i64 { f(x) }
        let result: i64 = call_fn(fn(x: i64) { x + 1 }, 42);
        "#,
    );
}

#[test]
fn fn_type_named_params_annotation() {
    // Named parameters in a function type annotation should work.
    common::typecheck_ok(
        r#"
        let f: fn(value: i64) -> i64 = fn(x: i64) { x + 1 };
        let result: i64 = f(42);
        "#,
    );
}

// ── Conversion expressions (`as` / `as?`) ──────────────────────────────

#[test]
fn cast_int_widening_ok() {
    common::typecheck_ok("let x: i32 = 1 as i32; let y = x as i64;");
}

#[test]
fn cast_int_narrowing_ok() {
    common::typecheck_ok("let x = 42; let y = x as i32;");
}

#[test]
fn cast_int_to_float_ok() {
    common::typecheck_ok("let x = 42; let y = x as f64;");
}

#[test]
fn cast_float_to_float_ok() {
    common::typecheck_ok("let x = 3.14; let y = x as f32;");
}

#[test]
fn cast_signed_to_unsigned_ok() {
    common::typecheck_ok("let x = 42; let y = x as u64;");
}

#[test]
fn cast_unsigned_to_signed_ok() {
    common::typecheck_ok("let x: u32 = 1 as u32; let y = x as i64;");
}

#[test]
fn cast_unknown_error() {
    let errs = common::typecheck_errors("fn f(x) { let y = x as i64; }");
    assert_eq!(errs.len(), 1);
    assert!(
        errs[0].contains("cannot use `as` cast on `unknown`"),
        "unexpected: {}",
        errs[0]
    );
}

#[test]
fn cast_string_to_int_error() {
    let errs = common::typecheck_errors(r#"let x = "hello"; let y = x as i64;"#);
    assert_eq!(errs.len(), 1);
    assert!(
        errs[0].contains("no `Into<i64>` implementation for `String`"),
        "unexpected: {}",
        errs[0]
    );
}

#[test]
fn cast_bool_to_int_error() {
    let errs = common::typecheck_errors("let x = true; let y = x as i64;");
    assert_eq!(errs.len(), 1);
    assert!(
        errs[0].contains("no `Into<i64>` implementation for `bool`"),
        "unexpected: {}",
        errs[0]
    );
}

#[test]
fn cast_identity_ok() {
    common::typecheck_ok("let x = 42; let y = x as i64;");
}

#[test]
fn try_cast_unknown_ok() {
    // `as?` is how unknown enters typed code.
    common::typecheck_ok("fn f(x) { let y = x as? i64; }");
}

#[test]
fn try_cast_float_to_int_ok() {
    common::typecheck_ok("let x = 3.14; let y = x as? i64;");
}

#[test]
fn try_cast_numeric_ok() {
    common::typecheck_ok("let x = 42; let y = x as? f64;");
}

#[test]
fn try_cast_identity_ok() {
    common::typecheck_ok("let x = 42; let y = x as? i64;");
}

#[test]
fn cast_result_type_propagates_to_binding() {
    // The result of `as` should be the target type, usable in typed bindings.
    common::typecheck_ok(
        r#"
        let x = 42;
        let y: i32 = x as i32;
        "#,
    );
}

#[test]
fn cast_result_type_mismatch_in_binding() {
    // The result type of `x as i32` is `i32`, not `i64`.
    let errs = common::typecheck_errors(
        r#"
        let x = 42;
        let y: i64 = x as i32;
        "#,
    );
    assert_eq!(errs.len(), 1);
    assert!(
        errs[0].contains("type mismatch in binding"),
        "unexpected: {}",
        errs[0]
    );
}

#[test]
fn cast_with_matching_into_impl_ok() {
    // User-defined `impl Into<Target> for Source` allows `as` cast.
    common::typecheck_ok(
        r#"
        protocol Into<T> { fn into(self) -> T }
        struct Celsius { degrees: f64 }
        struct Fahrenheit { degrees: f64 }
        impl Into<Fahrenheit> for Celsius {
            fn into(self) { Fahrenheit { degrees: self.degrees * 1.8 + 32.0 } }
        }
        let c = Celsius { degrees: 100.0 };
        let f = c as Fahrenheit;
        "#,
    );
}

#[test]
fn cast_with_wrong_into_target_error() {
    // `impl Into<Wrapper> for Wrapper` should NOT allow `x as i64`.
    let errs = common::typecheck_errors(
        r#"
        protocol Into<T> { fn into(self) -> T }
        struct Wrapper { value: i64 }
        impl Into<Wrapper> for Wrapper {
            fn into(self) { Wrapper { value: self.value } }
        }
        let x: Wrapper = Wrapper { value: 1 };
        let y = x as i64;
        "#,
    );
    assert_eq!(errs.len(), 1, "expected exactly 1 error, got: {errs:?}");
    assert!(
        errs[0].contains("no `Into<i64>` implementation for `Wrapper`"),
        "unexpected: {}",
        errs[0]
    );
}

// ── Native (builtin) protocol recognition ───────────────────────────────

#[test]
fn impl_against_native_protocol_functor_ok() {
    // The type checker should recognise `Functor` as a native protocol and
    // validate the impl block (method present + associated type bound).
    common::typecheck_ok(
        r#"
        struct MyBox { value: i64 }
        impl Functor for MyBox {
            type Wrapped = MyBox
            fn map(self, f) { MyBox { value: f(self.value) } }
        }
        "#,
    );
}

#[test]
fn impl_against_native_protocol_display_ok() {
    common::typecheck_ok(
        r#"
        struct Dog { name: String }
        impl Display for Dog {
            fn to_string(self) { self.name }
        }
        "#,
    );
}

#[test]
fn impl_against_native_protocol_display_uses_default_body() {
    // `Display` has a default `to_string` body, so an empty impl is valid.
    common::typecheck_ok(
        r#"
        struct Cat { name: String }
        impl Display for Cat {}
        "#,
    );
}

#[test]
fn impl_against_native_protocol_missing_method_error() {
    let errs = common::typecheck_errors(
        r#"
        struct Dog { name: String }
        impl Functor for Dog {
            type Wrapped = Dog
        }
        "#,
    );
    assert!(
        errs.iter()
            .any(|e| e.contains("missing method `map`") && e.contains("Functor")),
        "expected missing method error, got: {errs:?}"
    );
}

#[test]
fn impl_against_native_protocol_missing_associated_type_error() {
    let errs = common::typecheck_errors(
        r#"
        struct Dog { name: String }
        impl Functor for Dog {
            fn map(self, f) { Dog { name: f(self.name) } }
        }
        "#,
    );
    assert!(
        errs.iter()
            .any(|e| e.contains("missing associated type `Wrapped`") && e.contains("Functor")),
        "expected missing associated type error, got: {errs:?}"
    );
}

#[test]
fn where_clause_bound_referencing_native_protocol_ok() {
    common::typecheck_ok(
        r#"
        protocol MyProto {
            fn do_thing(self)
        }
        impl<I> MyProto for I
        where
            I: Functor
        {
            fn do_thing(self) { self }
        }
        "#,
    );
}

#[test]
fn impl_against_native_protocol_into_ok() {
    common::typecheck_ok(
        r#"
        struct Wrapper { value: i64 }
        impl Into<i64> for Wrapper {
            fn into(self) { self.value }
        }
        "#,
    );
}

#[test]
fn impl_against_native_protocol_try_into_ok() {
    common::typecheck_ok(
        r#"
        struct Wrapper { value: i64 }
        impl TryInto<i64> for Wrapper {
            fn try_into(self) { Result::Ok(self.value) }
        }
        "#,
    );
}

#[test]
fn impl_against_native_protocol_iterator_ok() {
    common::typecheck_ok(
        r#"
        struct Counter { n: i64 }
        impl Iterator for Counter {
            fn next(self) { Option::Some(self.n) }
        }
        "#,
    );
}

// ── Builtin method resolution ───────────────────────────────────────────

#[test]
fn builtin_regex_replace_all_returns_string() {
    common::typecheck_ok(
        r#"
        let regex = re"test";
        let result: String = regex.replace_all("input", "output");
        "#,
    );
}

#[test]
fn builtin_regex_replace_all_type_mismatch() {
    let errs = common::typecheck_errors(
        r#"
        let regex = re"test";
        let result: i64 = regex.replace_all("input", "output");
        "#,
    );
    assert!(
        errs.iter().any(|e| e.contains("type mismatch in binding")),
        "expected binding type mismatch, got: {errs:?}"
    );
}

#[test]
fn builtin_regex_test_returns_bool() {
    common::typecheck_ok(
        r#"
        let regex = re"test";
        let result: bool = regex.test("input");
        "#,
    );
}

#[test]
fn builtin_regex_replace_first_returns_string() {
    common::typecheck_ok(
        r#"
        let regex = re"test";
        let result: String = regex.replace_first("input", "output");
        "#,
    );
}

#[test]
fn builtin_stringbuf_to_string_returns_string() {
    common::typecheck_ok(
        r#"
        let buf = string::StringBuf();
        let s: String = buf.to_string();
        "#,
    );
}

#[test]
fn builtin_stringbuf_len_returns_i64() {
    common::typecheck_ok(
        r#"
        let buf = string::StringBuf();
        let n: i64 = buf.len();
        "#,
    );
}

#[test]
fn builtin_stringbuf_is_empty_returns_bool() {
    common::typecheck_ok(
        r#"
        let buf = string::StringBuf();
        let b: bool = buf.is_empty();
        "#,
    );
}

#[test]
fn builtin_option_is_some_returns_bool() {
    common::typecheck_ok(
        r#"
        let opt = Option::Some(42);
        let b: bool = opt.is_some();
        "#,
    );
}

#[test]
fn builtin_option_is_none_returns_bool() {
    common::typecheck_ok(
        r#"
        let opt = Option::Some(42);
        let b: bool = opt.is_none();
        "#,
    );
}

#[test]
fn builtin_result_is_ok_returns_bool() {
    common::typecheck_ok(
        r#"
        let res = Result::Ok(42);
        let b: bool = res.is_ok();
        "#,
    );
}

#[test]
fn builtin_result_is_err_returns_bool() {
    common::typecheck_ok(
        r#"
        let res = Result::Err("oops");
        let b: bool = res.is_err();
        "#,
    );
}

#[test]
fn builtin_re_returns_regex() {
    // re"..." returns Regex, so calling a Regex method should work.
    common::typecheck_ok(
        r#"
        let result: String = re"test".replace_all("input", "output");
        "#,
    );
}

#[test]
fn regex_flags_allows_zero_argument_getter() {
    common::typecheck_ok(
        r#"
        let flags: String = re"hello".flags();
        "#,
    );
}

#[test]
fn builtin_f_returns_string() {
    common::typecheck_ok(
        r#"
        let x = 42;
        let s: String = f"value: {x}";
        "#,
    );
}

#[test]
fn builtin_method_chaining_regex_pipeline() {
    // Mimics the html.tlang pattern: re"&".replace_all(input, "&amp;")
    common::typecheck_ok(
        r#"
        fn escape(input: String) -> String {
            re"&".replace_all(input, "&amp;")
        }
        "#,
    );
}

// ── Generic builtin methods ────────────────────────────────────────────

#[test]
fn list_foldl_infers_return_type_from_init() {
    // foldl(init: U, f: fn(U, T) -> U) -> U
    // init = 0 (i64) → U = i64 → return type is i64
    common::typecheck_ok(
        r#"
        fn sum(xs: List) -> i64 {
            xs.foldl(0, fn(acc, x) { acc })
        }
        "#,
    );
}

#[test]
fn list_foldl_wrong_return_type_error() {
    // foldl returns i64 (inferred from init=0), but annotation says String
    let errs = common::typecheck_errors(
        r#"
        fn sum(xs: List) -> String {
            xs.foldl(0, fn(acc, x) { acc })
        }
        "#,
    );
    assert!(
        errs.iter().any(|e| e.contains("return type mismatch")),
        "expected return type mismatch, got: {errs:?}"
    );
}

#[test]
fn list_map_infers_return_type_from_closure() {
    // map(f: fn(T) -> U) -> List<U>
    // closure returns String → U = String → map returns List<String>
    common::typecheck_ok(
        r#"
        fn to_strings(xs: List) -> List {
            xs.map(fn(x) { "hello" })
        }
        "#,
    );
}

#[test]
fn list_foldl_string_init_infers_string_return() {
    // foldl with String init → return type is String
    common::typecheck_ok(
        r#"
        fn join(xs: List) -> String {
            xs.foldl("", fn(acc, x) { acc })
        }
        "#,
    );
}

#[test]
fn list_foldl_string_init_wrong_return_error() {
    // foldl returns String (from init=""), but annotation says i64
    let errs = common::typecheck_errors(
        r#"
        fn join(xs: List) -> i64 {
            xs.foldl("", fn(acc, x) { acc })
        }
        "#,
    );
    assert!(
        errs.iter().any(|e| e.contains("return type mismatch")),
        "expected return type mismatch, got: {errs:?}"
    );
}

#[test]
fn list_filter_returns_list() {
    common::typecheck_ok(
        r#"
        fn evens(xs: List) -> List {
            xs.filter(fn(x) { true })
        }
        "#,
    );
}

#[test]
fn list_any_returns_bool() {
    common::typecheck_ok(
        r#"
        fn has_positive(xs: List) -> bool {
            xs.any(fn(x) { true })
        }
        "#,
    );
}

#[test]
fn list_all_returns_bool() {
    common::typecheck_ok(
        r#"
        fn all_positive(xs: List) -> bool {
            xs.all(fn(x) { true })
        }
        "#,
    );
}

#[test]
fn option_map_infers_return_type() {
    // Option.map(f: fn(T) -> U) -> U
    // closure returns String → map returns String
    common::typecheck_ok(
        r#"
        fn transform(opt: Option) -> String {
            opt.map(fn(x) { "value" })
        }
        "#,
    );
}

#[test]
fn option_map_wrong_return_type_error() {
    // map closure returns String, but function expects i64
    let errs = common::typecheck_errors(
        r#"
        fn transform(opt: Option) -> i64 {
            opt.map(fn(x) { "value" })
        }
        "#,
    );
    assert!(
        errs.iter().any(|e| e.contains("return type mismatch")),
        "expected return type mismatch, got: {errs:?}"
    );
}

#[test]
fn result_map_infers_return_type() {
    common::typecheck_ok(
        r#"
        fn transform(r: Result) -> String {
            r.map(fn(x) { "ok" })
        }
        "#,
    );
}

#[test]
fn string_split_returns_list() {
    common::typecheck_ok(
        r#"
        fn words(s: String) -> List {
            s.split(" ")
        }
        "#,
    );
}

#[test]
fn typed_list_return_accepts_bare_list_on_expected_side_only() {
    common::typecheck_ok(
        r#"
        fn erase(xs: List<i64>) -> List {
            xs
        }
        "#,
    );
}

#[test]
fn string_contains_returns_bool() {
    common::typecheck_ok(
        r#"
        fn has_hello(s: String) -> bool {
            s.contains("hello")
        }
        "#,
    );
}

#[test]
fn string_len_returns_i64() {
    common::typecheck_ok(
        r#"
        fn length(s: String) -> i64 {
            s.len()
        }
        "#,
    );
}

#[test]
fn string_trim_returns_string() {
    common::typecheck_ok(
        r#"
        fn cleaned(s: String) -> String {
            s.trim()
        }
        "#,
    );
}

#[test]
fn string_method_wrong_return_type_error() {
    let errs = common::typecheck_errors(
        r#"
        fn length(s: String) -> String {
            s.len()
        }
        "#,
    );
    assert!(
        errs.iter().any(|e| e.contains("return type mismatch")),
        "expected return type mismatch, got: {errs:?}"
    );
}

// ── Builtin free-function closure inference (map, Functor::map) ─────────

#[test]
fn functor_map_infers_closure_param_from_list() {
    // Functor::map([1,2,3], fn(x) { x + 1 }) → x should be i64
    // Returning String from closure while assigning to List<i64> should error
    let errs = common::typecheck_errors(
        r#"
        let xs: List<i64> = [1, 2, 3];
        let ys: List<i64> = Functor::map(xs, fn(x) { "hello" });
        "#,
    );
    assert!(
        !errs.is_empty(),
        "expected type error: closure returns String but List<i64> expected"
    );
}

#[test]
fn functor_map_closure_ok() {
    // Functor::map should work without errors when types are consistent
    common::typecheck_ok(
        r#"
        let xs: List<i64> = [1, 2, 3];
        let ys = Functor::map(xs, fn(x) { x + 1 });
        "#,
    );
}

#[test]
fn builtin_functor_map_accepts_option_receiver() {
    common::typecheck_ok(
        r#"
        let some = Option::Some(5);
        Functor::map(some, fn(_) { "mapped" });

        let none = Option::None;
        Functor::map(none, fn(_) { "mapped" });
        "#,
    );
}

#[test]
fn builtin_functor_map_accepts_result_receiver() {
    common::typecheck_ok(
        r#"
        let ok = Result::Ok(10);
        Functor::map(ok, fn(_) { "mapped" });

        let err = Result::Err("error");
        Functor::map(err, fn(_) { "mapped" });
        "#,
    );
}

#[test]
fn user_protocol_dispatch_infers_receiver_type_for_closure() {
    common::typecheck_ok(
        r#"
        protocol MyFunctor<T> {
          type Wrapped<U>
          fn map<U>(self, f: fn(T) -> U) -> Wrapped<U>
        }

        impl<T> MyFunctor<T> for Option<T> {
          type Wrapped<U> = Option<U>
          fn map<U>(Option::Some(x), f) { Option::Some(f(x)) }
          fn map<U>(Option::None, _) { Option::None }
        }

        let mapped = MyFunctor::map(Option::Some(5), fn (x) { x * 2 });
        "#,
    );
}

#[test]
fn spread_string_is_compatible_with_list_string_param() {
    common::typecheck_ok(
        r#"
        fn reverse_chars([]: List<String>) -> String { "" }
        fn reverse_chars([x, ...xs]: List<String>) -> String { reverse_chars(xs) + x }

        fn reverse_string(str: String) -> String { reverse_chars([...str]) }
        "#,
    );
}

#[test]
fn list_pattern_without_element_annotation_stays_permissive() {
    common::typecheck_ok(
        r#"
        fn sum([], acc: i64) -> i64 { acc }
        fn sum([x, ...xs], acc: i64) -> i64 { rec sum(xs, acc + x) }
        "#,
    );
}

#[test]
fn pipeline_map_infers_closure_param_from_list() {
    // [1,2,3] |> map(fn(x) { "hello" }) → assigning to List<i64> should error
    let errs = common::typecheck_errors(
        r#"
        let ys: List<i64> = [1, 2, 3] |> map(fn(x) { "hello" });
        "#,
    );
    assert!(
        !errs.is_empty(),
        "expected type error: closure returns String but List<i64> expected"
    );
}

#[test]
fn pipeline_map_closure_ok() {
    // Pipeline map should work without errors when types are consistent
    common::typecheck_ok(
        r#"
        let ys = [1, 2, 3] |> map(fn(x) { x + 1 });
        "#,
    );
}

#[test]
fn pipeline_map_chain_infers_types() {
    // Full pipeline: [1,2,3] |> map(fn(x) { x * 2 }) — result should be List<i64>
    // Assigning to String should error
    let errs = common::typecheck_errors(
        r#"
        let result: String = [1, 2, 3] |> map(fn(x) { x * 2 });
        "#,
    );
    assert!(
        !errs.is_empty(),
        "expected type error: List<i64> assigned to String"
    );
}
