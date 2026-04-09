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
            Dog(name),
            Cat(name),
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
            Dog(name),
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
            Dog(name),
            Cat(name),
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
            Dog(name),
            Cat(name),
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
        TyKind::Path(path) => {
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
        TyKind::Path(path) => {
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
