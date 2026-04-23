use js_sys::Function;
use serde::Deserialize;
use std::sync::Once;
use tlang_bindings_js::tlang::{Runner, Tlang};
use wasm_bindgen::JsCast;
use wasm_bindgen::prelude::*;
use wasm_bindgen_test::*;

// ---------------------------------------------------------------------------
// Test infrastructure
// ---------------------------------------------------------------------------

/// Ensure WASM constructors (and thus `inventory` registrations) have run.
///
/// `#[wasm_bindgen(start)]` does not fire in `wasm-pack test --node`, so we
/// call `init()` manually the first time any test needs it.
static INIT: Once = Once::new();

fn ensure_init() {
    INIT.call_once(|| {
        tlang_bindings_js::init();
    });
}

/// Captures values passed to the `capture(x)` function from tlang code.
///
/// Uses a global JS array so we can create the JS function with an explicit
/// parameter (`length = 1`) without requiring wasm_bindgen Closure lifetimes.
/// Tests run sequentially in `wasm-pack test --node`, so the global is safe.
struct Capture;

#[allow(dead_code)]
impl Capture {
    fn reset() {
        js_sys::eval("globalThis.__tlang_capture = []").unwrap();
    }

    fn function() -> Function {
        // Coerce BigInt (from i64/u64) to Number so .as_f64() works in assertions
        Function::new_with_args(
            "x",
            "globalThis.__tlang_capture.push(typeof x === 'bigint' ? Number(x) : x)",
        )
    }

    fn all() -> js_sys::Array {
        js_sys::eval("globalThis.__tlang_capture")
            .unwrap()
            .dyn_into::<js_sys::Array>()
            .unwrap()
    }

    fn first_number() -> Option<f64> {
        Self::all().get(0).as_f64()
    }

    fn first_bool() -> Option<bool> {
        Self::all().get(0).as_bool()
    }

    fn first_string() -> Option<String> {
        Self::all().get(0).as_string()
    }

    fn first_is_truthy() -> bool {
        let v = Self::all().get(0);
        !v.is_null() && !v.is_undefined() && v.as_bool() != Some(false)
    }
}

/// Run a tlang program with the interpreter, capturing values via `capture()`.
fn eval_capturing(source: &str) -> Result<(), JsError> {
    ensure_init();
    Capture::reset();
    let mut tlang = Tlang::new(source.to_string(), Runner::Interpreter);
    tlang.define_js_fn("capture", Capture::function());
    tlang.eval()?;
    Ok(())
}

// ---------------------------------------------------------------------------
// Smoketest
// ---------------------------------------------------------------------------

#[wasm_bindgen_test]
fn smoketest() -> Result<(), JsError> {
    ensure_init();
    let mut tlang = Tlang::new("log(1);".to_string(), Runner::Interpreter);
    tlang.eval()?;
    Ok(())
}

// ---------------------------------------------------------------------------
// Interpreter – expressions & operators
// ---------------------------------------------------------------------------

#[wasm_bindgen_test]
fn interpreter_arithmetic() -> Result<(), JsError> {
    // 2 + 3 * 4 = 14  (operator precedence)
    eval_capturing("capture(2 + 3 * 4);")?;
    assert_eq!(Capture::first_number(), Some(14.0));
    Ok(())
}

#[wasm_bindgen_test]
fn interpreter_power_operator() -> Result<(), JsError> {
    eval_capturing("capture(2 ** 10);")?;
    assert_eq!(Capture::first_number(), Some(1024.0));
    Ok(())
}

#[wasm_bindgen_test]
fn interpreter_boolean_expressions() -> Result<(), JsError> {
    eval_capturing("capture(true && !false);")?;
    assert_eq!(Capture::first_bool(), Some(true));
    Ok(())
}

#[wasm_bindgen_test]
fn interpreter_comparison_operators() -> Result<(), JsError> {
    eval_capturing("capture(3 > 2 && 1 < 2 && 2 == 2 && 3 != 4);")?;
    assert_eq!(Capture::first_bool(), Some(true));
    Ok(())
}

// ---------------------------------------------------------------------------
// Interpreter – functions & recursion
// ---------------------------------------------------------------------------

#[wasm_bindgen_test]
fn interpreter_pattern_matching_factorial() -> Result<(), JsError> {
    eval_capturing(
        "fn factorial(0) { 1 }
         fn factorial(n) { n * factorial(n - 1) }
         capture(factorial(5));",
    )?;
    assert_eq!(Capture::first_number(), Some(120.0));
    Ok(())
}

#[wasm_bindgen_test]
fn interpreter_if_else_factorial() -> Result<(), JsError> {
    eval_capturing(
        "fn fac(n) { if n <= 1 { 1 } else { n * fac(n - 1) } }
         capture(fac(6));",
    )?;
    assert_eq!(Capture::first_number(), Some(720.0));
    Ok(())
}

#[wasm_bindgen_test]
fn interpreter_tail_recursive_factorial() -> Result<(), JsError> {
    eval_capturing(
        "fn factorial(n) { go(n, 1) }
         fn go(0, acc) { acc }
         fn go(n, acc) { rec go(n - 1, n * acc) }
         capture(factorial(10));",
    )?;
    assert_eq!(Capture::first_number(), Some(3628800.0));
    Ok(())
}

#[wasm_bindgen_test]
fn interpreter_fibonacci() -> Result<(), JsError> {
    eval_capturing(
        "fn fib(n) { if n <= 1 { n } else { fib(n - 1) + fib(n - 2) } }
         capture(fib(10));",
    )?;
    assert_eq!(Capture::first_number(), Some(55.0));
    Ok(())
}

#[wasm_bindgen_test]
fn interpreter_guard_clauses() -> Result<(), JsError> {
    // Route through the guarded clause for positive values.
    eval_capturing(
        "fn classify(n: i64) if n > 0 { 1 }
         fn classify(_: i64) { 0 }
         capture(classify(7));",
    )?;
    assert_eq!(Capture::first_number(), Some(1.0));
    Ok(())
}

// ---------------------------------------------------------------------------
// Interpreter – closures & higher-order functions
// ---------------------------------------------------------------------------

#[wasm_bindgen_test]
fn interpreter_closures() -> Result<(), JsError> {
    eval_capturing(
        "fn add(a) { fn(b) { a + b } }
         let add5 = add(5);
         capture(add5(3));",
    )?;
    assert_eq!(Capture::first_number(), Some(8.0));
    Ok(())
}

#[wasm_bindgen_test]
fn interpreter_map_builtin() -> Result<(), JsError> {
    // map squares [1,2,3,4,5] → [1,4,9,16,25], then sum with foldl → 55
    eval_capturing(
        "fn foldl([], acc, _) { acc }
         fn foldl([x, ...xs], acc, f) { rec foldl(xs, f(acc, x), f) }
         [1, 2, 3, 4, 5]
         |> map(fn(x) { x * x })
         |> foldl(0, fn(acc, x) { acc + x })
         |> capture;",
    )?;
    assert_eq!(Capture::first_number(), Some(55.0));
    Ok(())
}

#[wasm_bindgen_test]
fn interpreter_pipeline_operator() -> Result<(), JsError> {
    // squares of [0..9], keep evens, sum → 120
    eval_capturing(
        "fn filter([], _) { [] }
         fn filter([x, ...xs], f) if f(x) { [x, ...filter(xs, f)] }
         fn filter([_, ...xs], f) { rec filter(xs, f) }
         fn foldl([], acc, _) { acc }
         fn foldl([x, ...xs], acc, f) { rec foldl(xs, f(acc, x), f) }
         [0,1,2,3,4,5,6,7,8,9]
         |> map(fn(x) { x ** 2 })
         |> filter(fn(x) { x % 2 == 0 })
         |> foldl(0, fn(acc, x) { acc + x })
         |> capture;",
    )?;
    assert_eq!(Capture::first_number(), Some(120.0));
    Ok(())
}

#[wasm_bindgen_test]
fn interpreter_function_composition() -> Result<(), JsError> {
    eval_capturing(
        "fn compose(f, g) { fn(x) { f(g(x)) } }
         let double_then_inc = compose(fn(x) { x + 1 }, fn(x) { x * 2 });
         capture(double_then_inc(4));",
    )?;
    // (4 * 2) + 1 = 9
    assert_eq!(Capture::first_number(), Some(9.0));
    Ok(())
}

// ---------------------------------------------------------------------------
// Interpreter – enums
// ---------------------------------------------------------------------------

#[wasm_bindgen_test]
fn interpreter_option_some_unwrap() -> Result<(), JsError> {
    eval_capturing("capture(Option::Some(42).unwrap());")?;
    assert_eq!(Capture::first_number(), Some(42.0));
    Ok(())
}

#[wasm_bindgen_test]
fn interpreter_option_is_some() -> Result<(), JsError> {
    eval_capturing("capture(Option::Some(1).is_some());")?;
    assert_eq!(Capture::first_bool(), Some(true));
    Ok(())
}

#[wasm_bindgen_test]
fn interpreter_option_is_none() -> Result<(), JsError> {
    eval_capturing("capture(Option::None.is_none());")?;
    assert_eq!(Capture::first_bool(), Some(true));
    Ok(())
}

#[wasm_bindgen_test]
fn interpreter_custom_enum_pattern_matching() -> Result<(), JsError> {
    eval_capturing(
        "enum Expr {
             Number(isize),
             Add(Expr, Expr),
         }
         fn evaluate(Expr::Number(n)) { n }
         fn evaluate(Expr::Add(left, right)) { evaluate(left) + evaluate(right) }
         capture(evaluate(Expr::Add(Expr::Number(3 as isize), Expr::Number(4 as isize))));",
    )?;
    assert_eq!(Capture::first_number(), Some(7.0));
    Ok(())
}

#[wasm_bindgen_test]
fn interpreter_nested_enum_match() -> Result<(), JsError> {
    eval_capturing(
        "enum Expr {
             Num(isize),
             Add(Expr, Expr),
             Mul(Expr, Expr),
         }
         fn eval(Expr::Num(n)) { n }
         fn eval(Expr::Add(l, r)) { eval(l) + eval(r) }
         fn eval(Expr::Mul(l, r)) { eval(l) * eval(r) }
         // (2 + 3) * 4 = 20
         capture(eval(Expr::Mul(Expr::Add(Expr::Num(2 as isize), Expr::Num(3 as isize)), Expr::Num(4 as isize))));",
    )?;
    assert_eq!(Capture::first_number(), Some(20.0));
    Ok(())
}

// ---------------------------------------------------------------------------
// Interpreter – structs
// ---------------------------------------------------------------------------

#[wasm_bindgen_test]
fn interpreter_struct_field_access() -> Result<(), JsError> {
    eval_capturing(
        "struct Point { x: isize, y: isize }
         let p = Point { x: 3, y: 4 };
         capture(p.x + p.y);",
    )?;
    assert_eq!(Capture::first_number(), Some(7.0));
    Ok(())
}

#[wasm_bindgen_test]
fn interpreter_struct_static_constructor_and_method() -> Result<(), JsError> {
    eval_capturing(
        "struct Point { x: isize, y: isize }
         fn Point::new(x: isize, y: isize) -> Point { Point { x, y } }
         fn Point.sum(self) -> isize { self.x + self.y }
         capture(Point::new(3 as isize, 4 as isize).sum());",
    )?;
    assert_eq!(Capture::first_number(), Some(7.0));
    Ok(())
}

// ---------------------------------------------------------------------------
// Interpreter – loops
// ---------------------------------------------------------------------------

#[wasm_bindgen_test]
fn interpreter_for_loop_accumulation() -> Result<(), JsError> {
    eval_capturing(
        "let sum = 0;
         for x in [1, 2, 3, 4, 5] { sum = sum + x; };
         capture(sum);",
    )?;
    assert_eq!(Capture::first_number(), Some(15.0));
    Ok(())
}

#[wasm_bindgen_test]
fn interpreter_loop_with_break() -> Result<(), JsError> {
    eval_capturing(
        "let i = 0;
         let sum = 0;
         loop {
             if i >= 5 { break; }
             sum = sum + i;
             i = i + 1;
         }
         capture(sum);",
    )?;
    // 0+1+2+3+4 = 10
    assert_eq!(Capture::first_number(), Some(10.0));
    Ok(())
}

// ---------------------------------------------------------------------------
// JS runner – compilation
// ---------------------------------------------------------------------------

#[wasm_bindgen_test]
fn js_runner_produces_nonempty_output() -> Result<(), JsError> {
    let mut tlang = Tlang::new("pub fn add(a, b) { a + b }".to_string(), Runner::JavaScript);
    tlang.compile_to_js()?;
    assert!(!tlang.js().is_empty());
    Ok(())
}

#[wasm_bindgen_test]
fn js_runner_output_contains_function_name() -> Result<(), JsError> {
    let mut tlang = Tlang::new("pub fn add(a, b) { a + b }".to_string(), Runner::JavaScript);
    tlang.compile_to_js()?;
    let js = tlang.js();
    assert!(js.contains("add"), "Expected 'add' in generated JS:\n{js}");
    Ok(())
}

#[wasm_bindgen_test]
fn js_runner_source_map_is_generated() -> Result<(), JsError> {
    let mut tlang = Tlang::new("fn add(a, b) { a + b }".to_string(), Runner::JavaScript);
    let source_map = tlang.get_source_map();
    assert!(source_map.is_some(), "source map should be generated");
    assert!(!source_map.unwrap().is_empty());
    Ok(())
}

#[wasm_bindgen_test]
fn js_runner_js_ast_string_is_nonempty() -> Result<(), JsError> {
    let mut tlang = Tlang::new("fn foo(x) { x + 1 }".to_string(), Runner::JavaScript);
    tlang.compile_to_js()?;
    assert!(!tlang.js_ast_string().is_empty());
    Ok(())
}

// ---------------------------------------------------------------------------
// API surface
// ---------------------------------------------------------------------------

#[wasm_bindgen_test]
fn get_hir_string_nonempty() -> Result<(), JsError> {
    let mut tlang = Tlang::new("fn foo() { 1 + 2 }".to_string(), Runner::Interpreter);
    assert!(!tlang.hir_string().is_empty());
    Ok(())
}

#[wasm_bindgen_test]
fn get_ast_string_nonempty() -> Result<(), JsError> {
    let mut tlang = Tlang::new("fn foo() { 1 + 2 }".to_string(), Runner::Interpreter);
    assert!(!tlang.ast_string().is_empty());
    Ok(())
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct TestSignatureHelp {
    signatures: Vec<TestSignatureInformation>,
    active_signature: u32,
    active_parameter: u32,
}

#[derive(Deserialize)]
struct TestSignatureInformation {
    label: String,
}

#[wasm_bindgen_test]
fn get_signature_help_returns_function_signature() -> Result<(), JsError> {
    let source = "fn add(a: i64, b: i64) -> i64 { a + b }\nlet _ = add(1, 2);";
    let mut tlang = Tlang::new(source.to_string(), Runner::JavaScript);
    let cursor = source.find("2);").unwrap() as u32;
    let value = tlang.get_signature_help(cursor)?;
    let help: TestSignatureHelp = serde_wasm_bindgen::from_value(value).unwrap();

    assert_eq!(help.active_signature, 0);
    assert_eq!(help.active_parameter, 1);
    assert_eq!(help.signatures[0].label, "add(a: i64, b: i64) -> i64");
    Ok(())
}

#[wasm_bindgen_test]
fn get_type_at_position_returns_symbol_type() -> Result<(), JsError> {
    let source = "fn id(x: i64) -> i64 {\n  x\n}";
    let mut tlang = Tlang::new(source.to_string(), Runner::JavaScript);
    tlang.analyze();
    let cursor = (source.rfind("\n  x").unwrap() + 3) as u32;
    let value = tlang.get_type_at_position(cursor)?;
    let ty: String = serde_wasm_bindgen::from_value(value).unwrap();

    assert_eq!(ty, "i64");
    Ok(())
}

#[wasm_bindgen_test]
fn get_hir_pretty_nonempty() -> Result<(), JsError> {
    let mut tlang = Tlang::new("pub fn foo() { 1 + 2 }".to_string(), Runner::Interpreter);
    let pretty = tlang.hir_pretty(None)?;
    assert!(!pretty.is_empty());
    Ok(())
}

#[wasm_bindgen_test]
fn hir_pretty_shows_inferred_types_after_typecheck() -> Result<(), JsError> {
    let source = "fn inc(x: i64) { x + 1 }\ninc(2);";

    let mut interpreter = Tlang::new(source.to_string(), Runner::Interpreter);
    let interpreter_hir = interpreter.hir_pretty(None)?;
    assert!(interpreter_hir.contains("fn inc(x: i64) -> i64 {"));

    let mut javascript = Tlang::new(source.to_string(), Runner::JavaScript);
    let javascript_hir = javascript.hir_pretty(None)?;
    assert!(javascript_hir.contains("fn inc(x: i64) -> i64 {"));

    Ok(())
}

#[wasm_bindgen_test]
fn get_stdlib_source_nonempty() {
    assert!(!tlang_bindings_js::tlang::get_standard_library_source().is_empty());
}

#[wasm_bindgen_test]
fn get_stdlib_native_js_nonempty() {
    assert!(!tlang_bindings_js::tlang::get_standard_library_native_js().is_empty());
}

// ---------------------------------------------------------------------------
// Error / diagnostic reporting
// ---------------------------------------------------------------------------

#[wasm_bindgen_test]
fn no_parse_errors_for_valid_source() -> Result<(), JsError> {
    // A function definition is unambiguously valid at module level.
    let mut tlang = Tlang::new("fn add(a, b) { a + b }".to_string(), Runner::Interpreter);
    assert!(tlang.render_parse_errors().is_empty());
    Ok(())
}

#[wasm_bindgen_test]
fn semantic_error_for_undefined_variable() -> Result<(), JsError> {
    // Syntactically valid, but references an undefined variable in the body.
    let mut tlang = Tlang::new(
        "fn foo() { undefined_var + 1 }".to_string(),
        Runner::Interpreter,
    );
    tlang.analyze();
    assert!(
        !tlang.render_diagnostics().is_empty(),
        "expected a semantic diagnostic for undefined variable"
    );
    Ok(())
}

#[wasm_bindgen_test]
fn interpreter_eval_returns_type_errors() {
    let mut tlang = Tlang::new(
        "fn takes(x: bool) { x }\ntakes(\"1\");".to_string(),
        Runner::Interpreter,
    );
    let err = match tlang.eval() {
        Ok(_) => panic!("expected type mismatch error"),
        Err(err) => err,
    };
    assert!(
        format!("{err:?}").contains("argument type mismatch"),
        "expected type mismatch error, got: {err:?}"
    );
}

#[wasm_bindgen_test]
fn js_compile_returns_type_errors() {
    let mut tlang = Tlang::new(
        "fn takes(x: bool) { x }\ntakes(\"1\");".to_string(),
        Runner::JavaScript,
    );
    let err = tlang.compile_to_js().unwrap_err();
    assert!(
        format!("{err:?}").contains("argument type mismatch"),
        "expected type mismatch error, got: {err:?}"
    );
}

#[wasm_bindgen_test]
fn no_diagnostics_for_valid_code() -> Result<(), JsError> {
    // The function must be called; otherwise the analyzer reports "unused function".
    let mut tlang = Tlang::new(
        "fn add(a, b) { a + b }\nadd(1, 2);".to_string(),
        Runner::Interpreter,
    );
    tlang.analyze();
    assert!(
        tlang.render_diagnostics().is_empty(),
        "unexpected diagnostics: {}",
        tlang.render_diagnostics()
    );
    Ok(())
}

#[wasm_bindgen_test]
fn render_error_diagnostics_is_subset_of_render_diagnostics() -> Result<(), JsError> {
    let mut tlang = Tlang::new("fn foo() { bad_name + 1 }".to_string(), Runner::Interpreter);
    tlang.analyze();
    let all = tlang.render_diagnostics();
    let errors_only = tlang.render_error_diagnostics();
    assert!(!errors_only.is_empty());
    assert!(all.len() >= errors_only.len());
    Ok(())
}

// ---------------------------------------------------------------------------
// JS function interop via defineFunction
// ---------------------------------------------------------------------------

#[wasm_bindgen_test]
fn define_function_zero_args() -> Result<(), JsError> {
    ensure_init();
    let f = Function::new_no_args("return 42");
    Capture::reset();
    let mut tlang = Tlang::new("capture(get_answer());".to_string(), Runner::Interpreter);
    tlang.define_js_fn("get_answer", f);
    tlang.define_js_fn("capture", Capture::function());
    tlang.eval()?;
    assert_eq!(Capture::first_number(), Some(42.0));
    Ok(())
}

#[wasm_bindgen_test]
fn define_function_two_args() -> Result<(), JsError> {
    ensure_init();
    // Args arrive as BigInt (from i64); coerce to Number for regular JS arithmetic
    let f = Function::new_with_args("a, b", "return Number(a) + Number(b)");
    Capture::reset();
    let mut tlang = Tlang::new("capture(js_add(10, 32));".to_string(), Runner::Interpreter);
    tlang.define_js_fn("js_add", f);
    tlang.define_js_fn("capture", Capture::function());
    tlang.eval()?;
    assert_eq!(Capture::first_number(), Some(42.0));
    Ok(())
}

#[wasm_bindgen_test]
fn define_function_returns_boolean() -> Result<(), JsError> {
    ensure_init();
    // Arg arrives as BigInt; coerce to Number for the modulo operator
    let f = Function::new_with_args("x", "return Number(x) % 2 === 0");
    Capture::reset();
    let mut tlang = Tlang::new("capture(is_even(4));".to_string(), Runner::Interpreter);
    tlang.define_js_fn("is_even", f);
    tlang.define_js_fn("capture", Capture::function());
    tlang.eval()?;
    assert_eq!(Capture::first_bool(), Some(true));
    Ok(())
}

#[wasm_bindgen_test]
fn define_function_used_in_map() -> Result<(), JsError> {
    ensure_init();
    // Use a JS-defined function inside a tlang map call, then fold the results.
    // Arg arrives as BigInt; use BigInt arithmetic so the result stays numeric.
    let double = Function::new_with_args("x", "return Number(x) * 2");
    Capture::reset();
    let mut tlang = Tlang::new(
        "fn foldl([], acc, _) { acc }
         fn foldl([x, ...xs], acc, f) { rec foldl(xs, f(acc, x), f) }
         [1, 2, 3] |> map(js_double) |> foldl(0, fn(acc, x) { acc + x }) |> capture;"
            .to_string(),
        Runner::Interpreter,
    );
    tlang.define_js_fn("js_double", double);
    tlang.define_js_fn("capture", Capture::function());
    tlang.eval()?;
    // (1*2) + (2*2) + (3*2) = 12
    assert_eq!(Capture::first_number(), Some(12.0));
    Ok(())
}
