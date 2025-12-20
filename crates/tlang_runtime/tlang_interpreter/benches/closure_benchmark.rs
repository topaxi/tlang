//! Benchmarks for closure performance in the tlang interpreter.
//!
//! These benchmarks measure the performance of various closure patterns
//! to help identify any performance regressions when refactoring closure
//! capture mechanisms for GC compatibility.
//!
//! Run with: `cargo bench --package tlang_interpreter`
//!
//! Note: These benchmarks include parse + compile overhead in addition to
//! execution time. For more accurate execution-only benchmarks, consider
//! pre-parsing the expressions.

use criterion::{Criterion, black_box, criterion_group, criterion_main};

mod helpers {
    use tlang_ast_lowering::lower_to_hir;
    use tlang_hir::hir;
    use tlang_hir_opt::HirOptimizer;
    use tlang_interpreter::Interpreter;
    use tlang_parser::Parser;
    use tlang_semantics::SemanticAnalyzer;

    /// Parse and lower source code to HIR module.
    pub fn parse_to_hir(src: &str) -> hir::Module {
        let ast = Parser::from_source(src).parse().expect("Parse error");

        let mut analyzer = SemanticAnalyzer::default();
        analyzer.add_builtin_symbols(&Interpreter::builtin_symbols());
        analyzer.analyze(&ast).expect("Semantic analysis error");

        let (mut module, meta) = lower_to_hir(
            &ast,
            analyzer.symbol_id_allocator(),
            analyzer.root_symbol_table(),
            analyzer.symbol_tables().clone(),
        );

        let mut optimizer = HirOptimizer::default();
        optimizer.optimize_hir(&mut module, meta.into());

        module
    }

    /// Create an interpreter with preloaded source code.
    pub fn setup_interpreter(src: &str) -> Interpreter {
        let mut interpreter = Interpreter::new();
        let hir = parse_to_hir(src);
        interpreter.eval(&hir);
        interpreter
    }

    /// Evaluate a full program (including parsing).
    pub fn eval_program(interpreter: &mut Interpreter, src: &str) -> tlang_memory::TlangValue {
        let hir = parse_to_hir(src);
        interpreter.eval(&hir)
    }
}

use helpers::*;

/// Benchmark: Simple closure creation and invocation.
/// Tests the overhead of creating a closure that captures one variable.
fn bench_simple_closure(c: &mut Criterion) {
    let mut interpreter = setup_interpreter(
        r#"
        fn make_adder(a) {
            return fn adder(b) { a + b };
        }
        "#,
    );

    c.bench_function("simple_closure_create_and_call", |b| {
        b.iter(|| eval_program(&mut interpreter, black_box("make_adder(5)(10)")));
    });
}

/// Benchmark: Counter closure with mutable state.
/// Tests closures that mutate captured variables across multiple invocations.
fn bench_counter_closure(c: &mut Criterion) {
    let mut interpreter = setup_interpreter(
        r#"
        fn make_counter(init) {
            let count = init;
            return fn counter() {
                count = count + 1;
                count
            };
        }
        let counter = make_counter(0);
        "#,
    );

    c.bench_function("counter_closure_call", |b| {
        b.iter(|| eval_program(&mut interpreter, black_box("counter()")));
    });
}

/// Benchmark: Nested closures.
/// Tests closures that create other closures, capturing from multiple scopes.
fn bench_nested_closures(c: &mut Criterion) {
    let mut interpreter = setup_interpreter(
        r#"
        fn outer(x) {
            return fn middle(y) {
                return fn inner(z) {
                    x + y + z
                };
            };
        }
        "#,
    );

    c.bench_function("nested_closure_create_and_call", |b| {
        b.iter(|| eval_program(&mut interpreter, black_box("outer(1)(2)(3)")));
    });
}

/// Benchmark: Closure passed to higher-order function.
/// Tests the common pattern of passing closures to map/filter/fold.
fn bench_closure_in_map(c: &mut Criterion) {
    let mut interpreter = setup_interpreter(
        r#"
        fn map([], _) { [] }
        fn map([x, ...xs], f) { [f(x), ...map(xs, f)] }
        
        let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
        let multiplier = 2;
        "#,
    );

    c.bench_function("closure_in_map", |b| {
        b.iter(|| {
            eval_program(
                &mut interpreter,
                black_box("map(numbers, fn(x) { x * multiplier })"),
            )
        });
    });
}

/// Benchmark: Closure with many captured variables.
/// Tests the overhead of capturing many variables.
fn bench_many_captures(c: &mut Criterion) {
    let mut interpreter = setup_interpreter(
        r#"
        fn many_captures() {
            let a = 1;
            let b = 2;
            let c = 3;
            let d = 4;
            let e = 5;
            return fn() { a + b + c + d + e };
        }
        let f = many_captures();
        "#,
    );

    c.bench_function("closure_many_captures", |b| {
        b.iter(|| eval_program(&mut interpreter, black_box("f()")));
    });
}

/// Benchmark: Recursive function using closure for accumulator.
/// Tests closures in recursive patterns.
fn bench_recursive_with_closure(c: &mut Criterion) {
    let mut interpreter = setup_interpreter(
        r#"
        fn foldl([], acc, _) { acc }
        fn foldl([x, ...xs], acc, f) { foldl(xs, f(acc, x), f) }
        
        let numbers = [1, 2, 3, 4, 5];
        "#,
    );

    c.bench_function("foldl_with_closure", |b| {
        b.iter(|| {
            eval_program(
                &mut interpreter,
                black_box("foldl(numbers, 0, fn(acc, x) { acc + x })"),
            )
        });
    });
}

criterion_group!(
    benches,
    bench_simple_closure,
    bench_counter_closure,
    bench_nested_closures,
    bench_closure_in_map,
    bench_many_captures,
    bench_recursive_with_closure,
);

criterion_main!(benches);
