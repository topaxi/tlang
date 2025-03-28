use criterion::{Criterion, criterion_group, criterion_main};
use tlang_ast_lowering::LoweringContext;
use tlang_interpreter::Interpreter;
use tlang_memory::prelude::TlangValue;
use tlang_parser::Parser;

struct BenchInterpreter {
    lowering_context: LoweringContext,
    interpreter: Interpreter,
}

impl BenchInterpreter {
    fn new() -> Self {
        BenchInterpreter {
            lowering_context: LoweringContext::default(),
            interpreter: Interpreter::new(),
        }
    }

    fn eval_root(&mut self, src: &str) -> TlangValue {
        let mut parser = Parser::from_source(src);
        let ast = parser.parse().unwrap();
        let hir = self.lowering_context.lower_module_in_current_scope(&ast);
        self.interpreter.eval(&hir)
    }

    fn eval(&mut self, src: &str) -> TlangValue {
        let module_src = format!("{src};");
        self.eval_root(&module_src)
    }
}

fn interpreter(initial_source: &str) -> BenchInterpreter {
    let mut interpreter = BenchInterpreter::new();
    interpreter.eval_root(initial_source);
    interpreter
}

// Benchmark for different Fibonacci implementations
fn fibonacci_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("Fibonacci Implementation");

    let recursive_fib = r#"
        fn fib(n) {
            if n <= 1 {
                n
            } else {
                fib(n - 1) + fib(n - 2)
            }
        }
    "#;

    let pattern_fib = r#"
        fn fib(0) { 0 }
        fn fib(1) { 1 }
        fn fib(n) { fib(n - 1) + fib(n - 2) }
    "#;

    let tail_recursive_fib = r#"
        fn fib(n) { fib(n, 0, 1) }
        fn fib(0, a, _) { a }
        fn fib(n, a, b) { rec fib(n - 1, b, a + b) }
    "#;

    // Run benchmarks for n=10
    group.bench_function("recursive_fib(10)", |b| {
        let mut interp = interpreter(recursive_fib);
        b.iter(|| interp.eval("fib(10)"));
    });

    group.bench_function("pattern_fib(10)", |b| {
        let mut interp = interpreter(pattern_fib);
        b.iter(|| interp.eval("fib(10)"));
    });

    group.bench_function("tail_recursive_fib(10)", |b| {
        let mut interp = interpreter(tail_recursive_fib);
        b.iter(|| interp.eval("fib(10)"));
    });

    group.finish();
}

// Benchmark for closure operations
fn closure_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("Closures");

    let adder_code = r#"
        fn make_adder(x) {
            return fn(y) { x + y };
        }

        let add5 = make_adder(5);
    "#;

    group.bench_function("closure_creation_and_call", |b| {
        let mut interp = interpreter(adder_code);
        b.iter(|| { interp.eval("make_adder(5)(10)") });
    });

    group.bench_function("reused_closure", |b| {
        let mut interp = interpreter(adder_code);
        interp.eval_root("let add5 = make_adder(5);");
        b.iter(|| { interp.eval("add5(10)") });
    });

    group.bench_function("closure_create_and_store", |b| {
        b.iter(|| {
            let mut interp = BenchInterpreter::new();
            interp.eval_root(
                "fn make_adder(x) { return fn(y) { x + y }; } let adder = make_adder(5);",
            )
        });
    });

    group.finish();
}

// Benchmark for list operations (which involve a lot of objects and cloning)
fn list_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("List Operations");

    let list_ops_code = r#"
        fn reverse(list) { reverse(list, []) }
        fn reverse([], acc) { acc }
        fn reverse([x, ...xs], acc) { rec reverse(xs, [x, ...acc]) }

        fn map(list, f) {
            map_helper(list, f, [])
        }

        fn map_helper([], f, acc) { acc }
        fn map_helper([x, ...xs], f, acc) {
            rec map_helper(xs, f, [...acc, f(x)])
        }
    "#;

    group.bench_function("list_reverse", |b| {
        let mut interp = interpreter(list_ops_code);
        b.iter(|| {
            // Reverse a 20-item list
            interp.eval(
                "reverse([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20])",
            )
        });
    });

    group.bench_function("list_map", |b| {
        let mut interp = interpreter(list_ops_code);
        b.iter(|| {
            // Map a function over a 20-item list
            interp.eval("map([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20], fn(x) { x * 2 })")
        });
    });

    group.finish();
}

// Benchmark for struct and field access (which also uses cloning)
fn struct_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("Struct Operations");

    let struct_code = r#"
        struct Point {
            x: Int,
            y: Int,
        }

        fn Point::new(x, y) {
            Point { x: x, y: y }
        }

        fn distance(p1, p2) {
            let dx = p1.x - p2.x;
            let dy = p1.y - p2.y;
            math::sqrt(dx * dx + dy * dy)
        }
    "#;

    group.bench_function("struct_creation", |b| {
        let mut interp = interpreter(struct_code);
        b.iter(|| interp.eval("Point::new(10, 20)"));
    });

    group.bench_function("struct_field_access", |b| {
        let mut interp = interpreter(struct_code);
        interp.eval_root("let p = Point::new(10, 20);");
        b.iter(|| interp.eval("p.x + p.y"));
    });

    group.bench_function("struct_method", |b| {
        let mut interp = interpreter(struct_code);
        interp.eval_root("let p1 = Point::new(0, 0); let p2 = Point::new(3, 4);");
        b.iter(|| interp.eval("distance(p1, p2)"));
    });

    group.finish();
}

// Benchmark for function calls with small argument lists
fn small_arg_call_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("Small Argument Function Calls");

    let test_code = r#"
        fn call0() { 42 }
        fn call1(a) { a }
        fn call2(a, b) { a + b }
        fn call3(a, b, c) { a + b + c }
        fn call4(a, b, c, d) { a + b + c + d }
        fn call5(a, b, c, d, e) { a + b + c + d + e }
        fn call6(a, b, c, d, e, f) { a + b + c + d + e + f }
        fn nested_call(a, b) { call1(a) + call1(b) }
    "#;

    group.bench_function("call0_no_args", |b| {
        let mut interp = interpreter(test_code);
        b.iter(|| interp.eval("call0()"));
    });

    group.bench_function("call1_one_arg", |b| {
        let mut interp = interpreter(test_code);
        b.iter(|| interp.eval("call1(1)"));
    });

    group.bench_function("call2_two_args", |b| {
        let mut interp = interpreter(test_code);
        b.iter(|| interp.eval("call2(1, 2)"));
    });

    group.bench_function("call3_three_args", |b| {
        let mut interp = interpreter(test_code);
        b.iter(|| interp.eval("call3(1, 2, 3)"));
    });

    group.bench_function("call4_four_args", |b| {
        let mut interp = interpreter(test_code);
        b.iter(|| interp.eval("call4(1, 2, 3, 4)"));
    });

    group.bench_function("call5_five_args", |b| {
        let mut interp = interpreter(test_code);
        b.iter(|| interp.eval("call5(1, 2, 3, 4, 5)"));
    });

    group.bench_function("call6_six_args", |b| {
        let mut interp = interpreter(test_code);
        b.iter(|| interp.eval("call6(1, 2, 3, 4, 5, 6)"));
    });

    group.bench_function("nested_calls", |b| {
        let mut interp = interpreter(test_code);
        b.iter(|| interp.eval("nested_call(10, 20)"));
    });

    group.finish();
}

criterion_group!(
    benches,
    fibonacci_benchmark,
    closure_benchmark,
    list_benchmark,
    struct_benchmark,
    small_arg_call_benchmark
);
criterion_main!(benches);
