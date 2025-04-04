use criterion::{Criterion, criterion_group, criterion_main};
use tlang_ast_lowering::LoweringContext;
use tlang_hir_opt::{
    constant_folding::ConstantFolder, constant_propagation::ConstantPropagator,
    dead_code_elimination::DeadCodeEliminator, hir_opt::HirOptimizer,
};
use tlang_interpreter::Interpreter;
use tlang_memory::prelude::TlangValue;
use tlang_parser::Parser;

struct BenchInterpreter {
    lowering_context: LoweringContext,
    interpreter: Interpreter,
    optimizer: HirOptimizer,
}

impl BenchInterpreter {
    fn new() -> Self {
        let mut optimizer = HirOptimizer::new();
        optimizer.add_pass(Box::new(ConstantFolder::new()));
        optimizer.add_pass(Box::new(ConstantPropagator::new()));
        optimizer.add_pass(Box::new(DeadCodeEliminator::new().with_preserve_root(true)));

        BenchInterpreter {
            lowering_context: LoweringContext::default(),
            interpreter: Interpreter::new(),
            optimizer,
        }
    }

    fn eval_root(&mut self, src: &str) -> TlangValue {
        let mut parser = Parser::from_source(src);
        let ast = parser.parse().unwrap();
        let mut hir = self.lowering_context.lower_module_in_current_scope(&ast);
        self.optimizer.optimize_module(&mut hir);
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
        b.iter(|| interp.eval("make_adder(5)(10)"));
    });

    group.bench_function("reused_closure", |b| {
        let mut interp = interpreter(adder_code);
        interp.eval_root("let add5 = make_adder(5);");
        b.iter(|| interp.eval("add5(10)"));
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

// Benchmark for binary search algorithm
fn binary_search_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("Binary Search");

    let binary_search_code = r#"
        // binary_search(a[], a) -> int
        fn binary_search(list, target) { binary_search(list, target, 0, len(list) - 1) }
        // binary_search(a[], a, int, int) -> int
        fn binary_search(_, _, low, high) if low > high; { -1 }
        // binary_search(a[], a, int, int) -> int
        fn binary_search(list, target, low, high) {
            let mid = math::floor((low + high) / 2);
            let midValue = list[mid];

            if midValue == target; {
                mid
            } else if midValue < target; {
                rec binary_search(list, target, mid + 1, high)
            } else {
                rec binary_search(list, target, low, mid - 1)
            }
        }
    "#;

    // Create sorted lists of different sizes
    let small_list = "[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]";
    let medium_list = (1..=100)
        .collect::<Vec<_>>()
        .iter()
        .map(|i| i.to_string())
        .collect::<Vec<_>>()
        .join(", ");
    let medium_list = format!("[{medium_list}]");
    let large_list = (1..=1000)
        .collect::<Vec<_>>()
        .iter()
        .map(|i| i.to_string())
        .collect::<Vec<_>>()
        .join(", ");
    let large_list = format!("[{large_list}]");

    // Binary search in small list (10 elements)
    group.bench_function("small_list_best_case", |b| {
        let mut interp = interpreter(binary_search_code);
        // Search for middle element - best case
        b.iter(|| interp.eval(&format!("binary_search({small_list}, 5)")));
    });

    group.bench_function("small_list_worst_case", |b| {
        let mut interp = interpreter(binary_search_code);
        // Search for non-existent element - worst case
        b.iter(|| interp.eval(&format!("binary_search({small_list}, 11)")));
    });

    // Binary search in medium list (100 elements)
    group.bench_function("medium_list_best_case", |b| {
        let mut interp = interpreter(binary_search_code);
        // Search for middle element - best case
        b.iter(|| interp.eval(&format!("binary_search({medium_list}, 50)")));
    });

    group.bench_function("medium_list_worst_case", |b| {
        let mut interp = interpreter(binary_search_code);
        // Search for non-existent element - worst case
        b.iter(|| interp.eval(&format!("binary_search({medium_list}, 101)")));
    });

    // Binary search in large list (1000 elements)
    group.bench_function("large_list_search", |b| {
        let mut interp = interpreter(binary_search_code);
        // Search for an element deep in the list
        b.iter(|| interp.eval(&format!("binary_search({large_list}, 750)")));
    });

    group.finish();
}

// Now let's add a more complex real-world benchmark: quicksort algorithm
fn quicksort_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("Quicksort");

    let quicksort_code = r#"
        // filter(a[], fn(a) -> bool) -> a[]
        fn filter([], _) { [] }
        fn filter([x, ...xs], f) if f(x) { [x, ...filter(xs, f)] }
        fn filter([_, ...xs], f) { rec filter(xs, f) }

        // deterministic quicksort for benchmarking (uses middle element as pivot)
        fn quicksort([]) { [] }
        fn quicksort([x]) { [x] }
        fn quicksort(list) {
            let pivotIndex = math::floor(len(list) / 2);
            let pivot = list[pivotIndex];
            let list = [...list.slice(0, pivotIndex), ...list.slice(pivotIndex+1)];
            let smaller = filter(list, fn(y) { y <= pivot });
            let greater = filter(list, fn(y) { y > pivot });
            [...quicksort(smaller), pivot, ...quicksort(greater)]
        }
    "#;

    // Create lists to sort
    let sorted_small = "[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]";
    let reverse_small = "[10, 9, 8, 7, 6, 5, 4, 3, 2, 1]";
    let random_small = "[5, 8, 1, 3, 9, 4, 7, 2, 6, 10]";

    // Medium-sized list with predetermined "random" values for benchmarking
    let random_medium = "[42, 17, 89, 36, 91, 53, 28, 76, 15, 62, 39, 84, 27, 63, 71, 
                         48, 19, 95, 33, 60, 8, 74, 51, 22, 68, 44, 56, 81, 12, 30]";

    // Benchmark quicksort on different inputs
    group.bench_function("sorted_list", |b| {
        let mut interp = interpreter(quicksort_code);
        b.iter(|| interp.eval(&format!("quicksort({sorted_small})")));
    });

    group.bench_function("reverse_list", |b| {
        let mut interp = interpreter(quicksort_code);
        b.iter(|| interp.eval(&format!("quicksort({reverse_small})")));
    });

    group.bench_function("random_small_list", |b| {
        let mut interp = interpreter(quicksort_code);
        b.iter(|| interp.eval(&format!("quicksort({random_small})")));
    });

    group.bench_function("random_medium_list", |b| {
        let mut interp = interpreter(quicksort_code);
        b.iter(|| interp.eval(&format!("quicksort({random_medium})")));
    });

    group.finish();
}

// Benchmark for tree traversal operations
fn tree_traversal_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("Tree Traversal");

    let tree_code = r#"
        // Define a binary tree structure using enums
        enum Tree {
            Empty,
            Node { value, left, right },
        }

        fn Tree::leaf(value) {
            Tree::Node {
                value: value,
                left: Option::None,
                right: Option::None,
            }
        }

        fn Tree::node(value, left, right) {
            Tree::Node {
                value: value,
                left: left,
                right: right,
            }
        }

        // In-order traversal of a binary tree
        fn inorder(Option::None) { [] }
        fn inorder(Tree::Node { value, left, right }) {
            [...inorder(left), value, ...inorder(right)]
        }

        // Pre-order traversal of a binary tree
        fn preorder(Option::None) { [] }
        fn preorder(Tree::Node { value, left, right }) {
            [value, ...preorder(left), ...preorder(right)]
        }

        // Post-order traversal of a binary tree
        fn postorder(Option::None) { [] }
        fn postorder(Tree::Node { value, left, right }) {
            [...postorder(left), ...postorder(right), value]
        }

        // Calculate the maximum depth of a tree
        fn max_depth(Option::None) { 0 }
        fn max_depth(Tree::Node { value, left, right }) {
            1 + math::max(max_depth(left), max_depth(right))
        }

        // Count the number of nodes in a tree
        fn count_nodes(Option::None) { 0 }
        fn count_nodes(Tree::Node { value, left, right }) {
            1 + count_nodes(left) + count_nodes(right)
        }

        // Create a balanced tree for testing
        let leaf1 = Tree::leaf(1);
        let leaf2 = Tree::leaf(3);
        let leaf3 = Tree::leaf(5);
        let leaf4 = Tree::leaf(7);
        
        let node1 = Tree::node(2, leaf1, leaf2);
        let node2 = Tree::node(6, leaf3, leaf4);
        
        let root = Tree::node(4, node1, node2);
    "#;

    group.bench_function("create_balanced_tree", |b| {
        b.iter(|| {
            let mut interp = BenchInterpreter::new();
            interp.eval_root(tree_code)
        });
    });

    group.bench_function("inorder_traversal", |b| {
        let mut interp = interpreter(tree_code);
        b.iter(|| interp.eval("inorder(root)"));
    });

    group.bench_function("preorder_traversal", |b| {
        let mut interp = interpreter(tree_code);
        b.iter(|| interp.eval("preorder(root)"));
    });

    group.bench_function("postorder_traversal", |b| {
        let mut interp = interpreter(tree_code);
        b.iter(|| interp.eval("postorder(root)"));
    });

    group.bench_function("max_depth_calculation", |b| {
        let mut interp = interpreter(tree_code);
        b.iter(|| interp.eval("max_depth(root)"));
    });

    group.bench_function("count_nodes", |b| {
        let mut interp = interpreter(tree_code);
        b.iter(|| interp.eval("count_nodes(root)"));
    });

    // Create a deeply nested tree to test performance with deeper recursion
    let deep_tree_setup = r#"
        // Create a deeper tree with left-skewed structure
        let deep_leaf = Tree::leaf(1);
        let deep_node1 = Tree::node(2, deep_leaf, Option::None);
        let deep_node2 = Tree::node(3, deep_node1, Option::None);
        let deep_node3 = Tree::node(4, deep_node2, Option::None);
        let deep_node4 = Tree::node(5, deep_node3, Option::None);
        let deep_node5 = Tree::node(6, deep_node4, Option::None);
        let deep_node6 = Tree::node(7, deep_node5, Option::None);
        let deep_root = Tree::node(8, deep_node6, Option::None);
    "#;

    group.bench_function("deep_tree_traversal", |b| {
        let mut interp = interpreter(tree_code);
        interp.eval_root(deep_tree_setup);
        b.iter(|| interp.eval("inorder(deep_root)"));
    });

    group.finish();
}

// Benchmark for constant folding and propagation
fn constant_optimization_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("Constant Optimization");

    // Test with complex constant expressions
    let complex_constants = r#"
        let a = 1 + 2 * 3 - 4 / 2;
        let b = (a + 5) * 2;
        let c = b - a * 3;
        let d = c + 10;
        let e = d * 2 - b;
        let f = e / 5 + a;
        let g = f * 3 - c;
        let h = g + d - e;
        let i = h * 2 / f;
        let j = i + g - h;
        j
    "#;

    // Test with chained constant operations
    let chained_constants = r#"
        let x = 1;
        let y = x + 2;
        let z = y * 3;
        let a = z - 4;
        let b = a / 5;
        let c = b + 6;
        let d = c * 7;
        let e = d - 8;
        let f = e / 9;
        f
    "#;

    // Test with dead code elimination
    let dead_code = r#"
        let unused1 = 1 + 2;
        let unused2 = 3 * 4;
        let unused3 = 5 - 6;
        let result = 42;
        let unused4 = result * 2;
        result
    "#;

    group.bench_function("complex_constants", |b| {
        let mut interp = BenchInterpreter::new();
        b.iter(|| interp.eval(complex_constants));
    });

    group.bench_function("chained_constants", |b| {
        let mut interp = BenchInterpreter::new();
        b.iter(|| interp.eval(chained_constants));
    });

    group.bench_function("dead_code", |b| {
        let mut interp = BenchInterpreter::new();
        b.iter(|| interp.eval(dead_code));
    });

    group.finish();
}

// Benchmark for mixed constant and variable operations
fn mixed_operations_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("Mixed Operations");

    let mixed_code = r#"
        fn calculate(x) {
            let a = x + 1;
            let b = a * 2;
            let c = b - 3;
            let d = c / 4;
            let e = d + 5;
            let f = e * 6;
            let g = f - 7;
            let h = g / 8;
            h
        }
    "#;

    group.bench_function("mixed_operations", |b| {
        let mut interp = interpreter(mixed_code);
        b.iter(|| interp.eval("calculate(10)"));
    });

    group.finish();
}

criterion_group!(
    benches,
    fibonacci_benchmark,
    closure_benchmark,
    list_benchmark,
    struct_benchmark,
    small_arg_call_benchmark,
    binary_search_benchmark,
    quicksort_benchmark,
    tree_traversal_benchmark,
    constant_optimization_benchmark,
    mixed_operations_benchmark,
);
criterion_main!(benches);
