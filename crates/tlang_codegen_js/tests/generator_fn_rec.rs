use indoc::indoc;
use pretty_assertions::assert_eq;
use tlang_ast::symbols::SymbolType;
use tlang_codegen_js::generator::CodegenJS;
use tlang_parser::parser::Parser;
use tlang_semantics::SemanticAnalyzer;

macro_rules! compile {
    ($source:expr) => {{
        let mut parser = Parser::from_source($source);
        let mut ast = parser.parse();
        let mut semantic_analyzer = SemanticAnalyzer::default();
        semantic_analyzer.add_builtin_symbols(vec![
            ("log", SymbolType::Function),
            ("max", SymbolType::Function),
            ("min", SymbolType::Function),
        ]);
        semantic_analyzer.analyze(&mut ast);
        let mut codegen = CodegenJS::default();
        codegen.generate_code(&ast);
        codegen.get_output().to_string()
    }};
}

#[test]
fn test_simple_self_recursive_tail_call_converted_to_loop() {
    let output = compile!(indoc! {"
        fn factorial(n, acc) {
            if n == 0 {
                return acc;
            } else {
                return rec factorial(n - 1, n * acc);
            };
        }
    "});
    let expected_output = indoc! {"
        function factorial(n, acc) {
            while (true) {
                if (n === 0) {
                    return acc;
                } else {
                    let $tmp$a = n - 1;
                    let $tmp$b = n * acc;
                    n = $tmp$a;
                    acc = $tmp$b;
                }
            }
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_fn_expression_explicit_tail_recursive_call_converted_to_loop() {
    let output = compile!(indoc! {"
        fn factorial(n) {
            let factorial_rec = fn rec_helper(n, acc) {
                if n == 0 {
                    return acc;
                } else {
                    return rec rec_helper(n - 1, n * acc);
                };
            };

            factorial_rec(n, 1)
        }
    "});
    let expected_output = indoc! {"
        function factorial(n) {
            let factorial_rec = function rec_helper(n, acc) {
                while (true) {
                    if (n === 0) {
                        return acc;
                    } else {
                        let $tmp$a = n - 1;
                        let $tmp$b = n * acc;
                        n = $tmp$a;
                        acc = $tmp$b;
                    }
                }
            };
            return factorial_rec(n, 1);
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_explicit_tail_recursive_call_converted_to_loop_factorial_simple() {
    let output = compile!(indoc! {"
        fn factorial(0, acc) { acc }
        fn factorial(n, acc) { return rec factorial(n - 1, n * acc); }
    "});
    let expected_output = indoc! {"
        function factorial(...args) {
            while (true) {
                if (args[0] === 0) {
                    let acc = args[1];
                    return acc;
                } else {
                    let n = args[0];
                    let acc = args[1];
                    let $tmp$a = n - 1;
                    let $tmp$b = n * acc;
                    args[0] = $tmp$a;
                    args[1] = $tmp$b;
                }
            }
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_explicit_tail_recursive_call_converted_to_loop_factorial_convenient() {
    let output = compile!(indoc! {"
        fn factorial(n) { factorial(n, 1) }
        fn factorial(0, acc) { acc }
        fn factorial(n, acc) { rec factorial(n - 1, n * acc) }
    "});
    let expected_output = indoc! {"
        function factorial(...args) {
            while (true) {
                if (args.length === 1) {
                    let n = args[0];
                    return factorial(n, 1);
                } else if (args.length === 2 && args[0] === 0) {
                    let acc = args[1];
                    return acc;
                } else if (args.length === 2) {
                    let n = args[0];
                    let acc = args[1];
                    let $tmp$a = n - 1;
                    let $tmp$b = n * acc;
                    args[0] = $tmp$a;
                    args[1] = $tmp$b;
                }
            }
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_tail_recursive_fibonacci() {
    let output = compile!(indoc! {"
        fn fibonacci(n) { fibonacci(n, 0, 1) }
        fn fibonacci(0, a, b) { a }
        fn fibonacci(1, a, b) { b }
        fn fibonacci(n, a, b) { rec fibonacci(n - 1, b, a + b) }
    "});
    let expected_output = indoc! {"
        function fibonacci(...args) {
            while (true) {
                if (args.length === 1) {
                    let n = args[0];
                    return fibonacci(n, 0, 1);
                } else if (args.length === 3 && args[0] === 0) {
                    let a = args[1];
                    let b = args[2];
                    return a;
                } else if (args.length === 3 && args[0] === 1) {
                    let a = args[1];
                    let b = args[2];
                    return b;
                } else if (args.length === 3) {
                    let n = args[0];
                    let a = args[1];
                    let b = args[2];
                    let $tmp$a = n - 1;
                    let $tmp$b = b;
                    let $tmp$c = a + b;
                    args[0] = $tmp$a;
                    args[1] = $tmp$b;
                    args[2] = $tmp$c;
                }
            }
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_foldl_impl() {
    let output = compile!(indoc! {"
        fn foldl([], acc, _) { acc }
        fn foldl([x, ...xs], acc, f) { rec foldl(xs, f(acc, x), f) }
    "});
    let expected_output = indoc! {"
        function foldl(...args) {
            while (true) {
                if (args[0].length === 0) {
                    let acc = args[1];
                    return acc;
                } else if (args[0].length >= 1) {
                    let acc = args[1];
                    let f = args[2];
                    let x = args[0][0];
                    let xs = args[0].slice(1);
                    let $tmp$a = xs;
                    let $tmp$b = f(acc, x);
                    let $tmp$c = f;
                    args[0] = $tmp$a;
                    args[1] = $tmp$b;
                    args[2] = $tmp$c;
                }
            }
        }
    "};
    assert_eq!(output, expected_output);
}
