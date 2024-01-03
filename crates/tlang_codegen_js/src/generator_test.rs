use crate::generator::CodegenJS;
use indoc::indoc;
use pretty_assertions::assert_eq;
use tlang_ast::symbols::SymbolType;
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
fn test_codegen_variable_declaration() {
    let output = compile!("let x = 42;");
    let expected_output = "let x = 42;\n";
    assert_eq!(output, expected_output);
}

#[test]
fn test_codegen_binary_expression() {
    let output = compile!("let x = 42 + 1;");
    let expected_output = "let x = 42 + 1;\n";
    assert_eq!(output, expected_output);
}

#[test]
fn test_variable_shadowing() {
    let output = compile!("let x = 42; let x = 43; x;");
    let expected_output = "let x = 42;\nlet x$a = 43;\nx$a;\n";
    assert_eq!(output, expected_output);
}

#[test]
fn test_codegen_function_declaration() {
    let output = compile!("fn main() {}");
    let expected_output = "function main() {\n}\n";
    assert_eq!(output, expected_output);
}

#[test]
fn test_codegen_function_call() {
    let output = compile!(indoc! {"
        fn main() {}
        main();
    "});
    let expected_output = indoc! {"
        function main() {
        }
        main();
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_codegen_function_expression() {
    let output = compile!("fn main() { let foo = fn() { 1 + 2 }; }");
    let expected_output = indoc! {"
        function main() {
            let foo = function() {
                return 1 + 2;
            };
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_codegen_functions_with_explicit_return_statements() {
    let output = compile!("fn main() { return 42; }");
    let expected_output = indoc! {"
        function main() {
            return 42;
        }
    "};
    assert_eq!(output, expected_output);

    let output = compile!("fn main() { return; }");
    let expected_output = indoc! {"
        function main() {
            return;
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_codegen_parenthesis_expression() {
    let output = compile!("let x = (42 + 1) * 2;");
    let expected_output = "let x = (42 + 1) * 2;\n";
    assert_eq!(output, expected_output);
}

#[test]
fn test_codegen_operator_precedence() {
    let output = compile!("let result = 1 + 2 * 3;");
    let expected_output = "let result = 1 + 2 * 3;\n";
    assert_eq!(output, expected_output);
}

#[test]
fn test_block_expression() {
    let output = compile!("let one = { 1 };");
    let expected_output = indoc! {"
        let $tmp$a;{
            $tmp$a = 1;
        };
        let one = $tmp$a;
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_if_else() {
    let output = compile!("if true { 1; } else { 2; }");
    let expected_output = indoc! {"
        if (true) {
            1;
        } else {
            2;
        }
    "};
    assert_eq!(output, expected_output);
}

#[ignore = "implement if/else in expression position first"]
#[test]
fn test_if_else_as_expression() {
    let output = compile!("fn main() { let result = if true { 1 } else { 2 }; }");
    let expected_output = indoc! {"
        function main() {
            let $tmp$a;
            if (true) {
                $tmp$a = 1;
            } else {
                $tmp$a = 2;
            }
            let result = $tmp$a;
        }
    "};
    assert_eq!(output, expected_output);
}

#[ignore = "implement if/else in expression position first"]
#[test]
fn test_if_else_as_expression_nested() {
    let output =
        compile!("fn main() { let result = if true { if true { 1 } else { 2 } } else { 3 }; }");
    let expected_output = indoc! {"
        function main() {
            let $tmp$a;{
                if (true) {
                    let $tmp$b;{
                        if (true) {
                            $tmp$b = 1;
                        } else {
                            $tmp$b = 2;
                        }
                    };
                    $tmp$a = $tmp$b;
                } else {
                    $tmp$a = 3;
                }
            };
            let result = $tmp$a;
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_recursive_function_definition() {
    let output = compile!(indoc! {"
        fn factorial(0) { 1 }
        fn factorial(n) { n * factorial(n - 1) }
    "});
    let expected_output = indoc! {"
        function factorial(...args) {
            if (args[0] === 0) {
                return 1;
            } else {
                let n = args[0];
                return n * factorial(n - 1);
            }
        }
    "};
    assert_eq!(output, expected_output);

    let output = compile!(indoc! {"
        fn fibonacci(0) { 0 }
        fn fibonacci(1) { 1 }
        fn fibonacci(n) { fibonacci(n - 1) + fibonacci(n - 2) }
    "});
    let expected_output = indoc! {"
        function fibonacci(...args) {
            if (args[0] === 0) {
                return 0;
            } else if (args[0] === 1) {
                return 1;
            } else {
                let n = args[0];
                return fibonacci(n - 1) + fibonacci(n - 2);
            }
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_recursive_function_definition_multiple_with_multiple_args() {
    let output = compile!(indoc! {"
        fn gcd(0, n) { n }
        fn gcd(m, 0) { m }
        fn gcd(m, n) { gcd(n, m % n) }
    "});
    let expected_output = indoc! {"
        function gcd(...args) {
            if (args[0] === 0) {
                let n = args[1];
                return n;
            } else if (args[1] === 0) {
                let m = args[0];
                return m;
            } else {
                let m = args[0];
                let n = args[1];
                return gcd(n, m % n);
            }
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_tail_recursive_factorial_nested() {
    let output = compile!(indoc! {"
        fn factorial(n) {
            fn factorial_rec(0, acc) { acc }
            fn factorial_rec(n, acc) { factorial_rec(n - 1, n * acc) }

            factorial_rec(n, 1)
        }
    "});
    let expected_output = indoc! {"
        function factorial(n) {
            function factorial_rec(...args) {
                if (args[0] === 0) {
                    let acc = args[1];
                    return acc;
                } else {
                    let n = args[0];
                    let acc = args[1];
                    return factorial_rec(n - 1, n * acc);
                }
            }
            return factorial_rec(n, 1);
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_tail_recursive_factorial_idiomatic() {
    let output = compile!(indoc! {"
        fn factorial(n) { factorial(n, 1) }
        fn factorial(0, acc) { acc }
        fn factorial(n, acc) { factorial(n - 1, n * acc) }
    "});
    let expected_output = indoc! {"
        function factorial(...args) {
            if (args.length === 1) {
                let n = args[0];
                return factorial(n, 1);
            } else if (args.length === 2 && args[0] === 0) {
                let acc = args[1];
                return acc;
            } else if (args.length === 2) {
                let n = args[0];
                let acc = args[1];
                return factorial(n - 1, n * acc);
            }
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_pipeline_operator() {
    let output = compile!("fn main() { 1 |> log; }");
    let expected_output = indoc! {"
        function main() {
            console.log(1);
        }
    "};
    assert_eq!(output, expected_output);

    let output = compile!("fn main() { 1 |> min |> max; }");
    let expected_output = indoc! {"
        function main() {
            Math.max(Math.min(1));
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_pipeline_operator_with_call_parenthesis() {
    let output = compile!("fn main() { 1 |> max(); }");
    let expected_output = indoc! {"
        function main() {
            Math.max(1);
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_pipeline_operator_with_call_parenthesis_and_arguments() {
    let output = compile!("fn main() { 1 |> foo(2); }");
    let expected_output = indoc! {"
        function main() {
            foo(1, 2);
        }
    "};
    assert_eq!(output, expected_output);

    let output = compile!("fn main() { 1 |> foo(2, 3); }");
    let expected_output = indoc! {"
        function main() {
            foo(1, 2, 3);
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_pipeline_operator_to_function_call_with_wildcards() {
    let output = compile!("fn main() { 1 |> foo(2, _); }");
    let expected_output = indoc! {"
        function main() {
            foo(2, 1);
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_list_literal() {
    let output = compile!("fn main() { [1, 2, 3] }");
    let expected_output = indoc! {"
        function main() {
            return [1, 2, 3];
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_recursive_sum() {
    let output = compile!(indoc! {"
        fn sum([]) { 0 }
        fn sum([x, ...xs]) { x + sum(xs) }
    "});
    let expected_output = indoc! {"
        function sum(...args) {
            if (args[0].length === 0) {
                return 0;
            } else if (args[0].length >= 1) {
                let x = args[0][0];
                let xs = args[0].slice(1);
                return x + sum(xs);
            }
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_recursive_map() {
    let output = compile!(indoc! {"
        fn map([], f) { [] }
        fn map([x, ...xs], f) { [f(x), ...map(xs, f)] }
    "});
    let expected_output = indoc! {"
        function map(...args) {
            if (args[0].length === 0) {
                let f = args[1];
                return [];
            } else if (args[0].length >= 1) {
                let f = args[1];
                let x = args[0][0];
                let xs = args[0].slice(1);
                return [f(x), ...map(xs, f)];
            }
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_partial_application() {
    let output = compile!("let add1 = add(_, 1); }");
    let expected_output = indoc! {"
        let add1 = function(...args) {
            return add(args[0], 1);
        };
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_partial_application_with_multiple_arguments() {
    let output = compile!("let add1 = add(_, 1, _); }");
    let expected_output = indoc! {"
        let add1 = function(...args) {
            return add(args[0], 1, args[1]);
        };
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_single_line_comments() {
    let output = compile!("// this is a comment");
    let expected_output = "// this is a comment\n";
    assert_eq!(output, expected_output);
}

#[test]
fn test_multi_line_comments() {
    let output = compile!("/* this is a comment */");
    let expected_output = "/* this is a comment */\n";
    assert_eq!(output, expected_output);

    let output = compile!("/* this is a comment\non multiple lines */");
    let expected_output = "/* this is a comment\non multiple lines */\n";
    assert_eq!(output, expected_output);
}

#[test]
fn test_string_literals() {
    let output = compile!("let x = \"hello\";");
    let expected_output = "let x = \"hello\";\n";
    assert_eq!(output, expected_output);
}

#[test]
fn test_char_literals() {
    let output = compile!("let x = 'a';");
    let expected_output = "let x = \"a\";\n";
    assert_eq!(output, expected_output);
}

#[test]
fn test_enums() {
    let output = compile!(indoc! {"
        enum Option {
            Some(x),
            None,
        }

        fn main() {
            let x = Option::Some(42);
        }
    "});
    let expected_output = indoc! {"
        const Option = {
            Some(x) {
                return {
                    tag: \"Some\",
                    \"0\": x,
                };
            },
            None: { tag: \"None\" },
        };
        function main() {
            let x = Option.Some(42);
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_enums_with_fields() {
    let output = compile!(indoc! {"
        enum Node {
            BinaryOperation {
                operator,
                left,
                right,
            },
        }

        fn main() {
            let x = Node::BinaryOperation {
                operator: \"+\",
                left: 1,
                right: 2,
            };
        }
    "});
    let expected_output = indoc! {"
        const Node = {
            BinaryOperation({ operator, left, right }) {
                return {
                    tag: \"BinaryOperation\",
                    operator,
                    left,
                    right,
                };
            },
        };
        function main() {
            let x = Node.BinaryOperation({
                operator: \"+\",
                left: 1,
                right: 2,
            });
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_enums_tree_implementation() {
    let output = compile!(indoc! {"
        enum Tree {
            Leaf(x),
            Node {
                left,
                right,
            },
        }

        fn main() {
            let x = Tree::Node {
                left: Tree::Leaf(1),
                right: Tree::Leaf(2),
            };
        }
    "});
    let expected_output = indoc! {"
        const Tree = {
            Leaf(x) {
                return {
                    tag: \"Leaf\",
                    \"0\": x,
                };
            },
            Node({ left, right }) {
                return {
                    tag: \"Node\",
                    left,
                    right,
                };
            },
        };
        function main() {
            let x = Tree.Node({
                left: Tree.Leaf(1),
                right: Tree.Leaf(2),
            });
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_maximum_depth_tree() {
    let output = compile!(indoc! {"
        enum Tree {
            Leaf(x),
            Node {
                left,
                right,
            },
        }

        fn maximum_depth(Tree::Leaf(_)) { 1 }
        fn maximum_depth(Tree::Node { left, right }) { 1 + max(maximum_depth(left), maximum_depth(right)) }

        fn main() {
            let x = Tree::Node {
                left: Tree::Leaf(1),
                right: Tree::Node {
                    left: Tree::Leaf(2),
                    right: Tree::Leaf(3),
                },
            };
        }
    "});
    let expected_output = indoc! {"
        const Tree = {
            Leaf(x) {
                return {
                    tag: \"Leaf\",
                    \"0\": x,
                };
            },
            Node({ left, right }) {
                return {
                    tag: \"Node\",
                    left,
                    right,
                };
            },
        };
        function maximum_depth(...args) {
            if (args[0].tag === \"Leaf\") {
                return 1;
            } else if (args[0].tag === \"Node\") {
                let left = args[0].left;
                let right = args[0].right;
                return 1 + Math.max(maximum_depth(left), maximum_depth(right));
            }
        }
        function main() {
            let x = Tree.Node({
                left: Tree.Leaf(1),
                right: Tree.Node({
                    left: Tree.Leaf(2),
                    right: Tree.Leaf(3),
                }),
            });
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_maximum_depth_tree_positional() {
    let output = compile!(indoc! {"
        enum Tree {
            Leaf(x),
            Node(left, right),
        }

        fn maximum_depth(Tree::Leaf(_)) { 1 }
        fn maximum_depth(Tree::Node(left, right)) { 1 + max(maximum_depth(left), maximum_depth(right)) }
    "});
    let expected_output = indoc! {"
        const Tree = {
            Leaf(x) {
                return {
                    tag: \"Leaf\",
                    \"0\": x,
                };
            },
            Node(left, right) {
                return {
                    tag: \"Node\",
                    \"0\": left,
                    \"1\": right,
                };
            },
        };
        function maximum_depth(...args) {
            if (args[0].tag === \"Leaf\") {
                return 1;
            } else if (args[0].tag === \"Node\") {
                let left = args[0][0];
                let right = args[0][1];
                return 1 + Math.max(maximum_depth(left), maximum_depth(right));
            }
        }
    "};
    assert_eq!(output, expected_output);
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
fn test_field_access_expressions() {
    let output = compile!(indoc! {"
        fn main() {
            let x = foo.bar;
        }
    "});
    let expected_output = indoc! {"
        function main() {
            let x = foo.bar;
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_index_access_expressions() {
    let output = compile!(indoc! {"
        fn main() {
            let x = foo[0];
        }
    "});
    let expected_output = indoc! {"
        function main() {
            let x = foo[0];
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

#[test]
#[ignore = "reimplement enums based on the draft below"]
fn test_declare_methods_on_option_enum() {
    // Static functions.
    // fn Option::is_option(Option::Some(_)) { true }
    // Methods on enum.
    // fn Option.is_some(Option::Some(_)) { true }
    let output = compile!(indoc! {"
        enum Option {
            Some(x),
            None,
        }

        fn Option::is_option(Option::Some(_)) { true }
        fn Option::is_option(Option::None) { true }
        fn Option::is_option(_) { false }

        fn Option.is_some(Option::Some(_)) { true }
        fn Option.is_some(Option::None) { false }

        fn Option.map(Option::Some(x), f) { Option::Some(f(x)) }
        fn Option.map(Option::None, _) { Option::None }
    "});
    let expected_output = indoc! {"
        class Option {
            static Some(x) {
                return new this({ tag: \"Some\", \"0\": x });
            }
            static get None() {
                const None = new this({ tag: \"None\" });
                Object.defineProperty(this, \"None\", { value: None });
                return None;
            }
            constructor({ tag, ...fields }) {
                this.tag = tag;
                Object.assign(this, fields);
            }
            static is_option(...args) {
                if (args[0].tag === \"Some\") {
                    return true;
                } else if (args[0].tag === \"None\") {
                    return true;
                } else {
                    return false;
                }
            }
            is_some(...args) {
                if (this.tag === \"Some\") {
                    return true;
                } else if (this.tag === \"None\") {
                    return false;
                }
            }
            map(...args) {
                if (this.tag === \"Some\") {
                    let x = this[0];
                    let f = args[0];
                    return Option.Some(f(x));
                } else if (this.tag === \"None\") {
                    return Option.None;
                }
            }
        }

    "};
    assert_eq!(output, expected_output);
}
