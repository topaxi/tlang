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
