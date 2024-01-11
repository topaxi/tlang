use indoc::indoc;
use pretty_assertions::assert_eq;
use tlang_ast::symbols::SymbolType;

mod common;

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
fn test_block_expression_with_statements() {
    let output = compile!("let one = { let x = 1; x };");
    let expected_output = indoc! {"
        let $tmp$a;{
            let x = 1;
            $tmp$a = x;
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

#[test]
fn test_if_else_as_expression_as_fn_completion() {
    let output = compile!("fn main() { if true { 1 } else { 2 } }");
    let expected_output = indoc! {"
        function main() {
            let $tmp$a;if (true) {
                $tmp$a = 1;
            } else {
                $tmp$a = 2;
            }
            return $tmp$a;
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_if_else_as_expression() {
    let output = compile!("fn main() { let result = if true { 1 } else { 2 }; }");
    let expected_output = indoc! {"
        function main() {
            let $tmp$a;if (true) {
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
    let output = compile!("let add1 = add(_, 1); }", &[("add", SymbolType::Function)]);
    let expected_output = indoc! {"
        let add1 = function(...args) {
            return add(args[0], 1);
        };
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_partial_application_with_multiple_arguments() {
    let output = compile!(
        "let add1 = add(_, 1, _); }",
        &[("add", SymbolType::Function)]
    );
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

#[test]
#[ignore = "implement structs based on the draft below"]
fn test_declare_methods_on_struct() {
    let output = compile!(indoc! {"
        struct Point {
            x,
            y,
        }
        fn Point::new(x, y) { Point { x, y } }
        fn Point.x(self) { self.x }
        fn Point.y(self) { self.y }
    "});
    // TODO: Based on classes? Or reinvent things?
    let expected_output = indoc! {"
        class Point {
            static new(x, y) {
                return new this({ x, y });
            }
            constructor({ x, y }) {
                this.x = x;
                this.y = y;
            }
            x() {
                return this.x;
            }
            y() {
                return this.y;
            }
        }
    "};
    assert_eq!(output, expected_output);
}
