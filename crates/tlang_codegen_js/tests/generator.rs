use indoc::indoc;
use pretty_assertions::assert_eq;
use tlang_ast::symbols::SymbolType;

use self::common::CodegenOptions;

mod common;

#[test]
fn test_codegen_not_expression() {
    let output = compile!("not true;");
    let expected_output = "!true;\n";
    assert_eq!(output, expected_output);

    let output = compile!("!true;");
    let expected_output = "!true;\n";
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
            let foo = () => 1 + 2;
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
        let one = (() => {
            return 1;
        })();
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_block_expression_with_statements() {
    let output = compile!("let one = { let x = 1; x };");
    let expected_output = indoc! {"
        let one = (() => {
            let x = 1;
            return x;
        })();
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
fn test_if_else_if() {
    let output = compile!("if true { 1; } else if (false) { 2; } else { 3; }");
    let expected_output = indoc! {"
        if (true) {
            1;
        } else if (false) {
            2;
        } else {
            3;
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_if_else_as_expression_as_fn_completion() {
    let output = compile!(
        "fn main() { if true { 1 } else { 2 } }",
        CodegenOptions::default().set_render_ternary(false)
    );
    let expected_output = indoc! {"
        function main() {
            if (true) {
                return 1;
            } else {
                return 2;
            }
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_if_else_as_expression() {
    let output = compile!(
        "fn main() { let result = if true { 1 } else { 2 }; }",
        CodegenOptions::default().set_render_ternary(false)
    );
    let expected_output = indoc! {"
        function main() {
            let result = (() => {
                if (true) {
                    return 1;
                } else {
                    return 2;
                }
            })();
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_if_else_as_expression_nested() {
    let output = compile!(
        indoc! {"
        fn main() {
            let result = if true {
                let x = if true { 1 } else { 2 };

                if x == 1 { 3 } else { 4 }
            } else {
                5
            };
        }
    "},
        CodegenOptions::default().set_render_ternary(false)
    );
    let expected_output = indoc! {"
        function main() {
            let result = (() => {
                if (true) {
                    let x = (() => {
                        if (true) {
                            return 1;
                        } else {
                            return 2;
                        }
                    })();
                    return (() => {
                        if (x === 1) {
                            return 3;
                        } else {
                            return 4;
                        }
                    })();
                } else {
                    return 5;
                }
            })();
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_if_else_if_as_expression() {
    let output =
        compile!("fn main() { let result = if true { 1 } else if true { 2 } else { 3 }; }");
    let expected_output = indoc! {"
        function main() {
            let result = (() => {
                if (true) {
                    return 1;
                } else if (true) {
                    return 2;
                } else {
                    return 3;
                }
            })();
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_if_else_ternary() {
    let output = compile!("fn main() { if true { 1 } else { 2 } }");
    let expected_output = indoc! {"
        function main() {
            return true ? 1 : 2;
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_if_else_ternary_precedence() {
    let output = compile!("fn main() { false && if true { 1 } else { 2 } }");
    let expected_output = indoc! {"
        function main() {
            return false && (true ? 1 : 2);
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
    let output = compile!("let add1 = add(_, 1);", vec![("add", SymbolType::Function)]);
    let expected_output = indoc! {"
        let add1 = (_p0) => add(_p0, 1);
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_partial_application_with_multiple_arguments() {
    let output = compile!(
        "let add1 = add(_, 1, _);",
        vec![("add", SymbolType::Function)]
    );
    let expected_output = indoc! {"
        let add1 = (_p0, _p1) => add(_p0, 1, _p1);
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
            let list = [];
            let x = list.length;
        }
    "});
    let expected_output = indoc! {"
        function main() {
            let list = [];
            let x = list.length;
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_index_access_expressions() {
    let output = compile!(indoc! {"
        fn main() {
            let foo = [1];
            let x = foo[0];
        }
    "});
    let expected_output = indoc! {"
        function main() {
            let foo = [1];
            let x = foo[0];
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
#[ignore = "Codegen currently returns undefined instead of the break value for loop expressions"]
fn test_loop_expression_break_value() {
    let output = compile!(indoc! {"
        fn test() {
            let x = loop {
                // Some condition/work eventually leads to break
                if true { // Simplified condition
                    break 100;
                }
            };
            return x; // Expect x to be 100
        }
    "});
    // Expected JS uses return inside the IIFE loop to provide the value
    let expected_output = indoc! {"
        function test() {
            let x = (() => {
                for (;;) {
                    if (true) {
                        return 100;
                    }
                }
            })();
            return x;
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_return_inside_if_expression() {
    let output = compile!(indoc! {"
        fn test(cond) {
            let y = 5;
            let x = if cond {
                y = 10; // Side effect before return
                return \"early_exit\"; // Should exit test() function
                1 // Unreachable in this branch
            } else {
                2
            };
            // This part should only execute if cond is false
            y = y + x;
            return y; // Should return 5 + 2 = 7 if cond is false
        }
    "});
    // Expected JS avoids IIFE because of the return statement.
    // The 'return' directly exits the function.
    let expected_output = indoc! {"
        function test(cond) {
            let y = 5;
            let x;
            if (cond) {
                y = 10;
                return \"early_exit\";
            } else {
                x = 2;
            }
            y = y + x;
            return y;
        }
    "};
    assert_eq!(output, expected_output);
}
