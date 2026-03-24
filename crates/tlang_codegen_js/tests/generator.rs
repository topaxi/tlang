use indoc::indoc;
use pretty_assertions::assert_eq;
use tlang_codegen_js::generator::{CodegenJS, shift_source_map_lines};
use tlang_defs::DefKind;
use tlang_parser::Parser;
use tlang_semantics::SemanticAnalyzer;

use self::common::CodegenOptions;

mod common;

fn hir_module_from(source: &str) -> tlang_hir::Module {
    let mut parser = Parser::from_source(source);
    let (mut ast, parse_meta) = parser.parse().expect("source should parse");
    let mut analyzer = SemanticAnalyzer::default();
    analyzer.add_builtin_symbols(CodegenJS::get_standard_library_symbols());
    analyzer
        .analyze(&mut ast)
        .expect("source should pass semantic analysis");
    let (module, _) = tlang_ast_lowering::lower_to_hir(
        &ast,
        &parse_meta.constant_pool_node_ids,
        analyzer.symbol_id_allocator(),
        analyzer.root_symbol_table(),
        analyzer.symbol_tables().clone(),
    )
    .expect("lowering should succeed");
    module
}

#[test]
fn test_get_standard_library_native_js() {
    let js = CodegenJS::get_standard_library_native_js();
    assert!(!js.is_empty());
}

#[test]
fn test_get_standard_library_native_js_files() {
    let files = CodegenJS::get_standard_library_native_js_files();
    assert!(!files.is_empty());
    for (name, content) in files {
        assert!(!name.is_empty());
        assert!(!content.is_empty());
    }
}

#[test]
fn test_get_js_ast_json() {
    let module = hir_module_from("let x = 1;");
    let mut codegen = CodegenJS::default();
    codegen
        .generate_code(&module)
        .expect("codegen should succeed");
    let json = codegen.get_js_ast_json();
    assert!(!json.is_empty());
}

#[test]
fn test_generate_code_with_source_map() {
    let source = "fn add(a, b) { a + b }";
    let module = hir_module_from(source);
    let mut codegen = CodegenJS::default();
    codegen
        .generate_code_with_source_map(&module, "test.tlang", source)
        .expect("codegen should succeed");
    assert!(!codegen.get_output().is_empty());
    assert!(codegen.get_source_map().is_some());
    assert!(codegen.get_source_map_json().is_some());
}

#[test]
fn test_shift_source_map_lines_zero_offset() {
    let source = "fn add(a, b) { a + b }";
    let module = hir_module_from(source);
    let mut codegen = CodegenJS::default();
    codegen
        .generate_code_with_source_map(&module, "test.tlang", source)
        .expect("codegen should succeed");
    let map_json = codegen.get_source_map_json().unwrap();
    // Zero offset: the map should be returned unchanged.
    let shifted = shift_source_map_lines(&map_json, 0);
    assert_eq!(shifted, map_json);
}

#[test]
fn test_shift_source_map_lines_with_offset() {
    let source = "fn add(a, b) { a + b }";
    let module = hir_module_from(source);
    let mut codegen = CodegenJS::default();
    codegen
        .generate_code_with_source_map(&module, "test.tlang", source)
        .expect("codegen should succeed");
    let map_json = codegen.get_source_map_json().unwrap();
    // Non-zero offset: the shifted map must differ from the original.
    let shifted = shift_source_map_lines(&map_json, 100);
    assert_ne!(shifted, map_json);
    // The shifted map must still be valid JSON (not empty).
    assert!(!shifted.is_empty());
}

#[test]
fn test_codegen_not_expression() {
    let output = compile!("not true;", CodegenOptions::default().optimize(false));
    let expected_output = "!true;\n";
    assert_eq!(output, expected_output);

    let output = compile!("!true;", CodegenOptions::default().optimize(false));
    let expected_output = "!true;\n";
    assert_eq!(output, expected_output);
}

#[test]
fn test_codegen_function_declaration() {
    let output = compile!("fn main() {}");
    let expected_output = "function main() {}\n";
    assert_eq!(output, expected_output);
}

#[test]
fn test_codegen_function_call() {
    let output = compile!(indoc! {"
        fn main() {}
        main();
    "});
    let expected_output = "function main() {}\nmain();\n";
    assert_eq!(output, expected_output);
}

#[test]
fn test_codegen_function_expression() {
    let output = compile!("fn main() { let foo = fn() { 1 + 2 }; }");
    let expected_output = indoc! {"
        function main() {
            let foo = () => 3;
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
    let output = compile!(
        "let x = (42 + 1) * 2;",
        CodegenOptions::default().optimize(false)
    );
    let expected_output = "let x = (42 + 1) * 2;\n";
    assert_eq!(output, expected_output);
}

#[test]
fn test_codegen_operator_precedence() {
    let output = compile!(
        "let result = 1 + 2 * 3;",
        CodegenOptions::default().optimize(false)
    );
    let expected_output = "let result = 1 + 2 * 3;\n";
    assert_eq!(output, expected_output);
}

#[test]
fn test_block_expression() {
    let output = compile!("let one = { 1 };");
    let expected_output = indoc! {"
        let $anf$0;
        $anf$0 = 1;
        let one = $anf$0;
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_block_expression_with_statements() {
    let output = compile!(
        "let one = { let x = 1; x };",
        CodegenOptions::default().optimize(false)
    );
    let expected_output = indoc! {"
        let $anf$0;
        let x = 1;
        $anf$0 = x;
        let one = $anf$0;
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
    let output = compile!("fn main() { if true { 1 } else { 2 } }");
    let expected_output = indoc! {"
        function main() {
            return true ? 1 : 2;
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_if_else_as_expression() {
    let output = compile!("fn main() { let result = if true { 1 } else { 2 }; }");
    let expected_output = indoc! {"
        function main() {
            let result = true ? 1 : 2;
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_if_else_as_expression_nested() {
    let output = compile!(indoc! {"
        fn main() {
            let result = if true {
                let x = if true { 1 } else { 2 };

                if x == 1 { 3 } else { 4 }
            } else {
                5
            };
        }
    "});
    let expected_output = indoc! {"
        function main() {
            let $anf$0;
            if (true) {
                let x = true ? 1 : 2;
                if (x === 1) {
                    $anf$0 = 3;
                } else {
                    $anf$0 = 4;
                }
            } else {
                $anf$0 = 5;
            }
            let result = $anf$0;
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
            let $anf$0;
            if (true) {
                $anf$0 = 1;
            } else if (true) {
                $anf$0 = 2;
            } else {
                $anf$0 = 3;
            }
            let result = $anf$0;
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
            return [
                1,
                2,
                3
            ];
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_partial_application() {
    let output = compile!("let add1 = add(_, 1);", vec![("add", DefKind::Function(2))]);
    let expected_output = indoc! {"
        let add1 = (_) => add(_, 1);
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_partial_application_with_multiple_arguments() {
    let output = compile!(
        "let add1 = add(_, 1, _);",
        vec![("add", DefKind::Function(3))]
    );
    let expected_output = indoc! {"
        let add1 = (_0, _1) => add(_0, 1, _1);
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_single_line_comments() {
    let output = compile!("// this is a comment\nlet a = 1;");
    let expected_output = "// this is a comment\nlet a = 1;\n";
    assert_eq!(output, expected_output);
}

#[test]
fn test_multi_line_comments() {
    let output = compile!("/* this is a comment */\nlet a = 1;");
    let expected_output = "/* this is a comment */\nlet a = 1;\n";
    assert_eq!(output, expected_output);

    let output = compile!("/* this is a comment\non multiple lines */\nlet a = 1;");
    let expected_output = "/* this is a comment\non multiple lines */\nlet a = 1;\n";
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

#[test]
fn test_and_or_as_keywords() {
    let output = compile!(
        indoc! {"
            fn main() {
                let x = true and false;
                let y = true or false;
            }
        "},
        CodegenOptions::default().optimize(false)
    );
    let expected_output = indoc! {"
        function main() {
            let x = true && false;
            let y = true || false;
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_dict_literal() {
    let output = compile!(indoc! {"
        fn main() {
            let x = { a: 1, b: 2 };
        }
    "});
    let expected_output = indoc! {"
        function main() {
            let x = {
                a: 1,
                b: 2
            };
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_dict_literal_shorthand() {
    let output = compile!(
        indoc! {"
            fn main() {
                let a = 1;
                let b = 2;
                let x = { a, b };
            }
        "},
        CodegenOptions::default().optimize(false)
    );
    let expected_output = indoc! {"
        function main() {
            let a = 1;
            let b = 2;
            let x = {
                a: a,
                b: b
            };
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_compile_stdlib_module_is_script_compatible() {
    // The bundle must be a plain script — no ES-module import/export declarations.
    // We check for `export ` (with space), `export{`, and `export*` which together
    // cover all valid ES module export forms: named, default, re-export, etc.
    let bundle = tlang_codegen_js::generator::CodegenJS::compile_stdlib_module();
    for line in bundle.lines() {
        let trimmed = line.trim_start();
        assert!(
            !trimmed.starts_with("export ")
                && !trimmed.starts_with("export{")
                && !trimmed.starts_with("export*"),
            "Stdlib bundle should not contain export declarations, but found: {line:?}"
        );
        assert!(
            !trimmed.starts_with("import ") && !trimmed.starts_with("import{"),
            "Stdlib bundle should not contain import declarations, but found: {line:?}"
        );
    }
}
