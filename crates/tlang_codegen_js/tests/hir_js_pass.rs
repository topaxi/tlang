use insta::assert_snapshot;
use tlang_ast_lowering::lower_to_hir;
use tlang_codegen_js::hir_js_pass::HirJsPass;
use tlang_hir::hir;
use tlang_hir_opt::{HirOptContext, HirPass};
use tlang_hir_pretty::HirPrettyOptions;
use tlang_parser::Parser;
use tlang_semantics::SemanticAnalyzer;
use tlang_symbols::SymbolType;

#[ctor::ctor]
fn before_all() {
    let _ = env_logger::builder()
        .filter_level(log::LevelFilter::Warn)
        .parse_default_env()
        .is_test(true)
        .try_init();
}

fn compile(source: &str) -> hir::LowerResult {
    let ast = Parser::from_source(source).parse().unwrap();
    let mut semantic_analyzer = SemanticAnalyzer::default();
    semantic_analyzer.add_builtin_symbols(&[("println", SymbolType::Function(u16::MAX))]);
    semantic_analyzer.analyze(&ast).unwrap();
    lower_to_hir(
        &ast,
        semantic_analyzer.symbol_id_allocator(),
        semantic_analyzer.root_symbol_table(),
        semantic_analyzer.symbol_tables().clone(),
    )
}

fn compile_and_apply_hir_js_pass(source: &str) -> hir::Module {
    let (mut module, meta) = compile(source);
    let mut ctx = HirOptContext::from(meta);
    let mut pass = HirJsPass::new();
    pass.optimize_hir(&mut module, &mut ctx);
    module
}

fn pretty_print(module: &hir::Module) -> String {
    let options = HirPrettyOptions {
        mark_unresolved: false,
        ..Default::default()
    };
    let mut prettier = tlang_hir_pretty::HirPretty::new(options);
    prettier.print_module(module);
    prettier.output().to_string()
}

#[test]
fn test_simple_expression_no_transformation() {
    let source = r#"
        fn main() {
            let x = 1 + 2;
            x
        }
    "#;
    let hir = compile_and_apply_hir_js_pass(source);
    assert_snapshot!(pretty_print(&hir));
}

#[test]
fn test_block_expression_flattening() {
    let source = r#"
        fn main() {
            let x = {
                let y = 1;
                y + 2
            };
            x
        }
    "#;
    let hir = compile_and_apply_hir_js_pass(source);
    assert_snapshot!(pretty_print(&hir));
}

#[test]
fn test_if_else_expression_flattening() {
    let source = r#"
        fn main() {
            let x = if true {
                let y = 1;
                y + 2
            } else {
                3
            };
            x
        }
    "#;
    let hir = compile_and_apply_hir_js_pass(source);
    assert_snapshot!(pretty_print(&hir));
}

#[test]
fn test_nested_complex_expressions() {
    let source = r#"
        fn main() {
            let x = if true {
                {
                    let a = 1;
                    a + 2
                }
            } else {
                {
                    let b = 3;
                    b * 2
                }
            };
            x
        }
    "#;
    let hir = compile_and_apply_hir_js_pass(source);
    assert_snapshot!(pretty_print(&hir));
}

#[test]
fn test_function_call_argument_flattening() {
    let source = r#"
        fn main() {
            println({
                let x = 1;
                x + 2
            });
        }
    "#;
    let hir = compile_and_apply_hir_js_pass(source);
    assert_snapshot!(pretty_print(&hir));
}

#[test]
fn test_binary_expression_with_complex_operands() {
    let source = r#"
        fn main() {
            let x = {
                let a = 1;
                a + 1
            } + {
                let b = 2;
                b * 2
            };
            x
        }
    "#;
    let hir = compile_and_apply_hir_js_pass(source);
    assert_snapshot!(pretty_print(&hir));
}

#[test]
fn test_short_circuit_operators_special_handling() {
    let source = r#"
        fn main() {
            let x = {
                let a = true;
                a
            } && {
                let b = false;
                b
            };
            x
        }
    "#;
    let hir = compile_and_apply_hir_js_pass(source);
    assert_snapshot!(pretty_print(&hir));
}

#[test]
fn test_list_expression_flattening() {
    let source = r#"
        fn main() {
            let x = [{
                let a = 1;
                a + 1
            }, {
                let b = 2;
                b * 2
            }];
            x
        }
    "#;
    let hir = compile_and_apply_hir_js_pass(source);
    assert_snapshot!(pretty_print(&hir));
}

#[test]
fn test_dict_expression_flattening() {
    let source = r#"
        fn main() {
            let x = [
                {
                    let a = 1;
                    a + 1
                },
                {
                    let b = "key";
                    b + "2"
                },
                {
                    let c = 2;
                    c * 2
                }
            ];
            x
        }
    "#;
    let hir = compile_and_apply_hir_js_pass(source);
    assert_snapshot!(pretty_print(&hir));
}

#[test]
fn test_index_access_flattening() {
    let source = r#"
        fn main() {
            let x = {
                let arr = [1, 2, 3];
                arr
            }[{
                let i = 1;
                i
            }];
            x
        }
    "#;
    let hir = compile_and_apply_hir_js_pass(source);
    assert_snapshot!(pretty_print(&hir));
}

#[test]
fn test_field_access_flattening() {
    let source = r#"
        fn main() {
            let x = {
                let obj = { field: 42 };
                obj
            }.field;
            x
        }
    "#;
    let hir = compile_and_apply_hir_js_pass(source);
    assert_snapshot!(pretty_print(&hir));
}

#[test]
fn test_unary_expression_flattening() {
    let source = r#"
        fn main() {
            let x = !{
                let a = true;
                a
            };
            x
        }
    "#;
    let hir = compile_and_apply_hir_js_pass(source);
    assert_snapshot!(pretty_print(&hir));
}

#[test]
fn test_return_statement_flattening() {
    let source = r#"
        fn main() {
            return {
                let x = 1;
                x + 2
            };
        }
    "#;
    let hir = compile_and_apply_hir_js_pass(source);
    assert_snapshot!(pretty_print(&hir));
}

#[test]
fn test_let_statement_flattening() {
    let source = r#"
        fn main() {
            let result = {
                let x = {
                    let y = 1;
                    y + 2
                };
                x * 3
            };
            result
        }
    "#;
    let hir = compile_and_apply_hir_js_pass(source);
    assert_snapshot!(pretty_print(&hir));
}

#[test]
fn test_expression_statement_flattening() {
    let source = r#"
        fn main() {
            {
                let x = {
                    let y = 1;
                    y + 2
                };
                println(x);
            };
        }
    "#;
    let hir = compile_and_apply_hir_js_pass(source);
    assert_snapshot!(pretty_print(&hir));
}

#[test]
fn test_temp_var_generation() {
    let source = r#"
        fn main() {
            let a = {
                let x = 1;
                x + 1
            };
            let b = {
                let y = 2;
                y + 2
            };
            let c = {
                let z = 3;
                z + 3
            };
            a + b + c
        }
    "#;
    let hir = compile_and_apply_hir_js_pass(source);
    assert_snapshot!(pretty_print(&hir));
}

#[test]
fn test_no_changes_for_simple_expressions() {
    let source = r#"
        fn main() {
            let x = 1;
            let y = 2;
            x + y
        }
    "#;
    let hir = compile_and_apply_hir_js_pass(source);
    assert_snapshot!(pretty_print(&hir));
}

#[test]
fn test_mixed_simple_and_complex_expressions() {
    let source = r#"
        fn main() {
            let simple = 1 + 2;
            let complex = {
                let inner = simple * 2;
                inner + 1
            };
            simple + complex
        }
    "#;
    let hir = compile_and_apply_hir_js_pass(source);
    assert_snapshot!(pretty_print(&hir));
}

#[test]
fn test_ternary_operator_preservation() {
    let source = r#"
        fn main() {
            let x = if true { 1 } else { 2 };
            x
        }
    "#;
    let hir = compile_and_apply_hir_js_pass(source);
    assert_snapshot!(pretty_print(&hir));
}

#[test]
fn test_complex_nested_function_calls() {
    let source = r#"
        fn main() {
            println({
                let a = {
                    let x = 1;
                    x + 1
                };
                let b = {
                    let y = 2;
                    y * 2
                };
                a + b
            });
        }
    "#;
    let hir = compile_and_apply_hir_js_pass(source);
    assert_snapshot!(pretty_print(&hir));
}