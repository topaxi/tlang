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
    assert_snapshot!(pretty_print(&hir), @r###"
    fn main() -> unknown {
        let x: unknown = (1 + 2);
        x
    }
    "###);
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
    assert_snapshot!(pretty_print(&hir), @r###"
    fn main() -> unknown {
        let $hir$0: unknown = _;
        {
            let y: unknown = 1;
            ($hir$0 = (y + 2));
        };
        let x: unknown = $hir$0;
        x
    }
    "###);
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
    assert_snapshot!(pretty_print(&hir), @r###"
    fn main() -> unknown {
        let $hir$0: unknown = _;
        if true {
            let y: unknown = 1;
            ($hir$0 = (y + 2));
        } else {
            ($hir$0 = 3);
        };
        let x: unknown = $hir$0;
        x
    }
    "###);
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
    assert_snapshot!(pretty_print(&hir), @r"
    fn main() -> unknown {
        let $hir$0: unknown = _;
        if true {
            ($hir$0 = {
                let a: unknown = 1;
                (a + 2)
            });
        } else {
            ($hir$0 = {
                let b: unknown = 3;
                (b * 2)
            });
        };
        let x: unknown = $hir$0;
        x
    }
    ");
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
    assert_snapshot!(pretty_print(&hir), @r"
    fn main() -> unknown {
        let $hir$0: unknown = _;
        {
            let x: unknown = 1;
            ($hir$0 = (x + 2));
        };
        println($hir$0);
    }
    ");
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
    assert_snapshot!(pretty_print(&hir), @r"
    fn main() -> unknown {
        let $hir$0: unknown = _;
        {
            let a: unknown = 1;
            ($hir$0 = (a + 1));
        };
        let $hir$1: unknown = _;
        {
            let b: unknown = 2;
            ($hir$1 = (b * 2));
        };
        let x: unknown = ($hir$0 + $hir$1);
        x
    }
    ");
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
    assert_snapshot!(pretty_print(&hir), @r"
    fn main() -> unknown {
        let $hir$0: unknown = _;
        {
            let a: unknown = true;
            ($hir$0 = a);
        };
        let $hir$1: unknown = _;
        if $hir$0 {
            let $hir$2: unknown = _;
            {
                let b: unknown = false;
                ($hir$2 = b);
            };
            ($hir$1 = $hir$2);
        } else {
            ($hir$1 = $hir$0);
        };
        let x: unknown = $hir$1;
        x
    }
    ");
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
    assert_snapshot!(pretty_print(&hir), @r"
    fn main() -> unknown {
        let $hir$0: unknown = _;
        {
            let a: unknown = 1;
            ($hir$0 = (a + 1));
        };
        let $hir$1: unknown = _;
        {
            let b: unknown = 2;
            ($hir$1 = (b * 2));
        };
        let x: unknown = [$hir$0, $hir$1];
        x
    }
    ");
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
    assert_snapshot!(pretty_print(&hir), @r#"
    fn main() -> unknown {
        let $hir$0: unknown = _;
        {
            let a: unknown = 1;
            ($hir$0 = (a + 1));
        };
        let $hir$1: unknown = _;
        {
            let b: unknown = "key";
            ($hir$1 = (b + "2"));
        };
        let $hir$2: unknown = _;
        {
            let c: unknown = 2;
            ($hir$2 = (c * 2));
        };
        let x: unknown = [$hir$0, $hir$1, $hir$2];
        x
    }
    "#);
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
    assert_snapshot!(pretty_print(&hir), @r"
    fn main() -> unknown {
        let $hir$0: unknown = _;
        {
            let arr: unknown = [1, 2, 3];
            ($hir$0 = arr);
        };
        let $hir$1: unknown = _;
        {
            let i: unknown = 1;
            ($hir$1 = i);
        };
        let x: unknown = $hir$0[$hir$1];
        x
    }
    ");
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
    assert_snapshot!(pretty_print(&hir), @r"
    fn main() -> unknown {
        let $hir$0: unknown = _;
        {
            let obj: unknown = {field: 42};
            ($hir$0 = obj);
        };
        let x: unknown = $hir$0.field;
        x
    }
    ");
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
    assert_snapshot!(pretty_print(&hir), @r"
    fn main() -> unknown {
        let $hir$0: unknown = _;
        {
            let a: unknown = true;
            ($hir$0 = a);
        };
        let x: unknown = !$hir$0;
        x
    }
    ");
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
    assert_snapshot!(pretty_print(&hir), @r"
    fn main() -> unknown {
        let $hir$0: unknown = _;
        {
            let x: unknown = 1;
            ($hir$0 = (x + 2));
        };
        return $hir$0;
    }
    ");
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
    assert_snapshot!(pretty_print(&hir), @r###"
    fn main() -> unknown {
        let $hir$0: unknown = _;
        {
            let x: unknown = {
                let y: unknown = 1;
                (y + 2)
            };
            ($hir$0 = (x * 3));
        };
        let result: unknown = $hir$0;
        result
    }
    "###);
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
    assert_snapshot!(pretty_print(&hir), @r"
    fn main() -> unknown {
        {
            let x: unknown = {
                let y: unknown = 1;
                (y + 2)
            };
            println(x);
        };
    }
    ");
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
    assert_snapshot!(pretty_print(&hir), @r"
    fn main() -> unknown {
        let $hir$0: unknown = _;
        {
            let x: unknown = 1;
            ($hir$0 = (x + 1));
        };
        let a: unknown = $hir$0;
        let $hir$1: unknown = _;
        {
            let y: unknown = 2;
            ($hir$1 = (y + 2));
        };
        let b: unknown = $hir$1;
        let $hir$2: unknown = _;
        {
            let z: unknown = 3;
            ($hir$2 = (z + 3));
        };
        let c: unknown = $hir$2;
        ((a + b) + c)
    }
    ");
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
    assert_snapshot!(pretty_print(&hir), @r###"
    fn main() -> unknown {
        let x: unknown = 1;
        let y: unknown = 2;
        (x + y)
    }
    "###);
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
    assert_snapshot!(pretty_print(&hir), @r"
    fn main() -> unknown {
        let simple: unknown = (1 + 2);
        let $hir$0: unknown = _;
        {
            let inner: unknown = (simple * 2);
            ($hir$0 = (inner + 1));
        };
        let complex: unknown = $hir$0;
        (simple + complex)
    }
    ");
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
    assert_snapshot!(pretty_print(&hir), @r###"
    fn main() -> unknown {
        let x: unknown = if true {
            1
        } else {
            2
        };
        x
    }
    "###);
}

#[test]
fn test_nested_if_else_expressions() {
    let source = r#"
        fn main() {
            let result = if true {
                let x = if true { 1 } else { 2 };
                if x == 1 { 3 } else { 4 }
            } else {
                5
            };
        }
    "#;
    let hir = compile_and_apply_hir_js_pass(source);
    assert_snapshot!(pretty_print(&hir), @r"
    fn main() -> unknown {
        let $hir$0: unknown = _;
        if true {
            let x: unknown = if true {
                1
            } else {
                2
            };
            let $hir$1: unknown = _;
            if (x == 1) {
                ($hir$1 = 3);
            } else {
                ($hir$1 = 4);
            };
            ($hir$0 = $hir$1);
        } else {
            ($hir$0 = 5);
        };
        let result: unknown = $hir$0;
    }
    ");
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
    assert_snapshot!(pretty_print(&hir), @r"
    fn main() -> unknown {
        let $hir$0: unknown = _;
        {
            let a: unknown = {
                let x: unknown = 1;
                (x + 1)
            };
            let b: unknown = {
                let y: unknown = 2;
                (y * 2)
            };
            ($hir$0 = (a + b));
        };
        println($hir$0);
    }
    ");
}

#[test]
fn test_simple_loop_expression_in_let() {
    let source = r#"
        fn main() {
            let result = loop {
                break 42;
            };
            result
        }
    "#;
    let hir = compile_and_apply_hir_js_pass(source);
    assert_snapshot!(pretty_print(&hir), @r###"
    fn main() -> unknown {
        __TEMP_VAR_LOOP__("$hir$1", loop {
            break 42;
        });
        __TEMP_VAR_LOOP__("$hir$0", $hir$1);
        let result: unknown = $hir$0;
        result
    }
    "###);
}

#[test]
fn test_loop_expression_with_complex_break() {
    let source = r#"
        fn main() {
            let x = 10;
            let result = loop {
                if x > 5 { 
                    let y = x * 2;
                    break y + 1; 
                }
            };
            result
        }
    "#;
    let hir = compile_and_apply_hir_js_pass(source);
    assert_snapshot!(pretty_print(&hir), @r###"
    fn main() -> unknown {
        let x: unknown = 10;
        __TEMP_VAR_LOOP__("$hir$1", loop {
            if (x > 5) {
                let y: unknown = (x * 2);
                break (y + 1);
            }
        });
        __TEMP_VAR_LOOP__("$hir$0", $hir$1);
        let result: unknown = $hir$0;
        result
    }
    "###);
}

#[test]
fn test_nested_loop_expressions() {
    let source = r#"
        fn main() {
            let outer = loop {
                let inner = loop {
                    break 5;
                };
                if inner == 5 { break inner * 2; }
            };
            outer
        }
    "#;
    let hir = compile_and_apply_hir_js_pass(source);
    assert_snapshot!(pretty_print(&hir), @r###"
    fn main() -> unknown {
        __TEMP_VAR_LOOP__("$hir$1", loop {
            let inner: unknown = loop {
                break 5;
            };
            if (inner == 5) {
                break (inner * 2);
            }
        });
        __TEMP_VAR_LOOP__("$hir$0", $hir$1);
        let outer: unknown = $hir$0;
        outer
    }
    "###);
}

#[test]
fn test_loop_expression_in_function_argument() {
    let source = r#"
        fn main() {
            println(loop {
                break 42;
            });
        }
    "#;
    let hir = compile_and_apply_hir_js_pass(source);
    assert_snapshot!(pretty_print(&hir), @r###"
    fn main() -> unknown {
        __TEMP_VAR_LOOP__("$hir$1", loop {
            break 42;
        });
        __TEMP_VAR_LOOP__("$hir$0", $hir$1);
        println($hir$0);
    }
    "###);
}

#[test]
fn test_loop_expression_in_binary_expression() {
    let source = r#"
        fn main() {
            let result = loop {
                break 10;
            } + loop {
                break 20;
            };
            result
        }
    "#;
    let hir = compile_and_apply_hir_js_pass(source);
    assert_snapshot!(pretty_print(&hir), @r###"
    fn main() -> unknown {
        __TEMP_VAR_LOOP__("$hir$2", loop {
            break 10;
        });
        __TEMP_VAR_LOOP__("$hir$0", $hir$2);
        __TEMP_VAR_LOOP__("$hir$3", loop {
            break 20;
        });
        __TEMP_VAR_LOOP__("$hir$1", $hir$3);
        let result: unknown = ($hir$0 + $hir$1);
        result
    }
    "###);
}

#[test]
fn test_loop_expression_in_if_condition() {
    let source = r#"
        fn main() {
            let result = if loop { break true; } {
                42
            } else {
                0
            };
            result
        }
    "#;
    let hir = compile_and_apply_hir_js_pass(source);
    assert_snapshot!(pretty_print(&hir), @r###"
    fn main() -> unknown {
        __TEMP_VAR_LOOP__("$hir$2", loop {
            break true;
        });
        __TEMP_VAR_LOOP__("$hir$0", $hir$2);
        let $hir$1: unknown = _;
        if $hir$0 {
            ($hir$1 = 42);
        } else {
            ($hir$1 = 0);
        };
        let result: unknown = $hir$1;
        result
    }
    "###);
}

#[test]
fn test_loop_expression_in_list() {
    let source = r#"
        fn main() {
            let result = [
                loop { break 1; },
                loop { break 2; },
                42
            ];
            result
        }
    "#;
    let hir = compile_and_apply_hir_js_pass(source);
    assert_snapshot!(pretty_print(&hir), @r###"
    fn main() -> unknown {
        __TEMP_VAR_LOOP__("$hir$2", loop {
            break 1;
        });
        __TEMP_VAR_LOOP__("$hir$0", $hir$2);
        __TEMP_VAR_LOOP__("$hir$3", loop {
            break 2;
        });
        __TEMP_VAR_LOOP__("$hir$1", $hir$3);
        let result: unknown = [$hir$0, $hir$1, 42];
        result
    }
    "###);
}

#[test]
fn test_loop_expression_with_block_break() {
    let source = r#"
        fn main() {
            let result = loop {
                let x = {
                    let y = 1;
                    y + 2
                };
                break x;
            };
            result
        }
    "#;
    let hir = compile_and_apply_hir_js_pass(source);
    assert_snapshot!(pretty_print(&hir), @r###"
    fn main() -> unknown {
        __TEMP_VAR_LOOP__("$hir$1", loop {
            let x: unknown = {
                let y: unknown = 1;
                (y + 2)
            };
            break x;
        });
        __TEMP_VAR_LOOP__("$hir$0", $hir$1);
        let result: unknown = $hir$0;
        result
    }
    "###);
}

#[test]
fn test_nested_loop_with_complex_expressions() {
    let source = r#"
        fn main() {
            let result = loop {
                let inner_result = loop {
                    let value = {
                        let a = 1;
                        a + 1
                    };
                    break value * 2;
                };
                if inner_result > 3 {
                    break inner_result + 10;
                }
            };
            result
        }
    "#;
    let hir = compile_and_apply_hir_js_pass(source);
    assert_snapshot!(pretty_print(&hir), @r###"
    fn main() -> unknown {
        __TEMP_VAR_LOOP__("$hir$1", loop {
            let inner_result: unknown = loop {
                let value: unknown = {
                    let a: unknown = 1;
                    (a + 1)
                };
                break (value * 2);
            };
            if (inner_result > 3) {
                break (inner_result + 10);
            }
        });
        __TEMP_VAR_LOOP__("$hir$0", $hir$1);
        let result: unknown = $hir$0;
        result
    }
    "###);
}

#[test]
fn test_loop_expression_in_field_access() {
    let source = r#"
        fn main() {
            let obj = { field: 0 };
            let result = obj[loop { break "field"; }];
            result
        }
    "#;
    let hir = compile_and_apply_hir_js_pass(source);
    assert_snapshot!(pretty_print(&hir), @r###"
    fn main() -> unknown {
        let obj: unknown = {field: 0};
        __TEMP_VAR_LOOP__("$hir$1", loop {
            break "field";
        });
        __TEMP_VAR_LOOP__("$hir$0", $hir$1);
        let result: unknown = obj[$hir$0];
        result
    }
    "###);
}

#[test]
fn test_for_loop_expression_in_let() {
    let source = r#"
        fn test() {
            let result = for i in [1, 2, 3] with acc = 0 {
                acc + i
            };
            result
        }
    "#;
    let hir = compile_and_apply_hir_js_pass(source);
    assert_snapshot!(pretty_print(&hir), @r###"
    fn test() -> unknown {
        let $hir$0: unknown = _;
        {
            let iterator$$: unknown = iterator::iter([1, 2, 3]);
            let accumulator$$: unknown = 0;
            ($hir$0 = __TEMP_VAR_LOOP__("$hir$1", loop {
                let acc: unknown = accumulator$$;
                (accumulator$$ = match iterator$$.next() {
                    Option::Some { 0: i } => {
                        (acc + i)
                    },
                    Option::None => {
                        break accumulator$$
                    },
                })
            }));
        };
        let result: unknown = $hir$0;
        result
    }
    "###);
}
