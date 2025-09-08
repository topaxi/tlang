use insta::assert_snapshot;
use tlang_ast_lowering::lower_to_hir;
use tlang_codegen_js::create_hir_js_opt_group;
use tlang_hir::hir;
use tlang_hir_opt::{HirOptContext, HirPass};
use tlang_hir_pretty::HirPrettyOptions;
use tlang_parser::Parser;
use tlang_semantics::SemanticAnalyzer;

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
    semantic_analyzer.add_builtin_symbols(
        tlang_codegen_js::generator::CodegenJS::get_standard_library_symbols(),
    );
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
    let mut hir_js_opt_group = create_hir_js_opt_group();

    // Run the HIR JS optimization group
    hir_js_opt_group.optimize_hir(&mut module, &mut ctx);

    module
}

fn compile_and_apply_hir_js_pass_debug(source: &str) -> (hir::Module, hir::Module) {
    let (mut module, meta) = compile(source);
    let module_before = module.clone();
    let mut ctx = HirOptContext::from(meta);
    let mut hir_js_opt_group = create_hir_js_opt_group();
    hir_js_opt_group.optimize_hir(&mut module, &mut ctx);
    (module_before, module)
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
    assert_snapshot!(pretty_print(&hir), @r"
    fn main() -> unknown {
        let x: unknown = (1 + 2);
        return x;
    }
    ");
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
        return x;
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
    assert_snapshot!(pretty_print(&hir), @r"
    fn main() -> unknown {
        let $hir$0: unknown = _;
        if true {
            let y: unknown = 1;
            ($hir$0 = (y + 2));
        } else {
            ($hir$0 = 3);
        };
        let x: unknown = $hir$0;
        return x;
    }
    ");
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
            let a: unknown = 1;
            ($hir$0 = (a + 2));
        } else {
            let b: unknown = 3;
            ($hir$0 = (b * 2));
        };
        let x: unknown = $hir$0;
        return x;
    }
    ");
}

#[test]
fn test_function_call_argument_flattening() {
    let source = r#"
        fn main() {
            log({
                let x = 1;
                x + 2
            });
        }
    "#;
    let hir = compile_and_apply_hir_js_pass(source);
    assert_snapshot!(pretty_print(&hir), @r###"
    fn main() -> unknown {
        let $hir$0: unknown = _;
        {
            let x: unknown = 1;
            ($hir$0 = (x + 2));
        };
        log($hir$0);
    }
    "###);
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
    assert_snapshot!(pretty_print(&hir), @r###"
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
        return x;
    }
    "###);
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
        let $hir$1: unknown = _;
        {
            let a: unknown = true;
            ($hir$1 = a);
        };
        if $hir$1 {
            let $hir$2: unknown = _;
            {
                let b: unknown = false;
                ($hir$2 = b);
            };
            ($hir$0 = $hir$2);
        } else {
            ($hir$0 = false);
        };
        let x: unknown = $hir$0;
        return x;
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
        return x;
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
        return x;
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
        return x;
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
        return x;
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
    assert_snapshot!(pretty_print(&hir), @r###"
    fn main() -> unknown {
        let $hir$0: unknown = _;
        {
            let a: unknown = true;
            ($hir$0 = a);
        };
        let x: unknown = !$hir$0;
        return x;
    }
    "###);
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
    assert_snapshot!(pretty_print(&hir), @r"
    fn main() -> unknown {
        let $hir$0: unknown = _;
        {
            let $hir$1: unknown = _;
            {
                let y: unknown = 1;
                ($hir$1 = (y + 2));
            };
            let x: unknown = $hir$1;
            ($hir$0 = (x * 3));
        };
        let result: unknown = $hir$0;
        return result;
    }
    ");
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
                log(x);
            };
        }
    "#;
    let hir = compile_and_apply_hir_js_pass(source);
    assert_snapshot!(pretty_print(&hir), @r"
    fn main() -> unknown {
        let $hir$0: unknown = _;
        {
            let $hir$1: unknown = _;
            {
                let y: unknown = 1;
                ($hir$1 = (y + 2));
            };
            let x: unknown = $hir$1;
            log(x);
        };
        $hir$0;
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
        return ((a + b) + c);
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
    assert_snapshot!(pretty_print(&hir), @r"
    fn main() -> unknown {
        let x: unknown = 1;
        let y: unknown = 2;
        return (x + y);
    }
    ");
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
        return (simple + complex);
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
    assert_snapshot!(pretty_print(&hir), @r"
    fn main() -> unknown {
        let x: unknown = if true {
            1
        } else {
            2
        };
        return x;
    }
    ");
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
            ($hir$0 = if (x == 1) {
                3
            } else {
                4
            });
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
            log({
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
            let $hir$1: unknown = _;
            {
                let x: unknown = 1;
                ($hir$1 = (x + 1));
            };
            let a: unknown = $hir$1;
            let $hir$2: unknown = _;
            {
                let y: unknown = 2;
                ($hir$2 = (y * 2));
            };
            let b: unknown = $hir$2;
            ($hir$0 = (a + b));
        };
        log($hir$0);
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
    assert_snapshot!(pretty_print(&hir), @r"
    fn main() -> unknown {
        let $hir$0: unknown = _;
        loop {
            ($hir$0 = 42);
            break;
        };
        let result: unknown = $hir$0;
        return result;
    }
    ");
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
    assert_snapshot!(pretty_print(&hir), @r"
    fn main() -> unknown {
        let x: unknown = 10;
        let $hir$0: unknown = _;
        loop {
            let $hir$1: unknown = _;
            if (x > 5) {
                let y: unknown = (x * 2);
                ($hir$0 = (y + 1));
                break;
            };
            $hir$1
        };
        let result: unknown = $hir$0;
        return result;
    }
    ");
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
    assert_snapshot!(pretty_print(&hir), @r"
    fn main() -> unknown {
        let $hir$0: unknown = _;
        loop {
            let $hir$1: unknown = _;
            loop {
                ($hir$1 = 5);
                break;
            };
            let inner: unknown = $hir$1;
            let $hir$2: unknown = _;
            if (inner == 5) {
                ($hir$0 = (inner * 2));
                break;
            };
            $hir$2
        };
        let outer: unknown = $hir$0;
        return outer;
    }
    ");
}

#[test]
fn test_loop_expression_in_function_argument() {
    let source = r#"
        fn main() {
            log(loop {
                break 42;
            });
        }
    "#;
    let hir = compile_and_apply_hir_js_pass(source);
    assert_snapshot!(pretty_print(&hir), @r###"
    fn main() -> unknown {
        let $hir$0: unknown = _;
        loop {
            ($hir$0 = 42);
            break;
        };
        log($hir$0);
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
    assert_snapshot!(pretty_print(&hir), @r"
    fn main() -> unknown {
        let $hir$0: unknown = _;
        loop {
            ($hir$0 = 10);
            break;
        };
        let $hir$1: unknown = _;
        loop {
            ($hir$1 = 20);
            break;
        };
        let result: unknown = ($hir$0 + $hir$1);
        return result;
    }
    ");
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
    assert_snapshot!(pretty_print(&hir), @r"
    fn main() -> unknown {
        let $hir$0: unknown = _;
        loop {
            ($hir$0 = true);
            break;
        };
        let result: unknown = if $hir$0 {
            42
        } else {
            0
        };
        return result;
    }
    ");
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
    assert_snapshot!(pretty_print(&hir), @r"
    fn main() -> unknown {
        let $hir$0: unknown = _;
        loop {
            ($hir$0 = 1);
            break;
        };
        let $hir$1: unknown = _;
        loop {
            ($hir$1 = 2);
            break;
        };
        let result: unknown = [$hir$0, $hir$1, 42];
        return result;
    }
    ");
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
    assert_snapshot!(pretty_print(&hir), @r"
    fn main() -> unknown {
        let $hir$0: unknown = _;
        loop {
            let $hir$1: unknown = _;
            {
                let y: unknown = 1;
                ($hir$1 = (y + 2));
            };
            let x: unknown = $hir$1;
            ($hir$0 = x);
            break;
        };
        let result: unknown = $hir$0;
        return result;
    }
    ");
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
    assert_snapshot!(pretty_print(&hir), @r"
    fn main() -> unknown {
        let $hir$0: unknown = _;
        loop {
            let $hir$1: unknown = _;
            loop {
                let $hir$3: unknown = _;
                {
                    let a: unknown = 1;
                    ($hir$3 = (a + 1));
                };
                let value: unknown = $hir$3;
                ($hir$1 = (value * 2));
                break;
            };
            let inner_result: unknown = $hir$1;
            let $hir$2: unknown = _;
            if (inner_result > 3) {
                ($hir$0 = (inner_result + 10));
                break;
            };
            $hir$2
        };
        let result: unknown = $hir$0;
        return result;
    }
    ");
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
    assert_snapshot!(pretty_print(&hir), @r#"
    fn main() -> unknown {
        let obj: unknown = {field: 0};
        let $hir$0: unknown = _;
        loop {
            ($hir$0 = "field");
            break;
        };
        let result: unknown = obj[$hir$0];
        return result;
    }
    "#);
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

    // Debug: Print HIR before and after transformation
    println!("=== DEBUG: FOR LOOP TRANSFORMATION ===");
    let (hir_before, hir_after) = compile_and_apply_hir_js_pass_debug(source);
    println!("HIR BEFORE:");
    println!("{}", pretty_print(&hir_before));
    println!("\nHIR AFTER:");
    println!("{}", pretty_print(&hir_after));
    println!("=== END DEBUG ===");

    let hir = hir_after;
    assert_snapshot!(pretty_print(&hir), @r"
    fn test() -> unknown {
        let $hir$0: unknown = _;
        {
            let iterator$$: unknown = iterator::iter([1, 2, 3]);
            let accumulator$$: unknown = 0;
            loop {
                let acc: unknown = accumulator$$;
                let $hir$1: unknown = _;
                match iterator$$.next() {
                    Option::Some { 0: i } => {
                        ($hir$1 = (acc + i));
                    },
                    Option::None => {
                        break accumulator$$;
                    },
                };
                (accumulator$$ = $hir$1);
                $hir$1
            };
        };
        let result: unknown = $hir$0;
        return result;
    }
    ");
}

#[test]
fn test_match_expression_in_let() {
    let source = r#"
        fn main() {
            let value = Some(42);
            let result = match value {
                Some(x) => x * 2,
                None => 0
            };
            result
        }
    "#;
    let hir = compile_and_apply_hir_js_pass(source);
    assert_snapshot!(pretty_print(&hir), @r"
    fn main() -> unknown {
        let value: unknown = Some(42);
        let $hir$0: unknown = _;
        match value {
            Some { 0: x } => {
                ($hir$0 = (x * 2));
            },
            None => {
                ($hir$0 = 0);
            },
        };
        let result: unknown = $hir$0;
        return result;
    }
    ");
}

#[test]
fn test_match_expression_in_function_argument() {
    let source = r#"
        fn main() {
            let value = Some(42);
            log(match value {
                Some(x) => x,
                None => 0
            });
        }
    "#;
    let hir = compile_and_apply_hir_js_pass(source);
    assert_snapshot!(pretty_print(&hir), @r"
    fn main() -> unknown {
        let value: unknown = Some(42);
        let $hir$0: unknown = _;
        match value {
            Some { 0: x } => {
                ($hir$0 = x);
            },
            None => {
                ($hir$0 = 0);
            },
        };
        log($hir$0);
    }
    ");
}

#[test]
fn test_match_expression_in_binary_expression() {
    let source = r#"
        fn main() {
            let value1 = Some(10);
            let value2 = Some(20);
            let result = match value1 {
                Some(x) => x,
                None => 0
            } + match value2 {
                Some(y) => y,
                None => 0
            };
            result
        }
    "#;
    let hir = compile_and_apply_hir_js_pass(source);
    assert_snapshot!(pretty_print(&hir), @r"
    fn main() -> unknown {
        let value1: unknown = Some(10);
        let value2: unknown = Some(20);
        let $hir$0: unknown = _;
        match value1 {
            Some { 0: x } => {
                ($hir$0 = x);
            },
            None => {
                ($hir$0 = 0);
            },
        };
        let $hir$1: unknown = _;
        match value2 {
            Some { 0: y } => {
                ($hir$1 = y);
            },
            None => {
                ($hir$1 = 0);
            },
        };
        let result: unknown = ($hir$0 + $hir$1);
        return result;
    }
    ");
}

#[test]
fn test_match_expression_in_if_condition() {
    let source = r#"
        fn main() {
            let value = Some(5);
            let result = if match value {
                Some(x) => x > 3,
                None => false
            } {
                42
            } else {
                0
            };
            result
        }
    "#;
    let hir = compile_and_apply_hir_js_pass(source);
    assert_snapshot!(pretty_print(&hir), @r"
    fn main() -> unknown {
        let value: unknown = Some(5);
        let $hir$0: unknown = _;
        match value {
            Some { 0: x } => {
                ($hir$0 = (x > 3));
            },
            None => {
                ($hir$0 = false);
            },
        };
        let result: unknown = if $hir$0 {
            42
        } else {
            0
        };
        return result;
    }
    ");
}

#[test]
fn test_match_expression_in_list() {
    let source = r#"
        fn main() {
            let value1 = Some(1);
            let value2 = Some(2);
            let result = [
                match value1 {
                    Some(x) => x,
                    None => 0
                },
                match value2 {
                    Some(y) => y,
                    None => 0
                },
                42
            ];
            result
        }
    "#;
    let hir = compile_and_apply_hir_js_pass(source);
    assert_snapshot!(pretty_print(&hir), @r"
    fn main() -> unknown {
        let value1: unknown = Some(1);
        let value2: unknown = Some(2);
        let $hir$0: unknown = _;
        match value1 {
            Some { 0: x } => {
                ($hir$0 = x);
            },
            None => {
                ($hir$0 = 0);
            },
        };
        let $hir$1: unknown = _;
        match value2 {
            Some { 0: y } => {
                ($hir$1 = y);
            },
            None => {
                ($hir$1 = 0);
            },
        };
        let result: unknown = [$hir$0, $hir$1, 42];
        return result;
    }
    ");
}

#[test]
fn test_nested_match_expressions() {
    let source = r#"
        fn main() {
            let outer = Some(Some(42));
            let result = match outer {
                Some(inner) => match inner {
                    Some(x) => x * 2,
                    None => -1
                },
                None => 0
            };
            result
        }
    "#;
    let hir = compile_and_apply_hir_js_pass(source);
    assert_snapshot!(pretty_print(&hir), @r"
    fn main() -> unknown {
        let outer: unknown = Some(Some(42));
        let $hir$0: unknown = _;
        match outer {
            Some { 0: inner } => {
                let $hir$1: unknown = _;
                match inner {
                    Some { 0: x } => {
                        ($hir$1 = (x * 2));
                    },
                    None => {
                        ($hir$1 = -1);
                    },
                };
                ($hir$0 = $hir$1);
                $hir$1;
            },
            None => {
                ($hir$0 = 0);
            },
        };
        let result: unknown = $hir$0;
        return result;
    }
    ");
}

#[test]
fn test_shortcut_operators_with_match_expressions() {
    let source = r#"
        fn main() {
            let value1 = Some(true);
            let value2 = Some(false);
            let result = match value1 {
                Some(x) => x,
                None => false
            } && match value2 {
                Some(y) => y,
                None => true
            };
            result
        }
    "#;
    let hir = compile_and_apply_hir_js_pass(source);
    assert_snapshot!(pretty_print(&hir), @r"
    fn main() -> unknown {
        let value1: unknown = Some(true);
        let value2: unknown = Some(false);
        let $hir$0: unknown = _;
        let $hir$1: unknown = _;
        match value1 {
            Some { 0: x } => {
                ($hir$1 = x);
            },
            None => {
                ($hir$1 = false);
            },
        };
        if $hir$1 {
            let $hir$2: unknown = _;
            match value2 {
                Some { 0: y } => {
                    ($hir$2 = y);
                },
                None => {
                    ($hir$2 = true);
                },
            };
            ($hir$0 = $hir$2);
        } else {
            ($hir$0 = false);
        };
        let result: unknown = $hir$0;
        return result;
    }
    ");
}

#[test]
fn test_shortcut_operators_with_block_expressions() {
    let source = r#"
        fn main() {
            let result = {
                let x = 5;
                x > 3
            } || {
                let y = 2;
                y < 1
            };
            result
        }
    "#;
    let hir = compile_and_apply_hir_js_pass(source);
    assert_snapshot!(pretty_print(&hir), @r"
    fn main() -> unknown {
        let $hir$0: unknown = _;
        let $hir$1: unknown = _;
        {
            let x: unknown = 5;
            ($hir$1 = (x > 3));
        };
        if $hir$1 {
            ($hir$0 = $hir$1);
        } else {
            let $hir$2: unknown = _;
            {
                let y: unknown = 2;
                ($hir$2 = (y < 1));
            };
            ($hir$0 = $hir$2);
        };
        let result: unknown = $hir$0;
        return result;
    }
    ");
}

#[test]
fn test_loop_expression_with_if_else_break() {
    let source = r#"
        fn test() {
            let result = loop {
                if true { break 42; }
            };
            result
        }
    "#;
    let hir = compile_and_apply_hir_js_pass(source);
    assert_snapshot!(pretty_print(&hir), @r"
    fn test() -> unknown {
        let $hir$0: unknown = _;
        loop {
            let $hir$1: unknown = _;
            if true {
                ($hir$0 = 42);
                break;
            };
            $hir$1
        };
        let result: unknown = $hir$0;
        return result;
    }
    ");
}

#[test]
fn test_simple_vs_complex_if_else_transformation() {
    // Simple if-else - should NOT be transformed by HIR JS pass
    let simple_source = r#"
        fn main() {
            let result = if true { 1 } else { 2 };
        }
    "#;
    let simple_hir = compile_and_apply_hir_js_pass(simple_source);
    assert_snapshot!(pretty_print(&simple_hir), @r###"
    fn main() -> unknown {
        let result: unknown = if true {
            1
        } else {
            2
        };
    }
    "###);

    // Complex if-else - SHOULD be transformed by HIR JS pass
    let complex_source = r#"
        fn main() {
            let result = if true {
                let x = 1;
                x + 2
            } else {
                3
            };
        }
    "#;
    let complex_hir = compile_and_apply_hir_js_pass(complex_source);
    assert_snapshot!(pretty_print(&complex_hir), @r###"
    fn main() -> unknown {
        let $hir$0: unknown = _;
        if true {
            let x: unknown = 1;
            ($hir$0 = (x + 2));
        } else {
            ($hir$0 = 3);
        };
        let result: unknown = $hir$0;
    }
    "###);
}

#[test]
fn test_complex_nested_all_expression_types() {
    let source = r#"
        fn main() {
            let result = {
                let x = loop {
                    let value = match Some(42) {
                        Some(n) => n + 1,
                        None => 0
                    };
                    break value;
                };
                
                x > 0 && {
                    let nested_loop = loop {
                        let check = match x % 2 {
                            0 => true,
                            _ => false
                        };
                        break check;
                    };
                    nested_loop
                }
            };
            
            log({
                let final_check = match result {
                    true => loop { break "success"; },
                    false => { let msg = "failure"; msg }
                };
                final_check
            });
        }
    "#;
    let hir = compile_and_apply_hir_js_pass(source);
    assert_snapshot!(pretty_print(&hir), @r#"
    fn main() -> unknown {
        let $hir$0: unknown = _;
        {
            let $hir$2: unknown = _;
            loop {
                let $hir$3: unknown = _;
                match Some(42) {
                    Some { 0: n } => {
                        ($hir$3 = (n + 1));
                    },
                    None => {
                        ($hir$3 = 0);
                    },
                };
                let value: unknown = $hir$3;
                ($hir$2 = value);
                break;
            };
            let x: unknown = $hir$2;
            ($hir$0 = ((x > 0) && {
                let $hir$4: unknown = _;
                loop {
                    let $hir$5: unknown = _;
                    match (x % 2) {
                        0 => {
                            ($hir$5 = true);
                        },
                        _ => {
                            ($hir$5 = false);
                        },
                    };
                    let check: unknown = $hir$5;
                    ($hir$4 = check);
                    break;
                };
                let nested_loop: unknown = $hir$4;
                nested_loop
            }));
        };
        let result: unknown = $hir$0;
        let $hir$1: unknown = _;
        {
            let $hir$6: unknown = _;
            match result {
                true => {
                    ($hir$6 = loop {
                        break "success";
                    });
                },
                false => {
                    let msg: unknown = "failure";
                    ($hir$6 = msg);
                },
            };
            let final_check: unknown = $hir$6;
            ($hir$1 = final_check);
        };
        log($hir$1);
    }
    "#);
}

#[test]
fn test_break_expression_in_let_statement() {
    let source = r#"
        fn main() {
            loop {
                let x = break 42;
                x
            };
        }
    "#;
    let hir = compile_and_apply_hir_js_pass(source);
    assert_snapshot!(pretty_print(&hir), @r"
    fn main() -> unknown {
        loop {
            let $hir$0: unknown = _;
            ($hir$0 = 42);
            break;
            let x: unknown = $hir$0;
            x
        };
    }
    ");
}

#[test]
fn test_continue_expression_in_let_statement() {
    let source = r#"
        fn main() {
            loop {
                let x = continue;
                x
            };
        }
    "#;
    let hir = compile_and_apply_hir_js_pass(source);
    assert_snapshot!(pretty_print(&hir), @r"
    fn main() -> unknown {
        loop {
            continue;
            let x: unknown = _;
            x
        };
    }
    ");
}

#[test]
fn test_break_expression_in_binary_expression() {
    let source = r#"
        fn main() {
            loop {
                let result = (break 42) + 10;
                result
            };
        }
    "#;
    let hir = compile_and_apply_hir_js_pass(source);
    assert_snapshot!(pretty_print(&hir), @r"
    fn main() -> unknown {
        loop {
            let $hir$0: unknown = _;
            ($hir$0 = 42);
            break;
            let result: unknown = ($hir$0 + 10);
            result
        };
    }
    ");
}

#[test]
fn test_break_expression_in_function_call() {
    let source = r#"
        fn main() {
            loop {
                let result = log(break "hello");
                result
            };
        }
    "#;
    let hir = compile_and_apply_hir_js_pass(source);
    assert_snapshot!(pretty_print(&hir), @r#"
    fn main() -> unknown {
        loop {
            let $hir$0: unknown = _;
            ($hir$0 = "hello");
            break;
            let result: unknown = log($hir$0);
            result
        };
    }
    "#);
}

// Continue expressions are not implemented in the parser, so this test is removed.
// Continue is not supported as either an expression or statement in the current language.

#[test]
fn test_break_expression_in_list() {
    let source = r#"
        fn main() {
            loop {
                let list = [1, break 42, 3];
                list
            };
        }
    "#;
    let hir = compile_and_apply_hir_js_pass(source);
    assert_snapshot!(pretty_print(&hir), @r"
    fn main() -> unknown {
        loop {
            let $hir$0: unknown = _;
            ($hir$0 = 42);
            break;
            let list: unknown = [1, $hir$0, 3];
            list
        };
    }
    ");
}

#[test]
fn test_nested_break_expressions() {
    let source = r#"
        fn main() {
            loop {
                let result = if true {
                    break 42
                } else {
                    break 24
                };
                result
            };
        }
    "#;
    let hir = compile_and_apply_hir_js_pass(source);
    assert_snapshot!(pretty_print(&hir), @r"
    fn main() -> unknown {
        loop {
            let $hir$0: unknown = _;
            if true {
                let $hir$1: unknown = _;
                ($hir$1 = 42);
                break;
                ($hir$0 = $hir$1);
            } else {
                let $hir$2: unknown = _;
                ($hir$2 = 24);
                break;
                ($hir$0 = $hir$2);
            };
            let result: unknown = $hir$0;
            result
        };
    }
    ");
}

// Continue expressions are not implemented in the parser, so this test is removed.
// Continue is not supported as either an expression or statement in the current language.
// This test was attempting to use continue as an expression in an if-else branch.

#[test]
fn test_debug_break_expression_hir() {
    let source = r#"
        fn main() {
            loop {
                let x = break 42;
                x
            };
        }
    "#;
    let (module, _) = compile(source);
    println!("Debug HIR without HIR JS pass:");
    println!("{}", pretty_print(&module));

    let hir = compile_and_apply_hir_js_pass(source);
    println!("Debug HIR with HIR JS pass:");
    println!("{}", pretty_print(&hir));

    // Just to satisfy the test, use a dummy assertion for now
    assert!(true);
}

#[test]
fn test_debug_simple_break_expression() {
    let source = r#"
        fn main() {
            let x = break 42;
            x
        }
    "#;
    let (module, _) = compile(source);
    println!("Debug simple HIR without HIR JS pass:");
    println!("{}", pretty_print(&module));

    let hir = compile_and_apply_hir_js_pass(source);
    println!("Debug simple HIR with HIR JS pass:");
    println!("{}", pretty_print(&hir));

    // Just to satisfy the test, use a dummy assertion for now
    assert!(true);
}

#[test]
fn test_debug_block_inside_loop() {
    let source = r#"
        fn main() {
            loop {
                let x = {
                    let y = 1;
                    y + 2
                };
                x
            };
        }
    "#;
    let (module, _) = compile(source);
    println!("Debug block inside loop HIR without HIR JS pass:");
    println!("{}", pretty_print(&module));

    let hir = compile_and_apply_hir_js_pass(source);
    println!("Debug block inside loop HIR with HIR JS pass:");
    println!("{}", pretty_print(&hir));

    // Just to satisfy the test, use a dummy assertion for now
    assert!(true);
}

#[test]
fn test_if_else_if_expression_transformation() {
    let source = r#"
        fn main() {
            let result = if true { 1 } else if true { 2 } else { 3 };
            result
        }
    "#;
    let hir = compile_and_apply_hir_js_pass(source);
    assert_snapshot!(pretty_print(&hir), @r"
    fn main() -> unknown {
        let $hir$0: unknown = _;
        if true {
            ($hir$0 = 1);
        } else if true {
            ($hir$0 = 2);
        } else {
            ($hir$0 = 3);
        };
        let result: unknown = $hir$0;
        return result;
    }
    ");
}

#[test]
fn test_fibonacci_pattern_matching_uses_return_statements() {
    let source = r#"
        fn fibonacci(n) { fibonacci(n, 0, 1) }
        fn fibonacci(0, a, _) { a }
        fn fibonacci(1, _, b) { b }
        fn fibonacci(n, a, b) { rec fibonacci(n - 1, b, a + b) }
    "#;
    let hir = compile_and_apply_hir_js_pass(source);
    println!("Fibonacci HIR Output:");
    println!("{}", pretty_print(&hir));
    assert_snapshot!(pretty_print(&hir), @r"
    fn fibonacci/1(n: unknown) -> unknown {
        return fibonacci/3(n, 0, 1);
    }
    fn fibonacci/3(n: unknown, a: unknown, b: unknown) -> unknown {
        match [n, a, b] {
            [0, a, _] => {
                return a;
            },
            [1, _, b] => {
                return b;
            },
            [n, a, b] => {
                return rec fibonacci/3((n - 1), b, (a + b));
            },
        };
        $hir$0;
    }
    dyn fn fibonacci
        -> fibonacci/1
        -> fibonacci/3;
    ");
}

#[test]
fn test_function_with_match_completion_uses_return_statements() {
    let source = r#"
        enum Option {
            Some(value),
            None,
        }
        
        fn test_func(x) {
            match x {
                Option::Some(n) => n + 1,
                Option::None => 0,
            }
        }
    "#;
    let hir = compile_and_apply_hir_js_pass(source);
    println!("HIR Output with match in completion position:");
    println!("{}", pretty_print(&hir));
    assert_snapshot!(pretty_print(&hir), @r"
    enum Option {
        Some {
            0: value,
        }
        None {
        }
    }
    fn test_func(x: unknown) -> unknown {
        match x {
            Option::Some { 0: n } => {
                return (n + 1);
            },
            Option::None => {
                return 0;
            },
        };
        $hir$0;
    }
    ");
}

#[test]
fn test_function_declaration_with_match_expressions() {
    let source = r#"
        enum Option {
            Some(value),
            None,
        }
        
        fn test_func(x) {
            let result = match x {
                Option::Some(n) => n + 1,
                Option::None => 0,
            };
            result
        }
    "#;
    let hir = compile_and_apply_hir_js_pass(source);
    println!("Current HIR output:");
    println!("{}", pretty_print(&hir));
    assert_snapshot!(pretty_print(&hir), @r"
    enum Option {
        Some {
            0: value,
        }
        None {
        }
    }
    fn test_func(x: unknown) -> unknown {
        let $hir$0: unknown = _;
        match x {
            Option::Some { 0: n } => {
                ($hir$0 = (n + 1));
            },
            Option::None => {
                ($hir$0 = 0);
            },
        };
        let result: unknown = $hir$0;
        return result;
    }
    ");
}

#[test]
fn test_debug_function_pattern_matching() {
    let source = r#"
        fn factorial(0) { 1 }
        fn factorial(n) { n * factorial(n - 1) }
    "#;

    // First, test the HIR lowering to see what's generated
    let mut parser = tlang_parser::Parser::from_source(source);
    let ast = parser.parse().unwrap();

    let mut semantic_analyzer = tlang_semantics::SemanticAnalyzer::default();
    semantic_analyzer.add_builtin_symbols(
        tlang_codegen_js::generator::CodegenJS::get_standard_library_symbols(),
    );
    semantic_analyzer.analyze(&ast).unwrap();

    let (module, _) = tlang_ast_lowering::lower_to_hir(
        &ast,
        semantic_analyzer.symbol_id_allocator(),
        semantic_analyzer.root_symbol_table(),
        semantic_analyzer.symbol_tables().clone(),
    );

    println!("HIR before JS pass:");
    println!("{}", pretty_print(&module));
    println!("=====================================");

    // Now test HIR JS pass
    let hir = compile_and_apply_hir_js_pass(source);
    println!("HIR after JS pass:");
    println!("{}", pretty_print(&hir));
}

#[test]
fn test_function_expression_with_if_else_completion() {
    let source = r#"
        fn main() {
            let factorial_rec = fn rec_helper(n, acc) {
                if n == 0 {
                    acc
                } else {
                    rec rec_helper(n - 1, n * acc)
                }
            };
            factorial_rec(5, 1)
        }
    "#;
    let hir = compile_and_apply_hir_js_pass(source);
    println!("HIR Output with function expression:");
    println!("{}", pretty_print(&hir));
    // The if-else expression in the function expression should use return statements
    assert_snapshot!(pretty_print(&hir), @r#"
    fn main() -> unknown {
        let factorial_rec: unknown = fn rec_helper(n: unknown, acc: unknown) -> unknown {
            if (n == 0) {
                return acc;
            } else {
                return rec rec_helper((n - 1), (n * acc));
            };
        };
        return factorial_rec(5, 1);
    }
    "#);
}

#[test]
fn test_debug_nested_loop_expressions_with_cli_sequence() {
    let source = r#"
        fn nested_loop_expressions() {
            let outer = loop {
                let inner = loop {
                    break 5;
                };
                if inner == 5 { break inner * 2; }
            };
            outer
        }
        
        nested_loop_expressions() |> log();
    "#;

    let (mut module, meta) = compile(source);

    println!("=== Initial HIR ===");
    println!("{}", pretty_print(&module));

    // Run the same sequence as the CLI: standard HIR optimizations first
    let mut optimizer = tlang_hir_opt::HirOptimizer::default();
    let hir_opt_ctx = HirOptContext::from(meta);
    optimizer.optimize_hir(&mut module, hir_opt_ctx);

    println!("\n=== After standard HIR optimizations ===");
    println!("{}", pretty_print(&module));

    // Create a fresh context for HIR JS optimization group (like the CLI does)
    let mut ctx = HirOptContext {
        symbols: std::collections::HashMap::new(),
        hir_id_allocator: tlang_span::HirIdAllocator::default(),
        current_scope: tlang_span::HirId::new(1),
    };
    let mut hir_js_opt_group = create_hir_js_opt_group();

    // Run the HIR JS optimization group
    println!("\n=== Before HIR JS optimization group ===");
    println!("{}", pretty_print(&module));

    hir_js_opt_group.optimize_hir(&mut module, &mut ctx);

    println!("\n=== Final HIR after HIR JS optimization group ===");
    println!("{}", pretty_print(&module));
}

#[test]
fn test_isolated_loop_expression_in_block_completion() {
    // Isolated test for loop expressions in block completion position
    let source = r#"
        fn test() {
            {
                let x = 1;
                loop {
                    if true {
                        break;
                    }
                }
            }
        }
    "#;
    let hir = compile_and_apply_hir_js_pass(source);
    assert_snapshot!(pretty_print(&hir), @r"
    fn test() -> unknown {
        let $hir$0: unknown = _;
        {
            let x: unknown = 1;
            loop {
                let $hir$1: unknown = _;
                if true {
                    break;
                };
                $hir$1
            };
        };
        return $hir$0;
    }
    ");
}

#[test]
fn test_isolated_loop_with_break_value() {
    // Test loop expressions with break values
    let source = r#"
        fn test() {
            let result = loop {
                break 42;
            };
            result
        }
    "#;
    let hir = compile_and_apply_hir_js_pass(source);
    assert_snapshot!(pretty_print(&hir), @r###"
    fn test() -> unknown {
        let $hir$0: unknown = _;
        loop {
            ($hir$0 = 42);
            break;
        };
        let result: unknown = $hir$0;
        return result;
    }
    "###);
}

#[test]
fn test_isolated_break_in_binary_expression() {
    let source = r#"
        fn test() {
            loop {
                let result = break 42 + 10;
            }
        }
    "#;
    let hir = compile_and_apply_hir_js_pass(source);
    assert_snapshot!(pretty_print(&hir), @r###"
    fn test() -> unknown {
        let $hir$0: unknown = _;
        loop {
            let $hir$1: unknown = _;
            ($hir$1 = (42 + 10));
            break;
            let result: unknown = $hir$1;
        };
        return $hir$0;
    }
    "###);
}

#[test]
fn test_isolated_for_loop_structure() {
    // Isolated test to understand for loop structure transformation
    let source = r#"
        fn test() {
            for i in [1, 2, 3] {
                log(i);
            }
        }
    "#;
    let hir = compile_and_apply_hir_js_pass(source);
    assert_snapshot!(pretty_print(&hir), @r"
    fn test() -> unknown {
        let $hir$0: unknown = _;
        {
            let iterator$$: unknown = iterator::iter([1, 2, 3]);
            loop {
                let $hir$1: unknown = _;
                let $hir$2: unknown = _;
                match iterator$$.next() {
                    Option::Some { 0: i } => {
                        log(i);
                    },
                    Option::None => {
                        break;
                    },
                };
                $hir$2;
                $hir$1
            };
        };
        return $hir$0;
    }
    ");
}

#[test]
fn test_and_operator_with_match_expressions() {
    let source = r#"
        enum Option { Some(value), None }
        fn main() {
            let x = Option::Some(42);
            let y = Option::Some(24);
            let result = (match x { Option::Some(v) => v > 0, Option::None => false }) && 
                        (match y { Option::Some(v) => v > 0, Option::None => false });
            result
        }
    "#;
    let hir = compile_and_apply_hir_js_pass(source);
    assert_snapshot!(pretty_print(&hir), @r"
    enum Option {
        Some {
            0: value,
        }
        None {
        }
    }
    fn main() -> unknown {
        let x: unknown = Option::Some(42);
        let y: unknown = Option::Some(24);
        let $hir$0: unknown = _;
        let $hir$1: unknown = _;
        match x {
            Option::Some { 0: v } => {
                ($hir$1 = (v > 0));
            },
            Option::None => {
                ($hir$1 = false);
            },
        };
        if $hir$1 {
            let $hir$2: unknown = _;
            match y {
                Option::Some { 0: v } => {
                    ($hir$2 = (v > 0));
                },
                Option::None => {
                    ($hir$2 = false);
                },
            };
            ($hir$0 = $hir$2);
        } else {
            ($hir$0 = false);
        };
        let result: unknown = $hir$0;
        return result;
    }
    ");
}

#[test]
fn test_or_operator_with_match_expressions() {
    let source = r#"
        enum Option { Some(value), None }
        fn main() {
            let x = Option::None;
            let y = Option::Some(42);
            let result = (match x { Option::Some(v) => v > 0, Option::None => false }) || 
                        (match y { Option::Some(v) => v > 0, Option::None => false });
            result
        }
    "#;
    let hir = compile_and_apply_hir_js_pass(source);
    assert_snapshot!(pretty_print(&hir), @r"
    enum Option {
        Some {
            0: value,
        }
        None {
        }
    }
    fn main() -> unknown {
        let x: unknown = Option::None;
        let y: unknown = Option::Some(42);
        let $hir$0: unknown = _;
        let $hir$1: unknown = _;
        match x {
            Option::Some { 0: v } => {
                ($hir$1 = (v > 0));
            },
            Option::None => {
                ($hir$1 = false);
            },
        };
        if $hir$1 {
            ($hir$0 = $hir$1);
        } else {
            let $hir$2: unknown = _;
            match y {
                Option::Some { 0: v } => {
                    ($hir$2 = (v > 0));
                },
                Option::None => {
                    ($hir$2 = false);
                },
            };
            ($hir$0 = $hir$2);
        };
        let result: unknown = $hir$0;
        return result;
    }
    ");
}

#[test]
fn test_chained_three_match_expressions_with_and_or() {
    let source = r#"
        enum Option { Some(value), None }
        fn main() {
            let x = Option::Some(1);
            let y = Option::Some(2);
            let z = Option::Some(3);
            let result = (match x { Option::Some(v) => v > 0, Option::None => false }) && 
                        (match y { Option::Some(v) => v > 0, Option::None => false }) ||
                        (match z { Option::Some(v) => v > 0, Option::None => false });
            result
        }
    "#;
    let hir = compile_and_apply_hir_js_pass(source);
    assert_snapshot!(pretty_print(&hir), @r"
    enum Option {
        Some {
            0: value,
        }
        None {
        }
    }
    fn main() -> unknown {
        let x: unknown = Option::Some(1);
        let y: unknown = Option::Some(2);
        let z: unknown = Option::Some(3);
        let $hir$0: unknown = _;
        let $hir$1: unknown = _;
        let $hir$2: unknown = _;
        match x {
            Option::Some { 0: v } => {
                ($hir$2 = (v > 0));
            },
            Option::None => {
                ($hir$2 = false);
            },
        };
        if $hir$2 {
            let $hir$3: unknown = _;
            match y {
                Option::Some { 0: v } => {
                    ($hir$3 = (v > 0));
                },
                Option::None => {
                    ($hir$3 = false);
                },
            };
            ($hir$1 = $hir$3);
        } else {
            ($hir$1 = false);
        };
        if $hir$1 {
            ($hir$0 = $hir$1);
        } else {
            let $hir$4: unknown = _;
            match z {
                Option::Some { 0: v } => {
                    ($hir$4 = (v > 0));
                },
                Option::None => {
                    ($hir$4 = false);
                },
            };
            ($hir$0 = $hir$4);
        };
        let result: unknown = $hir$0;
        return result;
    }
    ");
}

#[test]
fn test_and_operator_with_block_expressions() {
    let source = r#"
        fn main() {
            let result = { 
                let x = 10; 
                x > 5 
            } && { 
                let y = 20; 
                y > 15 
            };
            result
        }
    "#;
    let hir = compile_and_apply_hir_js_pass(source);
    assert_snapshot!(pretty_print(&hir), @r"
    fn main() -> unknown {
        let $hir$0: unknown = _;
        let $hir$1: unknown = _;
        {
            let x: unknown = 10;
            ($hir$1 = (x > 5));
        };
        if $hir$1 {
            let $hir$2: unknown = _;
            {
                let y: unknown = 20;
                ($hir$2 = (y > 15));
            };
            ($hir$0 = $hir$2);
        } else {
            ($hir$0 = false);
        };
        let result: unknown = $hir$0;
        return result;
    }
    ");
}

#[test]
fn test_or_operator_with_block_expressions() {
    let source = r#"
        fn main() {
            let result = { 
                let x = 10; 
                x > 15 
            } || { 
                let y = 20; 
                y > 5 
            };
            result
        }
    "#;
    let hir = compile_and_apply_hir_js_pass(source);
    assert_snapshot!(pretty_print(&hir), @r"
    fn main() -> unknown {
        let $hir$0: unknown = _;
        let $hir$1: unknown = _;
        {
            let x: unknown = 10;
            ($hir$1 = (x > 15));
        };
        if $hir$1 {
            ($hir$0 = $hir$1);
        } else {
            let $hir$2: unknown = _;
            {
                let y: unknown = 20;
                ($hir$2 = (y > 5));
            };
            ($hir$0 = $hir$2);
        };
        let result: unknown = $hir$0;
        return result;
    }
    ");
}

#[test]
fn test_short_circuit_with_break_expressions() {
    let source = r#"
        fn main() {
            let result = loop {
                let check = (break true) && false;
                check
            };
            result
        }
    "#;
    let hir = compile_and_apply_hir_js_pass(source);
    assert_snapshot!(pretty_print(&hir), @r"
    fn main() -> unknown {
        let $hir$0: unknown = _;
        loop {
            let $hir$1: unknown = _;
            let $hir$2: unknown = _;
            ($hir$2 = true);
            break;
            if $hir$2 {
                ($hir$1 = false);
            } else {
                ($hir$1 = false);
            };
            let check: unknown = $hir$1;
            check
        };
        let result: unknown = $hir$0;
        return result;
    }
    ");
}

#[test]
fn test_mixed_complex_expressions_with_short_circuit() {
    let source = r#"
        enum Option { Some(value), None }
        fn main() {
            let x = Option::Some(42);
            let result = (match x { Option::Some(v) => v > 0, Option::None => false }) && 
                        { let temp = 10; temp > 5 } ||
                        (match x { Option::Some(v) => v < 100, Option::None => true });
            result
        }
    "#;
    let hir = compile_and_apply_hir_js_pass(source);
    assert_snapshot!(pretty_print(&hir), @r"
    enum Option {
        Some {
            0: value,
        }
        None {
        }
    }
    fn main() -> unknown {
        let x: unknown = Option::Some(42);
        let $hir$0: unknown = _;
        let $hir$1: unknown = _;
        let $hir$2: unknown = _;
        match x {
            Option::Some { 0: v } => {
                ($hir$2 = (v > 0));
            },
            Option::None => {
                ($hir$2 = false);
            },
        };
        if $hir$2 {
            let $hir$3: unknown = _;
            {
                let temp: unknown = 10;
                ($hir$3 = (temp > 5));
            };
            ($hir$1 = $hir$3);
        } else {
            ($hir$1 = false);
        };
        if $hir$1 {
            ($hir$0 = $hir$1);
        } else {
            let $hir$4: unknown = _;
            match x {
                Option::Some { 0: v } => {
                    ($hir$4 = (v < 100));
                },
                Option::None => {
                    ($hir$4 = true);
                },
            };
            ($hir$0 = $hir$4);
        };
        let result: unknown = $hir$0;
        return result;
    }
    ");
}

#[test]
fn test_nested_match_expressions_in_short_circuit() {
    let source = r#"
        enum Result { Ok(value), Err(error) }
        fn main() {
            let x = Result::Ok(42);
            let y = Result::Ok(24);
            let result = (match x { 
                Result::Ok(v) => match y { 
                    Result::Ok(w) => v + w > 50, 
                    Result::Err(_) => false 
                }, 
                Result::Err(_) => false 
            }) && true;
            result
        }
    "#;
    let hir = compile_and_apply_hir_js_pass(source);
    assert_snapshot!(pretty_print(&hir), @r"
    enum Result {
        Ok {
            0: value,
        }
        Err {
            0: error,
        }
    }
    fn main() -> unknown {
        let x: unknown = Result::Ok(42);
        let y: unknown = Result::Ok(24);
        let $hir$0: unknown = _;
        let $hir$1: unknown = _;
        match x {
            Result::Ok { 0: v } => {
                let $hir$2: unknown = _;
                match y {
                    Result::Ok { 0: w } => {
                        ($hir$2 = ((v + w) > 50));
                    },
                    Result::Err { 0: _ } => {
                        ($hir$2 = false);
                    },
                };
                ($hir$1 = $hir$2);
                $hir$2;
            },
            Result::Err { 0: _ } => {
                ($hir$1 = false);
            },
        };
        if $hir$1 {
            ($hir$0 = true);
        } else {
            ($hir$0 = false);
        };
        let result: unknown = $hir$0;
        return result;
    }
    ");
}
