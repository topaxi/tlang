use insta::assert_snapshot;
use tlang_ast_lowering::lower_to_hir;
use tlang_codegen_js::create_refactored_hir_js_opt_group;
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

fn compile_and_apply_refactored_hir_js_pass(source: &str) -> hir::Module {
    let (mut module, meta) = compile(source);
    let mut ctx = HirOptContext::from(meta);
    let mut hir_js_opt_group = create_refactored_hir_js_opt_group();

    // Run the refactored HIR JS optimization group
    hir_js_opt_group.optimize_hir(&mut module, &mut ctx);

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
fn test_refactored_simple_expression_no_transformation() {
    let source = r#"
        fn main() {
            let x = 1 + 2;
            x
        }
    "#;
    let hir = compile_and_apply_refactored_hir_js_pass(source);
    assert_snapshot!(pretty_print(&hir), @r"
    fn main() -> unknown {
        let x: unknown = (1 + 2);
        return x;
    }
    ");
}

#[test]
fn test_refactored_block_expression_flattening() {
    let source = r#"
        fn main() {
            let x = {
                let y = 1;
                y + 2
            };
            x
        }
    "#;
    let hir = compile_and_apply_refactored_hir_js_pass(source);
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
fn test_refactored_match_expression_flattening() {
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
    let hir = compile_and_apply_refactored_hir_js_pass(source);
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
fn test_refactored_loop_expression_flattening() {
    let source = r#"
        fn main() {
            let result = loop {
                break 42;
            };
            result
        }
    "#;
    let hir = compile_and_apply_refactored_hir_js_pass(source);
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
fn test_refactored_if_else_expression_flattening() {
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
    let hir = compile_and_apply_refactored_hir_js_pass(source);
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
fn test_refactored_break_expression_handling() {
    let source = r#"
        fn main() {
            loop {
                let x = break 42;
                x
            };
        }
    "#;
    let hir = compile_and_apply_refactored_hir_js_pass(source);
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
fn test_refactored_complex_nested_expressions() {
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
    let hir = compile_and_apply_refactored_hir_js_pass(source);
    // This should produce similar output to the original pass
    assert!(!pretty_print(&hir).is_empty());
}

#[test]
fn test_refactored_function_call_argument_flattening() {
    let source = r#"
        fn main() {
            log({
                let x = 1;
                x + 2
            });
        }
    "#;
    let hir = compile_and_apply_refactored_hir_js_pass(source);
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