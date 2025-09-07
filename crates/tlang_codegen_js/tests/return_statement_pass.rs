use insta::assert_snapshot;
use tlang_ast_lowering::lower_to_hir;
use tlang_codegen_js::ReturnStatementPass;
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

fn compile_and_apply_return_statement_pass(source: &str) -> hir::Module {
    let (mut module, meta) = compile(source);
    let mut ctx = HirOptContext::from(meta);
    let mut pass = ReturnStatementPass::new();

    // Run the return statement pass
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
fn test_simple_function_completion_becomes_return() {
    let source = r#"
        fn test() {
            42
        }
    "#;
    let hir = compile_and_apply_return_statement_pass(source);
    assert_snapshot!(pretty_print(&hir), @r"
    fn test() -> unknown {
        return 42;
    }
    ");
}

#[test]
fn test_function_with_if_else_completion() {
    let source = r#"
        fn test(x) {
            if x > 0 {
                "positive"
            } else {
                "non-positive"
            }
        }
    "#;
    let hir = compile_and_apply_return_statement_pass(source);
    assert_snapshot!(pretty_print(&hir), @r###"
    fn test(x: unknown) -> unknown {
        return if (x > 0) {
            "positive"
        } else {
            "non-positive"
        };
    }
    "###);
}

#[test]
fn test_function_with_match_completion() {
    // Match expressions in function completion position should have return statements in each arm
    let source = r#"
        enum Option {
            Some(value),
            None,
        }
        
        fn test(opt) {
            match opt {
                Option::Some(val) => val + 1,
                Option::None => 0,
            }
        }
    "#;
    let hir = compile_and_apply_return_statement_pass(source);
    assert_snapshot!(pretty_print(&hir), @r###"
    enum Option {
        Some {
            0: value,
        }
        None {
        }
    }
    fn test(opt: unknown) -> unknown {
        match opt {
            Option::Some { 0: val } => {
                return (val + 1);
            },
            Option::None => {
                return 0;
            },
        };
    }
    "###);
}

#[test]
fn test_function_with_tail_call_completion() {
    let source = r#"
        fn factorial(n, acc) {
            if n <= 1 {
                acc
            } else {
                rec factorial(n - 1, n * acc)
            }
        }
    "#;
    let hir = compile_and_apply_return_statement_pass(source);
    assert_snapshot!(pretty_print(&hir), @r###"
    fn factorial(n: unknown, acc: unknown) -> unknown {
        if (n <= 1) {
            return acc;
        } else {
            return rec factorial((n - 1), (n * acc));
        };
    }
    "###);
}

#[test]
fn test_function_with_block_completion() {
    let source = r#"
        fn test() {
            {
                let x = 42;
                x + 1
            }
        }
    "#;
    let hir = compile_and_apply_return_statement_pass(source);
    assert_snapshot!(pretty_print(&hir), @r###"
    fn test() -> unknown {
        return {
            let x: unknown = 42;
            (x + 1)
        };
    }
    "###);
}

#[test]
fn test_function_expression_completion() {
    let source = r#"
        let f = fn(x) {
            x * 2
        };
    "#;
    let hir = compile_and_apply_return_statement_pass(source);
    assert_snapshot!(pretty_print(&hir), @r###"
    let f: unknown = fn anonymous(x: unknown) -> unknown {
        (x * 2)
    };
    "###);
}

#[test]
fn test_nested_functions_isolated() {
    let source = r#"
        fn outer() {
            let inner = fn(x) {
                x + 1
            };
            inner(42)
        }
    "#;
    let hir = compile_and_apply_return_statement_pass(source);
    assert_snapshot!(pretty_print(&hir), @r###"
    fn outer() -> unknown {
        let inner: unknown = fn anonymous(x: unknown) -> unknown {
            (x + 1)
        };
        return inner(42);
    }
    "###);
}

#[test]
fn test_non_function_contexts_unchanged() {
    let source = r#"
        let x = if true { 1 } else { 2 };
        let y = {
            let z = 3;
            z + 1
        };
    "#;
    let hir = compile_and_apply_return_statement_pass(source);
    assert_snapshot!(pretty_print(&hir), @r###"
    let x: unknown = if true {
        1
    } else {
        2
    };
    let y: unknown = {
        let z: unknown = 3;
        (z + 1)
    };
    "###);
}

#[test]
fn test_loop_expressions_in_function_not_affected() {
    let source = r#"
        fn test() {
            let result = loop {
                if true {
                    break 42;
                }
            };
            result
        }
    "#;
    let hir = compile_and_apply_return_statement_pass(source);
    assert_snapshot!(pretty_print(&hir), @r###"
    fn test() -> unknown {
        let result: unknown = loop {
            if true {
                break 42;
            }
        };
        return result;
    }
    "###);
}

#[test]
fn test_multiple_functions_isolated() {
    // This test demonstrates that the ReturnStatementPass correctly transforms
    // simple completion expressions but currently does not transform match
    // expressions to use return statements in each arm.
    let source = r#"
        fn first() {
            "first"
        }
        
        fn second() {
            if true {
                "second"
            } else {
                "alt"
            }
        }
        
        fn third() {
            match true {
                true => "match_true",
                false => "match_false",
            }
        }
    "#;
    let hir = compile_and_apply_return_statement_pass(source);
    assert_snapshot!(pretty_print(&hir), @r###"
    fn first() -> unknown {
        return "first";
    }
    fn second() -> unknown {
        return if true {
            "second"
        } else {
            "alt"
        };
    }
    fn third() -> unknown {
        match true {
            true => {
                return "match_true";
            },
            false => {
                return "match_false";
            },
        };
    }
    "###);
}

#[test]
fn test_function_with_statements_and_completion() {
    let source = r#"
        fn test() {
            let x = 10;
            let y = 20;
            x + y
        }
    "#;
    let hir = compile_and_apply_return_statement_pass(source);
    assert_snapshot!(pretty_print(&hir), @r###"
    fn test() -> unknown {
        let x: unknown = 10;
        let y: unknown = 20;
        return (x + y);
    }
    "###);
}

#[test]
fn test_nested_match_expressions() {
    // Nested match expressions should have return statements in their arms
    let source = r#"
        enum Option {
            Some(value),
            None,
        }
        
        enum Result {
            Ok(value),
            Err(msg),
        }
        
        fn test(opt) {
            match opt {
                Option::Some(val) => match val {
                    Result::Ok(x) => x * 2,
                    Result::Err(_) => -1,
                },
                Option::None => 0,
            }
        }
    "#;
    let hir = compile_and_apply_return_statement_pass(source);
    assert_snapshot!(pretty_print(&hir), @r###"
    enum Option {
        Some {
            0: value,
        }
        None {
        }
    }
    enum Result {
        Ok {
            0: value,
        }
        Err {
            0: msg,
        }
    }
    fn test(opt: unknown) -> unknown {
        match opt {
            Option::Some { 0: val } => {
                return match val {
                    Result::Ok { 0: x } => {
                        return (x * 2);
                    },
                    Result::Err { 0: _ } => {
                        return -1;
                    },
                };
            },
            Option::None => {
                return 0;
            },
        };
    }
    "###);
}

#[test]
fn test_function_with_match_and_complex_arms() {
    // Match expressions with complex arms (containing statements) should still use return statements
    let source = r#"
        enum Option {
            Some(value),
            None,
        }
        
        fn test(opt) {
            match opt {
                Option::Some(val) => {
                    let doubled = val * 2;
                    doubled + 1
                },
                Option::None => 0,
            }
        }
    "#;
    let hir = compile_and_apply_return_statement_pass(source);
    assert_snapshot!(pretty_print(&hir), @r###"
    enum Option {
        Some {
            0: value,
        }
        None {
        }
    }
    fn test(opt: unknown) -> unknown {
        match opt {
            Option::Some { 0: val } => {
                let doubled: unknown = (val * 2);
                return (doubled + 1);
            },
            Option::None => {
                return 0;
            },
        };
    }
    "###);
}

#[test]
fn test_isolated_tail_call_in_if_else() {
    // Isolated test for tail calls in if-else expressions - the core fix
    let source = r#"
        fn factorial(n, acc) {
            if n <= 1 {
                acc
            } else {
                rec factorial(n - 1, n * acc)
            }
        }
    "#;
    let hir = compile_and_apply_return_statement_pass(source);
    assert_snapshot!(pretty_print(&hir), @r###"
    fn factorial(n: unknown, acc: unknown) -> unknown {
        if (n <= 1) {
            return acc;
        } else {
            return rec factorial((n - 1), (n * acc));
        };
    }
    "###);
}

#[test]
fn test_isolated_simple_if_else_without_tail_calls() {
    // Ensure simple if-else without tail calls still work correctly
    let source = r#"
        fn test(x) {
            if x > 0 {
                "positive"
            } else {
                "negative"
            }
        }
    "#;
    let hir = compile_and_apply_return_statement_pass(source);
    assert_snapshot!(pretty_print(&hir), @r###"
    fn test(x: unknown) -> unknown {
        return if (x > 0) {
            "positive"
        } else {
            "negative"
        };
    }
    "###);
}

#[test]
fn test_isolated_nested_tail_calls() {
    // Test nested tail calls in complex if-else structures
    let source = r#"
        fn nested_recursive(x, y) {
            if x > 0 {
                if y > 0 {
                    rec nested_recursive(x - 1, y)
                } else {
                    rec nested_recursive(x, y + 1)
                }
            } else {
                x + y
            }
        }
    "#;
    let hir = compile_and_apply_return_statement_pass(source);
    assert_snapshot!(pretty_print(&hir), @r###"
    fn nested_recursive(x: unknown, y: unknown) -> unknown {
        if (x > 0) {
            return if (y > 0) {
                rec nested_recursive((x - 1), y)
            } else {
                rec nested_recursive(x, (y + 1))
            };
        } else {
            return (x + y);
        };
    }
    "###);
}
