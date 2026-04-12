use insta::assert_snapshot;
use tlang_hir_opt::anf_transform::{AnfTransform, FullAnfFilter};
use tlang_hir_opt::tail_call_validation::TailPositionAnalysis;

mod common;

fn optimizer() -> tlang_hir_opt::HirOptimizer {
    tlang_hir_opt::HirOptimizer::new(vec![
        // SymbolResolution must run before ANF so that callee paths have
        // their `res.hir_id()` set — the ANF pass uses HirId identity to
        // detect self-referencing tail calls.
        Box::new(tlang_hir_opt::symbol_resolution::SymbolResolution::default()),
        Box::new(AnfTransform::<FullAnfFilter>::default()),
    ])
}

#[test]
fn simple_let_binding() {
    let source = r#"let x = 1 + 2;"#;
    let hir = common::compile_and_optimize(source, &mut optimizer());
    assert_snapshot!(common::pretty_print(&hir), @"let x: unknown = (1 + 2);");
}

#[test]
fn let_with_literal() {
    let source = r#"let x = 42;"#;
    let hir = common::compile_and_optimize(source, &mut optimizer());
    assert_snapshot!(common::pretty_print(&hir), @"let x: unknown = 42;");
}

#[test]
fn if_else_in_let() {
    let source = r#"
        let x = if true { 1 } else { 2 };
    "#;
    let hir = common::compile_and_optimize(source, &mut optimizer());
    assert_snapshot!(common::pretty_print(&hir), @"
    let $anf$0: unknown = nil;
    if true {
        ($anf$0 = 1);
    } else {
        ($anf$0 = 2);
    };
    let x: unknown = $anf$0;
    ");
}

#[test]
fn match_in_let() {
    let source = r#"
        let y = 1;
        let x = match y {
            1 => 10,
            _ => 20,
        };
    "#;
    let hir = common::compile_and_optimize(source, &mut optimizer());
    assert_snapshot!(common::pretty_print(&hir), @"
    let y: unknown = 1;
    let $anf$0: unknown = nil;
    match y {
        1 => {
            ($anf$0 = 10);
        },
        _ => {
            ($anf$0 = 20);
        },
    };
    let x: unknown = $anf$0;
    ");
}

#[test]
fn nested_call_arguments() {
    let source = r#"
        fn add(a, b) { a + b }
        add(1 + 2, 3 + 4);
    "#;
    let hir = common::compile_and_optimize(source, &mut optimizer());
    assert_snapshot!(common::pretty_print(&hir), @"
    fn add(a: unknown, b: unknown) -> unknown {
        (a + b)
    }
    let $anf$0: unknown = (1 + 2);
    let $anf$1: unknown = (3 + 4);
    add($anf$0, $anf$1);
    ");
}

#[test]
fn binary_with_call() {
    let source = r#"
        fn foo() { 1 }
        let x = foo() + foo();
    "#;
    let hir = common::compile_and_optimize(source, &mut optimizer());
    assert_snapshot!(common::pretty_print(&hir), @"
    fn foo() -> unknown {
        1
    }
    let $anf$0: unknown = foo();
    let $anf$1: unknown = foo();
    let x: unknown = ($anf$0 + $anf$1);
    ");
}

#[test]
fn statement_level_if_not_lifted() {
    let source = r#"
        if true {
            1;
        } else {
            2;
        }
    "#;
    let hir = common::compile_and_optimize(source, &mut optimizer());
    assert_snapshot!(common::pretty_print(&hir), @"
    if true {
        1;
    } else {
        2;
    };
    ");
}

#[test]
fn function_expression_body_transformed() {
    let source = r#"
        let f = fn(x) { x + 1 };
    "#;
    let hir = common::compile_and_optimize(source, &mut optimizer());
    assert_snapshot!(common::pretty_print(&hir), @"
    let f: unknown = fn anonymous(x: unknown) -> unknown {
        (x + 1)
    };
    ");
}

#[test]
fn if_else_in_call_argument() {
    let source = r#"
        fn foo(x) { x }
        foo(if true { 1 } else { 2 });
    "#;
    let hir = common::compile_and_optimize(source, &mut optimizer());
    assert_snapshot!(common::pretty_print(&hir), @"
    fn foo(x: unknown) -> unknown {
        x
    }
    let $anf$0: unknown = nil;
    if true {
        ($anf$0 = 1);
    } else {
        ($anf$0 = 2);
    };
    foo($anf$0);
    ");
}

#[test]
fn nested_if_else_completions() {
    let source = r#"
        let x = if true {
            if false { 1 } else { 2 }
        } else {
            3
        };
    "#;
    let hir = common::compile_and_optimize(source, &mut optimizer());
    assert_snapshot!(common::pretty_print(&hir), @"
    let $anf$1: unknown = nil;
    if true {
        let $anf$0: unknown = nil;
        if false {
            ($anf$0 = 1);
        } else {
            ($anf$0 = 2);
        };
        ($anf$1 = $anf$0);
    } else {
        ($anf$1 = 3);
    };
    let x: unknown = $anf$1;
    ");
}

#[test]
fn list_with_compound_elements() {
    let source = r#"
        fn foo() { 1 }
        let xs = [foo(), foo()];
    "#;
    let hir = common::compile_and_optimize(source, &mut optimizer());
    assert_snapshot!(common::pretty_print(&hir), @"
    fn foo() -> unknown {
        1
    }
    let $anf$0: unknown = foo();
    let $anf$1: unknown = foo();
    let xs: unknown = [$anf$0, $anf$1];
    ");
}

#[test]
fn field_access_on_call() {
    let source = r#"
        struct Point { x: int, y: int }
        fn make_point() { Point { x: 1, y: 2 } }
        let x = make_point().x;
    "#;
    let hir = common::compile_and_optimize(source, &mut optimizer());
    assert_snapshot!(common::pretty_print(&hir));
}

#[test]
fn return_with_compound_expr() {
    let source = r#"
        fn foo() {
            return 1 + 2;
        }
    "#;
    let hir = common::compile_and_optimize(source, &mut optimizer());
    assert_snapshot!(common::pretty_print(&hir), @"
    fn foo() -> unknown {
        return (1 + 2);
    }
    ");
}

#[test]
fn multi_arm_function_with_guard() {
    // Regression: the match scrutinee lifted by FullAnfFilter was being lost
    // when fold_block cleared self.pending while processing arm bodies.
    // After the fix the let-binding for the list appears before the match.
    let source = r#"
        fn search(_, _, low, high) if low > high { -1 }
        fn search(list, target, low, high) { low + high }
    "#;
    let hir = common::compile_and_optimize(source, &mut optimizer());
    assert_snapshot!(common::pretty_print(&hir));
}

/// Optimizer that includes TailPositionAnalysis so that `rec` calls become
/// `TailCall` nodes before ANF runs.
fn optimizer_with_tail_calls() -> tlang_hir_opt::HirOptimizer {
    tlang_hir_opt::HirOptimizer::new(vec![
        Box::new(TailPositionAnalysis::default()),
        Box::new(tlang_hir_opt::symbol_resolution::SymbolResolution::default()),
        Box::new(AnfTransform::<FullAnfFilter>::default()),
    ])
}

#[test]
fn self_referencing_tail_call_not_lifted_in_multi_arity_function() {
    // Regression: when the ANF pass ran before SymbolResolution,
    // `is_self_referencing_tail_call` always returned false (path.res.hir_id()
    // was None), causing self-referencing TailCalls to be ANF-lifted into
    // $anf$ temps instead of being preserved as TailCall nodes. This broke
    // TCO for all multi-arity functions.
    let source = r#"
        fn binary_search(list, target) { binary_search(list, target, 0, 1) }
        fn binary_search(_, _, low, high) if low > high { -1 }
        fn binary_search(list, target, low, high) {
            let mid = low + high;
            if list == target {
                mid
            } else if list == target {
                rec binary_search(list, target, mid, high)
            } else {
                rec binary_search(list, target, low, mid)
            }
        }
    "#;
    let hir = common::compile_and_optimize(source, &mut optimizer_with_tail_calls());
    assert_snapshot!(common::pretty_print(&hir));
}

#[test]
fn multi_arm_function_guard_with_pattern_bound_var() {
    // Regression: guard referencing a pattern-bound variable (not a function
    // parameter) must NOT be lifted to before the match.
    let source = r#"
        fn filter([], _) { [] }
        fn filter([x, ...xs], f) if f(x) { [x] }
        fn filter([_, ...xs], f) { [] }
    "#;
    let hir = common::compile_and_optimize(source, &mut optimizer());
    assert_snapshot!(common::pretty_print(&hir));
}
