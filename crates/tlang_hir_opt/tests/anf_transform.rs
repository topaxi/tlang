use insta::assert_snapshot;
use tlang_hir_opt::anf_transform::{AnfTransform, FullAnfFilter};

mod common;

fn optimizer() -> tlang_hir_opt::HirOptimizer {
    tlang_hir_opt::HirOptimizer::new(vec![
        Box::new(AnfTransform::<FullAnfFilter>::default()),
        Box::new(tlang_hir_opt::symbol_resolution::SymbolResolution::default()),
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
    let __anf_0: unknown = nil;
    if true {
        (__anf_0 = 1);
    } else {
        (__anf_0 = 2);
    };
    let x: unknown = __anf_0;
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
    let __anf_0: unknown = nil;
    match y {
        1 => {
            (__anf_0 = 10);
        },
        _ => {
            (__anf_0 = 20);
        },
    };
    let x: unknown = __anf_0;
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
    let __anf_0: unknown = (1 + 2);
    let __anf_1: unknown = (3 + 4);
    add(__anf_0, __anf_1);
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
    let __anf_0: unknown = foo();
    let __anf_1: unknown = foo();
    let x: unknown = (__anf_0 + __anf_1);
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
    let __anf_0: unknown = nil;
    if true {
        (__anf_0 = 1);
    } else {
        (__anf_0 = 2);
    };
    foo(__anf_0);
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
    let __anf_1: unknown = nil;
    if true {
        let __anf_0: unknown = nil;
        if false {
            (__anf_0 = 1);
        } else {
            (__anf_0 = 2);
        };
        (__anf_1 = __anf_0);
    } else {
        (__anf_1 = 3);
    };
    let x: unknown = __anf_1;
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
    let __anf_0: unknown = foo();
    let __anf_1: unknown = foo();
    let xs: unknown = [__anf_0, __anf_1];
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
