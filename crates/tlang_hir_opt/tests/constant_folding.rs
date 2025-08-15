use insta::assert_snapshot;

mod common;

fn optimizer() -> tlang_hir_opt::HirOptimizer {
    tlang_hir_opt::HirOptimizer::new(vec![
        Box::new(tlang_hir_opt::symbol_resolution::SymbolResolution::default()),
        Box::new(tlang_hir_opt::constant_folding::ConstantFolding::default()),
    ])
}

#[test]
fn simple_binary_constant_folding() {
    let source = r#"
        let x: unknown = 1 + 2;
        println(x);
    "#;
    let hir = common::compile_and_optimize(source, &mut optimizer());
    assert_snapshot!(common::pretty_print(&hir), @r###"
    let x: unknown = 3;
    println(3);
    "###);
}

#[test]
fn multiplication_constant_folding() {
    let source = r#"
        let x: unknown = 2 * 3;
        println(x);
    "#;
    let hir = common::compile_and_optimize(source, &mut optimizer());
    assert_snapshot!(common::pretty_print(&hir), @r###"
    let x: unknown = 6;
    println(6);
    "###);
}

#[test]
fn chained_operations() {
    let source = r#"
        let x: unknown = 1 + 2;
        let y: unknown = x * 3;
        let z: unknown = y - 4;
        println(z);
    "#;
    let hir = common::compile_and_optimize(source, &mut optimizer());
    assert_snapshot!(common::pretty_print(&hir), @r###"
    let x: unknown = 3;
    let y: unknown = 9;
    let z: unknown = 5;
    println(5);
    "###);
}

#[test]
fn mixed_operations() {
    let source = r#"
        let x: unknown = (1 + 2) * 3;
        println(x);
    "#;
    let hir = common::compile_and_optimize(source, &mut optimizer());
    assert_snapshot!(common::pretty_print(&hir), @r###"
    let x: unknown = 9;
    println(9);
    "###);
}

#[test]
fn chained_constant_folding() {
    let source = r#"
        let x: unknown = 1 + 2;
        let y: unknown = x * 3;
        println(y);
    "#;
    let hir = common::compile_and_optimize(source, &mut optimizer());
    assert_snapshot!(common::pretty_print(&hir), @r###"
    let x: unknown = 3;
    let y: unknown = 9;
    println(9);
    "###);
}

#[test]
fn constant_folding_mutable_variable() {
    let source = r#"
        fn loop_test() {
            let i = 0;
            loop {
                if i >= 10 {
                    break;
                }

                i = i + 1;
            }
            i
        }
        loop_test() |> println();
    "#;

    let hir = common::compile_and_optimize(source, &mut optimizer());
    assert_snapshot!(common::pretty_print(&hir), @r###"
    fn loop_test() -> unknown {
        let i: unknown = 0;
        loop {
            if (i >= 10) {
                break;
            };
            (i = (i + 1));
        };
        i
    };
    println(loop_test());
    "###);
}

#[test]
fn constant_folding_booleans() {
    let source = r#"
        let a: unknown = true && false;
        let b: unknown = true || false;
        let c: unknown = !true;
        println(a, b, c);
    "#;
    let hir = common::compile_and_optimize(source, &mut optimizer());
    assert_snapshot!(common::pretty_print(&hir), @r###"
    let a: unknown = false;
    let b: unknown = true;
    let c: unknown = false;
    println(false, true, false);
    "###);
}
