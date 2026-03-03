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
    }
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

#[test]
fn integer_arithmetic_folding() {
    let source = r#"
        let a: unknown = 10 - 3;
        let b: unknown = 4 * 5;
        let c: unknown = 10 + 7;
        println(a, b, c);
    "#;
    let hir = common::compile_and_optimize(source, &mut optimizer());
    assert_snapshot!(common::pretty_print(&hir), @r###"
    let a: unknown = 7;
    let b: unknown = 20;
    let c: unknown = 17;
    println(7, 20, 17);
    "###);
}

#[test]
fn unsigned_integer_division_folding() {
    let source = r#"
        let a: unknown = 10 / 2;
        let b: unknown = 10 % 3;
        println(a, b);
    "#;
    let hir = common::compile_and_optimize(source, &mut optimizer());
    assert_snapshot!(common::pretty_print(&hir), @r###"
    let a: unknown = 5;
    let b: unknown = 1;
    println(5, 1);
    "###);
}

#[test]
fn exponentiation_folding() {
    let source = r#"
        let a: unknown = 2 ** 10;
        println(a);
    "#;
    let hir = common::compile_and_optimize(source, &mut optimizer());
    assert_snapshot!(common::pretty_print(&hir), @r###"
    let a: unknown = 1024;
    println(1024);
    "###);
}

#[test]
fn comparison_eq_folding() {
    let source = r#"
        let a: unknown = 1 == 1;
        let b: unknown = 1 == 2;
        println(a, b);
    "#;
    let hir = common::compile_and_optimize(source, &mut optimizer());
    assert_snapshot!(common::pretty_print(&hir), @r###"
    let a: unknown = true;
    let b: unknown = false;
    println(true, false);
    "###);
}

#[test]
fn comparison_ordering_folding() {
    let source = r#"
        let a: unknown = 1 < 2;
        let b: unknown = 2 > 1;
        let c: unknown = 2 <= 2;
        let d: unknown = 3 >= 4;
        println(a, b, c, d);
    "#;
    let hir = common::compile_and_optimize(source, &mut optimizer());
    assert_snapshot!(common::pretty_print(&hir), @r###"
    let a: unknown = true;
    let b: unknown = true;
    let c: unknown = true;
    let d: unknown = false;
    println(true, true, true, false);
    "###);
}

#[test]
fn comparison_neq_folding() {
    let source = r#"
        let a: unknown = 1 != 1;
        let b: unknown = 1 != 2;
        println(a, b);
    "#;
    let hir = common::compile_and_optimize(source, &mut optimizer());
    assert_snapshot!(common::pretty_print(&hir), @r###"
    let a: unknown = false;
    let b: unknown = true;
    println(false, true);
    "###);
}

#[test]
fn boolean_eq_folding() {
    let source = r#"
        let a: unknown = true == true;
        let b: unknown = true == false;
        println(a, b);
    "#;
    let hir = common::compile_and_optimize(source, &mut optimizer());
    assert_snapshot!(common::pretty_print(&hir), @r###"
    let a: unknown = true;
    let b: unknown = false;
    println(true, false);
    "###);
}

#[test]
fn bitwise_and_folding() {
    let source = r#"
        let a: unknown = 6 & 3;
        println(a);
    "#;
    let hir = common::compile_and_optimize(source, &mut optimizer());
    assert_snapshot!(common::pretty_print(&hir), @r###"
    let a: unknown = 2;
    println(2);
    "###);
}

#[test]
fn bitwise_or_folding() {
    let source = r#"
        let a: unknown = 4 | 2;
        println(a);
    "#;
    let hir = common::compile_and_optimize(source, &mut optimizer());
    assert_snapshot!(common::pretty_print(&hir), @r###"
    let a: unknown = 6;
    println(6);
    "###);
}

#[test]
fn bitwise_xor_folding() {
    let source = r#"
        let a: unknown = 5 ^ 3;
        println(a);
    "#;
    let hir = common::compile_and_optimize(source, &mut optimizer());
    assert_snapshot!(common::pretty_print(&hir), @r###"
    let a: unknown = 6;
    println(6);
    "###);
}

#[test]
fn unary_minus_folding() {
    let source = r#"
        let x: unknown = 5;
        let y: unknown = -x;
        println(y);
    "#;
    let hir = common::compile_and_optimize(source, &mut optimizer());
    assert_snapshot!(common::pretty_print(&hir), @r###"
    let x: unknown = 5;
    let y: unknown = -5;
    println(-5);
    "###);
}

#[test]
fn float_arithmetic_folding() {
    let source = r#"
        let a: unknown = 1.5 + 2.5;
        let b: unknown = 3.0 * 2.0;
        println(a, b);
    "#;
    let hir = common::compile_and_optimize(source, &mut optimizer());
    assert_snapshot!(common::pretty_print(&hir), @r###"
    let a: unknown = 4;
    let b: unknown = 6;
    println(4, 6);
    "###);
}
