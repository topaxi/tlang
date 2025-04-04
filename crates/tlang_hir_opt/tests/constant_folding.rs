use env_logger;
use insta::assert_snapshot;

mod common;

#[test]
fn simple_binary_constant_folding() {
    let source = r#"
        let x: unknown = 1 + 2;
        println(x);
    "#;
    let hir = common::compile_and_optimize(source);
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
    let hir = common::compile_and_optimize(source);
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
    let hir = common::compile_and_optimize(source);
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
    let hir = common::compile_and_optimize(source);
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
    let hir = common::compile_and_optimize(source);
    assert_snapshot!(common::pretty_print(&hir), @r###"
    let x: unknown = 3;
    let y: unknown = 9;
    println(9);
    "###);
}

#[test]
fn constant_folding_mutable_variable() {
    let _ = env_logger::builder().is_test(true).try_init();

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
        loop_test() |> log();
    "#;

    let hir = common::compile_and_optimize(source);
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
    log(loop_test());
    "###);
}
