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
    fn main() -> unknown {
        println(3);
    };
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
    fn main() -> unknown {
        println(6);
    };
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
    fn main() -> unknown {
        println(5);
    };
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
    fn main() -> unknown {
        println(9);
    };
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
    fn main() -> unknown {
        println(9);
    };
    "###);
}
