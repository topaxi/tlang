use insta::assert_snapshot;

mod common;
use common::compile_and_optimize;

#[test]
fn simple_binary_constant_folding() {
    let hir = compile_and_optimize(
        r#"
        let x: unknown = 1 + 2;
        println(x);
        "#,
    );
    assert_snapshot!(common::pretty_print(&hir), @r###"
    fn main() -> unknown {
        let x: unknown = 3;
        println(3);
    };
    "###);
}

#[test]
fn multiplication_constant_folding() {
    let hir = compile_and_optimize(
        r#"
        let x: unknown = 2 * 3;
        println(x);
        "#,
    );
    assert_snapshot!(common::pretty_print(&hir), @r###"
    fn main() -> unknown {
        let x: unknown = 6;
        println(6);
    };
    "###);
}

#[test]
fn chained_operations() {
    let hir = compile_and_optimize(
        r#"
        let x: unknown = 1 + 2;
        let y: unknown = x * 3;
        let z: unknown = y - 4;
        println(z);
        "#,
    );
    assert_snapshot!(common::pretty_print(&hir), @r###"
    fn main() -> unknown {
        let x: unknown = 3;
        let y: unknown = 9;
        let z: unknown = 5;
        println(5);
    };
    "###);
}

#[test]
fn mixed_operations() {
    let hir = compile_and_optimize(
        r#"
        let x: unknown = (1 + 2) * 3;
        println(x);
        "#,
    );
    assert_snapshot!(common::pretty_print(&hir), @r###"
    fn main() -> unknown {
        let x: unknown = 9;
        println(9);
    };
    "###);
}

#[test]
fn chained_constant_folding() {
    let hir = compile_and_optimize(
        r#"
        let x: unknown = 1 + 2;
        let y: unknown = x * 3;
        let z: unknown = y - 4;
        let w: unknown = z + 5;
        println(w);
        "#,
    );
    assert_snapshot!(common::pretty_print(&hir), @r###"
    fn main() -> unknown {
        let x: unknown = 3;
        let y: unknown = 9;
        let z: unknown = 5;
        let w: unknown = 10;
        println(10);
    };
    "###);
}
