use insta::assert_snapshot;
use tlang_hir_pretty::HirPretty;

mod common;

#[test]
fn simple_binary_constant_folding() {
    let hir = common::compile_and_optimize(
        r#"
        fn main() {
            let x = 1 + 2;
            println(x);
        }
        "#,
    );

    let mut prettier = HirPretty::default();
    prettier.print_module(&hir);

    assert_snapshot!(prettier.output(), @r###"
    fn main() -> unknown {
        let x: unknown = 3;
        println?(3);
    };
    "###);
}

#[test]
#[ignore]
fn chained_constant_folding() {
    let hir = common::compile_and_optimize(
        r#"
        fn main() {
            let x = 1 + 2;
            let y = x * 3;
            println(y);
        }
        "#,
    );

    let mut prettier = HirPretty::default();
    prettier.print_module(&hir);

    assert_snapshot!(prettier.output(), @r###"
    fn main() -> unknown {
        println?(9);
    };
    "###);
}
