use insta::assert_snapshot;
use tlang_hir_pretty::HirPretty;
use tlang_hir_opt::constant_folding::ConstantFolder;
use tlang_hir_opt::hir_opt::HirOptimizer;

mod common;

#[test]
fn simple_binary_constant_folding() {
    let source = r#"
        let x = 1 + 2;
        println?(x);
    "#;

    let mut hir = tlang_hir::hir_from_source(source).unwrap();
    let mut optimizer = HirOptimizer::new();
    optimizer.add_pass(Box::new(ConstantFolder::new()));
    optimizer.optimize_module(&mut hir);

    let formatted = tlang_hir::hir_to_string(&hir);
    insta::assert_snapshot!(formatted, @"println?(3);");
}

#[test]
fn multiplication_constant_folding() {
    let source = r#"
        let x = 2 * 3;
        println?(x);
    "#;

    let mut hir = tlang_hir::hir_from_source(source).unwrap();
    let mut optimizer = HirOptimizer::new();
    optimizer.add_pass(Box::new(ConstantFolder::new()));
    optimizer.optimize_module(&mut hir);

    let formatted = tlang_hir::hir_to_string(&hir);
    insta::assert_snapshot!(formatted, @"println?(6);");
}

#[test]
fn chained_operations() {
    let source = r#"
        let x = 1 + 2;
        let y = x * 3;
        let z = y - 4;
        println?(z);
    "#;

    let mut hir = tlang_hir::hir_from_source(source).unwrap();
    let mut optimizer = HirOptimizer::new();
    optimizer.add_pass(Box::new(ConstantFolder::new()));
    optimizer.optimize_module(&mut hir);

    let formatted = tlang_hir::hir_to_string(&hir);
    insta::assert_snapshot!(formatted, @"println?(5);");
}

#[test]
fn mixed_operations() {
    let source = r#"
        let x = (1 + 2) * (3 - 1);
        println?(x);
    "#;

    let mut hir = tlang_hir::hir_from_source(source).unwrap();
    let mut optimizer = HirOptimizer::new();
    optimizer.add_pass(Box::new(ConstantFolder::new()));
    optimizer.optimize_module(&mut hir);

    let formatted = tlang_hir::hir_to_string(&hir);
    insta::assert_snapshot!(formatted, @"println?(6);");
}

#[test]
#[ignore]
fn chained_constant_folding() {
    let source = r#"
        let x = 1 + 2;
        let y = x * 3;
        let z = y + 4;
        println?(z);
    "#;

    let mut hir = tlang_hir::hir_from_source(source).unwrap();
    let mut optimizer = HirOptimizer::new();
    optimizer.add_pass(Box::new(ConstantFolder::new()));
    optimizer.optimize_module(&mut hir);

    let formatted = tlang_hir::hir_to_string(&hir);
    insta::assert_snapshot!(formatted, @"println?(13);");
}
