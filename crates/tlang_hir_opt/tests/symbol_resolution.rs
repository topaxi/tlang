mod common;

use std::collections::HashMap;

use pretty_assertions::assert_eq;
use tlang_hir::hir;
use tlang_span::HirId;

use self::common::{collect_declarations, collect_paths};

fn optimizer() -> tlang_hir_opt::HirOptimizer {
    tlang_hir_opt::HirOptimizer::new(vec![Box::new(
        tlang_hir_opt::symbol_resolution::SymbolResolution::default(),
    )])
}

fn compile(source: &str) -> hir::Module {
    common::compile_and_optimize(source, &mut optimizer())
}

fn collect_res(hir: &mut hir::Module) -> Vec<(String, hir::Res)> {
    collect_paths(hir)
        .iter()
        .map(|path| (path.to_string(), path.res))
        .collect()
}

#[test]
fn test_simple_symbol_resolution() {
    let mut hir = compile(
        r#"
            fn main() {
                let x = 42;
                x + x
            }
        "#,
    );

    assert_eq!(
        collect_declarations(&mut hir),
        HashMap::from([
            (HirId::new(3), "main".to_string()),
            (HirId::new(7), "x".to_string()),
        ])
    );

    assert_eq!(
        collect_res(&mut hir),
        vec![
            ("main".to_string(), hir::Res::new_fn(HirId::new(3))),
            ("x".to_string(), hir::Res::new_local(HirId::new(7))),
            ("x".to_string(), hir::Res::new_local(HirId::new(7))),
        ]
    );
}

#[test]
fn test_symbol_resolution_with_same_scope_shadowing() {
    let mut hir = compile(
        r#"
            fn main() {
                let x = 42;
                let x = x + 23;
                x + x
            }
        "#,
    );

    assert_eq!(
        collect_declarations(&mut hir),
        HashMap::from([
            (HirId::new(3), "main".to_string()),
            (HirId::new(7), "x".to_string()),
            (HirId::new(12), "x".to_string()),
        ])
    );

    assert_eq!(
        collect_res(&mut hir),
        vec![
            ("main".to_string(), hir::Res::new_fn(HirId::new(3))),
            ("x".to_string(), hir::Res::new_local(HirId::new(7))),
            ("x".to_string(), hir::Res::new_local(HirId::new(12))),
            ("x".to_string(), hir::Res::new_local(HirId::new(12))),
        ]
    );
}

#[test]
fn test_enum_resolution() {
    let mut hir = compile(
        r#"
            enum Color {
                Red,
                Green,
                Blue,
            }

            fn main() {
                let c = Color::Red;
                c
            }
        "#,
    );

    assert_eq!(
        collect_declarations(&mut hir),
        HashMap::from([
            (HirId::new(3), "Color".to_string()),
            (HirId::new(4), "Color::Red".to_string()),
            (HirId::new(5), "Color::Green".to_string()),
            (HirId::new(6), "Color::Blue".to_string()),
            (HirId::new(7), "main".to_string()),
            (HirId::new(11), "c".to_string()),
        ])
    );

    assert_eq!(
        collect_res(&mut hir),
        vec![
            ("main".to_string(), hir::Res::new_fn(HirId::new(7))),
            (
                "Color::Red".to_string(),
                hir::Res::new_enum_variant(HirId::new(4))
            ),
            ("c".to_string(), hir::Res::new_local(HirId::new(11))),
        ]
    );
}
