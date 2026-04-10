#[macro_use]
mod common;

use tlang_hir::{self as hir, TyKind};

#[test]
fn test_generic_fn_type_param_resolved_to_var() {
    let module = common::hir_from_str("fn identity<T>(x: T) -> T { x }");

    let stmt = &module.block.stmts[0];
    let decl = match &stmt.kind {
        hir::StmtKind::FunctionDeclaration(decl) => decl,
        other => panic!("expected FunctionDeclaration, got {other:?}"),
    };

    // One type parameter: T.
    assert_eq!(decl.type_params.len(), 1);
    let tp = &decl.type_params[0];
    assert_eq!(tp.name.as_str(), "T");
    let tv_id = tp.type_var_id;

    // The parameter `x: T` should have its type annotation lowered to Var(tv_id).
    let param_ty = &decl.parameters[0].type_annotation;
    assert_eq!(param_ty.kind, TyKind::Var(tv_id));

    // The return type `-> T` should also be Var(tv_id).
    assert_eq!(decl.return_type.kind, TyKind::Var(tv_id));
}

#[test]
fn test_generic_fn_multiple_type_params() {
    let module = common::hir_from_str("fn pair<A, B>(a: A, b: B) -> A { a }");

    let stmt = &module.block.stmts[0];
    let decl = match &stmt.kind {
        hir::StmtKind::FunctionDeclaration(decl) => decl,
        other => panic!("expected FunctionDeclaration, got {other:?}"),
    };

    assert_eq!(decl.type_params.len(), 2);
    let tv_a = decl.type_params[0].type_var_id;
    let tv_b = decl.type_params[1].type_var_id;
    assert_ne!(tv_a, tv_b); // distinct type variables

    assert_eq!(decl.parameters[0].type_annotation.kind, TyKind::Var(tv_a));
    assert_eq!(decl.parameters[1].type_annotation.kind, TyKind::Var(tv_b));
    assert_eq!(decl.return_type.kind, TyKind::Var(tv_a));
}

#[test]
fn test_generic_struct_fields_resolved_to_var() {
    let module = common::hir_from_str(
        "struct Pair<T, U> {
            first: T,
            second: U,
        }",
    );

    let stmt = &module.block.stmts[0];
    let decl = match &stmt.kind {
        hir::StmtKind::StructDeclaration(decl) => decl,
        other => panic!("expected StructDeclaration, got {other:?}"),
    };

    assert_eq!(decl.type_params.len(), 2);
    let tv_t = decl.type_params[0].type_var_id;
    let tv_u = decl.type_params[1].type_var_id;

    assert_eq!(decl.fields[0].ty.kind, TyKind::Var(tv_t));
    assert_eq!(decl.fields[1].ty.kind, TyKind::Var(tv_u));
}

#[test]
fn test_generic_enum_variants_resolved_to_var() {
    let module = common::hir_from_str(
        "enum Result<T, E> {
            Ok(T),
            Err(E),
        }",
    );

    let stmt = &module.block.stmts[0];
    let decl = match &stmt.kind {
        hir::StmtKind::EnumDeclaration(decl) => decl,
        other => panic!("expected EnumDeclaration, got {other:?}"),
    };

    assert_eq!(decl.type_params.len(), 2);
    let tv_t = decl.type_params[0].type_var_id;
    let tv_e = decl.type_params[1].type_var_id;

    assert_eq!(decl.variants[0].parameters[0].ty.kind, TyKind::Var(tv_t));
    assert_eq!(decl.variants[1].parameters[0].ty.kind, TyKind::Var(tv_e));
}

#[test]
fn test_generic_protocol_method_resolved_to_var() {
    let module = common::hir_from_str(
        "protocol Into<T> {
            fn into(self) -> T
        }",
    );

    let stmt = &module.block.stmts[0];
    let decl = match &stmt.kind {
        hir::StmtKind::ProtocolDeclaration(decl) => decl,
        other => panic!("expected ProtocolDeclaration, got {other:?}"),
    };

    assert_eq!(decl.type_params.len(), 1);
    let tv_t = decl.type_params[0].type_var_id;

    // Method return type should be Var(tv_t).
    assert_eq!(decl.methods[0].return_type.kind, TyKind::Var(tv_t));
}

#[test]
fn test_non_generic_fn_type_unchanged() {
    // A function without type params should not have Var types.
    let module = common::hir_from_str("fn add(a: i64, b: i64) -> i64 { a }");

    let stmt = &module.block.stmts[0];
    let decl = match &stmt.kind {
        hir::StmtKind::FunctionDeclaration(decl) => decl,
        other => panic!("expected FunctionDeclaration, got {other:?}"),
    };

    assert!(decl.type_params.is_empty());
    assert_eq!(
        decl.parameters[0].type_annotation.kind,
        TyKind::Primitive(hir::PrimTy::I64)
    );
    assert_eq!(decl.return_type.kind, TyKind::Primitive(hir::PrimTy::I64));
}
