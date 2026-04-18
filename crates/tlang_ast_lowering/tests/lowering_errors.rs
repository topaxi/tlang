use tlang_ast::node::{
    Block, Expr, ExprKind, FunctionDeclaration, FunctionParameter, Ident, Module, Pat, PatKind,
    Stmt, StmtKind, Visibility,
};
use tlang_ast::token::Literal;
use tlang_span::{NodeId, Span};

/// Build a minimal AST module containing `FunctionDeclarations` whose name
/// expression is a literal (invalid — only `Path` and `FieldExpression` are
/// accepted).  Two declarations with the same arity are needed so that
/// `lower_fn_decl_matching` is exercised.
fn module_with_invalid_fn_name() -> Module {
    let invalid_name = Expr::new(
        NodeId::new(10),
        ExprKind::Literal(Box::new(Literal::UnsignedInteger(0))),
    );
    let body = Block::new(NodeId::new(11), vec![], None);

    let param = FunctionParameter::new(Pat::new(
        NodeId::new(20),
        PatKind::Identifier(Box::new(Ident::new("x", Span::default()))),
    ));

    let decl1 = FunctionDeclaration {
        id: NodeId::new(2),
        visibility: Visibility::Private,
        name: invalid_name.clone(),
        owner_type_params: vec![],
        type_params: vec![],
        parameters: vec![param.clone()],
        params_span: Span::default(),
        guard: None,
        return_type_annotation: None,
        body: body.clone(),
        leading_comments: vec![],
        trailing_comments: vec![],
        span: Span::default(),
    };

    let decl2 = FunctionDeclaration {
        id: NodeId::new(3),
        visibility: Visibility::Private,
        name: invalid_name,
        owner_type_params: vec![],
        type_params: vec![],
        parameters: vec![param],
        params_span: Span::default(),
        guard: None,
        return_type_annotation: None,
        body,
        leading_comments: vec![],
        trailing_comments: vec![],
        span: Span::default(),
    };

    let stmt = Stmt::new(
        NodeId::new(4),
        StmtKind::FunctionDeclarations(vec![decl1, decl2]),
    );

    Module::new(NodeId::new(1), vec![stmt], Span::default())
}

#[test]
fn invalid_function_name_returns_err() {
    let module = module_with_invalid_fn_name();
    let result = tlang_ast_lowering::lower_to_hir(
        &module,
        &[],
        Default::default(),
        Default::default(),
        Default::default(),
    );

    let errors = result.expect_err("lowering should fail with InvalidFunctionName");
    assert!(!errors.is_empty(), "expected at least one lowering error");
    assert!(
        errors.iter().all(|e| matches!(
            e,
            tlang_ast_lowering::LoweringError::InvalidFunctionName { .. }
        )),
        "all errors should be InvalidFunctionName, got: {errors:?}"
    );
}
