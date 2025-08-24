use tlang_ast::{
    node::{Expr, ExprKind, Module, Stmt, StmtKind},
    symbols::SymbolIdAllocator,
};
use tlang_ast_lowering::lower_to_hir;
use tlang_span::{NodeId, Span};

#[test]
fn test_visitor_lowering_works_correctly() {
    // Create a simple AST for testing
    let module = Module {
        id: NodeId::new(1),
        statements: vec![Stmt {
            id: NodeId::new(2),
            kind: StmtKind::Expr(Box::new(Expr {
                id: NodeId::new(3),
                kind: ExprKind::Literal(Box::new(tlang_ast::token::Literal::Integer(42))),
                span: Span::default(),
                leading_comments: vec![],
                trailing_comments: vec![],
            })),
            span: Span::default(),
            leading_comments: vec![],
            trailing_comments: vec![],
        }],
        span: Span::default(),
    };

    // Create symbol table setup using Default like the existing tests
    let symbol_id_allocator = SymbolIdAllocator::default();
    let root_symbol_table = Default::default();
    let symbol_tables = Default::default();

    // Lower using the visitor-based implementation
    let (lowered_module, _meta) = lower_to_hir(
        &module,
        symbol_id_allocator,
        root_symbol_table,
        symbol_tables,
    );

    // Verify the module was lowered correctly
    assert_eq!(lowered_module.block.stmts.len(), 1);
    assert_eq!(lowered_module.span, module.span);

    // Verify the statement was processed correctly
    let lowered_stmt = &lowered_module.block.stmts[0];

    // Should be an expression statement
    match &lowered_stmt.kind {
        tlang_hir::hir::StmtKind::Expr(lowered_expr) => {
            // Should be a literal expression
            match &lowered_expr.kind {
                tlang_hir::hir::ExprKind::Literal(lowered_lit) => {
                    assert_eq!(**lowered_lit, tlang_ast::token::Literal::Integer(42));
                }
                _ => panic!("Expected literal expression, found {:?}", lowered_expr.kind),
            }
        }
        _ => panic!(
            "Expected expression statement, found {:?}",
            lowered_stmt.kind
        ),
    }
}
