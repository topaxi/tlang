use std::{cell::RefCell, collections::HashMap, rc::Rc};

use tlang_ast::{
    node::{Expr, ExprKind, Module, Stmt, StmtKind},
    symbols::{SymbolIdAllocator, SymbolTable},
};
use tlang_ast_lowering::{lower_to_hir, lower_to_hir_with_visitor};
use tlang_span::{NodeId, Span};

#[test] 
fn test_visitor_lowering_produces_same_result_as_manual_lowering() {
    // Create a simple AST for testing
    let module = Module {
        id: NodeId::new(1),
        statements: vec![
            Stmt {
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
            }
        ],
        span: Span::default(),
    };

    // Create symbol table setup using Default like the existing tests
    let symbol_id_allocator = SymbolIdAllocator::default();
    let root_symbol_table = Default::default();
    let symbol_tables = Default::default();

    // Lower using the original method
    let (original_module, _original_meta) = lower_to_hir(
        &module,
        symbol_id_allocator,
        root_symbol_table,
        symbol_tables,
    );

    // Lower using the visitor method
    let symbol_id_allocator = SymbolIdAllocator::default();
    let root_symbol_table = Default::default();
    let symbol_tables = Default::default();
    let (visitor_module, _visitor_meta) = lower_to_hir_with_visitor(
        &module,
        symbol_id_allocator,
        root_symbol_table,
        symbol_tables,
    );

    // Verify both produce modules with the same structure
    assert_eq!(original_module.block.stmts.len(), visitor_module.block.stmts.len());
    assert_eq!(original_module.span, visitor_module.span);
    
    // Both should have processed the same number of statements
    if !original_module.block.stmts.is_empty() && !visitor_module.block.stmts.is_empty() {
        let original_stmt = &original_module.block.stmts[0];
        let visitor_stmt = &visitor_module.block.stmts[0];
        
        // Both should be expression statements
        match (&original_stmt.kind, &visitor_stmt.kind) {
            (tlang_hir::hir::StmtKind::Expr(original_expr), tlang_hir::hir::StmtKind::Expr(visitor_expr)) => {
                // Both should be literal expressions
                match (&original_expr.kind, &visitor_expr.kind) {
                    (tlang_hir::hir::ExprKind::Literal(original_lit), tlang_hir::hir::ExprKind::Literal(visitor_lit)) => {
                        assert_eq!(original_lit, visitor_lit);
                    },
                    _ => panic!("Expected both to be literal expressions"),
                }
            },
            _ => panic!("Expected both to be expression statements"),
        }
    }
}