mod common;

use log::debug;
use tlang_ast::span::Span;
use tlang_ast::token::Literal;
use tlang_ast::node::Ident;
use tlang_hir::hir::{Block, Expr, ExprKind, HirId, Module, Pat, PatKind, Path, Res, Stmt, StmtKind, Ty, TyKind};
use tlang_hir_opt::path_resolution::PathResolver;
use tlang_hir_opt::HirPass;

fn create_module(stmts: Vec<Stmt>) -> Module {
    Module {
        block: Block::new(stmts, None, Span::default()),
        span: Span::default(),
    }
}

fn create_stmt_let(hir_id: HirId, pat_hir_id: HirId, expr: Expr) -> Stmt {
    Stmt {
        kind: StmtKind::Let(
            Box::new(Pat {
                kind: PatKind::Identifier(pat_hir_id, Box::new(Ident::new("x", Span::default()))),
                span: Span::default(),
            }),
            Box::new(expr),
            Box::new(Ty {
                kind: TyKind::Unknown,
                span: Span::default(),
            }),
        ),
        span: Span::default(),
        hir_id,
        leading_comments: Vec::new(),
        trailing_comments: Vec::new(),
    }
}

fn create_stmt_expr(hir_id: HirId, expr: Expr) -> Stmt {
    Stmt {
        kind: StmtKind::Expr(Box::new(expr)),
        span: Span::default(),
        hir_id,
        leading_comments: Vec::new(),
        trailing_comments: Vec::new(),
    }
}

fn create_expr_literal(hir_id: HirId, value: i64) -> Expr {
    Expr {
        kind: ExprKind::Literal(Box::new(Literal::Integer(value))),
        span: Span::default(),
        hir_id,
    }
}

fn create_expr_path(hir_id: HirId, res: Res) -> Expr {
    Expr {
        kind: ExprKind::Path(Box::new(Path {
            res,
            segments: Vec::new(),
            span: Span::default(),
        })),
        span: Span::default(),
        hir_id,
    }
}

fn create_expr_block(hir_id: HirId, stmts: Vec<Stmt>) -> Expr {
    Expr {
        kind: ExprKind::Block(Box::new(Block::new(stmts, None, Span::default()))),
        span: Span::default(),
        hir_id,
    }
}

#[test]
fn test_upvar_resolution() {
    let mut module = create_module(vec![
        create_stmt_let(HirId::new(10), HirId::new(1), create_expr_literal(HirId::new(11), 1)),
        create_stmt_expr(
            HirId::new(20),
            create_expr_block(
                HirId::new(21),
                vec![
                    create_stmt_let(HirId::new(30), HirId::new(2), create_expr_literal(HirId::new(31), 2)),
                    create_stmt_expr(HirId::new(40), create_expr_path(HirId::new(41), Res::Local(HirId::new(1), 0))),
                ],
            ),
        ),
    ]);

    let mut resolver = PathResolver::new();
    resolver.optimize_module(&mut module);

    // Check that x is resolved as an upvar in the inner scope
    if let StmtKind::Expr(expr) = &module.block.stmts[1].kind {
        if let ExprKind::Block(block) = &expr.kind {
            if let StmtKind::Expr(expr) = &block.stmts[1].kind {
                if let ExprKind::Path(path) = &expr.kind {
                    assert_eq!(path.res, Res::Upvar(0, 0));
                } else {
                    panic!("Expected path expression");
                }
            } else {
                panic!("Expected expression statement");
            }
        } else {
            panic!("Expected block expression");
        }
    } else {
        panic!("Expected expression statement");
    }
}

#[test]
fn test_local_resolution() {
    let mut module = create_module(vec![
        create_stmt_let(HirId::new(10), HirId::new(1), create_expr_literal(HirId::new(11), 1)),
        create_stmt_expr(HirId::new(20), create_expr_path(HirId::new(21), Res::Local(HirId::new(1), 0))),
    ]);

    let mut resolver = PathResolver::new();
    resolver.optimize_module(&mut module);

    // Check that x is resolved as a local variable
    if let StmtKind::Expr(expr) = &module.block.stmts[1].kind {
        if let ExprKind::Path(path) = &expr.kind {
            assert_eq!(path.res, Res::Local(HirId::new(1), 0));
        } else {
            panic!("Expected path expression");
        }
    } else {
        panic!("Expected expression statement");
    }
} 