#![feature(box_patterns)]
use tlang_ast as ast;
use tlang_ast::node::{BinaryOpExpression, FunctionDeclaration, LetDeclaration};
use tlang_hir::hir;

struct LoweringContext {}

impl LoweringContext {
    pub fn new() -> Self {
        Self {}
    }

    fn lower_module(&mut self, module: &ast::node::Module) -> hir::Module {
        hir::Module {
            block: self.lower_block(&module.statements, &None, module.span),
            span: module.span,
        }
    }

    fn lower_block(
        &mut self,
        stmts: &[ast::node::Stmt],
        expr: &Option<ast::node::Expr>,
        span: ast::span::Span,
    ) -> hir::Block {
        let stmts = stmts.iter().map(|stmt| self.lower_stmt(stmt)).collect();
        let expr = expr.as_ref().map(|expr| self.lower_expr(expr));

        hir::Block { stmts, expr, span }
    }

    fn lower_expr(&mut self, node: &ast::node::Expr) -> hir::Expr {
        match &node.kind {
            ast::node::ExprKind::BinaryOp(box BinaryOpExpression { op, lhs, rhs }) => hir::Expr {
                kind: hir::ExprKind::Binary(
                    *op,
                    Box::new(self.lower_expr(lhs)),
                    Box::new(self.lower_expr(rhs)),
                ),
                span: node.span,
            },
            ast::node::ExprKind::Block(box ast::node::Block {
                statements,
                expression,
                span,
                ..
            }) => {
                let kind =
                    hir::ExprKind::Block(Box::new(self.lower_block(statements, expression, *span)));

                self.expr(*span, kind)
            }
            ast::node::ExprKind::None => {
                unreachable!("ExprKind::None should not be encountered, validate AST first")
            }
            _ => todo!(),
        }
    }

    fn expr(&mut self, span: ast::span::Span, kind: hir::ExprKind) -> hir::Expr {
        hir::Expr { kind, span }
    }

    fn lower_stmt(&mut self, node: &ast::node::Stmt) -> hir::Stmt {
        match &node.kind {
            ast::node::StmtKind::Expr(expr) => hir::Stmt {
                kind: hir::StmtKind::Expr(Box::new(self.lower_expr(expr))),
                span: node.span,
            },
            ast::node::StmtKind::Let(box LetDeclaration {
                pattern,
                expression,
                type_annotation,
            }) => hir::Stmt {
                kind: hir::StmtKind::Let(
                    self.lower_pat(pattern),
                    self.lower_expr(expression),
                    self.lower_ty(type_annotation),
                ),
                span: node.span,
            },
            ast::node::StmtKind::FunctionDeclaration(decl) => {
                let decl = self.lower_fn_decl(decl);

                hir::Stmt {
                    kind: hir::StmtKind::FunctionDeclaration(Box::new(decl)),
                    span: node.span,
                }
            }
            ast::node::StmtKind::None => {
                unreachable!("StmtKind::None should not be encountered, validate AST first")
            }
            _ => todo!(),
        }
    }

    fn lower_fn_param(&mut self, node: &ast::node::FunctionParameter) -> hir::FunctionParameter {
        hir::FunctionParameter {
            pattern: self.lower_pat(&node.pattern),
            type_annotation: self.lower_ty(&node.type_annotation),
            span: node.span,
        }
    }

    fn lower_fn_decl(&mut self, decl: &FunctionDeclaration) -> hir::FunctionDeclaration {
        let name = self.lower_expr(&decl.name);
        let parameters = decl
            .parameters
            .iter()
            .map(|param| self.lower_fn_param(param))
            .collect();
        let body = self.lower_block(&decl.body.statements, &decl.body.expression, decl.body.span);
        let return_type = self.lower_ty(&decl.return_type_annotation);

        hir::FunctionDeclaration {
            name,
            parameters,
            return_type,
            body,
            span: decl.span,
        }
    }

    fn lower_pat(&mut self, node: &ast::node::Pattern) -> hir::Pat {
        match node.kind {
            ast::node::PatternKind::Wildcard => hir::Pat {
                kind: hir::PatKind::Wildcard,
                span: node.span,
            },
            _ => todo!(),
        }
    }

    fn lower_ty(&mut self, _node: &Option<ast::node::Ty>) -> hir::Ty {
        hir::Ty {
            kind: hir::TyKind::Unknown,
            span: ast::span::Span::default(),
        }
    }
}

pub fn lower_to_hir(tlang_ast: ast::node::Module) -> hir::Module {
    let mut ctx = LoweringContext::new();
    ctx.lower_module(&tlang_ast)
}
