#![feature(box_patterns)]
use tlang_ast as ast;
use tlang_ast::node::{
    BinaryOpExpression, EnumPattern, FunctionDeclaration, Ident, IdentifierPattern, LetDeclaration,
};
use tlang_ast::token::kw;
use tlang_hir::hir::{self, HirId};

struct LoweringContext {
    unique_id: HirId,
    symbol_id_to_hir_id: std::collections::HashMap<ast::symbols::SymbolId, HirId>,
}

impl LoweringContext {
    pub fn new() -> Self {
        Self {
            unique_id: HirId::new(0),
            symbol_id_to_hir_id: std::collections::HashMap::default(),
        }
    }

    fn lower_symbol_id(&mut self, id: &ast::symbols::SymbolId) -> HirId {
        if let Some(hir_id) = self.symbol_id_to_hir_id.get(id) {
            *hir_id
        } else {
            let hir_id = self.unique_id;
            self.unique_id = self.unique_id.next();
            self.symbol_id_to_hir_id.insert(*id, hir_id);
            hir_id
        }
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
                symbol_table: _,
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
            hir_id: self.lower_symbol_id(&decl.id),
            name,
            parameters,
            return_type,
            body,
            span: decl.span,
        }
    }

    fn lower_path(&mut self, path: &ast::node::Path) -> hir::Path {
        let segments = path
            .segments
            .iter()
            .map(|seg| self.lower_path_segment(seg))
            .collect();

        hir::Path {
            segments,
            span: path.span,
        }
    }

    fn lower_path_segment(&mut self, seg: &Ident) -> hir::PathSegment {
        hir::PathSegment { ident: seg.clone() }
    }

    fn lower_pat(&mut self, node: &ast::node::Pattern) -> hir::Pat {
        match &node.kind {
            ast::node::PatternKind::Wildcard => hir::Pat {
                kind: hir::PatKind::Wildcard,
                span: node.span,
            },
            ast::node::PatternKind::Literal(box literal) => hir::Pat {
                kind: hir::PatKind::Literal(Box::new(literal.clone())),
                span: node.span,
            },
            ast::node::PatternKind::Identifier(box IdentifierPattern { id, name }) => hir::Pat {
                kind: hir::PatKind::Identifier(self.lower_symbol_id(id), Box::new(name.clone())),
                span: node.span,
            },
            ast::node::PatternKind::List(patterns) => hir::Pat {
                kind: hir::PatKind::List(patterns.iter().map(|pat| self.lower_pat(pat)).collect()),
                span: node.span,
            },
            ast::node::PatternKind::Rest(pattern) => hir::Pat {
                kind: hir::PatKind::Rest(Box::new(self.lower_pat(pattern))),
                span: node.span,
            },
            ast::node::PatternKind::Enum(box EnumPattern {
                // TODO: We lower the identifier to a path, in the AST this is an Expr, but it is
                //       always a path, we should update the AST to reflect this.
                identifier,
                elements,
                named_fields: _, // In HIR, we no longer care whether it's a named field or not
            }) => {
                let path = if let ast::node::ExprKind::Path(path) = &identifier.kind {
                    path
                } else {
                    unreachable!("EnumPattern identifier should be a path, validate AST first")
                };

                let path = self.lower_path(path);
                let elements = elements.iter().map(|pat| self.lower_pat(pat)).collect();

                hir::Pat {
                    kind: hir::PatKind::Enum(Box::new(path), elements),
                    span: node.span,
                }
            }
            ast::node::PatternKind::_Self(id) => hir::Pat {
                kind: hir::PatKind::Identifier(
                    self.lower_symbol_id(id),
                    Box::new(Ident::new(kw::_Self, node.span)),
                ),
                span: node.span,
            },
            ast::node::PatternKind::None => {
                unreachable!("PatternKind::None should not be encountered, validate AST first")
            }
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
