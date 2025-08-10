use tlang_ast as ast;
use tlang_ast::node::Ident;
use tlang_hir::hir;

use crate::LoweringContext;

impl LoweringContext {
    pub(crate) fn lower_loop(&mut self, loop_expr: &ast::node::Block) -> hir::ExprKind {
        let ast::node::Block {
            id: _,
            statements,
            expression,
            span,
        } = loop_expr;

        hir::ExprKind::Loop(Box::new(self.lower_block(
            statements,
            expression.as_ref(),
            *span,
        )))
    }

    pub(crate) fn lower_for_loop(&mut self, for_loop: &ast::node::ForLoop) -> hir::ExprKind {
        let block = self.with_new_scope(|this| {
            let iterator_binding_name = Ident::new("iterator$$", Default::default());
            let hir_id = this.unique_id();
            let iterator_binding_pat = hir::Pat {
                kind: hir::PatKind::Identifier(hir_id, Box::new(iterator_binding_name.clone())),
                span: Default::default(),
            };
            this.scope().def_local(iterator_binding_name.as_str());

            let accumulator_binding_name = Ident::new("accumulator$$", Default::default());
            let accumulator_initializer = if let Some((_pat, expr)) = &for_loop.acc {
                let hir_id = this.unique_id();
                this.scope().def_local(accumulator_binding_name.as_str());
                let accumulator_declaration = hir::Stmt::new(
                    hir_id,
                    hir::StmtKind::Let(
                        Box::new(hir::Pat {
                            kind: hir::PatKind::Identifier(
                                hir_id,
                                Box::new(accumulator_binding_name.clone()),
                            ),
                            span: Default::default(),
                        }),
                        Box::new(this.lower_expr(expr)),
                        Default::default(),
                    ),
                    Default::default(),
                );

                Some(accumulator_declaration)
            } else {
                None
            };

            let hir_id = this.unique_id();
            let iter_expr = this.lower_expr(&for_loop.iter);
            let iterator_binding_call = this.expr(
                for_loop.iter.span,
                hir::ExprKind::FieldAccess(
                    Box::new(iter_expr),
                    Ident::new("iter", Default::default()),
                ),
            );
            let iterator_binding_value = this.expr(
                for_loop.iter.span,
                hir::ExprKind::Call(Box::new(hir::CallExpression {
                    hir_id,
                    callee: iterator_binding_call,
                    arguments: vec![],
                })),
            );

            let loop_block = this.with_new_scope(|this| {
                this.lower_for_loop_body(for_loop, iterator_binding_name, &accumulator_binding_name)
            });

            let loop_expr = this.expr(
                for_loop.block.span,
                hir::ExprKind::Loop(Box::new(loop_block)),
            );

            let iterator_binding_stmt = hir::Stmt::new(
                this.unique_id(),
                hir::StmtKind::Let(
                    Box::new(iterator_binding_pat),
                    Box::new(iterator_binding_value),
                    Default::default(),
                ),
                Default::default(),
            );

            let mut init_loop_block = hir::Block::new(
                vec![iterator_binding_stmt],
                Some(loop_expr),
                for_loop.iter.span,
            );

            if let Some(stmt) = accumulator_initializer {
                init_loop_block.stmts.push(stmt);
            }

            init_loop_block
        });

        hir::ExprKind::Block(Box::new(block))
    }

    fn lower_for_loop_body(
        &mut self,
        for_loop: &ast::node::ForLoop,
        iterator_binding_name: Ident,
        accumulator_binding_name: &Ident,
    ) -> hir::Block {
        let iterator_binding = self.expr(
            Default::default(),
            hir::ExprKind::Path(Box::new(hir::Path::new(
                vec![hir::PathSegment::new(iterator_binding_name)],
                Default::default(),
            ))),
        );

        let iterator_next_field = self.expr(
            Default::default(),
            hir::ExprKind::FieldAccess(
                Box::new(iterator_binding),
                Ident::new("next", Default::default()),
            ),
        );

        let hir_id = self.unique_id();
        let call_next = self.expr(
            Default::default(),
            hir::ExprKind::Call(Box::new(hir::CallExpression {
                hir_id,
                callee: iterator_next_field,
                arguments: vec![],
            })),
        );

        let accumulator_path_expr = self.expr(
            Default::default(),
            hir::ExprKind::Path(Box::new(hir::Path::new(
                vec![hir::PathSegment::new(accumulator_binding_name.clone())],
                Default::default(),
            ))),
        );

        let loop_statements = if let Some((pat, _)) = &for_loop.acc {
            let hir_id = self.unique_id();
            let accumulator_ressignment = hir::Stmt::new(
                hir_id,
                hir::StmtKind::Let(
                    Box::new(self.lower_pat(pat)),
                    Box::new(accumulator_path_expr.clone()),
                    Box::default(),
                ),
                Default::default(),
            );
            vec![accumulator_ressignment]
        } else {
            vec![]
        };

        let loop_arm = self.with_new_scope(|this| {
            let for_loop_pat = this.lower_pat(&for_loop.pat);

            let loop_body = this.lower_block_in_current_scope(
                &for_loop.block.statements,
                for_loop.block.expression.as_ref(),
                for_loop.block.span,
            );

            hir::MatchArm {
                pat: hir::Pat {
                    kind: hir::PatKind::Enum(
                        Box::new(hir::Path::new(
                            vec![
                                hir::PathSegment::new(Ident::new("Option", Default::default())),
                                hir::PathSegment::new(Ident::new("Some", Default::default())),
                            ],
                            Default::default(),
                        )),
                        vec![(Ident::new("0", Default::default()), for_loop_pat)],
                    ),
                    span: Default::default(),
                },
                guard: None,
                block: loop_body,
                leading_comments: vec![],
                trailing_comments: vec![],
            }
        });

        let break_arm = self.with_new_scope(|this| {
            let accumulator_path_expr = for_loop.acc.as_ref().map(|_| {
                Box::new(this.expr(
                    Default::default(),
                    hir::ExprKind::Path(Box::new(hir::Path::new(
                        vec![hir::PathSegment::new(accumulator_binding_name.clone())],
                        Default::default(),
                    ))),
                ))
            });
            hir::MatchArm {
                pat: hir::Pat {
                    kind: hir::PatKind::Enum(
                        Box::new(hir::Path::new(
                            vec![
                                hir::PathSegment::new(Ident::new("Option", Default::default())),
                                hir::PathSegment::new(Ident::new("None", Default::default())),
                            ],
                            Default::default(),
                        )),
                        vec![],
                    ),
                    span: Default::default(),
                },
                guard: None,
                block: hir::Block::new(
                    vec![],
                    Some(this.expr(
                        Default::default(),
                        hir::ExprKind::Break(accumulator_path_expr),
                    )),
                    Default::default(),
                ),
                leading_comments: vec![],
                trailing_comments: vec![],
            }
        });

        let match_expr = self.expr(
            Default::default(),
            hir::ExprKind::Match(Box::new(call_next), vec![loop_arm, break_arm]),
        );

        let match_expr = if for_loop.acc.is_some() {
            self.expr(
                Default::default(),
                hir::ExprKind::Binary(
                    hir::BinaryOpKind::Assign,
                    Box::new(accumulator_path_expr.clone()),
                    Box::new(match_expr),
                ),
            )
        } else {
            match_expr
        };

        hir::Block::new(loop_statements, Some(match_expr), Default::default())
    }
}
