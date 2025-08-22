use tlang_ast as ast;
use tlang_ast::node::Ident;
use tlang_ast::symbols::SymbolType;
use tlang_hir::hir;
use tlang_span::NodeId;

use crate::LoweringContext;

impl LoweringContext {
    pub(crate) fn lower_loop(&mut self, loop_expr: &ast::node::Block) -> hir::ExprKind {
        hir::ExprKind::Loop(Box::new(self.lower_block(loop_expr)))
    }

    fn create_iterator_binding(&mut self) -> (tlang_span::HirId, Ident, hir::Pat) {
        let iterator_binding_hir_id = self.unique_id();
        let iterator_binding_name = Ident::new("iterator$$", Default::default());
        self.define_symbol_at(
            0,
            iterator_binding_hir_id,
            iterator_binding_name.as_str(),
            SymbolType::Variable,
            Default::default(),
        );
        let iterator_binding_pat = hir::Pat {
            kind: hir::PatKind::Identifier(
                iterator_binding_hir_id,
                Box::new(iterator_binding_name.clone()),
            ),
            span: Default::default(),
        };
        (iterator_binding_hir_id, iterator_binding_name, iterator_binding_pat)
    }

    fn create_accumulator_binding(
        &mut self,
        for_loop: &ast::node::ForLoop,
        iterator_binding_hir_id: tlang_span::HirId,
    ) -> (tlang_span::HirId, Ident, Option<hir::Stmt>) {
        let accumulator_binding_hir_id = self.unique_id();
        let accumulator_binding_name = Ident::new("accumulator$$", Default::default());
        let accumulator_initializer = if let Some((_pat, expr)) = &for_loop.acc {
            self.define_symbol_after(
                accumulator_binding_hir_id,
                accumulator_binding_name.as_str(),
                SymbolType::Variable,
                Default::default(),
                |s| s.hir_id == Some(iterator_binding_hir_id),
            );

            let accumulator_declaration = hir::Stmt::new(
                self.unique_id(),
                hir::StmtKind::Let(
                    Box::new(hir::Pat {
                        kind: hir::PatKind::Identifier(
                            accumulator_binding_hir_id,
                            Box::new(accumulator_binding_name.clone()),
                        ),
                        span: Default::default(),
                    }),
                    Box::new(self.lower_expr(expr)),
                    Default::default(),
                ),
                Default::default(),
            );

            Some(accumulator_declaration)
        } else {
            None
        };
        (accumulator_binding_hir_id, accumulator_binding_name, accumulator_initializer)
    }

    fn create_iterator_value(&mut self, for_loop: &ast::node::ForLoop) -> hir::Expr {
        let hir_id = self.unique_id();
        let iter_expr = self.lower_expr(&for_loop.iter);
        let iterator_binding_call = self.expr(
            for_loop.iter.span,
            hir::ExprKind::FieldAccess(
                Box::new(iter_expr),
                Ident::new("iter", Default::default()),
            ),
        );
        self.expr(
            for_loop.iter.span,
            hir::ExprKind::Call(Box::new(hir::CallExpression {
                hir_id,
                callee: iterator_binding_call,
                arguments: vec![],
            })),
        )
    }

    fn create_loop_paths(
        &mut self,
        iterator_binding_name: Ident,
        accumulator_binding_name: Ident,
        iterator_binding_hir_id: tlang_span::HirId,
        accumulator_binding_hir_id: tlang_span::HirId,
    ) -> (hir::Path, hir::Path) {
        let mut iterator_binding_path = hir::Path::new(
            vec![hir::PathSegment::new(iterator_binding_name)],
            Default::default(),
        );
        iterator_binding_path.res.set_hir_id(iterator_binding_hir_id);

        let mut accumulator_binding_path = hir::Path::new(
            vec![hir::PathSegment::new(accumulator_binding_name)],
            Default::default(),
        );
        accumulator_binding_path.res.set_hir_id(accumulator_binding_hir_id);

        (iterator_binding_path, accumulator_binding_path)
    }

    fn create_final_block(
        &mut self,
        for_loop: &ast::node::ForLoop,
        iterator_binding_pat: hir::Pat,
        iterator_binding_value: hir::Expr,
        loop_block: hir::Block,
        accumulator_initializer: Option<hir::Stmt>,
    ) -> hir::Block {
        let loop_expr = self.expr(
            for_loop.block.span,
            hir::ExprKind::Loop(Box::new(loop_block)),
        );

        let iterator_binding_stmt = hir::Stmt::new(
            self.unique_id(),
            hir::StmtKind::Let(
                Box::new(iterator_binding_pat),
                Box::new(iterator_binding_value),
                Default::default(),
            ),
            Default::default(),
        );

        let mut init_loop_block = hir::Block::new(
            self.lower_node_id(for_loop.id),
            vec![iterator_binding_stmt],
            Some(loop_expr),
            for_loop.iter.span,
        );

        if let Some(stmt) = accumulator_initializer {
            init_loop_block.stmts.push(stmt);
        }

        init_loop_block
    }

    pub(crate) fn lower_for_loop(
        &mut self,
        node_id: NodeId,
        for_loop: &ast::node::ForLoop,
    ) -> hir::ExprKind {
        let block = self.with_scope(node_id, |this| {
            let (iterator_binding_hir_id, iterator_binding_name, iterator_binding_pat) = 
                this.create_iterator_binding();

            let (accumulator_binding_hir_id, accumulator_binding_name, accumulator_initializer) =
                this.create_accumulator_binding(for_loop, iterator_binding_hir_id);

            let iterator_binding_value = this.create_iterator_value(for_loop);

            let loop_block = this.with_scope(for_loop.id, |this| {
                let (iterator_binding_path, accumulator_binding_path) = this.create_loop_paths(
                    iterator_binding_name.clone(),
                    accumulator_binding_name.clone(),
                    iterator_binding_hir_id,
                    accumulator_binding_hir_id,
                );

                this.lower_for_loop_body(for_loop, iterator_binding_path, accumulator_binding_path)
            });

            this.create_final_block(
                for_loop,
                iterator_binding_pat,
                iterator_binding_value,
                loop_block,
                accumulator_initializer,
            )
        });

        hir::ExprKind::Block(Box::new(block))
    }

    fn create_iterator_next_call(&mut self, iterator_binding_path: hir::Path) -> hir::Expr {
        let iterator_binding_expr = self.expr(
            Default::default(),
            hir::ExprKind::Path(Box::new(iterator_binding_path)),
        );
        let iterator_next_field = self.expr(
            Default::default(),
            hir::ExprKind::FieldAccess(
                Box::new(iterator_binding_expr),
                Ident::new("next", Default::default()),
            ),
        );

        let hir_id = self.unique_id();
        self.expr(
            Default::default(),
            hir::ExprKind::Call(Box::new(hir::CallExpression {
                hir_id,
                callee: iterator_next_field,
                arguments: vec![],
            })),
        )
    }

    fn create_loop_statements(&mut self, for_loop: &ast::node::ForLoop, accumulator_path_expr: &hir::Expr) -> Vec<hir::Stmt> {
        if let Some((pat, _)) = &for_loop.acc {
            let hir_id = self.unique_id();
            let accumulator_reassignment = hir::Stmt::new(
                hir_id,
                hir::StmtKind::Let(
                    Box::new(self.lower_pat(pat)),
                    Box::new(accumulator_path_expr.clone()),
                    Box::default(),
                ),
                Default::default(),
            );
            vec![accumulator_reassignment]
        } else {
            vec![]
        }
    }

    fn create_loop_arm(&mut self, for_loop: &ast::node::ForLoop) -> hir::MatchArm {
        self.with_scope(for_loop.block.id, |this| {
            let for_loop_pat = this.lower_pat(&for_loop.pat);
            let loop_body = this.lower_block_in_current_scope(&for_loop.block);

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
        })
    }

    fn lower_for_loop_body(
        &mut self,
        for_loop: &ast::node::ForLoop,
        iterator_binding_path: hir::Path,
        accumulator_binding_path: hir::Path,
    ) -> hir::Block {
        let call_next = self.create_iterator_next_call(iterator_binding_path);

        let accumulator_path_expr = self.expr(
            Default::default(),
            hir::ExprKind::Path(Box::new(accumulator_binding_path.clone())),
        );

        let loop_statements = self.create_loop_statements(for_loop, &accumulator_path_expr);

        let loop_arm = self.create_loop_arm(for_loop);
        
        let break_arm = self.with_new_scope(|this, _scope| {
            let accumulator_path_expr = for_loop.acc.as_ref().map(|_| {
                Box::new(this.expr(
                    Default::default(),
                    hir::ExprKind::Path(Box::new(accumulator_binding_path)),
                ))
            });

            let arm = hir::MatchArm {
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
                    this.unique_id(),
                    vec![],
                    Some(this.expr(
                        Default::default(),
                        hir::ExprKind::Break(accumulator_path_expr),
                    )),
                    Default::default(),
                ),
                leading_comments: vec![],
                trailing_comments: vec![],
            };

            (arm.block.hir_id, arm)
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

        hir::Block::new(
            self.unique_id(),
            loop_statements,
            Some(match_expr),
            Default::default(),
        )
    }
}
