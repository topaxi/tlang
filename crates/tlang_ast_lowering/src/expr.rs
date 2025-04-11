use std::collections::HashMap;

use log::debug;
use tlang_ast as ast;
use tlang_ast::node::{BinaryOpExpression, BinaryOpKind, Ident};
use tlang_ast::token::Literal;
use tlang_hir::hir;

use crate::LoweringContext;

impl LoweringContext {
    fn lower_exprs(&mut self, exprs: &[ast::node::Expr]) -> Vec<hir::Expr> {
        exprs.iter().map(|expr| self.lower_expr(expr)).collect()
    }

    pub(crate) fn lower_expr(&mut self, node: &ast::node::Expr) -> hir::Expr {
        debug!("Lowering expression {:?}", node.kind);

        let kind = match &node.kind {
            ast::node::ExprKind::BinaryOp(binary_expr) => self.lower_binary_expr(binary_expr),
            ast::node::ExprKind::Block(box ast::node::Block {
                id: _,
                statements,
                expression,
                span,
            }) => hir::ExprKind::Block(Box::new(self.lower_block(
                statements,
                expression.as_ref(),
                *span,
            ))),
            ast::node::ExprKind::Loop(box ast::node::Block {
                id: _,
                statements,
                expression,
                span,
            }) => hir::ExprKind::Loop(Box::new(self.lower_block(
                statements,
                expression.as_ref(),
                *span,
            ))),
            ast::node::ExprKind::ForLoop(box for_loop) => self.lower_for_loop(for_loop),
            ast::node::ExprKind::Break(expr) => {
                hir::ExprKind::Break(expr.as_ref().map(|expr| Box::new(self.lower_expr(expr))))
            }
            ast::node::ExprKind::Continue => hir::ExprKind::Continue,
            ast::node::ExprKind::Call(call_expr) => {
                hir::ExprKind::Call(Box::new(self.lower_call_expr(call_expr)))
            }
            ast::node::ExprKind::RecursiveCall(call_expr) => {
                hir::ExprKind::TailCall(Box::new(self.lower_call_expr(call_expr)))
            }
            ast::node::ExprKind::Cast(box expr, box ty) => {
                let expr = self.lower_expr(expr);
                let ty = self.lower_ty(Some(ty));
                hir::ExprKind::Cast(Box::new(expr), Box::new(ty))
            }
            ast::node::ExprKind::UnaryOp(op, expr) => {
                hir::ExprKind::Unary(*op, Box::new(self.lower_expr(expr)))
            }
            ast::node::ExprKind::Path(path) => hir::ExprKind::Path(Box::new(self.lower_path(path))),
            ast::node::ExprKind::FunctionExpression(decl) => {
                hir::ExprKind::FunctionExpression(Box::new(self.lower_fn_decl(decl)))
            }
            ast::node::ExprKind::List(exprs) => hir::ExprKind::List(self.lower_exprs(exprs)),
            ast::node::ExprKind::Dict(entries) => {
                let entries = entries
                    .iter()
                    .map(|(key, value)| (self.lower_expr(key), self.lower_expr(value)))
                    .collect();
                hir::ExprKind::Dict(entries)
            }
            ast::node::ExprKind::Let(pat, expr) => {
                let expression = self.lower_expr(expr);
                let pattern = self.lower_pat(pat);
                hir::ExprKind::Let(Box::new(pattern), Box::new(expression))
            }
            ast::node::ExprKind::FieldExpression(box ast::node::FieldAccessExpression {
                base,
                field,
            }) => {
                let expr = self.lower_expr(base);
                hir::ExprKind::FieldAccess(Box::new(expr), field.clone())
            }
            ast::node::ExprKind::IndexExpression(box ast::node::IndexAccessExpression {
                base,
                index,
            }) => {
                let expr = self.lower_expr(base);
                let index = self.lower_expr(index);
                hir::ExprKind::IndexAccess(Box::new(expr), Box::new(index))
            }
            ast::node::ExprKind::IfElse(box ast::node::IfElseExpression {
                condition,
                then_branch,
                else_branches,
            }) if let ast::node::ExprKind::Let(pat, expr) = &condition.kind => {
                self.lower_if_let_else(condition, then_branch, else_branches, pat, expr)
            }
            ast::node::ExprKind::IfElse(if_else_expr) => self.lower_if_else(if_else_expr),
            ast::node::ExprKind::Literal(box literal) => {
                hir::ExprKind::Literal(Box::new(literal.clone()))
            }
            ast::node::ExprKind::Match(box ast::node::MatchExpression { expression, arms }) => {
                let mut idents = HashMap::new();
                let expr = self.lower_expr(expression);
                let arms = arms
                    .iter()
                    .map(|arm| {
                        self.with_new_scope(|this| {
                            let pat = this.lower_pat_with_idents(&arm.pattern, &mut idents);
                            let guard = arm.guard.as_ref().map(|expr| this.lower_expr(expr));
                            let block =
                                if let ast::node::ExprKind::Block(block) = &arm.expression.kind {
                                    this.lower_block_in_current_scope(
                                        &block.statements,
                                        block.expression.as_ref(),
                                        block.span,
                                    )
                                } else {
                                    hir::Block::new(
                                        vec![],
                                        Some(this.lower_expr(&arm.expression)),
                                        arm.expression.span,
                                    )
                                };

                            hir::MatchArm {
                                pat,
                                guard,
                                block,
                                leading_comments: vec![],
                                trailing_comments: vec![],
                            }
                        })
                    })
                    .collect();
                hir::ExprKind::Match(Box::new(expr), arms)
            }
            ast::node::ExprKind::Range(box ast::node::RangeExpression {
                start,
                end,
                inclusive,
            }) => {
                let start = self.lower_expr(start);
                let end = self.lower_expr(end);
                hir::ExprKind::Range(Box::new(hir::RangeExpression {
                    start,
                    end,
                    inclusive: *inclusive,
                }))
            }
            ast::node::ExprKind::Wildcard => hir::ExprKind::Wildcard,
            ast::node::ExprKind::None => {
                unreachable!("ExprKind::None should not be encountered, validate AST first")
            }
        };

        hir::Expr {
            hir_id: self.lower_node_id(node.id),
            kind,
            span: node.span,
        }
    }

    fn lower_if_else(&mut self, if_else_expr: &ast::node::IfElseExpression) -> hir::ExprKind {
        let ast::node::IfElseExpression {
            condition,
            then_branch,
            else_branches,
        } = if_else_expr;

        let condition = self.lower_expr(condition);

        let consequence = self.lower_block(
            &then_branch.statements,
            then_branch.expression.as_ref(),
            then_branch.span,
        );

        let else_branches = else_branches
            .iter()
            .map(|clause| hir::ElseClause {
                condition: clause.condition.as_ref().map(|expr| self.lower_expr(expr)),
                consequence: self.lower_block(
                    &clause.consequence.statements,
                    clause.consequence.expression.as_ref(),
                    clause.consequence.span,
                ),
            })
            .collect();

        hir::ExprKind::IfElse(Box::new(condition), Box::new(consequence), else_branches)
    }

    fn lower_if_let_else(
        &mut self,
        condition: &ast::node::Expr,
        then_branch: &ast::node::Block,
        else_branches: &[ast::node::ElseClause],
        pat: &ast::node::Pat,
        expr: &ast::node::Expr,
    ) -> hir::ExprKind {
        let pat = self.lower_pat(pat);
        let expr = self.lower_expr(expr);
        let mut arms = Vec::with_capacity(else_branches.len() + 1);
        let block = self.lower_block(
            &then_branch.statements,
            then_branch.expression.as_ref(),
            then_branch.span,
        );

        arms.push(hir::MatchArm {
            pat,
            guard: None,
            block,
            leading_comments: condition.leading_comments.clone(),
            trailing_comments: condition.trailing_comments.clone(),
        });

        for else_branch in else_branches {
            let guard = else_branch
                .condition
                .as_ref()
                .map(|expr| self.lower_expr(expr));
            let block = self.lower_block(
                &else_branch.consequence.statements,
                else_branch.consequence.expression.as_ref(),
                else_branch.consequence.span,
            );

            arms.push(hir::MatchArm {
                pat: hir::Pat {
                    kind: hir::PatKind::Wildcard,
                    span: Default::default(),
                },
                guard,
                block,
                leading_comments: vec![],
                trailing_comments: vec![],
            });
        }

        hir::ExprKind::Match(Box::new(expr), arms)
    }

    fn lower_for_loop(&mut self, for_loop: &ast::node::ForLoop) -> hir::ExprKind {
        let block = self.with_new_scope(|this| {
            let iterator_binding_name = Ident::new("iterator$$", Default::default());
            let hir_id = this.unique_id();
            let iterator_binding_pat = hir::Pat {
                kind: hir::PatKind::Identifier(hir_id, Box::new(iterator_binding_name.clone())),
                span: Default::default(),
            };
            this.scope()
                .def_local(iterator_binding_name.as_str(), hir_id);
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
            let iterator_binding_stmt = hir::Stmt::new(
                this.unique_id(),
                hir::StmtKind::Let(
                    Box::new(iterator_binding_pat),
                    Box::new(iterator_binding_value),
                    Default::default(),
                ),
                Default::default(),
            );

            let accumulator_binding = for_loop.acc.as_ref().map(|(pat, expr)| {
                let pat = this.lower_pat(pat);
                let expr = this.lower_expr(expr);

                (pat, expr)
            });

            let iterator_binding_res = this
                .scope()
                .lookup(iterator_binding_name.as_str())
                .unwrap()
                .res();

            let iterator_binding = this.expr(
                Default::default(),
                hir::ExprKind::Path(Box::new(
                    hir::Path::new(
                        vec![hir::PathSegment::new(iterator_binding_name)],
                        Default::default(),
                    )
                    .with_res(iterator_binding_res),
                )),
            );

            let iterator_next_field = this.expr(
                Default::default(),
                hir::ExprKind::FieldAccess(
                    Box::new(iterator_binding),
                    Ident::new("next", Default::default()),
                ),
            );

            let hir_id = this.unique_id();
            let call_next = this.expr(
                Default::default(),
                hir::ExprKind::Call(Box::new(hir::CallExpression {
                    hir_id,
                    callee: iterator_next_field,
                    arguments: vec![],
                })),
            );

            let accumulator_binding_name = Ident::new("accumulator$$", Default::default());
            let accumulator_initializer = if let Some((_pat, expr)) = &accumulator_binding {
                let hir_id = this.unique_id();
                this.scope().def_local("accumulator$$", hir_id);
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
                        Box::new(expr.clone()),
                        Default::default(),
                    ),
                    Default::default(),
                );

                Some(accumulator_declaration)
            } else {
                None
            };

            let accumulator_binding_res = this
                .scope()
                .lookup(accumulator_binding_name.as_str())
                .map(|b| b.res())
                .unwrap_or_default();
            let accumulator_path_expr = this.expr(
                Default::default(),
                hir::ExprKind::Path(Box::new(
                    hir::Path::new(
                        vec![hir::PathSegment::new(accumulator_binding_name.clone())],
                        Default::default(),
                    )
                    .with_res(accumulator_binding_res),
                )),
            );

            let loop_arm = this.with_new_scope(|this| {
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
                                vec![hir::PathSegment::new(Ident::new(
                                    "Some",
                                    Default::default(),
                                ))],
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

            let break_arm = this.with_new_scope(|this| {
                let accumulator_path_expr = accumulator_binding.clone().map(|_| {
                    Box::new(
                        this.expr(
                            Default::default(),
                            hir::ExprKind::Path(Box::new(
                                hir::Path::new(
                                    vec![hir::PathSegment::new(accumulator_binding_name)],
                                    Default::default(),
                                )
                                .with_res(accumulator_binding_res),
                            )),
                        ),
                    )
                });
                hir::MatchArm {
                    pat: hir::Pat {
                        kind: hir::PatKind::Enum(
                            Box::new(hir::Path::new(
                                vec![hir::PathSegment::new(Ident::new(
                                    "None",
                                    Default::default(),
                                ))],
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

            let match_expr = this.expr(
                Default::default(),
                hir::ExprKind::Match(Box::new(call_next), vec![loop_arm, break_arm]),
            );

            let match_expr = if accumulator_binding.is_some() {
                this.expr(
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

            let loop_statements = if let Some((pat, _)) = &accumulator_binding {
                let hir_id = this.unique_id();
                let accumulator_ressignment = hir::Stmt::new(
                    hir_id,
                    hir::StmtKind::Let(
                        Box::new(pat.clone()),
                        Box::new(accumulator_path_expr.clone()),
                        Box::default(),
                    ),
                    Default::default(),
                );
                vec![accumulator_ressignment]
            } else {
                vec![]
            };

            let loop_expr = this.expr(
                Default::default(),
                hir::ExprKind::Loop(Box::new(hir::Block::new(
                    loop_statements,
                    Some(match_expr),
                    Default::default(),
                ))),
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

    fn lower_callee(&mut self, callee: &ast::node::Expr, arg_len: usize) -> hir::Expr {
        match &callee.kind {
            ast::node::ExprKind::Path(path) => {
                let mut path_with_argnum = path.clone();
                let new_name = format!(
                    "{}$${}",
                    path_with_argnum.segments.last().unwrap().as_str(),
                    arg_len
                );
                path_with_argnum
                    .segments
                    .last_mut()
                    .unwrap()
                    .set_name(&new_name);

                if self.has_binding(&path_with_argnum.join("::")) {
                    self.lower_expr(&ast::node::Expr {
                        id: callee.id,
                        kind: ast::node::ExprKind::Path(path_with_argnum),
                        span: callee.span,
                        leading_comments: callee.leading_comments.clone(),
                        trailing_comments: callee.trailing_comments.clone(),
                    })
                } else {
                    self.lower_expr(callee)
                }
            }
            _ => self.lower_expr(callee),
        }
    }

    fn lower_call_expr(&mut self, node: &ast::node::CallExpression) -> hir::CallExpression {
        let arguments = self.lower_exprs(&node.arguments);
        let callee = self.lower_callee(&node.callee, node.arguments.len());

        hir::CallExpression {
            hir_id: self.unique_id(),
            callee,
            arguments,
        }
    }

    fn lower_binary_expr(&mut self, node: &BinaryOpExpression) -> hir::ExprKind {
        let binary_op_kind = match node.op {
            BinaryOpKind::Assign => hir::BinaryOpKind::Assign,
            BinaryOpKind::Add => hir::BinaryOpKind::Add,
            BinaryOpKind::Subtract => hir::BinaryOpKind::Sub,
            BinaryOpKind::Multiply => hir::BinaryOpKind::Mul,
            BinaryOpKind::Divide => hir::BinaryOpKind::Div,
            BinaryOpKind::Modulo => hir::BinaryOpKind::Mod,
            BinaryOpKind::Exponentiation => hir::BinaryOpKind::Exp,
            BinaryOpKind::And => hir::BinaryOpKind::And,
            BinaryOpKind::Or => hir::BinaryOpKind::Or,
            BinaryOpKind::Equal => hir::BinaryOpKind::Eq,
            BinaryOpKind::NotEqual => hir::BinaryOpKind::NotEq,
            BinaryOpKind::LessThan => hir::BinaryOpKind::Less,
            BinaryOpKind::LessThanOrEqual => hir::BinaryOpKind::LessEq,
            BinaryOpKind::GreaterThan => hir::BinaryOpKind::Greater,
            BinaryOpKind::GreaterThanOrEqual => hir::BinaryOpKind::GreaterEq,
            BinaryOpKind::BitwiseAnd => hir::BinaryOpKind::BitwiseAnd,
            BinaryOpKind::BitwiseOr => hir::BinaryOpKind::BitwiseOr,
            BinaryOpKind::BitwiseXor => hir::BinaryOpKind::BitwiseXor,
            BinaryOpKind::Pipeline => {
                return match &node.rhs.kind {
                    ast::node::ExprKind::Path(_) => {
                        let lhs = self.lower_expr(&node.lhs);
                        let rhs = self.lower_callee(&node.rhs, 1);

                        hir::ExprKind::Call(Box::new(hir::CallExpression {
                            hir_id: self.unique_id(),
                            callee: rhs,
                            arguments: vec![lhs],
                        }))
                    }
                    ast::node::ExprKind::Call(call_expr) => {
                        let arguments = if call_expr.has_wildcard() {
                            call_expr
                                .arguments
                                .iter()
                                .map(|arg| {
                                    if matches!(arg.kind, ast::node::ExprKind::Wildcard) {
                                        self.lower_expr(&node.lhs)
                                    } else {
                                        self.lower_expr(arg)
                                    }
                                })
                                .collect()
                        } else {
                            let mut arguments = Vec::with_capacity(call_expr.arguments.len() + 1);
                            arguments.push(self.lower_expr(&node.lhs));
                            arguments.extend(self.lower_exprs(&call_expr.arguments));
                            arguments
                        };
                        let callee = self.lower_callee(&call_expr.callee, arguments.len());

                        hir::ExprKind::Call(Box::new(hir::CallExpression {
                            hir_id: self.unique_id(),
                            callee,
                            arguments,
                        }))
                    }
                    _ => unreachable!("Validate AST before lowering"),
                };
            }
        };

        let lhs = self.lower_expr(&node.lhs);
        let rhs = self.lower_expr(&node.rhs);

        hir::ExprKind::Binary(binary_op_kind, Box::new(lhs), Box::new(rhs))
    }
}
