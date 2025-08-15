use std::collections::HashMap;

use log::debug;
use tlang_ast as ast;
use tlang_ast::node::{BinaryOpExpression, BinaryOpKind};
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
            ast::node::ExprKind::Block(block) => {
                hir::ExprKind::Block(Box::new(self.lower_block(block)))
            }
            ast::node::ExprKind::Loop(block) => self.lower_loop(block),
            ast::node::ExprKind::ForLoop(for_loop) => self.lower_for_loop(for_loop),
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
            ast::node::ExprKind::Match(match_expr) => self.lower_match(match_expr),
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

    fn lower_match(&mut self, match_expr: &ast::node::MatchExpression) -> hir::ExprKind {
        let ast::node::MatchExpression { expression, arms } = match_expr;
        let mut idents = HashMap::new();
        let expr = self.lower_expr(expression);
        let arms = arms
            .iter()
            .map(|arm| {
                let scope_id = if let ast::node::ExprKind::Block(block) = &arm.expression.kind {
                    block.id
                } else {
                    arm.id
                };

                self.with_scope(scope_id, |this| {
                    let hir_id = this.lower_node_id(arm.id);
                    let pat = this.lower_pat_with_idents(&arm.pattern, &mut idents);
                    let guard = arm.guard.as_ref().map(|expr| this.lower_expr(expr));
                    let block = if let ast::node::ExprKind::Block(block) = &arm.expression.kind {
                        this.lower_block_in_current_scope(block)
                    } else {
                        hir::Block::new(
                            hir_id,
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

    fn lower_if_else(&mut self, if_else_expr: &ast::node::IfElseExpression) -> hir::ExprKind {
        let ast::node::IfElseExpression {
            condition,
            then_branch,
            else_branches,
        } = if_else_expr;

        let condition = self.lower_expr(condition);
        let consequence = self.lower_block(then_branch);
        let else_branches = else_branches
            .iter()
            .map(|clause| hir::ElseClause {
                condition: clause.condition.as_ref().map(|expr| self.lower_expr(expr)),
                consequence: self.lower_block(&clause.consequence),
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
        let block = self.lower_block(then_branch);

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
            let block = self.lower_block(&else_branch.consequence);

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

    fn lower_callee(&mut self, callee: &ast::node::Expr, arg_len: usize) -> hir::Expr {
        match &callee.kind {
            ast::node::ExprKind::Path(path) => {
                if self.has_multi_arity_fn(path.segments.last().unwrap().as_str(), arg_len) {
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
            BinaryOpKind::Pipeline => return self.lower_pipeline_operator(node),
        };

        let lhs = self.lower_expr(&node.lhs);
        let rhs = self.lower_expr(&node.rhs);

        hir::ExprKind::Binary(binary_op_kind, Box::new(lhs), Box::new(rhs))
    }

    fn lower_pipeline_operator(&mut self, node: &BinaryOpExpression) -> hir::ExprKind {
        match &node.rhs.kind {
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
        }
    }
}
