use std::collections::HashMap;

use log::debug;
use tlang_ast as ast;
use tlang_ast::keyword::kw;
use tlang_ast::node::{BinaryOpExpression, BinaryOpKind, UnaryOp};
use tlang_hir as hir;

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
            ast::node::ExprKind::ForLoop(for_loop) => self.lower_for_loop(node.id, for_loop),
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
            ast::node::ExprKind::FunctionExpression(decl) => self.lower_fn_expr(decl),
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
                hir::ExprKind::FieldAccess(Box::new(expr), *field)
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
            ast::node::ExprKind::Literal(box literal) => hir::ExprKind::Literal(Box::new(*literal)),
            // Note: TaggedString literals are expanded into Call expressions by the parser.
            // If a TaggedString survives to HIR lowering, it means the parser didn't handle it.
            ast::node::ExprKind::Implements(expr, path) => {
                hir::ExprKind::Implements(Box::new(self.lower_expr(expr)), self.lower_path(path))
            }
            ast::node::ExprKind::Matches(expr, pat) => self.lower_matches(expr, pat, node.span),
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
            ast::node::ExprKind::TaggedString { tag, parts, exprs } => {
                hir::ExprKind::TaggedString {
                    tag: Box::new(self.lower_expr(tag)),
                    parts: parts.clone(),
                    exprs: self.lower_exprs(exprs),
                }
            }
            ast::node::ExprKind::None => {
                unreachable!("ExprKind::None should not be encountered, validate AST first")
            }
        };

        hir::Expr {
            hir_id: self.lower_node_id(node.id),
            kind,
            ty: hir::Ty::unknown(),
            span: node.span,
        }
    }

    fn lower_fn_expr(&mut self, decl: &ast::node::FunctionDeclaration) -> hir::ExprKind {
        // Suspend the protocol dispatch context while lowering a nested
        // function expression: its own `self` (if any) is unrelated to
        // the outer protocol's `self`, so we must not rewrite field-access
        // calls inside the nested body.
        let saved_ctx = self.protocol_dispatch_ctx.take();
        let result = hir::ExprKind::FunctionExpression(Box::new(self.lower_fn_decl(decl)));
        self.protocol_dispatch_ctx = saved_ctx;
        result
    }

    fn lower_match(&mut self, match_expr: &ast::node::MatchExpression) -> hir::ExprKind {
        let ast::node::MatchExpression { expression, arms } = match_expr;
        let mut idents = HashMap::new();
        let expr = self.lower_expr(expression);
        let arms = arms
            .iter()
            .map(|arm| {
                // All arms enter the arm scope. For block-body arms this creates two distinct
                // scopes (arm scope + block scope), making `arm.hir_id != arm.block.hir_id`.
                // For inline-expr arms the block reuses the arm's hir_id, so they are equal.
                // This invariant drives the two-scope vs one-scope decision in the runtime.
                self.with_scope(arm.id, |this| {
                    let hir_id = this.lower_node_id(arm.id);
                    let pat = this.lower_pat_with_idents(&arm.pattern, &mut idents);
                    let guard = arm.guard.as_ref().map(|expr| this.lower_expr(expr));
                    let block = if let ast::node::ExprKind::Block(ast_block) = &arm.expression.kind
                    {
                        this.lower_block(ast_block)
                    } else {
                        hir::Block::new(
                            hir_id,
                            vec![],
                            Some(this.lower_expr(&arm.expression)),
                            arm.expression.span,
                        )
                    };

                    hir::MatchArm {
                        hir_id,
                        pat,
                        guard,
                        block,
                        pat_locals: 0,
                        leading_comments: vec![],
                        trailing_comments: vec![],
                    }
                })
            })
            .collect();
        hir::ExprKind::Match(Box::new(expr), arms)
    }

    /// Lowers `expr matches pat` into a synthetic `match expr { pat => true, _ => false }`.
    fn lower_matches(
        &mut self,
        expr: &ast::node::Expr,
        pat: &ast::node::Pat,
        span: tlang_span::Span,
    ) -> hir::ExprKind {
        let lowered_expr = self.lower_expr(expr);

        // Arm 1: `pat => true`
        let true_arm_hir_id = self.unique_id();
        let lowered_pat = self.lower_pat(pat);
        let true_block = hir::Block::new(
            true_arm_hir_id,
            vec![],
            Some(hir::Expr {
                hir_id: self.unique_id(),
                kind: hir::ExprKind::Literal(Box::new(tlang_ast::token::Literal::Boolean(true))),
                ty: hir::Ty::unknown(),
                span,
            }),
            span,
        );
        let true_arm = hir::MatchArm {
            hir_id: true_arm_hir_id,
            pat: lowered_pat,
            guard: None,
            block: true_block,
            pat_locals: 0,
            leading_comments: vec![],
            trailing_comments: vec![],
        };

        // Arm 2: `_ => false`
        let false_arm_hir_id = self.unique_id();
        let false_block = hir::Block::new(
            false_arm_hir_id,
            vec![],
            Some(hir::Expr {
                hir_id: self.unique_id(),
                kind: hir::ExprKind::Literal(Box::new(tlang_ast::token::Literal::Boolean(false))),
                ty: hir::Ty::unknown(),
                span,
            }),
            span,
        );
        let false_arm = hir::MatchArm {
            hir_id: false_arm_hir_id,
            pat: hir::Pat {
                kind: hir::PatKind::Wildcard,
                ty: hir::Ty::unknown(),
                span,
            },
            guard: None,
            block: false_block,
            pat_locals: 0,
            leading_comments: vec![],
            trailing_comments: vec![],
        };

        hir::ExprKind::Match(Box::new(lowered_expr), vec![true_arm, false_arm])
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
            hir_id: block.hir_id,
            pat,
            guard: None,
            block,
            pat_locals: 0,
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
                hir_id: block.hir_id,
                pat: hir::Pat {
                    kind: hir::PatKind::Wildcard,
                    ty: hir::Ty::unknown(),
                    span: Default::default(),
                },
                guard,
                block,
                pat_locals: 0,
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
                        "{}/{}",
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
        // ── Implicit self-dispatch in protocol default implementations ──────────
        //
        // Inside a protocol default method body, a call of the form
        //   `self.method(arg1, arg2, …)`
        // is rewritten to
        //   `OwningProtocol::method(self, arg1, arg2, …)`
        //
        // `method_dispatch_map` maps method name → the Ident of the protocol
        // that declares it.  It covers the current protocol's own methods AND
        // all methods from transitively-reachable constraint protocols, so
        // that default bodies can call methods from constraint protocols.
        if let ast::node::ExprKind::FieldExpression(box ast::node::FieldAccessExpression {
            base,
            field,
        }) = &node.callee.kind
            && let Some(ctx) = self.protocol_dispatch_ctx.as_ref()
        {
            // Identify the `self` receiver using compiler information:
            //
            // 1. Primary check – the path segment is the compiler's reserved
            //    `kw::_Self` keyword ("self"), which the parser only emits for
            //    the genuine `self` keyword, never for a regular identifier.
            //
            // 2. Reinforcing check – if semantic analysis has resolved the path
            //    to a specific declaration (`Res::Def(node_id)`), verify that
            //    the declaration is our tracked `self` parameter node.  This
            //    handles shadowing: a `let self = …` binding would resolve to a
            //    *different* NodeId and fail this check.
            let is_self_receiver = if let ast::node::ExprKind::Path(path) = &base.kind {
                if path.segments.len() == 1 && path.segments[0].as_str() == kw::_Self {
                    // If the AST resolver has already set a Def node id,
                    // use it for identity confirmation; otherwise fall back
                    // to the keyword check alone.
                    match path.res {
                        ast::node::Res::Def(node_id) => node_id == ctx.self_param_node_id,
                        ast::node::Res::Unresolved => true,
                        ast::node::Res::PrimTy => false,
                    }
                } else {
                    false
                }
            } else {
                false
            };

            if is_self_receiver
                && let Some(&protocol_ident) = ctx.method_dispatch_map.get(field.as_str())
            {
                // Build the qualified callee path `OwningProtocol::method_name`.
                // When the protocol method is defined with multiple arities,
                // append the `/arity` suffix (total arity = 1 for `self` +
                // the number of explicit arguments).
                let method_str = field.as_str();
                let total_arity = node.arguments.len() + 1; // +1 for self

                let method_segment_name = if self.has_multi_arity_fn(method_str, total_arity) {
                    format!("{method_str}/{total_arity}")
                } else {
                    method_str.to_string()
                };

                let span = node.callee.span;
                let path = hir::Path::new(
                    vec![
                        hir::PathSegment::from_str(protocol_ident.as_str(), protocol_ident.span),
                        hir::PathSegment::from_str(&method_segment_name, field.span),
                    ],
                    span,
                );

                let callee_hir = self.expr(span, hir::ExprKind::Path(Box::new(path)));

                // The first argument is the `self` receiver; the rest follow.
                let self_arg = self.lower_expr(base);
                let mut arguments = vec![self_arg];
                arguments.extend(self.lower_exprs(&node.arguments));

                return hir::CallExpression {
                    hir_id: self.unique_id(),
                    callee: callee_hir,
                    arguments,
                };
            }
        }

        // Normal (non-rewritten) call lowering.
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
            BinaryOpKind::Sub => hir::BinaryOpKind::Sub,
            BinaryOpKind::Mul => hir::BinaryOpKind::Mul,
            BinaryOpKind::Div => hir::BinaryOpKind::Div,
            BinaryOpKind::Mod => hir::BinaryOpKind::Mod,
            BinaryOpKind::Exp => hir::BinaryOpKind::Exp,
            BinaryOpKind::And => hir::BinaryOpKind::And,
            BinaryOpKind::Or => hir::BinaryOpKind::Or,
            BinaryOpKind::Eq => hir::BinaryOpKind::Eq,
            BinaryOpKind::NotEq => hir::BinaryOpKind::NotEq,
            BinaryOpKind::Less => hir::BinaryOpKind::Less,
            BinaryOpKind::LessEq => hir::BinaryOpKind::LessEq,
            BinaryOpKind::Greater => hir::BinaryOpKind::Greater,
            BinaryOpKind::GreaterEq => hir::BinaryOpKind::GreaterEq,
            BinaryOpKind::BitwiseAnd => hir::BinaryOpKind::BitwiseAnd,
            BinaryOpKind::BitwiseOr => hir::BinaryOpKind::BitwiseOr,
            BinaryOpKind::BitwiseXor => hir::BinaryOpKind::BitwiseXor,
            BinaryOpKind::Pipeline => return self.lower_pipeline_operator(node),
            BinaryOpKind::Match | BinaryOpKind::NotMatch => return self.lower_match_operator(node),
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

    // a =~ b  →  Accepts::accepts(b, a)   (RHS = pattern = self)
    // a !~ b  →  !Accepts::accepts(b, a)
    fn lower_match_operator(&mut self, node: &BinaryOpExpression) -> hir::ExprKind {
        let span = node.lhs.span;
        let lhs = self.lower_expr(&node.lhs);
        let rhs = self.lower_expr(&node.rhs);

        let callee_path = hir::Path::new(
            vec![
                hir::PathSegment::from_str("Accepts", span),
                hir::PathSegment::from_str("accepts", span),
            ],
            span,
        )
        .with_res(hir::Res::new_protocol_method());
        let callee = hir::Expr {
            hir_id: self.unique_id(),
            kind: hir::ExprKind::Path(Box::new(callee_path)),
            ty: hir::Ty::unknown(),
            span,
        };
        let call = hir::Expr {
            hir_id: self.unique_id(),
            kind: hir::ExprKind::Call(Box::new(hir::CallExpression {
                hir_id: self.unique_id(),
                callee,
                arguments: vec![rhs, lhs],
            })),
            ty: hir::Ty::unknown(),
            span,
        };

        if node.op == BinaryOpKind::NotMatch {
            hir::ExprKind::Unary(UnaryOp::Not, Box::new(call))
        } else {
            call.kind
        }
    }
}
