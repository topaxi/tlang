use tlang_ast::node::Ident;
use tlang_ast::token::Literal;
use tlang_hir::fold::{self, Folder, SmallVec};
use tlang_hir::{self as hir};
use tlang_span::Span;

use crate::hir_opt::{HirOptContext, HirOptError, HirPass};

/// Trait that controls which expressions the ANF pass lifts out of value
/// positions. Implementors return `true` for expressions that *cannot* remain
/// inline and must be bound to a temporary variable.
pub trait AnfFilter {
    /// Returns `true` when `expr` must be lifted out of a value position.
    fn needs_lifting(&self, expr: &hir::Expr) -> bool;
}

/// Full ANF filter: ensures all sub-expressions of operations are atomic
/// (paths, literals, wildcards, function expressions).
///
/// This filter considers any non-atomic expression as needing lifting.
/// The `AnfFolder` uses `normalize_for_binding` in binding positions
/// (let init, assignment RHS, return value, block completion) to prevent
/// operations with all-atomic sub-expressions from being lifted again.
#[derive(Default)]
pub struct FullAnfFilter;

impl FullAnfFilter {
    fn is_atomic(expr: &hir::Expr) -> bool {
        matches!(
            expr.kind,
            hir::ExprKind::Path(..)
                | hir::ExprKind::Literal(..)
                | hir::ExprKind::Wildcard
                | hir::ExprKind::FunctionExpression(..)
        )
    }
}

impl AnfFilter for FullAnfFilter {
    fn needs_lifting(&self, expr: &hir::Expr) -> bool {
        !Self::is_atomic(expr)
    }
}

/// Configurable ANF pass. Walks the HIR and lifts expressions that the
/// provided `AnfFilter` marks as needing lifting out of value positions
/// (let initialisers, call arguments, binary operands, etc.) into
/// temporary `let $anf$N = ...;` bindings.
pub struct AnfTransform<F: AnfFilter> {
    counter: usize,
    changed: bool,
    filter: F,
}

impl<F: AnfFilter + Default> Default for AnfTransform<F> {
    fn default() -> Self {
        Self {
            counter: 0,
            changed: false,
            filter: F::default(),
        }
    }
}

impl<F: AnfFilter> AnfTransform<F> {
    pub fn new(filter: F) -> Self {
        Self {
            counter: 0,
            changed: false,
            filter,
        }
    }
}

/// The folder that carries accumulated pending statements and the HIR context.
pub(crate) struct AnfFolder<'a, F: AnfFilter> {
    pub(crate) ctx: &'a mut HirOptContext,
    pub(crate) counter: &'a mut usize,
    /// Statements to inject before the current position in a block.
    pub(crate) pending: Vec<hir::Stmt>,
    pub(crate) changed: &'a mut bool,
    /// HirId of the function currently being folded (for self-referencing TailCall detection).
    pub(crate) current_function_hir_id: Option<tlang_span::HirId>,
    pub(crate) filter: &'a F,
}

impl<F: AnfFilter> AnfFolder<'_, F> {
    pub(crate) fn alloc_hir_id(&mut self) -> tlang_span::HirId {
        self.ctx.hir_id_allocator.next_id()
    }

    pub(crate) fn fresh_anf_var(&mut self) -> (String, tlang_span::HirId) {
        let n = *self.counter;
        *self.counter += 1;
        let name = format!("$anf${n}");
        let pat_hir_id = self.alloc_hir_id();
        (name, pat_hir_id)
    }

    /// Returns true if the TailCall is self-referencing (callee matches the
    /// current function). Only self-referencing TailCalls are converted to
    /// loops by the codegen; mutual TailCalls are regular calls.
    pub(crate) fn is_self_referencing_tail_call(&self, call: &hir::CallExpression) -> bool {
        if let Some(fn_hir_id) = self.current_function_hir_id
            && let hir::ExprKind::Path(ref path) = call.callee.kind
            && let Some(callee_hir_id) = path.res.hir_id()
        {
            return callee_hir_id == fn_hir_id;
        }
        false
    }

    pub(crate) fn make_path_expr(
        &mut self,
        name: &str,
        pat_hir_id: tlang_span::HirId,
    ) -> hir::Expr {
        let hir_id = self.alloc_hir_id();
        let mut path = hir::Path::new(
            vec![hir::PathSegment::from_str(name, Span::default())],
            Span::default(),
        );
        path.res = hir::Res::new_local(pat_hir_id);
        hir::Expr {
            hir_id,
            kind: hir::ExprKind::Path(Box::new(path)),
            ty: hir::Ty::unknown(),
            span: Span::default(),
        }
    }

    pub(crate) fn make_literal_none_expr(&mut self) -> hir::Expr {
        let hir_id = self.alloc_hir_id();
        hir::Expr {
            hir_id,
            kind: hir::ExprKind::Literal(Box::new(Literal::None)),
            ty: hir::Ty::unknown(),
            span: Span::default(),
        }
    }

    pub(crate) fn make_assign_stmt(
        &mut self,
        name: &str,
        pat_hir_id: tlang_span::HirId,
        value: hir::Expr,
    ) -> hir::Stmt {
        let hir_id = self.alloc_hir_id();
        let lhs = self.make_path_expr(name, pat_hir_id);
        hir::Stmt::new(
            hir_id,
            hir::StmtKind::Expr(Box::new(hir::Expr {
                hir_id: self.alloc_hir_id(),
                kind: hir::ExprKind::Binary(
                    hir::BinaryOpKind::Assign,
                    Box::new(lhs),
                    Box::new(value),
                ),
                ty: hir::Ty::unknown(),
                span: Span::default(),
            })),
            Span::default(),
        )
    }

    pub(crate) fn make_let_stmt(
        &mut self,
        name: &str,
        pat_hir_id: tlang_span::HirId,
        init: hir::Expr,
    ) -> hir::Stmt {
        let hir_id = self.alloc_hir_id();
        hir::Stmt::new(
            hir_id,
            hir::StmtKind::Let(
                Box::new(hir::Pat {
                    kind: hir::PatKind::Identifier(
                        pat_hir_id,
                        Box::new(Ident::new(name, Span::default())),
                    ),
                    ty: hir::Ty::unknown(),
                    span: Span::default(),
                }),
                Box::new(init),
                Box::default(),
            ),
            Span::default(),
        )
    }

    /// Rewrite the completions of an if/else to assign into `temp_name`,
    /// then strip the completion from each block (converting to statement form).
    pub(crate) fn rewrite_if_else_completions(
        &mut self,
        temp_name: &str,
        pat_hir_id: tlang_span::HirId,
        then_block: &mut hir::Block,
        else_branches: &mut [hir::ElseClause],
    ) {
        self.rewrite_block_completion(temp_name, pat_hir_id, then_block);
        for clause in else_branches.iter_mut() {
            self.rewrite_block_completion(temp_name, pat_hir_id, &mut clause.consequence);
        }
    }

    /// Rewrite a block's completion value into an assignment to `temp_name`.
    /// If the completion is itself a compound expression (if/else, match, block),
    /// recursively rewrite its branches to assign to `temp_name` instead of
    /// wrapping in `temp = if(...)` (which would be invalid JS).
    pub(crate) fn rewrite_block_completion(
        &mut self,
        temp_name: &str,
        pat_hir_id: tlang_span::HirId,
        block: &mut hir::Block,
    ) {
        if let Some(completion) = block.expr.take() {
            let completion_ty = completion.ty.clone();
            match completion.kind {
                hir::ExprKind::IfElse(cond, mut then_block, mut else_branches) => {
                    self.rewrite_if_else_completions(
                        temp_name,
                        pat_hir_id,
                        &mut then_block,
                        &mut else_branches,
                    );
                    let if_stmt = hir::Stmt::new(
                        self.alloc_hir_id(),
                        hir::StmtKind::Expr(Box::new(hir::Expr {
                            hir_id: completion.hir_id,
                            kind: hir::ExprKind::IfElse(cond, then_block, else_branches),
                            ty: completion_ty,
                            span: completion.span,
                        })),
                        completion.span,
                    );
                    block.stmts.push(if_stmt);
                }
                hir::ExprKind::Match(scrutinee, mut arms) => {
                    self.rewrite_match_completions(temp_name, pat_hir_id, &mut arms);
                    let match_stmt = hir::Stmt::new(
                        self.alloc_hir_id(),
                        hir::StmtKind::Expr(Box::new(hir::Expr {
                            hir_id: completion.hir_id,
                            kind: hir::ExprKind::Match(scrutinee, arms),
                            ty: completion_ty,
                            span: completion.span,
                        })),
                        completion.span,
                    );
                    block.stmts.push(match_stmt);
                }
                hir::ExprKind::Block(mut inner_block) => {
                    self.rewrite_block_completion(temp_name, pat_hir_id, &mut inner_block);
                    block.stmts.extend(inner_block.stmts);
                }
                // Break/Continue are control flow — emit as statements, not assignments.
                hir::ExprKind::Break(..) | hir::ExprKind::Continue => {
                    let stmt = hir::Stmt::new(
                        self.alloc_hir_id(),
                        hir::StmtKind::Expr(Box::new(completion)),
                        Span::default(),
                    );
                    block.stmts.push(stmt);
                }
                // Self-referencing TailCall becomes a loop continuation (no value).
                // Mutual TailCall is a regular call and must be assigned.
                hir::ExprKind::TailCall(ref call) if self.is_self_referencing_tail_call(call) => {
                    let stmt = hir::Stmt::new(
                        self.alloc_hir_id(),
                        hir::StmtKind::Expr(Box::new(completion)),
                        Span::default(),
                    );
                    block.stmts.push(stmt);
                }
                _ => {
                    let assign = self.make_assign_stmt(temp_name, pat_hir_id, completion);
                    block.stmts.push(assign);
                }
            }
        }
    }

    /// Rewrite match arm completions to assign into `temp_name`.
    pub(crate) fn rewrite_match_completions(
        &mut self,
        temp_name: &str,
        pat_hir_id: tlang_span::HirId,
        arms: &mut [hir::MatchArm],
    ) {
        for arm in arms.iter_mut() {
            self.rewrite_block_completion(temp_name, pat_hir_id, &mut arm.block);
        }
    }

    /// Lift a non-expressible expression out of a value position.
    /// Pushes the necessary preceding stmts into `self.pending` and returns
    /// a Path reference to the temp variable.
    pub(crate) fn lift_expr(&mut self, expr: hir::Expr) -> hir::Expr {
        let (temp_name, pat_hir_id) = self.fresh_anf_var();
        let expr_ty = expr.ty.clone();

        match expr.kind {
            // Control-flow constructs need the nil-init + branch-assignment
            // pattern because multiple branches assign into the same temp.
            hir::ExprKind::IfElse(cond, mut then_block, mut else_branches) => {
                let init = self.make_literal_none_expr();
                let let_stmt = self.make_let_stmt(&temp_name, pat_hir_id, init);
                self.pending.push(let_stmt);

                self.rewrite_if_else_completions(
                    &temp_name,
                    pat_hir_id,
                    &mut then_block,
                    &mut else_branches,
                );
                let if_stmt = hir::Stmt::new(
                    self.alloc_hir_id(),
                    hir::StmtKind::Expr(Box::new(hir::Expr {
                        hir_id: expr.hir_id,
                        kind: hir::ExprKind::IfElse(cond, then_block, else_branches),
                        ty: expr_ty,
                        span: expr.span,
                    })),
                    expr.span,
                );
                self.pending.push(if_stmt);
            }
            hir::ExprKind::Match(scrutinee, mut arms) => {
                let init = self.make_literal_none_expr();
                let let_stmt = self.make_let_stmt(&temp_name, pat_hir_id, init);
                self.pending.push(let_stmt);

                self.rewrite_match_completions(&temp_name, pat_hir_id, &mut arms);
                let match_stmt = hir::Stmt::new(
                    self.alloc_hir_id(),
                    hir::StmtKind::Expr(Box::new(hir::Expr {
                        hir_id: expr.hir_id,
                        kind: hir::ExprKind::Match(scrutinee, arms),
                        ty: expr_ty,
                        span: expr.span,
                    })),
                    expr.span,
                );
                self.pending.push(match_stmt);
            }
            hir::ExprKind::Block(mut block) => {
                let init = self.make_literal_none_expr();
                let let_stmt = self.make_let_stmt(&temp_name, pat_hir_id, init);
                self.pending.push(let_stmt);

                self.rewrite_block_completion(&temp_name, pat_hir_id, &mut block);
                self.pending.extend(block.stmts);
            }
            hir::ExprKind::Loop(mut block) => {
                let init = self.make_literal_none_expr();
                let let_stmt = self.make_let_stmt(&temp_name, pat_hir_id, init);
                self.pending.push(let_stmt);

                rewrite_break_values_in_block(&temp_name, pat_hir_id, &mut block, self);
                let loop_stmt = hir::Stmt::new(
                    self.alloc_hir_id(),
                    hir::StmtKind::Expr(Box::new(hir::Expr {
                        hir_id: expr.hir_id,
                        kind: hir::ExprKind::Loop(block),
                        ty: expr_ty,
                        span: expr.span,
                    })),
                    expr.span,
                );
                self.pending.push(loop_stmt);
            }
            // Non-control-flow: emit a direct `let $anf$N = expr;`
            _ => {
                let let_stmt = self.make_let_stmt(&temp_name, pat_hir_id, expr);
                self.pending.push(let_stmt);
            }
        }

        *self.changed = true;
        self.make_path_expr(&temp_name, pat_hir_id)
    }

    /// If `expr` needs lifting, lift it and return the Path reference.
    /// Otherwise, recursively fold its sub-expressions.
    /// Break/Continue are control flow — never lift them even though they
    /// are classified as non-expressible.
    /// Self-referencing TailCall is also control flow (becomes loop continuation).
    /// Mutual TailCall is a regular call and IS expressible.
    pub(crate) fn normalize_expr(&mut self, expr: hir::Expr) -> hir::Expr {
        if matches!(
            expr.kind,
            hir::ExprKind::Break(..) | hir::ExprKind::Continue
        ) {
            return fold::fold_expr(self, expr);
        }
        // Self-referencing TailCall becomes a loop continuation — never lift it.
        if let hir::ExprKind::TailCall(ref call) = expr.kind
            && self.is_self_referencing_tail_call(call)
        {
            return fold::fold_expr(self, expr);
        }
        if self.filter.needs_lifting(&expr) {
            let folded = self.fold_children(expr);
            // After folding children, non-expressible sub-expressions may have
            // been lifted into temps, making the expression expressible now.
            if self.filter.needs_lifting(&folded) {
                self.lift_expr(folded)
            } else {
                folded
            }
        } else {
            fold::fold_expr(self, expr)
        }
    }

    /// Normalize an expression for a binding position (let init, assignment RHS,
    /// return value).  Control-flow constructs (if/else, match, block, loop) are
    /// fully normalised via `normalize_expr` (which may lift them).  Operations
    /// are allowed to remain as the top-level expression as long as their
    /// sub-expressions are normalised.
    pub(crate) fn normalize_for_binding(&mut self, expr: hir::Expr) -> hir::Expr {
        match &expr.kind {
            // Control-flow constructs need full normalisation even in binding positions.
            hir::ExprKind::IfElse(..)
            | hir::ExprKind::Match(..)
            | hir::ExprKind::Block(..)
            | hir::ExprKind::Loop(..)
            | hir::ExprKind::Let(..)
            | hir::ExprKind::Break(..)
            | hir::ExprKind::Continue => self.normalize_expr(expr),
            // For operations, normalise sub-expressions (they must become atomic)
            // but keep the top-level operation in the binding position.
            _ => fold::fold_expr(self, expr),
        }
    }

    /// Fold the children of an expression without lifting the expression itself.
    pub(crate) fn fold_children(&mut self, expr: hir::Expr) -> hir::Expr {
        let expr_ty = expr.ty.clone();
        let kind = match expr.kind {
            hir::ExprKind::IfElse(cond, then_block, else_branches) => {
                let cond = self.normalize_expr(*cond);
                let then_block = self.fold_block(*then_block);
                let else_branches = else_branches
                    .into_iter()
                    .map(|clause| hir::ElseClause {
                        condition: clause.condition.map(|c| self.normalize_expr(c)),
                        consequence: self.fold_block(clause.consequence),
                    })
                    .collect();
                hir::ExprKind::IfElse(Box::new(cond), Box::new(then_block), else_branches)
            }
            hir::ExprKind::Match(scrutinee, arms) => {
                let scrutinee = self.normalize_expr(*scrutinee);
                let arms = arms
                    .into_iter()
                    .map(|arm| hir::MatchArm {
                        hir_id: arm.hir_id,
                        pat: arm.pat,
                        guard: arm.guard.map(|g| {
                            if let hir::ExprKind::Let(pat, inner) = g.kind {
                                // Preserve let-guards as-is so that the codegen can handle
                                // them via `generate_match_arm_guard`.  Only normalise the
                                // inner RHS expression; the let-guard node itself stays
                                // inline so the codegen sees the Let structure.
                                hir::Expr {
                                    hir_id: g.hir_id,
                                    kind: hir::ExprKind::Let(
                                        pat,
                                        Box::new(self.normalize_expr(*inner)),
                                    ),
                                    ty: g.ty,
                                    span: g.span,
                                }
                            } else {
                                // Regular guards may reference pattern-bound variables that
                                // are not available before the match expression.  Lifting
                                // them into the enclosing block (via normalize_expr) would
                                // evaluate them before the pattern binds those variables,
                                // producing incorrect results.  Pass the guard through
                                // unchanged so the codegen emits it inline in the condition.
                                g
                            }
                        }),
                        block: self.fold_block(arm.block),
                        pat_locals: arm.pat_locals,
                        leading_comments: arm.leading_comments,
                        trailing_comments: arm.trailing_comments,
                    })
                    .collect();
                hir::ExprKind::Match(Box::new(scrutinee), arms)
            }
            hir::ExprKind::Block(block) => hir::ExprKind::Block(Box::new(self.fold_block(*block))),
            hir::ExprKind::Loop(block) => hir::ExprKind::Loop(Box::new(self.fold_block(*block))),
            // Assignment RHS is a binding position: normalise sub-expressions
            // but don't lift the RHS operation itself.  This prevents infinite
            // loops when the ANF pass re-encounters its own generated assignments.
            hir::ExprKind::Binary(hir::BinaryOpKind::Assign, lhs, rhs) => {
                let lhs = fold::fold_expr(self, *lhs);
                let rhs = self.normalize_for_binding(*rhs);
                hir::ExprKind::Binary(hir::BinaryOpKind::Assign, Box::new(lhs), Box::new(rhs))
            }
            other => {
                return fold::fold_expr(
                    self,
                    hir::Expr {
                        hir_id: expr.hir_id,
                        kind: other,
                        ty: expr_ty,
                        span: expr.span,
                    },
                );
            }
        };
        hir::Expr {
            hir_id: expr.hir_id,
            kind,
            ty: expr_ty,
            span: expr.span,
        }
    }
}

/// Rewrite `break <value>` inside a loop to `{ __t = <value>; break; }`.
fn rewrite_break_values_in_block<F: AnfFilter>(
    temp_name: &str,
    pat_hir_id: tlang_span::HirId,
    block: &mut hir::Block,
    folder: &mut AnfFolder<F>,
) {
    for stmt in &mut block.stmts {
        rewrite_break_values_in_stmt(temp_name, pat_hir_id, stmt, folder);
    }
    if let Some(ref mut expr) = block.expr {
        rewrite_break_values_in_expr(temp_name, pat_hir_id, expr, folder);
    }
}

fn rewrite_break_values_in_stmt<F: AnfFilter>(
    temp_name: &str,
    pat_hir_id: tlang_span::HirId,
    stmt: &mut hir::Stmt,
    folder: &mut AnfFolder<F>,
) {
    match &mut stmt.kind {
        hir::StmtKind::Expr(expr) => {
            rewrite_break_values_in_expr(temp_name, pat_hir_id, expr, folder);
        }
        hir::StmtKind::Let(_, expr, _) => {
            rewrite_break_values_in_expr(temp_name, pat_hir_id, expr, folder);
        }
        hir::StmtKind::Const(_, _, expr, _) => {
            rewrite_break_values_in_expr(temp_name, pat_hir_id, expr, folder);
        }
        hir::StmtKind::Return(Some(expr)) => {
            rewrite_break_values_in_expr(temp_name, pat_hir_id, expr, folder);
        }
        hir::StmtKind::Return(None)
        | hir::StmtKind::FunctionDeclaration(_)
        | hir::StmtKind::DynFunctionDeclaration(_)
        | hir::StmtKind::EnumDeclaration(_)
        | hir::StmtKind::StructDeclaration(_)
        | hir::StmtKind::ProtocolDeclaration(_)
        | hir::StmtKind::ImplBlock(_) => {}
    }
}

fn rewrite_break_values_in_expr<F: AnfFilter>(
    temp_name: &str,
    pat_hir_id: tlang_span::HirId,
    expr: &mut hir::Expr,
    folder: &mut AnfFolder<F>,
) {
    match &mut expr.kind {
        hir::ExprKind::Break(Some(inner)) => {
            let value = std::mem::replace(
                inner.as_mut(),
                hir::Expr {
                    hir_id: folder.alloc_hir_id(),
                    kind: hir::ExprKind::Wildcard,
                    ty: hir::Ty::unknown(),
                    span: Span::default(),
                },
            );
            let assign = folder.make_assign_stmt(temp_name, pat_hir_id, value);
            let break_stmt = hir::Stmt::new(
                folder.alloc_hir_id(),
                hir::StmtKind::Expr(Box::new(hir::Expr {
                    hir_id: expr.hir_id,
                    kind: hir::ExprKind::Break(None),
                    ty: expr.ty.clone(),
                    span: expr.span,
                })),
                expr.span,
            );
            let block = hir::Block::new(
                folder.alloc_hir_id(),
                vec![assign, break_stmt],
                None,
                expr.span,
            );
            expr.kind = hir::ExprKind::Block(Box::new(block));
        }
        hir::ExprKind::IfElse(cond, then_block, else_branches) => {
            rewrite_break_values_in_expr(temp_name, pat_hir_id, cond, folder);
            rewrite_break_values_in_block(temp_name, pat_hir_id, then_block, folder);
            for clause in else_branches.iter_mut() {
                if let Some(ref mut c) = clause.condition {
                    rewrite_break_values_in_expr(temp_name, pat_hir_id, c, folder);
                }
                rewrite_break_values_in_block(
                    temp_name,
                    pat_hir_id,
                    &mut clause.consequence,
                    folder,
                );
            }
        }
        hir::ExprKind::Match(scrutinee, arms) => {
            rewrite_break_values_in_expr(temp_name, pat_hir_id, scrutinee, folder);
            for arm in arms.iter_mut() {
                if let Some(ref mut g) = arm.guard {
                    rewrite_break_values_in_expr(temp_name, pat_hir_id, g, folder);
                }
                rewrite_break_values_in_block(temp_name, pat_hir_id, &mut arm.block, folder);
            }
        }
        hir::ExprKind::Block(block) | hir::ExprKind::Loop(block) => {
            rewrite_break_values_in_block(temp_name, pat_hir_id, block, folder);
        }
        hir::ExprKind::Binary(_, lhs, rhs) => {
            rewrite_break_values_in_expr(temp_name, pat_hir_id, lhs, folder);
            rewrite_break_values_in_expr(temp_name, pat_hir_id, rhs, folder);
        }
        hir::ExprKind::Unary(_, inner)
        | hir::ExprKind::Cast(inner, _)
        | hir::ExprKind::TryCast(inner, _) => {
            rewrite_break_values_in_expr(temp_name, pat_hir_id, inner, folder);
        }
        hir::ExprKind::Call(call) | hir::ExprKind::TailCall(call) => {
            rewrite_break_values_in_expr(temp_name, pat_hir_id, &mut call.callee, folder);
            for arg in &mut call.arguments {
                rewrite_break_values_in_expr(temp_name, pat_hir_id, arg, folder);
            }
        }
        _ => {}
    }
}

impl<F: AnfFilter> Folder for AnfFolder<'_, F> {
    fn fold_block(&mut self, block: hir::Block) -> hir::Block {
        // Save any items accumulated by the caller before entering this block.
        // fold_block uses self.pending as a local accumulator; items belonging to
        // the enclosing scope (e.g. lifted match scrutinees or let-guard RHSes)
        // must survive the block's internal pending.clear() calls.
        let outer_pending = std::mem::take(&mut self.pending);

        let mut new_stmts = Vec::with_capacity(block.stmts.len());

        for stmt in block.stmts {
            self.pending.clear();
            let folded = self.fold_stmt(stmt);
            new_stmts.append(&mut self.pending);
            new_stmts.extend(folded);
        }

        self.pending.clear();
        let new_expr = block.expr.map(|expr| self.normalize_for_binding(expr));
        new_stmts.append(&mut self.pending);

        // Restore the caller's pending items.
        self.pending = outer_pending;

        hir::Block {
            hir_id: block.hir_id,
            stmts: new_stmts,
            expr: new_expr,
            scope: block.scope,
            span: block.span,
        }
    }

    fn fold_stmt(&mut self, stmt: hir::Stmt) -> SmallVec<hir::Stmt> {
        let kind = match stmt.kind {
            hir::StmtKind::Expr(expr) => {
                // Statement-level expressions: fold children but don't lift the
                // top-level expression itself (it's OK for if/match to be stmts).
                hir::StmtKind::Expr(Box::new(self.fold_children(*expr)))
            }
            hir::StmtKind::Let(pat, expr, ty) => {
                hir::StmtKind::Let(pat, Box::new(self.normalize_for_binding(*expr)), ty)
            }
            hir::StmtKind::Return(Some(expr)) => {
                hir::StmtKind::Return(Some(Box::new(self.normalize_for_binding(*expr))))
            }
            hir::StmtKind::FunctionDeclaration(decl) => {
                hir::StmtKind::FunctionDeclaration(Box::new(fold_function_decl(self, *decl)))
            }
            hir::StmtKind::ImplBlock(mut impl_block) => {
                impl_block.methods = impl_block
                    .methods
                    .into_iter()
                    .map(|decl| fold_function_decl(self, decl))
                    .collect();
                hir::StmtKind::ImplBlock(impl_block)
            }
            other => other,
        };

        SmallVec::one(hir::Stmt {
            hir_id: stmt.hir_id,
            kind,
            span: stmt.span,
            leading_comments: stmt.leading_comments,
            trailing_comments: stmt.trailing_comments,
        })
    }

    fn fold_expr(&mut self, expr: hir::Expr) -> hir::Expr {
        // Intercept FunctionExpression to set the function name context.
        if let hir::ExprKind::FunctionExpression(decl) = expr.kind {
            let folded_decl = fold_function_decl(self, *decl);
            return hir::Expr {
                hir_id: expr.hir_id,
                kind: hir::ExprKind::FunctionExpression(Box::new(folded_decl)),
                ty: expr.ty,
                span: expr.span,
            };
        }
        self.normalize_expr(expr)
    }
}

fn fold_function_decl<F: AnfFilter>(
    folder: &mut AnfFolder<F>,
    decl: hir::FunctionDeclaration,
) -> hir::FunctionDeclaration {
    let prev_fn_hir_id = folder.current_function_hir_id.take();
    // Store the function's HirId for self-referencing TailCall detection.
    folder.current_function_hir_id = Some(decl.hir_id);
    let body = folder.fold_block(decl.body);
    folder.current_function_hir_id = prev_fn_hir_id;
    hir::FunctionDeclaration {
        hir_id: decl.hir_id,
        visibility: decl.visibility,
        name: decl.name,
        type_params: decl.type_params,
        parameters: decl.parameters,
        params_span: decl.params_span,
        return_type: decl.return_type,
        has_return_type: decl.has_return_type,
        body,
        span: decl.span,
    }
}

impl<F: AnfFilter + Default> HirPass for AnfTransform<F> {
    fn name(&self) -> &'static str {
        std::any::type_name::<Self>()
    }

    fn optimize_hir(
        &mut self,
        module: &mut hir::Module,
        ctx: &mut HirOptContext,
    ) -> Result<bool, HirOptError> {
        self.changed = false;
        let mut folder = AnfFolder {
            ctx,
            counter: &mut self.counter,
            pending: Vec::new(),
            changed: &mut self.changed,
            current_function_hir_id: None,
            filter: &self.filter,
        };

        let old_module = std::mem::take(module);
        *module = folder.fold_module(old_module);
        Ok(self.changed)
    }
}
