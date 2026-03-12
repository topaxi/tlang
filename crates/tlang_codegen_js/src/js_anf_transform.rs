use tlang_ast::node::Ident;
use tlang_ast::token::Literal;
use tlang_hir::fold::{self, Folder, SmallVec};
use tlang_hir::{self as hir};
use tlang_hir_opt::hir_opt::{HirOptContext, HirPass};
use tlang_span::Span;

/// Returns true if the expression can be directly emitted as a JavaScript expression.
/// Mirrors `expr_can_render_as_js_expr` in the codegen.
fn is_js_expressible(expr: &hir::Expr) -> bool {
    match &expr.kind {
        hir::ExprKind::Path(..) | hir::ExprKind::Literal(..) | hir::ExprKind::Wildcard => true,
        hir::ExprKind::Binary(_, lhs, rhs) => is_js_expressible(lhs) && is_js_expressible(rhs),
        hir::ExprKind::Unary(_, inner) => is_js_expressible(inner),
        hir::ExprKind::Call(call) | hir::ExprKind::TailCall(call) => {
            is_js_expressible(&call.callee) && call.arguments.iter().all(is_js_expressible)
        }
        hir::ExprKind::Cast(inner, _) => is_js_expressible(inner),
        hir::ExprKind::FieldAccess(base, _) => is_js_expressible(base),
        hir::ExprKind::IndexAccess(base, index) => {
            is_js_expressible(base) && is_js_expressible(index)
        }
        hir::ExprKind::List(items) => items.iter().all(is_js_expressible),
        hir::ExprKind::Dict(pairs) => pairs
            .iter()
            .all(|(k, v)| is_js_expressible(k) && is_js_expressible(v)),
        hir::ExprKind::FunctionExpression(..) | hir::ExprKind::Range(..) => true,
        hir::ExprKind::IfElse(cond, then_block, else_branches) => {
            can_render_as_ternary(cond, then_block, else_branches)
        }
        hir::ExprKind::Let(..)
        | hir::ExprKind::Block(..)
        | hir::ExprKind::Loop(..)
        | hir::ExprKind::Break(..)
        | hir::ExprKind::Continue
        | hir::ExprKind::Match(..) => false,
    }
}

/// Check whether an if/else can be rendered as a JS ternary expression.
/// Must match the codegen's `should_render_if_else_as_ternary` constraints:
/// - Exactly one else branch (no nested else-if chains)
/// - All branches have expressible completions with no statements
fn can_render_as_ternary(
    cond: &hir::Expr,
    then_block: &hir::Block,
    else_branches: &[hir::ElseClause],
) -> bool {
    // The codegen won't nest ternary expressions for else-if chains.
    else_branches.len() == 1
        && else_branches[0].condition.is_none()
        && then_block.stmts.is_empty()
        && then_block.expr.as_ref().is_some_and(is_js_expressible)
        && is_js_expressible(cond)
        && else_branches[0].consequence.stmts.is_empty()
        && else_branches[0]
            .consequence
            .expr
            .as_ref()
            .is_some_and(is_js_expressible)
}

/// Returns true if the expression itself is non-JS-expressible (needs lifting).
fn needs_lifting(expr: &hir::Expr) -> bool {
    !is_js_expressible(expr)
}

/// A selective ANF pass that only lifts expressions which cannot be expressed
/// as JavaScript expressions (if/else with completions, match, block, etc.)
/// when they appear in value positions (let initialisers, call arguments,
/// binary operands, etc.).
///
/// After this pass, every expression in a value position is JS-expressible.
#[derive(Default)]
pub struct JsAnfTransform {
    counter: usize,
    changed: bool,
}

/// The folder that carries accumulated pending statements and the HIR context.
struct AnfFolder<'a> {
    ctx: &'a mut HirOptContext,
    counter: &'a mut usize,
    /// Statements to inject before the current position in a block.
    pending: Vec<hir::Stmt>,
    changed: &'a mut bool,
    /// Name of the function currently being folded (for self-referencing TailCall detection).
    current_function_name: Option<String>,
}

impl<'a> AnfFolder<'a> {
    fn alloc_hir_id(&mut self) -> tlang_span::HirId {
        self.ctx.hir_id_allocator.next_id()
    }

    fn fresh_anf_var(&mut self) -> (String, tlang_span::HirId) {
        let n = *self.counter;
        *self.counter += 1;
        let name = format!("__anf_{n}");
        let pat_hir_id = self.alloc_hir_id();
        (name, pat_hir_id)
    }

    /// Returns true if the TailCall is self-referencing (callee matches the
    /// current function). Only self-referencing TailCalls are converted to
    /// loops by the codegen; mutual TailCalls are regular calls.
    fn is_self_referencing_tail_call(&self, call: &hir::CallExpression) -> bool {
        if let Some(ref fn_name) = self.current_function_name
            && let hir::ExprKind::Path(ref path) = call.callee.kind
        {
            return path.last_ident().as_str() == fn_name;
        }
        false
    }

    fn make_path_expr(&mut self, name: &str, pat_hir_id: tlang_span::HirId) -> hir::Expr {
        let hir_id = self.alloc_hir_id();
        let mut path = hir::Path::new(
            vec![hir::PathSegment::from_str(name, Span::default())],
            Span::default(),
        );
        path.res = hir::Res::new_local(pat_hir_id);
        hir::Expr {
            hir_id,
            kind: hir::ExprKind::Path(Box::new(path)),
            span: Span::default(),
        }
    }

    fn make_literal_none_expr(&mut self) -> hir::Expr {
        let hir_id = self.alloc_hir_id();
        hir::Expr {
            hir_id,
            kind: hir::ExprKind::Literal(Box::new(Literal::None)),
            span: Span::default(),
        }
    }

    fn make_assign_stmt(
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
                span: Span::default(),
            })),
            Span::default(),
        )
    }

    fn make_let_stmt(
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
    fn rewrite_if_else_completions(
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
    fn rewrite_block_completion(
        &mut self,
        temp_name: &str,
        pat_hir_id: tlang_span::HirId,
        block: &mut hir::Block,
    ) {
        if let Some(completion) = block.expr.take() {
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
    fn rewrite_match_completions(
        &mut self,
        temp_name: &str,
        pat_hir_id: tlang_span::HirId,
        arms: &mut [hir::MatchArm],
    ) {
        for arm in arms.iter_mut() {
            self.rewrite_block_completion(temp_name, pat_hir_id, &mut arm.block);
        }
    }

    /// Lift a non-JS-expressible expression out of a value position.
    /// Pushes the necessary preceding stmts into `self.pending` and returns
    /// a Path reference to the temp variable.
    fn lift_expr(&mut self, expr: hir::Expr) -> hir::Expr {
        let (temp_name, pat_hir_id) = self.fresh_anf_var();
        let init = self.make_literal_none_expr();
        let let_stmt = self.make_let_stmt(&temp_name, pat_hir_id, init);
        self.pending.push(let_stmt);

        match expr.kind {
            hir::ExprKind::IfElse(cond, mut then_block, mut else_branches) => {
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
                        span: expr.span,
                    })),
                    expr.span,
                );
                self.pending.push(if_stmt);
            }
            hir::ExprKind::Match(scrutinee, mut arms) => {
                self.rewrite_match_completions(&temp_name, pat_hir_id, &mut arms);
                let match_stmt = hir::Stmt::new(
                    self.alloc_hir_id(),
                    hir::StmtKind::Expr(Box::new(hir::Expr {
                        hir_id: expr.hir_id,
                        kind: hir::ExprKind::Match(scrutinee, arms),
                        span: expr.span,
                    })),
                    expr.span,
                );
                self.pending.push(match_stmt);
            }
            hir::ExprKind::Block(mut block) => {
                self.rewrite_block_completion(&temp_name, pat_hir_id, &mut block);
                self.pending.extend(block.stmts);
            }
            hir::ExprKind::Loop(mut block) => {
                rewrite_break_values_in_block(&temp_name, pat_hir_id, &mut block, self);
                let loop_stmt = hir::Stmt::new(
                    self.alloc_hir_id(),
                    hir::StmtKind::Expr(Box::new(hir::Expr {
                        hir_id: expr.hir_id,
                        kind: hir::ExprKind::Loop(block),
                        span: expr.span,
                    })),
                    expr.span,
                );
                self.pending.push(loop_stmt);
            }
            _ => {
                let assign = self.make_assign_stmt(&temp_name, pat_hir_id, expr);
                self.pending.push(assign);
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
    /// Mutual TailCall is a regular call and IS JS-expressible.
    fn normalize_expr(&mut self, expr: hir::Expr) -> hir::Expr {
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
        if needs_lifting(&expr) {
            let folded = self.fold_children(expr);
            // After folding children, non-expressible sub-expressions may have
            // been lifted into temps, making the expression expressible now.
            if needs_lifting(&folded) {
                self.lift_expr(folded)
            } else {
                folded
            }
        } else {
            fold::fold_expr(self, expr)
        }
    }

    /// Fold the children of an expression without lifting the expression itself.
    fn fold_children(&mut self, expr: hir::Expr) -> hir::Expr {
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
                        guard: arm.guard.map(|g| self.normalize_expr(g)),
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
            other => {
                return fold::fold_expr(
                    self,
                    hir::Expr {
                        hir_id: expr.hir_id,
                        kind: other,
                        span: expr.span,
                    },
                );
            }
        };
        hir::Expr {
            hir_id: expr.hir_id,
            kind,
            span: expr.span,
        }
    }
}

/// Rewrite `break <value>` inside a loop to `{ __t = <value>; break; }`.
fn rewrite_break_values_in_block(
    temp_name: &str,
    pat_hir_id: tlang_span::HirId,
    block: &mut hir::Block,
    folder: &mut AnfFolder,
) {
    for stmt in &mut block.stmts {
        rewrite_break_values_in_stmt(temp_name, pat_hir_id, stmt, folder);
    }
    if let Some(ref mut expr) = block.expr {
        rewrite_break_values_in_expr(temp_name, pat_hir_id, expr, folder);
    }
}

fn rewrite_break_values_in_stmt(
    temp_name: &str,
    pat_hir_id: tlang_span::HirId,
    stmt: &mut hir::Stmt,
    folder: &mut AnfFolder,
) {
    match &mut stmt.kind {
        hir::StmtKind::Expr(expr) => {
            rewrite_break_values_in_expr(temp_name, pat_hir_id, expr, folder);
        }
        hir::StmtKind::Let(_, expr, _) => {
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

fn rewrite_break_values_in_expr(
    temp_name: &str,
    pat_hir_id: tlang_span::HirId,
    expr: &mut hir::Expr,
    folder: &mut AnfFolder,
) {
    match &mut expr.kind {
        hir::ExprKind::Break(Some(inner)) => {
            let value = std::mem::replace(
                inner.as_mut(),
                hir::Expr {
                    hir_id: folder.alloc_hir_id(),
                    kind: hir::ExprKind::Wildcard,
                    span: Span::default(),
                },
            );
            let assign = folder.make_assign_stmt(temp_name, pat_hir_id, value);
            let break_stmt = hir::Stmt::new(
                folder.alloc_hir_id(),
                hir::StmtKind::Expr(Box::new(hir::Expr {
                    hir_id: expr.hir_id,
                    kind: hir::ExprKind::Break(None),
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
        hir::ExprKind::Unary(_, inner) | hir::ExprKind::Cast(inner, _) => {
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

impl Folder for AnfFolder<'_> {
    fn fold_block(&mut self, block: hir::Block) -> hir::Block {
        let mut new_stmts = Vec::with_capacity(block.stmts.len());

        for stmt in block.stmts {
            self.pending.clear();
            let folded = self.fold_stmt(stmt);
            new_stmts.append(&mut self.pending);
            new_stmts.extend(folded);
        }

        self.pending.clear();
        let new_expr = block.expr.map(|expr| self.normalize_expr(expr));
        new_stmts.append(&mut self.pending);

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
                hir::StmtKind::Let(pat, Box::new(self.normalize_expr(*expr)), ty)
            }
            hir::StmtKind::Return(Some(expr)) => {
                hir::StmtKind::Return(Some(Box::new(self.normalize_expr(*expr))))
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
                span: expr.span,
            };
        }
        self.normalize_expr(expr)
    }
}

fn fold_function_decl(
    folder: &mut AnfFolder,
    decl: hir::FunctionDeclaration,
) -> hir::FunctionDeclaration {
    let prev_fn_name = folder.current_function_name.take();
    // Extract function name for self-referencing TailCall detection.
    if let hir::ExprKind::Path(ref path) = decl.name.kind {
        folder.current_function_name = Some(path.last_ident().to_string());
    }
    let body = folder.fold_block(decl.body);
    folder.current_function_name = prev_fn_name;
    hir::FunctionDeclaration {
        hir_id: decl.hir_id,
        name: decl.name,
        parameters: decl.parameters,
        return_type: decl.return_type,
        body,
        span: decl.span,
    }
}

impl HirPass for JsAnfTransform {
    fn name(&self) -> &'static str {
        "JsAnfTransform"
    }

    fn optimize_hir(&mut self, module: &mut hir::Module, ctx: &mut HirOptContext) -> bool {
        self.changed = false;
        let mut folder = AnfFolder {
            ctx,
            counter: &mut self.counter,
            pending: Vec::new(),
            changed: &mut self.changed,
            current_function_name: None,
        };

        let old_module = std::mem::take(module);
        *module = folder.fold_module(old_module);
        self.changed
    }
}
