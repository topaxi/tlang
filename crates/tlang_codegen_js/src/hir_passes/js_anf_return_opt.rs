use tlang_ast::token::Literal;
use tlang_hir::{self as hir};
use tlang_hir_opt::hir_opt::{HirOptContext, HirOptError, HirPass};

/// Post-ANF cleanup pass that detects the "return via ANF temp" pattern and
/// replaces it with direct `return` statements in each control-flow branch.
///
/// The ANF transform lifts non-expressible completions in return position into:
/// ```text
/// let $anf$N;
/// if (...) { $anf$N = val1; } else { $anf$N = val2; }
/// return $anf$N;          // or block.expr = $anf$N
/// ```
///
/// This pass rewrites those into idiomatic:
/// ```text
/// if (...) { return val1; } else { return val2; }
/// ```
#[derive(Default)]
pub struct JsAnfReturnOpt {
    changed: bool,
}

impl HirPass for JsAnfReturnOpt {
    fn name(&self) -> &'static str {
        "JsAnfReturnOpt"
    }

    fn optimize_hir(
        &mut self,
        module: &mut hir::Module,
        _ctx: &mut HirOptContext,
    ) -> Result<bool, HirOptError> {
        self.changed = false;
        self.visit_block(&mut module.block);
        Ok(self.changed)
    }
}

// ---------------------------------------------------------------------------
// Visitor: walk the entire HIR to find function bodies
// ---------------------------------------------------------------------------

impl JsAnfReturnOpt {
    fn visit_block(&mut self, block: &mut hir::Block) {
        for stmt in &mut block.stmts {
            self.visit_stmt(stmt);
        }
        if let Some(ref mut expr) = block.expr {
            self.visit_expr(expr);
        }
    }

    fn visit_stmt(&mut self, stmt: &mut hir::Stmt) {
        match &mut stmt.kind {
            hir::StmtKind::FunctionDeclaration(decl) => {
                self.visit_fn_body(&mut decl.body);
            }
            hir::StmtKind::ImplBlock(impl_block) => {
                for method in &mut impl_block.methods {
                    self.visit_fn_body(&mut method.body);
                }
            }
            hir::StmtKind::ProtocolDeclaration(decl) => {
                for method in &mut decl.methods {
                    if let Some(ref mut body) = method.body {
                        self.visit_fn_body(body);
                    }
                }
            }
            hir::StmtKind::Expr(expr) => self.visit_expr(expr),
            hir::StmtKind::Let(_, expr, _) => self.visit_expr(expr),
            hir::StmtKind::Return(Some(expr)) => self.visit_expr(expr),
            _ => {}
        }
    }

    fn visit_expr(&mut self, expr: &mut hir::Expr) {
        match &mut expr.kind {
            hir::ExprKind::FunctionExpression(decl) => {
                self.visit_fn_body(&mut decl.body);
            }
            hir::ExprKind::IfElse(cond, then_block, else_branches) => {
                self.visit_expr(cond);
                self.visit_block(then_block);
                for clause in else_branches {
                    if let Some(ref mut c) = clause.condition {
                        self.visit_expr(c);
                    }
                    self.visit_block(&mut clause.consequence);
                }
            }
            hir::ExprKind::Match(scrutinee, arms) => {
                self.visit_expr(scrutinee);
                for arm in arms {
                    self.visit_block(&mut arm.block);
                }
            }
            hir::ExprKind::Block(block) | hir::ExprKind::Loop(block) => {
                self.visit_block(block);
            }
            hir::ExprKind::Call(call) | hir::ExprKind::TailCall(call) => {
                self.visit_expr(&mut call.callee);
                for arg in &mut call.arguments {
                    self.visit_expr(arg);
                }
            }
            hir::ExprKind::Binary(_, lhs, rhs) => {
                self.visit_expr(lhs);
                self.visit_expr(rhs);
            }
            hir::ExprKind::Unary(_, inner)
            | hir::ExprKind::Cast(inner, _)
            | hir::ExprKind::TryCast(inner, _) => {
                self.visit_expr(inner);
            }
            hir::ExprKind::FieldAccess(base, _) => self.visit_expr(base),
            hir::ExprKind::IndexAccess(base, index) => {
                self.visit_expr(base);
                self.visit_expr(index);
            }
            hir::ExprKind::List(items) => {
                for item in items {
                    self.visit_expr(item);
                }
            }
            hir::ExprKind::Dict(pairs) => {
                for (k, v) in pairs {
                    self.visit_expr(k);
                    self.visit_expr(v);
                }
            }
            _ => {}
        }
    }

    /// Visit a function body: first recurse into nested functions, then try to
    /// optimize the body itself.
    fn visit_fn_body(&mut self, body: &mut hir::Block) {
        self.visit_block(body);
        self.try_optimize_fn_body(body);
    }
}

// ---------------------------------------------------------------------------
// Pattern detection + rewriting
// ---------------------------------------------------------------------------

impl JsAnfReturnOpt {
    /// Detect and apply the return-via-ANF-temp optimization on a function body.
    fn try_optimize_fn_body(&mut self, body: &mut hir::Block) {
        // Pattern A: block.expr = Some(Path("$anf$N"))
        // Pattern B: last stmt is Return(Path("$anf$N"))
        let (anf_name, is_pattern_b) = if let Some(name) =
            body.expr.as_ref().and_then(get_anf_var_name)
        {
            (name.to_string(), false)
        } else if let Some(hir::StmtKind::Return(Some(expr))) = body.stmts.last().map(|s| &s.kind) {
            if let Some(name) = get_anf_var_name(expr) {
                (name.to_string(), true)
            } else {
                return;
            }
        } else {
            return;
        };

        // Pattern A needs at least 2 stmts (let + control-flow).
        // Pattern B needs at least 3 stmts (let + control-flow + return).
        let stmts_len = body.stmts.len();
        let min_stmts = if is_pattern_b { 3 } else { 2 };
        if stmts_len < min_stmts {
            return;
        }

        // Locate the control-flow stmt and the let declaration.
        let cf_idx = if is_pattern_b {
            stmts_len - 2
        } else {
            stmts_len - 1
        };
        let let_idx = cf_idx - 1;

        if !is_anf_let_decl(&body.stmts[let_idx], &anf_name) {
            return;
        }
        if !is_control_flow_stmt(&body.stmts[cf_idx]) {
            return;
        }

        // Rewrite terminal `$anf$N = value` assignments → `return value;`
        rewrite_assignments_to_returns(&anf_name, &mut body.stmts[cf_idx]);

        // Clean up: remove the let declaration and the return/completion.
        if is_pattern_b {
            body.stmts.pop(); // Remove Return($anf$N)
        } else {
            body.expr = None; // Clear the ANF-var completion
        }
        body.stmts.remove(let_idx); // Remove let $anf$N;

        self.changed = true;
    }
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Extract the ANF variable name from a `Path("$anf$N")` expression.
fn get_anf_var_name(expr: &hir::Expr) -> Option<&str> {
    if let hir::ExprKind::Path(path) = &expr.kind {
        let name = path.last_ident().as_str();
        if name.starts_with("$anf$") {
            return Some(name);
        }
    }
    None
}

/// Check if a statement is `let $anf$N;` (i.e. `Let(Identifier($anf$N), Literal(None), _)`).
fn is_anf_let_decl(stmt: &hir::Stmt, anf_name: &str) -> bool {
    if let hir::StmtKind::Let(pat, init, _) = &stmt.kind
        && let hir::PatKind::Identifier(_, ident) = &pat.kind
    {
        return ident.as_str() == anf_name
            && matches!(&init.kind, hir::ExprKind::Literal(l) if **l == Literal::None);
    }
    false
}

/// Check if a statement is an expression-level IfElse or Match (control-flow).
fn is_control_flow_stmt(stmt: &hir::Stmt) -> bool {
    if let hir::StmtKind::Expr(expr) = &stmt.kind {
        matches!(
            expr.kind,
            hir::ExprKind::IfElse(..) | hir::ExprKind::Match(..)
        )
    } else {
        false
    }
}

/// Check if a statement is `$anf$N = value;`.
fn is_anf_assignment(stmt: &hir::Stmt, anf_name: &str) -> bool {
    if let hir::StmtKind::Expr(expr) = &stmt.kind
        && let hir::ExprKind::Binary(hir::BinaryOpKind::Assign, lhs, _) = &expr.kind
    {
        return get_anf_var_name(lhs) == Some(anf_name);
    }
    false
}

/// Rewrite assignments to the ANF var inside a control-flow statement into
/// `Return(value)` statements.
fn rewrite_assignments_to_returns(anf_name: &str, stmt: &mut hir::Stmt) {
    if let hir::StmtKind::Expr(expr) = &mut stmt.kind {
        match &mut expr.kind {
            hir::ExprKind::IfElse(_, then_block, else_branches) => {
                rewrite_block_terminal(anf_name, then_block);
                for clause in else_branches {
                    rewrite_block_terminal(anf_name, &mut clause.consequence);
                }
            }
            hir::ExprKind::Match(_, arms) => {
                for arm in arms {
                    rewrite_block_terminal(anf_name, &mut arm.block);
                }
            }
            _ => {}
        }
    }
}

/// Rewrite the terminal statement of a branch block:
/// - `$anf$N = value;` → `return value;`
/// - Nested IfElse/Match → recurse into their branches
/// - Other stmts (TailCall, Break, Continue) → leave as-is
fn rewrite_block_terminal(anf_name: &str, block: &mut hir::Block) {
    let Some(last_stmt) = block.stmts.last_mut() else {
        return;
    };

    if is_anf_assignment(last_stmt, anf_name) {
        replace_assignment_with_return(last_stmt);
    } else if let hir::StmtKind::Expr(expr) = &mut last_stmt.kind {
        match &mut expr.kind {
            hir::ExprKind::IfElse(_, then_block, else_branches) => {
                rewrite_block_terminal(anf_name, then_block);
                for clause in else_branches {
                    rewrite_block_terminal(anf_name, &mut clause.consequence);
                }
            }
            hir::ExprKind::Match(_, arms) => {
                for arm in arms {
                    rewrite_block_terminal(anf_name, &mut arm.block);
                }
            }
            _ => {}
        }
    }
}

/// Replace `Expr(Binary(Assign, Path($anf$N), value))` with `Return(Some(value))`.
fn replace_assignment_with_return(stmt: &mut hir::Stmt) {
    let old_kind = std::mem::replace(&mut stmt.kind, hir::StmtKind::Return(None));

    if let hir::StmtKind::Expr(expr) = old_kind
        && let hir::ExprKind::Binary(hir::BinaryOpKind::Assign, _, value) = expr.kind
    {
        stmt.kind = hir::StmtKind::Return(Some(value));
        return;
    }

    unreachable!("replace_assignment_with_return called on non-assignment");
}
