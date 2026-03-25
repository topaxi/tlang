//! Free variable analysis for closures.
//!
//! This pass runs after slot allocation. For each `FunctionExpression` (closure)
//! and `FunctionDeclaration` (named nested function) in the HIR, it:
//!
//! 1. Collects all `Slot::Upvar` references in the body, normalizing their
//!    `scope_index` relative to the closure's function scope (block depth 0).
//! 2. Lifts transitive captures: if a nested closure captures a variable from
//!    a scope above the current closure, the current closure also captures it.
//! 3. Stores the resulting `Vec<CaptureInfo>` on `HirScopeData::captures`.
//!
//! ## Normalized `scope_index`
//!
//! `Slot::Upvar(slot, scope_index)` uses a scope_index relative to the
//! *reference site* inside the closure body. The same captured variable gets
//! different `scope_index` values depending on how many nested scopes surround
//! the reference (e.g., 1 at the function body level, 2 inside a match arm).
//!
//! `CaptureInfo.scope_index` stores the *normalized* value:
//!
//! ```text
//! normalized = raw_scope_index - block_depth_within_function
//! ```
//!
//! This value is unique per captured variable and stable across reference sites,
//! making it usable as a key for selective capture in a later phase.

use std::collections::BTreeSet;

use tlang_hir::visit::{walk_block, walk_expr, walk_pat, walk_stmt};
use tlang_hir::{self as hir, CaptureInfo, Visitor};

use crate::HirPass;
use crate::hir_opt::{HirOptContext, HirOptError};

// ---------------------------------------------------------------------------
// CaptureCollector
// ---------------------------------------------------------------------------

/// Single-pass collector that walks a closure body and accumulates
/// normalized capture information.
///
/// Must be driven with [`walk_block`] (not [`Visitor::visit_block`]) on the
/// function body so the body block itself does not increment `block_depth`.
struct CaptureCollector {
    /// Deduplicated `(normalized_scope_index, slot_index)` captures.
    upvars: BTreeSet<(hir::ScopeIndex, hir::SlotIndex)>,
    /// Runtime-scope depth relative to the closure's function body (depth 0).
    block_depth: u16,
}

impl CaptureCollector {
    fn new() -> Self {
        Self {
            upvars: BTreeSet::new(),
            block_depth: 0,
        }
    }

    fn add_upvar(&mut self, slot_index: hir::SlotIndex, scope_index: hir::ScopeIndex) {
        let normalized = scope_index.saturating_sub(self.block_depth);
        self.upvars.insert((normalized, slot_index));
    }

    /// Lift transitive captures from a directly-nested function.
    ///
    /// If the nested function has a capture with `scope_index > 1`, that
    /// variable comes from a scope above *this* closure, so we also capture
    /// it at `scope_index - 1` to ensure GC can trace the full chain.
    fn lift_from(&mut self, nested: &hir::FunctionDeclaration) {
        for cap in nested.body.scope.captures() {
            if cap.scope_index > 1 {
                self.upvars.insert((cap.scope_index - 1, cap.slot_index));
            }
        }
    }
}

impl<'hir> Visitor<'hir> for CaptureCollector {
    type Context = ();

    fn visit_stmt(&mut self, stmt: &'hir mut hir::Stmt, ctx: &mut ()) {
        if let hir::StmtKind::FunctionDeclaration(nested) = &stmt.kind {
            // Named nested function — stop recursion, lift transitive captures.
            self.lift_from(nested);
            return;
        }
        walk_stmt(self, stmt, ctx);
    }

    fn visit_expr(&mut self, expr: &'hir mut hir::Expr, ctx: &mut ()) {
        match &mut expr.kind {
            hir::ExprKind::FunctionExpression(nested) => {
                // Anonymous closure — stop recursion, lift transitive captures.
                self.lift_from(nested);
            }
            hir::ExprKind::Match(discriminant, arms) => {
                self.visit_expr(discriminant, ctx);
                for arm in arms {
                    let has_pat_scope = arm.hir_id != arm.block.hir_id;
                    // The arm scope is always pushed before pattern binding and
                    // guard evaluation, regardless of whether it's separate from
                    // the block scope.
                    self.block_depth += 1;
                    walk_pat(self, &mut arm.pat, ctx);
                    if let Some(guard) = &mut arm.guard {
                        self.visit_expr(guard, ctx);
                    }
                    if has_pat_scope {
                        // Separate block scope inside the arm scope; visit_block
                        // adds another level.
                        self.visit_block(&mut arm.block, ctx);
                    } else {
                        // Arm scope and block scope are the same scope; walk
                        // directly to avoid a redundant depth increment.
                        walk_block(self, &mut arm.block, ctx);
                    }
                    self.block_depth -= 1;
                }
            }
            _ => walk_expr(self, expr, ctx),
        }
    }

    fn visit_block(&mut self, block: &'hir mut hir::Block, ctx: &mut ()) {
        self.block_depth += 1;
        walk_block(self, block, ctx);
        self.block_depth -= 1;
    }

    fn visit_path(&mut self, path: &'hir mut hir::Path, _ctx: &mut ()) {
        if let hir::Slot::Upvar(slot_index, scope_index) = path.res.slot() {
            self.add_upvar(slot_index, scope_index);
        }
    }
}

// ---------------------------------------------------------------------------
// Top-level pass
// ---------------------------------------------------------------------------

#[derive(Debug, Default)]
pub struct FreeVariableAnalysis;

impl FreeVariableAnalysis {
    fn process_function_decl(decl: &mut hir::FunctionDeclaration) {
        // Clear any stale captures from a previous run.
        decl.body.scope.set_captures(vec![]);

        let mut collector = CaptureCollector::new();
        // Use walk_block directly (not visit_block) so the function body itself
        // does not increment block_depth — it is at depth 0.
        walk_block(&mut collector, &mut decl.body, &mut ());

        if collector.upvars.is_empty() {
            return;
        }

        let captures: Vec<CaptureInfo> = collector
            .upvars
            .iter()
            .map(|&(scope_index, slot_index)| CaptureInfo {
                slot_index,
                scope_index,
            })
            .collect();

        decl.body.scope.set_captures(captures);
    }
}

impl<'hir> Visitor<'hir> for FreeVariableAnalysis {
    type Context = HirOptContext;

    fn visit_stmt(&mut self, stmt: &'hir mut hir::Stmt, ctx: &mut HirOptContext) {
        if let hir::StmtKind::FunctionDeclaration(decl) = &mut stmt.kind {
            // Process the body first (inside-out) so nested closures/functions
            // have their captures resolved before this function is processed.
            self.visit_block(&mut decl.body, ctx);
            Self::process_function_decl(decl);
            return;
        }
        walk_stmt(self, stmt, ctx);
    }

    fn visit_expr(&mut self, expr: &'hir mut hir::Expr, ctx: &mut HirOptContext) {
        // Process nested closures first (inside-out) so inner closures have
        // their captures resolved before the outer one is processed.
        if matches!(expr.kind, hir::ExprKind::FunctionExpression(_)) {
            walk_expr(self, expr, ctx);
            if let hir::ExprKind::FunctionExpression(decl) = &mut expr.kind {
                Self::process_function_decl(decl);
            }
        } else {
            walk_expr(self, expr, ctx);
        }
    }
}

impl HirPass for FreeVariableAnalysis {
    fn name(&self) -> &'static str {
        "FreeVariableAnalysis"
    }

    fn optimize_hir(
        &mut self,
        module: &mut hir::Module,
        ctx: &mut HirOptContext,
    ) -> Result<bool, HirOptError> {
        self.visit_module(module, ctx);
        Ok(false)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tlang_span::HirId;

    fn make_ctx() -> HirOptContext {
        HirOptContext {
            symbols: std::collections::HashMap::new(),
            hir_id_allocator: tlang_span::HirIdAllocator::default(),
            current_scope: HirId::new(1),
            diagnostics: Vec::new(),
        }
    }

    fn make_name_expr(id: usize) -> hir::Expr {
        use tlang_span::Span;
        hir::Expr {
            hir_id: HirId::new(id),
            kind: hir::ExprKind::Path(Box::new(hir::Path {
                segments: vec![],
                res: hir::Res::default(),
                span: Span::default(),
            })),
            ty: hir::Ty::default(),
            span: Span::default(),
        }
    }

    fn make_upvar_path_expr(id: usize, slot_index: usize, scope_index: u16) -> hir::Expr {
        use tlang_span::Span;
        hir::Expr {
            hir_id: HirId::new(id),
            kind: hir::ExprKind::Path(Box::new(hir::Path {
                segments: vec![],
                res: hir::Res::new_upvar(HirId::new(id + 100), slot_index, scope_index as usize),
                span: Span::default(),
            })),
            ty: hir::Ty::default(),
            span: Span::default(),
        }
    }

    /// Build a minimal `FunctionDeclaration` whose body's tail expression is `body_expr`.
    fn fn_decl_with_body_expr(body_expr: hir::Expr) -> hir::FunctionDeclaration {
        use tlang_span::Span;
        hir::FunctionDeclaration {
            hir_id: HirId::new(14),
            visibility: tlang_ast::node::Visibility::Private,
            name: make_name_expr(13),
            parameters: vec![],
            return_type: hir::Ty::default(),
            body: hir::Block {
                hir_id: HirId::new(12),
                stmts: vec![],
                expr: Some(body_expr),
                scope: hir::HirScopeData::default(),
                span: Span::default(),
            },
            span: Span::default(),
        }
    }

    /// Helper: build a minimal FunctionDeclaration whose body contains a
    /// single expression that references a path with the given slot.
    fn fn_expr_with_upvar(slot_index: usize, scope_index: u16) -> hir::FunctionDeclaration {
        fn_decl_with_body_expr(make_upvar_path_expr(11, slot_index, scope_index))
    }

    #[test]
    fn test_upvar_captured_at_fn_body_level() {
        // Reference is directly in the function body (block_depth = 0).
        // raw scope_index = 2, normalized = 2 - 0 = 2.
        let mut decl = fn_expr_with_upvar(3, 2);
        FreeVariableAnalysis::process_function_decl(&mut decl);

        let captures = decl.body.scope.captures();
        assert_eq!(captures.len(), 1);
        assert_eq!(captures[0].slot_index, 3);
        assert_eq!(captures[0].scope_index, 2);

        // The original Slot::Upvar in the HIR is unchanged (captures are GC-only).
        let body_expr = decl.body.expr.as_ref().unwrap();
        if let hir::ExprKind::Path(path) = &body_expr.kind {
            assert_eq!(path.res.slot(), hir::Slot::Upvar(3, 2));
        } else {
            panic!("expected Path expression");
        }
    }

    #[test]
    fn test_upvar_normalized_from_nested_block() {
        use tlang_span::Span;

        // Build: fn () { { upvar(slot=0, scope_index=3) } }
        // The upvar appears inside a Block expression (block_depth = 1).
        // raw = 3, normalized = 3 - 1 = 2.
        let inner_block = hir::Block {
            hir_id: HirId::new(20),
            stmts: vec![],
            expr: Some(make_upvar_path_expr(21, 0, 3)),
            scope: hir::HirScopeData::default(),
            span: Span::default(),
        };
        let block_expr = hir::Expr {
            hir_id: HirId::new(22),
            kind: hir::ExprKind::Block(Box::new(inner_block)),
            ty: hir::Ty::default(),
            span: Span::default(),
        };

        let mut decl = fn_decl_with_body_expr(block_expr);
        FreeVariableAnalysis::process_function_decl(&mut decl);

        let captures = decl.body.scope.captures();
        assert_eq!(captures.len(), 1);
        assert_eq!(captures[0].slot_index, 0);
        assert_eq!(captures[0].scope_index, 2, "normalized = 3 - 1 = 2");
    }

    #[test]
    fn test_same_upvar_at_different_depths_deduplicates() {
        use tlang_span::Span;

        // Build: fn () {
        //   upvar(slot=0, scope=1)       // at fn body (D=0), normalized = 1
        //   { upvar(slot=0, scope=2) }   // inside block (D=1), normalized = 1
        // }
        // Both references should produce exactly ONE CaptureInfo entry.
        let inner_block = hir::Block {
            hir_id: HirId::new(30),
            stmts: vec![],
            expr: Some(make_upvar_path_expr(31, 0, 2)),
            scope: hir::HirScopeData::default(),
            span: Span::default(),
        };
        let block_expr = hir::Expr {
            hir_id: HirId::new(32),
            kind: hir::ExprKind::Block(Box::new(inner_block)),
            ty: hir::Ty::default(),
            span: Span::default(),
        };
        let top_ref = make_upvar_path_expr(33, 0, 1);

        let mut decl = hir::FunctionDeclaration {
            hir_id: HirId::new(34),
            visibility: tlang_ast::node::Visibility::Private,
            name: make_name_expr(35),
            parameters: vec![],
            return_type: hir::Ty::default(),
            body: hir::Block {
                hir_id: HirId::new(36),
                stmts: vec![hir::Stmt::new(
                    HirId::new(37),
                    hir::StmtKind::Expr(Box::new(top_ref)),
                    Span::default(),
                )],
                expr: Some(block_expr),
                scope: hir::HirScopeData::default(),
                span: Span::default(),
            },
            span: Span::default(),
        };

        FreeVariableAnalysis::process_function_decl(&mut decl);

        let captures = decl.body.scope.captures();
        assert_eq!(
            captures.len(),
            1,
            "same variable at different depths should produce one capture"
        );
        assert_eq!(captures[0].slot_index, 0);
        assert_eq!(captures[0].scope_index, 1);
    }

    #[test]
    fn test_stale_captures_cleared() {
        // Pre-populate captures with stale data; after re-running with no
        // upvars, captures should be empty.
        use tlang_span::Span;

        let path = hir::Path {
            segments: vec![],
            res: {
                let mut res = hir::Res::new_local(HirId::new(10));
                res.set_slot(hir::Slot::Local(0));
                res
            },
            span: Span::default(),
        };
        let expr = hir::Expr {
            hir_id: HirId::new(11),
            kind: hir::ExprKind::Path(Box::new(path)),
            ty: hir::Ty::default(),
            span: Span::default(),
        };
        let mut scope = hir::HirScopeData::default();
        scope.set_captures(vec![CaptureInfo {
            slot_index: 99,
            scope_index: 99,
        }]);
        let mut decl = hir::FunctionDeclaration {
            hir_id: HirId::new(14),
            visibility: tlang_ast::node::Visibility::Private,
            name: make_name_expr(13),
            parameters: vec![],
            return_type: hir::Ty::default(),
            body: hir::Block {
                hir_id: HirId::new(12),
                stmts: vec![],
                expr: Some(expr),
                scope,
                span: Span::default(),
            },
            span: Span::default(),
        };

        FreeVariableAnalysis::process_function_decl(&mut decl);
        assert!(
            decl.body.scope.captures().is_empty(),
            "stale captures should be cleared when there are no upvars"
        );
    }

    #[test]
    fn no_upvars_leaves_empty_captures() {
        use tlang_span::Span;

        // Function with a local path, not an upvar
        let path = hir::Path {
            segments: vec![],
            res: {
                let mut res = hir::Res::new_local(HirId::new(10));
                res.set_slot(hir::Slot::Local(0));
                res
            },
            span: Span::default(),
        };

        let expr = hir::Expr {
            hir_id: HirId::new(11),
            kind: hir::ExprKind::Path(Box::new(path)),
            ty: hir::Ty::default(),
            span: Span::default(),
        };

        let body = hir::Block {
            hir_id: HirId::new(12),
            stmts: vec![],
            expr: Some(expr),
            scope: hir::HirScopeData::default(),
            span: Span::default(),
        };

        let mut decl = hir::FunctionDeclaration {
            hir_id: HirId::new(14),
            visibility: tlang_ast::node::Visibility::Private,
            name: make_name_expr(13),
            parameters: vec![],
            return_type: hir::Ty::default(),
            body,
            span: Span::default(),
        };

        FreeVariableAnalysis::process_function_decl(&mut decl);
        assert!(decl.body.scope.captures().is_empty());
    }

    #[test]
    fn test_pass_reports_no_changes() {
        let mut module = hir::Module::default();
        let mut ctx = make_ctx();
        let mut pass = FreeVariableAnalysis;
        let changed = pass.optimize_hir(&mut module, &mut ctx).unwrap();
        assert!(!changed, "FreeVariableAnalysis should not report changes");
    }
}
