//! Free variable analysis and capture remapping for closures.
//!
//! This pass runs after slot allocation. For each `FunctionExpression` (closure)
//! and `FunctionDeclaration` (named nested function) in the HIR, it:
//!
//! 1. Collects all `Slot::Upvar` references in the body, normalizing their
//!    `scope_index` relative to the closure's function scope (block depth 0).
//! 2. Lifts transitive captures: if a nested closure captures a variable from
//!    a scope above the current closure, the current closure also captures it.
//! 3. Stores the resulting `Vec<CaptureInfo>` on `HirScopeData::captures`.
//! 4. Remaps `Slot::Upvar` references so they index into the flat capture
//!    array. After remapping, `Upvar(cap_idx, block_depth + 1)` points to
//!    a single capture scope pushed at invocation time.
//! 5. Updates nested closures' `CaptureInfo` for transitive captures that
//!    now go through the current closure's capture scope.
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
//! This value is unique per captured variable and stable across reference sites.
//!
//! ## Post-remapping `Slot::Upvar`
//!
//! After the remapping pass, `Slot::Upvar(cap_idx, raw_scope)` in a closure
//! body means: read `captures[cap_idx]` from the capture scope, which is
//! `raw_scope` scopes back from the current scope (where
//! `raw_scope = block_depth + 1`).

use std::collections::{BTreeSet, HashMap};

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
    /// If the nested function captures a variable from *beyond* the current
    /// closure's function boundary (i.e., `scope_index > block_depth + 1`),
    /// the current closure must also capture it.
    ///
    /// The nested closure's normalized scope_index includes the block depth
    /// (`d`) between the nested fn body and the current closure's fn body.
    /// The current closure's scope_index is:
    ///
    /// ```text
    /// outer_scope = nested_scope - d - 1
    /// ```
    ///
    /// where `d = self.block_depth` at the point where the nested closure
    /// appears.
    fn lift_from(&mut self, nested: &hir::FunctionDeclaration) {
        for cap in nested.body.scope.captures() {
            if let Some(outer_scope_index) = cap.scope_index.checked_sub(self.block_depth + 1)
                && outer_scope_index > 0
            {
                self.upvars.insert((outer_scope_index, cap.slot_index));
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
// CaptureRemapper
// ---------------------------------------------------------------------------

/// Rewrites `Slot::Upvar` references in a closure body so they point into
/// a flat capture array instead of the original scope chain.
///
/// After running, every `Upvar(slot, scope_index)` in the body becomes
/// `Upvar(capture_index, block_depth + 1)`, where `capture_index` is the
/// position in the closure's `captures` Vec and `block_depth + 1` is the
/// distance from the reference site to the single capture scope that will
/// be pushed at invocation time.
///
/// Additionally, nested closures' `CaptureInfo` entries for transitive
/// captures (variables from beyond this closure) are updated to reference
/// the capture-scope layout.
struct CaptureRemapper {
    /// `(normalized_scope_index, slot_index) → capture_array_index`
    capture_map: HashMap<(hir::ScopeIndex, hir::SlotIndex), usize>,
    /// Block depth within the closure body (same tracking as CaptureCollector).
    block_depth: u16,
}

impl CaptureRemapper {
    fn new(captures: &[CaptureInfo]) -> Self {
        let capture_map = captures
            .iter()
            .enumerate()
            .map(|(i, cap)| ((cap.scope_index, cap.slot_index), i))
            .collect();

        Self {
            capture_map,
            block_depth: 0,
        }
    }

    /// Remap a single `Slot::Upvar` to point into the flat capture array.
    fn remap_upvar(
        &self,
        slot_index: hir::SlotIndex,
        scope_index: hir::ScopeIndex,
    ) -> Option<hir::Slot> {
        let normalized = scope_index.saturating_sub(self.block_depth);
        let cap_idx = self.capture_map.get(&(normalized, slot_index))?;
        let new_scope = self.block_depth + 1;
        Some(hir::Slot::Upvar(*cap_idx, new_scope))
    }

    /// Update a nested closure's `CaptureInfo` entries for transitive captures.
    ///
    /// Captures with `scope_index > block_depth + 1` come from beyond this
    /// closure. After remapping, they must reference this closure's capture
    /// scope instead of the original parent scopes.
    ///
    /// The `capture_origins` mechanism on `ScopeStack` ensures that even
    /// though the nested closure's CaptureInfo points at the parent capture
    /// array, `capture_position` resolves through to the ultimate original
    /// binding for write-back.
    fn update_nested_captures(&self, nested: &mut hir::FunctionDeclaration) {
        let threshold = self.block_depth + 1;
        let new_scope = self.block_depth + 2;

        let mut updated: Vec<CaptureInfo> = Vec::new();

        for cap in nested.body.scope.captures() {
            if cap.scope_index > threshold {
                // Transitive capture — find its position in our capture array.
                let outer_normalized = cap.scope_index - threshold;
                if let Some(&cap_idx) = self.capture_map.get(&(outer_normalized, cap.slot_index)) {
                    updated.push(CaptureInfo {
                        slot_index: cap_idx,
                        scope_index: new_scope,
                    });
                } else {
                    // Keep original (shouldn't happen with correct analysis).
                    updated.push(*cap);
                }
            } else {
                // Direct capture from this closure's body/blocks — unchanged.
                updated.push(*cap);
            }
        }

        nested.body.scope.set_captures(updated);
    }
}

impl<'hir> Visitor<'hir> for CaptureRemapper {
    type Context = ();

    fn visit_stmt(&mut self, stmt: &'hir mut hir::Stmt, ctx: &mut ()) {
        if let hir::StmtKind::FunctionDeclaration(nested) = &mut stmt.kind {
            self.update_nested_captures(nested);
            return;
        }
        walk_stmt(self, stmt, ctx);
    }

    fn visit_expr(&mut self, expr: &'hir mut hir::Expr, ctx: &mut ()) {
        match &mut expr.kind {
            hir::ExprKind::FunctionExpression(nested) => {
                self.update_nested_captures(nested);
            }
            hir::ExprKind::Match(discriminant, arms) => {
                self.visit_expr(discriminant, ctx);
                for arm in arms {
                    let has_pat_scope = arm.hir_id != arm.block.hir_id;
                    self.block_depth += 1;
                    walk_pat(self, &mut arm.pat, ctx);
                    if let Some(guard) = &mut arm.guard {
                        self.visit_expr(guard, ctx);
                    }
                    if has_pat_scope {
                        self.visit_block(&mut arm.block, ctx);
                    } else {
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
        if let hir::Slot::Upvar(slot_index, scope_index) = path.res.slot()
            && let Some(new_slot) = self.remap_upvar(slot_index, scope_index)
        {
            path.res.set_slot(new_slot);
        }
    }
}

// ---------------------------------------------------------------------------
// Top-level pass
// ---------------------------------------------------------------------------

#[derive(Debug, Default)]
pub struct FreeVariableAnalysis;

impl FreeVariableAnalysis {
    /// Collect captures for a function and optionally remap Upvar slots.
    ///
    /// `remap` should be `true` only for `FunctionExpression` (closures), which
    /// are invoked with a capture scope at runtime. `FunctionDeclaration` (named
    /// functions) are invoked via `with_root_scope` and must keep their original
    /// Upvar slots — but we still compute CaptureInfo so parent closures can
    /// lift transitive captures via `lift_from`.
    fn process_function_decl(decl: &mut hir::FunctionDeclaration, remap: bool) {
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

        decl.body.scope.set_captures(captures.clone());

        if remap {
            // Remap Upvar slots in the body to point into the flat capture array,
            // and update nested closures' CaptureInfo for the new layout.
            let mut remapper = CaptureRemapper::new(&captures);
            walk_block(&mut remapper, &mut decl.body, &mut ());
        }
    }
}

impl<'hir> Visitor<'hir> for FreeVariableAnalysis {
    type Context = HirOptContext;

    fn visit_stmt(&mut self, stmt: &'hir mut hir::Stmt, ctx: &mut HirOptContext) {
        if let hir::StmtKind::FunctionDeclaration(decl) = &mut stmt.kind {
            // Process the body first (inside-out) so nested closures/functions
            // have their captures resolved before this function is processed.
            self.visit_block(&mut decl.body, ctx);
            // Named functions are Fn objects (with_root_scope), not closures —
            // collect CaptureInfo for transitive lifting but do NOT remap.
            Self::process_function_decl(decl, false);
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
                // FunctionExpression → Closure objects (with_closure_scope) —
                // remap Upvars to index into the flat capture array.
                Self::process_function_decl(decl, true);
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
        FreeVariableAnalysis::process_function_decl(&mut decl, true);

        let captures = decl.body.scope.captures();
        assert_eq!(captures.len(), 1);
        assert_eq!(captures[0].slot_index, 3);
        assert_eq!(captures[0].scope_index, 2);

        // After remapping, Upvar(3, 2) → Upvar(cap_idx=0, block_depth+1=1).
        let body_expr = decl.body.expr.as_ref().unwrap();
        if let hir::ExprKind::Path(path) = &body_expr.kind {
            assert_eq!(path.res.slot(), hir::Slot::Upvar(0, 1));
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
        FreeVariableAnalysis::process_function_decl(&mut decl, true);

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

        FreeVariableAnalysis::process_function_decl(&mut decl, true);

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

        FreeVariableAnalysis::process_function_decl(&mut decl, true);
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

        FreeVariableAnalysis::process_function_decl(&mut decl, true);
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
