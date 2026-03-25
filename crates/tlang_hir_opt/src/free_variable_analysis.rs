//! Free variable analysis for closures.
//!
//! This pass runs after slot allocation and collects the set of upvar
//! references inside each closure (FunctionExpression). It then:
//!
//! 1. Stores a `Vec<CaptureInfo>` on the closure's `HirScopeData` so the
//!    runtime knows exactly which bindings to capture.
//! 2. Remaps every `Slot::Upvar(slot, scope)` inside the closure body to
//!    `Slot::Upvar(capture_index, 1)` so the runtime can serve those reads
//!    from a single flat capture scope pushed one level above the function
//!    scope.
//! 3. Handles nested closures by lifting transitive captures: if an inner
//!    closure needs a variable from an outer scope that passes through an
//!    intermediate closure, the intermediate closure also captures it and
//!    the inner closure's capture info is updated to reference the
//!    intermediate closure's capture scope.

use std::collections::BTreeSet;

use tlang_hir::visit::walk_expr;
use tlang_hir::{self as hir, CaptureInfo, Visitor};

use crate::HirPass;
use crate::hir_opt::{HirOptContext, HirOptError};

// ---------------------------------------------------------------------------
// ShallowUpvarCollector: gathers Slot::Upvar references WITHOUT entering
// nested FunctionExpressions (each closure manages its own captures).
// ---------------------------------------------------------------------------

struct ShallowUpvarCollector {
    upvars: BTreeSet<(hir::ScopeIndex, hir::SlotIndex)>,
}

impl ShallowUpvarCollector {
    fn new() -> Self {
        Self {
            upvars: BTreeSet::new(),
        }
    }
}

impl<'hir> Visitor<'hir> for ShallowUpvarCollector {
    type Context = ();

    fn visit_expr(&mut self, expr: &'hir mut hir::Expr, ctx: &mut ()) {
        // Stop at nested FunctionExpressions — they have their own captures.
        if matches!(expr.kind, hir::ExprKind::FunctionExpression(_)) {
            return;
        }
        walk_expr(self, expr, ctx);
    }

    fn visit_path(&mut self, path: &'hir mut hir::Path, _ctx: &mut ()) {
        if let hir::Slot::Upvar(slot_index, scope_index) = path.res.slot() {
            self.upvars.insert((scope_index, slot_index));
        }
    }
}

// ---------------------------------------------------------------------------
// ShallowUpvarRemapper: rewrites direct Slot::Upvar references (not in
// nested FunctionExpressions) to Slot::Upvar(capture_index, 1).
// ---------------------------------------------------------------------------

struct ShallowUpvarRemapper<'a> {
    capture_map: &'a std::collections::HashMap<(hir::ScopeIndex, hir::SlotIndex), usize>,
}

impl<'hir, 'a> Visitor<'hir> for ShallowUpvarRemapper<'a> {
    type Context = ();

    fn visit_expr(&mut self, expr: &'hir mut hir::Expr, ctx: &mut ()) {
        if matches!(expr.kind, hir::ExprKind::FunctionExpression(_)) {
            return;
        }
        walk_expr(self, expr, ctx);
    }

    fn visit_path(&mut self, path: &'hir mut hir::Path, _ctx: &mut ()) {
        if let hir::Slot::Upvar(slot_index, scope_index) = path.res.slot() {
            if let Some(&capture_index) = self.capture_map.get(&(scope_index, slot_index)) {
                path.res.set_slot(hir::Slot::Upvar(capture_index, 1));
            }
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
        // 1. Collect direct upvar references in the body (NOT entering nested
        //    FunctionExpressions — each manages its own captures).
        let mut collector = ShallowUpvarCollector::new();
        collector.visit_block(&mut decl.body, &mut ());
        let mut all_upvars = collector.upvars;

        // 2. Lift transitive captures from immediate nested closures.
        //    After inside-out processing, nested closures already have their
        //    captures resolved. Any capture with scope_index > 1 passes
        //    through this closure and needs to be lifted.
        //
        //    We collect nested closure info in two phases to satisfy the
        //    borrow checker: first read captures, then write updates.

        // Phase 2a: read nested closures' captures and decide what to lift.
        let lifts_per_closure = Self::collect_lifts(&decl.body, &mut all_upvars);

        if all_upvars.is_empty() {
            return;
        }

        // 3. Build the sorted capture list and index map.
        let captures: Vec<CaptureInfo> = all_upvars
            .iter()
            .map(|&(scope_index, slot_index)| CaptureInfo {
                slot_index,
                scope_index,
            })
            .collect();

        let capture_map: std::collections::HashMap<(hir::ScopeIndex, hir::SlotIndex), usize> =
            all_upvars
                .iter()
                .enumerate()
                .map(|(idx, &key)| (key, idx))
                .collect();

        // 4. Store captures on scope data.
        decl.body.scope.set_captures(captures);

        // 5. Remap direct upvar references in the body (not nested FEs).
        let mut remapper = ShallowUpvarRemapper {
            capture_map: &capture_map,
        };
        remapper.visit_block(&mut decl.body, &mut ());

        // 6. Update nested closures' capture info for lifted variables.
        Self::apply_lifts(&mut decl.body, &lifts_per_closure, &capture_map);
    }

    /// Walk the body's immediate children looking for FunctionExpressions.
    /// For each nested closure whose captures have scope > 1, record a
    /// `LiftInfo` and insert the adjusted key into `all_upvars`.
    fn collect_lifts(
        body: &hir::Block,
        all_upvars: &mut BTreeSet<(hir::ScopeIndex, hir::SlotIndex)>,
    ) -> Vec<(usize, Vec<(usize, (hir::ScopeIndex, hir::SlotIndex))>)> {
        // Vec of (closure_index_in_stmts_or_expr, Vec<(capture_idx, our_key)>)
        let mut result = Vec::new();

        // Helper: inspect a single expression for nested closures.
        fn inspect_expr(
            expr: &hir::Expr,
            all_upvars: &mut BTreeSet<(hir::ScopeIndex, hir::SlotIndex)>,
            result: &mut Vec<(usize, Vec<(usize, (hir::ScopeIndex, hir::SlotIndex))>)>,
            idx: usize,
        ) {
            if let hir::ExprKind::FunctionExpression(nested_decl) = &expr.kind {
                let mut lifts = Vec::new();
                for (cap_idx, cap) in nested_decl.body.scope.captures().iter().enumerate() {
                    if cap.scope_index > 1 {
                        let our_key = (cap.scope_index - 1, cap.slot_index);
                        all_upvars.insert(our_key);
                        lifts.push((cap_idx, our_key));
                    }
                }
                if !lifts.is_empty() {
                    result.push((idx, lifts));
                }
            }
            // Don't recurse — we only care about immediate children.
        }

        // Walk stmts' top-level expressions (shallow, no recursion into nested blocks).
        Self::visit_exprs_shallow(body, |idx, expr| {
            inspect_expr(expr, all_upvars, &mut result, idx);
        });

        result
    }

    /// Apply the lift updates to nested closures' capture info.
    fn apply_lifts(
        body: &mut hir::Block,
        lifts_per_closure: &[(usize, Vec<(usize, (hir::ScopeIndex, hir::SlotIndex))>)],
        capture_map: &std::collections::HashMap<(hir::ScopeIndex, hir::SlotIndex), usize>,
    ) {
        if lifts_per_closure.is_empty() {
            return;
        }

        // Build a set of indices that need updating for fast lookup.
        let lift_map: std::collections::HashMap<usize, &Vec<(usize, (hir::ScopeIndex, hir::SlotIndex))>> =
            lifts_per_closure.iter().map(|(idx, lifts)| (*idx, lifts)).collect();

        let mut counter = 0usize;
        Self::visit_exprs_shallow_mut(body, |idx, expr| {
            if let Some(lifts) = lift_map.get(&idx) {
                if let hir::ExprKind::FunctionExpression(nested_decl) = &mut expr.kind {
                    let mut caps = nested_decl.body.scope.captures().to_vec();
                    for &(cap_idx, ref our_key) in lifts.iter() {
                        let our_capture_idx = capture_map[our_key];
                        caps[cap_idx] = CaptureInfo {
                            slot_index: our_capture_idx,
                            scope_index: 2,
                        };
                    }
                    nested_decl.body.scope.set_captures(caps);
                }
            }
            counter += 1;
        });
    }

    /// Visit top-level expressions in a block shallowly (stmts + tail expr).
    /// The callback receives a sequential index and a reference to each expression.
    /// Does NOT recurse into sub-expressions or nested blocks.
    fn visit_exprs_shallow<F>(body: &hir::Block, mut f: F)
    where
        F: FnMut(usize, &hir::Expr),
    {
        let mut idx = 0;
        for stmt in &body.stmts {
            match &stmt.kind {
                hir::StmtKind::Expr(expr) => {
                    f(idx, expr);
                    idx += 1;
                }
                hir::StmtKind::Let(_, expr, _) => {
                    f(idx, expr);
                    idx += 1;
                }
                _ => {
                    idx += 1;
                }
            }
        }
        if let Some(expr) = &body.expr {
            f(idx, expr);
        }
    }

    /// Mutable version of visit_exprs_shallow.
    fn visit_exprs_shallow_mut<F>(body: &mut hir::Block, mut f: F)
    where
        F: FnMut(usize, &mut hir::Expr),
    {
        let mut idx = 0;
        for stmt in &mut body.stmts {
            match &mut stmt.kind {
                hir::StmtKind::Expr(expr) => {
                    f(idx, expr);
                    idx += 1;
                }
                hir::StmtKind::Let(_, expr, _) => {
                    f(idx, expr);
                    idx += 1;
                }
                _ => {
                    idx += 1;
                }
            }
        }
        if let Some(expr) = &mut body.expr {
            f(idx, expr);
        }
    }
}

impl<'hir> Visitor<'hir> for FreeVariableAnalysis {
    type Context = HirOptContext;

    fn visit_expr(&mut self, expr: &'hir mut hir::Expr, ctx: &mut HirOptContext) {
        // Process nested closures first (depth-first / inside-out) so that
        // inner closures have their captures resolved before the outer one.
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

    /// Helper: build a minimal FunctionDeclaration whose body contains a
    /// single expression that references a path with the given slot.
    fn fn_expr_with_upvar(slot_index: usize, scope_index: u16) -> hir::FunctionDeclaration {
        use tlang_span::Span;

        let path = hir::Path {
            segments: vec![],
            res: hir::Res::new_upvar(HirId::new(10), slot_index, scope_index as usize),
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

        let name_path = hir::Path {
            segments: vec![],
            res: hir::Res::default(),
            span: Span::default(),
        };
        let name_expr = hir::Expr {
            hir_id: HirId::new(13),
            kind: hir::ExprKind::Path(Box::new(name_path)),
            ty: hir::Ty::default(),
            span: Span::default(),
        };

        hir::FunctionDeclaration {
            hir_id: HirId::new(14),
            visibility: tlang_ast::node::Visibility::Private,
            name: name_expr,
            parameters: vec![],
            return_type: hir::Ty::default(),
            body,
            span: Span::default(),
        }
    }

    #[test]
    fn single_upvar_is_captured_and_remapped() {
        let mut decl = fn_expr_with_upvar(3, 2);
        FreeVariableAnalysis::process_function_decl(&mut decl);

        // Check captures stored on scope data
        let captures = decl.body.scope.captures();
        assert_eq!(captures.len(), 1);
        assert_eq!(captures[0].slot_index, 3);
        assert_eq!(captures[0].scope_index, 2);

        // Check that the path inside the body was remapped
        let body_expr = decl.body.expr.as_ref().unwrap();
        if let hir::ExprKind::Path(path) = &body_expr.kind {
            assert_eq!(path.res.slot(), hir::Slot::Upvar(0, 1));
        } else {
            panic!("expected Path expression");
        }
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

        let name_path = hir::Path {
            segments: vec![],
            res: hir::Res::default(),
            span: Span::default(),
        };
        let name_expr = hir::Expr {
            hir_id: HirId::new(13),
            kind: hir::ExprKind::Path(Box::new(name_path)),
            ty: hir::Ty::default(),
            span: Span::default(),
        };

        let mut decl = hir::FunctionDeclaration {
            hir_id: HirId::new(14),
            visibility: tlang_ast::node::Visibility::Private,
            name: name_expr,
            parameters: vec![],
            return_type: hir::Ty::default(),
            body,
            span: Span::default(),
        };

        FreeVariableAnalysis::process_function_decl(&mut decl);
        assert!(decl.body.scope.captures().is_empty());
    }

    #[test]
    fn pass_does_not_report_changed() {
        let mut module = hir::Module::default();
        let mut ctx = make_ctx();
        let mut pass = FreeVariableAnalysis;
        let changed = pass.optimize_hir(&mut module, &mut ctx).unwrap();
        assert!(!changed, "FreeVariableAnalysis should not report changes");
    }
}
