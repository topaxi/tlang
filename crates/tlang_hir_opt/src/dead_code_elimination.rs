use std::collections::HashSet;

use tlang_ast::node::Visibility;
use tlang_hir::{
    self as hir, BinaryOpKind, ExprKind, PatKind, StmtKind,
    visit::{self, Visitor},
};
use tlang_span::HirId;

use crate::hir_opt::{HirOptContext, HirOptError, HirPass};

/// Dead code elimination pass.
///
/// Removes top-level and nested statements whose bindings are never referenced
/// elsewhere, provided the right-hand side is free of side effects.  Declarations
/// (functions, structs, enums, protocols) that are never referenced are also
/// removed.
///
/// The pass is intentionally conservative: any expression that *might* produce
/// observable effects (calls, assignments, loops, breaks) is kept even when
/// its result is unused.
pub struct DeadCodeElimination {
    changed: bool,
}

impl DeadCodeElimination {
    pub fn new() -> Self {
        Self { changed: false }
    }
}

impl Default for DeadCodeElimination {
    fn default() -> Self {
        Self::new()
    }
}

// ---------------------------------------------------------------------------
// Phase 1 – Collect all HirIds that are *referenced* (read) by paths.
// ---------------------------------------------------------------------------

/// Walks the HIR tree and collects every `HirId` that a `Path` resolves to,
/// skipping self-referencing names of declarations (e.g. a function's own
/// name path that the resolver points back at itself).
struct ReferenceCollector {
    refs: HashSet<HirId>,
}

impl ReferenceCollector {
    fn new() -> Self {
        Self {
            refs: HashSet::new(),
        }
    }
}

impl<'hir> Visitor<'hir> for ReferenceCollector {
    fn visit_path(&mut self, path: &'hir mut hir::Path, _ctx: &mut Self::Context) {
        if let Some(hir_id) = path.res.hir_id() {
            self.refs.insert(hir_id);
        }
    }

    fn visit_stmt(&mut self, stmt: &'hir mut hir::Stmt, ctx: &mut Self::Context) {
        match &mut stmt.kind {
            // Skip visiting the declaration name to avoid counting
            // the self-referencing path as a "use".
            StmtKind::FunctionDeclaration(decl) => {
                self.enter_scope(decl.hir_id, ctx);
                for param in &mut decl.parameters {
                    self.visit_ident(&mut param.name, ctx);
                    self.visit_ty(&mut param.type_annotation, ctx);
                }
                self.visit_block(&mut decl.body, ctx);
                self.leave_scope(decl.hir_id, ctx);
            }
            StmtKind::DynFunctionDeclaration(_) => {
                // Name is a self-reference; no body to walk.
                // Variant references are handled in Phase 1.5 (propagation).
            }
            _ => visit::walk_stmt(self, stmt, ctx),
        }
    }
}

// ---------------------------------------------------------------------------
// Side-effect analysis helpers.
// ---------------------------------------------------------------------------

fn expr_has_side_effects(expr: &hir::Expr) -> bool {
    match &expr.kind {
        // Calls are always considered side-effectful.
        ExprKind::Call(_) | ExprKind::TailCall(_) => true,

        // Assignment is side-effectful.
        ExprKind::Binary(BinaryOpKind::Assign, _, _) => true,

        // Control flow.
        ExprKind::Loop(_) | ExprKind::Break(_) | ExprKind::Continue => true,

        // Tagged strings invoke the tag function.
        ExprKind::TaggedString { .. } => true,

        // Pure leaves.
        ExprKind::Literal(_) | ExprKind::Path(_) | ExprKind::Wildcard => false,

        // Function expressions are values (closures); creating them is pure.
        ExprKind::FunctionExpression(_) => false,

        // Compound expressions: recurse.
        ExprKind::Unary(_, inner) | ExprKind::Cast(inner, _) => expr_has_side_effects(inner),
        ExprKind::Binary(_, lhs, rhs) => expr_has_side_effects(lhs) || expr_has_side_effects(rhs),
        ExprKind::Let(_, inner) => expr_has_side_effects(inner),
        ExprKind::FieldAccess(base, _) => expr_has_side_effects(base),
        ExprKind::IndexAccess(base, index) => {
            expr_has_side_effects(base) || expr_has_side_effects(index)
        }
        ExprKind::List(items) => items.iter().any(expr_has_side_effects),
        ExprKind::Dict(pairs) => pairs
            .iter()
            .any(|(k, v)| expr_has_side_effects(k) || expr_has_side_effects(v)),
        ExprKind::Block(block) => block_has_side_effects(block),
        ExprKind::IfElse(cond, then_block, else_branches) => {
            expr_has_side_effects(cond)
                || block_has_side_effects(then_block)
                || else_branches.iter().any(|branch| {
                    branch.condition.as_ref().is_some_and(expr_has_side_effects)
                        || block_has_side_effects(&branch.consequence)
                })
        }
        ExprKind::Match(scrutinee, arms) => {
            expr_has_side_effects(scrutinee)
                || arms.iter().any(|arm| {
                    arm.guard.as_ref().is_some_and(expr_has_side_effects)
                        || block_has_side_effects(&arm.block)
                })
        }
        ExprKind::Range(range) => {
            expr_has_side_effects(&range.start) || expr_has_side_effects(&range.end)
        }
    }
}

fn block_has_side_effects(block: &hir::Block) -> bool {
    block.stmts.iter().any(stmt_has_side_effects)
        || block.expr.as_ref().is_some_and(expr_has_side_effects)
}

fn stmt_has_side_effects(stmt: &hir::Stmt) -> bool {
    match &stmt.kind {
        StmtKind::Expr(expr) => expr_has_side_effects(expr),
        StmtKind::Let(_, expr, _) | StmtKind::Const(_, _, expr, _) => expr_has_side_effects(expr),
        StmtKind::Return(_) => true,
        // Struct/enum method declarations (`fn Shape.area(...)`) install methods
        // on the type's prototype — this is a side effect.
        StmtKind::FunctionDeclaration(decl) => {
            matches!(decl.name.kind, ExprKind::FieldAccess(..))
        }
        StmtKind::DynFunctionDeclaration(decl) => {
            matches!(decl.name.kind, ExprKind::FieldAccess(..))
        }
        StmtKind::EnumDeclaration(_)
        | StmtKind::StructDeclaration(_)
        | StmtKind::ProtocolDeclaration(_)
        | StmtKind::ImplBlock(_) => false,
    }
}

// ---------------------------------------------------------------------------
// Phase 2 – Eliminate dead statements.
// ---------------------------------------------------------------------------

/// Returns the declaring HirId(s) for a statement, if it introduces named
/// bindings.
fn stmt_declared_ids(stmt: &hir::Stmt) -> Vec<HirId> {
    match &stmt.kind {
        StmtKind::Let(pat, _, _) | StmtKind::Const(_, pat, _, _) => pat_declared_ids(pat),
        StmtKind::FunctionDeclaration(decl) => vec![decl.hir_id],
        StmtKind::DynFunctionDeclaration(decl) => vec![decl.hir_id],
        StmtKind::EnumDeclaration(decl) => {
            let mut ids = vec![decl.hir_id];
            for variant in &decl.variants {
                ids.push(variant.hir_id);
            }
            ids
        }
        StmtKind::StructDeclaration(decl) => vec![decl.hir_id],
        StmtKind::ProtocolDeclaration(decl) => vec![decl.hir_id],
        StmtKind::ImplBlock(_) | StmtKind::Expr(_) | StmtKind::Return(_) => vec![],
    }
}

fn pat_declared_ids(pat: &hir::Pat) -> Vec<HirId> {
    match &pat.kind {
        PatKind::Identifier(hir_id, _) => vec![*hir_id],
        PatKind::List(pats) => pats.iter().flat_map(pat_declared_ids).collect(),
        PatKind::Rest(inner) => pat_declared_ids(inner),
        PatKind::Enum(_, fields) => fields
            .iter()
            .flat_map(|(_, p)| pat_declared_ids(p))
            .collect(),
        PatKind::Wildcard | PatKind::Literal(_) => vec![],
    }
}

/// Returns `true` if a statement declares a `pub` binding.
fn stmt_is_public(stmt: &hir::Stmt) -> bool {
    match &stmt.kind {
        StmtKind::Const(vis, _, _, _) => *vis == Visibility::Public,
        StmtKind::FunctionDeclaration(decl) => decl.visibility == Visibility::Public,
        StmtKind::DynFunctionDeclaration(_) => false,
        StmtKind::EnumDeclaration(decl) => decl.visibility == Visibility::Public,
        StmtKind::StructDeclaration(decl) => decl.visibility == Visibility::Public,
        StmtKind::ProtocolDeclaration(decl) => decl.visibility == Visibility::Public,
        _ => false,
    }
}

/// Returns `true` if the statement is dead and can be removed.
///
/// When `preserve_public` is true (top-level module block), public
/// declarations are never removed — they may be referenced by importing
/// modules.
fn is_dead_stmt(stmt: &hir::Stmt, refs: &HashSet<HirId>, preserve_public: bool) -> bool {
    if preserve_public && stmt_is_public(stmt) {
        return false;
    }

    let ids = stmt_declared_ids(stmt);

    if ids.is_empty() {
        // Non-declaring statements (expression statements, returns, impl blocks).
        match &stmt.kind {
            StmtKind::Expr(expr) => !expr_has_side_effects(expr),
            _ => false,
        }
    } else {
        // Keep if any declared id is referenced.
        let any_referenced = ids.iter().any(|id| refs.contains(id));
        if any_referenced {
            return false;
        }
        // Unreferenced — only remove if the RHS has no side effects.
        !stmt_has_side_effects(stmt)
    }
}

/// Walks blocks to remove dead statements, relying on the default `walk_block`
/// to recurse into surviving statements (including nested function bodies,
/// match arms, if/else blocks, etc.).
struct DeadCodeEliminator<'a> {
    refs: &'a HashSet<HirId>,
    changed: bool,
    /// HirIds of bindings that were removed, for symbol table cleanup.
    removed_ids: HashSet<HirId>,
}

impl<'hir> Visitor<'hir> for DeadCodeEliminator<'_> {
    fn visit_block(&mut self, block: &'hir mut hir::Block, ctx: &mut Self::Context) {
        let before = block.stmts.len();
        // Inside nested blocks (function bodies, match arms, etc.) there are
        // no public exports, so `preserve_public` is false.
        // Collect removed ids before retain.
        for stmt in &block.stmts {
            if is_dead_stmt(stmt, self.refs, false) {
                self.removed_ids.extend(stmt_declared_ids(stmt));
            }
        }
        block
            .stmts
            .retain(|stmt| !is_dead_stmt(stmt, self.refs, false));
        if block.stmts.len() != before {
            self.changed = true;
        }

        // Continue walking the surviving statements and completion expr.
        visit::walk_block(self, block, ctx);
    }
}

// ---------------------------------------------------------------------------
// HirPass implementation
// ---------------------------------------------------------------------------

impl HirPass for DeadCodeElimination {
    fn name(&self) -> &'static str {
        "DeadCodeElimination"
    }

    fn optimize_hir(
        &mut self,
        module: &mut hir::Module,
        ctx: &mut HirOptContext,
    ) -> Result<bool, HirOptError> {
        // Phase 1: collect referenced HirIds from paths.
        let mut collector = ReferenceCollector::new();
        collector.visit_module(module, &mut ());
        let mut refs = collector.refs;

        // Phase 1.5: propagate DynFunctionDeclaration <-> variant links.
        //
        // A DynFunctionDeclaration (multi-clause dispatch node) survives if:
        //   - it is referenced by a path (callers), OR
        //   - any of its variant FunctionDeclarations is `pub` (top-level export).
        //
        // When it survives, ALL its variant FunctionDeclarations must also
        // survive (even private ones), so we add their hir_ids to refs.
        for stmt in &module.block.stmts {
            if let StmtKind::DynFunctionDeclaration(decl) = &stmt.kind {
                let dyn_alive = refs.contains(&decl.hir_id)
                    || module.block.stmts.iter().any(|s| {
                        if let StmtKind::FunctionDeclaration(fd) = &s.kind {
                            decl.variants.iter().any(|(_, id)| *id == fd.hir_id)
                                && fd.visibility == Visibility::Public
                        } else {
                            false
                        }
                    });

                if dyn_alive {
                    // Keep the dyn dispatch node itself alive.
                    refs.insert(decl.hir_id);
                    // Keep all variant FunctionDeclarations alive.
                    for &(_, variant_id) in &decl.variants {
                        refs.insert(variant_id);
                    }
                }
            }
        }

        // Phase 2a: eliminate dead statements in the top-level module block,
        // preserving `pub` declarations that may be used by importing modules.
        // Collect removed ids before retain.
        let mut removed_ids = HashSet::new();
        for stmt in &module.block.stmts {
            if is_dead_stmt(stmt, &refs, true) {
                removed_ids.extend(stmt_declared_ids(stmt));
            }
        }
        let before = module.block.stmts.len();
        module
            .block
            .stmts
            .retain(|stmt| !is_dead_stmt(stmt, &refs, true));
        self.changed = module.block.stmts.len() != before;

        // Phase 2b: eliminate dead statements inside nested blocks (function
        // bodies, match arms, etc.) where there are no public exports.
        let mut eliminator = DeadCodeEliminator {
            refs: &refs,
            changed: false,
            removed_ids: HashSet::new(),
        };
        // Walk statements and the completion expression, but NOT the top-level
        // block itself (we already handled it above).
        for stmt in &mut module.block.stmts {
            eliminator.visit_stmt(stmt, &mut ());
        }
        if let Some(expr) = &mut module.block.expr {
            eliminator.visit_expr(expr, &mut ());
        }

        self.changed |= eliminator.changed;
        removed_ids.extend(eliminator.removed_ids);

        // Phase 3: prune symbol tables so that removed bindings no longer
        // occupy slots.  Without this, SlotAllocator would assign gap slots
        // for dead bindings, causing a mismatch with the interpreter's
        // sequential `next_var_index` counter.
        if !removed_ids.is_empty() {
            for symbol_table in ctx.symbols.values() {
                symbol_table
                    .borrow_mut()
                    .remove_symbols_by_hir_ids(&removed_ids);
            }
        }

        Ok(self.changed)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn call_is_side_effectful() {
        let call = hir::Expr {
            hir_id: HirId::new(1),
            kind: ExprKind::Call(Box::new(hir::CallExpression {
                hir_id: HirId::new(2),
                callee: hir::Expr {
                    hir_id: HirId::new(3),
                    kind: ExprKind::Path(Box::new(hir::Path::new(vec![], Default::default()))),
                    ty: Default::default(),
                    span: Default::default(),
                },
                arguments: vec![],
            })),
            ty: Default::default(),
            span: Default::default(),
        };
        assert!(expr_has_side_effects(&call));
    }

    #[test]
    fn literal_is_pure() {
        let lit = hir::Expr {
            hir_id: HirId::new(1),
            kind: ExprKind::Literal(Box::new(tlang_ast::token::Literal::Integer(42))),
            ty: Default::default(),
            span: Default::default(),
        };
        assert!(!expr_has_side_effects(&lit));
    }
}
