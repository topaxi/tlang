use std::collections::{HashMap, HashSet};

use tlang_hir::{
    self as hir, ExprKind, PatKind, StmtKind, TyKind,
    visit::{self, Visitor},
};
use tlang_span::HirId;

use crate::hir_opt::{HirOptContext, HirOptError, HirPass};

/// Maps an enum's `HirId` to the number of variants it declares.
type EnumVariantCounts = HashMap<HirId, usize>;

/// Maps a variant's `HirId` back to its parent enum `HirId`.
type VariantToEnum = HashMap<HirId, HirId>;

/// HIR optimisation pass that removes an unreachable trailing wildcard (or
/// catch-all identifier) arm from a `match` expression when every variant of
/// the matched enum is already covered by explicit `PatKind::Enum` arms.
///
/// **Must run after type checking.** The pass relies on the type checker
/// having populated `pat.ty.kind` on each match arm (including catch-alls)
/// with the scrutinee's type. This gives every catch-all pattern a
/// `TyKind::Path` whose inner `path.res.hir_id()` reliably identifies the
/// enum declaration — no string-name matching required.
///
/// The pass is intentionally conservative:
///
/// * It only fires when **all** non-wildcard arms are `PatKind::Enum` patterns
///   that resolve to variants of the **same** user-defined enum.
/// * It bails out when **any** arm carries a guard expression — a guard might
///   fail, causing fall-through to the wildcard.
/// * Only the **last** arm is considered for removal, and only when it is a
///   wildcard or a bare identifier (both are catch-all patterns).
/// * The trailing catch-all's type must resolve to the **same** enum as the
///   explicit variant arms. An `unknown` (or untyped) catch-all, or one typed
///   with a different type, is preserved.
/// * Each variant arm must only use catch-all sub-patterns (identifiers or
///   wildcards) in its payload fields. A restrictive sub-pattern (e.g. a
///   literal like `Enum::V(0)`) does not fully cover the variant.
pub struct ExhaustiveEnumMatch {
    enum_variant_counts: EnumVariantCounts,
    variant_to_enum: VariantToEnum,
    changed: bool,
}

impl ExhaustiveEnumMatch {
    pub fn new() -> Self {
        Self {
            enum_variant_counts: HashMap::new(),
            variant_to_enum: HashMap::new(),
            changed: false,
        }
    }

    /// Pre-populate lookup tables by scanning top-level enum declarations.
    fn build_enum_maps(&mut self, module: &hir::Module) {
        self.enum_variant_counts.clear();
        self.variant_to_enum.clear();

        Self::collect_enum_declarations(
            &module.block,
            &mut self.enum_variant_counts,
            &mut self.variant_to_enum,
        );
    }

    fn collect_enum_declarations(
        block: &hir::Block,
        counts: &mut EnumVariantCounts,
        variant_map: &mut VariantToEnum,
    ) {
        for stmt in &block.stmts {
            if let StmtKind::EnumDeclaration(decl) = &stmt.kind {
                counts.insert(decl.hir_id, decl.variants.len());
                for variant in &decl.variants {
                    variant_map.insert(variant.hir_id, decl.hir_id);
                }
            }
        }
    }

    /// Given a match arm's pattern, try to determine the parent enum `HirId`
    /// of the variant it matches.
    ///
    /// Two resolution strategies (tried in order):
    ///
    /// 1. **Pattern type annotation** (`pat.ty.res`) — reliable when the
    ///    pattern has an explicit type annotation that resolved to the enum
    ///    declaration `HirId`.
    /// 2. **Variant path resolution** (`path.res.hir_id()`) — the primary
    ///    strategy. After `IdentifierResolver`, the variant path points to
    ///    the variant declaration whose `HirId` is in `variant_to_enum`.
    fn resolve_enum_hir_id(&self, pat: &hir::Pat) -> Option<HirId> {
        if let PatKind::Enum(path, _) = &pat.kind {
            // Strategy 1: populated by lowering for patterns with explicit
            // `EnumName` type annotations; inferred annotations from
            // FnParamTypeInference use a synthetic NodeId and typically do not
            // resolve to the enum declaration.
            if let Some(enum_hir_id) = pat.ty.res
                && self.enum_variant_counts.contains_key(&enum_hir_id)
            {
                return Some(enum_hir_id);
            }

            // Strategy 2: variant path resolved to variant HirId → look up parent.
            if let Some(variant_hir_id) = path.res.hir_id()
                && let Some(&enum_hir_id) = self.variant_to_enum.get(&variant_hir_id)
            {
                return Some(enum_hir_id);
            }
        }

        None
    }

    /// Resolve the enum `HirId` from the catch-all pattern's type annotation.
    ///
    /// After type checking, the type checker sets `pat.ty.kind` on every match
    /// arm to the scrutinee's type. For enum-dispatching matches this is a
    /// `TyKind::Path(path, _)` whose `path.res.hir_id()` identifies the enum
    /// declaration.
    ///
    /// Returns `None` when:
    /// - The type is `Unknown` (untyped / explicitly `unknown`-annotated)
    /// - The type is not a `Path` (e.g. a primitive like `i64`)
    /// - The path does not resolve to a known enum declaration
    fn resolve_catchall_enum_hir_id(&self, pat: &hir::Pat) -> Option<HirId> {
        if let TyKind::Path(ref path, _) = pat.ty.kind {
            // Primary: pat.ty.res set by lowering for explicit annotations.
            if let Some(res_hir_id) = pat.ty.res
                && self.enum_variant_counts.contains_key(&res_hir_id)
            {
                return Some(res_hir_id);
            }

            // Fallback: path.res.hir_id() set by the type checker from the
            // scrutinee's resolved type.
            if let Some(hir_id) = path.res.hir_id()
                && self.enum_variant_counts.contains_key(&hir_id)
            {
                return Some(hir_id);
            }
        }

        None
    }

    /// Returns `true` if the trailing arm was removed.
    fn try_remove_wildcard_arm(&self, arms: &mut Vec<hir::MatchArm>) -> bool {
        // Need at least two arms: one or more explicit + a trailing catch-all.
        if arms.len() < 2 {
            return false;
        }

        let last = arms.last().unwrap();

        // Only remove catch-all arms (wildcard `_` or bare identifier).
        if !is_catch_all_pattern(&last.pat) {
            return false;
        }

        // Bail out if *any* arm (including the catch-all) has a guard.
        if arms.iter().any(|arm| arm.guard.is_some()) {
            return false;
        }

        // All non-wildcard arms must be Enum patterns resolving to the same enum.
        let non_catchall_arms = &arms[..arms.len() - 1];

        let mut enum_hir_id: Option<HirId> = None;
        let mut covered_variants: HashSet<HirId> = HashSet::new();

        for arm in non_catchall_arms {
            if let Some(eid) = self.resolve_enum_hir_id(&arm.pat) {
                match enum_hir_id {
                    None => enum_hir_id = Some(eid),
                    Some(existing) if existing != eid => return false, // mixed enums
                    _ => {}
                }

                // Only count the variant as covered if all payload sub-patterns
                // are catch-all (identifier or wildcard). A restrictive
                // sub-pattern (literal, nested enum, list, etc.) does not fully
                // cover the variant and might not match all values.
                if let PatKind::Enum(path, fields) = &arm.pat.kind
                    && let Some(variant_hir_id) = path.res.hir_id()
                    && fields.iter().all(|(_, p)| is_catch_all_pattern(p))
                {
                    covered_variants.insert(variant_hir_id);
                }
            } else {
                // Non-enum pattern among the explicit arms → bail.
                return false;
            }
        }

        let enum_hir_id = match enum_hir_id {
            Some(id) => id,
            None => return false,
        };

        // The catch-all's type must resolve to the same enum as the explicit
        // arms. After type checking, the type checker populates `pat.ty.kind`
        // with the scrutinee's type, so this check is reliable.
        if self.resolve_catchall_enum_hir_id(&arms.last().unwrap().pat) != Some(enum_hir_id) {
            return false;
        }

        let total_variants = match self.enum_variant_counts.get(&enum_hir_id) {
            Some(&count) => count,
            None => return false,
        };

        if covered_variants.len() >= total_variants {
            arms.pop(); // remove trailing catch-all
            true
        } else {
            false
        }
    }
}

impl Default for ExhaustiveEnumMatch {
    fn default() -> Self {
        Self::new()
    }
}

impl<'hir> Visitor<'hir> for ExhaustiveEnumMatch {
    fn visit_expr(&mut self, expr: &'hir mut hir::Expr, ctx: &mut Self::Context) {
        // Visit children first (post-order).
        visit::walk_expr(self, expr, ctx);

        if let ExprKind::Match(_, arms) = &mut expr.kind
            && self.try_remove_wildcard_arm(arms)
        {
            self.changed = true;
        }
    }
}

impl HirPass for ExhaustiveEnumMatch {
    fn name(&self) -> &'static str {
        "ExhaustiveEnumMatch"
    }

    fn optimize_hir(
        &mut self,
        module: &mut hir::Module,
        _ctx: &mut HirOptContext,
    ) -> Result<bool, HirOptError> {
        self.changed = false;
        self.build_enum_maps(module);
        self.visit_module(module, &mut ());
        Ok(self.changed)
    }
}

/// A pattern is catch-all if it unconditionally matches any value without
/// constraining the shape: `_` (wildcard) or a bare identifier.
fn is_catch_all_pattern(pat: &hir::Pat) -> bool {
    matches!(pat.kind, PatKind::Wildcard | PatKind::Identifier(_, _))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn is_catch_all_wildcard() {
        let pat = hir::Pat {
            kind: PatKind::Wildcard,
            ty: hir::Ty::unknown(),
            span: Default::default(),
        };
        assert!(is_catch_all_pattern(&pat));
    }

    #[test]
    fn is_catch_all_identifier() {
        let pat = hir::Pat {
            kind: PatKind::Identifier(
                HirId::new(1),
                Box::new(tlang_ast::node::Ident::new("x", Default::default())),
            ),
            ty: hir::Ty::unknown(),
            span: Default::default(),
        };
        assert!(is_catch_all_pattern(&pat));
    }
}
