use std::collections::{HashMap, HashSet};

use tlang_defs::{DefId, DefKind};
use tlang_hir::{
    self as hir, ExprKind, PatKind, Slot, StmtKind, TyKind,
    visit::{self, Visitor},
};
use tlang_span::HirId;

use crate::hir_opt::{HirOptContext, HirOptError, HirPass};

/// Maps an enum's `HirId` to the number of variants it declares.
type EnumVariantCounts = HashMap<HirId, usize>;

/// Maps a variant's `HirId` back to its parent enum `HirId`.
type VariantToEnum = HashMap<HirId, HirId>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum EnumIdentity {
    Hir(HirId),
    Symbol(DefId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum VariantIdentity {
    Hir(HirId),
    Global(usize),
}

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
    enum_paths: HashMap<String, EnumIdentity>,
    builtin_variant_to_enum: HashMap<usize, EnumIdentity>,
    builtin_variant_counts: HashMap<EnumIdentity, usize>,
    changed: bool,
}

impl ExhaustiveEnumMatch {
    pub fn new() -> Self {
        Self {
            enum_variant_counts: HashMap::new(),
            variant_to_enum: HashMap::new(),
            enum_paths: HashMap::new(),
            builtin_variant_to_enum: HashMap::new(),
            builtin_variant_counts: HashMap::new(),
            changed: false,
        }
    }

    /// Pre-populate lookup tables by scanning enum declarations throughout the
    /// whole module, including nested blocks.
    fn build_enum_maps(&mut self, module: &hir::Module, ctx: &HirOptContext) {
        self.enum_variant_counts.clear();
        self.variant_to_enum.clear();
        self.enum_paths.clear();
        self.builtin_variant_to_enum.clear();
        self.builtin_variant_counts.clear();

        Self::collect_enum_declarations(
            module,
            &mut self.enum_variant_counts,
            &mut self.variant_to_enum,
            &mut self.enum_paths,
        );
        self.collect_symbol_enums(ctx);
    }

    fn collect_enum_declarations(
        module: &hir::Module,
        counts: &mut EnumVariantCounts,
        variant_map: &mut VariantToEnum,
        paths: &mut HashMap<String, EnumIdentity>,
    ) {
        Self::collect_enum_declarations_from_block(&module.block, counts, variant_map, paths);
    }

    fn collect_enum_declarations_from_block(
        block: &hir::Block,
        counts: &mut EnumVariantCounts,
        variant_map: &mut VariantToEnum,
        paths: &mut HashMap<String, EnumIdentity>,
    ) {
        for stmt in &block.stmts {
            Self::collect_enum_declarations_from_stmt(stmt, counts, variant_map, paths);
        }

        if let Some(expr) = &block.expr {
            Self::collect_enum_declarations_from_expr(expr, counts, variant_map, paths);
        }
    }

    fn collect_enum_declarations_from_stmt(
        stmt: &hir::Stmt,
        counts: &mut EnumVariantCounts,
        variant_map: &mut VariantToEnum,
        paths: &mut HashMap<String, EnumIdentity>,
    ) {
        match &stmt.kind {
            StmtKind::EnumDeclaration(decl) => {
                counts.insert(decl.hir_id, decl.variants.len());
                paths.insert(
                    decl.name.as_str().to_string(),
                    EnumIdentity::Hir(decl.hir_id),
                );
                for variant in &decl.variants {
                    variant_map.insert(variant.hir_id, decl.hir_id);
                }
            }
            StmtKind::Expr(expr) => {
                Self::collect_enum_declarations_from_expr(expr, counts, variant_map, paths);
            }
            StmtKind::Return(Some(expr)) => {
                Self::collect_enum_declarations_from_expr(expr, counts, variant_map, paths);
            }
            StmtKind::Let(_, expr, _) | StmtKind::Const(_, _, expr, _) => {
                Self::collect_enum_declarations_from_expr(expr, counts, variant_map, paths);
            }
            StmtKind::FunctionDeclaration(decl) => {
                Self::collect_enum_declarations_from_block(&decl.body, counts, variant_map, paths);
            }
            StmtKind::DynFunctionDeclaration(_) => {}
            StmtKind::StructDeclaration(decl) => {
                for const_item in &decl.consts {
                    Self::collect_enum_declarations_from_expr(
                        &const_item.value,
                        counts,
                        variant_map,
                        paths,
                    );
                }
            }
            StmtKind::ProtocolDeclaration(decl) => {
                for method in &decl.methods {
                    if let Some(body) = &method.body {
                        Self::collect_enum_declarations_from_block(
                            body,
                            counts,
                            variant_map,
                            paths,
                        );
                    }
                }
                for const_item in &decl.consts {
                    Self::collect_enum_declarations_from_expr(
                        &const_item.value,
                        counts,
                        variant_map,
                        paths,
                    );
                }
            }
            StmtKind::ImplBlock(decl) => {
                for method in &decl.methods {
                    Self::collect_enum_declarations_from_block(
                        &method.body,
                        counts,
                        variant_map,
                        paths,
                    );
                }
            }
            StmtKind::Return(None) => {}
        }
    }

    fn collect_enum_declarations_from_expr(
        expr: &hir::Expr,
        counts: &mut EnumVariantCounts,
        variant_map: &mut VariantToEnum,
        paths: &mut HashMap<String, EnumIdentity>,
    ) {
        match &expr.kind {
            ExprKind::Block(block) | ExprKind::Loop(block) => {
                Self::collect_enum_declarations_from_block(block, counts, variant_map, paths);
            }
            ExprKind::Break(Some(expr))
            | ExprKind::Unary(_, expr)
            | ExprKind::FieldAccess(expr, _) => {
                Self::collect_enum_declarations_from_expr(expr, counts, variant_map, paths);
            }
            ExprKind::Call(call) | ExprKind::TailCall(call) => {
                Self::collect_enum_declarations_from_expr(&call.callee, counts, variant_map, paths);
                for arg in &call.arguments {
                    Self::collect_enum_declarations_from_expr(arg, counts, variant_map, paths);
                }
            }
            ExprKind::Cast(expr, _)
            | ExprKind::TryCast(expr, _)
            | ExprKind::IndexAccess(expr, _)
            | ExprKind::Implements(expr, _) => {
                Self::collect_enum_declarations_from_expr(expr, counts, variant_map, paths);
            }
            ExprKind::Binary(_, left, right) => {
                Self::collect_enum_declarations_from_expr(left, counts, variant_map, paths);
                Self::collect_enum_declarations_from_expr(right, counts, variant_map, paths);
            }
            ExprKind::Let(_, expr) => {
                Self::collect_enum_declarations_from_expr(expr, counts, variant_map, paths);
            }
            ExprKind::IfElse(condition, consequence, else_clauses) => {
                Self::collect_enum_declarations_from_expr(condition, counts, variant_map, paths);
                Self::collect_enum_declarations_from_block(consequence, counts, variant_map, paths);
                for else_clause in else_clauses {
                    if let Some(condition) = &else_clause.condition {
                        Self::collect_enum_declarations_from_expr(
                            condition,
                            counts,
                            variant_map,
                            paths,
                        );
                    }
                    Self::collect_enum_declarations_from_block(
                        &else_clause.consequence,
                        counts,
                        variant_map,
                        paths,
                    );
                }
            }
            ExprKind::List(items) => {
                for item in items {
                    Self::collect_enum_declarations_from_expr(item, counts, variant_map, paths);
                }
            }
            ExprKind::Dict(items) => {
                for (key, value) in items {
                    Self::collect_enum_declarations_from_expr(key, counts, variant_map, paths);
                    Self::collect_enum_declarations_from_expr(value, counts, variant_map, paths);
                }
            }
            ExprKind::Match(scrutinee, arms, _) => {
                Self::collect_enum_declarations_from_expr(scrutinee, counts, variant_map, paths);
                for arm in arms {
                    if let Some(guard) = &arm.guard {
                        Self::collect_enum_declarations_from_expr(
                            guard,
                            counts,
                            variant_map,
                            paths,
                        );
                    }
                    Self::collect_enum_declarations_from_block(
                        &arm.block,
                        counts,
                        variant_map,
                        paths,
                    );
                }
            }
            ExprKind::Range(range) => {
                Self::collect_enum_declarations_from_expr(&range.start, counts, variant_map, paths);
                Self::collect_enum_declarations_from_expr(&range.end, counts, variant_map, paths);
            }
            ExprKind::TaggedString { tag, exprs, .. } => {
                Self::collect_enum_declarations_from_expr(tag, counts, variant_map, paths);
                for expr in exprs {
                    Self::collect_enum_declarations_from_expr(expr, counts, variant_map, paths);
                }
            }
            ExprKind::FunctionExpression(decl) => {
                Self::collect_enum_declarations_from_block(&decl.body, counts, variant_map, paths);
            }
            ExprKind::Path(_)
            | ExprKind::Literal(_)
            | ExprKind::Wildcard
            | ExprKind::Continue
            | ExprKind::Break(None) => {}
        }
    }

    fn collect_symbol_enums(&mut self, ctx: &HirOptContext) {
        for scope in ctx.symbols.values() {
            for symbol in scope.read().unwrap().get_all_declared_local_symbols() {
                if symbol.kind == DefKind::Enum {
                    let identity = symbol
                        .hir_id
                        .map(EnumIdentity::Hir)
                        .unwrap_or(EnumIdentity::Symbol(symbol.id));
                    self.enum_paths
                        .entry(symbol.name.to_string())
                        .or_insert(identity);
                }
            }
        }

        for scope in ctx.symbols.values() {
            for symbol in scope.read().unwrap().get_all_declared_local_symbols() {
                let DefKind::EnumVariant(_) = symbol.kind else {
                    continue;
                };

                let Some((enum_name, _)) = symbol.name.rsplit_once("::") else {
                    continue;
                };
                let Some(identity) = self.enum_paths.get(enum_name).copied() else {
                    continue;
                };
                let Some(global_slot) = symbol.global_slot else {
                    continue;
                };

                self.builtin_variant_to_enum.insert(global_slot, identity);
                *self.builtin_variant_counts.entry(identity).or_insert(0) += 1;
            }
        }
    }

    fn resolve_variant_identity(&self, path: &hir::Path) -> Option<VariantIdentity> {
        if let Some(variant_hir_id) = path.res.hir_id() {
            return Some(VariantIdentity::Hir(variant_hir_id));
        }

        if let Slot::Global(slot) = path.res.slot() {
            return Some(VariantIdentity::Global(slot));
        }

        None
    }

    fn resolve_enum_identity(&self, pat: &hir::Pat) -> Option<EnumIdentity> {
        if let PatKind::Enum(path, _) = &pat.kind {
            if let Some(enum_hir_id) = pat.ty.res
                && self.enum_variant_counts.contains_key(&enum_hir_id)
            {
                return Some(EnumIdentity::Hir(enum_hir_id));
            }

            if let Some(variant_hir_id) = path.res.hir_id()
                && let Some(&enum_hir_id) = self.variant_to_enum.get(&variant_hir_id)
            {
                return Some(EnumIdentity::Hir(enum_hir_id));
            }

            if let Slot::Global(slot) = path.res.slot()
                && let Some(enum_identity) = self.builtin_variant_to_enum.get(&slot)
            {
                return Some(*enum_identity);
            }
        }

        None
    }

    fn resolve_type_enum_identity(&self, ty: &hir::Ty) -> Option<EnumIdentity> {
        if let TyKind::Path(ref path, _) = ty.kind {
            if let Some(res_hir_id) = ty.res
                && self.enum_variant_counts.contains_key(&res_hir_id)
            {
                return Some(EnumIdentity::Hir(res_hir_id));
            }

            if let Some(hir_id) = path.res.hir_id()
                && self.enum_variant_counts.contains_key(&hir_id)
            {
                return Some(EnumIdentity::Hir(hir_id));
            }

            if path.res.is_enum_def() {
                return self.enum_paths.get(&path.to_string()).copied();
            }
        }

        None
    }

    fn total_variant_count(&self, enum_identity: EnumIdentity) -> Option<usize> {
        match enum_identity {
            EnumIdentity::Hir(hir_id) => self.enum_variant_counts.get(&hir_id).copied(),
            EnumIdentity::Symbol(def_id) => self
                .builtin_variant_counts
                .get(&EnumIdentity::Symbol(def_id))
                .copied(),
        }
    }

    fn analyze_exhaustiveness(
        &self,
        scrutinee: &hir::Expr,
        arms: &[hir::MatchArm],
    ) -> Option<(EnumIdentity, bool)> {
        if arms.is_empty() {
            return None;
        }

        if arms.iter().any(|arm| arm.guard.is_some()) {
            return None;
        }

        let has_trailing_catchall = arms
            .last()
            .is_some_and(|arm| is_catch_all_pattern(&arm.pat));
        let explicit_arms = if has_trailing_catchall {
            &arms[..arms.len() - 1]
        } else {
            arms
        };

        if explicit_arms.is_empty() {
            return None;
        }

        let mut enum_identity: Option<EnumIdentity> = None;
        let mut covered_variants: HashSet<VariantIdentity> = HashSet::new();

        for arm in explicit_arms {
            {
                let identity = self.resolve_enum_identity(&arm.pat)?;
                match enum_identity {
                    None => enum_identity = Some(identity),
                    Some(existing) if existing != identity => return None, // mixed enums
                    _ => {}
                }

                // Only count the variant as covered if all payload sub-patterns
                // are catch-all (identifier or wildcard). A restrictive
                // sub-pattern (literal, nested enum, list, etc.) does not fully
                // cover the variant and might not match all values.
                if let PatKind::Enum(path, fields) = &arm.pat.kind
                    && let Some(variant_identity) = self.resolve_variant_identity(path)
                    && fields.iter().all(|(_, p)| is_catch_all_pattern(p))
                {
                    covered_variants.insert(variant_identity);
                }
            }
        }

        let enum_identity = enum_identity?;

        let scrutinee_enum_identity = if has_trailing_catchall {
            self.resolve_type_enum_identity(&arms.last().unwrap().pat.ty)
        } else {
            self.resolve_type_enum_identity(&scrutinee.ty)
        };

        if scrutinee_enum_identity != Some(enum_identity) {
            return None;
        }

        let total_variants = self.total_variant_count(enum_identity)?;

        if covered_variants.len() >= total_variants {
            Some((enum_identity, has_trailing_catchall))
        } else {
            None
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

        if let ExprKind::Match(scrutinee, arms, metadata) = &mut expr.kind {
            let was_exhaustive = metadata.exhaustive;
            metadata.exhaustive = self.analyze_exhaustiveness(scrutinee, arms).is_some();

            if metadata.exhaustive != was_exhaustive {
                self.changed = true;
            }

            if metadata.exhaustive
                && arms.len() >= 2
                && arms
                    .last()
                    .is_some_and(|arm| is_catch_all_pattern(&arm.pat))
            {
                arms.pop();
                self.changed = true;
            }
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
        ctx: &mut HirOptContext,
    ) -> Result<bool, HirOptError> {
        self.changed = false;
        self.build_enum_maps(module, ctx);
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
    use tlang_ast::node::{Ident, Visibility};

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

    #[test]
    fn build_enum_maps_collects_nested_enums() {
        let enum_hir_id = HirId::new(10);
        let variant_hir_id = HirId::new(11);
        let enum_stmt = hir::Stmt::new(
            HirId::new(12),
            StmtKind::EnumDeclaration(Box::new(hir::EnumDeclaration {
                hir_id: enum_hir_id,
                visibility: Visibility::Private,
                name: Ident::new("Local", Default::default()),
                type_params: vec![],
                variants: vec![hir::EnumVariant {
                    hir_id: variant_hir_id,
                    name: Ident::new("Only", Default::default()),
                    parameters: vec![],
                    discriminant: None,
                    span: Default::default(),
                }],
                consts: vec![],
            })),
            Default::default(),
        );

        let nested_block =
            hir::Block::new(HirId::new(13), vec![enum_stmt], None, Default::default());
        let module = hir::Module {
            hir_id: HirId::new(1),
            block: hir::Block::new(
                HirId::new(2),
                vec![hir::Stmt::new(
                    HirId::new(3),
                    StmtKind::Expr(Box::new(hir::Expr {
                        hir_id: HirId::new(4),
                        kind: ExprKind::Block(Box::new(nested_block)),
                        ty: hir::Ty::unknown(),
                        span: Default::default(),
                    })),
                    Default::default(),
                )],
                None,
                Default::default(),
            ),
            span: Default::default(),
        };

        let mut pass = ExhaustiveEnumMatch::default();
        let ctx = HirOptContext {
            symbols: Default::default(),
            hir_id_allocator: Default::default(),
            current_scope: HirId::new(1),
            diagnostics: Vec::new(),
        };
        pass.build_enum_maps(&module, &ctx);

        assert_eq!(pass.enum_variant_counts.get(&enum_hir_id), Some(&1));
        assert_eq!(
            pass.variant_to_enum.get(&variant_hir_id),
            Some(&enum_hir_id)
        );
    }
}
