use std::sync::{Arc, RwLock};

use log::debug;
use tlang_defs::{DefKind, DefScope};
use tlang_diagnostics::Diagnostic;
use tlang_hir::{self as hir, Visitor};
use tlang_span::HirId;

use crate::hir_opt::{HirOptContext, HirOptError, HirPass};

/// A HIR pass that detects unused struct fields, dot-methods, and struct
/// method aliases.
///
/// These symbol categories require type information to resolve
/// `FieldAccess` expressions (e.g. `v.x` → `Vector::x`) and dot-method
/// calls (e.g. `v.add(w)` → `Vector.add`). The AST-level
/// `VariableUsageValidator` suppresses them because it runs before type
/// checking.
///
/// This pass runs **after** the type checker so that every expression has a
/// resolved type. It walks the typed HIR, marks type-dependent symbols as
/// used, and then reports any that remain unused.
///
/// The pass is guarded so it only runs once per compilation.
#[derive(Default)]
pub struct UnusedSymbolDetector {
    /// Guard to ensure the pass only runs once per compilation.
    ran: bool,
    /// Stack of scope [`HirId`]s maintained during HIR traversal.
    scopes: Vec<HirId>,
}

impl HirPass for UnusedSymbolDetector {
    fn name(&self) -> &'static str {
        "UnusedSymbolDetector"
    }

    fn optimize_hir(
        &mut self,
        module: &mut hir::Module,
        ctx: &mut HirOptContext,
    ) -> Result<bool, HirOptError> {
        if self.ran {
            return Ok(false);
        }
        self.ran = true;

        // Initialize scope stack with the current (root) scope.
        self.scopes.push(ctx.current_scope);

        // Phase 1: Walk the typed HIR and mark type-dependent symbols as used.
        self.visit_module(module, ctx);

        // Phase 2: Report any remaining unused type-dependent symbols.
        self.report_unused_type_dependent_symbols(ctx);

        Ok(false)
    }
}

impl UnusedSymbolDetector {
    /// Extract the type name from a `TyKind`, if it is a user-defined
    /// struct/enum path.
    fn type_name_from_ty(ty: &hir::TyKind) -> Option<String> {
        match ty {
            hir::TyKind::Path(path, _) => Some(path.to_string()),
            _ => None,
        }
    }

    /// Mark the struct field symbol `TypeName::field` as used in the current
    /// scope's symbol table.
    fn mark_field_as_used(&self, type_name: &str, field_name: &str, ctx: &mut HirOptContext) {
        let qualified = format!("{type_name}::{field_name}");
        self.mark_symbol_as_used_by_name(&qualified, ctx);
    }

    /// Mark a dot-method symbol `TypeName.method` and its `::` alias
    /// `TypeName::method` as used in the current scope's symbol table.
    fn mark_method_as_used(
        &self,
        type_name: &str,
        method_name: &str,
        arity: usize,
        ctx: &mut HirOptContext,
    ) {
        let dot_name = format!("{type_name}.{method_name}");
        self.mark_symbol_as_used_by_name_and_arity(&dot_name, arity, ctx);

        let qualified = format!("{type_name}::{method_name}");
        self.mark_symbol_as_used_by_name_and_arity(&qualified, arity, ctx);
    }

    /// Look up a symbol by name in the current scope and mark it as used.
    fn mark_symbol_as_used_by_name(&self, name: &str, ctx: &mut HirOptContext) {
        let Some(symbol_table) = ctx.current_symbol_table() else {
            return;
        };

        let ids: Vec<_> = symbol_table
            .read()
            .unwrap()
            .get_by_name(name)
            .into_iter()
            .map(|s| s.id)
            .collect();

        for id in ids {
            debug!("UnusedSymbolDetector: marking `{name}` as used");
            symbol_table.write().unwrap().mark_as_used(id);
        }
    }

    /// Look up a symbol by name and arity in the current scope and mark it
    /// as used.
    fn mark_symbol_as_used_by_name_and_arity(
        &self,
        name: &str,
        arity: usize,
        ctx: &mut HirOptContext,
    ) {
        let Some(symbol_table) = ctx.current_symbol_table() else {
            return;
        };

        let ids: Vec<_> = symbol_table
            .read()
            .unwrap()
            .get_by_name_and_arity(name, arity)
            .into_iter()
            .map(|s| s.id)
            .collect();

        if ids.is_empty() {
            // Fall back to name-only lookup (e.g. for struct fields).
            self.mark_symbol_as_used_by_name(name, ctx);
            return;
        }

        for id in ids {
            debug!("UnusedSymbolDetector: marking `{name}/{arity}` as used");
            symbol_table.write().unwrap().mark_as_used(id);
        }
    }

    /// After walking the HIR, iterate all symbol tables and report any
    /// type-dependent symbols that are still unused.
    fn report_unused_type_dependent_symbols(&self, ctx: &mut HirOptContext) {
        for symbol_table in ctx.symbols.values() {
            Self::report_unused_in_scope(symbol_table, &mut ctx.diagnostics);
        }
    }

    fn report_unused_in_scope(
        symbol_table: &Arc<RwLock<DefScope>>,
        diagnostics: &mut Vec<Diagnostic>,
    ) {
        let table = symbol_table.read().unwrap();
        let unused: Vec<_> = table
            .get_all_declared_local_symbols()
            .filter(|s| !s.used)
            .filter(|s| !s.is_builtin())
            // Do not report the binding introduced within function bodies to
            // reference themselves (same filter as VariableUsageValidator).
            .filter(|s| !s.is_fn_self_binding())
            .filter(|s| !has_underscore_prefix(&s.name))
            .filter(is_type_dependent_symbol)
            .collect();

        for symbol in &unused {
            if symbol.is_any_fn() {
                diagnostics.push(Diagnostic::warn(
                    &format!(
                        "Unused {} `{}/{}`",
                        symbol.kind,
                        symbol.name,
                        symbol.kind.arity().unwrap_or(0),
                    ),
                    symbol.defined_at,
                ));
            } else {
                diagnostics.push(Diagnostic::warn(
                    &format!(
                        "Unused {} `{}`, if this is intentional, prefix the name with an underscore: `_{}`",
                        symbol.kind, symbol.name, symbol.name
                    ),
                    symbol.defined_at,
                ));
            }
        }
    }

    /// Process a `FieldAccess(base, field)` expression. If the base has a
    /// resolved struct/enum type, mark the corresponding field or method
    /// symbol as used.
    fn process_field_access(
        &self,
        base: &hir::Expr,
        field_name: &str,
        is_method_call: bool,
        call_arity: usize,
        ctx: &mut HirOptContext,
    ) {
        let Some(type_name) = Self::type_name_from_ty(&base.ty.kind) else {
            return;
        };

        if is_method_call {
            self.mark_method_as_used(&type_name, field_name, call_arity, ctx);
        } else {
            // Could be a struct field access or a method reference (without call).
            self.mark_field_as_used(&type_name, field_name, ctx);
            // Also try dot-method lookup in case it's a method reference.
            let dot_name = format!("{type_name}.{field_name}");
            self.mark_symbol_as_used_by_name(&dot_name, ctx);
            let alias_name = format!("{type_name}::{field_name}");
            self.mark_symbol_as_used_by_name(&alias_name, ctx);
        }
    }
}

/// Returns `true` if the symbol is one of the type-dependent categories that
/// the AST-level `VariableUsageValidator` suppresses.
fn is_type_dependent_symbol(symbol: &tlang_defs::Def) -> bool {
    matches!(symbol.kind, DefKind::StructField | DefKind::StructMethod(_))
}

/// Returns `true` if the symbol's effective local/member name starts with `_`.
///
/// For qualified names like `Point::_internal`, this checks the segment after
/// the last `::`. For dot-member names like `Vector._debug`, it checks the
/// segment after the final `.`.
fn has_underscore_prefix(name: &str) -> bool {
    let local_name = name
        .rsplit_once("::")
        .or_else(|| name.rsplit_once('.'))
        .map(|(_, rhs)| rhs)
        .unwrap_or(name);
    local_name.starts_with('_')
}

// ── HIR Visitor implementation ──────────────────────────────────────────

impl<'hir> hir::Visitor<'hir> for UnusedSymbolDetector {
    type Context = HirOptContext;

    fn enter_scope(&mut self, hir_id: HirId, ctx: &mut Self::Context) {
        if ctx.symbols.contains_key(&hir_id) {
            ctx.current_scope = hir_id;
            self.scopes.push(hir_id);
        }
    }

    fn leave_scope(&mut self, hir_id: HirId, ctx: &mut Self::Context) {
        if ctx.symbols.contains_key(&hir_id) {
            let left = self.scopes.pop();
            debug_assert!(left == Some(hir_id), "Mismatched scope exit");
            ctx.current_scope = self.scopes.last().copied().unwrap();
        }
    }

    fn visit_expr(&mut self, expr: &'hir mut hir::Expr, ctx: &mut Self::Context) {
        match &mut expr.kind {
            // Method call via dot-notation: `base.method(args)`
            hir::ExprKind::Call(call) | hir::ExprKind::TailCall(call) => {
                // Visit arguments first.
                for arg in &mut call.arguments {
                    self.visit_expr(arg, ctx);
                }

                if let hir::ExprKind::FieldAccess(base, ident) = &mut call.callee.kind {
                    // Visit the base expression.
                    self.visit_expr(base, ctx);

                    let field_name = ident.as_str().to_owned();
                    // Arity = number of arguments + 1 for `self`.
                    let arity = call.arguments.len() + 1;
                    self.process_field_access(base, &field_name, true, arity, ctx);
                } else {
                    self.visit_expr(&mut call.callee, ctx);
                }
            }
            // Field access: `base.field`
            hir::ExprKind::FieldAccess(base, ident) => {
                self.visit_expr(base, ctx);
                let field_name = ident.as_str().to_owned();
                self.process_field_access(base, &field_name, false, 0, ctx);
            }
            _ => hir::visit::walk_expr(self, expr, ctx),
        }
    }

    fn visit_pat(&mut self, pat: &'hir mut hir::Pat, ctx: &mut Self::Context) {
        if let hir::PatKind::Enum(path, fields) = &mut pat.kind
            && matches!(path.res.binding_kind(), hir::BindingKind::Struct)
        {
            let type_name = path.to_string();
            for (field, nested_pat) in fields {
                self.mark_field_as_used(&type_name, field.as_str(), ctx);
                self.visit_pat(nested_pat, ctx);
            }
            self.visit_path(path, ctx);
            return;
        }

        hir::visit::walk_pat(self, pat, ctx);
    }
}
