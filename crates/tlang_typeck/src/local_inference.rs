use std::collections::HashMap;

use tlang_ast::token::Literal;
use tlang_hir::{self as hir, BinaryOpKind, PrimTy, TyKind};
use tlang_span::{HirId, Span, TypeVarId, TypeVarIdAllocator};

use crate::unification::{UnificationError, UnificationTable};

/// Compute the coerced (widened) result type for an arithmetic operation between two
/// numeric primitive types, matching the main type-checker's `coerced_numeric_result`
/// semantics.
fn coerced_numeric_result(lhs: PrimTy, rhs: PrimTy) -> Option<PrimTy> {
    if !lhs.is_numeric() || !rhs.is_numeric() {
        return None;
    }
    if lhs == rhs {
        return Some(lhs);
    }
    if lhs.is_float() || rhs.is_float() {
        return Some(PrimTy::F64);
    }
    if lhs.is_signed_integer() && rhs.is_signed_integer() {
        return Some(PrimTy::I64);
    }
    if lhs.is_unsigned_integer() && rhs.is_unsigned_integer() {
        return Some(PrimTy::U64);
    }
    // Reach of this fallback: mixed signed/unsigned integer arithmetic (e.g.
    // i64 + u64).  The language does not have a dedicated "widest integer"
    // type that covers both, so we conservatively widen to f64, matching the
    // behaviour of the main type-checker's arithmetic coercion rules.
    Some(PrimTy::F64)
}

#[derive(Debug, Clone)]
pub(crate) struct LocalBindingSeed {
    pub(crate) type_var_id: TypeVarId,
    pub(crate) default_ty: TyKind,
    pub(crate) span: Span,
}

#[derive(Debug, Default, Clone)]
pub(crate) struct LocalInferenceScope {
    seeds_by_binding: HashMap<HirId, LocalBindingSeed>,
    binding_by_var: HashMap<TypeVarId, HirId>,
    solved_by_binding: HashMap<HirId, TyKind>,
}

#[derive(Debug, Clone)]
pub(crate) enum LocalInferenceError {
    Conflict {
        expected: Box<TyKind>,
        actual: Box<TyKind>,
        span: Span,
    },
    InfiniteType {
        type_var: TypeVarId,
        ty: Box<TyKind>,
        span: Span,
    },
}

impl LocalInferenceScope {
    pub(crate) fn collect(block: &hir::Block, allocator: &mut TypeVarIdAllocator) -> Self {
        let mut scope = Self::default();
        scope.collect_block_seeds(block, allocator);
        scope
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.seeds_by_binding.is_empty()
    }

    pub(crate) fn seed_type_for_binding(&self, hir_id: HirId) -> Option<TyKind> {
        if let Some(ty) = self.solved_by_binding.get(&hir_id) {
            return Some(ty.clone());
        }

        self.seeds_by_binding
            .get(&hir_id)
            .map(|seed| TyKind::Var(seed.type_var_id))
    }

    pub(crate) fn solve(
        &mut self,
        block: &hir::Block,
        expected_return: Option<&TyKind>,
    ) -> Result<bool, LocalInferenceError> {
        if self.is_empty() {
            return Ok(false);
        }

        let mut table = UnificationTable::new();
        for seed in self.seeds_by_binding.values() {
            table.register(seed.type_var_id);
        }

        // Collect arithmetic widening hints in a separate pass to avoid spurious
        // conflicts from equality-style unification of mixed numeric operands.
        // These are applied *after* equality constraints so explicit annotations
        // and assignment constraints take priority.
        let widening_hints = self.collect_block_widening_hints(block);

        self.collect_block_constraints(block, expected_return, &mut table)?;

        // Apply widening hints collected from arithmetic operations. These hints
        // represent the widest concrete numeric type seen as an operand to any
        // arithmetic op involving the local var. Errors are suppressed: if a var
        // is already bound by an explicit annotation or assignment constraint
        // (collected in the earlier collect_block_constraints pass), that binding
        // wins and the widening hint is simply ignored.
        for (&var_id, &prim) in &widening_hints {
            let _ = table.unify_var_ty(var_id, &TyKind::Primitive(prim));
        }

        self.solved_by_binding.clear();
        for (&binding_hir_id, seed) in &self.seeds_by_binding {
            let solved = table
                .probe(seed.type_var_id)
                .map(|ty| table.zonk(&ty))
                .filter(|ty| !matches!(ty, TyKind::Var(_)))
                .unwrap_or_else(|| seed.default_ty.clone());
            self.solved_by_binding.insert(binding_hir_id, solved);
        }

        Ok(true)
    }

    pub(crate) fn fallback_to_defaults(&mut self) {
        self.solved_by_binding.clear();
        for (&binding_hir_id, seed) in &self.seeds_by_binding {
            self.solved_by_binding
                .insert(binding_hir_id, seed.default_ty.clone());
        }
    }

    fn collect_block_seeds(&mut self, block: &hir::Block, allocator: &mut TypeVarIdAllocator) {
        for stmt in &block.stmts {
            self.collect_stmt_seeds(stmt, allocator);
        }
        if let Some(expr) = &block.expr {
            self.collect_expr_seeds(expr, allocator);
        }
    }

    fn collect_stmt_seeds(&mut self, stmt: &hir::Stmt, allocator: &mut TypeVarIdAllocator) {
        match &stmt.kind {
            hir::StmtKind::Let(pat, expr, ty) | hir::StmtKind::Const(_, pat, expr, ty) => {
                self.maybe_seed_binding(pat, expr, ty, stmt.span, allocator);
                self.collect_expr_seeds(expr, allocator);
            }
            hir::StmtKind::Expr(expr) => self.collect_expr_seeds(expr, allocator),
            hir::StmtKind::Return(Some(expr)) => self.collect_expr_seeds(expr, allocator),
            hir::StmtKind::FunctionDeclaration(_)
            | hir::StmtKind::DynFunctionDeclaration(_)
            | hir::StmtKind::EnumDeclaration(_)
            | hir::StmtKind::StructDeclaration(_)
            | hir::StmtKind::ProtocolDeclaration(_)
            | hir::StmtKind::ImplBlock(_)
            | hir::StmtKind::Return(None) => {}
        }
    }

    fn maybe_seed_binding(
        &mut self,
        pat: &hir::Pat,
        expr: &hir::Expr,
        ty: &hir::Ty,
        span: Span,
        allocator: &mut TypeVarIdAllocator,
    ) {
        if !matches!(ty.kind, TyKind::Unknown) {
            return;
        }
        let hir::PatKind::Identifier(hir_id, _) = &pat.kind else {
            return;
        };
        let Some(default_ty) = refineable_literal_default_ty(expr) else {
            return;
        };

        let type_var_id = allocator.next_id();
        self.seeds_by_binding.insert(
            *hir_id,
            LocalBindingSeed {
                type_var_id,
                default_ty,
                span,
            },
        );
        self.binding_by_var.insert(type_var_id, *hir_id);
    }

    fn collect_expr_seeds(&mut self, expr: &hir::Expr, allocator: &mut TypeVarIdAllocator) {
        match &expr.kind {
            hir::ExprKind::Block(block) | hir::ExprKind::Loop(block) => {
                self.collect_block_seeds(block, allocator);
            }
            hir::ExprKind::IfElse(condition, then_block, else_clauses) => {
                self.collect_expr_seeds(condition, allocator);
                self.collect_block_seeds(then_block, allocator);
                for clause in else_clauses {
                    if let Some(condition) = &clause.condition {
                        self.collect_expr_seeds(condition, allocator);
                    }
                    self.collect_block_seeds(&clause.consequence, allocator);
                }
            }
            hir::ExprKind::Match(scrutinee, arms) => {
                self.collect_expr_seeds(scrutinee, allocator);
                for arm in arms {
                    if let Some(guard) = &arm.guard {
                        self.collect_expr_seeds(guard, allocator);
                    }
                    self.collect_block_seeds(&arm.block, allocator);
                }
            }
            hir::ExprKind::Call(call) | hir::ExprKind::TailCall(call) => {
                self.collect_expr_seeds(&call.callee, allocator);
                for arg in &call.arguments {
                    self.collect_expr_seeds(arg, allocator);
                }
            }
            hir::ExprKind::Binary(_, lhs, rhs) => {
                self.collect_expr_seeds(lhs, allocator);
                self.collect_expr_seeds(rhs, allocator);
            }
            hir::ExprKind::Unary(_, operand)
            | hir::ExprKind::Cast(operand, _)
            | hir::ExprKind::TryCast(operand, _)
            | hir::ExprKind::Break(Some(operand))
            | hir::ExprKind::FieldAccess(operand, _)
            | hir::ExprKind::IndexAccess(operand, _)
            | hir::ExprKind::Implements(operand, _)
            | hir::ExprKind::Let(_, operand) => self.collect_expr_seeds(operand, allocator),
            hir::ExprKind::Range(range) => {
                self.collect_expr_seeds(&range.start, allocator);
                self.collect_expr_seeds(&range.end, allocator);
            }
            hir::ExprKind::List(elements) => {
                for element in elements {
                    self.collect_expr_seeds(element, allocator);
                }
            }
            hir::ExprKind::Dict(entries) => {
                for (key, value) in entries {
                    self.collect_expr_seeds(key, allocator);
                    self.collect_expr_seeds(value, allocator);
                }
            }
            hir::ExprKind::TaggedString { tag, exprs, .. } => {
                self.collect_expr_seeds(tag, allocator);
                for expr in exprs {
                    self.collect_expr_seeds(expr, allocator);
                }
            }
            hir::ExprKind::FunctionExpression(_)
            | hir::ExprKind::Break(None)
            | hir::ExprKind::Continue
            | hir::ExprKind::Literal(_)
            | hir::ExprKind::Path(_)
            | hir::ExprKind::Wildcard => {}
        }
    }

    fn collect_block_constraints(
        &self,
        block: &hir::Block,
        expected_return: Option<&TyKind>,
        table: &mut UnificationTable,
    ) -> Result<(), LocalInferenceError> {
        for stmt in &block.stmts {
            self.collect_stmt_constraints(stmt, expected_return, table)?;
        }

        if let Some(expr) = &block.expr {
            if let Some(expected_return) = expected_return {
                self.constrain_expr_to_type(expr, expected_return, expr.span, table)?;
            }
            self.collect_expr_constraints(expr, table)?;
        }

        Ok(())
    }

    fn collect_stmt_constraints(
        &self,
        stmt: &hir::Stmt,
        expected_return: Option<&TyKind>,
        table: &mut UnificationTable,
    ) -> Result<(), LocalInferenceError> {
        match &stmt.kind {
            hir::StmtKind::Let(pat, expr, ty) | hir::StmtKind::Const(_, pat, expr, ty) => {
                if let hir::PatKind::Identifier(binding_hir_id, _) = &pat.kind
                    && let Some(seed) = self.seeds_by_binding.get(binding_hir_id)
                    && is_constraining_ty(&ty.kind)
                {
                    table
                        .unify_var_ty(seed.type_var_id, &ty.kind)
                        .map_err(|err| self.map_unification_error(err, stmt.span))?;
                }
                self.collect_expr_constraints(expr, table)?;
                self.constrain_expr_to_type(expr, &ty.kind, stmt.span, table)?;
            }
            hir::StmtKind::Expr(expr) => self.collect_expr_constraints(expr, table)?,
            hir::StmtKind::Return(Some(expr)) => {
                self.collect_expr_constraints(expr, table)?;
                if let Some(ret_ty) = expected_return {
                    self.constrain_expr_to_type(expr, ret_ty, expr.span, table)?;
                }
            }
            hir::StmtKind::FunctionDeclaration(_)
            | hir::StmtKind::DynFunctionDeclaration(_)
            | hir::StmtKind::EnumDeclaration(_)
            | hir::StmtKind::StructDeclaration(_)
            | hir::StmtKind::ProtocolDeclaration(_)
            | hir::StmtKind::ImplBlock(_)
            | hir::StmtKind::Return(None) => {}
        }

        Ok(())
    }

    fn collect_expr_constraints(
        &self,
        expr: &hir::Expr,
        table: &mut UnificationTable,
    ) -> Result<(), LocalInferenceError> {
        match &expr.kind {
            hir::ExprKind::Block(block) | hir::ExprKind::Loop(block) => {
                self.collect_block_constraints(block, None, table)?;
            }
            hir::ExprKind::IfElse(condition, then_block, else_clauses) => {
                self.collect_expr_constraints(condition, table)?;
                self.collect_block_constraints(then_block, None, table)?;
                for clause in else_clauses {
                    if let Some(condition) = &clause.condition {
                        self.collect_expr_constraints(condition, table)?;
                    }
                    self.collect_block_constraints(&clause.consequence, None, table)?;
                }
            }
            hir::ExprKind::Match(scrutinee, arms) => {
                self.collect_expr_constraints(scrutinee, table)?;
                for arm in arms {
                    if let Some(guard) = &arm.guard {
                        self.collect_expr_constraints(guard, table)?;
                    }
                    self.collect_block_constraints(&arm.block, None, table)?;
                }
            }
            hir::ExprKind::Call(call) | hir::ExprKind::TailCall(call) => {
                self.collect_expr_constraints(&call.callee, table)?;
                for arg in &call.arguments {
                    self.collect_expr_constraints(arg, table)?;
                }
                if let TyKind::Fn(param_tys, _) = &call.callee.ty.kind {
                    for (arg, param_ty) in call.arguments.iter().zip(param_tys.iter()) {
                        self.constrain_expr_to_type(arg, &param_ty.kind, arg.span, table)?;
                    }
                }
            }
            hir::ExprKind::Binary(op, lhs, rhs) => {
                self.collect_expr_constraints(lhs, table)?;
                self.collect_expr_constraints(rhs, table)?;

                match op {
                    BinaryOpKind::Assign => {
                        if let Some(var) = self.local_var_from_expr(lhs) {
                            self.unify_local_with_expr_ty(var, &rhs.ty.kind, expr.span, table)?;
                            self.constrain_local_from_completions(var, rhs, expr.span, table)?;
                        }
                    }
                    // Arithmetic ops are handled via widening hints (see
                    // collect_block_widening_hints), not direct equality unification,
                    // to avoid spurious conflicts when mixed numeric types appear in
                    // the same expression (e.g. i64 + f64 → f64).
                    BinaryOpKind::Add
                    | BinaryOpKind::Sub
                    | BinaryOpKind::Mul
                    | BinaryOpKind::Div
                    | BinaryOpKind::Mod
                    | BinaryOpKind::Exp => {}
                    _ => {}
                }
            }
            hir::ExprKind::Unary(_, operand)
            | hir::ExprKind::Cast(operand, _)
            | hir::ExprKind::TryCast(operand, _)
            | hir::ExprKind::Break(Some(operand))
            | hir::ExprKind::FieldAccess(operand, _)
            | hir::ExprKind::Implements(operand, _)
            | hir::ExprKind::Let(_, operand) => self.collect_expr_constraints(operand, table)?,
            hir::ExprKind::IndexAccess(base, index) => {
                self.collect_expr_constraints(base, table)?;
                self.collect_expr_constraints(index, table)?;
            }
            hir::ExprKind::Range(range) => {
                self.collect_expr_constraints(&range.start, table)?;
                self.collect_expr_constraints(&range.end, table)?;
            }
            hir::ExprKind::List(elements) => {
                for element in elements {
                    self.collect_expr_constraints(element, table)?;
                }
            }
            hir::ExprKind::Dict(entries) => {
                for (key, value) in entries {
                    self.collect_expr_constraints(key, table)?;
                    self.collect_expr_constraints(value, table)?;
                }
            }
            hir::ExprKind::TaggedString { tag, exprs, .. } => {
                self.collect_expr_constraints(tag, table)?;
                for expr in exprs {
                    self.collect_expr_constraints(expr, table)?;
                }
            }
            hir::ExprKind::FunctionExpression(_)
            | hir::ExprKind::Break(None)
            | hir::ExprKind::Continue
            | hir::ExprKind::Literal(_)
            | hir::ExprKind::Path(_)
            | hir::ExprKind::Wildcard => {}
        }

        Ok(())
    }

    /// Collect widening hints for arithmetic binary operations in a block.
    ///
    /// For each seeded local var that appears as an operand in an arithmetic op
    /// (`+`, `-`, `*`, `/`, `%`, `**`), we record the coerced (widened) numeric
    /// result type based on the var's default type and the other operand's
    /// concrete type.  Widening is monotone: if the same var appears in multiple
    /// arithmetic ops, we keep the widest seen type.
    ///
    /// These hints are applied *after* equality constraints so that explicit
    /// annotations always take priority.
    fn collect_block_widening_hints(&self, block: &hir::Block) -> HashMap<TypeVarId, PrimTy> {
        let mut hints = HashMap::new();
        for stmt in &block.stmts {
            self.collect_stmt_widening_hints(stmt, &mut hints);
        }
        if let Some(expr) = &block.expr {
            self.collect_expr_widening_hints(expr, &mut hints);
        }
        hints
    }

    fn collect_stmt_widening_hints(
        &self,
        stmt: &hir::Stmt,
        hints: &mut HashMap<TypeVarId, PrimTy>,
    ) {
        match &stmt.kind {
            hir::StmtKind::Let(_, expr, _) | hir::StmtKind::Const(_, _, expr, _) => {
                self.collect_expr_widening_hints(expr, hints);
            }
            hir::StmtKind::Expr(expr) | hir::StmtKind::Return(Some(expr)) => {
                self.collect_expr_widening_hints(expr, hints);
            }
            hir::StmtKind::FunctionDeclaration(_)
            | hir::StmtKind::DynFunctionDeclaration(_)
            | hir::StmtKind::EnumDeclaration(_)
            | hir::StmtKind::StructDeclaration(_)
            | hir::StmtKind::ProtocolDeclaration(_)
            | hir::StmtKind::ImplBlock(_)
            | hir::StmtKind::Return(None) => {}
        }
    }

    #[allow(clippy::too_many_lines)]
    fn collect_expr_widening_hints(
        &self,
        expr: &hir::Expr,
        hints: &mut HashMap<TypeVarId, PrimTy>,
    ) {
        match &expr.kind {
            hir::ExprKind::Block(block) | hir::ExprKind::Loop(block) => {
                for stmt in &block.stmts {
                    self.collect_stmt_widening_hints(stmt, hints);
                }
                if let Some(inner) = &block.expr {
                    self.collect_expr_widening_hints(inner, hints);
                }
            }
            hir::ExprKind::IfElse(condition, then_block, else_clauses) => {
                self.collect_expr_widening_hints(condition, hints);
                for stmt in &then_block.stmts {
                    self.collect_stmt_widening_hints(stmt, hints);
                }
                if let Some(inner) = &then_block.expr {
                    self.collect_expr_widening_hints(inner, hints);
                }
                for clause in else_clauses {
                    if let Some(cond) = &clause.condition {
                        self.collect_expr_widening_hints(cond, hints);
                    }
                    for stmt in &clause.consequence.stmts {
                        self.collect_stmt_widening_hints(stmt, hints);
                    }
                    if let Some(inner) = &clause.consequence.expr {
                        self.collect_expr_widening_hints(inner, hints);
                    }
                }
            }
            hir::ExprKind::Match(scrutinee, arms) => {
                self.collect_expr_widening_hints(scrutinee, hints);
                for arm in arms {
                    if let Some(guard) = &arm.guard {
                        self.collect_expr_widening_hints(guard, hints);
                    }
                    for stmt in &arm.block.stmts {
                        self.collect_stmt_widening_hints(stmt, hints);
                    }
                    if let Some(inner) = &arm.block.expr {
                        self.collect_expr_widening_hints(inner, hints);
                    }
                }
            }
            hir::ExprKind::Call(call) | hir::ExprKind::TailCall(call) => {
                self.collect_expr_widening_hints(&call.callee, hints);
                for arg in &call.arguments {
                    self.collect_expr_widening_hints(arg, hints);
                }
            }
            hir::ExprKind::Binary(op, lhs, rhs) => {
                self.collect_expr_widening_hints(lhs, hints);
                self.collect_expr_widening_hints(rhs, hints);

                if matches!(
                    op,
                    BinaryOpKind::Add
                        | BinaryOpKind::Sub
                        | BinaryOpKind::Mul
                        | BinaryOpKind::Div
                        | BinaryOpKind::Mod
                        | BinaryOpKind::Exp
                ) {
                    self.accumulate_numeric_widening_from_pair(lhs, rhs, hints);
                }
            }
            hir::ExprKind::Unary(_, operand)
            | hir::ExprKind::Cast(operand, _)
            | hir::ExprKind::TryCast(operand, _)
            | hir::ExprKind::Break(Some(operand))
            | hir::ExprKind::FieldAccess(operand, _)
            | hir::ExprKind::Implements(operand, _)
            | hir::ExprKind::Let(_, operand) => self.collect_expr_widening_hints(operand, hints),
            hir::ExprKind::IndexAccess(base, index) => {
                self.collect_expr_widening_hints(base, hints);
                self.collect_expr_widening_hints(index, hints);
            }
            hir::ExprKind::Range(range) => {
                self.collect_expr_widening_hints(&range.start, hints);
                self.collect_expr_widening_hints(&range.end, hints);
            }
            hir::ExprKind::List(elements) => {
                for element in elements {
                    self.collect_expr_widening_hints(element, hints);
                }
            }
            hir::ExprKind::Dict(entries) => {
                for (key, value) in entries {
                    self.collect_expr_widening_hints(key, hints);
                    self.collect_expr_widening_hints(value, hints);
                }
            }
            hir::ExprKind::TaggedString { tag, exprs, .. } => {
                self.collect_expr_widening_hints(tag, hints);
                for expr in exprs {
                    self.collect_expr_widening_hints(expr, hints);
                }
            }
            hir::ExprKind::FunctionExpression(_)
            | hir::ExprKind::Break(None)
            | hir::ExprKind::Continue
            | hir::ExprKind::Literal(_)
            | hir::ExprKind::Path(_)
            | hir::ExprKind::Wildcard => {}
        }
    }

    /// For an arithmetic pair `lhs op rhs`, if one side is a seeded local var and
    /// the other is a concrete numeric primitive, update the widening hints map to
    /// reflect the coerced (widened) result type.
    fn accumulate_numeric_widening_from_pair(
        &self,
        lhs: &hir::Expr,
        rhs: &hir::Expr,
        hints: &mut HashMap<TypeVarId, PrimTy>,
    ) {
        let lhs_var = self.local_var_from_expr(lhs);
        let rhs_var = self.local_var_from_expr(rhs);

        if let Some(lhs_var) = lhs_var {
            self.accumulate_widening_hint(lhs_var, &rhs.ty.kind, hints);
        }
        if let Some(rhs_var) = rhs_var {
            self.accumulate_widening_hint(rhs_var, &lhs.ty.kind, hints);
        }

        // When both operands are seeded local vars, link them so they share the
        // same widening class: whichever gets a concrete hint from another use
        // will propagate the widening to the other.
        if let (Some(lhs_var), Some(rhs_var)) = (lhs_var, rhs_var) {
            let lhs_hint = hints.get(&lhs_var).copied();
            let rhs_hint = hints.get(&rhs_var).copied();
            match (lhs_hint, rhs_hint) {
                (Some(lp), Some(rp)) => {
                    if let Some(widened) = coerced_numeric_result(lp, rp) {
                        hints.insert(lhs_var, widened);
                        hints.insert(rhs_var, widened);
                    }
                }
                (Some(lp), None) => {
                    hints.insert(rhs_var, lp);
                }
                (None, Some(rp)) => {
                    hints.insert(lhs_var, rp);
                }
                (None, None) => {}
            }
        }
    }

    /// Compute the coerced numeric type for `local_var op other_ty` using the
    /// var's seed default type and update the widening hints map monotonically
    /// (only ever widening, never narrowing).
    fn accumulate_widening_hint(
        &self,
        local_var: TypeVarId,
        other_ty: &TyKind,
        hints: &mut HashMap<TypeVarId, PrimTy>,
    ) {
        let TyKind::Primitive(other_prim) = other_ty else {
            return;
        };
        if !other_prim.is_numeric() {
            return;
        }

        // Use the other operand's concrete type as the hint, widening
        // monotonically across multiple arithmetic uses of the same var.
        // Widening via coerced_numeric_result ensures that if the same var
        // is used with both i64 and f64, the hint becomes f64 (no conflict).
        let entry = hints.entry(local_var).or_insert(*other_prim);
        if let Some(widened) = coerced_numeric_result(*entry, *other_prim) {
            *entry = widened;
        }
    }

    fn constrain_expr_to_type(
        &self,
        expr: &hir::Expr,
        expected: &TyKind,
        span: Span,
        table: &mut UnificationTable,
    ) -> Result<(), LocalInferenceError> {
        if let Some(var) = self.local_var_from_expr(expr) {
            self.unify_local_with_ty(var, expected, span, table)?;
        }

        Ok(())
    }

    fn constrain_local_from_completions(
        &self,
        local_var: TypeVarId,
        expr: &hir::Expr,
        span: Span,
        table: &mut UnificationTable,
    ) -> Result<(), LocalInferenceError> {
        match &expr.kind {
            hir::ExprKind::Block(block) | hir::ExprKind::Loop(block) => {
                if let Some(inner) = &block.expr {
                    self.constrain_local_from_completions(local_var, inner, span, table)?;
                }
            }
            hir::ExprKind::IfElse(_, then_block, else_clauses) => {
                if let Some(expr) = &then_block.expr {
                    self.constrain_local_from_completions(local_var, expr, span, table)?;
                }
                for clause in else_clauses {
                    if let Some(expr) = &clause.consequence.expr {
                        self.constrain_local_from_completions(local_var, expr, span, table)?;
                    }
                }
            }
            hir::ExprKind::Match(_, arms) => {
                for arm in arms {
                    if let Some(expr) = &arm.block.expr {
                        self.constrain_local_from_completions(local_var, expr, span, table)?;
                    }
                }
            }
            hir::ExprKind::Break(Some(value)) => {
                self.unify_local_with_expr_ty(local_var, &value.ty.kind, span, table)?;
            }
            _ => {
                self.unify_local_with_expr_ty(local_var, &expr.ty.kind, span, table)?;
            }
        }

        Ok(())
    }

    fn unify_local_with_expr_ty(
        &self,
        local_var: TypeVarId,
        ty: &TyKind,
        span: Span,
        table: &mut UnificationTable,
    ) -> Result<(), LocalInferenceError> {
        if let Some(other_var) = self.local_var_from_ty(ty) {
            return self.unify_local_vars(local_var, other_var, span, table);
        }
        self.unify_local_with_ty(local_var, ty, span, table)
    }

    fn unify_local_vars(
        &self,
        left: TypeVarId,
        right: TypeVarId,
        span: Span,
        table: &mut UnificationTable,
    ) -> Result<(), LocalInferenceError> {
        if left == right {
            return Ok(());
        }
        table
            .unify_var_var(left, right)
            .map_err(|err| self.map_unification_error(err, span))
    }

    fn unify_local_with_ty(
        &self,
        local_var: TypeVarId,
        ty: &TyKind,
        span: Span,
        table: &mut UnificationTable,
    ) -> Result<(), LocalInferenceError> {
        if !is_constraining_ty(ty) {
            return Ok(());
        }
        table
            .unify_var_ty(local_var, ty)
            .map_err(|err| self.map_unification_error(err, span))
    }

    fn local_var_from_expr(&self, expr: &hir::Expr) -> Option<TypeVarId> {
        self.local_var_from_ty(&expr.ty.kind)
    }

    fn local_var_from_ty(&self, ty: &TyKind) -> Option<TypeVarId> {
        match ty {
            TyKind::Var(type_var_id) if self.binding_by_var.contains_key(type_var_id) => {
                Some(*type_var_id)
            }
            _ => None,
        }
    }

    fn map_unification_error(&self, err: UnificationError, span: Span) -> LocalInferenceError {
        match err {
            UnificationError::Conflict { left, right } => LocalInferenceError::Conflict {
                expected: left,
                actual: right,
                span,
            },
            UnificationError::OccursCheck(occurs) => {
                let span = self
                    .binding_by_var
                    .get(&occurs.var)
                    .and_then(|binding| self.seeds_by_binding.get(binding))
                    .map(|seed| seed.span)
                    .unwrap_or(span);
                LocalInferenceError::InfiniteType {
                    type_var: occurs.var,
                    ty: Box::new(occurs.ty),
                    span,
                }
            }
        }
    }
}

fn refineable_literal_default_ty(expr: &hir::Expr) -> Option<TyKind> {
    let hir::ExprKind::Literal(lit) = &expr.kind else {
        return None;
    };
    match lit.as_ref() {
        Literal::Integer(_) | Literal::UnsignedInteger(_) => Some(TyKind::Primitive(PrimTy::I64)),
        Literal::Float(_) => Some(TyKind::Primitive(PrimTy::F64)),
        _ => None,
    }
}

fn is_constraining_ty(ty: &TyKind) -> bool {
    !matches!(ty, TyKind::Unknown | TyKind::Var(_))
}
