use tlang_ast::node::UnaryOp;
use tlang_ast::token::Literal;
use tlang_hir::Visitor;
use tlang_hir::visit::{walk_block, walk_expr, walk_module, walk_stmt};
use tlang_hir::{self as hir, BinaryOpKind, PrimTy, Ty, TyKind};
use tlang_hir_opt::hir_opt::{HirOptContext, HirOptError, HirPass};

use crate::typing_context::TypingContext;
use crate::{TypeError, TypeInfo, TypeTable};

/// The main type-checking pass that runs on the HIR after lowering and
/// optimization.
///
/// It walks the HIR tree, assigns types to expressions and patterns,
/// and emits diagnostics for type mismatches.
#[derive(Debug, Default)]
pub struct TypeChecker {
    /// Accumulated type errors from the current pass.
    pub errors: Vec<TypeError>,
    /// Side-table mapping HIR nodes to their resolved types.
    pub type_table: TypeTable,
    /// Stack of typing contexts (strict/permissive) for nested functions.
    context_stack: Vec<TypingContext>,
}

impl TypeChecker {
    pub fn new() -> Self {
        Self::default()
    }

    fn current_context(&self) -> TypingContext {
        self.context_stack
            .last()
            .copied()
            .unwrap_or(TypingContext::Permissive)
    }

    fn push_context(&mut self, ctx: TypingContext) {
        self.context_stack.push(ctx);
    }

    fn pop_context(&mut self) {
        self.context_stack.pop();
    }

    // ── Literal typing ───────────────────────────────────────────────

    fn type_of_literal(lit: &Literal) -> TyKind {
        match lit {
            // Both integer variants default to i64: the lexer produces
            // UnsignedInteger for numeric literals (e.g. `1`), and the parser
            // introduces Integer for negated numeric literals (e.g. `-1`).
            // In the type system, all bare integer literals are `i64` by default.
            Literal::Integer(_) | Literal::UnsignedInteger(_) => TyKind::Primitive(PrimTy::I64),
            Literal::Float(_) => TyKind::Primitive(PrimTy::F64),
            Literal::Boolean(_) => TyKind::Primitive(PrimTy::Bool),
            Literal::String(_) => TyKind::Primitive(PrimTy::String),
            Literal::Char(_) => TyKind::Primitive(PrimTy::Char),
            Literal::None => TyKind::Primitive(PrimTy::Nil),
        }
    }

    // ── Binary operator typing ───────────────────────────────────────

    fn check_binary_op(
        &mut self,
        op: BinaryOpKind,
        lhs_ty: &TyKind,
        rhs_ty: &TyKind,
        span: tlang_span::Span,
    ) -> TyKind {
        let ctx = self.current_context();

        // Handle `unknown` operands.
        let lhs_unknown = matches!(lhs_ty, TyKind::Unknown);
        let rhs_unknown = matches!(rhs_ty, TyKind::Unknown);

        if lhs_unknown || rhs_unknown {
            if ctx.is_strict() {
                self.errors.push(TypeError::UnknownInStrictContext {
                    op: op.to_string(),
                    span,
                });
                return TyKind::Unknown;
            }
            // Permissive: propagate unknown.
            return TyKind::Unknown;
        }

        match op {
            // ── Arithmetic ───────────────────────────────────────
            BinaryOpKind::Add => self.check_add(lhs_ty, rhs_ty, span),
            BinaryOpKind::Sub
            | BinaryOpKind::Mul
            | BinaryOpKind::Div
            | BinaryOpKind::Mod
            | BinaryOpKind::Exp => self.check_arithmetic(op, lhs_ty, rhs_ty, span),

            // ── Comparison ───────────────────────────────────────
            BinaryOpKind::Eq
            | BinaryOpKind::NotEq
            | BinaryOpKind::Less
            | BinaryOpKind::LessEq
            | BinaryOpKind::Greater
            | BinaryOpKind::GreaterEq => self.check_comparison(op, lhs_ty, rhs_ty, span),

            // ── Logical ──────────────────────────────────────────
            BinaryOpKind::And | BinaryOpKind::Or => self.check_logical(op, lhs_ty, rhs_ty, span),

            // ── Bitwise ──────────────────────────────────────────
            BinaryOpKind::BitwiseAnd
            | BinaryOpKind::BitwiseOr
            | BinaryOpKind::BitwiseXor
            | BinaryOpKind::LeftShift
            | BinaryOpKind::RightShift => self.check_bitwise(op, lhs_ty, rhs_ty, span),

            // Assignment expressions evaluate to the assigned value.
            // Only preserve a concrete type when the assignment is type-compatible.
            BinaryOpKind::Assign => {
                if lhs_ty == rhs_ty {
                    rhs_ty.clone()
                } else {
                    TyKind::Unknown
                }
            }
        }
    }

    /// `+`: numeric addition **or** string concatenation.
    fn check_add(&mut self, lhs_ty: &TyKind, rhs_ty: &TyKind, span: tlang_span::Span) -> TyKind {
        // String concatenation.
        if matches!(lhs_ty, TyKind::Primitive(PrimTy::String))
            && matches!(rhs_ty, TyKind::Primitive(PrimTy::String))
        {
            return TyKind::Primitive(PrimTy::String);
        }
        self.check_arithmetic(BinaryOpKind::Add, lhs_ty, rhs_ty, span)
    }

    fn check_arithmetic(
        &mut self,
        op: BinaryOpKind,
        lhs_ty: &TyKind,
        rhs_ty: &TyKind,
        span: tlang_span::Span,
    ) -> TyKind {
        match (lhs_ty, rhs_ty) {
            (TyKind::Primitive(l), TyKind::Primitive(r))
                if l.is_numeric() && r.is_numeric() && l == r =>
            {
                TyKind::Primitive(*l)
            }
            _ => {
                self.errors.push(TypeError::InvalidBinaryOp {
                    op: op.to_string(),
                    lhs: lhs_ty.to_string(),
                    rhs: rhs_ty.to_string(),
                    span,
                });
                TyKind::Unknown
            }
        }
    }

    fn check_comparison(
        &mut self,
        op: BinaryOpKind,
        lhs_ty: &TyKind,
        rhs_ty: &TyKind,
        span: tlang_span::Span,
    ) -> TyKind {
        if lhs_ty == rhs_ty {
            TyKind::Primitive(PrimTy::Bool)
        } else {
            self.errors.push(TypeError::InvalidBinaryOp {
                op: op.to_string(),
                lhs: lhs_ty.to_string(),
                rhs: rhs_ty.to_string(),
                span,
            });
            TyKind::Unknown
        }
    }

    fn check_logical(
        &mut self,
        op: BinaryOpKind,
        lhs_ty: &TyKind,
        rhs_ty: &TyKind,
        span: tlang_span::Span,
    ) -> TyKind {
        let is_bool = |t: &TyKind| matches!(t, TyKind::Primitive(PrimTy::Bool));
        if is_bool(lhs_ty) && is_bool(rhs_ty) {
            TyKind::Primitive(PrimTy::Bool)
        } else {
            self.errors.push(TypeError::InvalidBinaryOp {
                op: op.to_string(),
                lhs: lhs_ty.to_string(),
                rhs: rhs_ty.to_string(),
                span,
            });
            TyKind::Unknown
        }
    }

    fn check_bitwise(
        &mut self,
        op: BinaryOpKind,
        lhs_ty: &TyKind,
        rhs_ty: &TyKind,
        span: tlang_span::Span,
    ) -> TyKind {
        match (lhs_ty, rhs_ty) {
            (TyKind::Primitive(l), TyKind::Primitive(r))
                if l.is_integer() && r.is_integer() && l == r =>
            {
                TyKind::Primitive(*l)
            }
            _ => {
                self.errors.push(TypeError::InvalidBinaryOp {
                    op: op.to_string(),
                    lhs: lhs_ty.to_string(),
                    rhs: rhs_ty.to_string(),
                    span,
                });
                TyKind::Unknown
            }
        }
    }

    // ── Unary operator typing ────────────────────────────────────────

    fn check_unary_op(
        &mut self,
        op: UnaryOp,
        operand_ty: &TyKind,
        span: tlang_span::Span,
    ) -> TyKind {
        let ctx = self.current_context();

        if matches!(operand_ty, TyKind::Unknown) {
            if ctx.is_strict() {
                self.errors.push(TypeError::UnknownInStrictContext {
                    op: unary_op_str(op).to_string(),
                    span,
                });
            }
            return TyKind::Unknown;
        }

        match op {
            UnaryOp::Minus => {
                if let TyKind::Primitive(p) = operand_ty
                    && p.is_numeric()
                {
                    return operand_ty.clone();
                }
                self.errors.push(TypeError::InvalidUnaryOp {
                    op: unary_op_str(op).to_string(),
                    operand: operand_ty.to_string(),
                    span,
                });
                TyKind::Unknown
            }
            UnaryOp::Not => {
                if matches!(operand_ty, TyKind::Primitive(PrimTy::Bool)) {
                    TyKind::Primitive(PrimTy::Bool)
                } else {
                    self.errors.push(TypeError::InvalidUnaryOp {
                        op: unary_op_str(op).to_string(),
                        operand: operand_ty.to_string(),
                        span,
                    });
                    TyKind::Unknown
                }
            }
            UnaryOp::BitwiseNot => {
                if let TyKind::Primitive(p) = operand_ty
                    && p.is_integer()
                {
                    return operand_ty.clone();
                }
                self.errors.push(TypeError::InvalidUnaryOp {
                    op: unary_op_str(op).to_string(),
                    operand: operand_ty.to_string(),
                    span,
                });
                TyKind::Unknown
            }
            // Rest/Spread are structural, not type-checked here.
            UnaryOp::Rest | UnaryOp::Spread => operand_ty.clone(),
        }
    }

    // ── Binding checking ─────────────────────────────────────────────

    fn check_binding_type(&mut self, declared_ty: &Ty, expr_ty: &TyKind, span: tlang_span::Span) {
        // No annotation → skip check.
        if matches!(declared_ty.kind, TyKind::Unknown) {
            return;
        }

        // Annotation boundary: `unknown` cannot flow into an annotated binding.
        if matches!(expr_ty, TyKind::Unknown) {
            self.errors.push(TypeError::BindingTypeMismatch {
                declared: declared_ty.kind.to_string(),
                actual: "unknown".to_string(),
                span,
            });
            return;
        }

        if declared_ty.kind != *expr_ty {
            self.errors.push(TypeError::BindingTypeMismatch {
                declared: declared_ty.kind.to_string(),
                actual: expr_ty.to_string(),
                span,
            });
        }
    }

    // ── Pattern binding registration ─────────────────────────────────

    /// Walk a pattern and register all bound identifiers in the type table
    /// with the given binding type.
    fn register_pat_bindings(&mut self, pat: &mut hir::Pat, binding_ty: &TyKind) {
        match &mut pat.kind {
            hir::PatKind::Identifier(hir_id, _) => {
                self.type_table.insert(
                    *hir_id,
                    TypeInfo {
                        ty: Ty {
                            kind: binding_ty.clone(),
                            ..Ty::default()
                        },
                    },
                );
            }
            hir::PatKind::List(pats) => {
                for p in pats {
                    self.register_pat_bindings(p, binding_ty);
                }
            }
            hir::PatKind::Rest(inner) => {
                self.register_pat_bindings(inner, binding_ty);
            }
            hir::PatKind::Enum(_, fields) => {
                for (_, p) in fields {
                    self.register_pat_bindings(p, binding_ty);
                }
            }
            hir::PatKind::Wildcard | hir::PatKind::Literal(_) => {}
        }
    }

    // ── HIR walk entry point ─────────────────────────────────────────

    fn check_module(&mut self, module: &mut hir::Module) {
        // Top-level is always permissive.
        self.push_context(TypingContext::Permissive);
        self.visit_module(module, &mut ());
        self.pop_context();
    }
}

fn unary_op_str(op: UnaryOp) -> &'static str {
    match op {
        UnaryOp::Minus => "-",
        UnaryOp::Not => "!",
        UnaryOp::BitwiseNot => "~",
        UnaryOp::Rest => "...",
        UnaryOp::Spread => "...",
    }
}

impl HirPass for TypeChecker {
    fn name(&self) -> &'static str {
        "TypeChecker"
    }

    fn optimize_hir(
        &mut self,
        module: &mut hir::Module,
        ctx: &mut HirOptContext,
    ) -> Result<bool, HirOptError> {
        self.check_module(module);

        // Convert any accumulated type errors into diagnostics.
        for error in self.errors.drain(..) {
            ctx.diagnostics.push((&error).into());
        }

        // The type checker does not transform the HIR (no changes reported).
        Ok(false)
    }
}

// ── Visitor implementation ──────────────────────────────────────────────

impl<'hir> Visitor<'hir> for TypeChecker {
    fn visit_module(&mut self, module: &'hir mut hir::Module, ctx: &mut Self::Context) {
        walk_module(self, module, ctx);
    }

    fn visit_block(&mut self, block: &'hir mut hir::Block, ctx: &mut Self::Context) {
        walk_block(self, block, ctx);
    }

    fn visit_stmt(&mut self, stmt: &'hir mut hir::Stmt, ctx: &mut Self::Context) {
        match &mut stmt.kind {
            hir::StmtKind::Let(pat, expr, ty) => {
                self.visit_expr(expr, ctx);

                let expr_ty_kind = expr.ty.kind.clone();

                // If there is an explicit annotation, check compatibility.
                self.check_binding_type(ty, &expr_ty_kind, stmt.span);

                // The binding's type is the annotation if present, else inferred.
                let binding_ty = if matches!(ty.kind, TyKind::Unknown) {
                    expr_ty_kind
                } else {
                    ty.kind.clone()
                };

                self.register_pat_bindings(pat, &binding_ty);
                pat.ty.kind = binding_ty;
            }
            hir::StmtKind::Const(_, pat, expr, ty) => {
                self.visit_expr(expr, ctx);

                let expr_ty_kind = expr.ty.kind.clone();

                self.check_binding_type(ty, &expr_ty_kind, stmt.span);

                let binding_ty = if matches!(ty.kind, TyKind::Unknown) {
                    expr_ty_kind
                } else {
                    ty.kind.clone()
                };

                self.register_pat_bindings(pat, &binding_ty);
                pat.ty.kind = binding_ty;
            }
            hir::StmtKind::FunctionDeclaration(decl) => {
                let fn_ctx = TypingContext::for_function(decl);
                self.push_context(fn_ctx);

                // Store parameter types in the type table.
                for param in &decl.parameters {
                    self.type_table.insert(
                        param.hir_id,
                        TypeInfo {
                            ty: param.type_annotation.clone(),
                        },
                    );
                }

                self.visit_block(&mut decl.body, ctx);
                self.pop_context();
            }
            _ => walk_stmt(self, stmt, ctx),
        }
    }

    fn visit_expr(&mut self, expr: &'hir mut hir::Expr, ctx: &mut Self::Context) {
        match &mut expr.kind {
            hir::ExprKind::Literal(lit) => {
                let ty_kind = Self::type_of_literal(lit);
                expr.ty.kind = ty_kind.clone();
                self.type_table.insert(
                    expr.hir_id,
                    TypeInfo {
                        ty: Ty {
                            kind: ty_kind,
                            ..Ty::default()
                        },
                    },
                );
            }
            hir::ExprKind::Binary(op, lhs, rhs) => {
                self.visit_expr(lhs, ctx);
                self.visit_expr(rhs, ctx);

                let op = *op;
                let result_ty = self.check_binary_op(op, &lhs.ty.kind, &rhs.ty.kind, expr.span);
                expr.ty.kind = result_ty.clone();
                self.type_table.insert(
                    expr.hir_id,
                    TypeInfo {
                        ty: Ty {
                            kind: result_ty,
                            ..Ty::default()
                        },
                    },
                );
            }
            hir::ExprKind::Unary(op, operand) => {
                self.visit_expr(operand, ctx);
                let op = *op;
                let result_ty = self.check_unary_op(op, &operand.ty.kind, expr.span);
                expr.ty.kind = result_ty.clone();
                self.type_table.insert(
                    expr.hir_id,
                    TypeInfo {
                        ty: Ty {
                            kind: result_ty,
                            ..Ty::default()
                        },
                    },
                );
            }
            hir::ExprKind::FunctionExpression(decl) => {
                let enclosing = self.current_context();
                let closure_ctx = TypingContext::for_closure(decl, enclosing);
                self.push_context(closure_ctx);

                for param in &decl.parameters {
                    self.type_table.insert(
                        param.hir_id,
                        TypeInfo {
                            ty: param.type_annotation.clone(),
                        },
                    );
                }

                self.visit_block(&mut decl.body, ctx);
                self.pop_context();
            }
            hir::ExprKind::Path(path) => {
                // Try to resolve the type from the type table via the path's
                // resolution HirId.
                if let Some(hir_id) = path.res.hir_id()
                    && let Some(info) = self.type_table.get(&hir_id)
                {
                    expr.ty = info.ty.clone();
                }
            }
            hir::ExprKind::Block(block) => {
                self.visit_block(block, ctx);
                // Block type is the type of its trailing expression if any.
                if let Some(trailing) = &block.expr {
                    expr.ty = trailing.ty.clone();
                }
            }
            _ => {
                walk_expr(self, expr, ctx);
            }
        }
    }
}
