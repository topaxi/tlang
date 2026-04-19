use std::collections::HashMap;

use tlang_ast::node::UnaryOp;
use tlang_ast::token::Literal;
use tlang_hir::Visitor;
use tlang_hir::visit::{walk_block, walk_expr, walk_module, walk_stmt};
use tlang_hir::{self as hir, BinaryOpKind, PrimTy, Ty, TyKind};
use tlang_hir_opt::hir_opt::{HirOptContext, HirOptError, HirPass};
use tlang_span::TypeVarId;

use crate::builtin_methods;
use crate::builtin_protocols;
use crate::builtin_types;
use crate::builtins;
use crate::type_table::{
    AssociatedTypeInfo, EnumInfo, ImplInfo, ProtocolInfo, ProtocolMethodInfo, StructInfo,
    VariantInfo,
};
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
    /// Stack of enclosing function return types for return-statement checking.
    return_type_stack: Vec<TyKind>,
    /// Stack of observed return-expression types collected during function
    /// body traversal.  Used to infer the return type when no annotation
    /// and no trailing expression are present (e.g. `fn f() { return 42; }`).
    observed_return_types: Vec<Vec<TyKind>>,
    /// Dot-method declarations keyed by their qualified `Type.method` name.
    /// Pre-scanned once at the start of `check_module` so both field-access
    /// typing and `apply` conflict detection can resolve user-defined methods.
    dot_methods: HashMap<String, tlang_span::HirId>,
    /// Synthetic for-loop iterator bindings keyed by the lowered iterator local.
    iterator_item_types: HashMap<tlang_span::HirId, TyKind>,
    /// The item type of the most recently visited for-loop iterator binding.
    ///
    /// Set by `record_iterator_binding_item_type` when processing a
    /// `let iterator$$` statement.  Consumed once by the immediately following
    /// `let accumulator$$` statement to enable contextual numeric literal
    /// inference for unannotated loop accumulators (e.g. `with sum = 0`
    /// infers `sum: isize` when the iterator yields `isize` items).
    pending_accumulator_type: Option<TyKind>,
}

impl TypeChecker {
    const MIN_PROTOCOL_PATH_SEGMENTS: usize = 2;

    pub fn new() -> Self {
        Self::default()
    }

    fn current_context(&self) -> TypingContext {
        self.context_stack
            .last()
            .copied()
            .unwrap_or(TypingContext::Permissive)
    }

    fn iterable_item_type(&self, ty: &TyKind) -> Option<TyKind> {
        match ty {
            TyKind::List(inner) | TyKind::Slice(inner) => Some(inner.kind.clone()),
            TyKind::Primitive(PrimTy::String) => Some(TyKind::Primitive(PrimTy::String)),
            TyKind::Path(path, _) => self
                .type_table
                .impls()
                .iter()
                .find(|info| {
                    info.protocol_name == "Iterable"
                        && info.target_type_name == path.join("::")
                        && info.protocol_type_arg_tys.len() == 1
                })
                .map(|info| info.protocol_type_arg_tys[0].kind.clone()),
            _ => None,
        }
    }

    fn record_iterator_binding_item_type(&mut self, pat: &hir::Pat, expr: &hir::Expr) {
        let hir::PatKind::Identifier(hir_id, ident) = &pat.kind else {
            return;
        };
        if ident.as_str() != "iterator$$" {
            return;
        }
        let hir::ExprKind::Call(call) = &expr.kind else {
            return;
        };
        let hir::ExprKind::Path(path) = &call.callee.kind else {
            return;
        };
        if path.join("::") != "Iterable::iter" || call.arguments.len() != 1 {
            return;
        }
        if let Some(item_ty) = self.iterable_item_type(&call.arguments[0].ty.kind) {
            self.iterator_item_types.insert(*hir_id, item_ty.clone());
            // Store as pending so the next `let accumulator$$` binding can
            // adopt this type when the accumulator has no outer annotation.
            self.pending_accumulator_type = Some(item_ty);
        }
    }

    fn iterator_next_item_type(&self, expr: &hir::Expr) -> Option<TyKind> {
        let (hir::ExprKind::Call(call) | hir::ExprKind::TailCall(call)) = &expr.kind else {
            return None;
        };
        let hir::ExprKind::Path(path) = &call.callee.kind else {
            return None;
        };
        if path.join("::") != "Iterator::next" || call.arguments.len() != 1 {
            return None;
        }
        let hir::ExprKind::Path(iterator_path) = &call.arguments[0].kind else {
            return None;
        };
        let hir_id = iterator_path.res.hir_id()?;
        self.iterator_item_types.get(&hir_id).cloned()
    }

    /// Seed the implicit receiver parameter of a dot-method declaration from
    /// the owning type when lowering left it unknown.
    ///
    /// This only applies to `fn Type.method(...)` declarations whose method
    /// name lowers to `ExprKind::FieldAccess`, have at least one parameter,
    /// and whose first parameter still has an unknown type annotation.
    fn seed_dot_method_receiver_type(&self, decl: &mut hir::FunctionDeclaration) {
        let hir::ExprKind::FieldAccess(base, _) = &decl.name.kind else {
            return;
        };
        let Some(receiver_path) = base.path() else {
            return;
        };
        let Some(receiver_param) = decl.parameters.first_mut() else {
            return;
        };
        if !matches!(receiver_param.type_annotation.kind, TyKind::Unknown) {
            return;
        }

        receiver_param.type_annotation = Ty {
            kind: TyKind::Path(
                receiver_path.clone(),
                decl.owner_type_params
                    .iter()
                    .map(|type_param| Ty {
                        kind: TyKind::Var(type_param.type_var_id),
                        ..Ty::default()
                    })
                    .collect(),
            ),
            ..Ty::default()
        };
    }

    /// Resolve the result type of `base[index]`.
    ///
    /// Only slice indexing currently has aligned type-checker and runtime
    /// semantics, so other receivers remain unknown until additional indexing
    /// behavior is defined and implemented.
    fn index_access_result_type(&self, base_ty: &TyKind) -> TyKind {
        match base_ty {
            TyKind::List(inner) | TyKind::Slice(inner) => inner.kind.clone(),
            _ => TyKind::Unknown,
        }
    }

    fn register_iterator_next_pattern_bindings(&mut self, pat: &mut hir::Pat, item_ty: &TyKind) {
        let hir::PatKind::Enum(path, fields) = &mut pat.kind else {
            return;
        };
        if path.join("::") != "Option::Some" {
            return;
        }
        if let Some((_, field_pat)) = fields.iter_mut().find(|(name, _)| name.as_str() == "0") {
            field_pat.ty.kind = item_ty.clone();
            self.register_pat_bindings(field_pat, item_ty);
        }
    }

    fn push_context(&mut self, ctx: TypingContext) {
        self.context_stack.push(ctx);
    }

    fn pop_context(&mut self) {
        self.context_stack.pop();
    }

    // ── Builtin collection types ─────────────────────────────────────

    /// Create the canonical `TyKind` for a builtin collection type name
    /// (`"List"` or `"Dict"`).  Delegates to `builtin_types::lookup`.
    fn builtin_type_path(name: &str) -> TyKind {
        builtin_types::lookup(name)
            .unwrap_or_else(|| panic!("unknown builtin collection type: {name}"))
    }

    /// Infer `List(elem_ty)` from a list literal's elements.
    ///
    /// When all elements agree on a single known type, the list is typed as
    /// `List(that_type)`.  Otherwise (empty list, mixed types, or unknown
    /// elements) falls back to the bare `List` path.
    fn infer_list_element_type(&self, expr: &hir::Expr) -> TyKind {
        if let hir::ExprKind::Unary(UnaryOp::Spread, inner) = &expr.kind {
            return self
                .iterable_item_type(&inner.ty.kind)
                .unwrap_or(TyKind::Unknown);
        }

        expr.ty.kind.clone()
    }

    fn infer_list_type(&self, elements: &[hir::Expr]) -> TyKind {
        let element_tys: Vec<TyKind> = elements
            .iter()
            .map(|expr| self.infer_list_element_type(expr))
            .collect();
        let common = Self::common_element_type(element_tys.iter());
        match common {
            Some(elem_kind) => TyKind::List(Box::new(Ty {
                kind: elem_kind,
                ..Ty::default()
            })),
            None => Self::builtin_type_path("List"),
        }
    }

    /// Infer `Dict(key_ty, val_ty)` from a dict literal's entries.
    fn infer_dict_type(entries: &[(hir::Expr, hir::Expr)]) -> TyKind {
        let key_common = Self::common_element_type(entries.iter().map(|(k, _)| &k.ty.kind));
        let val_common = Self::common_element_type(entries.iter().map(|(_, v)| &v.ty.kind));
        match (key_common, val_common) {
            (Some(k), Some(v)) => TyKind::Dict(
                Box::new(Ty {
                    kind: k,
                    ..Ty::default()
                }),
                Box::new(Ty {
                    kind: v,
                    ..Ty::default()
                }),
            ),
            _ => Self::builtin_type_path("Dict"),
        }
    }

    /// Find the common type across an iterator of `TyKind`s.
    ///
    /// Returns `Some(ty)` when all non-unknown elements agree on a single
    /// concrete type, `None` otherwise (empty, mixed, or all unknown).
    fn common_element_type<'a>(mut types: impl Iterator<Item = &'a TyKind>) -> Option<TyKind> {
        let first = types.find(|t| !matches!(t, TyKind::Unknown))?;
        if types.all(|t| matches!(t, TyKind::Unknown) || ty_kinds_compatible(t, first)) {
            Some(first.clone())
        } else {
            None
        }
    }

    /// Infer a branching expression type only when every completion resolves
    /// to the same concrete type. Any unknown, type-variable, or conflicting
    /// branch leaves the whole expression unresolved.
    fn common_completion_type<'a>(types: impl Iterator<Item = &'a TyKind>) -> Option<TyKind> {
        let mut inferred: Option<&TyKind> = None;

        for ty in types {
            if matches!(ty, TyKind::Unknown) || ty_contains_var(ty) {
                return None;
            }

            match inferred {
                None => inferred = Some(ty),
                Some(prev) if ty_kinds_compatible(prev, ty) => {}
                Some(_) => return None,
            }
        }

        inferred.cloned()
    }

    /// Assign `ty_kind` to `expr.ty` and record it in the type table.
    fn assign_expr_type(&mut self, expr: &mut hir::Expr, ty_kind: TyKind) {
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
        let lhs_unknown = matches!(lhs_ty, TyKind::Unknown | TyKind::Var(_));
        let rhs_unknown = matches!(rhs_ty, TyKind::Unknown | TyKind::Var(_));

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
            // When the LHS is nil or unknown (e.g. ANF temporaries initialised
            // as `let $anf = nil;`), adopt the RHS type so information flows
            // from match arms back to the consuming binding.
            BinaryOpKind::Assign => {
                if matches!(lhs_ty, TyKind::Unknown | TyKind::Primitive(PrimTy::Nil))
                    || ty_kinds_compatible(lhs_ty, rhs_ty)
                {
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

        Some(PrimTy::F64)
    }

    fn check_arithmetic(
        &mut self,
        op: BinaryOpKind,
        lhs_ty: &TyKind,
        rhs_ty: &TyKind,
        span: tlang_span::Span,
    ) -> TyKind {
        if matches!(self.current_context(), TypingContext::Permissive)
            && (matches!(lhs_ty, TyKind::Var(_)) || matches!(rhs_ty, TyKind::Var(_)))
        {
            return match (lhs_ty, rhs_ty) {
                (TyKind::Primitive(l), TyKind::Primitive(r)) => {
                    Self::coerced_numeric_result(*l, *r)
                        .map(TyKind::Primitive)
                        .unwrap_or(TyKind::Unknown)
                }
                (TyKind::Var(_), TyKind::Primitive(r)) | (TyKind::Primitive(r), TyKind::Var(_))
                    if r.is_numeric() =>
                {
                    TyKind::Primitive(*r)
                }
                _ => TyKind::Unknown,
            };
        }

        match (lhs_ty, rhs_ty) {
            (TyKind::Primitive(l), TyKind::Primitive(r)) => {
                if let Some(result) = Self::coerced_numeric_result(*l, *r) {
                    TyKind::Primitive(result)
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
        if matches!(self.current_context(), TypingContext::Permissive)
            && (matches!(lhs_ty, TyKind::Var(_)) || matches!(rhs_ty, TyKind::Var(_)))
        {
            return match (lhs_ty, rhs_ty) {
                (TyKind::Primitive(l), TyKind::Primitive(r))
                    if l.is_numeric() && r.is_numeric() =>
                {
                    TyKind::Primitive(PrimTy::Bool)
                }
                (TyKind::Var(_), TyKind::Primitive(r)) | (TyKind::Primitive(r), TyKind::Var(_))
                    if r.is_numeric() =>
                {
                    TyKind::Primitive(PrimTy::Bool)
                }
                _ => TyKind::Unknown,
            };
        }

        if ty_kinds_compatible(lhs_ty, rhs_ty) {
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

        if !ty_kinds_assignable(&declared_ty.kind, expr_ty) {
            self.errors.push(TypeError::BindingTypeMismatch {
                declared: declared_ty.kind.to_string(),
                actual: expr_ty.to_string(),
                span,
            });
        }
    }

    // ── Function signature helpers ───────────────────────────────────

    /// Construct a `TyKind::Fn(params, ret)` from parameter types and a
    /// return type kind.
    fn make_fn_ty(param_tys: Vec<Ty>, ret: TyKind) -> TyKind {
        TyKind::Fn(
            param_tys,
            Box::new(Ty {
                kind: ret,
                ..Ty::default()
            }),
        )
    }

    /// Build a `Fn(params) -> return` type for a function declaration and
    /// store it in the type table keyed by the function's HIR id.
    fn register_function_signature(&mut self, decl: &hir::FunctionDeclaration) {
        let param_tys: Vec<Ty> = decl
            .parameters
            .iter()
            .map(|p| p.type_annotation.clone())
            .collect();
        let fn_ty = Self::make_fn_ty(param_tys, decl.return_type.kind.clone());
        self.type_table.insert(
            decl.hir_id,
            TypeInfo {
                ty: Ty {
                    kind: fn_ty,
                    ..Ty::default()
                },
            },
        );
    }

    /// After visiting the function body, infer the return type from the
    /// body's trailing expression (if no explicit return type annotation)
    /// and check that body type matches the declared return type.
    fn check_function_body_return(&mut self, decl: &hir::FunctionDeclaration) {
        // Only check when the body has a trailing expression.
        // If the body ends with statements only (e.g. `return` statements),
        // the return type is checked via return-statement checking instead.
        let body_ty = match decl.body.expr.as_ref() {
            Some(e) => &e.ty.kind,
            None => return,
        };

        let declared_ret = &decl.return_type.kind;

        if matches!(declared_ret, TyKind::Unknown) || matches!(body_ty, TyKind::Unknown) {
            // No annotation or unknown body — nothing to check.
            return;
        }

        if !ty_kinds_assignable(declared_ret, body_ty) {
            self.errors.push(TypeError::ReturnTypeMismatch {
                expected: declared_ret.to_string(),
                actual: body_ty.to_string(),
                span: decl.body.span,
            });
        }
    }

    // ── Call expression checking ─────────────────────────────────────

    /// Type-check a call expression. Resolves the callee's function type,
    /// checks argument count and types, and returns the call's result type.
    fn check_call(&mut self, call: &hir::CallExpression, span: tlang_span::Span) -> TyKind {
        // Struct/enum record construction is lowered to a call with exactly
        // one dict argument (e.g. `Point { x: 1, y: 2 }` →
        // `Call(Point, [{ x: 1, y: 2 }])`). Only that lowered shape should
        // bypass normal positional argument checking. Any other constructor
        // call form must fall through to the regular call checker so that
        // wrong arity / wrong argument-shape diagnostics are still emitted.
        if let hir::ExprKind::Path(path) = &call.callee.kind {
            let binding_kind = path.res.binding_kind();
            if matches!(
                binding_kind,
                hir::BindingKind::Struct | hir::BindingKind::Variant
            ) && call.arguments.len() == 1
                && matches!(call.arguments[0].kind, hir::ExprKind::Dict(_))
            {
                // Preserve constructor result-type resolution metadata. For
                // enum variants, use the registered constructor signature's
                // return type instead of rebuilding a path with `as_init()`,
                // which drops `Path.res`.
                if let Some(hir_id) = path.res.hir_id()
                    && let Some(info) = self.type_table.get(&hir_id)
                    && let TyKind::Fn(_, ret) = &info.ty.kind
                {
                    return ret.kind.clone();
                }

                return match binding_kind {
                    hir::BindingKind::Struct => TyKind::Path((**path).clone(), Vec::new()),
                    hir::BindingKind::Variant => unreachable!(
                        "enum variant constructors must have a registered function signature"
                    ),
                    _ => unreachable!(),
                };
            }
            // Discriminant (unit) variant used as a value (no call args):
            // resolve to the enum type directly.
            if binding_kind == hir::BindingKind::Variant
                && let Some(hir_id) = path.res.hir_id()
                && let Some(info) = self.type_table.get(&hir_id)
                && !matches!(info.ty.kind, TyKind::Fn(_, _))
            {
                return info.ty.kind.clone();
            }
        }

        // Try to resolve the callee to a function signature.
        let fn_ty = self.resolve_call_callee_type(call);

        match fn_ty {
            Some(TyKind::Fn(ref param_tys, ref ret_ty)) => {
                let callee_name = callee_name_str(&call.callee);
                let is_variadic = callee_name.as_deref().is_some_and(builtins::is_variadic);
                let is_protocol_dispatch = matches!(
                    &call.callee.kind,
                    hir::ExprKind::Path(path) if self.protocol_dispatch_parts(path).is_some()
                );

                // Check argument count (skip for variadic builtins).
                if !is_variadic && param_tys.len() != call.arguments.len() {
                    self.errors.push(TypeError::ArgumentCountMismatch {
                        expected: param_tys.len(),
                        actual: call.arguments.len(),
                        span,
                    });
                    return ret_ty.kind.clone();
                }

                self.check_argument_types(
                    call,
                    param_tys,
                    &callee_name,
                    is_variadic,
                    is_protocol_dispatch,
                );

                // Instantiate generic return type: if the return type
                // contains type variables, match argument types against
                // parameter types to infer bindings and substitute.
                if ty_contains_var(&ret_ty.kind) {
                    let mut bindings = HashMap::new();
                    for (arg, param_ty) in call.arguments.iter().zip(param_tys.iter()) {
                        collect_type_var_bindings(&arg.ty.kind, &param_ty.kind, &mut bindings);
                    }
                    if !bindings.is_empty() {
                        return substitute_type_vars(&ret_ty.kind, &bindings);
                    }
                }

                ret_ty.kind.clone()
            }
            _ => {
                // Callee type is unknown — cannot check, propagate unknown.
                TyKind::Unknown
            }
        }
    }

    /// Validate argument types against parameter types and emit diagnostics.
    fn check_argument_types(
        &mut self,
        call: &hir::CallExpression,
        param_tys: &[Ty],
        callee_name: &Option<String>,
        is_variadic: bool,
        is_protocol_dispatch: bool,
    ) {
        if is_variadic {
            if let Some(param_ty) = param_tys.first() {
                for (i, arg) in call.arguments.iter().enumerate() {
                    self.check_single_arg(arg, param_ty, i, callee_name, is_protocol_dispatch);
                }
            }
        } else {
            for (i, (arg, param_ty)) in call.arguments.iter().zip(param_tys.iter()).enumerate() {
                self.check_single_arg(arg, param_ty, i, callee_name, is_protocol_dispatch);
            }
        }
    }

    fn check_single_arg(
        &mut self,
        arg: &hir::Expr,
        param_ty: &Ty,
        index: usize,
        callee_name: &Option<String>,
        is_protocol_dispatch: bool,
    ) {
        if matches!(param_ty.kind, TyKind::Unknown)
            || matches!(arg.ty.kind, TyKind::Unknown)
            || is_protocol_dispatch && Self::is_protocol_generic_placeholder(&param_ty.kind)
        {
            return;
        }
        if !ty_kinds_assignable(&param_ty.kind, &arg.ty.kind) {
            let param_name = callee_name
                .as_deref()
                .map(|n| format!("arg{index} of `{n}`"))
                .unwrap_or_else(|| format!("arg{index}"));
            self.errors.push(TypeError::ArgumentTypeMismatch {
                param_name,
                expected: param_ty.kind.to_string(),
                actual: arg.ty.kind.to_string(),
                span: arg.span,
            });
        }
    }

    fn is_protocol_generic_placeholder(kind: &TyKind) -> bool {
        let TyKind::Path(path, _) = kind else {
            return false;
        };
        if !path.res.is_unresolved() || path.segments.len() != 1 {
            return false;
        }
        let name = path.first_ident().as_str();
        name.len() == 1
            && name
                .chars()
                .next()
                .is_some_and(|ch| ch.is_ascii_uppercase())
    }

    /// Try to resolve the callee expression to a `TyKind::Fn`.
    fn resolve_callee_type(&self, callee: &hir::Expr) -> Option<TyKind> {
        match &callee.kind {
            hir::ExprKind::Path(path) => {
                // First try the type table (user-defined functions).
                if let Some(hir_id) = path.res.hir_id()
                    && let Some(info) = self.type_table.get(&hir_id)
                    && matches!(info.ty.kind, TyKind::Fn(..))
                {
                    return Some(info.ty.kind.clone());
                }
                // Only fall back to builtin signatures when the path does
                // not resolve to a local binding (parameter, variable,
                // upvar, closure).
                let is_local = matches!(
                    path.res.binding_kind(),
                    hir::BindingKind::Param
                        | hir::BindingKind::Local
                        | hir::BindingKind::Upvar
                        | hir::BindingKind::Closure
                );
                if is_local {
                    return None;
                }
                let name = path.join("::");
                builtins::lookup(&name)
            }
            _ => {
                // For other callee forms (e.g. field access, closures),
                // check if the expression already has a Fn type.
                match &callee.ty.kind {
                    TyKind::Fn(..) => Some(callee.ty.kind.clone()),
                    _ => None,
                }
            }
        }
    }

    fn resolve_call_callee_type(&self, call: &hir::CallExpression) -> Option<TyKind> {
        self.resolve_protocol_dispatch_callee_type(call)
            .or_else(|| self.resolve_builtin_method_call_callee_type(call))
            .or_else(|| self.resolve_callee_type(&call.callee))
    }

    fn resolve_builtin_method_call_callee_type(
        &self,
        call: &hir::CallExpression,
    ) -> Option<TyKind> {
        let hir::ExprKind::FieldAccess(base, method) = &call.callee.kind else {
            return None;
        };

        let type_name = builtin_methods::type_name_from_kind(&base.ty.kind)?;
        let candidates = builtin_methods::lookup_all(type_name, method.as_str());
        let selected = candidates
            .iter()
            .find(|candidate| {
                matches!(candidate, TyKind::Fn(params, _) if params.len() == call.arguments.len())
            })
            .or_else(|| candidates.first())?;

        Some(builtin_methods::substitute_receiver_type_vars(
            &base.ty.kind,
            selected,
        ))
    }

    fn resolve_protocol_dispatch_callee_type(&self, call: &hir::CallExpression) -> Option<TyKind> {
        let hir::ExprKind::Path(path) = &call.callee.kind else {
            return None;
        };
        if call.arguments.is_empty() {
            return None;
        }

        let (protocol_name, method_name) = self.protocol_dispatch_parts(path)?;
        let receiver_ty = &call.arguments[0].ty.kind;
        let receiver_type_name = Self::receiver_dispatch_type_name(receiver_ty)?;

        self.resolve_impl_protocol_dispatch_callee_type(
            &protocol_name,
            &method_name,
            receiver_ty,
            &receiver_type_name,
        )
        .or_else(|| {
            self.resolve_builtin_protocol_dispatch_callee_type(
                &protocol_name,
                &method_name,
                receiver_ty,
                &receiver_type_name,
            )
        })
    }

    fn resolve_impl_protocol_dispatch_callee_type(
        &self,
        protocol_name: &str,
        method_name: &str,
        receiver_ty: &TyKind,
        receiver_type_name: &str,
    ) -> Option<TyKind> {
        let proto_info = self.type_table.get_protocol_info(protocol_name)?;
        let method = proto_info
            .methods
            .iter()
            .find(|candidate| candidate.name.as_str() == method_name)?;
        let impl_info = self
            .type_table
            .find_impl_for(protocol_name, receiver_type_name)?;
        let protocol_bindings =
            self.resolve_protocol_dispatch_type_bindings(proto_info, impl_info, receiver_ty);

        let mut type_bindings = protocol_bindings.clone();
        if let Some(self_param) = method.param_tys.first() {
            let receiver_pattern = substitute_type_vars(&self_param.kind, &protocol_bindings);
            collect_type_var_bindings(receiver_ty, &receiver_pattern, &mut type_bindings);
        }

        let mut param_tys: Vec<Ty> = method
            .param_tys
            .iter()
            .map(|ty| Ty {
                kind: substitute_type_vars(&ty.kind, &type_bindings),
                ..ty.clone()
            })
            .collect();
        if let Some(self_param) = param_tys.first_mut() {
            self_param.kind = receiver_ty.clone();
        } else {
            param_tys.push(Ty {
                kind: receiver_ty.clone(),
                ..Ty::default()
            });
        }

        let substituted_return_ty = Ty {
            kind: substitute_type_vars(&method.return_ty.kind, &type_bindings),
            ..method.return_ty.clone()
        };
        let return_ty = self.resolve_protocol_dispatch_return_type(
            protocol_name,
            receiver_type_name,
            &substituted_return_ty,
        );

        Some(TyKind::Fn(param_tys, Box::new(return_ty)))
    }

    fn resolve_protocol_dispatch_type_bindings(
        &self,
        proto_info: &ProtocolInfo,
        impl_info: &ImplInfo,
        receiver_ty: &TyKind,
    ) -> HashMap<TypeVarId, TyKind> {
        let mut receiver_bindings = HashMap::new();
        self.collect_receiver_type_arg_bindings(receiver_ty, impl_info, &mut receiver_bindings);

        let mut protocol_bindings = HashMap::new();
        for (proto_type_param, impl_protocol_arg) in proto_info
            .type_param_var_ids
            .iter()
            .zip(impl_info.protocol_type_arg_tys.iter())
        {
            protocol_bindings.insert(
                *proto_type_param,
                substitute_type_vars(&impl_protocol_arg.kind, &receiver_bindings),
            );
        }

        protocol_bindings
    }

    fn collect_receiver_type_arg_bindings(
        &self,
        receiver_ty: &TyKind,
        impl_info: &ImplInfo,
        bindings: &mut HashMap<TypeVarId, TyKind>,
    ) {
        match receiver_ty {
            TyKind::List(inner) | TyKind::Slice(inner) if impl_info.target_type_name == "List" => {
                if let Some(pattern) = impl_info.target_type_arguments.first() {
                    collect_type_var_bindings(&inner.kind, &pattern.kind, bindings);
                }
            }
            TyKind::Dict(key, value) if impl_info.target_type_name == "Dict" => {
                if let Some(pattern) = impl_info.target_type_arguments.first() {
                    collect_type_var_bindings(&key.kind, &pattern.kind, bindings);
                }
                if let Some(pattern) = impl_info.target_type_arguments.get(1) {
                    collect_type_var_bindings(&value.kind, &pattern.kind, bindings);
                }
            }
            _ => {}
        }
    }

    fn resolve_builtin_protocol_dispatch_callee_type(
        &self,
        _protocol_name: &str,
        method_name: &str,
        receiver_ty: &TyKind,
        receiver_type_name: &str,
    ) -> Option<TyKind> {
        let method_ty = builtin_methods::lookup(receiver_type_name, method_name)?;
        let TyKind::Fn(method_params, method_ret) =
            builtin_methods::substitute_receiver_type_vars(receiver_ty, &method_ty)
        else {
            return None;
        };

        let mut param_tys = Vec::with_capacity(method_params.len() + 1);
        param_tys.push(Ty {
            kind: receiver_ty.clone(),
            ..Ty::default()
        });
        param_tys.extend(method_params);

        Some(TyKind::Fn(param_tys, method_ret))
    }

    fn resolve_protocol_dispatch_return_type(
        &self,
        protocol_name: &str,
        receiver_type_name: &str,
        return_ty: &Ty,
    ) -> Ty {
        let TyKind::Path(path, _) = &return_ty.kind else {
            return return_ty.clone();
        };
        if path.segments.len() != 1 {
            return return_ty.clone();
        }

        let assoc_type_name = path.first_ident().as_str();
        let Some(binding) = self.type_table.resolve_associated_type(
            protocol_name,
            receiver_type_name,
            assoc_type_name,
        ) else {
            return return_ty.clone();
        };

        Ty {
            kind: builtin_types::lookup(&binding).unwrap_or_else(|| {
                TyKind::Path(
                    hir::Path::new(
                        vec![hir::PathSegment::from_str(
                            &binding,
                            tlang_span::Span::default(),
                        )],
                        tlang_span::Span::default(),
                    ),
                    Vec::new(),
                )
            }),
            ..Ty::default()
        }
    }

    fn receiver_dispatch_type_name(receiver_ty: &TyKind) -> Option<String> {
        match receiver_ty {
            TyKind::List(_) | TyKind::Slice(_) => Some("List".to_string()),
            TyKind::Primitive(PrimTy::String) => Some("String".to_string()),
            TyKind::Path(path, _) => Some(path.join("::")),
            _ => None,
        }
    }

    fn protocol_dispatch_parts(&self, path: &hir::Path) -> Option<(String, String)> {
        if !Self::is_protocol_path(path) {
            return None;
        }

        let method_name = path.last_ident().as_str().to_string();
        let protocol_name = path.segments[..path.segments.len() - 1]
            .iter()
            .map(|segment| segment.ident.as_str())
            .collect::<Vec<_>>()
            .join("::");
        let proto_info = self.type_table.get_protocol_info(&protocol_name)?;
        proto_info
            .methods
            .iter()
            .any(|method| method.name.as_str() == method_name)
            .then_some((protocol_name, method_name))
    }

    fn is_protocol_path(path: &hir::Path) -> bool {
        path.segments.len() >= Self::MIN_PROTOCOL_PATH_SEGMENTS
    }

    fn visit_dispatch_receiver_arg(&mut self, call: &mut hir::CallExpression) -> bool {
        let Some(receiver) = call.arguments.first_mut() else {
            return false;
        };
        let hir::ExprKind::Path(path) = &call.callee.kind else {
            return false;
        };
        if self.protocol_dispatch_parts(path).is_none()
            || matches!(receiver.kind, hir::ExprKind::FunctionExpression(_))
        {
            return false;
        }

        self.visit_expr(receiver, &mut ());
        true
    }

    // ── Return statement checking ────────────────────────────────────

    fn check_return_type(&mut self, return_expr: Option<&hir::Expr>, span: tlang_span::Span) {
        let actual = return_expr
            .map(|e| &e.ty.kind)
            .unwrap_or(&TyKind::Primitive(PrimTy::Nil));

        // Record the observed return type for later inference.
        if let Some(observed) = self.observed_return_types.last_mut() {
            observed.push(actual.clone());
        }

        let expected = match self.return_type_stack.last() {
            Some(ty) => ty,
            None => return, // top-level, no function context
        };

        if matches!(expected, TyKind::Unknown) {
            return; // No declared return type — nothing to check.
        }

        if matches!(actual, TyKind::Unknown) {
            return; // Unknown value — skip.
        }

        if !ty_kinds_assignable(expected, actual) {
            self.errors.push(TypeError::ReturnTypeMismatch {
                expected: expected.to_string(),
                actual: actual.to_string(),
                span,
            });
        }
    }

    /// Infer a return type from observed `return` statements during the
    /// current function's body traversal.  If all observed returns agree
    /// on a single concrete (non-unknown) type, return that type; otherwise
    /// return `None`.
    fn infer_return_type_from_observed(&self) -> Option<TyKind> {
        let observed = self.observed_return_types.last()?;
        let mut inferred: Option<&TyKind> = None;
        for ty in observed {
            if matches!(ty, TyKind::Unknown) {
                continue;
            }
            match inferred {
                None => inferred = Some(ty),
                Some(prev) if ty_kinds_compatible(prev, ty) => {}
                Some(_) => return None, // conflicting types
            }
        }
        inferred.cloned()
    }

    fn partial_completion_type<'a>(types: impl Iterator<Item = &'a TyKind>) -> Option<TyKind> {
        let mut inferred: Option<&TyKind> = None;

        for ty in types {
            if matches!(ty, TyKind::Unknown) || ty_contains_var(ty) {
                continue;
            }

            match inferred {
                None => inferred = Some(ty),
                Some(prev) if ty_kinds_compatible(prev, ty) => {}
                Some(_) => return None,
            }
        }

        inferred.cloned()
    }

    fn partial_completion_type_owned(types: impl Iterator<Item = TyKind>) -> Option<TyKind> {
        let mut inferred: Option<TyKind> = None;

        for ty in types {
            if matches!(ty, TyKind::Unknown) || ty_contains_var(&ty) {
                continue;
            }

            match inferred.as_ref() {
                None => inferred = Some(ty),
                Some(prev) if ty_kinds_compatible(prev, &ty) => {}
                Some(_) => return None,
            }
        }

        inferred
    }

    fn is_provisional_return_seed_candidate(ty: &TyKind) -> bool {
        match ty {
            TyKind::Primitive(_) | TyKind::Path(..) => true,
            TyKind::Union(tys) => tys
                .iter()
                .all(|ty| Self::is_provisional_return_seed_candidate(&ty.kind)),
            TyKind::Unknown
            | TyKind::Fn(..)
            | TyKind::List(..)
            | TyKind::Slice(..)
            | TyKind::Dict(..)
            | TyKind::Never
            | TyKind::Var(_) => false,
        }
    }

    fn pattern_binds_nested(pat: &hir::Pat, target: hir::HirId, nested: bool) -> bool {
        match &pat.kind {
            hir::PatKind::Identifier(hir_id, _) => *hir_id == target && nested,
            hir::PatKind::List(items) => items
                .iter()
                .any(|item| Self::pattern_binds_nested(item, target, true)),
            hir::PatKind::Rest(inner) => Self::pattern_binds_nested(inner, target, true),
            hir::PatKind::Enum(_, fields) => fields
                .iter()
                .any(|(_, field)| Self::pattern_binds_nested(field, target, true)),
            hir::PatKind::Wildcard | hir::PatKind::Literal(_) => false,
        }
    }

    fn infer_provisional_return_type(expr: &hir::Expr) -> Option<TyKind> {
        if !matches!(expr.ty.kind, TyKind::Unknown) && !ty_contains_var(&expr.ty.kind) {
            return Some(expr.ty.kind.clone());
        }

        match &expr.kind {
            hir::ExprKind::Block(block) | hir::ExprKind::Loop(block) => block
                .expr
                .as_ref()
                .and_then(Self::infer_provisional_return_type),
            hir::ExprKind::Break(Some(value)) => Self::infer_provisional_return_type(value),
            hir::ExprKind::IfElse(_, then_block, else_clauses) => {
                let has_final_else = else_clauses.iter().any(|clause| clause.condition.is_none());
                if !has_final_else {
                    return None;
                }

                Self::partial_completion_type(
                    std::iter::once(
                        then_block
                            .expr
                            .as_ref()
                            .map(|e| &e.ty.kind)
                            .unwrap_or(&TyKind::Unknown),
                    )
                    .chain(else_clauses.iter().map(|clause| {
                        clause
                            .consequence
                            .expr
                            .as_ref()
                            .map(|e| &e.ty.kind)
                            .unwrap_or(&TyKind::Unknown)
                    })),
                )
            }
            hir::ExprKind::Match(_, arms) => {
                Self::partial_completion_type_owned(arms.iter().map(|arm| {
                    arm.block
                        .expr
                        .as_ref()
                        .and_then(|expr| match &expr.kind {
                            hir::ExprKind::Path(path)
                                if !matches!(expr.ty.kind, TyKind::Unknown)
                                    && !ty_contains_var(&expr.ty.kind) =>
                            {
                                path.res.hir_id().and_then(|hir_id| {
                                    Self::pattern_binds_nested(&arm.pat, hir_id, false)
                                        .then_some(expr.ty.kind.clone())
                                })
                            }
                            _ => Self::infer_provisional_return_type(expr),
                        })
                        .unwrap_or(TyKind::Unknown)
                }))
            }
            _ => None,
        }
    }

    fn recheck_body_with_provisional_return(
        &mut self,
        decl: &mut hir::FunctionDeclaration,
        error_checkpoint: usize,
    ) {
        if !matches!(decl.return_type.kind, TyKind::Unknown) {
            return;
        }

        let provisional = decl
            .body
            .expr
            .as_ref()
            .and_then(Self::infer_provisional_return_type)
            .or_else(|| self.infer_return_type_from_observed());

        let Some(provisional) = provisional else {
            return;
        };
        if matches!(provisional, TyKind::Unknown) || ty_contains_var(&provisional) {
            return;
        }
        if !Self::is_provisional_return_seed_candidate(&provisional) {
            return;
        }

        self.errors.truncate(error_checkpoint);
        decl.return_type.kind = provisional.clone();
        self.register_function_signature(decl);

        if let Some(ret) = self.return_type_stack.last_mut() {
            *ret = provisional.clone();
        }
        if let Some(observed) = self.observed_return_types.last_mut() {
            observed.clear();
        }

        if let Some(expr) = &mut decl.body.expr {
            self.apply_expected_expr_type(expr, &provisional);
        }
        self.visit_block(&mut decl.body, &mut ());
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
                // When the binding type is a list/slice, decompose it so that
                // element patterns get the element type and rest patterns get
                // a Slice type (the view into remaining elements).
                let elem_ty = self.iterable_item_type(binding_ty);

                // Rest patterns produce a Slice<T> regardless of whether the
                // original collection is List<T> or Slice<T>.
                let rest_ty = match binding_ty {
                    TyKind::List(inner) | TyKind::Slice(inner) => {
                        TyKind::Slice(Box::new(inner.as_ref().clone()))
                    }
                    _ => {
                        // Non-list binding (e.g. Iterable impl or unknown) —
                        // build Slice from the iterable element type if known.
                        let inner_kind = elem_ty.clone().unwrap_or(TyKind::Unknown);
                        TyKind::Slice(Box::new(Ty {
                            kind: inner_kind,
                            ..Ty::default()
                        }))
                    }
                };

                for p in pats {
                    let is_rest = matches!(p.kind, hir::PatKind::Rest(_));
                    if is_rest {
                        self.register_pat_bindings(p, &rest_ty);
                    } else {
                        let ty = elem_ty.as_ref().unwrap_or(&TyKind::Unknown);
                        self.register_pat_bindings(p, ty);
                    }
                }
            }
            hir::PatKind::Rest(inner) => {
                self.register_pat_bindings(inner, binding_ty);
            }
            hir::PatKind::Enum(path, fields) => {
                // Try to resolve individual field types from struct/enum metadata.
                //
                // For single-segment paths (`Page { title }`), the path names the
                // struct directly and we look up struct field info.
                //
                // For multi-segment paths (`Animal::Dog(name)`), the second-to-last
                // segment names the enum and the last segment names the variant.
                //
                // When the field type cannot be resolved (unknown struct/variant),
                // we fall back to `Unknown` to avoid false type errors — unresolved
                // bindings remain permissive rather than inheriting the container
                // type which would cause spurious type-mismatch errors.
                let last_ident = path.last_ident().as_str().to_owned();
                let struct_hir_id = Self::resolve_type_hir_id(binding_ty);
                let is_enum_variant = path.segments.len() >= 2;

                for (field_idx, (field_key, p)) in fields.iter_mut().enumerate() {
                    let field_ty = if is_enum_variant {
                        // Enum variant pattern: look up variant parameter type.
                        let enum_name = path.segments[path.segments.len() - 2]
                            .ident
                            .as_str()
                            .to_owned();
                        let variant_name = &last_ident;
                        let find_variant_field = |info: &EnumInfo| {
                            info.variants
                                .iter()
                                .find(|v| v.name.as_str() == variant_name)
                                .and_then(|v| {
                                    v.parameters
                                        .iter()
                                        .find(|(n, _)| n.as_str() == field_key.as_str())
                                        .or_else(|| v.parameters.get(field_idx))
                                        .map(|(_, ty)| ty.kind.clone())
                                })
                        };
                        struct_hir_id
                            .and_then(|id| self.type_table.get_enum_info(&id))
                            .and_then(find_variant_field)
                            .or_else(|| {
                                self.type_table
                                    .get_enum_info_by_name(&enum_name)
                                    .and_then(find_variant_field)
                            })
                    } else {
                        // Struct pattern: look up struct field type.
                        struct_hir_id
                            .and_then(|id| {
                                self.resolve_field_type(binding_ty, &id, field_key.as_str())
                            })
                            .or_else(|| {
                                self.type_table
                                    .get_struct_info_by_name(&last_ident)
                                    .and_then(|info| {
                                        info.fields
                                            .iter()
                                            .find(|(n, _)| n.as_str() == field_key.as_str())
                                            .map(|(_, ty)| ty.kind.clone())
                                    })
                            })
                    };

                    if let Some(field_ty) = field_ty.filter(Self::should_propagate_field_ty) {
                        p.ty.kind = field_ty.clone();
                        self.register_pat_bindings(p, &field_ty);
                    } else {
                        // Cannot resolve field type; leave Unknown to avoid false errors.
                        self.register_pat_bindings(p, &TyKind::Unknown);
                    }
                }
            }
            hir::PatKind::Wildcard | hir::PatKind::Literal(_) => {}
        }
    }

    /// Register pattern bindings against the scrutinee expression itself.
    ///
    /// This preserves heterogeneous element types for synthetic match
    /// scrutinees like `[arg0, arg1]` used by multi-clause function lowering.
    /// Falling back to the aggregate list type would smear every element into a
    /// homogeneous `List`, which breaks typed multi-parameter dispatch.
    fn register_match_pat_bindings(&mut self, pat: &mut hir::Pat, scrutinee: &hir::Expr) {
        match (&mut pat.kind, &scrutinee.kind) {
            (hir::PatKind::List(pats), hir::ExprKind::List(items))
                if pats.len() == items.len()
                    && !pats
                        .iter()
                        .any(|pat| matches!(pat.kind, hir::PatKind::Rest(_))) =>
            {
                pat.ty.kind = scrutinee.ty.kind.clone();
                for (item_pat, item_expr) in pats.iter_mut().zip(items.iter()) {
                    self.register_match_pat_bindings(item_pat, item_expr);
                }
            }
            _ => {
                pat.ty.kind = scrutinee.ty.kind.clone();
                self.register_pat_bindings(pat, &scrutinee.ty.kind);
            }
        }
    }

    /// Field types declared in structs/enums are safe to propagate to
    /// destructuring bindings only when they resolved to a concrete type.
    fn should_propagate_field_ty(kind: &TyKind) -> bool {
        match kind {
            TyKind::Unknown | TyKind::Var(_) => false,
            TyKind::Path(path, _) => !path.res.is_unresolved(),
            _ => true,
        }
    }

    fn constructor_param_ty(ty: &Ty) -> Ty {
        if Self::should_propagate_field_ty(&ty.kind) {
            ty.clone()
        } else {
            Ty::unknown()
        }
    }

    // ── Struct and enum declaration registration ──────────────────────

    /// Register a struct declaration in the type table: store field metadata
    /// and register a constructor function type `Fn(field_tys…) → Path(Struct)`.
    fn register_struct_declaration(&mut self, decl: &hir::StructDeclaration) {
        let mut struct_path = hir::Path::new(
            vec![hir::PathSegment { ident: decl.name }],
            tlang_span::Span::default(),
        );
        // Set the resolution so that field access can find the struct info.
        struct_path.res.set_hir_id(decl.hir_id);
        struct_path.res.set_binding_kind(hir::BindingKind::Struct);
        let struct_ty_kind = TyKind::Path(
            struct_path,
            decl.type_params
                .iter()
                .map(|type_param| Ty {
                    kind: TyKind::Var(type_param.type_var_id),
                    ..Ty::default()
                })
                .collect(),
        );

        // Store struct field metadata.
        let fields: Vec<(tlang_ast::node::Ident, Ty)> =
            decl.fields.iter().map(|f| (f.name, f.ty.clone())).collect();
        self.type_table.insert_struct_info(
            decl.hir_id,
            StructInfo {
                name: decl.name,
                type_param_var_ids: decl.type_params.iter().map(|tp| tp.type_var_id).collect(),
                fields: fields.clone(),
            },
        );

        // Register constructor: Fn(field_types…) → StructPath
        let param_tys: Vec<Ty> = fields.iter().map(|(_, ty)| ty.clone()).collect();
        let fn_ty = Self::make_fn_ty(param_tys, struct_ty_kind);
        self.type_table.insert(
            decl.hir_id,
            TypeInfo {
                ty: Ty {
                    kind: fn_ty,
                    ..Ty::default()
                },
            },
        );
    }

    /// Register an enum declaration in the type table: store variant metadata
    /// and register each variant constructor.
    fn register_enum_declaration(&mut self, decl: &hir::EnumDeclaration) {
        let mut enum_path = hir::Path::new(
            vec![hir::PathSegment { ident: decl.name }],
            tlang_span::Span::default(),
        );
        // Set the resolution so that variant access can find the enum info.
        enum_path.res.set_hir_id(decl.hir_id);
        enum_path.res.set_binding_kind(hir::BindingKind::Enum);
        let enum_ty_kind = TyKind::Path(
            enum_path,
            decl.type_params
                .iter()
                .map(|type_param| Ty {
                    kind: TyKind::Var(type_param.type_var_id),
                    ..Ty::default()
                })
                .collect(),
        );

        // Collect variant info.
        let variants: Vec<VariantInfo> = decl
            .variants
            .iter()
            .map(|v| VariantInfo {
                name: v.name,
                parameters: v
                    .parameters
                    .iter()
                    .map(|p| (p.name, p.ty.clone()))
                    .collect(),
            })
            .collect();

        self.type_table.insert_enum_info(
            decl.hir_id,
            EnumInfo {
                name: decl.name,
                type_param_var_ids: decl.type_params.iter().map(|tp| tp.type_var_id).collect(),
                variants,
            },
        );

        // Register each variant as a constructor function.
        for variant in &decl.variants {
            if variant.parameters.is_empty() {
                // Discriminant/unit variant: its type is the enum type.
                self.type_table.insert(
                    variant.hir_id,
                    TypeInfo {
                        ty: Ty {
                            kind: enum_ty_kind.clone(),
                            ..Ty::default()
                        },
                    },
                );
            } else {
                // Constructor variant: Fn(param_types…) → EnumPath
                let param_tys: Vec<Ty> = variant
                    .parameters
                    .iter()
                    .map(|p| Self::constructor_param_ty(&p.ty))
                    .collect();
                let fn_ty = Self::make_fn_ty(param_tys, enum_ty_kind.clone());
                self.type_table.insert(
                    variant.hir_id,
                    TypeInfo {
                        ty: Ty {
                            kind: fn_ty,
                            ..Ty::default()
                        },
                    },
                );
            }
        }
    }

    // ── Protocol and impl block registration ─────────────────────────

    /// Register a protocol declaration in the type table: store method
    /// signatures and constraint protocol names.
    fn register_protocol_declaration(&mut self, decl: &hir::ProtocolDeclaration) {
        let methods: Vec<ProtocolMethodInfo> = decl
            .methods
            .iter()
            .map(|m| ProtocolMethodInfo {
                name: m.name,
                param_tys: m
                    .parameters
                    .iter()
                    .map(|p| p.type_annotation.clone())
                    .collect(),
                return_ty: m.return_type.clone(),
                has_default_body: m.body.is_some(),
            })
            .collect();
        let constraints: Vec<String> = decl.constraints.iter().map(|c| c.join("::")).collect();
        let associated_types: Vec<AssociatedTypeInfo> = decl
            .associated_types
            .iter()
            .map(|at| AssociatedTypeInfo {
                name: at.name,
                type_param_count: at.type_params.len(),
            })
            .collect();
        self.type_table.insert_protocol_info(ProtocolInfo {
            name: decl.name,
            type_param_var_ids: decl
                .type_params
                .iter()
                .map(|param| param.type_var_id)
                .collect(),
            methods,
            constraints,
            associated_types,
        });

        // Register each protocol method signature in the type table so
        // that `Protocol::method(…)` calls can resolve to the method's type.
        for method in &decl.methods {
            let param_tys: Vec<Ty> = method
                .parameters
                .iter()
                .map(|p| p.type_annotation.clone())
                .collect();
            let fn_ty = Self::make_fn_ty(param_tys, method.return_type.kind.clone());
            self.type_table.insert(
                method.hir_id,
                TypeInfo {
                    ty: Ty {
                        kind: fn_ty,
                        ..Ty::default()
                    },
                },
            );
        }
    }

    /// Validate an impl block: check that all required methods from the
    /// protocol are present, and type-check the methods.
    ///
    /// Note: impl registrations are handled by the pre-scan in `check_module`,
    /// so constraint checking is order-independent.
    #[allow(clippy::too_many_lines)]
    fn check_impl_block(&mut self, impl_block: &mut hir::ImplBlock) {
        let protocol_name = impl_block.protocol_name.join("::");
        let target_type_name = impl_block.target_type.join("::");

        // Check that `apply` directives do not conflict with existing
        // dot-methods on the target type.
        for apply_method in &impl_block.apply_methods {
            let dot_name = format!("{target_type_name}.{}", apply_method.as_str());
            if self.dot_methods.contains_key(&dot_name) {
                self.errors.push(TypeError::ApplyConflict {
                    method: apply_method.as_str().to_string(),
                    target_type: target_type_name.clone(),
                    span: apply_method.span,
                });
            }
        }

        // Validate where clause predicates for blanket impls.
        if let Some(wc) = &impl_block.where_clause {
            for pred in &wc.predicates {
                for bound in &pred.bounds {
                    let bound_name = bound.kind.to_string();
                    // Validate that the bound refers to a known protocol path.
                    match &bound.kind {
                        hir::TyKind::Unknown => {}
                        hir::TyKind::Path(..)
                            if self.type_table.get_protocol_info(&bound_name).is_some() => {}
                        _ => {
                            self.errors.push(TypeError::UnknownWhereClauseBound {
                                type_param: pred.name.as_str().to_string(),
                                bound: bound_name,
                                span: pred.span,
                            });
                        }
                    }
                }
            }
        }

        // Collect names of methods present in the impl.
        // `apply` directives only install the method on the target type;
        // they do not count toward satisfying the protocol contract.
        let impl_method_names: Vec<String> = impl_block
            .methods
            .iter()
            .map(|m| {
                // Method names in impl blocks are qualified as "Protocol::method"
                // in the HIR.  Extract the unqualified method name.
                let full = m.name();
                full.rsplit("::").next().unwrap_or(&full).to_string()
            })
            .collect();

        // Check protocol method requirements.
        if let Some(proto_info) = self.type_table.get_protocol_info(&protocol_name).cloned() {
            // Check that every required method is implemented.
            for proto_method in &proto_info.methods {
                let method_name = proto_method.name.as_str();
                let is_present = impl_method_names.iter().any(|n| n == method_name);
                if !is_present && !proto_method.has_default_body {
                    let span = impl_block.target_type.span;
                    self.errors.push(TypeError::MissingProtocolMethod {
                        method: method_name.to_string(),
                        protocol: protocol_name.clone(),
                        target_type: target_type_name.clone(),
                        span,
                    });
                }
            }

            // Check constraint protocols (skip for blanket impls since
            // constraints are deferred to instantiation sites).
            if impl_block.type_params.is_empty() {
                for constraint in &proto_info.constraints {
                    if !self.type_table.has_impl(constraint, &target_type_name) {
                        let span = impl_block.target_type.span;
                        self.errors.push(TypeError::MissingConstraintImpl {
                            constraint: constraint.clone(),
                            protocol: protocol_name.clone(),
                            target_type: target_type_name.clone(),
                            span,
                        });
                    }
                }
            }

            // Validate associated type bindings: every required associated type
            // must have a binding, and no extra bindings are allowed.
            let impl_assoc_names: Vec<String> = impl_block
                .associated_types
                .iter()
                .map(|at| at.name.as_str().to_string())
                .collect();

            for proto_assoc in &proto_info.associated_types {
                let name = proto_assoc.name.as_str();
                if !impl_assoc_names.iter().any(|n| n == name) {
                    let span = impl_block.target_type.span;
                    self.errors.push(TypeError::MissingAssociatedType {
                        name: name.to_string(),
                        protocol: protocol_name.clone(),
                        target_type: target_type_name.clone(),
                        span,
                    });
                }
            }

            let proto_assoc_names: Vec<String> = proto_info
                .associated_types
                .iter()
                .map(|at| at.name.as_str().to_string())
                .collect();
            for at in &impl_block.associated_types {
                let name = at.name.as_str();
                if !proto_assoc_names.iter().any(|n| n == name) {
                    self.errors.push(TypeError::UnexpectedAssociatedType {
                        name: name.to_string(),
                        protocol: protocol_name.clone(),
                        span: at.span,
                    });
                }
            }
        }

        // Type-check the impl methods.
        for decl in &mut impl_block.methods {
            self.typecheck_function_decl(decl);
        }
    }
    fn resolve_field_type(
        &self,
        base_ty_kind: &TyKind,
        struct_hir_id: &tlang_span::HirId,
        field_name: &str,
    ) -> Option<TyKind> {
        if let Some(info) = self.type_table.get_struct_info(struct_hir_id) {
            let mut bindings = HashMap::new();
            if !info.type_param_var_ids.is_empty() {
                let mut struct_path = hir::Path::new(
                    vec![hir::PathSegment { ident: info.name }],
                    tlang_span::Span::default(),
                );
                struct_path.res.set_hir_id(*struct_hir_id);
                struct_path.res.set_binding_kind(hir::BindingKind::Struct);
                let struct_pattern = TyKind::Path(
                    struct_path,
                    info.type_param_var_ids
                        .iter()
                        .map(|id| Ty {
                            kind: TyKind::Var(*id),
                            ..Ty::default()
                        })
                        .collect(),
                );
                collect_type_var_bindings(base_ty_kind, &struct_pattern, &mut bindings);
            }
            for (name, ty) in &info.fields {
                if name.as_str() == field_name {
                    return Some(substitute_type_vars(&ty.kind, &bindings));
                }
            }
        }
        None
    }

    fn resolve_dot_method_type(&self, base_ty_kind: &TyKind, field_name: &str) -> Option<TyKind> {
        let mut method_names = Vec::new();
        if let Some(type_name) = builtin_methods::type_name_from_kind(base_ty_kind) {
            method_names.push(format!("{type_name}.{field_name}"));
        }
        if let TyKind::Path(path, _) = base_ty_kind {
            method_names.push(format!("{}.{}", path.join("::"), field_name));
            method_names.push(format!("{}.{}", path.last_ident(), field_name));
        }

        let method_hir_id = method_names
            .into_iter()
            .find_map(|method_name| self.dot_methods.get(&method_name).copied())?;
        let method_info = self.type_table.get(&method_hir_id)?;

        let TyKind::Fn(param_tys, ret_ty) = &method_info.ty.kind else {
            return None;
        };

        let mut bindings = HashMap::new();
        let has_receiver = param_tys.first().is_some_and(|receiver_ty| {
            Self::dot_method_has_receiver(base_ty_kind, &receiver_ty.kind)
        });
        if has_receiver && let Some(receiver_ty) = param_tys.first() {
            collect_type_var_bindings(base_ty_kind, &receiver_ty.kind, &mut bindings);
        }

        let remaining_params = param_tys
            .iter()
            .skip(usize::from(has_receiver))
            .map(|param_ty| Ty {
                kind: substitute_type_vars(&param_ty.kind, &bindings),
                ..Ty::default()
            })
            .collect();
        let return_ty = substitute_type_vars(&ret_ty.kind, &bindings);

        Some(TyKind::Fn(
            remaining_params,
            Box::new(Ty {
                kind: return_ty,
                ..Ty::default()
            }),
        ))
    }

    fn dot_method_has_receiver(base_ty_kind: &TyKind, candidate_receiver_ty: &TyKind) -> bool {
        match (base_ty_kind, candidate_receiver_ty) {
            (TyKind::Path(base_path, _), TyKind::Path(receiver_path, _)) => {
                base_path == receiver_path
            }
            (TyKind::List(_), TyKind::List(_))
            | (TyKind::Slice(_), TyKind::Slice(_))
            | (TyKind::List(_), TyKind::Slice(_))
            | (TyKind::Slice(_), TyKind::List(_)) => true,
            (TyKind::Dict(..), TyKind::Dict(..)) => true,
            (TyKind::Primitive(lhs), TyKind::Primitive(rhs)) => lhs == rhs,
            _ => false,
        }
    }

    fn has_dot_method_variant(&self, base_ty_kind: &TyKind, field_name: &str) -> bool {
        let mut prefixes = Vec::new();
        if let Some(type_name) = builtin_methods::type_name_from_kind(base_ty_kind) {
            prefixes.push(format!("{type_name}.{field_name}/"));
        }
        if let TyKind::Path(path, _) = base_ty_kind {
            prefixes.push(format!("{}.{}/", path.join("::"), field_name));
            prefixes.push(format!("{}.{}/", path.last_ident(), field_name));
        }

        prefixes.into_iter().any(|prefix| {
            self.dot_methods
                .keys()
                .any(|name| name.starts_with(&prefix))
        })
    }

    /// Resolve the HirId of a struct/enum type from a TyKind::Path.
    fn resolve_type_hir_id(ty_kind: &TyKind) -> Option<tlang_span::HirId> {
        match ty_kind {
            TyKind::Path(path, _) => path.res.hir_id(),
            _ => None,
        }
    }

    /// Type-check a field access expression (`base.field`).
    /// Resolves the field's type from the base expression's struct type.
    /// Falls back to builtin method lookup for native types.
    /// Emits a diagnostic when the base is a known struct but the field
    /// does not exist.
    fn check_field_access(
        &mut self,
        base_ty_kind: &TyKind,
        field_name: &str,
        expr_hir_id: tlang_span::HirId,
        span: tlang_span::Span,
    ) -> TyKind {
        if let Some(hir_id) = Self::resolve_type_hir_id(base_ty_kind) {
            if let Some(field_ty) = self.resolve_field_type(base_ty_kind, &hir_id, field_name) {
                self.type_table.insert(
                    expr_hir_id,
                    TypeInfo {
                        ty: Ty {
                            kind: field_ty.clone(),
                            ..Ty::default()
                        },
                    },
                );
                return field_ty;
            }

            if let Some(method_ty) = self.resolve_dot_method_type(base_ty_kind, field_name) {
                self.type_table.insert(
                    expr_hir_id,
                    TypeInfo {
                        ty: Ty {
                            kind: method_ty.clone(),
                            ..Ty::default()
                        },
                    },
                );
                return method_ty;
            }

            if self.has_dot_method_variant(base_ty_kind, field_name) {
                self.type_table
                    .insert(expr_hir_id, TypeInfo { ty: Ty::unknown() });
                return TyKind::Unknown;
            }

            // Base is a known struct but the field does not exist.
            if let Some(info) = self.type_table.get_struct_info(&hir_id) {
                let available: Vec<String> =
                    info.fields.iter().map(|(n, _)| n.to_string()).collect();
                self.errors.push(TypeError::UnknownField {
                    field: field_name.to_string(),
                    type_name: info.name.to_string(),
                    available,
                    span,
                });
            }
        }

        // Fall back to builtin method lookup for native types.
        if let Some(type_name) = builtin_methods::type_name_from_kind(base_ty_kind)
            && let Some(method_ty) = builtin_methods::lookup(type_name, field_name)
        {
            // Pre-bind type variables from the receiver type.  For example,
            // `Slice(i64).map(...)` binds T→i64 so the method signature
            // `fn(fn(T) -> U) -> Slice(U)` becomes `fn(fn(i64) -> U) -> Slice(U)`.
            let method_ty =
                builtin_methods::substitute_receiver_type_vars(base_ty_kind, &method_ty);

            self.type_table.insert(
                expr_hir_id,
                TypeInfo {
                    ty: Ty {
                        kind: method_ty.clone(),
                        ..Ty::default()
                    },
                },
            );
            return method_ty;
        }

        TyKind::Unknown
    }

    // ── HIR walk entry point ─────────────────────────────────────────

    fn check_module(&mut self, module: &mut hir::Module) {
        // Seed the type table with builtin native protocol definitions so
        // that impl blocks and where-clause bounds can reference them.
        for info in builtin_protocols::all() {
            self.type_table.insert_protocol_info(info);
        }

        // Pre-scan the module for impl blocks and dot-methods so that
        // validation is order-independent (an impl or dot-method declared
        // after the first use site is still visible).
        let mut dot_methods = HashMap::new();
        for stmt in &module.block.stmts {
            match &stmt.kind {
                hir::StmtKind::FunctionDeclaration(decl) => {
                    if matches!(&decl.name.kind, hir::ExprKind::FieldAccess(..)) {
                        dot_methods.insert(decl.name(), decl.hir_id);
                    }
                }
                hir::StmtKind::ImplBlock(impl_block) => {
                    let is_blanket = !impl_block.type_params.is_empty();
                    let target_type_name = impl_block.target_type.join("::");
                    let target_type_name_str = target_type_name.as_str();
                    let where_predicates: Vec<(String, Vec<String>)> = impl_block
                        .where_clause
                        .as_ref()
                        .map(|wc| {
                            wc.predicates
                                .iter()
                                .map(|pred| {
                                    (
                                        pred.name.as_str().to_string(),
                                        pred.bounds.iter().map(|b| b.kind.to_string()).collect(),
                                    )
                                })
                                .collect()
                        })
                        .unwrap_or_default();
                    let associated_type_bindings: Vec<(String, String)> = impl_block
                        .associated_types
                        .iter()
                        .map(|at| (at.name.as_str().to_string(), at.ty.kind.to_string()))
                        .collect();
                    self.type_table.insert_impl_info(ImplInfo {
                        protocol_name: impl_block.protocol_name.join("::"),
                        target_type_name: target_type_name.clone(),
                        target_type_arguments: impl_block.target_type_arguments.clone(),
                        target_type_is_param: impl_block
                            .type_params
                            .iter()
                            .any(|param| param.name.as_str() == target_type_name_str),
                        protocol_type_args: impl_block
                            .type_arguments
                            .iter()
                            .map(|ty| ty.kind.to_string())
                            .collect(),
                        protocol_type_arg_tys: impl_block.type_arguments.clone(),
                        is_blanket,
                        where_predicates,
                        associated_type_bindings,
                    });
                }
                _ => {}
            }
        }
        self.dot_methods = dot_methods;

        // Top-level is always permissive.
        self.push_context(TypingContext::Permissive);
        self.visit_module(module, &mut ());
        self.pop_context();
    }

    // ── Expression visit helpers ─────────────────────────────────────

    /// Visit a Call or TailCall expression: check arguments, resolve
    /// callee, and set the expression's type to the return type.
    fn visit_binary_expr(
        &mut self,
        op: BinaryOpKind,
        lhs: &mut hir::Expr,
        rhs: &mut hir::Expr,
        expr_hir_id: tlang_span::HirId,
        expr_span: tlang_span::Span,
        expr_ty: &mut Ty,
    ) {
        self.visit_expr(lhs, &mut ());
        self.visit_expr(rhs, &mut ());
        let result_ty = self.check_binary_op(op, &lhs.ty.kind, &rhs.ty.kind, expr_span);

        // For assignments to ANF temporaries (or any variable whose current
        // type is nil/unknown), propagate the RHS type back to the LHS
        // binding so that subsequent reads see the concrete type.
        if op == BinaryOpKind::Assign
            && !matches!(result_ty, TyKind::Unknown)
            && matches!(
                lhs.ty.kind,
                TyKind::Unknown | TyKind::Primitive(PrimTy::Nil)
            )
        {
            lhs.ty.kind = result_ty.clone();
            if let hir::ExprKind::Path(path) = &lhs.kind
                && let Some(hir_id) = path.res.hir_id()
            {
                self.type_table.insert(
                    hir_id,
                    TypeInfo {
                        ty: Ty {
                            kind: result_ty.clone(),
                            ..Ty::default()
                        },
                    },
                );
            }
        }

        expr_ty.kind = result_ty.clone();
        self.type_table.insert(
            expr_hir_id,
            TypeInfo {
                ty: Ty {
                    kind: result_ty,
                    ..Ty::default()
                },
            },
        );
    }

    fn visit_unary_expr(
        &mut self,
        op: UnaryOp,
        operand: &mut hir::Expr,
        expr_hir_id: tlang_span::HirId,
        expr_span: tlang_span::Span,
        expr_ty: &mut Ty,
    ) {
        self.visit_expr(operand, &mut ());
        let result_ty = self.check_unary_op(op, &operand.ty.kind, expr_span);
        expr_ty.kind = result_ty.clone();
        self.type_table.insert(
            expr_hir_id,
            TypeInfo {
                ty: Ty {
                    kind: result_ty,
                    ..Ty::default()
                },
            },
        );
    }

    fn visit_implements_expr(
        &mut self,
        inner_expr: &mut hir::Expr,
        expr_hir_id: tlang_span::HirId,
        expr_ty: &mut Ty,
    ) {
        self.visit_expr(inner_expr, &mut ());
        let result_ty = TyKind::Primitive(PrimTy::Bool);
        expr_ty.kind = result_ty.clone();
        self.type_table.insert(
            expr_hir_id,
            TypeInfo {
                ty: Ty {
                    kind: result_ty,
                    ..Ty::default()
                },
            },
        );
    }

    // ── Conversion expression typing (`as` / `as?`) ─────────────────

    /// Check an infallible `as` cast.  The result type is the target type
    /// when the conversion is valid; otherwise an error is emitted.
    ///
    /// Rules:
    /// - `unknown as T` → compile error (`CastOnUnknown`)
    /// - numeric → numeric (widening, narrowing, int↔float, signed↔unsigned) → allowed
    /// - anything else → check for `Into<target>` impl, or `NoIntoImpl` error
    fn check_cast(&mut self, inner: &mut hir::Expr, target_ty: &hir::Ty, expr: &mut hir::Expr) {
        self.visit_expr(inner, &mut ());

        let source_kind = &inner.ty.kind;
        let target_kind = &target_ty.kind;
        let span = expr.span;

        // `unknown as T` is always an error — use `as?` instead.
        if matches!(source_kind, TyKind::Unknown) {
            self.errors.push(TypeError::CastOnUnknown { span });
            self.assign_expr_type(expr, target_kind.clone());
            return;
        }

        match (source_kind, target_kind) {
            // Numeric → Numeric conversions are always allowed via `as`:
            // widening, narrowing (saturating), int→float, signed→unsigned.
            (TyKind::Primitive(src), TyKind::Primitive(tgt))
                if src.is_numeric() && tgt.is_numeric() =>
            {
                self.assign_expr_type(expr, target_kind.clone());
            }
            // Same type → identity cast, always fine.
            _ if ty_kinds_assignable(target_kind, source_kind) => {
                self.assign_expr_type(expr, target_kind.clone());
            }
            _ => {
                // Check whether an `Into<target>` implementation is registered
                // for the source type.  We require the specific type-argument
                // match so that e.g. `impl Into<bool> for String` does not
                // allow `"x" as i64`.
                let source_name = source_kind.to_string();
                let target_name = target_kind.to_string();
                if self
                    .type_table
                    .has_impl_with_type_arg("Into", &source_name, &target_name)
                {
                    self.assign_expr_type(expr, target_kind.clone());
                } else {
                    self.errors.push(TypeError::NoIntoImpl {
                        from_ty: source_name,
                        to_ty: target_name,
                        span,
                    });
                    // Still assign the target type so downstream checking continues.
                    self.assign_expr_type(expr, target_kind.clone());
                }
            }
        }
    }

    /// Check a fallible `as?` (try-cast).  This is how `unknown` enters
    /// typed code: `unknown_val as? i64` is valid.
    ///
    /// Rules:
    /// - `unknown as? T` → allowed (runtime check)
    /// - float → int → allowed via `as?` (fails on NaN/infinity/out-of-range)
    /// - any numeric → numeric → allowed
    /// - any type → same type → allowed (identity)
    ///
    /// At runtime, `as?` always returns a `Result` (Ok on success, Err on
    /// failure), so the type-checker assigns `Result` as the expression type.
    fn check_try_cast(
        &mut self,
        inner: &mut hir::Expr,
        _target_ty: &hir::Ty,
        expr: &mut hir::Expr,
    ) {
        self.visit_expr(inner, &mut ());

        // `as?` always produces a `Result` regardless of the conversion path.
        let result_ty = Self::builtin_type_path("Result");
        self.assign_expr_type(expr, result_ty);
    }

    fn visit_call_expr(
        &mut self,
        call: &mut hir::CallExpression,
        expr_hir_id: tlang_span::HirId,
        expr_span: tlang_span::Span,
        expr_ty: &mut Ty,
    ) {
        // Visit callee first so its type is available for bidirectional
        // inference into closure arguments.
        self.visit_expr(&mut call.callee, &mut ());
        let receiver_visited = self.visit_dispatch_receiver_arg(call);
        let callee_fn_ty = self.resolve_call_callee_type(call);

        // When the callee is a generic function, we need to infer type
        // variable bindings from non-closure arguments first, then
        // substitute those bindings into expected closure parameter types
        // for bidirectional inference.
        let has_generic_params = callee_fn_ty.as_ref().is_some_and(ty_contains_var);
        let has_closures = call
            .arguments
            .iter()
            .any(|a| matches!(a.kind, hir::ExprKind::FunctionExpression(_)));

        if has_generic_params && has_closures {
            // Two-pass approach: visit non-closure args first to collect
            // type variable bindings, then visit closure args with
            // substituted expected types.
            let mut bindings = HashMap::new();
            if let Some(TyKind::Fn(ref param_tys, _)) = callee_fn_ty {
                // Pass 1: visit non-closure args and collect bindings.
                for (i, arg) in call.arguments.iter_mut().enumerate() {
                    if receiver_visited && i == 0 {
                        if let Some(param_ty) = param_tys.get(i) {
                            collect_type_var_bindings(&arg.ty.kind, &param_ty.kind, &mut bindings);
                        }
                        continue;
                    }
                    if !matches!(arg.kind, hir::ExprKind::FunctionExpression(_)) {
                        if let Some(param_ty) = param_tys.get(i) {
                            self.apply_expected_expr_type(arg, &param_ty.kind);
                        }
                        self.visit_expr(arg, &mut ());
                        if let Some(param_ty) = param_tys.get(i) {
                            collect_type_var_bindings(&arg.ty.kind, &param_ty.kind, &mut bindings);
                        }
                    }
                }

                // Pass 2: visit closure args with substituted expected types.
                for (i, arg) in call.arguments.iter_mut().enumerate() {
                    if let hir::ExprKind::FunctionExpression(decl) = &mut arg.kind
                        && let Some(expected_arg_ty) = param_tys.get(i)
                    {
                        let substituted = substitute_type_vars(&expected_arg_ty.kind, &bindings);
                        Self::apply_expected_closure_types(decl, &substituted);
                        self.visit_expr(arg, &mut ());
                        continue;
                    }
                    self.visit_expr(arg, &mut ());
                }
            }
        } else {
            // Simple path: visit all arguments in order with optional
            // bidirectional inference for closures.
            for (i, arg) in call.arguments.iter_mut().enumerate() {
                if receiver_visited && i == 0 {
                    continue;
                }
                if let Some(TyKind::Fn(ref param_tys, _)) = callee_fn_ty
                    && let Some(expected_arg_ty) = param_tys.get(i)
                {
                    self.apply_expected_expr_type(arg, &expected_arg_ty.kind);
                    if let hir::ExprKind::FunctionExpression(decl) = &mut arg.kind {
                        Self::apply_expected_closure_types(decl, &expected_arg_ty.kind);
                    }
                }
                self.visit_expr(arg, &mut ());
            }
        }

        let result_ty = self.check_call(call, expr_span);
        expr_ty.kind = result_ty.clone();
        self.type_table.insert(
            expr_hir_id,
            TypeInfo {
                ty: Ty {
                    kind: result_ty,
                    ..Ty::default()
                },
            },
        );
    }

    /// Push expected parameter types from a `Fn(…) → …` into a closure's
    /// unannotated parameters (bidirectional inference).
    fn apply_expected_closure_types(decl: &mut hir::FunctionDeclaration, expected: &TyKind) {
        if let TyKind::Fn(expected_params, expected_ret) = expected {
            for (param, expected_ty) in decl.parameters.iter_mut().zip(expected_params.iter()) {
                if matches!(param.type_annotation.kind, TyKind::Unknown) {
                    param.type_annotation = expected_ty.clone();
                }
            }
            // If the closure has no return type annotation, push the expected
            // return type so the body can be checked against it.
            if matches!(decl.return_type.kind, TyKind::Unknown) {
                decl.return_type = (**expected_ret).clone();
            }
        }
    }

    fn apply_expected_block_type(&mut self, block: &mut hir::Block, expected: &TyKind) {
        self.apply_expected_loop_accumulator_type(block, expected);

        if let Some(expr) = &mut block.expr {
            self.apply_expected_expr_type(expr, expected);
        }
    }

    fn apply_expected_loop_accumulator_type(&mut self, block: &mut hir::Block, expected: &TyKind) {
        fn can_refine_from_expected(kind: &TyKind) -> bool {
            match kind {
                TyKind::Unknown => true,
                TyKind::List(inner) | TyKind::Slice(inner) => matches!(inner.kind, TyKind::Unknown),
                TyKind::Dict(key, value) => {
                    matches!(key.kind, TyKind::Unknown) && matches!(value.kind, TyKind::Unknown)
                }
                TyKind::Path(path, type_args) => {
                    type_args.is_empty()
                        && matches!(path.to_string().as_str(), "List" | "Slice" | "Dict")
                }
                _ => false,
            }
        }

        for stmt in &mut block.stmts {
            let hir::StmtKind::Let(pat, _, ty) = &mut stmt.kind else {
                continue;
            };
            let hir::PatKind::Identifier(hir_id, ident) = &pat.kind else {
                continue;
            };
            if ident.as_str() != "accumulator$$" || !can_refine_from_expected(&ty.kind) {
                continue;
            }

            ty.kind = expected.clone();
            if matches!(pat.ty.kind, TyKind::Unknown) {
                pat.ty.kind = expected.clone();
            }
            self.type_table.insert(
                *hir_id,
                TypeInfo {
                    ty: Ty {
                        kind: expected.clone(),
                        ..Ty::default()
                    },
                },
            );
        }
    }

    fn apply_expected_expr_type(&mut self, expr: &mut hir::Expr, expected: &TyKind) {
        if matches!(expected, TyKind::Unknown) || ty_contains_var(expected) {
            return;
        }

        match &mut expr.kind {
            hir::ExprKind::Literal(lit) => {
                // Don't refine a literal that was inlined from an explicitly-typed
                // binding by constant propagation — its type is already concrete
                // and must be respected (e.g. `let x: i64 = 5; let y: isize = x`
                // should remain a type mismatch, not silently coerce `5` to `isize`).
                if !matches!(expr.ty.kind, TyKind::Unknown) {
                    return;
                }
                if matches!(
                    lit.as_ref(),
                    Literal::Integer(_) | Literal::UnsignedInteger(_) | Literal::Float(_)
                ) && let TyKind::Primitive(prim) = expected
                    && prim.is_numeric()
                {
                    expr.ty.kind = expected.clone();
                }
            }
            hir::ExprKind::FunctionExpression(decl) => {
                Self::apply_expected_closure_types(decl, expected);
            }
            hir::ExprKind::Binary(BinaryOpKind::Assign, lhs, rhs) => {
                self.apply_expected_expr_type(lhs, expected);
                self.apply_expected_expr_type(rhs, expected);
            }
            // Propagate a numeric expected type through arithmetic binary
            // operations so that bare integer / float literals inside
            // expressions like `5 + 3`, `a * 2`, etc. pick up the contextual
            // numeric type from an enclosing annotated binding or return type.
            // Non-literal operands (paths, calls) will silently ignore the
            // hint because their `apply_expected_expr_type` branch falls to
            // `_ => {}`, which is correct — we only refine literals here.
            hir::ExprKind::Binary(op, lhs, rhs)
                if matches!(
                    op,
                    BinaryOpKind::Add
                        | BinaryOpKind::Sub
                        | BinaryOpKind::Mul
                        | BinaryOpKind::Div
                        | BinaryOpKind::Mod
                        | BinaryOpKind::Exp
                ) && let TyKind::Primitive(prim) = expected
                    && prim.is_numeric() =>
            {
                self.apply_expected_expr_type(lhs, expected);
                self.apply_expected_expr_type(rhs, expected);
            }
            hir::ExprKind::Block(block) | hir::ExprKind::Loop(block) => {
                self.apply_expected_block_type(block, expected);
            }
            hir::ExprKind::IfElse(_, then_block, else_clauses) => {
                self.apply_expected_block_type(then_block, expected);
                for else_clause in else_clauses {
                    self.apply_expected_block_type(&mut else_clause.consequence, expected);
                }
            }
            hir::ExprKind::Match(_, arms) => {
                for arm in arms {
                    self.apply_expected_block_type(&mut arm.block, expected);
                }
            }
            hir::ExprKind::Break(Some(value)) => {
                self.apply_expected_expr_type(value, expected);
            }
            hir::ExprKind::List(elements) => {
                if let TyKind::List(inner) | TyKind::Slice(inner) = expected {
                    if elements.is_empty() {
                        expr.ty.kind = expected.clone();
                    }
                    for element in elements {
                        self.apply_expected_expr_type(element, &inner.kind);
                    }
                }
            }
            hir::ExprKind::Dict(entries) => {
                if let TyKind::Dict(key_ty, value_ty) = expected {
                    if entries.is_empty() {
                        expr.ty.kind = expected.clone();
                    }
                    for (key, value) in entries {
                        self.apply_expected_expr_type(key, &key_ty.kind);
                        self.apply_expected_expr_type(value, &value_ty.kind);
                    }
                }
            }
            _ => {}
        }
    }

    /// Visit a FunctionExpression (closure): register parameters, visit
    /// body, infer return type, and set the expression's type to `Fn(…) → …`.
    fn visit_closure_expr(&mut self, decl: &mut hir::FunctionDeclaration, expr_ty: &mut Ty) {
        let enclosing = self.current_context();
        let closure_ctx = TypingContext::for_closure(decl, enclosing);
        self.push_context(closure_ctx);

        self.register_function_signature(decl);

        for param in &decl.parameters {
            self.type_table.insert(
                param.hir_id,
                TypeInfo {
                    ty: param.type_annotation.clone(),
                },
            );
        }

        self.return_type_stack.push(decl.return_type.kind.clone());
        self.observed_return_types.push(Vec::new());
        let error_checkpoint = self.errors.len();
        if !matches!(decl.return_type.kind, TyKind::Unknown)
            && let Some(expr) = &mut decl.body.expr
        {
            self.apply_expected_expr_type(expr, &decl.return_type.kind);
        }
        self.visit_block(&mut decl.body, &mut ());
        self.recheck_body_with_provisional_return(decl, error_checkpoint);

        // Check that the closure body type matches the declared return type.
        self.check_function_body_return(decl);

        let body_ret = decl
            .body
            .expr
            .as_ref()
            .map(|e| e.ty.kind.clone())
            .or_else(|| self.infer_return_type_from_observed())
            .unwrap_or_else(|| decl.return_type.kind.clone());
        let ret_ty = if matches!(decl.return_type.kind, TyKind::Unknown)
            || ty_contains_var(&decl.return_type.kind)
        {
            // When the declared return type is unknown or contains
            // unresolved type variables (from bidirectional inference),
            // use the inferred body return type instead.
            body_ret
        } else {
            decl.return_type.kind.clone()
        };

        // Write the resolved return type back so inlay hints can read it.
        if !decl.has_return_type {
            decl.return_type.kind = ret_ty.clone();
        }

        let param_tys: Vec<Ty> = decl
            .parameters
            .iter()
            .map(|p| p.type_annotation.clone())
            .collect();
        let fn_ty = Self::make_fn_ty(param_tys, ret_ty);
        expr_ty.kind = fn_ty.clone();

        // Update the type table for the declaration so that inlay hints
        // and hover can retrieve the fully-inferred closure signature.
        self.type_table.insert(
            decl.hir_id,
            TypeInfo {
                ty: Ty {
                    kind: fn_ty,
                    ..Ty::default()
                },
            },
        );

        self.observed_return_types.pop();
        self.return_type_stack.pop();
        self.pop_context();
    }

    /// Visit a top-level function declaration statement.
    fn visit_function_decl_stmt(&mut self, decl: &mut hir::FunctionDeclaration) {
        self.typecheck_function_decl(decl);
    }

    /// Common helper: register a function declaration, type-check its body,
    /// and infer return types.  Used by both top-level function declarations
    /// and impl block methods.
    fn typecheck_function_decl(&mut self, decl: &mut hir::FunctionDeclaration) {
        self.seed_dot_method_receiver_type(decl);
        let fn_ctx = TypingContext::for_function(decl);
        self.push_context(fn_ctx);
        self.register_function_signature(decl);

        for param in &decl.parameters {
            self.type_table.insert(
                param.hir_id,
                TypeInfo {
                    ty: param.type_annotation.clone(),
                },
            );
        }

        self.return_type_stack.push(decl.return_type.kind.clone());
        self.observed_return_types.push(Vec::new());
        let error_checkpoint = self.errors.len();
        if !matches!(decl.return_type.kind, TyKind::Unknown)
            && let Some(expr) = &mut decl.body.expr
        {
            self.apply_expected_expr_type(expr, &decl.return_type.kind);
        }
        self.visit_block(&mut decl.body, &mut ());
        self.recheck_body_with_provisional_return(decl, error_checkpoint);
        self.check_function_body_return(decl);

        if matches!(decl.return_type.kind, TyKind::Unknown) {
            let body_ty = decl
                .body
                .expr
                .as_ref()
                .map(|e| e.ty.kind.clone())
                .or_else(|| self.infer_return_type_from_observed())
                .unwrap_or_else(|| decl.return_type.kind.clone());

            // Write the inferred return type back so inlay hints can read it.
            if !decl.has_return_type {
                decl.return_type.kind = body_ty.clone();
            }

            let signature_ret_ty = if decl.has_return_type {
                decl.return_type.kind.clone()
            } else {
                body_ty.clone()
            };

            let param_tys: Vec<Ty> = decl
                .parameters
                .iter()
                .map(|p| p.type_annotation.clone())
                .collect();
            let fn_ty = Self::make_fn_ty(param_tys, signature_ret_ty);
            self.type_table.insert(
                decl.hir_id,
                TypeInfo {
                    ty: Ty {
                        kind: fn_ty,
                        ..Ty::default()
                    },
                },
            );
        }

        self.observed_return_types.pop();
        self.return_type_stack.pop();
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
                // Contextual inference for unannotated loop accumulators:
                // when a `let accumulator$$` binding has no explicit type
                // annotation, try to adopt the iterator's item type so that
                // bare numeric literals like `0` pick up the right numeric
                // type without requiring an explicit cast (e.g. `with sum = 0`
                // infers `sum: isize` when the iterator yields `isize` items).
                let is_accumulator = matches!(
                    &pat.kind,
                    hir::PatKind::Identifier(_, ident) if ident.as_str() == "accumulator$$"
                );
                let is_iterator = matches!(
                    &pat.kind,
                    hir::PatKind::Identifier(_, ident) if ident.as_str() == "iterator$$"
                );

                if is_accumulator && matches!(ty.kind, TyKind::Unknown) {
                    // Consume the pending iterator item type (set by the iterator$$
                    // stmt immediately before this one).  Only apply it when the
                    // item type is a numeric primitive AND the initial value is a
                    // bare integer or float literal (not a cast, call, etc.) so
                    // that explicit initialiser types are always respected.
                    if let Some(item_ty) = self.pending_accumulator_type.take() {
                        if matches!(&item_ty, TyKind::Primitive(prim) if prim.is_numeric())
                            && matches!(
                                &expr.kind,
                                hir::ExprKind::Literal(lit)
                                    if matches!(lit.as_ref(),
                                        Literal::Integer(_)
                                        | Literal::UnsignedInteger(_)
                                        | Literal::Float(_))
                            )
                        {
                            ty.kind = item_ty;
                        }
                    }
                } else if !is_iterator {
                    // Clear pending accumulator type for any stmt that is neither
                    // the iterator$$ nor the accumulator$$, preventing leakage to
                    // unrelated bindings.
                    self.pending_accumulator_type = None;
                }

                if !matches!(ty.kind, TyKind::Unknown) {
                    self.apply_expected_expr_type(expr, &ty.kind);
                }
                self.visit_expr(expr, ctx);
                self.record_iterator_binding_item_type(pat, expr);

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
                if !matches!(ty.kind, TyKind::Unknown) {
                    self.apply_expected_expr_type(expr, &ty.kind);
                }
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
                self.visit_function_decl_stmt(decl);
            }
            hir::StmtKind::Return(expr) => {
                if let Some(e) = expr.as_mut() {
                    if let Some(expected_ret) = self.return_type_stack.last().cloned() {
                        self.apply_expected_expr_type(e, &expected_ret);
                    }
                    self.visit_expr(e, ctx);
                }
                let span = stmt.span;
                self.check_return_type(expr.as_deref(), span);
            }
            hir::StmtKind::StructDeclaration(decl) => {
                self.register_struct_declaration(decl);
                // Walk the struct's const items.
                for const_item in &mut decl.consts {
                    self.visit_expr(&mut const_item.value, ctx);
                }
            }
            hir::StmtKind::EnumDeclaration(decl) => {
                self.register_enum_declaration(decl);
                // Walk the enum's const items and variant discriminants.
                for variant in &mut decl.variants {
                    if let Some(disc) = &mut variant.discriminant {
                        self.visit_expr(disc, ctx);
                    }
                }
                for const_item in &mut decl.consts {
                    self.visit_expr(&mut const_item.value, ctx);
                }
            }
            hir::StmtKind::ProtocolDeclaration(decl) => {
                self.register_protocol_declaration(decl);
                // Walk default method bodies.
                for method in &mut decl.methods {
                    if let Some(body) = &mut method.body {
                        let fn_ctx = TypingContext::Permissive;
                        self.push_context(fn_ctx);
                        self.visit_block(body, ctx);
                        self.pop_context();
                    }
                }
                for const_item in &mut decl.consts {
                    self.visit_expr(&mut const_item.value, ctx);
                }
            }
            hir::StmtKind::ImplBlock(impl_block) => {
                self.check_impl_block(impl_block);
            }
            _ => walk_stmt(self, stmt, ctx),
        }
    }

    #[allow(clippy::too_many_lines)]
    fn visit_expr(&mut self, expr: &'hir mut hir::Expr, ctx: &mut Self::Context) {
        match &mut expr.kind {
            hir::ExprKind::Literal(lit) => {
                let ty_kind = match lit.as_ref() {
                    Literal::Integer(_) | Literal::UnsignedInteger(_) | Literal::Float(_)
                        if matches!(
                            expr.ty.kind,
                            TyKind::Primitive(prim) if prim.is_numeric()
                        ) =>
                    {
                        expr.ty.kind.clone()
                    }
                    _ => Self::type_of_literal(lit),
                };
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
                let op = *op;
                let hir_id = expr.hir_id;
                let span = expr.span;
                self.visit_binary_expr(op, lhs, rhs, hir_id, span, &mut expr.ty);
            }
            hir::ExprKind::Unary(op, operand) => {
                let op = *op;
                let hir_id = expr.hir_id;
                let span = expr.span;
                self.visit_unary_expr(op, operand, hir_id, span, &mut expr.ty);
            }
            hir::ExprKind::FunctionExpression(decl) => {
                self.visit_closure_expr(decl, &mut expr.ty);
            }
            hir::ExprKind::Call(call) | hir::ExprKind::TailCall(call) => {
                let hir_id = expr.hir_id;
                let span = expr.span;
                self.visit_call_expr(call, hir_id, span, &mut expr.ty);
            }
            hir::ExprKind::Path(path) => {
                // Try to resolve the type from the type table via the path's
                // resolution HirId.
                if let Some(hir_id) = path.res.hir_id()
                    && let Some(info) = self.type_table.get(&hir_id)
                {
                    expr.ty = info.ty.clone();
                } else {
                    // Only fall back to builtin signature lookup when the path
                    // does NOT resolve to a local binding (parameter, variable,
                    // upvar). Local bindings with unknown type should stay
                    // unknown rather than being shadowed by a builtin of the
                    // same name (e.g. a closure parameter named `f` should not
                    // resolve to the tagged-string `f` function).
                    let is_local = matches!(
                        path.res.binding_kind(),
                        hir::BindingKind::Param
                            | hir::BindingKind::Local
                            | hir::BindingKind::Upvar
                            | hir::BindingKind::Closure
                    );
                    if !is_local {
                        let name = path.join("::");
                        if let Some(fn_ty) = builtins::lookup(&name) {
                            expr.ty.kind = fn_ty;
                        }
                    }
                }
            }
            hir::ExprKind::IfElse(cond, then_block, else_clauses) => {
                self.visit_expr(cond, ctx);
                self.visit_block(then_block, ctx);

                let mut branch_tys = Vec::with_capacity(1 + else_clauses.len());
                branch_tys.push(
                    then_block
                        .expr
                        .as_ref()
                        .map(|e| e.ty.kind.clone())
                        .unwrap_or(TyKind::Unknown),
                );

                let mut has_final_else = false;
                for clause in else_clauses.iter_mut() {
                    if let Some(cond) = &mut clause.condition {
                        self.visit_expr(cond, ctx);
                    } else {
                        has_final_else = true;
                    }
                    self.visit_block(&mut clause.consequence, ctx);
                    branch_tys.push(
                        clause
                            .consequence
                            .expr
                            .as_ref()
                            .map(|e| e.ty.kind.clone())
                            .unwrap_or(TyKind::Unknown),
                    );
                }

                let if_ty = if has_final_else {
                    Self::common_completion_type(branch_tys.iter()).unwrap_or(TyKind::Unknown)
                } else {
                    TyKind::Unknown
                };
                self.assign_expr_type(expr, if_ty);
            }
            hir::ExprKind::Match(scrutinee, arms) => {
                self.visit_expr(scrutinee, ctx);
                let iterator_item_ty = self.iterator_next_item_type(scrutinee);
                let mut arm_tys = Vec::with_capacity(arms.len());
                for arm in arms.iter_mut() {
                    self.register_match_pat_bindings(&mut arm.pat, scrutinee);
                    if let Some(item_ty) = &iterator_item_ty {
                        self.register_iterator_next_pattern_bindings(&mut arm.pat, item_ty);
                    }
                    if let Some(guard) = &mut arm.guard {
                        self.visit_expr(guard, ctx);
                    }
                    self.visit_block(&mut arm.block, ctx);
                    arm_tys.push(
                        arm.block
                            .expr
                            .as_ref()
                            .map(|e| e.ty.kind.clone())
                            .unwrap_or(TyKind::Unknown),
                    );
                }
                let match_ty =
                    Self::common_completion_type(arm_tys.iter()).unwrap_or(TyKind::Unknown);
                self.assign_expr_type(expr, match_ty);
            }
            hir::ExprKind::Loop(block) => {
                self.visit_block(block, ctx);
                let loop_ty = block
                    .expr
                    .as_ref()
                    .map(|e| e.ty.kind.clone())
                    .unwrap_or(TyKind::Unknown);
                self.assign_expr_type(expr, loop_ty);
            }
            hir::ExprKind::Break(value) => {
                let break_ty = if let Some(value) = value {
                    self.visit_expr(value, ctx);
                    value.ty.kind.clone()
                } else {
                    TyKind::Primitive(PrimTy::Nil)
                };
                self.assign_expr_type(expr, break_ty);
            }
            hir::ExprKind::Block(block) => {
                self.visit_block(block, ctx);
                // Block type is the type of its trailing expression if any.
                if let Some(trailing) = &block.expr {
                    expr.ty = trailing.ty.clone();
                }
            }
            hir::ExprKind::FieldAccess(base, ident) => {
                self.visit_expr(base, ctx);
                let field_name = ident.as_str();
                let base_ty = base.ty.kind.clone();
                expr.ty.kind =
                    self.check_field_access(&base_ty, field_name, expr.hir_id, expr.span);
            }
            hir::ExprKind::IndexAccess(base, index) => {
                self.visit_expr(base, ctx);
                self.visit_expr(index, ctx);
                let result_ty = self.index_access_result_type(&base.ty.kind);
                self.assign_expr_type(expr, result_ty);
            }
            hir::ExprKind::Implements(inner_expr, _path) => {
                let hir_id = expr.hir_id;
                self.visit_implements_expr(inner_expr, hir_id, &mut expr.ty);
            }
            hir::ExprKind::Cast(..) | hir::ExprKind::TryCast(..) => {
                // Extract the inner data and check the cast; we reassemble
                // after because `check_cast`/`check_try_cast` need mutable
                // access to the inner expression and the outer expression.
                let is_try_cast = matches!(expr.kind, hir::ExprKind::TryCast(..));
                let (mut inner, target_ty) =
                    match std::mem::replace(&mut expr.kind, hir::ExprKind::Wildcard) {
                        hir::ExprKind::Cast(inner, ty) | hir::ExprKind::TryCast(inner, ty) => {
                            (*inner, *ty)
                        }
                        _ => unreachable!(),
                    };

                if is_try_cast {
                    self.check_try_cast(&mut inner, &target_ty, expr);
                } else {
                    self.check_cast(&mut inner, &target_ty, expr);
                }

                // Restore the original expression kind.
                expr.kind = if is_try_cast {
                    hir::ExprKind::TryCast(Box::new(inner), Box::new(target_ty))
                } else {
                    hir::ExprKind::Cast(Box::new(inner), Box::new(target_ty))
                };
            }
            hir::ExprKind::List(elements) => {
                for elem in elements.iter_mut() {
                    self.visit_expr(elem, ctx);
                }
                let ty_kind = if elements.is_empty()
                    && matches!(expr.ty.kind, TyKind::List(_) | TyKind::Slice(_))
                {
                    expr.ty.kind.clone()
                } else {
                    self.infer_list_type(elements)
                };
                self.assign_expr_type(expr, ty_kind);
            }
            hir::ExprKind::Dict(entries) => {
                for (key, val) in entries.iter_mut() {
                    self.visit_expr(key, ctx);
                    self.visit_expr(val, ctx);
                }
                let ty_kind = if entries.is_empty() && matches!(expr.ty.kind, TyKind::Dict(..)) {
                    expr.ty.kind.clone()
                } else {
                    Self::infer_dict_type(entries)
                };
                self.assign_expr_type(expr, ty_kind);
            }
            hir::ExprKind::TaggedString {
                tag,
                parts: _,
                exprs,
            } => {
                self.visit_expr(tag, ctx);
                for e in exprs.iter_mut() {
                    self.visit_expr(e, ctx);
                }
                // The result type of a tagged string is the return type of
                // the tag function (e.g. `re"…"` → Regex, `f"…"` → String).
                let result_ty = match &tag.ty.kind {
                    TyKind::Fn(_, ret) => ret.kind.clone(),
                    _ => TyKind::Unknown,
                };
                self.assign_expr_type(expr, result_ty);
            }
            _ => {
                walk_expr(self, expr, ctx);
            }
        }
    }
}

/// Extract a human-readable name from a callee expression.
fn callee_name_str(callee: &hir::Expr) -> Option<String> {
    match &callee.kind {
        hir::ExprKind::Path(path) => Some(path.join("::")),
        _ => None,
    }
}

/// Compare two [`TyKind`] values structurally, ignoring metadata
/// (`res`, `span`) inside nested [`Ty`] nodes.
///
/// This is used for type compatibility checking where two types produced
/// from different source locations (e.g. an annotation vs an inferred
/// expression type) need to be compared purely by shape.
fn ty_kinds_compatible(a: &TyKind, b: &TyKind) -> bool {
    ty_kinds_assignable(a, b) || ty_kinds_assignable(b, a)
}

/// Check whether `actual` can be used where `expected` is required.
///
/// This is directional:
/// - `List<unknown>` accepts `List<i64>`
/// - `List<i64>` does *not* accept `List<unknown>`
fn ty_kinds_assignable(expected: &TyKind, actual: &TyKind) -> bool {
    match (expected, actual) {
        // Type variables are placeholders during generic checking and should
        // not force mismatches.
        (TyKind::Var(_), _) | (_, TyKind::Var(_)) => true,
        (TyKind::Unknown, _) => true,
        (_, TyKind::Unknown) => false,
        (TyKind::Never, TyKind::Never) => true,
        (TyKind::Primitive(expected), TyKind::Primitive(actual)) => expected == actual,
        (TyKind::Fn(expected_params, expected_ret), TyKind::Fn(actual_params, actual_ret)) => {
            expected_params.len() == actual_params.len()
                && expected_params
                    .iter()
                    .zip(actual_params.iter())
                    .all(|(expected, actual)| ty_kinds_assignable(&expected.kind, &actual.kind))
                && ty_kinds_assignable(&expected_ret.kind, &actual_ret.kind)
        }
        (TyKind::List(expected), TyKind::List(actual))
        | (TyKind::Slice(expected), TyKind::Slice(actual))
        | (TyKind::List(expected), TyKind::Slice(actual))
        | (TyKind::Slice(expected), TyKind::List(actual)) => {
            ty_kinds_assignable(&expected.kind, &actual.kind)
        }
        (TyKind::Dict(expected_key, expected_val), TyKind::Dict(actual_key, actual_val)) => {
            ty_kinds_assignable(&expected_key.kind, &actual_key.kind)
                && ty_kinds_assignable(&expected_val.kind, &actual_val.kind)
        }
        (TyKind::Path(expected_path, expected_args), TyKind::Path(actual_path, actual_args)) => {
            expected_path == actual_path
                && expected_args.len() == actual_args.len()
                && expected_args
                    .iter()
                    .zip(actual_args.iter())
                    .all(|(expected, actual)| ty_kinds_assignable(&expected.kind, &actual.kind))
        }
        // Remaining bare builtin-generic paths act like unconstrained
        // annotations on the expected side only.
        (TyKind::Path(path, _), TyKind::List(_) | TyKind::Slice(_))
            if path.to_string() == "List" =>
        {
            true
        }
        (TyKind::Path(path, _), TyKind::Dict(..)) if path.to_string() == "Dict" => true,
        // If a builtin collection is still represented as a bare path on the
        // actual side, treat it as carrying unknown inner types.
        (TyKind::List(expected), TyKind::Path(path, type_args))
        | (TyKind::Slice(expected), TyKind::Path(path, type_args))
            if type_args.is_empty() && path.to_string() == "List" =>
        {
            ty_kinds_assignable(&expected.kind, &TyKind::Unknown)
        }
        (TyKind::Dict(expected_key, expected_val), TyKind::Path(path, type_args))
            if type_args.is_empty() && path.to_string() == "Dict" =>
        {
            ty_kinds_assignable(&expected_key.kind, &TyKind::Unknown)
                && ty_kinds_assignable(&expected_val.kind, &TyKind::Unknown)
        }
        (TyKind::Union(expected_members), TyKind::Union(actual_members)) => {
            actual_members.iter().all(|actual_member| {
                expected_members.iter().any(|expected_member| {
                    ty_kinds_assignable(&expected_member.kind, &actual_member.kind)
                })
            })
        }
        // A concrete type is assignable to a union when at least one member
        // accepts it.
        (TyKind::Union(members), actual) => members
            .iter()
            .any(|member| ty_kinds_assignable(&member.kind, actual)),
        // A union value is assignable to a concrete type only if every member
        // is assignable to that target.
        (expected, TyKind::Union(members)) => members
            .iter()
            .all(|member| ty_kinds_assignable(expected, &member.kind)),
        _ => false,
    }
}

// ── Generic type variable instantiation ──────────────────────────────

/// Check whether a [`TyKind`] contains any `Var` (unresolved type
/// variable) nodes.
fn ty_contains_var(ty: &TyKind) -> bool {
    match ty {
        TyKind::Var(_) => true,
        TyKind::List(inner) | TyKind::Slice(inner) => ty_contains_var(&inner.kind),
        TyKind::Dict(k, v) => ty_contains_var(&k.kind) || ty_contains_var(&v.kind),
        TyKind::Fn(params, ret) => {
            params.iter().any(|p| ty_contains_var(&p.kind)) || ty_contains_var(&ret.kind)
        }
        TyKind::Path(_, type_args) => type_args.iter().any(|arg| ty_contains_var(&arg.kind)),
        TyKind::Union(tys) => tys.iter().any(|t| ty_contains_var(&t.kind)),
        _ => false,
    }
}

/// Structurally match a concrete type against a pattern that may contain
/// `Var` placeholders, and collect the bindings into `bindings`.
///
/// For example, matching `List(i64)` against `List(Var(T))` adds
/// `T → i64`.  Conflicting bindings (the same `Var` mapped to different
/// concrete types) are silently ignored (first-wins).
fn collect_type_var_bindings(
    concrete: &TyKind,
    pattern: &TyKind,
    bindings: &mut HashMap<TypeVarId, TyKind>,
) {
    match (concrete, pattern) {
        // Skip Unknown types — they carry no information.
        (TyKind::Unknown, _) | (_, TyKind::Unknown) => {}

        // The core case: a type variable in the pattern position.
        (_, TyKind::Var(id)) => {
            bindings.entry(*id).or_insert_with(|| concrete.clone());
        }
        (TyKind::List(c), TyKind::List(p))
        | (TyKind::Slice(c), TyKind::Slice(p))
        | (TyKind::List(c), TyKind::Slice(p))
        | (TyKind::Slice(c), TyKind::List(p)) => {
            collect_type_var_bindings(&c.kind, &p.kind, bindings);
        }
        (TyKind::Dict(ck, cv), TyKind::Dict(pk, pv)) => {
            collect_type_var_bindings(&ck.kind, &pk.kind, bindings);
            collect_type_var_bindings(&cv.kind, &pv.kind, bindings);
        }
        (TyKind::Fn(c_params, c_ret), TyKind::Fn(p_params, p_ret)) => {
            for (cp, pp) in c_params.iter().zip(p_params.iter()) {
                collect_type_var_bindings(&cp.kind, &pp.kind, bindings);
            }
            collect_type_var_bindings(&c_ret.kind, &p_ret.kind, bindings);
        }
        (TyKind::Path(concrete_path, concrete_args), TyKind::Path(pattern_path, pattern_args))
            if concrete_path == pattern_path && concrete_args.len() == pattern_args.len() =>
        {
            for (concrete_arg, pattern_arg) in concrete_args.iter().zip(pattern_args.iter()) {
                collect_type_var_bindings(&concrete_arg.kind, &pattern_arg.kind, bindings);
            }
        }
        // Bare `List` path (unparameterised) vs List/Slice(Var(T)): no binding.
        // Bare `Dict` path vs Dict(Var, Var): no binding.
        _ => {}
    }
}

/// Substitute all `Var` occurrences in `ty` with their bindings from
/// the map, recursing into compound types.  Unbound variables are left
/// unchanged.
fn substitute_type_vars(ty: &TyKind, bindings: &HashMap<TypeVarId, TyKind>) -> TyKind {
    match ty {
        TyKind::Var(id) => bindings.get(id).cloned().unwrap_or_else(|| ty.clone()),
        TyKind::List(inner) => TyKind::List(Box::new(Ty {
            kind: substitute_type_vars(&inner.kind, bindings),
            ..Ty::default()
        })),
        TyKind::Slice(inner) => TyKind::Slice(Box::new(Ty {
            kind: substitute_type_vars(&inner.kind, bindings),
            ..Ty::default()
        })),
        TyKind::Dict(k, v) => TyKind::Dict(
            Box::new(Ty {
                kind: substitute_type_vars(&k.kind, bindings),
                ..Ty::default()
            }),
            Box::new(Ty {
                kind: substitute_type_vars(&v.kind, bindings),
                ..Ty::default()
            }),
        ),
        TyKind::Fn(params, ret) => {
            let params = params
                .iter()
                .map(|p| Ty {
                    kind: substitute_type_vars(&p.kind, bindings),
                    ..Ty::default()
                })
                .collect();
            let ret = Box::new(Ty {
                kind: substitute_type_vars(&ret.kind, bindings),
                ..Ty::default()
            });
            TyKind::Fn(params, ret)
        }
        TyKind::Union(tys) => TyKind::Union(
            tys.iter()
                .map(|t| Ty {
                    kind: substitute_type_vars(&t.kind, bindings),
                    ..Ty::default()
                })
                .collect(),
        ),
        TyKind::Path(path, type_args) => TyKind::Path(
            path.clone(),
            type_args
                .iter()
                .map(|arg| Ty {
                    kind: substitute_type_vars(&arg.kind, bindings),
                    ..Ty::default()
                })
                .collect(),
        ),
        _ => ty.clone(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tlang_ast::node::Ident;

    fn dummy_hir_id() -> tlang_span::HirId {
        tlang_span::HirId::new(1)
    }

    fn make_param(name: &str, ty_kind: TyKind) -> hir::FunctionParameter {
        hir::FunctionParameter {
            hir_id: dummy_hir_id(),
            name: Ident::new(name, tlang_span::Span::default()),
            type_annotation: Ty {
                kind: ty_kind,
                ..Ty::default()
            },
            has_type_annotation: false,
            span: tlang_span::Span::default(),
        }
    }

    fn make_fn_decl(params: Vec<hir::FunctionParameter>, ret: TyKind) -> hir::FunctionDeclaration {
        let dummy_name = hir::Expr {
            hir_id: dummy_hir_id(),
            kind: hir::ExprKind::Wildcard,
            ty: Ty::default(),
            span: tlang_span::Span::default(),
        };
        hir::FunctionDeclaration {
            hir_id: dummy_hir_id(),
            visibility: tlang_ast::node::Visibility::Private,
            name: dummy_name,
            owner_type_params: Vec::new(),
            type_params: Vec::new(),
            parameters: params,
            params_span: tlang_span::Span::default(),
            return_hint_spans: Vec::new(),
            return_hint_arm_indices: Vec::new(),
            return_type: Ty {
                kind: ret,
                ..Ty::default()
            },
            has_return_type: false,
            body: hir::Block::new(dummy_hir_id(), vec![], None, tlang_span::Span::default()),
            span: tlang_span::Span::default(),
        }
    }

    #[test]
    fn apply_expected_closure_types_fills_unknown_params() {
        let mut decl = make_fn_decl(
            vec![
                make_param("x", TyKind::Unknown),
                make_param("y", TyKind::Unknown),
            ],
            TyKind::Unknown,
        );

        let expected = TyKind::Fn(
            vec![
                Ty {
                    kind: TyKind::Primitive(PrimTy::I64),
                    ..Ty::default()
                },
                Ty {
                    kind: TyKind::Primitive(PrimTy::Bool),
                    ..Ty::default()
                },
            ],
            Box::new(Ty {
                kind: TyKind::Primitive(PrimTy::String),
                ..Ty::default()
            }),
        );

        TypeChecker::apply_expected_closure_types(&mut decl, &expected);

        assert_eq!(
            decl.parameters[0].type_annotation.kind,
            TyKind::Primitive(PrimTy::I64)
        );
        assert_eq!(
            decl.parameters[1].type_annotation.kind,
            TyKind::Primitive(PrimTy::Bool)
        );
        assert_eq!(decl.return_type.kind, TyKind::Primitive(PrimTy::String));
    }

    #[test]
    fn apply_expected_closure_types_preserves_annotated_params() {
        let mut decl = make_fn_decl(
            vec![
                make_param("x", TyKind::Primitive(PrimTy::F64)),
                make_param("y", TyKind::Unknown),
            ],
            TyKind::Primitive(PrimTy::I64),
        );

        let expected = TyKind::Fn(
            vec![
                Ty {
                    kind: TyKind::Primitive(PrimTy::I64),
                    ..Ty::default()
                },
                Ty {
                    kind: TyKind::Primitive(PrimTy::Bool),
                    ..Ty::default()
                },
            ],
            Box::new(Ty {
                kind: TyKind::Primitive(PrimTy::String),
                ..Ty::default()
            }),
        );

        TypeChecker::apply_expected_closure_types(&mut decl, &expected);

        // Annotated param should NOT be overridden.
        assert_eq!(
            decl.parameters[0].type_annotation.kind,
            TyKind::Primitive(PrimTy::F64)
        );
        // Unannotated param should be filled.
        assert_eq!(
            decl.parameters[1].type_annotation.kind,
            TyKind::Primitive(PrimTy::Bool)
        );
        // Annotated return type should NOT be overridden.
        assert_eq!(decl.return_type.kind, TyKind::Primitive(PrimTy::I64));
    }

    #[test]
    fn apply_expected_closure_types_noop_for_non_fn() {
        let mut decl = make_fn_decl(vec![make_param("x", TyKind::Unknown)], TyKind::Unknown);

        // When expected is not a Fn type, nothing should change.
        TypeChecker::apply_expected_closure_types(&mut decl, &TyKind::Primitive(PrimTy::I64));

        assert_eq!(decl.parameters[0].type_annotation.kind, TyKind::Unknown);
        assert_eq!(decl.return_type.kind, TyKind::Unknown);
    }

    #[test]
    fn collect_bindings_from_slice() {
        use tlang_span::TypeVarIdAllocator;

        let mut alloc = TypeVarIdAllocator::new(1);
        let t = alloc.next_id();

        let concrete = TyKind::Slice(Box::new(Ty {
            kind: TyKind::Primitive(PrimTy::I64),
            ..Ty::default()
        }));
        let pattern = TyKind::Slice(Box::new(Ty {
            kind: TyKind::Var(t),
            ..Ty::default()
        }));

        let mut bindings = HashMap::new();
        collect_type_var_bindings(&concrete, &pattern, &mut bindings);

        assert_eq!(bindings.len(), 1);
        assert_eq!(bindings[&t], TyKind::Primitive(PrimTy::I64));
    }

    #[test]
    fn substitute_vars_in_return_type() {
        use tlang_span::TypeVarIdAllocator;

        let mut alloc = TypeVarIdAllocator::new(1);
        let t = alloc.next_id();
        let u = alloc.next_id();

        let mut bindings = HashMap::new();
        bindings.insert(t, TyKind::Primitive(PrimTy::I64));
        bindings.insert(u, TyKind::Primitive(PrimTy::F64));

        // Slice(Var(U)) → Slice(f64)
        let ty = TyKind::Slice(Box::new(Ty {
            kind: TyKind::Var(u),
            ..Ty::default()
        }));
        let result = substitute_type_vars(&ty, &bindings);
        assert_eq!(
            result,
            TyKind::Slice(Box::new(Ty {
                kind: TyKind::Primitive(PrimTy::F64),
                ..Ty::default()
            }))
        );
    }

    #[test]
    fn collect_bindings_from_fn_type() {
        use tlang_span::TypeVarIdAllocator;

        let mut alloc = TypeVarIdAllocator::new(1);
        let t = alloc.next_id();
        let u = alloc.next_id();

        // concrete: fn(i64) -> f64
        let concrete = TyKind::Fn(
            vec![Ty {
                kind: TyKind::Primitive(PrimTy::I64),
                ..Ty::default()
            }],
            Box::new(Ty {
                kind: TyKind::Primitive(PrimTy::F64),
                ..Ty::default()
            }),
        );
        // pattern: fn(Var(T)) -> Var(U)
        let pattern = TyKind::Fn(
            vec![Ty {
                kind: TyKind::Var(t),
                ..Ty::default()
            }],
            Box::new(Ty {
                kind: TyKind::Var(u),
                ..Ty::default()
            }),
        );

        let mut bindings = HashMap::new();
        collect_type_var_bindings(&concrete, &pattern, &mut bindings);

        assert_eq!(bindings.len(), 2);
        assert_eq!(bindings[&t], TyKind::Primitive(PrimTy::I64));
        assert_eq!(bindings[&u], TyKind::Primitive(PrimTy::F64));
    }

    #[test]
    fn unbound_vars_remain_unchanged() {
        use tlang_span::TypeVarIdAllocator;

        let mut alloc = TypeVarIdAllocator::new(1);
        let t = alloc.next_id();
        let u = alloc.next_id();

        let mut bindings = HashMap::new();
        bindings.insert(t, TyKind::Primitive(PrimTy::I64));
        // u is intentionally NOT in bindings

        let ty = TyKind::Var(u);
        let result = substitute_type_vars(&ty, &bindings);
        assert_eq!(result, TyKind::Var(u));
    }

    fn make_path_expr(segments: &[&str]) -> hir::Expr {
        hir::Expr {
            hir_id: dummy_hir_id(),
            kind: hir::ExprKind::Path(Box::new(hir::Path::new(
                segments
                    .iter()
                    .map(|segment| hir::PathSegment::from_str(segment, tlang_span::Span::default()))
                    .collect(),
                tlang_span::Span::default(),
            ))),
            ty: Ty::default(),
            span: tlang_span::Span::default(),
        }
    }

    #[test]
    fn protocol_dispatch_parts_requires_known_protocol_method() {
        let mut checker = TypeChecker::new();
        checker.type_table.insert_protocol_info(ProtocolInfo {
            name: Ident::new("Functor", tlang_span::Span::default()),
            type_param_var_ids: Vec::new(),
            methods: vec![ProtocolMethodInfo {
                name: Ident::new("map", tlang_span::Span::default()),
                param_tys: Vec::new(),
                return_ty: Ty::unknown(),
                has_default_body: false,
            }],
            constraints: Vec::new(),
            associated_types: Vec::new(),
        });

        let functor_map = make_path_expr(&["Functor", "map"]);
        let option_map = make_path_expr(&["Option", "map"]);
        let functor_missing = make_path_expr(&["Functor", "missing"]);

        assert_eq!(
            checker.protocol_dispatch_parts(functor_map.path().unwrap()),
            Some(("Functor".to_string(), "map".to_string()))
        );
        assert!(
            checker
                .protocol_dispatch_parts(option_map.path().unwrap())
                .is_none()
        );
        assert!(
            checker
                .protocol_dispatch_parts(functor_missing.path().unwrap())
                .is_none()
        );
    }

    #[test]
    fn visit_dispatch_receiver_arg_requires_known_protocol_dispatch_site() {
        let mut checker = TypeChecker::new();
        checker.type_table.insert_protocol_info(ProtocolInfo {
            name: Ident::new("Functor", tlang_span::Span::default()),
            type_param_var_ids: Vec::new(),
            methods: vec![ProtocolMethodInfo {
                name: Ident::new("map", tlang_span::Span::default()),
                param_tys: Vec::new(),
                return_ty: Ty::unknown(),
                has_default_body: false,
            }],
            constraints: Vec::new(),
            associated_types: Vec::new(),
        });

        let mut protocol_call = hir::CallExpression {
            hir_id: dummy_hir_id(),
            callee: make_path_expr(&["Functor", "map"]),
            arguments: vec![make_path_expr(&["value"])],
        };
        let mut non_protocol_call = hir::CallExpression {
            hir_id: dummy_hir_id(),
            callee: make_path_expr(&["Option", "map"]),
            arguments: vec![make_path_expr(&["value"])],
        };

        assert!(checker.visit_dispatch_receiver_arg(&mut protocol_call));
        assert!(!checker.visit_dispatch_receiver_arg(&mut non_protocol_call));
    }
}
