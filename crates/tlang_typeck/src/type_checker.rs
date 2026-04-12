use std::collections::{HashMap, HashSet};

use tlang_ast::node::UnaryOp;
use tlang_ast::token::Literal;
use tlang_hir::Visitor;
use tlang_hir::visit::{walk_block, walk_expr, walk_module, walk_stmt};
use tlang_hir::{self as hir, BinaryOpKind, PrimTy, Ty, TyKind};
use tlang_hir_opt::hir_opt::{HirOptContext, HirOptError, HirPass};
use tlang_span::TypeVarId;

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
    /// Dot-method names collected from the module's function declarations
    /// (e.g. `"Animal.greet"` from `fn Animal.greet(self)`).  Pre-scanned
    /// once at the start of `check_module` for `apply` conflict detection.
    dot_methods: HashSet<String>,
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

    // ── Builtin collection types ─────────────────────────────────────

    /// Create the canonical `TyKind` for a builtin collection type name
    /// (`"List"` or `"Dict"`).  Delegates to `builtin_types::lookup`.
    fn builtin_type_path(name: &str) -> TyKind {
        builtin_types::lookup(name)
            .unwrap_or_else(|| panic!("unknown builtin collection type: {name}"))
    }

    /// Infer `Slice(elem_ty)` from a list literal's elements.
    ///
    /// When all elements agree on a single known type, the list is typed as
    /// `Slice(that_type)`.  Otherwise (empty list, mixed types, or unknown
    /// elements) falls back to the bare `List` path.
    fn infer_list_type(elements: &[hir::Expr]) -> TyKind {
        let common = Self::common_element_type(elements.iter().map(|e| &e.ty.kind));
        match common {
            Some(elem_kind) => TyKind::Slice(Box::new(Ty {
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
        if types.all(|t| matches!(t, TyKind::Unknown) || t == first) {
            Some(first.clone())
        } else {
            None
        }
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

        if !ty_kinds_compatible(&declared_ty.kind, expr_ty) {
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

        if !ty_kinds_compatible(declared_ret, body_ty) {
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
                return match binding_kind {
                    hir::BindingKind::Struct => TyKind::Path((**path).clone()),
                    hir::BindingKind::Variant => {
                        if let Some(hir_id) = path.res.hir_id()
                            && let Some(info) = self.type_table.get(&hir_id)
                            && let TyKind::Fn(_, ret) = &info.ty.kind
                        {
                            ret.kind.clone()
                        } else {
                            unreachable!(
                                "enum variant constructors must have a registered function signature"
                            )
                        }
                    }
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
        let fn_ty = self.resolve_callee_type(&call.callee);

        match fn_ty {
            Some(TyKind::Fn(ref param_tys, ref ret_ty)) => {
                let callee_name = callee_name_str(&call.callee);
                let is_variadic = callee_name.as_deref().is_some_and(builtins::is_variadic);

                // Check argument count (skip for variadic builtins).
                if !is_variadic && param_tys.len() != call.arguments.len() {
                    self.errors.push(TypeError::ArgumentCountMismatch {
                        expected: param_tys.len(),
                        actual: call.arguments.len(),
                        span,
                    });
                    return ret_ty.kind.clone();
                }

                self.check_argument_types(call, param_tys, &callee_name, is_variadic);

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
    ) {
        if is_variadic {
            if let Some(param_ty) = param_tys.first() {
                for (i, arg) in call.arguments.iter().enumerate() {
                    self.check_single_arg(arg, param_ty, i, callee_name);
                }
            }
        } else {
            for (i, (arg, param_ty)) in call.arguments.iter().zip(param_tys.iter()).enumerate() {
                self.check_single_arg(arg, param_ty, i, callee_name);
            }
        }
    }

    fn check_single_arg(
        &mut self,
        arg: &hir::Expr,
        param_ty: &Ty,
        index: usize,
        callee_name: &Option<String>,
    ) {
        if matches!(param_ty.kind, TyKind::Unknown) || matches!(arg.ty.kind, TyKind::Unknown) {
            return;
        }
        if !ty_kinds_compatible(&arg.ty.kind, &param_ty.kind) {
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
                // Then try builtin signatures.
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

        if !ty_kinds_compatible(expected, actual) {
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
                Some(prev) if prev == ty => {}
                Some(_) => return None, // conflicting types
            }
        }
        inferred.cloned()
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
                // element patterns get the element type and rest patterns keep
                // the full list type.
                let elem_ty = match binding_ty {
                    TyKind::Slice(inner) => Some(inner.kind.clone()),
                    _ => None,
                };

                for p in pats {
                    let is_rest = matches!(p.kind, hir::PatKind::Rest(_));
                    if is_rest {
                        // Rest patterns (`...xs`) keep the full list type.
                        self.register_pat_bindings(p, binding_ty);
                    } else {
                        let ty = elem_ty.as_ref().unwrap_or(binding_ty);
                        self.register_pat_bindings(p, ty);
                    }
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
        let struct_ty_kind = TyKind::Path(struct_path);

        // Store struct field metadata.
        let fields: Vec<(tlang_ast::node::Ident, Ty)> =
            decl.fields.iter().map(|f| (f.name, f.ty.clone())).collect();
        self.type_table.insert_struct_info(
            decl.hir_id,
            StructInfo {
                name: decl.name,
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
        let enum_ty_kind = TyKind::Path(enum_path);

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
                let param_tys: Vec<Ty> = variant.parameters.iter().map(|p| p.ty.clone()).collect();
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
            if self.dot_methods.contains(&dot_name) {
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
        struct_hir_id: &tlang_span::HirId,
        field_name: &str,
    ) -> Option<TyKind> {
        if let Some(info) = self.type_table.get_struct_info(struct_hir_id) {
            for (name, ty) in &info.fields {
                if name.as_str() == field_name {
                    return Some(ty.kind.clone());
                }
            }
        }
        None
    }

    /// Resolve the HirId of a struct/enum type from a TyKind::Path.
    fn resolve_type_hir_id(ty_kind: &TyKind) -> Option<tlang_span::HirId> {
        match ty_kind {
            TyKind::Path(path) => path.res.hir_id(),
            _ => None,
        }
    }

    /// Type-check a field access expression (`base.field`).
    /// Resolves the field's type from the base expression's struct type.
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
            if let Some(field_ty) = self.resolve_field_type(&hir_id, field_name) {
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
        TyKind::Unknown
    }

    // ── HIR walk entry point ─────────────────────────────────────────

    fn check_module(&mut self, module: &mut hir::Module) {
        // Pre-scan the module for impl blocks and dot-methods so that
        // validation is order-independent (an impl or dot-method declared
        // after the first use site is still visible).
        let mut dot_methods = HashSet::new();
        for stmt in &module.block.stmts {
            match &stmt.kind {
                hir::StmtKind::FunctionDeclaration(decl) => {
                    if matches!(&decl.name.kind, hir::ExprKind::FieldAccess(..)) {
                        dot_methods.insert(decl.name());
                    }
                }
                hir::StmtKind::ImplBlock(impl_block) => {
                    let is_blanket = !impl_block.type_params.is_empty();
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
                        target_type_name: impl_block.target_type.join("::"),
                        protocol_type_args: impl_block
                            .type_arguments
                            .iter()
                            .map(|ty| ty.kind.to_string())
                            .collect(),
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
            _ if ty_kinds_compatible(source_kind, target_kind) => {
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
        let callee_fn_ty = self.resolve_callee_type(&call.callee);

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
                    if !matches!(arg.kind, hir::ExprKind::FunctionExpression(_)) {
                        self.visit_expr(arg, &mut ());
                        if let Some(param_ty) = param_tys.get(i) {
                            collect_type_var_bindings(&arg.ty.kind, &param_ty.kind, &mut bindings);
                        }
                    }
                }

                // Pass 2: visit closure args with substituted expected types.
                for (i, arg) in call.arguments.iter_mut().enumerate() {
                    if let hir::ExprKind::FunctionExpression(decl) = &mut arg.kind {
                        if let Some(expected_arg_ty) = param_tys.get(i) {
                            let substituted =
                                substitute_type_vars(&expected_arg_ty.kind, &bindings);
                            Self::apply_expected_closure_types(decl, &substituted);
                        }
                        self.visit_expr(arg, &mut ());
                    }
                }
            }
        } else {
            // Simple path: visit all arguments in order with optional
            // bidirectional inference for closures.
            for (i, arg) in call.arguments.iter_mut().enumerate() {
                if let hir::ExprKind::FunctionExpression(decl) = &mut arg.kind
                    && let Some(TyKind::Fn(ref param_tys, _)) = callee_fn_ty
                    && let Some(expected_arg_ty) = param_tys.get(i)
                {
                    Self::apply_expected_closure_types(decl, &expected_arg_ty.kind);
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
        self.visit_block(&mut decl.body, &mut ());

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

        let param_tys: Vec<Ty> = decl
            .parameters
            .iter()
            .map(|p| p.type_annotation.clone())
            .collect();
        expr_ty.kind = Self::make_fn_ty(param_tys, ret_ty);

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
        self.visit_block(&mut decl.body, &mut ());
        self.check_function_body_return(decl);

        if matches!(decl.return_type.kind, TyKind::Unknown) {
            let body_ty = decl
                .body
                .expr
                .as_ref()
                .map(|e| e.ty.kind.clone())
                .or_else(|| self.infer_return_type_from_observed())
                .unwrap_or_else(|| decl.return_type.kind.clone());
            let param_tys: Vec<Ty> = decl
                .parameters
                .iter()
                .map(|p| p.type_annotation.clone())
                .collect();
            let fn_ty = Self::make_fn_ty(param_tys, body_ty);
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
                self.visit_function_decl_stmt(decl);
            }
            hir::StmtKind::Return(expr) => {
                if let Some(e) = expr.as_mut() {
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
                    // Try builtin signature lookup for unresolved paths.
                    let name = path.join("::");
                    if let Some(fn_ty) = builtins::lookup(&name) {
                        expr.ty.kind = fn_ty;
                    }
                }
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
                let ty_kind = Self::infer_list_type(elements);
                self.assign_expr_type(expr, ty_kind);
            }
            hir::ExprKind::Dict(entries) => {
                for (key, val) in entries.iter_mut() {
                    self.visit_expr(key, ctx);
                    self.visit_expr(val, ctx);
                }
                let ty_kind = Self::infer_dict_type(entries);
                self.assign_expr_type(expr, ty_kind);
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
    match (a, b) {
        (TyKind::Unknown, TyKind::Unknown) | (TyKind::Never, TyKind::Never) => true,
        (TyKind::Primitive(pa), TyKind::Primitive(pb)) => pa == pb,
        (TyKind::Fn(pa, ra), TyKind::Fn(pb, rb)) => {
            pa.len() == pb.len()
                && pa
                    .iter()
                    .zip(pb.iter())
                    .all(|(a, b)| ty_kinds_compatible(&a.kind, &b.kind))
                && ty_kinds_compatible(&ra.kind, &rb.kind)
        }
        (TyKind::Slice(a), TyKind::Slice(b)) => ty_kinds_compatible(&a.kind, &b.kind),
        (TyKind::Dict(ka, va), TyKind::Dict(kb, vb)) => {
            ty_kinds_compatible(&ka.kind, &kb.kind) && ty_kinds_compatible(&va.kind, &vb.kind)
        }
        (TyKind::Path(pa), TyKind::Path(pb)) => pa == pb,
        // A bare `List`/`Dict` path annotation is compatible with any
        // parameterised `Slice(_)`/`Dict(_, _)` — the annotation just doesn't
        // constrain the element type.
        (TyKind::Path(p), TyKind::Slice(_)) | (TyKind::Slice(_), TyKind::Path(p))
            if p.to_string() == "List" =>
        {
            true
        }
        (TyKind::Path(p), TyKind::Dict(..)) | (TyKind::Dict(..), TyKind::Path(p))
            if p.to_string() == "Dict" =>
        {
            true
        }
        (TyKind::Union(ua), TyKind::Union(ub)) => {
            ua.len() == ub.len()
                && ua
                    .iter()
                    .zip(ub.iter())
                    .all(|(a, b)| ty_kinds_compatible(&a.kind, &b.kind))
        }
        (TyKind::Var(a), TyKind::Var(b)) => a == b,
        // A type variable in either position is compatible with anything —
        // this is needed so that generic signatures do not spuriously trigger
        // type-mismatch errors during argument checking.
        (TyKind::Var(_), _) | (_, TyKind::Var(_)) => true,
        _ => false,
    }
}

// ── Generic type variable instantiation ──────────────────────────────

/// Check whether a [`TyKind`] contains any `Var` (unresolved type
/// variable) nodes.
fn ty_contains_var(ty: &TyKind) -> bool {
    match ty {
        TyKind::Var(_) => true,
        TyKind::Slice(inner) => ty_contains_var(&inner.kind),
        TyKind::Dict(k, v) => ty_contains_var(&k.kind) || ty_contains_var(&v.kind),
        TyKind::Fn(params, ret) => {
            params.iter().any(|p| ty_contains_var(&p.kind)) || ty_contains_var(&ret.kind)
        }
        TyKind::Union(tys) => tys.iter().any(|t| ty_contains_var(&t.kind)),
        _ => false,
    }
}

/// Structurally match a concrete type against a pattern that may contain
/// `Var` placeholders, and collect the bindings into `bindings`.
///
/// For example, matching `Slice(i64)` against `Slice(Var(T))` adds
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
        (TyKind::Slice(c), TyKind::Slice(p)) => {
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
        // Bare `List` path (unparameterised) vs Slice(Var(T)): no binding.
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
            type_params: Vec::new(),
            parameters: params,
            params_span: tlang_span::Span::default(),
            return_type: Ty {
                kind: ret,
                ..Ty::default()
            },
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
}
