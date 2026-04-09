use std::collections::HashSet;

use tlang_ast::node::UnaryOp;
use tlang_ast::token::Literal;
use tlang_hir::Visitor;
use tlang_hir::visit::{walk_block, walk_expr, walk_module, walk_stmt};
use tlang_hir::{self as hir, BinaryOpKind, PrimTy, Ty, TyKind};
use tlang_hir_opt::hir_opt::{HirOptContext, HirOptError, HirPass};

use crate::builtins;
use crate::type_table::{
    EnumInfo, ImplInfo, ProtocolInfo, ProtocolMethodInfo, StructInfo, VariantInfo,
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

        if declared_ret != body_ty {
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

                // Check each argument type.
                // For variadic builtins, validate every argument against the
                // (single) parameter type; for normal calls, zip 1:1.
                if is_variadic {
                    if let Some(param_ty) = param_tys.first() {
                        for (i, arg) in call.arguments.iter().enumerate() {
                            if matches!(param_ty.kind, TyKind::Unknown)
                                || matches!(arg.ty.kind, TyKind::Unknown)
                            {
                                continue;
                            }
                            if arg.ty.kind != param_ty.kind {
                                let param_name = callee_name
                                    .as_deref()
                                    .map(|n| format!("arg{i} of `{n}`"))
                                    .unwrap_or_else(|| format!("arg{i}"));
                                self.errors.push(TypeError::ArgumentTypeMismatch {
                                    param_name,
                                    expected: param_ty.kind.to_string(),
                                    actual: arg.ty.kind.to_string(),
                                    span: arg.span,
                                });
                            }
                        }
                    }
                } else {
                    for (i, (arg, param_ty)) in
                        call.arguments.iter().zip(param_tys.iter()).enumerate()
                    {
                        if matches!(param_ty.kind, TyKind::Unknown)
                            || matches!(arg.ty.kind, TyKind::Unknown)
                        {
                            continue;
                        }
                        if arg.ty.kind != param_ty.kind {
                            let param_name = callee_name
                                .as_deref()
                                .map(|n| format!("arg{i} of `{n}`"))
                                .unwrap_or_else(|| format!("arg{i}"));
                            self.errors.push(TypeError::ArgumentTypeMismatch {
                                param_name,
                                expected: param_ty.kind.to_string(),
                                actual: arg.ty.kind.to_string(),
                                span: arg.span,
                            });
                        }
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

        if expected != actual {
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
        self.type_table.insert_protocol_info(ProtocolInfo {
            name: decl.name,
            methods,
            constraints,
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

            // Check constraint protocols.
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
                    self.type_table.insert_impl_info(ImplInfo {
                        protocol_name: impl_block.protocol_name.join("::"),
                        target_type_name: impl_block.target_type.join("::"),
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
    fn visit_call_expr(
        &mut self,
        call: &mut hir::CallExpression,
        expr_hir_id: tlang_span::HirId,
        expr_span: tlang_span::Span,
        expr_ty: &mut Ty,
    ) {
        for arg in &mut call.arguments {
            self.visit_expr(arg, &mut ());
        }
        self.visit_expr(&mut call.callee, &mut ());

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
        let ret_ty = if matches!(decl.return_type.kind, TyKind::Unknown) {
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
                self.visit_expr(inner_expr, ctx);
                let result_ty = TyKind::Primitive(PrimTy::Bool);
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
