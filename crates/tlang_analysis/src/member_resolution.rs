//! Centralised semantic member resolution for editor features.
//!
//! This module provides a single backend that hover, dot-completion,
//! signature help, and inlay hints can all consult for member resolution
//! on typed expressions.  It operates on typed HIR (plus the type table
//! and symbol index) and exposes protocol-agnostic queries.
//!
//! ## Coordinate system
//!
//! All public functions accept **0-based line** and **0-based UTF-16 column**
//! positions (matching the LSP / CodeMirror convention).  Internal helpers
//! handle conversion to byte offsets and the lexer's mixed coordinate system
//! so callers do not need to worry about coordinate mismatches.

use tlang_defs::DefKind;
use tlang_hir as hir;
use tlang_hir::TyKind;
use tlang_span::Span;
use tlang_typeck::builtin_fields;
use tlang_typeck::builtin_methods;

use crate::signature_help::{ParameterInformation, SignatureInformation};
use crate::symbol_index::SymbolIndex;
use crate::typed_hir::TypedHir;

// ── Public types ───────────────────────────────────────────────────────

/// The kind of a resolved member.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MemberKind {
    /// A method (user-defined or builtin).
    Method,
    /// A struct field.
    Field,
}

/// Information about a specific member resolved at a cursor position.
#[derive(Debug, Clone)]
pub struct ResolvedMember {
    /// The member name (e.g. `"replace_all"`).
    pub name: String,
    /// The type of the receiver expression.
    pub receiver_ty: TyKind,
    /// Whether this is a method or field.
    pub kind: MemberKind,
    /// Callable signature, if the member is a method.
    pub signature: Option<SignatureInformation>,
    /// The return type, if known.
    pub return_ty: Option<TyKind>,
    /// The source span where the member is defined (`None` for builtins).
    pub def_span: Option<Span>,
    /// Whether this is a builtin method (no user source location).
    pub builtin: bool,
}

/// A candidate member for dot-completion.
#[derive(Debug, Clone)]
pub struct MemberCandidate {
    /// The member name.
    pub name: String,
    /// Whether this is a method or field.
    pub kind: MemberKind,
    /// Callable signature, if the member is a method.
    pub signature: Option<SignatureInformation>,
    /// The return type, if known.
    pub return_ty: Option<TyKind>,
    /// The source span where the member is defined (`None` for builtins).
    pub def_span: Option<Span>,
    /// Whether this is a builtin method.
    pub builtin: bool,
}

// ── Public query API ───────────────────────────────────────────────────

/// Resolve the member under the cursor in a `receiver.member` expression.
///
/// Returns information about the member if the cursor is on a field-access
/// or method-access member name in typed HIR.
pub fn resolve_member_at_position(
    source: &str,
    typed_hir: &TypedHir,
    line: u32,
    utf16_col: u32,
) -> Option<ResolvedMember> {
    let offset = utf16_line_column_to_byte_offset(source, line, utf16_col);
    let (_base, ident, base_ty) = find_field_access_at_offset(&typed_hir.module.block, offset)?;

    let type_name = builtin_methods::type_name_from_kind(&base_ty)?;
    let member_name = ident.as_str();

    // 1. Try builtin methods first.
    if let Some(sig_ty) = builtin_methods::lookup(type_name, member_name) {
        let sig_ty = builtin_methods::substitute_receiver_type_vars(&base_ty, &sig_ty);
        let return_ty = extract_return_ty(&sig_ty);
        let signature = signature_from_builtin(type_name, member_name, &sig_ty);
        return Some(ResolvedMember {
            name: member_name.to_string(),
            receiver_ty: base_ty,
            kind: MemberKind::Method,
            signature: Some(signature),
            return_ty,
            def_span: None,
            builtin: true,
        });
    }

    // 2. Try user-defined methods from protocol impls.
    if let Some(info) = resolve_protocol_method(&typed_hir.type_table, type_name, member_name) {
        return Some(ResolvedMember {
            name: member_name.to_string(),
            receiver_ty: base_ty,
            kind: MemberKind::Method,
            signature: Some(info.0),
            return_ty: info.1,
            def_span: None,
            builtin: false,
        });
    }

    // 3. Try builtin fields.
    if let Some(field_ty) = builtin_fields::lookup(type_name, member_name) {
        return Some(ResolvedMember {
            name: member_name.to_string(),
            receiver_ty: base_ty,
            kind: MemberKind::Field,
            signature: None,
            return_ty: Some(field_ty),
            def_span: None,
            builtin: true,
        });
    }

    // 4. Try user-defined methods/fields from the HIR (struct methods, etc.).
    if let Some(info) = resolve_hir_member(&typed_hir.module.block, type_name, member_name) {
        return Some(ResolvedMember {
            name: member_name.to_string(),
            receiver_ty: base_ty,
            kind: info.kind,
            signature: info.signature,
            return_ty: info.return_ty,
            def_span: info.def_span,
            builtin: false,
        });
    }

    None
}

/// List all member candidates available for dot-completion at a position.
///
/// The position should be immediately after the dot in `receiver.`.  The
/// function finds the receiver expression, determines its type, and gathers
/// all available members (user-defined + protocol impls + builtins).
pub fn complete_members_at_position(
    source: &str,
    typed_hir: &TypedHir,
    symbol_index: Option<&SymbolIndex>,
    line: u32,
    utf16_col: u32,
) -> Vec<MemberCandidate> {
    let type_name = receiver_type_before_dot(source, typed_hir, line, utf16_col);
    let type_name = match type_name {
        Some(t) => t,
        None => return vec![],
    };

    complete_members_for_type(typed_hir, symbol_index, &type_name)
}

/// List all member candidates for a given type name.
///
/// This is a lower-level helper that can be called when the type name is
/// already known (e.g. from `type_at_definition`).
pub fn complete_members_for_type(
    typed_hir: &TypedHir,
    symbol_index: Option<&SymbolIndex>,
    type_name: &str,
) -> Vec<MemberCandidate> {
    let mut candidates = Vec::new();
    let mut seen = std::collections::HashSet::new();

    // 1. Builtin fields.
    for field in builtin_fields::fields_for(type_name) {
        if seen.insert(field.name.to_string()) {
            candidates.push(MemberCandidate {
                name: field.name.to_string(),
                kind: MemberKind::Field,
                signature: None,
                return_ty: Some(field.ty),
                def_span: None,
                builtin: true,
            });
        }
    }

    // 2. Builtin methods.
    for method in builtin_methods::methods_for(type_name) {
        if seen.insert(method.name.to_string()) {
            let return_ty = extract_return_ty(&method.signature);
            let signature = signature_from_builtin(type_name, method.name, &method.signature);
            candidates.push(MemberCandidate {
                name: method.name.to_string(),
                kind: MemberKind::Method,
                signature: Some(signature),
                return_ty,
                def_span: None,
                builtin: true,
            });
        }
    }

    // 3. Protocol impl methods.
    for impl_info in typed_hir.type_table.impls() {
        if impl_info.target_type_name != type_name {
            continue;
        }
        let Some(protocol) = typed_hir
            .type_table
            .get_protocol_info(&impl_info.protocol_name)
        else {
            continue;
        };
        for method in &protocol.methods {
            let name = method.name.to_string();
            if seen.insert(name.clone()) {
                let params: Vec<String> = method
                    .param_tys
                    .iter()
                    .skip(1) // skip self
                    .map(|ty| ty.kind.to_string())
                    .collect();
                let ret_str = method.return_ty.kind.to_string();
                let signature = SignatureInformation {
                    label: format!("{type_name}.{name}({}) -> {ret_str}", params.join(", ")),
                    parameters: params
                        .into_iter()
                        .map(|label| ParameterInformation { label })
                        .collect(),
                };
                candidates.push(MemberCandidate {
                    name,
                    kind: MemberKind::Method,
                    signature: Some(signature),
                    return_ty: Some(method.return_ty.kind.clone()),
                    def_span: None,
                    builtin: false,
                });
            }
        }
    }

    // 4. User-defined methods and fields from the symbol index.
    if let Some(index) = symbol_index {
        for item in index.collect_method_completions(type_name) {
            if seen.insert(item.label.clone()) {
                let kind = if item.kind == DefKind::StructField {
                    MemberKind::Field
                } else {
                    MemberKind::Method
                };
                candidates.push(MemberCandidate {
                    name: item.label,
                    kind,
                    signature: None,
                    return_ty: None,
                    def_span: None,
                    builtin: false,
                });
            }
        }
    }

    candidates
}

// ── HIR expression finder ──────────────────────────────────────────────

/// Find the innermost expression at a byte offset in a HIR block.
///
/// This is a general-purpose traversal that can locate any expression node
/// in typed HIR — `FieldAccess`, `Path`, calls, literals, etc.
pub fn find_hir_expr_at_position(block: &hir::Block, offset: u32) -> Option<&hir::Expr> {
    let mut best: Option<&hir::Expr> = None;

    for stmt in &block.stmts {
        if let Some(found) = find_expr_in_stmt(stmt, offset) {
            best = update_best_expr(best, Some(found));
        }
    }

    if let Some(expr) = &block.expr
        && let Some(found) = find_expr_in_expr(expr, offset)
    {
        best = update_best_expr(best, Some(found));
    }

    best
}

fn find_expr_in_stmt(stmt: &hir::Stmt, offset: u32) -> Option<&hir::Expr> {
    if !span_contains_offset(&stmt.span, offset) {
        return None;
    }

    match &stmt.kind {
        hir::StmtKind::Expr(expr) => find_expr_in_expr(expr, offset),
        hir::StmtKind::Let(_, init, _) | hir::StmtKind::Const(_, _, init, _) => {
            find_expr_in_expr(init, offset)
        }
        hir::StmtKind::Return(Some(expr)) => find_expr_in_expr(expr, offset),
        hir::StmtKind::FunctionDeclaration(decl) => find_hir_expr_at_position(&decl.body, offset),
        hir::StmtKind::ImplBlock(impl_block) => impl_block
            .methods
            .iter()
            .find_map(|decl| find_hir_expr_at_position(&decl.body, offset)),
        hir::StmtKind::ProtocolDeclaration(decl) => decl
            .methods
            .iter()
            .filter_map(|method| method.body.as_ref())
            .find_map(|body| find_hir_expr_at_position(body, offset)),
        hir::StmtKind::DynFunctionDeclaration(_)
        | hir::StmtKind::EnumDeclaration(_)
        | hir::StmtKind::StructDeclaration(_)
        | hir::StmtKind::Return(None) => None,
    }
}

fn find_expr_in_expr(expr: &hir::Expr, offset: u32) -> Option<&hir::Expr> {
    if !span_contains_offset(&expr.span, offset) {
        return None;
    }

    // Try to find a tighter match in children first.
    let mut best: Option<&hir::Expr> = None;

    match &expr.kind {
        hir::ExprKind::Call(call) | hir::ExprKind::TailCall(call) => {
            best = update_best_expr(best, find_expr_in_expr(&call.callee, offset));
            for arg in &call.arguments {
                best = update_best_expr(best, find_expr_in_expr(arg, offset));
            }
        }
        hir::ExprKind::Block(block) | hir::ExprKind::Loop(block) => {
            best = update_best_expr(best, find_hir_expr_at_position(block, offset));
        }
        hir::ExprKind::FunctionExpression(decl) => {
            best = update_best_expr(best, find_hir_expr_at_position(&decl.body, offset));
        }
        hir::ExprKind::IfElse(cond, then_block, else_clauses) => {
            best = update_best_expr(best, find_expr_in_expr(cond, offset));
            best = update_best_expr(best, find_hir_expr_at_position(then_block, offset));
            for clause in else_clauses {
                if let Some(cond) = &clause.condition {
                    best = update_best_expr(best, find_expr_in_expr(cond, offset));
                }
                best =
                    update_best_expr(best, find_hir_expr_at_position(&clause.consequence, offset));
            }
        }
        hir::ExprKind::Binary(_, lhs, rhs) => {
            best = update_best_expr(best, find_expr_in_expr(lhs, offset));
            best = update_best_expr(best, find_expr_in_expr(rhs, offset));
        }
        hir::ExprKind::Unary(_, inner)
        | hir::ExprKind::FieldAccess(inner, _)
        | hir::ExprKind::Cast(inner, _)
        | hir::ExprKind::TryCast(inner, _)
        | hir::ExprKind::Break(Some(inner))
        | hir::ExprKind::Let(_, inner)
        | hir::ExprKind::Implements(inner, _) => {
            best = update_best_expr(best, find_expr_in_expr(inner, offset));
        }
        hir::ExprKind::Match(scrutinee, arms, _) => {
            best = update_best_expr(best, find_expr_in_expr(scrutinee, offset));
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    best = update_best_expr(best, find_expr_in_expr(guard, offset));
                }
                best = update_best_expr(best, find_hir_expr_at_position(&arm.block, offset));
            }
        }
        hir::ExprKind::IndexAccess(base, index) => {
            best = update_best_expr(best, find_expr_in_expr(base, offset));
            best = update_best_expr(best, find_expr_in_expr(index, offset));
        }
        hir::ExprKind::List(items) => {
            for item in items {
                best = update_best_expr(best, find_expr_in_expr(item, offset));
            }
        }
        hir::ExprKind::Dict(entries) => {
            for (key, value) in entries {
                best = update_best_expr(best, find_expr_in_expr(key, offset));
                best = update_best_expr(best, find_expr_in_expr(value, offset));
            }
        }
        hir::ExprKind::TaggedString { tag, exprs, .. } => {
            best = update_best_expr(best, find_expr_in_expr(tag, offset));
            for e in exprs {
                best = update_best_expr(best, find_expr_in_expr(e, offset));
            }
        }
        hir::ExprKind::Range(range) => {
            best = update_best_expr(best, find_expr_in_expr(&range.start, offset));
            best = update_best_expr(best, find_expr_in_expr(&range.end, offset));
        }
        hir::ExprKind::Literal(_)
        | hir::ExprKind::Path(_)
        | hir::ExprKind::Continue
        | hir::ExprKind::Break(None)
        | hir::ExprKind::Wildcard => {}
    }

    // If no tighter child was found, the current expression is the best.
    Some(best.unwrap_or(expr))
}

fn update_best_expr<'a>(
    current: Option<&'a hir::Expr>,
    candidate: Option<&'a hir::Expr>,
) -> Option<&'a hir::Expr> {
    let Some(candidate) = candidate else {
        return current;
    };
    match current {
        Some(c) if expr_span_len(c) <= expr_span_len(candidate) => Some(c),
        _ => Some(candidate),
    }
}

fn expr_span_len(expr: &hir::Expr) -> u32 {
    expr.span.end.saturating_sub(expr.span.start)
}

// ── FieldAccess finder ─────────────────────────────────────────────────

/// Find a `FieldAccess(base, ident)` expression at a byte offset.
///
/// Returns `(base_expr, member_ident, base_type)` if the cursor is on the
/// member name of a field-access expression.
fn find_field_access_at_offset(
    block: &hir::Block,
    offset: u32,
) -> Option<(&hir::Expr, &tlang_ast::node::Ident, TyKind)> {
    find_field_access_in_block(block, offset)
}

fn find_field_access_in_block(
    block: &hir::Block,
    offset: u32,
) -> Option<(&hir::Expr, &tlang_ast::node::Ident, TyKind)> {
    for stmt in &block.stmts {
        if let Some(found) = find_field_access_in_stmt(stmt, offset) {
            return Some(found);
        }
    }
    if let Some(expr) = &block.expr {
        return find_field_access_in_expr(expr, offset);
    }
    None
}

fn find_field_access_in_stmt(
    stmt: &hir::Stmt,
    offset: u32,
) -> Option<(&hir::Expr, &tlang_ast::node::Ident, TyKind)> {
    if !span_contains_offset(&stmt.span, offset) {
        return None;
    }
    match &stmt.kind {
        hir::StmtKind::Expr(expr) => find_field_access_in_expr(expr, offset),
        hir::StmtKind::Let(_, init, _) | hir::StmtKind::Const(_, _, init, _) => {
            find_field_access_in_expr(init, offset)
        }
        hir::StmtKind::Return(Some(expr)) => find_field_access_in_expr(expr, offset),
        hir::StmtKind::FunctionDeclaration(decl) => find_field_access_in_block(&decl.body, offset),
        hir::StmtKind::ImplBlock(impl_block) => impl_block
            .methods
            .iter()
            .find_map(|decl| find_field_access_in_block(&decl.body, offset)),
        hir::StmtKind::ProtocolDeclaration(decl) => decl
            .methods
            .iter()
            .filter_map(|method| method.body.as_ref())
            .find_map(|body| find_field_access_in_block(body, offset)),
        _ => None,
    }
}

#[allow(clippy::too_many_lines)]
fn find_field_access_in_expr(
    expr: &hir::Expr,
    offset: u32,
) -> Option<(&hir::Expr, &tlang_ast::node::Ident, TyKind)> {
    if !span_contains_offset(&expr.span, offset) {
        return None;
    }

    // Check if this expression is a FieldAccess and the cursor is on the member.
    if let hir::ExprKind::FieldAccess(base, ident) = &expr.kind {
        if span_contains_offset(&ident.span, offset) {
            let base_ty = base.ty.kind.clone();
            return Some((base, ident, base_ty));
        }
        // Recurse into the base expression.
        return find_field_access_in_expr(base, offset);
    }

    // Recurse into children.
    match &expr.kind {
        hir::ExprKind::Call(call) | hir::ExprKind::TailCall(call) => {
            if let Some(found) = find_field_access_in_expr(&call.callee, offset) {
                return Some(found);
            }
            for arg in &call.arguments {
                if let Some(found) = find_field_access_in_expr(arg, offset) {
                    return Some(found);
                }
            }
        }
        hir::ExprKind::Block(block) | hir::ExprKind::Loop(block) => {
            return find_field_access_in_block(block, offset);
        }
        hir::ExprKind::FunctionExpression(decl) => {
            return find_field_access_in_block(&decl.body, offset);
        }
        hir::ExprKind::IfElse(cond, then_block, else_clauses) => {
            if let Some(found) = find_field_access_in_expr(cond, offset) {
                return Some(found);
            }
            if let Some(found) = find_field_access_in_block(then_block, offset) {
                return Some(found);
            }
            for clause in else_clauses {
                if let Some(cond) = &clause.condition
                    && let Some(found) = find_field_access_in_expr(cond, offset)
                {
                    return Some(found);
                }
                if let Some(found) = find_field_access_in_block(&clause.consequence, offset) {
                    return Some(found);
                }
            }
        }
        hir::ExprKind::Binary(_, lhs, rhs) => {
            if let Some(found) = find_field_access_in_expr(lhs, offset) {
                return Some(found);
            }
            if let Some(found) = find_field_access_in_expr(rhs, offset) {
                return Some(found);
            }
        }
        hir::ExprKind::Unary(_, inner)
        | hir::ExprKind::Cast(inner, _)
        | hir::ExprKind::TryCast(inner, _)
        | hir::ExprKind::Break(Some(inner))
        | hir::ExprKind::Let(_, inner)
        | hir::ExprKind::Implements(inner, _) => {
            return find_field_access_in_expr(inner, offset);
        }
        hir::ExprKind::Match(scrutinee, arms, _) => {
            if let Some(found) = find_field_access_in_expr(scrutinee, offset) {
                return Some(found);
            }
            for arm in arms {
                if let Some(guard) = &arm.guard
                    && let Some(found) = find_field_access_in_expr(guard, offset)
                {
                    return Some(found);
                }
                if let Some(found) = find_field_access_in_block(&arm.block, offset) {
                    return Some(found);
                }
            }
        }
        hir::ExprKind::IndexAccess(base, index) => {
            if let Some(found) = find_field_access_in_expr(base, offset) {
                return Some(found);
            }
            if let Some(found) = find_field_access_in_expr(index, offset) {
                return Some(found);
            }
        }
        hir::ExprKind::List(items) => {
            for item in items {
                if let Some(found) = find_field_access_in_expr(item, offset) {
                    return Some(found);
                }
            }
        }
        hir::ExprKind::Dict(entries) => {
            for (key, value) in entries {
                if let Some(found) = find_field_access_in_expr(key, offset) {
                    return Some(found);
                }
                if let Some(found) = find_field_access_in_expr(value, offset) {
                    return Some(found);
                }
            }
        }
        hir::ExprKind::TaggedString { tag, exprs, .. } => {
            if let Some(found) = find_field_access_in_expr(tag, offset) {
                return Some(found);
            }
            for e in exprs {
                if let Some(found) = find_field_access_in_expr(e, offset) {
                    return Some(found);
                }
            }
        }
        hir::ExprKind::Range(range) => {
            if let Some(found) = find_field_access_in_expr(&range.start, offset) {
                return Some(found);
            }
            if let Some(found) = find_field_access_in_expr(&range.end, offset) {
                return Some(found);
            }
        }
        hir::ExprKind::Literal(_)
        | hir::ExprKind::Path(_)
        | hir::ExprKind::Continue
        | hir::ExprKind::Break(None)
        | hir::ExprKind::Wildcard => {}
        // FieldAccess is handled above before the match
        hir::ExprKind::FieldAccess(_, _) => unreachable!(),
    }

    None
}

// ── Receiver type resolution for dot-completion ────────────────────────

/// Determine the receiver type for dot-completion at a position.
///
/// The position should be immediately after the dot (or during partial
/// member typing).  This function tries to find the receiver expression
/// and its type from the typed HIR.
fn receiver_type_before_dot(
    source: &str,
    typed_hir: &TypedHir,
    line: u32,
    utf16_col: u32,
) -> Option<String> {
    // The cursor is right after the dot — the dot is at col-1 and the
    // receiver ends before that.  We scan for the receiver in the typed
    // HIR by finding the expression at the dot position.
    if utf16_col == 0 {
        return None;
    }

    // Try to find a FieldAccess expression whose member ident starts near
    // the cursor.  First, try the exact offset.
    let offset = utf16_line_column_to_byte_offset(source, line, utf16_col);

    // Look at the byte just before the offset for the dot character.
    let dot_offset = offset.saturating_sub(1);

    // Walk the HIR to find the innermost expression that contains the
    // dot offset.  If it's a FieldAccess, we can get the base type directly.
    if let Some(expr) = find_hir_expr_at_position(&typed_hir.module.block, dot_offset) {
        // If we landed on a FieldAccess, use the base type.
        if let hir::ExprKind::FieldAccess(base, _) = &expr.kind {
            return type_name_for_ty(&base.ty.kind);
        }
        // Otherwise the expression itself might be the receiver.
        return type_name_for_ty(&expr.ty.kind);
    }

    None
}

// ── Internal helpers ───────────────────────────────────────────────────

fn type_name_for_ty(ty: &TyKind) -> Option<String> {
    builtin_methods::type_name_from_kind(ty).map(|s| s.to_string())
}

fn span_contains_offset(span: &Span, offset: u32) -> bool {
    span.start <= offset && offset <= span.end
}

/// Build a `SignatureInformation` from a builtin method's `TyKind::Fn`.
fn signature_from_builtin(
    type_name: &str,
    method_name: &str,
    sig_ty: &TyKind,
) -> SignatureInformation {
    if let TyKind::Fn(params, ret) = sig_ty {
        let param_strs: Vec<String> = params.iter().map(|p| p.kind.to_string()).collect();
        SignatureInformation {
            label: format!(
                "{type_name}.{method_name}({}) -> {}",
                param_strs.join(", "),
                ret.kind
            ),
            parameters: param_strs
                .into_iter()
                .map(|label| ParameterInformation { label })
                .collect(),
        }
    } else {
        SignatureInformation {
            label: format!("{type_name}.{method_name}"),
            parameters: vec![],
        }
    }
}

/// Extract the return type from a `TyKind::Fn(params, ret)`.
fn extract_return_ty(sig_ty: &TyKind) -> Option<TyKind> {
    if let TyKind::Fn(_, ret) = sig_ty {
        Some(ret.kind.clone())
    } else {
        None
    }
}

/// Resolve a method from protocol impls in the type table.
fn resolve_protocol_method(
    type_table: &tlang_typeck::TypeTable,
    target_type_name: &str,
    method_name: &str,
) -> Option<(SignatureInformation, Option<TyKind>)> {
    let impl_info = type_table
        .impls()
        .iter()
        .filter(|info| info.target_type_name == target_type_name)
        .find(|info| {
            type_table
                .get_protocol_info(&info.protocol_name)
                .is_some_and(|protocol| {
                    protocol
                        .methods
                        .iter()
                        .any(|m| m.name.as_str() == method_name)
                })
        })?;

    let protocol = type_table.get_protocol_info(&impl_info.protocol_name)?;
    let method = protocol
        .methods
        .iter()
        .find(|m| m.name.as_str() == method_name)?;

    let params: Vec<String> = method
        .param_tys
        .iter()
        .skip(1) // skip self
        .map(|ty| ty.kind.to_string())
        .collect();

    let signature = SignatureInformation {
        label: format!(
            "{target_type_name}.{method_name}({}) -> {}",
            params.join(", "),
            method.return_ty.kind
        ),
        parameters: params
            .into_iter()
            .map(|label| ParameterInformation { label })
            .collect(),
    };

    Some((signature, Some(method.return_ty.kind.clone())))
}

/// Resolve a member from user-defined declarations in the HIR.
struct HirMemberInfo {
    kind: MemberKind,
    signature: Option<SignatureInformation>,
    return_ty: Option<TyKind>,
    def_span: Option<Span>,
}

fn resolve_hir_member(
    block: &hir::Block,
    type_name: &str,
    member_name: &str,
) -> Option<HirMemberInfo> {
    let dotted_name = format!("{type_name}.{member_name}");

    for stmt in &block.stmts {
        match &stmt.kind {
            hir::StmtKind::FunctionDeclaration(decl) if decl.name() == dotted_name => {
                let params: Vec<String> = decl
                    .parameters
                    .iter()
                    .skip(1) // skip self
                    .map(|p| format!("{}: {}", p.name, p.type_annotation.kind))
                    .collect();
                let ret = &decl.return_type.kind;
                let signature = SignatureInformation {
                    label: format!("{type_name}.{member_name}({}) -> {ret}", params.join(", ")),
                    parameters: params
                        .into_iter()
                        .map(|label| ParameterInformation { label })
                        .collect(),
                };
                return Some(HirMemberInfo {
                    kind: MemberKind::Method,
                    signature: Some(signature),
                    return_ty: Some(ret.clone()),
                    def_span: Some(decl.name.span),
                });
            }
            hir::StmtKind::StructDeclaration(decl) if decl.name.to_string() == type_name => {
                for field in &decl.fields {
                    if field.name.to_string() == member_name {
                        return Some(HirMemberInfo {
                            kind: MemberKind::Field,
                            signature: None,
                            return_ty: Some(field.ty.kind.clone()),
                            def_span: Some(field.name.span),
                        });
                    }
                }
            }
            hir::StmtKind::ImplBlock(impl_block) => {
                if impl_block.target_type.to_string() != type_name {
                    continue;
                }
                for decl in &impl_block.methods {
                    // Match qualified names (e.g. "Type.member") or
                    // unqualified names (e.g. "member") used by impl blocks.
                    let is_match = decl.name() == dotted_name
                        || decl.name() == member_name
                        || decl
                            .name()
                            .strip_prefix(&format!("{type_name}."))
                            .is_some_and(|n| n == member_name);
                    if is_match {
                        let params: Vec<String> = decl
                            .parameters
                            .iter()
                            .skip(1)
                            .map(|p| format!("{}: {}", p.name, p.type_annotation.kind))
                            .collect();
                        let ret = &decl.return_type.kind;
                        let signature = SignatureInformation {
                            label: format!(
                                "{type_name}.{member_name}({}) -> {ret}",
                                params.join(", ")
                            ),
                            parameters: params
                                .into_iter()
                                .map(|label| ParameterInformation { label })
                                .collect(),
                        };
                        return Some(HirMemberInfo {
                            kind: MemberKind::Method,
                            signature: Some(signature),
                            return_ty: Some(ret.clone()),
                            def_span: Some(decl.name.span),
                        });
                    }
                }
            }
            _ => {}
        }
    }

    None
}

// ── Coordinate conversion ──────────────────────────────────────────────

/// Convert an LSP-style (0-based line, 0-based UTF-16 column) position to
/// a byte offset into `source`.
///
/// This is the canonical conversion used by all member-resolution entry
/// points so callers pass LSP positions and the module handles translation
/// internally.
fn utf16_line_column_to_byte_offset(source: &str, line: u32, utf16_column: u32) -> u32 {
    let mut current_line = 0u32;
    let mut line_start = source.len();

    for (i, ch) in source.char_indices() {
        if current_line == line {
            line_start = i;
            break;
        }
        if ch == '\n' {
            current_line += 1;
            if current_line == line {
                line_start = i + ch.len_utf8();
                break;
            }
        }
    }

    if line == 0 {
        line_start = 0;
    }

    if line_start == source.len() && current_line < line {
        return source.len() as u32;
    }

    let mut utf16_count = 0u32;
    let mut byte_offset = line_start;

    for ch in source[line_start..].chars() {
        if ch == '\n' || utf16_count >= utf16_column {
            break;
        }

        utf16_count += ch.len_utf16() as u32;
        byte_offset += ch.len_utf8();
    }

    byte_offset as u32
}

// ── Tests ──────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::typed_hir::lower_and_typecheck;
    use crate::{CompilationTarget, analyze_for_target};

    fn typed_hir(source: &str) -> TypedHir {
        let result = analyze_for_target(source, CompilationTarget::Js);
        lower_and_typecheck(&result).unwrap_or_else(|| {
            panic!(
                "lowering/typechecking should succeed.\n  parse_issues: {:?}\n  diagnostics: {:?}",
                result.parse_issues,
                result.all_diagnostics()
            )
        })
    }

    // ── find_hir_expr_at_position ──────────────────────────────────────

    #[test]
    fn find_expr_at_literal() {
        let source = "let x = 42;";
        let hir = typed_hir(source);
        let offset = source.find("42").unwrap() as u32;
        let expr = find_hir_expr_at_position(&hir.module.block, offset);
        assert!(expr.is_some(), "should find expression at literal");
        assert!(
            matches!(expr.unwrap().kind, hir::ExprKind::Literal(_)),
            "expected Literal, got {:?}",
            expr.unwrap().kind
        );
    }

    #[test]
    fn find_expr_at_field_access() {
        let source = r#"let _ = re"foo".replace_all("a", "b");"#;
        let hir = typed_hir(source);
        let offset = source.find("replace_all").unwrap() as u32;
        let expr = find_hir_expr_at_position(&hir.module.block, offset);
        assert!(expr.is_some(), "should find expression at field access");
    }

    // ── builtin method enumeration ─────────────────────────────────────

    #[test]
    fn methods_for_regex_includes_replace_all() {
        let methods = builtin_methods::methods_for("Regex");
        assert!(
            methods.iter().any(|m| m.name == "replace_all"),
            "Regex should have replace_all, got: {:?}",
            methods.iter().map(|m| m.name).collect::<Vec<_>>()
        );
    }

    #[test]
    fn methods_for_string_includes_len() {
        let methods = builtin_methods::methods_for("String");
        assert!(
            methods.iter().any(|m| m.name == "len"),
            "String should have len"
        );
    }

    #[test]
    fn methods_for_unknown_type_is_empty() {
        let methods = builtin_methods::methods_for("FooBarBaz");
        assert!(methods.is_empty());
    }

    // ── resolve_member_at_position ─────────────────────────────────────

    #[test]
    fn resolve_builtin_regex_replace_all() {
        let source = r#"let _ = re"foo".replace_all("a", "b");"#;
        let hir = typed_hir(source);
        let col = source.find("replace_all").unwrap() as u32;
        let resolved = resolve_member_at_position(source, &hir, 0, col);
        assert!(resolved.is_some(), "should resolve re\"foo\".replace_all");
        let resolved = resolved.unwrap();
        assert_eq!(resolved.name, "replace_all");
        assert!(resolved.builtin);
        assert_eq!(resolved.kind, MemberKind::Method);
        assert!(resolved.signature.is_some());
    }

    #[test]
    fn resolve_builtin_string_len() {
        let source = r#"let _ = "hello".len();"#;
        let hir = typed_hir(source);
        let col = source.find("len").unwrap() as u32;
        let resolved = resolve_member_at_position(source, &hir, 0, col);
        assert!(resolved.is_some(), "should resolve \"hello\".len");
        let resolved = resolved.unwrap();
        assert_eq!(resolved.name, "len");
        assert!(resolved.builtin);
    }

    #[test]
    fn resolve_builtin_temporal_duration_field() {
        let source = r#"
let start = Temporal::PlainDate::from("2025-03-01");
let end = Temporal::PlainDate::from("2025-03-16");
let _ = end.since(start).days;
"#;
        let typed_hir = typed_hir(source);
        let line = source.lines().nth(3).unwrap();
        let col = line.find("days").unwrap() as u32;
        let resolved = resolve_member_at_position(source, &typed_hir, 3, col);
        assert!(resolved.is_some(), "should resolve Temporal::Duration.days");
        let resolved = resolved.unwrap();
        assert_eq!(resolved.name, "days");
        assert_eq!(resolved.kind, MemberKind::Field);
        assert!(resolved.builtin);
        assert_eq!(
            resolved.return_ty,
            Some(TyKind::Primitive(hir::PrimTy::I64))
        );
    }

    // ── complete_members_at_position ───────────────────────────────────

    #[test]
    fn complete_members_for_regex_includes_builtins() {
        let source = r#"let _ = re"foo".test("bar");"#;
        let hir = typed_hir(source);
        let candidates = complete_members_for_type(&hir, None, "Regex");
        let names: Vec<&str> = candidates.iter().map(|c| c.name.as_str()).collect();
        assert!(names.contains(&"replace_all"), "should contain replace_all");
        assert!(
            names.contains(&"replace_first"),
            "should contain replace_first"
        );
        assert!(names.contains(&"test"), "should contain test");
    }

    #[test]
    fn complete_members_for_string_includes_builtins() {
        let source = r#"let _ = "hello".len();"#;
        let hir = typed_hir(source);
        let candidates = complete_members_for_type(&hir, None, "String");
        let names: Vec<&str> = candidates.iter().map(|c| c.name.as_str()).collect();
        assert!(names.contains(&"len"), "should contain len");
        assert!(names.contains(&"trim"), "should contain trim");
        assert!(names.contains(&"contains"), "should contain contains");
    }

    #[test]
    fn complete_members_for_list_includes_map() {
        let source = "let x = [1, 2, 3];";
        let hir = typed_hir(source);
        let candidates = complete_members_for_type(&hir, None, "List");
        let names: Vec<&str> = candidates.iter().map(|c| c.name.as_str()).collect();
        assert!(names.contains(&"map"), "should contain map");
        assert!(names.contains(&"filter"), "should contain filter");
        assert!(names.contains(&"foldl"), "should contain foldl");
    }

    #[test]
    fn complete_members_for_temporal_duration_includes_fields() {
        let source = r#"let start = Temporal::PlainDate::from("2025-03-01");"#;
        let hir = typed_hir(source);
        let candidates = complete_members_for_type(&hir, None, "Duration");

        let days = candidates.iter().find(|candidate| candidate.name == "days");
        assert!(days.is_some(), "should contain builtin field 'days'");
        assert_eq!(days.unwrap().kind, MemberKind::Field);

        let calendar = candidates
            .iter()
            .find(|candidate| candidate.name == "calendar");
        assert!(
            calendar.is_none(),
            "Duration should not contain fields from other Temporal types"
        );

        let field_names: Vec<&str> = candidates
            .iter()
            .filter(|candidate| candidate.kind == MemberKind::Field)
            .map(|candidate| candidate.name.as_str())
            .collect();
        assert!(field_names.contains(&"years"));
        assert!(field_names.contains(&"months"));
        assert!(field_names.contains(&"weeks"));
        assert!(field_names.contains(&"days"));
        assert!(field_names.contains(&"hours"));
        assert!(field_names.contains(&"minutes"));
        assert!(field_names.contains(&"seconds"));
        assert!(field_names.contains(&"milliseconds"));
        assert!(field_names.contains(&"microseconds"));
        assert!(field_names.contains(&"nanoseconds"));
        assert!(field_names.contains(&"sign"));
        assert!(field_names.contains(&"blank"));
    }

    #[test]
    fn complete_members_no_duplicates() {
        let source = r#"let _ = "hello".len();"#;
        let hir = typed_hir(source);
        let candidates = complete_members_for_type(&hir, None, "String");
        let mut names: Vec<&str> = candidates.iter().map(|c| c.name.as_str()).collect();
        let len_before = names.len();
        names.sort();
        names.dedup();
        assert_eq!(len_before, names.len(), "should have no duplicate members");
    }

    #[test]
    fn resolve_member_multiline() {
        let source = "let x = 1;\nlet _ = re\"foo\".replace_all(\"a\", \"b\");";
        let hir = typed_hir(source);
        // "replace_all" is on line 1
        let line1 = source.lines().nth(1).unwrap();
        let col = line1.find("replace_all").unwrap() as u32;
        let resolved = resolve_member_at_position(source, &hir, 1, col);
        assert!(
            resolved.is_some(),
            "should resolve replace_all on second line"
        );
        assert_eq!(resolved.unwrap().name, "replace_all");
    }

    #[test]
    fn complete_user_struct_methods() {
        let source = r#"
struct Vector { x: i64, y: i64 }
fn Vector.add(self, other: Vector) -> Vector {
    Vector { x: self.x + other.x, y: self.y + other.y }
}
"#;
        let result = analyze_for_target(source, CompilationTarget::Js);
        let hir = lower_and_typecheck(&result).expect("should succeed");
        let index = crate::symbol_index::SymbolIndex::from_analyzer(&result.analyzer);
        let candidates = complete_members_for_type(&hir, Some(&index), "Vector");
        let names: Vec<&str> = candidates.iter().map(|c| c.name.as_str()).collect();
        assert!(
            names.contains(&"add"),
            "should contain user-defined method 'add', got: {names:?}"
        );
    }

    #[test]
    fn resolve_member_on_chained_custom_method_call() {
        let source = r#"
struct Counter {
    value: i64,
}

fn Counter.inc(self) -> Counter {
    Counter { value: self.value + 1 }
}

let start: Counter = Counter { value: 0 };
let _ = start.inc().inc();
"#;
        let hir = typed_hir(source);
        let line = source.lines().nth(10).unwrap();
        let col = line.rfind("inc").unwrap() as u32;
        let resolved = resolve_member_at_position(source, &hir, 10, col);
        assert!(resolved.is_some(), "should resolve chained Counter.inc");
        let resolved = resolved.unwrap();
        assert_eq!(resolved.name, "inc");
        assert_eq!(resolved.kind, MemberKind::Method);
    }

    #[test]
    fn complete_struct_fields_have_field_kind() {
        let source = r#"
struct Point { x: i64, y: i64 }
fn Point.distance(self) -> i64 { self.x + self.y }
"#;
        let result = analyze_for_target(source, CompilationTarget::Js);
        let hir = lower_and_typecheck(&result).expect("should succeed");
        let index = crate::symbol_index::SymbolIndex::from_analyzer(&result.analyzer);
        let candidates = complete_members_for_type(&hir, Some(&index), "Point");

        let field_x = candidates.iter().find(|c| c.name == "x");
        assert!(field_x.is_some(), "should contain field 'x'");
        assert_eq!(
            field_x.unwrap().kind,
            MemberKind::Field,
            "struct field 'x' should have Field kind"
        );

        let method = candidates.iter().find(|c| c.name == "distance");
        assert!(method.is_some(), "should contain method 'distance'");
        assert_eq!(
            method.unwrap().kind,
            MemberKind::Method,
            "struct method 'distance' should have Method kind"
        );
    }
}
