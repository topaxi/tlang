//! Shared signature-help analysis for the LSP server and browser bindings.

use tlang_hir as hir;
use tlang_hir::Ty;
use tlang_span::{HirId, Span};
use tlang_typeck::{TypeTable, builtins};

use crate::inlay_hints::TypedHir;

/// Protocol-agnostic signature-help payload.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SignatureHelp {
    pub signatures: Vec<SignatureInformation>,
    pub active_signature: u32,
    pub active_parameter: u32,
}

/// A single callable signature.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SignatureInformation {
    pub label: String,
    pub parameters: Vec<ParameterInformation>,
}

/// A single parameter label within a signature.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParameterInformation {
    pub label: String,
}

/// Compute signature help for the call surrounding the given editor position.
///
/// `line` is 0-based and `utf16_column` is a UTF-16 code-unit column, matching
/// LSP and CodeMirror conventions.
pub fn signature_help_at(
    source: &str,
    typed_hir: &TypedHir,
    line: u32,
    utf16_column: u32,
) -> Option<SignatureHelp> {
    let cursor = utf16_line_column_to_byte_offset(source, line, utf16_column);
    let call = find_innermost_call(&typed_hir.module.block, cursor)?;
    let open_paren = find_call_open_paren(source, call)?;
    let signature = signature_for_call(typed_hir, call)?;
    let active_parameter = active_parameter(source, open_paren.saturating_add(1), cursor);
    let active_parameter = clamp_active_parameter(&signature, active_parameter);

    Some(SignatureHelp {
        signatures: vec![signature],
        active_signature: 0,
        active_parameter,
    })
}

fn signature_for_call(
    typed_hir: &TypedHir,
    call: &hir::CallExpression,
) -> Option<SignatureInformation> {
    match &call.callee.kind {
        hir::ExprKind::Path(path) => {
            let name = path.join("::");
            if let Some(hir_id) = path.res.hir_id()
                && let Some(signature) =
                    signature_from_hir_id(&typed_hir.module.block, hir_id, false)
            {
                return Some(rename_signature(signature, &name));
            }

            builtins::lookup_signature(&name).map(|builtin| {
                signature_from_types(
                    &name,
                    builtin.params,
                    &builtin.ret.to_string(),
                    builtin.variadic,
                    false,
                )
            })
        }
        hir::ExprKind::FieldAccess(base, ident) => {
            let type_name = type_name_for_expr(base)?;

            signature_from_dotted_method(&typed_hir.module.block, &type_name, ident.as_str())
                .or_else(|| {
                    signature_from_protocol_impls(&typed_hir.type_table, &type_name, ident.as_str())
                })
                .or_else(|| {
                    if let hir::TyKind::Fn(params, ret) = &call.callee.ty.kind {
                        Some(signature_from_ty(
                            &format!("{type_name}.{}", ident.as_str()),
                            params,
                            ret,
                            false,
                            true,
                        ))
                    } else {
                        None
                    }
                })
        }
        _ => {
            if let hir::TyKind::Fn(params, ret) = &call.callee.ty.kind {
                Some(signature_from_ty("fn", params, ret, false, false))
            } else {
                None
            }
        }
    }
}

fn signature_from_protocol_impls(
    type_table: &TypeTable,
    target_type_name: &str,
    method_name: &str,
) -> Option<SignatureInformation> {
    if let Some(impl_info) = type_table
        .impls()
        .iter()
        .filter(|impl_info| impl_info.target_type_name == target_type_name)
        .find(|impl_info| {
            type_table
                .get_protocol_info(&impl_info.protocol_name)
                .is_some_and(|protocol| {
                    protocol
                        .methods
                        .iter()
                        .any(|method| method.name.as_str() == method_name)
                })
        })
    {
        let protocol = type_table.get_protocol_info(&impl_info.protocol_name)?;
        let method = protocol
            .methods
            .iter()
            .find(|method| method.name.as_str() == method_name)?;

        let params: Vec<String> = method
            .param_tys
            .iter()
            .skip(1)
            .map(|ty| ty.kind.to_string())
            .collect();

        Some(SignatureInformation {
            label: format!(
                "{target_type_name}.{method_name}({}) -> {}",
                params.join(", "),
                method.return_ty.kind
            ),
            parameters: params
                .into_iter()
                .map(|label| ParameterInformation { label })
                .collect(),
        })
    } else {
        None
    }
}

fn signature_from_dotted_method(
    block: &hir::Block,
    type_name: &str,
    method_name: &str,
) -> Option<SignatureInformation> {
    let dotted_name = format!("{type_name}.{method_name}");
    signature_from_name(block, &dotted_name, true)
}

fn signature_from_block(
    block: &hir::Block,
    dotted_name: &str,
    omit_first_param: bool,
) -> Option<SignatureInformation> {
    signature_from_name(block, dotted_name, omit_first_param)
}

fn signature_from_name(
    block: &hir::Block,
    dotted_name: &str,
    omit_first_param: bool,
) -> Option<SignatureInformation> {
    for stmt in &block.stmts {
        if let Some(signature) = signature_from_stmt_name(stmt, dotted_name, omit_first_param) {
            return Some(signature);
        }
    }

    block
        .expr
        .as_ref()
        .and_then(|expr| signature_from_expr_name(expr, dotted_name, omit_first_param))
}

fn signature_from_stmt_name(
    stmt: &hir::Stmt,
    dotted_name: &str,
    omit_first_param: bool,
) -> Option<SignatureInformation> {
    match &stmt.kind {
        hir::StmtKind::FunctionDeclaration(decl) if decl.name() == dotted_name => {
            Some(signature_from_function_decl(decl, omit_first_param))
        }
        hir::StmtKind::Expr(expr) => signature_from_expr_name(expr, dotted_name, omit_first_param),
        hir::StmtKind::FunctionDeclaration(decl) => {
            signature_from_block(&decl.body, dotted_name, omit_first_param)
        }
        hir::StmtKind::ImplBlock(impl_block) => {
            for decl in &impl_block.methods {
                if decl.name() == dotted_name {
                    return Some(signature_from_function_decl(decl, omit_first_param));
                }

                if let Some(signature) =
                    signature_from_block(&decl.body, dotted_name, omit_first_param)
                {
                    return Some(signature);
                }
            }
            None
        }
        hir::StmtKind::ProtocolDeclaration(decl) => {
            for method in &decl.methods {
                if let Some(body) = &method.body
                    && let Some(signature) =
                        signature_from_block(body, dotted_name, omit_first_param)
                {
                    return Some(signature);
                }
            }
            None
        }
        _ => None,
    }
}

fn signature_from_expr_name(
    expr: &hir::Expr,
    dotted_name: &str,
    omit_first_param: bool,
) -> Option<SignatureInformation> {
    match &expr.kind {
        hir::ExprKind::FunctionExpression(decl) if decl.name() == dotted_name => {
            Some(signature_from_function_decl(decl, omit_first_param))
        }
        hir::ExprKind::FunctionExpression(decl) => {
            signature_from_block(&decl.body, dotted_name, omit_first_param)
        }
        hir::ExprKind::Block(block) | hir::ExprKind::Loop(block) => {
            signature_from_block(block, dotted_name, omit_first_param)
        }
        hir::ExprKind::IfElse(cond, then_block, else_clauses) => {
            signature_from_expr_name(cond, dotted_name, omit_first_param)
                .or_else(|| signature_from_block(then_block, dotted_name, omit_first_param))
                .or_else(|| {
                    else_clauses.iter().find_map(|clause| {
                        clause
                            .condition
                            .as_ref()
                            .and_then(|cond| {
                                signature_from_expr_name(cond, dotted_name, omit_first_param)
                            })
                            .or_else(|| {
                                signature_from_block(
                                    &clause.consequence,
                                    dotted_name,
                                    omit_first_param,
                                )
                            })
                    })
                })
        }
        hir::ExprKind::Call(call) | hir::ExprKind::TailCall(call) => {
            signature_from_expr_name(&call.callee, dotted_name, omit_first_param).or_else(|| {
                call.arguments
                    .iter()
                    .find_map(|arg| signature_from_expr_name(arg, dotted_name, omit_first_param))
            })
        }
        hir::ExprKind::Binary(_, lhs, rhs) => {
            signature_from_expr_name(lhs, dotted_name, omit_first_param)
                .or_else(|| signature_from_expr_name(rhs, dotted_name, omit_first_param))
        }
        hir::ExprKind::Unary(_, inner)
        | hir::ExprKind::FieldAccess(inner, _)
        | hir::ExprKind::Cast(inner, _)
        | hir::ExprKind::TryCast(inner, _)
        | hir::ExprKind::Break(Some(inner)) => {
            signature_from_expr_name(inner, dotted_name, omit_first_param)
        }
        hir::ExprKind::Let(_, inner) | hir::ExprKind::Implements(inner, _) => {
            signature_from_expr_name(inner, dotted_name, omit_first_param)
        }
        hir::ExprKind::Match(scrutinee, arms) => {
            signature_from_expr_name(scrutinee, dotted_name, omit_first_param).or_else(|| {
                arms.iter()
                    .find_map(|arm| signature_from_block(&arm.block, dotted_name, omit_first_param))
            })
        }
        hir::ExprKind::IndexAccess(base, index) => {
            signature_from_expr_name(base, dotted_name, omit_first_param)
                .or_else(|| signature_from_expr_name(index, dotted_name, omit_first_param))
        }
        hir::ExprKind::List(items) => items
            .iter()
            .find_map(|item| signature_from_expr_name(item, dotted_name, omit_first_param)),
        hir::ExprKind::Dict(entries) => entries.iter().find_map(|(key, value)| {
            signature_from_expr_name(key, dotted_name, omit_first_param)
                .or_else(|| signature_from_expr_name(value, dotted_name, omit_first_param))
        }),
        hir::ExprKind::TaggedString { tag, exprs, .. } => {
            signature_from_expr_name(tag, dotted_name, omit_first_param).or_else(|| {
                exprs
                    .iter()
                    .find_map(|expr| signature_from_expr_name(expr, dotted_name, omit_first_param))
            })
        }
        hir::ExprKind::Range(range) => {
            signature_from_expr_name(&range.start, dotted_name, omit_first_param)
                .or_else(|| signature_from_expr_name(&range.end, dotted_name, omit_first_param))
        }
        hir::ExprKind::Literal(_)
        | hir::ExprKind::Path(_)
        | hir::ExprKind::Continue
        | hir::ExprKind::Break(None)
        | hir::ExprKind::Wildcard => None,
    }
}

fn signature_from_hir_id(
    block: &hir::Block,
    hir_id: HirId,
    omit_first_param: bool,
) -> Option<SignatureInformation> {
    for stmt in &block.stmts {
        if let Some(signature) = signature_from_stmt_hir_id(stmt, hir_id, omit_first_param) {
            return Some(signature);
        }
    }

    block
        .expr
        .as_ref()
        .and_then(|expr| signature_from_expr_hir_id(expr, hir_id, omit_first_param))
}

fn signature_from_stmt_hir_id(
    stmt: &hir::Stmt,
    hir_id: HirId,
    omit_first_param: bool,
) -> Option<SignatureInformation> {
    match &stmt.kind {
        hir::StmtKind::FunctionDeclaration(decl) if decl.hir_id == hir_id => {
            Some(signature_from_function_decl(decl, omit_first_param))
        }
        hir::StmtKind::FunctionDeclaration(decl) => {
            signature_from_hir_id(&decl.body, hir_id, omit_first_param)
        }
        hir::StmtKind::ImplBlock(impl_block) => {
            for decl in &impl_block.methods {
                if decl.hir_id == hir_id {
                    return Some(signature_from_function_decl(decl, omit_first_param));
                }

                if let Some(signature) = signature_from_hir_id(&decl.body, hir_id, omit_first_param)
                {
                    return Some(signature);
                }
            }
            None
        }
        hir::StmtKind::ProtocolDeclaration(decl) => {
            for method in &decl.methods {
                if method.hir_id == hir_id {
                    return Some(signature_from_protocol_method(
                        decl.name.as_str(),
                        method,
                        omit_first_param,
                    ));
                }

                if let Some(body) = &method.body
                    && let Some(signature) = signature_from_hir_id(body, hir_id, omit_first_param)
                {
                    return Some(signature);
                }
            }
            None
        }
        hir::StmtKind::EnumDeclaration(decl) => decl.variants.iter().find_map(|variant| {
            (variant.hir_id == hir_id)
                .then(|| signature_from_enum_variant(decl.name.as_str(), variant))
        }),
        hir::StmtKind::Expr(expr) => signature_from_expr_hir_id(expr, hir_id, omit_first_param),
        _ => None,
    }
}

fn signature_from_expr_hir_id(
    expr: &hir::Expr,
    hir_id: HirId,
    omit_first_param: bool,
) -> Option<SignatureInformation> {
    match &expr.kind {
        hir::ExprKind::FunctionExpression(decl) if decl.hir_id == hir_id => {
            Some(signature_from_function_decl(decl, omit_first_param))
        }
        hir::ExprKind::FunctionExpression(decl) => {
            signature_from_hir_id(&decl.body, hir_id, omit_first_param)
        }
        hir::ExprKind::Block(block) | hir::ExprKind::Loop(block) => {
            signature_from_hir_id(block, hir_id, omit_first_param)
        }
        hir::ExprKind::IfElse(cond, then_block, else_clauses) => {
            signature_from_expr_hir_id(cond, hir_id, omit_first_param)
                .or_else(|| signature_from_hir_id(then_block, hir_id, omit_first_param))
                .or_else(|| {
                    else_clauses.iter().find_map(|clause| {
                        clause
                            .condition
                            .as_ref()
                            .and_then(|cond| {
                                signature_from_expr_hir_id(cond, hir_id, omit_first_param)
                            })
                            .or_else(|| {
                                signature_from_hir_id(&clause.consequence, hir_id, omit_first_param)
                            })
                    })
                })
        }
        hir::ExprKind::Call(call) | hir::ExprKind::TailCall(call) => {
            signature_from_expr_hir_id(&call.callee, hir_id, omit_first_param).or_else(|| {
                call.arguments
                    .iter()
                    .find_map(|arg| signature_from_expr_hir_id(arg, hir_id, omit_first_param))
            })
        }
        hir::ExprKind::Binary(_, lhs, rhs) => {
            signature_from_expr_hir_id(lhs, hir_id, omit_first_param)
                .or_else(|| signature_from_expr_hir_id(rhs, hir_id, omit_first_param))
        }
        hir::ExprKind::Unary(_, inner)
        | hir::ExprKind::FieldAccess(inner, _)
        | hir::ExprKind::Cast(inner, _)
        | hir::ExprKind::TryCast(inner, _)
        | hir::ExprKind::Break(Some(inner)) => {
            signature_from_expr_hir_id(inner, hir_id, omit_first_param)
        }
        hir::ExprKind::Let(_, inner) | hir::ExprKind::Implements(inner, _) => {
            signature_from_expr_hir_id(inner, hir_id, omit_first_param)
        }
        hir::ExprKind::Match(scrutinee, arms) => {
            signature_from_expr_hir_id(scrutinee, hir_id, omit_first_param).or_else(|| {
                arms.iter()
                    .find_map(|arm| signature_from_hir_id(&arm.block, hir_id, omit_first_param))
            })
        }
        hir::ExprKind::IndexAccess(base, index) => {
            signature_from_expr_hir_id(base, hir_id, omit_first_param)
                .or_else(|| signature_from_expr_hir_id(index, hir_id, omit_first_param))
        }
        hir::ExprKind::List(items) => items
            .iter()
            .find_map(|item| signature_from_expr_hir_id(item, hir_id, omit_first_param)),
        hir::ExprKind::Dict(entries) => entries.iter().find_map(|(key, value)| {
            signature_from_expr_hir_id(key, hir_id, omit_first_param)
                .or_else(|| signature_from_expr_hir_id(value, hir_id, omit_first_param))
        }),
        hir::ExprKind::TaggedString { tag, exprs, .. } => {
            signature_from_expr_hir_id(tag, hir_id, omit_first_param).or_else(|| {
                exprs
                    .iter()
                    .find_map(|expr| signature_from_expr_hir_id(expr, hir_id, omit_first_param))
            })
        }
        hir::ExprKind::Range(range) => {
            signature_from_expr_hir_id(&range.start, hir_id, omit_first_param)
                .or_else(|| signature_from_expr_hir_id(&range.end, hir_id, omit_first_param))
        }
        hir::ExprKind::Literal(_)
        | hir::ExprKind::Path(_)
        | hir::ExprKind::Continue
        | hir::ExprKind::Break(None)
        | hir::ExprKind::Wildcard => None,
    }
}

fn signature_from_function_decl(
    decl: &hir::FunctionDeclaration,
    omit_first_param: bool,
) -> SignatureInformation {
    signature_from_named_params(
        &decl.name(),
        decl.parameters
            .iter()
            .map(|param| (param.name.to_string(), &param.type_annotation))
            .collect::<Vec<_>>(),
        &decl.return_type,
        omit_first_param,
    )
}

fn signature_from_protocol_method(
    protocol_name: &str,
    method: &hir::ProtocolMethodSignature,
    omit_first_param: bool,
) -> SignatureInformation {
    signature_from_named_params(
        &format!("{protocol_name}::{}", method.name),
        method
            .parameters
            .iter()
            .map(|param| (param.name.to_string(), &param.type_annotation))
            .collect::<Vec<_>>(),
        &method.return_type,
        omit_first_param,
    )
}

fn signature_from_enum_variant(
    enum_name: &str,
    variant: &hir::EnumVariant,
) -> SignatureInformation {
    let params: Vec<String> = variant
        .parameters
        .iter()
        .map(|field| format!("{}: {}", field.name, field.ty.kind))
        .collect();

    SignatureInformation {
        label: format!(
            "{enum_name}::{}({}) -> {enum_name}",
            variant.name,
            params.join(", ")
        ),
        parameters: params
            .into_iter()
            .map(|label| ParameterInformation { label })
            .collect(),
    }
}

fn signature_from_named_params(
    name: &str,
    params: Vec<(String, &Ty)>,
    ret: &Ty,
    omit_first_param: bool,
) -> SignatureInformation {
    let params: Vec<String> = params
        .into_iter()
        .skip(usize::from(omit_first_param))
        .map(|(name, ty)| format!("{name}: {}", ty.kind))
        .collect();

    SignatureInformation {
        label: format!("{name}({}) -> {}", params.join(", "), ret.kind),
        parameters: params
            .into_iter()
            .map(|label| ParameterInformation { label })
            .collect(),
    }
}

fn signature_from_ty(
    name: &str,
    params: &[Ty],
    ret: &Ty,
    variadic: bool,
    omit_first_param: bool,
) -> SignatureInformation {
    let param_kinds: Vec<String> = params
        .iter()
        .skip(usize::from(omit_first_param))
        .map(|param| param.kind.to_string())
        .collect();

    signature_from_types(name, &param_kinds, &ret.kind.to_string(), variadic, false)
}

fn signature_from_types(
    name: &str,
    params: &[impl ToString],
    ret: &str,
    variadic: bool,
    omit_first_param: bool,
) -> SignatureInformation {
    let mut params: Vec<String> = params
        .iter()
        .skip(usize::from(omit_first_param))
        .map(ToString::to_string)
        .collect();

    let label = if variadic && !params.is_empty() {
        format!("{name}({}, ...) -> {ret}", params.join(", "))
    } else {
        format!("{name}({}) -> {ret}", params.join(", "))
    };

    SignatureInformation {
        label,
        parameters: params
            .drain(..)
            .map(|label| ParameterInformation { label })
            .collect(),
    }
}

fn rename_signature(mut signature: SignatureInformation, name: &str) -> SignatureInformation {
    if let Some((_, rest)) = signature.label.split_once('(') {
        signature.label = format!("{name}({rest}");
    } else {
        signature.label = name.to_string();
    }

    signature
}

fn clamp_active_parameter(signature: &SignatureInformation, active_parameter: u32) -> u32 {
    let last = signature.parameters.len().saturating_sub(1) as u32;
    active_parameter.min(last)
}

fn type_name_for_expr(expr: &hir::Expr) -> Option<String> {
    let name = expr.ty.kind.to_string();
    if name == "unknown" { None } else { Some(name) }
}

fn find_innermost_call(block: &hir::Block, offset: u32) -> Option<&hir::CallExpression> {
    let mut best = None;

    for stmt in &block.stmts {
        update_best_call(&mut best, find_call_in_stmt(stmt, offset));
    }

    if let Some(expr) = &block.expr {
        update_best_call(&mut best, find_call_in_expr(expr, offset));
    }

    best
}

fn find_call_in_stmt(stmt: &hir::Stmt, offset: u32) -> Option<&hir::CallExpression> {
    if !span_contains_offset(&stmt.span, offset) {
        return None;
    }

    match &stmt.kind {
        hir::StmtKind::Expr(expr) => find_call_in_expr(expr, offset),
        hir::StmtKind::Let(_, init, _) | hir::StmtKind::Const(_, _, init, _) => {
            find_call_in_expr(init, offset)
        }
        hir::StmtKind::Return(Some(expr)) => find_call_in_expr(expr, offset),
        hir::StmtKind::FunctionDeclaration(decl) => find_innermost_call(&decl.body, offset),
        hir::StmtKind::ImplBlock(impl_block) => impl_block
            .methods
            .iter()
            .find_map(|decl| find_innermost_call(&decl.body, offset)),
        hir::StmtKind::ProtocolDeclaration(decl) => decl
            .methods
            .iter()
            .filter_map(|method| method.body.as_ref())
            .find_map(|body| find_innermost_call(body, offset)),
        hir::StmtKind::DynFunctionDeclaration(_)
        | hir::StmtKind::EnumDeclaration(_)
        | hir::StmtKind::StructDeclaration(_)
        | hir::StmtKind::Return(None) => None,
    }
}

fn find_call_in_expr(expr: &hir::Expr, offset: u32) -> Option<&hir::CallExpression> {
    if !span_contains_offset(&expr.span, offset) {
        return None;
    }

    let mut best = None;

    match &expr.kind {
        hir::ExprKind::Call(call) | hir::ExprKind::TailCall(call) => {
            update_best_call(&mut best, find_call_in_expr(&call.callee, offset));
            for arg in &call.arguments {
                update_best_call(&mut best, find_call_in_expr(arg, offset));
            }

            if best.is_none() {
                best = Some(call.as_ref());
            }
        }
        hir::ExprKind::Block(block) | hir::ExprKind::Loop(block) => {
            update_best_call(&mut best, find_innermost_call(block, offset));
        }
        hir::ExprKind::FunctionExpression(decl) => {
            update_best_call(&mut best, find_innermost_call(&decl.body, offset));
        }
        hir::ExprKind::IfElse(cond, then_block, else_clauses) => {
            update_best_call(&mut best, find_call_in_expr(cond, offset));
            update_best_call(&mut best, find_innermost_call(then_block, offset));
            for clause in else_clauses {
                if let Some(cond) = &clause.condition {
                    update_best_call(&mut best, find_call_in_expr(cond, offset));
                }
                update_best_call(&mut best, find_innermost_call(&clause.consequence, offset));
            }
        }
        hir::ExprKind::Binary(_, lhs, rhs) => {
            update_best_call(&mut best, find_call_in_expr(lhs, offset));
            update_best_call(&mut best, find_call_in_expr(rhs, offset));
        }
        hir::ExprKind::Unary(_, inner)
        | hir::ExprKind::FieldAccess(inner, _)
        | hir::ExprKind::Cast(inner, _)
        | hir::ExprKind::TryCast(inner, _)
        | hir::ExprKind::Break(Some(inner))
        | hir::ExprKind::Let(_, inner)
        | hir::ExprKind::Implements(inner, _) => {
            update_best_call(&mut best, find_call_in_expr(inner, offset));
        }
        hir::ExprKind::Match(scrutinee, arms) => {
            update_best_call(&mut best, find_call_in_expr(scrutinee, offset));
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    update_best_call(&mut best, find_call_in_expr(guard, offset));
                }
                update_best_call(&mut best, find_innermost_call(&arm.block, offset));
            }
        }
        hir::ExprKind::IndexAccess(base, index) => {
            update_best_call(&mut best, find_call_in_expr(base, offset));
            update_best_call(&mut best, find_call_in_expr(index, offset));
        }
        hir::ExprKind::List(items) => {
            for item in items {
                update_best_call(&mut best, find_call_in_expr(item, offset));
            }
        }
        hir::ExprKind::Dict(entries) => {
            for (key, value) in entries {
                update_best_call(&mut best, find_call_in_expr(key, offset));
                update_best_call(&mut best, find_call_in_expr(value, offset));
            }
        }
        hir::ExprKind::TaggedString { tag, exprs, .. } => {
            update_best_call(&mut best, find_call_in_expr(tag, offset));
            for expr in exprs {
                update_best_call(&mut best, find_call_in_expr(expr, offset));
            }
        }
        hir::ExprKind::Range(range) => {
            update_best_call(&mut best, find_call_in_expr(&range.start, offset));
            update_best_call(&mut best, find_call_in_expr(&range.end, offset));
        }
        hir::ExprKind::Literal(_)
        | hir::ExprKind::Path(_)
        | hir::ExprKind::Continue
        | hir::ExprKind::Break(None)
        | hir::ExprKind::Wildcard => {}
    }

    best
}

fn update_best_call<'a>(
    best: &mut Option<&'a hir::CallExpression>,
    candidate: Option<&'a hir::CallExpression>,
) {
    let Some(candidate) = candidate else {
        return;
    };

    match best {
        Some(current) if call_span_len(candidate) >= call_span_len(current) => {}
        _ => *best = Some(candidate),
    }
}

fn call_span_len(call: &hir::CallExpression) -> u32 {
    call_span_end_hint(call).saturating_sub(call.callee.span.start)
}

fn span_contains_offset(span: &Span, offset: u32) -> bool {
    span.start <= offset && offset <= span.end
}

fn find_call_open_paren(source: &str, call: &hir::CallExpression) -> Option<u32> {
    let start = (call.callee.span.end as usize).min(source.len());
    let end = (call_span_end_hint(call) as usize).min(source.len());
    if start >= end {
        return None;
    }

    for (offset, ch) in source[start..end].char_indices() {
        if ch == '(' {
            return Some((start + offset) as u32);
        }
    }

    None
}

fn active_parameter(source: &str, start: u32, cursor: u32) -> u32 {
    let start = start.min(source.len() as u32) as usize;
    let cursor = cursor.min(source.len() as u32) as usize;
    let mut active = 0u32;
    let mut paren_depth = 0u32;
    let mut bracket_depth = 0u32;
    let mut brace_depth = 0u32;
    let mut in_string = false;
    let mut escaped = false;

    for ch in source[start..cursor].chars() {
        if in_string {
            if escaped {
                escaped = false;
                continue;
            }
            match ch {
                '\\' => escaped = true,
                '"' => in_string = false,
                _ => {}
            }
            continue;
        }

        match ch {
            '"' => in_string = true,
            '(' => paren_depth += 1,
            ')' => paren_depth = paren_depth.saturating_sub(1),
            '[' => bracket_depth += 1,
            ']' => bracket_depth = bracket_depth.saturating_sub(1),
            '{' => brace_depth += 1,
            '}' => brace_depth = brace_depth.saturating_sub(1),
            ',' if paren_depth == 0 && bracket_depth == 0 && brace_depth == 0 => active += 1,
            _ => {}
        }
    }

    active
}

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

fn call_span_end_hint(call: &hir::CallExpression) -> u32 {
    call.arguments
        .last()
        .map_or(call.callee.span.end, |arg| arg.span.end)
        .saturating_add(2)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{CompilationTarget, analyze_for_target};

    fn typed_hir(source: &str) -> TypedHir {
        let result = analyze_for_target(source, CompilationTarget::Js);
        crate::inlay_hints::lower_and_typecheck(&result)
            .expect("lowering/typechecking should succeed")
    }

    #[test]
    fn signature_help_for_function_call_uses_parameter_names() {
        let source = "fn add(a: i64, b: i64) -> i64 { a + b }\nlet _ = add(1, 2);";
        let typed_hir = typed_hir(source);
        let help = signature_help_at(source, &typed_hir, 1, 15).expect("signature help");

        assert_eq!(help.active_parameter, 1);
        assert_eq!(help.signatures[0].label, "add(a: i64, b: i64) -> i64");
        assert_eq!(help.signatures[0].parameters[1].label, "b: i64");
    }

    #[test]
    fn signature_help_for_builtin_variadic_clamps_active_parameter() {
        let source = "let x = math::min(1.0, 2.0, 3.0);";
        let typed_hir = typed_hir(source);
        let help = signature_help_at(source, &typed_hir, 0, 28).expect("signature help");

        assert_eq!(help.active_parameter, 0);
        assert_eq!(help.signatures[0].label, "math::min(f64, ...) -> f64");
    }

    #[test]
    fn signature_help_for_dot_method_omits_self_parameter() {
        let source = "struct Vector { x: i64 }\nfn Vector.add(self, other: Vector) -> Vector { self }\nlet v1 = Vector { x: 1 };\nlet v2 = Vector { x: 2 };\nlet _ = v1.add(v2);";
        let typed_hir = typed_hir(source);
        let help = signature_help_at(source, &typed_hir, 4, 16).expect("signature help");

        assert_eq!(
            help.signatures[0].label,
            "Vector.add(other: Vector) -> Vector"
        );
        assert_eq!(help.signatures[0].parameters.len(), 1);
        assert_eq!(help.signatures[0].parameters[0].label, "other: Vector");
    }

    #[test]
    fn signature_help_for_protocol_method_uses_qualified_name() {
        let source = r#"
protocol Greet {
    fn greet(self) -> String
}

enum Animal {
    Dog,
}

impl Greet for Animal {
    fn greet(self) -> String { "Woof!" }
}

let dog = Animal::Dog;
let _ = Greet::greet(dog);
"#;
        let typed_hir = typed_hir(source);
        let help = signature_help_at(source, &typed_hir, 14, 24).expect("signature help");

        assert_eq!(
            help.signatures[0].label,
            "Greet::greet(self: unknown) -> String"
        );
    }
}
