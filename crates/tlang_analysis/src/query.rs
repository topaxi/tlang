//! High-level symbol query API for hover and goto-definition.
//!
//! This module provides [`resolve_symbol`] which combines the AST node finder
//! and symbol index to resolve the symbol under a cursor position.  Both the
//! LSP server and the WASM playground bindings share this implementation to
//! ensure consistent behaviour.

use std::collections::HashMap;

use tlang_ast::node as ast;
use tlang_ast::token::{CommentKind, CommentToken, Literal};
use tlang_defs::DefKind;
use tlang_hir as hir;
use tlang_span::{HirId, NodeId, Span, TypeVarId};
use tlang_typeck::builtins;

use crate::find_node;
use crate::inlay_hints;
use crate::symbol_index::SymbolIndex;
use crate::typed_hir::TypedHir;

/// Information about a symbol resolved from a cursor position.
#[derive(Debug)]
pub struct ResolvedSymbol {
    /// The identifier name as written in source.
    pub name: String,
    /// The fully qualified symbol name stored in the symbol index.
    pub qualified_name: String,
    /// The span of the identifier under the cursor.
    pub ident_span: Span,
    /// The kind of the resolved definition.
    pub def_kind: DefKind,
    /// The span where the symbol was defined.
    pub def_span: Span,
    /// Whether the symbol is a builtin (no source location to jump to).
    pub builtin: bool,
    /// Optional inferred type string (e.g. `"i64"`, `"Vector"`).
    pub type_info: Option<String>,
    /// Optional AST node id of the definition.
    pub node_id: Option<NodeId>,
    /// Optional HIR id of the definition.
    pub hir_id: Option<HirId>,
    /// Optional declaration-like hover signature for callables.
    pub signature: Option<String>,
    /// Optional rendered documentation for the definition.
    pub documentation: Option<String>,
}

impl ResolvedSymbol {
    /// Format the hover text for this symbol.
    ///
    /// When type information is available, includes it:
    /// `(variable) v1: Vector` or `(function) add/2`.
    pub fn hover_text(&self) -> String {
        if let Some(signature) = &self.signature {
            if let Some(documentation) = &self.documentation {
                return format!("{signature}\n\n{documentation}");
            }
            return signature.clone();
        }

        let kind_label = self.def_kind.to_string();
        if let Some(arity) = self.def_kind.arity() {
            if arity == u16::MAX {
                format!("({kind_label}) {name}/*", name = self.name)
            } else {
                format!("({kind_label}) {name}/{arity}", name = self.name)
            }
        } else if let Some(ty) = &self.type_info {
            format!("({kind_label}) {name}: {ty}", name = self.name)
        } else {
            format!("({kind_label}) {name}", name = self.name)
        }
    }
}

/// Populate hover-specific type, signature, and documentation details.
pub fn enrich_hover_symbol(
    module: &ast::Module,
    typed_hir: Option<&TypedHir>,
    symbol: &mut ResolvedSymbol,
) {
    if let Some(typed_hir) = typed_hir
        && !symbol.builtin
    {
        let def_line = symbol.def_span.start_lc.line;
        let def_col = if def_line > 0 {
            symbol.def_span.start_lc.column.saturating_sub(1)
        } else {
            symbol.def_span.start_lc.column
        };
        symbol.type_info = inlay_hints::type_at_definition(typed_hir, def_line, def_col);
    }

    let Some(callable) = find_ast_callable(
        module,
        symbol.node_id,
        &symbol.qualified_name,
        symbol.def_kind.arity(),
    ) else {
        if symbol.builtin
            && let Some(signature) = builtins::lookup_hover_signature(&symbol.qualified_name)
        {
            symbol.signature = Some(format_builtin_signature(signature));
            symbol.documentation = signature.documentation.map(str::to_string);
        }
        return;
    };

    symbol.documentation = extract_doc_comments(callable.leading_comments());
    symbol.signature = Some(format_callable_signature(
        &callable,
        typed_hir,
        symbol.hir_id,
    ));
}

fn format_builtin_signature(signature: &builtins::BuiltinHoverSignature) -> String {
    let params = signature
        .params
        .iter()
        .enumerate()
        .map(|(index, (name, ty))| {
            if signature.variadic && index == signature.params.len().saturating_sub(1) {
                format!("{name}: {ty}...")
            } else {
                format!("{name}: {ty}")
            }
        })
        .collect::<Vec<_>>()
        .join(", ");

    format!("fn {}({params}) -> {}", signature.name, signature.ret)
}

/// Resolve the symbol under the cursor at the given **0-based** `(line, column)`.
///
/// The column is automatically adjusted for the lexer's coordinate system
/// (line 0 uses 0-based columns, subsequent lines use 1-based columns).
///
/// Returns `None` when the cursor is not on an identifier or the identifier
/// cannot be resolved in the symbol index.
pub fn resolve_symbol(
    module: &ast::Module,
    index: &SymbolIndex,
    line: u32,
    column_0based: u32,
) -> Option<ResolvedSymbol> {
    // The lexer uses 0-based columns on line 0 but 1-based columns on
    // subsequent lines (current_column resets to 1 after '\n').  Editor
    // positions are always 0-based, so adjust here.
    let lexer_column = if line > 0 {
        column_0based + 1
    } else {
        column_0based
    };

    let found = find_node::find_node_at_position(module, line, lexer_column)?;

    // Look up the symbol in the scope's symbol table.
    let entry = index
        .get_closest_by_name(found.scope_id, &found.name, found.span)
        .or_else(|| {
            // When the cursor is on a field of a field-access expression
            // (e.g. `v1.add`), the bare name `add` may not be in scope.
            // Try to resolve the base variable to its defining type and
            // attempt a qualified lookup like `Vector::add`.
            let base_name = found.field_base.as_deref()?;
            let base_entry = index.get_closest_by_name(found.scope_id, base_name, found.span)?;

            // The base must be a struct or enum to attempt qualified method lookup.
            if !matches!(base_entry.kind, DefKind::Struct | DefKind::Enum) {
                // The base is a variable/parameter — try to find its type
                // by looking for qualified names ending with `::member_name`.
                return index.find_member_by_suffix(found.scope_id, &found.name, found.span);
            }

            // Base is a type name itself (e.g. `Vector.add` or `Option.map`)
            let qualified = format!("{}::{}", base_name, found.name);
            index.get_closest_by_name(found.scope_id, &qualified, found.span)
        })
        .or_else(|| {
            // Fallback for hovering on a member/field name in a declaration
            // context (e.g. `to_string` in `protocol Display { fn to_string(…) }`
            // or `x` in `struct Vector { x: i64 }`).  The bare name is not in the
            // symbol table but the qualified name (`Display::to_string`,
            // `Vector::x`) is.
            //
            // Only attempt this when `find_node` explicitly flagged the identifier
            // as being in a declaration-name context, to avoid mis-resolving
            // undefined locals to an unrelated `Type::member` in scope.
            if found.is_declaration_name {
                index.find_member_by_suffix(found.scope_id, &found.name, found.span)
            } else {
                None
            }
        })?;

    Some(ResolvedSymbol {
        name: found.name,
        qualified_name: entry.name.to_string(),
        ident_span: found.span,
        def_kind: entry.kind,
        def_span: entry.defined_at,
        builtin: entry.builtin,
        type_info: None,
        node_id: entry.node_id,
        hir_id: entry.hir_id,
        signature: None,
        documentation: None,
    })
}

enum AstCallableDecl<'a> {
    Function {
        decl: &'a ast::FunctionDeclaration,
        leading_comments: &'a [CommentToken],
    },
    ProtocolMethod {
        protocol: &'a ast::ProtocolDeclaration,
        method: &'a ast::ProtocolMethodSignature,
    },
}

impl AstCallableDecl<'_> {
    fn leading_comments(&self) -> &[CommentToken] {
        match self {
            AstCallableDecl::Function {
                leading_comments, ..
            } => leading_comments,
            AstCallableDecl::ProtocolMethod { method, .. } => &method.leading_comments,
        }
    }

    fn name_span(&self) -> Span {
        match self {
            AstCallableDecl::Function { decl, .. } => decl.name.span,
            AstCallableDecl::ProtocolMethod { method, .. } => method.name.span,
        }
    }
}

enum HirCallableDecl<'a> {
    Function(&'a hir::FunctionDeclaration),
    ProtocolMethod {
        protocol: &'a hir::ProtocolDeclaration,
        method: &'a hir::ProtocolMethodSignature,
    },
}

fn find_ast_callable<'a>(
    module: &'a ast::Module,
    node_id: Option<NodeId>,
    qualified_name: &str,
    expected_arity: Option<u16>,
) -> Option<AstCallableDecl<'a>> {
    for stmt in &module.statements {
        if let Some(callable) =
            find_ast_callable_in_stmt(stmt, node_id, qualified_name, expected_arity)
        {
            return Some(callable);
        }
    }
    None
}

fn find_ast_callable_in_stmt<'a>(
    stmt: &'a ast::Stmt,
    node_id: Option<NodeId>,
    qualified_name: &str,
    expected_arity: Option<u16>,
) -> Option<AstCallableDecl<'a>> {
    match &stmt.kind {
        ast::StmtKind::FunctionDeclaration(decl) => {
            if matches_function_decl(decl, node_id, qualified_name, expected_arity) {
                Some(AstCallableDecl::Function {
                    decl,
                    leading_comments: if decl.leading_comments.is_empty() {
                        &stmt.leading_comments
                    } else {
                        &decl.leading_comments
                    },
                })
            } else {
                None
            }
        }
        ast::StmtKind::FunctionDeclarations(decls) => decls
            .iter()
            .filter(|decl| {
                matches_grouped_function_decl(decl, node_id, qualified_name, expected_arity)
            })
            .max_by_key(|decl| informative_parameter_count(decl))
            .map(|decl| AstCallableDecl::Function {
                decl,
                leading_comments: if decl.leading_comments.is_empty() {
                    &stmt.leading_comments
                } else {
                    &decl.leading_comments
                },
            }),
        ast::StmtKind::ImplBlock(impl_block) => impl_block.methods.iter().find_map(|decl| {
            matches_function_decl(decl, node_id, qualified_name, expected_arity).then_some(
                AstCallableDecl::Function {
                    decl,
                    leading_comments: &decl.leading_comments,
                },
            )
        }),
        ast::StmtKind::ProtocolDeclaration(protocol) => {
            protocol.methods.iter().find_map(|method| {
                let protocol_qualified_name = format!("{}::{}", protocol.name, method.name);
                (node_id == Some(method.id) || protocol_qualified_name == qualified_name)
                    .then_some(AstCallableDecl::ProtocolMethod { protocol, method })
            })
        }
        _ => None,
    }
}

fn matches_function_decl(
    decl: &ast::FunctionDeclaration,
    node_id: Option<NodeId>,
    qualified_name: &str,
    expected_arity: Option<u16>,
) -> bool {
    node_id == Some(decl.id)
        || (decl.name_or_invalid() == qualified_name
            && expected_arity.is_none_or(|arity| decl.parameters.len() as u16 == arity))
}

fn matches_grouped_function_decl(
    decl: &ast::FunctionDeclaration,
    node_id: Option<NodeId>,
    qualified_name: &str,
    expected_arity: Option<u16>,
) -> bool {
    if expected_arity.is_some() {
        decl.name_or_invalid() == qualified_name
            && expected_arity.is_none_or(|arity| decl.parameters.len() as u16 == arity)
    } else {
        matches_function_decl(decl, node_id, qualified_name, expected_arity)
    }
}

fn informative_parameter_count(decl: &ast::FunctionDeclaration) -> usize {
    decl.parameters
        .iter()
        .filter(|param| !is_uninformative_param_pattern(&param.pattern))
        .count()
}

fn is_uninformative_param_pattern(pattern: &ast::Pat) -> bool {
    pattern.is_wildcard()
        || matches!(
            &pattern.kind,
            ast::PatKind::Identifier(ident) if ident.is_wildcard()
        )
}

fn find_hir_callable<'a>(
    typed_hir: &'a TypedHir,
    hir_id: Option<HirId>,
) -> Option<HirCallableDecl<'a>> {
    let hir_id = hir_id?;
    find_hir_callable_in_block(&typed_hir.module.block, hir_id)
}

fn find_hir_callable_by_span<'a>(
    typed_hir: &'a TypedHir,
    name_span: Span,
) -> Option<HirCallableDecl<'a>> {
    find_hir_callable_in_block_by_span(&typed_hir.module.block, name_span)
}

fn find_hir_callable_in_block(block: &hir::Block, hir_id: HirId) -> Option<HirCallableDecl<'_>> {
    for stmt in &block.stmts {
        if let Some(callable) = find_hir_callable_in_stmt(stmt, hir_id) {
            return Some(callable);
        }
    }
    block
        .expr
        .as_ref()
        .and_then(|expr| find_hir_callable_in_expr(expr, hir_id))
}

fn find_hir_callable_in_stmt(stmt: &hir::Stmt, hir_id: HirId) -> Option<HirCallableDecl<'_>> {
    match &stmt.kind {
        hir::StmtKind::FunctionDeclaration(decl) if decl.hir_id == hir_id => {
            Some(HirCallableDecl::Function(decl))
        }
        hir::StmtKind::FunctionDeclaration(decl) => find_hir_callable_in_block(&decl.body, hir_id),
        hir::StmtKind::ImplBlock(impl_block) => impl_block.methods.iter().find_map(|decl| {
            if decl.hir_id == hir_id {
                Some(HirCallableDecl::Function(decl))
            } else {
                find_hir_callable_in_block(&decl.body, hir_id)
            }
        }),
        hir::StmtKind::ProtocolDeclaration(protocol) => {
            protocol.methods.iter().find_map(|method| {
                if method.hir_id == hir_id {
                    Some(HirCallableDecl::ProtocolMethod { protocol, method })
                } else {
                    method
                        .body
                        .as_ref()
                        .and_then(|body| find_hir_callable_in_block(body, hir_id))
                }
            })
        }
        hir::StmtKind::Expr(expr) => find_hir_callable_in_expr(expr, hir_id),
        _ => None,
    }
}

fn find_hir_callable_in_expr(expr: &hir::Expr, hir_id: HirId) -> Option<HirCallableDecl<'_>> {
    match &expr.kind {
        hir::ExprKind::FunctionExpression(decl) if decl.hir_id == hir_id => {
            Some(HirCallableDecl::Function(decl))
        }
        hir::ExprKind::FunctionExpression(decl) => find_hir_callable_in_block(&decl.body, hir_id),
        hir::ExprKind::Block(block) | hir::ExprKind::Loop(block) => {
            find_hir_callable_in_block(block, hir_id)
        }
        hir::ExprKind::IfElse(condition, then_block, else_clauses) => {
            find_hir_callable_in_expr(condition, hir_id)
                .or_else(|| find_hir_callable_in_block(then_block, hir_id))
                .or_else(|| {
                    else_clauses.iter().find_map(|clause| {
                        clause
                            .condition
                            .as_ref()
                            .and_then(|condition| find_hir_callable_in_expr(condition, hir_id))
                            .or_else(|| find_hir_callable_in_block(&clause.consequence, hir_id))
                    })
                })
        }
        hir::ExprKind::Call(call) | hir::ExprKind::TailCall(call) => {
            find_hir_callable_in_expr(&call.callee, hir_id).or_else(|| {
                call.arguments
                    .iter()
                    .find_map(|argument| find_hir_callable_in_expr(argument, hir_id))
            })
        }
        hir::ExprKind::Binary(_, lhs, rhs) => find_hir_callable_in_expr(lhs, hir_id)
            .or_else(|| find_hir_callable_in_expr(rhs, hir_id)),
        hir::ExprKind::Unary(_, inner)
        | hir::ExprKind::FieldAccess(inner, _)
        | hir::ExprKind::Cast(inner, _)
        | hir::ExprKind::TryCast(inner, _)
        | hir::ExprKind::Break(Some(inner))
        | hir::ExprKind::Let(_, inner)
        | hir::ExprKind::Implements(inner, _) => find_hir_callable_in_expr(inner, hir_id),
        hir::ExprKind::Match(scrutinee, arms, _) => find_hir_callable_in_expr(scrutinee, hir_id)
            .or_else(|| {
                arms.iter()
                    .find_map(|arm| find_hir_callable_in_block(&arm.block, hir_id))
            }),
        hir::ExprKind::IndexAccess(base, index) => find_hir_callable_in_expr(base, hir_id)
            .or_else(|| find_hir_callable_in_expr(index, hir_id)),
        hir::ExprKind::List(items) => items
            .iter()
            .find_map(|item| find_hir_callable_in_expr(item, hir_id)),
        hir::ExprKind::Dict(entries) => entries.iter().find_map(|(key, value)| {
            find_hir_callable_in_expr(key, hir_id)
                .or_else(|| find_hir_callable_in_expr(value, hir_id))
        }),
        hir::ExprKind::TaggedString { tag, exprs, .. } => find_hir_callable_in_expr(tag, hir_id)
            .or_else(|| {
                exprs
                    .iter()
                    .find_map(|expr| find_hir_callable_in_expr(expr, hir_id))
            }),
        hir::ExprKind::Range(range) => find_hir_callable_in_expr(&range.start, hir_id)
            .or_else(|| find_hir_callable_in_expr(&range.end, hir_id)),
        hir::ExprKind::Literal(_)
        | hir::ExprKind::Path(_)
        | hir::ExprKind::Continue
        | hir::ExprKind::Break(None)
        | hir::ExprKind::Wildcard => None,
    }
}

fn find_hir_callable_in_block_by_span(
    block: &hir::Block,
    name_span: Span,
) -> Option<HirCallableDecl<'_>> {
    for stmt in &block.stmts {
        if let Some(callable) = find_hir_callable_in_stmt_by_span(stmt, name_span) {
            return Some(callable);
        }
    }
    block
        .expr
        .as_ref()
        .and_then(|expr| find_hir_callable_in_expr_by_span(expr, name_span))
}

fn find_hir_callable_in_stmt_by_span(
    stmt: &hir::Stmt,
    name_span: Span,
) -> Option<HirCallableDecl<'_>> {
    match &stmt.kind {
        hir::StmtKind::FunctionDeclaration(decl) if decl.name.span == name_span => {
            Some(HirCallableDecl::Function(decl))
        }
        hir::StmtKind::FunctionDeclaration(decl) => {
            find_hir_callable_in_block_by_span(&decl.body, name_span)
        }
        hir::StmtKind::ImplBlock(impl_block) => impl_block.methods.iter().find_map(|decl| {
            if decl.name.span == name_span {
                Some(HirCallableDecl::Function(decl))
            } else {
                find_hir_callable_in_block_by_span(&decl.body, name_span)
            }
        }),
        hir::StmtKind::ProtocolDeclaration(protocol) => {
            protocol.methods.iter().find_map(|method| {
                if method.name.span == name_span {
                    Some(HirCallableDecl::ProtocolMethod { protocol, method })
                } else {
                    method
                        .body
                        .as_ref()
                        .and_then(|body| find_hir_callable_in_block_by_span(body, name_span))
                }
            })
        }
        hir::StmtKind::Expr(expr) => find_hir_callable_in_expr_by_span(expr, name_span),
        _ => None,
    }
}

fn find_hir_callable_in_expr_by_span(
    expr: &hir::Expr,
    name_span: Span,
) -> Option<HirCallableDecl<'_>> {
    match &expr.kind {
        hir::ExprKind::FunctionExpression(decl) if decl.name.span == name_span => {
            Some(HirCallableDecl::Function(decl))
        }
        hir::ExprKind::FunctionExpression(decl) => {
            find_hir_callable_in_block_by_span(&decl.body, name_span)
        }
        hir::ExprKind::Block(block) | hir::ExprKind::Loop(block) => {
            find_hir_callable_in_block_by_span(block, name_span)
        }
        hir::ExprKind::IfElse(condition, then_block, else_clauses) => {
            find_hir_callable_in_expr_by_span(condition, name_span)
                .or_else(|| find_hir_callable_in_block_by_span(then_block, name_span))
                .or_else(|| {
                    else_clauses.iter().find_map(|clause| {
                        clause
                            .condition
                            .as_ref()
                            .and_then(|condition| {
                                find_hir_callable_in_expr_by_span(condition, name_span)
                            })
                            .or_else(|| {
                                find_hir_callable_in_block_by_span(&clause.consequence, name_span)
                            })
                    })
                })
        }
        hir::ExprKind::Call(call) | hir::ExprKind::TailCall(call) => {
            find_hir_callable_in_expr_by_span(&call.callee, name_span).or_else(|| {
                call.arguments
                    .iter()
                    .find_map(|argument| find_hir_callable_in_expr_by_span(argument, name_span))
            })
        }
        hir::ExprKind::Binary(_, lhs, rhs) => find_hir_callable_in_expr_by_span(lhs, name_span)
            .or_else(|| find_hir_callable_in_expr_by_span(rhs, name_span)),
        hir::ExprKind::Unary(_, inner)
        | hir::ExprKind::FieldAccess(inner, _)
        | hir::ExprKind::Cast(inner, _)
        | hir::ExprKind::TryCast(inner, _)
        | hir::ExprKind::Break(Some(inner))
        | hir::ExprKind::Let(_, inner)
        | hir::ExprKind::Implements(inner, _) => {
            find_hir_callable_in_expr_by_span(inner, name_span)
        }
        hir::ExprKind::Match(scrutinee, arms, _) => {
            find_hir_callable_in_expr_by_span(scrutinee, name_span).or_else(|| {
                arms.iter()
                    .find_map(|arm| find_hir_callable_in_block_by_span(&arm.block, name_span))
            })
        }
        hir::ExprKind::IndexAccess(base, index) => {
            find_hir_callable_in_expr_by_span(base, name_span)
                .or_else(|| find_hir_callable_in_expr_by_span(index, name_span))
        }
        hir::ExprKind::List(items) => items
            .iter()
            .find_map(|item| find_hir_callable_in_expr_by_span(item, name_span)),
        hir::ExprKind::Dict(entries) => entries.iter().find_map(|(key, value)| {
            find_hir_callable_in_expr_by_span(key, name_span)
                .or_else(|| find_hir_callable_in_expr_by_span(value, name_span))
        }),
        hir::ExprKind::TaggedString { tag, exprs, .. } => {
            find_hir_callable_in_expr_by_span(tag, name_span).or_else(|| {
                exprs
                    .iter()
                    .find_map(|expr| find_hir_callable_in_expr_by_span(expr, name_span))
            })
        }
        hir::ExprKind::Range(range) => find_hir_callable_in_expr_by_span(&range.start, name_span)
            .or_else(|| find_hir_callable_in_expr_by_span(&range.end, name_span)),
        hir::ExprKind::Literal(_)
        | hir::ExprKind::Path(_)
        | hir::ExprKind::Continue
        | hir::ExprKind::Break(None)
        | hir::ExprKind::Wildcard => None,
    }
}

fn format_callable_signature(
    callable: &AstCallableDecl<'_>,
    typed_hir: Option<&TypedHir>,
    hir_id: Option<HirId>,
) -> String {
    let hir_callable = typed_hir.and_then(|typed_hir| {
        find_hir_callable(typed_hir, hir_id)
            .or_else(|| find_hir_callable_by_span(typed_hir, callable.name_span()))
    });

    match callable {
        AstCallableDecl::Function { decl, .. } => {
            format_function_signature(decl, typed_hir, hir_callable.as_ref())
        }
        AstCallableDecl::ProtocolMethod { protocol, method } => {
            format_protocol_method_signature(protocol, method, typed_hir, hir_callable.as_ref())
        }
    }
}

fn format_function_signature(
    decl: &ast::FunctionDeclaration,
    typed_hir: Option<&TypedHir>,
    hir_callable: Option<&HirCallableDecl<'_>>,
) -> String {
    match hir_callable {
        Some(HirCallableDecl::Function(hir_decl)) => {
            let type_var_names = hir_function_type_var_names(hir_decl);
            let params = decl
                .parameters
                .iter()
                .enumerate()
                .map(|(index, param)| {
                    format_parameter(param, hir_decl.parameters.get(index), &type_var_names)
                })
                .collect::<Vec<_>>()
                .join(", ");
            let return_kind = typed_hir
                .and_then(|typed_hir| inferred_function_return_ty(typed_hir, hir_decl))
                .unwrap_or(&hir_decl.return_type.kind);

            format!(
                "fn {}({params}) -> {}",
                decl.name_or_invalid(),
                format_hir_ty_kind(return_kind, &type_var_names)
            )
        }
        _ => {
            let params = decl
                .parameters
                .iter()
                .map(format_parameter_from_ast)
                .collect::<Vec<_>>()
                .join(", ");

            format!(
                "fn {}({params}) -> {}",
                decl.name_or_invalid(),
                decl.return_type_annotation
                    .as_ref()
                    .map_or_else(|| "unknown".to_string(), format_ast_ty)
            )
        }
    }
}

fn format_protocol_method_signature(
    protocol: &ast::ProtocolDeclaration,
    method: &ast::ProtocolMethodSignature,
    typed_hir: Option<&TypedHir>,
    hir_callable: Option<&HirCallableDecl<'_>>,
) -> String {
    match hir_callable {
        Some(HirCallableDecl::ProtocolMethod {
            protocol: hir_protocol,
            method: hir_method,
        }) => {
            let type_var_names = hir_protocol_method_type_var_names(hir_protocol, hir_method);
            let params = method
                .parameters
                .iter()
                .enumerate()
                .map(|(index, param)| {
                    format_parameter(param, hir_method.parameters.get(index), &type_var_names)
                })
                .collect::<Vec<_>>()
                .join(", ");
            let return_kind = typed_hir
                .and_then(|typed_hir| inferred_protocol_method_return_ty(typed_hir, hir_method))
                .unwrap_or(&hir_method.return_type.kind);

            format!(
                "fn {}.{}({params}) -> {}",
                protocol.name,
                method.name,
                format_hir_ty_kind(return_kind, &type_var_names)
            )
        }
        _ => {
            let params = method
                .parameters
                .iter()
                .map(format_parameter_from_ast)
                .collect::<Vec<_>>()
                .join(", ");

            format!(
                "fn {}.{}({params}) -> {}",
                protocol.name,
                method.name,
                method
                    .return_type_annotation
                    .as_ref()
                    .map_or_else(|| "unknown".to_string(), format_ast_ty)
            )
        }
    }
}

fn inferred_function_return_ty<'a>(
    typed_hir: &'a TypedHir,
    decl: &'a hir::FunctionDeclaration,
) -> Option<&'a hir::TyKind> {
    (!matches!(decl.return_type.kind, hir::TyKind::Unknown))
        .then_some(&decl.return_type.kind)
        .or_else(|| {
            typed_hir
                .type_table
                .get(&decl.hir_id)
                .and_then(|info| match &info.ty.kind {
                    hir::TyKind::Fn(_, ret) => Some(&ret.kind),
                    _ => None,
                })
        })
        .or_else(|| decl.body.expr.as_ref().map(|expr| &expr.ty.kind))
}

fn inferred_protocol_method_return_ty<'a>(
    typed_hir: &'a TypedHir,
    method: &'a hir::ProtocolMethodSignature,
) -> Option<&'a hir::TyKind> {
    (!matches!(method.return_type.kind, hir::TyKind::Unknown))
        .then_some(&method.return_type.kind)
        .or_else(|| {
            typed_hir
                .type_table
                .get(&method.hir_id)
                .and_then(|info| match &info.ty.kind {
                    hir::TyKind::Fn(_, ret) => Some(&ret.kind),
                    _ => None,
                })
        })
        .or_else(|| {
            method
                .body
                .as_ref()
                .and_then(|body| body.expr.as_ref().map(|expr| &expr.ty.kind))
        })
}

fn format_parameter(
    param: &ast::FunctionParameter,
    hir_param: Option<&hir::FunctionParameter>,
    type_var_names: &HashMap<TypeVarId, String>,
) -> String {
    if is_self_parameter(param) {
        return "self".to_string();
    }

    let pat = preferred_parameter_name(param, hir_param);
    let ty = hir_param
        .map(|param| param.type_annotation.kind.clone())
        .unwrap_or(hir::TyKind::Unknown);

    format!("{pat}: {}", format_hir_ty_kind(&ty, type_var_names))
}

fn format_parameter_from_ast(param: &ast::FunctionParameter) -> String {
    if is_self_parameter(param) {
        return "self".to_string();
    }

    let pat = format_ast_pat(&param.pattern);
    let ty = param
        .type_annotation
        .as_ref()
        .map_or_else(|| "unknown".to_string(), format_ast_ty);

    format!("{pat}: {ty}")
}

fn is_self_parameter(param: &ast::FunctionParameter) -> bool {
    matches!(param.pattern.kind, ast::PatKind::_Self)
}

fn preferred_parameter_name(
    param: &ast::FunctionParameter,
    hir_param: Option<&hir::FunctionParameter>,
) -> String {
    match &param.pattern.kind {
        ast::PatKind::Identifier(ident) if !ident.is_wildcard() => ident.to_string(),
        ast::PatKind::_Self => "self".to_string(),
        _ => hir_param
            .map(|param| param.name.to_string())
            .unwrap_or_else(|| format_ast_pat(&param.pattern)),
    }
}

fn hir_function_type_var_names(decl: &hir::FunctionDeclaration) -> HashMap<TypeVarId, String> {
    decl.all_type_params()
        .map(|type_param| (type_param.type_var_id, type_param.name.to_string()))
        .collect()
}

fn hir_protocol_method_type_var_names(
    protocol: &hir::ProtocolDeclaration,
    method: &hir::ProtocolMethodSignature,
) -> HashMap<TypeVarId, String> {
    protocol
        .type_params
        .iter()
        .chain(method.type_params.iter())
        .map(|type_param| (type_param.type_var_id, type_param.name.to_string()))
        .collect()
}

fn format_hir_ty_kind(ty: &hir::TyKind, type_var_names: &HashMap<TypeVarId, String>) -> String {
    match ty {
        hir::TyKind::Unknown => "unknown".to_string(),
        hir::TyKind::Primitive(prim) => prim.to_string(),
        hir::TyKind::Fn(params, ret) => format!(
            "fn({}) -> {}",
            params
                .iter()
                .map(|param| format_hir_ty_kind(&param.kind, type_var_names))
                .collect::<Vec<_>>()
                .join(", "),
            format_hir_ty_kind(&ret.kind, type_var_names)
        ),
        hir::TyKind::List(inner) => {
            format!("List<{}>", format_hir_ty_kind(&inner.kind, type_var_names))
        }
        hir::TyKind::Slice(inner) => {
            format!("Slice<{}>", format_hir_ty_kind(&inner.kind, type_var_names))
        }
        hir::TyKind::Dict(key, value) => format!(
            "Dict<{}, {}>",
            format_hir_ty_kind(&key.kind, type_var_names),
            format_hir_ty_kind(&value.kind, type_var_names)
        ),
        hir::TyKind::Never => "never".to_string(),
        hir::TyKind::Var(id) => type_var_names
            .get(id)
            .cloned()
            .unwrap_or_else(|| format!("?{id}")),
        hir::TyKind::Path(path, type_args) => {
            if type_args.is_empty() {
                path.to_string()
            } else {
                format!(
                    "{path}<{}>",
                    type_args
                        .iter()
                        .map(|ty| format_hir_ty_kind(&ty.kind, type_var_names))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
        }
        hir::TyKind::Union(types) => types
            .iter()
            .map(|ty| format_hir_ty_kind(&ty.kind, type_var_names))
            .collect::<Vec<_>>()
            .join(" | "),
    }
}

fn format_ast_ty(ty: &ast::Ty) -> String {
    match &ty.kind {
        ast::TyKind::Unknown => "unknown".to_string(),
        ast::TyKind::Path(path) => {
            if ty.parameters.is_empty() {
                path.to_string()
            } else {
                format!(
                    "{path}<{}>",
                    ty.parameters
                        .iter()
                        .map(format_ast_ty)
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
        }
        ast::TyKind::Union(paths) => paths
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>()
            .join(" | "),
        ast::TyKind::Fn(params, ret) => format!(
            "fn({}) -> {}",
            params
                .iter()
                .map(|param| match &param.name {
                    Some(name) => format!("{name}: {}", format_ast_ty(&param.ty)),
                    None => format_ast_ty(&param.ty),
                })
                .collect::<Vec<_>>()
                .join(", "),
            format_ast_ty(ret)
        ),
    }
}

fn format_ast_pat(pat: &ast::Pat) -> String {
    match &pat.kind {
        ast::PatKind::None => "<unknown>".to_string(),
        ast::PatKind::Identifier(ident) => ident.to_string(),
        ast::PatKind::Literal(literal) => format_literal(literal),
        ast::PatKind::List(items) => format!(
            "[{}]",
            items
                .iter()
                .map(format_ast_pat)
                .collect::<Vec<_>>()
                .join(", ")
        ),
        ast::PatKind::Rest(inner) => format!("...{}", format_ast_pat(inner)),
        ast::PatKind::Enum(enum_pat) => {
            if enum_pat.elements.is_empty() {
                enum_pat.path.to_string()
            } else {
                let fields = enum_pat
                    .elements
                    .iter()
                    .map(|(name, pat)| {
                        if matches!(&pat.kind, ast::PatKind::Identifier(ident) if ident.as_str() == name.as_str())
                        {
                            name.to_string()
                        } else {
                            format!("{name}: {}", format_ast_pat(pat))
                        }
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{} {{ {fields} }}", enum_pat.path)
            }
        }
        ast::PatKind::Wildcard => "_".to_string(),
        ast::PatKind::_Self => "self".to_string(),
    }
}

fn format_literal(literal: &Literal) -> String {
    match literal {
        Literal::Boolean(value) => value.to_string(),
        Literal::Integer(value) => value.to_string(),
        Literal::UnsignedInteger(value) => value.to_string(),
        Literal::Float(value) => value.to_string(),
        Literal::String(value) => format!("\"{value}\""),
        Literal::Char(value) => format!("'{}'", value),
        Literal::None => "nil".to_string(),
    }
}

/// Extract rendered `///` doc comments from parser-attached comment tokens.
///
/// Parser comment attachment strips the leading `//` from single-line comments,
/// so a source doc comment like `/// hello` arrives here as a `CommentToken`
/// whose text starts with a single leading slash (`"/ hello"`).
fn extract_doc_comments(comments: &[CommentToken]) -> Option<String> {
    let docs = comments
        .iter()
        .filter_map(|comment| match comment.kind {
            CommentKind::SingleLine => comment
                .text
                .strip_prefix('/')
                .map(|text| text.trim_start().to_string()),
            CommentKind::MultiLine => None,
        })
        .collect::<Vec<_>>();

    if docs.is_empty() {
        None
    } else {
        Some(docs.join("\n"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::symbol_index::SymbolIndex;
    use crate::typed_hir::lower_and_typecheck;

    fn setup_and_resolve(source: &str, line: u32, column: u32) -> Option<ResolvedSymbol> {
        let result = crate::analyze(source, |_| {});
        let module = result.module.as_ref()?;
        let index = SymbolIndex::from_analyzer(&result.analyzer);
        resolve_symbol(module, &index, line, column)
    }

    fn setup_and_resolve_hover(source: &str, line: u32, column: u32) -> Option<ResolvedSymbol> {
        let result = crate::analyze(source, |_| {});
        let module = result.module.as_ref()?;
        let index = SymbolIndex::from_analyzer(&result.analyzer);
        let mut resolved = resolve_symbol(module, &index, line, column)?;
        let typed_hir = lower_and_typecheck(&result);
        enrich_hover_symbol(module, typed_hir.as_ref(), &mut resolved);
        Some(resolved)
    }

    fn setup_and_resolve_hover_js(source: &str, line: u32, column: u32) -> Option<ResolvedSymbol> {
        let result = crate::analyze_with_js_symbols(source);
        let module = result.module.as_ref()?;
        let index = SymbolIndex::from_analyzer(&result.analyzer);
        let mut resolved = resolve_symbol(module, &index, line, column)?;
        let typed_hir = lower_and_typecheck(&result);
        enrich_hover_symbol(module, typed_hir.as_ref(), &mut resolved);
        Some(resolved)
    }

    #[test]
    fn resolve_variable_reference() {
        let resolved = setup_and_resolve("fn f(x) { x }", 0, 10);
        assert!(resolved.is_some(), "should resolve `x` reference");
        let resolved = resolved.unwrap();
        assert_eq!(resolved.name, "x");
        assert_eq!(resolved.def_kind, DefKind::Parameter);
        assert!(!resolved.builtin);
    }

    #[test]
    fn resolve_function_name() {
        let resolved = setup_and_resolve("fn hello() { 1 }", 0, 3);
        assert!(resolved.is_some(), "should resolve function name");
        assert_eq!(resolved.unwrap().name, "hello");
    }

    #[test]
    fn resolve_returns_none_on_whitespace() {
        let resolved = setup_and_resolve("fn f() { }", 0, 9);
        assert!(resolved.is_none());
    }

    #[test]
    fn resolve_let_binding_shadowing_parameter() {
        // `let a` shadows parameter `a`. Hovering on the let binding name
        // should resolve to the variable, not the shadowed parameter.
        // `a` in `let a = 0` is at col 16 (0-based).
        let resolved = setup_and_resolve("fn foo(a) { let a = 0; a }", 0, 16);
        assert!(resolved.is_some(), "should resolve `a` in let binding");
        let resolved = resolved.unwrap();
        assert_eq!(
            resolved.def_kind,
            DefKind::Variable,
            "should be a variable, not a parameter"
        );
    }

    #[test]
    fn resolve_let_binding_shadowing_parameter_multiline() {
        // Same but across lines. Hovering on the `a` in `let a = 0` should
        // resolve to the variable, not the outer parameter.
        let source = "fn foo(a) {\n  let a = 0;\n  a\n}";
        // `let a = 0;` is on line 1, `a` is at 0-based col 6
        let resolved = setup_and_resolve(source, 1, 6);
        assert!(
            resolved.is_some(),
            "should resolve `a` in let binding on line 1"
        );
        let resolved = resolved.unwrap();
        assert_eq!(
            resolved.def_kind,
            DefKind::Variable,
            "should be a variable, not a parameter"
        );
    }

    #[test]
    fn resolve_multiline_uses_0based_column() {
        // Caller passes 0-based column; resolve_symbol adjusts for lexer.
        let resolved = setup_and_resolve("fn f() {\n  let x = 1;\n  x\n}", 2, 2);
        assert!(
            resolved.is_some(),
            "should resolve `x` on line 2 with 0-based column"
        );
        assert_eq!(resolved.unwrap().name, "x");
    }

    #[test]
    fn hover_text_for_parameter() {
        let resolved = setup_and_resolve("fn f(x) { x }", 0, 10).unwrap();
        assert_eq!(resolved.hover_text(), "(parameter) x");
    }

    #[test]
    fn hover_text_includes_arity_for_function() {
        let resolved =
            setup_and_resolve("fn add(a, b) { a + b }\nlet _ = add(1, 2);", 0, 3).unwrap();
        assert!(
            resolved.hover_text().contains("function"),
            "hover should mention 'function'"
        );
        assert!(
            resolved.hover_text().contains("add"),
            "hover should contain name"
        );
    }

    #[test]
    fn hover_text_formats_function_signature_and_docs() {
        let source = "/// Add numbers\nfn add(a: i64, b) -> i64 { a }\nlet _ = add(1, 2);";
        let resolved = setup_and_resolve_hover(source, 1, 3).unwrap();

        assert_eq!(
            resolved.hover_text(),
            "fn add(a: i64, b: unknown) -> i64\n\nAdd numbers"
        );
    }

    #[test]
    fn hover_text_uses_inferred_function_return_type() {
        let source = "enum SafeHtml { Html(String) }\nfn text(v) { SafeHtml::Html(v) }\nlet _ = text(\"x\");";
        let resolved = setup_and_resolve_hover(source, 1, 3).unwrap();

        assert_eq!(resolved.hover_text(), "fn text(v: unknown) -> SafeHtml");
    }

    #[test]
    fn hover_text_uses_matching_function_arity() {
        let source = "fn binary_search(list, target) { binary_search(list, target, 0, len(list) - 1) }\nfn binary_search(_, _, low, high) if low > high; { -1 }\nfn binary_search(list, target, low, high) {\n    let mid = math::floor((low + high) / 2);\n    let midValue = list[mid];\n\n    if midValue == target; {\n        mid\n    } else if midValue < target; {\n        rec binary_search(list, target, mid + 1, high)\n    } else {\n        rec binary_search(list, target, low, mid - 1)\n    }\n}";
        let resolved = setup_and_resolve_hover(source, 9, 12).unwrap();

        assert_eq!(
            resolved.hover_text(),
            "fn binary_search(list: unknown, target: unknown, low: unknown, high: unknown) -> unknown"
        );
    }

    #[test]
    fn hover_text_formats_builtin_signature_and_docs() {
        let source = "let f = len;";
        let resolved = setup_and_resolve_hover_js(source, 0, 8).unwrap();

        assert_eq!(
            resolved.hover_text(),
            "fn len(iterable: Iterable<unknown>) -> i64\n\nReturns the number of items in an iterable value."
        );
    }

    #[test]
    fn hover_text_formats_type_method_signature() {
        let source = "/// Convert to text\nfn String.to_string(self) -> String { self }\nlet s = \"x\";\ns.to_string();";
        let resolved = setup_and_resolve_hover(source, 3, 2).unwrap();

        assert_eq!(
            resolved.hover_text(),
            "fn String.to_string(self) -> String\n\nConvert to text"
        );
    }

    #[test]
    fn hover_text_formats_protocol_method_signature_and_docs() {
        let source = "protocol Display {\n    /// Render this value\n    fn to_string(self) -> String { \"\" }\n}";
        let resolved = setup_and_resolve_hover(source, 2, 7).unwrap();

        assert_eq!(
            resolved.hover_text(),
            "fn Display.to_string(self) -> String\n\nRender this value"
        );
    }

    #[test]
    fn goto_definition_span_for_parameter() {
        let resolved = setup_and_resolve("fn f(x) { x }", 0, 10).unwrap();
        // Definition span should point to the parameter declaration, not the reference.
        assert!(!resolved.builtin);
        // The def_span.start should be at the parameter `x` position (col 5).
        assert_eq!(resolved.def_span.start_lc.column, 5);
    }

    #[test]
    fn resolve_multi_segment_path_vector_new() {
        // `Vector::new` should resolve as a qualified name.
        // tlang uses `fn Vector::new(...)` syntax for static methods
        let source = "struct Vector { x: i64 }\nfn Vector::new(x: i64) -> Vector { Vector { x } }\nlet v = Vector::new(1);";
        // Hover on `Vector` part of `Vector::new` at line 2, col 8 (0-based)
        let resolved = setup_and_resolve(source, 2, 8);
        assert!(resolved.is_some(), "should resolve Vector::new");
        assert_eq!(resolved.unwrap().name, "Vector::new");
    }

    #[test]
    fn resolve_field_expression_method() {
        // `v1.add(v2)` — hovering on `add` should resolve to `Vector::add`.
        // tlang uses `fn Vector.add(self, ...)` for instance methods
        let source = "struct Vector { x: i64 }\nfn Vector.add(self, other) { self }\nlet v1 = Vector { x: 1 };\nv1.add(v1);";
        // `add` on line 3, col 3 (0-based)
        let resolved = setup_and_resolve(source, 3, 3);
        assert!(
            resolved.is_some(),
            "should resolve field method call `v1.add`"
        );
        let resolved = resolved.unwrap();
        // The method resolves to "Vector::add" in the symbol table.
        assert_eq!(resolved.name, "add");
        assert!(
            resolved.def_kind.arity().is_some(),
            "should resolve to a callable"
        );
    }

    #[test]
    fn resolve_return_type_annotation() {
        // Hovering on `Vector` in `-> Vector` should resolve to the struct.
        let source = "struct Vector { x: i64 }\nfn Vector::new(x: i64) -> Vector { Vector { x } }";
        // "-> Vector" on line 1 — the "Vector" type starts at 0-based col 26
        let resolved = setup_and_resolve(source, 1, 26);
        assert!(
            resolved.is_some(),
            "should resolve return type annotation 'Vector'"
        );
        let resolved = resolved.unwrap();
        assert_eq!(resolved.name, "Vector");
        assert_eq!(resolved.def_kind, DefKind::Struct);
    }

    #[test]
    fn resolve_parameter_type_annotation() {
        // Hovering on `Vector` in `other: Vector` should resolve to the struct.
        let source =
            "struct Vector { x: i64 }\nfn Vector.add(self, other: Vector) -> Vector { self }";
        // `Vector` type in `other: Vector` on line 1 — the "V" starts at 0-based col 27
        let resolved = setup_and_resolve(source, 1, 27);
        assert!(
            resolved.is_some(),
            "should resolve parameter type annotation 'Vector'"
        );
        let resolved = resolved.unwrap();
        assert_eq!(resolved.name, "Vector");
        assert_eq!(resolved.def_kind, DefKind::Struct);
    }

    #[test]
    fn resolve_primitive_type_annotation_not_found() {
        // Primitive types like `i64` are not in the symbol index,
        // so they don't resolve to a definition.
        let source = "struct Vector { x: i64 }\nfn Vector::new(x: i64) -> Vector { Vector { x } }";
        // `i64` in `x: i64` on line 1 — starts at 0-based col 18
        let resolved = setup_and_resolve(source, 1, 18);
        assert!(
            resolved.is_none(),
            "primitive type 'i64' should not resolve (not a user-defined symbol)"
        );
    }

    #[test]
    fn resolve_field_access_on_variable() {
        // `other.x` should resolve to the struct field `Vector::x`
        let source = "struct Vector { x: i64, y: i64 }\nfn Vector.add(self, other: Vector) -> Vector {\n  let sum_x = other.x;\n  sum_x\n}";
        // `x` in `other.x` on line 2 — 0-based col 20
        let resolved = setup_and_resolve(source, 2, 20);
        assert!(resolved.is_some(), "field 'x' in 'other.x' should resolve");
        let resolved = resolved.unwrap();
        assert_eq!(resolved.def_kind, DefKind::StructField);
    }

    #[test]
    fn resolve_field_access_on_self() {
        // `self.x` should resolve to the struct field `Vector::x`
        let source =
            "struct Vector { x: i64, y: i64 }\nfn Vector.get_x(self) -> i64 {\n  self.x\n}";
        // `x` in `self.x` on line 2 — 0-based col 7
        let resolved = setup_and_resolve(source, 2, 7);
        assert!(resolved.is_some(), "field 'x' in 'self.x' should resolve");
        let resolved = resolved.unwrap();
        assert_eq!(resolved.def_kind, DefKind::StructField);
    }

    #[test]
    fn resolve_method_call_on_self() {
        // `self.add` should resolve to the method `Vector::add`
        let source = "struct Vector { x: i64 }\nfn Vector.add(self, other: Vector) -> Vector { self }\nfn Vector.add_twice(self, other: Vector) -> Vector {\n  self.add(other)\n}";
        // `add` in `self.add` on line 3 — 0-based col 7
        let resolved = setup_and_resolve(source, 3, 7);
        assert!(
            resolved.is_some(),
            "method 'add' in 'self.add' should resolve"
        );
        let resolved = resolved.unwrap();
        assert!(
            resolved.def_kind.arity().is_some(),
            "resolved symbol should be a function/method"
        );
    }

    #[test]
    fn resolve_enum_method_call_on_variable() {
        // `opt.map(f)` should resolve to the method `Option::map`
        let source = "enum Option<T> {\n  Some(T),\n  None,\n}\nfn Option.map(Option::Some(x), f) { Option::Some(f(x)) }\nfn Option.map(Option::None, _) { Option::None }\nlet opt = Option::Some(1);\nopt.map(fn (x) { x + 1 });";
        // `map` in `opt.map` on line 7 — 0-based col 4
        let resolved = setup_and_resolve(source, 7, 4);
        assert!(
            resolved.is_some(),
            "method 'map' in 'opt.map' should resolve"
        );
        let resolved = resolved.unwrap();
        assert!(
            resolved.def_kind.arity().is_some(),
            "resolved enum method should be callable"
        );
    }

    #[test]
    fn resolve_enum_method_call_on_self() {
        // `self.is_some()` inside an enum method should resolve
        let source = "enum Option<T> {\n  Some(T),\n  None,\n}\nfn Option.is_some(Option::Some(_)) { true }\nfn Option.is_some(Option::None) { false }\nfn Option.check(self) {\n  self.is_some()\n}";
        // `is_some` in `self.is_some` on line 7 — 0-based col 7
        let resolved = setup_and_resolve(source, 7, 7);
        assert!(
            resolved.is_some(),
            "method 'is_some' in 'self.is_some' should resolve"
        );
    }

    #[test]
    fn resolve_protocol_declaration_name() {
        // Hovering on the protocol name should resolve to `DefKind::Protocol`.
        let source = "protocol Display {\n    fn to_string(self) { \"\" }\n}";
        // `Display` starts at col 9 on line 0
        let resolved = setup_and_resolve(source, 0, 9);
        assert!(resolved.is_some(), "protocol name 'Display' should resolve");
        let resolved = resolved.unwrap();
        assert_eq!(resolved.name, "Display");
        assert_eq!(
            resolved.def_kind,
            tlang_defs::DefKind::Protocol,
            "should resolve to DefKind::Protocol"
        );
    }

    #[test]
    fn resolve_protocol_method_in_signature() {
        // Hovering on method name inside a protocol declaration.
        let source = "protocol Display {\n    fn to_string(self) { \"\" }\n}";
        // `to_string` starts at col 7 on line 1 (0-based)
        let resolved = setup_and_resolve(source, 1, 7);
        assert!(
            resolved.is_some(),
            "protocol method 'to_string' should resolve"
        );
        let resolved = resolved.unwrap();
        assert_eq!(resolved.name, "to_string");
        assert!(
            resolved.def_kind.arity().is_some(),
            "protocol method should have an arity"
        );
    }

    #[test]
    fn resolve_protocol_name_in_impl_block() {
        // Hovering on the protocol name in `impl Display for MyType` should resolve.
        let source = "protocol Display {\n    fn to_string(self) { \"\" }\n}\nstruct MyType {}\nimpl Display for MyType {\n    fn to_string(self) { \"\" }\n}";
        // `Display` in `impl Display for MyType` is on line 4, col 5 (0-based)
        let resolved = setup_and_resolve(source, 4, 5);
        assert!(
            resolved.is_some(),
            "protocol name 'Display' in impl block should resolve"
        );
        let resolved = resolved.unwrap();
        assert_eq!(resolved.name, "Display");
        assert_eq!(
            resolved.def_kind,
            tlang_defs::DefKind::Protocol,
            "should resolve to DefKind::Protocol"
        );
    }

    #[test]
    fn resolve_recursive_call_in_multi_clause_function() {
        // In the second clause of a multi-clause function, the recursive call
        // `map(xs, f)` should resolve to the function itself.
        let source = "fn map([], _) { [] }\nfn map([x, ...xs], f) { [f(x), ...map(xs, f)] }";

        // Try a range of columns to find where 'map' is in the recursive call.
        // The recursive call is in `...map(xs, f)` on line 1.
        let mut found_and_resolved = false;
        for col in 30..45 {
            let resolved = setup_and_resolve(source, 1, col);
            if let Some(ref r) = resolved
                && r.name == "map"
                && r.def_kind.arity().is_some()
            {
                found_and_resolved = true;
                break;
            }
        }

        assert!(
            found_and_resolved,
            "should resolve recursive `map` call in multi-clause function"
        );
    }

    #[test]
    fn resolve_for_loop_binding_definition() {
        let source = r#"enum Tree {
    Empty,
    Node { value: isize, left: Tree, right: Tree },
}

impl Iterable<isize> for Tree {
    fn iter(self) {
        Iterable::iter(self.to_list())
    }
}

fn Tree.to_list(Tree::Empty) { [] }
fn Tree.to_list(Tree::Node { value, left, right }) {
    [...left, value, ...right]
}

let tree = Tree::Empty;
for x in tree {
    x |> log();
};"#;
        let resolved = setup_and_resolve(source, 17, 4);
        assert!(resolved.is_some(), "should resolve loop binding definition");
        let resolved = resolved.unwrap();
        assert_eq!(resolved.name, "x");
        assert_eq!(resolved.def_kind, DefKind::Variable);
    }

    #[test]
    fn resolve_for_loop_binding_reference() {
        let source = r#"enum Tree {
    Empty,
    Node { value: isize, left: Tree, right: Tree },
}

impl Iterable<isize> for Tree {
    fn iter(self) {
        Iterable::iter(self.to_list())
    }
}

fn Tree.to_list(Tree::Empty) { [] }
fn Tree.to_list(Tree::Node { value, left, right }) {
    [...left, value, ...right]
}

let tree = Tree::Empty;
for x in tree {
    x |> log();
};"#;
        let resolved = setup_and_resolve(source, 18, 4);
        assert!(resolved.is_some(), "should resolve loop binding reference");
        let resolved = resolved.unwrap();
        assert_eq!(resolved.name, "x");
        assert_eq!(resolved.def_kind, DefKind::Variable);
    }
}
