use std::collections::HashSet;

use lsp_types::{
    SemanticToken, SemanticTokenModifier, SemanticTokenType, SemanticTokens, SemanticTokensLegend,
};
use tlang_analysis::inlay_hints::TypedHir;
use tlang_analysis::{member_resolution, query};
use tlang_ast::node as ast;
use tlang_defs::DefKind;
use tlang_span::Span;

use crate::diagnostics;
use crate::document_store::SymbolIndex;

const TOKEN_TYPES: [TokenKind; 12] = [
    TokenKind::Namespace,
    TokenKind::Type,
    TokenKind::Struct,
    TokenKind::Enum,
    TokenKind::Interface,
    TokenKind::TypeParameter,
    TokenKind::Function,
    TokenKind::Method,
    TokenKind::Parameter,
    TokenKind::Variable,
    TokenKind::Property,
    TokenKind::EnumMember,
];

const TOKEN_MODIFIERS: [TokenModifier; 3] = [
    TokenModifier::Declaration,
    TokenModifier::Readonly,
    TokenModifier::DefaultLibrary,
];

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenKind {
    Namespace,
    Type,
    Struct,
    Enum,
    Interface,
    TypeParameter,
    Function,
    Method,
    Parameter,
    Variable,
    Property,
    EnumMember,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenModifier {
    Declaration,
    Readonly,
    DefaultLibrary,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CollectedSemanticToken {
    pub span: Span,
    pub kind: TokenKind,
    pub modifiers: Vec<TokenModifier>,
}

impl CollectedSemanticToken {
    pub fn has_modifier(&self, modifier: TokenModifier) -> bool {
        self.modifiers.contains(&modifier)
    }
}

pub fn legend() -> SemanticTokensLegend {
    SemanticTokensLegend {
        token_types: TOKEN_TYPES.iter().copied().map(token_type).collect(),
        token_modifiers: TOKEN_MODIFIERS
            .iter()
            .copied()
            .map(token_modifier)
            .collect(),
    }
}

pub fn collect(
    source: &str,
    module: &ast::Module,
    index: &SymbolIndex,
    typed_hir: Option<&TypedHir>,
) -> Vec<CollectedSemanticToken> {
    let mut collector = Collector {
        source,
        module,
        index,
        typed_hir,
        tokens: Vec::new(),
        seen: HashSet::new(),
        active_type_params: Vec::new(),
    };

    collector.visit_module(module);
    collector.tokens.sort_by_key(|token| {
        (
            token.span.start_lc.line,
            token.span.start_lc.column,
            token.span.end_lc.line,
            token.span.end_lc.column,
            token_kind_index(token.kind),
            modifiers_bitset(&token.modifiers),
        )
    });
    collector.tokens
}

pub fn encode(tokens: &[CollectedSemanticToken]) -> SemanticTokens {
    let mut data = Vec::with_capacity(tokens.len());
    let mut prev_line = 0u32;
    let mut prev_start = 0u32;

    for token in tokens {
        let range = diagnostics::span_to_range(&token.span);
        let line = range.start.line;
        let start = range.start.character;
        let length = range.end.character.saturating_sub(range.start.character);
        if length == 0 {
            continue;
        }

        let delta_line = line.saturating_sub(prev_line);
        let delta_start = if delta_line == 0 {
            start.saturating_sub(prev_start)
        } else {
            start
        };

        data.push(SemanticToken {
            delta_line,
            delta_start,
            length,
            token_type: token_kind_index(token.kind),
            token_modifiers_bitset: modifiers_bitset(&token.modifiers),
        });

        prev_line = line;
        prev_start = start;
    }

    SemanticTokens {
        result_id: None,
        data,
    }
}

struct Collector<'a> {
    source: &'a str,
    module: &'a ast::Module,
    index: &'a SymbolIndex,
    typed_hir: Option<&'a TypedHir>,
    tokens: Vec<CollectedSemanticToken>,
    seen: HashSet<(u32, u32, u32, u32, TokenKind, u32)>,
    active_type_params: Vec<HashSet<String>>,
}

impl Collector<'_> {
    fn visit_module(&mut self, module: &ast::Module) {
        for stmt in &module.statements {
            self.visit_stmt(stmt);
        }
    }

    fn visit_stmt(&mut self, stmt: &ast::Stmt) {
        match &stmt.kind {
            ast::StmtKind::None => {}
            ast::StmtKind::Expr(expr) => self.visit_expr(expr),
            ast::StmtKind::Let(let_decl) => {
                self.visit_binding_pat(&let_decl.pattern, TokenKind::Variable);
                self.visit_expr(&let_decl.expression);
                if let Some(ty) = &let_decl.type_annotation {
                    self.visit_ty(ty);
                }
            }
            ast::StmtKind::Const(const_decl) => {
                self.push(
                    const_decl.name.span,
                    TokenKind::Variable,
                    &[TokenModifier::Declaration, TokenModifier::Readonly],
                );
                self.visit_expr(&const_decl.expression);
                if let Some(ty) = &const_decl.type_annotation {
                    self.visit_ty(ty);
                }
            }
            ast::StmtKind::FunctionDeclaration(decl) => self.visit_fn_decl(decl),
            ast::StmtKind::FunctionDeclarations(decls) => {
                for decl in decls {
                    self.visit_fn_decl(decl);
                }
            }
            ast::StmtKind::Return(expr) => {
                if let Some(expr) = expr {
                    self.visit_expr(expr);
                }
            }
            ast::StmtKind::EnumDeclaration(decl) => self.visit_enum_decl(decl),
            ast::StmtKind::StructDeclaration(decl) => self.visit_struct_decl(decl),
            ast::StmtKind::ProtocolDeclaration(decl) => self.visit_protocol_decl(decl),
            ast::StmtKind::ImplBlock(impl_block) => self.visit_impl_block(impl_block),
            ast::StmtKind::UseDeclaration(decl) => self.visit_use_decl(decl),
            ast::StmtKind::ModDeclaration(decl) => {
                for name in &decl.names {
                    self.push(
                        name.span,
                        TokenKind::Namespace,
                        &[TokenModifier::Declaration],
                    );
                }
            }
        }
    }

    fn visit_struct_decl(&mut self, decl: &ast::StructDeclaration) {
        self.push(
            decl.name.span,
            TokenKind::Struct,
            &[TokenModifier::Declaration],
        );
        self.push_type_params(&decl.type_params);

        for field in &decl.fields {
            self.push(
                field.name.span,
                TokenKind::Property,
                &[TokenModifier::Declaration],
            );
            self.visit_ty(&field.ty);
        }

        for const_decl in &decl.consts {
            self.push(
                const_decl.name.span,
                TokenKind::Variable,
                &[TokenModifier::Declaration, TokenModifier::Readonly],
            );
            self.visit_expr(&const_decl.expression);
        }

        self.pop_type_params();
    }

    fn visit_enum_decl(&mut self, decl: &ast::EnumDeclaration) {
        self.push(
            decl.name.span,
            TokenKind::Enum,
            &[TokenModifier::Declaration],
        );
        self.push_type_params(&decl.type_params);

        for variant in &decl.variants {
            self.push(
                variant.name.span,
                TokenKind::EnumMember,
                &[TokenModifier::Declaration],
            );
            for field in &variant.parameters {
                self.push(
                    field.name.span,
                    TokenKind::Property,
                    &[TokenModifier::Declaration],
                );
                self.visit_ty(&field.ty);
            }
            if let Some(expr) = &variant.discriminant {
                self.visit_expr(expr);
            }
        }

        for const_decl in &decl.consts {
            self.push(
                const_decl.name.span,
                TokenKind::Variable,
                &[TokenModifier::Declaration, TokenModifier::Readonly],
            );
            self.visit_expr(&const_decl.expression);
        }

        self.pop_type_params();
    }

    fn visit_protocol_decl(&mut self, decl: &ast::ProtocolDeclaration) {
        self.push(
            decl.name.span,
            TokenKind::Interface,
            &[TokenModifier::Declaration],
        );
        self.push_type_params(&decl.type_params);

        for constraint in &decl.constraints {
            self.visit_path(constraint, true);
        }
        for assoc_type in &decl.associated_types {
            self.push(
                assoc_type.name.span,
                TokenKind::Type,
                &[TokenModifier::Declaration],
            );
            self.push_type_params(&assoc_type.type_params);
            self.pop_type_params();
        }
        for method in &decl.methods {
            self.push(
                method.name.span,
                TokenKind::Method,
                &[TokenModifier::Declaration],
            );
            self.push_type_params(&method.type_params);
            for param in &method.parameters {
                self.visit_fn_param(param);
            }
            if let Some(ret_ty) = &method.return_type_annotation {
                self.visit_ty(ret_ty);
            }
            if let Some(body) = &method.body {
                self.visit_block(body);
            }
            self.pop_type_params();
        }
        for const_decl in &decl.consts {
            self.push(
                const_decl.name.span,
                TokenKind::Variable,
                &[TokenModifier::Declaration, TokenModifier::Readonly],
            );
            self.visit_expr(&const_decl.expression);
        }

        self.pop_type_params();
    }

    fn visit_impl_block(&mut self, impl_block: &ast::ImplBlock) {
        self.push_type_params(&impl_block.type_params);
        self.visit_path(&impl_block.protocol_name, true);
        for ty in &impl_block.type_arguments {
            self.visit_ty(ty);
        }
        self.visit_path(&impl_block.target_type, true);
        for ty in &impl_block.target_type_arguments {
            self.visit_ty(ty);
        }
        if let Some(where_clause) = &impl_block.where_clause {
            for predicate in &where_clause.predicates {
                if self.is_active_type_param(predicate.name.as_str()) {
                    self.push(predicate.name.span, TokenKind::TypeParameter, &[]);
                } else {
                    self.push(predicate.name.span, TokenKind::Type, &[]);
                }
                for bound in &predicate.bounds {
                    self.visit_ty(bound);
                }
            }
        }
        for assoc_type in &impl_block.associated_types {
            self.push(
                assoc_type.name.span,
                TokenKind::Type,
                &[TokenModifier::Declaration],
            );
            self.push_type_params(&assoc_type.type_params);
            self.visit_ty(&assoc_type.ty);
            self.pop_type_params();
        }
        for method in &impl_block.methods {
            self.visit_fn_decl(method);
        }
        for ident in &impl_block.apply_methods {
            self.push(ident.span, TokenKind::Method, &[]);
        }
        self.pop_type_params();
    }

    fn visit_use_decl(&mut self, decl: &ast::UseDeclaration) {
        for (index, ident) in decl.path.iter().enumerate() {
            let (kind, modifiers) =
                self.classify_path_prefix(&decl.path, index, false, ident.span, false);
            self.push(ident.span, kind, &modifiers);
        }

        for item in &decl.items {
            let mut qualified_item = decl.path.clone();
            qualified_item.push(item.name);
            let (item_kind, item_modifiers) = self.classify_path_prefix(
                &qualified_item,
                qualified_item.len().saturating_sub(1),
                false,
                item.span,
                true,
            );
            self.push(item.name.span, item_kind, &item_modifiers);

            if let Some(alias) = &item.alias {
                self.push(
                    alias.span,
                    TokenKind::Variable,
                    &[TokenModifier::Declaration],
                );
            }
        }
    }

    fn visit_fn_decl(&mut self, decl: &ast::FunctionDeclaration) {
        self.push_type_params(&decl.owner_type_params);
        self.push_type_params(&decl.type_params);

        match &decl.name.kind {
            ast::ExprKind::Path(path) => {
                if let Some(last) = path.segments.last() {
                    self.push(
                        last.span,
                        TokenKind::Function,
                        &[TokenModifier::Declaration],
                    );
                }
            }
            ast::ExprKind::FieldExpression(field_expr) => {
                if let Some(path) = field_expr.base.path() {
                    self.visit_path(path, true);
                } else {
                    self.visit_expr(&field_expr.base);
                }
                self.push(
                    field_expr.field.span,
                    TokenKind::Method,
                    &[TokenModifier::Declaration],
                );
            }
            _ => self.visit_expr(&decl.name),
        }

        for parameter in &decl.parameters {
            self.visit_fn_param(parameter);
        }
        if let Some(guard) = &decl.guard {
            self.visit_expr(guard);
        }
        if let Some(ret_ty) = &decl.return_type_annotation {
            self.visit_ty(ret_ty);
        }
        self.visit_block(&decl.body);

        self.pop_type_params();
        self.pop_type_params();
    }

    fn visit_fn_param(&mut self, parameter: &ast::FunctionParameter) {
        self.visit_binding_pat(&parameter.pattern, TokenKind::Parameter);
        if let Some(ty) = &parameter.type_annotation {
            self.visit_ty(ty);
        }
    }

    fn visit_block(&mut self, block: &ast::Block) {
        for stmt in &block.statements {
            self.visit_stmt(stmt);
        }
        if let Some(expr) = &block.expression {
            self.visit_expr(expr);
        }
    }

    fn visit_expr(&mut self, expr: &ast::Expr) {
        match &expr.kind {
            ast::ExprKind::None
            | ast::ExprKind::Continue
            | ast::ExprKind::Literal(_)
            | ast::ExprKind::Wildcard => {}
            ast::ExprKind::Block(block) | ast::ExprKind::Loop(block) => self.visit_block(block),
            ast::ExprKind::Call(call) | ast::ExprKind::RecursiveCall(call) => {
                for argument in &call.arguments {
                    self.visit_expr(argument);
                }
                self.visit_expr(&call.callee);
            }
            ast::ExprKind::Cast(inner, ty) | ast::ExprKind::TryCast(inner, ty) => {
                self.visit_expr(inner);
                self.visit_ty(ty);
            }
            ast::ExprKind::Implements(inner, path) => {
                self.visit_expr(inner);
                self.visit_path(path, true);
            }
            ast::ExprKind::Dict(entries) => {
                for (key, value) in entries {
                    self.visit_expr(key);
                    self.visit_expr(value);
                }
            }
            ast::ExprKind::ForLoop(for_loop) => {
                self.visit_expr(&for_loop.iter);
                if let Some((pat, expr)) = &for_loop.acc {
                    self.visit_binding_pat(pat, TokenKind::Variable);
                    self.visit_expr(expr);
                }
                self.visit_binding_pat(&for_loop.pat, TokenKind::Variable);
                self.visit_block(&for_loop.block);
                if let Some(else_block) = &for_loop.else_block {
                    self.visit_block(else_block);
                }
            }
            ast::ExprKind::FunctionExpression(decl) => self.visit_fn_decl(decl),
            ast::ExprKind::FieldExpression(field_expr) => {
                self.visit_expr(&field_expr.base);
                self.visit_member_ident(&field_expr.field);
            }
            ast::ExprKind::IndexExpression(index_expr) => {
                self.visit_expr(&index_expr.base);
                self.visit_expr(&index_expr.index);
            }
            ast::ExprKind::Let(pattern, expr) => {
                self.visit_binding_pat(pattern, TokenKind::Variable);
                self.visit_expr(expr);
            }
            ast::ExprKind::IfElse(if_else) => {
                self.visit_expr(&if_else.condition);
                self.visit_block(&if_else.then_branch);
                for branch in &if_else.else_branches {
                    if let Some(condition) = &branch.condition {
                        self.visit_expr(condition);
                    }
                    self.visit_block(&branch.consequence);
                }
            }
            ast::ExprKind::List(exprs) => {
                for expr in exprs {
                    self.visit_expr(expr);
                }
            }
            ast::ExprKind::Path(path) => self.visit_path(path, false),
            ast::ExprKind::UnaryOp(_, expr) | ast::ExprKind::Break(Some(expr)) => {
                self.visit_expr(expr)
            }
            ast::ExprKind::BinaryOp(binary) => {
                self.visit_expr(&binary.lhs);
                self.visit_expr(&binary.rhs);
            }
            ast::ExprKind::Matches(expr, pat) => {
                self.visit_expr(expr);
                self.visit_binding_pat(pat, TokenKind::Variable);
            }
            ast::ExprKind::Match(match_expr) => {
                self.visit_expr(&match_expr.expression);
                for arm in &match_expr.arms {
                    self.visit_binding_pat(&arm.pattern, TokenKind::Variable);
                    if let Some(guard) = &arm.guard {
                        self.visit_expr(guard);
                    }
                    self.visit_expr(&arm.expression);
                }
            }
            ast::ExprKind::Range(range) => {
                self.visit_expr(&range.start);
                self.visit_expr(&range.end);
            }
            ast::ExprKind::TaggedString { tag, exprs, .. } => {
                self.visit_expr(tag);
                for expr in exprs {
                    self.visit_expr(expr);
                }
            }
            ast::ExprKind::Break(None) => {}
        }
    }

    fn visit_binding_pat(&mut self, pat: &ast::Pat, binding_kind: TokenKind) {
        match &pat.kind {
            ast::PatKind::Identifier(ident) => {
                self.push(ident.span, binding_kind, &[TokenModifier::Declaration]);
            }
            ast::PatKind::List(patterns) => {
                for pattern in patterns {
                    self.visit_binding_pat(pattern, binding_kind);
                }
            }
            ast::PatKind::Rest(pattern) => self.visit_binding_pat(pattern, binding_kind),
            ast::PatKind::Enum(enum_pattern) => {
                self.visit_path(&enum_pattern.path, false);
                for (field, pattern) in &enum_pattern.elements {
                    self.push(field.span, TokenKind::Property, &[]);
                    self.visit_binding_pat(pattern, binding_kind);
                }
            }
            ast::PatKind::Literal(_)
            | ast::PatKind::_Self
            | ast::PatKind::Wildcard
            | ast::PatKind::None => {}
        }
    }

    fn visit_ty(&mut self, ty: &ast::Ty) {
        for param in &ty.parameters {
            self.visit_ty(param);
        }
        match &ty.kind {
            ast::TyKind::Unknown => {}
            ast::TyKind::Path(path) => self.visit_path(path, true),
            ast::TyKind::Union(paths) => {
                for path in paths {
                    self.visit_path(path, true);
                }
            }
            ast::TyKind::Fn(params, ret) => {
                for param in params {
                    if let Some(name) = &param.name {
                        self.push(name.span, TokenKind::Parameter, &[]);
                    }
                    self.visit_ty(&param.ty);
                }
                self.visit_ty(ret);
            }
        }
    }

    fn visit_path(&mut self, path: &ast::Path, type_context: bool) {
        if path.segments.is_empty() {
            return;
        }

        for (index, segment) in path.segments.iter().enumerate() {
            let is_last = index + 1 == path.segments.len();
            let (kind, modifiers) =
                self.classify_path_prefix(&path.segments, index, type_context, path.span, is_last);
            self.push(segment.span, kind, &modifiers);
        }
    }

    fn visit_member_ident(&mut self, ident: &ast::Ident) {
        if let Some((kind, modifiers)) = self.classify_member(ident.span) {
            self.push(ident.span, kind, &modifiers);
            return;
        }
        if let Some((kind, modifiers)) = self.classify_resolved_symbol(ident.span) {
            self.push(ident.span, kind, &modifiers);
            return;
        }
        self.push(ident.span, TokenKind::Property, &[]);
    }

    fn classify_member(&self, span: Span) -> Option<(TokenKind, Vec<TokenModifier>)> {
        let typed_hir = self.typed_hir?;
        let pos = diagnostics::span_to_range(&span).start;
        let member = member_resolution::resolve_member_at_position(
            self.source,
            typed_hir,
            pos.line,
            pos.character,
        )?;

        let mut modifiers = Vec::new();
        if member.builtin {
            modifiers.push(TokenModifier::DefaultLibrary);
        }

        Some((
            match member.kind {
                member_resolution::MemberKind::Method => TokenKind::Method,
                member_resolution::MemberKind::Field => TokenKind::Property,
            },
            modifiers,
        ))
    }

    fn classify_resolved_symbol(&self, span: Span) -> Option<(TokenKind, Vec<TokenModifier>)> {
        let pos = diagnostics::span_to_range(&span).start;
        let resolved = query::resolve_symbol(self.module, self.index, pos.line, pos.character)?;
        Some((
            map_def_kind(resolved.def_kind),
            modifiers_for_symbol(&resolved),
        ))
    }

    fn classify_path_prefix(
        &self,
        segments: &[ast::Ident],
        index: usize,
        type_context: bool,
        span: Span,
        is_last: bool,
    ) -> (TokenKind, Vec<TokenModifier>) {
        let ident = &segments[index];
        if type_context && segments.len() == 1 && self.is_active_type_param(ident.as_str()) {
            return (TokenKind::TypeParameter, vec![]);
        }

        if is_last && let Some((kind, modifiers)) = self.classify_resolved_symbol(ident.span) {
            return (kind, modifiers);
        }

        let qualified_name = segments
            .iter()
            .take(index + 1)
            .map(ast::Ident::as_str)
            .collect::<Vec<_>>()
            .join("::");

        if let Some((kind, modifiers)) = self.classify_qualified_name(&qualified_name, span) {
            return (kind, modifiers);
        }

        if is_last {
            if type_context || starts_with_uppercase(ident.as_str()) {
                (TokenKind::Type, vec![])
            } else {
                (TokenKind::Namespace, vec![])
            }
        } else if starts_with_uppercase(ident.as_str()) {
            (TokenKind::Type, vec![])
        } else {
            (TokenKind::Namespace, vec![])
        }
    }

    fn classify_qualified_name(
        &self,
        qualified_name: &str,
        span: Span,
    ) -> Option<(TokenKind, Vec<TokenModifier>)> {
        let entry = self
            .index
            .get_closest_by_name(self.module.id, qualified_name, span)?;
        let resolved = query::ResolvedSymbol {
            name: qualified_name.to_string(),
            qualified_name: entry.name.to_string(),
            ident_span: span,
            def_kind: entry.kind,
            def_span: entry.defined_at,
            builtin: entry.builtin,
            temp: entry.temp,
            type_info: None,
            node_id: entry.node_id,
            hir_id: entry.hir_id,
            signature: None,
            documentation: None,
        };
        Some((map_def_kind(entry.kind), modifiers_for_symbol(&resolved)))
    }

    fn push_type_params(&mut self, type_params: &[ast::TypeParam]) {
        if type_params.is_empty() {
            self.active_type_params.push(HashSet::new());
            return;
        }

        let names: HashSet<String> = type_params
            .iter()
            .map(|type_param| type_param.name.as_str().to_string())
            .collect();
        self.active_type_params.push(names);

        for type_param in type_params {
            self.push(
                type_param.name.span,
                TokenKind::TypeParameter,
                &[TokenModifier::Declaration],
            );
        }
        for type_param in type_params {
            for bound in &type_param.bounds {
                self.visit_ty(bound);
            }
        }
    }

    fn pop_type_params(&mut self) {
        self.active_type_params.pop();
    }

    fn is_active_type_param(&self, name: &str) -> bool {
        self.active_type_params
            .iter()
            .rev()
            .any(|scope| scope.contains(name))
    }

    fn push(&mut self, span: Span, kind: TokenKind, modifiers: &[TokenModifier]) {
        let range = diagnostics::span_to_range(&span);
        if range.start.line != range.end.line || range.start.character >= range.end.character {
            return;
        }

        let modifiers = modifiers.to_vec();
        let key = (
            range.start.line,
            range.start.character,
            range.end.line,
            range.end.character,
            kind,
            modifiers_bitset(&modifiers),
        );
        if self.seen.insert(key) {
            self.tokens.push(CollectedSemanticToken {
                span,
                kind,
                modifiers,
            });
        }
    }
}

fn map_def_kind(kind: DefKind) -> TokenKind {
    match kind {
        DefKind::Module => TokenKind::Namespace,
        DefKind::Variable | DefKind::Const => TokenKind::Variable,
        DefKind::Function(_) | DefKind::FunctionSelfRef(_) => TokenKind::Function,
        DefKind::Parameter => TokenKind::Parameter,
        DefKind::Enum => TokenKind::Enum,
        DefKind::EnumVariant(_) => TokenKind::EnumMember,
        DefKind::Struct => TokenKind::Struct,
        DefKind::Protocol => TokenKind::Interface,
        DefKind::ProtocolMethod(_) | DefKind::StructMethod(_) => TokenKind::Method,
        DefKind::StructField => TokenKind::Property,
    }
}

fn modifiers_for_symbol(resolved: &query::ResolvedSymbol) -> Vec<TokenModifier> {
    let mut modifiers = Vec::new();
    if resolved.def_kind == DefKind::Const {
        modifiers.push(TokenModifier::Readonly);
    }
    if resolved.builtin {
        modifiers.push(TokenModifier::DefaultLibrary);
    }
    modifiers
}

fn starts_with_uppercase(name: &str) -> bool {
    name.chars()
        .next()
        .is_some_and(|ch| ch.is_ascii_uppercase())
}

fn token_type(kind: TokenKind) -> SemanticTokenType {
    match kind {
        TokenKind::Namespace => SemanticTokenType::NAMESPACE,
        TokenKind::Type => SemanticTokenType::TYPE,
        TokenKind::Struct => SemanticTokenType::STRUCT,
        TokenKind::Enum => SemanticTokenType::ENUM,
        TokenKind::Interface => SemanticTokenType::INTERFACE,
        TokenKind::TypeParameter => SemanticTokenType::TYPE_PARAMETER,
        TokenKind::Function => SemanticTokenType::FUNCTION,
        TokenKind::Method => SemanticTokenType::METHOD,
        TokenKind::Parameter => SemanticTokenType::PARAMETER,
        TokenKind::Variable => SemanticTokenType::VARIABLE,
        TokenKind::Property => SemanticTokenType::PROPERTY,
        TokenKind::EnumMember => SemanticTokenType::ENUM_MEMBER,
    }
}

fn token_modifier(modifier: TokenModifier) -> SemanticTokenModifier {
    match modifier {
        TokenModifier::Declaration => SemanticTokenModifier::DECLARATION,
        TokenModifier::Readonly => SemanticTokenModifier::READONLY,
        TokenModifier::DefaultLibrary => SemanticTokenModifier::DEFAULT_LIBRARY,
    }
}

fn token_kind_index(kind: TokenKind) -> u32 {
    TOKEN_TYPES
        .iter()
        .position(|candidate| *candidate == kind)
        .unwrap_or_else(|| panic!("token kind {kind:?} must be in legend")) as u32
}

fn modifiers_bitset(modifiers: &[TokenModifier]) -> u32 {
    modifiers.iter().fold(0u32, |bits, modifier| {
        let index = TOKEN_MODIFIERS
            .iter()
            .position(|candidate| *candidate == *modifier)
            .unwrap_or_else(|| panic!("token modifier {modifier:?} must be in legend"))
            as u32;
        bits | (1 << index)
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use tlang_analysis::CompilationTarget;

    fn collect_for(source: &str) -> Vec<CollectedSemanticToken> {
        let result = tlang_analysis::analyze_for_target(source, CompilationTarget::Js);
        let module = result.module.as_ref().expect("source should parse");
        let index = SymbolIndex::from_analyzer(&result.analyzer);
        let typed_hir = tlang_analysis::inlay_hints::lower_and_typecheck(&result);
        collect(source, module, &index, typed_hir.as_ref())
    }

    fn find_token<'a>(
        tokens: &'a [CollectedSemanticToken],
        source: &'a str,
        text: &str,
        kind: TokenKind,
        nth: usize,
    ) -> &'a CollectedSemanticToken {
        tokens
            .iter()
            .filter(|token| {
                token.kind == kind
                    && &source[token.span.start as usize..token.span.end as usize] == text
            })
            .nth(nth)
            .unwrap_or_else(|| panic!("missing token `{text}` of kind {kind:?} in {tokens:?}"))
    }

    #[test]
    fn collects_declarations_and_references() {
        let source = "struct Vector { x: i64 }\nfn add(value: Vector) -> Vector {\n  let local = value;\n  local\n}";
        let tokens = collect_for(source);

        assert!(
            find_token(&tokens, source, "Vector", TokenKind::Struct, 0)
                .has_modifier(TokenModifier::Declaration)
        );
        assert!(
            find_token(&tokens, source, "x", TokenKind::Property, 0)
                .has_modifier(TokenModifier::Declaration)
        );
        assert!(
            find_token(&tokens, source, "add", TokenKind::Function, 0)
                .has_modifier(TokenModifier::Declaration)
        );
        assert!(
            find_token(&tokens, source, "value", TokenKind::Parameter, 0)
                .has_modifier(TokenModifier::Declaration)
        );
        assert!(
            find_token(&tokens, source, "local", TokenKind::Variable, 0)
                .has_modifier(TokenModifier::Declaration)
        );
        assert!(
            !find_token(&tokens, source, "local", TokenKind::Variable, 1)
                .has_modifier(TokenModifier::Declaration)
        );
    }

    #[test]
    fn collects_enum_members_methods_and_builtin_symbols() {
        let source = "enum Option<T> { Some(T), None }\nfn Option.is_some(Option::Some(value)) {\n  log(value)\n}";
        let tokens = collect_for(source);

        assert!(
            find_token(&tokens, source, "T", TokenKind::TypeParameter, 0)
                .has_modifier(TokenModifier::Declaration)
        );
        assert!(
            find_token(&tokens, source, "Some", TokenKind::EnumMember, 0)
                .has_modifier(TokenModifier::Declaration)
        );
        assert!(
            find_token(&tokens, source, "is_some", TokenKind::Method, 0)
                .has_modifier(TokenModifier::Declaration)
        );
        assert!(
            !find_token(&tokens, source, "Some", TokenKind::EnumMember, 1)
                .has_modifier(TokenModifier::Declaration)
        );
        assert!(
            find_token(&tokens, source, "log", TokenKind::Function, 0)
                .has_modifier(TokenModifier::DefaultLibrary)
        );
    }

    #[test]
    fn collects_namespace_tokens_for_module_paths() {
        let source = "pub mod math;\nuse math::add as plus;\nfn main() { plus(1, 2) }";
        let tokens = collect_for(source);

        assert!(
            find_token(&tokens, source, "math", TokenKind::Namespace, 0)
                .has_modifier(TokenModifier::Declaration)
        );
        assert!(
            !find_token(&tokens, source, "math", TokenKind::Namespace, 1)
                .has_modifier(TokenModifier::Declaration)
        );
        assert!(
            find_token(&tokens, source, "plus", TokenKind::Variable, 0)
                .has_modifier(TokenModifier::Declaration)
        );
    }

    #[test]
    fn classifies_use_items_and_qualified_prefixes() {
        let source = "struct Point { x: i64 }\nfn Point.sum(self) -> i64 { self.x }\nuse Point::sum as plus;\nfn main() { plus }";
        let tokens = collect_for(source);

        assert_eq!(
            find_token(&tokens, source, "Point", TokenKind::Struct, 1).kind,
            TokenKind::Struct
        );
        assert_eq!(
            find_token(&tokens, source, "sum", TokenKind::Method, 1).kind,
            TokenKind::Method
        );
        assert!(
            find_token(&tokens, source, "plus", TokenKind::Variable, 0)
                .has_modifier(TokenModifier::Declaration)
        );
    }

    #[test]
    fn classifies_non_final_enum_path_segments() {
        let source = "enum Option<T> { Some(T), None }\nfn wrap(value) { Option::Some(value) }";
        let tokens = collect_for(source);

        assert_eq!(
            find_token(&tokens, source, "Option", TokenKind::Enum, 1).kind,
            TokenKind::Enum
        );
        assert_eq!(
            find_token(&tokens, source, "Some", TokenKind::EnumMember, 1).kind,
            TokenKind::EnumMember
        );
    }

    #[test]
    fn encodes_zero_based_multiline_positions() {
        let source = "fn add(value) {\n  value\n}";
        let tokens = collect_for(source);
        let encoded = encode(&tokens);
        let value_ref = encoded
            .data
            .iter()
            .find(|token| token.delta_line == 1 && token.delta_start == 2)
            .expect("expected token on line 1 column 2");
        assert_eq!(value_ref.length, 5);
        assert_eq!(value_ref.token_type, token_kind_index(TokenKind::Parameter));
        assert_eq!(value_ref.token_modifiers_bitset, modifiers_bitset(&[]));
    }
}
