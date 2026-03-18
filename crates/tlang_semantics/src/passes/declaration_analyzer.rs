use log::debug;
use std::cell::RefCell;
use std::rc::Rc;
use tlang_ast::keyword::kw;
use tlang_ast::node::{
    Expr, ExprKind, FunctionDeclaration, FunctionParameter, ImplBlock, Module, Pat, PatKind, Stmt,
    StmtKind,
};
use tlang_ast::visit::{Visitor, walk_expr, walk_stmt};
use tlang_span::{NodeId, Span};
use tlang_defs::{Def, DefScope, DefKind};

use crate::analyzer::{SemanticAnalysisContext, SemanticAnalysisPass};

/// The declaration analyzer is responsible for collecting all the declarations in a module.
#[derive(Default)]
pub struct DeclarationAnalyzer {
    symbol_table_stack: Vec<Rc<RefCell<DefScope>>>,
    symbol_type_context: Vec<DefKind>,
}

impl DeclarationAnalyzer {
    fn current_symbol_table(&self) -> &Rc<RefCell<DefScope>> {
        self.symbol_table_stack.last().unwrap()
    }

    fn push_symbol_table(
        &mut self,
        node_id: NodeId,
        ctx: &mut SemanticAnalysisContext,
    ) -> Rc<RefCell<DefScope>> {
        debug!("Entering new scope for node: {node_id}");

        let parent = self.current_symbol_table().clone();
        let new_symbol_table = Rc::new(RefCell::new(DefScope::new(parent)));
        ctx.symbol_tables.insert(node_id, new_symbol_table.clone());
        self.symbol_table_stack.push(new_symbol_table.clone());

        new_symbol_table
    }

    fn pop_symbol_table(&mut self) -> Rc<RefCell<DefScope>> {
        debug!("Leaving scope");

        self.symbol_table_stack.pop().unwrap()
    }

    #[inline(always)]
    fn declare_symbol(
        &mut self,
        ctx: &mut SemanticAnalysisContext,
        node_id: NodeId,
        name: &str,
        kind: DefKind,
        defined_at: Span,
        scope_start: u32,
    ) {
        let id = ctx.symbol_id_allocator.next_id();
        let symbol_info =
            Def::new(id, name, kind, defined_at, scope_start).with_node_id(node_id);

        debug!("Declaring symbol: {symbol_info:#?}");

        self.current_symbol_table().borrow_mut().insert(symbol_info);
    }

    fn visit_impl_block(&mut self, impl_block: &ImplBlock, ctx: &mut SemanticAnalysisContext) {
        let protocol_name = impl_block.protocol_name.to_string();
        for method in &impl_block.methods {
            // Register the protocol-qualified path (e.g., Greet::greet)
            let method_name = method.name();
            let qualified_name = format!("{protocol_name}::{method_name}");
            self.declare_symbol(
                ctx,
                method.id,
                &qualified_name,
                DefKind::ProtocolMethod(method.parameters.len() as u16),
                method.span,
                method.span.end_lc.line,
            );

            // Enter function scope for parameter/body analysis
            self.enter_scope(method.id, ctx);

            // Declare the function self-reference binding (needed for
            // multi-clause lowering which calls shift() to remove it).
            self.declare_symbol(
                ctx,
                method.id,
                &method_name,
                DefKind::FunctionSelfRef(method.parameters.len() as u16),
                method.name.span,
                method.name.span.end_lc.line,
            );

            self.symbol_type_context.push(DefKind::Parameter);
            for param in &method.parameters {
                self.collect_pattern(&param.pattern, param.span.end_lc.line, ctx);
            }
            self.symbol_type_context.pop();

            for stmt in &method.body.statements {
                self.visit_stmt(stmt, ctx);
            }
            if let Some(expr) = &method.body.expression {
                self.visit_expr(expr, ctx);
            }
            self.leave_scope(method.id, ctx);
        }
    }
}

impl SemanticAnalysisPass for DeclarationAnalyzer {
    fn analyze(&mut self, module: &Module, ctx: &mut SemanticAnalysisContext, is_root: bool) {
        // Initialize symbol table stack with root table
        self.symbol_table_stack = vec![ctx.root_symbol_table.clone()];

        if !is_root {
            self.push_symbol_table(module.id, ctx);
        }

        self.visit_module(module, ctx);

        if !is_root {
            self.pop_symbol_table();
        }

        // After collecting all declarations, we should be left with the root symbol table on the
        // symbol table stack.
        debug_assert_eq!(self.symbol_table_stack.len(), 1);
    }
}

impl<'ast> Visitor<'ast> for DeclarationAnalyzer {
    type Context = SemanticAnalysisContext;

    fn enter_scope(&mut self, node_id: NodeId, ctx: &mut Self::Context) {
        self.push_symbol_table(node_id, ctx);
    }

    fn leave_scope(&mut self, _node_id: NodeId, _ctx: &mut Self::Context) {
        self.pop_symbol_table();
    }

    fn visit_module(&mut self, module: &'ast Module, ctx: &mut Self::Context) {
        // Don't use walk_module as it includes scope management that conflicts
        // with our explicit scope management in analyze method
        for statement in &module.statements {
            self.visit_stmt(statement, ctx);
        }
    }

    fn visit_stmt(&mut self, stmt: &'ast Stmt, ctx: &mut Self::Context) {
        match &stmt.kind {
            StmtKind::FunctionDeclaration(decl) => {
                self.visit_fn_decl(decl, ctx);
            }
            StmtKind::FunctionDeclarations(decls) => {
                for decl in decls {
                    self.visit_fn_decl(decl, ctx);
                }
            }
            StmtKind::EnumDeclaration(decl) => {
                self.declare_symbol(
                    ctx,
                    stmt.id,
                    decl.name.as_str(),
                    DefKind::Enum,
                    stmt.span,
                    stmt.span.end_lc.line,
                );

                for element in &decl.variants {
                    self.declare_symbol(
                        ctx,
                        element.id,
                        &(decl.name.to_string() + "::" + element.name.as_str()),
                        DefKind::EnumVariant(element.parameters.len() as u16),
                        element.span,
                        element.span.end_lc.line,
                    );
                }
            }
            StmtKind::StructDeclaration(decl) => {
                self.declare_symbol(
                    ctx,
                    stmt.id,
                    decl.name.as_str(),
                    DefKind::Struct,
                    stmt.span,
                    stmt.span.end_lc.line,
                );

                // Also store the struct declaration for later reference
                ctx.struct_declarations
                    .insert(decl.name.to_string(), (**decl).clone());
            }
            StmtKind::ProtocolDeclaration(decl) => {
                self.declare_symbol(
                    ctx,
                    stmt.id,
                    decl.name.as_str(),
                    DefKind::Protocol,
                    stmt.span,
                    stmt.span.end_lc.line,
                );
            }
            StmtKind::ImplBlock(impl_block) => {
                self.visit_impl_block(impl_block, ctx);
                return; // Don't walk the statement again
            }
            StmtKind::Let(decl) => {
                self.visit_expr(&decl.expression, ctx);
                self.collect_pattern(&decl.pattern, stmt.span.end_lc.line, ctx);
                return; // Don't walk the statement again
            }
            _ => {}
        }

        // For other statement types, use the default walker
        if !matches!(
            &stmt.kind,
            StmtKind::FunctionDeclaration(_)
                | StmtKind::FunctionDeclarations(_)
                | StmtKind::EnumDeclaration(_)
                | StmtKind::StructDeclaration(_)
                | StmtKind::ProtocolDeclaration(_)
                | StmtKind::ImplBlock(_)
                | StmtKind::Let(_)
        ) {
            walk_stmt(self, stmt, ctx);
        }
    }

    fn visit_fn_decl(&mut self, declaration: &'ast FunctionDeclaration, ctx: &mut Self::Context) {
        let name_as_str = declaration.name();

        self.declare_symbol(
            ctx,
            declaration.id,
            &name_as_str,
            DefKind::Function(declaration.parameters.len() as u16),
            declaration.name.span,
            declaration.span.end_lc.line,
        );

        // Enter function scope
        self.enter_scope(declaration.id, ctx);

        // The function name is also declared and bound within the function itself.
        // Similar to what JS does.
        self.declare_symbol(
            ctx,
            declaration.id,
            &name_as_str,
            DefKind::FunctionSelfRef(declaration.parameters.len() as u16),
            declaration.name.span,
            declaration.name.span.end_lc.line,
        );

        // Handle parameters
        self.symbol_type_context.push(DefKind::Parameter);
        for param in &declaration.parameters {
            self.visit_fn_param(param, ctx);
        }
        self.symbol_type_context.pop();

        // Handle guard and body
        if let Some(ref guard) = declaration.guard {
            self.visit_expr(guard, ctx);
        }

        self.visit_block(
            &declaration.body.statements,
            &declaration.body.expression,
            ctx,
        );

        // Leave function scope
        self.leave_scope(declaration.id, ctx);
    }

    fn visit_expr(&mut self, expr: &'ast Expr, ctx: &mut Self::Context) {
        match &expr.kind {
            ExprKind::Let(pattern, expr) => {
                self.visit_expr(expr, ctx);
                self.collect_pattern(pattern, expr.span.end_lc.line, ctx);
            }
            ExprKind::FunctionExpression(decl) => {
                // Declare the function self-reference symbol first
                let name_as_str = decl.name();
                self.enter_scope(decl.id, ctx);
                self.declare_symbol(
                    ctx,
                    decl.id,
                    &name_as_str,
                    DefKind::FunctionSelfRef(decl.parameters.len() as u16),
                    decl.name.span,
                    decl.name.span.end_lc.line,
                );

                // Use default function declaration walker for the rest
                self.symbol_type_context.push(DefKind::Parameter);
                for param in &decl.parameters {
                    self.visit_fn_param(param, ctx);
                }
                self.symbol_type_context.pop();

                if let Some(ref guard) = decl.guard {
                    self.visit_expr(guard, ctx);
                }

                self.visit_block(&decl.body.statements, &decl.body.expression, ctx);
                self.leave_scope(decl.id, ctx);
            }
            _ => {
                // For all other expressions, use the default walker which includes
                // proper scope management for ForLoop, Match, IfElse, Block, etc.
                walk_expr(self, expr, ctx);
            }
        }
    }

    fn visit_fn_param(&mut self, parameter: &'ast FunctionParameter, ctx: &mut Self::Context) {
        self.collect_pattern(&parameter.pattern, parameter.span.end_lc.line, ctx);
        // Don't visit type annotation as it doesn't contain declarations
    }

    fn visit_pat(&mut self, pattern: &'ast Pat, ctx: &mut Self::Context) {
        // Override to use collect_pattern instead of the default walker
        // This ensures that when the default walkers call visit_pat, we collect patterns correctly
        self.collect_pattern(pattern, pattern.span.end_lc.line, ctx);
    }
}

impl DeclarationAnalyzer {
    fn collect_pattern(
        &mut self,
        pattern: &Pat,
        scope_start: u32,
        ctx: &mut SemanticAnalysisContext,
    ) {
        match &pattern.kind {
            PatKind::Identifier(ident) => {
                self.declare_symbol(
                    ctx,
                    pattern.id,
                    ident.as_str(),
                    self.symbol_type_context
                        .last()
                        .copied()
                        .unwrap_or(DefKind::Variable),
                    pattern.span,
                    scope_start,
                );
            }
            PatKind::_Self => {
                self.declare_symbol(
                    ctx,
                    pattern.id,
                    kw::_Self,
                    DefKind::Variable,
                    pattern.span,
                    scope_start,
                );
            }
            PatKind::List(patterns) => {
                for pattern in patterns {
                    self.collect_pattern(pattern, scope_start, ctx);
                }
            }
            PatKind::Rest(pattern) => {
                self.collect_pattern(pattern, scope_start, ctx);
            }
            PatKind::Enum(enum_pattern) => {
                for (_ident, pat) in &enum_pattern.elements {
                    self.collect_pattern(pat, scope_start, ctx);
                }
            }
            PatKind::Wildcard => {} // Wildcard discards values, nothing to do here.
            PatKind::Literal(_) => {} // Literal patterns don't need to be declared.
            _ => {}
        }
    }
}
