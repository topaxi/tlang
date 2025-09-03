use log::debug;
use std::cell::RefCell;
use std::rc::Rc;
use tlang_ast::keyword::kw;
use tlang_ast::node::{
    Expr, ExprKind, FunctionDeclaration, FunctionParameter, Module, Pat, PatKind, Stmt, StmtKind,
};
use tlang_ast::visit::{Visitor, walk_expr, walk_stmt};
use tlang_span::{LineColumn, NodeId, Span};
use tlang_symbols::{SymbolInfo, SymbolTable, SymbolType};

use crate::analyzer::{SemanticAnalysisContext, SemanticAnalysisPass};

/**
 * The declaration analyzer is responsible for collecting all the declarations in a module.
 */
pub struct DeclarationAnalyzer {
    symbol_table_stack: Vec<Rc<RefCell<SymbolTable>>>,
    symbol_type_context: Vec<SymbolType>,
}

impl Default for DeclarationAnalyzer {
    fn default() -> Self {
        Self::new()
    }
}

impl DeclarationAnalyzer {
    pub fn new() -> Self {
        DeclarationAnalyzer {
            symbol_table_stack: vec![],
            symbol_type_context: vec![],
        }
    }

    fn current_symbol_table(&self) -> &Rc<RefCell<SymbolTable>> {
        self.symbol_table_stack.last().unwrap()
    }

    fn push_symbol_table(
        &mut self,
        node_id: NodeId,
        ctx: &mut SemanticAnalysisContext,
    ) -> Rc<RefCell<SymbolTable>> {
        debug!("Entering new scope for node: {}", node_id);

        let parent = self.current_symbol_table().clone();
        let new_symbol_table = Rc::new(RefCell::new(SymbolTable::new(parent)));
        ctx.symbol_tables.insert(node_id, new_symbol_table.clone());
        self.symbol_table_stack.push(new_symbol_table.clone());

        new_symbol_table
    }

    fn pop_symbol_table(&mut self) -> Rc<RefCell<SymbolTable>> {
        debug!("Leaving scope");

        self.symbol_table_stack.pop().unwrap()
    }

    #[inline(always)]
    fn declare_symbol(
        &mut self,
        ctx: &mut SemanticAnalysisContext,
        node_id: NodeId,
        name: &str,
        symbol_type: SymbolType,
        defined_at: Span,
        scope_start: LineColumn,
    ) {
        let id = ctx.symbol_id_allocator.next_id();
        let symbol_info =
            SymbolInfo::new(id, name, symbol_type, defined_at, scope_start).with_node_id(node_id);

        debug!("Declaring symbol: {:#?}", symbol_info);

        self.current_symbol_table().borrow_mut().insert(symbol_info);
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
                    SymbolType::Enum,
                    stmt.span,
                    stmt.span.end,
                );

                for element in &decl.variants {
                    self.declare_symbol(
                        ctx,
                        element.id,
                        &(decl.name.to_string() + "::" + element.name.as_str()),
                        SymbolType::EnumVariant(element.parameters.len() as u16),
                        element.span,
                        element.span.end,
                    );
                }
            }
            StmtKind::StructDeclaration(decl) => {
                self.declare_symbol(
                    ctx,
                    stmt.id,
                    decl.name.as_str(),
                    SymbolType::Struct,
                    stmt.span,
                    stmt.span.end,
                );
            }
            StmtKind::Let(decl) => {
                self.visit_expr(&decl.expression, ctx);
                self.collect_pattern(&decl.pattern, stmt.span.end, ctx);
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
            SymbolType::Function(declaration.parameters.len() as u16),
            declaration.name.span,
            declaration.span.end,
        );

        // Enter function scope
        self.enter_scope(declaration.id, ctx);

        // The function name is also declared and bound within the function itself.
        // Similar to what JS does.
        self.declare_symbol(
            ctx,
            declaration.id,
            &name_as_str,
            SymbolType::FunctionSelfRef(declaration.parameters.len() as u16),
            declaration.name.span,
            declaration.name.span.end,
        );

        // Handle parameters
        self.symbol_type_context.push(SymbolType::Parameter);
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
                self.collect_pattern(pattern, expr.span.end, ctx);
            }
            ExprKind::FunctionExpression(decl) => {
                // Declare the function self-reference symbol first
                let name_as_str = decl.name();
                self.enter_scope(decl.id, ctx);
                self.declare_symbol(
                    ctx,
                    decl.id,
                    &name_as_str,
                    SymbolType::FunctionSelfRef(decl.parameters.len() as u16),
                    decl.name.span,
                    decl.name.span.end,
                );

                // Use default function declaration walker for the rest
                self.symbol_type_context.push(SymbolType::Parameter);
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
        self.collect_pattern(&parameter.pattern, parameter.span.end, ctx);
        // Don't visit type annotation as it doesn't contain declarations
    }

    fn visit_pat(&mut self, pattern: &'ast Pat, ctx: &mut Self::Context) {
        // Override to use collect_pattern instead of the default walker
        // This ensures that when the default walkers call visit_pat, we collect patterns correctly
        self.collect_pattern(pattern, pattern.span.end, ctx);
    }
}

impl DeclarationAnalyzer {
    fn collect_pattern(
        &mut self,
        pattern: &Pat,
        scope_start: LineColumn,
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
                        .unwrap_or(SymbolType::Variable),
                    pattern.span,
                    scope_start,
                );
            }
            PatKind::_Self => {
                self.declare_symbol(
                    ctx,
                    pattern.id,
                    kw::_Self,
                    SymbolType::Variable,
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
