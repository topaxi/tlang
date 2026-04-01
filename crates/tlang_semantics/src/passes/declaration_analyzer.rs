use log::debug;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use tlang_ast::keyword::kw;
use tlang_ast::node::{
    ConstDeclaration, EnumDeclaration, Expr, ExprKind, FunctionDeclaration, FunctionParameter,
    ImplBlock, Module, Pat, PatKind, Stmt, StmtKind, UnaryOp,
};
use tlang_ast::token::Literal;
use tlang_ast::visit::{Visitor, walk_expr, walk_stmt};
use tlang_defs::{Def, DefKind, DefScope};
use tlang_span::{NodeId, Span};

use crate::analyzer::{SemanticAnalysisContext, SemanticAnalysisPass};
use crate::diagnostic;

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
        self.push_symbol_table_inner(node_id, ctx, false)
    }

    fn push_function_symbol_table(
        &mut self,
        node_id: NodeId,
        ctx: &mut SemanticAnalysisContext,
    ) -> Rc<RefCell<DefScope>> {
        self.push_symbol_table_inner(node_id, ctx, true)
    }

    fn push_symbol_table_inner(
        &mut self,
        node_id: NodeId,
        ctx: &mut SemanticAnalysisContext,
        is_function_scope: bool,
    ) -> Rc<RefCell<DefScope>> {
        debug!("Entering new scope for node: {node_id} (function_scope={is_function_scope})");

        let parent = self.current_symbol_table().clone();
        let new_symbol_table = if is_function_scope {
            Rc::new(RefCell::new(DefScope::new_function_scope(parent)))
        } else {
            Rc::new(RefCell::new(DefScope::new(parent)))
        };
        ctx.symbol_tables.insert(node_id, new_symbol_table.clone());
        self.symbol_table_stack.push(new_symbol_table.clone());

        new_symbol_table
    }

    /// Pop the current scope from the symbol table stack.
    ///
    /// This is used for **both** function scopes (pushed via
    /// `push_function_symbol_table`) and block scopes (pushed via
    /// `push_symbol_table`).  The distinction between function and
    /// non-function scopes is encoded in the `DefScope::is_function_scope`
    /// flag, not in the push/pop path.
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
        let symbol_info = Def::new(id, name, kind, defined_at, scope_start).with_node_id(node_id);

        debug!("Declaring symbol: {symbol_info:#?}");

        self.current_symbol_table().borrow_mut().insert(symbol_info);
    }

    fn register_type_const_items(
        &mut self,
        ctx: &mut SemanticAnalysisContext,
        type_name: &str,
        consts: &[ConstDeclaration],
    ) {
        for const_item in consts {
            let qualified_name = format!("{}::{}", type_name, const_item.name.as_str());
            self.declare_symbol(
                ctx,
                const_item.id,
                &qualified_name,
                DefKind::Const,
                const_item.span,
                const_item.span.end_lc.line,
            );
        }
    }

    fn visit_impl_block(&mut self, impl_block: &ImplBlock, ctx: &mut SemanticAnalysisContext) {
        let protocol_name = impl_block.protocol_name.to_string();
        let target_type = impl_block.target_type.to_string();

        // Track this impl for constraint validation
        ctx.protocol_impls
            .push((protocol_name.clone(), target_type));

        for method in &impl_block.methods {
            // Register the protocol-qualified path (e.g., Greet::greet)
            let Some(method_name) = method.name() else {
                continue;
            };
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
            self.push_function_symbol_table(method.id, ctx);

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
            self.pop_symbol_table();
        }
    }

    /// Validate discriminant expressions in an enum declaration.
    ///
    /// Each discriminant must be a constant literal (or a unary negation of a
    /// literal) so that the JS backend can embed it directly in a `const`
    /// object initializer without triggering a temporal dead-zone reference.
    /// Duplicate discriminant values within the same enum are also flagged as
    /// a warning because they make match arm shadowing silent and confusing.
    fn validate_enum_discriminants(
        &self,
        decl: &EnumDeclaration,
        ctx: &mut SemanticAnalysisContext,
    ) {
        let mut seen_values: HashMap<DiscriminantKey, &str> = HashMap::new();
        for variant in &decl.variants {
            let Some(discriminant_expr) = variant.discriminant.as_deref() else {
                continue;
            };

            if !is_const_discriminant_expr(discriminant_expr) {
                ctx.add_diagnostic(diagnostic::error_at!(
                    discriminant_expr.span,
                    "discriminant value for `{}::{}` must be a constant literal",
                    decl.name,
                    variant.name,
                ));
                continue;
            }

            if let Some(lit) = extract_discriminant_literal(discriminant_expr) {
                let key = DiscriminantKey::from_literal(&lit);
                if let Some(prev_variant) = seen_values.get(&key) {
                    ctx.add_diagnostic(diagnostic::warn_at!(
                        variant.span,
                        "discriminant value of `{}::{}` is the same as `{}::{}`, earlier patterns will shadow this one",
                        decl.name,
                        variant.name,
                        decl.name,
                        prev_variant,
                    ));
                } else {
                    seen_values.insert(key, variant.name.as_str());
                }
            }
        }
    }
}

/// Check whether an expression is a constant-literal discriminant (i.e. a
/// value that can be computed without any references to the enclosing scope).
/// Acceptable forms are:
/// - A plain literal (`1`, `"foo"`, `true`, …)
/// - A unary negation applied directly to a numeric literal (`-1`)
///
/// Everything else (path references, binary expressions, function calls, …)
/// is rejected because in the JavaScript backend discriminant values are
/// embedded inside a single `const EnumName = { … }` initializer, so any
/// self-reference to `EnumName` would trigger a temporal dead-zone (TDZ)
/// reference error at runtime.
fn is_const_discriminant_expr(expr: &Expr) -> bool {
    match &expr.kind {
        ExprKind::Literal(_) => true,
        ExprKind::UnaryOp(UnaryOp::Minus, inner) => {
            matches!(inner.kind, ExprKind::Literal(_))
        }
        _ => false,
    }
}

/// Extract the normalised `Literal` from a constant-discriminant expression.
/// Returns `None` if the expression is not a constant literal (callers should
/// have validated with [`is_const_discriminant_expr`] first).
fn extract_discriminant_literal(expr: &Expr) -> Option<Literal> {
    match &expr.kind {
        ExprKind::Literal(lit) => Some(**lit),
        ExprKind::UnaryOp(UnaryOp::Minus, inner) => {
            if let ExprKind::Literal(lit) = &inner.kind {
                Some(lit.invert_sign())
            } else {
                None
            }
        }
        _ => None,
    }
}

/// A normalised, comparable representation of a discriminant literal used for
/// duplicate-value detection.  Integers and unsigned integers are normalised
/// to the same `i128` bucket so that `Integer(1)` and `UnsignedInteger(1)`
/// are considered equal.
#[derive(PartialEq, Eq, Hash, Debug)]
enum DiscriminantKey {
    Integer(i128),
    Float(u64),
    String(String),
    Bool(bool),
    None,
}

impl DiscriminantKey {
    fn from_literal(lit: &Literal) -> Self {
        match lit {
            Literal::Integer(n) => Self::Integer(*n as i128),
            Literal::UnsignedInteger(n) => Self::Integer(*n as i128),
            Literal::Float(f) => Self::Float(f.to_bits()),
            Literal::String(s) | Literal::Char(s) => Self::String(s.to_string()),
            Literal::Boolean(b) => Self::Bool(*b),
            Literal::None => Self::None,
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

    #[allow(clippy::too_many_lines)]
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

                // Register enum const members with qualified names
                self.register_type_const_items(ctx, decl.name.as_str(), &decl.consts);

                self.validate_enum_discriminants(decl, ctx);
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

                // Register struct const members with qualified names
                self.register_type_const_items(ctx, decl.name.as_str(), &decl.consts);

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

                // Register protocol constraints for later validation
                ctx.protocol_constraints.insert(
                    decl.name.to_string(),
                    decl.constraints.iter().map(|c| c.to_string()).collect(),
                );

                // Register protocol const members with qualified names
                self.register_type_const_items(ctx, decl.name.as_str(), &decl.consts);

                // Register protocol default method implementations as qualified
                // symbols so that call-site validation (`Foldable::sum(xs)`)
                // can resolve them, and so that lowering can create the correct
                // symbol-table scope.
                //
                // Abstract methods (those without a body) are NOT registered
                // here — they are registered later by the `impl` block that
                // provides the concrete implementation.
                let protocol_name = decl.name.as_str();
                for method in &decl.methods {
                    if method.body.is_none() {
                        continue;
                    }

                    let qualified_name = format!("{}::{}", protocol_name, method.name.as_str());
                    self.declare_symbol(
                        ctx,
                        method.id,
                        &qualified_name,
                        DefKind::ProtocolMethod(method.parameters.len() as u16),
                        method.span,
                        method.span.end_lc.line,
                    );

                    // Create a function scope so that the `self` parameter is
                    // reachable during lowering (for the implicit self-dispatch
                    // rewrite performed in `lower_protocol_decl`).
                    //
                    // Mirrors the scope setup in `visit_impl_block`: first a
                    // FunctionSelfRef for the method name, then parameters.
                    self.push_function_symbol_table(method.id, ctx);

                    // Declare the function self-reference binding (needed by the
                    // slot allocator — slot 0 is always the callee).
                    self.declare_symbol(
                        ctx,
                        method.id,
                        method.name.as_str(),
                        DefKind::FunctionSelfRef(method.parameters.len() as u16),
                        method.name.span,
                        method.name.span.end_lc.line,
                    );

                    self.symbol_type_context.push(DefKind::Parameter);
                    for param in &method.parameters {
                        self.collect_pattern(&param.pattern, param.span.end_lc.line, ctx);
                    }
                    self.symbol_type_context.pop();

                    // Walk the default body so that identifiers within it are
                    // visible to the variable-usage validator.
                    if let Some(body) = &method.body {
                        for stmt in &body.statements {
                            self.visit_stmt(stmt, ctx);
                        }
                        if let Some(expr) = &body.expression {
                            self.visit_expr(expr, ctx);
                        }
                    }

                    self.pop_symbol_table();
                }

                return; // Don't walk the statement again
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
            StmtKind::Const(decl) => {
                self.visit_expr(&decl.expression, ctx);
                self.declare_symbol(
                    ctx,
                    decl.id,
                    decl.name.as_str(),
                    DefKind::Const,
                    decl.span,
                    decl.span.end_lc.line,
                );
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
                | StmtKind::Const(_)
        ) {
            walk_stmt(self, stmt, ctx);
        }
    }

    fn visit_fn_decl(&mut self, declaration: &'ast FunctionDeclaration, ctx: &mut Self::Context) {
        let name_as_str = declaration.name_or_invalid();
        self.declare_symbol(
            ctx,
            declaration.id,
            &name_as_str,
            DefKind::Function(declaration.parameters.len() as u16),
            declaration.name.span,
            declaration.span.end_lc.line,
        );

        // For dot-method declarations (e.g. `fn Expense.is_food(...)`) also
        // register the `::` qualified form (`Expense::is_food`) so that
        // callers can reference the method as a first-class function value.
        if let ExprKind::FieldExpression(ref field_expr) = declaration.name.kind
            && let Some(base_path) = field_expr.base.path()
        {
            let qualified = format!("{}::{}", base_path, field_expr.field);
            self.declare_symbol(
                ctx,
                declaration.id,
                &qualified,
                DefKind::StructMethod(declaration.parameters.len() as u16),
                declaration.name.span,
                declaration.span.end_lc.line,
            );
        }

        // Enter function scope — marked as a function boundary so that
        // `DefScope::get_slot` can distinguish intra-function block access
        // from cross-function closure captures.
        self.push_function_symbol_table(declaration.id, ctx);

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
        self.pop_symbol_table();
    }

    fn visit_expr(&mut self, expr: &'ast Expr, ctx: &mut Self::Context) {
        match &expr.kind {
            ExprKind::Let(pattern, expr) => {
                self.visit_expr(expr, ctx);
                self.collect_pattern(pattern, expr.span.end_lc.line, ctx);
            }
            ExprKind::FunctionExpression(decl) => {
                // Declare the function self-reference symbol first
                let name_as_str = decl.name_or_invalid();
                self.push_function_symbol_table(decl.id, ctx);
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
                self.pop_symbol_table();
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
