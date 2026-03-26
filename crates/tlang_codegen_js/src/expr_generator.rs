use oxc_ast::NONE;
use oxc_ast::ast::*;
use oxc_span::{GetSpanMut, SPAN};
use tlang_ast::node::{self as ast, Ident};
use tlang_ast::token::Literal;
use tlang_hir as hir;

use crate::binary_operator_generator::{map_binary_op, map_unary_op};
use crate::generator::InnerCodegen;
use crate::js;

impl<'a> InnerCodegen<'a> {
    pub fn generate_expr(&mut self, expr: &hir::Expr) -> Expression<'a> {
        let mut js_expr = match &expr.kind {
            hir::ExprKind::Literal(literal) => self.generate_literal(literal),
            hir::ExprKind::Path(path) => self.generate_path_expression(path),
            hir::ExprKind::Binary(op, lhs, rhs) if *op == hir::BinaryOpKind::Assign => {
                self.generate_assignment(lhs, rhs)
            }
            hir::ExprKind::Binary(op, lhs, rhs) => self.generate_binary_op(*op, lhs, rhs),
            hir::ExprKind::Unary(op, inner) => self.generate_unary_op(op, inner),
            hir::ExprKind::Call(call_expr) => self.generate_call_expression(call_expr),
            hir::ExprKind::TailCall(call_expr) => {
                // Mutual tail call (not self-referencing) → regular call
                self.generate_call_expression(call_expr)
            }
            hir::ExprKind::Cast(inner, _) => self.generate_expr(inner),
            hir::ExprKind::FieldAccess(base, field) => {
                self.generate_field_access_expression(base, field)
            }
            hir::ExprKind::IndexAccess(base, index) => {
                self.generate_index_access_expression(base, index)
            }
            hir::ExprKind::List(items) => self.generate_list_expression(items),
            hir::ExprKind::Dict(kvs) => self.generate_dict_expression(kvs),
            hir::ExprKind::FunctionExpression(decl) => self.generate_function_expression(decl),
            hir::ExprKind::IfElse(cond, then_branch, else_branches) => {
                // After ANF, if-else in expression position is always ternary-worthy.
                self.generate_ternary(cond, then_branch, else_branches)
            }
            hir::ExprKind::Wildcard => {
                // Wildcard in expression position is used for partial application
                // and is handled by the call expression generator.
                self.undefined_expr()
            }
            hir::ExprKind::Break(_)
            | hir::ExprKind::Continue
            | hir::ExprKind::Block(_)
            | hir::ExprKind::Loop(_)
            | hir::ExprKind::Match(..) => {
                unreachable!(
                    "Block/Loop/Match/Break/Continue should not appear in expression position after ANF"
                )
            }
            hir::ExprKind::Let(..) => self.unsupported_expr("let expressions", expr.span),
            hir::ExprKind::Range(_) => self.unsupported_expr("range expressions", expr.span),
        };
        *js_expr.span_mut() = Self::hir_span(expr.span);
        js_expr
    }

    pub fn generate_literal(&mut self, literal: &Literal) -> Expression<'a> {
        match literal {
            Literal::Integer(value) => self.num_expr(*value as f64),
            Literal::UnsignedInteger(value) => self.num_expr(*value as f64),
            Literal::Float(value) => self.num_expr(*value),
            Literal::Boolean(value) => self.bool_expr(*value),
            Literal::String(id) | Literal::Char(id) => self.str_expr(tlang_intern::get(*id)),
            Literal::None => self.undefined_expr(),
        }
    }

    fn generate_path_expression(&mut self, path: &hir::Path) -> Expression<'a> {
        // If we have a full identifier which resolves in the current scope, use it directly.
        if let Some(identifier) = self.current_scope().resolve_variable(&path.to_string()) {
            return self.ident_expr(&js::safe_js_variable_name(&identifier));
        }

        let first_segment = path.segments.first().unwrap();
        let first_name = first_segment.ident.as_str();

        let mut result = if self.is_protocol(first_name) {
            self.ident_expr(&CodegenJS::protocol_js_name(first_name))
        } else if let Some(identifier) = self.current_scope().resolve_variable(first_name) {
            // First segment resolves individually (e.g. `Option` → `Option`).
            self.ident_expr(&js::safe_js_variable_name(&identifier))
        } else if !path.res.is_unresolved() {
            // The semantic analyser resolved this reference (e.g. a cross-module
            // import or a semantic-builtin hint) even though the codegen scope
            // does not have a local entry yet.  Emit the JS-safe first segment
            // name as-is so the bundled output remains correct.
            self.ident_expr(&js::safe_js_variable_name(first_name))
        } else {
            // Truly unresolved: the semantic analyser never found this binding.
            // This is a compiler bug — emit an error and a safe fallback.
            self.generate_identifier(&first_segment.ident)
        };

        for segment in &path.segments[1..] {
            let prop = js::safe_js_variable_name(segment.ident.as_str());
            result = self.static_member_expr(result, &prop);
        }

        result
    }

    fn generate_identifier(&mut self, name: &Ident) -> Expression<'a> {
        let name_string = name.as_str();
        match self.current_scope().resolve_variable(name_string) {
            Some(identifier) => self.ident_expr(&js::safe_js_variable_name(&identifier)),
            None => self.unresolved_identifier_expr(name_string, name.span),
        }
    }

    fn generate_assignment(&mut self, lhs: &hir::Expr, rhs: &hir::Expr) -> Expression<'a> {
        let target = self.generate_assignment_target(lhs);
        let value = self.generate_expr(rhs);
        self.assign_expr(target, value)
    }

    fn generate_assignment_target(&mut self, expr: &hir::Expr) -> AssignmentTarget<'a> {
        match &expr.kind {
            hir::ExprKind::Path(path) => {
                let name = if let Some(resolved) =
                    self.current_scope().resolve_variable(&path.to_string())
                {
                    js::safe_js_variable_name(&resolved)
                } else {
                    js::safe_js_variable_name(path.first_ident().as_str())
                };

                if path.segments.len() == 1 {
                    self.assignment_target_ident(&name)
                } else {
                    let mut obj = self.ident_expr(&name);
                    for segment in &path.segments[1..path.segments.len() - 1] {
                        let prop = js::safe_js_variable_name(segment.ident.as_str());
                        obj = self.static_member_expr(obj, &prop);
                    }
                    let last =
                        js::safe_js_variable_name(path.segments.last().unwrap().ident.as_str());
                    self.assignment_target_member(obj, &last)
                }
            }
            hir::ExprKind::FieldAccess(base, field) => {
                let obj = self.generate_expr(base);
                self.assignment_target_member(obj, field.as_str())
            }
            hir::ExprKind::IndexAccess(base, index) => {
                let obj = self.generate_expr(base);
                let idx = self.generate_expr(index);
                self.assignment_target_computed(obj, idx)
            }
            _ => unreachable!("Invalid assignment target"),
        }
    }

    fn generate_binary_op(
        &mut self,
        op: hir::BinaryOpKind,
        lhs: &hir::Expr,
        rhs: &hir::Expr,
    ) -> Expression<'a> {
        let left = self.generate_expr(lhs);
        let right = self.generate_expr(rhs);

        match map_binary_op(op) {
            Ok(bin_op) => self.ast.expression_binary(SPAN, left, bin_op, right),
            Err(log_op) => self.ast.expression_logical(SPAN, left, log_op, right),
        }
    }

    fn generate_unary_op(&mut self, op: &ast::UnaryOp, expr: &hir::Expr) -> Expression<'a> {
        if matches!(op, ast::UnaryOp::Spread) {
            // Spread in expression position: generate $spread(expr) without the `...` prefix.
            // The actual SpreadElement wrapping happens in list/call expression generators.
            let inner = self.generate_expr(expr);
            let spread_fn = self.ident_expr("$spread");
            return self.call_expr(spread_fn, vec![Argument::from(inner)]);
        }

        let argument = self.generate_expr(expr);
        self.ast.expression_unary(SPAN, map_unary_op(op), argument)
    }

    fn generate_field_access_expression(
        &mut self,
        base: &hir::Expr,
        field: &Ident,
    ) -> Expression<'a> {
        let obj = self.generate_expr(base);
        self.static_member_expr(obj, field.as_str())
    }

    fn generate_index_access_expression(
        &mut self,
        base: &hir::Expr,
        index: &hir::Expr,
    ) -> Expression<'a> {
        let obj = self.generate_expr(base);
        let idx = self.generate_expr(index);
        self.computed_member_expr(obj, idx)
    }

    fn generate_list_expression(&mut self, items: &[hir::Expr]) -> Expression<'a> {
        let elements: Vec<ArrayExpressionElement<'a>> = items
            .iter()
            .map(|item| {
                if let hir::ExprKind::Unary(ast::UnaryOp::Spread, inner) = &item.kind {
                    let inner_expr = self.generate_expr(inner);
                    let spread_fn = self.ident_expr("$spread");
                    let call = self.call_expr(spread_fn, vec![Argument::from(inner_expr)]);
                    ArrayExpressionElement::SpreadElement(self.ast.alloc_spread_element(SPAN, call))
                } else {
                    ArrayExpressionElement::from(self.generate_expr(item))
                }
            })
            .collect();
        self.ast
            .expression_array(SPAN, self.ast.vec_from_iter(elements))
    }

    fn generate_dict_expression(&mut self, kvs: &[(hir::Expr, hir::Expr)]) -> Expression<'a> {
        let properties: Vec<ObjectPropertyKind<'a>> = kvs
            .iter()
            .map(|(key, value)| {
                let shorthand = key.path() == value.path();
                // Dict keys are always property names, not variable references.
                // For single-segment path keys (simple identifiers like `a` in
                // `{ a: 1 }`), emit the raw name directly without scope lookup.
                // Multi-segment paths (e.g. `Foo::bar`) are evaluated as computed
                // keys to preserve their semantics.
                let key_expr = match &key.kind {
                    hir::ExprKind::Path(path) if path.segments.len() == 1 => {
                        self.ident_expr(&js::safe_js_variable_name(path.first_ident().as_str()))
                    }
                    _ => self.generate_expr(key),
                };
                let value_expr = self.generate_expr(value);
                let property_key = PropertyKey::from(key_expr);
                ObjectPropertyKind::ObjectProperty(self.ast.alloc_object_property(
                    SPAN,
                    PropertyKind::Init,
                    property_key,
                    value_expr,
                    false,
                    shorthand,
                    false,
                ))
            })
            .collect();
        self.ast
            .expression_object(SPAN, self.ast.vec_from_iter(properties))
    }

    fn generate_ternary(
        &mut self,
        cond: &hir::Expr,
        then_branch: &hir::Block,
        else_branches: &[hir::ElseClause],
    ) -> Expression<'a> {
        let test = self.generate_expr(cond);
        let consequent = self.generate_expr(then_branch.expr.as_ref().unwrap());
        let alternate = self.generate_expr(else_branches[0].consequence.expr.as_ref().unwrap());
        self.ast
            .expression_conditional(SPAN, test, consequent, alternate)
    }

    /// Generate an if-else chain as statements (not ternary).
    pub fn generate_if_else_stmts(
        &mut self,
        cond: &hir::Expr,
        then_branch: &hir::Block,
        else_branches: &[hir::ElseClause],
    ) -> Vec<Statement<'a>> {
        let test = self.generate_expr(cond);
        let consequent_stmts = self.generate_block_stmts_scoped(then_branch);
        let consequent = self.block_stmt(consequent_stmts);

        let alternate = if else_branches.is_empty() {
            None
        } else {
            Some(self.generate_else_chain(else_branches))
        };

        vec![self.ast.statement_if(SPAN, test, consequent, alternate)]
    }

    fn generate_else_chain(&mut self, else_branches: &[hir::ElseClause]) -> Statement<'a> {
        if else_branches.is_empty() {
            return self.block_stmt(vec![]);
        }

        let branch = &else_branches[0];
        let body_stmts = self.generate_block_stmts_scoped(&branch.consequence);
        let body = self.block_stmt(body_stmts);

        if let Some(ref condition) = branch.condition {
            // else if (condition) { ... }
            let test = self.generate_expr(condition);
            let alternate = if else_branches.len() > 1 {
                Some(self.generate_else_chain(&else_branches[1..]))
            } else {
                None
            };
            self.ast.statement_if(SPAN, test, body, alternate)
        } else {
            // else { ... }
            body
        }
    }

    pub fn generate_call_expression(&mut self, call_expr: &hir::CallExpression) -> Expression<'a> {
        let wildcard_count = call_expr.wildcard_count();

        if wildcard_count > 0 {
            return self.generate_partial_application(call_expr, wildcard_count);
        }

        let callee = self.generate_expr(&call_expr.callee);
        let args: Vec<Argument<'a>> = call_expr
            .arguments
            .iter()
            .map(|arg| {
                if let hir::ExprKind::Unary(ast::UnaryOp::Spread, inner) = &arg.kind {
                    let inner_expr = self.generate_expr(inner);
                    let spread_fn = self.ident_expr("$spread");
                    let call = self.call_expr(spread_fn, vec![Argument::from(inner_expr)]);
                    Argument::SpreadElement(self.ast.alloc_spread_element(SPAN, call))
                } else {
                    Argument::from(self.generate_expr(arg))
                }
            })
            .collect();
        self.call_expr(callee, args)
    }

    fn generate_partial_application(
        &mut self,
        call_expr: &hir::CallExpression,
        wildcard_count: usize,
    ) -> Expression<'a> {
        let mut placeholders = Vec::with_capacity(wildcard_count);

        if wildcard_count == 1 {
            placeholders.push("_".to_string());
        } else {
            for _ in 0..wildcard_count {
                let tmp = self.current_scope().declare_unique_variable("_");
                placeholders.push(tmp);
            }
        }

        // Build arrow: (placeholders) => callee(args_with_placeholders)
        let params: Vec<FormalParameter<'a>> = placeholders
            .iter()
            .map(|name| self.formal_param(name))
            .collect();
        let formal_params = self.formal_params(self.ast.vec_from_iter(params));

        let callee = self.generate_expr(&call_expr.callee);
        let mut wildcard_index = 0;
        let args: Vec<Argument<'a>> = call_expr
            .arguments
            .iter()
            .map(|arg| {
                if let hir::ExprKind::Wildcard = arg.kind {
                    let ph = &placeholders[wildcard_index];
                    wildcard_index += 1;
                    Argument::from(self.ident_expr(ph))
                } else {
                    Argument::from(self.generate_expr(arg))
                }
            })
            .collect();

        let call = self.call_expr(callee, args);
        let body_stmt = self.ast.statement_expression(SPAN, call);
        let body = self.fn_body(self.ast.vec1(body_stmt));

        self.ast.expression_arrow_function(
            SPAN,
            true, // expression = true (body is single expression)
            false,
            NONE,
            formal_params,
            NONE,
            body,
        )
    }

    /// Generate block statements (stmts + optional completion as ExpressionStatement).
    pub fn generate_block_stmts(&mut self, block: &hir::Block) -> Vec<Statement<'a>> {
        let mut result = self.generate_stmts(&block.stmts);

        if let Some(expr) = &block.expr {
            // Only emit completion if it has side effects
            if !matches!(
                expr.kind,
                hir::ExprKind::Path(_) | hir::ExprKind::Literal(_)
            ) {
                let e = self.generate_expr(expr);
                result.push(self.expr_stmt(e));
            }
        }

        result
    }

    /// Generate block statements with scope isolation (no propagation).
    /// Variables declared inside won't leak to the parent scope.
    pub fn generate_block_stmts_scoped(&mut self, block: &hir::Block) -> Vec<Statement<'a>> {
        self.push_scope();
        let result = self.generate_block_stmts(block);
        self.pop_scope();
        result
    }

    /// Generate block statements, propagating local variables to the parent scope.
    pub fn generate_block_stmts_propagate_scope(
        &mut self,
        block: &hir::Block,
    ) -> Vec<Statement<'a>> {
        self.push_scope();
        let result = self.generate_block_stmts(block);
        let locals = self.current_scope().local_variables().clone();
        self.pop_scope();
        for (name, js_name) in locals {
            self.current_scope().declare_variable_alias(&name, &js_name);
        }
        result
    }

    /// Generate a loop expression as statements.
    pub fn generate_loop_stmts(&mut self, block: &hir::Block) -> Vec<Statement<'a>> {
        let body_stmts = self.generate_block_stmts_scoped(block);
        let body = self.block_stmt(body_stmts);
        // for (;;) { ... }
        vec![self.ast.statement_for(SPAN, None, None, None, body)]
    }

    /// Generate break statement(s).
    pub fn generate_break_stmts(&mut self, value: &Option<Box<hir::Expr>>) -> Vec<Statement<'a>> {
        let mut stmts = Vec::new();
        if let Some(value) = value {
            let e = self.generate_expr(value);
            stmts.push(self.expr_stmt(e));
        }
        stmts.push(self.ast.statement_break(SPAN, None));
        stmts
    }
}

use crate::generator::CodegenJS;
