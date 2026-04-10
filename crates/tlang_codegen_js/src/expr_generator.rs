use oxc_ast::NONE;
use oxc_ast::ast::*;
use oxc_span::{GetSpanMut, SPAN};
use tlang_ast::node::{self as ast, Ident};
use tlang_ast::token::Literal;
use tlang_hir as hir;

use crate::binary_operator_generator::{map_binary_op, map_unary_op};
use crate::generator::InnerCodegen;
use crate::js;
use tlang_builtins_js as builtins;

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
            hir::ExprKind::Cast(inner, target_ty) => self.generate_cast_expr(inner, target_ty),
            hir::ExprKind::TryCast(inner, target_ty) => {
                self.generate_try_cast_expr(inner, target_ty)
            }
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
            hir::ExprKind::Implements(value_expr, protocol_path) => {
                self.generate_implements_expr(value_expr, protocol_path)
            }
            hir::ExprKind::TaggedString { tag, parts, exprs } => {
                self.generate_tagged_string(tag, parts, exprs)
            }
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
        let first_segment = path.segments.first().unwrap();
        let first_name = first_segment.ident.as_str();

        // 1. Single-segment: HirId-based resolution (user-defined bindings)
        if path.segments.len() == 1
            && let Some(hir_id) = path.res.hir_id()
            && let Some(js_name) = self.name_map.resolve(hir_id)
        {
            return self.ident_expr(js_name);
        }

        // 2. Multi-segment: resolve first segment, then member-chain the rest.
        //    E.g. `Vector::new` → `Vector.$new`, `Option::Some` → `Option.Some`
        if path.segments.len() > 1 {
            let path_str = path.to_string();

            // 2a. Full path builtin (e.g. "math::pi" → "Math.PI")
            if let Some(js_name) = builtins::lookup(&path_str) {
                return self.ident_expr(js_name);
            }

            // 2b. Protocol check
            if self.is_protocol(first_name) {
                let mut result = self.ident_expr(&CodegenJS::protocol_js_name(first_name));
                for segment in &path.segments[1..] {
                    let prop = js::safe_js_variable_name(segment.ident.as_str());
                    result = self.static_member_expr(result, &prop);
                }
                return result;
            }

            // 2c. First-segment builtin with member chain (e.g. "math" → "Math")
            if let Some(first_js) = builtins::lookup(first_name) {
                let mut result = self.ident_expr(first_js);
                for segment in &path.segments[1..] {
                    let prop = js::safe_js_variable_name(segment.ident.as_str());
                    result = self.static_member_expr(result, &prop);
                }
                return result;
            }

            // 2d. Resolve first segment via NameMap, then member-chain
            let first_js = if path.res.hir_id().is_some() {
                // The HIR resolution for a multi-segment path like `Vector::new`
                // points at the function declaration.  But the JS call site needs
                // the member-access form `Vector.$new`, not the flat `Vector__new`.
                // So resolve the first segment individually.
                self.name_map
                    .resolve_by_name(first_name)
                    .map(|s| s.to_string())
                    .or_else(|| {
                        // If first segment not in NameMap by name, it might be a
                        // type/struct that was registered under its own HirId.
                        // Fall back to safe JS name of the source name.
                        Some(js::safe_js_variable_name(first_name))
                    })
            } else if !path.res.is_unresolved() {
                Some(js::safe_js_variable_name(first_name))
            } else {
                // path.res.is_unresolved() — try name-based fallback.
                // If that also fails, return None so the single-segment error
                // path (step 4) records a CodegenError instead of silently
                // emitting a raw JS name.
                self.name_map
                    .resolve_by_name(first_name)
                    .map(|s| s.to_string())
            };

            if let Some(first_js) = first_js {
                let mut result = self.ident_expr(&first_js);
                let is_struct_method = path.res.binding_kind() == hir::BindingKind::StructMethod;
                if is_struct_method {
                    // StructMethod aliases (e.g. `Expense::is_food` or
                    // `Temporal::PlainDate::foo`) reference dot-methods on the
                    // prototype. Build the full type expression from all
                    // segments except the last, insert `.prototype`, then the
                    // final method name, and wrap with `$uncurryThis`.
                    let segments = &path.segments;
                    for segment in &segments[1..segments.len() - 1] {
                        let prop = js::safe_js_variable_name(segment.ident.as_str());
                        result = self.static_member_expr(result, &prop);
                    }
                    result = self.static_member_expr(result, "prototype");
                    let method = js::safe_js_variable_name(segments.last().unwrap().ident.as_str());
                    result = self.static_member_expr(result, &method);
                    result = self.call_expr(
                        self.ident_expr("$uncurryThis"),
                        vec![Argument::from(result)],
                    );
                } else {
                    for segment in &path.segments[1..] {
                        let prop = js::safe_js_variable_name(segment.ident.as_str());
                        result = self.static_member_expr(result, &prop);
                    }
                }
                return result;
            }
        }

        // 3. Single-segment fallbacks

        let path_str = path.to_string();

        // 3a. Builtin lookup
        if let Some(js_name) = builtins::lookup(&path_str) {
            return self.ident_expr(js_name);
        }

        // 3b. Protocol check
        if self.is_protocol(first_name) {
            return self.ident_expr(&CodegenJS::protocol_js_name(first_name));
        }

        // 3c. Resolved by semantic analysis but not in NameMap
        if !path.res.is_unresolved() {
            return self.ident_expr(&js::safe_js_variable_name(first_name));
        }

        // 3d. Name-based fallback for paths where semantic analysis didn't
        //     resolve the reference but the name was registered in the NameMap
        //     (e.g. `self` in protocol default method bodies).
        if let Some(js_name) = self.name_map.resolve_by_name(first_name) {
            return self.ident_expr(js_name);
        }

        // 4. Truly unresolved — emit error + fallback
        self.generate_identifier(&first_segment.ident)
    }

    fn generate_identifier(&mut self, name: &Ident) -> Expression<'a> {
        let name_string = name.as_str();
        // Last-resort builtin check (should rarely be reached)
        if let Some(js_name) = builtins::lookup(name_string) {
            return self.ident_expr(js_name);
        }
        self.unresolved_identifier_expr(name_string, name.span)
    }

    fn generate_assignment(&mut self, lhs: &hir::Expr, rhs: &hir::Expr) -> Expression<'a> {
        let target = self.generate_assignment_target(lhs);
        let value = self.generate_expr(rhs);
        self.assign_expr(target, value)
    }

    fn generate_assignment_target(&mut self, expr: &hir::Expr) -> AssignmentTarget<'a> {
        match &expr.kind {
            hir::ExprKind::Path(path) => {
                let first_ident = path.first_ident();
                let first_name = first_ident.as_str();
                let name = if let Some(hir_id) = path.res.hir_id() {
                    if let Some(resolved) = self.name_map.resolve(hir_id) {
                        resolved.to_string()
                    } else {
                        // HirId is present but the identifier was never registered
                        // in the NameMap — record an error and fall back to a safe
                        // placeholder so codegen can continue collecting errors.
                        self.errors
                            .push(crate::error::CodegenError::unresolved_identifier(
                                first_name,
                                first_ident.span,
                            ));
                        js::safe_js_variable_name(first_name)
                    }
                } else {
                    builtins::lookup(&path.to_string())
                        .map(|s| s.to_string())
                        .unwrap_or_else(|| js::safe_js_variable_name(first_name))
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

    fn generate_implements_expr(
        &mut self,
        value_expr: &hir::Expr,
        protocol_path: &hir::Path,
    ) -> Expression<'a> {
        let protocol_obj = self.generate_path_expression(protocol_path);
        let has_impl = self.static_member_expr(protocol_obj, "$implements");
        let value = self.generate_expr(value_expr);
        self.call_expr(has_impl, vec![Argument::from(value)])
    }

    /// Generate a cast expression (`as`).
    ///
    /// Dispatches through the `Into` protocol: `Into::into(value, "targetType")`.
    /// For builtin numeric types in JavaScript this is effectively
    /// a no-op since JS has a single number type, but user-defined
    /// `Into` implementations will be called.
    fn generate_cast_expr(&mut self, inner: &hir::Expr, target_ty: &hir::Ty) -> Expression<'a> {
        let value = self.generate_expr(inner);
        let into_fn = self.static_member_expr(
            self.ident_expr(&crate::generator::CodegenJS::protocol_js_name("Into")),
            "into",
        );
        let mut args = vec![Argument::from(value)];
        // Pass the target type as a string key for generic protocol dispatch.
        let type_key = format!("{}", target_ty.kind);
        args.push(Argument::from(self.str_expr(&type_key)));
        self.call_expr(into_fn, args)
    }

    /// Generate a try-cast expression (`as?`).
    ///
    /// Dispatches through the `TryInto` protocol: `TryInto::try_into(value, "targetType")`.
    /// The `TryInto` implementation is expected to return a `Result`
    /// (Ok on success, Err on failure).
    fn generate_try_cast_expr(&mut self, inner: &hir::Expr, target_ty: &hir::Ty) -> Expression<'a> {
        let value = self.generate_expr(inner);
        let try_into_fn = self.static_member_expr(
            self.ident_expr(&crate::generator::CodegenJS::protocol_js_name("TryInto")),
            "try_into",
        );
        let mut args = vec![Argument::from(value)];
        let type_key = format!("{}", target_ty.kind);
        args.push(Argument::from(self.str_expr(&type_key)));
        self.call_expr(try_into_fn, args)
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

    fn generate_tagged_string(
        &mut self,
        tag: &hir::Expr,
        parts: &[Box<str>],
        exprs: &[hir::Expr],
    ) -> Expression<'a> {
        let tag_expr = self.generate_expr(tag);
        let dollar_tag = self.ident_expr("$tag");
        let wrapped_tag = self.call_expr(dollar_tag, vec![Argument::from(tag_expr)]);

        let mut quasis = self.ast.vec_with_capacity(parts.len());
        let mut expressions = self.ast.vec_with_capacity(exprs.len());

        for (i, part) in parts.iter().enumerate() {
            let is_tail = i == parts.len() - 1;
            let value = TemplateElementValue {
                raw: self.ast.atom(part.as_ref()),
                cooked: Some(self.ast.atom(part.as_ref())),
            };
            quasis.push(self.ast.template_element(SPAN, value, is_tail, true));
        }

        for expr in exprs {
            expressions.push(self.generate_expr(expr));
        }

        let quasi = self.ast.template_literal(SPAN, quasis, expressions);
        self.ast
            .expression_tagged_template(SPAN, wrapped_tag, NONE, quasi)
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
                let tmp = self.name_map.alloc_unique("_");
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
    /// With HirId-keyed NameMap, bindings are flat — no explicit propagation needed.
    pub fn generate_block_stmts_propagate_scope(
        &mut self,
        block: &hir::Block,
    ) -> Vec<Statement<'a>> {
        self.generate_block_stmts(block)
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
