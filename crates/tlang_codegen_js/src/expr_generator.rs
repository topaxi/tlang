use tlang_ast::node::{self as ast, Ident};
use tlang_ast::token::Literal;
use tlang_hir::hir;

use log::debug;

use crate::binary_operator_generator::{
    JSAssociativity, JSOperatorInfo, map_operator_info, should_wrap_with_parentheses,
};
use crate::generator::{BlockContext, CodegenJS};

impl CodegenJS {
    pub(crate) fn generate_literal(&mut self, literal: &Literal) {
        match literal {
            Literal::Integer(value) => {
                self.push_str(&value.to_string());
            }
            Literal::UnsignedInteger(value) => {
                self.push_str(&value.to_string());
            }
            Literal::Float(value) => {
                self.push_str(&value.to_string());
            }
            Literal::Boolean(value) => {
                self.push_str(&value.to_string());
            }
            Literal::String(value) | Literal::Char(value) => {
                let escaped_value = value.replace('\\', "\\\\").replace('\"', "\\\"");
                self.push_str(&format!("\"{}\"", escaped_value));
            }
            Literal::None => unreachable!(),
        }
    }

    #[inline(always)]
    pub(crate) fn generate_optional_expr(
        &mut self,
        expr: Option<&hir::Expr>,
        parent_op: Option<hir::BinaryOpKind>,
    ) {
        if let Some(expr) = expr {
            self.generate_expr(expr, parent_op, BlockContext::Expression);
        }
    }

    fn generate_expr_inner_recursive(&mut self, expr: &hir::Expr, parent_op: Option<hir::BinaryOpKind>) {
         debug!("Enter generate_expr_inner_recursive: {:?}", expr.kind);
         match &expr.kind {
            hir::ExprKind::Call(call_expr) => {
                 debug!("  -> generate_call_expression");
                 self.generate_call_expression(call_expr)
            }
            hir::ExprKind::TailCall(call_expr) => {
                 self.generate_call_expression(call_expr)
            }
            hir::ExprKind::Cast(cast_expr, _) => {
                self.generate_expr(cast_expr, parent_op, BlockContext::Expression);
            }
            hir::ExprKind::FieldAccess(base, field) => {
                self.generate_field_access_expression(base, field)
            }
            hir::ExprKind::Path(path) => self.generate_path_expression(path),
            hir::ExprKind::IndexAccess(base, index) => {
                self.generate_index_access_expression(base, index)
            }
            hir::ExprKind::Literal(literal) => self.generate_literal(literal),
            hir::ExprKind::List(items) => self.generate_list_expression(items),
            hir::ExprKind::Dict(kvs) => self.generate_dict_expression(kvs),
            hir::ExprKind::Unary(op, unary_expr) => self.generate_unary_op(op, unary_expr),
            hir::ExprKind::Binary(op, lhs, rhs) => {
                self.generate_binary_op(*op, lhs, rhs, parent_op)
            }
            hir::ExprKind::FunctionExpression(decl) => self.generate_function_expression(decl),
            hir::ExprKind::Range(_) => todo!("Range expression not implemented yet."),
            hir::ExprKind::Wildcard => unreachable!("Wildcard is not a simple expression"),

            hir::ExprKind::Block(_) |
            hir::ExprKind::Loop(_) |
            hir::ExprKind::Break(_) |
            hir::ExprKind::Continue |
            hir::ExprKind::IfElse(_,_,_) |
            hir::ExprKind::Match(_,_) |
            hir::ExprKind::Let(_,_) => {
                unreachable!(
                    "Complex expression type {:?} should not reach generate_expr_inner_recursive",
                    expr.kind
                )
            }
         }
         debug!("Exit generate_expr_inner_recursive: {:?}", expr.kind);
    }

    pub(crate) fn generate_expr(
        &mut self,
        expr: &hir::Expr,
        parent_op: Option<hir::BinaryOpKind>,
        context: BlockContext,
    ) {
        debug!("Enter generate_expr: {:?}, Context: {:?}", expr.kind, context);
        match context {
            BlockContext::Expression => {
                let is_simple = expr_can_render_as_js_expr(expr);
                debug!("  expr_can_render_as_js_expr returned: {}", is_simple);
                if is_simple {
                    debug!("  -> generate_expr_inner_recursive");
                    self.generate_expr_inner_recursive(expr, parent_op);
                } else {
                    // Check if the expression *itself* needs an IIFE wrapper
                    match &expr.kind {
                        hir::ExprKind::Block(_) |
                        hir::ExprKind::Loop(_) |
                        hir::ExprKind::Match(_,_) => {
                            // Match requires block structure, use IIFE
                            debug!("  -> generate_iife_expression for {:?}", expr.kind);
                            self.generate_iife_expression(expr, parent_op);
                        }
                        hir::ExprKind::IfElse(cond, then, els) => {
                            // Check if this IfElse can be rendered as a simple ternary
                            if self.should_render_if_else_as_ternary(cond, then, els) {
                                debug!("  -> generate_ternary_op for {:?}", expr.kind);
                                self.generate_ternary_op(
                                    cond,
                                    then.expr.as_ref().unwrap(), // Should be checked by should_render_*
                                    els[0].consequence.expr.as_ref().unwrap(), // Should be checked by should_render_*
                                    parent_op
                                );
                            } else {
                                // Otherwise, use IIFE for block structure
                                debug!("  -> generate_iife_expression for {:?}", expr.kind);
                                self.generate_iife_expression(expr, parent_op);
                            }
                        }
                        _ => {
                            // Other types (Binary, Call, etc.) are complex due to sub-expressions.
                            // Generate the structure recursively; sub-expressions will handle their own IIFEs.
                            debug!("  -> generate_expr_inner_recursive for complex sub-expression in {:?}", expr.kind);
                            self.generate_expr_inner_recursive(expr, parent_op);
                        }
                    }
                }
            }
            BlockContext::Statement | BlockContext::Program => {
                 debug!("  -> generate_expr_stmt");
                self.generate_expr_stmt(expr, parent_op);
            }
        }
        debug!("Exit generate_expr: {:?}", expr.kind);
    }

    /// Generates an expression as a statement.
    pub(crate) fn generate_expr_stmt(&mut self, expr: &hir::Expr, parent_op: Option<hir::BinaryOpKind>) {
        debug!("Enter generate_expr_stmt: {:?}", expr.kind);
        if expr_can_render_as_js_expr(expr) {
             debug!("  -> generate_expr_inner_recursive (as stmt)");
            self.generate_expr_inner_recursive(expr, parent_op);
            if self.needs_semicolon(Some(expr)) {
                 self.push_char(';');
            }
            self.push_newline();
        } else {
            match &expr.kind {
                hir::ExprKind::Block(block) => self.generate_block_statement(block),
                hir::ExprKind::Loop(block) => self.generate_loop_statement(block),
                hir::ExprKind::IfElse(cond, then, els) => {
                    self.generate_if_else_statement(cond, then, els, parent_op)
                }
                hir::ExprKind::Match(match_expr, arms) => {
                     self.generate_match_statement(match_expr, arms)
                 }
                hir::ExprKind::Break(expr_opt) => self.generate_break_statement(expr_opt.as_deref()),
                hir::ExprKind::Continue => self.generate_continue_statement(),
                hir::ExprKind::Let(_,_) => unreachable!("Let expression should be handled by generate_stmt"),
                _ => unreachable!(
                     "generate_expr_stmt called with simple or unhandled complex expr type: {:?}",
                     expr.kind
                 ),
            }
        }
        debug!("Exit generate_expr_stmt: {:?}", expr.kind);
    }

    fn generate_unary_op(&mut self, op: &ast::UnaryOp, expr: &hir::Expr) {
        match op {
            ast::UnaryOp::Not => self.push_char('!'),
            ast::UnaryOp::Minus => self.push_char('-'),
            ast::UnaryOp::Spread => self.push_str("..."),
            ast::UnaryOp::Rest => unreachable!("Rest operator is not an operator but a pattern"),
        }

        self.generate_expr(expr, None, BlockContext::Expression);
    }

    fn generate_field_access_expression(&mut self, base: &hir::Expr, field: &Ident) {
        self.generate_expr(base, None, BlockContext::Expression);
        self.push_char('.');
        self.push_str(field.as_str());
    }

    fn generate_index_access_expression(&mut self, base: &hir::Expr, index: &hir::Expr) {
        self.generate_expr(base, None, BlockContext::Expression);
        self.push_char('[');
        self.generate_expr(index, None, BlockContext::Expression);
        self.push_char(']');
    }

    fn generate_path_expression(&mut self, path: &hir::Path) {
        if path.segments.is_empty() {
            return; // Should not happen for valid paths
        }

        let first_segment_str = path.segments[0].ident.as_str();
        let mut first = true;

        // Check if the first segment resolves to a known variable/alias
        if let Some(resolved_first) = self.current_scope().resolve_variable(first_segment_str) {
            self.push_str(&resolved_first);
            first = false;
        } else {
            // If first segment doesn't resolve, generate it literally
            self.generate_identifier(&path.segments[0].ident);
            first = false;
        }

        // Generate remaining segments with dot separators
        for segment in path.segments.iter().skip(1) {
             self.push_char('.');
             self.generate_identifier(&segment.ident);
        }
    }

    fn generate_list_expression(&mut self, items: &[hir::Expr]) {
        self.push_char('[');
        for (i, item) in items.iter().enumerate() {
            if i > 0 {
                self.push_str(", ");
            }
            self.generate_expr(item, None, BlockContext::Expression);
        }
        self.push_char(']');
    }

    fn generate_dict_expression(&mut self, kvs: &[(hir::Expr, hir::Expr)]) {
        self.push_str("{\n");
        self.inc_indent();
        for (i, (key, value)) in kvs.iter().enumerate() {
            if i > 0 {
                self.push_str(",\n");
            }
            self.push_indent();

            // Check for shorthand property: key is Path, value is Path, and they match
            let key_path_str = match &key.kind {
                hir::ExprKind::Path(p) => Some(p.join("::")),
                _ => None,
            };
            let value_path_str = match &value.kind {
                 hir::ExprKind::Path(p) => Some(p.join("::")),
                 _ => None,
            };

            if key_path_str.is_some() && key_path_str == value_path_str {
                // Generate shorthand
                self.generate_path_expression(key.path().unwrap());
            } else {
                // Generate key: value
                self.generate_expr(key, None, BlockContext::Expression);
                self.push_str(": ");
                self.generate_expr(value, None, BlockContext::Expression);
            }
        }
        // Add trailing comma if there were items
        if !kvs.is_empty() {
            self.push_char(',');
        }
        self.push_newline();
        self.dec_indent();
        self.push_indent();
        self.push_char('}');
    }

    fn generate_identifier(&mut self, name: &Ident) {
        let name_string = name.as_str();
        let identifier = self
            .current_scope()
            .resolve_variable(name_string)
            .unwrap_or_else(|| name_string.to_string());
        self.push_str(&identifier);
    }

    fn generate_ternary_op(
        &mut self,
        expr: &hir::Expr,
        then_expr: &hir::Expr,
        else_expr: &hir::Expr,
        parent_op: Option<hir::BinaryOpKind>,
    ) {
        let needs_parenthesis = if let Some(parent_op) = parent_op {
            should_wrap_with_parentheses(
                map_operator_info(parent_op),
                JSOperatorInfo {
                    precedence: 0,
                    associativity: JSAssociativity::Right,
                },
            )
        } else {
            false
        };

        if needs_parenthesis {
            self.push_char('(');
        }

        self.generate_expr(expr, None, BlockContext::Expression);
        self.push_str(" ? ");
        self.generate_expr(then_expr, None, BlockContext::Expression);
        self.push_str(" : ");
        self.generate_expr(else_expr, None, BlockContext::Expression);

        if needs_parenthesis {
            self.push_char(')');
        }
    }

    pub(crate) fn should_render_if_else_as_ternary(
        &self,
        _expr: &hir::Expr,
        then_branch: &hir::Block,
        else_branches: &[hir::ElseClause],
    ) -> bool {
        if !self.get_render_ternary() { return false; }

        if else_branches.len() != 1 {
            return false;
        }
        if else_branches[0].condition.is_some() {
             return false;
        }

        let else_branch = &else_branches[0].consequence;

        block_is_simple_expression(then_branch) && block_is_simple_expression(else_branch)
    }

    fn generate_partial_application(
        &mut self,
        call_expr: &hir::CallExpression,
        wildcard_count: usize,
    ) {
        let mut tmp_vars = Vec::with_capacity(wildcard_count);
        for _ in 0..wildcard_count {
            tmp_vars.push(self.current_scope().declare_unique_variable("_p"));
        }

        self.push_str("(");
        self.push_str(&tmp_vars.join(", "));
        self.push_str(") => ");

        self.generate_expr(&call_expr.callee, None, BlockContext::Expression);
        self.push_char('(');

        let mut tmp_var_iter = tmp_vars.iter();
        for (i, arg) in call_expr.arguments.iter().enumerate() {
            if i > 0 {
                self.push_str(", ");
            }
            match &arg.kind {
                hir::ExprKind::Wildcard => {
                    self.push_str(tmp_var_iter.next().unwrap());
                }
                _ => {
                    self.generate_expr(arg, None, BlockContext::Expression);
                }
            }
        }

        self.push_char(')');
    }

    pub(crate) fn generate_call_expression(&mut self, call_expr: &hir::CallExpression) {
        let wildcard_count = call_expr
            .arguments
            .iter()
            .filter(|arg| matches!(arg.kind, hir::ExprKind::Wildcard))
            .count();

        if wildcard_count > 0 {
            self.generate_partial_application(call_expr, wildcard_count);
            return;
        }

        self.generate_expr(&call_expr.callee, None, BlockContext::Expression);
        self.push_char('(');
        for (i, arg) in call_expr.arguments.iter().enumerate() {
            if i > 0 {
                self.push_str(", ");
            }
            self.generate_expr(arg, None, BlockContext::Expression);
        }
        self.push_char(')');
    }

    // --- Placeholder implementations --- 
    pub(crate) fn generate_block_statement(&mut self, block: &hir::Block) {
        self.push_scope();
        self.push_str("{\n");
        self.inc_indent();

        self.generate_statements(&block.stmts);
        // Generate the final expression as a statement if it exists.
        if let Some(expr) = &block.expr {
             self.generate_expr(expr, None, BlockContext::Statement);
        }

        self.dec_indent();
        self.push_indent();
        self.push_str("}\n"); // Assuming newline is desired after block
        self.pop_scope();
    }

    fn generate_loop_statement(&mut self, block: &hir::Block) {
        // Loop as statement doesn't need a tmp_var or special break handling.
        self.push_indent(); // Indent the loop itself
        self.push_str("for (;;) ");
        self.generate_block_statement(block); // Generate block contents as statements
    }

    fn generate_if_else_statement(
        &mut self,
        expr: &hir::Expr,
        then_branch: &hir::Block,
        else_branches: &[hir::ElseClause],
        parent_op: Option<hir::BinaryOpKind>,
    ) {
        // Check if it can be rendered as a ternary expression even in statement context
        if self.should_render_if_else_as_ternary(expr, then_branch, else_branches) {
             self.push_indent(); // Indent the ternary statement
             // Ternary operands must be expressions
             self.generate_ternary_op(
                 expr,
                 then_branch.expr.as_ref().unwrap(), // should_render checks expr exists
                 else_branches[0].consequence.expr.as_ref().unwrap(), // should_render checks this structure
                 parent_op
             );
             self.push_str(";\n"); // Ternary as statement needs semicolon
             return;
        }

        // Generate standard if-else if-else chain
        self.push_indent(); // Indent the if statement
        self.push_str("if (");
        self.generate_expr(expr, None, BlockContext::Expression); // Condition must be expression
        self.push_str(") ");

        // Generate Then Branch Body
        self.push_str("{\n");
        self.inc_indent();
        self.generate_statements(&then_branch.stmts); // Generate inner statements
        if let Some(expr) = &then_branch.expr {
            // Generate final expr directly at current indent level
            self.push_indent(); // <<< Add indent before the statement
            self.generate_expr_inner_recursive(expr, None);
            if self.needs_semicolon(Some(expr)) { self.push_char(';'); }
            self.push_newline();
        }
        self.dec_indent();
        self.push_indent(); // <<< Add indent before closing brace
        self.push_str("}"); // Closing brace for then

        for else_clause in else_branches {
            self.push_indent(); // <<< Indent the else/else if line
            if let Some(cond) = &else_clause.condition {
                self.push_str(" else if ("); // Add space before 'else if'
                self.generate_expr(cond, None, BlockContext::Expression); // Condition must be expression
                self.push_str(") ");
            } else {
                self.push_str(" else "); // Add space before 'else'
            }
            // Generate the block for the else/else if branch

            // Generate Consequence Body
            self.push_str("{\n");
            self.inc_indent();
            self.generate_statements(&else_clause.consequence.stmts); // Generate inner statements
            if let Some(expr) = &else_clause.consequence.expr {
                // Generate final expr directly at current indent level
                self.push_indent(); // <<< Add indent before the statement
                self.generate_expr_inner_recursive(expr, None);
                if self.needs_semicolon(Some(expr)) { self.push_char(';'); }
                self.push_newline();
            }
            self.dec_indent();
            self.push_indent(); // <<< Add indent before closing brace
            self.push_str("}"); // Closing brace for else/else if
        }
        self.push_newline(); // Add final newline after the entire if-else chain
    }

    fn generate_match_statement(&mut self, match_expr: &hir::Expr, arms: &[hir::MatchArm]) {
        // 1. Evaluate the expression being matched into a temporary variable
        //    if it's not already a simple variable/literal.
        let match_value_var = if expr_can_render_as_js_expr(match_expr) {
            // If it's simple, generate it directly where needed (less efficient if used often)
            // For consistency and to avoid side-effects, let's use a temp var usually.
             let tmp = self.current_scope().declare_tmp_variable();
             self.push_indent();
             self.push_let_declaration(&tmp);
             self.push_str(" = ");
             self.generate_expr(match_expr, None, BlockContext::Expression);
             self.push_str(";\n");
             tmp
        } else {
             // Use IIFE for complex match expr, similar to generate_match_return
             let tmp = self.current_scope().declare_tmp_variable();
             self.push_indent();
             self.push_open_let_declaration(&tmp);
             self.generate_iife_expression(match_expr, None);
             self.push_str(";\n");
             tmp
        };

        // 2. Generate the if/else if chain
        for (i, arm) in arms.iter().enumerate() {
            self.push_indent();
            if i == 0 {
                self.push_str("if (");
            } else {
                self.push_str("else if (");
            }

            // Generate condition (pattern check + guard)
            self.generate_pattern_check_condition(&arm.pat, &match_value_var);
            if let Some(guard) = &arm.guard {
                self.push_str(" && "); // Combine pattern check and guard

                // Special handle Let guards
                if let hir::ExprKind::Let(pat, expr) = &guard.kind {
                    self.push_char('(');
                    // TODO: Implement actual let guard generation logic.
                    // Needs to generate the expression, attempt to match the pattern,
                    // and evaluate to true/false, potentially binding variables.
                    // For now, just generate true as a placeholder.
                    self.push_str("/* let guard stub */ true");
                    self.push_char(')');
                } else {
                    // Generate normal boolean expression guard
                    self.push_char('(');
                    self.generate_expr(guard, None, BlockContext::Expression);
                    self.push_char(')');
                }
            }
            self.push_str(") ");

            // Generate block for the arm
            self.push_scope(); // Scope for pattern bindings
            self.push_str("{\n");
            self.inc_indent();
            self.generate_pattern_bindings(&arm.pat, &match_value_var); // Emit let bindings
            self.generate_block_statement(&arm.block); // Use statement generation
            self.dec_indent();
            self.push_indent();
            self.push_str("}\n");
            self.pop_scope();
        }
        // TODO: Handle non-exhaustive match? Throw error?
    }

    // --- Pattern Matching Helper Stubs ---
    fn generate_pattern_check_condition(&mut self, pattern: &hir::Pat, value_var: &str) {
        match &pattern.kind {
            hir::PatKind::Literal(lit_box) => { // Match on Box<Literal>
                let lit = &**lit_box; // Dereference the Box
                self.push_str(&format!("{} === ", value_var));
                 match lit {
                     Literal::None => self.push_str("undefined"),
                     _ => self.generate_literal(lit),
                 }
            }
            hir::PatKind::Wildcard => {
                self.push_str("true");
            }
            hir::PatKind::Identifier(_, _) => { // Changed to Identifier(_, _)
                // Variable binding always satisfies the condition part.
                self.push_str("true");
            }
            _ => {
                 self.push_str("true /* complex pattern check stub */");
             }
        }
    }

    fn generate_pattern_bindings(&mut self, pattern: &hir::Pat, value_var: &str) {
         match &pattern.kind {
             hir::PatKind::Identifier(_, ident_box) => { // Changed to Identifier(_, ident_box)
                 // Declare and assign the matched value to the pattern variable
                 let binding_name = ident_box.as_str(); // Use as_str() method
                 // Resolve potential shadowing. Use declare_variable which handles this.
                 let resolved_name = self.current_scope().declare_variable(binding_name);
                 self.push_indent();
                 self.push_let_declaration(&resolved_name);
                 self.push_str(&format!(" = {};\n", value_var));
             }
             hir::PatKind::Literal(_) | hir::PatKind::Wildcard => {
                 // No bindings needed for literals or wildcards
             }
             _ => {
                  // Placeholder for more complex patterns that might have bindings
                  self.push_indent();
                  self.push_str("/* complex pattern bindings stub */\n");
             }
         }
    }
    // --- End Pattern Matching Helper Stubs ---
    
    fn generate_break_statement(&mut self, expr: Option<&hir::Expr>) {
        let context = self.current_context();
        let break_should_return = matches!(context, BlockContext::Expression);

        self.push_indent();
        if break_should_return {
            if let Some(val_expr) = expr {
                // Generate 'return <expr>;' for 'break <expr>;' inside loop expressions
                self.push_str("return ");
                self.generate_expr(val_expr, None, BlockContext::Expression); // Value must be an expression
                self.push_str(";\n");
            } else {
                // Generate 'return undefined;' for 'break;' inside loop expressions
                 self.push_str("return undefined;\n");
            }
        } else {
            // Generate standard 'break;'
            if expr.is_some() {
                 // Warn if break has a value but we're not in a context to return it.
                 eprintln!(
                     "Warning: Javascript 'break' with value used in statement context (value ignored)."
                 );
             }
            self.push_str("break;\n");
        }
    }

    fn generate_continue_statement(&mut self) {
         self.push_indent();
         self.push_str("continue;\n");
    }

    fn generate_iife_expression(&mut self, expr: &hir::Expr, parent_op: Option<hir::BinaryOpKind>) {
        self.push_str("(() => ");
        self.push_scope(); // Scope for the IIFE body
        self.push_context(BlockContext::Expression); // << Push context

        // Generate the block body within the IIFE
        match &expr.kind {
            hir::ExprKind::Block(block) => self.generate_block_return(block),
            hir::ExprKind::Loop(block) => self.generate_loop_return(block),
            hir::ExprKind::IfElse(cond, then, els) => {
                self.generate_if_else_return(cond, then, els, parent_op)
            }
            hir::ExprKind::Match(match_expr, arms) => {
                self.generate_match_return(match_expr, arms)
            }
            // Other expression types should have been handled by generate_expr routing
            // and should not reach here.
            _ => panic!(
                    "Internal Error: generate_iife_expression called with unexpected expr type: {:?}",
                     expr.kind
                ),
        }

        self.pop_context(); // << Pop context
        self.pop_scope();
        // Add indentation before closing the IIFE
        self.push_indent(); 
        self.push_str(")()");
    }

    /// Generates the body of a block within an IIFE, returning the final expression.
    fn generate_block_return(&mut self, block: &hir::Block) {
        self.push_str("{\n");
        self.inc_indent();

        self.generate_statements(&block.stmts);

        // Generate the final expression with a return statement.
        if let Some(expr) = &block.expr {
            self.push_indent();
            self.push_str("return ");
            // The value being returned must be an expression.
            self.generate_expr(expr, None, BlockContext::Expression);
            self.push_str(";\n");
        } else {
            // Implicit return undefined if no final expression.
            self.push_indent();
            self.push_str("return undefined;\n");
        }

        self.dec_indent();
        self.push_indent();
        self.push_str("}");
    }

    /// Generates the body of a loop within an IIFE, returning the value from break or undefined.
    fn generate_loop_return(&mut self, block: &hir::Block) {
        // TODO: This currently only supports break without value (returns undefined).
        // Need to refactor break handling to support `return value;` for `break value;`.
        self.push_str("{\n"); // Wrap loop in a block for potential return value var
        self.inc_indent();
        self.push_indent();
        self.push_indent();
        self.push_str("for (;;) ");
        self.generate_block_statement(block); // Generate block contents as statements
        // If the loop finishes without break, it implicitly returns undefined.
        self.push_indent();
        self.push_str("return undefined;\n");
        self.dec_indent();
        self.push_indent();
        self.push_str("}");
    }

    /// Generates the body of an if-else chain within an IIFE, returning the appropriate value.
    fn generate_if_else_return(
       &mut self,
       expr: &hir::Expr,
       then_branch: &hir::Block,
       else_branches: &[hir::ElseClause],
       parent_op: Option<hir::BinaryOpKind>,
   ) {
       // Check if it can be rendered as a ternary expression directly
       if self.should_render_if_else_as_ternary(expr, then_branch, else_branches) {
            // Ternary operands must be expressions
            self.generate_ternary_op(
                expr,
                then_branch.expr.as_ref().unwrap(), // should_render checks expr exists
                else_branches[0].consequence.expr.as_ref().unwrap(), // should_render checks this structure
                parent_op
            );
            // No return keyword needed here as the ternary IS the expression
            return;
       }

       // Generate standard if-else if-else chain with returns
       self.push_str("{\n"); // Wrap in block for return statements
       self.inc_indent(); // <<< Add indent for contents
       self.push_indent(); // Indent the first "if"
       self.push_str("if (");
       self.generate_expr(expr, None, BlockContext::Expression);
       self.push_str(") ");
       self.generate_block_return(then_branch); // Then branch returns its value

       for else_clause in else_branches {
           self.push_indent(); // Indent the "else if" / "else"
           if let Some(cond) = &else_clause.condition {
               self.push_str(" else if ("); // Add space before else if
               self.generate_expr(cond, None, BlockContext::Expression);
               self.push_str(") ");
           } else {
               self.push_str(" else "); // Add space before else
           }
           // Generate the block for the else/else if branch, returning its value
           self.generate_block_return(&else_clause.consequence);
       }
        // TODO: Handle implicit return undefined if no else branch matches?
        // JS functions return undefined by default if no return is hit.
       self.dec_indent(); // <<< Remove indent for contents
       self.push_indent(); // Indent the closing brace
       self.push_str("}");
   }

   /// Generates the body of a match expression within an IIFE, returning the appropriate value.
   fn generate_match_return(&mut self, match_expr: &hir::Expr, arms: &[hir::MatchArm]) {
       self.push_str("{\n"); // Wrap in block for temp var and returns
       self.inc_indent();

       // 1. Evaluate the expression being matched into a temporary variable
       let match_value_var = self.current_scope().declare_tmp_variable();
       if expr_can_render_as_js_expr(match_expr) {
           self.push_let_declaration_to_expr(&match_value_var, match_expr);
           self.push_str(";\n");
       } else {
           // Use a temporary IIFE *without return checks* to get the value into match_value_var
           // We can't use generate_expr_assignment_stmt here as it would incorrectly handle returns.
           self.push_open_let_declaration(&match_value_var);
           self.generate_iife_expression(match_expr, None); // Original IIFE generation
           self.push_str(";\n");
       }
       self.flush_statement_buffer(); // Ensure declaration is emitted first

       // 2. Generate the if/else if chain
       for (i, arm) in arms.iter().enumerate() {
           self.push_indent();
           if i == 0 {
               self.push_str("if (");
           } else {
               self.push_str("else if (");
           }

           // Generate condition (pattern check + guard)
           self.generate_pattern_check_condition(&arm.pat, &match_value_var);
           if let Some(guard) = &arm.guard {
               self.push_str(" && "); // Combine pattern check and guard

               // Special handle Let guards
               if let hir::ExprKind::Let(pat, expr) = &guard.kind {
                   self.push_char('(');
                   // TODO: Implement actual let guard generation logic.
                   // Needs to generate the expression, attempt to match the pattern,
                   // and evaluate to true/false, potentially binding variables.
                   // For now, just generate true as a placeholder.
                   self.push_str("/* let guard stub */ true");
                   self.push_char(')');
               } else {
                   // Generate normal boolean expression guard
                   self.push_char('(');
                   self.generate_expr(guard, None, BlockContext::Expression);
                   self.push_char(')');
               }
           }
           self.push_str(") ");

           // Generate block for the arm
           self.push_scope(); // Scope for pattern bindings
           self.push_str("{\n");
           self.inc_indent();
           self.generate_pattern_bindings(&arm.pat, &match_value_var); // Emit let bindings
           // Generate the block body, returning its value
           self.generate_block_return(&arm.block);
           self.dec_indent();
           self.push_indent();
           self.push_str("}\n");
           self.pop_scope();
       }

       // TODO: Handle non-exhaustive match? Throw error or return undefined?
       self.push_indent();
       self.push_str("return undefined; // TODO: Handle non-exhaustive match\n");

       self.dec_indent();
       self.push_indent();
       self.push_str("}");
   }

    /// Generates an expression that requires statement context (like IfElse, Block, Match)
    /// as a series of statements, assigning the result to `target_var`.
    /// Handles nested `return` statements by generating JS `return`.
    pub(crate) fn generate_expr_assignment_stmt(
        &mut self,
        expr: &hir::Expr,
        target_var: &str,
        parent_op: Option<hir::BinaryOpKind>,
    ) {
        match &expr.kind {
            hir::ExprKind::Block(block) => {
                self.generate_block_assignment_stmt(block, target_var)
            }
            hir::ExprKind::IfElse(cond, then_branch, else_branches) => {
                self.generate_if_else_assignment_stmt(
                    cond, then_branch, else_branches, target_var, parent_op,
                )
            }
            hir::ExprKind::Match(match_expr, arms) => {
                self.generate_match_assignment_stmt(match_expr, arms, target_var)
            }
            _ => {
                // Should not happen if called from generate_variable_declaration_stmt_transform
                // because it pre-filters expression kinds.
                // Generate as a simple expression assignment as a fallback.
                self.push_indent();
                self.push_str(&format!("{} = ", target_var));
                self.generate_expr(expr, parent_op, BlockContext::Expression);
                self.push_str(";\n");
            }
        }
    }

    /// Generates a block as statements, assigning the final expression to `target_var`.
    /// Handles nested `return` by generating JS `return`.
    /// Does NOT generate the surrounding curly braces `{}`.
    fn generate_block_assignment_stmt(&mut self, block: &hir::Block, target_var: &str) {
        self.push_scope();
        // self.push_str("{\n"); // Braces handled by caller
        // self.inc_indent();

        let original_indent = self.current_indent();
        // Inner statements need to be indented relative to the block's container
        self.generate_statements(&block.stmts);
        // Check if the block's statements already contained a return.
        let contains_stmt_return = block.stmts.iter().any(|stmt| matches!(stmt.kind, hir::StmtKind::Return(_)));

        if !contains_stmt_return {
            // Only generate assignment/final expr if no return statement was executed.
            // Generate the final expression, assigning it to the target variable.
            if let Some(expr) = &block.expr {
                // If the final expression itself contains return, generate it as a statement.
                // The JS return inside will handle exiting the function correctly.
                if expr_contains_return(expr) {
                     self.generate_expr_stmt(expr, None); // Generate as statement directly
                } else {
                    // Otherwise, generate the assignment.
                    self.push_indent();
                    self.push_str(&format!("{} = ", target_var));
                    self.generate_expr(expr, None, BlockContext::Expression); // Generate as expression
                    self.push_str(";\n");
                }
            } else {
                // Assign undefined if no final expression and no prior return.
                self.push_indent();
                self.push_str(&format!("{} = undefined;\n", target_var));
            }
        }

        // self.dec_indent();
        // self.push_indent();
        // self.push_str("}\n"); // Braces handled by caller
        // Ensure indent level is restored if generate_statements changed it unexpectedly
        self.set_indent(original_indent);
        self.pop_scope();
    }

    /// Generates an if-else chain as statements, assigning the result of the executed branch to `target_var`.
    /// Handles nested `return` by generating JS `return`.
    fn generate_if_else_assignment_stmt(
        &mut self,
        expr: &hir::Expr,
        then_branch: &hir::Block,
        else_branches: &[hir::ElseClause],
        target_var: &str,
        parent_op: Option<hir::BinaryOpKind>,
    ) {
        // If it can be a simple ternary, generate `target_var = ternary_expr;`
        // Note: Ternary cannot contain return, so this path is safe.
        if self.should_render_if_else_as_ternary(expr, then_branch, else_branches) {
            self.push_indent();
            self.push_str(&format!("{} = ", target_var));
            self.generate_ternary_op(
                expr,
                then_branch.expr.as_ref().unwrap(),
                else_branches[0].consequence.expr.as_ref().unwrap(),
                parent_op,
            );
            self.push_str(";\n");
            return;
        }

        // Generate standard if-else if-else chain
        self.push_str("if (");
        self.generate_expr(expr, None, BlockContext::Expression);
        self.push_str(") ");
        // Generate the block for the `then` branch, assigning its result
        self.push_str("{\n"); // Add opening brace
        self.inc_indent();
        self.generate_block_assignment_stmt(then_branch, target_var);
        self.dec_indent();
        self.push_indent();
        self.push_str("}\n"); // Add closing brace + newline

        for else_clause in else_branches {
            self.push_indent();
            if let Some(cond) = &else_clause.condition {
                self.push_str(" else if (");
                self.generate_expr(cond, None, BlockContext::Expression);
                self.push_str(") ");
            } else {
                self.push_str(" else ");
            }
            // Generate the block for the else/else if branch, assigning its result
            self.push_str("{\n"); // Add opening brace
            self.inc_indent();
            self.generate_block_assignment_stmt(&else_clause.consequence, target_var);
            self.dec_indent();
            self.push_indent();
            self.push_str("}\n"); // Add closing brace + newline
        }
        // Note: If the if/else is non-exhaustive, target_var might remain unassigned
        // if no branch is taken. JS default would be `undefined` if declared with `let`. implicitly.
    }

    /// Generates a match expression as statements, assigning the result of the executed arm to `target_var`.
    /// Handles nested `return` by generating JS `return`.
    fn generate_match_assignment_stmt(
        &mut self,
        match_expr: &hir::Expr,
        arms: &[hir::MatchArm],
        target_var: &str,
    ) {
        // 1. Evaluate the expression being matched into a temporary variable
        let match_value_var = self.current_scope().declare_tmp_variable();
        if expr_can_render_as_js_expr(match_expr) {
            self.push_let_declaration_to_expr(&match_value_var, match_expr);
            self.push_str(";\n");
        } else {
            // Use a temporary IIFE *without return checks* to get the value into match_value_var
            // We can't use generate_expr_assignment_stmt here as it would incorrectly handle returns.
            self.push_open_let_declaration(&match_value_var);
            self.generate_iife_expression(match_expr, None); // Original IIFE generation
            self.push_str(";\n");
        }
        self.flush_statement_buffer(); // Ensure declaration is emitted first

        // 2. Generate the if/else if chain for arms
        for (i, arm) in arms.iter().enumerate() {
            self.push_indent();
            if i == 0 {
                self.push_str("if (");
            } else {
                self.push_str("else if (");
            }

            // Generate condition (pattern check + guard)
            self.generate_pattern_check_condition(&arm.pat, &match_value_var);
            if let Some(guard) = &arm.guard {
                self.push_str(" && "); // Combine pattern check and guard

                // Special handle Let guards
                if let hir::ExprKind::Let(pat, expr) = &guard.kind {
                    self.push_char('(');
                    // TODO: Implement actual let guard generation logic.
                    // Needs to generate the expression, attempt to match the pattern,
                    // and evaluate to true/false, potentially binding variables.
                    // For now, just generate true as a placeholder.
                    self.push_str("/* let guard stub */ true");
                    self.push_char(')');
                } else {
                    // Generate normal boolean expression guard
                    self.push_char('(');
                    self.generate_expr(guard, None, BlockContext::Expression);
                    self.push_char(')');
                }
            }
            self.push_str(") ");

            // Generate block for the arm, assigning its result
            self.push_scope(); // Scope for pattern bindings
            self.push_str("{\n");
            self.inc_indent();
            self.generate_pattern_bindings(&arm.pat, &match_value_var); // Emit let bindings
            // Generate the block body, assigning its value to target_var
            // We add braces and indentation here as generate_block_assignment_stmt no longer does.
            self.generate_block_assignment_stmt(&arm.block, target_var);
            self.dec_indent();
            self.push_indent();
            self.push_str("}\n");
            self.pop_scope();
        }

        // TODO: Handle non-exhaustive match? Assign undefined or throw?
        // For now, implicitly assigns undefined if no arm matches.
    }
}

// --- Helper functions (should be outside impl) ---

/// Checks if a block contains only a single expression and no statements.
fn block_is_simple_expression(block: &hir::Block) -> bool {
    block.stmts.is_empty() && block.expr.is_some()
}

/// Recursively checks if an expression contains a `Return` statement.
pub(crate) fn expr_contains_return(expr: &hir::Expr) -> bool {
    match &expr.kind {
        // Base case: Return is handled by StmtKind check in block_contains_return.
        // hir::ExprKind::Return(_) => true, // This variant does not exist

        // Recursive cases: Check sub-expressions.
        hir::ExprKind::Cast(sub_expr, _) => expr_contains_return(sub_expr),
        hir::ExprKind::FieldAccess(base, _) => expr_contains_return(base),
        hir::ExprKind::IndexAccess(base, index) => expr_contains_return(base) || expr_contains_return(index),
        hir::ExprKind::List(items) => items.iter().any(expr_contains_return),
        hir::ExprKind::Dict(kvs) => kvs.iter().any(|(k, v)| expr_contains_return(k) || expr_contains_return(v)),
        hir::ExprKind::Unary(_, sub_expr) => expr_contains_return(sub_expr),
        hir::ExprKind::Binary(_, lhs, rhs) => expr_contains_return(lhs) || expr_contains_return(rhs),
        hir::ExprKind::FunctionExpression(decl) => {
            // Check the function body block.
            block_contains_return(&decl.body)
        }
        hir::ExprKind::Block(block) => block_contains_return(block),
        hir::ExprKind::Loop(block) => block_contains_return(block), // Loop body can have return
        hir::ExprKind::IfElse(cond, then_branch, else_branches) => {
            expr_contains_return(cond)
                || block_contains_return(then_branch)
                || else_branches.iter().any(|clause| {
                    clause.condition.as_ref().map_or(false, |c| expr_contains_return(c))
                        || block_contains_return(&clause.consequence)
                })
        }
        hir::ExprKind::Match(match_expr, arms) => {
            expr_contains_return(match_expr)
                || arms.iter().any(|arm| {
                    arm.guard.as_ref().map_or(false, |g| expr_contains_return(g))
                        || block_contains_return(&arm.block)
                })
        }
        hir::ExprKind::Let(_, init) => expr_contains_return(init),

        // Non-recursive cases: These cannot contain Return directly.
        hir::ExprKind::Literal(_) |
        hir::ExprKind::Path(_) |
        hir::ExprKind::Range(_) |
        hir::ExprKind::Wildcard |
        hir::ExprKind::Continue => false,

        // `break` can only exit loops/blocks, not functions, so we don't check it here.
        hir::ExprKind::Break(opt_expr) => opt_expr.as_ref().map_or(false, |e| expr_contains_return(e)),

        // Calls can't contain returns directly, but the called function might.
        // We don't analyze callees here.
        hir::ExprKind::Call(call) => {
            expr_contains_return(&call.callee) || call.arguments.iter().any(expr_contains_return)
        }
        hir::ExprKind::TailCall(call) => {
             expr_contains_return(&call.callee) || call.arguments.iter().any(expr_contains_return)
        }
    }
}

/// Helper for expr_contains_return to check inside a Block.
fn block_contains_return(block: &hir::Block) -> bool {
    block.stmts.iter().any(|stmt| match &stmt.kind {
        // Handle return statement specifically.
        hir::StmtKind::Return(option_expr_box) => {
            // Dereference the Box<Option<Expr>>
            match &**option_expr_box {
                Some(expr) => expr_contains_return(expr),
                None => true, // `return;` also counts
            }
        }

        // Handle expressions used as statements.
        hir::StmtKind::Expr(expr_box) => expr_contains_return(&*expr_box),

        // Handle let bindings with initializers (the second Box<Expr>).
        // The third element is Box<Ty>, which we ignore here.
        hir::StmtKind::Let(_, init_box, _) => expr_contains_return(&*init_box),

        // Other statement kinds cannot contain a return.
        _ => false,
    }) || block.expr.as_ref().map_or(false, |expr| expr_contains_return(expr))
}

// Helper function to check if an expression can be rendered as a JS expression,
// but *without* allowing nested IfElse to be considered ternary. This prevents infinite recursion.
fn sub_expr_can_render_as_js_expr(expr: &hir::Expr) -> bool {
     let result = match &expr.kind {
        hir::ExprKind::Literal(_) => true,
        hir::ExprKind::Path(_) => true,
        hir::ExprKind::FieldAccess(base, _) => sub_expr_can_render_as_js_expr(base),
        hir::ExprKind::IndexAccess(base, index) =>
            sub_expr_can_render_as_js_expr(base) && sub_expr_can_render_as_js_expr(index),
        hir::ExprKind::Unary(_, sub_expr) => sub_expr_can_render_as_js_expr(sub_expr),
        hir::ExprKind::Binary(_, lhs, rhs) =>
            sub_expr_can_render_as_js_expr(lhs) && sub_expr_can_render_as_js_expr(rhs),
        hir::ExprKind::Call(call) => {
            sub_expr_can_render_as_js_expr(&call.callee) &&
            call.arguments.iter().all(|arg| sub_expr_can_render_as_js_expr(arg)) &&
            !call.arguments.iter().any(|arg| matches!(arg.kind, hir::ExprKind::Wildcard))
        }
        hir::ExprKind::TailCall(call) => {
             sub_expr_can_render_as_js_expr(&call.callee) &&
             call.arguments.iter().all(|arg| sub_expr_can_render_as_js_expr(arg))
        }
        hir::ExprKind::List(items) => items.iter().all(sub_expr_can_render_as_js_expr),
        hir::ExprKind::Dict(kvs) => kvs.iter().all(|(k, v)| sub_expr_can_render_as_js_expr(k) && sub_expr_can_render_as_js_expr(v)),
        hir::ExprKind::FunctionExpression(_) => true,
        hir::ExprKind::Cast(sub_expr, _) => sub_expr_can_render_as_js_expr(sub_expr),
        hir::ExprKind::Range(_) => false,
        hir::ExprKind::Block(block) => {
             block_is_simple_expression(block) && sub_expr_can_render_as_js_expr(block.expr.as_ref().unwrap())
        },
        // IfElse is explicitly disallowed in sub-expressions to prevent recursion.
        hir::ExprKind::IfElse(_, _, _) => false,
        hir::ExprKind::Loop(_) => false,
        hir::ExprKind::Match(_, _) => false,
        hir::ExprKind::Break(_) | hir::ExprKind::Continue => false,
        hir::ExprKind::Let(_, _) => false,
        hir::ExprKind::Wildcard => false,
    };
    result
}

pub(crate) fn expr_can_render_as_js_expr(expr: &hir::Expr) -> bool {
     debug!("Enter expr_can_render_as_js_expr: {:?}", expr.kind);
     let result = match &expr.kind {
        hir::ExprKind::Literal(_) => true,
        hir::ExprKind::Path(_) => true,
        hir::ExprKind::FieldAccess(base, _) => expr_can_render_as_js_expr(base),
        hir::ExprKind::IndexAccess(base, index) =>
            expr_can_render_as_js_expr(base) && expr_can_render_as_js_expr(index),
        hir::ExprKind::Unary(_, sub_expr) => expr_can_render_as_js_expr(sub_expr),
        hir::ExprKind::Binary(_, lhs, rhs) =>
            expr_can_render_as_js_expr(lhs) && expr_can_render_as_js_expr(rhs),
        hir::ExprKind::Call(call) => {
            expr_can_render_as_js_expr(&call.callee) &&
            call.arguments.iter().all(|arg| expr_can_render_as_js_expr(arg)) &&
            !call.arguments.iter().any(|arg| matches!(arg.kind, hir::ExprKind::Wildcard))
        }
        hir::ExprKind::TailCall(call) => {
             expr_can_render_as_js_expr(&call.callee) &&
             call.arguments.iter().all(|arg| expr_can_render_as_js_expr(arg))
        }
        hir::ExprKind::List(items) => items.iter().all(expr_can_render_as_js_expr),
        hir::ExprKind::Dict(kvs) => kvs.iter().all(|(k, v)| expr_can_render_as_js_expr(k) && expr_can_render_as_js_expr(v)),
        hir::ExprKind::FunctionExpression(_) => true,
        hir::ExprKind::Cast(sub_expr, _) => expr_can_render_as_js_expr(sub_expr),
        hir::ExprKind::Range(_) => false,
        hir::ExprKind::Block(_) => false,
        hir::ExprKind::IfElse(_, _, _) => false,
        hir::ExprKind::Loop(_) => false,
        hir::ExprKind::Match(_, _) => false,
        hir::ExprKind::Break(_) | hir::ExprKind::Continue => false,
        hir::ExprKind::Let(_,_) => false, // Let is a statement/declaration, not a simple expression
        hir::ExprKind::Wildcard => false,
    };
    debug!("Exit expr_can_render_as_js_expr: {:?} -> {}", expr.kind, result);
    result
}
