use tlang_hir::hir;

use crate::generator::{BlockContext, CodegenJS};

impl CodegenJS {
    pub(crate) fn generate_stmt(&mut self, statement: &hir::Stmt) {
        self.generate_comments(&statement.leading_comments);

        match &statement.kind {
            hir::StmtKind::Expr(expr) => {
                // Special handling for loops in statement context
                if let hir::ExprKind::Loop(block) = &expr.kind {
                    self.push_indent();
                    self.generate_loop_statement(block);
                    self.push_newline();
                } else if let hir::ExprKind::Block(block) = &expr.kind {
                    // Check if this block represents a for loop pattern
                    if self.is_for_loop_pattern(block) {
                        // Generate this block as a for loop with proper iteration
                        self.generate_for_loop_block(block);
                        return; // Don't continue with regular block processing
                    } else {
                        // Special handling for blocks that contain loops as completion expressions
                        if let Some(completion_expr) = &block.expr {
                            if let hir::ExprKind::Loop(loop_block) = &completion_expr.kind {
                                // This is a block with a loop completion expression (e.g., for loops)
                                // Generate the block statements first, then the loop as a statement
                                self.push_indent();
                                self.push_str("{");
                                self.push_newline();
                                self.inc_indent();
                                
                                // Generate the block statements (e.g., let iterator$$ = ...)
                                for stmt in &block.stmts {
                                    self.generate_stmt(stmt);
                                }
                                
                                // Generate the loop as a statement
                                self.generate_loop_statement(loop_block);
                                
                                self.dec_indent();
                                self.push_indent();
                                self.push_str("}");
                                self.push_newline();
                            } else {
                                // Regular block expression - use normal expression handling
                                self.push_indent();
                                self.push_context(BlockContext::Statement);
                                self.generate_expr(expr, None);
                                self.pop_context();

                                if self.needs_semicolon(Some(expr)) {
                                    self.push_char(';');
                                }
                                self.push_newline();
                            }
                        } else {
                            // Block without completion expression - use normal expression handling
                            self.push_indent();
                            self.push_context(BlockContext::Statement);
                            self.generate_expr(expr, None);
                            self.pop_context();

                            if self.needs_semicolon(Some(expr)) {
                                self.push_char(';');
                            }
                            self.push_newline();
                        }
                    }
                } else {
                    self.push_indent();
                    self.push_context(BlockContext::Statement);
                    self.generate_expr(expr, None);
                    self.pop_context();

                    if self.needs_semicolon(Some(expr)) {
                        self.push_char(';');
                    }
                    self.push_newline();
                }
            }
            hir::StmtKind::Let(pattern, expression, _ty) => {
                self.generate_variable_declaration(pattern, expression);
            }
            hir::StmtKind::FunctionDeclaration(decl) => self.generate_function_declaration(decl),
            hir::StmtKind::DynFunctionDeclaration(decl) => {
                self.generate_dyn_function_declaration(decl);
            }
            hir::StmtKind::Return(expr) => self.generate_return_statement(expr.as_deref()),
            hir::StmtKind::EnumDeclaration(decl) => self.generate_enum_declaration(decl),
            hir::StmtKind::StructDeclaration(decl) => self.generate_struct_declaration(decl),
        }

        self.generate_comments(&statement.trailing_comments);
    }

    #[inline(always)]
    pub(crate) fn generate_statements(&mut self, statements: &[hir::Stmt]) {
        for statement in statements {
            self.generate_stmt(statement);
            self.flush_statement_buffer();
        }
    }

    pub(crate) fn generate_variable_declaration(&mut self, pattern: &hir::Pat, value: &hir::Expr) {
        match &pattern.kind {
            hir::PatKind::Identifier(_, ident) => {
                self.generate_variable_declaration_identifier(ident.as_str(), value);
            }
            hir::PatKind::List(patterns) => {
                self.generate_variable_declaration_list_pattern(patterns, value);
            }
            _ => todo!("Variable declaration pattern matching is not implemented yet."),
        }
    }

    fn generate_variable_declaration_identifier(&mut self, name: &str, value: &hir::Expr) {
        let shadowed_name = self.current_scope().resolve_variable(name);
        let var_name = self.current_scope().declare_variable(name);
        self.push_context(BlockContext::Expression);
        if let Some(shadowed_name) = shadowed_name {
            self.current_scope()
                .declare_variable_alias(name, &shadowed_name);
        }
        self.push_let_declaration_to_expr(&var_name, value);
        self.push_str(";\n");
        self.current_scope().declare_variable_alias(name, &var_name);
        self.pop_context();
    }

    fn generate_variable_declaration_list_pattern(
        &mut self,
        patterns: &[hir::Pat],
        value: &hir::Expr,
    ) {
        self.push_indent();
        self.push_str("let [");

        let mut bindings = vec![];

        for (i, pattern) in patterns.iter().enumerate() {
            if i > 0 {
                self.push_str(", ");
            }
            match &pattern.kind {
                hir::PatKind::Identifier(_, ident) => {
                    let shadowed_name = self.current_scope().resolve_variable(ident.as_str());
                    let var_name = self.current_scope().declare_variable(ident.as_str());
                    self.push_str(&var_name);
                    if let Some(shadowed_name) = shadowed_name {
                        self.current_scope()
                            .declare_variable_alias(ident.as_str(), &shadowed_name);
                    }
                    bindings.push((ident, var_name));
                }
                hir::PatKind::Rest(pattern) => {
                    self.push_str("...");
                    self.generate_pat(pattern);
                }
                hir::PatKind::Wildcard => {}
                pattern_kind => todo!(
                    "Variable declaration pattern matching for {:?} is not implemented yet.",
                    pattern_kind
                ),
            }
        }

        self.push_str("] = ");
        self.push_context(BlockContext::Expression);
        self.generate_expr(value, None);

        for (ident_pattern, var_name) in bindings {
            self.current_scope()
                .declare_variable_alias(ident_pattern.as_str(), &var_name);
        }

        self.pop_context();
        self.push_str(";\n");
    }

    /// Generate a loop in statement context (not expression context)
    pub(crate) fn generate_loop_statement(&mut self, block: &hir::Block) {
        self.push_str("for (;;) {");
        self.push_newline();
        self.inc_indent();

        // Push loop context so break/continue expressions work correctly
        self.push_context(BlockContext::Loop);

        for stmt in &block.stmts {
            self.generate_stmt_in_loop_context(stmt);
        }

        if let Some(completion_expr) = &block.expr {
            self.push_indent();
            self.generate_expr_in_loop_context(completion_expr, None);
            if self.needs_semicolon_in_loop_context(completion_expr) {
                self.push_char(';');
            }
            self.push_newline();
        }

        // Pop loop context
        self.pop_context();

        self.dec_indent();
        self.push_indent();
        self.push_str("}");
    }

    /// Generate a statement within a statement loop context
    fn generate_stmt_in_loop_context(&mut self, stmt: &hir::Stmt) {
        match &stmt.kind {
            hir::StmtKind::Expr(expr) => {
                self.push_indent();
                self.generate_expr_in_loop_context(expr, None);
                if self.needs_semicolon_in_loop_context(expr) {
                    self.push_char(';');
                }
                self.push_newline();
            }
            _ => {
                // For other statement types, use normal generation
                self.generate_stmt(stmt);
            }
        }
    }

    /// Generate an expression within a statement loop context (break becomes break, not return)
    fn generate_expr_in_loop_context(
        &mut self,
        expr: &hir::Expr,
        parent_op: Option<hir::BinaryOpKind>,
    ) {
        match &expr.kind {
            hir::ExprKind::Break(break_expr) => {
                // In JavaScript loops, break cannot have a value
                // For loops with accumulators, we just generate 'break' and handle the return value elsewhere
                self.push_str("break");
                // Note: break_expr value is ignored in JavaScript loop context
                // The accumulator value should be handled by the loop return logic
            }
            hir::ExprKind::Continue => {
                self.push_str("continue");
            }
            hir::ExprKind::Block(block) => {
                self.push_str("{");
                self.push_newline();
                self.inc_indent();

                for stmt in &block.stmts {
                    self.generate_stmt_in_loop_context(stmt);
                }

                if let Some(completion_expr) = &block.expr {
                    self.push_indent();
                    self.generate_expr_in_loop_context(completion_expr, None);
                    if self.needs_semicolon_in_loop_context(completion_expr) {
                        self.push_char(';');
                    }
                    self.push_newline();
                }

                self.dec_indent();
                self.push_indent();
                self.push_str("}");
            }
            hir::ExprKind::IfElse(condition, then_branch, else_branches) => {
                self.push_str("if (");
                self.generate_expr_in_loop_context(condition, None);
                self.push_str(") ");
                self.generate_block_in_loop_context(then_branch);

                for else_clause in else_branches {
                    if let Some(else_condition) = &else_clause.condition {
                        self.push_str(" else if (");
                        self.generate_expr_in_loop_context(else_condition, None);
                        self.push_str(") ");
                    } else {
                        self.push_str(" else ");
                    }
                    self.generate_block_in_loop_context(&else_clause.consequence);
                }
            }
            _ => {
                // For other expressions, use normal generation
                self.push_context(BlockContext::Statement);
                self.generate_expr(expr, parent_op);
                self.pop_context();
            }
        }
    }

    /// Generate a block within a statement loop context
    fn generate_block_in_loop_context(&mut self, block: &hir::Block) {
        self.push_str("{");
        self.push_newline();
        self.inc_indent();

        for stmt in &block.stmts {
            self.generate_stmt_in_loop_context(stmt);
        }

        if let Some(completion_expr) = &block.expr {
            self.push_indent();
            self.generate_expr_in_loop_context(completion_expr, None);
            if self.needs_semicolon_in_loop_context(completion_expr) {
                self.push_char(';');
            }
            self.push_newline();
        }

        self.dec_indent();
        self.push_indent();
        self.push_str("}");
    }

    /// Check if an expression needs a semicolon in loop context
    fn needs_semicolon_in_loop_context(&self, expr: &hir::Expr) -> bool {
        !matches!(
            expr.kind,
            hir::ExprKind::Block(..)
                | hir::ExprKind::IfElse(..)
                | hir::ExprKind::Break(..)
                | hir::ExprKind::Continue
        )
    }

    /// Check if a block represents a for loop pattern
    /// For loops are typically lowered to blocks with iterator setup + pattern matching
    fn is_for_loop_pattern(&self, block: &hir::Block) -> bool {
        // Check if the block has iterator setup statements
        let has_iterator_setup = block.stmts.iter().any(|stmt| {
            if let hir::StmtKind::Let(pat, _expr, _) = &stmt.kind {
                if let hir::PatKind::Identifier(_, ident) = &pat.kind {
                    if ident.as_str().contains("iterator$$") {
                        return true;
                    }
                }
            }
            false
        });

        // Check if the block has pattern matching statements that check iterator results
        let has_iterator_pattern_matching = block.stmts.iter().any(|stmt| {
            let result = self.statement_contains_iterator_pattern_matching(stmt);
            result
        });

        // Also check the block completion expression for pattern matching
        let completion_has_pattern_matching = if let Some(completion_expr) = &block.expr {
            match &completion_expr.kind {
                hir::ExprKind::Loop(loop_block) => {
                    // Check if the loop block contains pattern matching
                    let loop_has_pattern_matching = loop_block.stmts.iter().any(|stmt| {
                        self.statement_contains_iterator_pattern_matching(stmt)
                    }) || loop_block.expr.as_ref().map_or(false, |expr| {
                        self.expr_contains_iterator_pattern_matching(expr)
                    });
                    loop_has_pattern_matching
                }
                _ => {
                    let result = self.expr_contains_iterator_pattern_matching(completion_expr);
                    result
                }
            }
        } else {
            false
        };

        let is_for_loop = has_iterator_setup && (has_iterator_pattern_matching || completion_has_pattern_matching);
        is_for_loop
    }

    /// Check if a statement contains iterator pattern matching
    fn statement_contains_iterator_pattern_matching(&self, stmt: &hir::Stmt) -> bool {
        match &stmt.kind {
            hir::StmtKind::Expr(expr) => {
                let result = self.expr_contains_iterator_pattern_matching(expr);
                result
            }
            _ => false,
        }
    }

    /// Check if an expression contains iterator pattern matching (break statements with Option checks)
    fn expr_contains_iterator_pattern_matching(&self, expr: &hir::Expr) -> bool {
        match &expr.kind {
            hir::ExprKind::Match(_match_expr, arms) => {
                // Check if this is a match on iterator results with Option patterns
                arms.iter().any(|arm| {
                    // Look for Option::None patterns with break expressions
                    if let hir::PatKind::Enum(path, _) = &arm.pat.kind {
                        if path.segments.len() >= 1 {
                            let segment = &path.segments[path.segments.len() - 1];
                            if segment.ident.as_str() == "None" {
                                // Check if the consequence has a break expression
                                return self.block_contains_break(&arm.block);
                            }
                        }
                    }
                    false
                })
            }
            hir::ExprKind::IfElse(_, then_branch, else_branches) => {
                // Check if this is an if-else pattern that handles Option results
                let then_has_break = self.block_contains_break(then_branch);
                let else_has_break = else_branches.iter().any(|else_branch| self.block_contains_break(&else_branch.consequence));
                then_has_break || else_has_break
            }
            _ => false,
        }
    }

    /// Check if a block contains break expressions
    fn block_contains_break(&self, block: &hir::Block) -> bool {
        block.stmts.iter().any(|stmt| self.statement_contains_break(stmt)) ||
        block.expr.as_ref().map_or(false, |expr| self.expr_contains_break(expr))
    }

    /// Check if a statement contains break expressions
    fn statement_contains_break(&self, stmt: &hir::Stmt) -> bool {
        match &stmt.kind {
            hir::StmtKind::Expr(expr) => self.expr_contains_break(expr),
            _ => false,
        }
    }

    /// Check if an expression contains break expressions
    fn expr_contains_break(&self, expr: &hir::Expr) -> bool {
        matches!(expr.kind, hir::ExprKind::Break(_))
    }

    /// Generate a for loop block with proper iteration wrapper
    fn generate_for_loop_block(&mut self, block: &hir::Block) {
        self.push_indent();
        self.push_str("{");
        self.push_newline();
        self.inc_indent();

        // Generate iterator setup statements first - ensure they are properly output
        for (i, stmt) in block.stmts.iter().enumerate() {
            if self.is_iterator_setup_statement(stmt) {
                // Force generation of iterator setup by handling it directly
                if let hir::StmtKind::Let(pat, expr, _) = &stmt.kind {
                    if let hir::PatKind::Identifier(_, ident) = &pat.kind {
                        if ident.as_str().contains("iterator$$") || ident.as_str().contains("accumulator$$") {
                            // Generate the statement with proper indentation and context
                            self.push_indent();
                            self.push_str("let ");
                            self.push_str(ident.as_str());
                            self.push_str(" = ");
                            self.generate_expr(expr, None);
                            self.push_str(";");
                            self.push_newline();
                            
                            // Force flush the statement buffer
                            self.flush_statement_buffer();
                            
                            continue;
                        }
                    }
                }
                // Fallback to normal statement generation
                self.generate_stmt(stmt);
            }
        }

        // Also handle accumulator statements
        for stmt in &block.stmts {
            if let hir::StmtKind::Let(pat, expr, _) = &stmt.kind {
                if let hir::PatKind::Identifier(_, ident) = &pat.kind {
                    if ident.as_str().contains("accumulator$$") && !self.is_iterator_setup_statement(stmt) {
                        self.push_indent();
                        self.generate_variable_declaration(pat, expr);
                    }
                }
            }
        }

        // Generate the for loop wrapper
        self.push_indent();
        self.push_str("for (;;) {");
        self.push_newline();
        self.inc_indent();
        
        // Force flush to ensure for loop wrapper appears
        self.flush_statement_buffer();

        // Set loop context for break statements
        self.push_context(BlockContext::Loop);

        // Generate non-iterator-setup statements inside the loop
        for stmt in &block.stmts {
            if !self.is_iterator_setup_statement(stmt) && !self.is_accumulator_setup_statement(stmt) {
                self.generate_stmt_in_loop_context(stmt);
            }
        }

        // Generate block completion expression if it exists and it's a loop
        if let Some(completion_expr) = &block.expr {
            if let hir::ExprKind::Loop(loop_block) = &completion_expr.kind {
                // Generate the loop content directly (pattern matching logic)
                for stmt in &loop_block.stmts {
                    self.generate_stmt_in_loop_context(stmt);
                }
                
                if let Some(loop_completion_expr) = &loop_block.expr {
                    self.push_indent();
                    self.generate_expr_in_loop_context(loop_completion_expr, None);
                    if self.needs_semicolon_in_loop_context(loop_completion_expr) {
                        self.push_char(';');
                    }
                    self.push_newline();
                }
            } else {
                // Handle other completion expressions normally
                self.push_indent();
                self.generate_expr_in_loop_context(completion_expr, None);
                if self.needs_semicolon_in_loop_context(completion_expr) {
                    self.push_char(';');
                }
                self.push_newline();
            }
        }

        // Restore context
        self.pop_context();

        // Close the for loop
        self.dec_indent();
        self.push_indent();
        self.push_str("}");
        self.push_newline();

        // Close the outer block
        self.dec_indent();
        self.push_indent();
        self.push_str("}");
        self.push_newline();
    }

    /// Check if a statement is iterator setup (should be outside the loop)
    fn is_iterator_setup_statement(&self, stmt: &hir::Stmt) -> bool {
        if let hir::StmtKind::Let(pat, _expr, _) = &stmt.kind {
            if let hir::PatKind::Identifier(_, ident) = &pat.kind {
                if ident.as_str().contains("iterator$$") {
                    return true;
                }
            }
        }
        false
    }

    /// Check if a statement is accumulator setup (should be outside the loop)
    fn is_accumulator_setup_statement(&self, stmt: &hir::Stmt) -> bool {
        if let hir::StmtKind::Let(pat, _expr, _) = &stmt.kind {
            if let hir::PatKind::Identifier(_, ident) = &pat.kind {
                if ident.as_str().contains("accumulator$$") {
                    return true;
                }
            }
        }
        false
    }
}
