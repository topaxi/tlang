use crate::{pattern_match_generator::MatchContextStack, scope::Scope};
use tlang_ast::{
    node::{self as ast, Ident},
    symbols::SymbolType,
    token::{Literal, Token, TokenKind},
};
use tlang_hir::hir;

// Before we indent a line, we reserve at least the indentation space plus some more for the the
// next statement. We start with an assumption of 128 below, this might the maximum overhead once
// we are done generating the code.
const STATEMENT_RESERVE_SIZE: usize = 128;

// When we create a new statement buffer, we reserve a certain amount of space for the statements.
const STATEMENT_BUFFER_CAPACITY: usize = 512;

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum BlockContext {
    // Are we in a top level Program?
    Program,
    // Are we in a StatementExpression with a Block?
    Statement,
    // Are we in an expression with a Block?
    Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct FunctionContext {
    // The name of the current function we're in.
    // Empty string for anonymous functions.
    pub name: String,

    // The parameter name bindings of the current function we're in.
    pub parameter_bindings: Vec<String>,

    // Is the current function body tail recursive?
    // This is used to determine if we should unwrap the recursion into a while loop.
    pub is_tail_recursive: bool,
}

#[derive(Debug)]
pub struct CodegenJS {
    output: String,
    indent_level: usize,
    scopes: Scope,
    context_stack: Vec<BlockContext>,
    function_context_stack: Vec<FunctionContext>,
    pub(crate) match_context_stack: MatchContextStack,
    statement_buffer: Vec<String>,
    completion_variables: Vec<Option<String>>,
}

impl Default for CodegenJS {
    fn default() -> Self {
        Self::new()
    }
}

impl CodegenJS {
    pub fn new() -> Self {
        Self {
            output: String::new(),
            indent_level: 0,
            scopes: Scope::default(),
            context_stack: vec![BlockContext::Program],
            function_context_stack: vec![],
            match_context_stack: Default::default(),
            statement_buffer: vec![String::with_capacity(STATEMENT_BUFFER_CAPACITY)],
            completion_variables: vec![None],
        }
    }

    pub fn get_standard_library_source() -> String {
        let stdlib_js_src = include_str!("../std/lib.tl");
        let notice = r#"// This file was auto-generated by the tlang compiler."#;

        format!("{}\n{}", notice, stdlib_js_src)
    }

    pub fn get_standard_library_symbols() -> &'static [(&'static str, SymbolType); 28] {
        &[
            ("Option", SymbolType::Enum),
            ("Result", SymbolType::Enum),
            ("Option::Some", SymbolType::EnumVariant),
            ("Option::None", SymbolType::EnumVariant),
            ("Result::Ok", SymbolType::EnumVariant),
            ("Result::Err", SymbolType::EnumVariant),
            ("Some", SymbolType::Function),
            ("None", SymbolType::Variable),
            ("Ok", SymbolType::Function),
            ("Err", SymbolType::Function),
            ("len", SymbolType::Function),
            ("log", SymbolType::Function),
            ("math", SymbolType::Module),
            ("math::max", SymbolType::Function),
            ("math::min", SymbolType::Function),
            ("math::floor", SymbolType::Function),
            ("math::random", SymbolType::Function),
            ("math::sqrt", SymbolType::Function),
            ("random_int", SymbolType::Function),
            ("compose", SymbolType::Function),
            ("map", SymbolType::Function),
            ("filter", SymbolType::Function),
            ("filter_map", SymbolType::Function),
            ("partition", SymbolType::Function),
            ("foldl", SymbolType::Function),
            ("foldr", SymbolType::Function),
            ("sum", SymbolType::Function),
            ("zip", SymbolType::Function),
        ]
    }

    pub(crate) fn push_statement_buffer(&mut self) {
        self.statement_buffer
            .push(String::with_capacity(STATEMENT_BUFFER_CAPACITY));
    }

    #[must_use]
    pub(crate) fn replace_statement_buffer(&mut self, buffer: String) -> String {
        let buf = self.statement_buffer.pop();
        self.statement_buffer.push(buffer);
        buf.unwrap()
    }

    #[must_use]
    pub(crate) fn replace_statement_buffer_with_empty_string(&mut self) -> String {
        self.replace_statement_buffer(String::with_capacity(STATEMENT_BUFFER_CAPACITY))
    }

    pub(crate) fn pop_statement_buffer(&mut self) -> String {
        debug_assert!(
            self.statement_buffer.len() != 1,
            "Cannot pop last statement buffer."
        );

        self.statement_buffer.pop().unwrap()
    }

    #[inline(always)]
    pub(crate) fn push_str(&mut self, str: &str) {
        self.statement_buffer.last_mut().unwrap().push_str(str);
    }

    #[inline(always)]
    pub(crate) fn push_char(&mut self, char: char) {
        self.statement_buffer.last_mut().unwrap().push(char);
    }

    #[inline(always)]
    pub(crate) fn push_newline(&mut self) {
        self.push_char('\n');
    }

    #[inline(always)]
    pub(crate) fn push_indent(&mut self) {
        // We are starting a new line, we should reserve at least the space for the indentation
        // plus some extra space for the statement.
        let reserve = self.indent_level * 4 + STATEMENT_RESERVE_SIZE;

        self.statement_buffer.last_mut().unwrap().reserve(reserve);

        for _ in 0..self.indent_level {
            self.push_str("    ");
        }
    }

    #[inline(always)]
    pub(crate) fn inc_indent(&mut self) {
        self.indent_level += 1;
    }

    #[inline(always)]
    pub(crate) fn dec_indent(&mut self) {
        self.indent_level -= 1;
    }

    #[inline(always)]
    pub(crate) fn flush_statement_buffer(&mut self) {
        self.output.push_str(self.statement_buffer.last().unwrap());
        self.statement_buffer.last_mut().unwrap().clear();
    }

    #[inline(always)]
    pub(crate) fn current_scope(&mut self) -> &mut Scope {
        &mut self.scopes
    }

    #[inline(always)]
    pub(crate) fn push_scope(&mut self) {
        self.scopes = Scope::new(Some(Box::new(self.scopes.clone())));
    }

    #[inline(always)]
    pub(crate) fn pop_scope(&mut self) {
        if let Some(parent) = self.scopes.get_parent() {
            self.scopes = parent.clone();
        }
    }

    pub(crate) fn current_context(&self) -> BlockContext {
        *self.context_stack.last().unwrap()
    }

    pub(crate) fn push_context(&mut self, context: BlockContext) {
        self.context_stack.push(context);
    }

    pub(crate) fn pop_context(&mut self) {
        self.context_stack.pop();
    }

    pub fn generate_code(&mut self, module: &hir::Module) {
        self.generate_statements(&module.block.stmts);
        self.output.shrink_to_fit();
    }

    pub(crate) fn get_function_context(&self) -> Option<&FunctionContext> {
        self.function_context_stack.last()
    }

    pub(crate) fn push_function_context(
        &mut self,
        name: &str,
        parameters: &[hir::FunctionParameter],
        parameter_bindings: &[String],
        is_tail_recursive: bool,
    ) {
        // We currently only need the parameter names/bindings for tail recursive functions.
        let parameter_bindings = if !is_tail_recursive {
            vec![]
        // If we have parameter_bindings provided, we use them to assign the parameters in
        // recursive function calls.
        } else if parameters.len() == parameter_bindings.len() {
            parameter_bindings.to_vec()
        } else {
            parameters
                .iter()
                .map(|param| {
                    if let hir::PatKind::Identifier(_, ident) = &param.pattern.kind {
                        ident.to_string()
                    } else {
                        // Encountered destructuring/pattern matching or wildcard, declare
                        // tmp variable to potentially dereference from. Unused at the moment.
                        self.scopes.declare_tmp_variable()
                    }
                })
                .collect()
        };

        self.function_context_stack.push(FunctionContext {
            name: name.to_string(),
            parameter_bindings,
            is_tail_recursive,
        });
    }

    pub(crate) fn pop_function_context(&mut self) {
        self.function_context_stack.pop();
    }

    #[inline(always)]
    pub(crate) fn push_completion_variable(&mut self, name: Option<String>) {
        self.completion_variables.push(name);
    }

    #[inline(always)]
    pub(crate) fn pop_completion_variable(&mut self) {
        self.completion_variables.pop();
    }

    #[inline(always)]
    pub(crate) fn current_completion_variable(&self) -> Option<&str> {
        self.completion_variables.last().unwrap().as_deref()
    }

    #[inline(always)]
    pub(crate) fn current_completion_variable_count(&self) -> usize {
        self.completion_variables.len()
    }

    #[inline(always)]
    pub(crate) fn nth_completion_variable(&self, index: usize) -> Option<&str> {
        self.completion_variables.get(index).unwrap().as_deref()
    }

    pub(crate) fn push_let_declaration(&mut self, name: &str) {
        self.push_indent();
        self.push_str("let ");
        self.push_str(name);
        self.push_char(';');
    }

    pub(crate) fn push_open_let_declaration(&mut self, name: &str) {
        self.push_indent();
        self.push_str("let ");
        self.push_str(name);
        self.push_str(" = ");
    }

    pub(crate) fn push_let_declaration_to_expr(&mut self, name: &str, expr: &hir::Expr) {
        self.push_open_let_declaration(name);
        self.generate_expr(expr, None);
    }

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
                self.push_str(&format!("\"{value}\""));
            }
        }
    }

    /// Generates blocks in expression position.
    fn generate_block_expression(&mut self, block: &hir::Block) {
        let has_completion_var = self.completion_variables.last().unwrap().is_some();
        let completion_tmp_var = self
            .completion_variables
            .last()
            .unwrap_or(&None)
            .clone()
            .unwrap_or_else(|| self.scopes.declare_tmp_variable());
        self.push_scope();

        // In a case of `let a = { 1 }`, we want to render the expression as a statement.
        // At this state, we should have `let a = ` in the statement buffer.
        // We do this by temporarily swapping the statement buffer, generating the
        // expression as an statement, and then swapping the statement buffer back.
        let lhs = if has_completion_var {
            String::new()
        } else {
            self.replace_statement_buffer_with_empty_string()
        };

        if block.has_completion() {
            if has_completion_var {
                // Halp I made a mess
            } else {
                self.push_indent();
                self.push_let_declaration(&completion_tmp_var);
                self.push_str("{\n");
                self.indent_level += 1;
            }
        }

        self.generate_statements(&block.stmts);

        if !block.has_completion() {
            self.push_statement_buffer();
            self.push_str(&lhs);
            self.flush_statement_buffer();
            self.pop_statement_buffer();
            self.flush_statement_buffer();
            self.pop_scope();
            return;
        }

        self.push_indent();
        if completion_tmp_var == "return" {
            self.push_str("return ");
        } else {
            self.push_str(&completion_tmp_var);
            self.push_str(" = ");
        }
        self.generate_optional_expr(&block.expr, None);
        self.push_char(';');
        self.push_newline();
        if !has_completion_var {
            self.indent_level -= 1;
            self.push_indent();
            self.push_str("};\n");
            self.flush_statement_buffer();
            self.push_str(&lhs);
            self.push_str(completion_tmp_var.as_str());
        }
        self.flush_statement_buffer();
        self.pop_scope();
    }

    fn generate_block(&mut self, block: &hir::Block) {
        self.push_scope();

        self.generate_statements(&block.stmts);
        self.generate_optional_expr(&block.expr, None);

        self.pop_scope();
    }

    #[inline(always)]
    pub(crate) fn generate_statements(&mut self, statements: &[hir::Stmt]) {
        for statement in statements {
            self.generate_stmt(statement);
            self.flush_statement_buffer();
        }
    }

    pub(crate) fn generate_comment(&mut self, comment: &Token) {
        match &comment.kind {
            TokenKind::SingleLineComment(comment) => {
                self.push_str("//");
                self.push_str(comment);
                self.push_newline();
            }
            TokenKind::MultiLineComment(comment) => {
                self.push_str("/*");
                self.push_str(comment);
                self.push_str("*/");
                self.push_newline();
            }
            _ => {}
        }
    }

    pub(crate) fn generate_comments(&mut self, comments: &[Token]) {
        for comment in comments {
            self.push_indent();
            self.generate_comment(comment);
        }
    }

    pub(crate) fn generate_stmt(&mut self, statement: &hir::Stmt) {
        self.generate_comments(&statement.leading_comments);

        match &statement.kind {
            hir::StmtKind::Expr(expr) => {
                self.push_indent();
                self.context_stack.push(BlockContext::Statement);
                self.generate_expr(expr, None);
                self.context_stack.pop();

                if let hir::ExprKind::IfElse { .. } = expr.kind {
                    self.push_newline();
                    return;
                }

                self.push_str(";\n");
            }
            hir::StmtKind::Let(pattern, expression, _ty) => {
                self.generate_variable_declaration(pattern, expression);
            }
            hir::StmtKind::FunctionDeclaration(decl) => self.generate_function_declaration(decl),
            hir::StmtKind::Return(expr) => self.generate_return_statement(expr),
            hir::StmtKind::EnumDeclaration(decl) => self.generate_enum_declaration(decl),
            hir::StmtKind::StructDeclaration(decl) => self.generate_struct_declaration(decl),
            hir::StmtKind::None => {}
        }

        self.generate_comments(&statement.trailing_comments);
    }

    #[inline(always)]
    pub(crate) fn generate_optional_expr(
        &mut self,
        expr: &Option<hir::Expr>,
        parent_op: Option<ast::BinaryOpKind>,
    ) {
        if let Some(expr) = expr {
            self.generate_expr(expr, parent_op);
        }
    }

    pub(crate) fn generate_expr(&mut self, expr: &hir::Expr, parent_op: Option<ast::BinaryOpKind>) {
        match &expr.kind {
            hir::ExprKind::Block(block) if self.current_context() == BlockContext::Expression => {
                self.generate_block_expression(block)
            }
            hir::ExprKind::Block(block) => self.generate_block(block),
            hir::ExprKind::Call(expr) => self.generate_call_expression(expr),
            hir::ExprKind::FieldAccess(base, field) => {
                self.generate_field_access_expression(base, field)
            }
            hir::ExprKind::Path(path) => self.generate_path_expression(path),
            hir::ExprKind::IndexAccess(base, index) => {
                self.generate_index_access_expression(base, index)
            }
            hir::ExprKind::Let(_pattern, _expr) => todo!("Let expression not implemented yet."),
            hir::ExprKind::Literal(literal) => self.generate_literal(literal),
            hir::ExprKind::List(items) => self.generate_list_expression(items),
            hir::ExprKind::Dict(kvs) => self.generate_dict_expression(kvs),
            hir::ExprKind::Unary(op, expr) => self.generate_unary_op(op, expr),
            hir::ExprKind::Binary(op, lhs, rhs) => {
                self.generate_binary_op(*op, lhs, rhs, parent_op)
            }
            hir::ExprKind::Match(expr, arms) => self.generate_match_expression(expr, arms),
            hir::ExprKind::IfElse(expr, then_branch, else_branches) => {
                self.generate_if_else(expr, then_branch, else_branches)
            }
            hir::ExprKind::FunctionExpression(decl) => self.generate_function_expression(decl),
            hir::ExprKind::TailCall(expr) => self.generate_recursive_call_expression(expr),
            hir::ExprKind::Range(_) => todo!("Range expression not implemented yet."),
            hir::ExprKind::Wildcard => {}
        }
    }

    fn generate_unary_op(&mut self, op: &ast::UnaryOp, expr: &hir::Expr) {
        match op {
            ast::UnaryOp::Not => self.push_char('!'),
            ast::UnaryOp::Minus => self.push_char('-'),
            ast::UnaryOp::Spread => self.push_str("..."),
            ast::UnaryOp::Rest => unreachable!("Rest operator is not an operator but a pattern"),
        }

        self.generate_expr(expr, None);
    }

    fn generate_field_access_expression(&mut self, base: &hir::Expr, field: &Ident) {
        self.generate_expr(base, None);
        self.push_char('.');
        self.push_str(field.as_str());
    }

    fn generate_index_access_expression(&mut self, base: &hir::Expr, index: &hir::Expr) {
        self.generate_expr(base, None);
        self.push_char('[');
        self.generate_expr(index, None);
        self.push_char(']');
    }

    fn generate_path_expression(&mut self, path: &hir::Path) {
        let first_segment = path.segments.first().unwrap();

        self.generate_identifier(&first_segment.ident);
        self.push_str(
            &path.segments[1..]
                .iter()
                .fold("".to_string(), |acc, segment| {
                    acc + "." + segment.ident.as_str()
                }),
        );
    }

    fn generate_list_expression(&mut self, items: &[hir::Expr]) {
        self.push_char('[');
        for (i, item) in items.iter().enumerate() {
            if i > 0 {
                self.push_str(", ");
            }
            self.generate_expr(item, None);
        }
        self.push_char(']');
    }

    fn generate_dict_expression(&mut self, kvs: &[(hir::Expr, hir::Expr)]) {
        self.push_str("{\n");
        self.indent_level += 1;
        for (i, (key, value)) in kvs.iter().enumerate() {
            if i > 0 {
                self.push_str(",\n");
            }
            self.push_indent();
            self.generate_expr(key, None);

            // If both key and value are the same identifier, we can use shorthand syntax.
            if key.path() != value.path() {
                self.push_str(": ");
                self.generate_expr(value, None);
            }
        }
        self.push_str(",\n");
        self.indent_level -= 1;
        self.push_indent();
        self.push_char('}');
    }

    // TODO: This is used for destructuring, we might not want to have destructuring in the
    //       language and use pattern matching instead.
    pub(crate) fn generate_pat(&mut self, pattern: &hir::Pat) {
        match &pattern.kind {
            hir::PatKind::Identifier(..) if pattern.is_self() => {
                self.current_scope().declare_variable_alias("self", "this");
            }
            hir::PatKind::Identifier(_, ident) => {
                let var_name = self.current_scope().declare_variable(ident.name.as_str());
                self.push_str(&var_name);
                self.current_scope()
                    .declare_variable_alias(ident.name.as_str(), &var_name);
            }
            hir::PatKind::Enum(_path, _enum_pattern) => {
                todo!("enum extraction outside of function parameters is not implemented yet.")
            }
            hir::PatKind::Literal(literal) => self.generate_literal(literal),
            hir::PatKind::List(patterns) => {
                self.push_char('[');
                for (i, pattern) in patterns.iter().enumerate() {
                    if i > 0 {
                        self.push_str(", ");
                    }
                    self.generate_pat(pattern);
                }
                self.push_char(']');
            }
            hir::PatKind::Rest(pattern) => {
                self.push_str("...");
                self.generate_pat(pattern);
            }
            hir::PatKind::Wildcard => {}
        }
    }

    fn generate_identifier(&mut self, name: &Ident) {
        let name_string = name.as_str();
        let identifier = self
            .current_scope()
            .resolve_variable(name_string)
            .unwrap_or_else(|| name_string.to_string());
        self.push_str(&identifier);
    }

    fn generate_variable_declaration(&mut self, pattern: &hir::Pat, value: &hir::Expr) {
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
        self.context_stack.push(BlockContext::Expression);
        if let Some(shadowed_name) = shadowed_name {
            self.current_scope()
                .declare_variable_alias(name, &shadowed_name);
        }
        self.push_let_declaration_to_expr(&var_name, value);
        self.push_str(";\n");
        self.current_scope().declare_variable_alias(name, &var_name);
        self.context_stack.pop();
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
        self.context_stack.push(BlockContext::Expression);
        self.generate_expr(value, None);

        for (ident_pattern, var_name) in bindings {
            self.current_scope()
                .declare_variable_alias(ident_pattern.name.as_str(), &var_name);
        }

        self.context_stack.pop();
        self.push_str(";\n");
    }

    fn generate_if_else(
        &mut self,
        expr: &hir::Expr,
        then_branch: &hir::Block,
        else_branches: &[hir::ElseClause],
    ) {
        let mut lhs = String::new();
        // TODO: Potentially in a return position or other expression, before we generate the if
        //       statement, replace the current statement buffer with a new one, generate the if
        //       statement, and then swap the statement buffer back.
        //       Similar to how we generate blocks in expression position.
        // TODO: Find a way to do this generically for all expressions which are represented as
        //       statements in JavaScript.
        // TODO: In case of recursive calls in tail position, we'll want to omit lhs.
        let has_block_completions =
            self.current_context() == BlockContext::Expression && then_branch.has_completion();
        if has_block_completions {
            // TODO: We could probably reuse existing completion vars here.
            if let Some("return") = self.current_completion_variable() {
                self.push_completion_variable(Some("return".to_string()));
                lhs = self.replace_statement_buffer_with_empty_string();
                self.push_indent();
            } else {
                lhs = self.replace_statement_buffer_with_empty_string();
                let completion_tmp_var = self.scopes.declare_tmp_variable();
                self.push_let_declaration(&completion_tmp_var);
                self.push_completion_variable(Some(completion_tmp_var));
            }
        } else {
            self.push_completion_variable(None);
        }
        self.push_str("if (");
        let indent_level = self.indent_level;
        self.indent_level = 0;
        self.generate_expr(expr, None);
        self.indent_level = indent_level;
        self.push_str(") {\n");
        self.indent_level += 1;
        self.flush_statement_buffer();
        self.generate_block_expression(then_branch);
        self.indent_level -= 1;

        for else_branch in else_branches {
            self.push_indent();
            self.push_str("} else");

            if let Some(ref condition) = else_branch.condition {
                self.push_str(" if (");
                let indent_level = self.indent_level;
                self.indent_level = 0;
                self.generate_expr(condition, None);
                self.indent_level = indent_level;
                self.push_char(')');
            }

            self.push_str(" {\n");
            self.indent_level += 1;
            self.flush_statement_buffer();
            self.generate_block_expression(&else_branch.consequence);
            self.indent_level -= 1;
        }

        self.push_indent();
        self.push_char('}');
        if has_block_completions && self.current_completion_variable() != Some("return") {
            self.push_newline();

            let completion_var = self.completion_variables.last().unwrap().clone().unwrap();

            // If we have an lhs, put the completion var as the rhs of the lhs.
            // Otherwise, we assign the completion_var to the previous completion_var.
            if !lhs.is_empty() {
                self.push_str(&lhs);
                self.push_str(&completion_var);
            } else {
                self.push_indent();
                let prev_completion_var = self.completion_variables
                    [self.completion_variables.len() - 2]
                    .clone()
                    .unwrap();
                self.push_str(&prev_completion_var);
                self.push_str(" = ");
                self.push_str(&completion_var);
                self.push_str(";\n");
            }
        }
        self.completion_variables.pop();
    }

    fn generate_partial_application(
        &mut self,
        call_expr: &hir::CallExpression,
        wildcard_count: usize,
    ) {
        let mut placeholders = Vec::with_capacity(wildcard_count);

        self.push_char('(');

        if wildcard_count == 1 {
            self.push_char('_');
            placeholders.push('_'.to_string());
        } else {
            for n in 0..wildcard_count {
                if n > 0 {
                    self.push_str(", ");
                }

                let tmp_var = self.scopes.declare_unique_variable("_");

                self.push_str(&tmp_var);

                placeholders.push(tmp_var);
            }
        }

        self.push_str(") => ");

        self.generate_expr(&call_expr.callee, None);
        self.push_char('(');

        let mut wildcard_index = 0;
        for (i, arg) in call_expr.arguments.iter().enumerate() {
            if i > 0 {
                self.push_str(", ");
            }

            if let hir::ExprKind::Wildcard = arg.kind {
                self.push_str(&placeholders[wildcard_index]);
                wildcard_index += 1;
            } else {
                self.generate_expr(arg, None);
            }
        }

        self.push_char(')');
    }

    pub(crate) fn generate_call_expression(&mut self, call_expr: &hir::CallExpression) {
        // TODO: If the call is to a struct, we instead call it with `new` and map the fields to
        // the positional arguments of the constructor.

        let wildcard_count = call_expr.wildcard_count();

        if wildcard_count > 0 {
            return self.generate_partial_application(call_expr, wildcard_count);
        }

        self.generate_expr(&call_expr.callee, None);
        self.push_char('(');

        let mut args_iter = call_expr.arguments.iter();

        if let Some(arg) = args_iter.next() {
            self.generate_expr(arg, None);
        }

        for arg in args_iter {
            self.push_str(", ");
            self.generate_expr(arg, None);
        }

        self.push_char(')');
    }

    pub fn get_output(&self) -> &str {
        &self.output
    }
}

pub(crate) fn needs_semicolon(expr: &Option<hir::Expr>) -> bool {
    if let Some(expr) = expr {
        !matches!(
            &expr.kind,
            hir::ExprKind::Block(..) | hir::ExprKind::IfElse(..) | hir::ExprKind::Match(..)
        )
    } else {
        true
    }
}
