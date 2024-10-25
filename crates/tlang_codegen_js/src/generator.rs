use crate::scope::Scope;
use tlang_ast::{
    node::{
        BinaryOpKind, Block, CallExpression, EnumPattern, Expr, ExprKind, FieldAccessExpression,
        FunctionParameter, Ident, IfElseExpression, IndexAccessExpression, Module, Path, Pattern,
        PatternKind, Stmt, StmtKind, UnaryOp,
    },
    symbols::SymbolType,
    token::{Literal, Token, TokenKind},
};

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
    // Are we in a FunctionDeclaration or FunctionExpression with a Block?
    FunctionBody,
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

#[derive(Debug, PartialEq)]
pub struct CodegenJS {
    output: String,
    indent_level: usize,
    scopes: Scope,
    context_stack: Vec<BlockContext>,
    function_context_stack: Vec<FunctionContext>,
    function_pre_body_declarations: Vec<(String, String)>,
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
            function_pre_body_declarations: vec![],
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
        assert!(
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

    pub fn generate_code(&mut self, module: &Module) {
        self.generate_statements(&module.statements);
        self.output.shrink_to_fit();
    }

    pub(crate) fn declare_function_pre_body_variable(&mut self, name: &str, value: &str) {
        self.function_pre_body_declarations
            .push((name.to_string(), value.to_string()));
    }

    pub(crate) fn consume_function_pre_body_declarations(&mut self) -> Vec<(String, String)> {
        std::mem::take(&mut self.function_pre_body_declarations)
    }

    pub(crate) fn get_function_context(&self) -> Option<&FunctionContext> {
        self.function_context_stack.last()
    }

    pub(crate) fn push_function_context(
        &mut self,
        name: &str,
        parameters: &[FunctionParameter],
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
                    if let PatternKind::Identifier(ident_pattern) = &param.pattern.kind {
                        ident_pattern.name.to_string()
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

    pub(crate) fn push_let_declaration_to_expr(&mut self, name: &str, expr: &Expr) {
        self.push_open_let_declaration(name);
        self.generate_expr(expr, None);
    }

    pub(crate) fn push_let_declaration_to_identifier(&mut self, name: &str, value: &str) {
        self.push_open_let_declaration(name);
        self.push_str(value);
        self.push_char(';');
        self.push_newline();
    }

    fn generate_literal(&mut self, literal: &Literal) {
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
    fn generate_block_expression(&mut self, block: &Block) {
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

        self.generate_statements(&block.statements);

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
        self.push_str(&completion_tmp_var);
        self.push_str(" = ");
        self.generate_optional_expr(&block.expression, None);
        self.push_str(";\n");
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

    pub(crate) fn generate_block(&mut self, block: &Block) {
        // Functions handle their scoping themselves
        if self.current_context() != BlockContext::FunctionBody {
            self.push_scope();
        }

        self.generate_statements(&block.statements);

        if !block.has_completion() {
            if self.current_context() != BlockContext::FunctionBody {
                self.pop_scope();
            }
            return;
        }

        if let BlockContext::FunctionBody = self.current_context() {
            // We only render the return statement if we are not in a tail recursive function body
            // and the node is RecursiveCall pointing to the current function.
            if let Some(function_context) = self.function_context_stack.last() {
                if function_context.is_tail_recursive && block.has_completion() {
                    if let Some(ref expression) = block.expression {
                        if let ExprKind::RecursiveCall(_) = expression.kind {
                            self.generate_expr(expression, None);
                            return;
                        }
                    }
                }
            }

            self.push_indent();
            self.push_str("return ");
            self.push_context(BlockContext::Expression);
            self.generate_optional_expr(&block.expression, None);
            self.pop_context();
            self.push_char(';');
            self.push_newline();
            self.flush_statement_buffer();
        }

        if self.current_context() != BlockContext::FunctionBody {
            self.pop_scope();
        }
    }

    #[inline(always)]
    fn generate_statements(&mut self, statements: &[Stmt]) {
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

    pub(crate) fn generate_stmt(&mut self, statement: &Stmt) {
        self.generate_comments(&statement.leading_comments);

        match &statement.kind {
            StmtKind::None => {}
            StmtKind::Expr(expr) => {
                self.push_indent();
                self.context_stack.push(BlockContext::Statement);
                self.generate_expr(expr, None);
                self.context_stack.pop();

                if let ExprKind::IfElse { .. } = expr.kind {
                    self.push_newline();
                    return;
                }

                self.push_str(";\n");
            }
            StmtKind::Let(decl) => {
                self.generate_variable_declaration(&decl.pattern, &decl.expression);
            }
            StmtKind::FunctionDeclaration(decl) => self.generate_function_declaration(decl),
            StmtKind::FunctionDeclarations(decls) => {
                self.generate_function_declarations(decls, &statement.leading_comments)
            }
            StmtKind::Return(expr) => self.generate_return_statement(expr),
            StmtKind::EnumDeclaration(decl) => self.generate_enum_declaration(decl),
            StmtKind::StructDeclaration(decl) => self.generate_struct_declaration(decl),
        }

        self.generate_comments(&statement.trailing_comments);
    }

    #[inline(always)]
    pub(crate) fn generate_optional_expr(
        &mut self,
        expr: &Option<Expr>,
        parent_op: Option<&BinaryOpKind>,
    ) {
        if let Some(expr) = expr {
            self.generate_expr(expr, parent_op);
        }
    }

    pub(crate) fn generate_expr(&mut self, expr: &Expr, parent_op: Option<&BinaryOpKind>) {
        match &expr.kind {
            ExprKind::None => unreachable!(),
            ExprKind::Block(block) if self.current_context() == BlockContext::Expression => {
                self.generate_block_expression(block)
            }
            ExprKind::Block(block) => self.generate_block(block),
            ExprKind::Call(expr) => self.generate_call_expression(expr),
            ExprKind::FieldExpression(expr) => self.generate_field_access_expression(expr),
            ExprKind::Path(path) => self.generate_path_expression(path),
            ExprKind::IndexExpression(expr) => self.generate_index_access_expression(expr),
            ExprKind::Let(_pattern, _expr) => todo!("Let expression not implemented yet."),
            ExprKind::Literal(literal) => self.generate_literal(literal),
            ExprKind::List(items) => self.generate_list_expression(items),
            ExprKind::Dict(kvs) => self.generate_dict_expression(kvs),
            ExprKind::UnaryOp(op, expr) => self.generate_unary_op(op, expr),
            ExprKind::BinaryOp(expr) => self.generate_binary_op(expr, parent_op),
            ExprKind::Match(expr) => self.generate_match_expression(expr),
            ExprKind::IfElse(expr) => self.generate_if_else(expr),
            ExprKind::FunctionExpression(decl) => self.generate_function_expression(decl),
            ExprKind::RecursiveCall(expr) => self.generate_recursive_call_expression(expr),
            ExprKind::Range(_) => todo!("Range expression not implemented yet."),
            ExprKind::Wildcard => {}
        }
    }

    fn generate_unary_op(&mut self, op: &UnaryOp, expr: &Expr) {
        match op {
            UnaryOp::Not => self.push_char('!'),
            UnaryOp::Minus => self.push_char('-'),
            UnaryOp::Spread => self.push_str("..."),
            _ => unimplemented!("PrefixOp {:?} not implemented yet.", op),
        }

        self.generate_expr(expr, None);
    }

    fn generate_field_access_expression(&mut self, field_access_expr: &FieldAccessExpression) {
        self.generate_expr(&field_access_expr.base, None);
        self.push_char('.');
        self.push_str(field_access_expr.field.as_str());
    }

    fn generate_index_access_expression(&mut self, index_access_expr: &IndexAccessExpression) {
        self.generate_expr(&index_access_expr.base, None);
        self.push_char('[');
        self.generate_expr(&index_access_expr.index, None);
        self.push_char(']');
    }

    fn generate_path_expression(&mut self, path: &Path) {
        let first_segment = path.segments.first().unwrap();

        self.generate_identifier(first_segment);
        self.push_str(
            &path.segments[1..]
                .iter()
                .fold("".to_string(), |acc, segment| {
                    acc + "." + segment.name.as_str()
                }),
        );
    }

    fn generate_list_expression(&mut self, items: &[Expr]) {
        self.push_char('[');
        for (i, item) in items.iter().enumerate() {
            if i > 0 {
                self.push_str(", ");
            }
            self.generate_expr(item, None);
        }
        self.push_char(']');
    }

    fn generate_dict_expression(&mut self, kvs: &[(Expr, Expr)]) {
        self.push_str("{\n");
        self.indent_level += 1;
        for (i, (key, value)) in kvs.iter().enumerate() {
            if i > 0 {
                self.push_str(",\n");
            }
            self.push_indent();
            self.generate_expr(key, None);

            if key != value {
                self.push_str(": ");
                self.generate_expr(value, None);
            }
        }
        self.push_str(",\n");
        self.indent_level -= 1;
        self.push_indent();
        self.push_char('}');
    }

    pub(crate) fn generate_pat(&mut self, pattern: &Pattern) {
        match &pattern.kind {
            PatternKind::Identifier(ident_pattern) => {
                let var_name = self
                    .current_scope()
                    .declare_variable(ident_pattern.name.as_str());
                self.push_str(&var_name);
                self.current_scope()
                    .declare_variable_alias(ident_pattern.name.as_str(), &var_name);
            }
            PatternKind::_Self => {
                self.current_scope().declare_variable_alias("self", "this");
            }
            PatternKind::Enum(enum_pattern) => self.generate_enum_extraction(enum_pattern),
            PatternKind::Literal(literal) => self.generate_literal(literal),
            PatternKind::List(patterns) => {
                self.push_char('[');
                for (i, pattern) in patterns.iter().enumerate() {
                    if i > 0 {
                        self.push_str(", ");
                    }
                    self.generate_pat(pattern);
                }
                self.push_char(']');
            }
            PatternKind::Rest(pattern) => {
                self.push_str("...");
                self.generate_pat(pattern);
            }
            PatternKind::Wildcard | PatternKind::None => {}
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

    fn generate_variable_declaration(&mut self, pattern: &Pattern, value: &Expr) {
        match &pattern.kind {
            PatternKind::Identifier(ident_pattern) => {
                self.generate_variable_declaration_identifier(ident_pattern.name.as_str(), value);
            }
            PatternKind::List(patterns) => {
                self.generate_variable_declaration_list_pattern(patterns, value);
            }
            _ => todo!("Variable declaration pattern matching is not implemented yet."),
        }
    }

    fn generate_variable_declaration_identifier(&mut self, name: &str, value: &Expr) {
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

    fn generate_variable_declaration_list_pattern(&mut self, patterns: &[Pattern], value: &Expr) {
        self.push_indent();
        self.push_str("let [");

        let mut bindings = vec![];

        for (i, pattern) in patterns.iter().enumerate() {
            if i > 0 {
                self.push_str(", ");
            }
            match &pattern.kind {
                PatternKind::Identifier(ident_pattern) => {
                    let shadowed_name = self
                        .current_scope()
                        .resolve_variable(ident_pattern.name.as_str());
                    let var_name = self
                        .current_scope()
                        .declare_variable(ident_pattern.name.as_str());
                    self.push_str(&var_name);
                    if let Some(shadowed_name) = shadowed_name {
                        self.current_scope()
                            .declare_variable_alias(ident_pattern.name.as_str(), &shadowed_name);
                    }
                    bindings.push((ident_pattern, var_name));
                }
                PatternKind::Rest(pattern) => {
                    self.push_str("...");
                    self.generate_pat(pattern);
                }
                PatternKind::Wildcard => {}
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

    fn generate_if_else(&mut self, if_else_expr: &IfElseExpression) {
        let mut lhs = String::new();
        // TODO: Potentially in a return position or other expression, before we generate the if
        //       statement, replace the current statement buffer with a new one, generate the if
        //       statement, and then swap the statement buffer back.
        //       Similar to how we generate blocks in expression position.
        // TODO: Find a way to do this generically for all expressions which are represented as
        //       statements in JavaScript.
        // TODO: In case of recursive calls in tail position, we'll want to omit lhs.
        let has_block_completions = self.current_context() == BlockContext::Expression
            && match &if_else_expr.then_branch.kind {
                ExprKind::Block(block) => block.has_completion(),
                _ => false,
            };
        if has_block_completions {
            lhs = self.replace_statement_buffer_with_empty_string();
            let completion_tmp_var = self.scopes.declare_tmp_variable();
            self.push_let_declaration(&completion_tmp_var);
            self.completion_variables.push(Some(completion_tmp_var));
        } else {
            self.completion_variables.push(None);
        }
        self.push_str("if (");
        let indent_level = self.indent_level;
        self.indent_level = 0;
        self.generate_expr(&if_else_expr.condition, None);
        self.indent_level = indent_level;
        self.push_str(") {\n");
        self.indent_level += 1;
        self.flush_statement_buffer();
        self.generate_expr(&if_else_expr.then_branch, None);
        self.indent_level -= 1;

        for else_branch in &if_else_expr.else_branches {
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
            self.generate_expr(&else_branch.consequence, None);
            self.indent_level -= 1;
        }

        self.push_indent();
        self.push_char('}');
        if has_block_completions {
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

    fn generate_enum_extraction(&mut self, _enum_pattern: &EnumPattern) {
        todo!("enum extraction outside of function parameters is not implemented yet.")
    }

    fn generate_partial_application(&mut self, call_expr: &CallExpression, wildcard_count: usize) {
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

            if let ExprKind::Wildcard = arg.kind {
                self.push_str(&placeholders[wildcard_index]);
                wildcard_index += 1;
            } else {
                self.generate_expr(arg, None);
            }
        }

        self.push_char(')');
    }

    pub(crate) fn generate_call_expression(&mut self, call_expr: &CallExpression) {
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
