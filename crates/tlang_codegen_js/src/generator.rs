use crate::{
    binary_operator_generator::generate_binary_op,
    function_generator::{
        fn_identifier_to_string, generate_function_declaration, generate_function_declarations,
        generate_function_expression, generate_return_statement,
    },
    pattern_match_generator::generate_match_expression,
    scope::Scope,
};
use tlang_ast::{
    node::{
        BinaryOpKind, Block, EnumDeclaration, Expr, ExprKind, FunctionParameter, Ident, Module,
        Pattern, PatternKind, Stmt, StmtKind, UnaryOp,
    },
    token::{Literal, Token, TokenKind},
};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BlockContext {
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
pub struct FunctionContext {
    // The name of the current function we're in.
    // Empty string for anonymous functions.
    pub name: String,

    // The parameters of the current function we're in.
    pub params: Vec<String>,

    // Is the current function body tail recursive?
    // This is used to determine if we should unwrap the recursion into a while loop.
    pub is_tail_recursive: bool,

    // Should remap to rest args?
    pub remap_to_rest_args: bool,
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
            statement_buffer: vec![String::new()],
            completion_variables: vec![None],
        }
    }

    pub fn get_standard_library_source() -> String {
        include_str!("../std/lib.tl").to_string()
    }

    pub fn push_statement_buffer(&mut self) {
        self.statement_buffer.push(String::new());
    }

    pub fn replace_statement_buffer(&mut self, buffer: String) -> String {
        let buf = self.statement_buffer.pop();
        self.statement_buffer.push(buffer);
        buf.unwrap()
    }

    pub fn pop_statement_buffer(&mut self) -> String {
        assert!(
            self.statement_buffer.len() != 1,
            "Cannot pop last statement buffer."
        );

        self.statement_buffer.pop().unwrap()
    }

    #[inline(always)]
    pub fn push_str(&mut self, str: &str) {
        self.statement_buffer.last_mut().unwrap().push_str(str);
    }

    #[inline(always)]
    pub fn push_char(&mut self, char: char) {
        self.statement_buffer.last_mut().unwrap().push(char);
    }

    #[inline(always)]
    pub fn push_indent(&mut self) {
        self.push_str(&self.get_indent());
    }

    #[inline(always)]
    pub fn inc_indent(&mut self) {
        self.indent_level += 1;
    }

    #[inline(always)]
    pub fn dec_indent(&mut self) {
        self.indent_level -= 1;
    }

    #[inline(always)]
    pub fn get_indent_level(&self) -> usize {
        self.indent_level
    }

    #[inline(always)]
    pub fn set_indent_level(&mut self, level: usize) {
        self.indent_level = level;
    }

    #[inline(always)]
    pub fn flush_statement_buffer(&mut self) {
        self.output.push_str(self.statement_buffer.last().unwrap());
        self.statement_buffer.last_mut().unwrap().clear();
    }

    #[inline(always)]
    pub fn current_scope(&mut self) -> &mut Scope {
        &mut self.scopes
    }

    #[inline(always)]
    pub fn push_scope(&mut self) {
        self.scopes = Scope::new(Some(Box::new(self.scopes.clone())));
    }

    #[inline(always)]
    pub fn pop_scope(&mut self) {
        if let Some(parent) = self.scopes.get_parent() {
            self.scopes = parent.clone();
        }
    }

    pub fn current_context(&self) -> BlockContext {
        *self.context_stack.last().unwrap()
    }

    pub fn push_context(&mut self, context: BlockContext) {
        self.context_stack.push(context);
    }

    pub fn pop_context(&mut self) {
        self.context_stack.pop();
    }

    pub fn generate_code(&mut self, module: &Module) {
        self.generate_statements(&module.statements);
    }

    pub fn declare_function_pre_body_variable(&mut self, name: &str, value: &str) {
        self.function_pre_body_declarations
            .push((name.to_string(), value.to_string()));
    }

    pub fn consume_function_pre_body_declarations(&mut self) -> Vec<(String, String)> {
        std::mem::take(&mut self.function_pre_body_declarations)
    }

    pub fn get_function_context(&self) -> Option<&FunctionContext> {
        self.function_context_stack.last()
    }

    pub fn push_function_context(
        &mut self,
        name: &str,
        parameters: &[FunctionParameter],
        is_tail_recursive: bool,
        remap_to_rest_args: bool,
    ) {
        self.function_context_stack.push(FunctionContext {
            name: name.to_string(),
            params: parameters
                .iter()
                .map(|param| {
                    if let PatternKind::Identifier { ref name, .. } = param.pattern.kind {
                        name.to_string()
                    } else {
                        // Encountered destructuring/pattern matching or wildcard, declare
                        // tmp variable to potentially dereference from. Unused at the moment.
                        self.scopes.declare_tmp_variable()
                    }
                })
                .collect(),
            is_tail_recursive,
            remap_to_rest_args,
        });
    }

    pub fn pop_function_context(&mut self) {
        self.function_context_stack.pop();
    }

    #[inline(always)]
    pub fn push_completion_variable(&mut self, name: Option<String>) {
        self.completion_variables.push(name);
    }

    #[inline(always)]
    pub fn pop_completion_variable(&mut self) {
        self.completion_variables.pop();
    }

    #[inline(always)]
    pub fn current_completion_variable(&self) -> Option<String> {
        self.completion_variables.last().unwrap().clone()
    }

    #[inline(always)]
    pub fn current_completion_variable_count(&self) -> usize {
        self.completion_variables.len()
    }

    #[inline(always)]
    pub fn nth_completion_variable(&self, index: usize) -> Option<&String> {
        self.completion_variables.get(index).unwrap().as_ref()
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
            self.replace_statement_buffer(String::new())
        };

        if block.has_completion() {
            if has_completion_var {
                // Halp I made a mess
            } else {
                self.push_indent();
                self.push_str(&format!("let {completion_tmp_var};{{\n"));
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
        self.push_str(&format!("{completion_tmp_var} = "));
        // TODO: Remove clone call
        self.generate_expr(&block.expression.clone().unwrap(), None);
        self.push_str(";\n");
        if !has_completion_var {
            self.indent_level -= 1;
            self.push_str(&self.get_indent());
            self.push_str("};\n");
            self.flush_statement_buffer();
            self.push_str(&lhs);
            self.push_str(completion_tmp_var.as_str());
        }
        self.flush_statement_buffer();
        self.pop_scope();
    }

    pub fn generate_block(&mut self, block: &Block) {
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
                    // TODO: Get rid of clone call
                    let expression = block.expression.clone().unwrap();
                    if let ExprKind::RecursiveCall(_) = expression.kind {
                        self.generate_expr(&expression, None);
                        return;
                    }
                }
            }

            self.push_str(&self.get_indent());
            self.push_str("return ");
            self.push_context(BlockContext::Expression);
            self.generate_expr(&block.expression.clone().unwrap(), None);
            self.pop_context();
            self.push_str(";\n");
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

    pub fn generate_comment(&mut self, comment: &Token) {
        match &comment.kind {
            TokenKind::SingleLineComment(comment) => {
                self.push_str(&format!("//{comment}\n"));
            }
            TokenKind::MultiLineComment(comment) => {
                self.push_str(&format!("/*{comment}*/\n"));
            }
            _ => {}
        }
    }

    pub fn generate_stmt(&mut self, statement: &Stmt) {
        for comment in &statement.leading_comments {
            self.generate_comment(comment);
        }

        match &statement.kind {
            StmtKind::None => {}
            StmtKind::Expr(expr) => {
                self.push_str(&self.get_indent());
                self.context_stack.push(BlockContext::Statement);
                self.generate_expr(expr, None);
                self.context_stack.pop();

                if let ExprKind::IfElse { .. } = expr.kind {
                    self.push_char('\n');
                    return;
                }

                self.push_str(";\n");
            }
            StmtKind::Let {
                pattern,
                expression,
                ..
            } => {
                self.generate_variable_declaration(pattern, expression);
            }
            StmtKind::FunctionDeclaration(decl) => generate_function_declaration(self, decl),
            StmtKind::FunctionDeclarations(decls) => {
                generate_function_declarations(self, decls, &statement.leading_comments)
            }
            StmtKind::Return(expr) => generate_return_statement(self, expr),
            StmtKind::EnumDeclaration(decl) => self.generate_enum_declaration(decl),
        }

        for comment in &statement.trailing_comments {
            self.generate_comment(comment);
        }
    }

    pub fn generate_expr(&mut self, expr: &Expr, parent_op: Option<&BinaryOpKind>) {
        match &expr.kind {
            ExprKind::None => {}
            ExprKind::Block(block) if self.current_context() == BlockContext::Expression => {
                self.generate_block_expression(block);
            }
            ExprKind::Block(block) => {
                self.generate_block(block);
            }
            ExprKind::Call {
                function,
                arguments,
            } => self.generate_call_expression(function, arguments),
            ExprKind::FieldExpression { base, field } => {
                self.generate_expr(base, None);
                self.push_char('.');
                self.push_str(&field.to_string());
            }
            ExprKind::Path(path) => {
                if path.segments.len() == 1 {
                    self.generate_identifier(path.segments.first().unwrap());
                } else {
                    self.push_str(
                        &path
                            .segments
                            .iter()
                            .map(|ident| ident.to_string())
                            .collect::<Vec<_>>()
                            .join("."),
                    );
                }
            }
            ExprKind::IndexExpression { base, index } => {
                self.generate_expr(base, None);
                self.push_char('[');
                self.generate_expr(index, None);
                self.push_char(']');
            }
            ExprKind::Let(_pattern, _expr) => todo!("Let expression not implemented yet."),
            ExprKind::Literal(literal) => self.generate_literal(literal),
            ExprKind::List(items) => {
                self.push_char('[');
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        self.push_str(", ");
                    }
                    self.generate_expr(item, None);
                }
                self.push_char(']');
            }
            ExprKind::Dict(kvs) => {
                self.push_str("{\n");
                self.indent_level += 1;
                for (i, (key, value)) in kvs.iter().enumerate() {
                    if i > 0 {
                        self.push_str(",\n");
                    }
                    self.push_str(&self.get_indent());
                    self.generate_expr(key, None);
                    self.push_str(": ");
                    self.generate_expr(value, None);
                }
                self.push_str(",\n");
                self.indent_level -= 1;
                self.push_str(&self.get_indent());
                self.push_char('}');
            }
            ExprKind::UnaryOp(op, expr) => {
                match op {
                    UnaryOp::Minus => self.push_char('-'),
                    UnaryOp::Not => self.push_char('!'),
                    UnaryOp::Spread => self.push_str("..."),
                    _ => unimplemented!("PrefixOp {:?} not implemented yet.", op),
                }
                self.generate_expr(expr, None);
            }
            ExprKind::BinaryOp { op, lhs, rhs } => {
                generate_binary_op(self, op, lhs, rhs, parent_op);
            }
            ExprKind::Match { expression, arms } => {
                generate_match_expression(self, expression, arms);
            }
            ExprKind::IfElse {
                condition,
                then_branch,
                else_branch,
            } => {
                self.generate_if_else(condition, then_branch, else_branch);
            }
            ExprKind::FunctionExpression(decl) => {
                generate_function_expression(self, decl);
            }
            ExprKind::RecursiveCall(expr) => {
                self.generate_recursive_call_expression(expr, parent_op);
            }
            ExprKind::Range {
                start: _,
                end: _,
                inclusive: _,
            } => todo!("Range expression not implemented yet."),
            ExprKind::Wildcard => {}
        }
    }

    pub fn generate_pat(&mut self, pattern: &Pattern) {
        match &pattern.kind {
            PatternKind::Identifier { name, .. } => {
                let var_name = self.current_scope().declare_variable(&name.to_string());
                self.push_str(&var_name);
                self.current_scope()
                    .declare_variable_alias(&name.to_string(), &var_name);
            }
            PatternKind::Enum {
                identifier,
                elements,
                named_fields,
            } => self.generate_enum_extraction(identifier, elements, *named_fields),
            PatternKind::Literal(expr) => self.generate_expr(expr, None),
            PatternKind::List(patterns) => {
                self.push_str("[");
                for (i, pattern) in patterns.iter().enumerate() {
                    if i > 0 {
                        self.push_str(", ");
                    }
                    self.generate_pat(pattern);
                }
                self.push_str("]");
            }
            PatternKind::Rest(pattern) => {
                self.push_str("...");
                self.generate_pat(pattern);
            }
            PatternKind::Wildcard => {}
        }
    }

    fn generate_identifier(&mut self, name: &Ident) {
        let name_string = name.to_string();
        let identifier = &self
            .current_scope()
            .resolve_variable(&name_string)
            .unwrap_or(name_string);
        self.push_str(identifier);
    }

    fn generate_variable_declaration(&mut self, pattern: &Pattern, value: &Expr) {
        match &pattern.kind {
            PatternKind::Identifier { name, .. } => {
                self.generate_variable_declaration_identifier(&name.to_string(), value);
            }
            PatternKind::List(patterns) => {
                self.generate_variable_declaration_list_pattern(patterns, value);
            }
            _ => todo!("Variable declaration pattern matching is not implemented yet."),
        }
    }

    fn generate_variable_declaration_identifier(&mut self, name: &str, value: &Expr) {
        self.push_str(&self.get_indent());
        let shadowed_name = self.current_scope().resolve_variable(name);
        let var_name = self.current_scope().declare_variable(name);
        self.push_str(&format!("let {var_name} = "));
        self.context_stack.push(BlockContext::Expression);
        if let Some(shadowed_name) = shadowed_name {
            self.current_scope()
                .declare_variable_alias(name, &shadowed_name);
        }
        self.generate_expr(value, None);
        self.current_scope().declare_variable_alias(name, &var_name);
        self.context_stack.pop();
        self.push_str(";\n");
    }

    fn generate_variable_declaration_list_pattern(&mut self, patterns: &[Pattern], value: &Expr) {
        self.push_str(&self.get_indent());
        self.push_str("let [");

        let mut bindings = vec![];

        for (i, pattern) in patterns.iter().enumerate() {
            if i > 0 {
                self.push_str(", ");
            }
            match &pattern.kind {
                PatternKind::Identifier { name, .. } => {
                    let shadowed_name = self.current_scope().resolve_variable(&name.to_string());
                    let var_name = self.current_scope().declare_variable(&name.to_string());
                    self.push_str(&var_name);
                    if let Some(shadowed_name) = shadowed_name {
                        self.current_scope()
                            .declare_variable_alias(&name.to_string(), &shadowed_name);
                    }
                    bindings.push((name, var_name));
                }
                _ => todo!("Variable declaration pattern matching is not implemented yet."),
            }
        }

        self.push_str("] = ");
        self.context_stack.push(BlockContext::Expression);
        self.generate_expr(value, None);

        for (name, var_name) in bindings {
            self.current_scope()
                .declare_variable_alias(&name.to_string(), &var_name);
        }

        self.context_stack.pop();
        self.push_str(";\n");
    }

    fn generate_if_else(
        &mut self,
        condition: &Expr,
        then_branch: &Expr,
        else_branch: &Option<Expr>,
    ) {
        let mut lhs = String::new();
        // TODO: Potentially in a return position or other expression, before we generate the if
        //       statement, replace the current statement buffer with a new one, generate the if
        //       statement, and then swap the statement buffer back.
        //       Similar to how we generate blocks in expression position.
        // TODO: Find a way to do this generically for all expressions which are represented as
        //       statements in JavaScript.
        // TODO: In case of recursive calls in tail position, we'll want to omit lhs.
        let has_block_completions = self.current_context() == BlockContext::Expression
            && match &then_branch.kind {
                ExprKind::Block(block) => block.has_completion(),
                _ => false,
            };
        if has_block_completions {
            lhs = self.replace_statement_buffer(String::new());
            let completion_tmp_var = self.scopes.declare_tmp_variable();
            self.push_indent();
            self.push_str(&format!("let {completion_tmp_var};"));
            self.completion_variables.push(Some(completion_tmp_var));
        } else {
            self.completion_variables.push(None);
        }
        self.push_str("if (");
        let indent_level = self.indent_level;
        self.indent_level = 0;
        self.generate_expr(condition, None);
        self.indent_level = indent_level;
        self.push_str(") {\n");
        self.indent_level += 1;
        self.flush_statement_buffer();
        self.generate_expr(then_branch, None);
        self.indent_level -= 1;

        if let Some(else_branch) = else_branch {
            self.push_str(&self.get_indent());
            self.push_str("} else {\n");
            self.indent_level += 1;
            self.flush_statement_buffer();
            self.generate_expr(else_branch, None);
            self.indent_level -= 1;
        }

        self.push_str(&self.get_indent());
        self.push_char('}');
        if has_block_completions {
            self.push_str("\n");
            // If we have an lhs, put the completion var as the rhs of the lhs.
            // Otherwise, we assign the completion_var to the previous completion_var.
            if !lhs.is_empty() {
                self.push_str(&lhs);
                let completion_var = self.completion_variables.last().unwrap().clone().unwrap();
                self.push_str(&completion_var);
            } else {
                self.push_str(&self.get_indent());
                let completion_var = self.completion_variables.last().unwrap().clone().unwrap();
                let prev_completion_var = self.completion_variables
                    [self.completion_variables.len() - 2]
                    .clone()
                    .unwrap();
                self.push_str(&format!("{prev_completion_var} = {completion_var};\n"));
            }
        }
        self.completion_variables.pop();
    }

    fn generate_enum_declaration(&mut self, decl: &EnumDeclaration) {
        self.push_str(&self.get_indent());
        self.push_str(&format!("const {} = {{\n", decl.name));
        self.indent_level += 1;
        for variant in &decl.variants {
            self.generate_enum_variant(&variant.name, variant.named_fields, &variant.parameters);
        }
        self.indent_level -= 1;
        self.push_str(&self.get_indent());
        self.push_str("};\n");
    }

    fn generate_enum_variant(&mut self, name: &Ident, named_fields: bool, parameters: &[Ident]) {
        self.push_str(&self.get_indent());

        if parameters.is_empty() {
            self.push_str(&format!("{name}: {{ tag: \"{name}\" }},\n"));
            return;
        }

        self.push_str(&format!("{name}("));
        if named_fields {
            self.push_str("{ ");
        }
        for (i, param) in parameters.iter().enumerate() {
            if i > 0 {
                self.push_str(", ");
            }
            self.push_str(&param.to_string());
        }
        if named_fields {
            self.push_str(" }");
        }
        self.push_str(") {\n");
        self.indent_level += 1;
        self.push_str(&self.get_indent());
        self.push_str("return {\n");
        self.indent_level += 1;
        self.push_str(&self.get_indent());
        self.push_str(format!("tag: \"{name}\",\n").as_str());
        for (i, param) in parameters.iter().enumerate() {
            self.push_str(&self.get_indent());
            if named_fields {
                self.push_str(&param.to_string());
            } else {
                self.push_str(&format!("[{i}]: "));
                self.push_str(&param.to_string());
            }
            self.push_str(",\n");
        }
        self.indent_level -= 1;
        self.push_str(&self.get_indent());
        self.push_str("};\n");
        self.indent_level -= 1;
        self.push_str(&self.get_indent());
        self.push_str("},\n");
    }

    fn generate_enum_extraction(
        &mut self,
        _identifier: &Expr,
        _elements: &[Pattern],
        _named_fields: bool,
    ) {
        todo!("enum extraction outside of function parameters is not implemented yet.")
    }

    fn generate_call_expression(&mut self, function: &Expr, arguments: &[Expr]) {
        let has_wildcards = arguments
            .iter()
            .any(|arg| matches!(arg.kind, ExprKind::Wildcard));

        if has_wildcards {
            self.push_str("function(...args) {\n");
            self.indent_level += 1;
            self.push_str(&self.get_indent());
            self.push_str("return ");
            self.generate_expr(function, None);
            self.push_char('(');

            let mut wildcard_index = 0;
            for (i, arg) in arguments.iter().enumerate() {
                if i > 0 {
                    self.push_str(", ");
                }

                if let ExprKind::Wildcard = arg.kind {
                    self.push_str(format!("args[{wildcard_index}]").as_str());
                    wildcard_index += 1;
                } else {
                    self.generate_expr(arg, None);
                }
            }

            self.push_str(");\n");
            self.indent_level -= 1;
            self.push_str(&self.get_indent());
            self.push_char('}');
            return;
        }

        self.generate_expr(function, None);
        self.push_char('(');

        for (i, arg) in arguments.iter().enumerate() {
            if i > 0 {
                self.push_str(", ");
            }
            self.generate_expr(arg, None);
        }
        self.push_char(')');
    }

    fn generate_recursive_call_expression(
        &mut self,
        node: &Expr,
        parent_op: Option<&BinaryOpKind>,
    ) {
        // If call expression is referencing the current function, all we do is update the arguments,
        // as we are in a while loop.
        if let ExprKind::Call {
            function,
            arguments,
        } = &node.kind
        {
            if let ExprKind::Path(_) = &function.kind {
                if let Some(function_context) = self.function_context_stack.last() {
                    if function_context.is_tail_recursive
                        // TODO: Comparing identifier by string might not be the best idea.
                        && function_context.name == fn_identifier_to_string(function)
                    {
                        let params = function_context.params.clone();
                        let remap_to_rest_args = function_context.remap_to_rest_args;
                        let tmp_vars = params
                            .iter()
                            .map(|_| self.scopes.declare_tmp_variable())
                            .collect::<Vec<_>>();

                        for (i, arg) in arguments.iter().enumerate() {
                            self.push_str(&self.get_indent());
                            self.push_str(&format!("let {} = ", tmp_vars[i]));
                            self.generate_expr(arg, None);
                            self.push_str(";\n");
                        }
                        if remap_to_rest_args {
                            for (i, _arg_name) in params.iter().enumerate() {
                                self.push_str(&self.get_indent());
                                self.push_str(&format!("args[{}] = {};\n", i, tmp_vars[i]));
                            }
                        } else {
                            for (i, arg_name) in params.iter().enumerate() {
                                self.push_str(&self.get_indent());
                                self.push_str(&format!("{} = {};\n", arg_name, tmp_vars[i]));
                            }
                        }
                        return;
                    }
                }
            }
        }

        // For any other referenced function, we do a normal call expression.
        self.generate_expr(node, parent_op);
    }

    fn get_indent(&self) -> String {
        "    ".repeat(self.indent_level)
    }

    pub fn get_output(&self) -> &str {
        &self.output
    }
}
