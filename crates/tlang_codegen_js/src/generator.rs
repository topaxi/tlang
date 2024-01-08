use crate::{
    binary_operator_generator::generate_binary_op,
    function_generator::{
        generate_function_declaration, generate_function_declarations,
        generate_function_expression, generate_function_parameter, generate_return_statement,
    },
    scope::Scope,
};
use tlang_ast::{
    node::{AstNode, BinaryOp, Node, PrefixOp},
    token::Literal,
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

    pub fn push_statement_buffer(&mut self) {
        self.statement_buffer.push(String::new());
    }

    pub fn replace_statement_buffer(&mut self, buffer: String) -> String {
        let buf = self.statement_buffer.pop();
        self.statement_buffer.push(buffer);
        buf.unwrap()
    }

    pub fn pop_statement_buffer(&mut self) -> String {
        if self.statement_buffer.len() == 1 {
            panic!("Cannot pop last statement buffer.");
        }

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
    pub fn flush_statement_buffer(&mut self) {
        println!(
            "Flushing statement buffer: {}",
            self.statement_buffer.last().unwrap()
        );
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

    pub fn generate_code(&mut self, node: &Node) {
        self.generate_node(node, None)
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
        parameters: &[Node],
        is_tail_recursive: bool,
        remap_to_rest_args: bool,
    ) {
        self.function_context_stack.push(FunctionContext {
            name: name.to_string(),
            params: parameters
                .iter()
                .map(|param| {
                    if let AstNode::FunctionParameter {
                        id: _,
                        node,
                        type_annotation: _,
                    } = &param.ast_node
                    {
                        if let AstNode::Identifier(ref name) = node.ast_node {
                            name.clone()
                        } else {
                            // Encountered destructuring/pattern matching or wildcard, declare
                            // tmp variable to potentially dereference from. Unused at the moment.
                            self.scopes.declare_tmp_variable()
                        }
                    } else {
                        panic!("Expected FunctionParameter, got {:?}", param);
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
                self.push_str(&format!("\"{}\"", value));
            }
        }
    }

    fn generate_comment(&mut self, ast_node: &AstNode) {
        match ast_node {
            AstNode::SingleLineComment(comment) => {
                self.push_str("//");
                self.push_str(comment);
                self.push_char('\n');
            }
            AstNode::MultiLineComment(comment) => {
                self.push_str("/*");
                self.push_str(comment);
                self.push_str("*/\n");
            }
            _ => unreachable!(),
        }
    }

    /// Generates blocks in expression position.
    fn generate_block_expression(&mut self, statements: &[Node], expression: &Option<Box<Node>>) {
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

        println!("Generating block expression");
        println!("lhs: {}", lhs);
        println!("Current output: {}", self.output);
        println!(
            "Current statement buffer: {}",
            self.statement_buffer.last().unwrap()
        );

        if expression.is_some() {
            if has_completion_var {
                // Halp I made a mess
            } else {
                self.push_indent();
                self.push_str(&format!("let {};{{\n", completion_tmp_var));
                self.indent_level += 1;
            }
        }

        self.generate_statements(statements);

        if expression.is_none() {
            self.push_statement_buffer();
            self.push_str(&lhs);
            self.flush_statement_buffer();
            self.pop_statement_buffer();
            self.flush_statement_buffer();
            self.pop_scope();
            return;
        }

        println!("Current output: {}", self.output);
        println!(
            "Current statement buffer: {}",
            self.statement_buffer.last().unwrap()
        );

        self.push_indent();
        self.push_str(&format!("{} = ", completion_tmp_var));
        self.generate_node(expression.as_ref().unwrap(), None);
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

    fn generate_block(&mut self, statements: &[Node], expression: &Option<Box<Node>>) {
        self.push_scope();

        self.generate_statements(statements);

        if expression.is_none() {
            self.pop_scope();
            return;
        }

        if let BlockContext::FunctionBody = self.current_context() {
            // We only render the return statement if we are not in a tail recursive function body
            // and the node is RecursiveCall pointing to the current function.
            if let Some(function_context) = self.function_context_stack.last() {
                if function_context.is_tail_recursive && expression.is_some() {
                    if let AstNode::RecursiveCall(_) = expression.as_ref().unwrap().ast_node {
                        self.generate_node(expression.as_ref().unwrap(), None);
                        return;
                    }
                }
            }

            self.push_str(&self.get_indent());
            self.push_str("return ");
            self.push_context(BlockContext::Expression);
            self.generate_node(expression.as_ref().unwrap(), None);
            self.pop_context();
            self.push_str(";\n");
            self.flush_statement_buffer();
        }

        self.pop_scope();
    }

    #[inline(always)]
    fn generate_statements(&mut self, statements: &[Node]) {
        for statement in statements {
            self.generate_node(statement, None);
            self.flush_statement_buffer();
        }
    }

    pub fn generate_node(&mut self, node: &Node, parent_op: Option<&BinaryOp>) {
        // TODO: Split into generate_statement and generate_expression.
        //       This should also help setting proper block contexts.
        match &node.ast_node {
            AstNode::SingleLineComment(_) | AstNode::MultiLineComment(_) => {
                self.generate_comment(&node.ast_node);
            }
            AstNode::Program(statements) => {
                self.generate_statements(statements);
                self.flush_statement_buffer();
            }
            AstNode::Block(statements, expression) if self.current_context() == BlockContext::Expression => {
                self.generate_block_expression(statements, expression);
            }
            AstNode::Block(statements, expression) => {
                self.generate_block(statements, expression);
            }
            AstNode::ExpressionStatement(expression) => {
                self.push_str(&self.get_indent());
                self.context_stack.push(BlockContext::Statement);
                self.generate_node(expression, None);
                self.context_stack.pop();

                if let AstNode::IfElse { .. } = expression.ast_node {
                    self.push_char('\n');
                    return;
                }

                self.push_str(";\n");
            }
            AstNode::Literal(literal) => self.generate_literal(literal),
            AstNode::List(items) => {
                self.push_char('[');
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        self.push_str(", ");
                    }
                    self.generate_node(item, None);
                }
                self.push_char(']');
            }
            AstNode::Dict(entries) => {
                self.push_str("{\n");
                self.indent_level += 1;
                for (i, (key, value)) in entries.iter().enumerate() {
                    if i > 0 {
                        self.push_str(",\n");
                    }
                    self.push_str(&self.get_indent());
                    self.generate_node(key, None);
                    self.push_str(": ");
                    self.generate_node(value, None);
                }
                self.push_str(",\n");
                self.indent_level -= 1;
                self.push_str(&self.get_indent());
                self.push_char('}');
            }
            AstNode::PrefixOp(op, node) => {
                match op {
                    PrefixOp::Spread => self.push_str("..."),
                    _ => unimplemented!("PrefixOp {:?} not implemented yet.", op),
                }
                self.generate_node(node, None);
            },
            AstNode::BinaryOp { op, lhs, rhs } => {
                generate_binary_op(self, op, lhs, rhs, parent_op);
            }
            AstNode::VariableDeclaration { id: _, pattern, expression , type_annotation: _} => {
                self.generate_variable_declaration(pattern, expression);
            }
            AstNode::Match {
                expression: _,
                arms: _,
            } => todo!(),
            AstNode::MatchArm {
                pattern: _,
                expression: _,
            } => todo!(),
            AstNode::Wildcard => unreachable!("Stray wildcard expression, you can only use _ wildcards in function declarations, pipelines and pattern matching."),
            AstNode::IfElse {
                condition,
                then_branch,
                else_branch,
            } => {
                self.generate_if_else(condition, then_branch, else_branch);
            }
            AstNode::FunctionParameter{ id: _, node, type_annotation: _ } => generate_function_parameter(self, node),
            AstNode::FunctionDeclaration { id: _, name, declaration } =>
                generate_function_declaration(self, name, declaration),
            AstNode::FunctionDeclarations { id: _, name, declarations } =>
                generate_function_declarations(self, name, declarations),
            AstNode::FunctionExpression { id: _, name, declaration } =>
                generate_function_expression(self, name, declaration),
            AstNode::ReturnStatement(expr) => generate_return_statement(self, expr),
            AstNode::Identifier(name) => self.generate_identifier(name),
            AstNode::NestedIdentifier(identifiers) => self.push_str(&identifiers.join(".")),
            AstNode::Call { function, arguments } =>
                self.generate_call_expression(function, arguments),
            AstNode::RecursiveCall(node) => {
                self.generate_recursive_call_expression(node, parent_op)
            },
            AstNode::FieldExpression { base, field } => {
                self.generate_node(base, None);
                self.push_str(".");
                self.generate_node(field, None);
            }
            AstNode::IndexExpression { base,  index } => {
                self.generate_node(base, None);
                self.push_str("[");
                self.generate_node(index, None);
                self.push_str("]");
            }
            AstNode::EnumDeclaration { id: _, name, variants } => self.generate_enum_declaration(name, variants),
            AstNode::EnumVariant { name, named_fields, parameters } => self.generate_enum_variant(name, *named_fields, parameters),
            AstNode::EnumExtraction { identifier, elements, named_fields } => self.generate_enum_extraction(identifier, elements, *named_fields),
            // Allow unreachable path, as we add new AST nodes, we do not want to automatically
            // start failing tests while we are still implementing the codegen.
            #[allow(unreachable_patterns)]
            _ => unimplemented!("{:?} not implemented yet.", node),
        }
    }

    fn generate_identifier(&mut self, name: &str) {
        let identifier = &self
            .current_scope()
            .resolve_variable(name)
            .unwrap_or(name.to_string());
        self.push_str(identifier);
    }

    fn generate_variable_declaration(&mut self, name: &Node, value: &Node) {
        let name = if let AstNode::Identifier(name) = &name.ast_node {
            name
        } else {
            todo!("Variable declaration pattern matching is not implemented yet.")
        };

        self.push_str(&self.get_indent());
        let name = self.scopes.declare_variable(name);
        self.push_str(&format!("let {} = ", name));
        self.context_stack.push(BlockContext::Expression);
        self.generate_node(value, None);
        self.context_stack.pop();
        self.push_str(";\n");
    }

    fn generate_if_else(
        &mut self,
        condition: &Node,
        then_branch: &Node,
        else_branch: &Option<Box<Node>>,
    ) {
        let mut lhs = String::new();
        // TODO: Potentially in a return position or other expression, before we generate the if
        //       statement, replace the current statement buffer with a new one, generate the if
        //       statement, and then swap the statement buffer back.
        //       Similar to how we generate blocks in expression position.
        // TODO: Find a way to do this generically for all expressions which are represented as
        //       statements in JavaScript.
        let has_block_completions = self.current_context() == BlockContext::Expression
            && match &then_branch.ast_node {
                AstNode::Block(_, expr) => expr.is_some(),
                _ => false,
            };
        if has_block_completions {
            lhs = self.replace_statement_buffer(String::new());
            let completion_tmp_var = self.scopes.declare_tmp_variable();
            self.push_indent();
            self.push_str(&format!("let {};", completion_tmp_var));
            self.completion_variables.push(Some(completion_tmp_var));
        }
        self.push_str("if (");
        let indent_level = self.indent_level;
        self.indent_level = 0;
        self.generate_node(condition, None);
        self.indent_level = indent_level;
        self.push_str(") {\n");
        self.indent_level += 1;
        self.generate_node(then_branch, None);
        self.indent_level -= 1;

        if let Some(else_branch) = else_branch {
            self.push_str(&self.get_indent());
            self.push_str("} else {\n");
            self.indent_level += 1;
            self.generate_node(else_branch, None);
            self.indent_level -= 1;
        }

        self.push_str(&self.get_indent());
        self.push_char('}');
        if has_block_completions {
            self.push_str("\n");
            self.push_str(&lhs);
            let completion_var = self.completion_variables.last().unwrap().clone().unwrap();
            self.push_str(&completion_var);
        }
    }

    fn generate_enum_declaration(&mut self, name: &str, variants: &[Node]) {
        self.push_str(&self.get_indent());
        self.push_str(&format!("const {} = {{\n", name));
        self.indent_level += 1;
        for variant in variants {
            self.generate_node(variant, None);
        }
        self.indent_level -= 1;
        self.push_str(&self.get_indent());
        self.push_str("};\n");
    }

    fn generate_enum_variant(&mut self, name: &str, named_fields: bool, parameters: &[Node]) {
        self.push_str(&self.get_indent());

        if parameters.is_empty() {
            self.push_str(&format!("{}: {{ tag: \"{}\" }},\n", name, name));
            return;
        }

        self.push_str(&format!("{}(", name));
        if named_fields {
            self.push_str("{ ");
        }
        for (i, param) in parameters.iter().enumerate() {
            if i > 0 {
                self.push_str(", ");
            }
            self.generate_node(param, None);
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
        self.push_str(format!("tag: \"{}\",\n", name).as_str());
        for (i, param) in parameters.iter().enumerate() {
            self.push_str(&self.get_indent());
            if named_fields {
                if let AstNode::Identifier(i) = &param.ast_node {
                    self.push_str(i);
                }
            } else {
                self.push_str(&format!("\"{}\": ", i));
                self.generate_node(param, None);
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
        _identifier: &Node,
        _elements: &[Node],
        _named_fields: bool,
    ) {
        todo!("enum extraction outside of function parameters is not implemented yet.")
    }

    fn generate_call_expression(&mut self, function: &Node, arguments: &[Node]) {
        let has_wildcards = arguments
            .iter()
            .any(|arg| matches!(arg.ast_node, AstNode::Wildcard));

        if has_wildcards {
            self.push_str("function(...args) {\n");
            self.indent_level += 1;
            self.push_str(&self.get_indent());
            self.push_str("return ");
            self.generate_node(function, None);
            self.push_char('(');

            let mut wildcard_index = 0;
            for (i, arg) in arguments.iter().enumerate() {
                if i > 0 {
                    self.push_str(", ");
                }

                if let AstNode::Wildcard = arg.ast_node {
                    self.push_str(format!("args[{}]", wildcard_index).as_str());
                    wildcard_index += 1;
                } else {
                    self.generate_node(arg, None);
                }
            }

            self.push_str(");\n");
            self.indent_level -= 1;
            self.push_str(&self.get_indent());
            self.push_char('}');
            return;
        }

        self.generate_node(function, None);
        self.push_char('(');

        for (i, arg) in arguments.iter().enumerate() {
            if i > 0 {
                self.push_str(", ");
            }
            self.generate_node(arg, None);
        }
        self.push_char(')');
    }

    fn generate_recursive_call_expression(&mut self, node: &Node, parent_op: Option<&BinaryOp>) {
        // If call expression is referencing the current function, all we do is update the arguments,
        // as we are in a while loop.
        if let AstNode::Call {
            function,
            arguments,
        } = &node.ast_node
        {
            if let AstNode::Identifier(name) = &function.ast_node {
                if let Some(function_context) = self.function_context_stack.last() {
                    if function_context.is_tail_recursive && &function_context.name == name {
                        let params = function_context.params.clone();
                        let remap_to_rest_args = function_context.remap_to_rest_args;
                        let tmp_vars = params
                            .iter()
                            .map(|_| self.scopes.declare_tmp_variable())
                            .collect::<Vec<_>>();

                        for (i, arg) in arguments.iter().enumerate() {
                            self.push_str(&self.get_indent());
                            self.push_str(&format!("let {} = ", tmp_vars[i]));
                            self.generate_node(arg, None);
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
        self.generate_node(node, parent_op)
    }

    fn get_indent(&self) -> String {
        "    ".repeat(self.indent_level)
    }

    pub fn get_output(&self) -> &str {
        &self.output
    }
}
