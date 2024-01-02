use crate::scope::Scope;
use tlang_ast::{
    node::{AstNode, BinaryOp, FunctionDeclaration, Node, PrefixOp},
    token::Literal,
};

#[derive(Debug, Clone, Copy, PartialEq)]
enum JSAssociativity {
    Left,
    Right,
}

#[derive(Debug, Clone, Copy)]
struct JSOperatorInfo {
    precedence: u8,
    associativity: JSAssociativity,
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum BlockContext {
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
struct FunctionContext {
    // The name of the current function we're in.
    // Empty string for anonymous functions.
    name: String,

    // The parameters of the current function we're in.
    params: Vec<String>,

    // Is the current function body tail recursive?
    // This is used to determine if we should unwrap the recursion into a while loop.
    is_tail_recursive: bool,

    // Should remap to rest args?
    remap_to_rest_args: bool,
}

#[derive(Debug, PartialEq)]
pub struct CodegenJS {
    output: String,
    indent_level: usize,
    scopes: Scope,
    context_stack: Vec<BlockContext>,
    function_context_stack: Vec<FunctionContext>,
    function_pre_body: String,
    current_statement_buffer: String,
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
            function_pre_body: String::new(),
            current_statement_buffer: String::new(),
        }
    }

    #[inline(always)]
    fn push_str(&mut self, str: &str) {
        self.current_statement_buffer.push_str(str);
    }

    #[inline(always)]
    fn push_char(&mut self, char: char) {
        self.current_statement_buffer.push(char);
    }

    fn flush_statement_buffer(&mut self) {
        self.output.push_str(&self.current_statement_buffer);
        self.current_statement_buffer.clear();
    }

    fn current_scope(&self) -> &Scope {
        &self.scopes
    }

    fn push_scope(&mut self) {
        self.scopes = Scope::new(Some(Box::new(self.scopes.clone())));
    }

    fn pop_scope(&mut self) {
        if let Some(parent) = self.scopes.get_parent() {
            self.scopes = parent.clone();
        }
    }

    fn current_context(&self) -> BlockContext {
        *self.context_stack.last().unwrap()
    }

    pub fn generate_code(&mut self, node: &Node) {
        self.generate_node(node, None)
    }

    fn generate_binary_op(&mut self, op: &BinaryOp) {
        match op {
            BinaryOp::Add => self.push_str(" + "),
            BinaryOp::Subtract => self.push_str(" - "),
            BinaryOp::Multiply => self.push_str(" * "),
            BinaryOp::Divide => self.push_str(" / "),
            BinaryOp::Modulo => self.push_str(" % "),
            BinaryOp::Exponentiation => self.push_str(" ** "),
            BinaryOp::Equal => self.push_str(" === "),
            BinaryOp::NotEqual => self.push_str(" !== "),
            BinaryOp::LessThan => self.push_str(" < "),
            BinaryOp::LessThanOrEqual => self.push_str(" <= "),
            BinaryOp::GreaterThan => self.push_str(" > "),
            BinaryOp::GreaterThanOrEqual => self.push_str(" >= "),
            BinaryOp::And => self.push_str(" && "),
            BinaryOp::Or => self.push_str(" || "),
            BinaryOp::BitwiseOr => self.push_str(" | "),
            BinaryOp::BitwiseAnd => self.push_str(" & "),
            BinaryOp::BitwiseXor => self.push_str(" ^ "),
            BinaryOp::Pipeline => unimplemented!("Pipeline does not neatly map to a JS operator."),
        }
    }

    fn map_operator_info(op: &BinaryOp) -> JSOperatorInfo {
        match op {
            BinaryOp::Add | BinaryOp::Subtract => JSOperatorInfo {
                precedence: 6,
                associativity: JSAssociativity::Left,
            },
            BinaryOp::Multiply | BinaryOp::Divide | BinaryOp::Modulo => JSOperatorInfo {
                precedence: 7,
                associativity: JSAssociativity::Left,
            },
            BinaryOp::Equal
            | BinaryOp::NotEqual
            | BinaryOp::LessThan
            | BinaryOp::LessThanOrEqual
            | BinaryOp::GreaterThan
            | BinaryOp::GreaterThanOrEqual => JSOperatorInfo {
                precedence: 5,
                associativity: JSAssociativity::Left,
            },
            BinaryOp::And => JSOperatorInfo {
                precedence: 3,
                associativity: JSAssociativity::Left,
            },
            BinaryOp::Or => JSOperatorInfo {
                precedence: 2,
                associativity: JSAssociativity::Left,
            },
            BinaryOp::BitwiseAnd | BinaryOp::BitwiseOr | BinaryOp::BitwiseXor => JSOperatorInfo {
                precedence: 8,
                associativity: JSAssociativity::Left,
            },
            BinaryOp::Exponentiation => JSOperatorInfo {
                precedence: 9,
                associativity: JSAssociativity::Right,
            },
            // TODO: Do we need this? As we'll map the pipeline operator to a function call.
            BinaryOp::Pipeline => JSOperatorInfo {
                precedence: 1,
                associativity: JSAssociativity::Left,
            },
        }
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

    fn generate_node(&mut self, node: &Node, parent_op: Option<&BinaryOp>) {
        match &node.ast_node {
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
            AstNode::Program(statements) => {
                for statement in statements {
                    self.generate_node(statement, None);
                    self.flush_statement_buffer();
                }
            }
            AstNode::Block(statements, expression) => {
                self.push_scope();

                for statement in statements {
                    self.generate_node(statement, None);
                    self.flush_statement_buffer();
                }

                if expression.is_none() {
                    self.pop_scope();
                    return;
                }

                match self.current_context() {
                    BlockContext::FunctionBody => {
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
                        self.generate_node(expression.as_ref().unwrap(), None);
                        self.push_str(";\n");
                    }
                    _ => {
                        // In a case of `let a = { 1 }`, we want to render the expression as a statement.
                        // We do this by temporarily swapping the statement buffer, generating the
                        // expression as an statement, and then swapping the statement buffer back.
                        let statement_buffer = std::mem::take(&mut self.current_statement_buffer);
                        let tmp_var = self.scopes.declare_tmp_variable();
                        self.push_str(&self.get_indent());
                        self.push_str(&format!("let {};{{\n", tmp_var));
                        self.indent_level += 1;
                        self.push_str(&self.get_indent());
                        self.push_str(&format!("{} = ", tmp_var));
                        self.generate_node(expression.as_ref().unwrap(), None);
                        self.push_str(";\n");
                        self.indent_level -= 1;
                        self.push_str(&self.get_indent());
                        self.push_str("};\n");
                        self.flush_statement_buffer();
                        self.current_statement_buffer = statement_buffer;
                        self.push_str(tmp_var.as_str());
                    }
                }

                self.flush_statement_buffer();
                self.pop_scope();
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
                let needs_parentheses = parent_op.map_or(false, |parent| {
                    Self::should_wrap_with_parentheses(op, parent)
                });

                if needs_parentheses {
                    self.push_char('(');
                }

                if let BinaryOp::Pipeline = op {
                    // If rhs was an identifier, we just pass lhs it as an argument to a function call.
                    if let AstNode::Identifier(_) = rhs.ast_node {
                        self.generate_node(rhs, None);
                        self.push_char('(');
                        self.generate_node(lhs, None);
                        self.push_char(')');
                    // If rhs is a Call node and we prepend the lhs to the argument list.
                    } else if let AstNode::Call { function, arguments } = &rhs.ast_node {
                        self.generate_node(function, None);
                        self.push_char('(');

                        // If we have a wildcard in the argument list, we instead replace the wildcard with the lhs.
                        // Otherwise we prepend the lhs to the argument list.
                        let has_wildcard = arguments.iter().any(|arg| matches!(arg.ast_node, AstNode::Wildcard));
                        if has_wildcard {
                            for (i, arg) in arguments.iter().enumerate() {
                                if i > 0 {
                                    self.push_str(", ");
                                }

                                if let AstNode::Wildcard = arg.ast_node {
                                    self.generate_node(lhs, None);
                                } else {
                                    self.generate_node(arg, None);
                                }
                            }
                        } else {
                            self.generate_node(lhs, None);
                            for arg in arguments.iter() {
                                self.push_str(", ");
                                self.generate_node(arg, None);
                            }
                        };
                        self.push_char(')');
                    }
                } else {
                    self.generate_node(lhs, Some(op));
                    self.generate_binary_op(op);
                    self.generate_node(rhs, Some(op));
                }

                if needs_parentheses {
                    self.push_char(')');
                }
            }
            AstNode::VariableDeclaration { id: _, name, value } => {
                self.push_str(&self.get_indent());
                let name = self.scopes.declare_variable(name);
                self.push_str(&format!("let {} = ", name));
                self.generate_node(value, None);
                self.push_str(";\n");
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
                self.push_str("if (");
                let indent_level = self.indent_level;
                self.indent_level = 0;
                self.generate_node(condition, None);
                self.indent_level = indent_level;
                self.push_str(") {\n");
                self.indent_level += 1;
                self.context_stack.push(BlockContext::Expression);
                self.generate_node(then_branch, None);
                self.context_stack.pop();
                self.indent_level -= 1;

                if let Some(else_branch) = else_branch {
                    self.push_str(&self.get_indent());
                    self.push_str("} else {\n");
                    self.indent_level += 1;
                    self.context_stack.push(BlockContext::Expression);
                    self.generate_node(else_branch, None);
                    self.context_stack.pop();
                    self.indent_level -= 1;
                }

                self.push_str(&self.get_indent());
                self.push_char('}');
            }
            AstNode::FunctionParameter{ id: _, node} => match node.ast_node {
                AstNode::Identifier { .. } => self.generate_node(node, None),
                AstNode::Literal(_) => self.generate_node(node, None),
                AstNode::List(_) => todo!(),
                // Wildcards are handled within pipeline and call expressions,
                AstNode::Wildcard => unreachable!("Unexpected wildcard in function parameter."),
                _ => todo!(),
            },
            AstNode::FunctionDeclaration { id: _, name, declaration } =>
                self.generate_function_declaration(name, declaration),
            AstNode::FunctionDeclarations { id: _, name, declarations } =>
                self.generate_function_declarations(name, declarations),
            AstNode::FunctionExpression { id: _, name, declaration } =>
                self.generate_function_expression(name, declaration),
            AstNode::ReturnStatement(expr) => self.generate_return_statement(expr),
            AstNode::Identifier(name) => self.push_str(&self.current_scope().resolve_variable(name).unwrap_or(name.to_string())),
            AstNode::NestedIdentifier(identifiers) => self.push_str(&identifiers.join(".")),
            AstNode::Call { function, arguments } =>
                self.generate_call_expression(function, arguments),
            AstNode::RecursiveCall(node) => {
                // If call expression is referencing the current function, all we do is update the arguments,
                // as we are in a while loop.
                if let AstNode::Call { function, arguments } = &node.ast_node {
                    if let AstNode::Identifier(name) = &function.ast_node {
                        if let Some(function_context) = self.function_context_stack.last() {
                            if function_context.is_tail_recursive && &function_context.name == name {
                                let params = function_context.params.clone();
                                let remap_to_rest_args = function_context.remap_to_rest_args;
                                let tmp_vars = params.iter().map(|_| self.scopes.declare_tmp_variable()).collect::<Vec<_>>();

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

    fn is_function_body_tail_recursive(function_name: &str, node: &Node) -> bool {
        // Recursively traverse nodes to check for tail recursive calls to the function itself.
        // We currently only support tail recursion to the function itself, not any other function.
        // Therefore we look for RecursiveCall nodes which reference the current function name.
        match &node.ast_node {
            AstNode::RecursiveCall(node) => {
                // Node is a Call expression, unwrap first.
                if let AstNode::Call {
                    function,
                    arguments: _,
                } = &node.ast_node
                {
                    // If the function is an identifier, check if it's the same as the current function name.
                    if let AstNode::Identifier(name) = &function.ast_node {
                        if name == function_name {
                            return true;
                        }
                    }
                }
                false
            }
            AstNode::Block(statements, expression) => {
                for statement in statements {
                    if Self::is_function_body_tail_recursive(function_name, statement) {
                        return true;
                    }
                }
                if let Some(expression) = expression {
                    return Self::is_function_body_tail_recursive(function_name, expression);
                }
                false
            }
            AstNode::ExpressionStatement(expression) => {
                Self::is_function_body_tail_recursive(function_name, expression)
            }
            AstNode::Match {
                expression,
                arms: _,
            } => Self::is_function_body_tail_recursive(function_name, expression),
            AstNode::MatchArm {
                pattern: _,
                expression,
            } => Self::is_function_body_tail_recursive(function_name, expression),
            AstNode::IfElse {
                condition,
                then_branch,
                else_branch,
            } => {
                Self::is_function_body_tail_recursive(function_name, condition)
                    || Self::is_function_body_tail_recursive(function_name, then_branch)
                    || else_branch.as_ref().map_or(false, |branch| {
                        Self::is_function_body_tail_recursive(function_name, branch)
                    })
            }
            AstNode::ReturnStatement(node) => {
                if let Some(node) = node {
                    Self::is_function_body_tail_recursive(function_name, node)
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    fn generate_function_body(&mut self, body: &Node, is_tail_recursive: bool) {
        self.context_stack.push(BlockContext::FunctionBody);
        self.push_str(&self.function_pre_body.clone());
        self.function_pre_body.clear();
        if is_tail_recursive {
            self.push_str(&self.get_indent());
            self.push_str("while (true) {\n");
            self.indent_level += 1;
        }
        self.generate_node(body, None);
        if is_tail_recursive {
            self.indent_level -= 1;
            self.push_str(&self.get_indent());
            self.push_str("}\n");
        }
        self.context_stack.pop();
    }

    fn generate_function_declaration(&mut self, name: &str, declaration: &FunctionDeclaration) {
        let is_tail_recursive = Self::is_function_body_tail_recursive(name, &declaration.body);
        self.function_context_stack.push(FunctionContext {
            name: name.to_string(),
            params: declaration
                .parameters
                .iter()
                .filter_map(|param| {
                    if let AstNode::FunctionParameter { id: _, node } = &param.ast_node {
                        if let AstNode::Identifier(ref name) = node.ast_node {
                            return Some(name.clone());
                        }
                    }
                    None
                })
                .collect(),
            is_tail_recursive,
            remap_to_rest_args: false,
        });

        self.push_str(&self.get_indent());
        self.push_str(&format!("function {}(", name));

        for (i, param) in declaration.parameters.iter().enumerate() {
            if i > 0 {
                self.push_str(", ");
            }
            self.generate_node(param, None);
        }

        self.push_str(") {\n");
        self.indent_level += 1;
        self.generate_function_body(&declaration.body, is_tail_recursive);
        self.indent_level -= 1;
        self.push_str(&self.get_indent());
        self.push_str("}\n");
        self.function_context_stack.pop();
    }

    fn generate_function_expression(
        &mut self,
        name: &Option<String>,
        declaration: &FunctionDeclaration,
    ) {
        let is_tail_recursive = Self::is_function_body_tail_recursive(
            &name.clone().unwrap_or("".to_string()).to_string(),
            &declaration.body,
        );
        self.function_context_stack.push(FunctionContext {
            name: name.clone().unwrap_or("".to_string()).to_string(),
            params: declaration
                .parameters
                .iter()
                .filter_map(|param| {
                    if let AstNode::FunctionParameter { id: _, node } = &param.ast_node {
                        if let AstNode::Identifier(name) = &node.ast_node {
                            return Some(name.clone());
                        }
                    }
                    None
                })
                .collect(),
            is_tail_recursive,
            remap_to_rest_args: false,
        });
        let function_keyword = match name {
            Some(name) => format!("function {}(", name),
            None => "function(".to_string(),
        };

        self.push_str(&function_keyword);

        for (i, param) in declaration.parameters.iter().enumerate() {
            if i > 0 {
                self.push_str(", ");
            }
            self.generate_node(param, None);
        }

        self.push_str(") {\n");
        self.indent_level += 1;
        self.generate_function_body(&declaration.body, is_tail_recursive);
        self.indent_level -= 1;
        self.push_str(&self.get_indent());
        self.push_char('}');
        self.function_context_stack.pop();
    }

    fn generate_function_declarations(&mut self, name: &str, declarations: &[FunctionDeclaration]) {
        // TODO: Handle tail recursion for multiple definitions.
        let is_any_definition_tail_recursive = declarations
            .iter()
            .any(|declaration| Self::is_function_body_tail_recursive(name, &declaration.body));
        self.push_str(&self.get_indent());
        self.push_str(&format!("function {}(...args) {{\n", name));
        self.indent_level += 1;

        if is_any_definition_tail_recursive {
            self.push_str(&self.get_indent());
            self.push_str("while (true) {\n");
            self.indent_level += 1;
        }

        self.push_str(&self.get_indent());
        for (i, declaration) in declarations.iter().enumerate() {
            // TODO: Only render else if there is another definition with a literal.
            if i > 0 {
                self.push_str(" else ");
            }

            // Expand parameter matching if any definition has a different amount of
            // paramaters.
            let parameter_variadic = declarations
                .iter()
                .map(|declaration| declaration.parameters.clone())
                .any(|params| params.len() != declaration.parameters.len());
            let pattern_matched_parameters =
                declaration
                    .parameters
                    .iter()
                    .enumerate()
                    .filter(|(_, param)| {
                        if let AstNode::FunctionParameter { id: _, node } = &param.ast_node {
                            matches!(
                                node.ast_node,
                                AstNode::Literal(_)
                                    | AstNode::List(_)
                                    | AstNode::EnumExtraction { .. }
                            )
                        } else {
                            false
                        }
                    });

            if parameter_variadic || pattern_matched_parameters.clone().count() > 0 {
                if parameter_variadic {
                    self.push_str("if (args.length === ");
                    self.push_str(&declaration.parameters.len().to_string());

                    if pattern_matched_parameters.clone().count() > 0 {
                        self.push_str(" && ");
                    }
                } else {
                    self.push_str("if (");
                }
                // Filter only literal params.
                for (j, (k, param)) in pattern_matched_parameters.enumerate() {
                    if j > 0 {
                        self.push_str(" && ");
                    }

                    if let AstNode::FunctionParameter { id: _, node } = &param.ast_node {
                        match &node.ast_node {
                            AstNode::Identifier(_) | AstNode::Literal(_) => {
                                self.push_str(&format!("args[{}] === ", k));
                                self.generate_node(param, None);
                            }
                            AstNode::List(patterns) => {
                                if patterns.is_empty() {
                                    self.push_str(&format!("args[{}].length === 0", k));
                                    continue;
                                }

                                // TODO: Handle multiple patterns, for now a simple recursive sum was the test case :D
                                let should_and = false;
                                for (i, pattern) in patterns.iter().enumerate() {
                                    if should_and {
                                        self.push_str(" && ");
                                    }

                                    match &pattern.ast_node {
                                        AstNode::Literal(_) => {
                                            self.push_str(&format!("args[{}][{}] === ", k, i));
                                            self.generate_node(pattern, None);
                                        }
                                        AstNode::PrefixOp(PrefixOp::Rest, identified) => {
                                            if let AstNode::Identifier(ref name) =
                                                identified.ast_node
                                            {
                                                self.push_str(&format!(
                                                    "args[{}].length >= {}",
                                                    k, i
                                                ));
                                                self.indent_level += 1;
                                                self.function_pre_body.push_str(&self.get_indent());
                                                self.function_pre_body.push_str(
                                                    format!(
                                                        "let {} = args[{}].slice({});\n",
                                                        name, k, i
                                                    )
                                                    .as_str(),
                                                );
                                                self.indent_level -= 1;
                                            }
                                        }
                                        AstNode::Identifier(name) => {
                                            self.indent_level += 1;
                                            self.function_pre_body.push_str(&self.get_indent());
                                            self.function_pre_body.push_str(
                                                format!("let {} = args[{}][{}];\n", name, k, i)
                                                    .as_str(),
                                            );
                                            self.indent_level -= 1;
                                        }
                                        _ => unreachable!(),
                                    }
                                }
                            }
                            AstNode::EnumExtraction {
                                identifier,
                                elements,
                                named_fields,
                            } => {
                                let identifier = match &identifier.ast_node {
                                    AstNode::Identifier(name) => name.clone(),
                                    AstNode::NestedIdentifier(names) => {
                                        names.clone().pop().unwrap()
                                    }
                                    _ => unreachable!(),
                                };
                                self.push_str(&format!("args[{}].tag === \"{}\"", k, identifier));
                                self.indent_level += 1;
                                for (i, element) in elements.iter().enumerate() {
                                    // Skip any Wildcards
                                    if let AstNode::Wildcard = element.ast_node {
                                        continue;
                                    }
                                    let identifier = match &element.ast_node {
                                        AstNode::Identifier(name) => name.clone(),
                                        AstNode::NestedIdentifier(names) => {
                                            names.clone().pop().unwrap()
                                        }
                                        _ => unreachable!(),
                                    };
                                    self.function_pre_body.push_str(&self.get_indent());

                                    if *named_fields {
                                        self.function_pre_body.push_str(&format!(
                                            "let {} = args[{}].{};\n",
                                            identifier, k, identifier
                                        ));
                                    } else {
                                        self.function_pre_body.push_str(&format!(
                                            "let {} = args[{}][{}];\n",
                                            identifier, k, i
                                        ));
                                    }
                                }
                                self.indent_level -= 1;
                            }
                            _ => unreachable!(),
                        }
                    }
                }
                self.push_str(") {\n");
            } else {
                self.push_str("{\n");
            }

            self.indent_level += 1;
            // Alias identifier args to the parameter names
            for (j, param) in declaration.parameters.iter().enumerate() {
                if let AstNode::FunctionParameter { id: _, node } = &param.ast_node {
                    if let AstNode::Identifier(ref name) = node.ast_node {
                        self.push_str(&self.get_indent());
                        self.push_str(&format!("let {} = args[{}];\n", name, j));
                    }
                }
            }
            // We handle the tail recursion case in multiple function body declarations ourselves higher up.
            self.function_context_stack.push(FunctionContext {
                name: name.to_string(),
                params: declaration
                    .parameters
                    .iter()
                    .filter_map(|param| {
                        if let AstNode::FunctionParameter { id: _, node } = &param.ast_node {
                            if let AstNode::Identifier(ref name) = node.ast_node {
                                return Some(name.clone());
                            }
                        }
                        None
                    })
                    .collect(),
                is_tail_recursive: is_any_definition_tail_recursive,
                remap_to_rest_args: true,
            });
            self.generate_function_body(&declaration.body, false);
            self.indent_level -= 1;
            self.push_str(&self.get_indent());
            self.push_char('}');
        }

        if is_any_definition_tail_recursive {
            self.indent_level -= 1;
            self.push_char('\n');
            self.push_str(&self.get_indent());
            self.push_char('}');
        }

        self.indent_level -= 1;
        self.push_char('\n');
        self.push_str(&self.get_indent());
        self.push_str("}\n");
        self.function_context_stack.pop();
    }

    fn generate_return_statement(&mut self, expr: &Option<Box<Node>>) {
        // We do not render a return statement if we are in a tail recursive function body.
        // Which calls the current function recursively.
        if expr.is_some() {
            if let AstNode::RecursiveCall(call_exp) = &expr.as_ref().unwrap().ast_node {
                let call_identifier = match &call_exp.ast_node {
                    AstNode::Call {
                        function,
                        arguments: _,
                    } => {
                        if let AstNode::Identifier(name) = &function.ast_node {
                            Some(name)
                        } else {
                            None
                        }
                    }
                    _ => None,
                };

                if call_identifier.is_some() {
                    if let Some(function_context) = self.function_context_stack.last() {
                        if function_context.is_tail_recursive
                            && &function_context.name == call_identifier.unwrap()
                        {
                            return self.generate_node(&expr.clone().unwrap(), None);
                        }
                    }
                }
            }
        }

        self.push_str(&self.get_indent());
        self.push_str("return");

        if let Some(expr) = expr {
            self.push_char(' ');
            self.generate_node(expr, None);
        }

        self.push_str(";\n");
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

    fn should_wrap_with_parentheses(op: &BinaryOp, parent_op: &BinaryOp) -> bool {
        let op_info = Self::map_operator_info(op);
        let parent_op_info = Self::map_operator_info(parent_op);

        if op_info.precedence < parent_op_info.precedence {
            return true;
        }

        op_info.precedence == parent_op_info.precedence
            && op_info.associativity == JSAssociativity::Right
    }

    fn get_indent(&self) -> String {
        "    ".repeat(self.indent_level)
    }

    pub fn get_output(&self) -> &str {
        &self.output
    }
}
