use tlang_parser::{
    ast::{BinaryOp, Node, PrefixOp},
    lexer::Literal,
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
}

#[derive(Debug, PartialEq)]
pub struct CodegenJS {
    output: String,
    indent_level: usize,
    context_stack: Vec<BlockContext>,
    function_context_stack: Vec<FunctionContext>,
    function_pre_body: String,
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
            context_stack: vec![BlockContext::Program],
            function_context_stack: vec![],
            function_pre_body: String::new(),
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
            BinaryOp::Add => self.output.push_str(" + "),
            BinaryOp::Subtract => self.output.push_str(" - "),
            BinaryOp::Multiply => self.output.push_str(" * "),
            BinaryOp::Divide => self.output.push_str(" / "),
            BinaryOp::Modulo => self.output.push_str(" % "),
            BinaryOp::Exponentiation => self.output.push_str(" ** "),
            BinaryOp::Equal => self.output.push_str(" === "),
            BinaryOp::NotEqual => self.output.push_str(" !== "),
            BinaryOp::LessThan => self.output.push_str(" < "),
            BinaryOp::LessThanOrEqual => self.output.push_str(" <= "),
            BinaryOp::GreaterThan => self.output.push_str(" > "),
            BinaryOp::GreaterThanOrEqual => self.output.push_str(" >= "),
            BinaryOp::And => self.output.push_str(" && "),
            BinaryOp::Or => self.output.push_str(" || "),
            BinaryOp::BitwiseOr => self.output.push_str(" | "),
            BinaryOp::BitwiseAnd => self.output.push_str(" & "),
            BinaryOp::BitwiseXor => self.output.push_str(" ^ "),
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
                self.output.push_str(&value.to_string());
            }
            Literal::UnsignedInteger(value) => {
                self.output.push_str(&value.to_string());
            }
            Literal::Float(value) => {
                self.output.push_str(&value.to_string());
            }
            Literal::Boolean(value) => {
                self.output.push_str(&value.to_string());
            }
            Literal::String(value) | Literal::Char(value) => {
                self.output.push_str(&format!("\"{}\"", value));
            }
        }
    }

    fn generate_node(&mut self, node: &Node, parent_op: Option<&BinaryOp>) {
        match node {
            Node::SingleLineComment(comment) => {
                self.output.push_str("//");
                self.output.push_str(comment);
                self.output.push('\n');
            }
            Node::MultiLineComment(comment) => {
                self.output.push_str("/*");
                self.output.push_str(comment);
                self.output.push_str("*/\n");
            }
            Node::Program(statements) => {
                for statement in statements {
                    self.generate_node(statement, None);
                }
            }
            Node::Block(statements, expression) => {
                for statement in statements {
                    self.generate_node(statement, None);
                }

                if expression.is_none() {
                    return;
                }

                match self.current_context() {
                    BlockContext::Program => unreachable!("Block with completion in ProgramBlock context not implemented yet."),
                    BlockContext::Statement => unimplemented!("Block with completion in StatementBlock context not implemented yet."),
                    BlockContext::FunctionBody => {
                        self.output.push_str(&self.get_indent());
                        self.output.push_str("return ");
                        self.generate_node(expression.as_ref().unwrap(), None);
                        self.output.push_str(";\n");
                    }
                    BlockContext::Expression => unimplemented!("Block with completion in ExpressionBlock context not implemented yet."),
                }
            }
            Node::ExpressionStatement(expression) => {
                self.output.push_str(&self.get_indent());
                self.context_stack.push(BlockContext::Statement);
                self.generate_node(expression, None);
                self.context_stack.pop();

                if let Node::IfElse { .. } = **expression {
                    self.output.push('\n');
                    return;
                }

                self.output.push_str(";\n");
            }
            Node::Literal(literal) => self.generate_literal(literal),
            Node::List(items) => {
                self.output.push('[');
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        self.output.push_str(", ");
                    }
                    self.generate_node(item, None);
                }
                self.output.push(']');
            }
            Node::Dict(entries) => {
                self.output.push_str("{\n");
                self.indent_level += 1;
                for (i, (key, value)) in entries.iter().enumerate() {
                    if i > 0 {
                        self.output.push_str(",\n");
                    }
                    self.output.push_str(&self.get_indent());
                    self.generate_node(key, None);
                    self.output.push_str(": ");
                    self.generate_node(value, None);
                }
                self.output.push_str(",\n");
                self.indent_level -= 1;
                self.output.push_str(&self.get_indent());
                self.output.push('}');
            }
            Node::PrefixOp(op, node) => {
                match op {
                    PrefixOp::Spread => self.output.push_str("..."),
                    _ => unimplemented!("PrefixOp {:?} not implemented yet.", op),
                }
                self.generate_node(node, None);
            },
            Node::BinaryOp { op, lhs, rhs } => {
                let needs_parentheses = parent_op.map_or(false, |parent| {
                    Self::should_wrap_with_parentheses(op, parent)
                });

                if needs_parentheses {
                    self.output.push('(');
                }

                if let BinaryOp::Pipeline = op {
                    // If rhs was an identifier, we just pass lhs it as an argument to a function call.
                    if let Node::Identifier(_) = **rhs {
                        self.generate_node(rhs, None);
                        self.output.push('(');
                        self.generate_node(lhs, None);
                        self.output.push(')');
                    // If rhs is a Call node and we prepend the lhs to the argument list.
                    } else if let Node::Call { function, arguments } = *rhs.clone() {
                        self.generate_node(&function, None);
                        self.output.push('(');

                        // If we have a wildcard in the argument list, we instead replace the wildcard with the lhs.
                        // Otherwise we prepend the lhs to the argument list.
                        let has_wildcard = arguments.iter().any(|arg| matches!(arg, Node::Wildcard));
                        if has_wildcard {
                            for (i, arg) in arguments.iter().enumerate() {
                                if i > 0 {
                                    self.output.push_str(", ");
                                }

                                if let Node::Wildcard = arg {
                                    self.generate_node(lhs, None);
                                } else {
                                    self.generate_node(arg, None);
                                }
                            }
                        } else {
                            self.generate_node(lhs, None);
                            for arg in arguments.iter() {
                                self.output.push_str(", ");
                                self.generate_node(arg, None);
                            }
                        };
                        self.output.push(')');
                    }
                } else {
                    self.generate_node(lhs, Some(op));
                    self.generate_binary_op(op);
                    self.generate_node(rhs, Some(op));
                }

                if needs_parentheses {
                    self.output.push(')');
                }
            }
            Node::VariableDeclaration { name, value } => {
                self.output.push_str(&self.get_indent());
                self.output.push_str(&format!("let {} = ", name));
                self.generate_node(value, None);
                self.output.push_str(";\n");
            }
            Node::Match {
                expression: _,
                arms: _,
            } => todo!(),
            Node::MatchArm {
                pattern: _,
                expression: _,
            } => todo!(),
            Node::Wildcard => unreachable!("Stray wildcard expression, you can only use _ wildcards in function declarations, pipelines and pattern matching."),
            Node::IfElse {
                condition,
                then_branch,
                else_branch,
            } => {
                self.output.push_str("if (");
                let indent_level = self.indent_level;
                self.indent_level = 0;
                self.generate_node(condition, None);
                self.indent_level = indent_level;
                self.output.push_str(") {\n");
                self.indent_level += 1;
                self.context_stack.push(BlockContext::Expression);
                self.generate_node(then_branch, None);
                self.context_stack.push(BlockContext::Expression);
                self.indent_level -= 1;

                if let Some(else_branch) = else_branch {
                    self.output.push_str(&self.get_indent());
                    self.output.push_str("} else {\n");
                    self.indent_level += 1;
                    self.context_stack.push(BlockContext::Expression);
                    self.generate_node(else_branch, None);
                    self.context_stack.push(BlockContext::Expression);
                    self.indent_level -= 1;
                }

                self.output.push_str(&self.get_indent());
                self.output.push('}');
            }
            Node::FunctionParameter(node) => match **node {
                Node::Identifier { .. } => self.generate_node(node, None),
                Node::Literal(_) => self.generate_node(node, None),
                Node::List(_) => todo!(),
                // Wildcards are handled within pipeline and call expressions,
                Node::Wildcard => unreachable!("Unexpected wildcard in function parameter."),
                _ => todo!(),
            },
            Node::FunctionDeclaration { name, parameters, body } =>
                self.generate_function_declaration(name, parameters, body),
            Node::FunctionDeclarations(name, definitions) =>
                self.generate_function_declarations(name, definitions),
            Node::FunctionExpression { name, parameters, body } =>
                self.generate_function_expression(name, parameters, body),
            Node::ReturnStatement(expr) => self.generate_return_statement(expr),
            Node::Identifier(name) => self.output.push_str(name),
            Node::NestedIdentifier(identifiers) => self.output.push_str(&identifiers.join(".")),
            Node::Call { function, arguments } =>
                self.generate_call_expression(function, arguments),
            Node::RecursiveCall(node) => {
                // If call expression is referencing the current function, all we do is update the arguments,
                // as we are in a while loop.
                if let Node::Call { function, arguments } = *node.clone() {
                    if let Node::Identifier(name) = *function {
                        if let Some(function_context) = self.function_context_stack.last() {
                            println!("{} == {}", function_context.name, name);
                            if function_context.is_tail_recursive && function_context.name == name {
                                let params = function_context.params.clone();

                                for (i, arg) in arguments.iter().enumerate() {
                                    self.output.push_str(&self.get_indent());
                                    self.output.push_str(&format!("let tmp{} = ", i));
                                    self.generate_node(arg, None);
                                    self.output.push_str(";\n");
                                }
                                for (i, arg_name) in params.iter().enumerate() {
                                    self.output.push_str(&self.get_indent());
                                    self.output.push_str(&format!("{} = tmp{};\n", arg_name, i));
                                }
                                return;
                            }
                        }
                    }
                }

                // For any other referenced function, we do a normal call expression.
                self.generate_node(node, parent_op)
            },
            Node::EnumDeclaration { name, variants } => self.generate_enum_declaration(name, variants),
            Node::EnumVariant { name, named_fields, parameters } => self.generate_enum_variant(name, *named_fields, parameters),
            Node::EnumExtraction { identifier, elements, named_fields } => self.generate_enum_extraction(identifier, elements, *named_fields),
            _ => unimplemented!("{:?} not implemented yet.", node),
        }
    }

    fn generate_enum_declaration(&mut self, name: &str, variants: &[Node]) {
        self.output.push_str(&self.get_indent());
        self.output.push_str(&format!("const {} = {{\n", name));
        self.indent_level += 1;
        for variant in variants {
            self.generate_node(variant, None);
        }
        self.indent_level -= 1;
        self.output.push_str(&self.get_indent());
        self.output.push_str("};\n");
    }

    fn generate_enum_variant(&mut self, name: &str, named_fields: bool, parameters: &[Node]) {
        self.output.push_str(&self.get_indent());

        if parameters.is_empty() {
            self.output
                .push_str(&format!("{}: {{ tag: \"{}\" }},\n", name, name));
            return;
        }

        self.output.push_str(&format!("{}(", name));
        if named_fields {
            self.output.push_str("{ ");
        }
        for (i, param) in parameters.iter().enumerate() {
            if i > 0 {
                self.output.push_str(", ");
            }
            self.generate_node(param, None);
        }
        if named_fields {
            self.output.push_str(" }");
        }
        self.output.push_str(") {\n");
        self.indent_level += 1;
        self.output.push_str(&self.get_indent());
        self.output.push_str("return {\n");
        self.indent_level += 1;
        self.output.push_str(&self.get_indent());
        self.output
            .push_str(format!("tag: \"{}\",\n", name).as_str());
        for (i, param) in parameters.iter().enumerate() {
            self.output.push_str(&self.get_indent());
            if named_fields {
                if let Node::Identifier(i) = param {
                    self.output.push_str(&i.to_string());
                }
            } else {
                self.output.push_str(&format!("\"{}\": ", i));
                self.generate_node(param, None);
            }
            self.output.push_str(",\n");
        }
        self.indent_level -= 1;
        self.output.push_str(&self.get_indent());
        self.output.push_str("};\n");
        self.indent_level -= 1;
        self.output.push_str(&self.get_indent());
        self.output.push_str("},\n");
    }

    fn generate_enum_extraction(
        &mut self,
        identifier: &Box<Node>,
        elements: &[Node],
        named_fields: bool,
    ) {
        self.output.push_str(&self.get_indent());
        self.output.push_str("const ");
        self.generate_node(identifier, None);
        self.output.push_str(" = (");
        self.generate_node(identifier, None);
        self.output.push_str(") => {\n");
        self.indent_level += 1;
        self.output.push_str(&self.get_indent());
        self.output.push_str("switch (");
        self.generate_node(identifier, None);
        self.output.push_str(".tag) {\n");
        self.indent_level += 1;
        for (i, element) in elements.iter().enumerate() {
            self.output.push_str(&self.get_indent());
            self.output.push_str(&format!("case \"{}\":\n", i));
            self.indent_level += 1;
            self.output.push_str(&self.get_indent());
            self.output.push_str("return ");
            self.generate_node(element, None);
            self.output.push_str(";\n");
            self.indent_level -= 1;
        }
        self.indent_level -= 1;
        self.output.push_str(&self.get_indent());
        self.output.push_str("}\n");
        self.indent_level -= 1;
        self.output.push_str(&self.get_indent());
        self.output.push_str("};\n");
    }

    fn is_function_body_tail_recursive(&self, function_name: &str, node: &Node) -> bool {
        println!(
            "Checking if {} is tail recursive: {:?}",
            function_name, node
        );
        // Recursively traverse nodes to check for tail recursive calls to the function itself.
        // We currently only support tail recursion to the function itself, not any other function.
        // Therefore we look for RecursiveCall nodes which reference the current function name.
        match node {
            Node::RecursiveCall(node) => {
                // Node is a Call expression, unwrap first.
                if let Node::Call {
                    function,
                    arguments: _,
                } = node.as_ref()
                {
                    // If the function is an identifier, check if it's the same as the current function name.
                    if let Node::Identifier(name) = function.as_ref() {
                        if name == function_name {
                            return true;
                        }
                    }
                }
                false
            }
            Node::Block(statements, expression) => {
                for statement in statements {
                    if self.is_function_body_tail_recursive(function_name, statement) {
                        return true;
                    }
                }
                if let Some(expression) = expression {
                    return self.is_function_body_tail_recursive(function_name, expression);
                }
                false
            }
            Node::ExpressionStatement(expression) => {
                self.is_function_body_tail_recursive(function_name, expression)
            }
            Node::Match {
                expression,
                arms: _,
            } => self.is_function_body_tail_recursive(function_name, expression),
            Node::MatchArm {
                pattern: _,
                expression,
            } => self.is_function_body_tail_recursive(function_name, expression),
            Node::IfElse {
                condition,
                then_branch,
                else_branch,
            } => {
                self.is_function_body_tail_recursive(function_name, condition)
                    || self.is_function_body_tail_recursive(function_name, then_branch)
                    || else_branch.as_ref().map_or(false, |branch| {
                        self.is_function_body_tail_recursive(function_name, branch)
                    })
            }
            Node::ReturnStatement(node) => {
                if node.is_some() {
                    self.is_function_body_tail_recursive(function_name, &node.clone().unwrap())
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    fn generate_function_body(&mut self, name: &str, body: &Node) {
        let is_tail_recursive = self.is_function_body_tail_recursive(name, body);
        self.context_stack.push(BlockContext::FunctionBody);
        self.output.push_str(&self.function_pre_body);
        self.function_pre_body.clear();
        if is_tail_recursive {
            self.output.push_str(&self.get_indent());
            self.output.push_str("while (true) {\n");
            self.indent_level += 1;
        }
        self.generate_node(body, None);
        if is_tail_recursive {
            self.indent_level -= 1;
            self.output.push_str(&self.get_indent());
            self.output.push_str("}\n");
        }
        self.context_stack.pop();
    }

    fn generate_function_declaration(&mut self, name: &str, parameters: &[Node], body: &Node) {
        let is_tail_recursive = self.is_function_body_tail_recursive(name, body);
        self.function_context_stack.push(FunctionContext {
            name: name.to_string(),
            params: parameters
                .iter()
                .filter_map(|param| {
                    if let Node::FunctionParameter(node) = param {
                        if let Node::Identifier(ref name) = **node {
                            return Some(name.clone());
                        }
                    }
                    None
                })
                .collect(),
            is_tail_recursive,
        });

        self.output.push_str(&self.get_indent());
        self.output.push_str(&format!("function {}(", name));

        for (i, param) in parameters.iter().enumerate() {
            if i > 0 {
                self.output.push_str(", ");
            }
            self.generate_node(param, None);
        }

        self.output.push_str(") {\n");
        self.indent_level += 1;
        self.generate_function_body(name, body);
        self.indent_level -= 1;
        self.output.push_str(&self.get_indent());
        self.output.push_str("}\n");
        self.function_context_stack.pop();
    }

    fn generate_function_expression(
        &mut self,
        name: &Option<String>,
        parameters: &[Node],
        body: &Node,
    ) {
        self.function_context_stack.push(FunctionContext {
            name: name.clone().unwrap_or("".to_string()).to_string(),
            params: parameters
                .iter()
                .filter_map(|param| {
                    if let Node::FunctionParameter(node) = param {
                        if let Node::Identifier(ref name) = **node {
                            return Some(name.clone());
                        }
                    }
                    None
                })
                .collect(),
            is_tail_recursive: self.is_function_body_tail_recursive(
                &name.clone().unwrap_or("".to_string()).to_string(),
                body,
            ),
        });
        let function_keyword = match name {
            Some(name) => format!("function {}(", name),
            None => "function(".to_string(),
        };

        self.output.push_str(&function_keyword);

        for (i, param) in parameters.iter().enumerate() {
            if i > 0 {
                self.output.push_str(", ");
            }
            self.generate_node(param, None);
        }

        self.output.push_str(") {\n");
        self.indent_level += 1;
        self.generate_function_body(&name.clone().unwrap_or("".to_string()), body);
        self.indent_level -= 1;
        self.output.push_str(&self.get_indent());
        self.output.push('}');
        self.function_context_stack.pop();
    }

    fn generate_function_declarations(
        &mut self,
        name: &str,
        definitions: &[(Vec<Node>, Box<Node>)],
    ) {
        // TODO: Handle tail recursion for multiple definitions.
        self.function_context_stack.push(FunctionContext {
            name: name.to_string(),
            params: definitions
                .iter()
                .flat_map(|(params, _)| {
                    params.iter().filter_map(|param| {
                        if let Node::FunctionParameter(node) = param {
                            if let Node::Identifier(ref name) = **node {
                                return Some(name.clone());
                            }
                        }
                        None
                    })
                })
                .collect(),
            is_tail_recursive: false,
        });
        self.output.push_str(&self.get_indent());
        self.output
            .push_str(&format!("function {}(...args) {{\n", name));
        self.indent_level += 1;
        self.output.push_str(&self.get_indent());

        for (i, (parameters, body)) in definitions.iter().enumerate() {
            // TODO: Only render else if there is another definition with a literal.
            if i > 0 {
                self.output.push_str(" else ");
            }

            // Expand parameter matching if any definition has a different amount of
            // paramaters.
            let parameter_variadic = definitions
                .iter()
                .any(|(params, _)| params.len() != parameters.len());
            let pattern_matched_parameters = parameters.iter().enumerate().filter(|(_, param)| {
                if let Node::FunctionParameter(node) = param {
                    matches!(
                        **node,
                        Node::Literal(_) | Node::List(_) | Node::EnumExtraction { .. }
                    )
                } else {
                    false
                }
            });

            if parameter_variadic || pattern_matched_parameters.clone().count() > 0 {
                if parameter_variadic {
                    self.output.push_str("if (args.length === ");
                    self.output.push_str(&parameters.len().to_string());

                    if pattern_matched_parameters.clone().count() > 0 {
                        self.output.push_str(" && ");
                    }
                } else {
                    self.output.push_str("if (");
                }
                // Filter only literal params.
                for (j, (k, param)) in pattern_matched_parameters.enumerate() {
                    if j > 0 {
                        self.output.push_str(" && ");
                    }

                    if let Node::FunctionParameter(node) = param {
                        match *node.clone() {
                            Node::Identifier(_) | Node::Literal(_) => {
                                self.output.push_str(&format!("args[{}] === ", k));
                                self.generate_node(param, None);
                            }
                            Node::List(patterns) => {
                                if patterns.is_empty() {
                                    self.output.push_str(&format!("args[{}].length === 0", k));
                                    continue;
                                }

                                // TODO: Handle multiple patterns, for now a simple recursive sum was the test case :D
                                let should_and = false;
                                for (i, pattern) in patterns.iter().enumerate() {
                                    if should_and {
                                        self.output.push_str(" && ");
                                    }

                                    match pattern {
                                        Node::Literal(_) => {
                                            self.output
                                                .push_str(&format!("args[{}][{}] === ", k, i));
                                            self.generate_node(pattern, None);
                                        }
                                        Node::PrefixOp(PrefixOp::Rest, identified) => {
                                            if let Node::Identifier(ref name) = **identified {
                                                self.output.push_str(&format!(
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
                                        Node::Identifier(name) => {
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
                            Node::EnumExtraction {
                                identifier,
                                elements,
                                named_fields,
                            } => {
                                let identifier = match *identifier {
                                    Node::Identifier(ref name) => name.clone(),
                                    Node::NestedIdentifier(ref names) => {
                                        names.clone().pop().unwrap()
                                    }
                                    _ => unreachable!(),
                                };
                                self.output
                                    .push_str(&format!("args[{}].tag === \"{}\"", k, identifier));
                                self.indent_level += 1;
                                for (i, element) in elements.iter().enumerate() {
                                    // Skip any Wildcards
                                    if let Node::Wildcard = element {
                                        continue;
                                    }
                                    let identifier = match element {
                                        Node::Identifier(ref name) => name.clone(),
                                        Node::NestedIdentifier(ref names) => {
                                            names.clone().pop().unwrap()
                                        }
                                        _ => unreachable!(),
                                    };
                                    self.function_pre_body.push_str(&self.get_indent());

                                    if named_fields {
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
                self.output.push_str(") {\n");
            } else {
                self.output.push_str("{\n");
            }

            self.indent_level += 1;
            // Alias identifier args to the parameter names
            for (j, param) in parameters.iter().enumerate() {
                if let Node::FunctionParameter(node) = param {
                    if let Node::Identifier(ref name) = **node {
                        self.output.push_str(&self.get_indent());
                        self.output
                            .push_str(&format!("let {} = args[{}];\n", name, j));
                    }
                }
            }
            self.generate_function_body(name, body);
            self.indent_level -= 1;
            self.output.push_str(&self.get_indent());
            self.output.push('}');
        }

        self.indent_level -= 1;
        self.output.push('\n');
        self.output.push_str(&self.get_indent());
        self.output.push_str("}\n");
        self.function_context_stack.pop();
    }

    fn generate_return_statement(&mut self, expr: &Option<Box<Node>>) {
        // We do not render a return statement if we are in a tail recursive function body.
        // Which calls the current function recursively.
        if expr.is_some() {
            if let Node::RecursiveCall(call_exp) = *expr.clone().unwrap() {
                let call_identifier = match *call_exp {
                    Node::Call {
                        function,
                        arguments: _,
                    } => {
                        if let Node::Identifier(name) = *function {
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
                            && function_context.name == call_identifier.unwrap()
                        {
                            return self.generate_node(&expr.clone().unwrap(), None);
                        }
                    }
                }
            }
        }

        self.output.push_str(&self.get_indent());
        self.output.push_str("return");

        if let Some(expr) = expr {
            self.output.push(' ');
            self.generate_node(expr, None);
        }

        self.output.push_str(";\n");
    }

    fn generate_call_expression(&mut self, function: &Node, arguments: &[Node]) {
        let has_wildcards = arguments.iter().any(|arg| matches!(arg, Node::Wildcard));

        if has_wildcards {
            self.output.push_str("function(...args) {\n");
            self.indent_level += 1;
            self.output.push_str(&self.get_indent());
            self.output.push_str("return ");
            self.generate_node(function, None);
            self.output.push('(');

            let mut wildcard_index = 0;
            for (i, arg) in arguments.iter().enumerate() {
                if i > 0 {
                    self.output.push_str(", ");
                }

                if let Node::Wildcard = arg {
                    self.output
                        .push_str(format!("args[{}]", wildcard_index).as_str());
                    wildcard_index += 1;
                } else {
                    self.generate_node(arg, None);
                }
            }

            self.output.push_str(");\n");
            self.indent_level -= 1;
            self.output.push_str(&self.get_indent());
            self.output.push('}');
            return;
        }

        self.generate_node(function, None);
        self.output.push('(');

        for (i, arg) in arguments.iter().enumerate() {
            if i > 0 {
                self.output.push_str(", ");
            }
            self.generate_node(arg, None);
        }
        self.output.push(')');
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
