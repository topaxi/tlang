use tlang_parser::{
    lexer::Literal,
    parser::{BinaryOp, Node},
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
    ProgramBlock,
    // Are we in a StatementExpression with a Block?
    StatementBlock,
    // Are we in a FunctionDeclaration or FunctionExpression with a Block?
    FunctionBodyBlock,
    // Are we in an expression with a Block?
    ExpressionBlock,
}

#[derive(Debug, PartialEq)]
pub struct CodegenJS {
    output: String,
    indent_level: usize,
    context_stack: Vec<BlockContext>,
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
            context_stack: vec![BlockContext::ProgramBlock],
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
        }
    }

    fn generate_node(&mut self, node: &Node, parent_op: Option<&BinaryOp>) {
        match node {
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
                    BlockContext::ProgramBlock => unreachable!("Block with completion in ProgramBlock context not implemented yet."),
                    BlockContext::StatementBlock => unimplemented!("Block with completion in StatementBlock context not implemented yet."),
                    BlockContext::FunctionBodyBlock => {
                        self.output.push_str(&self.get_indent());
                        self.output.push_str("return ");
                        self.generate_node(expression.as_ref().unwrap(), None);
                        self.output.push_str(";\n");
                    }
                    BlockContext::ExpressionBlock => unimplemented!("Block with completion in ExpressionBlock context not implemented yet."),
                }
            }
            Node::ExpressionStatement(expression) => {
                self.output.push_str(&self.get_indent());
                self.context_stack.push(BlockContext::StatementBlock);
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
            Node::PrefixOp(_, _) => todo!("PrefixOp not implemented yet."),
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
                        let has_wildcard = arguments.iter().any(|arg| match arg {
                            Node::Wildcard => true,
                            _ => false,
                        });
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
                self.context_stack.push(BlockContext::ExpressionBlock);
                self.generate_node(then_branch, None);
                self.context_stack.push(BlockContext::ExpressionBlock);
                self.indent_level -= 1;

                if let Some(else_branch) = else_branch {
                    self.output.push_str(&self.get_indent());
                    self.output.push_str("} else {\n");
                    self.indent_level += 1;
                    self.context_stack.push(BlockContext::ExpressionBlock);
                    self.generate_node(else_branch, None);
                    self.context_stack.push(BlockContext::ExpressionBlock);
                    self.indent_level -= 1;
                }

                self.output.push_str(&self.get_indent());
                self.output.push('}');
            }
            Node::FunctionParameter(node) => match **node {
                Node::Identifier { .. } => self.generate_node(node, None),
                Node::Literal(_) => self.generate_node(node, None),
                _ => todo!(),
            },
            Node::FunctionDeclaration {
                name,
                parameters,
                body,
            } => {
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
                self.context_stack.push(BlockContext::FunctionBodyBlock);
                self.generate_node(body, None);
                self.context_stack.pop();
                self.indent_level -= 1;
                self.output.push_str(&self.get_indent());
                self.output.push_str("}\n");
            }
            Node::FunctionDeclarations(name, definitions) => {
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
                    let literal_parameters = parameters.iter().enumerate().filter(|(_, param)| {
                        if let Node::FunctionParameter(node) = param {
                            matches!(**node, Node::Literal(_) | Node::List(_))
                        } else {
                            false
                        }
                    });

                    if parameter_variadic || literal_parameters.clone().count() > 0 {
                        if parameter_variadic {
                            self.output.push_str("if (args.length === ");
                            self.output.push_str(&parameters.len().to_string());

                            if literal_parameters.clone().count() > 0 {
                                self.output.push_str(" && ");
                            }
                        } else {
                            self.output.push_str("if (");
                        }
                        // Filter only literal params.
                        for (j, (k, param)) in literal_parameters.enumerate() {
                            if j > 0 {
                                self.output.push_str(" && ");
                            }
                            self.output.push_str(&format!("args[{}] === ", k));
                            self.generate_node(param, None);
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
                    self.context_stack.push(BlockContext::FunctionBodyBlock);
                    self.generate_node(body, None);
                    self.context_stack.pop();
                    self.indent_level -= 1;
                    self.output.push_str(&self.get_indent());
                    self.output.push('}');
                }

                self.indent_level -= 1;
                self.output.push('\n');
                self.output.push_str(&self.get_indent());
                self.output.push_str("}\n");
            }
            Node::FunctionExpression {
                name,
                parameters,
                body,
            } => {
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
                self.context_stack.push(BlockContext::FunctionBodyBlock);
                self.generate_node(body, None);
                self.context_stack.pop();
                self.indent_level -= 1;
                self.output.push_str(&self.get_indent());
                self.output.push('}');
            }
            Node::ReturnStatement(expr) => {
                self.output.push_str(&self.get_indent());
                self.output.push_str("return");

                if expr.is_some() {
                    self.output.push(' ');
                    self.generate_node(expr.as_ref().unwrap(), None);
                }

                self.output.push_str(";\n");
            }
            Node::Identifier(name) => {
                self.output.push_str(name);
            }
            Node::Call {
                function,
                arguments,
            } => {
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
        }
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
