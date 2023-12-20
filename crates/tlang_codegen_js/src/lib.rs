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

#[derive(Debug, PartialEq)]
pub struct CodegenJS {
    output: String,
    indent_level: usize,
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
        }
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
            // TODO: Handle block expressions
            Node::Block(statements, _expression) => {
                for statement in statements {
                    self.generate_node(statement, None);
                }
            }
            Node::ExpressionStatement(expression) => {
                self.output.push_str(&self.get_indent());
                self.generate_node(expression, None);

                if let Node::IfElse { .. } = **expression {
                    self.output.push('\n');
                    return;
                }

                self.output.push_str(";\n");
            }
            Node::Literal(literal) => self.generate_literal(literal),
            Node::BinaryOp { op, lhs, rhs } => {
                let needs_parentheses = parent_op.map_or(false, |parent| {
                    Self::should_wrap_with_parentheses(op, parent)
                });

                if needs_parentheses {
                    self.output.push('(');
                }

                self.generate_node(lhs, Some(op));
                self.generate_binary_op(op);
                self.generate_node(rhs, Some(op));

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
            Node::Wildcard => todo!(),
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
                self.generate_node(then_branch, None);
                self.indent_level -= 1;

                if let Some(else_branch) = else_branch {
                    self.output.push_str(&self.get_indent());
                    self.output.push_str("} else {\n");
                    self.indent_level += 1;
                    self.generate_node(else_branch, None);
                    self.indent_level -= 1;
                }

                self.output.push_str(&self.get_indent());
                self.output.push('}');
            }
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
                    self.output.push_str(param);
                }

                self.output.push_str(") {\n");
                self.indent_level += 1;
                self.generate_node(body, None);
                self.indent_level -= 1;
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
                    self.output.push_str(param);
                }

                self.output.push_str(") {\n");
                self.indent_level += 1;
                self.generate_node(body, None);
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

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use tlang_parser::{lexer::Lexer, parser::Parser};

    macro_rules! gen {
        ($source:expr) => {{
            let lexer = Lexer::new($source);
            let mut parser = Parser::new(lexer);
            let node = parser.parse_program();
            let mut codegen = CodegenJS::default();
            codegen.generate_code(&node);
            codegen.get_output().to_string()
        }};
    }

    #[test]
    fn test_codegen_variable_declaration() {
        let output = gen!("let x = 42;");
        let expected_output = "let x = 42;\n";
        assert_eq!(output, expected_output);
    }

    #[test]
    fn test_codegen_binary_expression() {
        let output = gen!("let x = 42 + 1;");
        let expected_output = "let x = 42 + 1;\n";
        assert_eq!(output, expected_output);
    }

    #[test]
    fn test_codegen_function_declaration() {
        let output = gen!("fn main() {}");
        let expected_output = "function main() {\n}\n";
        assert_eq!(output, expected_output);
    }

    #[test]
    fn test_codegen_function_call() {
        let output = gen!("fn main() { foo(); }");
        let expected_output = indoc! {"
            function main() {
                foo();
            }
        "};
        assert_eq!(output, expected_output);
    }

    #[test]
    fn test_codegen_function_expression() {
        let output = gen!("fn main() { let foo = fn() { 1 + 2; }; }");
        let expected_output = indoc! {"
            function main() {
                let foo = function() {
                    1 + 2;
                };
            }
        "};
        assert_eq!(output, expected_output);
    }

    #[test]
    fn test_codegen_functions_with_explicit_return_statements() {
        let output = gen!("fn main() { return 42; }");
        let expected_output = indoc! {"
            function main() {
                return 42;
            }
        "};
        assert_eq!(output, expected_output);

        let output = gen!("fn main() { return; }");
        let expected_output = indoc! {"
            function main() {
                return;
            }
        "};
        assert_eq!(output, expected_output);
    }

    #[test]
    fn test_codegen_parenthesis_expression() {
        let output = gen!("let x = (42 + 1) * 2;");
        let expected_output = "let x = (42 + 1) * 2;\n";
        assert_eq!(output, expected_output);
    }

    #[test]
    fn test_codegen_operator_precedence() {
        let output = gen!("let result = 1 + 2 * 3;");
        let expected_output = "let result = 1 + 2 * 3;\n";
        assert_eq!(output, expected_output);
    }

    #[test]
    fn test_if_else() {
        let output = gen!("fn main() { if true { 1; } else { 2; } }");
        let expected_output = indoc! {"
            function main() {
                if (true) {
                    1;
                } else {
                    2;
                }
            }
        "};
        assert_eq!(output, expected_output);
    }
}
