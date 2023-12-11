use tlang_parser::{
    lexer::Literal,
    parser::{BinaryOp, Node},
};

#[derive(Debug, PartialEq)]
pub struct Codegen {
    output: String,
    indent_level: usize,
}

impl Default for Codegen {
    fn default() -> Self {
        Self::new()
    }
}

impl Codegen {
    pub fn new() -> Self {
        Self {
            output: String::new(),
            indent_level: 0,
        }
    }

    pub fn generate_code(&mut self, node: &Node) {
        self.generate_node(node)
    }

    fn generate_binary_op(&mut self, op: &BinaryOp) {
        match op {
            BinaryOp::Add => self.output.push_str(" + "),
            BinaryOp::Subtract => self.output.push_str(" - "),
            BinaryOp::Multiply => self.output.push_str(" * "),
            BinaryOp::Divide => self.output.push_str(" / "),
            BinaryOp::Modulo => self.output.push_str(" % "),
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

    fn generate_node(&mut self, node: &Node) {
        match node {
            Node::Program(statements) => {
                for statement in statements {
                    self.generate_node(statement);
                }
            }
            Node::Literal(literal) => {
                self.output.push_str(&self.get_indent());

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
                }
            }
            Node::BinaryOp { op, lhs, rhs } => {
                self.output.push_str(&self.get_indent());
                self.generate_node(lhs);
                self.generate_binary_op(op);
                self.generate_node(rhs);
            }
            Node::VariableDeclaration { name, value } => {
                self.output.push_str(&self.get_indent());
                self.output.push_str(&format!("let {} = ", name));
                self.generate_node(value);
                self.output.push_str(";\n");
            }
            Node::IfElse { .. } => todo!("implement if/else codegen"),
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
                self.generate_node(body);
                self.indent_level -= 1;
                self.output.push_str(&self.get_indent());
                self.output.push_str("}\n");
            }
            Node::Identifier(name) => {
                self.output.push_str(name);
            }
            Node::Call {
                function,
                arguments,
            } => {
                self.output.push_str(&self.get_indent());
                self.generate_node(function);
                self.output.push('(');
                for (i, arg) in arguments.iter().enumerate() {
                    if i > 0 {
                        self.output.push_str(", ");
                    }
                    self.generate_node(arg);
                }
                self.output.push_str(");\n");
            }
        }
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
    use tlang_parser::{lexer::Lexer, parser::Parser};

    macro_rules! gen {
        ($source:expr) => {{
            let lexer = Lexer::new($source);
            let mut parser = Parser::new(lexer);
            let node = parser.parse_program();
            let mut codegen = Codegen::default();
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
        let expected_output = "function main() {\n    foo();\n}\n";
        assert_eq!(output, expected_output);
    }
}
