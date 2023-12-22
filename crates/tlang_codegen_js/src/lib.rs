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
                self.generate_node(body, None);
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
                            matches!(**node, Node::Literal(_))
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
                    self.generate_node(body, None);
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
    use pretty_assertions::assert_eq;
    use tlang_parser::{lexer::Lexer, parser::Parser};

    macro_rules! compile {
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
        let output = compile!("let x = 42;");
        let expected_output = "let x = 42;\n";
        assert_eq!(output, expected_output);
    }

    #[test]
    fn test_codegen_binary_expression() {
        let output = compile!("let x = 42 + 1;");
        let expected_output = "let x = 42 + 1;\n";
        assert_eq!(output, expected_output);
    }

    #[test]
    fn test_codegen_function_declaration() {
        let output = compile!("fn main() {}");
        let expected_output = "function main() {\n}\n";
        assert_eq!(output, expected_output);
    }

    #[test]
    fn test_codegen_function_call() {
        let output = compile!("fn main() { foo(); }");
        let expected_output = indoc! {"
            function main() {
                foo();
            }
        "};
        assert_eq!(output, expected_output);
    }

    #[test]
    fn test_codegen_function_expression() {
        let output = compile!("fn main() { let foo = fn() { 1 + 2; }; }");
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
        let output = compile!("fn main() { return 42; }");
        let expected_output = indoc! {"
            function main() {
                return 42;
            }
        "};
        assert_eq!(output, expected_output);

        let output = compile!("fn main() { return; }");
        let expected_output = indoc! {"
            function main() {
                return;
            }
        "};
        assert_eq!(output, expected_output);
    }

    #[test]
    fn test_codegen_parenthesis_expression() {
        let output = compile!("let x = (42 + 1) * 2;");
        let expected_output = "let x = (42 + 1) * 2;\n";
        assert_eq!(output, expected_output);
    }

    #[test]
    fn test_codegen_operator_precedence() {
        let output = compile!("let result = 1 + 2 * 3;");
        let expected_output = "let result = 1 + 2 * 3;\n";
        assert_eq!(output, expected_output);
    }

    #[test]
    fn test_if_else() {
        let output = compile!("fn main() { if true { 1; } else { 2; } }");
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

    #[ignore = "implement if/else in expression position first"]
    #[test]
    fn test_if_else_as_expression() {
        let output = compile!("fn main() { let result = if true { 1 } else { 2 }; }");
        let expected_output = indoc! {"
            function main() {
                let tmp0;
                if (true) {
                    tmp0 = 1;
                } else {
                    tmp0 = 2;
                }
                let result = tmp0;
            }
        "};
        assert_eq!(output, expected_output);
    }

    #[test]
    fn test_recursive_function_definition() {
        let output = compile!(indoc! {"
            fn factorial(0) { return 1; }
            fn factorial(n) { return n * factorial(n - 1); }
        "});
        let expected_output = indoc! {"
            function factorial(...args) {
                if (args[0] === 0) {
                    return 1;
                } else {
                    let n = args[0];
                    return n * factorial(n - 1);
                }
            }
        "};
        assert_eq!(output, expected_output);

        let output = compile!(indoc! {"
            fn fibonacci(0) { return 0; }
            fn fibonacci(1) { return 1; }
            fn fibonacci(n) { return fibonacci(n - 1) + fibonacci(n - 2); }
        "});
        let expected_output = indoc! {"
            function fibonacci(...args) {
                if (args[0] === 0) {
                    return 0;
                } else if (args[0] === 1) {
                    return 1;
                } else {
                    let n = args[0];
                    return fibonacci(n - 1) + fibonacci(n - 2);
                }
            }
        "};
        assert_eq!(output, expected_output);
    }

    #[test]
    fn test_recursive_function_definition_multiple_with_multiple_args() {
        let output = compile!(indoc! {"
            fn gcd(0, n) { return n; }
            fn gcd(m, 0) { return m; }
            fn gcd(m, n) { return gcd(n, m % n); }
        "});
        let expected_output = indoc! {"
            function gcd(...args) {
                if (args[0] === 0) {
                    let n = args[1];
                    return n;
                } else if (args[1] === 0) {
                    let m = args[0];
                    return m;
                } else {
                    let m = args[0];
                    let n = args[1];
                    return gcd(n, m % n);
                }
            }
        "};
        assert_eq!(output, expected_output);
    }

    #[test]
    fn test_tail_recursive_factorial_nested() {
        let output = compile!(indoc! {"
            fn factorial(n) {
                fn factorial_rec(0, acc) { return acc; }
                fn factorial_rec(n, acc) { return factorial_rec(n - 1, n * acc); }

                return factorial_rec(n, 1);
            }
        "});
        let expected_output = indoc! {"
            function factorial(n) {
                function factorial_rec(...args) {
                    if (args[0] === 0) {
                        let acc = args[1];
                        return acc;
                    } else {
                        let n = args[0];
                        let acc = args[1];
                        return factorial_rec(n - 1, n * acc);
                    }
                }
                return factorial_rec(n, 1);
            }
        "};
        assert_eq!(output, expected_output);
    }

    #[test]
    fn test_tail_recursive_factorial_idiomatic() {
        let output = compile!(indoc! {"
            fn factorial(n) { return factorial(n, 1); }
            fn factorial(0, acc) { return acc; }
            fn factorial(n, acc) { return factorial(n - 1, n * acc); }
        "});
        let expected_output = indoc! {"
            function factorial(...args) {
                if (args.length === 1) {
                    let n = args[0];
                    return factorial(n, 1);
                } else if (args.length === 2 && args[0] === 0) {
                    let acc = args[1];
                    return acc;
                } else if (args.length === 2) {
                    let n = args[0];
                    let acc = args[1];
                    return factorial(n - 1, n * acc);
                }
            }
        "};
        assert_eq!(output, expected_output);
    }

    #[test]
    fn test_pipeline_operator() {
        let output = compile!("fn main() { 1 |> log; }");
        let expected_output = indoc! {"
            function main() {
                log(1);
            }
        "};
        assert_eq!(output, expected_output);

        let output = compile!("fn main() { 1 |> foo |> bar; }");
        let expected_output = indoc! {"
            function main() {
                bar(foo(1));
            }
        "};
        assert_eq!(output, expected_output);
    }

    #[test]
    fn test_pipeline_operator_with_call_parenthesis() {
        let output = compile!("fn main() { 1 |> foo(); }");
        let expected_output = indoc! {"
            function main() {
                foo(1);
            }
        "};
        assert_eq!(output, expected_output);
    }

    #[test]
    fn test_pipeline_operator_with_call_parenthesis_and_arguments() {
        let output = compile!("fn main() { 1 |> foo(2); }");
        let expected_output = indoc! {"
            function main() {
                foo(1, 2);
            }
        "};
        assert_eq!(output, expected_output);

        let output = compile!("fn main() { 1 |> foo(2, 3); }");
        let expected_output = indoc! {"
            function main() {
                foo(1, 2, 3);
            }
        "};
        assert_eq!(output, expected_output);
    }

    #[test]
    fn test_pipeline_operator_to_function_call_with_wildcards() {
        let output = compile!("fn main() { 1 |> foo(2, _); }");
        let expected_output = indoc! {"
            function main() {
                foo(2, 1);
            }
        "};
        assert_eq!(output, expected_output);
    }
}
