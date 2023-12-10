use crate::lexer::{Lexer, Literal, Token};

#[derive(Debug, Clone, Copy, PartialEq)]
enum Associativity {
    Left,
    Right,
}

#[derive(Debug, PartialEq)]
pub enum Node {
    Program(Vec<Node>),
    Literal(Literal),
    BinaryOp {
        op: BinaryOp,
        lhs: Box<Node>,
        rhs: Box<Node>,
    },
    VariableDeclaration {
        name: String,
        value: Box<Node>,
    },
    Identifier(String),
    Call {
        function: Box<Node>,
        arguments: Vec<Node>,
    },
}

impl<'a> From<&'a Token> for Node {
    fn from(token: &Token) -> Self {
        match token {
            Token::Literal(literal) => Node::Literal(literal.clone()),
            Token::Identifier(name) => Node::Identifier(name.clone()),
            _ => unimplemented!(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct OperatorInfo {
    precedence: u8,
    associativity: Associativity,
}

#[derive(Debug, PartialEq)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    And,
    Or,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
}

pub struct Parser<'src> {
    lexer: Lexer<'src>,
    current_token: Option<Token>,
}

impl<'src> Parser<'src> {
    pub fn new(lexer: Lexer<'src>) -> Parser {
        let mut parser = Parser {
            lexer,
            current_token: None,
        };
        parser.advance();
        parser
    }

    fn consume_token(&mut self, expected: Token) {
        let actual = self.current_token.as_ref().unwrap();
        if *actual != expected {
            panic!("Expected token {:?}, found {:?}", expected, actual);
        }
        self.advance();
    }

    fn consume_identifier(&mut self) -> String {
        let actual = self.current_token.as_ref().unwrap();
        match actual {
            Token::Identifier(name) => {
                let name = name.clone();
                self.advance();
                name
            }
            _ => {
                panic!("Expected identifier, found {:?}", actual);
            }
        }
    }

    fn advance(&mut self) {
        self.current_token = Some(self.lexer.next_token());
        println!("Advanced to {:?}", self.current_token);
    }

    fn parse_statement(&mut self) -> Node {
        println!("Parsing statement {:?}", self.current_token);

        let token = match self.current_token {
            Some(Token::Let) => self.parse_variable_declaration(),
            _ => self.parse_expression(),
        };

        self.consume_token(Token::Semicolon);

        token
    }

    pub fn parse_program(&mut self) -> Node {
        let mut statements = Vec::new();

        while self.current_token != Some(Token::Eof) {
            statements.push(self.parse_statement());
        }

        Node::Program(statements)
    }

    fn parse_variable_declaration(&mut self) -> Node {
        self.consume_token(Token::Let);
        let name = self.consume_identifier();
        self.consume_token(Token::EqualSign);
        let value = self.parse_expression();

        Node::VariableDeclaration {
            name,
            value: Box::new(value),
        }
    }

    fn parse_call_expression(&mut self, function: Node) -> Node {
        self.advance();

        let mut arguments = Vec::new();
        while self.current_token != Some(Token::RParen) {
            arguments.push(self.parse_expression());

            if let Some(Token::Comma) = self.current_token {
                self.advance();
            }
        }

        self.consume_token(Token::RParen);

        Node::Call {
            function: Box::new(function),
            arguments,
        }
    }

    fn parse_primary_expression(&mut self) -> Node {
        match &self.current_token {
            Some(Token::LParen) => {
                self.advance();
                let expression = self.parse_expression();
                self.consume_token(Token::RParen);
                expression
            }
            Some(token) => {
                let node: Node = token.into();

                self.advance();

                if let Node::Identifier(_) = node {
                    if let Some(Token::LParen) = self.current_token {
                        return self.parse_call_expression(node);
                    }
                }

                node
            }
            _ => panic!("Expected primary expression, found None"),
        }
    }

    fn is_binary_op(token: &Token) -> bool {
        matches!(
            token,
            Token::Plus
                | Token::Minus
                | Token::Asterisk
                | Token::Slash
                | Token::Percent
                | Token::Caret
                | Token::EqualEqual
                | Token::NotEqual
                | Token::LessThan
                | Token::LessThanOrEqual
                | Token::GreaterThan
                | Token::GreaterThanOrEqual
                | Token::Pipe
                | Token::Ampersand
                | Token::DoublePipe
                | Token::DoubleAmpersand
        )
    }

    fn map_binary_op(token: &Token) -> BinaryOp {
        match token {
            Token::Plus => BinaryOp::Add,
            Token::Minus => BinaryOp::Subtract,
            Token::Asterisk => BinaryOp::Multiply,
            Token::Slash => BinaryOp::Divide,
            Token::Percent => BinaryOp::Modulo,
            Token::EqualEqual => BinaryOp::Equal,
            Token::NotEqual => BinaryOp::NotEqual,
            Token::LessThan => BinaryOp::LessThan,
            Token::LessThanOrEqual => BinaryOp::LessThanOrEqual,
            Token::GreaterThan => BinaryOp::GreaterThan,
            Token::GreaterThanOrEqual => BinaryOp::GreaterThanOrEqual,
            Token::Pipe => BinaryOp::BitwiseOr,
            Token::Ampersand => BinaryOp::BitwiseAnd,
            Token::Caret => BinaryOp::BitwiseXor,
            Token::DoublePipe => BinaryOp::Or,
            Token::DoubleAmpersand => BinaryOp::And,
            _ => {
                unimplemented!("Expected binary operator, found {:?}", token)
            }
        }
    }

    fn map_operator_info(operator: &BinaryOp) -> OperatorInfo {
        match operator {
            BinaryOp::Add | BinaryOp::Subtract => OperatorInfo {
                precedence: 6,
                associativity: Associativity::Left,
            },
            BinaryOp::Multiply | BinaryOp::Divide | BinaryOp::Modulo => OperatorInfo {
                precedence: 7,
                associativity: Associativity::Left,
            },
            BinaryOp::Equal
            | BinaryOp::NotEqual
            | BinaryOp::LessThan
            | BinaryOp::LessThanOrEqual
            | BinaryOp::GreaterThan
            | BinaryOp::GreaterThanOrEqual => OperatorInfo {
                precedence: 5,
                associativity: Associativity::Left,
            },
            BinaryOp::And => OperatorInfo {
                precedence: 3,
                associativity: Associativity::Left,
            },
            BinaryOp::Or => OperatorInfo {
                precedence: 2,
                associativity: Associativity::Left,
            },
            BinaryOp::BitwiseAnd | BinaryOp::BitwiseOr | BinaryOp::BitwiseXor => OperatorInfo {
                precedence: 8,
                associativity: Associativity::Left,
            },
        }
    }

    fn compare_precedence(op1: &OperatorInfo, op2: &OperatorInfo) -> bool {
        if op1.precedence == op2.precedence {
            return op1.associativity == Associativity::Left;
        }

        op1.precedence > op2.precedence
    }

    fn parse_expression(&mut self) -> Node {
        self.parse_expression_with_precedence(0)
    }

    fn parse_expression_with_precedence(&mut self, precedence: u8) -> Node {
        let mut lhs = self.parse_primary_expression();

        while let Some(token) = self.current_token.as_ref() {
            if !Self::is_binary_op(token) {
                break;
            }

            let operator = Self::map_binary_op(token);
            let operator_info = Self::map_operator_info(&operator);

            if Self::compare_precedence(
                &OperatorInfo {
                    precedence,
                    associativity: Associativity::Left,
                },
                &operator_info,
            ) {
                break;
            }

            self.advance();

            let rhs = self.parse_expression_with_precedence(operator_info.precedence);

            lhs = Node::BinaryOp {
                op: operator,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            };
        }

        lhs
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;

    macro_rules! parse {
        ($source:expr) => {{
            let lexer = Lexer::new($source);
            let mut parser = Parser::new(lexer);
            parser.parse_program()
        }};
    }

    #[test]
    fn test_simple_variable_declaration() {
        let program = parse!("let x = 1 + 2;");

        assert_eq!(
            program,
            Node::Program(vec![Node::VariableDeclaration {
                name: "x".to_string(),
                value: Box::new(Node::BinaryOp {
                    op: BinaryOp::Add,
                    lhs: Box::new(Node::Literal(Literal::Integer(1))),
                    rhs: Box::new(Node::Literal(Literal::Integer(2))),
                }),
            }])
        );
    }

    #[test]
    fn test_simple_arithmetic_calculations() {
        let program = parse!("1 * 2 + 3;");
        assert_eq!(
            program,
            Node::Program(vec![Node::BinaryOp {
                op: BinaryOp::Add,
                lhs: Box::new(Node::BinaryOp {
                    op: BinaryOp::Multiply,
                    lhs: Box::new(Node::Literal(Literal::Integer(1))),
                    rhs: Box::new(Node::Literal(Literal::Integer(2))),
                }),
                rhs: Box::new(Node::Literal(Literal::Integer(3))),
            }])
        );
    }

    #[test]
    fn test_simple_arithmetic_sum_mult_precedence() {
        let program = parse!("1 + 2 * 3;");
        assert_eq!(
            program,
            Node::Program(vec![Node::BinaryOp {
                op: BinaryOp::Add,
                lhs: Box::new(Node::Literal(Literal::Integer(1))),
                rhs: Box::new(Node::BinaryOp {
                    op: BinaryOp::Multiply,
                    lhs: Box::new(Node::Literal(Literal::Integer(2))),
                    rhs: Box::new(Node::Literal(Literal::Integer(3))),
                }),
            }])
        );
    }

    #[test]
    fn test_simple_arithmetic_sum_mult_precedence_parentheses() {
        let program = parse!("(1 + 2) * 3;");
        assert_eq!(
            program,
            Node::Program(vec![Node::BinaryOp {
                op: BinaryOp::Multiply,
                lhs: Box::new(Node::BinaryOp {
                    op: BinaryOp::Add,
                    lhs: Box::new(Node::Literal(Literal::Integer(1))),
                    rhs: Box::new(Node::Literal(Literal::Integer(2))),
                }),
                rhs: Box::new(Node::Literal(Literal::Integer(3))),
            }])
        );
    }

    #[test]
    fn test_simple_arithmetic_with_identifiers() {
        let program = parse!("let x = 1; let y = 2; x + y;");
        assert_eq!(
            program,
            Node::Program(vec![
                Node::VariableDeclaration {
                    name: "x".to_string(),
                    value: Box::new(Node::Literal(Literal::Integer(1))),
                },
                Node::VariableDeclaration {
                    name: "y".to_string(),
                    value: Box::new(Node::Literal(Literal::Integer(2))),
                },
                Node::BinaryOp {
                    op: BinaryOp::Add,
                    lhs: Box::new(Node::Identifier("x".to_string())),
                    rhs: Box::new(Node::Identifier("y".to_string())),
                }
            ])
        );
    }

    #[test]
    fn test_simple_call() {
        let program = parse!("foo(1, 2);");
        assert_eq!(
            program,
            Node::Program(vec![Node::Call {
                function: Box::new(Node::Identifier("foo".to_string())),
                arguments: vec![
                    Node::Literal(Literal::Integer(1)),
                    Node::Literal(Literal::Integer(2))
                ],
            }])
        );
    }

    #[test]
    fn test_nested_call() {
        let program = parse!("foo(bar(1), 2);");
        assert_eq!(
            program,
            Node::Program(vec![Node::Call {
                function: Box::new(Node::Identifier("foo".to_string())),
                arguments: vec![
                    Node::Call {
                        function: Box::new(Node::Identifier("bar".to_string())),
                        arguments: vec![Node::Literal(Literal::Integer(1))]
                    },
                    Node::Literal(Literal::Integer(2))
                ],
            }])
        );
    }

    #[test]
    fn test_call_with_expression() {
        let program = parse!("foo(1 + 2, 3);");
        assert_eq!(
            program,
            Node::Program(vec![Node::Call {
                function: Box::new(Node::Identifier("foo".to_string())),
                arguments: vec![
                    Node::BinaryOp {
                        op: BinaryOp::Add,
                        lhs: Box::new(Node::Literal(Literal::Integer(1))),
                        rhs: Box::new(Node::Literal(Literal::Integer(2))),
                    },
                    Node::Literal(Literal::Integer(3))
                ],
            }])
        );
    }
}