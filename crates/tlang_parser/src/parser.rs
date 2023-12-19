use crate::lexer::{Lexer, Literal, Token};
use log::debug;

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
    ExpressionStatement(Box<Node>),
    VariableDeclaration {
        name: String,
        value: Box<Node>,
    },
    FunctionDeclaration {
        name: String,
        parameters: Vec<String>,
        body: Box<Node>,
    },
    FunctionExpression {
        name: Option<String>,
        parameters: Vec<String>,
        body: Box<Node>,
    },
    Match {
        expression: Box<Node>,
        arms: Vec<Node>,
    },
    MatchArm {
        pattern: Box<Node>,
        expression: Box<Node>,
    },
    Wildcard,
    IfElse {
        condition: Box<Node>,
        then_branch: Box<Node>,
        else_branch: Option<Box<Node>>,
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
            _ => unimplemented!(
                "Expected token to be a literal or identifier, found {:?}",
                token
            ),
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
    Exponentiation,
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

    fn expect_token(&mut self, expected: Token) {
        let actual = self.current_token.as_ref().unwrap();
        if *actual != expected {
            panic!(
                "Exprected {:?} on line {}, column {}, found {:?} instead\n{}\n{}",
                expected,
                self.lexer.current_line(),
                self.lexer.current_column(),
                actual,
                self.lexer
                    .source()
                    .lines()
                    .nth(self.lexer.current_line() - 1)
                    .unwrap(),
                " ".repeat(self.lexer.current_column() - 1) + "^"
            );
        }
    }

    fn consume_token(&mut self, expected: Token) {
        self.expect_token(expected);
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
                panic!(
                    "Expected identifier on line {}, column {}, found {:?} instead\n{}\n{}",
                    self.lexer.current_line(),
                    self.lexer.current_column(),
                    actual,
                    self.lexer
                        .source()
                        .lines()
                        .nth(self.lexer.current_line() - 1)
                        .unwrap(),
                    " ".repeat(self.lexer.current_column() - 1) + "^"
                );
            }
        }
    }

    fn advance(&mut self) {
        self.current_token = Some(self.lexer.next_token());
        debug!("Advanced to {:?}", self.current_token);
    }

    fn parse_statement(&mut self) -> Option<Node> {
        debug!("Parsing statement {:?}", self.current_token);

        // Skip stray semicolons.
        if let Some(Token::Semicolon) = self.current_token {
            self.advance();

            return None;
        }

        let node = match self.current_token {
            Some(Token::Let) => self.parse_variable_declaration(),
            Some(Token::Fn) => self.parse_function_declaration(),
            _ => Node::ExpressionStatement(Box::new(self.parse_expression())),
        };

        // FunctionDeclarations statements does not need to be terminated with a semicolon.
        if let Node::FunctionDeclaration { .. } = node {
            return Some(node);
        }

        // Expressions like IfElse as statements also do not need to be terminated with a semicolon.
        if let Node::ExpressionStatement(ref expr) = node {
            if let Node::IfElse { .. } = **expr {
                return Some(node);
            }
        }

        self.consume_token(Token::Semicolon);

        Some(node)
    }

    pub fn parse_program(&mut self) -> Node {
        let mut statements = Vec::new();

        while self.current_token != Some(Token::RBrace) && self.current_token != Some(Token::Eof) {
            let statement = self.parse_statement();

            if let Some(statement) = statement {
                statements.push(statement);
            }
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
            Some(Token::LBrace) => {
                self.advance();
                let token = self.parse_program();
                self.consume_token(Token::RBrace);
                token
            }
            Some(Token::If) => {
                self.advance();
                let condition = self.parse_expression();
                self.expect_token(Token::LBrace);
                let then_branch = self.parse_primary_expression();
                let else_branch = if let Some(Token::Else) = self.current_token {
                    self.advance();
                    self.expect_token(Token::LBrace);
                    Some(Box::new(self.parse_primary_expression()))
                } else {
                    None
                };
                Node::IfElse {
                    condition: Box::new(condition),
                    then_branch: Box::new(then_branch),
                    else_branch,
                }
            }
            Some(Token::Fn) => self.parse_function_expression(),
            Some(Token::Match) => self.parse_match_expression(),
            Some(token) => {
                let node: Node = token.into();

                self.advance();

                if let Node::Identifier(identifier) = &node {
                    if identifier == "_" {
                        return Node::Wildcard;
                    }

                    if let Some(Token::LParen) = self.current_token {
                        return self.parse_call_expression(node);
                    }
                }

                node
            }
            _ => panic!(
                "Expected primary expression on line {}, column {}, found {:?} instead\n{}\n{}",
                self.lexer.current_line(),
                self.lexer.current_column(),
                self.current_token,
                self.lexer
                    .source()
                    .lines()
                    .nth(self.lexer.current_line() - 1)
                    .unwrap(),
                " ".repeat(self.lexer.current_column() - 1) + "^"
            ),
        }
    }

    fn parse_match_expression(&mut self) -> Node {
        self.consume_token(Token::Match);
        let expression = self.parse_expression();
        self.consume_token(Token::LBrace);

        let mut arms = Vec::new();
        while self.current_token != Some(Token::RBrace) {
            let pattern = self.parse_expression();
            self.consume_token(Token::FatArrow);
            let expression = self.parse_expression();
            arms.push(Node::MatchArm {
                pattern: Box::new(pattern),
                expression: Box::new(expression),
            });
            if let Some(Token::Comma) = self.current_token {
                self.advance();
            }
        }

        self.consume_token(Token::RBrace);

        Node::Match {
            expression: Box::new(expression),
            arms,
        }
    }

    fn parse_function_expression(&mut self) -> Node {
        self.consume_token(Token::Fn);
        let name = if let Some(Token::Identifier(name)) = &self.current_token {
            let name = name.to_owned();
            self.advance();
            Some(name)
        } else {
            None
        };

        let mut parameters = Vec::new();
        if name.is_some() || self.current_token == Some(Token::LParen) {
            self.consume_token(Token::LParen);
            while self.current_token != Some(Token::RParen) {
                parameters.push(self.consume_identifier());
                if let Some(Token::Comma) = self.current_token {
                    self.advance();
                }
            }
            self.consume_token(Token::RParen);
        }

        self.expect_token(Token::LBrace);
        let body = self.parse_primary_expression();
        Node::FunctionExpression {
            name,
            parameters,
            body: Box::new(body),
        }
    }

    fn parse_function_declaration(&mut self) -> Node {
        self.consume_token(Token::Fn);
        let name = self.consume_identifier();
        self.consume_token(Token::LParen);
        let mut parameters = Vec::new();
        while self.current_token != Some(Token::RParen) {
            parameters.push(self.consume_identifier());
            if let Some(Token::Comma) = self.current_token {
                self.advance();
            }
        }
        self.consume_token(Token::RParen);
        self.expect_token(Token::LBrace);
        let body = self.parse_primary_expression();
        Node::FunctionDeclaration {
            name,
            parameters,
            body: Box::new(body),
        }
    }

    fn is_binary_op(token: &Token) -> bool {
        matches!(
            token,
            Token::Plus
                | Token::Minus
                | Token::Asterisk
                | Token::AsteriskAsterisk
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
            Token::AsteriskAsterisk => BinaryOp::Exponentiation,
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
            BinaryOp::Exponentiation => OperatorInfo {
                precedence: 9,
                associativity: Associativity::Right,
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
        self.parse_expression_with_precedence(0, Associativity::Left)
    }

    fn parse_expression_with_precedence(
        &mut self,
        precedence: u8,
        associativity: Associativity,
    ) -> Node {
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
                    associativity,
                },
                &operator_info,
            ) {
                break;
            }

            self.advance();

            let rhs = self.parse_expression_with_precedence(
                operator_info.precedence,
                operator_info.associativity,
            );

            lhs = Node::BinaryOp {
                op: operator,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            };
        }

        lhs
    }
}
