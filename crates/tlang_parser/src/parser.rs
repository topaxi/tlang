use crate::{
    ast::{Associativity, BinaryOp, Node, OperatorInfo, PrefixOp},
    lexer::{Lexer, Token},
};
use log::debug;

pub struct Parser<'src> {
    lexer: Lexer<'src>,
    current_token: Option<Token>,
    next_token: Option<Token>,
}

impl<'src> Parser<'src> {
    pub fn new(lexer: Lexer<'src>) -> Parser {
        Parser {
            lexer,
            current_token: None,
            next_token: None,
        }
    }

    pub fn from_source(source: &'src str) -> Parser<'src> {
        Parser::new(Lexer::new(source))
    }

    pub fn parse(&mut self) -> Node {
        self.advance();
        self.advance();
        self.parse_program()
    }

    fn expect_token(&mut self, expected: Token) {
        let actual = self.current_token.as_ref().unwrap();
        if *actual != expected {
            panic!(
                "Expected {:?} on line {}, column {}, found {:?} instead\n{}\n{}",
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
        self.current_token = self.next_token.clone();
        self.next_token = Some(self.lexer.next_token());
        debug!("Advanced to {:?}", self.current_token);
    }

    pub fn peek_token(&self) -> Option<&Token> {
        self.next_token.as_ref()
    }

    fn parse_statement(&mut self) -> (bool, Option<Node>) {
        debug!("Parsing statement {:?}", self.current_token);

        // Skip stray semicolons.
        if let Some(Token::Semicolon) = self.current_token {
            self.advance();

            return (false, None);
        }

        let node = match self.current_token {
            Some(Token::Let) => self.parse_variable_declaration(),
            Some(Token::Fn) => self.parse_function_declaration(),
            Some(Token::Return) => self.parse_return_statement(),
            Some(Token::Enum) => todo!(),
            Some(Token::SingleLineComment(_) | Token::MultiLineComment(_)) => {
                let comment: Node = self.current_token.as_ref().unwrap().into();
                self.advance();
                return (false, Some(comment));
            }
            _ => Node::ExpressionStatement(Box::new(self.parse_expression())),
        };

        // FunctionDeclaration statements does not need to be terminated with a semicolon.
        match node {
            Node::FunctionDeclaration { .. } | Node::FunctionDeclarations(_, _) => {
                return (false, Some(node))
            }
            _ => (),
        }

        // Expressions like IfElse as statements also do not need to be terminated with a semicolon.
        if let Node::ExpressionStatement(ref expr) = node {
            if let Node::IfElse { .. } = **expr {
                return (false, Some(node));
            }
        }

        (true, Some(node))
    }

    fn parse_return_statement(&mut self) -> Node {
        self.consume_token(Token::Return);

        if self.current_token == Some(Token::Semicolon) {
            return Node::ReturnStatement(None);
        }

        Node::ReturnStatement(Some(Box::new(self.parse_expression())))
    }

    fn parse_statements(&mut self, may_complete: bool) -> (Vec<Node>, Option<Box<Node>>) {
        let mut statements = Vec::new();
        let mut completion_expression = None;

        while self.current_token != Some(Token::RBrace) && self.current_token != Some(Token::Eof) {
            if let (consume_semicolon, Some(statement)) = self.parse_statement() {
                if may_complete
                    && self.current_token == Some(Token::RBrace)
                    && matches!(&statement, Node::ExpressionStatement(_))
                {
                    let expression = match statement {
                        Node::ExpressionStatement(expr) => expr,
                        _ => unreachable!(),
                    };
                    completion_expression = Some(expression);
                    break;
                }

                statements.push(statement);

                if consume_semicolon {
                    self.consume_token(Token::Semicolon);
                }
            }
        }

        (statements, completion_expression)
    }

    fn parse_program(&mut self) -> Node {
        Node::Program(self.parse_statements(false).0)
    }

    fn parse_block(&mut self) -> Node {
        self.consume_token(Token::LBrace);
        let (statements, completion) = self.parse_statements(true);
        self.consume_token(Token::RBrace);

        Node::Block(statements, completion)
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

    fn parse_list_extraction(&mut self) -> Node {
        self.consume_token(Token::LBracket);

        let mut elements = Vec::new();
        // Only allow literal values, identifiers and and rest params for now.
        while self.current_token != Some(Token::RBracket) {
            let element = match self.current_token {
                Some(Token::Identifier(_) | Token::Literal(_) | Token::LBracket) => {
                    self.parse_primary_expression()
                }
                Some(Token::DotDotDot) => {
                    self.advance();
                    Node::PrefixOp(PrefixOp::Rest, Box::new(self.parse_primary_expression()))
                }
                _ => panic!(
                    "Expected identifier or literal on line {}, column {}, found {:?} instead\n{}\n{}",
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
            };

            elements.push(element);

            // Allow trailing comma.
            if self.current_token == Some(Token::RBracket) {
                break;
            }

            self.consume_token(Token::Comma);
        }

        self.consume_token(Token::RBracket);

        Node::List(elements)
    }

    fn parse_list_expression(&mut self) -> Node {
        self.consume_token(Token::LBracket);

        let mut elements = Vec::new();
        while self.current_token != Some(Token::RBracket) {
            let element = match self.current_token {
                Some(Token::DotDotDot) => {
                    self.advance();

                    Node::PrefixOp(PrefixOp::Spread, Box::new(self.parse_primary_expression()))
                }
                _ => self.parse_expression(),
            };

            elements.push(element);

            // Allow trailing comma.
            if self.current_token == Some(Token::RBracket) {
                break;
            }

            self.consume_token(Token::Comma);
        }

        self.consume_token(Token::RBracket);

        Node::List(elements)
    }

    fn parse_primary_expression(&mut self) -> Node {
        match &self.current_token {
            Some(Token::LParen) => {
                self.advance();
                let expression = self.parse_expression();
                self.consume_token(Token::RParen);
                expression
            }
            Some(Token::LBrace) => self.parse_block(),
            Some(Token::LBracket) => self.parse_list_expression(),
            Some(Token::If) => {
                self.advance();
                let condition = self.parse_expression();
                self.expect_token(Token::LBrace);
                let then_branch = self.parse_block();
                let else_branch = if let Some(Token::Else) = self.current_token {
                    self.advance();
                    Some(Box::new(self.parse_block()))
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
                parameters.push(Node::FunctionParameter(Box::new(Node::Identifier(
                    self.consume_identifier(),
                ))));

                if let Some(Token::Comma) = self.current_token {
                    self.advance();
                }
            }
            self.consume_token(Token::RParen);
        }

        let body = self.parse_block();
        Node::FunctionExpression {
            name,
            parameters,
            body: Box::new(body),
        }
    }

    fn parse_function_declaration(&mut self) -> Node {
        let mut name: Option<String> = None;
        let mut definitions: Vec<(Vec<Node>, Box<Node>)> = Vec::new();

        while let Some(Token::Fn) = self.current_token {
            if let Some(Token::Identifier(current_definition_name)) = self.peek_token() {
                if name.is_some() && name.as_ref().unwrap() != current_definition_name {
                    break;
                }
            }

            self.advance();
            let current_definition_name = self.consume_identifier();

            if name.is_none() {
                name = Some(current_definition_name.clone());
            }

            self.consume_token(Token::LParen);

            let mut parameters = Vec::new();
            while self.current_token != Some(Token::RParen) {
                let parameter = match self.current_token {
                Some(Token::Identifier(_) | Token::Literal(_)) => self.parse_primary_expression(),
                Some(Token::LBracket) => self.parse_list_extraction(),
                _ => panic!(
                    "Expected identifier or literal on line {}, column {}, found {:?} instead\n{}\n{}",
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
            };

                parameters.push(Node::FunctionParameter(Box::new(parameter)));

                if let Some(Token::Comma) = self.current_token {
                    self.advance();
                }
            }
            self.consume_token(Token::RParen);

            let body = self.parse_block();

            definitions.push((parameters, Box::new(body)));
        }

        if definitions.len() == 1 {
            let (parameters, body) = definitions.pop().unwrap();

            return Node::FunctionDeclaration {
                name: name.unwrap(),
                parameters,
                body,
            };
        }

        Node::FunctionDeclarations(name.unwrap(), definitions)
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
                | Token::Pipeline
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
            Token::Pipeline => BinaryOp::Pipeline,
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
            BinaryOp::Pipeline => OperatorInfo {
                precedence: 1,
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
