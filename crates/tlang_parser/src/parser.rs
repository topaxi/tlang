use crate::lexer::{Lexer, Literal, Token};

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
}

#[derive(Debug, PartialEq)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Power,
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

    fn parse_primary_expression(&mut self) -> Node {
        println!("Parsing primary expression {:?}", self.current_token);

        match &self.current_token {
            Some(Token::Literal(literal)) => {
                let literal = literal.clone();
                self.advance();
                Node::Literal(literal)
            }
            _ => {
                unimplemented!()
            }
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
            Token::Caret => BinaryOp::Power,
            Token::EqualEqual => BinaryOp::Equal,
            Token::NotEqual => BinaryOp::NotEqual,
            Token::LessThan => BinaryOp::LessThan,
            Token::LessThanOrEqual => BinaryOp::LessThanOrEqual,
            Token::GreaterThan => BinaryOp::GreaterThan,
            Token::GreaterThanOrEqual => BinaryOp::GreaterThanOrEqual,
            Token::Pipe => BinaryOp::BitwiseOr,
            Token::Ampersand => BinaryOp::BitwiseAnd,
            Token::DoublePipe => BinaryOp::Or,
            Token::DoubleAmpersand => BinaryOp::And,
            _ => {
                unimplemented!("Expected binary operator, found {:?}", token)
            }
        }
    }

    fn get_precedence(token: &Token) -> u8 {
        match token {
            Token::Pipe => 1,
            Token::DoublePipe => 2,
            Token::Ampersand => 3,
            Token::DoubleAmpersand => 4,
            Token::EqualEqual
            | Token::NotEqual
            | Token::LessThan
            | Token::LessThanOrEqual
            | Token::GreaterThan
            | Token::GreaterThanOrEqual => 5,
            Token::Plus | Token::Minus => 6,
            Token::Asterisk | Token::Slash | Token::Percent => 7,
            Token::Caret => 8,
            _ => {
                unimplemented!("Expected operator, found {:?}", token)
            }
        }
    }

    fn parse_expression(&mut self) -> Node {
        let lhs = self.parse_primary_expression();

        println!("Parsing expression {:?}", self.current_token);
        println!(
            "Is binary op? {:?}",
            Self::is_binary_op(self.current_token.as_ref().unwrap())
        );

        match &self.current_token {
            _ if Self::is_binary_op(self.current_token.as_ref().unwrap()) => {
                let token = self.current_token.as_ref().unwrap().clone();
                self.advance();
                let rhs = self.parse_expression();
                Node::BinaryOp {
                    op: Self::map_binary_op(&token),
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                }
            }
            Some(Token::Semicolon) => lhs,
            _ => {
                unimplemented!("Expected operator, found {:?}", self.current_token)
            }
        }
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

    #[ignore = "not implemented yet"]
    #[test]
    fn test_simple_arithmetic_sum_mult_precedence() {
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
}
