use tlang_ast::node::{
    self, Associativity, AstNode, BinaryOp, FunctionDeclaration, Node, OperatorInfo, UnaryOp,
};
use tlang_ast::span::Span;
use tlang_ast::symbols::SymbolId;
use tlang_ast::token::{Token, TokenKind};

use crate::error::{ParseError, ParseErrorKind};
use crate::lexer::Lexer;
use log::debug;

pub struct Parser<'src> {
    lexer: Lexer<'src>,
    previous_token: Option<Token>,
    current_token: Option<Token>,
    next_token: Option<Token>,

    // Id to identifiy symbols, e.g. functions and variables.
    unique_id: SymbolId,

    // We have to copy over the lexers position as we scan ahead of the current token.
    current_line: usize,
    current_column: usize,

    errors: Vec<ParseError>,
}

impl<'src> Parser<'src> {
    pub fn new(lexer: Lexer<'src>) -> Parser {
        Parser {
            lexer,
            previous_token: None,
            current_token: None,
            next_token: None,
            unique_id: SymbolId::new(0),
            current_line: 0,
            current_column: 0,
            errors: Vec::new(),
        }
    }

    pub fn errors(&self) -> Vec<ParseError> {
        self.errors.clone()
    }

    fn unique_id(&mut self) -> SymbolId {
        self.unique_id = self.unique_id.next();
        self.unique_id
    }

    pub fn from_source(source: &'src str) -> Parser<'src> {
        Parser::new(Lexer::new(source))
    }

    pub fn parse(&mut self) -> Result<Node, Vec<ParseError>> {
        self.advance();
        self.advance();

        let program = self.parse_program();

        if self.errors().is_empty() {
            Ok(program)
        } else {
            Err(self.errors())
        }
    }

    fn create_span_from_current_token(&self) -> Span {
        Span::from_start_token(self.current_token.as_ref().unwrap())
    }

    fn end_span_from_previous_token(&self, span: &mut Span) {
        span.end_by_token(self.previous_token.as_ref().unwrap());
    }

    fn push_unexpected_token_error(&mut self, expected: &str, actual: Option<Token>) {
        self.errors.push(ParseError {
            msg: format!("Expected {}, found {:?}", expected, actual),
            kind: ParseErrorKind::UnexpectedToken(actual.clone().unwrap()),
            span: actual.as_ref().unwrap().span.clone(),
        });
    }

    fn panic_unexpected_token(&self, expected: &str, actual: Option<Token>) {
        let token = actual.as_ref().unwrap();
        let start_span = &token.span.start;
        let source_line = self.lexer.source().lines().nth(start_span.line).unwrap();
        let caret = " ".repeat(start_span.column) + "^";

        panic!(
            "Expected {} on line {}, column {}, found {:?} instead\n{}\n{}",
            expected, start_span.line, start_span.column, token.kind, source_line, caret
        );
    }

    fn panic_unexpected_node(&self, expected: &str, actual: Option<Node>) {
        let node = actual.as_ref().unwrap();
        let start_span = &node.span.start;
        let source_line = self.lexer.source().lines().nth(start_span.line).unwrap();
        let caret = " ".repeat(start_span.column) + "^";

        panic!(
            "Expected {} on line {}, column {}, found {:?} instead\n{}\n{}",
            expected, start_span.line, start_span.column, node.ast_node, source_line, caret
        );
    }

    fn expect_token(&mut self, expected: TokenKind) {
        let actual = self.current_token.as_ref().unwrap();
        if actual.kind != expected {
            self.push_unexpected_token_error(&format!("{:?}", expected), Some(actual.clone()));

            while let Some(token) = self.current_token.as_ref() {
                if token.kind == expected {
                    break;
                }

                self.advance();
            }
        }
    }

    fn consume_token(&mut self, expected: TokenKind) {
        self.expect_token(expected);
        self.advance();
    }

    fn consume_identifier(&mut self) -> String {
        let actual = self.current_token.as_ref().unwrap();
        match &actual.kind {
            TokenKind::Identifier(name) => {
                let name = name.clone();
                self.advance();
                name
            }
            _ => {
                self.push_unexpected_token_error("identifier", Some(actual.clone()));

                while let Some(token) = self.current_token.as_ref() {
                    if let TokenKind::Identifier(name) = &token.kind {
                        let name = name.clone();
                        self.advance();
                        return name;
                    }

                    self.advance();
                }

                unreachable!()
            }
        }
    }

    fn advance(&mut self) {
        if self.current_token_kind() == Some(TokenKind::Eof)
            && self.peek_token_kind() == Some(TokenKind::Eof)
        {
            // In error recovery we scan ahead and look for the expected token.
            // If we reach the end of the file, we don't want to continue scanning and
            // break our `while let Some(_) = self.current_token` loop.
            self.current_token = None;
            self.next_token = None;

            return;
        }

        self.previous_token = self.current_token.clone();
        self.current_token = self.next_token.clone();
        self.current_column = self.lexer.current_column();
        self.current_line = self.lexer.current_line();
        self.next_token = Some(self.lexer.next_token());
        debug!("Advanced to {:?}", self.current_token);
    }

    #[inline(always)]
    fn current_token_kind(&self) -> Option<TokenKind> {
        self.current_token.as_ref().map(|token| token.kind.clone())
    }

    #[inline(always)]
    pub fn peek_token_kind(&self) -> Option<TokenKind> {
        self.next_token.as_ref().map(|token| token.kind.clone())
    }

    fn save_state(&self) -> (Lexer<'src>, Option<Token>, Option<Token>) {
        (
            self.lexer.clone(),
            self.current_token.clone(),
            self.next_token.clone(),
        )
    }

    fn restore_state(&mut self, state: (Lexer<'src>, Option<Token>, Option<Token>)) {
        self.lexer = state.0;
        self.current_token = state.1;
        self.next_token = state.2;
    }

    fn parse_statement(&mut self) -> (bool, Option<Node>) {
        debug!("Parsing statement {:?}", self.current_token);

        // Skip stray semicolons.
        if let Some(TokenKind::Semicolon) = self.current_token_kind() {
            self.advance();

            return (false, None);
        }

        let mut span = self.create_span_from_current_token();
        let mut node = match self.current_token_kind() {
            Some(TokenKind::Let) => self.parse_variable_declaration(),
            Some(TokenKind::Fn) => self.parse_function_declaration(),
            Some(TokenKind::Return) => self.parse_return_statement(),
            Some(TokenKind::Enum) => self.parse_enum_declaration(),
            Some(TokenKind::SingleLineComment(_) | TokenKind::MultiLineComment(_)) => {
                return (false, Some(self.parse_comment()));
            }
            _ => node::new!(ExpressionStatement(Box::new(self.parse_expression()))),
        };
        self.end_span_from_previous_token(&mut span);
        node.span = span;

        // FunctionDeclaration statements does not need to be terminated with a semicolon.
        match node.ast_node {
            AstNode::FunctionSingleDeclaration { .. } | AstNode::FunctionDeclarations { .. } => {
                return (false, Some(node))
            }
            _ => (),
        }

        // Neither do EnumDeclaration statements.
        if let AstNode::EnumDeclaration { .. } = node.ast_node {
            return (false, Some(node));
        }

        // Expressions like IfElse as statements also do not need to be terminated with a semicolon.
        if let AstNode::ExpressionStatement(ref expr) = node.ast_node {
            if let AstNode::IfElse { .. } = expr.ast_node {
                return (false, Some(node));
            }
        }

        (true, Some(node))
    }

    fn parse_comment(&mut self) -> Node {
        let comment: Node = self.current_token.as_ref().unwrap().into();
        self.advance();
        comment
    }

    fn parse_return_statement(&mut self) -> Node {
        self.consume_token(TokenKind::Return);

        if self.current_token_kind() == Some(TokenKind::Semicolon) {
            return node::new!(ReturnStatement(Box::new(None)));
        }

        node::new!(ReturnStatement(Box::new(Some(self.parse_expression()))))
    }

    fn parse_single_identifier(&mut self) -> Node {
        // How do we generalize this? Callbacks come to mind, but I'd rather not.
        // Adding another stack is also not ideal.
        let mut span = self.create_span_from_current_token();
        let identifier = self.consume_identifier();
        self.end_span_from_previous_token(&mut span);
        node::new!(Identifier(identifier), span)
    }

    fn parse_identifier(&mut self) -> Node {
        let mut span = self.create_span_from_current_token();
        let mut identifiers = vec![self.consume_identifier()];
        while let Some(TokenKind::NamespaceSeparator) = self.current_token_kind() {
            self.advance();
            identifiers.push(self.consume_identifier());
        }

        self.end_span_from_previous_token(&mut span);

        if identifiers.len() == 1 {
            node::new!(Identifier(identifiers.pop().unwrap()), span)
        } else {
            node::new!(NestedIdentifier(identifiers), span)
        }
    }

    fn parse_enum_declaration(&mut self) -> Node {
        self.consume_token(TokenKind::Enum);
        let name = self.consume_identifier();
        self.consume_token(TokenKind::LBrace);
        let mut variants = Vec::new();
        while self.current_token_kind() != Some(TokenKind::RBrace) {
            variants.push(self.parse_enum_variant());
            if let Some(TokenKind::Comma) = self.current_token_kind() {
                self.advance();
            }
        }
        self.consume_token(TokenKind::RBrace);
        node::new!(EnumDeclaration {
            id: self.unique_id(),
            name: name,
            variants: variants,
        })
    }

    /// Parses an enum variant, e.g. `Foo`, `Foo(1, 2, 3)` and
    /// `Foo { bar, baz }`.
    fn parse_enum_variant(&mut self) -> Node {
        let mut span = self.create_span_from_current_token();
        let name = self.consume_identifier();
        log::debug!("Parsing enum variant {}", name);
        let mut node: Node = match self.current_token_kind() {
            Some(TokenKind::LParen) => {
                self.advance();
                let mut parameters = Vec::new();
                while self.current_token_kind() != Some(TokenKind::RParen) {
                    parameters.push(node::new!(Identifier(self.consume_identifier())));
                    if let Some(TokenKind::Comma) = self.current_token_kind() {
                        self.advance();
                    }
                }
                self.consume_token(TokenKind::RParen);
                AstNode::EnumVariant {
                    name,
                    named_fields: false,
                    parameters,
                }
                .into()
            }
            Some(TokenKind::LBrace) => {
                self.advance();
                let mut parameters = Vec::new();
                while self.current_token_kind() != Some(TokenKind::RBrace) {
                    parameters.push(node::new!(Identifier(self.consume_identifier())));
                    if let Some(TokenKind::Comma) = self.current_token_kind() {
                        self.advance();
                    }
                }
                self.consume_token(TokenKind::RBrace);
                AstNode::EnumVariant {
                    name,
                    named_fields: true,
                    parameters,
                }
                .into()
            }
            _ => AstNode::EnumVariant {
                name,
                named_fields: false,
                parameters: Vec::new(),
            }
            .into(),
        };

        self.end_span_from_previous_token(&mut span);
        node.span = span;
        node
    }

    fn parse_statements(&mut self, may_complete: bool) -> (Vec<Node>, Box<Option<Node>>) {
        let mut statements = Vec::new();
        let mut completion_expression = Box::new(None);

        while self.current_token_kind() != Some(TokenKind::RBrace)
            && self.current_token_kind() != Some(TokenKind::Eof)
        {
            if let (consume_semicolon, Some(mut statement)) = self.parse_statement() {
                if may_complete
                    && self.current_token_kind() == Some(TokenKind::RBrace)
                    && matches!(&statement.ast_node, AstNode::ExpressionStatement(_))
                {
                    let mut expression = match statement.ast_node {
                        AstNode::ExpressionStatement(expr) => expr,
                        _ => unreachable!(),
                    };
                    self.end_span_from_previous_token(&mut statement.span);
                    expression.span = statement.span;
                    completion_expression = Box::new(Some(*expression));
                    break;
                }

                if consume_semicolon {
                    self.consume_token(TokenKind::Semicolon);
                    // TODO: Do we count the semicolon as part of the statement?
                    self.end_span_from_previous_token(&mut statement.span);
                }

                statements.push(statement);
            }
        }

        (statements, completion_expression)
    }

    fn parse_program(&mut self) -> Node {
        node::new!(Program(self.parse_statements(false).0))
    }

    fn parse_block(&mut self) -> Node {
        self.consume_token(TokenKind::LBrace);
        let (statements, completion) = self.parse_statements(true);
        self.consume_token(TokenKind::RBrace);

        node::new!(Block(statements, completion))
    }

    fn parse_variable_declaration_pattern(&mut self) -> Node {
        match self.current_token_kind() {
            // Literal values will be pattern matched
            Some(TokenKind::Literal(_)) => self.parse_primary_expression(),
            Some(TokenKind::Identifier(ref identifier)) if identifier == "_" => {
                self.advance();
                node::new!(Wildcard)
            }
            // Identifiers can be namespaced identifiers or call expressions, which should be pattern matched.
            // Simple identifiers will be used as is and mapped to a function parameter.
            Some(TokenKind::Identifier(_)) => {
                if let Some(TokenKind::NamespaceSeparator) = self.peek_token_kind() {
                    self.parse_enum_extraction()
                } else if let Some(TokenKind::LParen | TokenKind::LBrace) = self.peek_token_kind() {
                    self.parse_enum_extraction()
                } else {
                    node::new!(Identifier(self.consume_identifier()))
                }
            }
            Some(TokenKind::LBracket) => self.parse_list_extraction(),
            _ => {
                self.panic_unexpected_token(
                    "literal, identifier or list extraction",
                    self.current_token.clone(),
                );
                unreachable!()
            }
        }
    }

    fn parse_variable_declaration(&mut self) -> Node {
        self.consume_token(TokenKind::Let);

        // We probably want to know whether we are parsing a plain declaration of `if let`.
        let pattern = self.parse_variable_declaration_pattern();

        let type_annotation = match self.current_token_kind() {
            Some(TokenKind::Colon) => {
                self.advance();
                self.parse_type_annotation()
            }
            _ => None,
        };
        self.consume_token(TokenKind::EqualSign);
        let value = self.parse_expression();

        node::new!(VariableDeclaration {
            id: self.unique_id(),
            pattern: Box::new(pattern),
            expression: Box::new(value),
            type_annotation: Box::new(type_annotation),
        })
    }

    /// Parses a function call expression, e.g. `foo()`, `foo(1, 2, 3)` and
    /// `foo { bar, baz }`.
    fn parse_call_expression(&mut self, function: Node) -> Node {
        log::debug!(
            "Parsing call expression, current token: {:?}",
            self.current_token
        );
        let is_dict_call = self.current_token_kind() == Some(TokenKind::LBrace);
        let mut arguments = Vec::new();

        log::debug!("Is dict call: {}", is_dict_call);

        if is_dict_call {
            arguments.push(self.parse_block_or_dict());
        } else {
            self.consume_token(TokenKind::LParen);
            let mut is_first_argument = true;
            while self.current_token_kind() != Some(TokenKind::RParen) {
                if !is_first_argument {
                    self.consume_token(TokenKind::Comma);
                } else {
                    is_first_argument = false;
                }

                arguments.push(self.parse_expression());
            }

            self.consume_token(TokenKind::RParen);
        }

        AstNode::Call {
            function: Box::new(function),
            arguments,
        }
        .into()
    }

    fn parse_identifier_pattern(&mut self) -> Node {
        let name = self.consume_identifier();

        if name == "_" {
            return node::new!(Wildcard);
        }

        node::new!(IdentifierPattern {
            id: self.unique_id(),
            name: name,
        })
    }

    fn parse_list_extraction(&mut self) -> Node {
        self.consume_token(TokenKind::LBracket);

        let mut elements = Vec::new();
        // Only allow literal values, identifiers and and rest params for now.
        while self.current_token_kind() != Some(TokenKind::RBracket) {
            let element = match self.current_token_kind() {
                Some(TokenKind::Identifier(_)) => self.parse_identifier_pattern(),
                Some(TokenKind::Literal(_) | TokenKind::LBracket) => {
                    self.parse_primary_expression()
                }
                Some(TokenKind::DotDotDot) => {
                    self.advance();
                    node::new!(UnaryOp(
                        UnaryOp::Rest,
                        Box::new(self.parse_identifier_pattern())
                    ))
                }
                _ => {
                    self.panic_unexpected_token(
                        "identifier, literal or rest parameter",
                        self.current_token.clone(),
                    );
                    unreachable!()
                }
            };

            elements.push(element);

            // Allow trailing comma.
            if self.current_token_kind() == Some(TokenKind::RBracket) {
                break;
            }

            self.consume_token(TokenKind::Comma);
        }

        self.consume_token(TokenKind::RBracket);

        node::new!(ListPattern(elements))
    }

    fn parse_list_expression(&mut self) -> Node {
        self.consume_token(TokenKind::LBracket);

        let mut elements = Vec::new();
        while self.current_token_kind() != Some(TokenKind::RBracket) {
            let element = match self.current_token_kind() {
                Some(TokenKind::DotDotDot) => {
                    self.advance();

                    node::new!(UnaryOp(UnaryOp::Spread, Box::new(self.parse_expression())))
                }
                _ => self.parse_expression(),
            };

            elements.push(element);

            // Allow trailing comma.
            if self.current_token_kind() == Some(TokenKind::RBracket) {
                break;
            }

            self.consume_token(TokenKind::Comma);
        }

        self.consume_token(TokenKind::RBracket);

        node::new!(List(elements))
    }

    fn parse_block_or_dict(&mut self) -> Node {
        self.consume_token(TokenKind::LBrace);
        // If the first token is a identifier followed by a colon, we assume a dict.
        if let Some(TokenKind::Identifier(_)) = self.current_token_kind() {
            if let Some(TokenKind::Colon) = self.peek_token_kind() {
                let mut elements = Vec::new();
                while self.current_token_kind() != Some(TokenKind::RBrace) {
                    let key = self.parse_primary_expression();
                    self.consume_token(TokenKind::Colon);
                    let value = self.parse_expression();
                    elements.push((key, value));
                    if let Some(TokenKind::Comma) = self.current_token_kind() {
                        self.advance();
                    }
                }
                self.consume_token(TokenKind::RBrace);
                return node::new!(Dict(elements));
            }
        }

        let (statements, completion) = self.parse_statements(true);
        self.consume_token(TokenKind::RBrace);

        node::new!(Block(statements, completion))
    }

    /// Can be a Identifier, NestedIdentifier or FieldExpression with a single field name.
    fn parse_function_name(&mut self) -> Node {
        let mut identifiers = vec![self.consume_identifier()];

        if let Some(TokenKind::NamespaceSeparator) = self.current_token_kind() {
            while let Some(TokenKind::NamespaceSeparator) = self.current_token_kind() {
                self.advance();
                identifiers.push(self.consume_identifier());
            }

            node::new!(NestedIdentifier(identifiers))
        } else if let Some(TokenKind::Dot) = self.current_token_kind() {
            self.advance();
            node::new!(FieldExpression {
                base: Box::new(node::new!(Identifier(identifiers.pop().unwrap()))),
                field: Box::new(node::new!(Identifier(self.consume_identifier()))),
            })
        } else {
            node::new!(Identifier(identifiers.pop().unwrap()))
        }
    }

    fn parse_if_else_expression(&mut self) -> Node {
        self.consume_token(TokenKind::If);
        let condition = if let Some(TokenKind::Let) = self.current_token_kind() {
            self.parse_variable_declaration()
        } else {
            self.parse_expression()
        };
        // TODO: Reevaluate whether we want `foo {}` to be a `foo({})` call expression.
        //       As this collides with `if let` statements.
        // Semicolon or parenthesis are currently needed to disambiguate call with dictionary and block.
        // E.g. `if let x = foo { .. }` is recognized as `foo({ .. })`
        // vs `if let x = foo; { .. }` which will properly parse.
        if let Some(TokenKind::Semicolon) = self.current_token_kind() {
            self.advance();
        }

        self.expect_token(TokenKind::LBrace);
        let then_branch = self.parse_block();
        let else_branch = if let Some(TokenKind::Else) = self.current_token_kind() {
            self.advance();

            if let Some(TokenKind::If) = self.current_token_kind() {
                Some(self.parse_if_else_expression())
            } else {
                Some(self.parse_block())
            }
        } else {
            None
        };

        node::new!(IfElse {
            condition: Box::new(condition),
            then_branch: Box::new(then_branch),
            else_branch: Box::new(else_branch),
        })
    }

    fn parse_unary_expression(&mut self) -> Node {
        self.advance();
        node::new!(UnaryOp(
            UnaryOp::Minus,
            Box::new(self.parse_primary_expression())
        ))
    }

    fn parse_primary_expression(&mut self) -> Node {
        let mut span = self.create_span_from_current_token();
        let mut node = match &self.current_token_kind() {
            Some(TokenKind::Minus) => self.parse_unary_expression(),
            Some(TokenKind::LParen) => {
                self.advance();
                let expression = self.parse_expression();
                self.consume_token(TokenKind::RParen);
                expression
            }
            Some(TokenKind::LBrace) => self.parse_block_or_dict(),
            Some(TokenKind::LBracket) => self.parse_list_expression(),
            Some(TokenKind::If) => self.parse_if_else_expression(),
            Some(TokenKind::Fn) => self.parse_function_expression(),
            Some(TokenKind::Rec) => {
                self.advance();
                node::new!(RecursiveCall(Box::new(self.parse_expression())))
            }
            Some(TokenKind::Match) => self.parse_match_expression(),
            Some(_) => {
                let mut node: Node = self.current_token.as_ref().unwrap().into();

                self.advance();

                if let AstNode::Identifier(identifier) = &node.ast_node {
                    if identifier == "_" {
                        node = node::new!(Wildcard);
                    } else {
                        if let Some(TokenKind::NamespaceSeparator) = self.current_token_kind() {
                            let mut identifiers = vec![identifier.clone()];
                            while let Some(TokenKind::NamespaceSeparator) =
                                self.current_token_kind()
                            {
                                self.advance();
                                identifiers.push(self.consume_identifier());
                            }
                            node = node::new!(NestedIdentifier(identifiers));
                        }

                        if let Some(TokenKind::LParen | TokenKind::LBrace) =
                            self.current_token_kind()
                        {
                            node = self.parse_call_expression(node);
                        }
                    }
                }

                node
            }
            _ => {
                self.panic_unexpected_token("primary expression", self.current_token.clone());
                unreachable!()
            }
        };

        self.end_span_from_previous_token(&mut span);
        node.span = span;
        node
    }

    fn parse_match_expression(&mut self) -> Node {
        self.consume_token(TokenKind::Match);
        let expression = self.parse_expression();
        self.consume_token(TokenKind::LBrace);

        let mut arms = Vec::new();
        while self.current_token_kind() != Some(TokenKind::RBrace) {
            let pattern = self.parse_expression();
            self.consume_token(TokenKind::FatArrow);
            let expression = self.parse_expression();
            arms.push(node::new!(MatchArm {
                pattern: Box::new(pattern),
                expression: Box::new(expression),
            }));
            if let Some(TokenKind::Comma) = self.current_token_kind() {
                self.advance();
            }
        }

        self.consume_token(TokenKind::RBrace);

        node::new!(Match {
            expression: Box::new(expression),
            arms: arms,
        })
    }

    fn parse_parameter_list(&mut self) -> Vec<Node> {
        let mut parameters = Vec::new();

        if self.current_token_kind() == Some(TokenKind::LParen) {
            self.consume_token(TokenKind::LParen);

            let mut is_first_parameter = true;
            while self.current_token_kind() != Some(TokenKind::RParen) {
                if !is_first_parameter {
                    self.consume_token(TokenKind::Comma);
                } else {
                    is_first_parameter = false;
                }

                let parameter = self.parse_variable_declaration_pattern();

                let type_annotation = match self.current_token_kind() {
                    Some(TokenKind::Colon) => {
                        self.advance();
                        self.parse_type_annotation()
                    }
                    _ => None,
                };

                log::debug!("Parsed parameter: {:?}", parameter);
                parameters.push(node::new!(FunctionParameter {
                    id: self.unique_id(),
                    pattern: Box::new(parameter),
                    type_annotation: Box::new(type_annotation),
                }));
            }
            self.consume_token(TokenKind::RParen);
        }

        parameters
    }

    fn is_type_annotation_token(token: &TokenKind) -> bool {
        matches!(
            token,
            TokenKind::Identifier(_)
                | TokenKind::GreaterThan
                | TokenKind::LessThan
                | TokenKind::NamespaceSeparator
        )
    }

    fn parse_type_annotation(&mut self) -> Option<Node> {
        let token = self.current_token_kind()?;

        if !Self::is_type_annotation_token(&token) {
            return None;
        }

        match token {
            TokenKind::Identifier(_) => {
                let identifier = self.parse_identifier();
                let parameters = self.parse_type_annotation_parameters();

                Some(node::new!(TypeAnnotation {
                    name: Box::new(identifier),
                    parameters: parameters,
                }))
            }
            _ => panic!("Expected type annotation, found {:?}", token),
        }
    }

    fn parse_type_annotation_parameters(&mut self) -> Vec<Node> {
        let mut parameters = Vec::new();

        if let Some(TokenKind::LessThan) = self.current_token_kind() {
            self.advance();
            while self.current_token_kind() != Some(TokenKind::GreaterThan) {
                let parameter = self.parse_type_annotation().unwrap();
                parameters.push(parameter);
                if let Some(TokenKind::Comma) = self.current_token_kind() {
                    self.advance();
                }
            }
            self.consume_token(TokenKind::GreaterThan);
        }

        parameters
    }

    fn parse_return_type(&mut self) -> Option<Node> {
        if let Some(TokenKind::Arrow) = self.current_token_kind() {
            self.advance();
            let return_type = self.parse_type_annotation()?;
            Some(return_type)
        } else {
            None
        }
    }

    fn parse_function_expression(&mut self) -> Node {
        self.consume_token(TokenKind::Fn);
        let name = if let Some(TokenKind::Identifier(_)) = &self.current_token_kind() {
            Some(self.parse_single_identifier())
        } else {
            None
        };

        let parameters = self.parse_parameter_list();
        let return_type = self.parse_return_type();
        let body = self.parse_block();

        node::new!(FunctionExpression {
            id: self.unique_id(),
            name: Box::new(name),
            declaration: Box::new(node::new!(FunctionDeclaration(FunctionDeclaration {
                parameters,
                guard: Box::new(None),
                body: Box::new(body),
                return_type_annotation: Box::new(return_type),
            }))),
        })
    }

    /// Parses an enum extraction, e.g. `Foo(bar, baz)` and `Foo { bar, baz }` within
    /// a function declarations parameter list.
    fn parse_enum_extraction(&mut self) -> Node {
        let identifier = Box::new(self.parse_identifier());
        let is_dict_extraction = self.current_token_kind() == Some(TokenKind::LBrace);

        if is_dict_extraction {
            self.consume_token(TokenKind::LBrace);
            let mut elements = Vec::new();
            while self.current_token_kind() != Some(TokenKind::RBrace) {
                elements.push(self.parse_identifier_pattern());
                if let Some(TokenKind::Comma) = self.current_token_kind() {
                    self.advance();
                }
            }
            self.consume_token(TokenKind::RBrace);
            node::new!(EnumPattern {
                identifier: identifier,
                elements: elements,
                named_fields: true,
            })
        } else if let Some(TokenKind::LParen) = self.current_token_kind() {
            self.consume_token(TokenKind::LParen);
            let mut elements = Vec::new();
            while self.current_token_kind() != Some(TokenKind::RParen) {
                let mut identifier = self.parse_identifier_pattern();

                // Remap identifier of _ to Wildcard
                if let AstNode::Identifier(ref ident) = identifier.ast_node {
                    if ident == "_" {
                        identifier = node::new!(Wildcard);
                    }
                }

                elements.push(identifier);
                if let Some(TokenKind::Comma) = self.current_token_kind() {
                    self.advance();
                }
            }
            self.consume_token(TokenKind::RParen);
            AstNode::EnumPattern {
                identifier,
                elements,
                named_fields: false,
            }
            .into()
        } else {
            node::new!(EnumPattern {
                identifier: identifier,
                elements: Vec::new(),
                named_fields: false,
            })
        }
    }

    fn parse_function_declaration(&mut self) -> Node {
        let mut name: Option<Node> = None;
        let mut declarations: Vec<Node> = Vec::new();

        while matches!(
            self.current_token_kind(),
            Some(TokenKind::Fn)
                | Some(TokenKind::SingleLineComment(_))
                | Some(TokenKind::MultiLineComment(_))
        ) {
            // We have to check if it is the same function declaration as the previous one.
            // Parse ahead to get the identifier of the function, which might be an Identifier,
            // NestedIdentifier or FieldExpression.
            // We do this by cloning the lexer and advancing it until we find the identifier.
            // If we find an identifier that is not the same as the one we already parsed, we stop
            // parsing function declarations and replace the lexer with the cloned lexer.
            // If we find the same identifier, we parse the function declaration and continue
            // parsing function declarations.
            let saved_state = self.save_state();

            let mut comments: Vec<Node> = Vec::new();
            while matches!(
                self.current_token_kind(),
                Some(TokenKind::SingleLineComment(_)) | Some(TokenKind::MultiLineComment(_))
            ) {
                comments.push(self.current_token.as_ref().unwrap().into());
                self.advance();
            }

            if let Some(TokenKind::Fn) = self.current_token_kind() {
                self.advance();
            } else {
                // We found a comment, but not a function declaration, stop parsing function
                // declarations and rewind the lexer.
                self.restore_state(saved_state);
                break;
            }

            if let Some(TokenKind::Identifier(_)) = self.current_token_kind() {
                let node = self.parse_function_name();

                if name.is_some() && name.as_ref().unwrap().ast_node != node.ast_node {
                    // Not the same identifier, stop parsing function declarations and rewind the
                    // lexer.
                    self.restore_state(saved_state);
                    break;
                }

                match node.ast_node {
                    AstNode::Identifier(_) | AstNode::NestedIdentifier(_) => {
                        // We found a simple identifier, which we can use as the name of the function.
                        name = Some(node);
                    }
                    AstNode::FieldExpression { .. } => {
                        // We found a field expression, which we can use as the name of the function.
                        name = Some(node);
                    }
                    _ => {
                        // We found something else, which we can't use as the name of the function.
                        // Stop parsing function declarations and rewind the lexer and panic.
                        self.restore_state(saved_state);
                        self.panic_unexpected_token("identifier", self.current_token.clone());
                    }
                }
            }

            self.expect_token(TokenKind::LParen);
            declarations.extend_from_slice(&comments);
            let parameters = self.parse_parameter_list();
            let guard = self.parse_guard_clause();
            let return_type = self.parse_return_type();
            let body = self.parse_block();

            declarations.push(node::new!(FunctionDeclaration(FunctionDeclaration {
                parameters,
                guard: Box::new(guard),
                body: Box::new(body),
                return_type_annotation: Box::new(return_type),
            })));
        }

        if declarations.len() == 1 {
            let declaration = declarations.pop().unwrap();

            if let AstNode::FunctionDeclaration(_) = declaration.ast_node {
                return node::new!(FunctionSingleDeclaration {
                    id: self.unique_id(),
                    name: Box::new(name.unwrap()),
                    declaration: Box::new(declaration)
                });
            } else {
                unreachable!();
            }
        }

        node::new!(FunctionDeclarations {
            id: self.unique_id(),
            name: Box::new(name.unwrap()),
            declarations: declarations,
        })
    }

    fn parse_guard_clause(&mut self) -> Option<Node> {
        if let Some(TokenKind::If) = self.current_token_kind() {
            self.advance();

            if let Some(TokenKind::Let) = self.current_token_kind() {
                let decl = self.parse_variable_declaration();

                // TODO: Should we restrict and validate valid nodes in if let guard clauses?
                return Some(decl);
            }

            // Valid guard clauses are function calls, binary logical expressions and unary logical
            // expressions.
            let expression = self.parse_expression();

            match expression.ast_node {
                AstNode::Call { .. } | AstNode::BinaryOp { .. } | AstNode::UnaryOp { .. } => (),
                _ => {
                    self.panic_unexpected_node(
                        "function call, binary logical expression or unary logical expression",
                        Some(expression.clone()),
                    );
                }
            }

            // Disambiguate between call with dict and block.
            if let Some(TokenKind::Semicolon) = self.current_token_kind() {
                self.advance();
            }

            Some(expression)
        } else {
            None
        }
    }

    fn is_binary_op(token: &TokenKind) -> bool {
        matches!(
            token,
            TokenKind::Plus
                | TokenKind::Minus
                | TokenKind::Asterisk
                | TokenKind::AsteriskAsterisk
                | TokenKind::Slash
                | TokenKind::Percent
                | TokenKind::Caret
                | TokenKind::EqualEqual
                | TokenKind::NotEqual
                | TokenKind::LessThan
                | TokenKind::LessThanOrEqual
                | TokenKind::GreaterThan
                | TokenKind::GreaterThanOrEqual
                | TokenKind::Pipe
                | TokenKind::Ampersand
                | TokenKind::DoublePipe
                | TokenKind::DoubleAmpersand
                | TokenKind::Pipeline
                | TokenKind::And
                | TokenKind::Or
        )
    }

    fn map_binary_op(token: &TokenKind) -> BinaryOp {
        match token {
            TokenKind::Plus => BinaryOp::Add,
            TokenKind::Minus => BinaryOp::Subtract,
            TokenKind::Asterisk => BinaryOp::Multiply,
            TokenKind::AsteriskAsterisk => BinaryOp::Exponentiation,
            TokenKind::Slash => BinaryOp::Divide,
            TokenKind::Percent => BinaryOp::Modulo,
            TokenKind::EqualEqual => BinaryOp::Equal,
            TokenKind::NotEqual => BinaryOp::NotEqual,
            TokenKind::LessThan => BinaryOp::LessThan,
            TokenKind::LessThanOrEqual => BinaryOp::LessThanOrEqual,
            TokenKind::GreaterThan => BinaryOp::GreaterThan,
            TokenKind::GreaterThanOrEqual => BinaryOp::GreaterThanOrEqual,
            TokenKind::Pipe => BinaryOp::BitwiseOr,
            TokenKind::Ampersand => BinaryOp::BitwiseAnd,
            TokenKind::Caret => BinaryOp::BitwiseXor,
            TokenKind::DoublePipe => BinaryOp::Or,
            TokenKind::DoubleAmpersand => BinaryOp::And,
            TokenKind::Pipeline => BinaryOp::Pipeline,
            TokenKind::And => BinaryOp::And,
            TokenKind::Or => BinaryOp::Or,
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
        let mut span = self.create_span_from_current_token();
        let mut node = self.parse_expression_with_precedence(0, Associativity::Left);
        self.end_span_from_previous_token(&mut span);
        node.span = span;
        node
    }

    fn parse_expression_with_precedence(
        &mut self,
        precedence: u8,
        associativity: Associativity,
    ) -> Node {
        let mut span = self.create_span_from_current_token();
        let mut lhs = self.parse_primary_expression();

        loop {
            match self.current_token_kind().as_ref() {
                Some(TokenKind::Dot) => {
                    self.advance();
                    let field_name = self.consume_identifier();
                    lhs = node::new!(FieldExpression {
                        base: Box::new(lhs),
                        field: Box::new(node::new!(Identifier(field_name))),
                    });
                }
                Some(TokenKind::LBracket) => {
                    self.advance();
                    let index = self.parse_expression_with_precedence(0, Associativity::Left);
                    self.consume_token(TokenKind::RBracket);
                    lhs = node::new!(IndexExpression {
                        base: Box::new(lhs),
                        index: Box::new(index),
                    });
                }
                Some(TokenKind::LParen) => {
                    lhs = self.parse_call_expression(lhs);
                }
                Some(token) if Self::is_binary_op(token) => {
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

                    lhs = node::new!(BinaryOp {
                        op: operator,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    });
                }
                _ => break,
            }
        }

        // TODO: We might want to refactor this into a more explicit and
        //       generic way of handling spans.
        // If we have overwritten lhs and the new node has not assigned a span,
        // we end the span here.
        if lhs.span == Span::default() {
            self.end_span_from_previous_token(&mut span);
            lhs.span = span;
        }

        lhs
    }
}
