use tlang_ast::node::{
    self, Associativity, AstNode, BinaryOp, FunctionDeclaration, Node, OperatorInfo, PrefixOp,
};
use tlang_ast::symbols::SymbolId;
use tlang_ast::token::Token;

use crate::lexer::Lexer;
use log::debug;

pub struct Parser<'src> {
    lexer: Lexer<'src>,
    current_token: Option<Token>,
    next_token: Option<Token>,

    // Id to identifiy symbols, e.g. functions and variables.
    unique_id: SymbolId,

    // We have to copy over the lexers position as we scan ahead of the current token.
    current_line: usize,
    current_column: usize,
}

impl<'src> Parser<'src> {
    pub fn new(lexer: Lexer<'src>) -> Parser {
        Parser {
            lexer,
            current_token: None,
            next_token: None,
            unique_id: SymbolId::new(0),
            current_line: 0,
            current_column: 0,
        }
    }

    fn unique_id(&mut self) -> SymbolId {
        self.unique_id = self.unique_id.next();
        self.unique_id
    }

    pub fn from_source(source: &'src str) -> Parser<'src> {
        Parser::new(Lexer::new(source))
    }

    pub fn parse(&mut self) -> Node {
        self.advance();
        self.advance();
        self.parse_program()
    }

    fn panic_unexpected_token(&self, expected: &str, actual: Option<Token>) {
        let line = self.current_line;
        // Technically we are still off by one here as we are already pointing at the end of the current token,
        // but it's better than nothing.
        // We'd either have to store the beginning or the length of the current token.
        let column = self.current_column;
        let source_line = self.lexer.source().lines().nth(line - 1).unwrap();
        let caret = " ".repeat(column - 1) + "^";

        panic!(
            "Expected {} on line {}, column {}, found {:?} instead\n{}\n{}",
            expected, line, column, actual, source_line, caret
        );
    }

    fn panic_unexpected_node(&self, expected: &str, actual: Option<Node>) {
        let actual = actual.unwrap();
        let line = self.current_line;
        // Technically we are still off by one here as we are already pointing at the end of the current token,
        // but it's better than nothing.
        // We'd either have to store the beginning or the length of the current token.
        let column = self.current_column;
        let source_line = self.lexer.source().lines().nth(line - 1).unwrap();
        let caret = " ".repeat(column - 1) + "^";

        panic!(
            "Expected {} on line {}, column {}, found {:?} instead\n{}\n{}",
            expected, line, column, actual, source_line, caret
        );
    }

    fn expect_token(&mut self, expected: Token) {
        let actual = self.current_token.as_ref().unwrap();
        if *actual != expected {
            self.panic_unexpected_token(&format!("{:?}", expected), Some(actual.clone()));
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
                self.panic_unexpected_token("identifier", Some(actual.clone()));
                unreachable!()
            }
        }
    }

    fn advance(&mut self) {
        self.current_token = self.next_token.clone();
        self.current_column = self.lexer.current_column();
        self.current_line = self.lexer.current_line();
        self.next_token = Some(self.lexer.next_token());
        debug!("Advanced to {:?}", self.current_token);
    }

    pub fn peek_token(&self) -> Option<&Token> {
        self.next_token.as_ref()
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
        if let Some(Token::Semicolon) = self.current_token {
            self.advance();

            return (false, None);
        }

        let node = match self.current_token {
            Some(Token::Let) => self.parse_variable_declaration(),
            Some(Token::Fn) => self.parse_function_declaration(),
            Some(Token::Return) => self.parse_return_statement(),
            Some(Token::Enum) => self.parse_enum_declaration(),
            Some(Token::SingleLineComment(_) | Token::MultiLineComment(_)) => {
                let comment: Node = self.current_token.as_ref().unwrap().into();
                self.advance();
                return (false, Some(comment));
            }
            _ => node::new!(ExpressionStatement(Box::new(self.parse_expression()))),
        };

        // FunctionDeclaration statements does not need to be terminated with a semicolon.
        match node.ast_node {
            AstNode::FunctionDeclaration { .. } | AstNode::FunctionDeclarations { .. } => {
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

    fn parse_return_statement(&mut self) -> Node {
        self.consume_token(Token::Return);

        if self.current_token == Some(Token::Semicolon) {
            return node::new!(ReturnStatement(None));
        }

        node::new!(ReturnStatement(Some(Box::new(self.parse_expression()))))
    }

    fn parse_single_identifier(&mut self) -> Node {
        node::new!(Identifier(self.consume_identifier()))
    }

    fn parse_identifier(&mut self) -> Node {
        let mut identifiers = vec![self.consume_identifier()];
        while let Some(Token::NamespaceSeparator) = self.current_token {
            self.advance();
            identifiers.push(self.consume_identifier());
        }

        if identifiers.len() == 1 {
            node::new!(Identifier(identifiers.pop().unwrap()))
        } else {
            node::new!(NestedIdentifier(identifiers))
        }
    }

    fn parse_enum_declaration(&mut self) -> Node {
        self.consume_token(Token::Enum);
        let name = self.consume_identifier();
        self.consume_token(Token::LBrace);
        let mut variants = Vec::new();
        while self.current_token != Some(Token::RBrace) {
            variants.push(self.parse_enum_variant());
            if let Some(Token::Comma) = self.current_token {
                self.advance();
            }
        }
        self.consume_token(Token::RBrace);
        node::new!(EnumDeclaration {
            id: self.unique_id(),
            name: name,
            variants: variants,
        })
    }

    /// Parses an enum variant, e.g. `Foo`, `Foo(1, 2, 3)` and
    /// `Foo { bar, baz }`.
    fn parse_enum_variant(&mut self) -> Node {
        let name = self.consume_identifier();
        log::debug!("Parsing enum variant {}", name);
        match self.current_token {
            Some(Token::LParen) => {
                self.advance();
                let mut parameters = Vec::new();
                while self.current_token != Some(Token::RParen) {
                    parameters.push(node::new!(Identifier(self.consume_identifier())));
                    if let Some(Token::Comma) = self.current_token {
                        self.advance();
                    }
                }
                self.consume_token(Token::RParen);
                AstNode::EnumVariant {
                    name,
                    named_fields: false,
                    parameters,
                }
                .into()
            }
            Some(Token::LBrace) => {
                self.advance();
                let mut parameters = Vec::new();
                while self.current_token != Some(Token::RBrace) {
                    parameters.push(node::new!(Identifier(self.consume_identifier())));
                    if let Some(Token::Comma) = self.current_token {
                        self.advance();
                    }
                }
                self.consume_token(Token::RBrace);
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
        }
    }

    fn parse_statements(&mut self, may_complete: bool) -> (Vec<Node>, Option<Box<Node>>) {
        let mut statements = Vec::new();
        let mut completion_expression = None;

        while self.current_token != Some(Token::RBrace) && self.current_token != Some(Token::Eof) {
            if let (consume_semicolon, Some(statement)) = self.parse_statement() {
                if may_complete
                    && self.current_token == Some(Token::RBrace)
                    && matches!(&statement.ast_node, AstNode::ExpressionStatement(_))
                {
                    let expression = match statement.ast_node {
                        AstNode::ExpressionStatement(expr) => expr,
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
        node::new!(Program(self.parse_statements(false).0))
    }

    fn parse_block(&mut self) -> Node {
        self.consume_token(Token::LBrace);
        let (statements, completion) = self.parse_statements(true);
        self.consume_token(Token::RBrace);

        node::new!(Block(statements, completion))
    }

    fn parse_variable_declaration_pattern(&mut self) -> Node {
        match self.current_token {
            // Literal values will be pattern matched
            Some(Token::Literal(_)) => self.parse_primary_expression(),
            Some(Token::Identifier(ref identifier)) if identifier == "_" => {
                self.advance();
                node::new!(Wildcard)
            }
            // Identifiers can be namespaced identifiers or call expressions, which should be pattern matched.
            // Simple identifiers will be used as is and mapped to a function parameter.
            Some(Token::Identifier(_)) => {
                if let Some(Token::NamespaceSeparator) = self.peek_token() {
                    self.parse_enum_extraction()
                } else if let Some(Token::LParen | Token::LBrace) = self.peek_token() {
                    self.parse_enum_extraction()
                } else {
                    node::new!(Identifier(self.consume_identifier()))
                }
            }
            Some(Token::LBracket) => self.parse_list_extraction(),
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
        self.consume_token(Token::Let);

        // We probably want to know whether we are parsing a plain declaration of `if let`.
        let pattern = self.parse_variable_declaration_pattern();

        let type_annotation = match self.current_token {
            Some(Token::Colon) => {
                self.advance();
                self.parse_type_annotation()
            }
            _ => None,
        };
        self.consume_token(Token::EqualSign);
        let value = self.parse_expression();

        node::new!(VariableDeclaration {
            id: self.unique_id(),
            pattern: Box::new(pattern),
            expression: Box::new(value),
            type_annotation: type_annotation.map(Box::new),
        })
    }

    /// Parses a function call expression, e.g. `foo()`, `foo(1, 2, 3)` and
    /// `foo { bar, baz }`.
    fn parse_call_expression(&mut self, function: Node) -> Node {
        log::debug!(
            "Parsing call expression, current token: {:?}",
            self.current_token
        );
        let is_dict_call = self.current_token == Some(Token::LBrace);
        let mut arguments = Vec::new();

        log::debug!("Is dict call: {}", is_dict_call);

        if is_dict_call {
            arguments.push(self.parse_block_or_dict());
        } else {
            self.consume_token(Token::LParen);
            while self.current_token != Some(Token::RParen) {
                arguments.push(self.parse_expression());

                if let Some(Token::Comma) = self.current_token {
                    self.advance();
                }
            }

            self.consume_token(Token::RParen);
        }

        AstNode::Call {
            function: Box::new(function),
            arguments,
        }
        .into()
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
                    node::new!(PrefixOp(
                        PrefixOp::Rest,
                        Box::new(self.parse_primary_expression())
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
            if self.current_token == Some(Token::RBracket) {
                break;
            }

            self.consume_token(Token::Comma);
        }

        self.consume_token(Token::RBracket);

        node::new!(List(elements))
    }

    fn parse_list_expression(&mut self) -> Node {
        self.consume_token(Token::LBracket);

        let mut elements = Vec::new();
        while self.current_token != Some(Token::RBracket) {
            let element = match self.current_token {
                Some(Token::DotDotDot) => {
                    self.advance();

                    node::new!(PrefixOp(
                        PrefixOp::Spread,
                        Box::new(self.parse_expression())
                    ))
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

        node::new!(List(elements))
    }

    fn parse_block_or_dict(&mut self) -> Node {
        self.consume_token(Token::LBrace);
        // If the first token is a identifier followed by a colon, we assume a dict.
        if let Some(Token::Identifier(_)) = self.current_token {
            if let Some(Token::Colon) = self.peek_token() {
                let mut elements = Vec::new();
                while self.current_token != Some(Token::RBrace) {
                    let key = self.parse_primary_expression();
                    self.consume_token(Token::Colon);
                    let value = self.parse_expression();
                    elements.push((key, value));
                    if let Some(Token::Comma) = self.current_token {
                        self.advance();
                    }
                }
                self.consume_token(Token::RBrace);
                return node::new!(Dict(elements));
            }
        }

        let (statements, completion) = self.parse_statements(true);
        self.consume_token(Token::RBrace);

        node::new!(Block(statements, completion))
    }

    /// Can be a Identifier, NestedIdentifier or FieldExpression with a single field name.
    fn parse_function_name(&mut self) -> Node {
        let mut identifiers = vec![self.consume_identifier()];

        if let Some(Token::NamespaceSeparator) = self.current_token {
            while let Some(Token::NamespaceSeparator) = self.current_token {
                self.advance();
                identifiers.push(self.consume_identifier());
            }

            node::new!(NestedIdentifier(identifiers))
        } else if let Some(Token::Dot) = self.current_token {
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
        self.consume_token(Token::If);
        let condition = if let Some(Token::Let) = self.current_token {
            self.parse_variable_declaration()
        } else {
            self.parse_expression()
        };
        // TODO: Reevaluate whether we want `foo {}` to be a `foo({})` call expression.
        //       As this collides with `if let` statements.
        // Semicolon is currently needed to disambiguate call with dictionary and block.
        // E.g. `if let x = foo { .. }` is recognized as `foo({ .. })`
        // vs `if let x = foo; { .. }` which will properly parse.
        if let Some(Token::Semicolon) = self.current_token {
            self.advance();
        }
        self.expect_token(Token::LBrace);
        let then_branch = self.parse_block();
        let else_branch = if let Some(Token::Else) = self.current_token {
            self.advance();
            Some(Box::new(self.parse_block()))
        } else {
            None
        };

        node::new!(IfElse {
            condition: Box::new(condition),
            then_branch: Box::new(then_branch),
            else_branch: else_branch,
        })
    }

    fn parse_primary_expression(&mut self) -> Node {
        match &self.current_token {
            Some(Token::LParen) => {
                self.advance();
                let expression = self.parse_expression();
                self.consume_token(Token::RParen);
                expression
            }
            Some(Token::LBrace) => self.parse_block_or_dict(),
            Some(Token::LBracket) => self.parse_list_expression(),
            Some(Token::If) => self.parse_if_else_expression(),
            Some(Token::Fn) => self.parse_function_expression(),
            Some(Token::Rec) => {
                self.advance();
                node::new!(RecursiveCall(Box::new(self.parse_expression())))
            }
            Some(Token::Match) => self.parse_match_expression(),
            Some(token) => {
                let mut node: Node = token.into();

                self.advance();

                if let AstNode::Identifier(identifier) = &node.ast_node {
                    if identifier == "_" {
                        return node::new!(Wildcard);
                    }

                    if let Some(Token::NamespaceSeparator) = self.current_token {
                        let mut identifiers = vec![identifier.clone()];
                        while let Some(Token::NamespaceSeparator) = self.current_token {
                            self.advance();
                            identifiers.push(self.consume_identifier());
                        }
                        node = node::new!(NestedIdentifier(identifiers));
                    }

                    if let Some(Token::LParen | Token::LBrace) = self.current_token {
                        return self.parse_call_expression(node);
                    }
                }

                node
            }
            _ => {
                self.panic_unexpected_token("primary expression", self.current_token.clone());
                unreachable!()
            }
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
            arms.push(node::new!(MatchArm {
                pattern: Box::new(pattern),
                expression: Box::new(expression),
            }));
            if let Some(Token::Comma) = self.current_token {
                self.advance();
            }
        }

        self.consume_token(Token::RBrace);

        node::new!(Match {
            expression: Box::new(expression),
            arms: arms,
        })
    }

    fn parse_parameter_list(&mut self) -> Vec<Node> {
        let mut parameters = Vec::new();

        if self.current_token == Some(Token::LParen) {
            self.consume_token(Token::LParen);

            while self.current_token != Some(Token::RParen) {
                let parameter = self.parse_variable_declaration_pattern();

                let type_annotation = match self.current_token {
                    Some(Token::Colon) => {
                        self.advance();
                        self.parse_type_annotation()
                    }
                    _ => None,
                };

                log::debug!("Parsed parameter: {:?}", parameter);
                parameters.push(node::new!(FunctionParameter {
                    id: self.unique_id(),
                    node: Box::new(parameter),
                    type_annotation: type_annotation.map(Box::new),
                }));

                if let Some(Token::Comma) = self.current_token {
                    self.advance();
                }
            }
            self.consume_token(Token::RParen);
        }

        parameters
    }

    fn is_type_annotation_token(token: &Token) -> bool {
        matches!(
            token,
            Token::Identifier(_) | Token::GreaterThan | Token::LessThan | Token::NamespaceSeparator
        )
    }

    fn parse_type_annotation(&mut self) -> Option<Node> {
        let token = self.current_token.as_ref()?;

        if !Self::is_type_annotation_token(token) {
            return None;
        }

        match token {
            Token::Identifier(_) => {
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

        if let Some(Token::LessThan) = self.current_token {
            self.advance();
            while self.current_token != Some(Token::GreaterThan) {
                let parameter = self.parse_type_annotation().unwrap();
                parameters.push(parameter);
                if let Some(Token::Comma) = self.current_token {
                    self.advance();
                }
            }
            self.consume_token(Token::GreaterThan);
        }

        parameters
    }

    fn parse_return_type(&mut self) -> Option<Node> {
        if let Some(Token::Arrow) = self.current_token {
            self.advance();
            let return_type = self.parse_type_annotation()?;
            Some(return_type)
        } else {
            None
        }
    }

    fn parse_function_expression(&mut self) -> Node {
        self.consume_token(Token::Fn);
        let name = if let Some(Token::Identifier(_)) = &self.current_token {
            Some(self.parse_single_identifier())
        } else {
            None
        };

        let parameters = self.parse_parameter_list();
        let return_type = self.parse_return_type();
        let body = self.parse_block();

        node::new!(FunctionExpression {
            id: self.unique_id(),
            name: name.map(Box::new),
            declaration: Box::new(FunctionDeclaration {
                parameters,
                guard: None,
                body: Box::new(body),
                return_type_annotation: return_type.map(Box::new),
            }),
        })
    }

    /// Parses an enum extraction, e.g. `Foo(bar, baz)` and `Foo { bar, baz }` within
    /// a function declarations parameter list.
    fn parse_enum_extraction(&mut self) -> Node {
        let identifier = Box::new(self.parse_identifier());
        let is_dict_extraction = self.current_token == Some(Token::LBrace);

        if is_dict_extraction {
            self.consume_token(Token::LBrace);
            let mut elements = Vec::new();
            while self.current_token != Some(Token::RBrace) {
                elements.push(self.parse_identifier());
                if let Some(Token::Comma) = self.current_token {
                    self.advance();
                }
            }
            self.consume_token(Token::RBrace);
            node::new!(EnumExtraction {
                identifier: identifier,
                elements: elements,
                named_fields: true,
            })
        } else if let Some(Token::LParen) = self.current_token {
            self.consume_token(Token::LParen);
            let mut elements = Vec::new();
            while self.current_token != Some(Token::RParen) {
                let mut identifier = self.parse_identifier();

                // Remap identifier of _ to Wildcard
                if let AstNode::Identifier(ref ident) = identifier.ast_node {
                    if ident == "_" {
                        identifier = node::new!(Wildcard);
                    }
                }

                elements.push(identifier);
                if let Some(Token::Comma) = self.current_token {
                    self.advance();
                }
            }
            self.consume_token(Token::RParen);
            AstNode::EnumExtraction {
                identifier,
                elements,
                named_fields: false,
            }
            .into()
        } else {
            node::new!(EnumExtraction {
                identifier: identifier,
                elements: Vec::new(),
                named_fields: false,
            })
        }
    }

    fn parse_function_declaration(&mut self) -> Node {
        let mut name: Option<Node> = None;
        let mut declarations: Vec<FunctionDeclaration> = Vec::new();

        while let Some(Token::Fn) = self.current_token {
            if let Some(Token::Identifier(_)) = self.peek_token() {
                // Found an identifier after the fn keyword. We have to check if it is the same.
                // Parse ahead to get the identifier of the function, which might be an Identifier,
                // NestedIdentifier or FieldExpression.
                // We do this by cloning the lexer and advancing it until we find the identifier.
                // If we find an identifier that is not the same as the one we already parsed, we stop
                // parsing function declarations and replace the lexer with the cloned lexer.
                // If we find the same identifier, we parse the function declaration and continue
                // parsing function declarations.
                let saved_state = self.save_state();

                self.advance(); // Hope for the best and advance the lexer.

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

            self.expect_token(Token::LParen);
            let parameters = self.parse_parameter_list();
            let guard = self.parse_guard_clause();
            let return_type = self.parse_return_type();
            let body = self.parse_block();

            declarations.push(FunctionDeclaration {
                parameters,
                guard: guard.map(Box::new),
                body: Box::new(body),
                return_type_annotation: return_type.map(Box::new),
            });
        }

        if declarations.len() == 1 {
            let declaration = Box::new(declarations.pop().unwrap());

            return node::new!(FunctionDeclaration {
                id: self.unique_id(),
                name: Box::new(name.unwrap()),
                declaration: declaration
            });
        }

        node::new!(FunctionDeclarations {
            id: self.unique_id(),
            name: Box::new(name.unwrap()),
            declarations: declarations,
        })
    }

    fn parse_guard_clause(&mut self) -> Option<Node> {
        if let Some(Token::If) = self.current_token {
            self.advance();

            if let Some(Token::Let) = self.current_token {
                let decl = self.parse_variable_declaration();

                // TODO: Should we restrict and validate valid nodes in if let guard clauses?
                return Some(decl);
            }

            // Valid guard clauses are function calls, binary logical expressions and unary logical
            // expressions.
            let expression = self.parse_expression();

            match expression.ast_node {
                AstNode::Call { .. } | AstNode::BinaryOp { .. } | AstNode::PrefixOp { .. } => (),
                _ => {
                    self.panic_unexpected_node(
                        "function call, binary logical expression or unary logical expression",
                        Some(expression.clone()),
                    );
                }
            }

            Some(expression)
        } else {
            None
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

        loop {
            match self.current_token.as_ref() {
                Some(Token::Dot) => {
                    self.advance();
                    let field_name = self.consume_identifier();
                    lhs = node::new!(FieldExpression {
                        base: Box::new(lhs),
                        field: Box::new(node::new!(Identifier(field_name))),
                    });
                }
                Some(Token::LBracket) => {
                    self.advance();
                    let index = self.parse_expression_with_precedence(0, Associativity::Left);
                    self.consume_token(Token::RBracket);
                    lhs = node::new!(IndexExpression {
                        base: Box::new(lhs),
                        index: Box::new(index),
                    });
                }
                Some(Token::LParen) => {
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

        lhs
    }
}
