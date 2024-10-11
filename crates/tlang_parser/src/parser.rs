use tlang_ast::node::{
    self, Associativity, BinaryOpExpression, BinaryOpKind, Block, CallExpression, ElseClause,
    EnumDeclaration, EnumPattern, EnumVariant, Expr, ExprKind, FieldAccessExpression,
    FunctionDeclaration, FunctionParameter, Ident, IdentifierPattern, IfElseExpression,
    IndexAccessExpression, LetDeclaration, MatchArm, MatchExpression, Module, OperatorInfo, Path,
    Pattern, Stmt, StmtKind, StructDeclaration, StructField, Ty, UnaryOp,
};
use tlang_ast::span::Span;
use tlang_ast::symbols::SymbolId;
use tlang_ast::token::{Keyword, Token, TokenKind};
use tlang_lexer::Lexer;

use crate::error::{ParseError, ParseErrorKind};
use crate::macros::expect_token_matches;
use log::debug;

pub struct Parser<'src> {
    lexer: Lexer<'src>,
    previous_span: Option<Span>,
    current_token: Option<Token>,
    next_token: Option<Token>,

    recoverable: bool,

    // Id to identifiy symbols, e.g. functions and variables.
    unique_id: SymbolId,

    errors: Vec<ParseError>,
}

impl<'src> Parser<'src> {
    pub fn new(lexer: Lexer<'src>) -> Parser<'src> {
        Parser {
            lexer,
            previous_span: None,
            current_token: None,
            next_token: None,
            unique_id: SymbolId::new(0),
            errors: Vec::new(),
            recoverable: false,
        }
    }

    pub fn set_recoverable(&mut self, recoverable: bool) -> &mut Self {
        self.recoverable = recoverable;
        self
    }

    pub fn recoverable(&self) -> bool {
        self.recoverable
    }

    pub fn get_errors(&self) -> Vec<ParseError> {
        self.errors.clone()
    }

    fn unique_id(&mut self) -> SymbolId {
        self.unique_id = self.unique_id.next();
        self.unique_id
    }

    pub fn from_source(source: &'src str) -> Parser<'src> {
        Parser::new(Lexer::new(source))
    }

    pub fn parse(&mut self) -> Result<Module, &[ParseError]> {
        self.advance();
        self.advance();

        let module = self.parse_module();

        if self.errors.is_empty() {
            Ok(module)
        } else {
            Err(&self.errors)
        }
    }

    fn create_span_from_current_token(&self) -> Span {
        self.current_token.as_ref().unwrap().span
    }

    fn end_span_from_previous_token(&self, span: &mut Span) {
        span.end = self.previous_span.unwrap().end
    }

    #[inline(never)]
    fn push_unexpected_token_error(&mut self, expected: &str, actual: Option<Token>) {
        self.errors.push(ParseError {
            msg: format!(
                "Expected {}, found {:?}",
                expected,
                actual.as_ref().unwrap().kind
            ),
            kind: ParseErrorKind::UnexpectedToken(actual.clone().unwrap()),
            span: actual.as_ref().unwrap().span,
        });
    }

    #[inline(never)]
    fn panic_unexpected_token(&self, expected: &str, actual: Option<Token>) {
        let token = actual.as_ref().unwrap();
        let start_span = &token.span.start;
        let source_line = self
            .lexer
            .source()
            .lines()
            .nth(start_span.line as usize)
            .unwrap();
        let caret = " ".repeat(start_span.column as usize) + "^";

        panic!(
            "Expected {} on line {}, column {}, found {:?} instead\n{}\n{}",
            expected, start_span.line, start_span.column, token.kind, source_line, caret
        );
    }

    #[inline(never)]
    fn panic_unexpected_stmt(&self, expected: &str, actual: Option<Stmt>) {
        let node = actual.as_ref().unwrap();
        let start_span = &node.span.start;
        let source_line = self
            .lexer
            .source()
            .lines()
            .nth(start_span.line as usize)
            .unwrap();
        let caret = " ".repeat(start_span.column as usize) + "^";

        panic!(
            "Expected {} on line {}, column {}, found {:?} instead\n{}\n{}",
            expected, start_span.line, start_span.column, node.kind, source_line, caret
        );
    }

    #[inline(never)]
    fn panic_unexpected_expr(&self, expected: &str, actual: Option<Expr>) {
        let node = actual.as_ref().unwrap();
        let start_span = &node.span.start;
        let source_line = self
            .lexer
            .source()
            .lines()
            .nth(start_span.line as usize)
            .unwrap();
        let caret = " ".repeat(start_span.column as usize) + "^";

        panic!(
            "Expected {} on line {}, column {}, found {:?} instead\n{}\n{}",
            expected, start_span.line, start_span.column, node.kind, source_line, caret
        );
    }

    fn expect_token(&mut self, expected: TokenKind) {
        expect_token_matches!(
            self,
            &format!("{expected:?}"),
            _kind if *_kind == expected
        );
    }

    fn expect_token_not(&mut self, expected: TokenKind) {
        expect_token_matches!(
            self,
            &format!("not {:?}", expected),
            _kind if *_kind != expected
        );
    }

    fn consume_token(&mut self, expected: TokenKind) {
        self.expect_token(expected);
        self.advance();
        // Should we check for any comments here? And buffer them for the next node which supports
        // tracking comments?
    }

    #[inline(always)]
    fn consume_keyword_token(&mut self, expected: Keyword) {
        self.consume_token(TokenKind::Keyword(expected));
    }

    fn parse_identifier(&mut self) -> Ident {
        expect_token_matches!(self, TokenKind::Identifier(_));

        let current_token = self.current_token.as_ref().unwrap();
        let ident = Ident::new(
            current_token.kind.get_identifier().unwrap(),
            current_token.span,
        );

        self.advance();

        ident
    }

    fn at_eof(&self) -> bool {
        matches!(self.current_token_kind(), Some(TokenKind::Eof))
    }

    fn advance(&mut self) {
        if self.at_eof() {
            // In error recovery we scan ahead and look for the expected token.
            // If we reach the end of the file, we don't want to continue scanning and
            // break our `while let Some(_) = self.current_token` loop.
            self.current_token = None;
            self.next_token = None;

            return;
        }

        self.previous_span = self.current_token.as_ref().map(|token| token.span);
        self.current_token = self.next_token.take();

        self.next_token = Some(self.lexer.next_token());
        debug!("Advanced to {:?}", self.current_token);
    }

    #[inline(always)]
    fn current_token_kind(&self) -> Option<&TokenKind> {
        self.current_token.as_ref().map(|token| &token.kind)
    }

    #[inline(always)]
    pub fn peek_token_kind(&self) -> Option<&TokenKind> {
        self.next_token.as_ref().map(|token| &token.kind)
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

    fn parse_comments(&mut self) -> Vec<Token> {
        let mut comments: Vec<Token> = Vec::new();
        while matches!(
            self.current_token_kind(),
            Some(TokenKind::SingleLineComment(_) | TokenKind::MultiLineComment(_))
        ) {
            comments.push(self.current_token.clone().unwrap());
            self.advance();
        }
        comments
    }

    fn parse_statement(&mut self) -> (bool, Option<Stmt>) {
        debug!("Parsing statement {:?}", self.current_token);

        // Skip stray semicolons.
        if let Some(TokenKind::Semicolon) = self.current_token_kind() {
            self.advance();

            return (false, None);
        }

        let comments = self.parse_comments();

        if matches!(self.current_token_kind(), Some(TokenKind::Eof)) {
            let mut node = node::stmt!(None);
            node.leading_comments = comments;
            return (false, Some(node));
        }

        let mut span = self.create_span_from_current_token();
        let mut node = match self.current_token_kind().and_then(|tk| tk.get_keyword()) {
            Some(Keyword::Let) => self.parse_variable_declaration(),
            Some(Keyword::Fn)
                // If the next token is an identifier, we assume a function declaration.
                // If it's not, we assume a function expression which is handled as a primary expression.
                if matches!(self.peek_token_kind(), Some(TokenKind::Identifier(_))) =>
            {
                self.parse_function_declaration()
            }
            Some(Keyword::Return) => self.parse_return_statement(),
            Some(Keyword::Enum) => self.parse_enum_declaration(),
            Some(Keyword::Struct) => self.parse_struct_declaration(),
            _ => node::stmt!(Expr(Box::new(self.parse_expression()))),
        };
        node.leading_comments = comments;
        self.end_span_from_previous_token(&mut span);
        node.span = span;

        // FunctionDeclaration statements does not need to be terminated with a semicolon.
        match node.kind {
            StmtKind::FunctionDeclaration { .. } | StmtKind::FunctionDeclarations { .. } => {
                return (false, Some(node))
            }
            _ => (),
        }

        // Neither do EnumDeclaration statements.
        if let StmtKind::EnumDeclaration { .. } = node.kind {
            return (false, Some(node));
        }

        // Nor do StructDeclaration statements.
        if let StmtKind::StructDeclaration { .. } = node.kind {
            return (false, Some(node));
        }

        // Expressions like IfElse as statements also do not need to be terminated with a semicolon.
        if let StmtKind::Expr(ref expr) = node.kind {
            if let ExprKind::IfElse { .. } = expr.kind {
                return (false, Some(node));
            }
        }

        (true, Some(node))
    }

    fn parse_return_statement(&mut self) -> Stmt {
        self.consume_keyword_token(Keyword::Return);

        if matches!(self.current_token_kind(), Some(TokenKind::Semicolon)) {
            return node::stmt!(Return(Box::new(None)));
        }

        node::stmt!(Return(Box::new(Some(self.parse_expression()))))
    }

    fn parse_single_identifier(&mut self) -> Expr {
        let path = Path::from_ident(self.parse_identifier());
        let span = path.span;
        node::expr!(Path(Box::new(path))).with_span(span)
    }

    fn parse_path(&mut self) -> Path {
        let mut path = Path::from_ident(self.parse_identifier());
        while let Some(TokenKind::PathSeparator) = self.current_token_kind() {
            self.advance();
            path.push(self.parse_identifier());
        }

        self.end_span_from_previous_token(&mut path.span);
        path
    }

    fn parse_path_expression(&mut self) -> Expr {
        // How do we generalize this span handling?
        // Callbacks come to mind, but I'd rather not.
        // Adding another stack is also not ideal.
        let path = self.parse_path();
        let span = path.span;

        node::expr!(Path(Box::new(path))).with_span(span)
    }

    fn parse_struct_declaration(&mut self) -> Stmt {
        self.consume_keyword_token(Keyword::Struct);
        let name = self.parse_identifier();
        self.consume_token(TokenKind::LBrace);
        let mut fields = Vec::with_capacity(2);
        while !matches!(self.current_token_kind(), Some(TokenKind::RBrace)) {
            fields.push(self.parse_struct_field());
            if let Some(TokenKind::Comma) = self.current_token_kind() {
                self.advance();
            }
        }
        self.consume_token(TokenKind::RBrace);
        node::stmt!(StructDeclaration(Box::new(StructDeclaration {
            id: self.unique_id(),
            name,
            fields,
        })))
    }

    fn parse_struct_field(&mut self) -> StructField {
        let name = self.parse_identifier();
        self.consume_token(TokenKind::Colon);
        let ty = self.parse_type_annotation();
        (name, ty)
    }

    fn parse_enum_declaration(&mut self) -> Stmt {
        self.consume_keyword_token(Keyword::Enum);

        let name = self.parse_identifier();
        self.consume_token(TokenKind::LBrace);
        let mut variants = Vec::with_capacity(2);
        while !matches!(self.current_token_kind(), Some(TokenKind::RBrace)) {
            variants.push(self.parse_enum_variant());
            if let Some(TokenKind::Comma) = self.current_token_kind() {
                self.advance();
            }
        }
        self.consume_token(TokenKind::RBrace);
        node::stmt!(EnumDeclaration(Box::new(EnumDeclaration {
            id: self.unique_id(),
            name,
            variants,
        })))
    }

    /// Parses an enum variant, e.g. `Foo`, `Foo(1, 2, 3)` and
    /// `Foo { bar, baz }`.
    fn parse_enum_variant(&mut self) -> EnumVariant {
        let span = self.create_span_from_current_token();
        let name = self.parse_identifier();
        log::debug!("Parsing enum variant {}", name);
        let mut node = match self.current_token_kind() {
            Some(TokenKind::LParen) => {
                self.advance();
                let mut parameters = Vec::new();
                while !matches!(self.current_token_kind(), Some(TokenKind::RParen)) {
                    parameters.push(self.parse_identifier());
                    if let Some(TokenKind::Comma) = self.current_token_kind() {
                        self.advance();
                    }
                }
                self.consume_token(TokenKind::RParen);
                EnumVariant {
                    name,
                    named_fields: false,
                    parameters,
                    span,
                }
            }
            Some(TokenKind::LBrace) => {
                self.advance();
                let mut parameters = Vec::new();
                while !matches!(self.current_token_kind(), Some(TokenKind::RBrace)) {
                    parameters.push(self.parse_identifier());
                    if let Some(TokenKind::Comma) = self.current_token_kind() {
                        self.advance();
                    }
                }
                self.consume_token(TokenKind::RBrace);
                EnumVariant {
                    name,
                    named_fields: true,
                    parameters,
                    span,
                }
            }
            _ => EnumVariant {
                name,
                named_fields: false,
                parameters: Vec::new(),
                span,
            },
        };

        self.end_span_from_previous_token(&mut node.span);
        node
    }

    fn parse_statements(&mut self, may_complete: bool) -> (Vec<Stmt>, Option<Expr>) {
        let mut statements = Vec::new();
        let mut completion_expression = None;

        while !matches!(
            self.current_token_kind(),
            Some(TokenKind::RBrace | TokenKind::Eof)
        ) {
            if let (consume_semicolon, Some(mut statement)) = self.parse_statement() {
                if may_complete
                    && matches!(self.current_token_kind(), Some(TokenKind::RBrace))
                    && matches!(
                        statement.kind,
                        StmtKind::Expr(_) | StmtKind::FunctionDeclaration(_)
                    )
                {
                    let mut expression = match statement.kind {
                        StmtKind::Expr(expr) => expr,
                        // Function declaration as completion expression.
                        // We remap the function declaration node to a function expression.
                        StmtKind::FunctionDeclaration(decl) => {
                            Box::new(node::expr!(FunctionExpression(decl)))
                        }
                        _ => {
                            self.panic_unexpected_stmt(
                                "expression statement or function declaration",
                                Some(statement),
                            );
                            unreachable!()
                        }
                    };
                    self.end_span_from_previous_token(&mut statement.span);
                    expression.span = statement.span;
                    completion_expression = Some(*expression);
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

        if let Some(stmt) = statements.last_mut() {
            stmt.trailing_comments = self.parse_comments();
        }

        (statements, completion_expression)
    }

    fn parse_module(&mut self) -> Module {
        Module::new(self.parse_statements(false).0)
    }

    fn parse_block(&mut self) -> Block {
        let mut span = self.create_span_from_current_token();
        self.consume_token(TokenKind::LBrace);
        let (statements, expression) = self.parse_statements(true);
        self.consume_token(TokenKind::RBrace);
        self.end_span_from_previous_token(&mut span);

        Block::new(statements, expression).with_span(span)
    }

    fn parse_let_expression(&mut self) -> Expr {
        self.consume_keyword_token(Keyword::Let);
        let pattern = self.parse_pattern();
        self.consume_token(TokenKind::EqualSign);
        let value = self.parse_expression();
        node::expr!(Let(Box::new(pattern), Box::new(value)))
    }

    fn parse_variable_declaration(&mut self) -> Stmt {
        debug!("Parsing variable declaration");
        self.consume_keyword_token(Keyword::Let);

        // We probably want to know whether we are parsing a plain declaration of `if let`.
        let pattern = self.parse_pattern();

        let type_annotation = match self.current_token_kind() {
            Some(TokenKind::Colon) => {
                self.advance();
                self.parse_optional_type_annotation()
            }
            _ => None,
        };
        self.consume_token(TokenKind::EqualSign);
        let expression = self.parse_expression();

        node::stmt!(Let(Box::new(LetDeclaration {
            pattern,
            expression,
            type_annotation,
        })))
    }

    /// Parses a function call expression, e.g. `foo()`, `foo(1, 2, 3)` and
    /// `foo { bar, baz }`.
    fn parse_call_expression(&mut self, expr: Expr) -> Expr {
        let mut span = expr.span;
        log::debug!(
            "Parsing call expression, current token: {:?}",
            self.current_token
        );
        let is_dict_call = matches!(self.current_token_kind(), Some(TokenKind::LBrace));
        let mut arguments = Vec::new();

        log::debug!("Is dict call: {}", is_dict_call);

        if is_dict_call {
            arguments.push(self.parse_block_or_dict());
        } else {
            self.consume_token(TokenKind::LParen);
            let mut is_first_argument = true;
            while !matches!(self.current_token_kind(), Some(TokenKind::RParen)) {
                if !is_first_argument {
                    self.consume_token(TokenKind::Comma);
                } else {
                    is_first_argument = false;
                }

                if matches!(self.current_token_kind(), Some(TokenKind::RParen)) {
                    break;
                }

                arguments.push(self.parse_expression());
            }

            self.consume_token(TokenKind::RParen);
        }

        self.end_span_from_previous_token(&mut span);

        node::expr!(Call(Box::new(CallExpression {
            callee: expr,
            arguments,
        })))
        .with_span(span)
    }

    fn parse_identifier_pattern(&mut self) -> Pattern {
        let current_token = self.current_token.as_ref().unwrap();

        // TODO: We only do this because we call `parse_identifier_pattern` within
        // `parse_enum_extraction` and that code there is quite messy.
        if let TokenKind::Keyword(Keyword::Underscore) = current_token.kind {
            let wildcard_pattern = node::pat!(Wildcard).with_span(current_token.span);
            self.advance();
            return wildcard_pattern;
        }

        let name = self.parse_identifier();
        let mut span = name.span;

        let mut node = node::pat!(Identifier(Box::new(IdentifierPattern {
            id: self.unique_id(),
            name,
        })));

        self.end_span_from_previous_token(&mut span);
        node.span = span;
        node
    }

    fn parse_list_extraction(&mut self) -> Pattern {
        self.consume_token(TokenKind::LBracket);

        let mut elements = Vec::new();
        // Only allow literal values, identifiers and and rest params for now.
        while !matches!(self.current_token_kind(), Some(TokenKind::RBracket)) {
            expect_token_matches!(
                self,
                "identifier, literal, wildcard or rest parameter",
                TokenKind::Identifier(_)
                    | TokenKind::Literal(_)
                    | TokenKind::Keyword(Keyword::Underscore)
                    | TokenKind::DotDotDot
                    | TokenKind::LBracket
            );

            let element = match self.current_token_kind() {
                Some(
                    TokenKind::Identifier(_)
                    | TokenKind::Literal(_)
                    | TokenKind::Keyword(Keyword::Underscore)
                    | TokenKind::LBracket,
                ) => self.parse_pattern(),
                Some(TokenKind::DotDotDot) => {
                    self.advance();
                    node::pat!(Rest(Box::new(self.parse_identifier_pattern())))
                }
                _ => unreachable!("Expected identifier, literal or rest parameter"),
            };

            elements.push(element);

            // Allow trailing comma.
            if matches!(self.current_token_kind(), Some(TokenKind::RBracket)) {
                break;
            }

            self.consume_token(TokenKind::Comma);
        }

        self.consume_token(TokenKind::RBracket);

        node::pat!(List(elements))
    }

    fn parse_list_expression(&mut self) -> Expr {
        self.consume_token(TokenKind::LBracket);

        let mut elements = Vec::new();
        while !matches!(self.current_token_kind(), Some(TokenKind::RBracket)) {
            let element = match self.current_token_kind() {
                Some(TokenKind::DotDotDot) => {
                    self.advance();

                    node::expr!(UnaryOp(UnaryOp::Spread, Box::new(self.parse_expression())))
                }
                _ => self.parse_expression(),
            };

            elements.push(element);

            // Allow trailing comma.
            if matches!(self.current_token_kind(), Some(TokenKind::RBracket)) {
                break;
            }

            self.consume_token(TokenKind::Comma);
        }

        self.consume_token(TokenKind::RBracket);

        node::expr!(List(elements))
    }

    fn parse_block_or_dict(&mut self) -> Expr {
        let mut span = self.create_span_from_current_token();
        self.consume_token(TokenKind::LBrace);
        // If the first token is a identifier followed by a colon, we assume a dict.
        if let Some(TokenKind::Identifier(_)) = self.current_token_kind() {
            if let Some(TokenKind::Colon) = self.peek_token_kind() {
                let mut elements = Vec::new();
                while !matches!(self.current_token_kind(), Some(TokenKind::RBrace)) {
                    let key = self.parse_primary_expression();
                    self.consume_token(TokenKind::Colon);
                    let value = self.parse_expression();
                    elements.push((key, value));
                    if let Some(TokenKind::Comma) = self.current_token_kind() {
                        self.advance();
                    }
                }
                self.consume_token(TokenKind::RBrace);
                return node::expr!(Dict(elements));
            }
        }

        let (statements, expression) = self.parse_statements(true);
        self.consume_token(TokenKind::RBrace);
        self.end_span_from_previous_token(&mut span);

        node::expr!(Block(Box::new(
            Block::new(statements, expression).with_span(span)
        )))
        .with_span(span)
    }

    /// Can be a Identifier, NestedIdentifier or FieldExpression with a single field name.
    fn parse_function_name(&mut self) -> Expr {
        let path = self.parse_path_expression();
        let mut span = path.span;

        let mut expr = if let Some(TokenKind::Dot) = self.current_token_kind() {
            self.advance();
            node::expr!(FieldExpression(Box::new(FieldAccessExpression {
                base: path,
                field: self.parse_identifier(),
            })))
        } else {
            path
        };

        self.end_span_from_previous_token(&mut span);
        expr.span = span;
        expr
    }

    fn parse_if_condition(&mut self) -> Expr {
        self.consume_keyword_token(Keyword::If);

        let condition = if let Some(TokenKind::Keyword(Keyword::Let)) = self.current_token_kind() {
            self.parse_let_expression()
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

        condition
    }

    fn parse_if_else_expression(&mut self) -> Expr {
        let condition = self.parse_if_condition();

        self.expect_token(TokenKind::LBrace);
        let then_branch = node::expr!(Block(Box::new(self.parse_block())));

        let mut else_branches = Vec::new();
        while let Some(TokenKind::Keyword(Keyword::Else)) = self.current_token_kind() {
            self.advance();

            let condition = if let Some(TokenKind::Keyword(Keyword::If)) = self.current_token_kind()
            {
                Some(self.parse_if_condition())
            } else {
                None
            };

            self.expect_token(TokenKind::LBrace);
            let consequence = node::expr!(Block(Box::new(self.parse_block())));

            if condition.is_none() {
                self.expect_token_not(TokenKind::Keyword(Keyword::Else));
            }

            else_branches.push(ElseClause {
                condition,
                consequence,
            });
        }

        node::expr!(IfElse(Box::new(IfElseExpression {
            condition,
            then_branch,
            else_branches,
        })))
    }

    fn parse_unary_expression(&mut self) -> Expr {
        match self.current_token_kind() {
            Some(TokenKind::Minus) => {
                self.advance();

                node::expr!(UnaryOp(
                    UnaryOp::Minus,
                    Box::new(self.parse_primary_expression())
                ))
            }

            Some(TokenKind::ExclamationMark | TokenKind::Keyword(Keyword::Not)) => {
                self.advance();

                node::expr!(UnaryOp(
                    UnaryOp::Not,
                    Box::new(self.parse_primary_expression())
                ))
            }

            _ => unreachable!("Unexpected token kind"),
        }
    }

    fn parse_primary_expression(&mut self) -> Expr {
        debug!("Parsing primary expression {:?}", self.current_token);

        let comments = self.parse_comments();

        expect_token_matches!(
            self,
            "primary expression",
            TokenKind::Minus
                | TokenKind::ExclamationMark
                | TokenKind::LParen
                | TokenKind::LBrace
                | TokenKind::LBracket
                | TokenKind::Keyword(
                    Keyword::If
                        | Keyword::Fn
                        | Keyword::Rec
                        | Keyword::Match
                        | Keyword::Not
                        | Keyword::Underscore
                )
                | TokenKind::Identifier(_)
                | TokenKind::Literal(_)
        );

        let mut span = self.create_span_from_current_token();
        let mut node = match self.current_token_kind() {
            Some(
                TokenKind::Minus | TokenKind::ExclamationMark | TokenKind::Keyword(Keyword::Not),
            ) => self.parse_unary_expression(),
            Some(TokenKind::LParen) => {
                self.advance();
                let expression = self.parse_expression();
                self.consume_token(TokenKind::RParen);
                expression
            }
            Some(TokenKind::LBrace) => self.parse_block_or_dict(),
            Some(TokenKind::LBracket) => self.parse_list_expression(),
            Some(TokenKind::Keyword(Keyword::If)) => self.parse_if_else_expression(),
            Some(TokenKind::Keyword(Keyword::Fn)) => self.parse_function_expression(),
            Some(TokenKind::Keyword(Keyword::Rec)) => {
                self.advance();
                let expr = self.parse_expression();
                let call_expr = match expr.kind {
                    ExprKind::Call(call) => call,
                    _ => {
                        self.panic_unexpected_expr("call expression", Some(expr));
                        unreachable!()
                    }
                };

                node::expr!(RecursiveCall(call_expr)).with_span(expr.span)
            }
            Some(TokenKind::Keyword(Keyword::Match)) => self.parse_match_expression(),
            Some(TokenKind::Keyword(Keyword::Underscore)) => {
                self.advance();
                node::expr!(Wildcard)
            }
            Some(TokenKind::Literal(literal)) => {
                let literal = literal.clone();
                self.advance();
                node::expr!(Literal(Box::new(literal)))
            }
            Some(TokenKind::Identifier(identifier)) => {
                let identifier = identifier.clone();
                let identifier_span = self.create_span_from_current_token();

                self.advance();

                let mut path = Path::from_ident(Ident::new(&identifier, identifier_span));
                let mut span = path.span;

                while let Some(TokenKind::PathSeparator) = self.current_token_kind() {
                    self.advance();
                    path.push(self.parse_identifier());
                }

                self.end_span_from_previous_token(&mut span);

                let mut expr = node::expr!(Path(Box::new(path))).with_span(span);

                if let Some(TokenKind::LParen | TokenKind::LBrace) = self.current_token_kind() {
                    expr = self.parse_call_expression(expr);
                }

                expr
            }
            _ => {
                self.panic_unexpected_token("primary expression", self.current_token.clone());
                unreachable!("Expected primary expression")
            }
        };

        self.end_span_from_previous_token(&mut span);
        node.span = span;
        node.leading_comments = comments;
        node.trailing_comments = self.parse_comments();
        node
    }

    fn parse_match_expression(&mut self) -> Expr {
        self.consume_keyword_token(Keyword::Match);
        let expression = self.parse_expression();
        self.consume_token(TokenKind::LBrace);

        let mut arms = Vec::new();
        while !matches!(self.current_token_kind(), Some(TokenKind::RBrace)) {
            let pattern = self.parse_pattern();
            self.consume_token(TokenKind::FatArrow);
            let expression = self.parse_expression();
            arms.push(MatchArm {
                pattern,
                expression,
            });
            if let Some(TokenKind::Comma) = self.current_token_kind() {
                self.advance();
            }
        }

        self.consume_token(TokenKind::RBrace);

        node::expr!(Match(Box::new(MatchExpression { expression, arms })))
    }

    fn parse_parameter_list(&mut self) -> Vec<FunctionParameter> {
        let mut parameters = Vec::new();

        if let Some(TokenKind::LParen) = self.current_token_kind() {
            self.consume_token(TokenKind::LParen);

            let mut is_first_parameter = true;
            while !matches!(self.current_token_kind(), Some(TokenKind::RParen)) {
                if !is_first_parameter {
                    self.consume_token(TokenKind::Comma);
                } else {
                    is_first_parameter = false;
                }

                let mut parameter_span = self.create_span_from_current_token();
                let parameter_pattern = self.parse_pattern();

                let type_annotation = match self.current_token_kind() {
                    Some(TokenKind::Colon) => {
                        self.advance();
                        self.parse_optional_type_annotation()
                    }
                    _ => None,
                };

                self.end_span_from_previous_token(&mut parameter_span);

                parameters.push(FunctionParameter {
                    pattern: parameter_pattern,
                    type_annotation,
                    span: parameter_span,
                });
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
                | TokenKind::PathSeparator
        )
    }

    fn parse_type_annotation(&mut self) -> Ty {
        expect_token_matches!(self, "type annotation", TokenKind::Identifier(_));

        match self.current_token_kind() {
            Some(TokenKind::Identifier(_)) => {
                let identifier = self.parse_path();
                let parameters = self.parse_type_annotation_parameters();

                Ty::new(identifier).with_parameters(parameters)
            }
            Some(token) => unreachable!("Expected type annotation, found {:?}", token),
            None => unreachable!("Expected type annotation"),
        }
    }

    fn parse_optional_type_annotation(&mut self) -> Option<Ty> {
        let token = self.current_token_kind()?;

        if !Self::is_type_annotation_token(token) {
            return None;
        }

        self.parse_type_annotation().into()
    }

    fn parse_type_annotation_parameters(&mut self) -> Vec<Ty> {
        let mut parameters = Vec::new();

        if let Some(TokenKind::LessThan) = self.current_token_kind() {
            self.advance();
            while !matches!(self.current_token_kind(), Some(TokenKind::GreaterThan)) {
                let parameter = self.parse_type_annotation();
                parameters.push(parameter);
                if let Some(TokenKind::Comma) = self.current_token_kind() {
                    self.advance();
                }
            }
            self.consume_token(TokenKind::GreaterThan);
        }

        parameters
    }

    fn parse_return_type(&mut self) -> Option<Ty> {
        if let Some(TokenKind::Arrow) = self.current_token_kind() {
            self.advance();
            self.parse_optional_type_annotation()
        } else {
            None
        }
    }

    fn parse_function_expression(&mut self) -> Expr {
        let mut span = self.create_span_from_current_token();
        self.consume_keyword_token(Keyword::Fn);
        let name = if let Some(TokenKind::Identifier(_)) = self.current_token_kind() {
            self.parse_single_identifier()
        } else {
            node::expr!(Path(Box::new(Path::from_ident(Ident::new(
                "anonymous",
                Default::default()
            )))))
        };

        let parameters = self.parse_parameter_list();
        let return_type = self.parse_return_type();
        let body = self.parse_block();

        self.end_span_from_previous_token(&mut span);

        node::expr!(FunctionExpression(Box::new(FunctionDeclaration {
            id: self.unique_id(),
            name,
            parameters,
            body,
            return_type_annotation: return_type,
            span,
            ..Default::default()
        })))
        .with_span(span)
    }

    /// Parses an enum extraction, e.g. `Foo(bar, baz)` and `Foo { bar, baz }` within
    /// a function declarations parameter list.
    fn parse_enum_extraction(&mut self) -> Pattern {
        let identifier = self.parse_path_expression();
        let is_dict_extraction = matches!(self.current_token_kind(), Some(TokenKind::LBrace));

        if is_dict_extraction {
            self.consume_token(TokenKind::LBrace);
            let mut elements = Vec::new();
            while !matches!(self.current_token_kind(), Some(TokenKind::RBrace)) {
                elements.push(self.parse_identifier_pattern());
                if let Some(TokenKind::Comma) = self.current_token_kind() {
                    self.advance();
                }
            }
            self.consume_token(TokenKind::RBrace);
            node::pat!(Enum(Box::new(EnumPattern {
                identifier,
                elements,
                named_fields: true,
            })))
        } else if let Some(TokenKind::LParen) = self.current_token_kind() {
            self.consume_token(TokenKind::LParen);
            let mut elements = Vec::new();
            while !matches!(self.current_token_kind(), Some(TokenKind::RParen)) {
                elements.push(self.parse_identifier_pattern());
                if let Some(TokenKind::Comma) = self.current_token_kind() {
                    self.advance();
                }
            }
            self.consume_token(TokenKind::RParen);
            node::pat!(Enum(Box::new(EnumPattern {
                identifier,
                elements,
                named_fields: false,
            })))
        } else {
            node::pat!(Enum(Box::new(EnumPattern {
                identifier,
                elements: Vec::new(),
                named_fields: false,
            })))
        }
    }

    fn parse_function_declaration(&mut self) -> Stmt {
        let id = self.unique_id();
        let mut name: Option<Expr> = None;
        let mut declarations: Vec<FunctionDeclaration> = Vec::new();

        while matches!(
            self.current_token_kind(),
            Some(
                TokenKind::Keyword(Keyword::Fn)
                    | TokenKind::SingleLineComment(_)
                    | TokenKind::MultiLineComment(_)
            )
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

            let comments = self.parse_comments();
            let mut span = self.create_span_from_current_token();

            if let Some(TokenKind::Keyword(Keyword::Fn)) = self.current_token_kind() {
                self.advance();
            } else {
                // We found a comment, but not a function declaration, stop parsing function
                // declarations and rewind the lexer.
                self.restore_state(saved_state);
                break;
            }

            if let Some(TokenKind::Identifier(_)) = self.current_token_kind() {
                let node = self.parse_function_name();

                if name.is_some()
                    && self.fn_name_identifier_to_string(name.as_ref().unwrap())
                        != self.fn_name_identifier_to_string(&node)
                {
                    // Not the same identifier, stop parsing function declarations and rewind the
                    // lexer.
                    self.restore_state(saved_state);
                    break;
                }

                match node.kind {
                    ExprKind::Path(_) => {
                        // We found a simple identifier, which we can use as the name of the function.
                        name = Some(node);
                    }
                    ExprKind::FieldExpression { .. } => {
                        // We found a field expression, which we can use as the name of the function.
                        name = Some(node);
                    }
                    _ => {
                        // We found something else, which we can't use as the name of the function.
                        // Stop parsing function declarations and rewind the lexer and panic.
                        self.restore_state(saved_state);
                        self.panic_unexpected_token("identifier", self.current_token.clone());
                        unreachable!();
                    }
                }
            }

            self.expect_token(TokenKind::LParen);
            let parameters = self.parse_parameter_list();
            let guard = self.parse_guard_clause();
            let return_type = self.parse_return_type();
            let body = self.parse_block();

            self.end_span_from_previous_token(&mut span);

            declarations.push(FunctionDeclaration {
                id,
                name: name.clone().unwrap(),
                parameters,
                guard,
                body,
                return_type_annotation: return_type,
                leading_comments: comments,
                span,
                ..Default::default()
            });
        }

        if declarations.len() == 1 {
            return node::stmt!(FunctionDeclaration(Box::new(declarations.pop().unwrap())));
        }

        node::stmt!(FunctionDeclarations(declarations))
    }

    fn parse_guard_clause(&mut self) -> Option<Expr> {
        if let Some(TokenKind::Keyword(Keyword::If)) = self.current_token_kind() {
            self.advance();

            if let Some(TokenKind::Keyword(Keyword::Let)) = self.current_token_kind() {
                let decl = self.parse_let_expression();

                return Some(decl);
            }

            // Valid guard clauses are function calls, binary logical expressions and unary logical
            // expressions.
            let expression = self.parse_expression();

            match expression.kind {
                ExprKind::Call { .. } | ExprKind::BinaryOp { .. } | ExprKind::UnaryOp { .. } => (),
                _ => {
                    self.panic_unexpected_expr(
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
                | TokenKind::Keyword(Keyword::And | Keyword::Or)
        )
    }

    fn map_binary_op(token: &TokenKind) -> BinaryOpKind {
        match token {
            TokenKind::Plus => BinaryOpKind::Add,
            TokenKind::Minus => BinaryOpKind::Subtract,
            TokenKind::Asterisk => BinaryOpKind::Multiply,
            TokenKind::AsteriskAsterisk => BinaryOpKind::Exponentiation,
            TokenKind::Slash => BinaryOpKind::Divide,
            TokenKind::Percent => BinaryOpKind::Modulo,
            TokenKind::EqualEqual => BinaryOpKind::Equal,
            TokenKind::NotEqual => BinaryOpKind::NotEqual,
            TokenKind::LessThan => BinaryOpKind::LessThan,
            TokenKind::LessThanOrEqual => BinaryOpKind::LessThanOrEqual,
            TokenKind::GreaterThan => BinaryOpKind::GreaterThan,
            TokenKind::GreaterThanOrEqual => BinaryOpKind::GreaterThanOrEqual,
            TokenKind::Pipe => BinaryOpKind::BitwiseOr,
            TokenKind::Ampersand => BinaryOpKind::BitwiseAnd,
            TokenKind::Caret => BinaryOpKind::BitwiseXor,
            TokenKind::DoublePipe => BinaryOpKind::Or,
            TokenKind::DoubleAmpersand => BinaryOpKind::And,
            TokenKind::Pipeline => BinaryOpKind::Pipeline,
            TokenKind::Keyword(Keyword::And) => BinaryOpKind::And,
            TokenKind::Keyword(Keyword::Or) => BinaryOpKind::Or,
            _ => {
                unimplemented!("Expected binary operator, found {:?}", token)
            }
        }
    }

    fn map_operator_info(operator: &BinaryOpKind) -> OperatorInfo {
        match operator {
            BinaryOpKind::Add | BinaryOpKind::Subtract => OperatorInfo {
                precedence: 6,
                associativity: Associativity::Left,
            },
            BinaryOpKind::Multiply | BinaryOpKind::Divide | BinaryOpKind::Modulo => OperatorInfo {
                precedence: 7,
                associativity: Associativity::Left,
            },
            BinaryOpKind::Equal
            | BinaryOpKind::NotEqual
            | BinaryOpKind::LessThan
            | BinaryOpKind::LessThanOrEqual
            | BinaryOpKind::GreaterThan
            | BinaryOpKind::GreaterThanOrEqual => OperatorInfo {
                precedence: 5,
                associativity: Associativity::Left,
            },
            BinaryOpKind::And => OperatorInfo {
                precedence: 3,
                associativity: Associativity::Left,
            },
            BinaryOpKind::Or => OperatorInfo {
                precedence: 2,
                associativity: Associativity::Left,
            },
            BinaryOpKind::BitwiseAnd | BinaryOpKind::BitwiseOr | BinaryOpKind::BitwiseXor => {
                OperatorInfo {
                    precedence: 8,
                    associativity: Associativity::Left,
                }
            }
            BinaryOpKind::Exponentiation => OperatorInfo {
                precedence: 9,
                associativity: Associativity::Right,
            },
            BinaryOpKind::Pipeline => OperatorInfo {
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

    fn parse_pattern_literal(&mut self) -> Pattern {
        expect_token_matches!(self, "literal", TokenKind::Literal(_));
        node::pat!(Literal(Box::new(self.parse_expression())))
    }

    fn parse_pattern(&mut self) -> Pattern {
        expect_token_matches!(
            self,
            "literal, identifier or list extraction",
            TokenKind::Literal(_)
                | TokenKind::Identifier(_)
                | TokenKind::Keyword(Keyword::Underscore)
                | TokenKind::LBracket
        );

        let mut span = self.create_span_from_current_token();
        let mut node = match self.current_token_kind() {
            // Literal values will be pattern matched
            Some(TokenKind::Literal(_)) => self.parse_pattern_literal(),
            Some(TokenKind::Keyword(Keyword::Underscore)) => {
                self.advance();
                node::pat!(Wildcard)
            }
            // Identifiers can be namespaced identifiers or call expressions, which should be pattern matched.
            // Simple identifiers will be used as is and mapped to a function parameter.
            Some(TokenKind::Identifier(_)) => {
                if let Some(TokenKind::PathSeparator) = self.peek_token_kind() {
                    self.parse_enum_extraction()
                } else if let Some(TokenKind::LParen | TokenKind::LBrace) = self.peek_token_kind() {
                    self.parse_enum_extraction()
                } else {
                    self.parse_identifier_pattern()
                }
            }
            Some(TokenKind::LBracket) => self.parse_list_extraction(),
            _ => unreachable!("Expected pattern, found {:?}", self.current_token_kind()),
        };

        self.end_span_from_previous_token(&mut span);
        node.span = span;
        node
    }

    fn parse_expression(&mut self) -> Expr {
        self.parse_expression_with_precedence(0, Associativity::Left)
    }

    fn parse_expression_with_precedence(
        &mut self,
        precedence: u8,
        associativity: Associativity,
    ) -> Expr {
        debug!(
            "Parsing expression with precedence {} {:#?}",
            precedence, associativity
        );
        let mut lhs = self.parse_primary_expression();
        let mut span = lhs.span;

        loop {
            match self.current_token_kind() {
                Some(TokenKind::Dot) => {
                    self.advance();
                    lhs.trailing_comments = self.parse_comments();
                    let field_name = self.parse_identifier();
                    lhs = node::expr!(FieldExpression(Box::new(FieldAccessExpression {
                        base: lhs,
                        field: field_name,
                    })));
                }
                Some(TokenKind::LBracket) => {
                    self.advance();
                    let index = self.parse_expression_with_precedence(0, Associativity::Left);
                    self.consume_token(TokenKind::RBracket);
                    lhs = node::expr!(IndexExpression(Box::new(IndexAccessExpression {
                        base: lhs,
                        index,
                    })));
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

                    lhs = node::expr!(BinaryOp(Box::new(BinaryOpExpression {
                        op: operator,
                        lhs,
                        rhs,
                    })));
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

        lhs.trailing_comments.extend(self.parse_comments());
        lhs
    }

    fn fn_name_identifier_to_string(&self, identifier: &Expr) -> String {
        match &identifier.kind {
            ExprKind::Path(path) => path.join("::"),

            ExprKind::FieldExpression(field) => {
                self.fn_name_identifier_to_string(&field.base) + "." + field.field.as_str()
            }
            _ => {
                self.panic_unexpected_expr(
                    "identifier, nested identifier or field expression",
                    Some(identifier.clone()),
                );
                unreachable!()
            }
        }
    }
}
