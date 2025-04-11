use tlang_ast::keyword::{Keyword, kw};
use tlang_ast::node::{
    self, Associativity, BinaryOpExpression, BinaryOpKind, Block, CallExpression, ElseClause,
    EnumDeclaration, EnumPattern, EnumVariant, Expr, ExprKind, FieldAccessExpression,
    FunctionDeclaration, FunctionParameter, Ident, IfElseExpression, IndexAccessExpression,
    LetDeclaration, MatchArm, MatchExpression, Module, OperatorInfo, Pat, Path, Stmt, StmtKind,
    StructDeclaration, StructField, Ty, UnaryOp,
};
use tlang_ast::node_id::NodeId;
use tlang_ast::span::Span;
use tlang_ast::token::{Literal, Token, TokenKind};
use tlang_lexer::Lexer;

use crate::error::{ParseError, ParseIssue, ParseIssueKind};
use crate::macros::expect_token_matches;
use log::debug;

pub struct Parser<'src> {
    lexer: Lexer<'src>,
    previous_span: Span,
    current_token: Token,
    next_token: Token,

    recoverable: bool,

    unique_id: NodeId,

    errors: Vec<ParseIssue>,
}

impl<'src> Parser<'src> {
    pub fn new(lexer: Lexer<'src>) -> Parser<'src> {
        Parser {
            lexer,
            previous_span: Span::default(),
            current_token: Token::default(),
            next_token: Token::default(),
            unique_id: NodeId::new(0),
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

    pub fn get_errors(&self) -> &[ParseIssue] {
        &self.errors
    }

    fn unique_id(&mut self) -> NodeId {
        self.unique_id = self.unique_id.next();
        self.unique_id
    }

    pub fn from_source(source: &'src str) -> Parser<'src> {
        Parser::new(Lexer::new(source))
    }

    pub fn parse(&mut self) -> Result<Module, ParseError> {
        self.current_token = self.lexer.next_token();
        self.next_token = self.lexer.next_token();

        let module = self.parse_module();

        if self.errors.is_empty() {
            Ok(module)
        } else {
            Err(ParseError::new(self.errors.clone()))
        }
    }

    fn create_span_from_current_token(&self) -> Span {
        self.current_token.span
    }

    fn end_span_from_previous_token(&self, span: &mut Span) {
        span.end = self.previous_span.end
    }

    #[inline(never)]
    fn push_unexpected_token_error(&mut self, expected: &str, actual: Token) {
        let span = actual.span;
        let msg = format!("Expected {}, found {:?}", expected, actual.kind);
        let kind = ParseIssueKind::UnexpectedToken(actual);

        self.errors.push(ParseIssue { msg, kind, span });
    }

    #[inline(never)]
    fn panic_unexpected_token(&self, expected: &str, actual: Token) {
        let start_span = actual.span.start;
        let source_line = self
            .lexer
            .source()
            .lines()
            .nth(start_span.line as usize)
            .unwrap();
        let caret = " ".repeat(start_span.column as usize) + "^";

        panic!(
            "Expected {} on line {}, column {}, found {:?} instead\n{}\n{}",
            expected, start_span.line, start_span.column, actual.kind, source_line, caret
        );
    }

    #[inline(never)]
    fn panic_unexpected_stmt(&self, expected: &str, actual: Stmt) {
        let start_span = actual.span.start;
        let source_line = self
            .lexer
            .source()
            .lines()
            .nth(start_span.line as usize)
            .unwrap();
        let caret = " ".repeat(start_span.column as usize) + "^";

        panic!(
            "Expected {} on line {}, column {}, found {:?} instead\n{}\n{}",
            expected, start_span.line, start_span.column, actual.kind, source_line, caret
        );
    }

    #[inline(never)]
    fn panic_unexpected_expr(&self, expected: &str, actual: Option<Expr>) {
        let node = actual.as_ref().unwrap();
        let start_span = node.span.start;
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
            &format!("not {expected:?}"),
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

        let current_token = self.advance();

        Ident::new(current_token.get_identifier().unwrap(), current_token.span)
    }

    fn at_eof(&self) -> bool {
        matches!(self.current_token_kind(), TokenKind::Eof)
    }

    fn advance(&mut self) -> Token {
        if self.at_eof() {
            // In error recovery we scan ahead and look for the expected token.
            // If we reach the end of the file, we don't want to continue scanning and
            // break our `while let Some(_) = self.current_token` loop.
            let consumed_token = self.current_token.clone();
            self.next_token = Token::new(TokenKind::Eof, consumed_token.span);

            return consumed_token;
        }

        let next_token = std::mem::replace(&mut self.next_token, self.lexer.next_token());
        let consumed_token = std::mem::replace(&mut self.current_token, next_token);
        self.previous_span = consumed_token.span;
        debug!("Advanced to {:?}", self.current_token);
        consumed_token
    }

    #[inline(always)]
    fn current_token_kind(&self) -> &TokenKind {
        &self.current_token.kind
    }

    #[inline(always)]
    pub fn peek_token_kind(&self) -> &TokenKind {
        &self.next_token.kind
    }

    fn save_state(&self) -> (Lexer<'src>, Token, Token) {
        debug!("Saving state at {:?}", self.current_token);

        (
            self.lexer.clone(),
            self.current_token.clone(),
            self.next_token.clone(),
        )
    }

    fn restore_state(&mut self, state: (Lexer<'src>, Token, Token)) {
        debug!("Restoring state to {:?}", state.1);

        self.lexer = state.0;
        self.current_token = state.1;
        self.next_token = state.2;
    }

    fn parse_comments(&mut self) -> Vec<Token> {
        let mut comments: Vec<Token> = Vec::new();
        while matches!(
            self.current_token_kind(),
            TokenKind::SingleLineComment(_) | TokenKind::MultiLineComment(_)
        ) {
            comments.push(self.advance());
        }
        comments
    }

    fn parse_statement(&mut self) -> (bool, Option<Stmt>) {
        debug!("Parsing statement {:?}", self.current_token);

        // Skip stray semicolons.
        if matches!(self.current_token_kind(), TokenKind::Semicolon) {
            self.advance();

            return (false, None);
        }

        let comments = self.parse_comments();

        if matches!(self.current_token_kind(), TokenKind::Eof) {
            let mut node = node::stmt!(self.unique_id(), None);
            node.leading_comments = comments;
            return (false, Some(node));
        }

        let mut span = self.create_span_from_current_token();
        let mut node = match &self.current_token.kind {
            TokenKind::Keyword(Keyword::Let) => self.parse_variable_declaration(),
            TokenKind::Keyword(Keyword::Fn)
                // If the next token is an identifier, we assume a function declaration.
                // If it's not, we assume a function expression which is handled as a primary expression.
                if matches!(self.peek_token_kind(), TokenKind::Identifier(_)) =>
            {
                self.parse_function_declaration()
            }
            TokenKind::Keyword(Keyword::Return) => self.parse_return_statement(),
            TokenKind::Keyword(Keyword::Enum) => self.parse_enum_declaration(),
            TokenKind::Keyword(Keyword::Struct) => self.parse_struct_declaration(),
            TokenKind::LBrace => self.parse_block_stmt(),
            _ => node::stmt!(self.unique_id(), Expr(Box::new(self.parse_expression()))),
        };
        node.leading_comments = comments;
        self.end_span_from_previous_token(&mut span);
        node.span = span;

        match node.kind {
            // FunctionDeclaration statements do not need to be terminated with a semicolon.
            StmtKind::FunctionDeclaration { .. } | StmtKind::FunctionDeclarations { .. }
            // Neither do EnumDeclaration statements.
            | StmtKind::EnumDeclaration { .. }
            // Nor do StructDeclaration statements.
            | StmtKind::StructDeclaration { .. } => {
                (false, Some(node))
            }
            // Expressions like IfElse as statements also do not need to be terminated with a semicolon.
            StmtKind::Expr(ref expr)
                if matches!(expr.kind, ExprKind::IfElse(..) | ExprKind::Match(..) | ExprKind::Loop(..) | ExprKind::ForLoop(..)) => {
                (false, Some(node))
            }
            _ => (true, Some(node))
        }
    }

    fn parse_return_statement(&mut self) -> Stmt {
        self.consume_keyword_token(Keyword::Return);

        if matches!(self.current_token_kind(), TokenKind::Semicolon) {
            return node::stmt!(self.unique_id(), Return(Box::new(None)));
        }

        node::stmt!(
            self.unique_id(),
            Return(Box::new(Some(self.parse_expression())))
        )
    }

    fn parse_single_identifier(&mut self) -> Expr {
        let path = Path::from_ident(self.parse_identifier());
        let span = path.span;
        node::expr!(self.unique_id(), Path(Box::new(path))).with_span(span)
    }

    fn parse_path(&mut self) -> Path {
        let mut path = Path::from_ident(self.parse_identifier());
        while matches!(self.current_token_kind(), TokenKind::PathSeparator) {
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

        node::expr!(self.unique_id(), Path(Box::new(path))).with_span(span)
    }

    fn parse_struct_declaration(&mut self) -> Stmt {
        self.consume_keyword_token(Keyword::Struct);
        let name = self.parse_identifier();
        self.consume_token(TokenKind::LBrace);
        let mut fields = Vec::with_capacity(2);
        while !matches!(self.current_token_kind(), TokenKind::RBrace) {
            fields.push(self.parse_struct_field());

            if matches!(self.current_token_kind(), TokenKind::Comma) {
                self.advance();
            }
        }
        self.consume_token(TokenKind::RBrace);
        node::stmt!(
            self.unique_id(),
            StructDeclaration(Box::new(StructDeclaration { name, fields }))
        )
    }

    fn parse_struct_field(&mut self) -> StructField {
        let name = self.parse_identifier();
        self.consume_token(TokenKind::Colon);
        let ty = self.parse_type_annotation();
        StructField {
            id: self.unique_id(),
            name,
            ty,
        }
    }

    fn parse_enum_declaration(&mut self) -> Stmt {
        self.consume_keyword_token(Keyword::Enum);

        let name = self.parse_identifier();
        self.consume_token(TokenKind::LBrace);
        let mut variants = Vec::with_capacity(2);
        while !matches!(self.current_token_kind(), TokenKind::RBrace) {
            variants.push(self.parse_enum_variant());

            if matches!(self.current_token_kind(), TokenKind::Comma) {
                self.advance();
            }
        }
        self.consume_token(TokenKind::RBrace);
        node::stmt!(
            self.unique_id(),
            EnumDeclaration(Box::new(EnumDeclaration { name, variants }))
        )
    }

    /// Parses an enum variant, e.g. `Foo`, `Foo(1, 2, 3)` and
    /// `Foo { bar, baz }`.
    fn parse_enum_variant(&mut self) -> EnumVariant {
        let span = self.create_span_from_current_token();
        let name = self.parse_identifier();
        log::debug!("Parsing enum variant {}", name);
        let mut node = match self.current_token_kind() {
            TokenKind::LParen => {
                self.advance();
                let mut parameters = Vec::new();
                let mut index = 0;
                while !matches!(self.current_token_kind(), TokenKind::RParen) {
                    let ident = Ident::new(&index.to_string(), Span::default());
                    let ty = self.parse_type_annotation();
                    parameters.push(StructField {
                        id: self.unique_id(),
                        name: ident,
                        ty,
                    });
                    if matches!(self.current_token_kind(), TokenKind::Comma) {
                        self.advance();
                    }
                    index += 1;
                }
                self.consume_token(TokenKind::RParen);
                EnumVariant {
                    id: self.unique_id(),
                    name,
                    parameters,
                    span,
                }
            }
            TokenKind::LBrace => {
                self.advance();
                let mut parameters = Vec::new();
                while !matches!(self.current_token_kind(), TokenKind::RBrace) {
                    let ident = self.parse_identifier();
                    let ty = if matches!(self.current_token_kind(), TokenKind::Colon) {
                        self.advance();
                        self.parse_type_annotation()
                    } else {
                        node::Ty {
                            id: self.unique_id(),
                            name: Path::new(vec![Ident::new("unknown", Span::default())]),
                            parameters: Vec::new(),
                            span: Span::default(),
                        }
                    };

                    parameters.push(StructField {
                        id: self.unique_id(),
                        name: ident,
                        ty,
                    });
                    if matches!(self.current_token_kind(), TokenKind::Comma) {
                        self.advance();
                    }
                }
                self.consume_token(TokenKind::RBrace);
                EnumVariant {
                    id: self.unique_id(),
                    name,
                    parameters,
                    span,
                }
            }
            _ => EnumVariant {
                id: self.unique_id(),
                name,
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
            TokenKind::RBrace | TokenKind::Eof
        ) {
            if let (consume_semicolon, Some(mut statement)) = self.parse_statement() {
                if may_complete
                    && matches!(self.current_token_kind(), TokenKind::RBrace)
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
                            Box::new(node::expr!(self.unique_id(), FunctionExpression(decl)))
                        }
                        _ => {
                            self.panic_unexpected_stmt(
                                "expression statement or function declaration",
                                statement,
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
        Module::new(self.unique_id(), self.parse_statements(false).0)
    }

    fn parse_block(&mut self) -> Block {
        let mut span = self.create_span_from_current_token();
        self.consume_token(TokenKind::LBrace);
        let (statements, expression) = self.parse_statements(true);
        self.consume_token(TokenKind::RBrace);
        self.end_span_from_previous_token(&mut span);

        Block::new(self.unique_id(), statements, expression).with_span(span)
    }

    fn parse_block_expr(&mut self) -> Expr {
        node::expr!(self.unique_id(), Block(Box::new(self.parse_block())))
    }

    fn parse_block_stmt(&mut self) -> Stmt {
        node::stmt!(self.unique_id(), Expr(Box::new(self.parse_block_expr())))
    }

    fn parse_let_expression(&mut self) -> Expr {
        self.consume_keyword_token(Keyword::Let);
        let pattern = self.parse_pattern();
        self.consume_token(TokenKind::EqualSign);
        let value = self.parse_expression();
        node::expr!(self.unique_id(), Let(Box::new(pattern), Box::new(value)))
    }

    fn parse_variable_declaration(&mut self) -> Stmt {
        debug!("Parsing variable declaration");
        self.consume_keyword_token(Keyword::Let);

        // We probably want to know whether we are parsing a plain declaration of `if let`.
        let pattern = self.parse_pattern();

        let type_annotation = match self.current_token_kind() {
            TokenKind::Colon => {
                self.advance();
                self.parse_optional_type_annotation()
            }
            _ => None,
        };
        self.consume_token(TokenKind::EqualSign);
        let expression = self.parse_expression();

        node::stmt!(
            self.unique_id(),
            Let(Box::new(LetDeclaration {
                pattern,
                expression,
                type_annotation,
            }))
        )
    }

    /// Parses a function call expression, e.g. `foo()`, `foo(1, 2, 3)` and
    /// `foo { bar, baz }`.
    fn parse_call_expression(&mut self, expr: Expr) -> Expr {
        let mut span = expr.span;
        log::debug!(
            "Parsing call expression, current token: {:?}",
            self.current_token
        );
        let is_dict_call = matches!(self.current_token_kind(), TokenKind::LBrace);
        let mut arguments = Vec::new();

        log::debug!("Is dict call: {}", is_dict_call);

        if is_dict_call {
            arguments.push(self.parse_block_or_dict());
        } else {
            self.consume_token(TokenKind::LParen);
            let mut is_first_argument = true;
            while !matches!(self.current_token_kind(), TokenKind::RParen) {
                if is_first_argument {
                    is_first_argument = false;
                } else {
                    self.consume_token(TokenKind::Comma);
                }

                if matches!(self.current_token_kind(), TokenKind::RParen) {
                    break;
                }

                arguments.push(self.parse_expression());
            }

            self.consume_token(TokenKind::RParen);
        }

        self.end_span_from_previous_token(&mut span);

        node::expr!(
            self.unique_id(),
            Call(Box::new(CallExpression {
                callee: expr,
                arguments,
            }))
        )
        .with_span(span)
    }

    fn parse_identifier_pattern(&mut self) -> Pat {
        let id = self.unique_id();

        // TODO: We only do this because we call `parse_identifier_pattern` within
        // `parse_enum_extraction` and that code there is quite messy.
        if matches!(
            self.current_token_kind(),
            TokenKind::Keyword(Keyword::Underscore)
        ) {
            let current_token = self.advance();
            return node::pat!(id, Wildcard).with_span(current_token.span);
        }

        let name = self.parse_identifier();
        let mut span = name.span;

        let mut node = node::pat!(id, Identifier(Box::new(name)));

        self.end_span_from_previous_token(&mut span);
        node.span = span;
        node
    }

    fn parse_list_extraction(&mut self) -> Pat {
        self.consume_token(TokenKind::LBracket);

        let mut elements = Vec::new();
        // Only allow literal values, identifiers and and rest params for now.
        while !matches!(self.current_token_kind(), TokenKind::RBracket) {
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
                TokenKind::DotDotDot => {
                    self.advance();
                    node::pat!(
                        self.unique_id(),
                        Rest(Box::new(self.parse_identifier_pattern()))
                    )
                }
                _ => self.parse_pattern(),
            };

            elements.push(element);

            // Allow trailing comma.
            if matches!(self.current_token_kind(), TokenKind::RBracket) {
                break;
            }

            self.consume_token(TokenKind::Comma);
        }

        self.consume_token(TokenKind::RBracket);

        node::pat!(self.unique_id(), List(elements))
    }

    fn parse_list_expression(&mut self) -> Expr {
        self.consume_token(TokenKind::LBracket);

        let mut elements = Vec::new();
        while !matches!(self.current_token_kind(), TokenKind::RBracket) {
            let element = match self.current_token_kind() {
                TokenKind::DotDotDot => {
                    self.advance();

                    node::expr!(
                        self.unique_id(),
                        UnaryOp(UnaryOp::Spread, Box::new(self.parse_expression()))
                    )
                }
                _ => self.parse_expression(),
            };

            elements.push(element);

            // Allow trailing comma.
            if matches!(self.current_token_kind(), TokenKind::RBracket) {
                break;
            }

            self.consume_token(TokenKind::Comma);
        }

        self.consume_token(TokenKind::RBracket);

        node::expr!(self.unique_id(), List(elements))
    }

    fn parse_dict_elements(&mut self) -> Vec<(Expr, Expr)> {
        let mut elements = Vec::new();
        while !matches!(self.current_token_kind(), TokenKind::RBrace) {
            let key = self.parse_expression();

            // If key is an single identifier and we follow up with a comma or rbrace, then we are
            // using the short hand syntax for dictionary elements.
            if let ExprKind::Path(path) = &key.kind {
                if path.segments.len() == 1
                    && matches!(
                        self.current_token_kind(),
                        TokenKind::Comma | TokenKind::RBrace
                    )
                {
                    if matches!(self.current_token_kind(), TokenKind::Comma) {
                        self.advance();
                    }
                    elements.push((key.clone(), key));
                    continue;
                }
            }

            self.consume_token(TokenKind::Colon);
            elements.push((key, self.parse_expression()));
            if matches!(self.current_token_kind(), TokenKind::Comma) {
                self.advance();
            }
        }
        elements
    }

    fn parse_block_or_dict(&mut self) -> Expr {
        let mut span = self.create_span_from_current_token();
        self.consume_token(TokenKind::LBrace);
        // If immediately closed or the first token is a identifier followed by a colon or comma,
        // we assume we are parsing a dict.
        let is_dict = matches!(self.current_token_kind(), TokenKind::RBrace)
            || (matches!(self.current_token_kind(), TokenKind::Identifier(_))
                && matches!(self.peek_token_kind(), TokenKind::Colon | TokenKind::Comma));
        if is_dict {
            let elements = self.parse_dict_elements();
            self.consume_token(TokenKind::RBrace);
            return node::expr!(self.unique_id(), Dict(elements));
        }

        let (statements, expression) = self.parse_statements(true);
        self.consume_token(TokenKind::RBrace);
        self.end_span_from_previous_token(&mut span);

        node::expr!(
            self.unique_id(),
            Block(Box::new(
                Block::new(self.unique_id(), statements, expression).with_span(span)
            ))
        )
        .with_span(span)
    }

    /// Can be a Identifier, NestedIdentifier or FieldExpression with a single field name.
    fn parse_function_name(&mut self) -> Expr {
        let path = self.parse_path_expression();
        let mut span = path.span;

        let mut expr = if matches!(self.current_token_kind(), TokenKind::Dot) {
            self.advance();
            node::expr!(
                self.unique_id(),
                FieldExpression(Box::new(FieldAccessExpression {
                    base: path,
                    field: self.parse_identifier(),
                }))
            )
        } else {
            path
        };

        self.end_span_from_previous_token(&mut span);
        expr.span = span;
        expr
    }

    fn parse_if_condition(&mut self) -> Expr {
        self.consume_keyword_token(Keyword::If);

        let condition = if matches!(self.current_token_kind(), TokenKind::Keyword(Keyword::Let)) {
            self.parse_let_expression()
        } else {
            self.parse_expression()
        };

        // TODO: Reevaluate whether we want `foo {}` to be a `foo({})` call expression.
        //       As this collides with `if let` statements.
        // Semicolon or parenthesis are currently needed to disambiguate call with dictionary and block.
        // E.g. `if let x = foo { .. }` is recognized as `foo({ .. })`
        // vs `if let x = foo; { .. }` which will properly parse.
        if matches!(self.current_token_kind(), TokenKind::Semicolon) {
            self.advance();
        }

        condition
    }

    fn parse_if_else_expression(&mut self) -> Expr {
        let condition = self.parse_if_condition();

        self.expect_token(TokenKind::LBrace);
        let then_branch = self.parse_block();

        let mut else_branches = Vec::new();
        while matches!(self.current_token_kind(), TokenKind::Keyword(Keyword::Else)) {
            self.advance();

            let condition = if matches!(self.current_token_kind(), TokenKind::Keyword(Keyword::If))
            {
                Some(self.parse_if_condition())
            } else {
                None
            };

            self.expect_token(TokenKind::LBrace);
            let consequence = self.parse_block();

            if condition.is_none() {
                self.expect_token_not(TokenKind::Keyword(Keyword::Else));
            }

            else_branches.push(ElseClause {
                condition,
                consequence,
            });
        }

        node::expr!(
            self.unique_id(),
            IfElse(Box::new(IfElseExpression {
                condition,
                then_branch,
                else_branches,
            }))
        )
    }

    fn parse_unary_expression(&mut self) -> Expr {
        match self.current_token_kind() {
            TokenKind::Minus => {
                self.advance();

                node::expr!(
                    self.unique_id(),
                    UnaryOp(UnaryOp::Minus, Box::new(self.parse_primary_expression()))
                )
            }

            TokenKind::ExclamationMark | TokenKind::Keyword(Keyword::Not) => {
                self.advance();

                node::expr!(
                    self.unique_id(),
                    UnaryOp(UnaryOp::Not, Box::new(self.parse_primary_expression()))
                )
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
                        | Keyword::Loop
                        | Keyword::For
                        | Keyword::Break
                        | Keyword::Continue
                        | Keyword::Not
                        | Keyword::Underscore
                        | Keyword::_Self
                )
                | TokenKind::Identifier(_)
                | TokenKind::Literal(_)
        );

        let mut span = self.create_span_from_current_token();
        let mut node = match self.current_token_kind() {
            TokenKind::Minus | TokenKind::Plus
                if matches!(
                    self.peek_token_kind(),
                    // During lexing, we only emit unsigned integers.
                    TokenKind::Literal(Literal::UnsignedInteger(_) | Literal::Float(_))
                ) =>
            {
                let invert = matches!(self.advance().kind, TokenKind::Minus);
                let literal = self.advance().take_literal().unwrap();

                if invert {
                    node::expr!(self.unique_id(), Literal(Box::new(literal.invert_sign())))
                } else {
                    node::expr!(self.unique_id(), Literal(Box::new(literal)))
                }
            }

            TokenKind::Minus | TokenKind::ExclamationMark | TokenKind::Keyword(Keyword::Not) => {
                self.parse_unary_expression()
            }
            TokenKind::LParen => {
                self.advance();
                let expression = self.parse_expression();
                self.consume_token(TokenKind::RParen);
                expression
            }
            TokenKind::LBrace => self.parse_block_or_dict(),
            TokenKind::LBracket => self.parse_list_expression(),
            TokenKind::Keyword(Keyword::If) => self.parse_if_else_expression(),
            TokenKind::Keyword(Keyword::Fn) => self.parse_function_expression(),
            TokenKind::Keyword(Keyword::Loop) => {
                self.advance();
                let block = self.parse_block();
                node::expr!(self.unique_id(), Loop(Box::new(block)))
            }
            TokenKind::Keyword(Keyword::For) => {
                self.advance();
                let pat = self.parse_pattern();
                self.consume_token(TokenKind::Keyword(Keyword::In));
                let iter = self.parse_expression();
                let acc = if matches!(self.current_token_kind(), TokenKind::Keyword(Keyword::With))
                {
                    self.advance();
                    let pat = self.parse_pattern();
                    self.consume_token(TokenKind::EqualSign);
                    let expr = self.parse_expression();
                    Some((pat, expr))
                } else {
                    None
                };
                let block = self.parse_block();

                node::expr!(
                    self.unique_id(),
                    ForLoop(Box::new(node::ForLoop {
                        pat,
                        iter,
                        acc,
                        block
                    }))
                )
            }
            TokenKind::Keyword(Keyword::Break) => {
                self.advance();

                if matches!(
                    self.current_token_kind(),
                    // Bit hacky...
                    TokenKind::Semicolon | TokenKind::RBrace
                ) {
                    return node::expr!(self.unique_id(), Break(None));
                }

                let expr = self.parse_expression();
                let expr = if matches!(expr.kind, ExprKind::None) {
                    None
                } else {
                    Some(Box::new(expr))
                };
                node::expr!(self.unique_id(), Break(expr))
            }
            TokenKind::Keyword(Keyword::Rec) => {
                self.advance();
                let expr = self.parse_expression();
                let call_expr = match expr.kind {
                    ExprKind::Call(call) => call,
                    _ => {
                        self.panic_unexpected_expr("call expression", Some(expr));
                        unreachable!()
                    }
                };

                node::expr!(self.unique_id(), RecursiveCall(call_expr)).with_span(expr.span)
            }
            TokenKind::Keyword(Keyword::Match) => self.parse_match_expression(),
            TokenKind::Keyword(Keyword::Underscore) => {
                self.advance();
                node::expr!(self.unique_id(), Wildcard)
            }
            TokenKind::Literal(_) => {
                let literal = self.advance().take_literal().unwrap();
                node::expr!(self.unique_id(), Literal(Box::new(literal)))
            }
            // TODO: Mostly copied from Identifier further below, `self` might be easier to just be
            //       an identifier.
            TokenKind::Keyword(Keyword::_Self) => {
                let identifier_span = self.create_span_from_current_token();

                self.advance();

                let mut path = Path::from_ident(Ident::new(kw::_Self, identifier_span));
                let mut span = path.span;

                while matches!(self.current_token_kind(), TokenKind::PathSeparator) {
                    self.advance();
                    path.push(self.parse_identifier());
                }

                self.end_span_from_previous_token(&mut span);

                let mut expr = node::expr!(self.unique_id(), Path(Box::new(path))).with_span(span);

                if matches!(
                    self.current_token_kind(),
                    TokenKind::LParen | TokenKind::LBrace
                ) {
                    expr = self.parse_call_expression(expr);
                }

                expr
            }
            TokenKind::Identifier(_) => {
                let identifier_span = self.create_span_from_current_token();

                let token = self.advance();
                let identifier = token.get_identifier().unwrap();

                let mut path = Path::from_ident(Ident::new(identifier, identifier_span));
                let mut span = path.span;

                while matches!(self.current_token_kind(), TokenKind::PathSeparator) {
                    self.advance();
                    path.push(self.parse_identifier());
                }

                self.end_span_from_previous_token(&mut span);

                let mut expr = node::expr!(self.unique_id(), Path(Box::new(path))).with_span(span);

                if matches!(
                    self.current_token_kind(),
                    TokenKind::LParen | TokenKind::LBrace
                ) {
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

        // TODO: Reevaluate whether we want `foo {}` to be a `foo({})` call expression.
        //       As this collides with `if let` statements.
        // Semicolon or parenthesis are currently needed to disambiguate call with dictionary and block.
        // E.g. `if let x = foo { .. }` is recognized as `foo({ .. })`
        // vs `if let x = foo; { .. }` which will properly parse.
        if matches!(self.current_token_kind(), TokenKind::Semicolon) {
            self.advance();
        }

        self.consume_token(TokenKind::LBrace);

        let mut arms = Vec::new();
        while !matches!(self.current_token_kind(), TokenKind::RBrace) {
            let pattern = self.parse_pattern();
            let guard = self.parse_guard_clause();
            self.consume_token(TokenKind::FatArrow);
            let expression = self.parse_expression();
            arms.push(MatchArm {
                id: self.unique_id(),
                pattern,
                guard,
                expression,
            });
            if matches!(self.current_token_kind(), TokenKind::Comma) {
                self.advance();
            }
        }

        self.consume_token(TokenKind::RBrace);

        node::expr!(
            self.unique_id(),
            Match(Box::new(MatchExpression { expression, arms }))
        )
    }

    fn parse_parameter_list(&mut self) -> Vec<FunctionParameter> {
        let mut parameters = Vec::new();

        if matches!(self.current_token_kind(), TokenKind::LParen) {
            self.consume_token(TokenKind::LParen);

            let mut is_first_parameter = true;
            while !matches!(self.current_token_kind(), TokenKind::RParen) {
                if is_first_parameter {
                    is_first_parameter = false;
                } else {
                    self.consume_token(TokenKind::Comma);
                    if matches!(self.current_token_kind(), TokenKind::RParen) {
                        break;
                    }
                }

                let mut parameter_span = self.create_span_from_current_token();
                let parameter_pattern = self.parse_pattern();

                let type_annotation = match self.current_token_kind() {
                    TokenKind::Colon => {
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
            TokenKind::Identifier(_) => {
                let identifier = self.parse_path();
                let parameters = self.parse_type_annotation_parameters();

                Ty::new(self.unique_id(), identifier).with_parameters(parameters)
            }
            token => unreachable!("Expected type annotation, found {:?}", token),
        }
    }

    fn parse_optional_type_annotation(&mut self) -> Option<Ty> {
        let token = self.current_token_kind();

        if !Self::is_type_annotation_token(token) {
            return None;
        }

        Some(self.parse_type_annotation())
    }

    fn parse_type_annotation_parameters(&mut self) -> Vec<Ty> {
        let mut parameters = Vec::new();

        if matches!(self.current_token_kind(), TokenKind::LessThan) {
            self.advance();
            while !matches!(self.current_token_kind(), TokenKind::GreaterThan) {
                let parameter = self.parse_type_annotation();
                parameters.push(parameter);
                if matches!(self.current_token_kind(), TokenKind::Comma) {
                    self.advance();
                }
            }
            self.consume_token(TokenKind::GreaterThan);
        }

        parameters
    }

    fn parse_return_type(&mut self) -> Option<Ty> {
        if matches!(self.current_token_kind(), TokenKind::Arrow) {
            self.advance();
            self.parse_optional_type_annotation()
        } else {
            None
        }
    }

    fn parse_function_expression(&mut self) -> Expr {
        let mut span = self.create_span_from_current_token();
        self.consume_keyword_token(Keyword::Fn);
        let name = if matches!(self.current_token_kind(), TokenKind::Identifier(_)) {
            self.parse_single_identifier()
        } else {
            node::expr!(
                self.unique_id(),
                Path(Box::new(Path::from_ident(Ident::new(
                    "anonymous",
                    Default::default()
                ))))
            )
        };

        let parameters = self.parse_parameter_list();
        let return_type = self.parse_return_type();
        let body = self.parse_block();

        self.end_span_from_previous_token(&mut span);

        node::expr!(
            self.unique_id(),
            FunctionExpression(Box::new(FunctionDeclaration {
                id: self.unique_id(),
                name,
                parameters,
                body,
                return_type_annotation: return_type,
                span,
                ..Default::default()
            }))
        )
        .with_span(span)
    }

    /// Parses an enum extraction, e.g. `Foo(bar, baz)` and `Foo { bar, baz }`
    fn parse_enum_extraction(&mut self) -> Pat {
        let path = self.parse_path();
        let is_dict_extraction = matches!(self.current_token_kind(), TokenKind::LBrace);

        if is_dict_extraction {
            self.consume_token(TokenKind::LBrace);
            let mut elements = Vec::new();
            while !matches!(self.current_token_kind(), TokenKind::RBrace) {
                let ident = self.parse_identifier();

                if matches!(self.current_token_kind(), TokenKind::Colon) {
                    self.advance();
                    let pattern = self.parse_pattern();
                    elements.push((ident, pattern));
                } else {
                    elements.push((
                        ident.clone(),
                        node::pat!(self.unique_id(), Identifier(Box::new(ident))),
                    ));
                }

                if matches!(self.current_token_kind(), TokenKind::Comma) {
                    self.advance();
                }
            }
            self.consume_token(TokenKind::RBrace);
            node::pat!(
                self.unique_id(),
                Enum(Box::new(EnumPattern { path, elements }))
            )
        } else if matches!(self.current_token_kind(), TokenKind::LParen) {
            self.consume_token(TokenKind::LParen);
            let mut elements = Vec::new();
            let mut index = 0;
            while !matches!(self.current_token_kind(), TokenKind::RParen) {
                let ident = Ident::new(&index.to_string(), Span::default());
                index += 1;
                elements.push((ident, self.parse_pattern()));
                if matches!(self.current_token_kind(), TokenKind::Comma) {
                    self.advance();
                }
            }
            self.consume_token(TokenKind::RParen);
            node::pat!(
                self.unique_id(),
                Enum(Box::new(EnumPattern { path, elements }))
            )
        } else {
            node::pat!(
                self.unique_id(),
                Enum(Box::new(EnumPattern {
                    path,
                    elements: Vec::new(),
                }))
            )
        }
    }

    fn parse_function_declaration(&mut self) -> Stmt {
        let mut name: Option<Expr> = None;
        let mut declarations: Vec<FunctionDeclaration> = Vec::new();

        while matches!(
            self.current_token_kind(),
            TokenKind::Keyword(Keyword::Fn)
                | TokenKind::SingleLineComment(_)
                | TokenKind::MultiLineComment(_)
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

            if matches!(self.current_token_kind(), TokenKind::Keyword(Keyword::Fn)) {
                self.advance();
            } else {
                // We found a comment, but not a function declaration, stop parsing function
                // declarations and rewind the lexer.
                self.restore_state(saved_state);
                break;
            }

            if matches!(self.current_token_kind(), TokenKind::Identifier(_)) {
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
                id: self.unique_id(),
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
            return node::stmt!(
                self.unique_id(),
                FunctionDeclaration(Box::new(declarations.pop().unwrap()))
            );
        }

        node::stmt!(self.unique_id(), FunctionDeclarations(declarations))
    }

    fn parse_guard_clause(&mut self) -> Option<Expr> {
        if !matches!(self.current_token_kind(), TokenKind::Keyword(Keyword::If)) {
            return None;
        }

        self.advance();

        if matches!(self.current_token_kind(), TokenKind::Keyword(Keyword::Let)) {
            return Some(self.parse_let_expression());
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
        if matches!(self.current_token_kind(), TokenKind::Semicolon) {
            self.advance();
        }

        Some(expression)
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
                | TokenKind::EqualSign
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
            TokenKind::EqualSign => BinaryOpKind::Assign,
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
            BinaryOpKind::Assign => OperatorInfo {
                precedence: 1,
                associativity: Associativity::Right,
            },
            BinaryOpKind::Add | BinaryOpKind::Subtract => OperatorInfo {
                precedence: 7,
                associativity: Associativity::Left,
            },
            BinaryOpKind::Multiply | BinaryOpKind::Divide | BinaryOpKind::Modulo => OperatorInfo {
                precedence: 8,
                associativity: Associativity::Left,
            },
            BinaryOpKind::Equal
            | BinaryOpKind::NotEqual
            | BinaryOpKind::LessThan
            | BinaryOpKind::LessThanOrEqual
            | BinaryOpKind::GreaterThan
            | BinaryOpKind::GreaterThanOrEqual => OperatorInfo {
                precedence: 6,
                associativity: Associativity::Left,
            },
            BinaryOpKind::And => OperatorInfo {
                precedence: 4,
                associativity: Associativity::Left,
            },
            BinaryOpKind::Or => OperatorInfo {
                precedence: 3,
                associativity: Associativity::Left,
            },
            BinaryOpKind::BitwiseAnd | BinaryOpKind::BitwiseOr | BinaryOpKind::BitwiseXor => {
                OperatorInfo {
                    precedence: 9,
                    associativity: Associativity::Left,
                }
            }
            BinaryOpKind::Exponentiation => OperatorInfo {
                precedence: 10,
                associativity: Associativity::Right,
            },
            BinaryOpKind::Pipeline => OperatorInfo {
                precedence: 2,
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

    fn parse_literal(&mut self) -> Literal {
        expect_token_matches!(self, "literal", TokenKind::Literal(_));

        match self.current_token_kind() {
            TokenKind::Literal(_) => self.advance().take_literal().unwrap(),
            _ => unreachable!("Expected literal"),
        }
    }

    fn parse_pattern_literal(&mut self) -> Pat {
        expect_token_matches!(self, "literal", TokenKind::Literal(_));

        node::pat!(self.unique_id(), Literal(Box::new(self.parse_literal())))
    }

    fn parse_pattern(&mut self) -> Pat {
        let comments = self.parse_comments();

        expect_token_matches!(
            self,
            "literal, identifier or list extraction",
            TokenKind::Literal(_)
                | TokenKind::Identifier(_)
                | TokenKind::Keyword(Keyword::Underscore | Keyword::_Self)
                | TokenKind::LBracket
        );

        debug!("Parsing pattern {:?}", self.current_token);

        let mut span = self.create_span_from_current_token();
        let mut node = match self.current_token_kind() {
            // Literal values will be pattern matched
            TokenKind::Literal(_) => self.parse_pattern_literal(),
            TokenKind::Keyword(Keyword::_Self) => {
                self.advance();
                node::pat!(self.unique_id(), _Self)
            }
            TokenKind::Keyword(Keyword::Underscore) => {
                self.advance();
                node::pat!(self.unique_id(), Wildcard)
            }
            // Identifiers can be namespaced identifiers or call expressions, which should be pattern matched.
            // Simple identifiers will be used as is and mapped to a function parameter.
            TokenKind::Identifier(_) => match self.peek_token_kind() {
                TokenKind::PathSeparator => self.parse_enum_extraction(),
                TokenKind::LParen | TokenKind::LBrace => self.parse_enum_extraction(),
                _ => self.parse_identifier_pattern(),
            },
            TokenKind::LBracket => self.parse_list_extraction(),
            _ => unreachable!("Expected pattern, found {:?}", self.current_token_kind()),
        };

        self.end_span_from_previous_token(&mut span);
        node.span = span;
        node.leading_comments = comments;
        node.trailing_comments = self.parse_comments();
        node
    }

    fn parse_expression(&mut self) -> Expr {
        debug!("Parsing expression {:?}", self.current_token);

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
                TokenKind::Dot => {
                    self.advance();
                    lhs.trailing_comments = self.parse_comments();
                    let field_name = self.parse_identifier();
                    lhs = node::expr!(
                        self.unique_id(),
                        FieldExpression(Box::new(FieldAccessExpression {
                            base: lhs,
                            field: field_name,
                        }))
                    );
                }
                TokenKind::LBracket => {
                    self.advance();
                    let index = self.parse_expression_with_precedence(0, Associativity::Left);
                    self.consume_token(TokenKind::RBracket);
                    lhs = node::expr!(
                        self.unique_id(),
                        IndexExpression(Box::new(IndexAccessExpression { base: lhs, index }))
                    );
                }
                TokenKind::LParen => {
                    lhs = self.parse_call_expression(lhs);
                }
                token if Self::is_binary_op(token) => {
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

                    lhs = node::expr!(
                        self.unique_id(),
                        BinaryOp(Box::new(BinaryOpExpression {
                            op: operator,
                            lhs,
                            rhs,
                        }))
                    );
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
