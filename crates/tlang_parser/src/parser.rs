use tlang_ast::keyword::{Keyword, kw};
use tlang_ast::node::{
    self, AssociatedTypeBinding, AssociatedTypeDeclaration, Associativity, BinaryOpExpression,
    BinaryOpKind, Block, CallExpression, ConstDeclaration, ElseClause, EnumDeclaration,
    EnumPattern, EnumVariant, Expr, ExprKind, FieldAccessExpression, FunctionDeclaration,
    FunctionParameter, FunctionTypeParam, Ident, IfElseExpression, ImplBlock,
    IndexAccessExpression, LetDeclaration, MatchArm, MatchExpression, ModDeclaration, Module,
    OperatorInfo, Pat, Path, ProtocolDeclaration, ProtocolMethodSignature, Stmt, StmtKind,
    StructDeclaration, StructField, Ty, TyKind, TypeParam, UnaryOp, UseDeclaration, UseItem,
    Visibility, WhereClause, WherePredicate,
};
use tlang_ast::token::{CommentKind, CommentToken, Literal, TaggedStringPart, Token, TokenKind};
use tlang_lexer::Lexer;
use tlang_span::{NodeId, NodeIdAllocator, Span};

use crate::error::{ParseError, ParseIssue, ParseIssueKind};
use crate::macros::expect_token_matches;
use log::debug;

/// Metadata produced alongside the parsed [`Module`] by [`Parser::parse`].
///
/// Mirrors the pattern of [`tlang_hir::LowerResultMeta`] for the lowering phase.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseMeta {
    /// NodeIds of expressions that produce compile-time-constant values
    /// (e.g. tagged string parts lists). Pass these to `lower_to_hir` so they
    /// are mapped to HIR ids and registered in the runtime constant pool.
    pub constant_pool_node_ids: Vec<NodeId>,
    /// State of the node-id allocator after parsing. Use this when chaining a
    /// subsequent incremental parse to avoid NodeId collisions.
    pub node_id_allocator: NodeIdAllocator,
}

pub struct Parser<'src> {
    lexer: Lexer<'src>,
    previous_span: Span,
    current_token: Token,
    next_token: Token,

    node_id_allocator: NodeIdAllocator,

    errors: Vec<ParseIssue>,

    trailing_comments_buffer: Vec<CommentToken>,

    /// NodeIds of expressions that produce compile-time-constant values
    /// (e.g. tagged string parts lists). Propagated through lowering into
    /// the HIR constant pool for singleton caching at runtime.
    constant_pool_node_ids: Vec<NodeId>,
}

impl<'src> Parser<'src> {
    pub fn new(lexer: Lexer<'src>) -> Parser<'src> {
        Parser {
            lexer,
            previous_span: Span::default(),
            current_token: Token::default(),
            next_token: Token::default(),
            node_id_allocator: NodeIdAllocator::default(),
            errors: Vec::new(),
            trailing_comments_buffer: Vec::new(),
            constant_pool_node_ids: Vec::new(),
        }
    }

    pub fn with_line_offset(mut self, line_offset: u32) -> Self {
        self.lexer.set_line_offset(line_offset);
        self
    }

    pub fn with_byte_offset(mut self, byte_offset: u32) -> Self {
        self.lexer.set_byte_offset(byte_offset);
        self
    }

    pub fn errors(&self) -> &[ParseIssue] {
        &self.errors
    }

    pub fn set_node_id_allocator(mut self, allocator: NodeIdAllocator) -> Self {
        self.node_id_allocator = allocator;
        self
    }

    fn unique_id(&mut self) -> NodeId {
        self.node_id_allocator.next_id()
    }

    pub fn from_source(source: &'src str) -> Parser<'src> {
        Parser::new(Lexer::new(source))
    }

    pub fn parse(&mut self) -> Result<(Module, ParseMeta), ParseError> {
        self.current_token = self.lexer.next_token();
        self.next_token = self.lexer.next_token();

        let module = self.parse_module();

        if self.errors.is_empty() {
            let meta = ParseMeta {
                constant_pool_node_ids: std::mem::take(&mut self.constant_pool_node_ids),
                node_id_allocator: self.node_id_allocator,
            };
            Ok((module, meta))
        } else {
            Err(ParseError::new(self.errors.clone()))
        }
    }

    fn create_span_from_current_token(&self) -> Span {
        self.current_token.span
    }

    fn end_span_from_previous_token(&self, span: &mut Span) {
        span.end = self.previous_span.end;
        span.end_lc = self.previous_span.end_lc;
    }

    #[inline(never)]
    fn push_unexpected_token_error(&mut self, expected: &str, actual: Token) {
        let span = actual.span;
        let msg = format!("Expected {}, found {:?}", expected, actual.kind);
        let kind = ParseIssueKind::UnexpectedToken(format!("{:?}", actual.kind));

        self.errors.push(ParseIssue { msg, kind, span });
    }

    #[inline(never)]
    fn push_unexpected_expr_error(&mut self, expected: &str, actual: &Expr) {
        let span = actual.span;
        let msg = format!("Expected {}, found {:?}", expected, actual.kind);
        let kind = ParseIssueKind::UnexpectedToken(format!("{:?}", actual.kind));

        self.errors.push(ParseIssue { msg, kind, span });
    }

    #[inline(never)]
    #[allow(clippy::needless_pass_by_value)]
    fn panic_unexpected_stmt(&self, expected: &str, actual: Stmt) -> ! {
        let start_lc = actual.span.start_lc;
        let source_line = self
            .lexer
            .source()
            .lines()
            .nth(start_lc.line as usize)
            .unwrap_or_default();
        let caret = " ".repeat(start_lc.column as usize) + "^";

        panic!(
            "Expected {} on line {}, column {}, found {:?} instead\n{}\n{}",
            expected, start_lc.line, start_lc.column, actual.kind, source_line, caret
        );
    }

    #[allow(clippy::needless_pass_by_value)]
    fn expect_token(&mut self, expected: TokenKind) {
        expect_token_matches!(
            self,
            &format!("{expected:?}"),
            _kind if _kind == expected
        );
    }

    #[allow(clippy::needless_pass_by_value)]
    fn expect_token_not(&mut self, expected: TokenKind) {
        expect_token_matches!(
            self,
            &format!("not {expected:?}"),
            _kind if _kind != expected
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

    /// Returns `true` for tokens that may legally be parsed as identifiers.
    ///
    /// `type` and `where` are contextual keywords for protocol syntax, but they
    /// remain valid identifiers in ordinary expression, pattern, and binding
    /// positions to preserve existing programs.
    fn is_contextual_identifier_token(token: TokenKind) -> bool {
        matches!(
            token,
            TokenKind::Identifier
                | TokenKind::Keyword(Keyword::Type)
                | TokenKind::Keyword(Keyword::Where)
        )
    }

    fn parse_identifier(&mut self) -> Ident {
        expect_token_matches!(
            self,
            TokenKind::Identifier
                | TokenKind::Keyword(Keyword::Type)
                | TokenKind::Keyword(Keyword::Where)
        );

        let current_token = self.advance();
        let name = self.lexer.span_text(current_token.span);

        Ident::new(name, current_token.span)
    }

    fn at_eof(&self) -> bool {
        matches!(self.current_token_kind(), TokenKind::Eof)
    }

    /// Returns `true` while the current token is **not** the expected closing
    /// delimiter and the parser has not reached EOF.  Use this in every
    /// delimiter-bounded `while` loop instead of a manual `matches!` check so
    /// that an unexpected EOF always terminates the loop instead of spinning
    /// forever on incomplete input.
    fn not_at_closing(&self, closing: TokenKind) -> bool {
        let current = self.current_token_kind();
        current != closing && current != TokenKind::Eof
    }

    fn advance(&mut self) -> Token {
        let consumed = self.current_token;

        if self.at_eof() {
            self.next_token = Token::new(TokenKind::Eof, consumed.span);
            return consumed;
        }

        self.current_token = self.next_token;
        self.next_token = self.lexer.next_token();
        self.previous_span = consumed.span;
        debug!("Advanced to {:?}", self.current_token);
        consumed
    }

    #[inline(always)]
    fn current_token_kind(&self) -> TokenKind {
        self.current_token.kind
    }

    #[inline(always)]
    pub fn peek_token_kind(&self) -> TokenKind {
        self.next_token.kind
    }

    fn save_state(&self) -> (Lexer<'src>, Token, Token) {
        debug!("Saving state at {:?}", self.current_token);

        (self.lexer.clone(), self.current_token, self.next_token)
    }

    fn restore_state(&mut self, state: (Lexer<'src>, Token, Token)) {
        debug!("Restoring state to {:?}", state.1);

        self.lexer = state.0;
        self.current_token = state.1;
        self.next_token = state.2;
    }

    fn parse_comments(&mut self) -> Vec<CommentToken> {
        let mut comments: Vec<CommentToken> = Vec::new();
        while matches!(
            self.current_token_kind(),
            TokenKind::SingleLineComment | TokenKind::MultiLineComment
        ) {
            let token = self.advance();
            let (kind, text) = match token.kind {
                TokenKind::SingleLineComment => {
                    let full = self.lexer.span_text(token.span);
                    // Strip the leading "//"
                    (CommentKind::SingleLine, &full[2..])
                }
                TokenKind::MultiLineComment => {
                    let full = self.lexer.span_text(token.span);
                    // Strip the leading "/*" and trailing "*/" (if present).
                    // A properly closed comment is at least 4 chars: "/**/".
                    let inner = if full.len() >= 4 && full.ends_with("*/") {
                        &full[2..full.len() - 2]
                    } else {
                        &full[2..]
                    };
                    (CommentKind::MultiLine, inner)
                }
                _ => unreachable!(),
            };
            comments.push(CommentToken {
                kind,
                text: text.into(),
                span: token.span,
            });
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

        // Comments before a closing brace are trailing comments of the previous statement.
        if matches!(self.current_token_kind(), TokenKind::RBrace) {
            self.trailing_comments_buffer = comments;
            return (false, None);
        }

        let mut span = self.create_span_from_current_token();
        let visibility = if matches!(self.current_token_kind(), TokenKind::Keyword(Keyword::Pub)) {
            self.advance();
            Visibility::Public
        } else {
            Visibility::Private
        };
        let mut node = match self.current_token.kind {
            TokenKind::Keyword(Keyword::Use) => self.parse_use_declaration(visibility),
            TokenKind::Keyword(Keyword::Mod) => self.parse_mod_declaration(visibility),
            TokenKind::Keyword(Keyword::Let) => self.parse_variable_declaration(),
            TokenKind::Keyword(Keyword::Const) => self.parse_const_declaration(visibility),
            TokenKind::Keyword(Keyword::Fn)
                // If the next token is an identifier, we assume a function declaration.
                // If it's not, we assume a function expression which is handled as a primary expression.
                if matches!(self.peek_token_kind(), TokenKind::Identifier) =>
            {
                self.parse_function_declaration(visibility)
            }
            TokenKind::Keyword(Keyword::Enum) => self.parse_enum_declaration(visibility),
            TokenKind::Keyword(Keyword::Struct) => self.parse_struct_declaration(visibility),
            TokenKind::Keyword(Keyword::Protocol) => self.parse_protocol_declaration(visibility),
            TokenKind::Keyword(Keyword::Return) => self.parse_return_statement(),
            TokenKind::Keyword(Keyword::Impl) => self.parse_impl_block(),
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
            | StmtKind::StructDeclaration { .. }
            // Nor do ProtocolDeclaration/ImplBlock statements.
            | StmtKind::ProtocolDeclaration { .. }
            | StmtKind::ImplBlock { .. } => {
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
            return node::stmt!(self.unique_id(), Return(None));
        }

        node::stmt!(
            self.unique_id(),
            Return(Some(Box::new(self.parse_expression())))
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

    /// Check if the current position is at a `const` or `pub const` item.
    /// Returns `Some(visibility)` if so, `None` otherwise.
    fn try_parse_const_item_visibility(&mut self) -> Option<Visibility> {
        if matches!(
            self.current_token_kind(),
            TokenKind::Keyword(Keyword::Const)
        ) {
            Some(Visibility::Private)
        } else if matches!(self.current_token_kind(), TokenKind::Keyword(Keyword::Pub))
            && matches!(self.peek_token_kind(), TokenKind::Keyword(Keyword::Const))
        {
            self.advance(); // consume `pub`
            Some(Visibility::Public)
        } else {
            None
        }
    }

    fn parse_const_item(&mut self, item_visibility: Visibility) -> ConstDeclaration {
        let mut span = self.create_span_from_current_token();
        self.consume_keyword_token(Keyword::Const);
        let const_name = self.parse_identifier();
        let type_annotation = match self.current_token_kind() {
            TokenKind::Colon => {
                self.advance();
                self.parse_optional_type_annotation()
            }
            _ => None,
        };
        self.consume_token(TokenKind::EqualSign);
        let expression = self.parse_expression();
        self.end_span_from_previous_token(&mut span);
        if matches!(self.current_token_kind(), TokenKind::Semicolon) {
            self.advance();
        }
        ConstDeclaration {
            id: self.unique_id(),
            visibility: item_visibility,
            name: const_name,
            expression,
            type_annotation,
            span,
        }
    }

    fn parse_struct_declaration(&mut self, visibility: Visibility) -> Stmt {
        self.consume_keyword_token(Keyword::Struct);
        let name = self.parse_identifier();
        let type_params = self.parse_type_params();
        self.consume_token(TokenKind::LBrace);
        let mut fields = Vec::with_capacity(2);
        let mut consts = Vec::new();
        while self.not_at_closing(TokenKind::RBrace) {
            if let Some(item_visibility) = self.try_parse_const_item_visibility() {
                consts.push(self.parse_const_item(item_visibility));
            } else {
                fields.push(self.parse_struct_field());

                if matches!(self.current_token_kind(), TokenKind::Comma) {
                    self.advance();
                }
            }
        }
        self.consume_token(TokenKind::RBrace);
        node::stmt!(
            self.unique_id(),
            StructDeclaration(Box::new(StructDeclaration {
                visibility,
                name,
                type_params,
                fields,
                consts,
            }))
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

    fn parse_enum_declaration(&mut self, visibility: Visibility) -> Stmt {
        self.consume_keyword_token(Keyword::Enum);

        let name = self.parse_identifier();
        let type_params = self.parse_type_params();
        self.consume_token(TokenKind::LBrace);
        let mut variants = Vec::with_capacity(2);
        let mut consts = Vec::new();
        while self.not_at_closing(TokenKind::RBrace) {
            if let Some(item_visibility) = self.try_parse_const_item_visibility() {
                consts.push(self.parse_const_item(item_visibility));
            } else {
                variants.push(self.parse_enum_variant());

                if matches!(self.current_token_kind(), TokenKind::Comma) {
                    self.advance();
                }
            }
        }
        self.consume_token(TokenKind::RBrace);
        node::stmt!(
            self.unique_id(),
            EnumDeclaration(Box::new(EnumDeclaration {
                visibility,
                name,
                type_params,
                variants,
                consts,
            }))
        )
    }

    /// Parses an enum variant, e.g. `Foo`, `Foo(1, 2, 3)`, `Foo { bar, baz }`,
    /// and `Foo = <expr>` for simple variants with discriminant values.
    fn parse_enum_variant(&mut self) -> EnumVariant {
        let span = self.create_span_from_current_token();
        let name = self.parse_identifier();
        log::debug!("Parsing enum variant {name}");
        let mut node = match self.current_token_kind() {
            TokenKind::LParen => {
                self.advance();
                let mut parameters = Vec::new();
                let mut index = 0;
                while self.not_at_closing(TokenKind::RParen) {
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
                    discriminant: None,
                    span,
                }
            }
            TokenKind::LBrace => {
                self.advance();
                let mut parameters = Vec::new();
                while self.not_at_closing(TokenKind::RBrace) {
                    let ident = self.parse_identifier();
                    let ty = if matches!(self.current_token_kind(), TokenKind::Colon) {
                        self.advance();
                        self.parse_type_annotation()
                    } else {
                        node::Ty {
                            id: self.unique_id(),
                            kind: TyKind::Unknown,
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
                    discriminant: None,
                    span,
                }
            }
            _ => {
                let discriminant = if matches!(self.current_token_kind(), TokenKind::EqualSign) {
                    self.advance();
                    Some(Box::new(self.parse_expression()))
                } else {
                    None
                };
                EnumVariant {
                    id: self.unique_id(),
                    name,
                    parameters: Vec::new(),
                    discriminant,
                    span,
                }
            }
        };

        self.end_span_from_previous_token(&mut node.span);
        node
    }

    fn parse_protocol_declaration(&mut self, visibility: Visibility) -> Stmt {
        self.consume_keyword_token(Keyword::Protocol);
        let name = self.parse_identifier();
        let type_params = self.parse_type_params();

        // Parse optional constraint list: `: A + B + C`
        let constraints = if matches!(self.current_token_kind(), TokenKind::Colon) {
            self.advance();
            let mut constraints = vec![self.parse_path()];
            while matches!(self.current_token_kind(), TokenKind::Plus) {
                self.advance();
                constraints.push(self.parse_path());
            }
            constraints
        } else {
            Vec::new()
        };

        self.consume_token(TokenKind::LBrace);
        let mut methods = Vec::new();
        let mut consts = Vec::new();
        let mut associated_types = Vec::new();
        while self.not_at_closing(TokenKind::RBrace) {
            if matches!(
                self.current_token_kind(),
                TokenKind::SingleLineComment | TokenKind::MultiLineComment
            ) {
                self.parse_comments();
            } else if let Some(item_visibility) = self.try_parse_const_item_visibility() {
                consts.push(self.parse_const_item(item_visibility));
            } else if matches!(self.current_token_kind(), TokenKind::Keyword(Keyword::Type)) {
                associated_types.push(self.parse_associated_type_declaration());
            } else {
                methods.push(self.parse_protocol_method_signature());
            }
        }
        self.consume_token(TokenKind::RBrace);
        node::stmt!(
            self.unique_id(),
            ProtocolDeclaration(Box::new(ProtocolDeclaration {
                visibility,
                name,
                type_params,
                constraints,
                associated_types,
                methods,
                consts,
            }))
        )
    }

    /// Parse an associated type declaration inside a protocol body, such as
    /// `type Wrapped<T, U>`.
    fn parse_associated_type_declaration(&mut self) -> AssociatedTypeDeclaration {
        let mut span = self.create_span_from_current_token();
        self.consume_keyword_token(Keyword::Type);
        let name = self.parse_identifier();
        let type_params = self.parse_type_params();
        self.consume_optional_semicolon();
        self.end_span_from_previous_token(&mut span);
        AssociatedTypeDeclaration {
            id: self.unique_id(),
            name,
            type_params,
            span,
        }
    }

    fn parse_protocol_method_signature(&mut self) -> ProtocolMethodSignature {
        let mut span = self.create_span_from_current_token();
        self.consume_keyword_token(Keyword::Fn);
        let name = self.parse_identifier();
        let type_params = self.parse_type_params();
        self.expect_token(TokenKind::LParen);
        let (parameters, _params_span) = self.parse_parameter_list();
        let return_type = self.parse_return_type();

        // Optional default implementation body
        let body = if matches!(self.current_token_kind(), TokenKind::LBrace) {
            Some(self.parse_block())
        } else {
            None
        };

        self.end_span_from_previous_token(&mut span);
        ProtocolMethodSignature {
            id: self.unique_id(),
            name,
            type_params,
            parameters,
            return_type_annotation: return_type,
            body,
            span,
        }
    }

    /// Parse an optional impl-level where clause like
    /// `where T: Bound1 + Bound2, U: Bound3`.
    ///
    /// Returns `None` when no `where` keyword is present.
    fn parse_where_clause(&mut self) -> Option<WhereClause> {
        if !matches!(
            self.current_token_kind(),
            TokenKind::Keyword(Keyword::Where)
        ) {
            return None;
        }

        let mut span = self.create_span_from_current_token();
        self.consume_keyword_token(Keyword::Where);
        let mut predicates = Vec::new();

        loop {
            predicates.push(self.parse_where_predicate());
            if !matches!(self.current_token_kind(), TokenKind::Comma) {
                break;
            }
            self.advance();
        }

        self.end_span_from_previous_token(&mut span);
        Some(WhereClause { predicates, span })
    }

    /// Parse a single where-clause predicate like `T: Display + Clone`.
    fn parse_where_predicate(&mut self) -> WherePredicate {
        let mut span = self.create_span_from_current_token();
        let name = self.parse_identifier();
        self.consume_token(TokenKind::Colon);

        let mut bounds = vec![self.parse_type_annotation()];
        while matches!(self.current_token_kind(), TokenKind::Plus) {
            self.advance();
            bounds.push(self.parse_type_annotation());
        }

        self.end_span_from_previous_token(&mut span);
        WherePredicate { name, bounds, span }
    }

    /// Parse an associated type binding inside an impl block, such as
    /// `type Wrapped<T> = ConcreteType<T>`.
    fn parse_associated_type_binding(&mut self) -> AssociatedTypeBinding {
        let mut span = self.create_span_from_current_token();
        self.consume_keyword_token(Keyword::Type);
        let name = self.parse_identifier();
        let type_params = self.parse_type_params();
        self.consume_token(TokenKind::EqualSign);
        let ty = self.parse_type_annotation();
        self.consume_optional_semicolon();
        self.end_span_from_previous_token(&mut span);
        AssociatedTypeBinding {
            id: self.unique_id(),
            name,
            type_params,
            ty,
            span,
        }
    }

    fn consume_optional_semicolon(&mut self) {
        if matches!(self.current_token_kind(), TokenKind::Semicolon) {
            self.advance();
        }
    }

    fn parse_impl_block(&mut self) -> Stmt {
        self.consume_keyword_token(Keyword::Impl);
        let type_params = self.parse_type_params();
        let protocol_name = self.parse_path();
        let type_arguments = self.parse_type_annotation_parameters();
        self.consume_keyword_token(Keyword::For);
        let target_type = self.parse_path();
        let target_type_arguments = self.parse_type_annotation_parameters();
        let where_clause = self.parse_where_clause();
        self.consume_token(TokenKind::LBrace);
        let mut methods = Vec::new();
        let mut apply_methods = Vec::new();
        let mut associated_types = Vec::new();
        while matches!(
            self.current_token_kind(),
            TokenKind::Keyword(Keyword::Fn)
                | TokenKind::Keyword(Keyword::Apply)
                | TokenKind::Keyword(Keyword::Type)
                | TokenKind::SingleLineComment
                | TokenKind::MultiLineComment
        ) {
            // Consume stray comments so the loop makes progress even if no
            // fn/apply/type follows.
            if matches!(
                self.current_token_kind(),
                TokenKind::SingleLineComment | TokenKind::MultiLineComment
            ) {
                self.parse_comments();
                continue;
            }

            if matches!(
                self.current_token_kind(),
                TokenKind::Keyword(Keyword::Apply)
            ) {
                self.consume_keyword_token(Keyword::Apply);
                apply_methods.push(self.parse_identifier());
                while matches!(self.current_token_kind(), TokenKind::Comma) {
                    self.consume_token(TokenKind::Comma);
                    apply_methods.push(self.parse_identifier());
                }
                self.consume_token(TokenKind::Semicolon);
            } else if matches!(self.current_token_kind(), TokenKind::Keyword(Keyword::Type)) {
                associated_types.push(self.parse_associated_type_binding());
            } else {
                let fn_stmt = self.parse_function_declaration(Visibility::Private);
                match fn_stmt.kind {
                    StmtKind::FunctionDeclaration(decl) => methods.push(*decl),
                    StmtKind::FunctionDeclarations(decls) => {
                        if decls.is_empty() {
                            // parse_function_declaration failed and restored state
                            // without consuming any tokens. Break to avoid infinite loop.
                            break;
                        }
                        methods.extend(decls);
                    }
                    _ => unreachable!(),
                }
            }
        }
        self.consume_token(TokenKind::RBrace);
        node::stmt!(
            self.unique_id(),
            ImplBlock(Box::new(ImplBlock {
                type_params,
                protocol_name,
                type_arguments,
                target_type,
                target_type_arguments,
                where_clause,
                associated_types,
                methods,
                apply_methods,
            }))
        )
    }

    /// Parse a `use` declaration.
    ///
    /// Supports:
    /// - `use string::parse::from_char_code`
    /// - `use string::parse::from_char_code as alias`
    /// - `use string::{from_char_code, char_code_at}`
    /// - `use string::{from_char_code as fcc, char_code_at}`
    fn parse_use_declaration(&mut self, visibility: Visibility) -> Stmt {
        self.consume_keyword_token(Keyword::Use);

        let mut path_segments = Vec::new();

        // Parse the path prefix: `a::b::c` or `a::b::{...}`
        path_segments.push(self.parse_identifier());

        while matches!(self.current_token_kind(), TokenKind::PathSeparator) {
            self.advance(); // consume `::`

            // Check for grouped imports: `use path::{a, b}`
            if matches!(self.current_token_kind(), TokenKind::LBrace) {
                self.advance(); // consume `{`
                let mut items = Vec::new();

                loop {
                    let mut item_span = self.create_span_from_current_token();
                    let name = self.parse_identifier();
                    let alias =
                        if matches!(self.current_token_kind(), TokenKind::Keyword(Keyword::As)) {
                            self.advance();
                            Some(self.parse_identifier())
                        } else {
                            None
                        };
                    self.end_span_from_previous_token(&mut item_span);
                    items.push(UseItem {
                        name,
                        alias,
                        span: item_span,
                    });

                    if matches!(self.current_token_kind(), TokenKind::Comma) {
                        self.advance();
                    } else {
                        break;
                    }
                }

                self.consume_token(TokenKind::RBrace);

                return node::stmt!(
                    self.unique_id(),
                    UseDeclaration(Box::new(UseDeclaration {
                        visibility,
                        path: path_segments,
                        items,
                        span: Span::default(),
                    }))
                );
            }

            path_segments.push(self.parse_identifier());
        }

        // Single import: `use a::b::c` or `use a::b::c as alias`
        // The last segment is the imported symbol name
        let last = path_segments
            .pop()
            .expect("use path must have at least one segment");
        let alias = if matches!(self.current_token_kind(), TokenKind::Keyword(Keyword::As)) {
            self.advance();
            Some(self.parse_identifier())
        } else {
            None
        };

        let item_span = if let Some(ref a) = alias {
            Span {
                start: last.span.start,
                end: a.span.end,
                start_lc: last.span.start_lc,
                end_lc: a.span.end_lc,
            }
        } else {
            last.span
        };

        node::stmt!(
            self.unique_id(),
            UseDeclaration(Box::new(UseDeclaration {
                visibility,
                path: path_segments,
                items: vec![UseItem {
                    name: last,
                    alias,
                    span: item_span,
                }],
                span: Span::default(),
            }))
        )
    }

    /// Parse a `mod` declaration (with optional `pub` visibility).
    ///
    /// `pub mod parse, utils`
    /// `mod internal`
    fn parse_mod_declaration(&mut self, visibility: Visibility) -> Stmt {
        self.consume_keyword_token(Keyword::Mod);

        let mut names = Vec::new();
        names.push(self.parse_identifier());

        while matches!(self.current_token_kind(), TokenKind::Comma) {
            self.advance();
            names.push(self.parse_identifier());
        }

        node::stmt!(
            self.unique_id(),
            ModDeclaration(Box::new(ModDeclaration {
                visibility,
                names,
                span: Span::default(),
            }))
        )
    }

    fn parse_statements(&mut self, may_complete: bool) -> (Vec<Stmt>, Option<Expr>) {
        let mut statements = Vec::new();
        let mut completion_expression = None;

        while self.not_at_closing(TokenKind::RBrace) {
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
                        _ => self.panic_unexpected_stmt(
                            "expression statement or function declaration",
                            statement,
                        ),
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
            let mut trailing = std::mem::take(&mut self.trailing_comments_buffer);
            trailing.extend(self.parse_comments());
            stmt.trailing_comments = trailing;
        }

        (statements, completion_expression)
    }

    fn parse_module(&mut self) -> Module {
        let mut span = self.create_span_from_current_token();
        let module_id = self.unique_id();
        let statements = self.parse_statements(false).0;
        self.end_span_from_previous_token(&mut span);

        Module::new(module_id, statements, span)
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
        // Use precedence 3 to prevent {} from being parsed as function call (precedence 2)
        let value = self.parse_expression_with_precedence(3, Associativity::Left);
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

    fn parse_const_declaration(&mut self, visibility: Visibility) -> Stmt {
        debug!("Parsing const declaration");
        let mut span = self.create_span_from_current_token();
        self.consume_keyword_token(Keyword::Const);

        let name = self.parse_identifier();

        let type_annotation = match self.current_token_kind() {
            TokenKind::Colon => {
                self.advance();
                self.parse_optional_type_annotation()
            }
            _ => None,
        };
        self.consume_token(TokenKind::EqualSign);
        let expression = self.parse_expression();

        self.end_span_from_previous_token(&mut span);
        node::stmt!(
            self.unique_id(),
            Const(Box::new(ConstDeclaration {
                id: self.unique_id(),
                visibility,
                name,
                expression,
                type_annotation,
                span,
            }))
        )
    }

    /// `foo { bar, baz }`.
    fn parse_call_expression(&mut self, expr: Expr) -> Expr {
        let mut span = expr.span;
        log::debug!(
            "Parsing call expression, current token: {:?}",
            self.current_token
        );
        let is_dict_call = matches!(self.current_token_kind(), TokenKind::LBrace);
        let mut arguments = Vec::new();

        log::debug!("Is dict call: {is_dict_call}");

        if is_dict_call {
            arguments.push(self.parse_block_or_dict());
        } else {
            self.consume_token(TokenKind::LParen);
            let mut is_first_argument = true;
            while self.not_at_closing(TokenKind::RParen) {
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
        while self.not_at_closing(TokenKind::RBracket) {
            expect_token_matches!(
                self,
                "identifier, literal, wildcard or rest parameter",
                TokenKind::Identifier
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
        while self.not_at_closing(TokenKind::RBracket) {
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
        while self.not_at_closing(TokenKind::RBrace) {
            let key = self.parse_expression();

            // If key is an single identifier and we follow up with a comma or rbrace, then we are
            // using the short hand syntax for dictionary elements.
            if let ExprKind::Path(path) = &key.kind
                && path.segments.len() == 1
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
            || (Self::is_contextual_identifier_token(self.current_token_kind())
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

    /// Can be a Identifier, `NestedIdentifier` or `FieldExpression` with a single field name.
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
            // Use precedence 3 to prevent {} from being parsed as function call (precedence 2)
            self.parse_expression_with_precedence(3, Associativity::Left)
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

            TokenKind::Tilde => {
                self.advance();

                node::expr!(
                    self.unique_id(),
                    UnaryOp(
                        UnaryOp::BitwiseNot,
                        Box::new(self.parse_primary_expression())
                    )
                )
            }

            _ => unreachable!("Unexpected token kind"),
        }
    }

    fn parse_signed_numeric_literal(&mut self) -> Expr {
        let invert = matches!(self.advance().kind, TokenKind::Minus);
        let literal = self.advance().literal().unwrap();

        if invert {
            node::expr!(self.unique_id(), Literal(Box::new(literal.invert_sign())))
        } else {
            node::expr!(self.unique_id(), Literal(Box::new(literal)))
        }
    }

    fn parse_parenthesized_expression(&mut self) -> Expr {
        self.advance();
        let expression = self.parse_expression();
        self.consume_token(TokenKind::RParen);
        expression
    }

    fn parse_recursive_call(&mut self) -> Expr {
        self.advance();
        let expr = self.parse_expression();
        let span = expr.span;
        let call_expr = match expr.kind {
            ExprKind::Call(call) => call,
            _ => {
                self.push_unexpected_token_error("call expression after `rec`", self.current_token);
                return node::expr!(
                    self.unique_id(),
                    Literal(Box::new(Literal::UnsignedInteger(0)))
                );
            }
        };

        node::expr!(self.unique_id(), RecursiveCall(call_expr)).with_span(span)
    }

    fn parse_wildcard(&mut self) -> Expr {
        self.advance();
        node::expr!(self.unique_id(), Wildcard)
    }

    fn parse_literal_expression(&mut self) -> Expr {
        let span = self.create_span_from_current_token();
        let literal = self.advance().literal().unwrap();

        node::expr!(self.unique_id(), Literal(Box::new(literal))).with_span(span)
    }

    fn parse_tagged_string_expression(&mut self) -> Expr {
        let span = self.create_span_from_current_token();
        let token = self.current_token;
        let TokenKind::TaggedStringStart(quote) = token.kind else {
            unreachable!()
        };
        // Extract tag name from the span: span covers "tag<quote>content<quote>",
        // but the tag is just the identifier before the opening quote.
        let full = self.lexer.span_text(token.span);
        let tag = &full[..full.len() - quote.len_utf8()];
        // Take the eagerly-buffered parts BEFORE advancing. `advance()` pre-loads
        // the next token via a lexer lookahead; if that token is another tagged
        // string, `pending_tagged_parts` would be overwritten before we get a
        // chance to read the parts that belong to this tagged string.
        let parts = self.lexer.take_tagged_string_parts().unwrap_or_default();
        self.advance();
        self.expand_tagged_string(tag, parts, span)
    }

    /// Expand a tagged string literal into a function call expression:
    /// `tag"text{expr}more"` → `tag(["text", "more"], [expr])`
    fn expand_tagged_string(
        &mut self,
        tag: &str,
        parts: Vec<TaggedStringPart>,
        span: Span,
    ) -> Expr {
        let mut string_parts: Vec<Box<str>> = Vec::new();
        let mut value_exprs: Vec<Expr> = Vec::new();

        for part in parts {
            match part {
                TaggedStringPart::Literal(text) => {
                    string_parts.push(text);
                }
                TaggedStringPart::Interpolation {
                    source: raw_source,
                    line,
                    column,
                    byte_offset,
                } => {
                    let expr =
                        self.parse_interpolation_expr(&raw_source, line, column, byte_offset);
                    value_exprs.push(expr);
                }
            }
        }

        let callee_path = Path::from_ident(Ident::new(tag, span));
        let tag_expr = node::expr!(self.unique_id(), Path(Box::new(callee_path))).with_span(span);

        let node_id = self.unique_id();

        // Tagged strings with no interpolation are pure — cache them as constants.
        if value_exprs.is_empty() {
            self.constant_pool_node_ids.push(node_id);
        }

        Expr::new(
            node_id,
            ExprKind::TaggedString {
                tag: Box::new(tag_expr),
                parts: string_parts,
                exprs: value_exprs,
            },
        )
        .with_span(span)
    }

    /// Parse a raw interpolation source string as a full expression using a sub-parser.
    /// The sub-parser shares this parser's `NodeIdAllocator` to avoid ID collisions.
    /// `line`, `column`, and `byte_offset` are the position of the first character of
    /// the interpolation body in the original source, so generated spans are correct.
    fn parse_interpolation_expr(
        &mut self,
        source: &str,
        line: u32,
        column: u32,
        byte_offset: u32,
    ) -> Expr {
        let lexer = Lexer::new(source).with_offset(line, column, byte_offset);
        let mut sub_parser = Parser::new(lexer).set_node_id_allocator(self.node_id_allocator);

        // Prime the sub-parser's token stream
        sub_parser.current_token = sub_parser.lexer.next_token();
        sub_parser.next_token = sub_parser.lexer.next_token();

        let expr = sub_parser.parse_expression();

        // Verify that all interpolation tokens were consumed (catches `{x y}` style errors)
        if !matches!(sub_parser.current_token_kind(), TokenKind::Eof) {
            sub_parser
                .push_unexpected_token_error("end of interpolation", sub_parser.current_token);
        }

        // Propagate node ID allocator state back to parent
        self.node_id_allocator = sub_parser.node_id_allocator;

        // Propagate any parse errors
        self.errors.extend(sub_parser.errors);

        expr
    }

    fn parse_self_expression(&mut self) -> Expr {
        let identifier_span = self.create_span_from_current_token();

        self.advance();

        let mut path = Path::from_ident(Ident::new(kw::_Self, identifier_span));
        let mut span = path.span;

        while matches!(self.current_token_kind(), TokenKind::PathSeparator) {
            self.advance();
            path.push(self.parse_identifier());
        }

        self.end_span_from_previous_token(&mut span);
        path.span = span;

        let mut expr = node::expr!(self.unique_id(), Path(Box::new(path))).with_span(span);

        if matches!(self.current_token_kind(), TokenKind::LParen) {
            expr = self.parse_call_expression(expr);
        }

        expr
    }

    fn parse_primary_expression(&mut self) -> Expr {
        debug!("Parsing primary expression {:?}", self.current_token);

        let comments = self.parse_comments();

        expect_token_matches!(
            self,
            "primary expression",
            TokenKind::Minus
                | TokenKind::ExclamationMark
                | TokenKind::Tilde
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
                | TokenKind::Identifier
                | TokenKind::Keyword(Keyword::Type | Keyword::Where)
                | TokenKind::Literal(_)
                | TokenKind::TaggedStringStart(_)
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
                self.parse_signed_numeric_literal()
            }

            TokenKind::Minus
            | TokenKind::ExclamationMark
            | TokenKind::Keyword(Keyword::Not)
            | TokenKind::Tilde => self.parse_unary_expression(),
            TokenKind::LParen => self.parse_parenthesized_expression(),
            TokenKind::LBrace => self.parse_block_or_dict(),
            TokenKind::LBracket => self.parse_list_expression(),
            TokenKind::Keyword(Keyword::If) => self.parse_if_else_expression(),
            TokenKind::Keyword(Keyword::Fn) => self.parse_function_expression(),
            TokenKind::Keyword(Keyword::Loop) => self.parse_loop(),
            TokenKind::Keyword(Keyword::For) => self.parse_for_loop(),
            TokenKind::Keyword(Keyword::Break) => self.parse_break_expr(),
            TokenKind::Keyword(Keyword::Rec) => self.parse_recursive_call(),
            TokenKind::Keyword(Keyword::Match) => self.parse_match_expression(),
            TokenKind::Keyword(Keyword::Underscore) => self.parse_wildcard(),
            TokenKind::Literal(_) => self.parse_literal_expression(),
            TokenKind::TaggedStringStart(_) => self.parse_tagged_string_expression(),
            TokenKind::Keyword(Keyword::_Self) => self.parse_self_expression(),
            token if Self::is_contextual_identifier_token(token) => self.parse_identifier_expr(),
            _ => {
                // `expect_token_matches!` above already pushed an error and advanced
                // past the unexpected token.  Return a placeholder so the caller
                // can continue recovering.
                node::expr!(
                    self.unique_id(),
                    Literal(Box::new(Literal::UnsignedInteger(0)))
                )
            }
        };

        self.end_span_from_previous_token(&mut span);
        node.span = span;
        node.leading_comments = comments;
        node.trailing_comments = self.parse_comments();
        node
    }

    fn parse_identifier_expr(&mut self) -> Expr {
        let identifier_span = self.create_span_from_current_token();

        let token = self.advance();
        let identifier = self.lexer.span_text(token.span);

        let mut path = Path::from_ident(Ident::new(identifier, identifier_span));
        let mut span = path.span;

        while matches!(self.current_token_kind(), TokenKind::PathSeparator) {
            self.advance();
            path.push(self.parse_identifier());
        }

        self.end_span_from_previous_token(&mut span);
        path.span = span;

        let mut expr = node::expr!(self.unique_id(), Path(Box::new(path))).with_span(span);

        if matches!(self.current_token_kind(), TokenKind::LParen) {
            expr = self.parse_call_expression(expr);
        }

        expr
    }

    fn parse_match_expression(&mut self) -> Expr {
        self.consume_keyword_token(Keyword::Match);
        // Use precedence 3 to prevent {} from being parsed as function call (precedence 2)
        let expression = self.parse_expression_with_precedence(3, Associativity::Left);

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
        while self.not_at_closing(TokenKind::RBrace) {
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

    fn parse_parameter_list(&mut self) -> (Vec<FunctionParameter>, Span) {
        let mut parameters = Vec::new();
        let mut params_span = Span::default();

        if matches!(self.current_token_kind(), TokenKind::LParen) {
            params_span = self.create_span_from_current_token();
            self.consume_token(TokenKind::LParen);

            let mut is_first_parameter = true;
            while self.not_at_closing(TokenKind::RParen) {
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
            self.end_span_from_previous_token(&mut params_span);
        }

        (parameters, params_span)
    }

    fn is_type_annotation_token(token: TokenKind) -> bool {
        matches!(
            token,
            TokenKind::Identifier
                | TokenKind::GreaterThan
                | TokenKind::LessThan
                | TokenKind::PathSeparator
                | TokenKind::Keyword(Keyword::Fn)
        )
    }

    fn parse_type_annotation(&mut self) -> Ty {
        // Function type annotation: `fn(name: Type, …) -> RetType`
        if matches!(self.current_token_kind(), TokenKind::Keyword(Keyword::Fn)) {
            return self.parse_function_type_annotation();
        }

        expect_token_matches!(self, "type annotation", TokenKind::Identifier);

        match self.current_token_kind() {
            token if Self::is_contextual_identifier_token(token) => {
                let mut span = self.create_span_from_current_token();
                let identifier = self.parse_path();
                let parameters = self.parse_type_annotation_parameters();
                self.end_span_from_previous_token(&mut span);

                Ty::new(self.unique_id(), identifier)
                    .with_parameters(parameters)
                    .with_span(span)
            }
            _ => Ty::new_unknown(self.unique_id()),
        }
    }

    /// Parse a function type annotation: `fn(name: Type, …) -> RetType`.
    ///
    /// The parameter names are optional — `fn(i64) -> bool` is also valid.
    fn parse_function_type_annotation(&mut self) -> Ty {
        let mut span = self.create_span_from_current_token();
        self.consume_keyword_token(Keyword::Fn);
        self.consume_token(TokenKind::LParen);

        let mut params = Vec::new();
        let mut first = true;
        while self.not_at_closing(TokenKind::RParen) {
            if first {
                first = false;
            } else {
                self.consume_token(TokenKind::Comma);
                if matches!(self.current_token_kind(), TokenKind::RParen) {
                    break;
                }
            }

            // Disambiguate `name: Type` vs bare `Type`:
            // peek ahead to see if we have `Ident Colon`.
            let (name, ty) = if Self::is_contextual_identifier_token(self.current_token_kind())
                && self.peek_token_kind() == TokenKind::Colon
            {
                let name = self.parse_identifier();
                self.consume_token(TokenKind::Colon);
                let ty = self.parse_type_annotation();
                (Some(name), ty)
            } else {
                let ty = self.parse_type_annotation();
                (None, ty)
            };

            params.push(FunctionTypeParam { name, ty });
        }
        self.consume_token(TokenKind::RParen);

        let return_type = if matches!(self.current_token_kind(), TokenKind::Arrow) {
            self.advance();
            self.parse_type_annotation()
        } else {
            Ty::new_unknown(self.unique_id())
        };

        self.end_span_from_previous_token(&mut span);

        Ty {
            id: self.unique_id(),
            kind: TyKind::Fn(params, Box::new(return_type)),
            parameters: vec![],
            span,
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
            while self.not_at_closing(TokenKind::GreaterThan) {
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

    /// Parse a type parameter list `<T, U, V>` for generic declarations.
    ///
    /// Returns an empty vec when no `<` follows the current position.
    fn parse_type_params(&mut self) -> Vec<TypeParam> {
        let mut type_params = Vec::new();

        if !matches!(self.current_token_kind(), TokenKind::LessThan) {
            return type_params;
        }

        self.advance(); // consume `<`

        while self.not_at_closing(TokenKind::GreaterThan) {
            let mut span = self.create_span_from_current_token();
            let name = self.parse_identifier();
            self.end_span_from_previous_token(&mut span);
            type_params.push(TypeParam {
                id: self.unique_id(),
                name,
                span,
            });
            if matches!(self.current_token_kind(), TokenKind::Comma) {
                self.advance();
            }
        }

        self.consume_token(TokenKind::GreaterThan);

        type_params
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
        let name = if Self::is_contextual_identifier_token(self.current_token_kind()) {
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

        let (parameters, params_span) = self.parse_parameter_list();
        let return_type = self.parse_return_type();
        let body = self.parse_block();

        self.end_span_from_previous_token(&mut span);

        node::expr!(
            self.unique_id(),
            FunctionExpression(Box::new(FunctionDeclaration {
                id: self.unique_id(),
                visibility: Visibility::Private,
                name,
                type_params: vec![],
                parameters,
                params_span,
                body,
                return_type_annotation: return_type,
                span,
                guard: None,
                leading_comments: vec![],
                trailing_comments: vec![],
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
            while self.not_at_closing(TokenKind::RBrace) {
                let ident = self.parse_identifier();

                if matches!(self.current_token_kind(), TokenKind::Colon) {
                    self.advance();
                    let pattern = self.parse_pattern();
                    elements.push((ident, pattern));
                } else {
                    elements.push((
                        ident,
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
            while self.not_at_closing(TokenKind::RParen) {
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

    fn parse_function_declaration(&mut self, visibility: Visibility) -> Stmt {
        let mut name: Option<Expr> = None;
        let mut declarations: Vec<FunctionDeclaration> = Vec::new();
        let mut clause_visibility = visibility;
        let mut type_params: Vec<TypeParam> = Vec::new();

        while matches!(
            self.current_token_kind(),
            TokenKind::Keyword(Keyword::Fn)
                | TokenKind::Keyword(Keyword::Pub)
                | TokenKind::SingleLineComment
                | TokenKind::MultiLineComment
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

            // For subsequent clauses, check for `pub` keyword.
            if !declarations.is_empty() {
                if matches!(self.current_token_kind(), TokenKind::Keyword(Keyword::Pub)) {
                    self.advance();
                    clause_visibility = Visibility::Public;
                } else {
                    clause_visibility = Visibility::Private;
                }
            }

            if matches!(self.current_token_kind(), TokenKind::Keyword(Keyword::Fn)) {
                self.advance();
            } else {
                // We found a comment, but not a function declaration, stop parsing function
                // declarations and rewind the lexer.
                self.restore_state(saved_state);
                break;
            }

            if Self::is_contextual_identifier_token(self.current_token_kind()) {
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
                        // Stop parsing function declarations and rewind the lexer.
                        self.restore_state(saved_state);
                        self.push_unexpected_token_error("identifier", self.current_token);
                        break;
                    }
                }
            } else if name.is_none() {
                // No identifier after `fn` keyword and no prior clause name to reuse.
                self.push_unexpected_token_error("function name", self.current_token);
                self.restore_state(saved_state);
                break;
            }

            // Parse type parameters. Store them from the first clause; consume
            // (and discard) type params on subsequent clauses so the parser
            // advances past them. All clauses of a multi-clause function share
            // the same type parameters — cross-clause consistency is validated
            // during semantic analysis, not at parse time.
            let clause_type_params = self.parse_type_params();
            if declarations.is_empty() {
                type_params = clause_type_params;
            }

            self.expect_token(TokenKind::LParen);
            let (parameters, params_span) = self.parse_parameter_list();
            let guard = self.parse_guard_clause();
            let return_type = self.parse_return_type();
            let body = self.parse_block();

            self.end_span_from_previous_token(&mut span);

            let Some(fn_name) = name.clone() else {
                break;
            };

            declarations.push(FunctionDeclaration {
                id: self.unique_id(),
                visibility: clause_visibility,
                name: fn_name,
                type_params: type_params.clone(),
                parameters,
                params_span,
                guard,
                body,
                return_type_annotation: return_type,
                leading_comments: comments,
                trailing_comments: vec![],
                span,
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
        // Use precedence 3 to prevent {} from being parsed as function calls (precedence 2)
        let expression = self.parse_expression_with_precedence(3, Associativity::Left);

        match expression.kind {
            ExprKind::Call { .. } | ExprKind::BinaryOp { .. } | ExprKind::UnaryOp { .. } => (),
            _ => {
                self.push_unexpected_token_error(
                    "function call, binary logical expression or unary logical expression",
                    self.current_token,
                );
            }
        }

        // Disambiguate between call with dict and block.
        if matches!(self.current_token_kind(), TokenKind::Semicolon) {
            self.advance();
        }

        Some(expression)
    }

    fn is_binary_op(token: TokenKind) -> bool {
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
                | TokenKind::Matches
                | TokenKind::NotMatches
                | TokenKind::LessThan
                | TokenKind::LessThanOrEqual
                | TokenKind::GreaterThan
                | TokenKind::GreaterThanOrEqual
                | TokenKind::Pipe
                | TokenKind::Ampersand
                | TokenKind::DoublePipe
                | TokenKind::DoubleAmpersand
                | TokenKind::LeftShift
                | TokenKind::RightShift
                | TokenKind::Pipeline
                | TokenKind::Keyword(Keyword::And | Keyword::Or)
        )
    }

    fn map_binary_op(token: TokenKind) -> BinaryOpKind {
        match token {
            TokenKind::Plus => BinaryOpKind::Add,
            TokenKind::Minus => BinaryOpKind::Sub,
            TokenKind::Asterisk => BinaryOpKind::Mul,
            TokenKind::AsteriskAsterisk => BinaryOpKind::Exp,
            TokenKind::Slash => BinaryOpKind::Div,
            TokenKind::Percent => BinaryOpKind::Mod,
            TokenKind::EqualSign => BinaryOpKind::Assign,
            TokenKind::EqualEqual => BinaryOpKind::Eq,
            TokenKind::NotEqual => BinaryOpKind::NotEq,
            TokenKind::Matches => BinaryOpKind::Match,
            TokenKind::NotMatches => BinaryOpKind::NotMatch,
            TokenKind::LessThan => BinaryOpKind::Less,
            TokenKind::LessThanOrEqual => BinaryOpKind::LessEq,
            TokenKind::GreaterThan => BinaryOpKind::Greater,
            TokenKind::GreaterThanOrEqual => BinaryOpKind::GreaterEq,
            TokenKind::Pipe => BinaryOpKind::BitwiseOr,
            TokenKind::Ampersand => BinaryOpKind::BitwiseAnd,
            TokenKind::Caret => BinaryOpKind::BitwiseXor,
            TokenKind::LeftShift => BinaryOpKind::LeftShift,
            TokenKind::RightShift => BinaryOpKind::RightShift,
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
            BinaryOpKind::Add | BinaryOpKind::Sub => OperatorInfo {
                precedence: 7,
                associativity: Associativity::Left,
            },
            BinaryOpKind::Mul | BinaryOpKind::Div | BinaryOpKind::Mod => OperatorInfo {
                precedence: 8,
                associativity: Associativity::Left,
            },
            BinaryOpKind::Eq
            | BinaryOpKind::NotEq
            | BinaryOpKind::Match
            | BinaryOpKind::NotMatch
            | BinaryOpKind::Less
            | BinaryOpKind::LessEq
            | BinaryOpKind::Greater
            | BinaryOpKind::GreaterEq => OperatorInfo {
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
            BinaryOpKind::LeftShift | BinaryOpKind::RightShift => OperatorInfo {
                precedence: 5,
                associativity: Associativity::Left,
            },
            BinaryOpKind::Exp => OperatorInfo {
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
            TokenKind::Literal(_) => self.advance().literal().unwrap(),
            _ => Literal::None,
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
                | TokenKind::Identifier
                | TokenKind::Keyword(Keyword::Type | Keyword::Where)
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
            token if Self::is_contextual_identifier_token(token) => match self.peek_token_kind() {
                TokenKind::PathSeparator => self.parse_enum_extraction(),
                TokenKind::LParen | TokenKind::LBrace => self.parse_enum_extraction(),
                _ => self.parse_identifier_pattern(),
            },
            TokenKind::LBracket => self.parse_list_extraction(),
            _ => node::pat!(self.unique_id(), None),
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
        debug!("Parsing expression with precedence {precedence} {associativity:#?}");
        let mut lhs = self.parse_primary_expression();
        let mut span = lhs.span;

        loop {
            match self.current_token_kind() {
                TokenKind::Dot => {
                    self.advance();
                    lhs.trailing_comments = self.parse_comments();
                    let field_name = self.parse_identifier();
                    let mut field_span = span;
                    self.end_span_from_previous_token(&mut field_span);
                    lhs = node::expr!(
                        self.unique_id(),
                        FieldExpression(Box::new(FieldAccessExpression {
                            base: lhs,
                            field: field_name,
                        }))
                    )
                    .with_span(field_span);
                }
                TokenKind::LBracket => {
                    self.advance();
                    let index = self.parse_expression_with_precedence(0, Associativity::Left);
                    self.consume_token(TokenKind::RBracket);
                    let mut index_span = span;
                    self.end_span_from_previous_token(&mut index_span);
                    lhs = node::expr!(
                        self.unique_id(),
                        IndexExpression(Box::new(IndexAccessExpression { base: lhs, index }))
                    )
                    .with_span(index_span);
                }
                TokenKind::LParen => {
                    lhs = self.parse_call_expression(lhs);
                }
                TokenKind::LBrace => {
                    // Function call with dictionary syntax: foo {}
                    // Give this low precedence (2) so it doesn't interfere with operators
                    let call_precedence = 2;
                    if precedence > call_precedence {
                        break;
                    }
                    lhs = self.parse_call_expression(lhs);
                }
                TokenKind::Keyword(Keyword::Implements | Keyword::Matches | Keyword::As) => {
                    // All three keywords have comparison precedence (6), left-associative
                    let op_info = OperatorInfo {
                        precedence: 6,
                        associativity: Associativity::Left,
                    };
                    if Self::compare_precedence(
                        &OperatorInfo {
                            precedence,
                            associativity,
                        },
                        &op_info,
                    ) {
                        break;
                    }
                    lhs = self.parse_keyword_suffix(lhs);
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

                    let mut binop_span = span;
                    self.end_span_from_previous_token(&mut binop_span);

                    lhs = node::expr!(
                        self.unique_id(),
                        BinaryOp(Box::new(BinaryOpExpression {
                            op: operator,
                            lhs,
                            rhs,
                        }))
                    )
                    .with_span(binop_span);
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

    fn parse_keyword_suffix(&mut self, lhs: Expr) -> Expr {
        let keyword = self.current_token_kind();
        self.advance();
        match keyword {
            TokenKind::Keyword(Keyword::Implements) => {
                let path = self.parse_path();
                node::expr!(self.unique_id(), Implements(Box::new(lhs), Box::new(path)))
            }
            TokenKind::Keyword(Keyword::Matches) => {
                let pat = self.parse_pattern();
                node::expr!(self.unique_id(), Matches(Box::new(lhs), pat))
            }
            TokenKind::Keyword(Keyword::As) => {
                // Check for `as?` (try-cast) vs `as` (cast)
                let is_try_cast = matches!(self.current_token_kind(), TokenKind::QuestionMark);
                if is_try_cast {
                    self.advance();
                }
                let ty = self.parse_type_annotation();
                if is_try_cast {
                    node::expr!(self.unique_id(), TryCast(Box::new(lhs), Box::new(ty)))
                } else {
                    node::expr!(self.unique_id(), Cast(Box::new(lhs), Box::new(ty)))
                }
            }
            _ => unreachable!(),
        }
    }

    fn fn_name_identifier_to_string(&mut self, identifier: &Expr) -> String {
        match &identifier.kind {
            ExprKind::Path(path) => path.to_string(),

            ExprKind::FieldExpression(field) => {
                let base = self.fn_name_identifier_to_string(&field.base.clone());
                base + "." + field.field.as_str()
            }
            _ => {
                self.push_unexpected_expr_error(
                    "identifier, nested identifier or field expression as function name",
                    identifier,
                );
                String::from("<error>")
            }
        }
    }

    fn parse_loop(&mut self) -> Expr {
        self.advance();
        let block = self.parse_block();
        node::expr!(self.unique_id(), Loop(Box::new(block)))
    }

    fn parse_for_loop(&mut self) -> Expr {
        self.advance();
        let pat = self.parse_pattern();
        self.consume_token(TokenKind::Keyword(Keyword::In));
        // Use precedence 3 to prevent {} from being parsed as function call (precedence 2)
        let iter = self.parse_expression_with_precedence(3, Associativity::Left);
        if matches!(self.current_token_kind(), TokenKind::Semicolon) {
            self.advance();
        }
        let acc = if matches!(self.current_token_kind(), TokenKind::Keyword(Keyword::With)) {
            self.advance();
            let pat = self.parse_pattern();
            self.consume_token(TokenKind::EqualSign);
            // Use precedence 3 to prevent {} from being parsed as function call (precedence 2)
            let expr = self.parse_expression_with_precedence(3, Associativity::Left);
            if matches!(self.current_token_kind(), TokenKind::Semicolon) {
                self.advance();
            }
            Some((pat, expr))
        } else {
            None
        };
        let block = self.parse_block();

        let else_block = if matches!(self.current_token_kind(), TokenKind::Keyword(Keyword::Else)) {
            self.advance();
            Some(self.parse_block())
        } else {
            None
        };

        let loop_id = self.unique_id();

        node::expr!(
            self.unique_id(),
            ForLoop(Box::new(node::ForLoop {
                id: loop_id,
                pat,
                iter,
                acc,
                block,
                else_block,
            }))
        )
    }

    fn parse_break_expr(&mut self) -> Expr {
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
}
