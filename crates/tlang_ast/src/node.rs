use serde::Serialize;
use std::rc::Rc;
use std::{cell::RefCell, fmt::Display};

use crate::token::Token;
use crate::{
    span::{Span, Spanned},
    symbols::{SymbolId, SymbolTable},
    token::Literal,
};

#[derive(Clone, Debug, PartialEq, Serialize)]
pub struct Ident {
    pub name: String,
    pub span: Span,
}

impl Ident {
    pub fn new(name: &str, span: Span) -> Self {
        Ident {
            name: name.to_string(),
            span,
        }
    }

    pub fn as_str(&self) -> &str {
        &self.name
    }
}

impl Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize)]
pub enum Associativity {
    Left,
    Right,
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct FunctionParameter {
    pub pattern: Pattern,
    pub type_annotation: Option<Ty>,
    pub span: Span,
}

impl FunctionParameter {
    pub fn new(pattern: Pattern) -> Self {
        FunctionParameter {
            pattern,
            type_annotation: None,
            span: Span::default(),
        }
    }

    pub fn with_span(mut self, span: Span) -> Self {
        self.span = span;
        self
    }

    pub fn with_type_annotation(mut self, type_annotation: Ty) -> Self {
        self.type_annotation = Some(type_annotation);
        self
    }

    pub fn name(self) -> Option<String> {
        match self.pattern.kind {
            PatternKind::Identifier { ref name, .. } => Some(name.to_string()),
            PatternKind::Wildcard => Some(String::from("_")),
            _ => None,
        }
    }
}

#[derive(Debug, Default, PartialEq, Clone, Serialize)]
pub struct FunctionDeclaration {
    pub id: SymbolId,
    pub name: Expr,
    pub parameters: Vec<FunctionParameter>,
    pub guard: Option<Expr>,
    pub return_type_annotation: Option<Ty>,
    pub body: Block,
    pub leading_comments: Vec<Token>,
    pub trailing_comments: Vec<Token>,
    pub symbol_table: Option<Rc<RefCell<SymbolTable>>>,
    pub span: Span,
}

#[derive(Debug, Default, PartialEq, Clone, Serialize)]
pub struct Block {
    pub statements: Vec<Stmt>,
    pub expression: Option<Expr>,
    pub span: Span,
    pub symbol_table: Option<Rc<RefCell<SymbolTable>>>,
}

impl Block {
    pub fn new(statements: Vec<Stmt>, expression: Option<Expr>) -> Self {
        Block {
            statements,
            expression,
            ..Default::default()
        }
    }

    pub fn with_span(mut self, span: Span) -> Self {
        self.span = span;
        self
    }

    pub fn has_completion(&self) -> bool {
        self.expression.is_some()
    }
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct CallExpression {
    pub callee: Expr,
    pub arguments: Vec<Expr>,
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct FieldAccessExpression {
    pub base: Expr,
    pub field: Ident,
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct IndexAccessExpression {
    pub base: Expr,
    pub index: Expr,
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct IfElseExpression {
    pub condition: Expr,
    pub then_branch: Expr,
    pub else_branches: Vec<ElseClause>,
}

#[derive(Debug, Default, PartialEq, Clone, Serialize)]
pub struct Expr {
    pub kind: ExprKind,
    pub leading_comments: Vec<Token>,
    pub trailing_comments: Vec<Token>,
    pub symbol_table: Option<Rc<RefCell<SymbolTable>>>,
    pub span: Span,
}

impl Expr {
    pub fn new(kind: ExprKind) -> Self {
        Expr {
            kind,
            ..Default::default()
        }
    }

    pub fn with_span(mut self, span: Span) -> Self {
        self.span = span;
        self
    }

    pub fn is_wildcard(&self) -> bool {
        matches!(self.kind, ExprKind::Wildcard)
    }
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct ElseClause {
    pub condition: Option<Expr>,
    pub consequence: Expr,
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct BinaryOpExpression {
    pub op: BinaryOpKind,
    pub lhs: Expr,
    pub rhs: Expr,
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct MatchExpression {
    pub expression: Expr,
    pub arms: Vec<MatchArm>,
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct RangeExpression {
    pub start: Expr,
    pub end: Expr,
    pub inclusive: bool,
}

#[derive(Debug, Default, PartialEq, Clone, Serialize)]
pub enum ExprKind {
    #[default]
    None,
    Block(Box<Block>),
    Call(Box<CallExpression>),
    Dict(Vec<(Expr, Expr)>),
    FunctionExpression(Box<FunctionDeclaration>),
    FieldExpression(Box<FieldAccessExpression>),
    IndexExpression(Box<IndexAccessExpression>),
    // Let expression, only valid within if conditions and guards
    Let(Box<Pattern>, Box<Expr>),
    IfElse(Box<IfElseExpression>),
    List(Vec<Expr>),
    Literal(Box<Literal>),
    Path(Box<Path>),
    UnaryOp(UnaryOp, Box<Expr>),
    BinaryOp(Box<BinaryOpExpression>),
    Match(Box<MatchExpression>),
    // TODO: This should probably be RecursiveCall(Box<CallExpression>)
    RecursiveCall(Box<Expr>),
    Range(Box<RangeExpression>),
    Wildcard,
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct Path {
    pub segments: Vec<Ident>,
    pub span: Span,
}

impl Path {
    pub fn new(segments: Vec<Ident>) -> Self {
        Path {
            segments,
            span: Span::default(),
        }
    }

    pub fn from_ident(ident: Ident) -> Self {
        let span = ident.span;

        Path {
            segments: vec![ident],
            span,
        }
    }

    pub fn with_span(mut self, span: Span) -> Self {
        self.span = span;
        self
    }

    pub fn push(&mut self, ident: Ident) {
        self.segments.push(ident);
    }

    pub fn join(&self, separator: &str) -> String {
        self.segments
            .iter()
            .map(|segment| segment.as_str())
            .collect::<Vec<_>>()
            .join(separator)
    }
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct Pattern {
    pub kind: PatternKind,
    pub span: Span,
}

impl Pattern {
    pub fn new(kind: PatternKind) -> Self {
        Pattern {
            kind,
            span: Span::default(),
        }
    }

    pub fn with_span(mut self, span: Span) -> Self {
        self.span = span;
        self
    }

    pub fn get_symbol_id(&self) -> Option<SymbolId> {
        match &self.kind {
            PatternKind::Identifier { id, .. } => Some(*id),
            _ => None,
        }
    }

    pub fn get_all_symbol_ids(&self) -> Vec<SymbolId> {
        match &self.kind {
            PatternKind::Identifier { id, .. } => vec![*id],
            PatternKind::List(patterns) => patterns
                .iter()
                .flat_map(Pattern::get_all_symbol_ids)
                .collect(),
            PatternKind::Rest(pattern) => pattern.get_all_symbol_ids(),
            PatternKind::Enum { elements, .. } => elements
                .iter()
                .flat_map(Pattern::get_all_symbol_ids)
                .collect(),
            PatternKind::Wildcard => vec![],
            pattern_kind => todo!(
                "Getting symbol ids for pattern kind {:?} not implemented yet",
                pattern_kind
            ),
        }
    }

    pub fn is_wildcard(&self) -> bool {
        matches!(self.kind, PatternKind::Wildcard)
    }
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub enum PatternKind {
    Identifier {
        id: SymbolId,
        name: Ident,
    },
    Literal(Box<Expr>),
    List(Vec<Pattern>),
    Rest(Box<Pattern>),
    Enum {
        identifier: Box<Expr>,
        elements: Vec<Pattern>,
        named_fields: bool,
    },
    Wildcard,
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct EnumVariant {
    pub name: Ident,
    pub named_fields: bool,
    pub parameters: Vec<Ident>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct EnumDeclaration {
    pub id: SymbolId,
    pub name: Ident,
    pub variants: Vec<EnumVariant>,
}

#[derive(Debug, Default, PartialEq, Clone, Serialize)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
    pub leading_comments: Vec<Token>,
    pub trailing_comments: Vec<Token>,
    pub symbol_table: Option<Rc<RefCell<SymbolTable>>>,
}

impl Stmt {
    pub fn new(kind: StmtKind) -> Self {
        Stmt {
            kind,
            ..Default::default()
        }
    }

    pub fn with_span(mut self, span: Span) -> Self {
        self.span = span;
        self
    }
}

#[derive(Debug, Default, PartialEq, Clone, Serialize)]
pub enum StmtKind {
    #[default]
    None,
    Expr(Box<Expr>),
    Let(Box<LetDeclaration>),
    FunctionDeclaration(Box<FunctionDeclaration>),
    // Should this really be handled within the parser or should this be done in later stages?
    FunctionDeclarations(Vec<FunctionDeclaration>),
    Return(Box<Option<Expr>>),
    EnumDeclaration(Box<EnumDeclaration>),
    StructDeclaration(Box<StructDeclaration>),
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct LetDeclaration {
    pub pattern: Pattern,
    pub expression: Expr,
    pub type_annotation: Option<Ty>,
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct StructDeclaration {
    pub id: SymbolId,
    pub name: Ident,
    pub fields: Vec<StructField>,
}

pub type StructField = (Ident, Ty);

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct Ty {
    pub name: Path,
    pub parameters: Vec<Ty>,
    pub span: Span,
}

impl Ty {
    pub fn new(name: Path) -> Self {
        Ty {
            name,
            parameters: vec![],
            span: Span::default(),
        }
    }

    pub fn with_parameters(mut self, parameters: Vec<Ty>) -> Self {
        self.parameters = parameters;
        self
    }

    pub fn with_span(mut self, span: Span) -> Self {
        self.span = span;
        self
    }
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub expression: Expr,
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct Module {
    pub statements: Vec<Stmt>,
    pub symbol_table: Option<Rc<RefCell<SymbolTable>>>,
    pub span: Span,
}

impl Module {
    pub fn new(statements: Vec<Stmt>) -> Self {
        Module {
            statements,
            symbol_table: None,
            span: Span::default(),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy, Serialize)]
pub enum UnaryOp {
    Minus,
    Not,
    Rest,
    Spread,
}

#[derive(Debug, Clone, Copy, Serialize)]
pub struct OperatorInfo {
    pub precedence: u8,
    pub associativity: Associativity,
}

#[derive(Debug, PartialEq, Clone, Copy, Serialize)]
pub enum BinaryOpKind {
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
    Pipeline,
}

pub type BinaryOp = Spanned<BinaryOpKind>;

#[macro_export]
macro_rules! expr {
    ($kind:ident) => {{
        use tlang_ast::node::{Expr, ExprKind};

        Expr::new(ExprKind::$kind)
    }};

    ($kind:ident($($arg:expr),* $(,)?)) => {{
        use tlang_ast::node::{Expr, ExprKind};

        Expr::new(ExprKind::$kind($($arg),*))
    }};

    ($kind:ident { $( $field:ident : $value:expr ),* $(,)? }) => {{
        use tlang_ast::node::{Expr, ExprKind};
        Expr::new(ExprKind::$kind { $( $field : $value ),* })
    }};
}

#[macro_export]
macro_rules! pat {
    ($kind:ident) => {{
        use tlang_ast::node::{Pattern, PatternKind};

        Pattern::new(PatternKind::$kind)
    }};

    ($kind:ident($($arg:expr),* $(,)?)) => {{
        use tlang_ast::node::{Pattern, PatternKind};

        Pattern::new(PatternKind::$kind($($arg),*))
    }};

    ($kind:ident { $( $field:ident : $value:expr ),* $(,)? }) => {{
        use tlang_ast::node::{Pattern, PatternKind};

        Pattern::new(PatternKind::$kind { $( $field : $value ),* })
    }};
}

#[macro_export]
macro_rules! stmt {
    ($kind:ident) => {{
        use tlang_ast::node::{Stmt, StmtKind};
        Stmt::new(StmtKind::$kind)
    }};

    ($kind:ident($($arg:expr),* $(,)?)) => {{
        use tlang_ast::node::{Stmt, StmtKind};
        Stmt::new(StmtKind::$kind($($arg),*))
    }};

    ($kind:ident { $( $field:ident : $value:expr ),* $(,)? }) => {{
        use tlang_ast::node::{Stmt, StmtKind};
        Stmt::new(StmtKind::$kind { $( $field : $value ),* })
    }};
}

pub use expr;
pub use pat;
pub use stmt;
