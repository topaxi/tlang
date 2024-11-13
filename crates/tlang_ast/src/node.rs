use serde::Serialize;
use std::fmt::Display;

use crate::node_id::NodeId;
use crate::token::Token;
use crate::{
    span::{Span, Spanned},
    token::Literal,
};

pub use crate::macros::*;

#[derive(Clone, Debug, Serialize)]
pub struct Ident {
    pub name: String,
    pub span: Span,
}

impl PartialEq for Ident {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
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
}

#[derive(Debug, Default, PartialEq, Clone, Serialize)]
pub struct FunctionDeclaration {
    pub id: NodeId,
    pub name: Expr,
    pub parameters: Vec<FunctionParameter>,
    pub guard: Option<Expr>,
    pub return_type_annotation: Option<Ty>,
    pub body: Block,
    pub leading_comments: Vec<Token>,
    pub trailing_comments: Vec<Token>,
    pub span: Span,
}

#[derive(Debug, Default, PartialEq, Clone, Serialize)]
pub struct Block {
    pub id: NodeId,
    pub statements: Vec<Stmt>,
    pub expression: Option<Expr>,
    pub span: Span,
}

impl Block {
    pub fn new(id: NodeId, statements: Vec<Stmt>, expression: Option<Expr>) -> Self {
        Block {
            id,
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

impl CallExpression {
    pub fn has_wildcard(&self) -> bool {
        self.arguments.iter().any(Expr::is_wildcard)
    }

    pub fn wildcard_count(&self) -> usize {
        self.arguments
            .iter()
            .filter(|expr| expr.is_wildcard())
            .count()
    }
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
    pub id: NodeId,
    pub kind: ExprKind,
    pub leading_comments: Vec<Token>,
    pub trailing_comments: Vec<Token>,
    pub span: Span,
}

impl Expr {
    pub fn new(id: NodeId, kind: ExprKind) -> Self {
        Expr {
            id,
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
    RecursiveCall(Box<CallExpression>),
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

#[derive(Debug, Default, PartialEq, Clone, Serialize)]
pub struct Pattern {
    pub id: NodeId,
    pub kind: PatternKind,
    pub leading_comments: Vec<Token>,
    pub trailing_comments: Vec<Token>,
    pub span: Span,
}

impl Pattern {
    pub fn new(id: NodeId, kind: PatternKind) -> Self {
        Pattern {
            id,
            kind,
            ..Default::default()
        }
    }

    pub fn with_span(mut self, span: Span) -> Self {
        self.span = span;
        self
    }

    pub fn get_all_node_ids(&self) -> Vec<NodeId> {
        let mut ids = vec![self.id];

        match &self.kind {
            PatternKind::List(patterns) => {
                ids.extend(patterns.iter().flat_map(Pattern::get_all_node_ids));
            }
            PatternKind::Rest(pattern) => {
                ids.extend(pattern.get_all_node_ids());
            }
            PatternKind::Enum(enum_pattern) => {
                ids.extend(
                    enum_pattern
                        .elements
                        .iter()
                        .flat_map(Pattern::get_all_node_ids),
                );
            }
            PatternKind::None
            | PatternKind::Literal(_)
            | PatternKind::Identifier(_)
            | PatternKind::_Self
            | PatternKind::Wildcard => {}
        }

        ids
    }

    pub fn is_wildcard(&self) -> bool {
        matches!(self.kind, PatternKind::Wildcard)
    }
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct EnumPattern {
    pub path: Path,
    pub elements: Vec<Pattern>,
    pub named_fields: bool,
}

#[derive(Debug, Default, PartialEq, Clone, Serialize)]
pub enum PatternKind {
    #[default]
    None,
    Identifier(Box<Ident>),
    Literal(Box<Literal>),
    List(Vec<Pattern>),
    Rest(Box<Pattern>),
    Enum(Box<EnumPattern>),
    Wildcard,
    // TODO: As mentioned on the Keyword enum, we might want this to just be an Identifier(Pattern)
    _Self,
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct EnumVariant {
    pub id: NodeId,
    pub name: Ident,
    pub named_fields: bool,
    pub parameters: Vec<Ident>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct EnumDeclaration {
    pub name: Ident,
    pub variants: Vec<EnumVariant>,
}

#[derive(Debug, Default, PartialEq, Clone, Serialize)]
pub struct Stmt {
    pub id: NodeId,
    pub kind: StmtKind,
    pub span: Span,
    pub leading_comments: Vec<Token>,
    pub trailing_comments: Vec<Token>,
}

impl Stmt {
    pub fn new(id: NodeId, kind: StmtKind) -> Self {
        Stmt {
            id,
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
    pub name: Ident,
    pub fields: Vec<StructField>,
}

// TODO: Add NodeId to StructField
pub type StructField = (Ident, Ty);

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct Ty {
    pub id: NodeId,
    pub name: Path,
    pub parameters: Vec<Ty>,
    pub span: Span,
}

impl Ty {
    pub fn new(id: NodeId, name: Path) -> Self {
        Ty {
            id,
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
    pub id: NodeId,
    pub pattern: Pattern,
    pub guard: Option<Expr>,
    pub expression: Expr,
}

#[derive(Debug, Default, PartialEq, Clone, Serialize)]
pub struct Module {
    pub id: NodeId,
    pub statements: Vec<Stmt>,
    pub span: Span,
}

impl Module {
    pub fn new(id: NodeId, statements: Vec<Stmt>) -> Self {
        Module {
            id,
            statements,
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
    Assign,
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
