#[cfg(feature = "serde")]
use serde::Serialize;
use std::fmt::Display;
use tlang_span::Spanned;

use tlang_span::{NodeId, Span};

use crate::keyword::kw;
use crate::token::CommentToken;
use crate::token::Literal;
use tlang_intern::{Symbol, get as intern_get, intern};

pub use crate::macros::*;

#[derive(Copy, Clone, Debug, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Ident {
    name: Symbol,
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
            name: intern(name),
            span,
        }
    }

    pub fn set_name(&mut self, name: &str) {
        self.name = intern(name);
    }

    pub fn set_arity(&mut self, arity: usize) {
        self.name = intern(&format!("{}/{}", self.as_str(), arity));
    }

    pub fn is_self(&self) -> bool {
        self.as_str() == kw::_Self
    }

    pub fn is_wildcard(&self) -> bool {
        self.as_str() == kw::Underscore
    }

    pub fn as_str(&self) -> &str {
        intern_get(self.name)
    }
}

impl Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum Associativity {
    Left,
    Right,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct FunctionParameter {
    pub pattern: Pat,
    pub type_annotation: Option<Ty>,
    pub span: Span,
}

impl FunctionParameter {
    pub fn new(pattern: Pat) -> Self {
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

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum Visibility {
    #[default]
    Private,
    Public,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct ConstDeclaration {
    pub id: NodeId,
    pub visibility: Visibility,
    pub name: Ident,
    pub expression: Expr,
    pub type_annotation: Option<Ty>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct FunctionDeclaration {
    pub id: NodeId,
    pub visibility: Visibility,
    pub name: Expr,
    pub parameters: Vec<FunctionParameter>,
    pub guard: Option<Expr>,
    pub return_type_annotation: Option<Ty>,
    pub body: Block,
    pub leading_comments: Vec<CommentToken>,
    pub trailing_comments: Vec<CommentToken>,
    pub span: Span,
}

impl FunctionDeclaration {
    pub fn name(&self) -> Option<String> {
        match &self.name.kind {
            ExprKind::Path(path) => Some(path.to_string()),
            ExprKind::FieldExpression(expr) if let Some(path) = expr.base.path() => {
                Some(format!("{}.{}", path, expr.field))
            }
            _ => None,
        }
    }

    /// Like [`name()`](Self::name), but returns `"<invalid>"` instead of
    /// `None` when the name expression is not a recognised form.  Useful in
    /// diagnostic messages and symbol-table entries where an absent name would
    /// produce confusing output.
    pub fn name_or_invalid(&self) -> String {
        self.name().unwrap_or_else(|| "<invalid>".to_string())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
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
            span: Span::default(),
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

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
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

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct FieldAccessExpression {
    pub base: Expr,
    pub field: Ident,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct IndexAccessExpression {
    pub base: Expr,
    pub index: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct IfElseExpression {
    pub condition: Expr,
    pub then_branch: Block,
    pub else_branches: Vec<ElseClause>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Expr {
    pub id: NodeId,
    pub kind: ExprKind,
    pub leading_comments: Vec<CommentToken>,
    pub trailing_comments: Vec<CommentToken>,
    pub span: Span,
}

impl Expr {
    pub fn new(id: NodeId, kind: ExprKind) -> Self {
        Expr {
            id,
            kind,
            leading_comments: vec![],
            trailing_comments: vec![],
            span: Span::default(),
        }
    }

    pub fn with_span(mut self, span: Span) -> Self {
        self.span = span;
        self
    }

    pub fn is_wildcard(&self) -> bool {
        matches!(self.kind, ExprKind::Wildcard)
    }

    pub fn path(&self) -> Option<&Path> {
        if let ExprKind::Path(path) = &self.kind {
            Some(path)
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct ElseClause {
    pub condition: Option<Expr>,
    pub consequence: Block,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct BinaryOpExpression {
    pub op: BinaryOpKind,
    pub lhs: Expr,
    pub rhs: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct MatchExpression {
    pub expression: Expr,
    pub arms: Vec<MatchArm>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct RangeExpression {
    pub start: Expr,
    pub end: Expr,
    pub inclusive: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct ForLoop {
    pub id: NodeId,
    pub pat: Pat,
    pub iter: Expr,
    pub acc: Option<(Pat, Expr)>,
    pub block: Block,
    pub else_block: Option<Block>,
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum ExprKind {
    #[default]
    None,
    Block(Box<Block>),
    Call(Box<CallExpression>),
    Cast(Box<Expr>, Box<Ty>),
    Dict(Vec<(Expr, Expr)>),
    Loop(Box<Block>),
    ForLoop(Box<ForLoop>),
    Break(Option<Box<Expr>>),
    Continue,
    FunctionExpression(Box<FunctionDeclaration>),
    FieldExpression(Box<FieldAccessExpression>),
    IndexExpression(Box<IndexAccessExpression>),
    // Let expression, only valid within if conditions and guards
    Let(Box<Pat>, Box<Expr>),
    IfElse(Box<IfElseExpression>),
    List(Vec<Expr>),
    Literal(Box<Literal>),
    Path(Box<Path>),
    UnaryOp(UnaryOp, Box<Expr>),
    BinaryOp(Box<BinaryOpExpression>),
    Match(Box<MatchExpression>),
    RecursiveCall(Box<CallExpression>),
    Range(Box<RangeExpression>),
    TaggedString {
        tag: Box<Expr>,
        parts: Vec<Box<str>>,
        exprs: Vec<Expr>,
    },
    Wildcard,
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum Res {
    /// Not yet resolved.
    #[default]
    Unresolved,
    /// Resolved to a user-declared definition node.
    Def(NodeId),
    /// Resolved to a builtin / primitive type (no declaration node).
    PrimTy,
}

/// AST representation of a path.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Path {
    pub segments: Vec<Ident>,
    pub res: Res,
    pub span: Span,
}

impl Display for Path {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.join("::"))
    }
}

impl Path {
    pub fn new(segments: Vec<Ident>) -> Self {
        Path {
            segments,
            res: Res::default(),
            span: Span::default(),
        }
    }

    pub fn from_ident(ident: Ident) -> Self {
        let span = ident.span;

        Path {
            segments: vec![ident],
            res: Res::default(),
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
            .map(Ident::as_str)
            .collect::<Vec<_>>()
            .join(separator)
    }

    pub fn join_with(&self, segment_str: &str) -> String {
        self.to_string() + "::" + segment_str
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Pat {
    pub id: NodeId,
    pub kind: PatKind,
    pub leading_comments: Vec<CommentToken>,
    pub trailing_comments: Vec<CommentToken>,
    pub span: Span,
}

impl Pat {
    pub fn new(id: NodeId, kind: PatKind) -> Self {
        Pat {
            id,
            kind,
            leading_comments: vec![],
            trailing_comments: vec![],
            span: Span::default(),
        }
    }

    pub fn with_span(mut self, span: Span) -> Self {
        self.span = span;
        self
    }

    pub fn get_all_node_ids(&self) -> Vec<NodeId> {
        let mut ids = vec![self.id];

        match &self.kind {
            PatKind::List(patterns) => {
                ids.extend(patterns.iter().flat_map(Pat::get_all_node_ids));
            }
            PatKind::Rest(pattern) => {
                ids.extend(pattern.get_all_node_ids());
            }
            PatKind::Enum(enum_pattern) => {
                ids.extend(
                    enum_pattern
                        .elements
                        .iter()
                        .map(|(_ident, pattern)| pattern)
                        .flat_map(Pat::get_all_node_ids),
                );
            }
            PatKind::None
            | PatKind::Literal(_)
            | PatKind::Identifier(_)
            | PatKind::_Self
            | PatKind::Wildcard => {}
        }

        ids
    }

    pub fn is_wildcard(&self) -> bool {
        matches!(self.kind, PatKind::Wildcard)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct EnumPattern {
    pub path: Path,
    pub elements: Vec<(Ident, Pat)>,
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum PatKind {
    #[default]
    None,
    Identifier(Box<Ident>),
    Literal(Box<Literal>),
    List(Vec<Pat>),
    Rest(Box<Pat>),
    Enum(Box<EnumPattern>),
    Wildcard,
    // TODO: As mentioned on the Keyword enum, we might want this to just be an Identifier(Pattern)
    _Self,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct EnumVariant {
    pub id: NodeId,
    pub name: Ident,
    pub parameters: Vec<StructField>,
    pub discriminant: Option<Box<Expr>>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct EnumDeclaration {
    pub visibility: Visibility,
    pub name: Ident,
    pub variants: Vec<EnumVariant>,
    pub consts: Vec<ConstDeclaration>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Stmt {
    pub id: NodeId,
    pub kind: StmtKind,
    pub span: Span,
    pub leading_comments: Vec<CommentToken>,
    pub trailing_comments: Vec<CommentToken>,
}

impl Stmt {
    pub fn new(id: NodeId, kind: StmtKind) -> Self {
        Stmt {
            id,
            kind,
            span: Span::default(),
            leading_comments: vec![],
            trailing_comments: vec![],
        }
    }

    pub fn with_span(mut self, span: Span) -> Self {
        self.span = span;
        self
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct ProtocolDeclaration {
    pub visibility: Visibility,
    pub name: Ident,
    pub constraints: Vec<Path>,
    pub methods: Vec<ProtocolMethodSignature>,
    pub consts: Vec<ConstDeclaration>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct ProtocolMethodSignature {
    pub id: NodeId,
    pub name: Ident,
    pub parameters: Vec<FunctionParameter>,
    pub return_type_annotation: Option<Ty>,
    pub body: Option<Block>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct ImplBlock {
    pub protocol_name: Path,
    pub target_type: Path,
    pub methods: Vec<FunctionDeclaration>,
    pub apply_methods: Vec<Ident>,
}

/// A single import item within a `use` declaration.
///
/// Represents either `name` or `name as alias`.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct UseItem {
    pub name: Ident,
    pub alias: Option<Ident>,
    pub span: Span,
}

/// A `use` declaration for importing symbols from other modules.
///
/// Supports:
/// - `use string::parse::from_char_code`
/// - `use string::parse::from_char_code as fcc`
/// - `use string::{from_char_code, char_code_at}`
/// - `use string::{from_char_code as fcc, char_code_at}`
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct UseDeclaration {
    pub path: Vec<Ident>,
    pub items: Vec<UseItem>,
    pub span: Span,
}

/// A `pub mod` declaration for exposing submodules.
///
/// `pub mod parse, utils` exposes the `parse` and `utils` submodules.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct ModDeclaration {
    pub visibility: Visibility,
    pub names: Vec<Ident>,
    pub span: Span,
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum StmtKind {
    #[default]
    None,
    Expr(Box<Expr>),
    Let(Box<LetDeclaration>),
    Const(Box<ConstDeclaration>),
    FunctionDeclaration(Box<FunctionDeclaration>),
    // TODO: We should probably deal with this in HIR instead.
    FunctionDeclarations(Vec<FunctionDeclaration>),
    Return(Option<Box<Expr>>),
    EnumDeclaration(Box<EnumDeclaration>),
    StructDeclaration(Box<StructDeclaration>),
    ProtocolDeclaration(Box<ProtocolDeclaration>),
    ImplBlock(Box<ImplBlock>),
    UseDeclaration(Box<UseDeclaration>),
    ModDeclaration(Box<ModDeclaration>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct LetDeclaration {
    pub pattern: Pat,
    pub expression: Expr,
    pub type_annotation: Option<Ty>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct StructDeclaration {
    pub visibility: Visibility,
    pub name: Ident,
    pub fields: Vec<StructField>,
    pub consts: Vec<ConstDeclaration>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct StructField {
    pub id: NodeId,
    pub name: Ident,
    pub ty: Ty,
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum TyKind {
    #[default]
    Unknown,
    Path(Path),
    Union(Vec<Path>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Ty {
    pub id: NodeId,
    pub kind: TyKind,
    pub parameters: Vec<Ty>,
    pub span: Span,
}

impl Ty {
    pub fn new(id: NodeId, name: Path) -> Self {
        Ty {
            id,
            kind: TyKind::Path(name),
            parameters: vec![],
            span: Span::default(),
        }
    }

    pub fn new_unknown(id: NodeId) -> Self {
        Ty {
            id,
            kind: TyKind::Unknown,
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

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct MatchArm {
    pub id: NodeId,
    pub pattern: Pat,
    pub guard: Option<Expr>,
    pub expression: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Module {
    pub id: NodeId,
    pub statements: Vec<Stmt>,
    pub span: Span,
}

impl Module {
    pub fn new(id: NodeId, statements: Vec<Stmt>, span: Span) -> Self {
        Module {
            id,
            statements,
            span,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum UnaryOp {
    Minus,
    Not,
    BitwiseNot,
    Rest,
    Spread,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct OperatorInfo {
    pub precedence: u8,
    pub associativity: Associativity,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum BinaryOpKind {
    Assign,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Exp,
    Eq,
    NotEq,
    Less,
    LessEq,
    Greater,
    GreaterEq,
    And,
    Or,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    Pipeline,
    Match,
    NotMatch,
}

pub type BinaryOp = Spanned<BinaryOpKind>;
