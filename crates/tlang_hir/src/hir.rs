use serde::Serialize;
use tlang_ast::node::{BinaryOpKind, Ident, UnaryOp};
use tlang_ast::span::Span;
use tlang_ast::token::{Literal, Token};

#[derive(Debug, PartialEq, Clone, Copy, Serialize)]
pub struct HirId(usize);

impl HirId {
    pub fn new(id: usize) -> Self {
        HirId(id)
    }

    pub fn next(&self) -> Self {
        HirId(self.0 + 1)
    }
}

#[derive(Debug, Serialize)]
pub struct Path {
    pub segments: Vec<PathSegment>,
    pub span: Span,
}

impl Path {
    pub fn join(&self, separator: &str) -> String {
        self.segments
            .iter()
            .map(|segment| segment.ident.as_str())
            .collect::<Vec<_>>()
            .join(separator)
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct PathSegment {
    pub ident: Ident,
}

impl PathSegment {
    pub fn new(ident: Ident) -> Self {
        PathSegment { ident }
    }

    pub fn from_str(name: &str, span: Span) -> Self {
        PathSegment {
            ident: Ident::new(name, span),
        }
    }
}

#[derive(Debug, Default, Serialize)]
pub struct Module {
    pub block: Block,
    pub span: Span,
}

#[derive(Debug, Default, Serialize)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub expr: Option<Expr>,
    pub span: Span,
}

impl Block {
    pub fn has_completion(&self) -> bool {
        self.expr.is_some()
    }
}

#[derive(Debug, Serialize)]
pub struct Stmt {
    pub hir_id: HirId,
    pub kind: StmtKind,
    pub span: Span,
    // TODO: We might want to handle this somehow different, as we pass them on from the AST to
    //       HIR, which feels somewhat unnecessary.
    pub leading_comments: Vec<Token>,
    pub trailing_comments: Vec<Token>,
}

#[derive(Debug, Serialize)]
pub enum StmtKind {
    Expr(Box<Expr>),
    Let(Box<Pat>, Box<Expr>, Box<Ty>),
    FunctionDeclaration(Box<FunctionDeclaration>),
    Return(Box<Option<Expr>>),
    EnumDeclaration(Box<EnumDeclaration>),
    StructDeclaration(Box<StructDeclaration>),
}

#[derive(Debug, Serialize)]
pub struct Pat {
    pub kind: PatKind,
    pub span: Span,
}

impl Pat {
    pub fn is_self(&self) -> bool {
        match self.kind {
            PatKind::Identifier(_, ref ident) => ident.name == "self",
            _ => false,
        }
    }

    pub fn is_wildcard(&self) -> bool {
        matches!(self.kind, PatKind::Wildcard)
    }
}

#[derive(Debug, Serialize)]
pub enum PatKind {
    Wildcard,
    Identifier(HirId, Box<Ident>),
    Literal(Box<Literal>),
    List(Vec<Pat>),
    Rest(Box<Pat>),
    Enum(Box<Path>, Vec<Pat>),
}

#[derive(Debug, Serialize)]
pub struct ElseClause {
    pub condition: Option<Expr>,
    pub consequence: Block,
}

#[derive(Debug, Serialize)]
pub struct MatchArm {
    pub pat: Pat,
    pub guard: Option<Expr>,
    pub expr: Expr,
}

#[derive(Debug, Serialize)]
pub struct RangeExpression {
    pub start: Expr,
    pub end: Expr,
    pub inclusive: bool,
}

#[derive(Debug, Serialize)]
pub struct CallExpression {
    pub callee: Expr,
    pub arguments: Vec<Expr>,
}

impl CallExpression {
    pub fn has_wildcard(&self) -> bool {
        self.arguments.iter().any(|arg| arg.is_wildcard())
    }

    pub fn wildcard_count(&self) -> usize {
        self.arguments
            .iter()
            .filter(|arg| arg.is_wildcard())
            .count()
    }
}

#[derive(Debug, Serialize)]
pub struct Expr {
    pub hir_id: HirId,
    pub kind: ExprKind,
    pub span: Span,
}

impl Expr {
    pub fn is_wildcard(&self) -> bool {
        matches!(self.kind, ExprKind::Wildcard)
    }
}

#[derive(Debug, Serialize)]
pub enum ExprKind {
    Block(Box<Block>),
    Call(Box<CallExpression>),
    TailCall(Box<CallExpression>),
    Binary(BinaryOpKind, Box<Expr>, Box<Expr>),
    Unary(UnaryOp, Box<Expr>),
    // Let expression, only valid within if conditions and guards
    Let(Box<Pat>, Box<Expr>),
    IfElse(Box<Expr>, Box<Block>, Vec<ElseClause>),
    Path(Box<Path>),
    List(Vec<Expr>),
    Dict(Vec<(Expr, Expr)>),
    FunctionExpression(Box<FunctionDeclaration>),
    FieldAccess(Box<Expr>, Ident),
    IndexAccess(Box<Expr>, Box<Expr>),
    Literal(Box<Literal>),
    Match(Box<Expr>, Vec<MatchArm>),
    Range(Box<RangeExpression>),
    Wildcard, // TODO: This might be better to just be an identifier
}

#[derive(Debug, Serialize)]
pub struct Ty {
    pub kind: TyKind,
    pub span: Span,
}

#[derive(Debug, Serialize)]
pub enum TyKind {
    Unknown,
    Path(Path),
}

#[derive(Debug, Serialize)]
pub struct FunctionParameter {
    pub pattern: Pat,
    pub type_annotation: Ty,
    pub span: Span,
}

#[derive(Debug, Serialize)]
pub struct FunctionDeclaration {
    pub hir_id: HirId,
    pub name: Expr,
    pub parameters: Vec<FunctionParameter>,
    pub return_type: Ty,
    pub body: Block,
    pub span: Span,
}

impl FunctionDeclaration {
    pub fn new_empty_fn(hir_id: HirId, name: Expr, parameters: Vec<FunctionParameter>) -> Self {
        FunctionDeclaration {
            hir_id,
            name,
            parameters,
            return_type: Ty {
                kind: TyKind::Unknown,
                span: Span::default(),
            },
            body: Block {
                stmts: vec![],
                expr: None,
                span: Span::default(),
            },
            span: Span::default(),
        }
    }
}

#[derive(Debug, Serialize)]
pub struct StructDeclaration {
    pub hir_id: HirId,
    pub name: Ident,
    pub fields: Vec<StructField>,
}

#[derive(Debug, Serialize)]
pub struct StructField {
    pub hir_id: HirId,
    pub name: Ident,
    pub ty: Ty,
}

#[derive(Debug, Serialize)]
pub struct EnumDeclaration {
    pub hir_id: HirId,
    pub name: Ident,
    pub variants: Vec<EnumVariant>,
}

#[derive(Debug, Serialize)]
pub struct EnumVariant {
    pub hir_id: HirId,
    pub name: Ident,
    pub parameters: Vec<Ident>,
    // TODO: Do we want/need this in the HIR?
    pub named_fields: bool,
    pub span: Span,
}
