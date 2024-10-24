use tlang_ast::node::{BinaryOpKind, Ident, UnaryOp};
use tlang_ast::span::Span;
use tlang_ast::token::Literal;

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct HirId(usize);

impl HirId {
    pub fn new(id: usize) -> Self {
        HirId(id)
    }

    pub fn next(&self) -> Self {
        HirId(self.0 + 1)
    }
}

#[derive(Debug)]
pub struct Path {
    pub segments: Vec<PathSegment>,
    pub span: Span,
}

#[derive(Debug)]
pub struct PathSegment {
    pub ident: Ident,
}

#[derive(Debug)]
pub struct Module {
    pub block: Block,
    pub span: Span,
}

#[derive(Debug)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub expr: Option<Expr>,
    pub span: Span,
}

#[derive(Debug)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum StmtKind {
    Expr(Box<Expr>),
    Let(Box<Pat>, Box<Expr>, Box<Ty>),
    FunctionDeclaration(Box<FunctionDeclaration>),
    Return(Box<Option<Expr>>),
    EnumDeclaration,
    StructDeclaration,
}

#[derive(Debug)]
pub struct Pat {
    pub kind: PatKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum PatKind {
    Wildcard,
    Identifier(HirId, Box<Ident>),
    Literal(Box<Literal>),
    List(Vec<Pat>),
    Rest(Box<Pat>),
    Enum(Box<Path>, Vec<Pat>),
}

#[derive(Debug)]
pub struct ElseClause {
    pub condition: Option<Expr>,
    pub consequence: Block,
}

#[derive(Debug)]
pub struct MatchArm {
    pub pat: Pat,
    pub guard: Option<Expr>,
    pub expr: Expr,
}

#[derive(Debug)]
pub struct RangeExpression {
    pub start: Expr,
    pub end: Expr,
    pub inclusive: bool,
}

#[derive(Debug)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum ExprKind {
    Block(Box<Block>),
    Call(Box<Expr>, Vec<Expr>),
    TailCall(Box<Expr>, Vec<Expr>),
    MethodCall(PathSegment, Box<Expr>, Vec<Expr>),
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

#[derive(Debug)]
pub struct Ty {
    pub kind: TyKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum TyKind {
    Unknown,
    Path(Path),
}

#[derive(Debug)]
pub struct FunctionParameter {
    pub pattern: Pat,
    pub type_annotation: Ty,
    pub span: Span,
}

#[derive(Debug)]
pub struct FunctionDeclaration {
    pub hir_id: HirId,
    pub name: Expr,
    pub parameters: Vec<FunctionParameter>,
    pub return_type: Ty,
    pub body: Block,
    pub span: Span,
}
