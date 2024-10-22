use tlang_ast::node::{BinaryOpKind, Ident, UnaryOp};
use tlang_ast::span::Span;

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
    Let(Pat, Expr, Ty),
    FunctionDeclaration(Box<FunctionDeclaration>),
    Return(Expr),
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
    Identifier,
    Literal,
    List(Vec<Pat>),
    Rest(Box<Pat>),
    Enum,
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
    IfElse(Box<Expr>, Box<Expr>, Option<Box<Expr>>),
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
    pub name: Expr,
    pub parameters: Vec<FunctionParameter>,
    pub return_type: Ty,
    pub body: Block,
    pub span: Span,
}
