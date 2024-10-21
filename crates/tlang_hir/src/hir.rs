use tlang_ast::node::{BinaryOp, Ident, UnaryOp};
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
    Expr(Expr),
    Let(Pat, Option<Expr>),
    FunctionDeclaration,
    Return,
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
}

#[derive(Debug)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum ExprKind {
    Call,
    TailCall,
    MethodCall(PathSegment, Box<Expr>, Vec<Expr>),
    Binary(BinaryOp, Box<Expr>, Box<Expr>),
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
    Path(Path),
}
