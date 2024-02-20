use serde::Serialize;
use std::rc::Rc;
use std::{cell::RefCell, fmt::Display};

use crate::{
    span::{Span, Spanned},
    symbols::{SymbolId, SymbolTable},
    token::{Literal, TokenKind},
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
    pub pattern: Box<Pattern>,
    pub type_annotation: Box<Option<Ty>>,
    pub span: Span,
}

impl FunctionParameter {
    pub fn new(pattern: Pattern) -> Self {
        FunctionParameter {
            pattern: Box::new(pattern),
            type_annotation: Box::new(None),
            span: Span::default(),
        }
    }

    pub fn with_span(mut self, span: Span) -> Self {
        self.span = span;
        self
    }

    pub fn with_type_annotation(mut self, type_annotation: Ty) -> Self {
        self.type_annotation = Box::new(Some(type_annotation));
        self
    }
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct FunctionDeclaration {
    pub id: SymbolId,
    pub name: Box<Expr>,
    pub parameters: Vec<FunctionParameter>,
    pub guard: Box<Option<Expr>>,
    pub return_type_annotation: Box<Option<Ty>>,
    pub body: Box<Expr>,
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct Expr {
    pub kind: ExprKind,
    pub symbol_table: Option<Rc<RefCell<SymbolTable>>>,
    pub span: Span,
}

impl Expr {
    pub fn new(kind: ExprKind) -> Self {
        Expr {
            kind,
            symbol_table: None,
            span: Span::default(),
        }
    }

    pub fn with_span(mut self, span: Span) -> Self {
        self.span = span;
        self
    }
}

#[derive(Debug, Default, PartialEq, Clone, Serialize)]
pub enum ExprKind {
    #[default]
    None,
    Block(Vec<Stmt>, Box<Option<Expr>>),
    Call {
        function: Box<Expr>,
        arguments: Vec<Expr>,
    },
    Dict(Vec<(Expr, Expr)>),
    FunctionExpression(FunctionDeclaration),
    FieldExpression {
        base: Box<Expr>,
        field: Box<Ident>,
    },
    IndexExpression {
        base: Box<Expr>,
        index: Box<Expr>,
    },
    // Let expression, only valid within if conditions and guards
    Let(Box<Pattern>, Box<Expr>),
    IfElse {
        condition: Box<Expr>,
        then_branch: Box<Expr>,
        else_branch: Box<Option<Expr>>,
    },
    List(Vec<Expr>),
    Literal(Literal),
    Path(Path),
    UnaryOp(UnaryOp, Box<Expr>),
    BinaryOp {
        op: BinaryOpKind,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Match {
        expression: Box<Expr>,
        arms: Vec<MatchArm>,
    },
    RecursiveCall(Box<Expr>),
    Range {
        start: Box<Expr>,
        end: Box<Expr>,
        inclusive: bool,
    },
    Wildcard,
}

impl<'a> From<&'a TokenKind> for ExprKind {
    fn from(token: &TokenKind) -> Self {
        match token {
            TokenKind::Literal(literal) => ExprKind::Literal(literal.clone()),
            TokenKind::Identifier(name) => {
                ExprKind::Path(Path::from_ident(Ident::new(name, Default::default())))
            }
            _ => unimplemented!(
                "Expected token to be a literal or identifier, found {:?}",
                token
            ),
        }
    }
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
        Path {
            segments: vec![ident.clone()],
            span: ident.span,
        }
    }

    pub fn with_span(mut self, span: Span) -> Self {
        self.span = span;
        self
    }

    pub fn push(&mut self, ident: Ident) {
        self.segments.push(ident);
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
                .flat_map(|pattern| pattern.get_all_symbol_ids())
                .collect(),
            PatternKind::Rest(pattern) => pattern.get_all_symbol_ids(),
            PatternKind::Enum { elements, .. } => elements
                .iter()
                .flat_map(|pattern| pattern.get_all_symbol_ids())
                .collect(),
            _ => todo!(
                "Getting symbol ids for pattern kind {:?} not implemented yet",
                self.kind
            ),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub enum PatternKind {
    Identifier {
        id: SymbolId,
        name: Ident,
    },
    Literal(Literal),
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

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
    pub symbol_table: Option<Rc<RefCell<SymbolTable>>>,
}

impl Stmt {
    pub fn new(kind: StmtKind) -> Self {
        Stmt {
            kind,
            span: Span::default(),
            symbol_table: None,
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
    Let {
        pattern: Box<Pattern>,
        expression: Box<Expr>,
        type_annotation: Box<Option<Ty>>,
    },
    FunctionDeclaration(FunctionDeclaration),
    // Should this really be handled within the parser or should this be done in later stages?
    FunctionDeclarations(Vec<Stmt>),
    Return(Box<Option<Expr>>),
    SingleLineComment(String),
    MultiLineComment(String),
    EnumDeclaration(EnumDeclaration),
}

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
