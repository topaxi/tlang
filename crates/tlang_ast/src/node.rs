use serde::Serialize;
use std::cell::RefCell;
use std::rc::Rc;

use crate::{
    span::{Span, Spanned},
    symbols::{SymbolId, SymbolTable},
    token::{Literal, Token, TokenKind},
};

#[derive(Debug, Clone, Copy, PartialEq, Serialize)]
pub enum Associativity {
    Left,
    Right,
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub enum NodeKind {}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct Node<N = NodeKind> {
    pub ast_node: N,
    pub symbol_table: Option<Rc<RefCell<SymbolTable>>>,
    pub span: Span,
}

impl Node {
    pub fn new(ast_node: NodeKind, span: Span) -> Self {
        Node {
            ast_node,
            symbol_table: None,
            span,
        }
    }
}

impl From<AstNode> for Node {
    fn from(ast_node: AstNode) -> Self {
        Node {
            ast_node,
            symbol_table: None,
            span: Span::default(),
        }
    }
}

impl<'a> From<&'a Token> for Node {
    fn from(token: &Token) -> Self {
        Node {
            ast_node: AstNode::from(&token.kind),
            symbol_table: None,
            span: Span::from_token(token),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct FunctionDeclaration {
    pub parameters: Vec<Node>,
    pub guard: Box<Option<Node>>,
    pub return_type_annotation: Box<Option<Node>>,
    pub body: Box<Node>,
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub enum ExprKind {
    Block(Vec<Node>, Box<Option<Node>>),
    Call {
        function: Box<Expr>,
        arguments: Vec<Expr>,
    },
    Dict(Vec<(Node, Node)>),
    FunctionExpression {
        id: SymbolId,
        name: Box<Option<Node>>,
        declaration: Box<Node>,
    },
    FieldExpression {
        base: Box<Node>,
        field: Box<Node>,
    },
    IndexExpression {
        base: Box<Node>,
        index: Box<Node>,
    },
    IfElse {
        condition: Box<Node>,
        then_branch: Box<Node>,
        else_branch: Box<Option<Node>>,
    },
    List(Vec<Node>),
    Literal(Literal),
    Identifier(String),
    UnaryOp(UnaryOp, Box<Expr>),
    BinaryOp {
        op: BinaryOpKind,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Match {
        expression: Box<Node>,
        arms: Vec<Node>,
    },
    NestedIdentifier(Vec<String>),
}

impl<'a> From<&'a TokenKind> for ExprKind {
    fn from(token: &TokenKind) -> Self {
        match token {
            TokenKind::Literal(literal) => ExprKind::Literal(literal.clone()),
            TokenKind::Identifier(name) => ExprKind::Identifier(name.clone()),
            _ => unimplemented!(
                "Expected token to be a literal or identifier, found {:?}",
                token
            ),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub enum AstNode {
    None,
    Module(Vec<Node>),
    ListPattern(Vec<Node>),
    ExpressionStatement(Box<Node>),
    VariableDeclaration {
        // Unique identifier for the variable, used to reference it in the symbol table and
        // distinguish it from other variables with the same name.
        id: SymbolId,
        pattern: Box<Node>,
        expression: Box<Node>,
        type_annotation: Box<Option<Node>>,
    },
    // Do we still need this to wrap a struct or could we inline?
    FunctionDeclaration(FunctionDeclaration),
    FunctionSingleDeclaration {
        id: SymbolId,
        name: Box<Node>,
        declaration: Box<Node>,
    },
    FunctionDeclarations {
        id: SymbolId,
        name: Box<Node>,
        declarations: Vec<Node>,
    },
    FunctionParameter {
        id: SymbolId,
        pattern: Box<Node>,
        type_annotation: Box<Option<Node>>,
    },
    ReturnStatement(Box<Option<Node>>),
    MatchArm {
        pattern: Box<Node>,
        expression: Box<Node>,
    },
    Wildcard,
    Identifier(String),
    IdentifierPattern {
        id: SymbolId,
        name: String,
    },
    RecursiveCall(Box<Node>),
    SingleLineComment(String),
    MultiLineComment(String),
    EnumDeclaration {
        id: SymbolId,
        name: String,
        variants: Vec<Node>,
    },
    EnumVariant {
        name: String,
        named_fields: bool,
        parameters: Vec<Node>,
    },
    EnumPattern {
        identifier: Box<Node>,
        elements: Vec<Node>,
        named_fields: bool,
    },
    Range {
        start: Box<Node>,
        end: Box<Node>,
        inclusive: bool,
    },
    TypeAnnotation {
        name: Box<Node>,
        parameters: Vec<Node>,
    },
}

impl Default for AstNode {
    fn default() -> Self {
        AstNode::None
    }
}

impl<'a> From<&'a TokenKind> for AstNode {
    fn from(token: &TokenKind) -> Self {
        match token {
            TokenKind::Identifier(name) => AstNode::Identifier(name.clone()),
            TokenKind::SingleLineComment(comment) => AstNode::SingleLineComment(comment.clone()),
            TokenKind::MultiLineComment(comment) => AstNode::MultiLineComment(comment.clone()),
            _ => unimplemented!(
                "Expected token to be a literal or identifier, found {:?}",
                token
            ),
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
macro_rules! new {
    ($node:ident) => {{
        use tlang_ast::node::{Node, AstNode};
        use tlang_ast::span::Span;

        Node::new(AstNode::$node, Span::default())
    }};

    ($node:ident($( $arg:expr ),* $(,)? )) => {{
        use tlang_ast::node::{Node, AstNode};
        use tlang_ast::span::Span;

        Node::new(AstNode::$node( $( $arg ),* ), Span::default())
    }};

    ($node:ident { $( $field:ident : $value:expr ),* $(,)? }) => {{
        use tlang_ast::node::{Node, AstNode};
        use tlang_ast::span::Span;

        Node::new(AstNode::$node { $( $field : $value ),* }, Span::default())
    }};

    ($node:ident, $span:expr) => {{
        use tlang_ast::node::{Node, AstNode};

        Node::new(AstNode::$node, $span)
    }};

    ($node:ident($( $arg:expr ),* $(,)? ), $span:expr) => {{
        use tlang_ast::node::{Node, AstNode};

        Node::new(AstNode::$node( $( $arg ),* ), $span)
    }};

    ($node:ident { $( $field:ident : $value:expr ),* $(,)? }, $span:expr) => {{
        use tlang_ast::node::{Node, AstNode};

        Node::new(AstNode::$node { $( $field : $value ),* }, $span)
    }};
}

pub use new;
