use serde::Serialize;
use std::cell::RefCell;
use std::rc::Rc;

use crate::{
    span::Span,
    symbols::{SymbolId, SymbolTable},
    token::{Literal, Token, TokenKind},
};

#[derive(Debug, Clone, Copy, PartialEq, Serialize)]
pub enum Associativity {
    Left,
    Right,
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct Node {
    pub ast_node: AstNode,
    pub symbol_table: Option<Rc<RefCell<SymbolTable>>>,
    pub span: Span,
}

impl Node {
    pub fn new(ast_node: AstNode, span: Span) -> Self {
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
pub enum AstNode {
    None,
    Program(Vec<Node>),
    Block(Vec<Node>, Box<Option<Node>>),
    Literal(Literal),
    List(Vec<Node>),
    ListPattern(Vec<Node>),
    Dict(Vec<(Node, Node)>),
    UnaryOp(UnaryOp, Box<Node>),
    BinaryOp {
        op: BinaryOp,
        lhs: Box<Node>,
        rhs: Box<Node>,
    },
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
    FunctionExpression {
        id: SymbolId,
        name: Box<Option<Node>>,
        declaration: Box<Node>,
    },
    FunctionParameter {
        id: SymbolId,
        pattern: Box<Node>,
        type_annotation: Box<Option<Node>>,
    },
    ReturnStatement(Box<Option<Node>>),
    Match {
        expression: Box<Node>,
        arms: Vec<Node>,
    },
    MatchArm {
        pattern: Box<Node>,
        expression: Box<Node>,
    },
    Wildcard,
    IfElse {
        condition: Box<Node>,
        then_branch: Box<Node>,
        else_branch: Box<Option<Node>>,
    },
    Identifier(String),
    IdentifierPattern {
        id: SymbolId,
        name: String,
    },
    NestedIdentifier(Vec<String>),
    Call {
        function: Box<Node>,
        arguments: Vec<Node>,
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
    FieldExpression {
        base: Box<Node>,
        field: Box<Node>,
    },
    IndexExpression {
        base: Box<Node>,
        index: Box<Node>,
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

impl<'a> From<&'a TokenKind> for AstNode {
    fn from(token: &TokenKind) -> Self {
        match token {
            TokenKind::Literal(literal) => AstNode::Literal(literal.clone()),
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
pub enum BinaryOp {
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
