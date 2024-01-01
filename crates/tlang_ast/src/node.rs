use std::cell::RefCell;
use std::rc::Rc;

use crate::{
    symbols::{SymbolId, SymbolTable},
    token::{Literal, Token},
};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Associativity {
    Left,
    Right,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Node {
    pub ast_node: AstNode,
    pub symbol_table: Option<Rc<RefCell<SymbolTable>>>,
}

impl Node {
    pub fn new(ast_node: AstNode) -> Self {
        Node {
            ast_node,
            symbol_table: None,
        }
    }
}

impl From<AstNode> for Node {
    fn from(ast_node: AstNode) -> Self {
        Node {
            ast_node,
            symbol_table: None,
        }
    }
}

impl<'a> From<&'a Token> for Node {
    fn from(token: &Token) -> Self {
        Node {
            ast_node: AstNode::from(token),
            symbol_table: None,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionDeclaration {
    pub parameters: Vec<Node>,
    pub body: Box<Node>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum AstNode {
    None,
    Program(Vec<Node>),
    Block(Vec<Node>, Option<Box<Node>>),
    Literal(Literal),
    List(Vec<Node>),
    Dict(Vec<(Node, Node)>),
    PrefixOp(PrefixOp, Box<Node>),
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
        name: String,
        value: Box<Node>,
    },
    FunctionDeclaration {
        id: SymbolId,
        name: String,
        declaration: FunctionDeclaration,
    },
    FunctionDeclarations {
        id: SymbolId,
        name: String,
        declarations: Vec<FunctionDeclaration>,
    },
    FunctionExpression {
        id: SymbolId,
        name: Option<String>,
        declaration: FunctionDeclaration,
    },
    FunctionParameter {
        id: SymbolId,
        node: Box<Node>,
    },
    ReturnStatement(Option<Box<Node>>),
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
        else_branch: Option<Box<Node>>,
    },
    Identifier(String),
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
    EnumExtraction {
        identifier: Box<Node>,
        elements: Vec<Node>,
        named_fields: bool,
    },
}

impl<'a> From<&'a Token> for AstNode {
    fn from(token: &Token) -> Self {
        match token {
            Token::Literal(literal) => AstNode::Literal(literal.clone()),
            Token::Identifier(name) => AstNode::Identifier(name.clone()),
            Token::SingleLineComment(comment) => AstNode::SingleLineComment(comment.clone()),
            Token::MultiLineComment(comment) => AstNode::MultiLineComment(comment.clone()),
            _ => unimplemented!(
                "Expected token to be a literal or identifier, found {:?}",
                token
            ),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum PrefixOp {
    Rest,
    Spread,
}

#[derive(Debug, Clone, Copy)]
pub struct OperatorInfo {
    pub precedence: u8,
    pub associativity: Associativity,
}

#[derive(Debug, PartialEq, Clone, Copy)]
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
    ($node:ident) => {
        Node::new(AstNode::$node)
    };

    ($node:ident($( $arg:expr ),* $(,)? )) => {
        Node::new(AstNode::$node( $( $arg ),* ))
    };

    ($node:ident { $( $field:ident : $value:expr ),* $(,)? }) => {
        Node::new(AstNode::$node { $( $field : $value ),* })
    };
}

pub use new;
