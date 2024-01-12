use tlang_ast::{node::Node, span::Span, token::Token};

#[derive(Debug, Clone, PartialEq)]
pub struct ParseError {
    pub msg: String,
    pub kind: ParseErrorKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParseErrorKind {
    UnexpectedToken(Token),
    UnexpectedNode(Node),
    UnexpectedEof,
}
