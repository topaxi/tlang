# tlang_parser

The parser for the tlang programming language.

## Responsibilities

*   Consumes a stream of `Token`s from `tlang_lexer`.
*   Implements a recursive descent parser.
*   Produces an Abstract Syntax Tree (AST) defined in `tlang_ast`.
*   Handles syntax error recovery (to some extent).
