# tlang_lexer

The lexer (lexical analyzer) for the tlang programming language.

## Responsibilities

*   Takes raw source code string input.
*   Produces a stream of `Token`s.
*   Handles whitespace and comment skipping.
*   Tracks source location (Span) for each token.
