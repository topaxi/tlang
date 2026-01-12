# tlang_span

Source code location tracking for tlang.

## Responsibilities

*   Defines `Span` type (start, end position).
*   Utilities for converting Spans to line/column numbers.
*   Used by `tlang_lexer` and `tlang_ast` to report error locations.
