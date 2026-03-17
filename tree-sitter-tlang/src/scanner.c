#include "tree_sitter/parser.h"

#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

// Token types produced by the external scanner.
// These must match the order in grammar.js externals array.
enum TokenType {
  TAGGED_STRING_START,    // opening quote after tag identifier
  TAGGED_STRING_CONTENT,  // text content between interpolations
  TAGGED_STRING_END,      // closing quote
};

// Scanner state: tracks nesting depth for tagged strings.
// When we enter a tagged string, we push onto the stack.
// When we enter an interpolation and encounter a nested tagged string,
// we push again.
#define MAX_DEPTH 32

typedef struct {
  uint8_t depth;
  char quotes[MAX_DEPTH]; // quote character at each nesting level
} Scanner;

void *tree_sitter_tlang_external_scanner_create(void) {
  Scanner *scanner = calloc(1, sizeof(Scanner));
  return scanner;
}

void tree_sitter_tlang_external_scanner_destroy(void *payload) {
  free(payload);
}

unsigned tree_sitter_tlang_external_scanner_serialize(void *payload,
                                                      char *buffer) {
  Scanner *scanner = (Scanner *)payload;
  if (scanner->depth == 0) {
    buffer[0] = 0;
    return 1;
  }
  buffer[0] = scanner->depth;
  memcpy(buffer + 1, scanner->quotes, scanner->depth);
  return 1 + scanner->depth;
}

void tree_sitter_tlang_external_scanner_deserialize(void *payload,
                                                     const char *buffer,
                                                     unsigned length) {
  Scanner *scanner = (Scanner *)payload;
  scanner->depth = 0;
  memset(scanner->quotes, 0, MAX_DEPTH);

  if (length > 0) {
    scanner->depth = (uint8_t)buffer[0];
    if (scanner->depth > MAX_DEPTH) {
      scanner->depth = MAX_DEPTH;
    }
    if (scanner->depth > 0 && length >= 1 + scanner->depth) {
      memcpy(scanner->quotes, buffer + 1, scanner->depth);
    }
  }
}

static void advance_lexer(TSLexer *lexer) { lexer->advance(lexer, false); }

static bool is_ident_start(int32_t c) {
  return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
}

// Scan the opening quote of a tagged string.
// Called when the grammar expects TAGGED_STRING_START after an identifier.
static bool scan_tagged_string_start(Scanner *scanner, TSLexer *lexer) {
  if (lexer->lookahead != '"' && lexer->lookahead != '\'') {
    return false;
  }

  if (scanner->depth >= MAX_DEPTH) {
    return false;
  }

  char quote = (char)lexer->lookahead;
  scanner->quotes[scanner->depth] = quote;
  scanner->depth++;

  advance_lexer(lexer);
  lexer->result_symbol = TAGGED_STRING_START;
  lexer->mark_end(lexer);
  return true;
}

// Scan tagged string content.
// Consumes characters until:
//   - closing quote (end of tagged string)
//   - `{` followed by ident-start character (start of interpolation)
//   - `{{` → consume as literal `{` (escaped brace)
//   - `\{` → consume as literal `{` (escaped brace)
//   - `\}` → consume as literal `}` (escaped brace)
//   - `}}` → consume as literal `}` (escaped brace)
//   - EOF
static bool scan_tagged_string_content(Scanner *scanner, TSLexer *lexer) {
  if (scanner->depth == 0) {
    return false;
  }

  char quote = scanner->quotes[scanner->depth - 1];
  bool has_content = false;

  while (true) {
    if (lexer->eof(lexer)) {
      lexer->mark_end(lexer);
      break;
    }

    int32_t c = lexer->lookahead;

    if (c == (int32_t)quote) {
      // End of tagged string — don't consume the quote
      lexer->mark_end(lexer);
      break;
    }

    if (c == '{') {
      // Mark before the { so we can return content up to here
      lexer->mark_end(lexer);
      advance_lexer(lexer);
      int32_t next = lexer->lookahead;

      if (next == '{') {
        // {{ → literal {, consume both
        advance_lexer(lexer);
        has_content = true;
        continue;
      }

      if (is_ident_start(next)) {
        // Interpolation start — content ends before the {
        // mark_end was already set before {
        break;
      }

      // Not interpolation (e.g., {2,5} regex quantifier) — literal {
      has_content = true;
      continue;
    }

    if (c == '}') {
      // Check for escape: }}
      advance_lexer(lexer);
      if (lexer->lookahead == '}') {
        advance_lexer(lexer);
      }
      has_content = true;
      continue;
    }

    if (c == '\\') {
      // Escape sequence — consume backslash + next char
      advance_lexer(lexer);
      if (!lexer->eof(lexer)) {
        advance_lexer(lexer);
      }
      has_content = true;
      continue;
    }

    advance_lexer(lexer);
    has_content = true;
  }

  if (has_content) {
    lexer->result_symbol = TAGGED_STRING_CONTENT;
    return true;
  }

  return false;
}

// Scan the closing quote of a tagged string.
static bool scan_tagged_string_end(Scanner *scanner, TSLexer *lexer) {
  if (scanner->depth == 0) {
    return false;
  }

  char quote = scanner->quotes[scanner->depth - 1];

  if (lexer->lookahead == (int32_t)quote) {
    advance_lexer(lexer);
    scanner->depth--;
    lexer->result_symbol = TAGGED_STRING_END;
    lexer->mark_end(lexer);
    return true;
  }

  return false;
}

bool tree_sitter_tlang_external_scanner_scan(void *payload, TSLexer *lexer,
                                              const bool *valid_symbols) {
  Scanner *scanner = (Scanner *)payload;

  // Try to match closing quote first (highest priority when valid)
  if (valid_symbols[TAGGED_STRING_END]) {
    if (scan_tagged_string_end(scanner, lexer)) {
      return true;
    }
  }

  // Try to match content
  if (valid_symbols[TAGGED_STRING_CONTENT]) {
    if (scan_tagged_string_content(scanner, lexer)) {
      return true;
    }
  }

  // Try to match opening quote
  if (valid_symbols[TAGGED_STRING_START]) {
    if (scan_tagged_string_start(scanner, lexer)) {
      return true;
    }
  }

  return false;
}
