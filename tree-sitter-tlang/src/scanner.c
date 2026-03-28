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
  TRIPLE_STRING,          // plain triple-quoted string: """...""" or '''...'''
};

// Scanner state: tracks nesting depth for tagged strings.
// When we enter a tagged string, we push onto the stack.
// When we enter an interpolation and encounter a nested tagged string,
// we push again.
#define MAX_DEPTH 32

// Flags stored alongside each quote character
#define QUOTE_TRIPLE 0x80  // bit flag: triple-quoted string
#define QUOTE_MASK   0x7F  // mask to extract the actual quote character

typedef struct {
  uint8_t depth;
  char quotes[MAX_DEPTH]; // quote character (+ QUOTE_TRIPLE flag) at each nesting level
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
// Detects both single-quote and triple-quote variants.
static bool scan_tagged_string_start(Scanner *scanner, TSLexer *lexer) {
  if (lexer->lookahead != '"' && lexer->lookahead != '\'') {
    return false;
  }

  if (scanner->depth >= MAX_DEPTH) {
    return false;
  }

  char quote = (char)lexer->lookahead;
  bool triple = false;

  advance_lexer(lexer);
  lexer->mark_end(lexer); // mark after first quote (single-quote boundary)

  // Check for triple-quote: two more identical quote characters
  if (lexer->lookahead == (int32_t)quote) {
    advance_lexer(lexer);
    if (lexer->lookahead == (int32_t)quote) {
      advance_lexer(lexer);
      lexer->mark_end(lexer); // mark after all three quotes
      triple = true;
    }
    // If only two quotes (e.g., tag""), mark_end stays after first quote.
    // The second quote will be re-read by the parser as the close.
  }

  scanner->quotes[scanner->depth] = triple ? (quote | QUOTE_TRIPLE) : quote;
  scanner->depth++;

  lexer->result_symbol = TAGGED_STRING_START;
  return true;
}

// Scan tagged string content.
// Consumes characters until:
//   - closing quote (end of tagged string) — single or triple depending on mode
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

  uint8_t stored = (uint8_t)scanner->quotes[scanner->depth - 1];
  char quote = (char)(stored & QUOTE_MASK);
  bool triple = (stored & QUOTE_TRIPLE) != 0;
  bool has_content = false;

  while (true) {
    if (lexer->eof(lexer)) {
      lexer->mark_end(lexer);
      break;
    }

    int32_t c = lexer->lookahead;

    if (c == (int32_t)quote) {
      if (triple) {
        // For triple-quote, check for three consecutive quotes
        lexer->mark_end(lexer);
        advance_lexer(lexer);
        if (lexer->lookahead == (int32_t)quote) {
          advance_lexer(lexer);
          if (lexer->lookahead == (int32_t)quote) {
            // Triple close found — content ends before the first quote
            break;
          }
          // Only two quotes — they're literal content
          has_content = true;
          continue;
        }
        // Single quote in triple-quoted string — literal content
        has_content = true;
        continue;
      } else {
        // Single-quote mode: end of tagged string — don't consume the quote
        lexer->mark_end(lexer);
        break;
      }
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
// Consumes one quote for single-quoted, three for triple-quoted.
static bool scan_tagged_string_end(Scanner *scanner, TSLexer *lexer) {
  if (scanner->depth == 0) {
    return false;
  }

  uint8_t stored = (uint8_t)scanner->quotes[scanner->depth - 1];
  char quote = (char)(stored & QUOTE_MASK);
  bool triple = (stored & QUOTE_TRIPLE) != 0;

  if (lexer->lookahead != (int32_t)quote) {
    return false;
  }

  if (triple) {
    advance_lexer(lexer);
    if (lexer->lookahead != (int32_t)quote) return false;
    advance_lexer(lexer);
    if (lexer->lookahead != (int32_t)quote) return false;
    advance_lexer(lexer);
  } else {
    advance_lexer(lexer);
  }

  scanner->depth--;
  lexer->result_symbol = TAGGED_STRING_END;
  lexer->mark_end(lexer);
  return true;
}

static void skip_whitespace(TSLexer *lexer) {
  while (lexer->lookahead == ' ' || lexer->lookahead == '\t' ||
         lexer->lookahead == '\n' || lexer->lookahead == '\r') {
    lexer->advance(lexer, true);
  }
}

// Scan a plain triple-quoted string: """...""" or '''...'''.
// Recognizes the opening triple-quote, consumes content (including escape
// sequences and lone/double quotes), and consumes the closing triple-quote.
// Skips leading whitespace (rolled back by tree-sitter if scan fails).
static bool scan_triple_string(TSLexer *lexer) {
  skip_whitespace(lexer);

  if (lexer->lookahead != '"' && lexer->lookahead != '\'') {
    return false;
  }

  char quote = (char)lexer->lookahead;

  // Check for triple-quote opening: we need three consecutive quote characters
  advance_lexer(lexer);
  if (lexer->lookahead != (int32_t)quote) return false;
  advance_lexer(lexer);
  if (lexer->lookahead != (int32_t)quote) return false;
  advance_lexer(lexer);

  // Now consume content until closing triple-quote
  while (true) {
    if (lexer->eof(lexer)) {
      // Unterminated — still accept what we have
      lexer->mark_end(lexer);
      lexer->result_symbol = TRIPLE_STRING;
      return true;
    }

    int32_t c = lexer->lookahead;

    if (c == (int32_t)quote) {
      advance_lexer(lexer);
      if (lexer->lookahead == (int32_t)quote) {
        advance_lexer(lexer);
        if (lexer->lookahead == (int32_t)quote) {
          // Closing triple-quote found
          advance_lexer(lexer);
          lexer->mark_end(lexer);
          lexer->result_symbol = TRIPLE_STRING;
          return true;
        }
        // Two quotes — content, continue
        continue;
      }
      // One quote — content, continue
      continue;
    }

    if (c == '\\') {
      // Escape sequence — consume backslash + next char
      advance_lexer(lexer);
      if (!lexer->eof(lexer)) {
        advance_lexer(lexer);
      }
      continue;
    }

    advance_lexer(lexer);
  }
}

bool tree_sitter_tlang_external_scanner_scan(void *payload, TSLexer *lexer,
                                              const bool *valid_symbols) {
  Scanner *scanner = (Scanner *)payload;

  // When all symbols are valid, we're in error recovery mode.
  // Be conservative: only try self-contained tokens (triple strings).
  bool all_valid = valid_symbols[TAGGED_STRING_START] &&
                   valid_symbols[TAGGED_STRING_CONTENT] &&
                   valid_symbols[TAGGED_STRING_END] &&
                   valid_symbols[TRIPLE_STRING];
  if (all_valid) {
    if (scan_triple_string(lexer)) {
      return true;
    }
    return false;
  }

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

  // Try to match plain triple-quoted string.
  // Only when TAGGED_STRING_START is not valid, to avoid consuming the
  // opening quote that a tagged string needs. When both are valid (which
  // shouldn't normally happen since tagged strings require a preceding
  // identifier), tagged string takes priority.
  if (valid_symbols[TRIPLE_STRING] && !valid_symbols[TAGGED_STRING_START]) {
    if (scan_triple_string(lexer)) {
      return true;
    }
  }

  // Try to match opening quote for tagged strings
  if (valid_symbols[TAGGED_STRING_START]) {
    if (scan_tagged_string_start(scanner, lexer)) {
      return true;
    }
    // If tagged string start failed (not a quote), try triple string as fallback
    if (valid_symbols[TRIPLE_STRING]) {
      if (scan_triple_string(lexer)) {
        return true;
      }
    }
  }

  return false;
}
