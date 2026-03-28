//!trackNewline

import { ContextTracker, ExternalTokenizer } from '@lezer/lr';
import {
  spaces,
  newline,
  BlockComment,
  LineComment,
  TagStringTag,
  TaggedStringOpenDouble,
  TaggedStringDoubleClose,
  TaggedStringOpenSingle,
  TaggedStringSingleClose,
  TaggedStringOpenTripleDouble,
  TaggedStringTripleDoubleClose,
  TaggedStringOpenTripleSingle,
  TaggedStringTripleSingleClose,
  TaggedStringContentDouble,
  TaggedStringContentSingle,
  TaggedStringContentTripleDouble,
  TaggedStringContentTripleSingle,
  InterpolationStart,
  InterpolationEnd,
  TripleString,
} from './parser.terms.js';

export const trackNewline = new ContextTracker({
  start: false,
  shift(context, term) {
    return term == LineComment || term == BlockComment || term == spaces
      ? context
      : term == newline;
  },
  strict: false,
});

//!externalTokenizers

import { insertSemi, noSemi } from './parser.terms.js';

const space = [9, 10, 11, 12, 13, 32, 133, 160];
const braceR = 125;
const semicolon = 59;
const slash = 47;
const star = 42;

export const insertSemicolon = new ExternalTokenizer(
  (input, stack) => {
    let { next } = input;
    if (next == braceR || next == -1 || stack.context)
      input.acceptToken(insertSemi);
  },
  { contextual: true, fallback: true },
);

export const noSemicolon = new ExternalTokenizer(
  (input, stack) => {
    let { next } = input,
      after;
    if (space.indexOf(next) > -1) return;
    if (next == slash && ((after = input.peek(1)) == slash || after == star))
      return;
    if (next != braceR && next != semicolon && next != -1 && !stack.context)
      input.acceptToken(noSemi);
  },
  { contextual: true },
);

//!taggedStringTokenizers

const identifierStartChar = (ch) =>
  (ch >= 65 && ch <= 90) ||
  (ch >= 97 && ch <= 122) ||
  ch === 95 ||
  ch === 36 ||
  ch > 127;

const identifierContinueChar = (ch) =>
  identifierStartChar(ch) || (ch >= 48 && ch <= 57);

// Tokenizes the tag identifier of a tagged string (e.g., `re` in re"...").
// Only emits if the identifier is immediately followed by a quote character.
export const tagStringTagTokenizer = new ExternalTokenizer((input) => {
  if (!identifierStartChar(input.next)) return;
  let len = 1;
  while (identifierContinueChar(input.peek(len))) len++;
  const ch = input.peek(len);
  if (ch !== 34 /* " */ && ch !== 39 /* ' */) return;
  for (let i = 0; i < len; i++) input.advance();
  input.acceptToken(TagStringTag);
});

// Tokenizes the opening and closing quotes of tagged strings.
// Uses stack.canShift to determine whether to emit open or close variant.
// Input is only advanced after confirming a token can be shifted, to avoid
// consuming the quote without producing a token.
export const taggedStringQuoteTokenizer = new ExternalTokenizer(
  (input, stack) => {
    if (input.next === 34 /* " */) {
      // Check for triple-quote first (""")
      if (input.peek(1) === 34 && input.peek(2) === 34) {
        if (stack.canShift(TaggedStringOpenTripleDouble)) {
          input.advance();
          input.advance();
          input.advance();
          input.acceptToken(TaggedStringOpenTripleDouble);
        } else if (stack.canShift(TaggedStringTripleDoubleClose)) {
          input.advance();
          input.advance();
          input.advance();
          input.acceptToken(TaggedStringTripleDoubleClose);
        }
        return;
      }
      if (stack.canShift(TaggedStringOpenDouble)) {
        input.advance();
        input.acceptToken(TaggedStringOpenDouble);
      } else if (stack.canShift(TaggedStringDoubleClose)) {
        input.advance();
        input.acceptToken(TaggedStringDoubleClose);
      }
    } else if (input.next === 39 /* ' */) {
      // Check for triple-quote first (''')
      if (input.peek(1) === 39 && input.peek(2) === 39) {
        if (stack.canShift(TaggedStringOpenTripleSingle)) {
          input.advance();
          input.advance();
          input.advance();
          input.acceptToken(TaggedStringOpenTripleSingle);
        } else if (stack.canShift(TaggedStringTripleSingleClose)) {
          input.advance();
          input.advance();
          input.advance();
          input.acceptToken(TaggedStringTripleSingleClose);
        }
        return;
      }
      if (stack.canShift(TaggedStringOpenSingle)) {
        input.advance();
        input.acceptToken(TaggedStringOpenSingle);
      } else if (stack.canShift(TaggedStringSingleClose)) {
        input.advance();
        input.acceptToken(TaggedStringSingleClose);
      }
    }
  },
  { contextual: true },
);

// Reads tagged string content up to the closing quote or an interpolation start.
// Handles: \x (escape), {{ (literal {), }} (literal }), {digit (literal {, not interpolation).
function readTaggedStringContent(input, closeChar) {
  let hasContent = false;
  while (true) {
    const ch = input.next;
    if (ch === -1 || ch === closeChar) break;
    if (ch === 92 /* \ */) {
      input.advance();
      if (input.next !== -1) input.advance();
      hasContent = true;
      continue;
    }
    if (ch === 123 /* { */) {
      const next = input.peek(1);
      if (next === 123 /* {{ → literal { */) {
        input.advance();
        input.advance();
        hasContent = true;
        continue;
      }
      if (identifierStartChar(next)) break; // interpolation start — stop here
      // Literal { (e.g., regex quantifier like {2,5})
    }
    if (ch === 125 /* } */) {
      input.advance();
      if (input.next === 125 /* }} → literal } */) input.advance();
      hasContent = true;
      continue;
    }
    input.advance();
    hasContent = true;
  }
  return hasContent;
}

// Reads triple-quoted tagged string content up to closing triple-quote or interpolation start.
function readTripleTaggedStringContent(input, closeChar) {
  let hasContent = false;
  while (true) {
    const ch = input.next;
    if (ch === -1) break;
    // Check for closing triple-quote
    if (
      ch === closeChar &&
      input.peek(1) === closeChar &&
      input.peek(2) === closeChar
    )
      break;
    if (ch === 92 /* \ */) {
      input.advance();
      if (input.next !== -1) input.advance();
      hasContent = true;
      continue;
    }
    if (ch === 123 /* { */) {
      const next = input.peek(1);
      if (next === 123 /* {{ → literal { */) {
        input.advance();
        input.advance();
        hasContent = true;
        continue;
      }
      if (identifierStartChar(next)) break; // interpolation start — stop here
    }
    if (ch === 125 /* } */) {
      input.advance();
      if (input.next === 125 /* }} → literal } */) input.advance();
      hasContent = true;
      continue;
    }
    input.advance();
    hasContent = true;
  }
  return hasContent;
}

// Tokenizes the string content between interpolations (or between quotes).
export const taggedStringContentTokenizer = new ExternalTokenizer(
  (input, stack) => {
    if (stack.canShift(TaggedStringContentTripleDouble)) {
      if (readTripleTaggedStringContent(input, 34 /* " */))
        input.acceptToken(TaggedStringContentTripleDouble);
    } else if (stack.canShift(TaggedStringContentTripleSingle)) {
      if (readTripleTaggedStringContent(input, 39 /* ' */))
        input.acceptToken(TaggedStringContentTripleSingle);
    } else if (stack.canShift(TaggedStringContentDouble)) {
      if (readTaggedStringContent(input, 34 /* " */))
        input.acceptToken(TaggedStringContentDouble);
    } else if (stack.canShift(TaggedStringContentSingle)) {
      if (readTaggedStringContent(input, 39 /* ' */))
        input.acceptToken(TaggedStringContentSingle);
    }
  },
  { contextual: true },
);

// Tokenizes interpolation delimiters: { (start) and } (end).
// InterpolationStart only fires when { is followed by an identifier-start char.
export const taggedStringInterpolationTokenizer = new ExternalTokenizer(
  (input, stack) => {
    if (input.next === 123 /* { */ && stack.canShift(InterpolationStart)) {
      if (identifierStartChar(input.peek(1))) {
        input.advance();
        input.acceptToken(InterpolationStart);
      }
    } else if (input.next === 125 /* } */ && stack.canShift(InterpolationEnd)) {
      input.advance();
      input.acceptToken(InterpolationEnd);
    }
  },
  { contextual: true },
);

// Tokenizes plain triple-quoted strings ("""...""" and '''...''').
export const tripleStringTokenizer = new ExternalTokenizer((input) => {
  const quote = input.next;
  if (quote !== 34 /* " */ && quote !== 39 /* ' */) return;
  if (input.peek(1) !== quote || input.peek(2) !== quote) return;

  // Consume opening triple-quote
  input.advance();
  input.advance();
  input.advance();

  // Read content until closing triple-quote or EOF
  while (input.next !== -1) {
    if (input.next === 92 /* \ */) {
      input.advance();
      if (input.next !== -1) input.advance();
      continue;
    }
    if (
      input.next === quote &&
      input.peek(1) === quote &&
      input.peek(2) === quote
    ) {
      input.advance();
      input.advance();
      input.advance();
      input.acceptToken(TripleString);
      return;
    }
    input.advance();
  }
  // Unterminated triple-quoted string — accept what we have
  input.acceptToken(TripleString);
});
