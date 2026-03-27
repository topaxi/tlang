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
  TaggedStringContentDouble,
  TaggedStringContentSingle,
  InterpolationStart,
  InterpolationEnd,
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
      if (stack.canShift(TaggedStringOpenDouble)) {
        input.advance();
        input.acceptToken(TaggedStringOpenDouble);
      } else if (stack.canShift(TaggedStringDoubleClose)) {
        input.advance();
        input.acceptToken(TaggedStringDoubleClose);
      }
    } else if (input.next === 39 /* ' */) {
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

// Tokenizes the string content between interpolations (or between quotes).
export const taggedStringContentTokenizer = new ExternalTokenizer(
  (input, stack) => {
    if (stack.canShift(TaggedStringContentDouble)) {
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
