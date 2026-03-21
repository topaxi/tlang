import { $assert } from './globals.js';
import { string } from './string.tlang.js';

export const $StringBuf = class StringBuf {
  #buf;

  constructor(initial = '') {
    this.#buf = [initial];
  }

  push(s) {
    this.#buf.push(s);
    return this;
  }

  push_char(c) {
    $assert(
      typeof c === 'string' && c.length !== 0,
      'StringBuf.push_char expected a non-empty string',
    );

    this.#buf.push(c);
    return this;
  }

  clear() {
    this.#buf.length = 0;
    this.#buf.push('');
    return this;
  }

  to_string() {
    if (this.#buf.length === 1) return this.#buf[0];

    const s = this.#buf.join('');
    this.#buf = [s];
    return s;
  }

  len() {
    return this.#buf.reduce((sum, s) => sum + s.length, 0);
  }

  is_empty() {
    return this.len() === 0;
  }

  toString() {
    return this.to_string();
  }

  [Symbol.for('nodejs.util.inspect.custom')]() {
    return this.to_string();
  }
};

string ??= {};
string.StringBuf = (initial = '') => new $StringBuf(initial);
