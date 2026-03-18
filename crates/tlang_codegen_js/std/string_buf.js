const $StringBuf = class StringBuf {
  #buf;

  constructor(initial = '') {
    this.#buf = [initial];
  }

  push(s) {
    this.#buf.push(s);
    return this;
  }

  push_char(c) {
    this.#buf.push(c);
    return this;
  }

  clear() {
    this.#buf = [];
    return this;
  }

  to_string() {
    const s = this.#buf.join('');
    this.#buf = [s];
    return s;
  }

  len() {
    return this.to_string().length;
  }

  is_empty() {
    return this.to_string().length === 0;
  }

  toString() {
    return this.to_string();
  }

  [Symbol.for('nodejs.util.inspect.custom')]() {
    return this.to_string();
  }
};

const StringBuf = (initial = '') => new $StringBuf(initial);
