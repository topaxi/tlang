const $TlangRegex = class Regex {
  #source;
  #flags;
  #__re = null;

  constructor(source, flags = '') {
    this.#source = source;
    this.#flags = flags;
  }

  get #re() {
    return (this.#__re ??= new RegExp(this.#source, this.#flags));
  }

  test(str) {
    return this.#re.test(str);
  }

  exec(str) {
    const m = this.#re.exec(str);
    if (m === null) return Option.None;
    return Option.Some(m[0]);
  }

  replace_all(str, replacement) {
    const flags = this.#flags.includes('g') ? this.#flags : this.#flags + 'g';
    return str.replaceAll(new RegExp(this.#source, flags), replacement);
  }

  replace_first(str, replacement) {
    const re = new RegExp(this.#source, this.#flags.replace('g', ''));
    return str.replace(re, replacement);
  }

  flags(newFlags) {
    if (newFlags === undefined) return this.#flags;
    return new $TlangRegex(this.#source, newFlags);
  }

  toString() {
    return `/${this.#source}/${this.#flags}`;
  }

  toJSON() {
    return this.toString();
  }

  [Symbol.for('nodejs.util.inspect.custom')]() {
    return this.toString();
  }
};
