/* eslint-disable no-undef, @typescript-eslint/no-unused-vars */
function panic(msg) {
  throw new Error(msg);
}

function $installMethod(proto, methodName, dispatch) {
  if (proto[methodName] !== undefined)
    panic(
      `Method collision: '${methodName}' already defined on ${proto.constructor?.name ?? typeof proto}`,
    );
  proto[methodName] = function (...args) {
    return dispatch(this, ...args);
  };
}

$Functor ??= {};
$Functor.List ??= {};
$Functor.List.map ??= function (self, f) {
  const result = new Array(self.length);
  for (let i = 0; i < self.length; i++) result[i] = f(self[i]);
  return result;
};

$Iterable ??= {};
$Iterable.List = class ListIterable {
  static iter(self) {
    return new this(self);
  }

  #list;
  #index = 0;

  /**
   * @generic T
   * @param {Array<T>} list
   */
  constructor(list) {
    this.#list = list;
  }

  next() {
    if (this.#index < this.#list.length) {
      return Option.Some(this.#list[this.#index++]);
    }

    return Option.None;
  }
};

$Iterator ??= {};
$Iterator[$Iterable.List.name] = class ListIterator {
  /**
   * @param {$Iterable.List} self
   */
  static next(self) {
    return self.next();
  }
};

function $spread(value) {
  if (value[Symbol.iterator]) return value;
  return $iter(value);

  function* $iter(value) {
    const iter = $Iterable.iter(value);
    for (;;) {
      const { tag, [0]: value } = $Iterator.next(iter);
      if (tag === Option.Some) {
        yield value;
      } else {
        break;
      }
    }
  }
}

string ??= {};

/**
 * @param {string} str
 * @param {number} idx
 */
string.char_code_at = (str, idx) => str.charCodeAt(idx);

class __TlangRegex {
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
    return new __TlangRegex(this.#source, newFlags);
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
}
