import { Option } from './option.tlang.js';
import { $Functor, $Iterable, $Iterator } from './protocols.tlang.js';

$impl($Functor, Array, {
  map(self, f) {
    return self.map((v) => f(v));
  },
});

class ListIterable {
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
}

$impl($Iterable, Array, ListIterable);

$impl($Iterator, ListIterable, {
  next(self) {
    return self.next();
  },
});

export function $spread(value) {
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
