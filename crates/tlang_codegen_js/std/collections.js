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
