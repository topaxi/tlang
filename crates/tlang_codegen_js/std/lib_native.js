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

Functor ??= {};
Functor.List ??= {};
Functor.List.map ??= function (self, f) {
  const result = new Array(self.length);
  for (let i = 0; i < self.length; i++) result[i] = f(self[i]);
  return result;
};

class ArrayIterator {
  constructor(list) {
    this.list = list;
    this.index = 0;
  }
}

Iterable ??= {};
Iterable.List ??= {};
Iterable.List.iter ??= function (self) {
  return new ArrayIterator(self);
};

Iterator ??= {};
Iterator.ArrayIterator ??= {};
Iterator.ArrayIterator.next ??= function (self) {
  if (self.index < self.list.length) {
    return Option.Some(self.list[self.index++]);
  }
  return Option.None;
};
