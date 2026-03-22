export function panic(msg) {
  throw new Error(msg);
}

export function $getType(value) {
  return Array.isArray(value)
    ? 'List'
    : (value?.constructor?.name ?? typeof value);
}

export class $Protocol {
  #def;
  #impls = new Map();

  constructor(def) {
    this.#def = def;

    for (let methodName in def) {
      this[methodName] = this.#apply(methodName);
    }
  }

  $setImpl(Type, methods) {
    this.#impls.set(Type, methods);
  }

  #apply(methodName) {
    return (self, ...args) => this.#call(methodName, self, ...args);
  }

  #call(methodName, self, ...args) {
    const Type = self?.constructor;
    const impl = this.#impls.get(Type) ?? this.#def;
    const method = impl[methodName] ?? this.#def[methodName];

    return method.call(impl, self, ...args);
  }
}

export function $protocol(def) {
  return new $Protocol(def);
}

export function $impl(protocol, Type, methods) {
  protocol.$setImpl(Type, methods);
}

export function $installMethod(proto, methodName, dispatch) {
  $assert(
    !Reflect.has(proto, methodName),
    () =>
      `Method collision: '${methodName}' already defined on ${$getType(proto)}`,
  );

  proto[methodName] = function (...args) {
    return dispatch(this, ...args);
  };
}

export class $AssertError extends Error {}

/**
 * @param {boolean} cond - The condition to assert.param
 * @param {string | () => string} msg - The error message to throw if the assertion fails.
 * @return {asserts cond}
 */
export function $assert(cond, msg) {
  if (!cond) throw new $AssertError(typeof msg === 'function' ? msg() : msg);
}
