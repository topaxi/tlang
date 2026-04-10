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
  #constraints;

  constructor(def, constraints = []) {
    this.#def = def;
    this.#constraints = constraints;

    for (let methodName in def) {
      this[methodName] = this.#apply(methodName);
    }
  }

  $setImpl(Type, methods, typeArgs = null) {
    const key = typeArgs ? `${Type?.name ?? Type}::${typeArgs}` : Type;
    this.#impls.set(key, methods);
  }

  $implements(value) {
    const Type = value?.constructor;
    return this.#impls.has(Type);
  }

  $getConstraints() {
    return this.#constraints;
  }

  #apply(methodName) {
    return (self, ...args) => this.#call(methodName, self, ...args);
  }

  #call(methodName, self, ...args) {
    const Type = self?.constructor;

    // If the last argument is a string type-arg key (passed by the compiler
    // for generic protocol dispatch like `Into<i64>`), try a type-parameterized
    // lookup first.
    let impl;
    const lastArg = args[args.length - 1];
    if (typeof lastArg === 'string' && args.length > 0) {
      const typeArgKey = `${Type?.name ?? Type}::${lastArg}`;
      impl = this.#impls.get(typeArgKey);
      if (impl) {
        // Remove the type-arg key from the argument list before calling the method.
        args = args.slice(0, -1);
      }
    }

    if (!impl) {
      impl = this.#impls.get(Type) ?? this.#def;
    }

    const method = impl[methodName] ?? this.#def[methodName];

    return method.call(impl, self, ...args);
  }
}

export function $protocol(...constraintsAndDef) {
  const def = constraintsAndDef.pop();
  return new $Protocol(def, constraintsAndDef);
}

export function $impl(protocol, Type, methods, typeArgs) {
  protocol.$setImpl(Type, methods, typeArgs);
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

export const $uncurryThis = (() => {
  const cache = new WeakMap();
  const uncurry = Function.prototype.bind.bind(Function.prototype.call);

  /**
   * @template This
   * @template {(this: This, ...args: any[]) => any} T
   * @param {T} fn - The function to uncurry.
   * @return {(self: This, ...args: Parameters<T>) => ReturnType<T>} The uncurried function.
   */
  return (fn) => {
    let uncurried = cache.get(fn);
    if (uncurried == null) {
      uncurried = uncurry(fn);
      cache.set(fn, uncurried);
    }
    return uncurried;
  };
})();

export const $tag = (() => {
  const cache = new WeakMap();

  /**
   * Wraps a tlang tag function `(parts: string[], values: unknown[]) => T`
   * into a JavaScript-compatible tagged template function
   * `(strings: TemplateStringsArray, ...values: unknown[]) => T`.
   *
   * This lets tlang tag functions be called with native JS template literal
   * syntax, preserving the same `(parts, values)` calling convention.
   *
   * @template T
   * @param {(parts: string[], values: unknown[]) => T} fn
   * @return {(strings: TemplateStringsArray, ...values: unknown[]) => T}
   */
  return (fn) => {
    let tagged = cache.get(fn);
    if (tagged == null) {
      tagged = (strings, ...values) => fn([...strings], values);
      cache.set(fn, tagged);
    }
    return tagged;
  };
})();
