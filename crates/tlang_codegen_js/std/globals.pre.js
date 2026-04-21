const $typeArgSymbol = Symbol('$typeArg');

/**
 * Wraps a string type-argument key in an object tagged with a Symbol so that
 * normal string arguments can never be interpreted as type-args during generic
 * protocol dispatch.
 * @param {string} key
 * @returns {{ [$typeArgSymbol]: true, key: string }}
 */
export function $typeArg(key) {
  return { [$typeArgSymbol]: true, key };
}

/**
 * Throws an error with the given message.
 * @param {string} msg
 */
export function panic(msg) {
  throw new Error(msg);
}

/**
 * Get the type/constructor of a value
 * @param {unknown} value
 * @return {{ new(): any } | null | undefined}
 */
function $getType(value) {
  return value?.constructor;
}

/**
 * Returns a string representation of the type of the given value, with special
 * cases for arrays (returning 'List') and null/undefined (returning
 * 'null'/'undefined').
 * @param {unknown} value
 * @return {string}
 */
export function $getTypeName(value) {
  return $typeName($getType(value));
}

/**
 * Returns a string representation of the given type, with special cases for
 * arrays and null/undefined.
 * @param {{ new(): any } | null} Type
 * @return {string}
 */
function $typeName(Type) {
  if (Type == null) return typeof Type;
  if (Type === Array) return 'List';
  return Type.name;
}

export class $Protocol {
  /** @property {object} */
  #def;

  /** @property {Map<string, object>} */
  #impls = new Map();

  /** @property {$Protocol[]} */
  #constraints;

  /** @type {{ methods: object, constraints: $Protocol[] } | null} */
  #blanketImpl = null;

  /**
   * Creates a new protocol instance.
   * @param {object} def
   * @param {$Procotol[]}
   */
  constructor(def, constraints = []) {
    this.#def = def;
    this.#constraints = constraints;

    for (let methodName in def) {
      this[methodName] = this.#apply(methodName);
    }
  }

  /**
   * Sets the implementation for a given type or a blanket implementation if Type is null.
   * @param {Function | null} Type - The constructor function of the type or null for a blanket implementation.
   * @param {object} methods - The methods to implement for the given type.
   * @param {string | null} [typeArgs] - Optional type arguments for generic protocol dispatch.
   * @param {Array<$Protocol>} [constraints] - Optional constraints for the implementation.
   */
  $setImpl(Type, methods, typeArgs = null, constraints = []) {
    if (Type === null) {
      // Blanket impl — store separately with its constraint protocols.
      this.#blanketImpl = { methods, constraints };
      return;
    }
    const key = typeArgs ? `${$typeName(Type)}::${typeArgs}` : Type;
    this.#impls.set(key, methods);
  }

  /**
   * @param {unknown} value
   * @return {boolean}
   */
  $implements(value) {
    const Type = $getType(value);

    if (this.#impls.has(Type)) return true;

    // Check blanket impl: if all constraint protocols are satisfied, this
    // protocol is considered implemented.
    if (this.#blanketImpl != null) {
      return this.#blanketImpl.constraints.every((c) => c.$implements(value));
    }

    return false;
  }

  $getConstraints() {
    return this.#constraints;
  }

  #apply(methodName) {
    return (self, ...args) => this.#call(methodName, self, ...args);
  }

  #call(methodName, self, ...args) {
    const Type = $getType(self);

    // If the last argument is a type-arg sentinel (tagged with $typeArgSymbol
    // by the compiler for generic protocol dispatch like `Into<i64>`), try a
    // type-parameterized lookup first.
    let impl;
    const lastArg = args[args.length - 1];
    if (
      lastArg != null &&
      lastArg[$typeArgSymbol] === true &&
      args.length > 0
    ) {
      const typeArgKey = `${$typeName(Type)}::${lastArg.key}`;
      impl = this.#impls.get(typeArgKey);
      if (impl) {
        // Remove the type-arg sentinel from the argument list before calling the method.
        args = args.slice(0, -1);
      }
    }

    // Concrete impl takes priority (specificity: concrete > blanket > default).
    if (!impl) {
      impl = this.#impls.get(Type);
    }

    // Blanket impl fallback: use if all constraint protocols are satisfied.
    if (!impl && this.#blanketImpl != null) {
      const blanket = this.#blanketImpl;
      if (blanket.constraints.every((c) => c.$implements(self))) {
        impl = blanket.methods;
      }
    }

    // Final fallback: default methods from the protocol definition.
    if (!impl) {
      impl = this.#def;
    }

    const method = impl[methodName] ?? this.#def[methodName];

    return method.call(impl, self, ...args);
  }
}

/**
 * @overload
 * @param {object} def - THe protocol definition.
 * @return {$Protocol} The created protocol instance.
 *
 * @overload
 * @param {...$Protocol} constraints - Optional constraints for the protocol.
 * @param {object} def - The protocol definition.
 * @return {$Protocol} The created protocol instance.
 *
 * Creates a new protocol instance.
 * @param {...$Protocol} constraints - Constraints for the protocol.
 * @param {object} def - The protocol definition.
 * @return {$Protocol} The created protocol instance.
 */
export function $protocol(...constraintsAndDef) {
  const def = constraintsAndDef.pop();
  return new $Protocol(def, constraintsAndDef);
}

/**
 * @param {$Protocol} protocol - The protocol to implement.
 * @param {Function | null} Type - The constructor function of the type or null for a blanket implementation.
 * @param {object} methods - The methods to implement for the given type.
 * @param {string | null} [typeArgs] - Optional type arguments for generic protocol dispatch.
 * @param {Array<$Protocol>} [constraints] - Optional constraints for the implementation.
 */
export function $impl(protocol, Type, methods, typeArgs, constraints) {
  protocol.$setImpl(Type, methods, typeArgs, constraints);
}

/**
 * Installs the given method on the prototype of a type, dispatching to the
 * appropriate protocol implementation at runtime.
 *
 * @param {object} proto - The prototype object to install the method on (e.g. `String.prototype`).
 * @param {string} methodName - The name of the method to install.
 * @param {Function} dispatch - The dispatch function to call when the method is invoked.
 * @return {void}
 */
export function $installMethod(proto, methodName, dispatch) {
  $assert(
    !Reflect.has(proto, methodName),
    () =>
      `Method collision: '${methodName}' already defined on ${$getTypeName(proto)}`,
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

/**
 * @param {string | () => string} msg - The error message to throw when called.
 */
export function $unreachable(msg = 'Reached unreachable code') {
  $assert(false, msg);
}

/**
 * @param {unknown} value - The value to get the enum tag name for.
 * @return {string | null}
 */
function $enumTagName(value) {
  if (value == null || typeof value !== 'object') return null;
  const tag = value.tag;
  if (tag == null) return null;
  if (typeof tag === 'function') return tag.name;
  // For singleton enum variants: find the matching static property on the constructor
  const ctor = $getType(value);
  if (ctor != null) {
    for (const key of Object.keys(ctor)) {
      if (ctor[key] === tag) return key;
    }
  }
  return null;
}

/**
 * Throws a descriptive error for a non-exhaustive pattern match.
 * @param {unknown} [value] - The unmatched value.
 * @return {never}
 */
export function $matchError(value) {
  const typeName = $getTypeName(value);
  const tagName = $enumTagName(value);
  const desc =
    typeName && tagName
      ? `${typeName}::${tagName}`
      : (tagName ?? typeName ?? $getTypeName(value));
  $unreachable(`Non-exhaustive pattern match: unmatched value of type ${desc}`);
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
