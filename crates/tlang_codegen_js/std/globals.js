export function panic(msg) {
  throw new Error(msg);
}

export function $getType(value) {
  return Array.isArray(value)
    ? 'List'
    : (value?.constructor?.name ?? typeof value);
}

export function $dispatch(protocol, method) {
  return function (self, ...args) {
    const __impl = protocol[$getType(self)] ?? protocol.__default;
    return __impl[method](self, ...args);
  };
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
