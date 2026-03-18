/* eslint-disable @typescript-eslint/no-unused-vars */
function panic(msg) {
  throw new Error(msg);
}

function $installMethod(proto, methodName, dispatch) {
  $assert(
    !Reflect.has(proto, methodName),
    `Method collision: '${methodName}' already defined on ${proto.constructor?.name ?? typeof proto}`,
  );

  proto[methodName] = function (...args) {
    return dispatch(this, ...args);
  };
}

class $AssertError extends Error {}

/**
 * @param {boolean} cond - The condition to assert.param
 * @param {string} msg - The error message to throw if the assertion fails.
 * @return {asserts cond}
 */
function $assert(cond, msg) {
  if (!cond) throw new $AssertError(msg);
}
