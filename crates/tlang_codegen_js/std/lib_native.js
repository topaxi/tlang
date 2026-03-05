/* eslint-disable @typescript-eslint/no-unused-vars */
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
