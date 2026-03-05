function panic(msg) { throw new Error(msg); }
Functor.List = {};
Functor.List.map = function(self, f) {
    const result = new Array(self.length);
    for (let i = 0; i < self.length; i++) result[i] = f(self[i]);
    return result;
};
