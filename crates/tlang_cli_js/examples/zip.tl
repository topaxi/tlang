// zip(a[], b[]) -> [a, b][]
fn zip([], []) { [] }
fn zip([x, ...xs], [y, ...ys]) { [[x, y], ...zip(xs, ys)] }

let list1 = [1, 2, 3];
let list2 = ["a", "b", "c"];

list1
|> zip(list2)
|> log();