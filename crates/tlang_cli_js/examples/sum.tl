fn sum([]) { 0 }
fn sum([x, ...xs]) { x + sum(xs) }

log(sum([1, 2, 3, 4, 5]));
