fn sum([]) { 0 }
fn sum([x, ...xs]) { x + sum(xs) }
