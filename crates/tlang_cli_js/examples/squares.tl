// map(a[], fn(a) -> b) -> b[]
fn map([], f) { [] }
fn map([x, ...xs], f) { [f(x), ...map(xs, f)] }

fn square(x) { x ** 2 }

[1, 2, 3, 4, 5]
|> map(square)
|> log();