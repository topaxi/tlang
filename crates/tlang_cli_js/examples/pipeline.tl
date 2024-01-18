// map(a[], fn(a) -> b) -> b[]
fn map([], _) { [] }
fn map([x, ...xs], f) { [f(x), ...map(xs, f)] }

// filter(a[], fn(a) -> bool) -> a[]
fn filter([], _) { [] }
fn filter([x, ...xs], f) if f(x) { [x, ...filter(xs, f)] }
fn filter([_, ...xs], f) { rec filter(xs, f) }

// foldl(a[], b, fn(b, a) -> b) -> b
fn foldl([], acc, _) { acc }
fn foldl([x, ...xs], acc, f) { rec foldl(xs, f(acc, x), f) }

[1,2,3,4,5]
|> map(fn (x) { x ** 2 })
|> filter(fn (x) { x % 2 == 0 })
|> foldl(0, fn (acc, x) { acc + x })
|> log();
