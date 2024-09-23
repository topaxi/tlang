// apply_map(fn(a) -> b, b[], a) -> b[]
fn apply_map(f, acc, x) { [ ...acc, f(x) ] }

// apply_filter(fn(a) -> bool, a[], a) -> a[]
fn apply_filter(f, acc, x) if f(x) { [ ...acc, x ] }
fn apply_filter(_, acc, _) { acc }

// map(a[], fn(a) -> b) -> b[]
fn map(xs, f) { foldl(xs, [], apply_map(f, _, _)) }

// filter(a[], fn(a) -> bool) -> a[]
fn filter(xs, f) { foldl(xs, [], apply_filter(f, _, _)) }

// foldl(a[], b, fn(b, a) -> b) -> b
fn foldl([], acc, _) { acc }
fn foldl([x, ...xs], acc, f) { rec foldl(xs, f(acc, x), f) }

[1,2,3,4,5]
|> map(fn (x) { x ** 2 })
|> filter(fn (x) { x % 2 == 0 })
|> foldl(0, fn (acc, x) { acc + x })
|> log();
