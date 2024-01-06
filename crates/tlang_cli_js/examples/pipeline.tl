// map(a[], fn(a) -> b) -> b[]
fn map([], f) { [] }
fn map([x, ...xs], f) { [f(x), ...map(xs, f)] }

// filter(a[], fn(a) -> bool) -> a[]
fn filter([], f) { [] }
fn filter([x, ...xs], f) {
    if f(x) {
        return [x, ...filter(xs, f)];
    } else {
        return rec filter(xs, f);
    };
}

// foldl(a[], b, fn(b, a) -> b) -> b
fn foldl([], acc, _) { acc }
fn foldl([x, ...xs], acc, f) { rec foldl(xs, f(acc, x), f) }

[1,2,3]
|> map(fn (x) { x ** 2 })
|> filter(fn (x) { x % 2 == 0 })
|> foldl(0, fn (acc, x) { acc + x })
|> log();
