enum Option {
    Some(value),
    None,
}

// filter_map(a[], fn(a) -> Option(b)) -> b[]
fn filter_map([], _) { [] }
fn filter_map([x, ...xs], f) if let Option::Some(y) = f(x) { [y, ...filter_map(xs, f)] }
fn filter_map([_, ...xs], f) { rec filter_map(xs, f) }

[1,2,3,4,5]
|> filter_map(fn(x) { if x % 2 == 0 { Option::Some(x ** 2) } else { Option::None } })
|> log();
