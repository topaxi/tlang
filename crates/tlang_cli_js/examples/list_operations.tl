enum Option {
    Some(value),
    None,
}

// reverse(a[]) -> a[]
fn reverse(list) { reverse(list, []) }
// reverse(a[], a[]) -> a[]
fn reverse([], acc) { acc }
fn reverse([x, ...xs], acc) { rec reverse(xs, [x, ...acc]) }

// reduce(a[], fn(b, a) -> b, b) -> b
fn reduce([], _, acc) { acc }
fn reduce([x, ...xs], f, acc) { rec reduce(xs, f, f(acc, x)) }

// find(a[], fn(a) -> bool) -> Option(a)
fn find(list, predicate) { find(list, predicate, Option::None) }
// find(a[], fn(a) -> bool, Option(a)) -> Option(a)
fn find([], _, acc) { acc }
fn find([x, ...xs], predicate, _) if predicate(x) { Option::Some(x) }
fn find([_, ...xs], predicate, acc) { rec find(xs, predicate, acc) }

// all(a[], fn(a) -> bool) -> bool
fn all(list, predicate) { all(list, predicate, true) }
// all(a[], fn(a) -> bool, bool) -> bool
fn all([], _, acc) { acc }
fn all([x, ...xs], predicate, _) if not predicate(x) { false }
fn all([_, ...xs], predicate, acc) { rec all(xs, predicate, acc) }

// any(a[], fn(a) -> bool) -> bool
fn any(list, predicate) { any(list, predicate, false) }
// any(a[], fn(a) -> bool, bool) -> bool
fn any([], _, acc) { acc }
fn any([x, ...xs], predicate, _) if predicate(x) { true }
fn any([_, ...xs], predicate, acc) { rec any(xs, predicate, acc) }

// flatten(a[][]) -> a[]
fn flatten(list) { flatten(list, []) }
// flatten(a[][], a[]) -> a[]
fn flatten([], acc) { acc }
fn flatten([x, ...xs], acc) { rec flatten(xs, [...acc, ...x]) }

// take(a[], int) -> a[]
fn take(list, n) { take(list, n, []) }
// take(a[], int, a[]) -> a[]
fn take([], _, acc) { acc }
fn take([x, ...xs], n, acc) if n > 0 { rec take(xs, n - 1, [...acc, x]) }
fn take(_, _, acc) { acc }

// drop(a[], int) -> a[]
fn drop(list, n) { drop(list, n, []) }
// drop(a[], int, a[]) -> a[]
fn drop([], _, acc) { acc }
fn drop([x, ...xs], n, acc) if n > 0 { rec drop(xs, n - 1, acc) }
fn drop([x, ...xs], _, acc) { rec drop(xs, 0, [...acc, x]) }

// Reverse the list
[1, 2, 3, 4, 5]
|> reverse()
|> log();

// Calculate the sum of the list elements
[1, 2, 3, 4, 5]
|> reduce(fn (acc, x) { acc + x }, 0)
|> log();

// Find the first element greater than 3
[1, 2, 3, 4, 5]
|> find(fn (x) { x > 3 })
|> log();

// Check if all elements are less than 6
[1, 2, 3, 4, 5]
|> all(fn (x) { x < 6 })
|> log();

// Check if any element is greater than 4
[1, 2, 3, 4, 5]
|> any(fn (x) { x > 4 })
|> log();

// Flatten the list of lists
[[1, 2], [3, 4], [5]]
|> flatten()
|> log();

// Take the first three elements
[1, 2, 3, 4, 5]
|> take(3)
|> log();

// Drop the first two elements
[1, 2, 3, 4, 5]
|> drop(2)
|> log();
