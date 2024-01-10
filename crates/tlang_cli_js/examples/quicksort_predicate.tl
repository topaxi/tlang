// len(a[]) -> int
fn len(list) { list.length }

// random_int(int) -> int
fn random_int(max) { floor(random() * max) }

// partition(a[], fn(a) -> bool) -> (a[], a[])
fn partition([], _) { [[], []] }
fn partition(list, predicate) { partition(list, predicate, [], []) }
// partition(a[], fn(a) -> bool, a[], a[]) -> (a[], a[])
fn partition([], _, satisfies, doesNotSatisfy) { [satisfies, doesNotSatisfy] }
fn partition([x, ...xs], predicate, satisfies, doesNotSatisfy) if predicate(x) {
  rec partition(xs, predicate, [...satisfies, x], doesNotSatisfy)
}
fn partition([x, ...xs], predicate, satisfies, doesNotSatisfy) {
  rec partition(xs, predicate, satisfies, [...doesNotSatisfy, x])
}

// quicksort(a[], fn(a, a) -> int) -> a[]
fn quicksort([], _) { [] }
fn quicksort(list, predicate) {
  let pivotIndex = random_int(len(list));
  let pivot = list[pivotIndex];
  let list = [...list.slice(0, pivotIndex), ...list.slice(pivotIndex+1)];

  let partitioned = list |> partition(fn(x) { predicate(x, pivot) < 0 });

  let smaller = partitioned[0] |> quicksort(predicate);
  let greater = partitioned[1] |> quicksort(predicate);

  [...smaller, pivot, ...greater]
}

[3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5]
|> quicksort(fn(a, b) { b - a })
|> log();
