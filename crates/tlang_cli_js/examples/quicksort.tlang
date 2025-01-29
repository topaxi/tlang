// random_int(int) -> int
fn random_int(max) { math::random() * max |> math::floor() }

// filter(a[], fn(a) -> bool) -> a[]
fn filter([], _) { [] }
fn filter([x, ...xs], f) if f(x) { [x, ...filter(xs, f)] }
fn filter([_, ...xs], f) { rec filter(xs, f) }

// quicksort(a[]) -> a[]
fn quicksort([]) { [] }
fn quicksort(list) {
  let pivotIndex = random_int(len(list));
  let pivot = list[pivotIndex];
  let list = [...list.slice(0, pivotIndex), ...list.slice(pivotIndex+1)];
  let smaller = list |> filter(fn(y) { y <= pivot });
  let greater = list |> filter(fn(y) { y > pivot });
  [...quicksort(smaller), pivot, ...quicksort(greater)]
}

[3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5]
|> quicksort()
|> log();
