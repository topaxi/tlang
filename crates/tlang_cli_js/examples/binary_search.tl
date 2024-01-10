// len(a[]) -> int
fn len(list) { list.length }

// binary_search(a[], a) -> int
fn binary_search(list, target) { binary_search(list, target, 0, len(list) - 1) }
// binary_search(a[], a, int, int) -> int
fn binary_search(_, _, low, high) if low > high; { -1 }
fn binary_search(list, target, low, high) {
    let mid = floor((low + high) / 2);
    let midValue = list[mid];

    if midValue == target; {
        mid
    } else if midValue < target; {
        binary_search(list, target, mid + 1, high)
    } else {
        binary_search(list, target, low, mid - 1)
    }
}

[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
|> binary_search(7)
|> log();
