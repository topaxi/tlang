fn factorial(n) { factorial(n, 1) }
fn factorial(n, acc) {
    match [n, acc] {
        [0, acc] => acc,
        [n, acc] => rec factorial(n - 1, n * acc)
    }
}

factorial(5)
|> log;
