// factorial(int) -> int
fn factorial(n) { factorial(n, 1) }
// factorial(0, int) -> int
fn factorial(0, acc) { acc }
// factorial(int, int) -> int
fn factorial(n, acc) { rec factorial(n - 1, n * acc) }

factorial(5)
|> log();
