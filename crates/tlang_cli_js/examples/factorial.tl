// factorial(int) -> int
// factorial(int, int) -> int
fn factorial(n) { factorial(n, 1) }
fn factorial(0, acc) { acc }
fn factorial(n, acc) { rec factorial(n - 1, n * acc) }

factorial(5)
|> log();
