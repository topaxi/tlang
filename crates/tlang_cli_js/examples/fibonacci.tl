// fibonacci(int) -> int
fn fibonacci(n) { fibonacci(n, 0, 1) }
// fibonacci(0, int, int) -> int
fn fibonacci(0, a, _) { a }
// fibonacci(1, int, int) -> int
fn fibonacci(1, _, b) { b }
// fibonacci(int, int, int) -> int
fn fibonacci(n, a, b) { rec fibonacci(n - 1, b, a + b) }

fibonacci(50)
|> log();
