fn fibonacci(n) { fibonacci(n, 0, 1) }
fn fibonacci(0, a, _) { a }
fn fibonacci(1, _, b) { b }
fn fibonacci(n, a, b) { rec fibonacci(n - 1, b, a + b) }

fibonacci(50)
|> log();
