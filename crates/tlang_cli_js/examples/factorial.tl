fn factorial(n) { factorial(n, 1) }
fn factorial(0, acc) { acc }
fn factorial(n, acc) { factorial(n - 1, n * acc) }
