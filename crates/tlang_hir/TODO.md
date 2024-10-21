# TODOs

## Fn parsing AST to IR

```
fn foo(a) { ... }  // foo_1
fn foo(a, b) { ... }  // foo_2
```

to

```
foo_1(a) { ... }
foo_2(a, b) { ... }
foo(a, b) {
    if (callee.length == 2) return foo_2(a, b)
    if (callee.length == 1) return foo_1(a)
    unreachable()
}
```

if multiple with same argument length, desugar into foo_x with a match
expression in the body.

```
fn factorial(n) { factorial(n, 1) }
fn factorial(0, acc) { acc }
fn factorial(n, acc) { rec factorial(n - 1, n * acc) }
```

to

```
fn factorial_1(n) { factorial_2(n, 1) }
fn factorial_2(n, acc) {
    match (n, acc) {
        (0, acc) => acc,
        (n, acc) => rec factortorial(n - 1, n * acc)
    }
}
```

## Call expression parsing

```
foo(1) to foo_1(a)
foo(a, b) to foo_2(a, b)
foo(a, b, c) to foo(a, b, c) // plus type error
unknownfn(a, b) to unknownfn(a, b) // plus type error / warning of missing decl
```
