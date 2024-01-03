fn foldl([], acc, _) { acc }
fn foldl([x, ...xs], acc, f) { rec foldl(xs, f(acc, x), f) }

fn add(x, y) { x + y }
fn multiply(x, y) { x * y }

let numbers = [1, 2, 3, 4, 5];
let sum = numbers |> foldl(0, add);
let product = numbers |> foldl(1, multiply);

sum
|> log();

product
|> log();
