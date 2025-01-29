enum Expr {
    Value(int),
    Add(Expr, Expr),
    Subtract(Expr, Expr),
    Multiply(Expr, Expr),
    Divide(Expr, Expr),
}

// evaluate(Expr) -> int
fn evaluate(Expr::Value(val)) { val }
fn evaluate(Expr::Add(left, right)) { evaluate(left) + evaluate(right) }
fn evaluate(Expr::Subtract(left, right)) { evaluate(left) - evaluate(right) }
fn evaluate(Expr::Multiply(left, right)) { evaluate(left) * evaluate(right) }
fn evaluate(Expr::Divide(left, right)) { evaluate(left) / evaluate(right) }

// Example Usage
let expr = Expr::Add(
    Expr::Multiply(
        Expr::Value(2),
        Expr::Value(3)
    ),
    Expr::Subtract(
        Expr::Value(10),
        Expr::Value(4)
    )
);

expr
|> evaluate()
|> log();

