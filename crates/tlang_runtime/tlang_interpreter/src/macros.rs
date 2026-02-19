/// Propagate control flow (`Return`, `TailCall`, etc.), otherwise extract the value.
#[macro_export]
macro_rules! eval_value {
    ($this:expr, $expr:expr) => {
        match $expr {
            EvalResult::Value(val) => {
                $this.state.push_temp_root(val);
                val
            }
            other => return other,
        }
    };
}

/// Propagate control flow (`Return`, `TailCall`, etc.), otherwise extract the value.
#[macro_export]
macro_rules! eval_match_value {
    ($expr:expr) => {
        match $expr {
            EvalResult::Value(val) => val,
            other => return MatchResult::NotMatched(other),
        }
    };
}

/// Propagate control flow if it's not `Value`
#[macro_export]
macro_rules! propagate {
    ($expr:expr) => {
        match $expr {
            EvalResult::Value(_) | EvalResult::Void => {}
            other => return other,
        }
    };
}

/// Evaluate a list of hir:Expr expressions into a vector of values, propagating control flow if necessary.
#[macro_export]
macro_rules! eval_exprs {
    ($this:expr, $eval:expr, $exprs:expr) => {{ eval_exprs!($this, $eval, $exprs, $exprs.len()) }};
    ($this:expr, $eval:expr, $exprs:expr, $capacity:expr) => {{
        let mut exprs = SmallVec::<[TlangValue; 4]>::with_capacity($capacity);
        for expr in &$exprs {
            exprs.push(eval_value!($this, $eval($this, expr)));
        }
        exprs
    }};
}
