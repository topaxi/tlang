#[macro_export]
macro_rules! expr {
    ($kind:ident) => {{
        use tlang_ast::node::{Expr, ExprKind};

        Expr::new(ExprKind::$kind)
    }};

    ($kind:ident($($arg:expr),* $(,)?)) => {{
        use tlang_ast::node::{Expr, ExprKind};

        Expr::new(ExprKind::$kind($($arg),*))
    }};
}

#[macro_export]
macro_rules! pat {
    ($kind:ident) => {{
        use tlang_ast::node::{Pattern, PatternKind};

        Pattern::new(PatternKind::$kind)
    }};

    ($kind:ident($($arg:expr),* $(,)?)) => {{
        use tlang_ast::node::{Pattern, PatternKind};

        Pattern::new(PatternKind::$kind($($arg),*))
    }};
}

#[macro_export]
macro_rules! stmt {
    ($kind:ident) => {{
        use tlang_ast::node::{Stmt, StmtKind};
        Stmt::new(StmtKind::$kind)
    }};

    ($kind:ident($($arg:expr),* $(,)?)) => {{
        use tlang_ast::node::{Stmt, StmtKind};
        Stmt::new(StmtKind::$kind($($arg),*))
    }};
}

pub use expr;
pub use pat;
pub use stmt;
