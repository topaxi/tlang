#[macro_export]
macro_rules! expr {
    ($id:expr, $kind:ident) => {{
        use tlang_ast::node::{Expr, ExprKind};

        Expr::new($id, ExprKind::$kind)
    }};

    ($id:expr, $kind:ident($($arg:expr),* $(,)?)) => {{
        use tlang_ast::node::{Expr, ExprKind};

        Expr::new($id, ExprKind::$kind($($arg),*))
    }};
}

#[macro_export]
macro_rules! pat {
    ($id:expr, $kind:ident) => {{
        use tlang_ast::node::{Pat, PatKind};

        Pat::new($id, PatKind::$kind)
    }};

    ($id:expr, $kind:ident($($arg:expr),* $(,)?)) => {{
        use tlang_ast::node::{Pat, PatKind};

        Pat::new($id, PatKind::$kind($($arg),*))
    }};
}

#[macro_export]
macro_rules! stmt {
    ($id:expr, $kind:ident) => {{
        use tlang_ast::node::{Stmt, StmtKind};
        Stmt::new($id, StmtKind::$kind)
    }};

    ($id:expr, $kind:ident($($arg:expr),* $(,)?)) => {{
        use tlang_ast::node::{Stmt, StmtKind};
        Stmt::new($id, StmtKind::$kind($($arg),*))
    }};
}

#[macro_export]
macro_rules! define_keywords {
    ($($keyword:ident => $str_value:expr),*) => {
        #[derive(Copy, Clone, Debug, PartialEq, Serialize)]
        pub enum Keyword {
            $($keyword),*
        }

        impl From<&str> for Keyword {
            fn from(s: &str) -> Self {
                match s {
                    $($str_value => Keyword::$keyword,)*
                    _ => panic!("Unknown keyword: {}", s),
                }
            }
        }

        pub const KEYWORDS: &[&str] = &[$($str_value),*];

        pub fn is_keyword(s: &str) -> bool {
            KEYWORDS.contains(&s)
        }

        #[allow(non_upper_case_globals)]
        pub mod kw {
            $(pub const $keyword: &str = $str_value;)*
        }
    };
}

pub use expr;
pub use pat;
pub use stmt;
