macro_rules! advance_until {
    ($parser:ident, $pattern:pat) => {
        while !$parser.at_eof() {
            if matches!($parser.current_token_kind(), $pattern) {
                break;
            }
            $parser.advance();
        }
    };
}

macro_rules! expect_token_matches {
    ($parser:ident, $pattern:pat $(if $guard:expr)? $(,)?) => {{
        use crate::macros::advance_until;

        match $parser.current_token_kind() {
            $pattern $(if $guard)? => (),
            _ => {
                if !$parser.recoverable() {
                    $parser.panic_unexpected_token(&format!("{:?}", stringify!($pattern)), $parser.current_token.clone());
                }

                $parser.push_unexpected_token_error(
                    &format!("{:?}", stringify!($pattern)),
                    $parser.current_token.clone(),
                );
                advance_until!($parser, $pattern);
            }
        }
    }};
    ($parser:ident, $message:expr, $pattern:pat $(if $guard:expr)? $(,)?) => {{
        use crate::macros::advance_until;

        match $parser.current_token_kind() {
            $pattern $(if $guard)? => (),
            _ => {
                if !$parser.recoverable() {
                    $parser.panic_unexpected_token($message, $parser.current_token.clone());
                }

                $parser.push_unexpected_token_error(
                    $message,
                    $parser.current_token.clone(),
                );
                advance_until!($parser, $pattern);
            }
        }
    }};
}

pub(crate) use advance_until;
pub(crate) use expect_token_matches;
