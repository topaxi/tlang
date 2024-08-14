// reverse_string(string) -> string
fn reverse_string("") { "" }
fn reverse_string([x, ...xs]) { rec reverse_string(xs) + x }

// is_palindrome(string) -> bool
fn is_palindrome(str) { str == reverse_string(str) }

"racecar"
|> is_palindrome()
|> log();

"hello"
|> is_palindrome()
|> log();
