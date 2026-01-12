// reverse_string(string) -> string
fn reverse_string(str) { reverse_string(str, "") }
// reverse_string(string, string) -> string
fn reverse_string("", acc) { acc }
fn reverse_string([x, ...xs], acc) { rec reverse_string(xs, x + acc) }

// is_palindrome(string) -> bool
fn is_palindrome(str) { str == reverse_string(str) }

"racecar"
|> is_palindrome()
|> log();

"hello"
|> is_palindrome()
|> log();
