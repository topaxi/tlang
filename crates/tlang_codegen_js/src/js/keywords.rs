/// Macro to define each keyword as a constant and within a list of all keywords
macro_rules! define_js_keywords {
    ($($keyword:ident => $str_value:expr),*) => {
        pub const KEYWORDS: &[&str] = &[$($str_value),*];

        #[allow(non_upper_case_globals, unused)]
        pub mod kw {
            $(pub const $keyword: &str = $str_value;)*
        }
    };
}

define_js_keywords! {
    Await => "await",
    Async => "async",
    Break => "break",
    Case => "case",
    Catch => "catch",
    Class => "class",
    Const => "const",
    Continue => "continue",
    Debugger => "debugger",
    Default => "default",
    Delete => "delete",
    Do => "do",
    Else => "else",
    Enum => "enum",
    Export => "export",
    Extends => "extends",
    False => "false",
    Finally => "finally",
    For => "for",
    Function => "function",
    If => "if",
    Import => "import",
    In => "in",
    InstanceOf => "instanceof",
    New => "new",
    Null => "null",
    Return => "return",
    Super => "super",
    Switch => "switch",
    This => "this",
    Throw => "throw",
    True => "true",
    Try => "try",
    TypeOf => "typeof",
    Var => "var",
    Void => "void",
    While => "while",
    With => "with",
    Yield => "yield"
}

pub fn is_keyword(s: &str) -> bool {
    KEYWORDS.contains(&s)
}
