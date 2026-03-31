use crate::define_keywords;

define_keywords! {
    Let => "let",
    Fn => "fn",
    Return => "return",
    Rec => "rec",
    If => "if",
    Else => "else",
    Match => "match",
    Enum => "enum",
    Struct => "struct",
    Not => "not",
    And => "and",
    Or => "or",
    As => "as",
    For => "for",
    In => "in",
    Loop => "loop",
    Break => "break",
    Continue => "continue",
    With => "with",

    // TODO: Similar to `self`, we might want to treat this as an identifier.
    Underscore => "_",
    // TODO: It might actually be better to just treat this as an identifier.
    _Self => "self",

    Protocol => "protocol",
    Impl => "impl",
    Apply => "apply",

    Pub => "pub",
    Mod => "mod",
    Use => "use",

    Const => "const",

    // Reserved keywords, unused at the moment
    SelfUpper => "Self",
    While => "while"
}
