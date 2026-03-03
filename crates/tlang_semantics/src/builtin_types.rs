/// User-facing primitive and builtin type names for tlang.
///
/// These mirror Rust-style naming conventions. `nil` is intentionally absent
/// as it is an internal runtime-only concept not exposed to user code.
pub const BOOL: &str = "bool";

pub const I8: &str = "i8";
pub const I16: &str = "i16";
pub const I32: &str = "i32";
pub const I64: &str = "i64";
pub const ISIZE: &str = "isize";

pub const U8: &str = "u8";
pub const U16: &str = "u16";
pub const U32: &str = "u32";
pub const U64: &str = "u64";
pub const USIZE: &str = "usize";

pub const F32: &str = "f32";
pub const F64: &str = "f64";

pub const CHAR: &str = "char";

/// The single heap-allocated string type, maps to `TlangObjectKind::String`.
pub const STRING: &str = "String";

/// The sliceable sequence type. Used when a `PatKind::List` pattern (`[]` or
/// `[x, ...xs]`) is matched, since the interpreter handles list patterns on
/// both `TlangObjectKind::Slice` (list slices) and `TlangObjectKind::String`
/// (strings).
pub const SLICE: &str = "Slice";

/// All user-facing builtin type names, for validation or registry lookup.
pub const ALL: &[&str] = &[
    BOOL, I8, I16, I32, I64, ISIZE, U8, U16, U32, U64, USIZE, F32, F64, CHAR, STRING, SLICE,
];
