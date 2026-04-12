//! Static registry of method signatures for builtin types.
//!
//! This gives the type checker knowledge of the methods available on native
//! types (Regex, Option, Result, StringBuf, List) and their return types,
//! without depending on the runtime (`tlang_memory`).
//!
//! ## Builtin type methods (tlang syntax)
//!
//! ```tlang
//! struct Regex { source: String, flags: String }
//! impl Regex {
//!     fn test(self, haystack: String) -> bool { ... }
//!     fn exec(self, haystack: String) -> Option<String> { ... }
//!     fn replace_all(self, haystack: String, replacement: String) -> String { ... }
//!     fn replace_first(self, haystack: String, replacement: String) -> String { ... }
//!     fn flags(self, new_flags: String) -> Regex { ... }
//! }
//!
//! struct StringBuf {}
//! impl StringBuf {
//!     fn push(self, s: String) -> StringBuf { ... }
//!     fn push_char(self, c: String) -> StringBuf { ... }
//!     fn clear(self) -> StringBuf { ... }
//!     fn to_string(self) -> String { ... }
//!     fn len(self) -> i64 { ... }
//!     fn is_empty(self) -> bool { ... }
//! }
//!
//! enum Option<T> { Some(T), None }
//! impl Option {
//!     fn is_some(self) -> bool { ... }
//!     fn is_none(self) -> bool { ... }
//!     fn unwrap(self) -> T { ... }
//!     fn map(self, f: fn(T) -> U) -> Option<U> { ... }
//! }
//!
//! enum Result<T, E> { Ok(T), Err(E) }
//! impl Result {
//!     fn is_ok(self) -> bool { ... }
//!     fn is_err(self) -> bool { ... }
//!     fn unwrap(self) -> T { ... }
//!     fn map(self, f: fn(T) -> U) -> Result<U, E> { ... }
//! }
//!
//! impl List {
//!     fn slice(self, start: i64, end: i64) -> List { ... }
//! }
//! ```

use tlang_hir::{PrimTy, Ty, TyKind};

use crate::builtin_types;

/// A builtin method signature descriptor.
struct BuiltinMethodDef {
    type_name: &'static str,
    method_name: &'static str,
    /// Parameter types (including `self`).
    params: &'static [TyKind],
    ret: TyKind,
    /// If set, the return type is resolved at lookup time via
    /// `builtin_types::lookup`.
    ret_builtin_type: Option<&'static str>,
}

/// Authoritative registry of builtin method type signatures.
///
/// Kept in sync with `crates/tlang_core/tlang_stdlib/src/`.
static BUILTIN_METHODS: &[BuiltinMethodDef] = &[
    // ── Regex methods ───────────────────────────────────────────────
    // fn Regex.test(self, haystack: String) -> bool
    BuiltinMethodDef {
        type_name: "Regex",
        method_name: "test",
        params: &[TyKind::Unknown, TyKind::Primitive(PrimTy::String)],
        ret: TyKind::Primitive(PrimTy::Bool),
        ret_builtin_type: None,
    },
    // fn Regex.exec(self, haystack: String) -> Option<String>
    BuiltinMethodDef {
        type_name: "Regex",
        method_name: "exec",
        params: &[TyKind::Unknown, TyKind::Primitive(PrimTy::String)],
        ret: TyKind::Unknown,
        ret_builtin_type: Some("Option"),
    },
    // fn Regex.replace_all(self, haystack: String, replacement: String) -> String
    BuiltinMethodDef {
        type_name: "Regex",
        method_name: "replace_all",
        params: &[
            TyKind::Unknown,
            TyKind::Primitive(PrimTy::String),
            TyKind::Primitive(PrimTy::String),
        ],
        ret: TyKind::Primitive(PrimTy::String),
        ret_builtin_type: None,
    },
    // fn Regex.replace_first(self, haystack: String, replacement: String) -> String
    BuiltinMethodDef {
        type_name: "Regex",
        method_name: "replace_first",
        params: &[
            TyKind::Unknown,
            TyKind::Primitive(PrimTy::String),
            TyKind::Primitive(PrimTy::String),
        ],
        ret: TyKind::Primitive(PrimTy::String),
        ret_builtin_type: None,
    },
    // fn Regex.flags(self, new_flags: String) -> Regex
    BuiltinMethodDef {
        type_name: "Regex",
        method_name: "flags",
        params: &[TyKind::Unknown, TyKind::Primitive(PrimTy::String)],
        ret: TyKind::Unknown,
        ret_builtin_type: Some("Regex"),
    },
    // ── StringBuf methods ───────────────────────────────────────────
    // fn StringBuf.push(self, s: String) -> StringBuf
    BuiltinMethodDef {
        type_name: "StringBuf",
        method_name: "push",
        params: &[TyKind::Unknown, TyKind::Primitive(PrimTy::String)],
        ret: TyKind::Unknown,
        ret_builtin_type: Some("StringBuf"),
    },
    // fn StringBuf.push_char(self, c: String) -> StringBuf
    BuiltinMethodDef {
        type_name: "StringBuf",
        method_name: "push_char",
        params: &[TyKind::Unknown, TyKind::Primitive(PrimTy::String)],
        ret: TyKind::Unknown,
        ret_builtin_type: Some("StringBuf"),
    },
    // fn StringBuf.clear(self) -> StringBuf
    BuiltinMethodDef {
        type_name: "StringBuf",
        method_name: "clear",
        params: &[TyKind::Unknown],
        ret: TyKind::Unknown,
        ret_builtin_type: Some("StringBuf"),
    },
    // fn StringBuf.to_string(self) -> String
    BuiltinMethodDef {
        type_name: "StringBuf",
        method_name: "to_string",
        params: &[TyKind::Unknown],
        ret: TyKind::Primitive(PrimTy::String),
        ret_builtin_type: None,
    },
    // fn StringBuf.len(self) -> i64
    BuiltinMethodDef {
        type_name: "StringBuf",
        method_name: "len",
        params: &[TyKind::Unknown],
        ret: TyKind::Primitive(PrimTy::I64),
        ret_builtin_type: None,
    },
    // fn StringBuf.is_empty(self) -> bool
    BuiltinMethodDef {
        type_name: "StringBuf",
        method_name: "is_empty",
        params: &[TyKind::Unknown],
        ret: TyKind::Primitive(PrimTy::Bool),
        ret_builtin_type: None,
    },
    // ── Option methods ──────────────────────────────────────────────
    // fn Option.is_some(self) -> bool
    BuiltinMethodDef {
        type_name: "Option",
        method_name: "is_some",
        params: &[TyKind::Unknown],
        ret: TyKind::Primitive(PrimTy::Bool),
        ret_builtin_type: None,
    },
    // fn Option.is_none(self) -> bool
    BuiltinMethodDef {
        type_name: "Option",
        method_name: "is_none",
        params: &[TyKind::Unknown],
        ret: TyKind::Primitive(PrimTy::Bool),
        ret_builtin_type: None,
    },
    // fn Option.unwrap(self) -> T
    BuiltinMethodDef {
        type_name: "Option",
        method_name: "unwrap",
        params: &[TyKind::Unknown],
        ret: TyKind::Unknown,
        ret_builtin_type: None,
    },
    // fn Option.map(self, f) -> Option
    BuiltinMethodDef {
        type_name: "Option",
        method_name: "map",
        params: &[TyKind::Unknown, TyKind::Unknown],
        ret: TyKind::Unknown,
        ret_builtin_type: Some("Option"),
    },
    // ── Result methods ──────────────────────────────────────────────
    // fn Result.is_ok(self) -> bool
    BuiltinMethodDef {
        type_name: "Result",
        method_name: "is_ok",
        params: &[TyKind::Unknown],
        ret: TyKind::Primitive(PrimTy::Bool),
        ret_builtin_type: None,
    },
    // fn Result.is_err(self) -> bool
    BuiltinMethodDef {
        type_name: "Result",
        method_name: "is_err",
        params: &[TyKind::Unknown],
        ret: TyKind::Primitive(PrimTy::Bool),
        ret_builtin_type: None,
    },
    // fn Result.unwrap(self) -> T
    BuiltinMethodDef {
        type_name: "Result",
        method_name: "unwrap",
        params: &[TyKind::Unknown],
        ret: TyKind::Unknown,
        ret_builtin_type: None,
    },
    // fn Result.map(self, f) -> Result
    BuiltinMethodDef {
        type_name: "Result",
        method_name: "map",
        params: &[TyKind::Unknown, TyKind::Unknown],
        ret: TyKind::Unknown,
        ret_builtin_type: Some("Result"),
    },
    // ── List methods ────────────────────────────────────────────────
    // fn List.slice(self, start: i64, end: i64) -> List
    BuiltinMethodDef {
        type_name: "List",
        method_name: "slice",
        params: &[
            TyKind::Unknown,
            TyKind::Primitive(PrimTy::I64),
            TyKind::Primitive(PrimTy::I64),
        ],
        ret: TyKind::Unknown,
        ret_builtin_type: Some("List"),
    },
];

/// Look up the type signature for a method on a builtin type.
///
/// Returns `TyKind::Fn(params, ret)` if found, `None` otherwise.
/// The `self` parameter is excluded from the returned Fn params since
/// method calls pass `self` implicitly.
pub fn lookup(type_name: &str, method_name: &str) -> Option<TyKind> {
    BUILTIN_METHODS
        .iter()
        .find(|m| m.type_name == type_name && m.method_name == method_name)
        .map(|m| {
            let ret_kind = match m.ret_builtin_type {
                Some(name) => builtin_types::lookup(name).unwrap_or_else(|| m.ret.clone()),
                None => m.ret.clone(),
            };
            // Skip the first param (self) — method calls pass self implicitly.
            let params: Vec<Ty> = m
                .params
                .iter()
                .skip(1)
                .map(|k| Ty {
                    kind: k.clone(),
                    ..Ty::default()
                })
                .collect();
            let ret_ty = Ty {
                kind: ret_kind,
                ..Ty::default()
            };
            TyKind::Fn(params, Box::new(ret_ty))
        })
}

/// Extract the type name from a `TyKind` for method lookup purposes.
pub fn type_name_from_kind(kind: &TyKind) -> Option<&str> {
    match kind {
        TyKind::Primitive(PrimTy::String) => Some("String"),
        TyKind::Primitive(PrimTy::Bool) => Some("bool"),
        TyKind::Primitive(PrimTy::I64) => Some("i64"),
        TyKind::Path(path) => {
            let name = path.segments.last().map(|s| s.ident.as_str())?;
            Some(name)
        }
        TyKind::Slice(_) => Some("List"),
        _ => None,
    }
}
