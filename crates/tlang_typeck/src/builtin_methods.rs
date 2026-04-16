//! Registry of method signatures for builtin types.
//!
//! This gives the type checker knowledge of the methods available on native
//! types (Regex, Option, Result, StringBuf, List, String) and their return
//! types, without depending on the runtime (`tlang_memory`).
//!
//! Generic methods use `TyKind::Var` with well-known `TypeVarId` constants
//! in the 10_001+ range to avoid collisions with user-defined type variables.
//! The type checker's existing generic infrastructure (`collect_type_var_bindings`,
//! `substitute_type_vars`) handles instantiation automatically.
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
//!     fn flags(self) -> String { ... }
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
//!     fn map<U>(self, f: fn(T) -> U) -> U { ... }
//! }
//!
//! enum Result<T, E> { Ok(T), Err(E) }
//! impl Result {
//!     fn is_ok(self) -> bool { ... }
//!     fn is_err(self) -> bool { ... }
//!     fn unwrap(self) -> T { ... }
//!     fn map<U>(self, f: fn(T) -> U) -> U { ... }
//! }
//!
//! impl List<T> {
//!     fn slice(self, start: i64) -> List<T> { ... }
//!     fn slice(self, start: i64, end: i64) -> List<T> { ... }
//!     fn map<U>(self, f: fn(T) -> U) -> List<U> { ... }
//!     fn filter(self, f: fn(T) -> bool) -> List<T> { ... }
//!     fn foldl<U>(self, init: U, f: fn(U, T) -> U) -> U { ... }
//!     fn foldr<U>(self, init: U, f: fn(T, U) -> U) -> U { ... }
//!     fn find(self, f: fn(T) -> bool) -> T { ... }
//!     fn any(self, f: fn(T) -> bool) -> bool { ... }
//!     fn all(self, f: fn(T) -> bool) -> bool { ... }
//! }
//! ```

use tlang_hir::{PrimTy, Ty, TyKind};
use tlang_span::TypeVarId;

use crate::builtin_types;

// Well-known type variable IDs for builtin generic parameters.
// These use a high range (10_001+) to avoid collisions with user code.
const VAR_T: TypeVarId = TypeVarId::new(10_001);
const VAR_U: TypeVarId = TypeVarId::new(10_002);

fn ty(kind: TyKind) -> Ty {
    Ty {
        kind,
        ..Ty::default()
    }
}

fn fn_ty(params: Vec<TyKind>, ret: TyKind) -> TyKind {
    TyKind::Fn(params.into_iter().map(ty).collect(), Box::new(ty(ret)))
}

fn builtin_path(name: &str) -> TyKind {
    builtin_types::lookup(name).unwrap_or(TyKind::Unknown)
}

/// Look up the type signatures for a method on a builtin type.
///
/// Returns all matching `TyKind::Fn(params, ret)` overloads, or an empty `Vec`
/// if the method is unknown. The `self` parameter is excluded since
/// method calls pass `self` implicitly.
pub fn lookup_all(type_name: &str, method_name: &str) -> Vec<TyKind> {
    match type_name {
        "Regex" => lookup_regex(method_name),
        "StringBuf" => lookup_stringbuf(method_name),
        "Option" => lookup_option(method_name),
        "Result" => lookup_result(method_name),
        "List" => lookup_list(method_name),
        "String" => lookup_string(method_name),
        _ => vec![],
    }
}

/// Look up the first registered signature for a builtin method.
pub fn lookup(type_name: &str, method_name: &str) -> Option<TyKind> {
    lookup_all(type_name, method_name).into_iter().next()
}

fn lookup_regex(method: &str) -> Vec<TyKind> {
    use PrimTy::*;
    use TyKind::*;

    match method {
        "test" => vec![fn_ty(vec![Primitive(String)], Primitive(Bool))],
        "exec" => vec![fn_ty(vec![Primitive(String)], builtin_path("Option"))],
        "replace_all" => vec![fn_ty(
            vec![Primitive(String), Primitive(String)],
            Primitive(String),
        )],
        "replace_first" => vec![fn_ty(
            vec![Primitive(String), Primitive(String)],
            Primitive(String),
        )],
        "flags" => vec![
            fn_ty(vec![], Primitive(String)),
            fn_ty(vec![Primitive(String)], builtin_path("Regex")),
        ],
        _ => vec![],
    }
}

fn lookup_stringbuf(method: &str) -> Vec<TyKind> {
    use PrimTy::*;
    use TyKind::*;

    match method {
        "push" => vec![fn_ty(vec![Primitive(String)], builtin_path("StringBuf"))],
        "push_char" => vec![fn_ty(vec![Primitive(String)], builtin_path("StringBuf"))],
        "clear" => vec![fn_ty(vec![], builtin_path("StringBuf"))],
        "to_string" => vec![fn_ty(vec![], Primitive(String))],
        "len" => vec![fn_ty(vec![], Primitive(I64))],
        "is_empty" => vec![fn_ty(vec![], Primitive(Bool))],
        _ => vec![],
    }
}

fn lookup_option(method: &str) -> Vec<TyKind> {
    use PrimTy::*;
    use TyKind::*;

    let t = Var(VAR_T);
    let u = Var(VAR_U);

    match method {
        "is_some" => vec![fn_ty(vec![], Primitive(Bool))],
        "is_none" => vec![fn_ty(vec![], Primitive(Bool))],
        "unwrap" => vec![fn_ty(vec![], t)],
        // fn map<U>(self, f: fn(T) -> U) -> U
        // NOTE: Ideally returns Option<U>, but we can't express parameterised
        // paths yet.  Returning Var(U) still gives correct type propagation
        // through the closure's return type.
        "map" => vec![fn_ty(vec![fn_ty(vec![t], u.clone())], u)],
        _ => vec![],
    }
}

fn lookup_result(method: &str) -> Vec<TyKind> {
    use PrimTy::*;
    use TyKind::*;

    let t = Var(VAR_T);
    let u = Var(VAR_U);

    match method {
        "is_ok" => vec![fn_ty(vec![], Primitive(Bool))],
        "is_err" => vec![fn_ty(vec![], Primitive(Bool))],
        "unwrap" => vec![fn_ty(vec![], t)],
        // fn map<U>(self, f: fn(T) -> U) -> U
        "map" => vec![fn_ty(vec![fn_ty(vec![t], u.clone())], u)],
        _ => vec![],
    }
}

fn lookup_list(method: &str) -> Vec<TyKind> {
    use PrimTy::*;
    use TyKind::*;

    let t = Var(VAR_T);
    let u = Var(VAR_U);

    match method {
        "slice" => vec![
            fn_ty(vec![Primitive(I64)], Slice(Box::new(ty(t.clone())))),
            fn_ty(vec![Primitive(I64), Primitive(I64)], Slice(Box::new(ty(t)))),
        ],
        // fn map<U>(self, f: fn(T) -> U) -> List<U>
        "map" => vec![fn_ty(
            vec![fn_ty(vec![t], u.clone())],
            Slice(Box::new(ty(u))),
        )],
        // fn filter(self, f: fn(T) -> bool) -> List<T>
        "filter" => vec![fn_ty(
            vec![fn_ty(vec![t.clone()], Primitive(Bool))],
            Slice(Box::new(ty(t))),
        )],
        // fn foldl<U>(self, init: U, f: fn(U, T) -> U) -> U
        "foldl" => vec![fn_ty(
            vec![u.clone(), fn_ty(vec![u.clone(), t], u.clone())],
            u,
        )],
        // fn foldr<U>(self, init: U, f: fn(T, U) -> U) -> U
        "foldr" => vec![fn_ty(
            vec![u.clone(), fn_ty(vec![t, u.clone()], u.clone())],
            u,
        )],
        // fn find(self, f: fn(T) -> bool) -> T
        "find" => vec![fn_ty(vec![fn_ty(vec![t.clone()], Primitive(Bool))], t)],
        // fn any(self, f: fn(T) -> bool) -> bool
        "any" => vec![fn_ty(
            vec![fn_ty(vec![t], Primitive(Bool))],
            Primitive(Bool),
        )],
        // fn all(self, f: fn(T) -> bool) -> bool
        "all" => vec![fn_ty(
            vec![fn_ty(vec![t], Primitive(Bool))],
            Primitive(Bool),
        )],
        _ => vec![],
    }
}

fn lookup_string(method: &str) -> Vec<TyKind> {
    use PrimTy::*;
    use TyKind::*;

    match method {
        "len" => vec![fn_ty(vec![], Primitive(I64))],
        "trim" | "trim_start" | "trim_end" | "to_uppercase" | "to_lowercase" | "reverse" => {
            vec![fn_ty(vec![], Primitive(String))]
        }
        "starts_with" | "ends_with" | "contains" => {
            vec![fn_ty(vec![Primitive(String)], Primitive(Bool))]
        }
        "split" => vec![fn_ty(
            vec![Primitive(String)],
            Slice(Box::new(ty(Primitive(String)))),
        )],
        "replace" => vec![fn_ty(
            vec![Primitive(String), Primitive(String)],
            Primitive(String),
        )],
        "chars" => vec![fn_ty(vec![], Slice(Box::new(ty(Primitive(String)))))],
        "char_at" => vec![fn_ty(vec![Primitive(I64)], Primitive(String))],
        "slice" => vec![
            fn_ty(vec![Primitive(I64)], Primitive(String)),
            fn_ty(vec![Primitive(I64), Primitive(I64)], Primitive(String)),
        ],
        "repeat" => vec![fn_ty(vec![Primitive(I64)], Primitive(String))],
        "is_empty" => vec![fn_ty(vec![], Primitive(Bool))],
        "index_of" | "last_index_of" => vec![fn_ty(vec![Primitive(String)], Primitive(I64))],
        "pad_start" | "pad_end" => vec![fn_ty(
            vec![Primitive(I64), Primitive(String)],
            Primitive(String),
        )],
        _ => vec![],
    }
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

/// A builtin method descriptor for enumeration.
#[derive(Debug, Clone)]
pub struct BuiltinMethod {
    /// The method name (e.g. `"replace_all"`).
    pub name: &'static str,
    /// The method's type signature as `TyKind::Fn(params, ret)`.
    /// The `self` parameter is excluded.
    pub signature: TyKind,
}

/// The list of builtin type names that have methods.
pub const BUILTIN_TYPE_NAMES: &[&str] =
    &["Regex", "StringBuf", "Option", "Result", "List", "String"];

/// Enumerate all builtin methods available on a type.
///
/// Returns an empty `Vec` when `type_name` is not a recognised builtin type.
/// The returned signatures have `self` excluded (same convention as [`lookup`]).
pub fn methods_for(type_name: &str) -> Vec<BuiltinMethod> {
    match type_name {
        "Regex" => methods_for_regex(),
        "StringBuf" => methods_for_stringbuf(),
        "Option" => methods_for_option(),
        "Result" => methods_for_result(),
        "List" => methods_for_list(),
        "String" => methods_for_string(),
        _ => vec![],
    }
}

fn methods_for_regex() -> Vec<BuiltinMethod> {
    ["test", "exec", "replace_all", "replace_first", "flags"]
        .iter()
        .flat_map(|&name| {
            lookup_regex(name)
                .into_iter()
                .map(move |signature| BuiltinMethod { name, signature })
        })
        .collect()
}

fn methods_for_stringbuf() -> Vec<BuiltinMethod> {
    ["push", "push_char", "clear", "to_string", "len", "is_empty"]
        .iter()
        .flat_map(|&name| {
            lookup_stringbuf(name)
                .into_iter()
                .map(move |signature| BuiltinMethod { name, signature })
        })
        .collect()
}

fn methods_for_option() -> Vec<BuiltinMethod> {
    ["is_some", "is_none", "unwrap", "map"]
        .iter()
        .flat_map(|&name| {
            lookup_option(name)
                .into_iter()
                .map(move |signature| BuiltinMethod { name, signature })
        })
        .collect()
}

fn methods_for_result() -> Vec<BuiltinMethod> {
    ["is_ok", "is_err", "unwrap", "map"]
        .iter()
        .flat_map(|&name| {
            lookup_result(name)
                .into_iter()
                .map(move |signature| BuiltinMethod { name, signature })
        })
        .collect()
}

fn methods_for_list() -> Vec<BuiltinMethod> {
    [
        "slice", "map", "filter", "foldl", "foldr", "find", "any", "all",
    ]
    .iter()
    .flat_map(|&name| {
        lookup_list(name)
            .into_iter()
            .map(move |signature| BuiltinMethod { name, signature })
    })
    .collect()
}

fn methods_for_string() -> Vec<BuiltinMethod> {
    [
        "len",
        "trim",
        "trim_start",
        "trim_end",
        "to_uppercase",
        "to_lowercase",
        "reverse",
        "starts_with",
        "ends_with",
        "contains",
        "split",
        "replace",
        "chars",
        "char_at",
        "slice",
        "repeat",
        "is_empty",
        "index_of",
        "last_index_of",
        "pad_start",
        "pad_end",
    ]
    .iter()
    .flat_map(|&name| {
        lookup_string(name)
            .into_iter()
            .map(move |signature| BuiltinMethod { name, signature })
    })
    .collect()
}

/// Pre-bind type variable `T` from the receiver type into a method signature.
///
/// For `Slice(i64).map(...)`, this binds `T → i64` in the method signature
/// so `fn(fn(T) -> U) -> Slice(U)` becomes `fn(fn(i64) -> U) -> Slice(U)`.
/// This enables bidirectional inference to propagate concrete element types
/// into closure parameters.
pub fn substitute_receiver_type_vars(receiver_ty: &TyKind, method_ty: &TyKind) -> TyKind {
    use std::collections::HashMap;

    let mut bindings: HashMap<TypeVarId, TyKind> = HashMap::new();

    match receiver_ty {
        // List<T> = Slice(inner) → bind T to the inner element type
        TyKind::Slice(inner) if !matches!(inner.kind, TyKind::Unknown) => {
            bindings.insert(VAR_T, inner.kind.clone());
        }
        _ => {}
    }

    if bindings.is_empty() {
        return method_ty.clone();
    }

    substitute_ty(method_ty, &bindings)
}

fn substitute_ty(ty: &TyKind, bindings: &std::collections::HashMap<TypeVarId, TyKind>) -> TyKind {
    match ty {
        TyKind::Var(id) => bindings.get(id).cloned().unwrap_or_else(|| ty.clone()),
        TyKind::Slice(inner) => TyKind::Slice(Box::new(Ty {
            kind: substitute_ty(&inner.kind, bindings),
            ..Ty::default()
        })),
        TyKind::Fn(params, ret) => {
            let params = params
                .iter()
                .map(|p| Ty {
                    kind: substitute_ty(&p.kind, bindings),
                    ..Ty::default()
                })
                .collect();
            let ret = Box::new(Ty {
                kind: substitute_ty(&ret.kind, bindings),
                ..Ty::default()
            });
            TyKind::Fn(params, ret)
        }
        _ => ty.clone(),
    }
}
