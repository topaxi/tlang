use tlang_hir::{PrimTy, Ty, TyKind};
use tlang_span::TypeVarId;

use crate::builtin_types;

/// A builtin function signature: the function name, parameter types,
/// and return type.
pub struct BuiltinSignature {
    pub name: &'static str,
    pub params: &'static [TyKind],
    pub ret: TyKind,
    /// If `true`, the function accepts a variable number of arguments.
    /// Argument types are still constrained by `params`.
    pub variadic: bool,
    /// If set, the return type is resolved at lookup time via
    /// `builtin_types::lookup` (for types like `Regex` that can't be
    /// expressed as static `TyKind::Path`).
    pub ret_builtin_type: Option<&'static str>,
}

// Well-known type variable IDs — shared with builtin_methods.rs.
const VAR_T: TypeVarId = TypeVarId::new(10_001);
const VAR_U: TypeVarId = TypeVarId::new(10_002);

fn mk_ty(kind: TyKind) -> Ty {
    Ty {
        kind,
        ..Ty::default()
    }
}

fn mk_fn_ty(params: Vec<TyKind>, ret: TyKind) -> TyKind {
    TyKind::Fn(
        params.into_iter().map(mk_ty).collect(),
        Box::new(mk_ty(ret)),
    )
}

/// Look up the type signature for a built-in function by name.
///
/// Returns a `TyKind::Fn` wrapping the parameter types and return type,
/// or `None` if the name is not a known builtin.
pub fn lookup(name: &str) -> Option<TyKind> {
    // Try generic (heap-allocated) signatures first, then static ones.
    if let Some(ty) = lookup_generic(name) {
        return Some(ty);
    }

    lookup_signature(name).map(|b| {
        let params: Vec<Ty> = b
            .params
            .iter()
            .map(|k| Ty {
                kind: k.clone(),
                ..Ty::default()
            })
            .collect();
        let ret_kind = match b.ret_builtin_type {
            Some(type_name) => builtin_types::lookup(type_name).unwrap_or_else(|| b.ret.clone()),
            None => b.ret.clone(),
        };
        let ret = Ty {
            kind: ret_kind,
            ..Ty::default()
        };
        TyKind::Fn(params, Box::new(ret))
    })
}

/// Signatures for generic builtin free functions that require
/// heap-allocated types (`Vec`, `Box`) and can't live in a static array.
fn lookup_generic(name: &str) -> Option<TyKind> {
    let t = TyKind::Var(VAR_T);
    let u = TyKind::Var(VAR_U);

    match name {
        // map<T, U>(iterable: List<T>, f: fn(T) -> U) -> List<U>
        "map" => Some(mk_fn_ty(
            vec![
                TyKind::List(Box::new(mk_ty(t.clone()))),
                mk_fn_ty(vec![t], u.clone()),
            ],
            TyKind::List(Box::new(mk_ty(u))),
        )),
        _ => None,
    }
}

/// Look up the raw signature metadata for a built-in function by name.
pub fn lookup_signature(name: &str) -> Option<&'static BuiltinSignature> {
    BUILTIN_SIGNATURES.iter().find(|b| b.name == name)
}

/// Returns `true` if the named builtin accepts a variable number of
/// arguments (e.g. `log`).
pub fn is_variadic(name: &str) -> bool {
    BUILTIN_SIGNATURES
        .iter()
        .any(|b| b.name == name && b.variadic)
}

/// Authoritative registry of builtin function type signatures.
static BUILTIN_SIGNATURES: &[BuiltinSignature] = &[
    // log(unknown) → nil — accepts anything
    BuiltinSignature {
        name: "log",
        params: &[TyKind::Unknown],
        ret: TyKind::Primitive(PrimTy::Nil),
        variadic: true,
        ret_builtin_type: None,
    },
    // len(unknown) → i64
    BuiltinSignature {
        name: "len",
        params: &[TyKind::Unknown],
        ret: TyKind::Primitive(PrimTy::I64),
        variadic: false,
        ret_builtin_type: None,
    },
    // math::sqrt(f64) → f64
    BuiltinSignature {
        name: "math::sqrt",
        params: &[TyKind::Primitive(PrimTy::F64)],
        ret: TyKind::Primitive(PrimTy::F64),
        variadic: false,
        ret_builtin_type: None,
    },
    // math::floor(f64) → i64
    BuiltinSignature {
        name: "math::floor",
        params: &[TyKind::Primitive(PrimTy::F64)],
        ret: TyKind::Primitive(PrimTy::I64),
        variadic: false,
        ret_builtin_type: None,
    },
    // math::random() → f64
    BuiltinSignature {
        name: "math::random",
        params: &[],
        ret: TyKind::Primitive(PrimTy::F64),
        variadic: false,
        ret_builtin_type: None,
    },
    // math::min(f64...) → f64
    BuiltinSignature {
        name: "math::min",
        params: &[TyKind::Primitive(PrimTy::F64)],
        ret: TyKind::Primitive(PrimTy::F64),
        variadic: true,
        ret_builtin_type: None,
    },
    // math::max(f64...) → f64
    BuiltinSignature {
        name: "math::max",
        params: &[TyKind::Primitive(PrimTy::F64)],
        ret: TyKind::Primitive(PrimTy::F64),
        variadic: true,
        ret_builtin_type: None,
    },
    // panic(String) → never
    BuiltinSignature {
        name: "panic",
        params: &[TyKind::Primitive(PrimTy::String)],
        ret: TyKind::Never,
        variadic: false,
        ret_builtin_type: None,
    },
    // string::from_char_code(i64) → String
    BuiltinSignature {
        name: "string::from_char_code",
        params: &[TyKind::Primitive(PrimTy::I64)],
        ret: TyKind::Primitive(PrimTy::String),
        variadic: false,
        ret_builtin_type: None,
    },
    // string::char_code_at(String, i64) → i64
    BuiltinSignature {
        name: "string::char_code_at",
        params: &[
            TyKind::Primitive(PrimTy::String),
            TyKind::Primitive(PrimTy::I64),
        ],
        ret: TyKind::Primitive(PrimTy::I64),
        variadic: false,
        ret_builtin_type: None,
    },
    // compose(fn, fn) → fn
    BuiltinSignature {
        name: "compose",
        params: &[TyKind::Unknown, TyKind::Unknown],
        ret: TyKind::Unknown,
        variadic: false,
        ret_builtin_type: None,
    },
    // re(parts, values) → Regex
    BuiltinSignature {
        name: "re",
        params: &[TyKind::Unknown, TyKind::Unknown],
        ret: TyKind::Unknown, // resolved via ret_builtin_type
        variadic: false,
        ret_builtin_type: Some("Regex"),
    },
    // f(parts, values) → String  (tagged string interpolation)
    BuiltinSignature {
        name: "f",
        params: &[TyKind::Unknown, TyKind::Unknown],
        ret: TyKind::Primitive(PrimTy::String),
        variadic: false,
        ret_builtin_type: None,
    },
    // Option::Some(value) → Option
    BuiltinSignature {
        name: "Option::Some",
        params: &[TyKind::Unknown],
        ret: TyKind::Unknown,
        variadic: false,
        ret_builtin_type: Some("Option"),
    },
    // Result::Ok(value) → Result
    BuiltinSignature {
        name: "Result::Ok",
        params: &[TyKind::Unknown],
        ret: TyKind::Unknown,
        variadic: false,
        ret_builtin_type: Some("Result"),
    },
    // Result::Err(error) → Result
    BuiltinSignature {
        name: "Result::Err",
        params: &[TyKind::Unknown],
        ret: TyKind::Unknown,
        variadic: false,
        ret_builtin_type: Some("Result"),
    },
    // string::StringBuf(initial?) → StringBuf
    BuiltinSignature {
        name: "string::StringBuf",
        params: &[TyKind::Primitive(PrimTy::String)],
        ret: TyKind::Unknown,
        variadic: true,
        ret_builtin_type: Some("StringBuf"),
    },
];
