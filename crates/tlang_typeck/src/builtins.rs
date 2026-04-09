use tlang_hir::{PrimTy, Ty, TyKind};

/// A builtin function signature: the function name, parameter types,
/// and return type.
pub struct BuiltinSignature {
    pub name: &'static str,
    pub params: &'static [TyKind],
    pub ret: TyKind,
    /// If `true`, the function accepts a variable number of arguments.
    /// Argument types are still constrained by `params`.
    pub variadic: bool,
}

/// Look up the type signature for a built-in function by name.
///
/// Returns a `TyKind::Fn` wrapping the parameter types and return type,
/// or `None` if the name is not a known builtin.
pub fn lookup(name: &str) -> Option<TyKind> {
    BUILTIN_SIGNATURES.iter().find(|b| b.name == name).map(|b| {
        let params: Vec<Ty> = b
            .params
            .iter()
            .map(|k| Ty {
                kind: k.clone(),
                ..Ty::default()
            })
            .collect();
        let ret = Ty {
            kind: b.ret.clone(),
            ..Ty::default()
        };
        TyKind::Fn(params, Box::new(ret))
    })
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
    },
    // len(unknown) → i64
    BuiltinSignature {
        name: "len",
        params: &[TyKind::Unknown],
        ret: TyKind::Primitive(PrimTy::I64),
        variadic: false,
    },
    // math::sqrt(f64) → f64
    BuiltinSignature {
        name: "math::sqrt",
        params: &[TyKind::Primitive(PrimTy::F64)],
        ret: TyKind::Primitive(PrimTy::F64),
        variadic: false,
    },
    // math::floor(f64) → i64
    BuiltinSignature {
        name: "math::floor",
        params: &[TyKind::Primitive(PrimTy::F64)],
        ret: TyKind::Primitive(PrimTy::I64),
        variadic: false,
    },
    // math::random() → f64
    BuiltinSignature {
        name: "math::random",
        params: &[],
        ret: TyKind::Primitive(PrimTy::F64),
        variadic: false,
    },
    // math::min(f64...) → f64
    BuiltinSignature {
        name: "math::min",
        params: &[TyKind::Primitive(PrimTy::F64)],
        ret: TyKind::Primitive(PrimTy::F64),
        variadic: true,
    },
    // math::max(f64...) → f64
    BuiltinSignature {
        name: "math::max",
        params: &[TyKind::Primitive(PrimTy::F64)],
        ret: TyKind::Primitive(PrimTy::F64),
        variadic: true,
    },
    // panic(String) → never
    BuiltinSignature {
        name: "panic",
        params: &[TyKind::Primitive(PrimTy::String)],
        ret: TyKind::Never,
        variadic: false,
    },
    // string::from_char_code(i64) → String
    BuiltinSignature {
        name: "string::from_char_code",
        params: &[TyKind::Primitive(PrimTy::I64)],
        ret: TyKind::Primitive(PrimTy::String),
        variadic: false,
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
    },
];
