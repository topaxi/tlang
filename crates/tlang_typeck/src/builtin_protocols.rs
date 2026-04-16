//! Static registry of native protocol type signatures for the type checker.
//!
//! This mirrors the typed protocol definitions from the JS codegen stdlib
//! (`protocols.tlang`) so the type checker can validate impl blocks and
//! where-clause bounds against native protocols without depending on the
//! runtime (`tlang_memory`).

use tlang_ast::node::Ident;
use tlang_span::Span;

use crate::type_table::{AssociatedTypeInfo, ProtocolInfo, ProtocolMethodInfo};

/// A static protocol method signature descriptor.
struct BuiltinMethod {
    name: &'static str,
    /// Number of parameters (including `self`).
    arity: usize,
    has_default_body: bool,
}

/// A static protocol descriptor.
struct BuiltinProtocol {
    name: &'static str,
    methods: &'static [BuiltinMethod],
    associated_types: &'static [(&'static str, usize)], // (name, type_param_count)
}

/// Authoritative registry of native protocol signatures.
///
/// Kept in sync with `crates/tlang_codegen_js/std/protocols.tlang` and
/// `crates/tlang_core/tlang_stdlib/src/protocols.rs`.
static BUILTIN_PROTOCOLS: &[BuiltinProtocol] = &[
    // protocol Truthy { fn truthy(self) -> bool }
    BuiltinProtocol {
        name: "Truthy",
        methods: &[BuiltinMethod {
            name: "truthy",
            arity: 1,
            has_default_body: false,
        }],
        associated_types: &[],
    },
    // protocol Functor<T> { type Wrapped<U>; fn map<U>(self, f: fn(T) -> U) -> Wrapped<U> }
    BuiltinProtocol {
        name: "Functor",
        methods: &[BuiltinMethod {
            name: "map",
            arity: 2,
            has_default_body: false,
        }],
        associated_types: &[("Wrapped", 1)],
    },
    // protocol Accepts { fn accepts(self, value) }
    BuiltinProtocol {
        name: "Accepts",
        methods: &[BuiltinMethod {
            name: "accepts",
            arity: 2,
            has_default_body: false,
        }],
        associated_types: &[],
    },
    // protocol Iterable<T> { fn iter(self) -> Iterator<T> }
    BuiltinProtocol {
        name: "Iterable",
        methods: &[BuiltinMethod {
            name: "iter",
            arity: 1,
            has_default_body: false,
        }],
        associated_types: &[],
    },
    // protocol Iterator<T> { fn next(self) -> Option<T> }
    BuiltinProtocol {
        name: "Iterator",
        methods: &[BuiltinMethod {
            name: "next",
            arity: 1,
            has_default_body: false,
        }],
        associated_types: &[],
    },
    // protocol Display { fn to_string(self) -> String { … } }
    BuiltinProtocol {
        name: "Display",
        methods: &[BuiltinMethod {
            name: "to_string",
            arity: 1,
            has_default_body: true,
        }],
        associated_types: &[],
    },
    // protocol Into<T> { fn into(self) -> T }
    BuiltinProtocol {
        name: "Into",
        methods: &[BuiltinMethod {
            name: "into",
            arity: 1,
            has_default_body: false,
        }],
        associated_types: &[],
    },
    // protocol TryInto<T> { fn try_into(self) -> Result<T, String> }
    BuiltinProtocol {
        name: "TryInto",
        methods: &[BuiltinMethod {
            name: "try_into",
            arity: 1,
            has_default_body: false,
        }],
        associated_types: &[],
    },
];

fn ident(name: &str) -> Ident {
    Ident::new(name, Span::default())
}

/// Returns `true` if `name` is a builtin native protocol.
pub fn is_builtin_protocol(name: &str) -> bool {
    BUILTIN_PROTOCOLS.iter().any(|p| p.name == name)
}

/// Return [`ProtocolInfo`] entries for all builtin native protocols.
pub fn all() -> Vec<ProtocolInfo> {
    BUILTIN_PROTOCOLS.iter().map(to_protocol_info).collect()
}

/// Return `(name, DefKind)` pairs for all builtin protocol names and their
/// methods, suitable for [`SemanticAnalyzer::add_builtin_symbols`].
///
/// Also includes builtin type names (Option, Result, Regex, StringBuf)
/// and their variants/constructors so the semantic analyzer doesn't reject
/// references to them.
pub fn builtin_symbols() -> Vec<(String, tlang_defs::DefKind)> {
    let mut symbols = Vec::new();

    // Protocol names and methods.
    for p in BUILTIN_PROTOCOLS {
        symbols.push((p.name.to_string(), tlang_defs::DefKind::Protocol));
        for m in p.methods {
            symbols.push((
                format!("{}::{}", p.name, m.name),
                tlang_defs::DefKind::ProtocolMethod(m.arity as u16),
            ));
        }
    }

    // Builtin enum types and their variants.
    symbols.extend([
        ("Option".to_string(), tlang_defs::DefKind::Enum),
        (
            "Option::Some".to_string(),
            tlang_defs::DefKind::EnumVariant(1),
        ),
        (
            "Option::None".to_string(),
            tlang_defs::DefKind::EnumVariant(0),
        ),
        ("Result".to_string(), tlang_defs::DefKind::Enum),
        (
            "Result::Ok".to_string(),
            tlang_defs::DefKind::EnumVariant(1),
        ),
        (
            "Result::Err".to_string(),
            tlang_defs::DefKind::EnumVariant(1),
        ),
    ]);

    // Builtin struct types and their constructors.
    symbols.extend([
        ("Regex".to_string(), tlang_defs::DefKind::Struct),
        ("StringBuf".to_string(), tlang_defs::DefKind::Struct),
        ("re".to_string(), tlang_defs::DefKind::Function(2)),
        (
            "string::StringBuf".to_string(),
            tlang_defs::DefKind::Function(u16::MAX),
        ),
    ]);

    // Global protocol dispatch functions (free-function wrappers).
    symbols.extend([
        // map(iterable, f) — Functor::map dispatch
        ("map".to_string(), tlang_defs::DefKind::Function(2)),
    ]);

    symbols
}

fn to_protocol_info(p: &BuiltinProtocol) -> ProtocolInfo {
    let methods = p
        .methods
        .iter()
        .map(|m| ProtocolMethodInfo {
            name: ident(m.name),
            // Parameter types are unknown for native protocol methods —
            // the macro doesn't carry type annotations. The type checker
            // uses arity for validation; full generic signatures are a
            // future enhancement.
            param_tys: (0..m.arity).map(|_| tlang_hir::Ty::unknown()).collect(),
            return_ty: tlang_hir::Ty::unknown(),
            has_default_body: m.has_default_body,
        })
        .collect();

    let associated_types = p
        .associated_types
        .iter()
        .map(|(name, type_param_count)| AssociatedTypeInfo {
            name: ident(name),
            type_param_count: *type_param_count,
        })
        .collect();

    ProtocolInfo {
        name: ident(p.name),
        type_param_var_ids: Vec::new(),
        methods,
        constraints: Vec::new(),
        associated_types,
    }
}
