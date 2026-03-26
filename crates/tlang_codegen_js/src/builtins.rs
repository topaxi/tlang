use tlang_defs::DefKind;

/// One entry in the JS glue registry: a tlang stdlib name, its JavaScript
/// equivalent, and (where applicable) the semantic `DefKind` used by the
/// semantic analyser.
///
/// `def_kind` is `None` for qualified-name aliases that are JS-glue-only and
/// have no separate semantic symbol table entry (e.g. `log::group`).
pub struct JsBuiltin {
    pub tlang_name: &'static str,
    pub js_name: &'static str,
    pub def_kind: Option<DefKind>,
}

impl JsBuiltin {
    const fn new(
        tlang_name: &'static str,
        js_name: &'static str,
        def_kind: Option<DefKind>,
    ) -> Self {
        Self {
            tlang_name,
            js_name,
            def_kind,
        }
    }
}

/// Authoritative registry mapping tlang stdlib names to their JavaScript
/// equivalents.  This is the single source of truth for:
///
/// - `Scope::default()` — populates the codegen scope so identifiers resolve
///   to their JS counterparts.
/// - `CodegenJS::get_standard_library_symbols()` — provides `DefKind` metadata
///   to the semantic analyser for all entries where `def_kind` is `Some`.
///
/// When adding a new JS standard-library binding, add it here once.  Entries
/// with `def_kind: None` are JS-glue-only aliases that are not registered as
/// independent semantic symbols.
pub static JS_BUILTINS: &[JsBuiltin] = &[
    JsBuiltin::new("Some", "Option.Some", Some(DefKind::EnumVariant(1))),
    JsBuiltin::new("None", "Option.None", Some(DefKind::EnumVariant(0))),
    JsBuiltin::new("Ok", "Result.Ok", Some(DefKind::EnumVariant(1))),
    JsBuiltin::new("Err", "Result.Err", Some(DefKind::EnumVariant(1))),
    JsBuiltin::new("log", "console.log", Some(DefKind::Function(u16::MAX))),
    JsBuiltin::new("log::log", "console.log", None),
    JsBuiltin::new("log::group", "console.group", None),
    JsBuiltin::new("log::groupEnd", "console.groupEnd", None),
    JsBuiltin::new("math", "Math", Some(DefKind::Module)),
    JsBuiltin::new("math::pi", "Math.PI", Some(DefKind::Variable)),
    JsBuiltin::new("math::min", "Math.min", Some(DefKind::Function(u16::MAX))),
    JsBuiltin::new("math::max", "Math.max", Some(DefKind::Function(u16::MAX))),
    JsBuiltin::new("math::floor", "Math.floor", Some(DefKind::Function(1))),
    JsBuiltin::new("math::sqrt", "Math.sqrt", Some(DefKind::Function(1))),
    JsBuiltin::new("math::random", "Math.random", Some(DefKind::Function(0))),
    JsBuiltin::new("random_int", "random_int", Some(DefKind::Function(1))),
    JsBuiltin::new(
        "string::StringBuf",
        "string.StringBuf",
        Some(DefKind::Struct),
    ),
    JsBuiltin::new(
        "string::from_char_code",
        "String.fromCharCode",
        Some(DefKind::Function(1)),
    ),
    JsBuiltin::new(
        "string::char_code_at",
        "string.char_code_at",
        Some(DefKind::Function(2)),
    ),
];
