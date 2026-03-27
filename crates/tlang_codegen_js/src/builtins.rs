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
/// - `builtins::lookup()` — backs the codegen name resolution so identifiers
///   resolve to their JS counterparts during code generation.
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
    // ── Temporal ───────────────────────────────────────────────────────
    JsBuiltin::new("Temporal", "Temporal", Some(DefKind::Module)),
    JsBuiltin::new("Temporal::Now", "Temporal.Now", Some(DefKind::Module)),
    JsBuiltin::new(
        "Temporal::Now::instant",
        "Temporal.Now.instant",
        Some(DefKind::Function(0)),
    ),
    JsBuiltin::new(
        "Temporal::Now::plain_date_time_iso",
        "Temporal.Now.plain_date_time_iso",
        Some(DefKind::Function(u16::MAX)),
    ),
    JsBuiltin::new(
        "Temporal::Now::plain_date_iso",
        "Temporal.Now.plain_date_iso",
        Some(DefKind::Function(u16::MAX)),
    ),
    JsBuiltin::new(
        "Temporal::Now::plain_time_iso",
        "Temporal.Now.plain_time_iso",
        Some(DefKind::Function(u16::MAX)),
    ),
    JsBuiltin::new(
        "Temporal::Now::zoned_date_time_iso",
        "Temporal.Now.zoned_date_time_iso",
        Some(DefKind::Function(u16::MAX)),
    ),
    JsBuiltin::new(
        "Temporal::Now::time_zone_id",
        "Temporal.Now.time_zone_id",
        Some(DefKind::Function(0)),
    ),
    JsBuiltin::new(
        "Temporal::Instant",
        "Temporal.Instant",
        Some(DefKind::Struct),
    ),
    JsBuiltin::new(
        "Temporal::Instant::from",
        "Temporal.Instant.from",
        Some(DefKind::Function(1)),
    ),
    JsBuiltin::new(
        "Temporal::Instant::from_epoch_milliseconds",
        "Temporal.Instant.from_epoch_milliseconds",
        Some(DefKind::Function(1)),
    ),
    JsBuiltin::new(
        "Temporal::Instant::compare",
        "Temporal.Instant.compare",
        Some(DefKind::Function(2)),
    ),
    JsBuiltin::new(
        "Temporal::Duration",
        "Temporal.Duration",
        Some(DefKind::Struct),
    ),
    JsBuiltin::new(
        "Temporal::Duration::from",
        "Temporal.Duration.from",
        Some(DefKind::Function(1)),
    ),
    JsBuiltin::new(
        "Temporal::PlainDate",
        "Temporal.PlainDate",
        Some(DefKind::Struct),
    ),
    JsBuiltin::new(
        "Temporal::PlainDate::from",
        "Temporal.PlainDate.from",
        Some(DefKind::Function(1)),
    ),
    JsBuiltin::new(
        "Temporal::PlainDate::new",
        "Temporal.PlainDate.new",
        Some(DefKind::Function(3)),
    ),
    JsBuiltin::new(
        "Temporal::PlainDate::compare",
        "Temporal.PlainDate.compare",
        Some(DefKind::Function(2)),
    ),
    JsBuiltin::new(
        "Temporal::PlainTime",
        "Temporal.PlainTime",
        Some(DefKind::Struct),
    ),
    JsBuiltin::new(
        "Temporal::PlainTime::from",
        "Temporal.PlainTime.from",
        Some(DefKind::Function(1)),
    ),
    JsBuiltin::new(
        "Temporal::PlainTime::new",
        "Temporal.PlainTime.new",
        Some(DefKind::Function(3)),
    ),
    JsBuiltin::new(
        "Temporal::PlainTime::compare",
        "Temporal.PlainTime.compare",
        Some(DefKind::Function(2)),
    ),
    JsBuiltin::new(
        "Temporal::PlainDateTime",
        "Temporal.PlainDateTime",
        Some(DefKind::Struct),
    ),
    JsBuiltin::new(
        "Temporal::PlainDateTime::from",
        "Temporal.PlainDateTime.from",
        Some(DefKind::Function(1)),
    ),
    JsBuiltin::new(
        "Temporal::PlainDateTime::compare",
        "Temporal.PlainDateTime.compare",
        Some(DefKind::Function(2)),
    ),
    JsBuiltin::new(
        "Temporal::ZonedDateTime",
        "Temporal.ZonedDateTime",
        Some(DefKind::Struct),
    ),
    JsBuiltin::new(
        "Temporal::ZonedDateTime::from",
        "Temporal.ZonedDateTime.from",
        Some(DefKind::Function(1)),
    ),
    JsBuiltin::new(
        "Temporal::ZonedDateTime::compare",
        "Temporal.ZonedDateTime.compare",
        Some(DefKind::Function(2)),
    ),
    JsBuiltin::new(
        "Temporal::PlainYearMonth",
        "Temporal.PlainYearMonth",
        Some(DefKind::Struct),
    ),
    JsBuiltin::new(
        "Temporal::PlainYearMonth::from",
        "Temporal.PlainYearMonth.from",
        Some(DefKind::Function(1)),
    ),
    JsBuiltin::new(
        "Temporal::PlainYearMonth::new",
        "Temporal.PlainYearMonth.new",
        Some(DefKind::Function(2)),
    ),
    JsBuiltin::new(
        "Temporal::PlainYearMonth::compare",
        "Temporal.PlainYearMonth.compare",
        Some(DefKind::Function(2)),
    ),
    JsBuiltin::new(
        "Temporal::PlainMonthDay",
        "Temporal.PlainMonthDay",
        Some(DefKind::Struct),
    ),
    JsBuiltin::new(
        "Temporal::PlainMonthDay::from",
        "Temporal.PlainMonthDay.from",
        Some(DefKind::Function(1)),
    ),
    JsBuiltin::new(
        "Temporal::PlainMonthDay::new",
        "Temporal.PlainMonthDay.new",
        Some(DefKind::Function(2)),
    ),
];

/// Look up a tlang name in the JS builtins registry.
pub fn lookup(tlang_name: &str) -> Option<&'static str> {
    JS_BUILTINS
        .iter()
        .find(|b| b.tlang_name == tlang_name)
        .map(|b| b.js_name)
}
