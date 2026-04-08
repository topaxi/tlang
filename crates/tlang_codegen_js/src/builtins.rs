/// One entry in the JS glue registry: a tlang stdlib name, its JavaScript
/// equivalent, and (where applicable) the semantic `DefKind` used by the
/// semantic analyser.
///
/// `def_kind` is `None` for qualified-name aliases that are JS-glue-only and
/// have no separate semantic symbol table entry (e.g. `log::group`).
pub struct JsBuiltin {
    pub tlang_name: &'static str,
    pub js_name: &'static str,
}

impl JsBuiltin {
    const fn new(tlang_name: &'static str, js_name: &'static str) -> Self {
        Self {
            tlang_name,
            js_name,
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
    JsBuiltin::new("Some", "Option.Some"),
    JsBuiltin::new("None", "Option.None"),
    JsBuiltin::new("Ok", "Result.Ok"),
    JsBuiltin::new("Err", "Result.Err"),
    JsBuiltin::new("log", "console.log"),
    JsBuiltin::new("log::log", "console.log"),
    JsBuiltin::new("log::group", "console.group"),
    JsBuiltin::new("log::groupEnd", "console.groupEnd"),
    JsBuiltin::new("math", "Math"),
    JsBuiltin::new("math::pi", "Math.PI"),
    JsBuiltin::new("math::min", "Math.min"),
    JsBuiltin::new("math::max", "Math.max"),
    JsBuiltin::new("math::floor", "Math.floor"),
    JsBuiltin::new("math::sqrt", "Math.sqrt"),
    JsBuiltin::new("math::random", "Math.random"),
    JsBuiltin::new("random_int", "random_int"),
    JsBuiltin::new("string::StringBuf", "string.StringBuf"),
    JsBuiltin::new("string::from_char_code", "String.fromCharCode"),
    JsBuiltin::new("string::char_code_at", "string.char_code_at"),
    // ── Temporal ───────────────────────────────────────────────────────
    JsBuiltin::new("Temporal", "Temporal"),
    JsBuiltin::new("Temporal::Now", "Temporal.Now"),
    JsBuiltin::new("Temporal::Now::instant", "Temporal.Now.instant"),
    JsBuiltin::new(
        "Temporal::Now::plain_date_time_iso",
        "Temporal.Now.plain_date_time_iso",
    ),
    JsBuiltin::new(
        "Temporal::Now::plain_date_iso",
        "Temporal.Now.plain_date_iso",
    ),
    JsBuiltin::new(
        "Temporal::Now::plain_time_iso",
        "Temporal.Now.plain_time_iso",
    ),
    JsBuiltin::new(
        "Temporal::Now::zoned_date_time_iso",
        "Temporal.Now.zoned_date_time_iso",
    ),
    JsBuiltin::new("Temporal::Now::time_zone_id", "Temporal.Now.time_zone_id"),
    JsBuiltin::new("Temporal::Instant", "Temporal.Instant"),
    JsBuiltin::new("Temporal::Instant::from", "Temporal.Instant.from"),
    JsBuiltin::new(
        "Temporal::Instant::from_epoch_milliseconds",
        "Temporal.Instant.from_epoch_milliseconds",
    ),
    JsBuiltin::new("Temporal::Instant::compare", "Temporal.Instant.compare"),
    JsBuiltin::new("Temporal::Duration", "Temporal.Duration"),
    JsBuiltin::new("Temporal::Duration::from", "Temporal.Duration.from"),
    JsBuiltin::new("Temporal::PlainDate", "Temporal.PlainDate"),
    JsBuiltin::new("Temporal::PlainDate::from", "Temporal.PlainDate.from"),
    JsBuiltin::new("Temporal::PlainDate::new", "Temporal.PlainDate.new"),
    JsBuiltin::new("Temporal::PlainDate::compare", "Temporal.PlainDate.compare"),
    JsBuiltin::new("Temporal::PlainTime", "Temporal.PlainTime"),
    JsBuiltin::new("Temporal::PlainTime::from", "Temporal.PlainTime.from"),
    JsBuiltin::new("Temporal::PlainTime::new", "Temporal.PlainTime.new"),
    JsBuiltin::new("Temporal::PlainTime::compare", "Temporal.PlainTime.compare"),
    JsBuiltin::new("Temporal::PlainDateTime", "Temporal.PlainDateTime"),
    JsBuiltin::new(
        "Temporal::PlainDateTime::from",
        "Temporal.PlainDateTime.from",
    ),
    JsBuiltin::new(
        "Temporal::PlainDateTime::compare",
        "Temporal.PlainDateTime.compare",
    ),
    JsBuiltin::new("Temporal::ZonedDateTime", "Temporal.ZonedDateTime"),
    JsBuiltin::new(
        "Temporal::ZonedDateTime::from",
        "Temporal.ZonedDateTime.from",
    ),
    JsBuiltin::new(
        "Temporal::ZonedDateTime::compare",
        "Temporal.ZonedDateTime.compare",
    ),
    JsBuiltin::new("Temporal::PlainYearMonth", "Temporal.PlainYearMonth"),
    JsBuiltin::new(
        "Temporal::PlainYearMonth::from",
        "Temporal.PlainYearMonth.from",
    ),
    JsBuiltin::new(
        "Temporal::PlainYearMonth::new",
        "Temporal.PlainYearMonth.new",
    ),
    JsBuiltin::new(
        "Temporal::PlainYearMonth::compare",
        "Temporal.PlainYearMonth.compare",
    ),
    JsBuiltin::new("Temporal::PlainMonthDay", "Temporal.PlainMonthDay"),
    JsBuiltin::new(
        "Temporal::PlainMonthDay::from",
        "Temporal.PlainMonthDay.from",
    ),
    JsBuiltin::new("Temporal::PlainMonthDay::new", "Temporal.PlainMonthDay.new"),
];

/// Look up a tlang name in the JS builtins registry.
pub fn lookup(tlang_name: &str) -> Option<&'static str> {
    JS_BUILTINS
        .iter()
        .find(|b| b.tlang_name == tlang_name)
        .map(|b| b.js_name)
}

/// Builtin tlang type → JavaScript constructor remappings for `impl` blocks.
///
/// Only types whose JavaScript constructor name *differs* from the tlang type
/// name need to be listed here.  All other builtin types keep their name as the
/// `$impl` constructor key.
///
/// This is the authoritative registry consumed by `generate_impl_block` when
/// the target type has no resolved `HirId` (i.e. it is a builtin/unresolved
/// type rather than a user-defined one).
pub static BUILTIN_TYPE_JS_CONSTRUCTORS: &[(&str, &str)] = &[
    // tlang `List` → JavaScript's native `Array` constructor.
    ("List", "Array"),
];

/// Returns the JavaScript constructor name to use as the `$impl` key for a
/// builtin tlang type.
///
/// Returns `None` when the tlang name should be used as-is (no remapping
/// exists for the given type).
pub fn builtin_type_js_constructor(tlang_name: &str) -> Option<&'static str> {
    BUILTIN_TYPE_JS_CONSTRUCTORS
        .iter()
        .find(|(name, _)| *name == tlang_name)
        .map(|(_, js)| *js)
}
