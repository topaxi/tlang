//! Single source of truth for the tlang JavaScript standard library builtins.
//!
//! Each entry in [`BUILTINS`] carries:
//!
//! - `tlang_name` — the tlang-side qualified name used everywhere in the
//!   compiler (e.g. `"math::floor"`).
//! - `js_name` — the JavaScript equivalent (`Some("Math.floor")`), or `None`
//!   when no remapping is needed and the tlang name is used as-is in JS output.
//! - `def_kind` — the semantic [`DefKind`] used by the analyser (`Some`), or
//!   `None` for JS-glue-only aliases that have no independent semantic symbol
//!   (e.g. `log::group`).
//!
//! Public API
//! ----------
//! - [`BUILTINS`] / helpers on [`JsBuiltin`] — the primary table.
//! - [`symbols`] — filtered `&'static [(&str, DefKind)]` for the analyser.
//! - [`lookup`] — tlang name → JS name for the code generator.
//! - [`BUILTIN_TYPE_JS_CONSTRUCTORS`] / [`builtin_type_js_constructor`] —
//!   tlang type → JS constructor remappings for `impl` blocks.

use std::sync::LazyLock;

use tlang_defs::DefKind;

// ── Core data structure ───────────────────────────────────────────────────────

/// One entry in the unified builtin registry.
pub struct JsBuiltin {
    /// tlang-side qualified name (e.g. `"math::floor"`).
    pub tlang_name: &'static str,
    /// JavaScript equivalent, or `None` when the tlang name is used as-is.
    pub js_name: Option<&'static str>,
    /// Semantic kind for the analyser, or `None` for JS-glue-only aliases.
    pub def_kind: Option<DefKind>,
}

impl JsBuiltin {
    /// Entry with both a semantic kind **and** a JS name remapping.
    const fn mapped(tlang_name: &'static str, js_name: &'static str, def_kind: DefKind) -> Self {
        Self {
            tlang_name,
            js_name: Some(js_name),
            def_kind: Some(def_kind),
        }
    }

    /// Entry with a semantic kind but **no** JS name remapping (tlang name is
    /// used verbatim in JS output).
    const fn semantic(tlang_name: &'static str, def_kind: DefKind) -> Self {
        Self {
            tlang_name,
            js_name: None,
            def_kind: Some(def_kind),
        }
    }

    /// JS-glue-only alias with **no** semantic symbol (analyser never sees it).
    const fn glue(tlang_name: &'static str, js_name: &'static str) -> Self {
        Self {
            tlang_name,
            js_name: Some(js_name),
            def_kind: None,
        }
    }
}

// ── Single authoritative table ────────────────────────────────────────────────

/// The single authoritative registry for all tlang JS standard-library
/// builtins.  When adding or removing a builtin, edit **only** this table.
///
/// - Use [`JsBuiltin::mapped`] when the JS name differs from the tlang name.
/// - Use [`JsBuiltin::semantic`] when no JS renaming is needed.
/// - Use [`JsBuiltin::glue`] for JS-only aliases with no semantic symbol.
pub static BUILTINS: &[JsBuiltin] = &[
    // ── Option / Result ────────────────────────────────────────────────────
    JsBuiltin::semantic("Option", DefKind::Enum),
    JsBuiltin::semantic("Result", DefKind::Enum),
    JsBuiltin::semantic("Option::Some", DefKind::EnumVariant(1)),
    JsBuiltin::semantic("Option::None", DefKind::EnumVariant(0)),
    JsBuiltin::semantic("Result::Ok", DefKind::EnumVariant(1)),
    JsBuiltin::semantic("Result::Err", DefKind::EnumVariant(1)),
    // Short aliases used without the Option:: / Result:: prefix
    JsBuiltin::mapped("Some", "Option.Some", DefKind::EnumVariant(1)),
    JsBuiltin::mapped("None", "Option.None", DefKind::EnumVariant(0)),
    JsBuiltin::mapped("Ok", "Result.Ok", DefKind::EnumVariant(1)),
    JsBuiltin::mapped("Err", "Result.Err", DefKind::EnumVariant(1)),
    // ── Core functions ─────────────────────────────────────────────────────
    JsBuiltin::mapped("log", "console.log", DefKind::Function(u16::MAX)),
    JsBuiltin::glue("log::log", "console.log"),
    JsBuiltin::glue("log::group", "console.group"),
    JsBuiltin::glue("log::groupEnd", "console.groupEnd"),
    JsBuiltin::semantic("len", DefKind::Function(1)),
    JsBuiltin::semantic("compose", DefKind::Function(2)),
    JsBuiltin::semantic("re", DefKind::Function(2)),
    JsBuiltin::semantic("f", DefKind::Function(2)),
    JsBuiltin::semantic("map", DefKind::Function(2)),
    JsBuiltin::semantic("panic", DefKind::Function(1)),
    JsBuiltin::semantic("random_int", DefKind::Function(1)),
    // ── math ───────────────────────────────────────────────────────────────
    JsBuiltin::mapped("math", "Math", DefKind::Module),
    JsBuiltin::mapped("math::pi", "Math.PI", DefKind::Variable),
    JsBuiltin::mapped("math::min", "Math.min", DefKind::Function(u16::MAX)),
    JsBuiltin::mapped("math::max", "Math.max", DefKind::Function(u16::MAX)),
    JsBuiltin::mapped("math::floor", "Math.floor", DefKind::Function(1)),
    JsBuiltin::mapped("math::sqrt", "Math.sqrt", DefKind::Function(1)),
    JsBuiltin::mapped("math::random", "Math.random", DefKind::Function(0)),
    // ── string ─────────────────────────────────────────────────────────────
    JsBuiltin::semantic("string", DefKind::Module),
    JsBuiltin::mapped("string::StringBuf", "string.StringBuf", DefKind::Struct),
    JsBuiltin::mapped(
        "string::from_char_code",
        "String.fromCharCode",
        DefKind::Function(1),
    ),
    JsBuiltin::mapped(
        "string::char_code_at",
        "string.char_code_at",
        DefKind::Function(2),
    ),
    // ── Builtin collection / protocol types ────────────────────────────────
    JsBuiltin::semantic("List", DefKind::Struct),
    JsBuiltin::semantic("ListIterator", DefKind::Struct),
    JsBuiltin::semantic("Regex", DefKind::Struct),
    JsBuiltin::semantic("Functor", DefKind::Protocol),
    JsBuiltin::semantic("Functor::map", DefKind::ProtocolMethod(2)),
    JsBuiltin::semantic("Accepts", DefKind::Protocol),
    JsBuiltin::semantic("Accepts::accepts", DefKind::ProtocolMethod(2)),
    JsBuiltin::semantic("Iterable", DefKind::Protocol),
    JsBuiltin::semantic("Iterable::iter", DefKind::ProtocolMethod(1)),
    JsBuiltin::semantic("Iterator", DefKind::Protocol),
    JsBuiltin::semantic("Iterator::next", DefKind::ProtocolMethod(1)),
    JsBuiltin::semantic("Display", DefKind::Protocol),
    JsBuiltin::semantic("Display::to_string", DefKind::ProtocolMethod(1)),
    JsBuiltin::semantic("Into", DefKind::Protocol),
    JsBuiltin::semantic("Into::into", DefKind::ProtocolMethod(1)),
    JsBuiltin::semantic("TryInto", DefKind::Protocol),
    JsBuiltin::semantic("TryInto::try_into", DefKind::ProtocolMethod(1)),
    // ── Temporal ───────────────────────────────────────────────────────────
    JsBuiltin::semantic("Temporal", DefKind::Module),
    JsBuiltin::mapped("Temporal::Now", "Temporal.Now", DefKind::Module),
    JsBuiltin::mapped(
        "Temporal::Now::instant",
        "Temporal.Now.instant",
        DefKind::Function(0),
    ),
    JsBuiltin::mapped(
        "Temporal::Now::plain_date_time_iso",
        "Temporal.Now.plain_date_time_iso",
        DefKind::Function(u16::MAX),
    ),
    JsBuiltin::mapped(
        "Temporal::Now::plain_date_iso",
        "Temporal.Now.plain_date_iso",
        DefKind::Function(u16::MAX),
    ),
    JsBuiltin::mapped(
        "Temporal::Now::plain_time_iso",
        "Temporal.Now.plain_time_iso",
        DefKind::Function(u16::MAX),
    ),
    JsBuiltin::mapped(
        "Temporal::Now::zoned_date_time_iso",
        "Temporal.Now.zoned_date_time_iso",
        DefKind::Function(u16::MAX),
    ),
    JsBuiltin::mapped(
        "Temporal::Now::time_zone_id",
        "Temporal.Now.time_zone_id",
        DefKind::Function(0),
    ),
    JsBuiltin::mapped("Temporal::Instant", "Temporal.Instant", DefKind::Struct),
    JsBuiltin::mapped(
        "Temporal::Instant::from",
        "Temporal.Instant.from",
        DefKind::Function(1),
    ),
    JsBuiltin::mapped(
        "Temporal::Instant::from_epoch_milliseconds",
        "Temporal.Instant.from_epoch_milliseconds",
        DefKind::Function(1),
    ),
    JsBuiltin::mapped(
        "Temporal::Instant::compare",
        "Temporal.Instant.compare",
        DefKind::Function(2),
    ),
    JsBuiltin::mapped("Temporal::Duration", "Temporal.Duration", DefKind::Struct),
    JsBuiltin::mapped(
        "Temporal::Duration::from",
        "Temporal.Duration.from",
        DefKind::Function(1),
    ),
    JsBuiltin::mapped("Temporal::PlainDate", "Temporal.PlainDate", DefKind::Struct),
    JsBuiltin::mapped(
        "Temporal::PlainDate::from",
        "Temporal.PlainDate.from",
        DefKind::Function(1),
    ),
    JsBuiltin::mapped(
        "Temporal::PlainDate::new",
        "Temporal.PlainDate.new",
        DefKind::Function(3),
    ),
    JsBuiltin::mapped(
        "Temporal::PlainDate::compare",
        "Temporal.PlainDate.compare",
        DefKind::Function(2),
    ),
    JsBuiltin::mapped("Temporal::PlainTime", "Temporal.PlainTime", DefKind::Struct),
    JsBuiltin::mapped(
        "Temporal::PlainTime::from",
        "Temporal.PlainTime.from",
        DefKind::Function(1),
    ),
    JsBuiltin::mapped(
        "Temporal::PlainTime::new",
        "Temporal.PlainTime.new",
        DefKind::Function(3),
    ),
    JsBuiltin::mapped(
        "Temporal::PlainTime::compare",
        "Temporal.PlainTime.compare",
        DefKind::Function(2),
    ),
    JsBuiltin::mapped(
        "Temporal::PlainDateTime",
        "Temporal.PlainDateTime",
        DefKind::Struct,
    ),
    JsBuiltin::mapped(
        "Temporal::PlainDateTime::from",
        "Temporal.PlainDateTime.from",
        DefKind::Function(1),
    ),
    JsBuiltin::mapped(
        "Temporal::PlainDateTime::compare",
        "Temporal.PlainDateTime.compare",
        DefKind::Function(2),
    ),
    JsBuiltin::mapped(
        "Temporal::ZonedDateTime",
        "Temporal.ZonedDateTime",
        DefKind::Struct,
    ),
    JsBuiltin::mapped(
        "Temporal::ZonedDateTime::from",
        "Temporal.ZonedDateTime.from",
        DefKind::Function(1),
    ),
    JsBuiltin::mapped(
        "Temporal::ZonedDateTime::compare",
        "Temporal.ZonedDateTime.compare",
        DefKind::Function(2),
    ),
    JsBuiltin::mapped(
        "Temporal::PlainYearMonth",
        "Temporal.PlainYearMonth",
        DefKind::Struct,
    ),
    JsBuiltin::mapped(
        "Temporal::PlainYearMonth::from",
        "Temporal.PlainYearMonth.from",
        DefKind::Function(1),
    ),
    JsBuiltin::mapped(
        "Temporal::PlainYearMonth::new",
        "Temporal.PlainYearMonth.new",
        DefKind::Function(2),
    ),
    JsBuiltin::mapped(
        "Temporal::PlainYearMonth::compare",
        "Temporal.PlainYearMonth.compare",
        DefKind::Function(2),
    ),
    JsBuiltin::mapped(
        "Temporal::PlainMonthDay",
        "Temporal.PlainMonthDay",
        DefKind::Struct,
    ),
    JsBuiltin::mapped(
        "Temporal::PlainMonthDay::from",
        "Temporal.PlainMonthDay.from",
        DefKind::Function(1),
    ),
    JsBuiltin::mapped(
        "Temporal::PlainMonthDay::new",
        "Temporal.PlainMonthDay.new",
        DefKind::Function(2),
    ),
];

// ── Derived views (cached) ────────────────────────────────────────────────────

static SYMBOLS: LazyLock<Vec<(&'static str, DefKind)>> = LazyLock::new(|| {
    BUILTINS
        .iter()
        .filter_map(|b| b.def_kind.map(|k| (b.tlang_name, k)))
        .collect()
});

/// Returns all semantic symbols for the tlang JS standard library.
///
/// This is the slice consumed by the semantic analyser; it excludes JS-glue
/// entries that have no independent semantic meaning.
pub fn symbols() -> &'static [(&'static str, DefKind)] {
    &SYMBOLS
}

/// Look up the JavaScript name for a tlang stdlib name.
///
/// Returns `None` when the name needs no remapping (the tlang name is valid
/// as-is in JavaScript output).
pub fn lookup(tlang_name: &str) -> Option<&'static str> {
    BUILTINS
        .iter()
        .find(|b| b.tlang_name == tlang_name)
        .and_then(|b| b.js_name)
}

// ── Type constructor remappings ───────────────────────────────────────────────

/// Builtin tlang type → JavaScript constructor remappings for `impl` blocks.
///
/// Only types whose JavaScript constructor name *differs* from the tlang type
/// name need to be listed here.
pub static BUILTIN_TYPE_JS_CONSTRUCTORS: &[(&str, &str)] = &[
    // tlang `List` → JavaScript's native `Array` constructor.
    ("List", "Array"),
];

/// Returns the JavaScript constructor name to use as the `$impl` key for a
/// builtin tlang type, or `None` when the tlang name should be used as-is.
pub fn builtin_type_js_constructor(tlang_name: &str) -> Option<&'static str> {
    BUILTIN_TYPE_JS_CONSTRUCTORS
        .iter()
        .find(|(name, _)| *name == tlang_name)
        .map(|(_, js)| *js)
}
