//! Semantic symbol definitions for the tlang JavaScript standard library.
//!
//! This crate provides [`symbols`] — a lightweight static slice of
//! `(name, DefKind)` pairs covering the full tlang JS stdlib surface.  It has
//! no heavy dependencies (no `oxc_*`, no codegen machinery) and is safe to
//! include in the LSP server and other tooling.
//!
//! The actual JavaScript name mappings (e.g. `log` → `console.log`) live in
//! `tlang_codegen_js::builtins` and are only needed during code generation.

use tlang_defs::DefKind;

/// All semantic symbols exposed by the tlang JS standard library.
///
/// Each entry is a `(tlang_name, DefKind)` pair.  The list is the single
/// authoritative source used by both the semantic analyser and the JS code
/// generator.
pub static SYMBOLS: &[(&str, DefKind)] = &[
    // ── Option / Result ────────────────────────────────────────────────
    ("Option", DefKind::Enum),
    ("Result", DefKind::Enum),
    ("Option::Some", DefKind::EnumVariant(1)),
    ("Option::None", DefKind::EnumVariant(0)),
    ("Result::Ok", DefKind::EnumVariant(1)),
    ("Result::Err", DefKind::EnumVariant(1)),
    // Short aliases (used without the Option:: / Result:: prefix)
    ("Some", DefKind::EnumVariant(1)),
    ("None", DefKind::EnumVariant(0)),
    ("Ok", DefKind::EnumVariant(1)),
    ("Err", DefKind::EnumVariant(1)),
    // ── Core functions / modules ───────────────────────────────────────
    ("log", DefKind::Function(u16::MAX)),
    ("len", DefKind::Function(1)),
    ("compose", DefKind::Function(2)),
    ("re", DefKind::Function(2)),
    ("f", DefKind::Function(2)),
    ("map", DefKind::Function(2)),
    ("panic", DefKind::Function(1)),
    ("random_int", DefKind::Function(1)),
    // ── math ───────────────────────────────────────────────────────────
    ("math", DefKind::Module),
    ("math::pi", DefKind::Variable),
    ("math::min", DefKind::Function(u16::MAX)),
    ("math::max", DefKind::Function(u16::MAX)),
    ("math::floor", DefKind::Function(1)),
    ("math::sqrt", DefKind::Function(1)),
    ("math::random", DefKind::Function(0)),
    // ── string ─────────────────────────────────────────────────────────
    ("string", DefKind::Module),
    ("string::StringBuf", DefKind::Struct),
    ("string::from_char_code", DefKind::Function(1)),
    ("string::char_code_at", DefKind::Function(2)),
    // ── Builtin collection / protocol types ────────────────────────────
    ("List", DefKind::Struct),
    ("ListIterator", DefKind::Struct),
    ("Regex", DefKind::Struct),
    ("Functor", DefKind::Protocol),
    ("Functor::map", DefKind::ProtocolMethod(2)),
    ("Accepts", DefKind::Protocol),
    ("Accepts::accepts", DefKind::ProtocolMethod(2)),
    ("Iterable", DefKind::Protocol),
    ("Iterable::iter", DefKind::ProtocolMethod(1)),
    ("Iterator", DefKind::Protocol),
    ("Iterator::next", DefKind::ProtocolMethod(1)),
    ("Display", DefKind::Protocol),
    ("Display::to_string", DefKind::ProtocolMethod(1)),
    // ── Temporal ───────────────────────────────────────────────────────
    ("Temporal", DefKind::Module),
    ("Temporal::Now", DefKind::Module),
    ("Temporal::Now::instant", DefKind::Function(0)),
    (
        "Temporal::Now::plain_date_time_iso",
        DefKind::Function(u16::MAX),
    ),
    ("Temporal::Now::plain_date_iso", DefKind::Function(u16::MAX)),
    ("Temporal::Now::plain_time_iso", DefKind::Function(u16::MAX)),
    (
        "Temporal::Now::zoned_date_time_iso",
        DefKind::Function(u16::MAX),
    ),
    ("Temporal::Now::time_zone_id", DefKind::Function(0)),
    ("Temporal::Instant", DefKind::Struct),
    ("Temporal::Instant::from", DefKind::Function(1)),
    (
        "Temporal::Instant::from_epoch_milliseconds",
        DefKind::Function(1),
    ),
    ("Temporal::Instant::compare", DefKind::Function(2)),
    ("Temporal::Duration", DefKind::Struct),
    ("Temporal::Duration::from", DefKind::Function(1)),
    ("Temporal::PlainDate", DefKind::Struct),
    ("Temporal::PlainDate::from", DefKind::Function(1)),
    ("Temporal::PlainDate::new", DefKind::Function(3)),
    ("Temporal::PlainDate::compare", DefKind::Function(2)),
    ("Temporal::PlainTime", DefKind::Struct),
    ("Temporal::PlainTime::from", DefKind::Function(1)),
    ("Temporal::PlainTime::new", DefKind::Function(3)),
    ("Temporal::PlainTime::compare", DefKind::Function(2)),
    ("Temporal::PlainDateTime", DefKind::Struct),
    ("Temporal::PlainDateTime::from", DefKind::Function(1)),
    ("Temporal::PlainDateTime::compare", DefKind::Function(2)),
    ("Temporal::ZonedDateTime", DefKind::Struct),
    ("Temporal::ZonedDateTime::from", DefKind::Function(1)),
    ("Temporal::ZonedDateTime::compare", DefKind::Function(2)),
    ("Temporal::PlainYearMonth", DefKind::Struct),
    ("Temporal::PlainYearMonth::from", DefKind::Function(1)),
    ("Temporal::PlainYearMonth::new", DefKind::Function(2)),
    ("Temporal::PlainYearMonth::compare", DefKind::Function(2)),
    ("Temporal::PlainMonthDay", DefKind::Struct),
    ("Temporal::PlainMonthDay::from", DefKind::Function(1)),
    ("Temporal::PlainMonthDay::new", DefKind::Function(2)),
];

/// Returns all semantic symbols for the tlang JS standard library.
pub fn symbols() -> &'static [(&'static str, DefKind)] {
    SYMBOLS
}
