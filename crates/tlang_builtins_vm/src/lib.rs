//! Semantic symbol definitions for the tlang interpreter (VM) standard library.
//!
//! This crate provides [`symbols`] — a lightweight static slice of
//! `(name, DefKind)` pairs covering the full tlang VM stdlib surface.  It has
//! no heavy dependencies (no VM machinery, no `inventory`, no native function
//! pointers) and is safe to include in the LSP server and other tooling.
//!
//! The actual runtime slot assignments and native function registration live
//! in `tlang_vm` and are only needed at execution time.
//!
//! # Keeping in sync
//!
//! A test in `tlang_vm` asserts that every symbol returned by
//! `VM::builtin_symbols()` (which iterates `inventory`) appears in this list,
//! catching drift when new native functions or protocols are added.

use tlang_defs::DefKind;

/// All semantic symbols exposed by the tlang VM standard library.
///
/// Each entry is a `(tlang_name, DefKind)` pair.  The list is the single
/// authoritative static source used by the LSP server and other tooling to
/// resolve interpreter-backend symbols without pulling in the full VM crate.
pub static SYMBOLS: &[(&str, DefKind)] = &[
    // ── Option ────────────────────────────────────────────────────────────────
    ("Option", DefKind::Enum),
    ("Option::Some", DefKind::Function(1)),
    ("Option::None", DefKind::EnumVariant(0)),
    // ── Result ────────────────────────────────────────────────────────────────
    ("Result", DefKind::Enum),
    ("Result::Ok", DefKind::Function(1)),
    ("Result::Err", DefKind::Function(1)),
    // ── Core global functions ─────────────────────────────────────────────────
    ("compose", DefKind::Function(2)),
    ("f", DefKind::Function(2)),
    ("len", DefKind::Function(1)),
    ("log", DefKind::Function(u16::MAX)),
    ("map", DefKind::Function(2)),
    ("panic", DefKind::Function(u16::MAX)),
    ("re", DefKind::Function(2)),
    ("random_int", DefKind::Function(1)),
    // ── math ──────────────────────────────────────────────────────────────────
    ("math", DefKind::Module),
    ("math::floor", DefKind::Function(1)),
    ("math::max", DefKind::Function(2)),
    ("math::min", DefKind::Function(2)),
    ("math::pi", DefKind::Variable),
    ("math::random", DefKind::Function(0)),
    ("math::random_int", DefKind::Function(1)),
    ("math::sqrt", DefKind::Function(1)),
    // ── string ────────────────────────────────────────────────────────────────
    ("string", DefKind::Module),
    ("string::StringBuf", DefKind::Function(u16::MAX)),
    ("string::char_code_at", DefKind::Function(2)),
    ("string::from_char_code", DefKind::Function(1)),
    // ── Builtin collection types ──────────────────────────────────────────────
    ("List", DefKind::Struct),
    ("ListIterator", DefKind::Struct),
    ("Regex", DefKind::Struct),
    // ── Protocols ─────────────────────────────────────────────────────────────
    ("Accepts", DefKind::Protocol),
    ("Accepts::accepts", DefKind::ProtocolMethod(2)),
    ("Display", DefKind::Protocol),
    ("Display::to_string", DefKind::ProtocolMethod(1)),
    ("Functor", DefKind::Protocol),
    ("Functor::map", DefKind::ProtocolMethod(2)),
    ("Into", DefKind::Protocol),
    ("Into::into", DefKind::ProtocolMethod(1)),
    ("Iterable", DefKind::Protocol),
    ("Iterable::iter", DefKind::ProtocolMethod(1)),
    ("Iterator", DefKind::Protocol),
    ("Iterator::next", DefKind::ProtocolMethod(1)),
    ("TryInto", DefKind::Protocol),
    ("TryInto::try_into", DefKind::ProtocolMethod(1)),
    ("Truthy", DefKind::Protocol),
    ("Truthy::truthy", DefKind::ProtocolMethod(1)),
    // ── Internal VM modules (Rust source module names) ────────────────────────
    ("collections", DefKind::Module),
    ("duration", DefKind::Module),
    ("globals", DefKind::Module),
    ("instant", DefKind::Module),
    ("now", DefKind::Module),
    ("option", DefKind::Module),
    ("plain_date", DefKind::Module),
    ("plain_date_time", DefKind::Module),
    ("plain_month_day", DefKind::Module),
    ("plain_time", DefKind::Module),
    ("plain_year_month", DefKind::Module),
    ("protocols", DefKind::Module),
    ("regex", DefKind::Module),
    ("result", DefKind::Module),
    ("zoned_date_time", DefKind::Module),
    // ── Temporal ──────────────────────────────────────────────────────────────
    ("Temporal", DefKind::Module),
    ("Temporal::Duration", DefKind::Struct),
    ("Temporal::Duration::from", DefKind::Function(1)),
    ("Temporal::Instant", DefKind::Struct),
    ("Temporal::Instant::from", DefKind::Function(1)),
    (
        "Temporal::Instant::from_epoch_milliseconds",
        DefKind::Function(1),
    ),
    ("Temporal::Now", DefKind::Module),
    ("Temporal::Now::instant", DefKind::Function(0)),
    ("Temporal::Now::plain_date_iso", DefKind::Function(u16::MAX)),
    (
        "Temporal::Now::plain_date_time_iso",
        DefKind::Function(u16::MAX),
    ),
    ("Temporal::Now::plain_time_iso", DefKind::Function(u16::MAX)),
    ("Temporal::Now::time_zone_id", DefKind::Function(0)),
    (
        "Temporal::Now::zoned_date_time_iso",
        DefKind::Function(u16::MAX),
    ),
    ("Temporal::PlainDate", DefKind::Struct),
    ("Temporal::PlainDate::from", DefKind::Function(1)),
    ("Temporal::PlainDateTime", DefKind::Struct),
    ("Temporal::PlainDateTime::from", DefKind::Function(1)),
    ("Temporal::PlainMonthDay", DefKind::Struct),
    ("Temporal::PlainMonthDay::from", DefKind::Function(1)),
    ("Temporal::PlainTime", DefKind::Struct),
    ("Temporal::PlainTime::from", DefKind::Function(1)),
    ("Temporal::PlainYearMonth", DefKind::Struct),
    ("Temporal::PlainYearMonth::from", DefKind::Function(1)),
    ("Temporal::ZonedDateTime", DefKind::Struct),
    ("Temporal::ZonedDateTime::from", DefKind::Function(1)),
];

/// Returns all semantic symbols for the tlang VM standard library.
pub fn symbols() -> &'static [(&'static str, DefKind)] {
    SYMBOLS
}
