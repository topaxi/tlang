use temporal_rs::options::{DifferenceSettings, RoundingMode, RoundingOptions, Unit};
use tlang_memory::{TlangValue, VMState};

/// Extract a string field from a struct at the given index.
pub(crate) fn get_string_field(vm: &VMState, this: TlangValue, index: usize) -> String {
    let s = vm.get_struct(this).expect("expected struct");
    vm.get_object(s[index])
        .and_then(|o| o.as_str())
        .unwrap_or("")
        .to_string()
}

/// Extract an i64 field from a struct at the given index.
///
/// Reads integer-valued `TlangValue` variants directly to avoid precision loss
/// from converting large integers (e.g. epoch nanoseconds) through `f64`.
/// Falls back to `as_f64()` only for non-integer values.
pub(crate) fn get_i64_field(vm: &VMState, this: TlangValue, index: usize) -> i64 {
    let s = vm.get_struct(this).expect("expected struct");
    match s[index] {
        TlangValue::I64(i) | TlangValue::I8(i) | TlangValue::I16(i) | TlangValue::I32(i) => i,
        TlangValue::U64(u) | TlangValue::U8(u) | TlangValue::U16(u) | TlangValue::U32(u) => {
            u as i64
        }
        other => other.as_f64() as i64,
    }
}

/// Parse a string argument from a TlangValue.
pub(crate) fn require_string(vm: &mut VMState, val: TlangValue, context: &str) -> String {
    vm.get_object(val)
        .and_then(|o| o.as_str())
        .unwrap_or_else(|| vm.panic(format!("{context}: expected string argument")))
        .to_string()
}

/// Parse a `Unit` from a TlangValue string.
pub(crate) fn parse_unit(vm: &mut VMState, val: TlangValue, context: &str) -> Unit {
    let s = require_string(vm, val, context);
    s.parse::<Unit>()
        .unwrap_or_else(|_| vm.panic(format!("{context}: invalid unit '{s}'")))
}

/// Parse optional `RoundingMode` from a TlangValue (Nil → `None`).
pub(crate) fn parse_rounding_mode_opt(
    vm: &mut VMState,
    val: TlangValue,
    context: &str,
) -> Option<RoundingMode> {
    if val.is_nil() {
        return None;
    }
    let s = require_string(vm, val, context);
    Some(
        s.parse::<RoundingMode>()
            .unwrap_or_else(|_| vm.panic(format!("{context}: invalid rounding mode '{s}'"))),
    )
}

/// Build `DifferenceSettings` from optional unit/mode values.
pub(crate) fn build_difference_settings(
    smallest_unit: Option<Unit>,
    largest_unit: Option<Unit>,
    rounding_mode: Option<RoundingMode>,
) -> DifferenceSettings {
    let mut s = DifferenceSettings::default();
    s.smallest_unit = smallest_unit;
    s.largest_unit = largest_unit;
    s.rounding_mode = Some(rounding_mode.unwrap_or(RoundingMode::Trunc));
    s
}

/// Build `RoundingOptions` from smallest_unit and optional rounding_mode.
pub(crate) fn build_rounding_options(
    smallest_unit: Unit,
    rounding_mode: Option<RoundingMode>,
) -> RoundingOptions {
    let mut o = RoundingOptions::default();
    o.smallest_unit = Some(smallest_unit);
    o.rounding_mode = Some(rounding_mode.unwrap_or(RoundingMode::HalfExpand));
    o
}

/// Return the current epoch nanoseconds from the system clock.
///
/// The value is returned as `i128` for full Temporal precision.
pub(crate) fn system_epoch_nanoseconds() -> i128 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .expect("system time before UNIX epoch")
        .as_nanos() as i128
}
