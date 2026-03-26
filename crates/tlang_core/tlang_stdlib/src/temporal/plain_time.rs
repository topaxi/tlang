use temporal_rs::PlainTime as TemporalPlainTime;
use temporal_rs::options::ToStringRoundingOptions;
use tlang_macros::{define_struct, native_fn};
use tlang_memory::value::object::{TlangObjectKind, TlangStruct};
use tlang_memory::{TlangValue, VMState};

use super::duration::{
    from_temporal as duration_from_temporal, to_temporal as duration_to_temporal,
};

const F_HOUR: usize = 0;
const F_MINUTE: usize = 1;
const F_SECOND: usize = 2;
const F_MILLISECOND: usize = 3;
const F_MICROSECOND: usize = 4;
const F_NANOSECOND: usize = 5;

/// Reconstruct a `temporal_rs::PlainTime` from struct fields.
fn to_temporal(vm: &VMState, this: TlangValue) -> TemporalPlainTime {
    let s = vm
        .get_struct(this)
        .expect("expected Temporal.PlainTime struct");
    TemporalPlainTime::try_new(
        s[F_HOUR].as_f64() as u8,
        s[F_MINUTE].as_f64() as u8,
        s[F_SECOND].as_f64() as u8,
        s[F_MILLISECOND].as_f64() as u16,
        s[F_MICROSECOND].as_f64() as u16,
        s[F_NANOSECOND].as_f64() as u16,
    )
    .expect("invalid PlainTime fields")
}

/// Create a new `Temporal.PlainTime` TlangValue.
pub(crate) fn from_temporal(vm: &mut VMState, pt: &TemporalPlainTime) -> TlangValue {
    let shape = vm
        .heap
        .builtin_shapes
        .lookup("Temporal.PlainTime")
        .expect("Temporal.PlainTime shape not registered");
    vm.new_object(TlangObjectKind::Struct(TlangStruct::new(
        shape,
        vec![
            TlangValue::I64(pt.hour() as i64),
            TlangValue::I64(pt.minute() as i64),
            TlangValue::I64(pt.second() as i64),
            TlangValue::I64(pt.millisecond() as i64),
            TlangValue::I64(pt.microsecond() as i64),
            TlangValue::I64(pt.nanosecond() as i64),
        ],
    )))
}

/// `Temporal.PlainTime.from(iso_string)` — parse an ISO 8601 time string.
#[native_fn(name = "Temporal::PlainTime::from")]
pub fn plain_time_from(vm: &mut VMState, arg: TlangValue) -> TlangValue {
    let s = vm
        .get_object(arg)
        .and_then(|o| o.as_str())
        .unwrap_or_else(|| vm.panic("Temporal.PlainTime.from: expected string".to_string()));
    let pt = TemporalPlainTime::from_utf8(s.as_bytes())
        .unwrap_or_else(|e| vm.panic(format!("Temporal.PlainTime.from: {e}")));
    from_temporal(vm, &pt)
}

define_struct! {
    struct Temporal.PlainTime {
        hour, minute, second,
        millisecond, microsecond, nanosecond
    }

    impl Temporal.PlainTime {
        fn add(this, dur_val) {
            let pt = to_temporal(vm, this);
            let dur = duration_to_temporal(vm, dur_val);
            let result = pt.add(&dur)
                .unwrap_or_else(|e| vm.panic(format!("Temporal.PlainTime.add: {e}")));
            from_temporal(vm, &result)
        }

        fn subtract(this, dur_val) {
            let pt = to_temporal(vm, this);
            let dur = duration_to_temporal(vm, dur_val);
            let result = pt.subtract(&dur)
                .unwrap_or_else(|e| vm.panic(format!("Temporal.PlainTime.subtract: {e}")));
            from_temporal(vm, &result)
        }

        fn since(this, other_val, smallest_unit_val, largest_unit_val) {
            let a = to_temporal(vm, this);
            let b = to_temporal(vm, other_val);
            let smallest = if smallest_unit_val.is_nil() { None } else {
                Some(super::parse_unit(vm, smallest_unit_val, "Temporal.PlainTime.since"))
            };
            let largest = if largest_unit_val.is_nil() { None } else {
                Some(super::parse_unit(vm, largest_unit_val, "Temporal.PlainTime.since"))
            };
            let settings = super::build_difference_settings(smallest, largest, None);
            let dur = a.since(&b, settings)
                .unwrap_or_else(|e| vm.panic(format!("Temporal.PlainTime.since: {e}")));
            duration_from_temporal(vm, &dur)
        }

        fn until(this, other_val, smallest_unit_val, largest_unit_val) {
            let a = to_temporal(vm, this);
            let b = to_temporal(vm, other_val);
            let smallest = if smallest_unit_val.is_nil() { None } else {
                Some(super::parse_unit(vm, smallest_unit_val, "Temporal.PlainTime.until"))
            };
            let largest = if largest_unit_val.is_nil() { None } else {
                Some(super::parse_unit(vm, largest_unit_val, "Temporal.PlainTime.until"))
            };
            let settings = super::build_difference_settings(smallest, largest, None);
            let dur = a.until(&b, settings)
                .unwrap_or_else(|e| vm.panic(format!("Temporal.PlainTime.until: {e}")));
            duration_from_temporal(vm, &dur)
        }

        fn round(this, smallest_unit_val, rounding_mode_val) {
            let pt = to_temporal(vm, this);
            let unit = super::parse_unit(vm, smallest_unit_val, "Temporal.PlainTime.round");
            let mode = super::parse_rounding_mode_opt(vm, rounding_mode_val, "Temporal.PlainTime.round");
            let options = super::build_rounding_options(unit, mode);
            let result = pt.round(options)
                .unwrap_or_else(|e| vm.panic(format!("Temporal.PlainTime.round: {e}")));
            from_temporal(vm, &result)
        }

        fn equals(this, other_val) {
            let a = to_temporal(vm, this);
            let b = to_temporal(vm, other_val);
            TlangValue::Bool(a == b)
        }

        fn to_string(this) {
            let pt = to_temporal(vm, this);
            let s = pt.to_ixdtf_string(ToStringRoundingOptions::default())
                .unwrap_or_else(|e| vm.panic(format!("Temporal.PlainTime.to_string: {e}")));
            vm.new_string(s)
        }
    }

    impl Display for Temporal.PlainTime {
        fn to_string(this) {
            let pt = to_temporal(vm, this);
            let s = pt.to_ixdtf_string(ToStringRoundingOptions::default())
                .unwrap_or_else(|e| vm.panic(format!("Temporal.PlainTime.to_string: {e}")));
            vm.new_string(s)
        }
    }
}
