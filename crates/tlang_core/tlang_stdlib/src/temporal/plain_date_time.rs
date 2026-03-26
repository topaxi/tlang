use temporal_rs::Calendar;
use temporal_rs::PlainDateTime as TemporalPlainDateTime;
use temporal_rs::options::{DisplayCalendar, Overflow, ToStringRoundingOptions};
use tlang_macros::{define_struct, native_fn};
use tlang_memory::value::object::{TlangObjectKind, TlangStruct};
use tlang_memory::{TlangValue, VMState};

use super::duration::{
    from_temporal as duration_from_temporal, to_temporal as duration_to_temporal,
};

const F_YEAR: usize = 0;
const F_MONTH: usize = 1;
const F_DAY: usize = 2;
const F_HOUR: usize = 3;
const F_MINUTE: usize = 4;
const F_SECOND: usize = 5;
const F_MILLISECOND: usize = 6;
const F_MICROSECOND: usize = 7;
const F_NANOSECOND: usize = 8;
const _F_CALENDAR: usize = 9;

/// Reconstruct a `temporal_rs::PlainDateTime` from struct fields.
fn to_temporal(vm: &VMState, this: TlangValue) -> TemporalPlainDateTime {
    let s = vm
        .get_struct(this)
        .expect("expected Temporal.PlainDateTime struct");
    TemporalPlainDateTime::try_new(
        s[F_YEAR].as_f64() as i32,
        s[F_MONTH].as_f64() as u8,
        s[F_DAY].as_f64() as u8,
        s[F_HOUR].as_f64() as u8,
        s[F_MINUTE].as_f64() as u8,
        s[F_SECOND].as_f64() as u8,
        s[F_MILLISECOND].as_f64() as u16,
        s[F_MICROSECOND].as_f64() as u16,
        s[F_NANOSECOND].as_f64() as u16,
        Calendar::ISO,
    )
    .expect("invalid PlainDateTime fields")
}

/// Create a new `Temporal.PlainDateTime` TlangValue.
pub(crate) fn from_temporal(vm: &mut VMState, pdt: &TemporalPlainDateTime) -> TlangValue {
    let shape = vm
        .heap
        .builtin_shapes
        .lookup("Temporal.PlainDateTime")
        .expect("Temporal.PlainDateTime shape not registered");
    let cal_str = vm.new_string("iso8601".to_string());
    vm.new_object(TlangObjectKind::Struct(TlangStruct::new(
        shape,
        vec![
            TlangValue::I64(pdt.year() as i64),
            TlangValue::I64(pdt.month() as i64),
            TlangValue::I64(pdt.day() as i64),
            TlangValue::I64(pdt.hour() as i64),
            TlangValue::I64(pdt.minute() as i64),
            TlangValue::I64(pdt.second() as i64),
            TlangValue::I64(pdt.millisecond() as i64),
            TlangValue::I64(pdt.microsecond() as i64),
            TlangValue::I64(pdt.nanosecond() as i64),
            cal_str,
        ],
    )))
}

/// `Temporal.PlainDateTime.from(iso_string)` — parse an ISO 8601 date-time string.
#[native_fn(name = "Temporal::PlainDateTime::from")]
pub fn plain_date_time_from(vm: &mut VMState, arg: TlangValue) -> TlangValue {
    let s = vm
        .get_object(arg)
        .and_then(|o| o.as_str())
        .unwrap_or_else(|| vm.panic("Temporal.PlainDateTime.from: expected string".to_string()));
    let pdt = TemporalPlainDateTime::from_utf8(s.as_bytes())
        .unwrap_or_else(|e| vm.panic(format!("Temporal.PlainDateTime.from: {e}")));
    from_temporal(vm, &pdt)
}

define_struct! {
    struct Temporal.PlainDateTime {
        year, month, day,
        hour, minute, second,
        millisecond, microsecond, nanosecond,
        calendar
    }

    impl Temporal.PlainDateTime {
        fn add(this, dur_val) {
            let pdt = to_temporal(vm, this);
            let dur = duration_to_temporal(vm, dur_val);
            let result = pdt.add(&dur, Some(Overflow::Constrain))
                .unwrap_or_else(|e| vm.panic(format!("Temporal.PlainDateTime.add: {e}")));
            from_temporal(vm, &result)
        }

        fn subtract(this, dur_val) {
            let pdt = to_temporal(vm, this);
            let dur = duration_to_temporal(vm, dur_val);
            let result = pdt.subtract(&dur, Some(Overflow::Constrain))
                .unwrap_or_else(|e| vm.panic(format!("Temporal.PlainDateTime.subtract: {e}")));
            from_temporal(vm, &result)
        }

        fn since(this, other_val, smallest_unit_val, largest_unit_val) {
            let a = to_temporal(vm, this);
            let b = to_temporal(vm, other_val);
            let smallest = if smallest_unit_val.is_nil() { None } else {
                Some(super::parse_unit(vm, smallest_unit_val, "Temporal.PlainDateTime.since"))
            };
            let largest = if largest_unit_val.is_nil() { None } else {
                Some(super::parse_unit(vm, largest_unit_val, "Temporal.PlainDateTime.since"))
            };
            let settings = super::build_difference_settings(smallest, largest, None);
            let dur = a.since(&b, settings)
                .unwrap_or_else(|e| vm.panic(format!("Temporal.PlainDateTime.since: {e}")));
            duration_from_temporal(vm, &dur)
        }

        fn until(this, other_val, smallest_unit_val, largest_unit_val) {
            let a = to_temporal(vm, this);
            let b = to_temporal(vm, other_val);
            let smallest = if smallest_unit_val.is_nil() { None } else {
                Some(super::parse_unit(vm, smallest_unit_val, "Temporal.PlainDateTime.until"))
            };
            let largest = if largest_unit_val.is_nil() { None } else {
                Some(super::parse_unit(vm, largest_unit_val, "Temporal.PlainDateTime.until"))
            };
            let settings = super::build_difference_settings(smallest, largest, None);
            let dur = a.until(&b, settings)
                .unwrap_or_else(|e| vm.panic(format!("Temporal.PlainDateTime.until: {e}")));
            duration_from_temporal(vm, &dur)
        }

        fn round(this, smallest_unit_val, rounding_mode_val) {
            let pdt = to_temporal(vm, this);
            let unit = super::parse_unit(vm, smallest_unit_val, "Temporal.PlainDateTime.round");
            let mode = super::parse_rounding_mode_opt(vm, rounding_mode_val, "Temporal.PlainDateTime.round");
            let options = super::build_rounding_options(unit, mode);
            let result = pdt.round(options)
                .unwrap_or_else(|e| vm.panic(format!("Temporal.PlainDateTime.round: {e}")));
            from_temporal(vm, &result)
        }

        fn equals(this, other_val) {
            let a = to_temporal(vm, this);
            let b = to_temporal(vm, other_val);
            TlangValue::Bool(a == b)
        }

        fn to_string(this) {
            let pdt = to_temporal(vm, this);
            let s = pdt.to_ixdtf_string(ToStringRoundingOptions::default(), DisplayCalendar::Auto)
                .unwrap_or_else(|e| vm.panic(format!("Temporal.PlainDateTime.to_string: {e}")));
            vm.new_string(s)
        }
    }

    impl Display for Temporal.PlainDateTime {
        fn to_string(this) {
            let pdt = to_temporal(vm, this);
            let s = pdt.to_ixdtf_string(ToStringRoundingOptions::default(), DisplayCalendar::Auto)
                .unwrap_or_else(|e| vm.panic(format!("Temporal.PlainDateTime.to_string: {e}")));
            vm.new_string(s)
        }
    }
}
