use temporal_rs::PlainYearMonth as TemporalPlainYearMonth;
use temporal_rs::options::Overflow;
use tlang_macros::{define_struct, native_fn};
use tlang_memory::value::object::{TlangObjectKind, TlangStruct};
use tlang_memory::{TlangValue, VMState};

use super::duration::{
    from_temporal as duration_from_temporal, to_temporal as duration_to_temporal,
};

const F_YEAR: usize = 0;
const F_MONTH: usize = 1;
const _F_CALENDAR: usize = 2;

/// Reconstruct a `temporal_rs::PlainYearMonth` from struct fields.
fn to_temporal(vm: &VMState, this: TlangValue) -> TemporalPlainYearMonth {
    let s = vm
        .get_struct(this)
        .expect("expected Temporal.PlainYearMonth struct");
    let year = s[F_YEAR].as_f64() as i32;
    let month = s[F_MONTH].as_f64() as u8;
    TemporalPlainYearMonth::try_new_iso(year, month, None).expect("invalid PlainYearMonth fields")
}

/// Create a new `Temporal.PlainYearMonth` TlangValue.
pub(crate) fn from_temporal(vm: &mut VMState, pym: &TemporalPlainYearMonth) -> TlangValue {
    let shape = vm
        .heap
        .builtin_shapes
        .lookup("Temporal.PlainYearMonth")
        .expect("Temporal.PlainYearMonth shape not registered");
    let cal_str = vm.new_string("iso8601".to_string());
    vm.new_object(TlangObjectKind::Struct(TlangStruct::new(
        shape,
        vec![
            TlangValue::I64(pym.year() as i64),
            TlangValue::I64(pym.month() as i64),
            cal_str,
        ],
    )))
}

/// `Temporal.PlainYearMonth.from(iso_string)` — parse an ISO 8601 year-month.
#[native_fn(name = "Temporal::PlainYearMonth::from")]
pub fn plain_year_month_from(vm: &mut VMState, arg: TlangValue) -> TlangValue {
    let s = vm
        .get_object(arg)
        .and_then(|o| o.as_str())
        .unwrap_or_else(|| vm.panic("Temporal.PlainYearMonth.from: expected string".to_string()));
    let pym = TemporalPlainYearMonth::from_utf8(s.as_bytes())
        .unwrap_or_else(|e| vm.panic(format!("Temporal.PlainYearMonth.from: {e}")));
    from_temporal(vm, &pym)
}

define_struct! {
    struct Temporal.PlainYearMonth {
        year, month, calendar
    }

    impl Temporal.PlainYearMonth {
        fn add(this, dur_val) {
            let pym = to_temporal(vm, this);
            let dur = duration_to_temporal(vm, dur_val);
            let result = pym.add(&dur, Overflow::Constrain)
                .unwrap_or_else(|e| vm.panic(format!("Temporal.PlainYearMonth.add: {e}")));
            from_temporal(vm, &result)
        }

        fn subtract(this, dur_val) {
            let pym = to_temporal(vm, this);
            let dur = duration_to_temporal(vm, dur_val);
            let result = pym.subtract(&dur, Overflow::Constrain)
                .unwrap_or_else(|e| vm.panic(format!("Temporal.PlainYearMonth.subtract: {e}")));
            from_temporal(vm, &result)
        }

        fn since(this, other_val, smallest_unit_val, largest_unit_val) {
            let a = to_temporal(vm, this);
            let b = to_temporal(vm, other_val);
            let smallest = if smallest_unit_val.is_nil() { None } else {
                Some(super::parse_unit(vm, smallest_unit_val, "Temporal.PlainYearMonth.since"))
            };
            let largest = if largest_unit_val.is_nil() { None } else {
                Some(super::parse_unit(vm, largest_unit_val, "Temporal.PlainYearMonth.since"))
            };
            let settings = super::build_difference_settings(smallest, largest, None);
            let dur = a.since(&b, settings)
                .unwrap_or_else(|e| vm.panic(format!("Temporal.PlainYearMonth.since: {e}")));
            duration_from_temporal(vm, &dur)
        }

        fn until(this, other_val, smallest_unit_val, largest_unit_val) {
            let a = to_temporal(vm, this);
            let b = to_temporal(vm, other_val);
            let smallest = if smallest_unit_val.is_nil() { None } else {
                Some(super::parse_unit(vm, smallest_unit_val, "Temporal.PlainYearMonth.until"))
            };
            let largest = if largest_unit_val.is_nil() { None } else {
                Some(super::parse_unit(vm, largest_unit_val, "Temporal.PlainYearMonth.until"))
            };
            let settings = super::build_difference_settings(smallest, largest, None);
            let dur = a.until(&b, settings)
                .unwrap_or_else(|e| vm.panic(format!("Temporal.PlainYearMonth.until: {e}")));
            duration_from_temporal(vm, &dur)
        }

        fn equals(this, other_val) {
            let a = to_temporal(vm, this);
            let b = to_temporal(vm, other_val);
            TlangValue::Bool(a == b)
        }

        fn to_string(this) {
            let pym = to_temporal(vm, this);
            vm.new_string(pym.to_string())
        }
    }

    impl Display for Temporal.PlainYearMonth {
        fn to_string(this) {
            let pym = to_temporal(vm, this);
            vm.new_string(pym.to_string())
        }
    }
}
