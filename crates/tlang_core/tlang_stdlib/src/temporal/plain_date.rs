use temporal_rs::Calendar;
use temporal_rs::PlainDate as TemporalPlainDate;
use temporal_rs::options::Overflow;
use tlang_macros::{define_struct, native_fn};
use tlang_memory::value::object::{TlangObjectKind, TlangStruct};
use tlang_memory::{TlangValue, VMState};

use super::duration::{
    from_temporal as duration_from_temporal, to_temporal as duration_to_temporal,
};

const F_YEAR: usize = 0;
const F_MONTH: usize = 1;
const F_DAY: usize = 2;
const _F_CALENDAR: usize = 3;

/// Reconstruct a `temporal_rs::PlainDate` from struct fields.
fn to_temporal(vm: &VMState, this: TlangValue) -> TemporalPlainDate {
    let s = vm
        .get_struct(this)
        .expect("expected Temporal.PlainDate struct");
    let year = s[F_YEAR].as_f64() as i32;
    let month = s[F_MONTH].as_f64() as u8;
    let day = s[F_DAY].as_f64() as u8;
    TemporalPlainDate::try_new(year, month, day, Calendar::ISO).expect("invalid PlainDate fields")
}

/// Create a new `Temporal.PlainDate` TlangValue.
pub(crate) fn from_temporal(vm: &mut VMState, pd: &TemporalPlainDate) -> TlangValue {
    let shape = vm
        .heap
        .builtin_shapes
        .lookup("Temporal.PlainDate")
        .expect("Temporal.PlainDate shape not registered");
    let cal_str = vm.new_string("iso8601".to_string());
    vm.new_object(TlangObjectKind::Struct(TlangStruct::new(
        shape,
        vec![
            TlangValue::I64(pd.year() as i64),
            TlangValue::I64(pd.month() as i64),
            TlangValue::I64(pd.day() as i64),
            cal_str,
        ],
    )))
}

/// `Temporal.PlainDate.from(iso_string)` — parse an ISO 8601 date string.
#[native_fn(name = "Temporal::PlainDate::from")]
pub fn plain_date_from(vm: &mut VMState, arg: TlangValue) -> TlangValue {
    let s = vm
        .get_object(arg)
        .and_then(|o| o.as_str())
        .unwrap_or_else(|| vm.panic("Temporal.PlainDate.from: expected string".to_string()));
    let pd = TemporalPlainDate::from_utf8(s.as_bytes())
        .unwrap_or_else(|e| vm.panic(format!("Temporal.PlainDate.from: {e}")));
    from_temporal(vm, &pd)
}

define_struct! {
    struct Temporal.PlainDate {
        year, month, day, calendar
    }

    impl Temporal.PlainDate {
        fn add(this, dur_val) {
            let pd = to_temporal(vm, this);
            let dur = duration_to_temporal(vm, dur_val);
            let result = pd.add(&dur, Some(Overflow::Constrain))
                .unwrap_or_else(|e| vm.panic(format!("Temporal.PlainDate.add: {e}")));
            from_temporal(vm, &result)
        }

        fn subtract(this, dur_val) {
            let pd = to_temporal(vm, this);
            let dur = duration_to_temporal(vm, dur_val);
            let result = pd.subtract(&dur, Some(Overflow::Constrain))
                .unwrap_or_else(|e| vm.panic(format!("Temporal.PlainDate.subtract: {e}")));
            from_temporal(vm, &result)
        }

        fn since(this, other_val, smallest_unit_val, largest_unit_val) {
            let a = to_temporal(vm, this);
            let b = to_temporal(vm, other_val);
            let smallest = if smallest_unit_val.is_nil() { None } else {
                Some(super::parse_unit(vm, smallest_unit_val, "Temporal.PlainDate.since"))
            };
            let largest = if largest_unit_val.is_nil() { None } else {
                Some(super::parse_unit(vm, largest_unit_val, "Temporal.PlainDate.since"))
            };
            let settings = super::build_difference_settings(smallest, largest, None);
            let dur = a.since(&b, settings)
                .unwrap_or_else(|e| vm.panic(format!("Temporal.PlainDate.since: {e}")));
            duration_from_temporal(vm, &dur)
        }

        fn until(this, other_val, smallest_unit_val, largest_unit_val) {
            let a = to_temporal(vm, this);
            let b = to_temporal(vm, other_val);
            let smallest = if smallest_unit_val.is_nil() { None } else {
                Some(super::parse_unit(vm, smallest_unit_val, "Temporal.PlainDate.until"))
            };
            let largest = if largest_unit_val.is_nil() { None } else {
                Some(super::parse_unit(vm, largest_unit_val, "Temporal.PlainDate.until"))
            };
            let settings = super::build_difference_settings(smallest, largest, None);
            let dur = a.until(&b, settings)
                .unwrap_or_else(|e| vm.panic(format!("Temporal.PlainDate.until: {e}")));
            duration_from_temporal(vm, &dur)
        }

        fn equals(this, other_val) {
            let a = to_temporal(vm, this);
            let b = to_temporal(vm, other_val);
            TlangValue::Bool(a == b)
        }

        fn day_of_week(this) {
            let pd = to_temporal(vm, this);
            TlangValue::I64(pd.day_of_week() as i64)
        }

        fn day_of_year(this) {
            let pd = to_temporal(vm, this);
            TlangValue::I64(pd.day_of_year() as i64)
        }

        fn to_string(this) {
            let pd = to_temporal(vm, this);
            vm.new_string(pd.to_string())
        }
    }

    impl Display for Temporal.PlainDate {
        fn to_string(this) {
            let pd = to_temporal(vm, this);
            vm.new_string(pd.to_string())
        }
    }
}
