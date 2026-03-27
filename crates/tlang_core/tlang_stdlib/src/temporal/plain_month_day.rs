use temporal_rs::PlainMonthDay as TemporalPlainMonthDay;
use tlang_macros::{define_struct, native_fn};
use tlang_memory::value::object::{TlangObjectKind, TlangStruct};
use tlang_memory::{TlangValue, VMState};

const F_MONTH_CODE: usize = 0;
const F_DAY: usize = 1;
const _F_CALENDAR: usize = 2;

/// Reconstruct a `temporal_rs::PlainMonthDay` from struct fields.
fn to_temporal(vm: &VMState, this: TlangValue) -> TemporalPlainMonthDay {
    let s = vm
        .get_struct(this)
        .expect("expected Temporal.PlainMonthDay struct");
    // Require a valid string `month_code` field; do not default silently.
    let month_code = vm
        .get_object(s[F_MONTH_CODE])
        .and_then(|o| o.as_str())
        .expect("Temporal.PlainMonthDay: missing or invalid `month_code` field");
    let day = s[F_DAY].as_f64() as u8;
    // Parse month number from month_code (e.g. "M01" → 1, "M12" → 12).
    let month: u8 = month_code
        .strip_prefix('M')
        .and_then(|n| n.parse().ok())
        .unwrap_or_else(|| {
            panic!(
                "Temporal.PlainMonthDay: invalid month_code `{month_code}`; expected like \"M01\"..\"M12\""
            )
        });
    let iso_str = format!("--{month:02}-{day:02}");
    TemporalPlainMonthDay::from_utf8(iso_str.as_bytes())
        .unwrap_or_else(|e| panic!("Temporal.PlainMonthDay: invalid fields: {e}"))
}

/// Create a new `Temporal.PlainMonthDay` TlangValue.
pub(crate) fn from_temporal(vm: &mut VMState, pmd: &TemporalPlainMonthDay) -> TlangValue {
    let shape = vm
        .heap
        .builtin_shapes
        .lookup("Temporal.PlainMonthDay")
        .expect("Temporal.PlainMonthDay shape not registered");
    let mc = vm.new_string(pmd.month_code().as_str().to_string());
    let cal_str = vm.new_string("iso8601".to_string());
    vm.new_object(TlangObjectKind::Struct(TlangStruct::new(
        shape,
        vec![mc, TlangValue::I64(pmd.day() as i64), cal_str],
    )))
}

/// `Temporal.PlainMonthDay.from(iso_string)` — parse an ISO 8601 month-day.
#[native_fn(name = "Temporal::PlainMonthDay::from")]
pub fn plain_month_day_from(vm: &mut VMState, arg: TlangValue) -> TlangValue {
    let s = vm
        .get_object(arg)
        .and_then(|o| o.as_str())
        .unwrap_or_else(|| vm.panic("Temporal.PlainMonthDay.from: expected string".to_string()));
    let pmd = TemporalPlainMonthDay::from_utf8(s.as_bytes())
        .unwrap_or_else(|e| vm.panic(format!("Temporal.PlainMonthDay.from: {e}")));
    from_temporal(vm, &pmd)
}

define_struct! {
    struct Temporal.PlainMonthDay {
        month_code, day, calendar
    }

    impl Temporal.PlainMonthDay {
        fn equals(this, other_val) {
            let a = to_temporal(vm, this);
            let b = to_temporal(vm, other_val);
            TlangValue::Bool(a == b)
        }

        fn to_string(this) {
            let pmd = to_temporal(vm, this);
            vm.new_string(pmd.to_string())
        }
    }

    impl Display for Temporal.PlainMonthDay {
        fn to_string(this) {
            let pmd = to_temporal(vm, this);
            vm.new_string(pmd.to_string())
        }
    }
}
