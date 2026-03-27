use temporal_rs::Duration as TemporalDuration;
use temporal_rs::options::ToStringRoundingOptions;
use temporal_rs::partial::PartialDuration;
use tlang_macros::{define_struct, native_fn};
use tlang_memory::value::object::{TlangObjectKind, TlangStruct};
use tlang_memory::{TlangValue, VMState};

const F_YEARS: usize = 0;
const F_MONTHS: usize = 1;
const F_WEEKS: usize = 2;
const F_DAYS: usize = 3;
const F_HOURS: usize = 4;
const F_MINUTES: usize = 5;
const F_SECONDS: usize = 6;
const F_MILLISECONDS: usize = 7;
const F_MICROSECONDS: usize = 8;
const F_NANOSECONDS: usize = 9;
const _F_SIGN: usize = 10;
const _F_BLANK: usize = 11;

/// Reconstruct a `temporal_rs::Duration` from struct field values.
pub(crate) fn to_temporal(vm: &VMState, this: TlangValue) -> TemporalDuration {
    let s = vm
        .get_struct(this)
        .expect("expected Temporal.Duration struct");

    // Extract integer fields directly to avoid f64 precision loss.
    fn get_i64(val: &TlangValue) -> i64 {
        match val {
            TlangValue::I64(v) | TlangValue::I8(v) | TlangValue::I16(v) | TlangValue::I32(v) => *v,
            TlangValue::U64(v) | TlangValue::U8(v) | TlangValue::U16(v) | TlangValue::U32(v) => {
                *v as i64
            }
            other => {
                let f = other.as_f64();
                if !f.is_finite() || f.fract() != 0.0 {
                    panic!("Temporal.Duration: field must be a finite integer");
                }
                f as i64
            }
        }
    }

    TemporalDuration::new(
        get_i64(&s[F_YEARS]),
        get_i64(&s[F_MONTHS]),
        get_i64(&s[F_WEEKS]),
        get_i64(&s[F_DAYS]),
        get_i64(&s[F_HOURS]),
        get_i64(&s[F_MINUTES]),
        get_i64(&s[F_SECONDS]),
        get_i64(&s[F_MILLISECONDS]),
        get_i64(&s[F_MICROSECONDS]) as i128,
        get_i64(&s[F_NANOSECONDS]) as i128,
    )
    .expect("invalid duration fields")
}

/// Create a new `Temporal.Duration` TlangValue from a `temporal_rs::Duration`.
pub(crate) fn from_temporal(vm: &mut VMState, dur: &TemporalDuration) -> TlangValue {
    let shape = vm
        .heap
        .builtin_shapes
        .lookup("Temporal.Duration")
        .expect("Temporal.Duration shape not registered");
    let sign_val: i64 = match dur.sign() {
        temporal_rs::Sign::Positive => 1,
        temporal_rs::Sign::Zero => 0,
        temporal_rs::Sign::Negative => -1,
    };
    vm.new_object(TlangObjectKind::Struct(TlangStruct::new(
        shape,
        vec![
            TlangValue::I64(dur.years()),
            TlangValue::I64(dur.months()),
            TlangValue::I64(dur.weeks()),
            TlangValue::I64(dur.days()),
            TlangValue::I64(dur.hours()),
            TlangValue::I64(dur.minutes()),
            TlangValue::I64(dur.seconds()),
            TlangValue::I64(dur.milliseconds()),
            TlangValue::I64(i64::try_from(dur.microseconds()).unwrap_or_else(|_| {
                vm.panic("Temporal.Duration: microseconds out of i64 range".to_string())
            })),
            TlangValue::I64(i64::try_from(dur.nanoseconds()).unwrap_or_else(|_| {
                vm.panic("Temporal.Duration: nanoseconds out of i64 range".to_string())
            })),
            TlangValue::I64(sign_val),
            TlangValue::Bool(dur.is_zero()),
        ],
    )))
}

/// `Temporal.Duration.from(iso_string)` — construct from an ISO 8601 string.
#[native_fn(name = "Temporal::Duration::from")]
pub fn duration_from(vm: &mut VMState, arg: TlangValue) -> TlangValue {
    if let Some(s) = vm.get_object(arg).and_then(|o| o.as_str()) {
        let dur = TemporalDuration::from_utf8(s.as_bytes())
            .unwrap_or_else(|e| vm.panic(format!("Temporal.Duration.from: {e}")));
        return from_temporal(vm, &dur);
    }

    // If arg is a struct (record), extract fields as a partial duration.
    if let Some(fields) = vm.get_struct(arg) {
        let f = |idx: usize| -> Option<i64> {
            fields.get(idx).filter(|v| !v.is_nil()).map(|v| match v {
                TlangValue::I64(i)
                | TlangValue::I8(i)
                | TlangValue::I16(i)
                | TlangValue::I32(i) => i,
                TlangValue::U64(u)
                | TlangValue::U8(u)
                | TlangValue::U16(u)
                | TlangValue::U32(u) => u as i64,
                other => other.as_f64() as i64,
            })
        };
        let dur = TemporalDuration::from_partial_duration(PartialDuration {
            years: f(0),
            months: f(1),
            weeks: f(2),
            days: f(3),
            hours: f(4),
            minutes: f(5),
            seconds: f(6),
            milliseconds: f(7),
            microseconds: f(8).map(|v| v as i128),
            nanoseconds: f(9).map(|v| v as i128),
        })
        .unwrap_or_else(|e| vm.panic(format!("Temporal.Duration.from: {e}")));
        return from_temporal(vm, &dur);
    }

    vm.panic("Temporal.Duration.from: expected string or record".to_string())
}

define_struct! {
    struct Temporal.Duration {
        years, months, weeks, days,
        hours, minutes, seconds,
        milliseconds, microseconds, nanoseconds,
        sign, blank
    }

    impl Temporal.Duration {
        fn add(this, other_val) {
            let a = to_temporal(vm, this);
            let b = to_temporal(vm, other_val);
            let result = a.add(&b)
                .unwrap_or_else(|e| vm.panic(format!("Temporal.Duration.add: {e}")));
            from_temporal(vm, &result)
        }

        fn subtract(this, other_val) {
            let a = to_temporal(vm, this);
            let b = to_temporal(vm, other_val);
            let result = a.subtract(&b)
                .unwrap_or_else(|e| vm.panic(format!("Temporal.Duration.subtract: {e}")));
            from_temporal(vm, &result)
        }

        fn negated(this) {
            let dur = to_temporal(vm, this);
            from_temporal(vm, &dur.negated())
        }

        fn abs(this) {
            let dur = to_temporal(vm, this);
            from_temporal(vm, &dur.abs())
        }

        fn total(this, unit_val) {
            let dur = to_temporal(vm, this);
            let unit = super::parse_unit(vm, unit_val, "Temporal.Duration.total");
            let result = dur.total(unit, None)
                .unwrap_or_else(|e| vm.panic(format!("Temporal.Duration.total: {e}")));
            TlangValue::F64(result.as_inner())
        }

        fn to_string(this) {
            let dur = to_temporal(vm, this);
            let s = dur.as_temporal_string(ToStringRoundingOptions::default())
                .unwrap_or_else(|e| vm.panic(format!("Temporal.Duration.to_string: {e}")));
            vm.new_string(s)
        }
    }

    impl Display for Temporal.Duration {
        fn to_string(this) {
            let dur = to_temporal(vm, this);
            let s = dur.as_temporal_string(ToStringRoundingOptions::default())
                .unwrap_or_else(|e| vm.panic(format!("Temporal.Duration.to_string: {e}")));
            vm.new_string(s)
        }
    }
}
