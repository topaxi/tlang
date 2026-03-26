use temporal_rs::Instant as TemporalInstant;
use temporal_rs::options::ToStringRoundingOptions;
use tlang_macros::{define_struct, native_fn};
use tlang_memory::value::object::{TlangObjectKind, TlangStruct};
use tlang_memory::{TlangValue, VMState};

use super::duration::from_temporal as duration_from_temporal;

const F_EPOCH_NANOSECONDS: usize = 0;
const _F_EPOCH_MILLISECONDS: usize = 1;

/// Reconstruct a `temporal_rs::Instant` from struct fields.
pub(crate) fn to_temporal(vm: &VMState, this: TlangValue) -> TemporalInstant {
    let s = vm
        .get_struct(this)
        .expect("expected Temporal.Instant struct");
    // epoch_nanoseconds stored as I64 (fits current dates until ~2262).
    let ns = s[F_EPOCH_NANOSECONDS].as_f64() as i128;
    TemporalInstant::try_new(ns).expect("invalid epoch nanoseconds")
}

/// Create a new `Temporal.Instant` TlangValue from a `temporal_rs::Instant`.
pub(crate) fn from_temporal(vm: &mut VMState, inst: &TemporalInstant) -> TlangValue {
    let shape = vm
        .heap
        .builtin_shapes
        .lookup("Temporal.Instant")
        .expect("Temporal.Instant shape not registered");
    vm.new_object(TlangObjectKind::Struct(TlangStruct::new(
        shape,
        vec![
            TlangValue::I64(inst.as_i128() as i64),
            TlangValue::I64(inst.epoch_milliseconds()),
        ],
    )))
}

/// `Temporal.Instant.from(iso_string)` — parse an ISO 8601 / RFC 9557 string.
#[native_fn(name = "Temporal::Instant::from")]
pub fn instant_from(vm: &mut VMState, arg: TlangValue) -> TlangValue {
    let s = vm
        .get_object(arg)
        .and_then(|o| o.as_str())
        .unwrap_or_else(|| vm.panic("Temporal.Instant.from: expected string argument".to_string()));
    let inst = TemporalInstant::from_utf8(s.as_bytes())
        .unwrap_or_else(|e| vm.panic(format!("Temporal.Instant.from: {e}")));
    from_temporal(vm, &inst)
}

/// `Temporal.Instant.fromEpochMilliseconds(ms)` — create from epoch milliseconds.
#[native_fn(name = "Temporal::Instant::fromEpochMilliseconds")]
pub fn instant_from_epoch_ms(vm: &mut VMState, ms_val: TlangValue) -> TlangValue {
    let ms = ms_val.as_f64() as i64;
    let inst = TemporalInstant::from_epoch_milliseconds(ms)
        .unwrap_or_else(|e| vm.panic(format!("Temporal.Instant.fromEpochMilliseconds: {e}")));
    from_temporal(vm, &inst)
}

define_struct! {
    struct Temporal.Instant {
        epoch_nanoseconds, epoch_milliseconds
    }

    impl Temporal.Instant {
        fn add(this, dur_val) {
            let inst = to_temporal(vm, this);
            let dur = super::duration::to_temporal(vm, dur_val);
            let result = inst.add(&dur)
                .unwrap_or_else(|e| vm.panic(format!("Temporal.Instant.add: {e}")));
            from_temporal(vm, &result)
        }

        fn subtract(this, dur_val) {
            let inst = to_temporal(vm, this);
            let dur = super::duration::to_temporal(vm, dur_val);
            let result = inst.subtract(&dur)
                .unwrap_or_else(|e| vm.panic(format!("Temporal.Instant.subtract: {e}")));
            from_temporal(vm, &result)
        }

        fn since(this, other_val, smallest_unit_val, largest_unit_val, rounding_mode_val) {
            let a = to_temporal(vm, this);
            let b = to_temporal(vm, other_val);
            let smallest = if smallest_unit_val.is_nil() { None } else {
                Some(super::parse_unit(vm, smallest_unit_val, "Temporal.Instant.since"))
            };
            let largest = if largest_unit_val.is_nil() { None } else {
                Some(super::parse_unit(vm, largest_unit_val, "Temporal.Instant.since"))
            };
            let mode = super::parse_rounding_mode_opt(vm, rounding_mode_val, "Temporal.Instant.since");
            let settings = super::build_difference_settings(smallest, largest, mode);
            let dur = a.since(&b, settings)
                .unwrap_or_else(|e| vm.panic(format!("Temporal.Instant.since: {e}")));
            duration_from_temporal(vm, &dur)
        }

        fn until(this, other_val, smallest_unit_val, largest_unit_val, rounding_mode_val) {
            let a = to_temporal(vm, this);
            let b = to_temporal(vm, other_val);
            let smallest = if smallest_unit_val.is_nil() { None } else {
                Some(super::parse_unit(vm, smallest_unit_val, "Temporal.Instant.until"))
            };
            let largest = if largest_unit_val.is_nil() { None } else {
                Some(super::parse_unit(vm, largest_unit_val, "Temporal.Instant.until"))
            };
            let mode = super::parse_rounding_mode_opt(vm, rounding_mode_val, "Temporal.Instant.until");
            let settings = super::build_difference_settings(smallest, largest, mode);
            let dur = a.until(&b, settings)
                .unwrap_or_else(|e| vm.panic(format!("Temporal.Instant.until: {e}")));
            duration_from_temporal(vm, &dur)
        }

        fn round(this, smallest_unit_val, rounding_mode_val) {
            let inst = to_temporal(vm, this);
            let unit = super::parse_unit(vm, smallest_unit_val, "Temporal.Instant.round");
            let mode = super::parse_rounding_mode_opt(vm, rounding_mode_val, "Temporal.Instant.round");
            let options = super::build_rounding_options(unit, mode);
            let result = inst.round(options)
                .unwrap_or_else(|e| vm.panic(format!("Temporal.Instant.round: {e}")));
            from_temporal(vm, &result)
        }

        fn equals(this, other_val) {
            let a = to_temporal(vm, this);
            let b = to_temporal(vm, other_val);
            TlangValue::Bool(a == b)
        }

        fn to_string(this) {
            let inst = to_temporal(vm, this);
            let s = inst.to_ixdtf_string(None, ToStringRoundingOptions::default())
                .unwrap_or_else(|e| vm.panic(format!("Temporal.Instant.to_string: {e}")));
            vm.new_string(s)
        }
    }

    impl Display for Temporal.Instant {
        fn to_string(this) {
            let inst = to_temporal(vm, this);
            let s = inst.to_ixdtf_string(None, ToStringRoundingOptions::default())
                .unwrap_or_else(|e| vm.panic(format!("Temporal.Instant.to_string: {e}")));
            vm.new_string(s)
        }
    }
}
