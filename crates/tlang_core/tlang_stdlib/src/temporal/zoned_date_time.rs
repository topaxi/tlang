use temporal_rs::options::{
    Disambiguation, DisplayCalendar, DisplayOffset, DisplayTimeZone, OffsetDisambiguation,
    Overflow, ToStringRoundingOptions,
};
use temporal_rs::{Calendar, TimeZone, ZonedDateTime as TemporalZonedDateTime};
use tlang_macros::{define_struct, native_fn};
use tlang_memory::value::object::{TlangObjectKind, TlangStruct};
use tlang_memory::{TlangValue, VMState};

use super::duration::{
    from_temporal as duration_from_temporal, to_temporal as duration_to_temporal,
};
use super::instant::from_temporal as instant_from_temporal;
use super::plain_date::from_temporal as plain_date_from_temporal;
use super::plain_date_time::from_temporal as plain_date_time_from_temporal;
use super::plain_time::from_temporal as plain_time_from_temporal;

const F_EPOCH_NANOSECONDS: usize = 0;
const F_TIMEZONE: usize = 1;
const _F_CALENDAR: usize = 2;

/// Reconstruct a `temporal_rs::ZonedDateTime` from struct fields.
fn to_temporal(vm: &VMState, this: TlangValue) -> TemporalZonedDateTime {
    let s = vm
        .get_struct(this)
        .expect("expected Temporal.ZonedDateTime struct");
    // Read epoch_nanoseconds as integer directly to avoid f64 precision loss.
    let ns: i128 = match s[F_EPOCH_NANOSECONDS] {
        TlangValue::I64(v) | TlangValue::I8(v) | TlangValue::I16(v) | TlangValue::I32(v) => {
            v as i128
        }
        _ => panic!("Temporal.ZonedDateTime: epoch_nanoseconds field must be an integer"),
    };
    let tz_str = super::get_string_field(vm, this, F_TIMEZONE);
    let tz = TimeZone::try_from_str(&tz_str).expect("invalid timezone");
    TemporalZonedDateTime::try_new_iso(ns, tz).expect("invalid ZonedDateTime fields")
}

/// Create a new `Temporal.ZonedDateTime` TlangValue.
pub(crate) fn from_temporal(vm: &mut VMState, zdt: &TemporalZonedDateTime) -> TlangValue {
    let shape = vm
        .heap
        .builtin_shapes
        .lookup("Temporal.ZonedDateTime")
        .expect("Temporal.ZonedDateTime shape not registered");
    let tz_str = zdt
        .time_zone()
        .identifier()
        .unwrap_or_else(|_| "UTC".to_string());
    let tz_val = vm.new_string(tz_str);
    let cal_str = vm.new_string("iso8601".to_string());
    let epoch_ns_i128 = zdt.epoch_nanoseconds().as_i128();
    let epoch_ns_i64 = i64::try_from(epoch_ns_i128).unwrap_or_else(|_| {
        vm.panic(
            "Temporal.ZonedDateTime: epochNanoseconds out of range for i64 storage".to_string(),
        )
    });
    vm.new_object(TlangObjectKind::Struct(TlangStruct::new(
        shape,
        vec![TlangValue::I64(epoch_ns_i64), tz_val, cal_str],
    )))
}

/// `Temporal.ZonedDateTime.from(iso_string)` — parse an RFC 9557 string.
#[native_fn(name = "Temporal::ZonedDateTime::from")]
pub fn zoned_date_time_from(vm: &mut VMState, arg: TlangValue) -> TlangValue {
    let s = vm
        .get_object(arg)
        .and_then(|o| o.as_str())
        .unwrap_or_else(|| vm.panic("Temporal.ZonedDateTime.from: expected string".to_string()));
    let zdt = TemporalZonedDateTime::from_utf8(
        s.as_bytes(),
        Disambiguation::Compatible,
        OffsetDisambiguation::Reject,
    )
    .unwrap_or_else(|e| vm.panic(format!("Temporal.ZonedDateTime.from: {e}")));
    from_temporal(vm, &zdt)
}

define_struct! {
    struct Temporal.ZonedDateTime {
        epoch_nanoseconds, timezone, calendar
    }

    impl Temporal.ZonedDateTime {
        fn add(this, dur_val) {
            let zdt = to_temporal(vm, this);
            let dur = duration_to_temporal(vm, dur_val);
            let result = zdt.add(&dur, Some(Overflow::Constrain))
                .unwrap_or_else(|e| vm.panic(format!("Temporal.ZonedDateTime.add: {e}")));
            from_temporal(vm, &result)
        }

        fn subtract(this, dur_val) {
            let zdt = to_temporal(vm, this);
            let dur = duration_to_temporal(vm, dur_val);
            let result = zdt.subtract(&dur, Some(Overflow::Constrain))
                .unwrap_or_else(|e| vm.panic(format!("Temporal.ZonedDateTime.subtract: {e}")));
            from_temporal(vm, &result)
        }

        fn since(this, other_val, smallest_unit_val, largest_unit_val) {
            let a = to_temporal(vm, this);
            let b = to_temporal(vm, other_val);
            let smallest = if smallest_unit_val.is_nil() { None } else {
                Some(super::parse_unit(vm, smallest_unit_val, "Temporal.ZonedDateTime.since"))
            };
            let largest = if largest_unit_val.is_nil() { None } else {
                Some(super::parse_unit(vm, largest_unit_val, "Temporal.ZonedDateTime.since"))
            };
            let settings = super::build_difference_settings(smallest, largest, None);
            let dur = a.since(&b, settings)
                .unwrap_or_else(|e| vm.panic(format!("Temporal.ZonedDateTime.since: {e}")));
            duration_from_temporal(vm, &dur)
        }

        fn until(this, other_val, smallest_unit_val, largest_unit_val) {
            let a = to_temporal(vm, this);
            let b = to_temporal(vm, other_val);
            let smallest = if smallest_unit_val.is_nil() { None } else {
                Some(super::parse_unit(vm, smallest_unit_val, "Temporal.ZonedDateTime.until"))
            };
            let largest = if largest_unit_val.is_nil() { None } else {
                Some(super::parse_unit(vm, largest_unit_val, "Temporal.ZonedDateTime.until"))
            };
            let settings = super::build_difference_settings(smallest, largest, None);
            let dur = a.until(&b, settings)
                .unwrap_or_else(|e| vm.panic(format!("Temporal.ZonedDateTime.until: {e}")));
            duration_from_temporal(vm, &dur)
        }

        fn round(this, smallest_unit_val, rounding_mode_val) {
            let zdt = to_temporal(vm, this);
            let unit = super::parse_unit(vm, smallest_unit_val, "Temporal.ZonedDateTime.round");
            let mode = super::parse_rounding_mode_opt(vm, rounding_mode_val, "Temporal.ZonedDateTime.round");
            let options = super::build_rounding_options(unit, mode);
            let result = zdt.round(options)
                .unwrap_or_else(|e| vm.panic(format!("Temporal.ZonedDateTime.round: {e}")));
            from_temporal(vm, &result)
        }

        fn equals(this, other_val) {
            let a = to_temporal(vm, this);
            let b = to_temporal(vm, other_val);
            TlangValue::Bool(a.compare_instant(&b) == std::cmp::Ordering::Equal)
        }

        fn to_instant(this) {
            let zdt = to_temporal(vm, this);
            instant_from_temporal(vm, &zdt.to_instant())
        }

        fn to_plain_date(this) {
            let zdt = to_temporal(vm, this);
            let pd = temporal_rs::PlainDate::try_new(
                zdt.year(), zdt.month(), zdt.day(), Calendar::ISO
            ).unwrap_or_else(|e| vm.panic(format!("Temporal.ZonedDateTime.to_plain_date: {e}")));
            plain_date_from_temporal(vm, &pd)
        }

        fn to_plain_time(this) {
            let zdt = to_temporal(vm, this);
            let pt = temporal_rs::PlainTime::try_new(
                zdt.hour(), zdt.minute(), zdt.second(),
                zdt.millisecond(), zdt.microsecond(), zdt.nanosecond(),
            ).unwrap_or_else(|e| vm.panic(format!("Temporal.ZonedDateTime.to_plain_time: {e}")));
            plain_time_from_temporal(vm, &pt)
        }

        fn to_plain_date_time(this) {
            let zdt = to_temporal(vm, this);
            let pdt = temporal_rs::PlainDateTime::try_new(
                zdt.year(), zdt.month(), zdt.day(),
                zdt.hour(), zdt.minute(), zdt.second(),
                zdt.millisecond(), zdt.microsecond(), zdt.nanosecond(),
                Calendar::ISO,
            ).unwrap_or_else(|e| vm.panic(format!("Temporal.ZonedDateTime.to_plain_date_time: {e}")));
            plain_date_time_from_temporal(vm, &pdt)
        }

        fn year(this) {
            let zdt = to_temporal(vm, this);
            TlangValue::I64(zdt.year() as i64)
        }

        fn month(this) {
            let zdt = to_temporal(vm, this);
            TlangValue::I64(zdt.month() as i64)
        }

        fn day(this) {
            let zdt = to_temporal(vm, this);
            TlangValue::I64(zdt.day() as i64)
        }

        fn hour(this) {
            let zdt = to_temporal(vm, this);
            TlangValue::I64(zdt.hour() as i64)
        }

        fn minute(this) {
            let zdt = to_temporal(vm, this);
            TlangValue::I64(zdt.minute() as i64)
        }

        fn second(this) {
            let zdt = to_temporal(vm, this);
            TlangValue::I64(zdt.second() as i64)
        }

        fn offset(this) {
            let zdt = to_temporal(vm, this);
            vm.new_string(zdt.offset().to_string())
        }

        fn to_string(this) {
            let zdt = to_temporal(vm, this);
            let s = zdt.to_ixdtf_string(
                DisplayOffset::Auto,
                DisplayTimeZone::Auto,
                DisplayCalendar::Auto,
                ToStringRoundingOptions::default(),
            ).unwrap_or_else(|e| vm.panic(format!("Temporal.ZonedDateTime.to_string: {e}")));
            vm.new_string(s)
        }
    }

    impl Display for Temporal.ZonedDateTime {
        fn to_string(this) {
            let zdt = to_temporal(vm, this);
            let s = zdt.to_ixdtf_string(
                DisplayOffset::Auto,
                DisplayTimeZone::Auto,
                DisplayCalendar::Auto,
                ToStringRoundingOptions::default(),
            ).unwrap_or_else(|e| vm.panic(format!("Temporal.ZonedDateTime.to_string: {e}")));
            vm.new_string(s)
        }
    }
}
