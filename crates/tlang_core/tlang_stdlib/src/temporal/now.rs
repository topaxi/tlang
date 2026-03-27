use temporal_rs::{
    Calendar, Instant, PlainDate, PlainDateTime, PlainTime, TimeZone, ZonedDateTime,
};
use tlang_macros::native_fn;
use tlang_memory::{TlangValue, VMState};

use super::instant::from_temporal as instant_from_temporal;
use super::plain_date::from_temporal as plain_date_from_temporal;
use super::plain_date_time::from_temporal as plain_date_time_from_temporal;
use super::plain_time::from_temporal as plain_time_from_temporal;
use super::zoned_date_time::from_temporal as zoned_from_temporal;

/// `Temporal.Now.instant()` — returns the current system time as an Instant.
#[native_fn(name = "Temporal::Now::instant")]
pub fn now_instant(vm: &mut VMState) -> TlangValue {
    let ns = super::system_epoch_nanoseconds();
    let inst =
        Instant::try_new(ns).unwrap_or_else(|e| vm.panic(format!("Temporal.Now.instant: {e}")));
    instant_from_temporal(vm, &inst)
}

/// `Temporal.Now.zonedDateTimeISO(tz?)` — current time as a ZonedDateTime.
#[native_fn(name = "Temporal::Now::zoned_date_time_iso")]
pub fn now_zoned_date_time_iso(vm: &mut VMState, args: &[TlangValue]) -> TlangValue {
    let ns = super::system_epoch_nanoseconds();
    let tz_val = args.first().copied().unwrap_or(TlangValue::Nil);
    let tz = if tz_val.is_nil() {
        TimeZone::utc()
    } else {
        let s = super::require_string(vm, tz_val, "Temporal.Now.zonedDateTimeISO");
        TimeZone::try_from_str(&s)
            .unwrap_or_else(|e| vm.panic(format!("Temporal.Now.zonedDateTimeISO: {e}")))
    };
    let zdt = ZonedDateTime::try_new_iso(ns, tz)
        .unwrap_or_else(|e| vm.panic(format!("Temporal.Now.zonedDateTimeISO: {e}")));
    zoned_from_temporal(vm, &zdt)
}

/// `Temporal.Now.plainDateTimeISO(tz?)` — current time as a PlainDateTime.
#[native_fn(name = "Temporal::Now::plain_date_time_iso")]
pub fn now_plain_date_time_iso(vm: &mut VMState, args: &[TlangValue]) -> TlangValue {
    let ns = super::system_epoch_nanoseconds();
    let tz_val = args.first().copied().unwrap_or(TlangValue::Nil);
    let tz = if tz_val.is_nil() {
        TimeZone::utc()
    } else {
        let s = super::require_string(vm, tz_val, "Temporal.Now.plainDateTimeISO");
        TimeZone::try_from_str(&s)
            .unwrap_or_else(|e| vm.panic(format!("Temporal.Now.plainDateTimeISO: {e}")))
    };
    let zdt = ZonedDateTime::try_new_iso(ns, tz)
        .unwrap_or_else(|e| vm.panic(format!("Temporal.Now.plainDateTimeISO: {e}")));
    let pdt = PlainDateTime::try_new(
        zdt.year(),
        zdt.month(),
        zdt.day(),
        zdt.hour(),
        zdt.minute(),
        zdt.second(),
        zdt.millisecond(),
        zdt.microsecond(),
        zdt.nanosecond(),
        Calendar::ISO,
    )
    .unwrap_or_else(|e| vm.panic(format!("Temporal.Now.plainDateTimeISO: {e}")));
    plain_date_time_from_temporal(vm, &pdt)
}

/// `Temporal.Now.plainDateISO(tz?)` — current date as a PlainDate.
#[native_fn(name = "Temporal::Now::plain_date_iso")]
pub fn now_plain_date_iso(vm: &mut VMState, args: &[TlangValue]) -> TlangValue {
    let ns = super::system_epoch_nanoseconds();
    let tz_val = args.first().copied().unwrap_or(TlangValue::Nil);
    let tz = if tz_val.is_nil() {
        TimeZone::utc()
    } else {
        let s = super::require_string(vm, tz_val, "Temporal.Now.plainDateISO");
        TimeZone::try_from_str(&s)
            .unwrap_or_else(|e| vm.panic(format!("Temporal.Now.plainDateISO: {e}")))
    };
    let zdt = ZonedDateTime::try_new_iso(ns, tz)
        .unwrap_or_else(|e| vm.panic(format!("Temporal.Now.plainDateISO: {e}")));
    let pd = PlainDate::try_new(zdt.year(), zdt.month(), zdt.day(), Calendar::ISO)
        .unwrap_or_else(|e| vm.panic(format!("Temporal.Now.plainDateISO: {e}")));
    plain_date_from_temporal(vm, &pd)
}

/// `Temporal.Now.plainTimeISO(tz?)` — current time as a PlainTime.
#[native_fn(name = "Temporal::Now::plain_time_iso")]
pub fn now_plain_time_iso(vm: &mut VMState, args: &[TlangValue]) -> TlangValue {
    let ns = super::system_epoch_nanoseconds();
    let tz_val = args.first().copied().unwrap_or(TlangValue::Nil);
    let tz = if tz_val.is_nil() {
        TimeZone::utc()
    } else {
        let s = super::require_string(vm, tz_val, "Temporal.Now.plainTimeISO");
        TimeZone::try_from_str(&s)
            .unwrap_or_else(|e| vm.panic(format!("Temporal.Now.plainTimeISO: {e}")))
    };
    let zdt = ZonedDateTime::try_new_iso(ns, tz)
        .unwrap_or_else(|e| vm.panic(format!("Temporal.Now.plainTimeISO: {e}")));
    let pt = PlainTime::try_new(
        zdt.hour(),
        zdt.minute(),
        zdt.second(),
        zdt.millisecond(),
        zdt.microsecond(),
        zdt.nanosecond(),
    )
    .unwrap_or_else(|e| vm.panic(format!("Temporal.Now.plainTimeISO: {e}")));
    plain_time_from_temporal(vm, &pt)
}

/// `Temporal.Now.timeZoneId()` — returns `"UTC"` (default system timezone).
#[native_fn(name = "Temporal::Now::time_zone_id")]
pub fn now_time_zone_id(vm: &mut VMState) -> TlangValue {
    vm.new_string("UTC".to_string())
}
