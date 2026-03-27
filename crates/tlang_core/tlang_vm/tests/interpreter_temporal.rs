#![cfg(feature = "binary")]

use tlang_memory::prelude::*;

mod common;

// ── Duration ────────────────────────────────────────────────────────────────

#[test]
fn test_duration_from_iso_string() {
    let s = common::eval_to_string(r#"Temporal::Duration::from("PT2H30M").to_string()"#);
    assert_eq!(s, "PT2H30M");
}

#[test]
fn test_duration_fields() {
    let v = common::eval(r#"Temporal::Duration::from("PT2H30M").hours"#);
    assert_eq!(v, TlangValue::I64(2));

    let v = common::eval(r#"Temporal::Duration::from("PT2H30M").minutes"#);
    assert_eq!(v, TlangValue::I64(30));
}

#[test]
fn test_duration_sign_and_blank() {
    let v = common::eval(r#"Temporal::Duration::from("PT0S").sign"#);
    assert_eq!(v, TlangValue::I64(0));

    let v = common::eval(r#"Temporal::Duration::from("PT0S").blank"#);
    assert_eq!(v, TlangValue::Bool(true));

    let v = common::eval(r#"Temporal::Duration::from("PT1H").sign"#);
    assert_eq!(v, TlangValue::I64(1));

    let v = common::eval(r#"Temporal::Duration::from("PT1H").blank"#);
    assert_eq!(v, TlangValue::Bool(false));
}

#[test]
fn test_duration_add() {
    let s = common::eval_to_string(
        r#"
        let a = Temporal::Duration::from("PT1H");
        let b = Temporal::Duration::from("PT30M");
        a.add(b).to_string()
        "#,
    );
    assert_eq!(s, "PT1H30M");
}

#[test]
fn test_duration_subtract() {
    let s = common::eval_to_string(
        r#"
        let a = Temporal::Duration::from("PT2H");
        let b = Temporal::Duration::from("PT30M");
        a.subtract(b).to_string()
        "#,
    );
    assert_eq!(s, "PT1H30M");
}

#[test]
fn test_duration_negated() {
    let v = common::eval(
        r#"
        let d = Temporal::Duration::from("PT1H");
        d.negated().sign
        "#,
    );
    assert_eq!(v, TlangValue::I64(-1));
}

#[test]
fn test_duration_abs() {
    let v = common::eval(
        r#"
        let d = Temporal::Duration::from("PT1H").negated();
        d.abs().sign
        "#,
    );
    assert_eq!(v, TlangValue::I64(1));
}

#[test]
fn test_duration_total() {
    let v = common::eval(r#"Temporal::Duration::from("PT1H30M").total("minute")"#);
    assert!(matches!(v, TlangValue::F64(f) if (f - 90.0).abs() < 1e-10));
}

// ── Instant ─────────────────────────────────────────────────────────────────

#[test]
fn test_instant_from_string() {
    let s =
        common::eval_to_string(r#"Temporal::Instant::from("2025-03-15T14:30:00Z").to_string()"#);
    assert_eq!(s, "2025-03-15T14:30:00Z");
}

#[test]
fn test_instant_from_epoch_milliseconds() {
    let v = common::eval(r#"Temporal::Instant::from_epoch_milliseconds(0).epoch_milliseconds"#);
    assert_eq!(v, TlangValue::I64(0));
}

#[test]
fn test_instant_epoch_fields() {
    let v = common::eval(r#"Temporal::Instant::from("2025-03-15T14:30:00Z").epoch_milliseconds"#);
    assert_eq!(v, TlangValue::I64(1_742_049_000_000));
}

#[test]
fn test_instant_add() {
    let s = common::eval_to_string(
        r#"
        let inst = Temporal::Instant::from("2025-03-15T14:30:00Z");
        let dur = Temporal::Duration::from("PT1H");
        inst.add(dur).to_string()
        "#,
    );
    assert_eq!(s, "2025-03-15T15:30:00Z");
}

#[test]
fn test_instant_subtract() {
    let s = common::eval_to_string(
        r#"
        let inst = Temporal::Instant::from("2025-03-15T14:30:00Z");
        let dur = Temporal::Duration::from("PT30M");
        inst.subtract(dur).to_string()
        "#,
    );
    assert_eq!(s, "2025-03-15T14:00:00Z");
}

#[test]
fn test_instant_until() {
    let v = common::eval(
        r#"
        let i1 = Temporal::Instant::from("2025-01-01T00:00:00Z");
        let i2 = Temporal::Instant::from("2025-01-01T02:30:00Z");
        i1.until(i2).seconds
        "#,
    );
    assert_eq!(v, TlangValue::I64(9000));
}

#[test]
fn test_instant_since() {
    let v = common::eval(
        r#"
        let i1 = Temporal::Instant::from("2025-01-01T02:30:00Z");
        let i2 = Temporal::Instant::from("2025-01-01T00:00:00Z");
        i1.since(i2).seconds
        "#,
    );
    assert_eq!(v, TlangValue::I64(9000));
}

#[test]
fn test_instant_round() {
    let s = common::eval_to_string(
        r#"
        let inst = Temporal::Instant::from("2025-03-15T14:37:22Z");
        inst.round("hour").to_string()
        "#,
    );
    assert_eq!(s, "2025-03-15T15:00:00Z");
}

#[test]
fn test_instant_equals() {
    let v = common::eval(
        r#"
        let a = Temporal::Instant::from("2025-06-15T00:00:00Z");
        let b = Temporal::Instant::from("2025-06-15T00:00:00Z");
        a.equals(b)
        "#,
    );
    assert_eq!(v, TlangValue::Bool(true));

    let v = common::eval(
        r#"
        let a = Temporal::Instant::from("2025-06-15T00:00:00Z");
        let b = Temporal::Instant::from("2025-06-16T00:00:00Z");
        a.equals(b)
        "#,
    );
    assert_eq!(v, TlangValue::Bool(false));
}

// ── PlainDate ───────────────────────────────────────────────────────────────

#[test]
fn test_plain_date_from_string() {
    let s = common::eval_to_string(r#"Temporal::PlainDate::from("2025-03-15").to_string()"#);
    assert_eq!(s, "2025-03-15");
}

#[test]
fn test_plain_date_fields() {
    let v = common::eval(r#"Temporal::PlainDate::from("2025-03-15").year"#);
    assert_eq!(v, TlangValue::I64(2025));

    let v = common::eval(r#"Temporal::PlainDate::from("2025-03-15").month"#);
    assert_eq!(v, TlangValue::I64(3));

    let v = common::eval(r#"Temporal::PlainDate::from("2025-03-15").day"#);
    assert_eq!(v, TlangValue::I64(15));
}

#[test]
fn test_plain_date_day_of_week() {
    let v = common::eval(r#"Temporal::PlainDate::from("2025-03-15").day_of_week()"#);
    assert_eq!(v, TlangValue::I64(6)); // Saturday
}

#[test]
fn test_plain_date_day_of_year() {
    let v = common::eval(r#"Temporal::PlainDate::from("2025-03-15").day_of_year()"#);
    assert_eq!(v, TlangValue::I64(74));
}

#[test]
fn test_plain_date_add() {
    let s = common::eval_to_string(
        r#"
        let d = Temporal::PlainDate::from("2025-03-15");
        let dur = Temporal::Duration::from("P10D");
        d.add(dur).to_string()
        "#,
    );
    assert_eq!(s, "2025-03-25");
}

#[test]
fn test_plain_date_subtract() {
    let s = common::eval_to_string(
        r#"
        let d = Temporal::PlainDate::from("2025-03-15");
        let dur = Temporal::Duration::from("P5D");
        d.subtract(dur).to_string()
        "#,
    );
    assert_eq!(s, "2025-03-10");
}

#[test]
fn test_plain_date_until() {
    let v = common::eval(
        r#"
        let d1 = Temporal::PlainDate::from("2025-01-01");
        let d2 = Temporal::PlainDate::from("2025-03-01");
        d1.until(d2).days
        "#,
    );
    assert_eq!(v, TlangValue::I64(59));
}

#[test]
fn test_plain_date_since() {
    let v = common::eval(
        r#"
        let d1 = Temporal::PlainDate::from("2025-03-01");
        let d2 = Temporal::PlainDate::from("2025-01-01");
        d1.since(d2).days
        "#,
    );
    assert_eq!(v, TlangValue::I64(59));
}

#[test]
fn test_plain_date_equals() {
    let v = common::eval(
        r#"
        let a = Temporal::PlainDate::from("2025-03-15");
        let b = Temporal::PlainDate::from("2025-03-15");
        a.equals(b)
        "#,
    );
    assert_eq!(v, TlangValue::Bool(true));

    let v = common::eval(
        r#"
        let a = Temporal::PlainDate::from("2025-03-15");
        let b = Temporal::PlainDate::from("2025-03-16");
        a.equals(b)
        "#,
    );
    assert_eq!(v, TlangValue::Bool(false));
}

// ── PlainDateTime ───────────────────────────────────────────────────────────

#[test]
fn test_plain_date_time_from_string() {
    let s = common::eval_to_string(
        r#"Temporal::PlainDateTime::from("2025-03-15T14:30:00").to_string()"#,
    );
    assert_eq!(s, "2025-03-15T14:30:00");
}

#[test]
fn test_plain_date_time_fields() {
    let v = common::eval(r#"Temporal::PlainDateTime::from("2025-03-15T14:30:45").year"#);
    assert_eq!(v, TlangValue::I64(2025));

    let v = common::eval(r#"Temporal::PlainDateTime::from("2025-03-15T14:30:45").month"#);
    assert_eq!(v, TlangValue::I64(3));

    let v = common::eval(r#"Temporal::PlainDateTime::from("2025-03-15T14:30:45").day"#);
    assert_eq!(v, TlangValue::I64(15));

    let v = common::eval(r#"Temporal::PlainDateTime::from("2025-03-15T14:30:45").hour"#);
    assert_eq!(v, TlangValue::I64(14));

    let v = common::eval(r#"Temporal::PlainDateTime::from("2025-03-15T14:30:45").minute"#);
    assert_eq!(v, TlangValue::I64(30));

    let v = common::eval(r#"Temporal::PlainDateTime::from("2025-03-15T14:30:45").second"#);
    assert_eq!(v, TlangValue::I64(45));
}

#[test]
fn test_plain_date_time_add() {
    let s = common::eval_to_string(
        r#"
        let dt = Temporal::PlainDateTime::from("2025-03-15T14:30:00");
        let dur = Temporal::Duration::from("PT2H15M");
        dt.add(dur).to_string()
        "#,
    );
    assert_eq!(s, "2025-03-15T16:45:00");
}

#[test]
fn test_plain_date_time_subtract() {
    let s = common::eval_to_string(
        r#"
        let dt = Temporal::PlainDateTime::from("2025-03-15T14:30:00");
        let dur = Temporal::Duration::from("PT30M");
        dt.subtract(dur).to_string()
        "#,
    );
    assert_eq!(s, "2025-03-15T14:00:00");
}

#[test]
fn test_plain_date_time_until() {
    let v = common::eval(
        r#"
        let a = Temporal::PlainDateTime::from("2025-03-15T10:00:00");
        let b = Temporal::PlainDateTime::from("2025-03-15T14:30:00");
        a.until(b).hours
        "#,
    );
    assert_eq!(v, TlangValue::I64(4));
}

#[test]
fn test_plain_date_time_since() {
    let v = common::eval(
        r#"
        let a = Temporal::PlainDateTime::from("2025-03-15T14:30:00");
        let b = Temporal::PlainDateTime::from("2025-03-15T10:00:00");
        a.since(b).hours
        "#,
    );
    assert_eq!(v, TlangValue::I64(4));
}

#[test]
fn test_plain_date_time_round() {
    let s = common::eval_to_string(
        r#"
        let dt = Temporal::PlainDateTime::from("2025-03-15T14:37:22");
        dt.round("hour").to_string()
        "#,
    );
    assert_eq!(s, "2025-03-15T15:00:00");
}

#[test]
fn test_plain_date_time_equals() {
    let v = common::eval(
        r#"
        let a = Temporal::PlainDateTime::from("2025-03-15T14:30:00");
        let b = Temporal::PlainDateTime::from("2025-03-15T14:30:00");
        a.equals(b)
        "#,
    );
    assert_eq!(v, TlangValue::Bool(true));
}

// ── PlainTime ───────────────────────────────────────────────────────────────

#[test]
fn test_plain_time_from_string() {
    let s = common::eval_to_string(r#"Temporal::PlainTime::from("14:30:00").to_string()"#);
    assert_eq!(s, "14:30:00");
}

#[test]
fn test_plain_time_fields() {
    let v = common::eval(r#"Temporal::PlainTime::from("14:30:45").hour"#);
    assert_eq!(v, TlangValue::I64(14));

    let v = common::eval(r#"Temporal::PlainTime::from("14:30:45").minute"#);
    assert_eq!(v, TlangValue::I64(30));

    let v = common::eval(r#"Temporal::PlainTime::from("14:30:45").second"#);
    assert_eq!(v, TlangValue::I64(45));
}

#[test]
fn test_plain_time_add() {
    let s = common::eval_to_string(
        r#"
        let t = Temporal::PlainTime::from("14:30:00");
        let dur = Temporal::Duration::from("PT2H15M");
        t.add(dur).to_string()
        "#,
    );
    assert_eq!(s, "16:45:00");
}

#[test]
fn test_plain_time_subtract() {
    let s = common::eval_to_string(
        r#"
        let t = Temporal::PlainTime::from("14:30:00");
        let dur = Temporal::Duration::from("PT30M");
        t.subtract(dur).to_string()
        "#,
    );
    assert_eq!(s, "14:00:00");
}

#[test]
fn test_plain_time_until() {
    let v = common::eval(
        r#"
        let a = Temporal::PlainTime::from("10:00:00");
        let b = Temporal::PlainTime::from("14:30:00");
        a.until(b).hours
        "#,
    );
    assert_eq!(v, TlangValue::I64(4));
}

#[test]
fn test_plain_time_since() {
    let v = common::eval(
        r#"
        let a = Temporal::PlainTime::from("14:30:00");
        let b = Temporal::PlainTime::from("10:00:00");
        a.since(b).hours
        "#,
    );
    assert_eq!(v, TlangValue::I64(4));
}

#[test]
fn test_plain_time_round() {
    let s = common::eval_to_string(
        r#"
        let t = Temporal::PlainTime::from("14:37:22");
        t.round("hour").to_string()
        "#,
    );
    assert_eq!(s, "15:00:00");
}

#[test]
fn test_plain_time_equals() {
    let v = common::eval(
        r#"
        let a = Temporal::PlainTime::from("14:30:00");
        let b = Temporal::PlainTime::from("14:30:00");
        a.equals(b)
        "#,
    );
    assert_eq!(v, TlangValue::Bool(true));

    let v = common::eval(
        r#"
        let a = Temporal::PlainTime::from("14:30:00");
        let b = Temporal::PlainTime::from("15:00:00");
        a.equals(b)
        "#,
    );
    assert_eq!(v, TlangValue::Bool(false));
}

// ── PlainMonthDay ───────────────────────────────────────────────────────────

#[test]
fn test_plain_month_day_from_string() {
    let s = common::eval_to_string(r#"Temporal::PlainMonthDay::from("--12-25").to_string()"#);
    assert!(s.contains("12-25"));
}

#[test]
fn test_plain_month_day_fields() {
    let v = common::eval(r#"Temporal::PlainMonthDay::from("--12-25").day"#);
    assert_eq!(v, TlangValue::I64(25));
}

#[test]
fn test_plain_month_day_equals() {
    let v = common::eval(
        r#"
        let a = Temporal::PlainMonthDay::from("--12-25");
        let b = Temporal::PlainMonthDay::from("--12-25");
        a.equals(b)
        "#,
    );
    assert_eq!(v, TlangValue::Bool(true));

    let v = common::eval(
        r#"
        let a = Temporal::PlainMonthDay::from("--12-25");
        let b = Temporal::PlainMonthDay::from("--01-01");
        a.equals(b)
        "#,
    );
    assert_eq!(v, TlangValue::Bool(false));
}

// ── PlainYearMonth ──────────────────────────────────────────────────────────

#[test]
fn test_plain_year_month_from_string() {
    let s = common::eval_to_string(r#"Temporal::PlainYearMonth::from("2025-06").to_string()"#);
    assert_eq!(s, "2025-06");
}

#[test]
fn test_plain_year_month_fields() {
    let v = common::eval(r#"Temporal::PlainYearMonth::from("2025-06").year"#);
    assert_eq!(v, TlangValue::I64(2025));

    let v = common::eval(r#"Temporal::PlainYearMonth::from("2025-06").month"#);
    assert_eq!(v, TlangValue::I64(6));
}

#[test]
fn test_plain_year_month_add() {
    let s = common::eval_to_string(
        r#"
        let ym = Temporal::PlainYearMonth::from("2025-06");
        let dur = Temporal::Duration::from("P3M");
        ym.add(dur).to_string()
        "#,
    );
    assert_eq!(s, "2025-09");
}

#[test]
fn test_plain_year_month_subtract() {
    let s = common::eval_to_string(
        r#"
        let ym = Temporal::PlainYearMonth::from("2025-06");
        let dur = Temporal::Duration::from("P2M");
        ym.subtract(dur).to_string()
        "#,
    );
    assert_eq!(s, "2025-04");
}

#[test]
fn test_plain_year_month_until() {
    let v = common::eval(
        r#"
        let a = Temporal::PlainYearMonth::from("2025-01");
        let b = Temporal::PlainYearMonth::from("2025-06");
        a.until(b).months
        "#,
    );
    assert_eq!(v, TlangValue::I64(5));
}

#[test]
fn test_plain_year_month_since() {
    let v = common::eval(
        r#"
        let a = Temporal::PlainYearMonth::from("2025-06");
        let b = Temporal::PlainYearMonth::from("2025-01");
        a.since(b).months
        "#,
    );
    assert_eq!(v, TlangValue::I64(5));
}

#[test]
fn test_plain_year_month_equals() {
    let v = common::eval(
        r#"
        let a = Temporal::PlainYearMonth::from("2025-06");
        let b = Temporal::PlainYearMonth::from("2025-06");
        a.equals(b)
        "#,
    );
    assert_eq!(v, TlangValue::Bool(true));

    let v = common::eval(
        r#"
        let a = Temporal::PlainYearMonth::from("2025-06");
        let b = Temporal::PlainYearMonth::from("2025-07");
        a.equals(b)
        "#,
    );
    assert_eq!(v, TlangValue::Bool(false));
}

// ── ZonedDateTime ───────────────────────────────────────────────────────────

#[test]
fn test_zoned_date_time_from_string() {
    let s = common::eval_to_string(
        r#"Temporal::ZonedDateTime::from("2025-03-15T14:30:00+00:00[UTC]").to_string()"#,
    );
    assert!(s.contains("2025-03-15"));
    assert!(s.contains("14:30:00"));
}

#[test]
fn test_zoned_date_time_component_accessors() {
    let v =
        common::eval(r#"Temporal::ZonedDateTime::from("2025-03-15T14:30:45+00:00[UTC]").year()"#);
    assert_eq!(v, TlangValue::I64(2025));

    let v =
        common::eval(r#"Temporal::ZonedDateTime::from("2025-03-15T14:30:45+00:00[UTC]").month()"#);
    assert_eq!(v, TlangValue::I64(3));

    let v =
        common::eval(r#"Temporal::ZonedDateTime::from("2025-03-15T14:30:45+00:00[UTC]").day()"#);
    assert_eq!(v, TlangValue::I64(15));

    let v =
        common::eval(r#"Temporal::ZonedDateTime::from("2025-03-15T14:30:45+00:00[UTC]").hour()"#);
    assert_eq!(v, TlangValue::I64(14));

    let v =
        common::eval(r#"Temporal::ZonedDateTime::from("2025-03-15T14:30:45+00:00[UTC]").minute()"#);
    assert_eq!(v, TlangValue::I64(30));

    let v =
        common::eval(r#"Temporal::ZonedDateTime::from("2025-03-15T14:30:45+00:00[UTC]").second()"#);
    assert_eq!(v, TlangValue::I64(45));
}

#[test]
fn test_zoned_date_time_offset() {
    let s = common::eval_to_string(
        r#"Temporal::ZonedDateTime::from("2025-03-15T14:30:00+00:00[UTC]").offset()"#,
    );
    assert_eq!(s, "+00:00");
}

#[test]
fn test_zoned_date_time_add() {
    let s = common::eval_to_string(
        r#"
        let zdt = Temporal::ZonedDateTime::from("2025-03-15T14:30:00+00:00[UTC]");
        let dur = Temporal::Duration::from("PT2H");
        zdt.add(dur).to_string()
        "#,
    );
    assert!(s.contains("16:30:00"));
}

#[test]
fn test_zoned_date_time_subtract() {
    let s = common::eval_to_string(
        r#"
        let zdt = Temporal::ZonedDateTime::from("2025-03-15T14:30:00+00:00[UTC]");
        let dur = Temporal::Duration::from("PT30M");
        zdt.subtract(dur).to_string()
        "#,
    );
    assert!(s.contains("14:00:00"));
}

#[test]
fn test_zoned_date_time_until() {
    let v = common::eval(
        r#"
        let a = Temporal::ZonedDateTime::from("2025-03-15T10:00:00+00:00[UTC]");
        let b = Temporal::ZonedDateTime::from("2025-03-15T14:30:00+00:00[UTC]");
        a.until(b).hours
        "#,
    );
    assert_eq!(v, TlangValue::I64(4));
}

#[test]
fn test_zoned_date_time_since() {
    let v = common::eval(
        r#"
        let a = Temporal::ZonedDateTime::from("2025-03-15T14:30:00+00:00[UTC]");
        let b = Temporal::ZonedDateTime::from("2025-03-15T10:00:00+00:00[UTC]");
        a.since(b).hours
        "#,
    );
    assert_eq!(v, TlangValue::I64(4));
}

#[test]
fn test_zoned_date_time_round() {
    let s = common::eval_to_string(
        r#"
        let zdt = Temporal::ZonedDateTime::from("2025-03-15T14:37:22+00:00[UTC]");
        zdt.round("hour").to_string()
        "#,
    );
    assert!(s.contains("15:00:00"));
}

#[test]
fn test_zoned_date_time_equals() {
    let v = common::eval(
        r#"
        let a = Temporal::ZonedDateTime::from("2025-03-15T14:30:00+00:00[UTC]");
        let b = Temporal::ZonedDateTime::from("2025-03-15T14:30:00+00:00[UTC]");
        a.equals(b)
        "#,
    );
    assert_eq!(v, TlangValue::Bool(true));
}

#[test]
fn test_zoned_date_time_to_instant() {
    let v = common::eval(
        r#"
        let zdt = Temporal::ZonedDateTime::from("2025-03-15T14:30:00+00:00[UTC]");
        zdt.to_instant().epoch_milliseconds
        "#,
    );
    assert_eq!(v, TlangValue::I64(1_742_049_000_000));
}

#[test]
fn test_zoned_date_time_to_plain_date() {
    let s = common::eval_to_string(
        r#"
        let zdt = Temporal::ZonedDateTime::from("2025-03-15T14:30:00+00:00[UTC]");
        zdt.to_plain_date().to_string()
        "#,
    );
    assert_eq!(s, "2025-03-15");
}

#[test]
fn test_zoned_date_time_to_plain_time() {
    let s = common::eval_to_string(
        r#"
        let zdt = Temporal::ZonedDateTime::from("2025-03-15T14:30:00+00:00[UTC]");
        zdt.to_plain_time().to_string()
        "#,
    );
    assert_eq!(s, "14:30:00");
}

#[test]
fn test_zoned_date_time_to_plain_date_time() {
    let s = common::eval_to_string(
        r#"
        let zdt = Temporal::ZonedDateTime::from("2025-03-15T14:30:00+00:00[UTC]");
        zdt.to_plain_date_time().to_string()
        "#,
    );
    assert_eq!(s, "2025-03-15T14:30:00");
}

// ── Temporal.Now ────────────────────────────────────────────────────────────

#[test]
fn test_now_instant() {
    let v = common::eval("Temporal::Now::instant().epoch_milliseconds");
    // Should return a positive number representing current time.
    match v {
        TlangValue::I64(ms) => assert!(ms > 0, "epoch_milliseconds should be positive"),
        _ => panic!("expected I64, got {v:?}"),
    }
}

#[test]
fn test_now_zoned_date_time_iso_with_tz() {
    let v = common::eval(r#"Temporal::Now::zoned_date_time_iso("UTC").year()"#);
    match v {
        TlangValue::I64(year) => assert!(year >= 2025, "year should be >= 2025"),
        _ => panic!("expected I64, got {v:?}"),
    }
}

#[test]
fn test_now_plain_date_time_iso_with_tz() {
    let v = common::eval(r#"Temporal::Now::plain_date_time_iso("UTC").year"#);
    match v {
        TlangValue::I64(year) => assert!(year >= 2025, "year should be >= 2025"),
        _ => panic!("expected I64, got {v:?}"),
    }
}

#[test]
fn test_now_plain_date_iso_with_tz() {
    let v = common::eval(r#"Temporal::Now::plain_date_iso("UTC").year"#);
    match v {
        TlangValue::I64(year) => assert!(year >= 2025, "year should be >= 2025"),
        _ => panic!("expected I64, got {v:?}"),
    }
}

#[test]
fn test_now_plain_time_iso_with_tz() {
    let v = common::eval(r#"Temporal::Now::plain_time_iso("UTC").hour"#);
    match v {
        TlangValue::I64(hour) => assert!((0..24).contains(&hour), "hour should be 0-23"),
        _ => panic!("expected I64, got {v:?}"),
    }
}

#[test]
fn test_now_time_zone_id() {
    let s = common::eval_to_string("Temporal::Now::time_zone_id()");
    assert_eq!(s, "UTC");
}

// ── Helpers ─────────────────────────────────────────────────────────────────

#[test]
fn test_duration_all_fields() {
    let s = common::eval_to_string(r#"Temporal::Duration::from("P1Y2M3W4DT5H6M7S").to_string()"#);
    assert_eq!(s, "P1Y2M3W4DT5H6M7S");

    let v = common::eval(r#"Temporal::Duration::from("P1Y2M3W4DT5H6M7S").years"#);
    assert_eq!(v, TlangValue::I64(1));

    let v = common::eval(r#"Temporal::Duration::from("P1Y2M3W4DT5H6M7S").weeks"#);
    assert_eq!(v, TlangValue::I64(3));

    let v = common::eval(r#"Temporal::Duration::from("P1Y2M3W4DT5H6M7S").days"#);
    assert_eq!(v, TlangValue::I64(4));

    let v = common::eval(r#"Temporal::Duration::from("P1Y2M3W4DT5H6M7S").seconds"#);
    assert_eq!(v, TlangValue::I64(7));
}

#[test]
fn test_plain_date_time_sub_second_fields() {
    let v = common::eval(
        r#"Temporal::PlainDateTime::from("2025-03-15T14:30:45.123456789").millisecond"#,
    );
    assert_eq!(v, TlangValue::I64(123));

    let v = common::eval(
        r#"Temporal::PlainDateTime::from("2025-03-15T14:30:45.123456789").microsecond"#,
    );
    assert_eq!(v, TlangValue::I64(456));

    let v = common::eval(
        r#"Temporal::PlainDateTime::from("2025-03-15T14:30:45.123456789").nanosecond"#,
    );
    assert_eq!(v, TlangValue::I64(789));
}

#[test]
fn test_plain_time_sub_second_fields() {
    let v = common::eval(r#"Temporal::PlainTime::from("14:30:45.123456789").millisecond"#);
    assert_eq!(v, TlangValue::I64(123));

    let v = common::eval(r#"Temporal::PlainTime::from("14:30:45.123456789").microsecond"#);
    assert_eq!(v, TlangValue::I64(456));

    let v = common::eval(r#"Temporal::PlainTime::from("14:30:45.123456789").nanosecond"#);
    assert_eq!(v, TlangValue::I64(789));
}

#[test]
fn test_plain_date_calendar_field() {
    let s = common::eval_to_string(r#"Temporal::PlainDate::from("2025-03-15").calendar"#);
    assert_eq!(s, "iso8601");
}
