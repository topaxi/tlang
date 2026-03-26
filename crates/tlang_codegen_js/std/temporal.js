// temporal.js — snake_case aliases for the native JS Temporal API
//
// tlang uses snake_case; JS Temporal uses camelCase. This file installs
// aliases on the Temporal prototypes so compiled tlang code can call
// e.g. `date.day_of_week` instead of `date.dayOfWeek`.
//
// This deliberately mutates native prototypes, which is acceptable in
// the controlled tlang JS runtime environment.

if (typeof Temporal !== 'undefined') {
  // ── Helpers ──────────────────────────────────────────────────────────

  function aliasMethod(proto, snake, camel) {
    if (proto[camel] && !proto[snake]) {
      proto[snake] = proto[camel];
    }
  }

  function aliasGetter(proto, snake, camel) {
    const desc = Object.getOwnPropertyDescriptor(proto, camel);
    if (desc && desc.get && !Object.getOwnPropertyDescriptor(proto, snake)) {
      Object.defineProperty(proto, snake, { get: desc.get, configurable: true });
    }
  }

  function aliasStatic(cls, snake, camel) {
    if (cls[camel] && !cls[snake]) {
      cls[snake] = cls[camel];
    }
  }

  // ── Temporal.Now ────────────────────────────────────────────────────

  if (Temporal.Now) {
    const Now = Temporal.Now;
    Now.plain_date_time_iso = Now.plainDateTimeISO;
    Now.plain_date_iso = Now.plainDateISO;
    Now.plain_time_iso = Now.plainTimeISO;
    Now.zoned_date_time_iso = Now.zonedDateTimeISO;
    Now.time_zone_id = Now.timeZoneId;
  }

  // ── Temporal.Instant ────────────────────────────────────────────────

  if (Temporal.Instant) {
    const P = Temporal.Instant.prototype;
    aliasGetter(P, 'epoch_milliseconds', 'epochMilliseconds');
    aliasGetter(P, 'epoch_nanoseconds', 'epochNanoseconds');
    aliasMethod(P, 'to_zoned_date_time_iso', 'toZonedDateTimeISO');
    aliasMethod(P, 'to_string', 'toString');
    aliasMethod(P, 'to_json', 'toJSON');

    aliasStatic(Temporal.Instant, 'from_epoch_milliseconds', 'fromEpochMilliseconds');
    aliasStatic(Temporal.Instant, 'from_epoch_nanoseconds', 'fromEpochNanoseconds');
    Temporal.Instant.new = (ns) => new Temporal.Instant(ns);
  }

  // ── Temporal.Duration ───────────────────────────────────────────────

  if (Temporal.Duration) {
    const P = Temporal.Duration.prototype;
    aliasGetter(P, 'blank', 'blank');
    aliasMethod(P, 'negated', 'negated');
    aliasMethod(P, 'to_string', 'toString');
    aliasMethod(P, 'to_json', 'toJSON');

    Temporal.Duration.new = (...args) => new Temporal.Duration(...args);
  }

  // ── Temporal.PlainDate ──────────────────────────────────────────────

  if (Temporal.PlainDate) {
    const P = Temporal.PlainDate.prototype;
    aliasGetter(P, 'month_code', 'monthCode');
    aliasGetter(P, 'day_of_week', 'dayOfWeek');
    aliasGetter(P, 'day_of_year', 'dayOfYear');
    aliasGetter(P, 'week_of_year', 'weekOfYear');
    aliasGetter(P, 'year_of_week', 'yearOfWeek');
    aliasGetter(P, 'days_in_week', 'daysInWeek');
    aliasGetter(P, 'days_in_month', 'daysInMonth');
    aliasGetter(P, 'days_in_year', 'daysInYear');
    aliasGetter(P, 'months_in_year', 'monthsInYear');
    aliasGetter(P, 'in_leap_year', 'inLeapYear');
    aliasGetter(P, 'calendar_id', 'calendarId');

    aliasMethod(P, 'with_calendar', 'withCalendar');
    aliasMethod(P, 'to_plain_date_time', 'toPlainDateTime');
    aliasMethod(P, 'to_plain_year_month', 'toPlainYearMonth');
    aliasMethod(P, 'to_plain_month_day', 'toPlainMonthDay');
    aliasMethod(P, 'to_zoned_date_time', 'toZonedDateTime');
    aliasMethod(P, 'to_string', 'toString');
    aliasMethod(P, 'to_json', 'toJSON');

    Temporal.PlainDate.new = (y, m, d) => new Temporal.PlainDate(y, m, d);
  }

  // ── Temporal.PlainTime ──────────────────────────────────────────────

  if (Temporal.PlainTime) {
    const P = Temporal.PlainTime.prototype;
    aliasMethod(P, 'to_plain_date_time', 'toPlainDateTime');
    aliasMethod(P, 'to_string', 'toString');
    aliasMethod(P, 'to_json', 'toJSON');

    Temporal.PlainTime.new = (h, m, s) => new Temporal.PlainTime(h, m, s);
  }

  // ── Temporal.PlainDateTime ──────────────────────────────────────────

  if (Temporal.PlainDateTime) {
    const P = Temporal.PlainDateTime.prototype;
    aliasGetter(P, 'month_code', 'monthCode');
    aliasGetter(P, 'day_of_week', 'dayOfWeek');
    aliasGetter(P, 'day_of_year', 'dayOfYear');
    aliasGetter(P, 'week_of_year', 'weekOfYear');
    aliasGetter(P, 'year_of_week', 'yearOfWeek');
    aliasGetter(P, 'days_in_week', 'daysInWeek');
    aliasGetter(P, 'days_in_month', 'daysInMonth');
    aliasGetter(P, 'days_in_year', 'daysInYear');
    aliasGetter(P, 'months_in_year', 'monthsInYear');
    aliasGetter(P, 'in_leap_year', 'inLeapYear');
    aliasGetter(P, 'calendar_id', 'calendarId');

    aliasMethod(P, 'with_calendar', 'withCalendar');
    aliasMethod(P, 'with_plain_time', 'withPlainTime');
    aliasMethod(P, 'to_plain_date', 'toPlainDate');
    aliasMethod(P, 'to_plain_time', 'toPlainTime');
    aliasMethod(P, 'to_plain_year_month', 'toPlainYearMonth');
    aliasMethod(P, 'to_plain_month_day', 'toPlainMonthDay');
    aliasMethod(P, 'to_zoned_date_time', 'toZonedDateTime');
    aliasMethod(P, 'to_string', 'toString');
    aliasMethod(P, 'to_json', 'toJSON');
  }

  // ── Temporal.ZonedDateTime ──────────────────────────────────────────

  if (Temporal.ZonedDateTime) {
    const P = Temporal.ZonedDateTime.prototype;
    aliasGetter(P, 'month_code', 'monthCode');
    aliasGetter(P, 'day_of_week', 'dayOfWeek');
    aliasGetter(P, 'day_of_year', 'dayOfYear');
    aliasGetter(P, 'week_of_year', 'weekOfYear');
    aliasGetter(P, 'year_of_week', 'yearOfWeek');
    aliasGetter(P, 'days_in_week', 'daysInWeek');
    aliasGetter(P, 'days_in_month', 'daysInMonth');
    aliasGetter(P, 'days_in_year', 'daysInYear');
    aliasGetter(P, 'months_in_year', 'monthsInYear');
    aliasGetter(P, 'in_leap_year', 'inLeapYear');
    aliasGetter(P, 'calendar_id', 'calendarId');
    aliasGetter(P, 'time_zone_id', 'timeZoneId');
    aliasGetter(P, 'epoch_milliseconds', 'epochMilliseconds');
    aliasGetter(P, 'epoch_nanoseconds', 'epochNanoseconds');
    aliasGetter(P, 'offset_nanoseconds', 'offsetNanoseconds');
    aliasGetter(P, 'hours_in_day', 'hoursInDay');
    aliasGetter(P, 'start_of_day', 'startOfDay');

    aliasMethod(P, 'with_calendar', 'withCalendar');
    aliasMethod(P, 'with_time_zone', 'withTimeZone');
    aliasMethod(P, 'with_plain_time', 'withPlainTime');
    aliasMethod(P, 'to_instant', 'toInstant');
    aliasMethod(P, 'to_plain_date', 'toPlainDate');
    aliasMethod(P, 'to_plain_time', 'toPlainTime');
    aliasMethod(P, 'to_plain_date_time', 'toPlainDateTime');
    aliasMethod(P, 'to_string', 'toString');
    aliasMethod(P, 'to_json', 'toJSON');
  }

  // ── Temporal.PlainYearMonth ─────────────────────────────────────────

  if (Temporal.PlainYearMonth) {
    const P = Temporal.PlainYearMonth.prototype;
    aliasGetter(P, 'month_code', 'monthCode');
    aliasGetter(P, 'days_in_month', 'daysInMonth');
    aliasGetter(P, 'days_in_year', 'daysInYear');
    aliasGetter(P, 'months_in_year', 'monthsInYear');
    aliasGetter(P, 'in_leap_year', 'inLeapYear');
    aliasGetter(P, 'calendar_id', 'calendarId');

    aliasMethod(P, 'to_plain_date', 'toPlainDate');
    aliasMethod(P, 'to_string', 'toString');
    aliasMethod(P, 'to_json', 'toJSON');

    Temporal.PlainYearMonth.new = (y, m) => new Temporal.PlainYearMonth(y, m);
  }

  // ── Temporal.PlainMonthDay ──────────────────────────────────────────

  if (Temporal.PlainMonthDay) {
    const P = Temporal.PlainMonthDay.prototype;
    aliasGetter(P, 'month_code', 'monthCode');
    aliasGetter(P, 'calendar_id', 'calendarId');

    aliasMethod(P, 'to_plain_date', 'toPlainDate');
    aliasMethod(P, 'to_string', 'toString');
    aliasMethod(P, 'to_json', 'toJSON');

    Temporal.PlainMonthDay.new = (m, d) => new Temporal.PlainMonthDay(m, d);
  }
}
