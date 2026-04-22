use tlang_hir::TyKind;

#[derive(Debug, Clone)]
pub struct BuiltinField {
    pub name: &'static str,
    pub ty: TyKind,
}

fn lookup_fields(type_name: &str) -> &'static [(&'static str, fn() -> TyKind)] {
    use TyKind::Primitive;
    use tlang_hir::PrimTy::{Bool, I64, String as TString};

    match type_name {
        "Regex" => &[
            ("source", || Primitive(TString)),
            ("flags", || Primitive(TString)),
        ],
        "Instant" | "Temporal::Instant" => &[
            ("epoch_nanoseconds", || Primitive(I64)),
            ("epoch_milliseconds", || Primitive(I64)),
        ],
        "Duration" | "Temporal::Duration" => &[
            ("years", || Primitive(I64)),
            ("months", || Primitive(I64)),
            ("weeks", || Primitive(I64)),
            ("days", || Primitive(I64)),
            ("hours", || Primitive(I64)),
            ("minutes", || Primitive(I64)),
            ("seconds", || Primitive(I64)),
            ("milliseconds", || Primitive(I64)),
            ("microseconds", || Primitive(I64)),
            ("nanoseconds", || Primitive(I64)),
            ("sign", || Primitive(I64)),
            ("blank", || Primitive(Bool)),
        ],
        "PlainDate" | "Temporal::PlainDate" => &[
            ("year", || Primitive(I64)),
            ("month", || Primitive(I64)),
            ("day", || Primitive(I64)),
            ("calendar", || Primitive(TString)),
        ],
        "PlainDateTime" | "Temporal::PlainDateTime" => &[
            ("year", || Primitive(I64)),
            ("month", || Primitive(I64)),
            ("day", || Primitive(I64)),
            ("hour", || Primitive(I64)),
            ("minute", || Primitive(I64)),
            ("second", || Primitive(I64)),
            ("millisecond", || Primitive(I64)),
            ("microsecond", || Primitive(I64)),
            ("nanosecond", || Primitive(I64)),
            ("calendar", || Primitive(TString)),
        ],
        "PlainMonthDay" | "Temporal::PlainMonthDay" => &[
            ("month_code", || Primitive(TString)),
            ("day", || Primitive(I64)),
            ("calendar", || Primitive(TString)),
        ],
        "PlainTime" | "Temporal::PlainTime" => &[
            ("hour", || Primitive(I64)),
            ("minute", || Primitive(I64)),
            ("second", || Primitive(I64)),
            ("millisecond", || Primitive(I64)),
            ("microsecond", || Primitive(I64)),
            ("nanosecond", || Primitive(I64)),
        ],
        "PlainYearMonth" | "Temporal::PlainYearMonth" => &[
            ("year", || Primitive(I64)),
            ("month", || Primitive(I64)),
            ("calendar", || Primitive(TString)),
        ],
        "ZonedDateTime" | "Temporal::ZonedDateTime" => &[
            ("epoch_nanoseconds", || Primitive(I64)),
            ("timezone", || Primitive(TString)),
            ("calendar", || Primitive(TString)),
        ],
        _ => &[],
    }
}

pub fn lookup(type_name: &str, field_name: &str) -> Option<TyKind> {
    lookup_fields(type_name)
        .iter()
        .find(|(name, _)| *name == field_name)
        .map(|(_, mk_ty)| mk_ty())
}

pub fn fields_for(type_name: &str) -> Vec<BuiltinField> {
    lookup_fields(type_name)
        .iter()
        .map(|(name, mk_ty)| BuiltinField { name, ty: mk_ty() })
        .collect()
}
