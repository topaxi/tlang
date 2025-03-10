#[macro_export]
macro_rules! impl_from_tlang_value {
    ($($t:ty => $variant:ident),* $(,)?) => {
        $(
            impl From<$t> for TlangValue {
                fn from(value: $t) -> Self {
                    TlangValue::$variant(value as _)
                }
            }
        )*
    };
}
