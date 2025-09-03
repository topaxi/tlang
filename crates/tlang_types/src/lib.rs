#[cfg(feature = "serde")]
use serde::Serialize;
use tlang_symbols::SymbolId;

#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum Type {
    Nil,
    Bool,

    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,

    F32,
    F64,

    Struct(SymbolId),
    Enum(SymbolId),

    Fn(FnType),

    TypeParam(SymbolId),

    #[default]
    Unknown,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct FnType {
    pub params: Vec<Type>,
    pub ret: Box<Type>,
}
