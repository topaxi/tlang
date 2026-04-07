use crate::id::Id;

#[derive(Debug, Eq, PartialEq, Clone, Copy, Hash)]
pub struct TypeVarIdTag;

pub type TypeVarId = Id<TypeVarIdTag>;
