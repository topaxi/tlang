use crate::id::{Id, IdAllocator};

#[derive(Debug, Eq, PartialEq, Clone, Copy, Hash)]
pub struct TypeVarIdTag;

pub type TypeVarId = Id<TypeVarIdTag>;
pub type TypeVarIdAllocator = IdAllocator<TypeVarIdTag>;
