use crate::id::{Id, IdAllocator};

#[derive(Debug, Eq, PartialEq, Clone, Copy, Hash)]
pub struct HirIdTag;

pub type HirId = Id<HirIdTag>;
pub type HirIdAllocator = IdAllocator<HirIdTag>;
