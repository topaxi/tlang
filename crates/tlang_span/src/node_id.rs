use crate::id::{Id, IdAllocator};

#[derive(Debug, Eq, PartialEq, Clone, Copy, Hash)]
pub struct NodeIdTag;

pub type NodeId = Id<NodeIdTag>;
pub type NodeIdAllocator = IdAllocator<NodeIdTag>;
