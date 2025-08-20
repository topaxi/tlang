use crate::id::{Id, IdAllocator};

#[derive(Debug, Eq, PartialEq, Clone, Copy, Hash)]
pub struct NodeIdTag;

pub type NodeId = Id<NodeIdTag>;

#[derive(Debug, Clone, Copy)]
pub struct NodeIdAllocator(IdAllocator<NodeIdTag>);

impl NodeIdAllocator {
    pub const fn new(start: usize) -> Self {
        NodeIdAllocator(IdAllocator::new(start))
    }

    pub const fn next_id(&mut self) -> NodeId {
        self.0.next_id()
    }
}

impl Default for NodeIdAllocator {
    fn default() -> Self {
        // Note: The default start value is 2 to avoid conflicts with the root node ID.
        NodeIdAllocator::new(2)
    }
}
