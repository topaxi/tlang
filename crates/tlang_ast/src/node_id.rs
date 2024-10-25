use serde::Serialize;

#[derive(Debug, Default, Eq, PartialEq, Clone, Copy, Serialize, Hash)]
pub struct NodeId(usize);

impl NodeId {
    pub fn new(id: usize) -> Self {
        NodeId(id)
    }

    pub fn next(&self) -> Self {
        NodeId(self.0 + 1)
    }
}
