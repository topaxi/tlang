use std::num::NonZero;

#[cfg(feature = "serde")]
use serde::Serialize;

#[derive(Debug, Eq, PartialEq, Clone, Copy, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct NodeId(NonZero<usize>);

impl NodeId {
    /// # Panics
    pub fn new(id: usize) -> Self {
        NodeId(NonZero::new(id).expect("NodeId must be non-zero"))
    }

    pub fn next(self) -> Self {
        NodeId(self.0.saturating_add(1))
    }
}
