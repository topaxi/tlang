use std::num::NonZero;

#[cfg(feature = "serde")]
use serde::Serialize;

#[derive(Debug, Eq, PartialEq, Clone, Copy, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct HirId(NonZero<usize>);

impl HirId {
    /// # Panics
    pub fn new(id: usize) -> Self {
        HirId(NonZero::new(id).expect("HirId must be non-zero"))
    }

    pub fn next(self) -> Self {
        HirId(self.0.saturating_add(1))
    }
}
