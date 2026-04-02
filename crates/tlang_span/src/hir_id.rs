use crate::id::{Id, IdAllocator};

#[derive(Debug, Eq, PartialEq, Clone, Copy, Hash)]
pub struct HirIdTag;

pub type HirId = Id<HirIdTag>;
pub type HirIdAllocator = IdAllocator<HirIdTag>;

/// Number of HirId slots reserved per module in a multi-module compilation.
///
/// Each module receives a fixed block of this size, starting at
/// `1 + module_index * HIRS_PER_MODULE`. This ensures HirIds are stable
/// across incremental rebuilds: adding, removing, or resizing an unrelated
/// module does not shift the HirId range of any other module.
///
/// The limit is intentionally generous — tlang's largest real-world modules
/// are well under 10 K HIR nodes. An overflow is detected at compile time
/// and reported as a hard error.
pub const HIRS_PER_MODULE: usize = 1_000_000;
