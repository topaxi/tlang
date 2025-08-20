use std::marker::PhantomData;
use std::num::NonZeroUsize;

#[cfg(feature = "serde")]
use serde::Serialize;

#[derive(Eq, PartialEq, Clone, Copy, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
#[cfg_attr(feature = "serde", serde(transparent))]
pub struct Id<T> {
    inner: NonZeroUsize,
    _marker: PhantomData<T>,
}

impl<T> Id<T> {
    /// # Panics
    pub const fn new(id: usize) -> Self {
        Id {
            inner: NonZeroUsize::new(id).expect("Id cannot be zero"),
            _marker: PhantomData,
        }
    }

    pub const fn next(self) -> Self {
        Id::new(self.inner.get().saturating_add(1))
    }
}

impl<T> std::fmt::Display for Id<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.inner.to_string())
    }
}

impl<T> std::fmt::Debug for Id<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}({})", std::any::type_name::<T>(), self.inner)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct IdAllocator<T: Copy> {
    next_id: Id<T>,
}

impl<T: Copy> IdAllocator<T> {
    /// Creates a new `IdAllocator` starting from the given `start` value.
    /// Must be greater than zero.
    /// # Panics
    pub const fn new(start: usize) -> Self {
        IdAllocator {
            next_id: Id::new(start),
        }
    }

    pub const fn next_id(&mut self) -> Id<T> {
        let id = self.next_id;
        self.next_id = self.next_id.next();
        id
    }
}

impl<T: Copy> Default for IdAllocator<T> {
    fn default() -> Self {
        Self::new(1)
    }
}
