use std::collections::HashMap;
use std::fmt;
use std::num::NonZeroU32;
use std::sync::{LazyLock, Mutex};

#[cfg(feature = "serde")]
use serde::Serializer;

/// A compact, `Copy`-able handle to an interned string.
///
/// Backed by a `NonZeroU32` so it fits in four bytes and an `Option<Symbol>`
/// still fits in four bytes.
///
/// The name "Symbol" follows rustc and browser-engine (Firefox/Gecko, WebKit, Servo)
/// conventions for interned, deduplicated strings.
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct Symbol(NonZeroU32);

struct Interner {
    /// Indexed by `Symbol.0.get() - 1`. Entries are never removed or moved.
    strings: Vec<Box<str>>,
    /// Reverse map for deduplication. Keys are `&'static str` slices that
    /// point into the stable heap allocations owned by `strings`.
    map: HashMap<&'static str, Symbol>,
}

impl Interner {
    fn new() -> Self {
        Interner {
            strings: Vec::new(),
            map: HashMap::new(),
        }
    }

    fn intern(&mut self, s: &str) -> Symbol {
        if let Some(&id) = self.map.get(s) {
            return id;
        }
        // IDs are 1-based so that NonZeroU32 is satisfied.
        let idx = self.strings.len() as u32 + 1;
        let id = Symbol(NonZeroU32::new(idx).expect("interner overflow"));
        let boxed: Box<str> = s.into();
        // SAFETY: `key` is a `&str` slice pointing into the heap allocation
        // owned by `boxed`.  `Box<str>` stores its contents at a stable heap
        // address; pushing the `Box` into `self.strings` only copies the fat
        // pointer (not the string bytes), so `key` remains valid after the
        // push.  `INTERNER` is a `static` whose `Interner` is never dropped
        // or replaced, so the allocation outlives the program and the
        // transmute to `&'static str` is sound.
        let key: &'static str = unsafe { std::mem::transmute::<&str, &'static str>(&*boxed) };
        self.strings.push(boxed);
        self.map.insert(key, id);
        id
    }

    fn get(&self, id: Symbol) -> &str {
        &self.strings[(id.0.get() - 1) as usize]
    }
}

static INTERNER: LazyLock<Mutex<Interner>> = LazyLock::new(|| Mutex::new(Interner::new()));

/// Intern a string, returning a deduplicated `Symbol`.
///
/// # Panics
///
/// Panics if the interner mutex is poisoned.
pub fn intern(s: &str) -> Symbol {
    INTERNER.lock().unwrap().intern(s)
}

/// Retrieve the string behind a `Symbol`.
///
/// The returned `&'static str` points into a heap-allocated `Box<str>` that is
/// stored in the global `INTERNER` and is never moved or deallocated.  The
/// lifetime transmute is therefore sound.
///
/// # Panics
///
/// Panics if the interner mutex is poisoned.
pub fn get(id: Symbol) -> &'static str {
    let guard = INTERNER.lock().unwrap();
    let s: &str = guard.get(id);
    // SAFETY: The Box<str> allocation is stable (Vec growth moves the Vec's
    // own pointer, not the box contents) and lives for the program lifetime.
    unsafe { std::mem::transmute::<&str, &'static str>(s) }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(get(*self))
    }
}

#[cfg(feature = "serde")]
impl serde::Serialize for Symbol {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_str(get(*self))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_dedup() {
        assert_eq!(intern("sym_foo_dedup"), intern("sym_foo_dedup"));
    }

    #[test]
    fn test_distinct() {
        assert_ne!(intern("sym_foo_distinct_a"), intern("sym_bar_distinct_b"));
    }

    #[test]
    fn test_round_trip() {
        let id = intern("sym_hello_round_trip");
        assert_eq!(get(id), "sym_hello_round_trip");
    }

    #[test]
    fn test_empty_string() {
        let id = intern("");
        assert_eq!(get(id), "");
    }

    #[test]
    fn test_thread_safety() {
        use std::thread;

        let handles: Vec<_> = (0..4)
            .map(|i| {
                thread::spawn(move || {
                    let s = format!("sym_thread_intern_{i}");
                    let id = intern(&s);
                    assert_eq!(get(id), s.as_str());
                })
            })
            .collect();

        for h in handles {
            h.join().unwrap();
        }
    }
}
