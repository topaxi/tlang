use std::collections::{HashMap, HashSet};

use tlang_span::HirId;

use crate::js;

/// A flat `HirId`-keyed name map that replaces the old string-based `Scope`
/// chain for identifier resolution.
///
/// **Resolution** is O(1) via `resolve(hir_id)`.  
/// **Name allocation** uses a lightweight scope stack to decide when JS names
/// can be reused after inner block scopes are popped (matching JS `let`
/// block-scoping semantics).
///
/// Builtin names (no `HirId`) are handled separately via `builtins::lookup`.
#[derive(Debug)]
pub(crate) struct NameMap {
    /// HirId → allocated JS identifier name.  Never shrinks — every
    /// declaration registered during codegen stays accessible for the lifetime
    /// of the compilation unit.
    bindings: HashMap<HirId, String>,

    /// Reverse map: source name → most recently registered JS name.
    /// Used as a fallback when HirId-based resolution is unavailable (e.g.
    /// protocol default method bodies where the HIR doesn't carry resolution).
    name_to_js: HashMap<String, String>,

    /// Stack of allocation scopes.  Each set tracks the JS names introduced
    /// at that scope level.  `push_scope` / `pop_scope` add / remove levels,
    /// enabling name reuse after inner block scopes are popped.
    scope_stack: Vec<HashSet<String>>,

    /// Monotonic counter for `$tmp$N` temporaries.
    tmp_counter: usize,
}

impl NameMap {
    pub fn new() -> Self {
        Self {
            bindings: HashMap::new(),
            name_to_js: HashMap::new(),
            // Start with one root scope level.
            scope_stack: vec![HashSet::new()],
            tmp_counter: 0,
        }
    }

    // -- Scope management ---------------------------------------------------

    /// Enter a new allocation scope (function body, block, match arm, …).
    pub fn push_scope(&mut self) {
        self.scope_stack.push(HashSet::new());
    }

    /// Leave the current allocation scope, releasing its names so they may be
    /// reused by subsequent sibling scopes.  The `HirId → String` bindings
    /// remain (resolution is flat and permanent).
    pub fn pop_scope(&mut self) {
        // Never pop the root scope.
        if self.scope_stack.len() > 1 {
            self.scope_stack.pop();
        }
    }

    // -- Registration (declaration sites) -----------------------------------

    /// Register a declaration `HirId` with its source name.  Returns the
    /// allocated JS name (safe for JS output, deduplicated against **all**
    /// visible scope levels).
    pub fn register(&mut self, hir_id: HirId, source_name: &str) -> String {
        if let Some(existing) = self.bindings.get(&hir_id) {
            return existing.clone();
        }
        let js_name = self.allocate_unique_name(source_name);
        self.bindings.insert(hir_id, js_name.clone());
        self.name_to_js
            .insert(source_name.to_string(), js_name.clone());
        js_name
    }

    /// Register a declaration checking only the **current** scope level for
    /// collisions (not parent scopes).  Used for function parameters and
    /// pre-registered declarations that intentionally shadow parent names.
    pub fn register_local(&mut self, hir_id: HirId, source_name: &str) -> String {
        if let Some(existing) = self.bindings.get(&hir_id) {
            return existing.clone();
        }
        let js_name = self.allocate_unique_name_local(source_name);
        self.bindings.insert(hir_id, js_name.clone());
        self.name_to_js
            .insert(source_name.to_string(), js_name.clone());
        js_name
    }

    /// Register a `HirId` with an exact JS name (no dedup, no `safe_js`).
    /// Used for special mappings like `self` → `this`.
    pub fn register_exact(&mut self, hir_id: HirId, js_name: &str) {
        let name = js_name.to_string();
        self.current_scope_mut().insert(name.clone());
        self.bindings.insert(hir_id, name);
    }

    /// Check whether a `HirId` is already registered.
    pub fn has(&self, hir_id: HirId) -> bool {
        self.bindings.contains_key(&hir_id)
    }

    // -- Resolution (use sites) ---------------------------------------------

    /// Resolve a `HirId` to its allocated JS name.
    pub fn resolve(&self, hir_id: HirId) -> Option<&str> {
        self.bindings.get(&hir_id).map(|s| s.as_str())
    }

    /// Fallback name-based resolution.  Returns the most recently registered
    /// JS name for the given source name.  Used when HirId-based resolution
    /// is unavailable (e.g. protocol default method bodies where the HIR
    /// doesn't carry resolution info).
    pub fn resolve_by_name(&self, source_name: &str) -> Option<&str> {
        self.name_to_js.get(source_name).map(|s| s.as_str())
    }

    // -- Synthetic names (no HirId) -----------------------------------------

    /// Allocate a unique JS name without a `HirId`.  Used for codegen-only
    /// names like enum variant constructor parameters.
    pub fn alloc_name(&mut self, source_name: &str) -> String {
        self.allocate_unique_name(source_name)
    }

    /// Allocate the next `$tmp$N` variable.
    pub fn alloc_tmp(&mut self) -> String {
        let name = format!("$tmp${}", self.tmp_counter);
        self.tmp_counter += 1;
        self.current_scope_mut().insert(name.clone());
        name
    }

    /// Allocate a unique variable with a custom prefix (`_0`, `_1`, …).
    pub fn alloc_unique(&mut self, prefix: &str) -> String {
        let mut suffix = 0usize;
        loop {
            let candidate = format!("{prefix}{suffix}");
            if !self.is_name_in_any_scope(&candidate) {
                self.current_scope_mut().insert(candidate.clone());
                return candidate;
            }
            suffix += 1;
        }
    }

    // -- Internals ----------------------------------------------------------

    fn current_scope_mut(&mut self) -> &mut HashSet<String> {
        self.scope_stack
            .last_mut()
            .expect("scope_stack is never empty")
    }

    /// Is `name` present in **any** scope level?
    fn is_name_in_any_scope(&self, name: &str) -> bool {
        self.scope_stack.iter().any(|s| s.contains(name))
    }

    /// Is `name` present in the **current** (top) scope level only?
    fn is_name_in_current_scope(&self, name: &str) -> bool {
        self.scope_stack.last().is_some_and(|s| s.contains(name))
    }

    /// Allocate a unique JS-safe name, deduplicating against **all** scope levels.
    fn allocate_unique_name(&mut self, source_name: &str) -> String {
        let safe = js::safe_js_variable_name(source_name);
        if !self.is_name_in_any_scope(&safe) {
            self.current_scope_mut().insert(safe.clone());
            return safe;
        }
        self.allocate_with_suffix(&safe)
    }

    /// Allocate a unique JS-safe name, deduplicating against the **current**
    /// scope level only.  Parent scope names are intentionally allowed to be
    /// shadowed (e.g. function parameters that reuse a name from an outer scope).
    fn allocate_unique_name_local(&mut self, source_name: &str) -> String {
        let safe = js::safe_js_variable_name(source_name);
        if !self.is_name_in_current_scope(&safe) {
            self.current_scope_mut().insert(safe.clone());
            return safe;
        }
        self.allocate_with_suffix(&safe)
    }

    /// Shared suffix-generation logic: strip any existing `$`-suffix from the
    /// base, then try `base$0`, `base$1`, … until one is free in all scopes.
    fn allocate_with_suffix(&mut self, safe_name: &str) -> String {
        let base = if let Some(pos) = safe_name.rfind('$') {
            &safe_name[..pos]
        } else {
            safe_name
        };
        let prefix = format!("{base}$");

        let mut suffix = 0usize;
        loop {
            let candidate = format!("{prefix}{suffix}");
            if !self.is_name_in_any_scope(&candidate) {
                self.current_scope_mut().insert(candidate.clone());
                return candidate;
            }
            suffix += 1;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn hir(n: usize) -> HirId {
        HirId::new(n)
    }

    #[test]
    fn register_returns_safe_js_name() {
        let mut map = NameMap::new();
        assert_eq!(map.register(hir(1), "void"), "$void");
    }

    #[test]
    fn register_deduplicates_same_scope() {
        let mut map = NameMap::new();
        assert_eq!(map.register(hir(1), "x"), "x");
        assert_eq!(map.register(hir(2), "x"), "x$0");
        assert_eq!(map.register(hir(3), "x"), "x$1");
    }

    #[test]
    fn register_deduplicates_across_scopes() {
        let mut map = NameMap::new();
        assert_eq!(map.register(hir(1), "x"), "x");
        map.push_scope();
        // "x" is visible from parent scope → dedup
        assert_eq!(map.register(hir(2), "x"), "x$0");
        map.pop_scope();
    }

    #[test]
    fn register_local_allows_shadowing() {
        let mut map = NameMap::new();
        assert_eq!(map.register(hir(1), "x"), "x");
        map.push_scope();
        // register_local checks only current scope → "x" is free here
        assert_eq!(map.register_local(hir(2), "x"), "x");
        map.pop_scope();
    }

    #[test]
    fn pop_scope_allows_name_reuse() {
        let mut map = NameMap::new();
        map.push_scope();
        assert_eq!(map.register(hir(1), "tmp"), "tmp");
        map.pop_scope();
        map.push_scope();
        // "tmp" was in the popped scope → available again
        assert_eq!(map.register(hir(2), "tmp"), "tmp");
        map.pop_scope();
    }

    #[test]
    fn resolve_returns_registered_name() {
        let mut map = NameMap::new();
        map.register(hir(1), "x");
        map.register(hir(2), "x");
        assert_eq!(map.resolve(hir(1)), Some("x"));
        assert_eq!(map.resolve(hir(2)), Some("x$0"));
        assert_eq!(map.resolve(hir(3)), None);
    }

    #[test]
    fn register_exact_stores_literal_name() {
        let mut map = NameMap::new();
        map.register_exact(hir(1), "this");
        assert_eq!(map.resolve(hir(1)), Some("this"));
    }

    #[test]
    fn alloc_tmp_increments() {
        let mut map = NameMap::new();
        assert_eq!(map.alloc_tmp(), "$tmp$0");
        assert_eq!(map.alloc_tmp(), "$tmp$1");
    }

    #[test]
    fn alloc_unique_skips_taken_names() {
        let mut map = NameMap::new();
        map.register(hir(1), "_0");
        assert_eq!(map.alloc_unique("_"), "_1");
    }

    #[test]
    fn idempotent_double_register() {
        let mut map = NameMap::new();
        assert_eq!(map.register(hir(1), "x"), "x");
        // Same HirId, same result — no dedup
        assert_eq!(map.register(hir(1), "x"), "x");
    }
}
