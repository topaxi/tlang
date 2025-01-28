use std::collections::HashMap;

#[derive(Debug, Default)]
pub struct Scope {
    bindings: HashMap<String, String>,
}

impl Scope {
    pub(crate) fn new() -> Self {
        Self {
            bindings: HashMap::new(),
        }
    }

    pub(crate) fn insert(&mut self, name: &str, binding: &str) {
        self.bindings.insert(name.to_string(), binding.to_string());
    }

    pub(crate) fn lookup(&self, name: &str) -> Option<&str> {
        self.bindings.get(name).map(|s| s.as_str())
    }
}
