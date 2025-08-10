use log::debug;

#[derive(Debug, Default)]
pub struct Scope {
    bindings: Vec<String>,
}

impl Scope {
    pub(crate) fn new() -> Self {
        Self {
            bindings: Vec::new(),
        }
    }

    fn create_binding(&mut self, string: &str) {
        debug!("Creating binding: {string:?}");

        self.bindings.push(string.into());
    }

    pub(crate) fn def_local(&mut self, name: &str) {
        self.create_binding(name);
    }

    pub(crate) fn lookup(&self, name: &str) -> Option<&str> {
        self.bindings
            .iter()
            .rev()
            .find(|b| *b == name)
            .map(|s| s.as_str())
    }
}
