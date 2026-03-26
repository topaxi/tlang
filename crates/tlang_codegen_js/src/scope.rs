use std::collections::HashMap;

use crate::builtins::JS_BUILTINS;
use crate::js;

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Scope {
    parent: Option<Box<Scope>>,

    variables: HashMap<String, String>,
}

impl Scope {
    pub(crate) fn new(parent: Option<Box<Scope>>) -> Self {
        Self {
            parent,
            variables: HashMap::new(),
        }
    }

    #[inline(always)]
    fn insert_variable(&mut self, name: &str, js_name: &str) -> String {
        let js_name = js::safe_js_variable_name(js_name);
        self.variables.insert(name.to_string(), js_name.clone());
        js_name
    }

    pub(crate) fn get_parent(&self) -> Option<&Scope> {
        self.parent.as_deref()
    }

    pub(crate) fn declare_local_variable(&mut self, name: &str) -> String {
        if !self.has_local_variable(name) {
            return self.insert_variable(name, name);
        }
        // If already declared, generate a new name with a suffix (e.g., $a, $b)
        let name_without_suffix = if name.contains('$') {
            &name[..name.rfind('$').unwrap()]
        } else {
            name
        };
        let new_name = self.get_unique_variable_name(&(name_without_suffix.to_string() + "$"));
        self.insert_variable(&new_name, &new_name);
        self.insert_variable(name, &new_name);
        new_name
    }

    pub(crate) fn declare_variable(&mut self, name: &str) -> String {
        // TODO: Local variable was enough before HIR, now HIR already gives some unique names to
        //       variables, which makes this a bit harder as we add and define more variables.
        //       Can this be solved in a better way?
        if !self.has_variable_in_scope(name) {
            self.insert_variable(name, name);

            return name.to_string();
        }
        // If already declared, generate a new name with a suffix (e.g., $a, $b)
        let name_without_suffix = if name.contains('$') {
            &name[..name.rfind('$').unwrap()]
        } else {
            name
        };
        let new_name = self.get_unique_variable_name(&(name_without_suffix.to_string() + "$"));
        self.insert_variable(&new_name, &new_name);
        self.insert_variable(name, &new_name);
        new_name
    }

    pub(crate) fn declare_variable_alias(&mut self, from: &str, to: &str) {
        self.insert_variable(from, to);
    }

    fn get_unique_variable_name(&self, prefix: &str) -> String {
        let mut suffix = 0usize;
        loop {
            let new_name = prefix.to_string() + &suffix.to_string();
            if !self.has_variable_in_scope(&new_name) {
                return new_name;
            }
            suffix += 1;
        }
    }

    pub(crate) fn declare_tmp_variable(&mut self) -> String {
        self.declare_unique_variable("$tmp$")
    }

    pub(crate) fn declare_unique_variable(&mut self, prefix: &str) -> String {
        let tmp_var_name = self.get_unique_variable_name(prefix);
        self.variables
            .insert(tmp_var_name.clone(), tmp_var_name.clone());
        tmp_var_name
    }

    /// Returns local variables declared in this scope (not inherited from parents).
    pub(crate) fn local_variables(&self) -> &HashMap<String, String> {
        &self.variables
    }

    #[inline(always)]
    pub(crate) fn has_local_variable(&self, name: &str) -> bool {
        self.variables.contains_key(name)
    }

    pub(crate) fn has_variable_in_scope(&self, name: &str) -> bool {
        if self.has_local_variable(name) {
            return true;
        }

        if let Some(parent) = &self.parent {
            return parent.has_variable_in_scope(name);
        }

        false
    }

    pub(crate) fn resolve_variable(&self, name: &str) -> Option<String> {
        if let Some(value) = self.variables.get(name) {
            return Some(value.clone());
        }

        if let Some(parent) = &self.parent {
            return parent.resolve_variable(name);
        }

        None
    }

    /// Resolve `name` only within **this** scope's local variables; does not
    /// traverse parent scopes.  Used by declaration generators to look up names
    /// that were pre-registered in the current scope without accidentally picking
    /// up builtin mappings from the global/parent scope.
    pub(crate) fn resolve_local_variable(&self, name: &str) -> Option<String> {
        self.variables.get(name).cloned()
    }
}

impl Default for Scope {
    fn default() -> Self {
        Self {
            parent: None,
            variables: JS_BUILTINS
                .iter()
                .map(|b| (b.tlang_name.to_string(), b.js_name.to_string()))
                .collect(),
        }
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    #[test]
    fn test_scope() {
        let mut scope = super::Scope::default();
        scope.declare_variable("void");

        assert_eq!(scope.resolve_variable("void"), Some("$void".to_string()));
    }
}
