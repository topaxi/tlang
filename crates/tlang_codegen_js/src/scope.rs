use std::collections::HashMap;

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

    pub(crate) fn get_parent(&self) -> Option<&Scope> {
        self.parent.as_deref()
    }

    pub(crate) fn declare_variable(&mut self, name: &str) -> String {
        if !self.has_local_variable(name) {
            self.variables.insert(name.to_string(), name.to_string());

            return name.to_string();
        }
        // If already declared, generate a new name with a suffix (e.g., $a, $b)
        let new_name = self.get_unique_variable_name(&(name.to_string() + "$"));
        self.variables.insert(name.to_string(), new_name.clone());
        new_name
    }

    pub(crate) fn declare_variable_alias(&mut self, from: &str, to: &str) {
        self.variables.insert(from.to_string(), to.to_string());
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
}

impl Default for Scope {
    fn default() -> Self {
        Self {
            parent: None,
            variables: HashMap::from_iter(vec![
                ("Some".to_string(), "Option::Some".to_string()),
                ("None".to_string(), "Option::None".to_string()),
                ("Ok".to_string(), "Result::Ok".to_string()),
                ("Err".to_string(), "Result::Err".to_string()),
                ("log".to_string(), "console.log".to_string()),
                ("math".to_string(), "Math".to_string()),
                ("math::min".to_string(), "Math.min".to_string()),
                ("math::max".to_string(), "Math.max".to_string()),
                ("math::floor".to_string(), "Math.floor".to_string()),
                ("math::random".to_string(), "Math.random".to_string()),
            ]),
        }
    }
}
