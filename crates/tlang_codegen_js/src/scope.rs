use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct Scope {
    parent: Option<Box<Scope>>,

    variables: HashMap<String, String>,
}

impl Scope {
    pub fn new(parent: Option<Box<Scope>>) -> Self {
        Self {
            parent,
            variables: HashMap::new(),
        }
    }

    pub fn get_parent(&self) -> Option<&Scope> {
        self.parent.as_deref()
    }

    pub fn declare_variable(&mut self, name: &str) -> String {
        if !self.has_local_variable(name) {
            self.variables.insert(name.to_string(), name.to_string());

            return name.to_string();
        }
        // If already declared, generate a new name with a suffix (e.g., $a, $b)
        let new_name = self.get_unique_variable_name(name);
        self.variables.insert(name.to_string(), new_name.clone());
        new_name
    }

    pub fn declare_variable_alias(&mut self, name: &str, alias: &str) {
        self.variables.insert(name.to_string(), alias.to_string());
    }

    fn get_unique_variable_name(&self, prefix: &str) -> String {
        let mut suffix = 0;
        loop {
            let new_name = format!("{prefix}${suffix}");
            if !self.has_variable_in_scope(&new_name) {
                return new_name;
            }
            suffix += 1;
        }
    }

    pub fn declare_tmp_variable(&mut self) -> String {
        self.declare_unique_variable("$tmp")
    }

    pub fn declare_unique_variable(&mut self, prefix: &str) -> String {
        let tmp_var_name = self.get_unique_variable_name(prefix);
        self.variables
            .insert(tmp_var_name.clone(), tmp_var_name.clone());
        tmp_var_name
    }

    #[inline(always)]
    pub fn has_local_variable(&self, name: &str) -> bool {
        self.variables.contains_key(name)
    }

    pub fn has_variable_in_scope(&self, name: &str) -> bool {
        if self.has_local_variable(name) {
            return true;
        }

        if let Some(parent) = &self.parent {
            return parent.has_variable_in_scope(name);
        }

        false
    }

    pub fn resolve_variable(&self, name: &str) -> Option<String> {
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
                ("min".to_string(), "Math.min".to_string()),
                ("max".to_string(), "Math.max".to_string()),
                ("floor".to_string(), "Math.floor".to_string()),
                ("random".to_string(), "Math.random".to_string()),
            ]),
        }
    }
}
