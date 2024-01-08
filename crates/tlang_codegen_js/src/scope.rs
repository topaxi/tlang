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
        if !self.variables.contains_key(name) {
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
        let mut suffix = 'a';
        loop {
            let new_name = format!("{}${}", prefix, suffix);
            if !self.variables.contains_key(&new_name) {
                return new_name;
            }
            suffix = (suffix as u8 + 1) as char;
        }
    }

    pub fn declare_tmp_variable(&mut self) -> String {
        let tmp_var_name = self.get_unique_variable_name("$tmp");
        self.variables
            .insert(tmp_var_name.clone(), tmp_var_name.clone());
        tmp_var_name
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
                ("log".to_string(), "console.log".to_string()),
                ("min".to_string(), "Math.min".to_string()),
                ("max".to_string(), "Math.max".to_string()),
                ("floor".to_string(), "Math.floor".to_string()),
                ("random".to_string(), "Math.random".to_string()),
            ]),
        }
    }
}
