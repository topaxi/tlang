use std::collections::HashSet;

use tlang_ast::node::Ident;
use tlang_hir::hir;

use crate::generator::CodegenJS;

impl CodegenJS {
    pub(crate) fn generate_enum_declaration(&mut self, decl: &hir::EnumDeclaration) {
        self.push_indent();
        self.push_str(&format!("class {} {{\n", decl.name));
        self.inc_indent();
        self.push_indent();
        self.push_str("tag = this;\n");

        let mut seen = HashSet::new();
        let field_names = decl
            .variants
            .iter()
            .flat_map(|variant| variant.parameters.iter())
            .map(|field| field.name.as_str())
            .filter(|name| seen.insert(name.to_string()))
            .collect::<Vec<_>>();

        for field_name in field_names {
            self.push_indent();
            if field_name.chars().all(char::is_numeric) {
                self.push_str(&format!("[{field_name}];\n"));
            } else {
                self.push_str(&format!("{field_name};\n"));
            }
        }

        for variant in &decl.variants {
            self.generate_enum_variant(&variant.name, &variant.parameters);
        }

        self.dec_indent();
        self.push_indent();
        self.push_str("}\n");
    }

    fn is_numeric(str: &str) -> bool {
        str.chars().all(char::is_numeric)
    }

    fn infer_parameter_name_from_type(&mut self, ty: &hir::Ty) -> String {
        match &ty.kind {
            hir::TyKind::Path(path) => self.current_scope().declare_variable(&path.join("_")),
            _ => self.current_scope().declare_variable("arg"),
        }
    }

    fn generate_enum_variant(&mut self, name: &Ident, parameters: &[hir::StructField]) {
        self.push_scope();
        self.push_indent();

        let named_fields = parameters
            .iter()
            .any(|param| !Self::is_numeric(param.name.as_str()));

        let parameter_names = if named_fields {
            parameters
                .iter()
                .map(|param| param.name.to_string())
                .collect::<Vec<_>>()
        } else {
            parameters
                .iter()
                .map(|param| self.infer_parameter_name_from_type(&param.ty))
                .collect::<Vec<_>>()
        };

        if parameters.is_empty() {
            self.push_str(&format!("static {name} = new this;\n"));
            self.pop_scope();
            return;
        }

        self.push_str(&format!("static {name} = ("));
        if named_fields {
            self.push_str("{ ");
        }
        for (i, parameter_name) in parameter_names.iter().enumerate() {
            if i > 0 {
                self.push_str(", ");
            }
            self.push_str(parameter_name);
        }
        if named_fields {
            self.push_str(" }");
        }
        self.push_str(&format!(") => Object.assign(new this, {{ tag: this.{name}"));

        for (i, param) in parameters.iter().enumerate() {
            self.push_str(", ");

            let parameter_name = parameter_names[i].as_str();
            let param_field_name = param.name.as_str();

            if parameter_name != param_field_name {
                if param.name.as_str().chars().all(char::is_numeric) {
                    self.push_str(&format!("[{param_field_name}]: "))
                } else {
                    self.push_str(&format!("{param_field_name}: "));
                }
            }

            self.push_str(parameter_name);
        }

        self.push_str(" });\n");
        self.pop_scope();
    }
}
