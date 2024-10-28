use tlang_ast::node::{self as ast, Ident};

use crate::generator::CodegenJS;

impl CodegenJS {
    pub(crate) fn generate_enum_declaration(&mut self, decl: &ast::EnumDeclaration) {
        self.push_indent();
        self.push_str(&format!("const {} = {{\n", decl.name));
        self.inc_indent();
        for variant in &decl.variants {
            self.generate_enum_variant(&variant.name, variant.named_fields, &variant.parameters);
        }
        self.dec_indent();
        self.push_indent();
        self.push_str("};\n");
    }

    fn generate_enum_variant(&mut self, name: &Ident, named_fields: bool, parameters: &[Ident]) {
        self.push_indent();

        if parameters.is_empty() {
            self.push_str(&format!("{name}: {{ tag: \"{name}\" }},\n"));
            return;
        }

        self.push_str(&format!("{name}("));
        if named_fields {
            self.push_str("{ ");
        }
        for (i, param) in parameters.iter().enumerate() {
            if i > 0 {
                self.push_str(", ");
            }
            self.push_str(param.as_str());
        }
        if named_fields {
            self.push_str(" }");
        }
        self.push_str(") {\n");
        self.inc_indent();
        self.push_indent();
        self.push_str("return {\n");
        self.inc_indent();
        self.push_indent();
        self.push_str(format!("tag: \"{name}\",\n").as_str());

        if named_fields {
            for param in parameters {
                self.push_indent();
                self.push_str(param.as_str());
                self.push_str(",\n");
            }
        } else {
            for (i, param) in parameters.iter().enumerate() {
                self.push_indent();
                self.push_str(&format!("[{i}]: "));
                self.push_str(param.as_str());
                self.push_str(",\n");
            }
        }

        self.dec_indent();
        self.push_indent();
        self.push_str("};\n");
        self.dec_indent();
        self.push_indent();
        self.push_str("},\n");
    }
}
