use tlang_ast::node::{Ident, StructDeclaration, Ty};

use crate::generator::CodegenJS;

impl CodegenJS {
    pub(crate) fn generate_struct_declaration(&mut self, decl: &StructDeclaration) {
        self.push_indent();
        self.push_str("function ");
        self.push_str(decl.name.as_str());
        self.push_str("(");

        for (i, (field_name, _field_type)) in decl.fields.iter().enumerate() {
            if i != 0 {
                self.push_str(", ");
            }

            self.push_str(field_name.as_str());
        }

        self.push_str(") {\n");

        self.inc_indent();

        for (field_name, field_type) in decl.fields.iter() {
            self.generate_struct_field(field_name, field_type);
        }

        self.dec_indent();
        self.push_indent();
        self.push_str("}\n");
    }

    fn generate_struct_field(&mut self, field_name: &Ident, _field_type: &Ty) {
        self.push_indent();
        self.push_str("this.");
        self.push_str(field_name.as_str());
        self.push_str(" = ");
        self.push_str(field_name.as_str());
        self.push_str(";\n");
    }
}
