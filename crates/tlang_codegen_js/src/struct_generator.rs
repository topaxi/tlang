use tlang_ast::node::{self as ast, Ident};

use crate::generator::CodegenJS;

impl CodegenJS {
    /// Generate the struct declaration
    ///
    /// For simplicities sake, we will generate a constructor function for the struct
    /// as we would have to track the type of variables/paths to know whether we should
    /// generate a create struct call (using `new`) or normal function call.
    pub(crate) fn generate_struct_declaration(&mut self, decl: &ast::StructDeclaration) {
        self.push_indent();
        self.push_str("function ");
        self.push_str(decl.name.as_str());
        self.push_str("Constructor(");

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

        self.push_indent();
        self.push_str("function ");
        self.push_str(decl.name.as_str());
        self.push_str("(props) {\n");
        self.inc_indent();

        self.push_indent();
        self.push_str("return new ");
        self.push_str(decl.name.as_str());
        self.push_str("Constructor(");
        for (i, (field_name, _field_type)) in decl.fields.iter().enumerate() {
            if i != 0 {
                self.push_str(", ");
            }

            self.push_str("props.");
            self.push_str(field_name.as_str());
        }
        self.push_str(");\n");
        self.push_str("}\n");
        self.dec_indent();
    }

    fn generate_struct_field(&mut self, field_name: &Ident, _field_type: &ast::Ty) {
        self.push_indent();
        self.push_str("this.");
        self.push_str(field_name.as_str());
        self.push_str(" = ");
        self.push_str(field_name.as_str());
        self.push_str(";\n");
    }
}
