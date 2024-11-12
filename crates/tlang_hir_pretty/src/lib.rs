use tlang_hir::hir;

pub struct HirPretty {
    output: String,
    indent_level: usize,
}

impl Default for HirPretty {
    fn default() -> Self {
        Self::new()
    }
}

impl HirPretty {
    fn new() -> Self {
        Self {
            output: String::new(),
            indent_level: 0,
        }
    }

    pub fn pretty_print(hir: &hir::Module) -> String {
        let mut pretty_printer = HirPretty::new();
        pretty_printer.print_module(hir);
        pretty_printer.output
    }

    fn push_str(&mut self, string: &str) {
        self.output.push_str(string);
    }

    fn push_char(&mut self, ch: char) {
        self.output.push(ch);
    }

    fn inc_indent(&mut self) {
        self.indent_level += 1;
    }

    fn dec_indent(&mut self) {
        self.indent_level -= 1;
    }

    fn push_indent(&mut self) {
        for _ in 0..self.indent_level {
            self.push_str("    ");
        }
    }

    fn push_newline(&mut self) {
        self.push_char('\n');
    }

    fn print_module(&mut self, module: &hir::Module) {
        self.print_stmts(&module.block.stmts);
    }

    fn print_block(&mut self, block: &hir::Block) {
        self.push_char('{');
        self.push_newline();
        self.inc_indent();
        self.print_stmts(&block.stmts);
        self.dec_indent();
        self.push_char('}');
        self.push_newline();
    }

    fn print_stmts(&mut self, stmts: &[hir::Stmt]) {
        for stmt in stmts {
            self.print_stmt(stmt);
        }
    }

    fn print_stmt(&mut self, stmt: &hir::Stmt) {
        self.push_indent();

        match &stmt.kind {
            hir::StmtKind::Expr(expr) => self.print_expr(expr),
            hir::StmtKind::FunctionDeclaration(decl) => self.print_function_declaration(decl),
            hir::StmtKind::Let(pat, expr, ty) => self.print_variable_declaration(pat, expr, ty),
            hir::StmtKind::Return(expr) => {
                self.push_str("return");

                if let Some(expr) = expr.as_ref() {
                    self.push_str(" ");
                    self.print_expr(expr);
                }
            }
            hir::StmtKind::None => {}
            _ => self.push_str("TODO"),
        }

        self.push_char(';');
        self.push_newline();
    }

    fn print_expr(&mut self, expr: &hir::Expr) {
        match &expr.kind {
            hir::ExprKind::Block(block) => self.print_block(block),
            _ => self.push_str("TODO"),
        }
    }

    fn print_function_declaration(&mut self, decl: &hir::FunctionDeclaration) {
        self.push_str("fn ");
        self.print_expr(&decl.name);
        self.push_str("(");
        for (i, param) in decl.parameters.iter().enumerate() {
            if i > 0 {
                self.push_str(", ");
            }

            self.print_pat(&param.pattern);
        }
        self.push_str(") -> ");
        self.print_ty(&decl.return_type);
        self.push_char(' ');
        self.print_block(&decl.body);
    }

    fn print_variable_declaration(&mut self, pat: &hir::Pat, expr: &hir::Expr, ty: &hir::Ty) {
        self.push_str("let ");
        self.print_pat(pat);
        self.push_str(": ");
        self.print_ty(ty);
        self.push_str(" = ");
        self.print_expr(expr);
    }

    fn print_pat(&mut self, pat: &hir::Pat) {
        match &pat.kind {
            hir::PatKind::Identifier(_id, name) => self.push_str(name.as_str()),
            _ => self.push_str("TODO"),
        }
    }

    fn print_ty(&mut self, ty: &hir::Ty) {
        match &ty.kind {
            hir::TyKind::Unknown => self.push_str("unknown"),
            hir::TyKind::Path(path) => self.push_str(&path.join("::")),
        }
    }
}
