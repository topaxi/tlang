use tlang_ast::node::{Ident, UnaryOp};
use tlang_ast::token::Literal;
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
        if let Some(expr) = &block.expr {
            self.push_indent();
            self.print_expr(expr);
            self.push_newline();
        }
        self.dec_indent();
        self.push_indent();
        self.push_char('}');
    }

    fn print_stmts(&mut self, stmts: &[hir::Stmt]) {
        for stmt in stmts {
            self.print_stmt(stmt);
        }
    }

    fn print_stmt(&mut self, stmt: &hir::Stmt) {
        self.push_indent();

        match &stmt.kind {
            hir::StmtKind::EnumDeclaration(decl) => self.print_enum_declaration(decl),
            hir::StmtKind::Expr(expr) => self.print_expr(expr),
            hir::StmtKind::FunctionDeclaration(decl) => self.print_function_declaration(decl),
            hir::StmtKind::DynFunctionDeclaration(decl) => {
                self.print_dyn_function_declaration(decl)
            }
            hir::StmtKind::Let(pat, expr, ty) => self.print_variable_declaration(pat, expr, ty),
            hir::StmtKind::Return(expr) => {
                self.push_str("return");

                if let Some(expr) = expr.as_ref() {
                    self.push_str(" ");
                    self.print_expr(expr);
                }
            }
            hir::StmtKind::StructDeclaration(decl) => self.print_struct_declaration(decl),
            hir::StmtKind::None => {}
        }

        self.push_char(';');
        self.push_newline();
    }

    fn print_struct_field(&mut self, field: &hir::StructField) {
        self.push_indent();
        self.print_ident(&field.name);
        self.push_str(": ");
        self.print_ty(&field.ty);
        self.push_str(",");
        self.push_newline();
    }

    fn print_enum_declaration(&mut self, decl: &hir::EnumDeclaration) {
        self.push_str("enum ");
        self.print_ident(&decl.name);
        self.push_str(" {");
        self.push_newline();
        self.inc_indent();
        for variant in &decl.variants {
            self.push_indent();
            self.print_ident(&variant.name);
            self.push_str(" {");
            self.push_newline();
            self.inc_indent();
            for field in &variant.parameters {
                self.print_struct_field(field);
            }
            self.dec_indent();
            self.push_indent();
            self.push_char('}');
            self.push_newline();
        }
        self.dec_indent();
        self.push_indent();
        self.push_char('}');
    }

    fn print_struct_declaration(&mut self, decl: &hir::StructDeclaration) {
        self.push_str("struct ");
        self.print_ident(&decl.name);
        self.push_str(" {");
        self.push_newline();
        self.inc_indent();
        for field in &decl.fields {
            self.print_struct_field(field);
        }
        self.dec_indent();
        self.push_indent();
        self.push_char('}');
        self.push_newline();
    }

    fn print_expr(&mut self, expr: &hir::Expr) {
        match &expr.kind {
            hir::ExprKind::Call(call_expr) => self.print_call_expr(call_expr),
            hir::ExprKind::TailCall(call_expr) => {
                self.push_str("rec ");
                self.print_call_expr(call_expr);
            }
            hir::ExprKind::Binary(op, lhs, rhs) => {
                self.print_binary_expr(op, lhs, rhs);
            }
            hir::ExprKind::Unary(op, expr) => {
                match op {
                    UnaryOp::Minus => self.push_char('-'),
                    UnaryOp::Not => self.push_char('!'),
                    UnaryOp::Spread => self.push_str("..."),
                    UnaryOp::Rest => self.push_str("..."),
                }
                self.print_expr(expr);
            }
            hir::ExprKind::Block(block) => self.print_block(block),
            hir::ExprKind::Dict(kvs) => self.print_dict(kvs),
            hir::ExprKind::FunctionExpression(decl) => self.print_function_declaration(decl),
            hir::ExprKind::FieldAccess(expr, ident) => {
                self.print_expr(expr);
                self.push_str(".");
                self.print_ident(ident);
            }
            hir::ExprKind::IndexAccess(expr, index) => {
                self.print_expr(expr);
                self.push_char('[');
                self.print_expr(index);
                self.push_char(']');
            }
            hir::ExprKind::IfElse(cond, then_branch, else_branches) => {
                self.push_str("if ");
                self.print_expr(cond);
                self.push_char(' ');
                self.print_block(then_branch);

                for else_branch in else_branches {
                    self.push_str(" else ");

                    if let Some(cond) = &else_branch.condition {
                        self.push_str("if ");
                        self.print_expr(cond);
                        self.push_char(' ');
                    }

                    self.print_block(&else_branch.consequence);
                }
            }
            hir::ExprKind::Let(pat, expr) => {
                self.push_str("let ");
                self.print_pat(pat);
                self.push_str(" = ");
                self.print_expr(expr);
            }
            hir::ExprKind::List(exprs) => {
                self.push_char('[');
                for (i, expr) in exprs.iter().enumerate() {
                    if i > 0 {
                        self.push_str(", ");
                    }
                    self.print_expr(expr);
                }
                self.push_char(']');
            }
            hir::ExprKind::Literal(literal) => self.print_literal(literal),
            hir::ExprKind::Match(expr, arms) => self.print_match_expr(expr, arms),
            hir::ExprKind::Path(path) => self.print_path(path),
            hir::ExprKind::Range(range_expr) => self.print_range_expr(range_expr),
            hir::ExprKind::Wildcard => self.push_char('_'),
        }
    }

    fn print_range_expr(&mut self, _range_expr: &hir::RangeExpression) {
        todo!("print_range_expr")
    }

    fn print_dict(&mut self, kvs: &[(hir::Expr, hir::Expr)]) {
        self.push_str("{");
        for (i, (key, value)) in kvs.iter().enumerate() {
            if i > 0 {
                self.push_str(", ");
            }
            self.print_expr(key);
            self.push_str(": ");
            self.print_expr(value);
        }
        self.push_str("}");
    }

    fn print_match_expr(&mut self, expr: &hir::Expr, arms: &[hir::MatchArm]) {
        self.push_str("match ");
        self.print_expr(expr);
        self.push_str(" {");
        self.push_newline();
        self.inc_indent();
        for arm in arms {
            self.push_indent();
            self.print_pat(&arm.pat);
            if let Some(guard) = &arm.guard {
                self.push_str(" if ");
                self.print_expr(guard);
            }
            self.push_str(" => ");
            self.print_expr(&arm.expr);
            self.push_char(',');
            self.push_newline();
        }
        self.dec_indent();
        self.push_indent();
        self.push_char('}');
    }

    fn print_binary_expr(&mut self, op: &hir::BinaryOpKind, lhs: &hir::Expr, rhs: &hir::Expr) {
        self.push_char('(');
        self.print_expr(lhs);
        self.push_char(' ');

        match op {
            hir::BinaryOpKind::Add => self.push_str("+"),
            hir::BinaryOpKind::Assign => self.push_str("="),
            hir::BinaryOpKind::Sub => self.push_str("-"),
            hir::BinaryOpKind::Mul => self.push_str("*"),
            hir::BinaryOpKind::Div => self.push_str("/"),
            hir::BinaryOpKind::Mod => self.push_str("%"),
            hir::BinaryOpKind::Exp => self.push_str("**"),
            hir::BinaryOpKind::And => self.push_str("&&"),
            hir::BinaryOpKind::Or => self.push_str("||"),
            hir::BinaryOpKind::Eq => self.push_str("=="),
            hir::BinaryOpKind::NotEq => self.push_str("!="),
            hir::BinaryOpKind::Less => self.push_str("<"),
            hir::BinaryOpKind::LessEq => self.push_str("<="),
            hir::BinaryOpKind::Greater => self.push_str(">"),
            hir::BinaryOpKind::GreaterEq => self.push_str(">="),
            hir::BinaryOpKind::BitwiseOr => self.push_str("|"),
            hir::BinaryOpKind::BitwiseAnd => self.push_str("&"),
            hir::BinaryOpKind::BitwiseXor => self.push_str("^"),
        }

        self.push_char(' ');
        self.print_expr(rhs);
        self.push_char(')');
    }

    fn print_call_expr(&mut self, call_expr: &hir::CallExpression) {
        self.print_expr(&call_expr.callee);
        self.push_str("(");
        for (i, arg) in call_expr.arguments.iter().enumerate() {
            if i > 0 {
                self.push_str(", ");
            }
            self.print_expr(arg);
        }
        self.push_str(")");
    }

    fn print_function_name(&mut self, name: &hir::Expr) {
        if let hir::ExprKind::Path(path) = &name.kind {
            self.push_str(&path.join("::"));
        } else {
            self.print_expr(name);
        }
    }

    fn print_function_declaration(&mut self, decl: &hir::FunctionDeclaration) {
        self.push_str("fn ");
        self.print_function_name(&decl.name);
        self.push_str("(");
        for (i, param) in decl.parameters.iter().enumerate() {
            if i > 0 {
                self.push_str(", ");
            }

            self.print_ident(&param.name);
            self.push_str(": ");
            self.print_ty(&param.type_annotation);
        }
        self.push_str(") -> ");
        self.print_ty(&decl.return_type);
        self.push_char(' ');
        self.print_block(&decl.body);
    }

    fn print_dyn_function_declaration(&mut self, decl: &hir::DynFunctionDeclaration) {
        self.push_str("dyn fn ");
        self.print_function_name(&decl.name);
        self.inc_indent();
        for variant in &decl.variants {
            self.push_newline();
            self.push_indent();
            self.push_str("-> ");
            self.print_function_name(&decl.name);
            self.push_str("$$");
            self.push_str(&variant.0.to_string());
        }
        self.dec_indent();
    }

    fn print_variable_declaration(&mut self, pat: &hir::Pat, expr: &hir::Expr, ty: &hir::Ty) {
        self.push_str("let ");
        self.print_pat(pat);
        self.push_str(": ");
        self.print_ty(ty);
        self.push_str(" = ");
        self.print_expr(expr);
    }

    #[inline(always)]
    fn print_ident(&mut self, ident: &Ident) {
        self.push_str(ident.as_str());
    }

    fn print_pat(&mut self, pat: &hir::Pat) {
        match &pat.kind {
            hir::PatKind::Identifier(_id, name) => self.print_ident(name),
            hir::PatKind::Enum(path, fields) => {
                self.print_path(path);
                self.push_str(" { ");
                for (i, (ident, pat)) in fields.iter().enumerate() {
                    if i > 0 {
                        self.push_str(", ");
                    }
                    self.print_ident(ident);
                    self.push_str(": ");
                    self.print_pat(pat);
                }
                self.push_str(" }");
            }
            hir::PatKind::List(pats) => {
                self.push_char('[');
                for (i, pat) in pats.iter().enumerate() {
                    if i > 0 {
                        self.push_str(", ");
                    }
                    self.print_pat(pat);
                }
                self.push_char(']');
            }
            hir::PatKind::Literal(lit) => self.print_literal(lit),
            hir::PatKind::Rest(pat) => {
                self.push_str("...");
                self.print_pat(pat);
            }
            hir::PatKind::Wildcard => self.push_char('_'),
        }
    }

    fn print_ty(&mut self, ty: &hir::Ty) {
        match &ty.kind {
            hir::TyKind::Unknown => self.push_str("unknown"),
            hir::TyKind::Path(path) => self.print_path(path),
        }
    }

    fn print_path(&mut self, path: &hir::Path) {
        self.push_str(&path.join("::"));

        // We print unresolved paths with a question mark.
        if path.res.is_unknown() {
            self.push_char('?');
        }
    }

    fn print_literal(&mut self, lit: &Literal) {
        match lit {
            Literal::Boolean(bool) => self.push_str(&bool.to_string()),
            Literal::String(s) => self.push_str(&format!("{:?}", s)),
            Literal::Char(c) => self.push_str(&format!("{:?}", c)),
            Literal::UnsignedInteger(u) => self.push_str(&u.to_string()),
            Literal::Integer(i) => self.push_str(&i.to_string()),
            Literal::Float(f) => self.push_str(&f.to_string()),
        }
    }
}
