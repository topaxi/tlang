use tlang_ast::node::{Ident, UnaryOp};
use tlang_ast::token::{Literal, Token, TokenKind};
use tlang_hir::hir;

pub struct HirPrettyOptions {
    pub tab_indent: bool,
    pub indent_size: usize,
    pub mark_unresolved: bool,
    pub print_ids: bool,
    pub comments: bool,
}

impl Default for HirPrettyOptions {
    fn default() -> Self {
        Self {
            indent_size: 4,
            tab_indent: false,
            mark_unresolved: true,
            print_ids: false,
            comments: true,
        }
    }
}

struct HirPrettyResolvedOptions {
    indent_string: String,
    mark_unresolved: bool,
    print_ids: bool,
    comments: bool,
}

impl Default for HirPrettyResolvedOptions {
    fn default() -> Self {
        Self::from(HirPrettyOptions::default())
    }
}

impl From<HirPrettyOptions> for HirPrettyResolvedOptions {
    fn from(options: HirPrettyOptions) -> Self {
        Self {
            indent_string: if options.tab_indent {
                "\t".to_string()
            } else {
                " ".repeat(options.indent_size)
            },
            mark_unresolved: options.mark_unresolved,
            print_ids: options.print_ids,
            comments: options.comments,
        }
    }
}

#[derive(Default)]
pub struct HirPretty {
    output: String,
    indent_level: usize,
    options: HirPrettyResolvedOptions,
}

impl HirPretty {
    pub fn new(options: HirPrettyOptions) -> Self {
        Self {
            options: HirPrettyResolvedOptions::from(options),
            ..Default::default()
        }
    }

    pub fn pretty_print(hir: &hir::Module) -> String {
        let mut pretty_printer = HirPretty::default();
        pretty_printer.print_module(hir);
        pretty_printer.output
    }

    fn push_str(&mut self, string: &str) {
        self.output.push_str(string);
    }

    #[allow(clippy::needless_pass_by_value)]
    fn push_string(&mut self, string: String) {
        self.push_str(&string);
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
            self.output.push_str(&self.options.indent_string);
        }
    }

    fn push_newline(&mut self) {
        self.push_char('\n');
    }

    pub fn output(&self) -> &str {
        &self.output
    }

    pub fn print_module(&mut self, module: &hir::Module) {
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

    fn print_comment(&mut self, comment: &Token) {
        if !self.options.comments {
            return;
        }

        match &comment.kind {
            TokenKind::SingleLineComment(comment) => {
                self.push_indent();
                self.push_str("// ");
                self.push_str(comment);
            }
            TokenKind::MultiLineComment(comment) => {
                self.push_indent();
                self.push_str("/* ");
                self.push_str(comment);
                self.push_str(" */");
            }
            _ => unreachable!(),
        }
    }

    fn print_comments(&mut self, comments: &[Token]) {
        if !self.options.comments {
            return;
        }

        for comment in comments {
            self.print_comment(comment);
            self.push_newline();
        }
    }

    fn print_stmts(&mut self, stmts: &[hir::Stmt]) {
        for stmt in stmts {
            self.print_stmt(stmt);
        }
    }

    fn print_stmt(&mut self, stmt: &hir::Stmt) {
        self.push_indent();
        self.print_comments(&stmt.leading_comments);

        match &stmt.kind {
            hir::StmtKind::EnumDeclaration(decl) => self.print_enum_declaration(decl),
            hir::StmtKind::Expr(expr) => {
                self.print_expr(expr);
                self.push_char(';');
            }
            hir::StmtKind::FunctionDeclaration(decl) => self.print_function_declaration(decl),
            hir::StmtKind::DynFunctionDeclaration(decl) => {
                self.print_dyn_function_declaration(decl);
                self.push_char(';');
            }
            hir::StmtKind::Let(pat, expr, ty) => {
                self.print_variable_declaration(pat, expr, ty);
                self.push_char(';');
            }
            hir::StmtKind::Return(expr) => {
                self.push_str("return");

                if let Some(expr) = expr.as_ref() {
                    self.push_str(" ");
                    self.print_expr(expr);
                }

                self.push_char(';');
            }
            hir::StmtKind::StructDeclaration(decl) => self.print_struct_declaration(decl),
        }

        self.push_newline();
        self.print_comments(&stmt.trailing_comments);
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
            if self.options.print_ids {
                self.push_char('#');
                self.push_str(&variant.hir_id.to_string());
            }
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
        if self.options.print_ids {
            self.push_char('#');
            self.push_str(&decl.hir_id.to_string());
        }
        self.push_str(" {");
        if !decl.fields.is_empty() {
            self.push_newline();
            self.inc_indent();
            for field in &decl.fields {
                self.print_struct_field(field);
            }
            self.dec_indent();
            self.push_indent();
        }
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
            hir::ExprKind::Cast(expr, ty) => {
                self.print_expr(expr);
                self.push_str(" as ");
                self.print_ty(ty);
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
            hir::ExprKind::Loop(block) => {
                self.push_str("loop ");
                self.print_block(block);
            }
            hir::ExprKind::Break(expr) => {
                self.push_str("break");

                if let Some(expr) = expr.as_ref() {
                    self.push_char(' ');
                    self.print_expr(expr);
                }
            }
            hir::ExprKind::Continue => self.push_str("continue"),
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
            self.print_match_arm(arm);
        }
        self.dec_indent();
        self.push_indent();
        self.push_char('}');
    }

    fn print_match_arm(&mut self, arm: &hir::MatchArm) {
        self.print_comments(&arm.leading_comments);
        self.push_indent();
        self.print_pat(&arm.pat);
        if let Some(guard) = &arm.guard {
            self.push_str(" if ");
            self.print_expr(guard);
        }
        self.push_str(" => ");
        self.print_block(&arm.block);
        self.push_char(',');
        self.push_newline();
        self.print_comments(&arm.leading_comments);
    }

    fn print_binary_expr(&mut self, op: &hir::BinaryOpKind, lhs: &hir::Expr, rhs: &hir::Expr) {
        self.push_char('(');
        self.print_expr(lhs);
        self.push_char(' ');

        match op {
            hir::BinaryOpKind::Add => self.push_char('+'),
            hir::BinaryOpKind::Assign => self.push_char('='),
            hir::BinaryOpKind::Sub => self.push_char('-'),
            hir::BinaryOpKind::Mul => self.push_char('*'),
            hir::BinaryOpKind::Div => self.push_char('/'),
            hir::BinaryOpKind::Mod => self.push_char('%'),
            hir::BinaryOpKind::Exp => self.push_str("**"),
            hir::BinaryOpKind::And => self.push_str("&&"),
            hir::BinaryOpKind::Or => self.push_str("||"),
            hir::BinaryOpKind::Eq => self.push_str("=="),
            hir::BinaryOpKind::NotEq => self.push_str("!="),
            hir::BinaryOpKind::Less => self.push_char('<'),
            hir::BinaryOpKind::LessEq => self.push_str("<="),
            hir::BinaryOpKind::Greater => self.push_char('>'),
            hir::BinaryOpKind::GreaterEq => self.push_str(">="),
            hir::BinaryOpKind::BitwiseOr => self.push_char('|'),
            hir::BinaryOpKind::BitwiseAnd => self.push_char('&'),
            hir::BinaryOpKind::BitwiseXor => self.push_char('^'),
        }

        self.push_char(' ');
        self.print_expr(rhs);
        self.push_char(')');
    }

    fn print_call_expr(&mut self, call_expr: &hir::CallExpression) {
        self.print_expr(&call_expr.callee);
        self.push_char('(');
        for (i, arg) in call_expr.arguments.iter().enumerate() {
            if i > 0 {
                self.push_str(", ");
            }
            self.print_expr(arg);
        }
        self.push_char(')');
    }

    fn print_function_name(&mut self, name: &hir::Expr, hir_id: hir::HirId) {
        if let hir::ExprKind::Path(path) = &name.kind {
            self.push_string(path.to_string());
        } else {
            self.print_expr(name);
        }

        if self.options.print_ids {
            self.push_char('#');
            self.push_str(&hir_id.to_string());
        }
    }

    fn print_function_declaration(&mut self, decl: &hir::FunctionDeclaration) {
        self.push_str("fn ");
        self.print_function_name(&decl.name, decl.hir_id);
        self.push_char('(');
        for (i, param) in decl.parameters.iter().enumerate() {
            if i > 0 {
                self.push_str(", ");
            }

            self.print_ident(&param.name);
            if self.options.print_ids {
                self.push_char('#');
                self.push_str(&param.hir_id.to_string());
            }
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
        self.print_function_name(&decl.name, decl.hir_id);
        self.inc_indent();
        for variant in &decl.variants {
            self.push_newline();
            self.push_indent();
            self.push_str("-> ");

            if let hir::ExprKind::Path(path) = &decl.name.kind {
                self.push_string(path.to_string());
            } else {
                self.print_expr(&decl.name);
            }

            self.push_char('/');
            self.push_string(variant.0.to_string());

            if self.options.print_ids {
                self.push_char('#');
                self.push_str(&variant.1.to_string());
            }
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
            hir::PatKind::Identifier(hir_id, name) => {
                self.print_ident(name);

                if self.options.print_ids {
                    self.push_char('#');
                    self.push_str(&hir_id.to_string());
                }
            }
            hir::PatKind::Enum(path, fields) => {
                self.print_path(path);

                if !fields.is_empty() {
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
        self.push_string(path.to_string());

        // We print unresolved paths with a question mark.
        if self.options.mark_unresolved && path.res.hir_id().is_none() {
            self.push_char('?');
        }

        if self.options.print_ids
            && let Some(hir_id) = path.res.hir_id()
        {
            self.push_str(&format!("#{}", hir_id));
        }
    }

    fn print_literal(&mut self, lit: &Literal) {
        match lit {
            Literal::Boolean(bool) => self.push_string(bool.to_string()),
            Literal::String(s) => self.push_string(format!("{s:?}")),
            Literal::Char(c) => self.push_string(format!("{c:?}")),
            Literal::UnsignedInteger(u) => self.push_string(u.to_string()),
            Literal::Integer(i) => self.push_string(i.to_string()),
            Literal::Float(f) => self.push_string(f.to_string()),
            Literal::None => unreachable!(),
        }
    }
}
