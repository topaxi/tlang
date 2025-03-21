use tlang_ast as ast;
use tlang_hir::hir;

fn parse_from_str(input: &str) -> ast::node::Module {
    let mut parser = tlang_parser::Parser::from_source(input);
    parser.parse().unwrap()
}

#[allow(dead_code)]
pub fn hir_from_str(input: &str) -> hir::Module {
    let ast = parse_from_str(input);
    tlang_ast_lowering::lower_to_hir(&ast)
}

// TODO: We want a visitor to do this.
struct PathCollector {
    paths: Vec<hir::Path>,
}

impl PathCollector {
    fn new() -> Self {
        Self { paths: Vec::new() }
    }

    fn collect(&mut self, node: &hir::Module) {
        self.collect_block(&node.block);
    }

    fn collect_block(&mut self, node: &hir::Block) {
        for stmt in &node.stmts {
            self.collect_stmt(stmt);
        }

        if let Some(expr) = &node.expr {
            self.collect_expr(expr);
        }
    }

    fn collect_stmt(&mut self, node: &hir::Stmt) {
        match &node.kind {
            hir::StmtKind::FunctionDeclaration(fn_decl) => {
                self.collect_expr(&fn_decl.name);
                self.collect_block(&fn_decl.body);
            }
            hir::StmtKind::StructDeclaration(_struct_decl) => {}
            hir::StmtKind::EnumDeclaration(_enum_decl) => {}
            hir::StmtKind::DynFunctionDeclaration(..) => {}
            hir::StmtKind::Let(_pat, expr, ..) => {
                self.collect_expr(expr);
            }
            stmt => todo!("{:?}", stmt),
        }
    }

    fn collect_expr(&mut self, node: &hir::Expr) {
        match &node.kind {
            hir::ExprKind::Path(path) => self.paths.push(*path.clone()),
            hir::ExprKind::Block(block) => self.collect_block(block),
            hir::ExprKind::Binary(_, lhs, rhs) => {
                self.collect_expr(lhs);
                self.collect_expr(rhs);
            }
            hir::ExprKind::Call(call_expr) | hir::ExprKind::TailCall(call_expr) => {
                self.collect_expr(&call_expr.callee);
                for arg in &call_expr.arguments {
                    self.collect_expr(arg);
                }
            }
            hir::ExprKind::Match(expr, arms) => {
                self.collect_expr(expr);
                for arm in arms {
                    self.collect_pat(&arm.pat);
                    if let Some(guard) = &arm.guard {
                        self.collect_expr(guard);
                    }
                    self.collect_block(&arm.block);
                }
            }
            hir::ExprKind::List(exprs) => {
                for expr in exprs {
                    self.collect_expr(expr);
                }
            }
            hir::ExprKind::Literal(..) => {}
            hir::ExprKind::FieldAccess(expr, _) => self.collect_expr(expr),
            hir::ExprKind::Dict(entries) => {
                for (key, value) in entries {
                    self.collect_expr(key);
                    self.collect_expr(value);
                }
            }
            expr => todo!("{:?}", expr),
        }
    }

    fn collect_pat(&mut self, node: &hir::Pat) {
        match &node.kind {
            hir::PatKind::List(patterns) => {
                for pat in patterns {
                    self.collect_pat(pat);
                }
            }
            hir::PatKind::Enum(path, kvs) => {
                self.paths.push(*path.clone());

                for (_, v) in kvs {
                    self.collect_pat(v);
                }
            }
            hir::PatKind::Identifier(..) | hir::PatKind::Literal(..) => {}
            pat => todo!("{:?}", pat),
        }
    }
}

#[allow(dead_code)]
pub fn collect_paths(node: &hir::Module) -> Vec<hir::Path> {
    let mut collector = PathCollector::new();
    collector.collect(node);
    collector.paths
}
