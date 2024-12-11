use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use tlang_ast::token;
use tlang_hir::hir;

trait Resolver {
    fn resolve_path(&self, path: &hir::Path) -> Option<TlangValue>;
    fn resolve_fn(&self, path: &hir::Path) -> Option<&hir::FunctionDeclaration>;
    fn resolve_struct(&self, path: &hir::Path) -> Option<&hir::StructDeclaration>;
}

#[derive(Debug, Default)]
pub struct Scope {
    pub parent: Option<Rc<RefCell<Scope>>>,
    pub values: HashMap<String, TlangValue>,
    pub fn_decls: HashMap<String, hir::FunctionDeclaration>,
    pub struct_decls: HashMap<String, hir::StructDeclaration>,
}

impl Resolver for Scope {
    fn resolve_path(&self, path: &hir::Path) -> Option<TlangValue> {
        let path_name = path.join("::");
        let value = self.values.get(&path_name);
        match value {
            Some(value) => Some(*value),
            None => match &self.parent {
                Some(parent) => parent.borrow().resolve_path(path),
                None => None,
            },
        }
    }

    fn resolve_fn(&self, path: &hir::Path) -> Option<&hir::FunctionDeclaration> {
        let path_name = path.join("::");
        let decl = self.fn_decls.get(&path_name);
        match decl {
            Some(decl) => Some(&decl),
            None => match &self.parent {
                Some(parent) => parent.borrow().resolve_fn(path),
                None => None,
            },
        }
    }

    fn resolve_struct(&self, path: &hir::Path) -> Option<&hir::StructDeclaration> {
        let path_name = path.join("::");
        let decl = self.struct_decls.get(&path_name);
        match decl {
            Some(decl) => Some(&decl),
            None => match &self.parent {
                Some(parent) => parent.borrow().resolve_struct(path),
                None => None,
            },
        }
    }
}

#[derive(Debug, Default)]
pub struct RootScope {
    pub scope: Rc<RefCell<Scope>>,
    pub native_fn_decls: HashMap<String, ()>,
    pub native_struct_decls: HashMap<String, ()>,
}

#[derive(Debug)]
pub struct TlangClosure {
    pub id: u64,
    // Closures hold a reference to the parent scope.
    pub scope: Option<Rc<RefCell<Scope>>>,
    pub decl: hir::FunctionDeclaration,
}

#[derive(Debug)]
pub struct TlangStruct {
    pub id: u64,
    pub field_values: Vec<TlangValue>,
}

#[derive(Debug, Clone, Copy)]
pub enum TlangObject {
    Struct(u64),
    Fn(u64),
}

#[derive(Debug, Clone, Copy)]
pub enum TlangValue {
    Nil,
    Int(i64),
    Float(f64),
    Bool(bool),
    Object(TlangObject),
}

#[derive(Debug, Default)]
pub struct Interpreter {
    root_scope: RootScope,
    scopes: Vec<Rc<RefCell<Scope>>>,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter::default()
    }

    pub fn eval(&mut self, input: &hir::Module) -> TlangValue {
        self.eval_block(&input.block)
    }

    pub fn eval_block(&mut self, block: &hir::Block) -> TlangValue {
        for stmt in &block.stmts {
            self.eval_stmt(stmt);
        }

        if let Some(expr) = &block.expr {
            self.eval_expr(expr)
        } else {
            TlangValue::Nil
        }
    }

    fn eval_stmt(&mut self, stmt: &hir::Stmt) -> TlangValue {
        match &stmt.kind {
            hir::StmtKind::Expr(expr) => self.eval_expr(expr),
            _ => todo!("eval_stmt: {:?}", stmt),
        }
    }

    fn eval_expr(&mut self, expr: &hir::Expr) -> TlangValue {
        match &expr.kind {
            hir::ExprKind::Literal(value) => self.eval_literal(value),
            hir::ExprKind::Block(block) => self.eval_block(block),
            hir::ExprKind::Binary(op, lhs, rhs) => self.eval_binary(*op, lhs, rhs),
            _ => todo!("eval_expr: {:?}", expr),
        }
    }

    fn eval_literal(&mut self, literal: &token::Literal) -> TlangValue {
        match literal {
            token::Literal::Integer(value) => TlangValue::Int(*value),
            token::Literal::UnsignedInteger(value) => TlangValue::Int(*value as i64),
            token::Literal::Float(value) => TlangValue::Float(*value),
            token::Literal::Boolean(value) => TlangValue::Bool(*value),
            _ => todo!("eval_literal: {:?}", literal),
        }
    }

    fn eval_binary(
        &mut self,
        op: hir::BinaryOpKind,
        lhs: &hir::Expr,
        rhs: &hir::Expr,
    ) -> TlangValue {
        match op {
            hir::BinaryOpKind::Add => {
                let lhs = self.eval_expr(lhs);
                let rhs = self.eval_expr(rhs);

                match (lhs, rhs) {
                    (TlangValue::Int(lhs), TlangValue::Int(rhs)) => TlangValue::Int(lhs + rhs),
                    (TlangValue::Float(lhs), TlangValue::Float(rhs)) => {
                        TlangValue::Float(lhs + rhs)
                    }
                    (TlangValue::Float(lhs), TlangValue::Int(rhs)) => {
                        TlangValue::Float(lhs + rhs as f64)
                    }
                    (TlangValue::Int(lhs), TlangValue::Float(rhs)) => {
                        TlangValue::Float(lhs as f64 + rhs)
                    }
                    _ => todo!("eval_binary: Add: {:?} + {:?}", lhs, rhs),
                }
            }

            _ => todo!("eval_binary: {:?}", op),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::error::Error;

    use super::*;
    use pretty_assertions::assert_matches;
    use tlang_ast_lowering::lower_to_hir;

    fn parse(src: &str) -> Result<hir::Module, Box<dyn Error>> {
        let mut parser = tlang_parser::parser::Parser::from_source(src);
        let ast = parser.parse()?;
        let hir = lower_to_hir(&ast);

        Ok(hir)
    }

    fn interpreter(initial_source: &str) -> Interpreter {
        let mut interpreter = Interpreter::new();
        interpreter.eval(&parse(initial_source).unwrap());
        interpreter
    }

    fn eval(interpreter: &mut Interpreter, src: &str) -> TlangValue {
        let block = format!("{{ {} }};", src);
        let hir = parse(&block).unwrap();

        match &hir.block.stmts[0].kind {
            hir::StmtKind::Expr(expr) => interpreter.eval_expr(expr),
            _ => todo!("eval: {:?}", hir),
        }
    }

    #[test]
    fn test_interpreter() {
        let mut interpreter = interpreter("");
        let result = interpreter.eval(&hir::Module::default());
        assert_matches!(result, TlangValue::Nil);
    }

    #[test]
    fn test_basic_arithmetic() {
        let mut interpreter = interpreter("");
        let result = eval(&mut interpreter, "1 + 2");
        assert_matches!(result, TlangValue::Int(3));
    }

    #[test]
    fn test_addition_of_mixed_ints_and_floats() {
        let mut interpreter = interpreter("");

        assert_matches!(eval(&mut interpreter, "1 + 2.0"), TlangValue::Float(3.0));
        assert_matches!(eval(&mut interpreter, "1.0 + 2"), TlangValue::Float(3.0));
    }
}
