use tlang_ast::token;
use tlang_hir::hir;

#[derive(Debug, Clone, Copy)]
pub enum TlangValue {
    Nil,
    Int(i64),
    Float(f64),
    Bool(bool),
}

#[derive(Debug)]
pub struct Interpreter {}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {}
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
                    _ => todo!("eval_binary: Add: {:?} + {:?}", lhs, rhs),
                }
            }

            _ => todo!("eval_binary: {:?}", op),
        }
    }
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use std::error::Error;

    use super::*;
    use pretty_assertions::assert_matches;

    fn parse(src: &str) -> Result<hir::Module, Box<dyn Error>> {
        let parser = tlang_parser::parser::Parser::from_source(src);
        let ast = parser.parse()?;

        tlang_parser::parse(src).unwrap()
    }

    fn interpreter(initial_source: &str) -> Interpreter {}

    #[test]
    fn test_interpreter() {
        let mut interpreter = Interpreter::new();
        let result = interpreter.eval(&hir::Module::default());
        assert_matches!(result, TlangValue::Nil);
    }
}
