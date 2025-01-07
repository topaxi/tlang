#![feature(if_let_guard)]
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use tlang_ast::token;
use tlang_hir::hir;

trait Resolver {
    fn resolve_path(&self, path: &hir::Path) -> Option<TlangValue>;
    fn resolve_fn(&self, path: &hir::Path) -> Option<Rc<hir::FunctionDeclaration>>;
    fn resolve_struct(&self, path: &hir::Path) -> Option<Rc<hir::StructDeclaration>>;
}

#[derive(Debug, Default)]
pub struct Scope {
    pub parent: Option<Rc<RefCell<Scope>>>,
    pub values: HashMap<String, TlangValue>,
    pub fn_decls: HashMap<String, Rc<hir::FunctionDeclaration>>,
    pub struct_decls: HashMap<String, Rc<hir::StructDeclaration>>,
}

impl Resolver for Scope {
    fn resolve_path(&self, path: &hir::Path) -> Option<TlangValue> {
        let path_name = path.join("::");

        match self.values.get(&path_name) {
            Some(value) => Some(*value),
            None => match &self.parent {
                Some(parent) => parent.borrow().resolve_path(path),
                None => None,
            },
        }
    }

    fn resolve_fn(&self, path: &hir::Path) -> Option<Rc<hir::FunctionDeclaration>> {
        let path_name = path.join("::");

        match self.fn_decls.get(&path_name) {
            Some(decl) => Some(decl.clone()),
            None => match &self.parent {
                Some(parent) => parent.borrow().resolve_fn(path),
                None => None,
            },
        }
    }

    fn resolve_struct(&self, path: &hir::Path) -> Option<Rc<hir::StructDeclaration>> {
        let path_name = path.join("::");

        match self.struct_decls.get(&path_name) {
            Some(decl) => Some(decl.clone()),
            None => match &self.parent {
                Some(parent) => parent.borrow().resolve_struct(path),
                None => None,
            },
        }
    }
}

#[derive(Debug, Default)]
pub struct RootScope {
    pub scope: Rc<Scope>,
    pub native_fn_decls: HashMap<String, ()>,
    pub native_struct_decls: HashMap<String, ()>,
}

impl Resolver for RootScope {
    fn resolve_path(&self, path: &hir::Path) -> Option<TlangValue> {
        self.scope.resolve_path(path)
    }
    fn resolve_fn(&self, path: &hir::Path) -> Option<Rc<hir::FunctionDeclaration>> {
        self.scope.resolve_fn(path)
    }
    fn resolve_struct(&self, path: &hir::Path) -> Option<Rc<hir::StructDeclaration>> {
        self.scope.resolve_struct(path)
    }
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
    current_scope: Rc<RefCell<Scope>>,
}

impl Resolver for Interpreter {
    fn resolve_path(&self, path: &hir::Path) -> Option<TlangValue> {
        match self.current_scope.borrow().resolve_path(path) {
            Some(value) => Some(value),
            None => self.root_scope.resolve_path(path),
        }
    }
    fn resolve_fn(&self, path: &hir::Path) -> Option<Rc<hir::FunctionDeclaration>> {
        match self.current_scope.borrow().resolve_fn(path) {
            Some(decl) => Some(decl),
            None => self.root_scope.resolve_fn(path),
        }
    }
    fn resolve_struct(&self, path: &hir::Path) -> Option<Rc<hir::StructDeclaration>> {
        match self.current_scope.borrow().resolve_struct(path) {
            Some(decl) => Some(decl),
            None => self.root_scope.resolve_struct(path),
        }
    }
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter::default()
    }

    fn enter_scope(&mut self) {
        let new_scope = Rc::new(RefCell::new(Scope {
            parent: Some(self.current_scope.clone()),
            ..Default::default()
        }));
        self.current_scope = new_scope;
    }

    fn exit_scope(&mut self) {
        let parent_scope = {
            let current_scope = self.current_scope.borrow();
            current_scope.parent.clone()
        };

        if let Some(parent) = parent_scope {
            self.current_scope = parent;
        } else {
            panic!("Attempted to exit root scope!");
        }
    }

    #[inline(always)]
    fn with_new_scope<F, R>(&mut self, f: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        self.enter_scope();
        let result = f(self);
        self.exit_scope();
        result
    }

    pub fn eval(&mut self, input: &hir::Module) -> TlangValue {
        self.eval_block_stmts(&input.block.stmts);
        self.eval_block_expr(&input.block)
    }

    #[inline(always)]
    fn eval_block_stmts(&mut self, stmts: &[hir::Stmt]) {
        for stmt in stmts {
            self.eval_stmt(stmt);
        }
    }

    #[inline(always)]
    fn eval_block_expr(&mut self, block: &hir::Block) -> TlangValue {
        if let Some(expr) = &block.expr {
            self.eval_expr(expr)
        } else {
            TlangValue::Nil
        }
    }

    pub fn eval_block(&mut self, block: &hir::Block) -> TlangValue {
        self.with_new_scope(|this| {
            this.eval_block_stmts(&block.stmts);
            this.eval_block_expr(block)
        })
    }

    fn eval_stmt(&mut self, stmt: &hir::Stmt) {
        match &stmt.kind {
            hir::StmtKind::Expr(expr) => {
                self.eval_expr(expr);
            }
            hir::StmtKind::FunctionDeclaration(decl) => {
                self.eval_fn_decl(decl);
            }
            hir::StmtKind::StructDeclaration(decl) => {
                self.eval_struct_decl(decl);
            }
            _ => todo!("eval_stmt: {:?}", stmt),
        }
    }

    fn eval_expr(&mut self, expr: &hir::Expr) -> TlangValue {
        match &expr.kind {
            hir::ExprKind::Literal(value) => self.eval_literal(value),
            hir::ExprKind::Block(block) => self.eval_block(block),
            hir::ExprKind::Binary(op, lhs, rhs) => self.eval_binary(*op, lhs, rhs),
            hir::ExprKind::Call(call_expr) => self.eval_call(call_expr),
            hir::ExprKind::Path(path) => self.resolve_path(path).unwrap_or(TlangValue::Nil),
            hir::ExprKind::IfElse(condition, consequence, else_clauses) => {
                let condition = self.eval_expr(condition);
                if let TlangValue::Bool(true) = condition {
                    self.eval_block(consequence)
                } else {
                    for else_clause in else_clauses {
                        if let Some(condition) = &else_clause.condition {
                            if let TlangValue::Bool(true) = self.eval_expr(condition) {
                                return self.eval_block(&else_clause.consequence);
                            }
                        } else {
                            return self.eval_block(&else_clause.consequence);
                        }
                    }
                    TlangValue::Nil
                }
            }
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
            hir::BinaryOpKind::And => {
                let lhs = self.eval_expr(lhs);

                if let TlangValue::Bool(true) = lhs {
                    let rhs = self.eval_expr(rhs);

                    if let TlangValue::Bool(true) = rhs {
                        return TlangValue::Bool(true);
                    }
                }

                return TlangValue::Bool(false);
            }

            hir::BinaryOpKind::Or => {
                let lhs = self.eval_expr(lhs);

                if let TlangValue::Bool(true) = lhs {
                    return TlangValue::Bool(true);
                }

                return self.eval_expr(rhs);
            }

            _ => {}
        }

        let lhs = self.eval_expr(lhs);
        let rhs = self.eval_expr(rhs);

        match op {
            hir::BinaryOpKind::Add => self.eval_arithmetic_op(lhs, rhs, |a, b| a + b),
            hir::BinaryOpKind::Sub => self.eval_arithmetic_op(lhs, rhs, |a, b| a - b),
            hir::BinaryOpKind::Mul => self.eval_arithmetic_op(lhs, rhs, |a, b| a * b),
            hir::BinaryOpKind::Div => self.eval_arithmetic_op(lhs, rhs, |a, b| a / b),
            hir::BinaryOpKind::Mod => self.eval_arithmetic_op(lhs, rhs, |a, b| a % b),
            hir::BinaryOpKind::Exp => self.eval_arithmetic_op(lhs, rhs, |a, b| a.powf(b)),

            hir::BinaryOpKind::Eq => self.eval_comparison_op(lhs, rhs, |a, b| a == b),
            hir::BinaryOpKind::NotEq => self.eval_comparison_op(lhs, rhs, |a, b| a != b),
            hir::BinaryOpKind::Greater => self.eval_comparison_op(lhs, rhs, |a, b| a > b),
            hir::BinaryOpKind::GreaterEq => self.eval_comparison_op(lhs, rhs, |a, b| a >= b),
            hir::BinaryOpKind::Less => self.eval_comparison_op(lhs, rhs, |a, b| a < b),
            hir::BinaryOpKind::LessEq => self.eval_comparison_op(lhs, rhs, |a, b| a <= b),

            hir::BinaryOpKind::BitwiseAnd => self.eval_bitwise_op(lhs, rhs, |a, b| a & b),
            hir::BinaryOpKind::BitwiseOr => self.eval_bitwise_op(lhs, rhs, |a, b| a | b),
            hir::BinaryOpKind::BitwiseXor => self.eval_bitwise_op(lhs, rhs, |a, b| a ^ b),

            hir::BinaryOpKind::Assign => {
                todo!("eval_binary: Assign not implemented");
            }

            hir::BinaryOpKind::And | hir::BinaryOpKind::Or => {
                unreachable!();
            }
        }
    }

    fn eval_arithmetic_op<F>(&self, lhs: TlangValue, rhs: TlangValue, op: F) -> TlangValue
    where
        F: Fn(f64, f64) -> f64,
    {
        match (lhs, rhs) {
            (TlangValue::Int(lhs), TlangValue::Int(rhs)) => {
                TlangValue::Int(op(lhs as f64, rhs as f64) as i64)
            }
            (TlangValue::Float(lhs), TlangValue::Float(rhs)) => TlangValue::Float(op(lhs, rhs)),
            (TlangValue::Float(lhs), TlangValue::Int(rhs)) => {
                TlangValue::Float(op(lhs, rhs as f64))
            }
            (TlangValue::Int(lhs), TlangValue::Float(rhs)) => {
                TlangValue::Float(op(lhs as f64, rhs))
            }
            _ => todo!("eval_arithmetic_op: incompatible types"),
        }
    }

    fn eval_comparison_op<F>(&self, lhs: TlangValue, rhs: TlangValue, op: F) -> TlangValue
    where
        F: Fn(f64, f64) -> bool,
    {
        match (lhs, rhs) {
            (TlangValue::Int(lhs), TlangValue::Int(rhs)) => {
                TlangValue::Bool(op(lhs as f64, rhs as f64))
            }
            (TlangValue::Float(lhs), TlangValue::Float(rhs)) => TlangValue::Bool(op(lhs, rhs)),
            (TlangValue::Float(lhs), TlangValue::Int(rhs)) => TlangValue::Bool(op(lhs, rhs as f64)),
            (TlangValue::Int(lhs), TlangValue::Float(rhs)) => TlangValue::Bool(op(lhs as f64, rhs)),
            _ => todo!("eval_comparison_op: incompatible types"),
        }
    }

    fn eval_bitwise_op<F>(&self, lhs: TlangValue, rhs: TlangValue, op: F) -> TlangValue
    where
        F: Fn(i64, i64) -> i64,
    {
        match (lhs, rhs) {
            (TlangValue::Int(lhs), TlangValue::Int(rhs)) => TlangValue::Int(op(lhs, rhs)),
            _ => todo!("eval_bitwise_op: incompatible types"),
        }
    }

    fn eval_fn_decl(&mut self, decl: &hir::FunctionDeclaration) {
        match decl.name.kind {
            hir::ExprKind::Path(ref path) => {
                let path_name = path.join("::");

                self.current_scope
                    .borrow_mut()
                    .fn_decls
                    .insert(path_name, Rc::new(decl.clone()));
            }
            _ => todo!("eval_fn_decl: {:?}", decl.name),
        }
    }

    fn eval_struct_decl(&mut self, _decl: &hir::StructDeclaration) {
        todo!();
    }

    fn eval_call(&mut self, call_expr: &hir::CallExpression) -> TlangValue {
        match &call_expr.callee.kind {
            hir::ExprKind::Path(path) if let Some(fn_decl) = self.resolve_fn(path) => {
                self.eval_fn_call(&fn_decl, &call_expr.arguments)
            }
            _ => todo!("eval_call: {:?}", call_expr.callee),
        }
    }

    fn eval_fn_call(
        &mut self,
        fn_decl: &hir::FunctionDeclaration,
        call_args: &[hir::Expr],
    ) -> TlangValue {
        if fn_decl.parameters.len() != call_args.len() {
            todo!("Mismatched number of arguments");
        }

        let args = self.with_new_scope(|this| {
            let mut args: Vec<TlangValue> = Vec::with_capacity(call_args.len());
            for arg in call_args {
                args.push(this.eval_expr(arg));
            }

            args
        });

        self.with_new_scope(|interpreter| {
            for (param, arg) in fn_decl.parameters.iter().zip(args.iter()) {
                interpreter
                    .current_scope
                    .borrow_mut()
                    .values
                    .insert(param.name.to_string(), *arg);
            }
            interpreter.eval_block(&fn_decl.body)
        })
    }
}

#[cfg(test)]
mod tests {
    use std::error::Error;

    use super::*;
    use indoc::indoc;
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

    #[test]
    fn test_simple_logic_operators() {
        let mut interpreter = interpreter("");

        let tests = [
            ("true && true", TlangValue::Bool(true)),
            ("true && false", TlangValue::Bool(false)),
            ("false && true", TlangValue::Bool(false)),
            ("false && false", TlangValue::Bool(false)),
            ("true || true", TlangValue::Bool(true)),
            ("true || false", TlangValue::Bool(true)),
            ("false || true", TlangValue::Bool(true)),
            ("false || false", TlangValue::Bool(false)),
        ];

        for (src, expected_value) in tests.iter() {
            match (eval(&mut interpreter, src), expected_value) {
                (TlangValue::Bool(actual), TlangValue::Bool(expected)) => {
                    assert_eq!(actual, *expected)
                }
                _ => panic!("Unexpected value"),
            }
        }
    }

    #[test]
    fn test_simple_function_declaration_and_call() {
        let mut interpreter = interpreter(indoc! {"
            fn add(a: Int, b: Int) -> Int {
                a + b
            }
        "});

        assert_matches!(eval(&mut interpreter, "add(1, 2)"), TlangValue::Int(3));
    }

    #[test]
    fn test_simple_recursive_fib() {
        let mut interpreter = interpreter(indoc! {"
            fn fib(n: Int) -> Int {
                if n <= 1 {
                    n
                } else {
                    fib(n - 1) + fib(n - 2)
                }
            }
        "});

        assert_matches!(eval(&mut interpreter, "fib(10)"), TlangValue::Int(55));
    }
}
