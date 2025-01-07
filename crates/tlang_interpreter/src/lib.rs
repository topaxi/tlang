#![feature(if_let_guard)]
#![feature(box_patterns)]
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use tlang_ast::token;
use tlang_hir::hir::{self, HirId};

trait Resolver {
    fn resolve_path(&self, path: &hir::Path) -> Option<TlangValue>;
    fn resolve_fn(&self, path: &hir::Path) -> Option<Rc<hir::FunctionDeclaration>>;
    fn resolve_struct(&self, path: &hir::Path) -> Option<Rc<hir::StructDeclaration>>;
}

#[derive(Debug, Default)]
pub(crate) struct Scope {
    pub parent: Option<Rc<RefCell<Scope>>>,
    pub values: HashMap<String, TlangValue>,
    pub return_value: Option<TlangValue>,
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
pub(crate) struct RootScope {
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
    pub id: HirId,
    // Closures hold a reference to the parent scope.
    pub(crate) scope: Rc<RefCell<Scope>>,
}

#[derive(Debug)]
pub struct TlangStruct {
    pub id: u64,
    pub field_values: Vec<TlangValue>,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct TlangObjectId(u64);

impl Default for TlangObjectId {
    fn default() -> Self {
        Self::new()
    }
}

impl TlangObjectId {
    pub fn new() -> Self {
        static NEXT_ID: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
        TlangObjectId(NEXT_ID.fetch_add(1, std::sync::atomic::Ordering::SeqCst))
    }
}

#[derive(Debug)]
pub enum TlangObjectKind {
    Struct,
    Fn(TlangClosure),
}

#[derive(Debug, Clone, Copy)]
pub enum TlangValue {
    Nil,
    Int(i64),
    Float(f64),
    Bool(bool),
    Object(TlangObjectId),
}

impl TlangValue {
    pub fn new_object() -> Self {
        TlangValue::Object(TlangObjectId::new())
    }

    pub fn get_object_id(&self) -> Option<TlangObjectId> {
        match self {
            TlangValue::Object(id) => Some(*id),
            _ => None,
        }
    }
}

#[derive(Debug, Default)]
pub struct Interpreter {
    root_scope: RootScope,
    current_scope: Rc<RefCell<Scope>>,
    closures: HashMap<HirId, hir::FunctionDeclaration>,
    objects: HashMap<TlangObjectId, TlangObjectKind>,
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

#[derive(Debug)]
enum StmtResult {
    None,
    Return,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter::default()
    }

    fn resolve_closure_decl(&self, id: HirId) -> Option<&hir::FunctionDeclaration> {
        self.closures.get(&id)
    }

    fn insert_closure_decl(&mut self, id: HirId, decl: hir::FunctionDeclaration) {
        self.closures.insert(id, decl);
    }

    fn get_object(&self, id: TlangObjectId) -> &TlangObjectKind {
        self.objects.get(&id).unwrap()
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

    #[inline(always)]
    fn with_scope<F, R>(&mut self, scope: Rc<RefCell<Scope>>, f: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        let previous_scope = self.current_scope.clone();
        self.current_scope = scope;
        let result = f(self);
        self.current_scope = previous_scope;
        result
    }

    pub fn eval(&mut self, input: &hir::Module) -> TlangValue {
        self.eval_block_inner(&input.block)
    }

    #[inline(always)]
    fn eval_block_stmts(&mut self, stmts: &[hir::Stmt]) -> StmtResult {
        for stmt in stmts {
            match self.eval_stmt(stmt) {
                StmtResult::Return => return StmtResult::Return,
                StmtResult::None => {}
            }
        }

        StmtResult::None
    }

    #[inline(always)]
    fn eval_block_expr(&mut self, block: &hir::Block) -> TlangValue {
        if let Some(expr) = &block.expr {
            self.eval_expr(expr)
        } else {
            TlangValue::Nil
        }
    }

    fn eval_block(&mut self, block: &hir::Block) -> TlangValue {
        self.with_new_scope(|this| this.eval_block_inner(block))
    }

    #[inline(always)]
    fn eval_block_inner(&mut self, block: &hir::Block) -> TlangValue {
        match self.eval_block_stmts(&block.stmts) {
            StmtResult::None => self.eval_block_expr(block),
            StmtResult::Return => self
                .current_scope
                .borrow_mut()
                .return_value
                .take()
                .unwrap_or(TlangValue::Nil),
        }
    }

    fn eval_stmt(&mut self, stmt: &hir::Stmt) -> StmtResult {
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
            hir::StmtKind::Let(pat, expr, ty) => {
                self.eval_let_stmt(pat, expr, ty);
            }
            hir::StmtKind::Return(box Some(expr)) => {
                let return_value = self.eval_expr(expr);
                self.current_scope.borrow_mut().return_value = Some(return_value);
                return StmtResult::Return;
            }
            hir::StmtKind::Return(_) => return StmtResult::Return,
            _ => todo!("eval_stmt: {:?}", stmt),
        }

        StmtResult::None
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
            hir::ExprKind::FunctionExpression(fn_decl) => {
                if self.resolve_closure_decl(fn_decl.hir_id).is_none() {
                    self.insert_closure_decl(fn_decl.hir_id, *fn_decl.clone());
                }

                let closure = TlangValue::new_object();

                self.objects.insert(
                    closure.get_object_id().unwrap(),
                    TlangObjectKind::Fn(TlangClosure {
                        id: fn_decl.hir_id,
                        scope: self.current_scope.clone(),
                    }),
                );

                closure
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
            _ => todo!(
                "eval_arithmetic_op: incompatible types, {:?} and {:?}",
                lhs,
                rhs
            ),
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

    fn eval_struct_decl(&mut self, decl: &hir::StructDeclaration) {
        self.current_scope
            .borrow_mut()
            .struct_decls
            .insert(decl.name.to_string(), Rc::new(decl.clone()));
    }

    fn eval_call(&mut self, call_expr: &hir::CallExpression) -> TlangValue {
        match &call_expr.callee.kind {
            hir::ExprKind::Path(path) if let Some(fn_decl) = self.resolve_fn(path) => {
                self.eval_fn_call(&fn_decl, &call_expr.arguments)
            }
            hir::ExprKind::Path(path)
                if let Some(TlangValue::Object(id)) = self.resolve_path(path) =>
            {
                match self.get_object(id) {
                    TlangObjectKind::Fn(closure) => {
                        let closure_decl = self.resolve_closure_decl(closure.id).unwrap().clone();

                        self.with_scope(closure.scope.clone(), |this| {
                            this.eval_fn_call(&closure_decl, &call_expr.arguments)
                        })
                    }
                    obj => todo!("eval_call: {:?}", obj),
                }
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

    fn eval_let_stmt(&mut self, pat: &hir::Pat, expr: &hir::Expr, _ty: &hir::Ty) {
        match &pat.kind {
            hir::PatKind::Identifier(_id, ident) => {
                let value = self.eval_expr(expr);
                self.current_scope
                    .borrow_mut()
                    .values
                    .insert(ident.to_string(), value);
            }
            _ => todo!("eval_let_stmt: {:?}", pat),
        }
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
        assert_matches!(eval(&mut interpreter, "fib(20)"), TlangValue::Int(6765));
    }

    #[test]
    fn test_simple_closure() {
        let mut interpreter = interpreter(indoc! {"
            fn make_adder(a: Int) {
                return fn adder(b: Int) -> Int {
                    a + b
                };
            }

            let add_5 = make_adder(5);
        "});

        assert_matches!(eval(&mut interpreter, "add_5(10)"), TlangValue::Int(15));
        assert_matches!(eval(&mut interpreter, "add_5(20)"), TlangValue::Int(25));
    }
}
