#![feature(if_let_guard)]
#![feature(box_patterns)]
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use tlang_ast::node::UnaryOp;
use tlang_ast::token;
use tlang_hir::hir::{self, HirId};

use self::resolver::Resolver;
use self::scope::Scope;
use self::value::{
    TlangClosure, TlangNativeFn, TlangObjectId, TlangObjectKind, TlangStruct, TlangValue,
};

mod resolver;
mod scope;
pub mod value;

pub struct InterpreterState {
    root_scope: Rc<RefCell<Scope>>,
    current_scope: Rc<RefCell<Scope>>,
    closures: HashMap<HirId, hir::FunctionDeclaration>,
    objects: HashMap<TlangObjectId, TlangObjectKind>,
}

impl Default for InterpreterState {
    fn default() -> Self {
        let root_scope = Rc::new(RefCell::new(Scope::default()));
        let current_scope = root_scope.clone();

        Self {
            root_scope,
            current_scope,
            closures: HashMap::new(),
            objects: HashMap::new(),
        }
    }
}

impl Resolver for InterpreterState {
    fn resolve_path(&self, path: &hir::Path) -> Option<TlangValue> {
        self.current_scope.borrow().resolve_path(path)
    }

    fn resolve_fn_decl(&self, id: hir::HirId) -> Option<Rc<hir::FunctionDeclaration>> {
        self.current_scope.borrow().resolve_fn_decl(id)
    }
}

impl InterpreterState {
    fn enter_scope(&mut self) {
        let child_scope = Scope::new_child(self.current_scope.clone());
        self.current_scope = Rc::new(RefCell::new(child_scope));
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

    pub fn new_object(&mut self, kind: TlangObjectKind) -> TlangValue {
        let obj = TlangValue::new_object();

        self.objects.insert(obj.get_object_id().unwrap(), kind);

        obj
    }

    pub fn new_list(&mut self, values: Vec<TlangValue>) -> TlangValue {
        self.new_object(TlangObjectKind::Struct(TlangStruct {
            field_values: values,
        }))
    }
}

pub struct Interpreter {
    state: InterpreterState,
    native_fns: HashMap<TlangObjectId, TlangNativeFn>,
}

impl Resolver for Interpreter {
    fn resolve_path(&self, path: &hir::Path) -> Option<TlangValue> {
        self.state.resolve_path(path)
    }

    fn resolve_fn_decl(&self, id: hir::HirId) -> Option<Rc<hir::FunctionDeclaration>> {
        self.state.resolve_fn_decl(id)
    }
}

#[derive(Debug)]
enum StmtResult {
    None,
    Return,
}

impl Default for Interpreter {
    fn default() -> Self {
        Interpreter::new()
    }
}

impl Interpreter {
    pub fn new() -> Self {
        let mut interpreter = Self {
            state: Default::default(),
            native_fns: HashMap::new(),
        };

        interpreter.define_native_fn("log", |_, args| {
            println!(
                "{}",
                args.iter()
                    .map(|v| v.to_string())
                    .collect::<Vec<_>>()
                    .join(" ")
            );
            TlangValue::Nil
        });

        interpreter
    }

    pub fn define_native_fn<F>(&mut self, name: &str, f: F)
    where
        F: Fn(&mut InterpreterState, &[TlangValue]) -> TlangValue + 'static,
    {
        let fn_object = self.state.new_object(TlangObjectKind::NativeFn);

        self.state
            .root_scope
            .borrow_mut()
            .insert_value(name.to_string(), fn_object);

        self.native_fns
            .insert(fn_object.get_object_id().unwrap(), Box::new(f));
    }

    fn resolve_closure_decl(&self, id: HirId) -> Option<&hir::FunctionDeclaration> {
        self.state.closures.get(&id)
    }

    fn insert_closure_decl(&mut self, id: HirId, decl: hir::FunctionDeclaration) {
        self.state.closures.insert(id, decl);
    }

    fn get_object(&self, id: TlangObjectId) -> &TlangObjectKind {
        self.state.objects.get(&id).unwrap()
    }

    #[inline(always)]
    fn enter_scope(&mut self) {
        self.state.enter_scope();
    }

    #[inline(always)]
    fn exit_scope(&mut self) {
        self.state.exit_scope();
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
        let previous_scope = std::mem::replace(&mut self.state.current_scope, scope);
        let result = f(self);
        self.state.current_scope = previous_scope;
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
                .state
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
                self.state.current_scope.borrow_mut().return_value = Some(return_value);
                return StmtResult::Return;
            }
            hir::StmtKind::Return(_) => return StmtResult::Return,
            _ => todo!("eval_stmt: {:?}", stmt),
        }

        StmtResult::None
    }

    fn eval_expr(&mut self, expr: &hir::Expr) -> TlangValue {
        match &expr.kind {
            hir::ExprKind::Path(path) => self.resolve_path(path).unwrap_or(TlangValue::Nil),
            hir::ExprKind::Literal(value) => self.eval_literal(value),
            hir::ExprKind::List(values) => self.eval_list_expr(values),
            hir::ExprKind::Block(block) => self.eval_block(block),
            hir::ExprKind::Binary(op, lhs, rhs) => self.eval_binary(*op, lhs, rhs),
            hir::ExprKind::Call(call_expr) => self.eval_call(call_expr),
            hir::ExprKind::TailCall(call_expr) => self.eval_tail_call(call_expr),
            hir::ExprKind::Unary(op, expr) => self.eval_unary(*op, expr),
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

                self.state
                    .new_object(TlangObjectKind::Closure(TlangClosure {
                        id: fn_decl.hir_id,
                        scope: self.state.current_scope.clone(),
                    }))
            }
            hir::ExprKind::Match(expr, arms) => self.eval_match(expr, arms),
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

    fn eval_unary(&mut self, op: UnaryOp, _expr: &hir::Expr) -> TlangValue {
        match op {
            UnaryOp::Rest => unreachable!("Rest operator implemented in eval_list_expr"),
            _ => todo!("eval_unary: {:?}", op),
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
        self.state
            .current_scope
            .borrow_mut()
            .fn_decls
            .insert(decl.hir_id, Rc::new(decl.clone()));

        match decl.name.kind {
            hir::ExprKind::Path(ref path) => {
                let path_name = path.join("::");
                let fn_object = self.state.new_object(TlangObjectKind::Fn(decl.hir_id));

                self.state
                    .current_scope
                    .borrow_mut()
                    .values
                    .insert(path_name, fn_object);
            }
            _ => todo!("eval_fn_decl: {:?}", decl.name),
        }
    }

    fn eval_struct_decl(&mut self, decl: &hir::StructDeclaration) {
        self.state
            .current_scope
            .borrow_mut()
            .struct_decls
            .insert(decl.hir_id, Rc::new(decl.clone()));
    }

    fn eval_tail_call(&mut self, call_expr: &hir::CallExpression) -> TlangValue {
        // For now, we just call the function normally.
        self.eval_call(call_expr)
    }

    fn eval_call(&mut self, call_expr: &hir::CallExpression) -> TlangValue {
        let mut args: Vec<TlangValue> = Vec::with_capacity(call_expr.arguments.len());
        for arg in &call_expr.arguments {
            args.push(self.eval_expr(arg));
        }

        match &call_expr.callee.kind {
            hir::ExprKind::Path(path)
                if let Some(TlangValue::Object(id)) = self.resolve_path(path) =>
            {
                if let Some(closure) = self.get_object(id).get_closure() {
                    let closure_decl = self.resolve_closure_decl(closure.id).unwrap().clone();

                    self.with_scope(closure.scope.clone(), |this| {
                        this.eval_fn_call(&closure_decl, &args)
                    })
                } else if let Some(id) = self.get_object(id).get_fn_hir_id() {
                    let fn_decl = self
                        .resolve_fn_decl(id)
                        .unwrap_or_else(|| panic!("Function {} not found", path.join("::")))
                        .clone();

                    self.with_scope(self.state.root_scope.clone(), |this| {
                        this.eval_fn_call(&fn_decl, &args)
                    })
                } else if let TlangObjectKind::NativeFn = self.get_object(id) {
                    let current_scope = self.state.current_scope.clone();
                    self.state.current_scope = self.state.root_scope.clone();

                    let result = if let Some(native_fn) = self.native_fns.get_mut(&id) {
                        native_fn(&mut self.state, &args)
                    } else {
                        panic!(
                            "`{:?}` is not a function: {:?}",
                            path.join("::"),
                            self.get_object(id)
                        );
                    };

                    self.state.current_scope = current_scope;
                    result
                } else {
                    panic!(
                        "`{:?}` is not a function: {:?}",
                        path.join("::"),
                        self.get_object(id)
                    );
                }
            }
            hir::ExprKind::Path(path) => {
                panic!(
                    "Function `{}` not found\nCurrent scope: {:?}",
                    path.join("::"),
                    self.state.current_scope.borrow()
                );
            }
            _ => todo!("eval_call: {:?}", call_expr.callee),
        }
    }

    fn eval_fn_call(
        &mut self,
        fn_decl: &hir::FunctionDeclaration,
        args: &[TlangValue],
    ) -> TlangValue {
        if fn_decl.parameters.len() != args.len() {
            panic!(
                "Function `{:?}` expects {} arguments, but got {}",
                fn_decl.name,
                fn_decl.parameters.len(),
                args.len()
            );
        }

        self.with_new_scope(|interpreter| {
            for (param, arg) in fn_decl.parameters.iter().zip(args.iter()) {
                interpreter
                    .state
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
                self.state
                    .current_scope
                    .borrow_mut()
                    .values
                    .insert(ident.to_string(), value);
            }
            _ => todo!("eval_let_stmt: {:?}", pat),
        }
    }

    fn eval_list_expr(&mut self, values: &[hir::Expr]) -> TlangValue {
        let mut field_values = Vec::with_capacity(values.len());
        for expr in values {
            if let hir::ExprKind::Unary(UnaryOp::Spread, expr) = &expr.kind {
                let value = self.eval_expr(expr);

                if let TlangValue::Object(id) = value {
                    let struct_values = &self.get_object(id).get_struct().unwrap().field_values;
                    field_values.extend(struct_values);
                } else {
                    panic!("Expected list, got {:?}", value);
                }
            } else {
                field_values.push(self.eval_expr(expr));
            }
        }

        self.state.new_list(field_values)
    }

    fn eval_match(&mut self, expr: &hir::Expr, arms: &[hir::MatchArm]) -> TlangValue {
        let match_value = self.eval_expr(expr);

        for arm in arms {
            if let Some(value) = self.eval_match_arm(arm, &match_value) {
                return value;
            }
        }

        TlangValue::Nil
    }

    /// Evaluates a match arm and returns the value if it matches, otherwise returns None.
    fn eval_match_arm(&mut self, arm: &hir::MatchArm, value: &TlangValue) -> Option<TlangValue> {
        self.with_new_scope(|this| {
            if this.eval_pat(&arm.pat, value) {
                if let Some(expr) = &arm.guard {
                    if !this.eval_expr(expr).is_truthy() {
                        return None;
                    }
                }

                Some(this.eval_expr(&arm.expr))
            } else {
                None
            }
        })
    }

    fn eval_pat(&mut self, pat: &hir::Pat, value: &TlangValue) -> bool {
        match &pat.kind {
            hir::PatKind::Literal(literal) => {
                let literal_value = self.eval_literal(literal);
                *value == literal_value
            }
            hir::PatKind::List(patterns) => {
                if let TlangValue::Object(id) = value {
                    let list_values_length = self
                        .state
                        .objects
                        .get(id)
                        .and_then(|o| o.get_struct())
                        .map(|s| s.field_values.len())
                        .unwrap_or(0);

                    // Empty list pattern is a special case, it only matches lists with 0 elements.
                    if patterns.is_empty() && list_values_length >= 1 {
                        return false;
                    }

                    // Not enough values in the list to match the pattern.
                    // Rest patterns are allowed to match 0 values.
                    let patterns_len =
                        patterns.len() - patterns.iter().find(|p| p.is_rest()).map_or(0, |_| 1);
                    if patterns_len > list_values_length {
                        return false;
                    }

                    let field_values = self
                        .state
                        .objects
                        .get(id)
                        .and_then(|o| o.get_struct())
                        .map(|s| s.field_values.clone())
                        .unwrap_or_default();

                    for (i, pat) in patterns.iter().enumerate() {
                        if let hir::PatKind::Rest(pat) = &pat.kind {
                            let rest_values = field_values[i..].to_vec();
                            let rest_object = self.state.new_list(rest_values);

                            return self.eval_pat(pat, &rest_object);
                        }

                        let field_value = field_values[i];
                        if !self.eval_pat(pat, &field_value) {
                            return false;
                        }
                    }

                    return true;
                }

                false
            }
            hir::PatKind::Identifier(_id, ident) => {
                self.state
                    .current_scope
                    .borrow_mut()
                    .values
                    .insert(ident.to_string(), *value);
                true
            }
            hir::PatKind::Wildcard => true,
            hir::PatKind::Rest(_) => unreachable!("Rest patterns can only appear in list patterns"),
            _ => todo!("eval_pat: {:?}, {:?}", pat, value),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use pretty_assertions::assert_matches;

    struct TestInterpreter {
        lowering_context: tlang_ast_lowering::LoweringContext,
        interpreter: Interpreter,
    }

    impl TestInterpreter {
        fn new() -> Self {
            TestInterpreter {
                lowering_context: tlang_ast_lowering::LoweringContext::default(),
                interpreter: Interpreter::new(),
            }
        }

        fn eval_root(&mut self, src: &str) -> TlangValue {
            let mut parser = tlang_parser::parser::Parser::from_source(src);
            let ast = parser.parse().unwrap();
            let hir = self.lowering_context.lower_module_in_current_scope(&ast);
            self.interpreter.eval(&hir)
        }

        fn eval(&mut self, src: &str) -> TlangValue {
            let block = format!("{{ {} }};", src);
            let mut parser = tlang_parser::parser::Parser::from_source(&block);
            let ast = parser.parse().unwrap();
            let hir = self.lowering_context.lower_module_in_current_scope(&ast);

            match &hir.block.stmts[0].kind {
                hir::StmtKind::Expr(expr) => self.interpreter.eval_expr(expr),
                _ => todo!("eval: {:?}", hir),
            }
        }
    }

    fn interpreter(initial_source: &str) -> TestInterpreter {
        let mut interpreter = TestInterpreter::new();
        interpreter.eval_root(initial_source);
        interpreter
    }

    #[test]
    fn test_interpreter() {
        let mut interpreter = interpreter("");
        let result = interpreter.eval("");
        assert_matches!(result, TlangValue::Nil);
    }

    #[test]
    fn test_basic_arithmetic() {
        let mut interpreter = interpreter("");
        let result = interpreter.eval("1 + 2");
        assert_matches!(result, TlangValue::Int(3));
    }

    #[test]
    fn test_addition_of_mixed_ints_and_floats() {
        let mut interpreter = interpreter("");

        assert_matches!(interpreter.eval("1 + 2.0"), TlangValue::Float(3.0));
        assert_matches!(interpreter.eval("1.0 + 2"), TlangValue::Float(3.0));
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
            match (interpreter.eval(src), expected_value) {
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

        assert_matches!(interpreter.eval("add(1, 2)"), TlangValue::Int(3));
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

        assert_matches!(interpreter.eval("fib(10)"), TlangValue::Int(55));
        assert_matches!(interpreter.eval("fib(20)"), TlangValue::Int(6765));
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

        assert_matches!(interpreter.eval("add_5(10)"), TlangValue::Int(15));
        assert_matches!(interpreter.eval("add_5(20)"), TlangValue::Int(25));
    }

    #[test]
    fn test_simple_pattern_matching() {
        let mut interpreter = interpreter(indoc! {"
            fn match_test(x: Int) -> Int {
                match x; {
                    1 => 2,
                    2 => 3,
                    _ => 4
                }
            }
        "});

        assert_matches!(interpreter.eval("match_test(1)"), TlangValue::Int(2));
        assert_matches!(interpreter.eval("match_test(2)"), TlangValue::Int(3));
        assert_matches!(interpreter.eval("match_test(3)"), TlangValue::Int(4));
    }

    #[test]
    fn test_list_pattern_matching() {
        let mut interpreter = interpreter(indoc! {"
            fn match_test(x: List<Int>) -> Int {
                match x; {
                    [1, 2] => 2,
                    [2, 3] => 3,
                    _ => 4
                }
            }
        "});
        assert_matches!(interpreter.eval("match_test([1, 2])"), TlangValue::Int(2));
        assert_matches!(interpreter.eval("match_test([2, 3])"), TlangValue::Int(3));
        assert_matches!(interpreter.eval("match_test([3, 4])"), TlangValue::Int(4));
    }

    #[test]
    fn test_pattern_matching_with_binding() {
        let mut interpreter = interpreter(indoc! {"
            fn match_test(x: Int) -> Int {
                match x; {
                    1 => 2,
                    n => n,
                }
            }
        "});
        assert_matches!(interpreter.eval("match_test(1)"), TlangValue::Int(2));
        assert_matches!(interpreter.eval("match_test(2)"), TlangValue::Int(2));
    }

    #[test]
    fn test_fn_pattern_matching() {
        let mut interpreter = interpreter(indoc! {"
            fn fib(n) { fib(n, 0, 1) }
            fn fib(0, a, _) { a }
            fn fib(1, _, b) { b }
            fn fib(n, a, b) { fib(n - 1, b, a + b) }
        "});

        assert_matches!(interpreter.eval("fib(10)"), TlangValue::Int(55));
    }

    #[test]
    fn test_builtin_log() {
        let mut interpreter = interpreter(indoc! {""});
        let calls = Rc::new(RefCell::new(vec![]));

        let calls_tracker = calls.clone();

        let log_fn_object_id = interpreter
            .interpreter
            .state
            .root_scope
            .borrow()
            .values
            .get("log")
            .unwrap()
            .get_object_id()
            .unwrap();

        interpreter.interpreter.native_fns.insert(
            log_fn_object_id,
            Box::new(move |_, args| {
                calls_tracker.borrow_mut().push(args.to_vec());
                TlangValue::Nil
            }),
        );

        assert_matches!(interpreter.eval("log(10)"), TlangValue::Nil);
        assert_matches!(calls.borrow()[0][..], [TlangValue::Int(10)]);
    }
}
