#![feature(if_let_guard)]
#![feature(box_patterns)]
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use log::debug;
use tlang_ast::node::{Ident, UnaryOp};
use tlang_ast::token;
use tlang_hir::hir::{self, HirId};

use self::resolver::Resolver;
use self::scope::Scope;
use self::state::InterpreterState;
use self::stdlib::collections::define_list_shape;
use self::value::{
    ShapeKey, TlangClosure, TlangNativeFn, TlangObjectId, TlangObjectKind, TlangStruct,
    TlangStructMethod, TlangStructShape, TlangValue,
};

mod resolver;
mod scope;
pub mod state;
#[cfg(feature = "stdlib")]
pub mod stdlib;
pub mod value;

pub struct NativeFn {
    pub name: &'static str,
    pub binding_name: &'static str,
    pub function: fn(&mut InterpreterState, &[TlangValue]) -> TlangValue,
    pub module_path: &'static str,
}

inventory::collect!(NativeFn);

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

    fn resolve_struct_decl(&self, path: &hir::Path) -> Option<Rc<hir::StructDeclaration>> {
        self.state.resolve_struct_decl(path)
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
        let list_shape_key = ShapeKey::new_native();

        let mut interpreter = Self {
            state: InterpreterState::new(list_shape_key),
            native_fns: HashMap::new(),
        };

        define_list_shape(&mut interpreter);

        for native_fn in inventory::iter::<NativeFn> {
            let fn_name = if native_fn.binding_name.is_empty() {
                let module_name = native_fn.module_path.split("::").last().unwrap();
                module_name.to_string() + "::" + native_fn.name
            } else {
                native_fn.binding_name.to_string()
            };

            interpreter.define_native_fn(&fn_name, native_fn.function);
        }

        interpreter
    }

    #[inline(always)]
    fn insert_native_fn<F>(&mut self, id: TlangObjectId, f: F)
    where
        F: Fn(&mut InterpreterState, &[TlangValue]) -> TlangValue + 'static,
    {
        self.native_fns.insert(id, Box::new(f));
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

        self.insert_native_fn(fn_object.get_object_id().unwrap(), f);
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
            hir::StmtKind::EnumDeclaration(decl) => {
                self.eval_enum_decl(decl);
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
            hir::StmtKind::None => unreachable!(),
        }

        StmtResult::None
    }

    fn eval_exprs(&mut self, exprs: &[hir::Expr]) -> Vec<TlangValue> {
        exprs.iter().map(|expr| self.eval_expr(expr)).collect()
    }

    fn eval_expr(&mut self, expr: &hir::Expr) -> TlangValue {
        match &expr.kind {
            hir::ExprKind::Path(path) => self.resolve_path(path).unwrap_or_else(|| {
                panic!(
                    "Could not resolve path: {}\nCurrent scope: {:#?}",
                    path.join("::"),
                    self.state.current_scope.borrow()
                )
            }),
            hir::ExprKind::Literal(value) => self.eval_literal(value),
            hir::ExprKind::List(values) => self.eval_list_expr(values),
            hir::ExprKind::Dict(entries) => self.eval_dict_expr(entries),
            hir::ExprKind::IndexAccess(lhs, rhs) => self.eval_index_access(lhs, rhs),
            hir::ExprKind::FieldAccess(lhs, rhs) => self.eval_field_access(lhs, rhs),
            hir::ExprKind::Block(block) => self.eval_block(block),
            hir::ExprKind::Binary(op, lhs, rhs) => self.eval_binary(*op, lhs, rhs),
            hir::ExprKind::Call(call_expr) => self.eval_call(call_expr),
            hir::ExprKind::TailCall(call_expr) => self.eval_tail_call(call_expr),
            hir::ExprKind::Unary(op, expr) => self.eval_unary(*op, expr),
            hir::ExprKind::IfElse(condition, consequence, else_clauses) => {
                self.eval_if_else(condition, consequence, else_clauses)
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

    fn eval_if_else(
        &mut self,
        condition: &hir::Expr,
        consequence: &hir::Block,
        else_clauses: &[hir::ElseClause],
    ) -> TlangValue {
        if self.eval_expr(condition).is_truthy(&self.state) {
            return self.eval_block(consequence);
        }

        for else_clause in else_clauses {
            if let Some(condition) = &else_clause.condition {
                if self.eval_expr(condition).is_truthy(&self.state) {
                    return self.eval_block(&else_clause.consequence);
                }
            } else {
                return self.eval_block(&else_clause.consequence);
            }
        }
        TlangValue::Nil
    }

    fn eval_index_access(&mut self, lhs: &hir::Expr, rhs: &hir::Expr) -> TlangValue {
        let rhs_value = self.eval_expr(rhs);

        let index = match rhs_value {
            TlangValue::Int(index) => index,
            TlangValue::Float(index) => index as i64,
            _ => todo!("eval_index_access: {:?}", rhs_value),
        };

        let lhs_value = self.eval_expr(lhs);

        if let Some(TlangObjectKind::Struct(obj)) = self.state.get_object(lhs_value) {
            return obj.field_values[index as usize];
        }

        todo!("eval_index_access: {:?}[{:?}]", lhs, rhs_value)
    }

    fn eval_field_access(&mut self, lhs: &hir::Expr, ident: &Ident) -> TlangValue {
        let value = self.eval_expr(lhs);
        if let Some(TlangObjectKind::Struct(obj)) = self.state.get_object(value) {
            if let Some(index) = self.state.get_field_index(obj.shape, ident.as_str()) {
                return obj.field_values[index];
            }
        }

        todo!("eval_field_access: {:?}.{:?}", lhs, ident);
    }

    fn eval_literal(&mut self, literal: &token::Literal) -> TlangValue {
        match literal {
            token::Literal::Integer(value) => TlangValue::Int(*value),
            token::Literal::UnsignedInteger(value) => TlangValue::Int(*value as i64),
            token::Literal::Float(value) => TlangValue::Float(*value),
            token::Literal::Boolean(value) => TlangValue::Bool(*value),
            token::Literal::String(value) => self.state.new_string(value.to_string()),
            token::Literal::Char(value) => self.state.new_string(value.to_string()),
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

                if lhs.is_truthy(&self.state) {
                    let rhs = self.eval_expr(rhs);

                    if rhs.is_truthy(&self.state) {
                        return TlangValue::Bool(true);
                    }
                }

                return TlangValue::Bool(false);
            }

            hir::BinaryOpKind::Or => {
                let lhs = self.eval_expr(lhs);

                if lhs.is_truthy(&self.state) {
                    return TlangValue::Bool(true);
                }

                return self.eval_expr(rhs);
            }

            _ => {}
        }

        let lhs = self.eval_expr(lhs);
        let rhs = self.eval_expr(rhs);

        if let TlangValue::Object(_) = lhs {
            return self.eval_object_binary_op(op, lhs, rhs);
        }

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

    fn eval_unary(&mut self, op: UnaryOp, expr: &hir::Expr) -> TlangValue {
        match op {
            UnaryOp::Not => TlangValue::Bool(!self.eval_expr(expr).is_truthy(&self.state)),
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
            (TlangValue::Object(_), _) => unreachable!(),
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
            (TlangValue::Object(_), _) => unreachable!(),
            _ => todo!("incompatible types, {:?} and {:?}", lhs, rhs),
        }
    }

    fn eval_object_binary_op(
        &mut self,
        op: hir::BinaryOpKind,
        lhs: TlangValue,
        rhs: TlangValue,
    ) -> TlangValue {
        match op {
            hir::BinaryOpKind::Eq | hir::BinaryOpKind::NotEq => {
                let is_not = matches!(op, hir::BinaryOpKind::NotEq);

                match (lhs, rhs) {
                    (TlangValue::Object(lhs_id), TlangValue::Object(rhs_id)) => {
                        if lhs_id == rhs_id {
                            return TlangValue::Bool(true ^ is_not);
                        }

                        let lhs = self.get_object(lhs_id);
                        let rhs = self.get_object(rhs_id);

                        match (lhs, rhs) {
                            (TlangObjectKind::Struct(lhs), TlangObjectKind::Struct(rhs)) => {
                                if lhs.shape != rhs.shape {
                                    return TlangValue::Bool(false ^ is_not);
                                }

                                // TODO: Implement comparisons between structs of the same shape.
                                TlangValue::Bool(false ^ is_not)
                            }
                            (TlangObjectKind::String(lhs), TlangObjectKind::String(rhs)) => {
                                TlangValue::Bool((lhs == rhs) ^ is_not)
                            }
                            _ => todo!("eval_object_binary_op: {:?}, {:?}, {:?}", op, lhs, rhs),
                        }
                    }
                    _ => todo!("eval_object_binary_op: {:?}, {:?}, {:?}", op, lhs, rhs),
                }
            }
            hir::BinaryOpKind::Add => match (lhs, rhs) {
                (TlangValue::Object(lhs_id), TlangValue::Object(rhs_id)) => {
                    let lhs = self.get_object(lhs_id);
                    let rhs = self.get_object(rhs_id);

                    match (lhs, rhs) {
                        (TlangObjectKind::String(lhs), TlangObjectKind::String(rhs)) => {
                            self.state.new_string(lhs.clone() + rhs)
                        }
                        _ => todo!("eval_object_binary_op: {:?}, {:?}, {:?}", op, lhs, rhs),
                    }
                }
                _ => todo!("eval_object_binary_op: {:?}, {:?}, {:?}", op, lhs, rhs),
            },
            _ => todo!("eval_object_binary_op: {:?}, {:?}, {:?}", op, lhs, rhs),
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

        match &decl.name.kind {
            hir::ExprKind::Path(ref path) => {
                let path_name = path.join("::");
                let fn_object = self.state.new_object(TlangObjectKind::Fn(decl.hir_id));

                self.state
                    .current_scope
                    .borrow_mut()
                    .values
                    .insert(path_name, fn_object);
            }
            hir::ExprKind::FieldAccess(expr, ident) => {
                let path = match &expr.kind {
                    hir::ExprKind::Path(path) => path,
                    _ => todo!("eval_fn_decl: {:?}", expr),
                };
                let struct_decl = self.resolve_struct_decl(path).unwrap();

                self.state.set_struct_method(
                    struct_decl.hir_id.into(),
                    ident.as_str(),
                    TlangStructMethod::HirId(decl.hir_id),
                );
            }
            _ => todo!("eval_fn_decl: {:?}", decl.name),
        }
    }

    fn eval_struct_decl(&mut self, decl: &hir::StructDeclaration) {
        self.state
            .current_scope
            .borrow_mut()
            .struct_decls
            .insert(decl.name.to_string(), Rc::new(decl.clone()));

        let struct_shape = TlangStructShape::new(
            decl.name.to_string(),
            decl.fields
                .iter()
                .map(|field| field.name.to_string())
                .collect(),
            HashMap::new(),
        );

        self.state.shapes.insert(decl.hir_id.into(), struct_shape);
    }

    fn eval_enum_decl(&mut self, decl: &hir::EnumDeclaration) {
        // Simple enums should be treated as incrementing integers.
        // Simple enums are enums where all variants are just a name.
        // The enum variants themselves will just be treated as integers.
        //
        // Enums with multiple fields should be treated as structs.
        // If a variant has no field, it should be treated as a struct with a single field,
        // and struct with its value with the qualified name shall be put into scope.
        let is_simple_enum = decl
            .variants
            .iter()
            .all(|variant| variant.parameters.is_empty());

        if is_simple_enum {
            for (value, variant) in decl.variants.iter().enumerate() {
                let path = format!("{}::{}", decl.name, variant.name.as_str());

                self.state
                    .current_scope
                    .borrow_mut()
                    .values
                    .insert(path, TlangValue::Int(value as i64));
            }
        } else {
            for (value, variant) in decl.variants.iter().enumerate() {
                // Enum variants with no fields should be treated as incrementing numbers.
                // Enums with multiple fields, should be treated as structs.
                if variant.parameters.is_empty() {
                    let path = format!("{}::{}", decl.name, variant.name.as_str());

                    self.state.define_struct_shape(
                        decl.hir_id.into(),
                        path.clone(),
                        vec!["0".to_string()],
                        HashMap::new(),
                    );

                    let obj = self.state.new_object(TlangObjectKind::Struct(TlangStruct {
                        shape: decl.hir_id.into(),
                        field_values: vec![TlangValue::Int(value as i64)],
                    }));

                    self.state
                        .current_scope
                        .borrow_mut()
                        .values
                        .insert(path, obj);
                } else {
                    self.eval_struct_decl(&map_enum_variant_decl_to_struct_decl(
                        variant, &decl.name,
                    ));
                }
            }
        }
    }

    fn eval_tail_call(&mut self, call_expr: &hir::CallExpression) -> TlangValue {
        // For now, we just call the function normally.
        self.eval_call(call_expr)
    }

    fn eval_call(&mut self, call_expr: &hir::CallExpression) -> TlangValue {
        debug!("eval_call: {:?}", call_expr);

        match &call_expr.callee.kind {
            hir::ExprKind::Path(path)
                if let Some(TlangValue::Object(id)) = self.resolve_path(path) =>
            {
                let args = self.eval_exprs(&call_expr.arguments);

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
                    self.exec_native_fn(id, &args)
                } else {
                    panic!(
                        "`{:?}` is not a function: {:?}",
                        path.join("::"),
                        self.get_object(id)
                    );
                }
            }
            // If the path resolves to a struct, we create a new object.
            hir::ExprKind::Path(path) if let Some(struct_decl) = self.resolve_struct_decl(path) => {
                // Struct calls are always calls with a single argument, which is a dict.
                // There is a special case for enum variant construction, as they might have
                // multiple arguments which are not wrapped in a dict but are struct values.
                // Currently the only way to distinguish this, is by checking whether the struct
                // definition is a struct with incremental numeric field names, which might cause
                // problems in case someone defines a struct with numeric fields themselves.
                let is_enum_variant_without_field_names = struct_decl
                    .fields
                    .iter()
                    .enumerate()
                    .all(|(i, field)| field.name.as_str() == i.to_string());

                let dict_map: HashMap<String, TlangValue> = if is_enum_variant_without_field_names {
                    call_expr
                        .arguments
                        .iter()
                        .enumerate()
                        .map(|(i, arg)| (i.to_string(), self.eval_expr(arg)))
                        .collect()
                } else {
                    let dict = &call_expr.arguments[0];
                    match &dict.kind {
                        hir::ExprKind::Dict(entries) => entries
                            .iter()
                            .map(|(key, value)| {
                                let key = match &key.kind {
                                    hir::ExprKind::Path(path) => path.first_ident().to_string(),
                                    _ => todo!("eval_call: {:?}", key),
                                };
                                let value = self.eval_expr(value);
                                (key, value)
                            })
                            .collect(),
                        _ => todo!("eval_call: {:?}", dict),
                    }
                };

                let field_values = struct_decl
                    .fields
                    .iter()
                    .map(|field| *dict_map.get(&field.name.to_string()).unwrap())
                    .collect();

                self.state.new_object(TlangObjectKind::Struct(TlangStruct {
                    shape: struct_decl.hir_id.into(),
                    field_values,
                }))
            }
            hir::ExprKind::Path(path) => {
                panic!(
                    "Function `{}` not found\nCurrent scope: {:?}",
                    path.join("::"),
                    self.state.current_scope.borrow()
                );
            }
            hir::ExprKind::FieldAccess(expr, ident) => {
                let call_target = self.eval_expr(expr);
                let mut args = self.eval_exprs(&call_expr.arguments);
                args.insert(0, call_target);

                match call_target {
                    TlangValue::Object(_id) => {
                        let struct_shape = self
                            .state
                            .get_object(call_target)
                            .and_then(|o| o.get_shape_key())
                            .and_then(|key| self.state.get_shape(key))
                            .unwrap();

                        match struct_shape.method_map.get(ident.as_str()) {
                            Some(TlangStructMethod::HirId(id)) => {
                                let fn_decl = self.resolve_fn_decl(*id).unwrap().clone();
                                self.with_scope(self.state.root_scope.clone(), |this| {
                                    this.eval_fn_call(&fn_decl, &args)
                                })
                            }
                            Some(TlangStructMethod::Native(id)) => self.exec_native_fn(*id, &args),
                            _ => {
                                panic!(
                                    "{} does not have a method {:?}, {:?}",
                                    struct_shape.name,
                                    ident.as_str(),
                                    struct_shape.method_map.keys()
                                );
                            }
                        }
                    }
                    _ => todo!("eval_call: {:?}", call_target),
                }
            }
            _ => todo!("eval_call: {:?}", call_expr.callee),
        }
    }

    fn eval_fn_call(
        &mut self,
        fn_decl: &hir::FunctionDeclaration,
        args: &[TlangValue],
    ) -> TlangValue {
        debug!("eval_fn_call: {:?} with args {:?}", fn_decl.name, args);

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

    fn exec_native_fn(&mut self, id: TlangObjectId, args: &[TlangValue]) -> TlangValue {
        // Do we need and want to reset the scope for native functions?
        // It could be somewhat interesting to manipulate the current scope from native functions.
        self.with_scope(self.state.root_scope.clone(), |this| {
            let result = if let Some(native_fn) = this.native_fns.get_mut(&id) {
                native_fn(&mut this.state, args)
            } else {
                panic!("Native function not found: {:?}", id);
            };
            result
        })
    }

    fn eval_let_stmt(&mut self, pat: &hir::Pat, expr: &hir::Expr, _ty: &hir::Ty) {
        let value = self.eval_expr(expr);

        if !self.eval_pat(pat, &value) {
            // We'd probably want to do it more like Rust via a if let statement, and have the
            // normal let statement be only valid for identifiers.
            panic!("Pattern did not match value");
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

    fn eval_dict_expr(&mut self, entries: &[(hir::Expr, hir::Expr)]) -> TlangValue {
        let mut field_values = Vec::with_capacity(entries.len());
        let mut shape_keys = Vec::with_capacity(entries.len());

        for entry in entries {
            field_values.push(self.eval_expr(&entry.1));

            // As we primarily had compilation to JS in mind, paths here should actually be
            // strings instead. Need to update the parser to emit strings instead of paths,
            // and paths when using brackets.
            match &entry.0.kind {
                hir::ExprKind::Path(path) => shape_keys.push(path.first_ident().to_string()),
                _ => todo!("eval_dict_expr: {:?}", entry.0),
            };
        }

        let shape = ShapeKey::from_dict_keys(&shape_keys);

        if self.state.get_shape(shape).is_none() {
            self.state
                .define_struct_shape(shape, "Dict".to_string(), shape_keys, HashMap::new());
        }

        self.state.new_object(TlangObjectKind::Struct(TlangStruct {
            shape,
            field_values,
        }))
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
                    if !this.eval_expr(expr).is_truthy(&this.state) {
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

                if value == &literal_value {
                    return true;
                }

                if let (TlangValue::Object(lhs), box token::Literal::String(rhs)) =
                    (value, &literal)
                {
                    if let TlangObjectKind::String(lhs) = self.get_object(*lhs) {
                        return lhs == rhs;
                    }
                }

                false
            }
            // TODO: Instead of having rest patterns within list patterns, we should have a pattern
            //       specifically for matching against tail values (list, strings, objects)
            hir::PatKind::List(patterns) => {
                if let TlangValue::Object(id) = value {
                    match self.get_object(*id) {
                        TlangObjectKind::Struct(list_struct) => {
                            let list_values_length = list_struct.field_values.len();

                            // Empty list pattern is a special case, it only matches lists with 0 elements.
                            if patterns.is_empty() && list_values_length >= 1 {
                                return false;
                            }

                            // Not enough values in the list to match the pattern.
                            // Rest patterns are allowed to match 0 values.
                            let patterns_len = patterns.len()
                                - patterns.iter().find(|p| p.is_rest()).map_or(0, |_| 1);
                            if patterns_len > list_values_length {
                                return false;
                            }

                            let field_values = list_struct.field_values.clone();

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
                        TlangObjectKind::String(str_value) => {
                            let str_value = str_value.clone();
                            for (i, pat) in patterns.iter().enumerate() {
                                if let hir::PatKind::Rest(pat) = &pat.kind {
                                    let rest_object = if i <= str_value.len() {
                                        let rest_values = str_value[i..].to_string();
                                        self.state.new_string(rest_values)
                                    } else {
                                        self.state.new_string(String::new())
                                    };

                                    return self.eval_pat(pat, &rest_object);
                                }

                                let char_match = if let Some(character) = str_value.chars().nth(i) {
                                    self.state.new_string(character.to_string())
                                } else {
                                    self.state.new_string(String::new())
                                };

                                if !self.eval_pat(pat, &char_match) {
                                    return false;
                                }
                            }

                            return true;
                        }
                        _ => todo!("eval_pat: {:?}", value),
                    }
                }
                false
            }
            hir::PatKind::Identifier(_id, ident) => {
                debug!("eval_pat: assigning {:?} to {}", value, ident);

                match value {
                    TlangValue::Object(id) => debug!("value: {:?}", self.get_object(*id)),
                    _ => debug!("value: {:?}", value),
                }

                self.state
                    .current_scope
                    .borrow_mut()
                    .values
                    .insert(ident.to_string(), *value);
                true
            }
            hir::PatKind::Wildcard => true,
            hir::PatKind::Enum(path, kvs) => match self.state.get_object(*value) {
                Some(TlangObjectKind::Struct(tlang_struct)) => {
                    let shape = self.state.get_shape(tlang_struct.shape).unwrap();
                    let path_name = path.join("::");

                    if shape.name != path_name {
                        return false;
                    }

                    let field_values = tlang_struct.field_values.clone();
                    let field_map = shape.field_map.clone();
                    kvs.iter()
                        .map(|(k, pat)| (field_map.get(&k.to_string()), pat))
                        .all(|(field_index, pat)| {
                            field_index.is_some_and(|field_index| {
                                self.eval_pat(pat, &field_values[*field_index])
                            })
                        })
                }
                _ => todo!("eval_pat: Enum({:?}, {:?})", path, kvs),
            },
            hir::PatKind::Rest(_) => unreachable!("Rest patterns can only appear in list patterns"),
        }
    }
}

fn map_enum_variant_decl_to_struct_decl(
    variant: &hir::EnumVariant,
    enum_name: &Ident,
) -> hir::StructDeclaration {
    let fields = variant
        .parameters
        .iter()
        .map(|field| hir::StructField {
            hir_id: field.hir_id,
            name: field.name.clone(),
            ty: field.ty.clone(),
        })
        .collect();

    hir::StructDeclaration {
        hir_id: variant.hir_id,
        name: Ident::new(
            &format!("{}::{}", enum_name.as_str(), variant.name.as_str()),
            enum_name.span,
        ),
        fields,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use pretty_assertions::assert_matches;

    #[ctor::ctor]
    fn before_all() {
        env_logger::init();
    }

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

    #[test]
    fn test_struct_field_access() {
        let mut interpreter = interpreter(indoc! {"
            struct Point {
                x: Int,
                y: Int,
            }
            let point = Point { x: 10, y: 20 };
        "});
        assert_matches!(interpreter.eval("point.x"), TlangValue::Int(10));
        assert_matches!(interpreter.eval("point.y"), TlangValue::Int(20));
    }

    #[test]
    fn test_enum_declaration_and_use() {
        let mut interpreter = interpreter(indoc! {"
            enum Values {
                One,
                Two,
                Three,
            }
        "});

        assert_matches!(interpreter.eval("Values::One"), TlangValue::Int(0));
        assert_matches!(interpreter.eval("Values::Two"), TlangValue::Int(1));
        assert_matches!(interpreter.eval("Values::Three"), TlangValue::Int(2));
    }

    #[test]
    fn test_tagged_enum_declaration_and_use() {
        let mut interpreter = interpreter(indoc! {"
            enum Option {
                None,
                Some(Int),
            }
            let some = Option::Some(10);
            let none = Option::None;
        "});

        assert!(interpreter
            .interpreter
            .state
            .shapes
            .iter()
            .any(|shape| shape.1.name == "Option::Some"));

        let some_value = interpreter.eval("some");
        let none_value = interpreter.eval("none");

        assert_matches!(some_value, TlangValue::Object(_));
        assert_matches!(none_value, TlangValue::Object(_));

        let some_data = match some_value {
            TlangValue::Object(id) => interpreter.interpreter.get_object(id).get_struct().unwrap(),
            val => panic!("Expected struct, got {:?}", val),
        };

        let none_data = match none_value {
            TlangValue::Object(id) => interpreter.interpreter.get_object(id).get_struct().unwrap(),
            val => panic!("Expected struct, got {:?}", val),
        };

        assert_matches!(some_data.field_values[0], TlangValue::Int(10));
        assert_matches!(none_data.field_values[0], TlangValue::Int(0));
    }
}
