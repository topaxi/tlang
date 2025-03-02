#![feature(if_let_guard)]
#![feature(box_patterns)]
use std::collections::HashMap;
use std::rc::Rc;

use log::debug;
use tlang_ast::node::{Ident, UnaryOp};
use tlang_ast::token;
use tlang_hir::hir::{self, HirId};

use self::resolver::Resolver;
use self::shape::{ShapeKey, TlangStructMethod, TlangStructShape};
use self::state::{InterpreterState, TailCall};
use self::stdlib::collections::define_list_shape;
use self::value::{
    NativeFnReturn, TlangArithmetic, TlangNativeFn, TlangObjectId, TlangObjectKind, TlangSlice,
    TlangStruct, TlangValue,
};

mod resolver;
mod scope;
mod shape;
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

/// Propagate control flow (`Return`, `TailCall`, etc.), otherwise extract the value.
macro_rules! eval_value {
    ($expr:expr) => {
        match $expr {
            EvalResult::Value(val) => val,
            other => return other,
        }
    };
}

/// Propagate control flow if it's not `Value`
macro_rules! propagate {
    ($expr:expr) => {
        match $expr {
            EvalResult::Value(_) | EvalResult::Void => {}
            other => return other,
        }
    };
}

#[derive(Debug, Clone, Copy)]
pub enum EvalResult {
    Void,
    Value(TlangValue),
    Return(TlangValue),
    TailCall,
}

impl EvalResult {
    fn unwrap_value(self) -> TlangValue {
        match self {
            EvalResult::Value(value) => value,
            EvalResult::Return(value) => value,
            EvalResult::Void => TlangValue::Nil,
            EvalResult::TailCall => panic!("Tried to unwrap a TailCall"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum MatchResult {
    Matched(EvalResult),
    NotMatched(EvalResult),
}

pub struct Interpreter {
    state: InterpreterState,
    native_fns: HashMap<TlangObjectId, TlangNativeFn>,
}

impl Resolver for Interpreter {
    fn resolve_path(&self, path: &hir::Path) -> Option<TlangValue> {
        self.state.resolve_path(path)
    }
}

impl Default for Interpreter {
    fn default() -> Self {
        Interpreter::new()
    }
}

impl Interpreter {
    pub fn new() -> Self {
        let mut interpreter = Self {
            state: InterpreterState::new(),
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

            interpreter.define_native_fn(&fn_name, move |state, args| {
                NativeFnReturn::Return((native_fn.function)(state, args))
            });
        }

        interpreter
    }

    fn panic(&self, message: String) -> ! {
        self.state.panic(message)
    }

    pub fn create_native_fn<F>(&mut self, f: F) -> TlangValue
    where
        F: Fn(&mut InterpreterState, &[TlangValue]) -> NativeFnReturn + 'static,
    {
        let fn_object = self.state.new_object(TlangObjectKind::NativeFn);

        self.native_fns
            .insert(fn_object.get_object_id().unwrap(), Box::new(f));

        fn_object
    }

    pub fn define_native_fn<F>(&mut self, name: &str, f: F) -> TlangValue
    where
        F: Fn(&mut InterpreterState, &[TlangValue]) -> NativeFnReturn + 'static,
    {
        let fn_object = self.create_native_fn(f);

        self.state.globals.insert(name.to_string(), fn_object);

        fn_object
    }

    fn get_closure_decl(&self, id: HirId) -> Option<Rc<hir::FunctionDeclaration>> {
        self.state.closures.get(&id).cloned()
    }

    fn get_object_by_id(&self, id: TlangObjectId) -> &TlangObjectKind {
        self.state.objects.get(id).unwrap()
    }

    fn get_object(&self, value: TlangValue) -> Option<&TlangObjectKind> {
        self.state.get_object(value)
    }

    fn get_struct(&self, value: TlangValue) -> Option<&TlangStruct> {
        self.state.get_struct(value)
    }

    fn get_slice(&self, value: TlangValue) -> Option<TlangSlice> {
        self.state.get_slice(value)
    }

    fn get_slice_value(&self, slice: TlangSlice, index: usize) -> TlangValue {
        self.state.get_slice_value(slice, index)
    }

    fn get_slice_values(&self, slice: TlangSlice) -> &[TlangValue] {
        self.state.get_slice_values(slice)
    }

    fn get_shape_of(&self, value: TlangValue) -> Option<&TlangStructShape> {
        self.get_struct(value)
            .and_then(|obj| self.state.get_shape(obj.shape))
    }

    fn get_str(&self, value: TlangValue) -> Option<&str> {
        self.get_object(value).and_then(|obj| obj.get_str())
    }

    #[inline(always)]
    fn push_value(&mut self, value: TlangValue) {
        self.state.current_scope().borrow_mut().push_value(value);
    }

    #[inline(always)]
    pub(crate) fn enter_scope<T>(&mut self, meta: &T)
    where
        T: hir::HirScope,
    {
        self.state.enter_scope(meta);
    }

    #[inline(always)]
    fn exit_scope(&mut self) {
        self.state.exit_scope();
    }

    #[inline(always)]
    fn with_new_scope<T, F, R>(&mut self, meta: &T, f: F) -> R
    where
        T: hir::HirScope,
        F: FnOnce(&mut Self) -> R,
    {
        self.enter_scope(meta);
        let result = f(self);
        self.exit_scope();
        result
    }

    #[inline(always)]
    fn with_new_fn_scope<F, R>(&mut self, fn_decl: Rc<hir::FunctionDeclaration>, f: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        self.state
            .push_call_stack(state::CallStackEntry::new_call(fn_decl.clone()));
        self.enter_scope(&fn_decl.clone());
        self.state
            .scope_stack
            .current_scope()
            .borrow_mut()
            .is_fn_scope = true;
        let result = f(self);
        self.exit_scope();
        self.state.pop_call_stack();
        result
    }

    #[inline(always)]
    fn with_root_scope<F, R>(&mut self, f: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        let root_scope = self.state.scope_stack.as_root();
        self.with_scope(&root_scope, f)
    }

    #[inline(always)]
    fn with_scope<F, R>(&mut self, scope_stack: &scope::ScopeStack, f: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        let old_scope = std::mem::replace(&mut self.state.scope_stack, scope_stack.clone());
        let result = f(self);
        self.state.scope_stack = old_scope;
        result
    }

    pub fn eval(&mut self, input: &hir::Module) -> TlangValue {
        self.eval_block_inner(&input.block).unwrap_value()
    }

    #[inline(always)]
    fn eval_block_inner(&mut self, block: &hir::Block) -> EvalResult {
        propagate!(self.eval_stmts(&block.stmts));

        self.eval_block_expr(block)
    }

    #[inline(always)]
    fn eval_block_expr(&mut self, block: &hir::Block) -> EvalResult {
        if let Some(expr) = &block.expr {
            self.eval_expr(expr)
        } else {
            EvalResult::Void
        }
    }

    fn eval_block(&mut self, block: &hir::Block) -> EvalResult {
        self.with_new_scope(block, |this| this.eval_block_inner(block))
    }

    fn eval_stmt(&mut self, stmt: &hir::Stmt) -> EvalResult {
        self.state.set_current_span(stmt.span);

        match &stmt.kind {
            hir::StmtKind::Expr(expr) => {
                return self.eval_expr(expr);
            }
            hir::StmtKind::FunctionDeclaration(decl) => {
                self.eval_fn_decl(decl);
            }
            hir::StmtKind::DynFunctionDeclaration(decl) => {
                self.eval_dyn_fn_decl(decl);
            }
            hir::StmtKind::StructDeclaration(decl) => {
                self.eval_struct_decl(decl);
            }
            hir::StmtKind::EnumDeclaration(decl) => {
                self.eval_enum_decl(decl);
            }
            hir::StmtKind::Let(pat, expr, ty) => return self.eval_let_stmt(pat, expr, ty),
            hir::StmtKind::Return(box Some(expr)) => {
                return EvalResult::Return(eval_value!(self.eval_expr(expr)));
            }
            hir::StmtKind::Return(_) => return EvalResult::Return(TlangValue::Nil),
            hir::StmtKind::None => unreachable!(),
        }

        EvalResult::Void
    }

    fn eval_stmts(&mut self, stmts: &[hir::Stmt]) -> EvalResult {
        for stmt in stmts {
            propagate!(self.eval_stmt(stmt));
        }

        EvalResult::Void
    }

    fn eval_exprs(&mut self, exprs: &[hir::Expr]) -> Vec<TlangValue> {
        exprs
            .iter()
            .map(|expr| self.eval_expr(expr).unwrap_value())
            .collect()
    }

    fn eval_expr(&mut self, expr: &hir::Expr) -> EvalResult {
        self.state.set_current_span(expr.span);

        match &expr.kind {
            hir::ExprKind::Path(path) => {
                EvalResult::Value(self.resolve_path(path).unwrap_or_else(|| {
                    self.panic(format!(
                        "Could not resolve path: {} ({:?})\nCurrent scope: {:#?}",
                        path.join("::"),
                        path.res,
                        self.state.scope_stack
                    ))
                }))
            }
            hir::ExprKind::Literal(value) => EvalResult::Value(self.eval_literal(value)),
            hir::ExprKind::List(values) => self.eval_list_expr(values),
            hir::ExprKind::Dict(entries) => self.eval_dict_expr(entries),
            hir::ExprKind::IndexAccess(lhs, rhs) => self.eval_index_access(lhs, rhs),
            hir::ExprKind::FieldAccess(lhs, rhs) => self.eval_field_access(lhs, rhs),
            hir::ExprKind::Block(block) => self.eval_block(block),
            hir::ExprKind::Binary(op, lhs, rhs) => self.eval_binary(*op, lhs, rhs),
            hir::ExprKind::Call(call_expr) => self.eval_call(call_expr),
            hir::ExprKind::TailCall(call_expr) => self.eval_tail_call(call_expr),
            hir::ExprKind::Cast(_expr, _ty) => todo!("eval_expr: Cast"),
            hir::ExprKind::Unary(op, expr) => self.eval_unary(*op, expr),
            hir::ExprKind::IfElse(condition, consequence, else_clauses) => {
                self.eval_if_else(condition, consequence, else_clauses)
            }
            hir::ExprKind::FunctionExpression(fn_decl) => {
                EvalResult::Value(self.state.new_closure(fn_decl))
            }
            hir::ExprKind::Match(expr, arms) => self.eval_match(expr, arms),
            hir::ExprKind::Range(..) => todo!("eval_expr: Range"),
            hir::ExprKind::Let(..) => self.panic(
                "Let expressions are only valid in match guards and if expressions".to_string(),
            ),
            hir::ExprKind::Wildcard => self.panic("Wildcard not allowed here".to_string()),
        }
    }

    fn eval_if_else(
        &mut self,
        condition: &hir::Expr,
        consequence: &hir::Block,
        else_clauses: &[hir::ElseClause],
    ) -> EvalResult {
        if eval_value!(self.eval_expr(condition)).is_truthy(&self.state) {
            return self.eval_block(consequence);
        }

        for else_clause in else_clauses {
            if let Some(condition) = &else_clause.condition {
                if eval_value!(self.eval_expr(condition)).is_truthy(&self.state) {
                    return self.eval_block(&else_clause.consequence);
                }
            } else {
                return self.eval_block(&else_clause.consequence);
            }
        }

        EvalResult::Void
    }

    fn eval_index_access(&mut self, lhs: &hir::Expr, rhs: &hir::Expr) -> EvalResult {
        let rhs_value = eval_value!(self.eval_expr(rhs));
        let lhs_value = eval_value!(self.eval_expr(lhs));

        match self.get_object(lhs_value) {
            Some(TlangObjectKind::Struct(obj)) => {
                EvalResult::Value(obj.field_values[rhs_value.as_usize()])
            }
            Some(TlangObjectKind::Slice(slice)) => {
                EvalResult::Value(self.get_slice_value(*slice, rhs_value.as_usize()))
            }
            _ => todo!("eval_index_access: {:?}[{:?}]", lhs, rhs_value),
        }
    }

    fn eval_field_access(&mut self, lhs: &hir::Expr, ident: &Ident) -> EvalResult {
        let value = eval_value!(self.eval_expr(lhs));

        if let Some(TlangObjectKind::Struct(obj)) = self.get_object(value) {
            if let Some(index) = self.state.get_field_index(obj.shape, ident.as_str()) {
                return EvalResult::Value(obj.field_values[index]);
            }

            let shape = self.state.get_shape(obj.shape).unwrap();
            self.panic(format!(
                "Could not find field `{}` on {}",
                ident, shape.name
            ));
        }

        if value.is_nil() {
            self.panic(format!("Cannot access field `{}` on nil", ident));
        }

        if !value.is_object() {
            self.panic(format!("Cannot access field `{}` on non-object", ident));
        }

        todo!(
            "eval_field_access: {}.{}",
            self.state.stringify(value),
            ident
        );
    }

    fn eval_literal(&mut self, literal: &token::Literal) -> TlangValue {
        match literal {
            token::Literal::Integer(value) => TlangValue::I64(*value),
            token::Literal::UnsignedInteger(value) => TlangValue::U64(*value),
            token::Literal::Float(value) => TlangValue::F64(*value),
            token::Literal::Boolean(value) => TlangValue::Bool(*value),
            token::Literal::String(value) => self.state.new_string(value.to_string()),
            token::Literal::Char(value) => self.state.new_string(value.to_string()),
            token::Literal::None => unreachable!(),
        }
    }

    fn eval_unary(&mut self, op: UnaryOp, expr: &hir::Expr) -> EvalResult {
        match op {
            UnaryOp::Not => EvalResult::Value(TlangValue::Bool(
                !eval_value!(self.eval_expr(expr)).is_truthy(&self.state),
            )),
            UnaryOp::Rest => unreachable!("Rest operator implemented in eval_list_expr"),
            _ => todo!("eval_unary: {:?}", op),
        }
    }

    fn eval_binary(
        &mut self,
        op: hir::BinaryOpKind,
        lhs: &hir::Expr,
        rhs: &hir::Expr,
    ) -> EvalResult {
        match op {
            hir::BinaryOpKind::And => {
                let lhs = eval_value!(self.eval_expr(lhs));

                if lhs.is_truthy(&self.state) {
                    let rhs = eval_value!(self.eval_expr(rhs));

                    if rhs.is_truthy(&self.state) {
                        return EvalResult::Value(TlangValue::Bool(true));
                    }
                }

                return EvalResult::Value(TlangValue::Bool(false));
            }

            hir::BinaryOpKind::Or => {
                let lhs = eval_value!(self.eval_expr(lhs));

                if lhs.is_truthy(&self.state) {
                    return EvalResult::Value(TlangValue::Bool(true));
                }

                return self.eval_expr(rhs);
            }

            _ => {}
        }

        let lhs = eval_value!(self.eval_expr(lhs));
        let rhs = eval_value!(self.eval_expr(rhs));

        if let TlangValue::Object(_) = lhs {
            return self.eval_object_binary_op(op, lhs, rhs);
        }

        let value = match op {
            hir::BinaryOpKind::Add => lhs.add(rhs),
            hir::BinaryOpKind::Sub => lhs.sub(rhs),
            hir::BinaryOpKind::Mul => lhs.mul(rhs),
            hir::BinaryOpKind::Div => lhs.div(rhs),
            hir::BinaryOpKind::Mod => lhs.rem(rhs),
            hir::BinaryOpKind::Exp => lhs.pow(rhs),

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
        };

        EvalResult::Value(value)
    }

    fn eval_comparison_op<F>(&self, lhs: TlangValue, rhs: TlangValue, op: F) -> TlangValue
    where
        F: Fn(f64, f64) -> bool,
    {
        TlangValue::Bool(op(lhs.as_f64(), rhs.as_f64()))
    }

    fn eval_object_binary_op(
        &mut self,
        op: hir::BinaryOpKind,
        lhs: TlangValue,
        rhs: TlangValue,
    ) -> EvalResult {
        match op {
            hir::BinaryOpKind::Eq | hir::BinaryOpKind::NotEq => {
                let is_not = matches!(op, hir::BinaryOpKind::NotEq);

                match (lhs, rhs) {
                    (TlangValue::Object(lhs_id), TlangValue::Object(rhs_id)) => {
                        if lhs_id == rhs_id {
                            return EvalResult::Value(TlangValue::Bool(true ^ is_not));
                        }

                        let lhs = self.get_object_by_id(lhs_id);
                        let rhs = self.get_object_by_id(rhs_id);

                        match (lhs, rhs) {
                            (TlangObjectKind::Struct(lhs), TlangObjectKind::Struct(rhs)) => {
                                if lhs.shape != rhs.shape {
                                    return EvalResult::Value(TlangValue::Bool(false ^ is_not));
                                }

                                // TODO: Implement comparisons between structs of the same shape.
                                EvalResult::Value(TlangValue::Bool(false ^ is_not))
                            }
                            (TlangObjectKind::String(lhs), TlangObjectKind::String(rhs)) => {
                                EvalResult::Value(TlangValue::Bool((lhs == rhs) ^ is_not))
                            }
                            _ => todo!("eval_object_binary_op: {:?}, {:?}, {:?}", op, lhs, rhs),
                        }
                    }
                    _ => todo!("eval_object_binary_op: {:?}, {:?}, {:?}", op, lhs, rhs),
                }
            }
            hir::BinaryOpKind::Add => match (lhs, rhs) {
                (TlangValue::Object(lhs_id), TlangValue::Object(rhs_id)) => {
                    let lhs = self.get_object_by_id(lhs_id);
                    let rhs = self.get_object_by_id(rhs_id);

                    match (lhs, rhs) {
                        (TlangObjectKind::String(lhs), TlangObjectKind::String(rhs)) => {
                            EvalResult::Value(self.state.new_string(lhs.clone() + rhs))
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
            (TlangValue::I64(lhs), TlangValue::I64(rhs)) => TlangValue::I64(op(lhs, rhs)),
            _ => todo!("eval_bitwise_op: incompatible types"),
        }
    }

    fn eval_fn_decl(&mut self, decl: &hir::FunctionDeclaration) {
        self.state
            .fn_decls
            .insert(decl.hir_id, Rc::new(decl.clone()));

        match &decl.name.kind {
            hir::ExprKind::Path(path) => {
                let fn_object = self.state.new_object(TlangObjectKind::Fn(decl.hir_id));

                self.push_value(fn_object);

                // Used for static struct method resolution, for now..
                self.state.globals.insert(path.join("::"), fn_object);
            }
            hir::ExprKind::FieldAccess(expr, ident) => {
                let path = match &expr.kind {
                    hir::ExprKind::Path(path) => path,
                    _ => todo!("eval_fn_decl: {:?}", expr),
                };
                let struct_decl = self.state.get_struct_decl(path).unwrap();

                self.state.set_struct_method(
                    struct_decl.hir_id.into(),
                    ident.as_str(),
                    TlangStructMethod::HirId(decl.hir_id),
                );
            }
            _ => todo!("eval_fn_decl: {:?}", decl.name),
        }
    }

    fn create_dyn_fn_object(&mut self, decl: &hir::DynFunctionDeclaration) -> TlangValue {
        let name = match &decl.name.kind {
            hir::ExprKind::Path(path) => path.join("::"),
            hir::ExprKind::FieldAccess(expr, ident) => {
                let path = match &expr.kind {
                    hir::ExprKind::Path(path) => path.join("::"),
                    _ => todo!("create_dyn_fn_object: {:?}", expr),
                };

                path + "::" + ident.as_str()
            }
            _ => todo!("create_dyn_fn_object: {:?}", decl.name),
        };
        let variants = decl.variants.clone();

        self.create_native_fn(move |state, args| {
            let variant = variants.iter().find_map(|(arity, hir_id)| {
                if *arity == args.len() {
                    Some(*hir_id)
                } else {
                    None
                }
            });
            if let Some(id) = variant {
                NativeFnReturn::DynamicCall(id)
            } else {
                state.panic(format!("Function {} not found", name))
            }
        })
    }

    fn eval_dyn_fn_decl(&mut self, decl: &hir::DynFunctionDeclaration) {
        let dyn_fn_object = self.create_dyn_fn_object(decl);

        match &decl.name.kind {
            hir::ExprKind::Path(_path) => {
                //self.push_value(dyn_fn_object);
            }
            hir::ExprKind::FieldAccess(expr, ident) => {
                let path = match &expr.kind {
                    hir::ExprKind::Path(path) => path,
                    _ => todo!("eval_dyn_fn_decl: {:?}", expr),
                };
                let struct_decl = self.state.get_struct_decl(path).unwrap();
                self.state.set_struct_method(
                    struct_decl.hir_id.into(),
                    ident.as_str(),
                    TlangStructMethod::Native(dyn_fn_object.get_object_id().unwrap()),
                );
            }
            _ => todo!("eval_dyn_fn_decl: {:?}", decl.name),
        }
    }

    fn eval_struct_decl(&mut self, decl: &hir::StructDeclaration) {
        self.state
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
            for (value, _variant) in decl.variants.iter().enumerate() {
                self.push_value(TlangValue::U64(value as u64));
            }
        } else {
            for (value, variant) in decl.variants.iter().enumerate() {
                // Enum variants with no fields should be treated as incrementing numbers.
                // Enums with multiple fields, should be treated as structs.
                if variant.parameters.is_empty() {
                    let path = format!("{}::{}", decl.name, variant.name.as_str());

                    self.state.define_struct_shape(
                        decl.hir_id.into(),
                        path,
                        vec!["0".to_string()],
                        HashMap::new(),
                    );

                    let obj = self.state.new_object(TlangObjectKind::Struct(TlangStruct {
                        shape: decl.hir_id.into(),
                        field_values: vec![TlangValue::U64(value as u64)],
                    }));

                    self.push_value(obj);
                } else {
                    self.eval_struct_decl(&map_enum_variant_decl_to_struct_decl(
                        variant, &decl.name,
                    ));
                }
            }
        }
    }

    fn eval_call_object(&mut self, callee: TlangValue, args: Vec<TlangValue>) -> TlangValue {
        let id = callee
            .get_object_id()
            .unwrap_or_else(|| self.panic(format!("`{:?}` is not a function", callee)));

        match self.get_object_by_id(id) {
            TlangObjectKind::Closure(closure) => {
                let closure_decl = self.get_closure_decl(closure.id).unwrap().clone();

                self.with_scope(&closure.scope_stack.clone(), |this| {
                    this.eval_fn_call(closure_decl, callee, &args)
                        .unwrap_value()
                })
            }
            TlangObjectKind::Fn(hir_id) => {
                let fn_decl = self
                    .state
                    .get_fn_decl(*hir_id)
                    .unwrap_or_else(|| self.panic("Function not found".to_string()))
                    .clone();

                self.with_root_scope(|this| {
                    this.eval_fn_call(fn_decl.clone(), callee, &args)
                        .unwrap_value()
                })
            }
            TlangObjectKind::NativeFn => self.exec_native_fn(id, callee, &args),
            obj => self.panic(format!("`{:?}` is not a function", obj)),
        }
    }

    fn eval_tail_call(&mut self, call_expr: &hir::CallExpression) -> EvalResult {
        if call_expr.has_wildcard() {
            self.panic("Tail call with wildcard arguments not allowed".to_string());
        }

        let callee = eval_value!(self.eval_expr(&call_expr.callee));
        let args = self.eval_exprs(&call_expr.arguments);

        self.state
            .current_call_frame_mut()
            .set_tail_call(TailCall { callee, args });

        EvalResult::TailCall
    }

    fn tail_call(&mut self) -> EvalResult {
        loop {
            let tail_call = self
                .state
                .current_call_frame_mut()
                .tail_call
                .take()
                .unwrap();

            debug!("tail_call: {:?}", tail_call);

            let fn_hir_id = if let TlangValue::Object(obj) = tail_call.callee {
                if let TlangObjectKind::Fn(hir_id) = self.get_object_by_id(obj) {
                    *hir_id
                } else {
                    self.panic(format!("`{:?}` is not a function", tail_call.callee));
                }
            } else {
                self.panic(format!("`{:?}` is not a function", tail_call.callee));
            };

            // Optimized for self referencial tail calls, if we are calling the same function,
            // we'll reuse the fn declaration stored on the current call frame.
            let fn_decl = match self.state.current_call_frame().get_fn_decl() {
                Some(fn_decl) if fn_decl.hir_id == fn_hir_id => fn_decl.clone(),
                _ => self.state.get_fn_decl(fn_hir_id).unwrap_or_else(|| {
                    self.panic(format!("Function `{:?}` not found", tail_call.callee));
                }),
            };

            // Instead of a recursive call, replace the current function scope
            self.replace_current_fn_scope(fn_decl.clone(), tail_call.callee, &tail_call.args);
            match self.eval_block_inner(&fn_decl.body) {
                EvalResult::TailCall => continue,
                result => return result,
            }
        }
    }

    fn replace_current_fn_scope(
        &mut self,
        fn_decl: Rc<hir::FunctionDeclaration>,
        callee: TlangValue,
        args: &[TlangValue],
    ) {
        debug!("replace_current_fn_scope: {:?}", fn_decl.name());

        self.state
            .current_call_frame_mut()
            .replace_fn_decl(fn_decl.clone());
        self.state.scope_stack.drop_block_scopes();
        self.state.scope_stack.clear_current_scope();

        // TODO: Methods currently do not reserve a slot for the fn itself.
        if fn_decl.name.path().is_some() {
            self.push_value(callee);
        }

        for arg in args {
            self.push_value(*arg);
        }
    }

    fn eval_partial_call(&mut self, call_expr: &hir::CallExpression) -> EvalResult {
        let callee = eval_value!(self.eval_expr(&call_expr.callee));
        let applied_args: Vec<TlangValue> = call_expr
            .arguments
            .iter()
            .map(|expr| {
                if expr.is_wildcard() {
                    TlangValue::Nil
                } else {
                    self.eval_expr(expr).unwrap_value()
                }
            })
            .collect();

        let fn_object = self.create_native_fn(move |_, args| {
            let mut applied_args = applied_args.clone();
            // For each arg in args, replace a hole (Nil) from the already applied args
            for arg in args {
                let index = applied_args
                    .iter()
                    .position(|a| *a == TlangValue::Nil)
                    .unwrap();

                applied_args[index] = *arg;
            }

            NativeFnReturn::PartialCall(Box::new((callee, applied_args)))
        });

        EvalResult::Value(fn_object)
    }

    fn eval_call(&mut self, call_expr: &hir::CallExpression) -> EvalResult {
        debug!("eval_call: {:?}", call_expr);

        if call_expr.has_wildcard() {
            return self.eval_partial_call(call_expr);
        }

        let return_value = match &call_expr.callee.kind {
            hir::ExprKind::Path(path) if let Some(value) = self.resolve_path(path) => {
                let args = self.eval_exprs(&call_expr.arguments);

                self.eval_call_object(value, args)
            }
            // If the path resolves to a struct, we create a new object.
            hir::ExprKind::Path(path)
                if let Some(struct_decl) = self.state.get_struct_decl(path) =>
            {
                self.eval_struct_ctor(call_expr, &struct_decl)
            }
            // The path might resolve to a struct method directly.
            hir::ExprKind::Path(path)
                if let Some(struct_decl) = self.state.get_struct_decl(&path.as_init()) =>
            {
                let args = self.eval_exprs(&call_expr.arguments);

                self.eval_call_struct_method(struct_decl.hir_id.into(), path.last_ident(), &args)
            }
            hir::ExprKind::Path(path) => {
                self.panic(format!(
                    "Function `{}` not found\nCurrent scope: {:?}",
                    path.join("::"),
                    self.state.current_scope().borrow()
                ));
            }
            hir::ExprKind::FieldAccess(expr, ident) => {
                let call_target = eval_value!(self.eval_expr(expr));
                let mut args = self.eval_exprs(&call_expr.arguments);
                args.insert(0, call_target);

                let shape_key = self.get_object(call_target).and_then(|o| o.get_shape_key());

                if let Some(shape_key) = shape_key {
                    self.eval_call_struct_method(shape_key, ident, &args)
                } else {
                    self.panic(format!("Field access on non-struct: {:?}", call_target));
                }
            }
            _ => todo!("eval_call: {:?}", call_expr.callee),
        };

        EvalResult::Value(return_value)
    }

    fn eval_fn_call(
        &mut self,
        fn_decl: Rc<hir::FunctionDeclaration>,
        callee: TlangValue,
        args: &[TlangValue],
    ) -> EvalResult {
        debug!(
            "eval_fn_call: {} {:?} with args {:?}",
            fn_decl.name(),
            self.state.stringify(callee),
            args.iter()
                .map(|a| self.state.stringify(*a))
                .collect::<Vec<_>>()
        );

        if fn_decl.parameters.len() != args.len() {
            self.state.panic(format!(
                "Function `{:?}` expects {} arguments, but got {}",
                fn_decl.name(),
                fn_decl.parameters.len(),
                args.len()
            ));
        }

        self.with_new_fn_scope(fn_decl.clone(), |this| {
            // TODO: Methods currently do not reserve a slot for the fn itself.
            if fn_decl.name.path().is_some() {
                this.push_value(callee);
            }

            for arg in args {
                this.push_value(*arg);
            }

            match this.eval_block_inner(&fn_decl.body) {
                EvalResult::TailCall => this.tail_call(),
                result => result,
            }
        })
    }

    fn exec_native_fn(
        &mut self,
        id: TlangObjectId,
        callee: TlangValue,
        args: &[TlangValue],
    ) -> TlangValue {
        // Do we need and want to reset the scope for native functions?
        // It could be somewhat interesting to manipulate the current scope from native functions.
        let r = self.with_root_scope(|this| {
            if let Some(native_fn) = this.native_fns.get_mut(&id) {
                native_fn(&mut this.state, args)
            } else {
                this.panic(format!("Native function not found: {:?}", id))
            }
        });

        match r {
            NativeFnReturn::Return(value) => value,
            NativeFnReturn::DynamicCall(id) => {
                if let Some(fn_decl) = self.state.get_fn_decl(id) {
                    self.with_root_scope(|this| this.eval_fn_call(fn_decl.clone(), callee, args))
                        .unwrap_value()
                } else {
                    self.panic(format!("Function not found: {:?}", id));
                }
            }
            NativeFnReturn::PartialCall(box (fn_object, args)) => {
                self.eval_call_object(fn_object, args)
            }
        }
    }

    fn eval_struct_ctor(
        &mut self,
        call_expr: &hir::CallExpression,
        struct_decl: &hir::StructDeclaration,
    ) -> TlangValue {
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
                .map(|(i, arg)| (i.to_string(), self.eval_expr(arg).unwrap_value()))
                .collect()
        } else {
            match &call_expr.arguments[0].kind {
                hir::ExprKind::Dict(entries) => entries
                    .iter()
                    .map(|(key, value)| {
                        let key = match &key.kind {
                            hir::ExprKind::Path(path) => path.first_ident().to_string(),
                            _ => todo!("eval_call: {:?}", key),
                        };
                        let value = self.eval_expr(value).unwrap_value();
                        (key, value)
                    })
                    .collect(),
                _ => todo!("eval_call: {:?}", call_expr.arguments[0]),
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

    fn eval_call_struct_method(
        &mut self,
        struct_key: ShapeKey,
        method_name: &Ident,
        args: &[TlangValue],
    ) -> TlangValue {
        let struct_shape = self.state.get_shape(struct_key).unwrap();

        match struct_shape.get_method(method_name.as_str()) {
            Some(TlangStructMethod::HirId(id)) => {
                let fn_decl = self.state.get_fn_decl(*id).unwrap().clone();
                self.with_root_scope(|this| {
                    // TODO: Struct methods should have a value to refer to.
                    this.eval_fn_call(fn_decl.clone(), TlangValue::Nil, args)
                        .unwrap_value()
                })
            }
            Some(TlangStructMethod::Native(id)) => {
                // TODO: Struct methods should have a value to refer to.
                self.exec_native_fn(*id, TlangValue::Nil, args)
            }
            _ => {
                self.panic(format!(
                    "{} does not have a method {:?}, {:?}",
                    struct_shape.name,
                    method_name.as_str(),
                    struct_shape.method_map.keys()
                ));
            }
        }
    }

    fn eval_let_stmt(&mut self, pat: &hir::Pat, expr: &hir::Expr, _ty: &hir::Ty) -> EvalResult {
        let value = eval_value!(self.eval_expr(expr));

        if !self.eval_pat(pat, value) {
            // We'd probably want to do it more like Rust via a if let statement, and have the
            // normal let statement be only valid for identifiers.
            self.panic(format!(
                "Pattern did not match value {:?}",
                self.state.stringify(value)
            ));
        }

        EvalResult::Void
    }

    fn eval_list_expr(&mut self, values: &[hir::Expr]) -> EvalResult {
        let mut field_values = Vec::with_capacity(values.len());

        for expr in values {
            if let hir::ExprKind::Unary(UnaryOp::Spread, expr) = &expr.kind {
                let value = eval_value!(self.eval_expr(expr));

                if let TlangValue::Object(id) = value {
                    match self.get_object_by_id(id) {
                        TlangObjectKind::Slice(slice) => {
                            field_values.extend_from_slice(self.get_slice_values(*slice))
                        }
                        TlangObjectKind::Struct(list_struct) => {
                            field_values.extend(&list_struct.field_values)
                        }
                        obj => self.panic(format!("Expected list, got {:?}", obj)),
                    }
                } else {
                    self.panic(format!("Expected list, got {:?}", value));
                }
            } else {
                field_values.push(eval_value!(self.eval_expr(expr)));
            }
        }

        EvalResult::Value(self.state.new_list(field_values))
    }

    fn eval_dict_expr(&mut self, entries: &[(hir::Expr, hir::Expr)]) -> EvalResult {
        let mut field_values: Vec<TlangValue> = Vec::with_capacity(entries.len());
        let mut shape_keys = Vec::with_capacity(entries.len());

        for entry in entries {
            field_values.push(eval_value!(self.eval_expr(&entry.1)));

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

        EvalResult::Value(self.state.new_object(TlangObjectKind::Struct(TlangStruct {
            shape,
            field_values,
        })))
    }

    fn eval_match(&mut self, expr: &hir::Expr, arms: &[hir::MatchArm]) -> EvalResult {
        let match_value = eval_value!(self.eval_expr(expr));

        debug!("eval_match: {}", self.state.stringify(match_value));

        for arm in arms {
            match self.eval_match_arm(arm, match_value) {
                MatchResult::Matched(value) => return value,
                MatchResult::NotMatched(eval_result) => propagate!(eval_result),
            }
        }

        EvalResult::Void
    }

    /// Evaluates a match arm and returns the value if it matches, otherwise returns None.
    fn eval_match_arm(&mut self, arm: &hir::MatchArm, value: TlangValue) -> MatchResult {
        //debug!("eval_match_arm: {:?} {:?}", arm, value);

        self.with_new_scope(arm, |this| {
            if !this.eval_pat(&arm.pat, value) {
                return MatchResult::NotMatched(EvalResult::Void);
            }

            if let Some(expr) = &arm.guard {
                if let hir::ExprKind::Let(pat, expr) = &expr.kind {
                    let value = match this.eval_expr(expr) {
                        EvalResult::Value(value) => value,
                        other => return MatchResult::NotMatched(other),
                    };

                    if !this.eval_pat(pat, value) {
                        return MatchResult::NotMatched(EvalResult::Void);
                    }
                } else if !this.eval_expr(expr).unwrap_value().is_truthy(&this.state) {
                    return MatchResult::NotMatched(EvalResult::Void);
                }
            }

            if let hir::ExprKind::Block(block) = &arm.expr.kind {
                // TODO: Do we need this?
                MatchResult::Matched(this.eval_block_inner(block))
            } else {
                MatchResult::Matched(this.eval_expr(&arm.expr))
            }
        })
    }

    fn eval_pat(&mut self, pat: &hir::Pat, value: TlangValue) -> bool {
        self.state.set_current_span(pat.span);

        match &pat.kind {
            hir::PatKind::Literal(literal) => {
                let literal_value = self.eval_literal(literal);

                if value == literal_value {
                    return true;
                }

                if let (TlangValue::Object(lhs), box token::Literal::String(rhs)) =
                    (value, &literal)
                {
                    if let TlangObjectKind::String(lhs) = self.get_object_by_id(lhs) {
                        return lhs == rhs;
                    }
                }

                false
            }
            hir::PatKind::List(patterns) => self.eval_pat_list(patterns, value),
            hir::PatKind::Identifier(_id, ident) => {
                debug!("eval_pat: {} = {}", ident, self.state.stringify(value));

                self.push_value(value);

                true
            }
            hir::PatKind::Wildcard => true,
            hir::PatKind::Enum(path, kvs) => match self.get_object(value) {
                Some(TlangObjectKind::Struct(_)) => {
                    let shape = self.get_shape_of(value).unwrap();
                    let path_name = path.join("::");

                    if shape.name != path_name {
                        return false;
                    }

                    kvs.iter().all(|(k, pat)| {
                        self.get_shape_of(value)
                            .and_then(|shape| shape.get_field_index(&k.to_string()))
                            .is_some_and(|field_index| {
                                let tlang_struct = self.get_struct(value).unwrap();
                                self.eval_pat(pat, tlang_struct.field_values[field_index])
                            })
                    })
                }
                _ => todo!("eval_pat: Enum({:?}, {:?})", path, kvs),
            },
            hir::PatKind::Rest(_) => unreachable!("Rest patterns can only appear in list patterns"),
        }
    }

    // TODO: Instead of having rest patterns within list patterns, we should have a pattern
    //       specifically for matching against tail values (list, strings, objects)
    fn eval_pat_list(&mut self, patterns: &[hir::Pat], value: TlangValue) -> bool {
        if !value.is_object() {
            return false;
        }

        match self.get_object(value).unwrap() {
            TlangObjectKind::Struct(list_struct) => {
                let list_values_length = list_struct.len();

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

                for (i, pat) in patterns.iter().enumerate() {
                    let list_struct = self.get_struct(value).unwrap();

                    if let hir::PatKind::Rest(pat) = &pat.kind {
                        let rest_object =
                            self.state
                                .new_slice(value, i, list_struct.field_values.len() - i);

                        return self.eval_pat(pat, rest_object);
                    }

                    if !self.eval_pat(pat, list_struct.field_values[i]) {
                        return false;
                    }
                }

                true
            }
            TlangObjectKind::Slice(slice) => {
                let list_values_length = slice.len();

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

                for (i, pat) in patterns.iter().enumerate() {
                    let list_slice = self.get_slice(value).unwrap();

                    if let hir::PatKind::Rest(pat) = &pat.kind {
                        let rest_object = self.state.new_slice(
                            list_slice.of(),
                            list_slice.start() + i,
                            list_slice.len() - i,
                        );

                        return self.eval_pat(pat, rest_object);
                    }

                    if !self.eval_pat(pat, self.get_slice_value(list_slice, i)) {
                        return false;
                    }
                }

                true
            }
            TlangObjectKind::String(_) => {
                for (i, pat) in patterns.iter().enumerate() {
                    let str_value = self.get_str(value).unwrap();

                    if let hir::PatKind::Rest(pat) = &pat.kind {
                        let rest_object = if i <= str_value.len() {
                            self.state.new_string(str_value[i..].to_string())
                        } else {
                            self.state.new_string(String::new())
                        };

                        return self.eval_pat(pat, rest_object);
                    }

                    let char_match = if let Some(character) = str_value.chars().nth(i) {
                        self.state.new_string(character.to_string())
                    } else {
                        self.state.new_string(String::new())
                    };

                    if !self.eval_pat(pat, char_match) {
                        return false;
                    }
                }

                true
            }
            _ => todo!("eval_pat: {:?}", value),
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
    use std::cell::RefCell;

    use super::*;
    use indoc::indoc;
    use pretty_assertions::{assert_eq, assert_matches};

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
            let mut parser = tlang_parser::Parser::from_source(src);
            let ast = parser.parse().unwrap();
            let hir = self.lowering_context.lower_module_in_current_scope(&ast);
            self.interpreter.eval(&hir)
        }

        fn eval(&mut self, src: &str) -> TlangValue {
            let block = format!("{{ {} }};", src);
            let mut parser = tlang_parser::Parser::from_source(&block);
            let ast = parser.parse().unwrap();
            let hir = self.lowering_context.lower_module_in_current_scope(&ast);

            match &hir.block.stmts[0].kind {
                hir::StmtKind::Expr(expr) => self.interpreter.eval_expr(expr).unwrap_value(),
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
        assert_matches!(result, TlangValue::U64(3));
    }

    #[test]
    fn test_addition_of_mixed_ints_and_floats() {
        let mut interpreter = interpreter("");

        assert_matches!(interpreter.eval("1 + 2.0"), TlangValue::F64(3.0));
        assert_matches!(interpreter.eval("1.0 + 2"), TlangValue::F64(3.0));
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

        assert_matches!(interpreter.eval("add(1, 2)"), TlangValue::U64(3));
    }

    #[test]
    fn test_early_return() {
        let mut interpreter = interpreter(indoc! {"
            fn r(a) {
                if a == 0; {
                    return 1;
                }

                2
            }
        "});
        assert_matches!(interpreter.eval("r(0)"), TlangValue::U64(1));
        assert_matches!(interpreter.eval("r(1)"), TlangValue::U64(2));
    }

    #[test]
    fn test_deeply_nested_early_return() {
        let mut interpreter = interpreter(indoc! {"
            fn r(a) {
                if a > 1; {
                    if a > 2; {
                        if a > 3; {
                            return 4;
                        }

                        return 3;
                    }

                    return 2;
                }

                1
            }
        "});

        assert_matches!(interpreter.eval("r(0)"), TlangValue::U64(1));
        assert_matches!(interpreter.eval("r(1)"), TlangValue::U64(1));
        assert_matches!(interpreter.eval("r(2)"), TlangValue::U64(2));
        assert_matches!(interpreter.eval("r(3)"), TlangValue::U64(3));
        assert_matches!(interpreter.eval("r(4)"), TlangValue::U64(4));
        assert_matches!(interpreter.eval("r(5)"), TlangValue::U64(4));
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

        assert_matches!(interpreter.eval("fib(10)"), TlangValue::U64(55));
        assert_matches!(interpreter.eval("fib(20)"), TlangValue::U64(6765));
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

        assert_matches!(interpreter.eval("add_5(10)"), TlangValue::U64(15));
        assert_matches!(interpreter.eval("add_5(20)"), TlangValue::U64(25));
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

        assert_matches!(interpreter.eval("match_test(1)"), TlangValue::U64(2));
        assert_matches!(interpreter.eval("match_test(2)"), TlangValue::U64(3));
        assert_matches!(interpreter.eval("match_test(3)"), TlangValue::U64(4));
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
        assert_matches!(interpreter.eval("match_test([1, 2])"), TlangValue::U64(2));
        assert_matches!(interpreter.eval("match_test([2, 3])"), TlangValue::U64(3));
        assert_matches!(interpreter.eval("match_test([3, 4])"), TlangValue::U64(4));
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
        assert_matches!(interpreter.eval("match_test(1)"), TlangValue::U64(2));
        assert_matches!(interpreter.eval("match_test(2)"), TlangValue::U64(2));
        assert_matches!(interpreter.eval("match_test(3)"), TlangValue::U64(3));
    }

    #[test]
    fn test_fn_pattern_matching() {
        let mut interpreter = interpreter(indoc! {"
            fn fib(n) { fib(n, 0, 1) }
            fn fib(0, a, _) { a }
            fn fib(1, _, b) { b }
            fn fib(n, a, b) { rec fib(n - 1, b, a + b) }
        "});

        assert_matches!(interpreter.eval("fib(10)"), TlangValue::U64(55));
        assert_matches!(interpreter.eval("fib(50)"), TlangValue::U64(12586269025));
    }

    #[test]
    fn test_builtin_log() {
        let mut interpreter = interpreter(indoc! {""});
        let calls = Rc::new(RefCell::new(vec![]));

        let calls_tracker = calls.clone();

        let log_fn_object_id = interpreter
            .interpreter
            .state
            .globals
            .get("log")
            .unwrap()
            .get_object_id()
            .unwrap();

        interpreter.interpreter.native_fns.insert(
            log_fn_object_id,
            Box::new(move |_, args| {
                calls_tracker.borrow_mut().push(args.to_vec());
                NativeFnReturn::Return(TlangValue::Nil)
            }),
        );

        assert_matches!(interpreter.eval("log(10)"), TlangValue::Nil);
        assert_matches!(calls.borrow()[0][..], [TlangValue::U64(10)]);
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
        assert_matches!(interpreter.eval("point.x"), TlangValue::U64(10));
        assert_matches!(interpreter.eval("point.y"), TlangValue::U64(20));
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

        assert_matches!(interpreter.eval("Values::One"), TlangValue::U64(0));
        assert_matches!(interpreter.eval("Values::Two"), TlangValue::U64(1));
        assert_matches!(interpreter.eval("Values::Three"), TlangValue::U64(2));
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
        assert_matches!(some_value, TlangValue::Object(_));

        let none_value = interpreter.eval("none");
        assert_matches!(none_value, TlangValue::Object(_));

        let some_data = match some_value {
            TlangValue::Object(id) => interpreter
                .interpreter
                .get_object_by_id(id)
                .get_struct()
                .unwrap(),
            val => panic!("Expected struct, got {:?}", val),
        };

        let none_data = match none_value {
            TlangValue::Object(id) => interpreter
                .interpreter
                .get_object_by_id(id)
                .get_struct()
                .unwrap(),
            val => panic!("Expected struct, got {:?}", val),
        };

        assert_matches!(some_data.field_values[0], TlangValue::U64(10));
        assert_matches!(none_data.field_values[0], TlangValue::U64(0));
    }

    #[test]
    fn test_rest_is_a_slice() {
        let mut interpreter = interpreter(indoc! {"
            fn as_slice([...rest]) { rest }
            fn as_slice(_) { [] }
        "});
        let list = interpreter.eval("as_slice([1, 2, 3])");
        let list_data = match list {
            TlangValue::Object(id) => interpreter
                .interpreter
                .get_object_by_id(id)
                .get_slice()
                .unwrap(),
            val => panic!("Expected slice, got {:?}", val),
        };
        assert_eq!(list_data.start(), 0);
        assert_eq!(list_data.len(), 3);
    }

    #[test]
    fn test_rest_is_a_slice_with_offset() {
        let mut interpreter = interpreter(indoc! {"
            fn as_slice([_, _, ...rest]) { rest }
            fn as_slice(_) { [] }

            fn head([]) { 0 }
            fn head([head]) { head }
        "});
        let list = interpreter.eval("as_slice(as_slice([1, 2, 3, 4, 5]))");
        let list_data = match list {
            TlangValue::Object(id) => interpreter
                .interpreter
                .get_object_by_id(id)
                .get_slice()
                .unwrap(),
            val => panic!("Expected slice, got {:?}", val),
        };
        assert_eq!(list_data.start(), 4);
        assert_eq!(list_data.len(), 1);

        let head = interpreter.eval("head(as_slice([1, 2, 3, 4, 5]))");
        assert_eq!(head.as_usize(), 3);
    }
}
